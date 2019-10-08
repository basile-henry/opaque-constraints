{- |
Description : Create opaque constraint synonyms
Copyright   : (c) Basile Henry, 2019

Have you ever wished GHC would complain about your constraint synonym not being
fulfilled rather than what it depends on?

There is a way! It is a bit convoluted, but if you're interested it is very well
explained in https://kcsongor.github.io/opaque-constraint-synonyms/

This package automates this setup using /TemplateHaskell/. It should be fairly
safe to use as long as the module `mkOpaque` is used in has an explicit export
list!
`mkOpaque` creates a data type that __should not__ be exported from your module!

Here is a small example module using `mkOpaque`:

@
{\-\# LANGUAGE ConstraintKinds      #-\}
{\-\# LANGUAGE FlexibleInstances    #-\}
{\-\# LANGUAGE TemplateHaskell      #-\}
{\-\# LANGUAGE UndecidableInstances #-\}

module Serialise (roundTrip) where

import Data.Constraint.Opaque

mkOpaque [d|
  -- This type alias for the constraints Show and Read is changed into a more
  -- "opaque" constraint synonym
  type Serialise a = (Show a, Read a)
  |]

roundTrip :: Serialise a => a -> a
roundTrip = read . show
@

Now in another module if we have the following:

@
foo :: a -> a
foo = roundTrip
@

GHC will complain about the missing @Serialise a@ constraint (mentioning it can
be fulfilled with @Show a@ and @Read a@ fulfilled) instead of skipping straight
to constraints @Serialise@ depends on.
This means we can use @Serialise@ as a proper abstraction over what it is a
synonym for instead of merely being a helper to write many constraints more
concisely.

Thanks to Csongor Kiss for the ideas making this possible!
-}

{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Constraint.Opaque
  ( mkOpaque
  , mkOpaquePoly
  ) where

-- base
import           Control.Monad              (filterM)
import           Data.IORef                 (atomicModifyIORef')
import           Data.List                  (foldl', nub)
import           Data.Maybe                 (mapMaybe, maybeToList)
import           Data.Typeable              (Typeable)
import           GHC.TypeLits               (KnownNat, KnownSymbol)

-- template-haskell
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax (counter)

{- |
`mkOpaque` changes constraint synonyms such that GHC will mention the synonym
in error messages.

The function is intended to be used with a declaration QuasiQuote @[d| ... |]@
as argument. The QuasiQuote may only contain constraint synonyms (and comments).
-}
mkOpaque :: DecsQ -> DecsQ
mkOpaque = mkOpaque' False

{- |
Same as `mkOpaque` but also supports constraints over non `*`/`Type` kinds
(for example @GHC.TypeLits.KnownNat (n :: Nat)@)

If unsure between using `mkOpaque` and `mkOpaquePoly`, using `mkOpaquePoly` is
a good default.

/Note/: This will require the @PolyKinds@ extension.
-}
mkOpaquePoly :: DecsQ -> DecsQ
mkOpaquePoly = mkOpaque' True

mkOpaque' :: Bool -> DecsQ -> DecsQ
mkOpaque' polyKinded decls = do
  synonyms <- (fmap . fmap) typeSynonym decls

  let constraints = concatMap synonymConstraints synonyms
  classes <- nub <$> filterM isInstantiableClass constraints

  count <- runIO $ atomicModifyIORef' counter (\c -> (succ c, c))
  opaque <- newName $ "Opaque_" ++ show count

  let opaqueData
        | polyKinded = do
          k <- newName "k"
          dataFamilyD opaque [] (Just $ VarT k)
        | otherwise = dataD (cxt []) opaque [] Nothing [] []

  sequence $ concat
    [ pure opaqueData
    , map (dummyInstance opaque) classes
    , map synonymClass synonyms
    , map synonymInstance synonyms
    , map (opaqueSynonymInstance opaque) synonyms
    ]

data TypeSynonym =
  TypeSynonym
    { synonym    :: Name
    , typeVars   :: [Name]
    , constraint :: Type
    }

typeSynonym :: Dec -> TypeSynonym
typeSynonym (TySynD name tyVarBndrs type') =
  TypeSynonym name (varName <$> tyVarBndrs) type'
  where
    varName (PlainTV n)    = n
    varName (KindedTV n _) = n
typeSynonym dec = error $
  "Expected type synonym but got:\n" ++
  (unlines . map ("  " ++) . lines . pprint) dec

synonymConstraints :: TypeSynonym -> [Name]
synonymConstraints = go . constraint
  where
    go = \case
      AppT l    r -> go l ++ go r
      ConT name   -> [name]
      _           -> []

synonymClass :: TypeSynonym -> DecQ
synonymClass TypeSynonym{..} =
  classD (cxt [pure constraint]) synonym (PlainTV <$> typeVars) [] []

synonymInstance :: TypeSynonym -> DecQ
synonymInstance TypeSynonym{..} =
  instanceD (cxt [pure constraint]) appliedSynonym []
  where
    appliedSynonym = foldl' appT (conT synonym) (varT <$> typeVars)

isInstantiableClass :: Name -> Q Bool
isInstantiableClass = fmap filterClasse . reify
  where
    nonInstantiableClasses =
      [ ''KnownNat
      , ''KnownSymbol
      , ''Typeable
      ]

    filterClasse c
      | ClassI dec _ <- c
      , ClassD _ name _ _ _ <- dec
      = name `notElem` nonInstantiableClasses

      | otherwise = False

-- This assumes only class declarations are passed as the second argument
dummyInstance :: Name -> Name -> DecQ
dummyInstance data' class' = do
  ClassI dec _ <- reify class'
  ClassD _ _ _ _ sigs <- pure dec

  let sigName (SigD n _) = Just n
      sigName _          = Nothing

      funs = mapMaybe sigName sigs

      errorFunction fun = do
        modStr <- pprint <$> thisModule
        let funStr = pprint fun
            dataStr = pprint data'
            err = [e|
              error
                $  funStr ++ ": Data type \"" ++ dataStr
                ++ "\" should not be exposed in module \"" ++ modStr ++ "\"!"
              |]
        funD fun [clause [] (normalB err) []]

  instanceD
    (cxt [])
    (pure $ AppT (ConT class') (ConT data'))
    (errorFunction <$> funs)

opaqueSynonymInstance :: Name -> TypeSynonym -> DecQ
opaqueSynonymInstance opaque TypeSynonym{..} = do
  preds <- maybeToList <$> instanceConstraints constraint

  instanceWithOverlapD
    (Just Overlapping)
    (cxt $ fmap pure preds)
    (pure $ AppT (ConT synonym) (ConT opaque))
    []
  where
    instanceConstraints :: Type -> Q (Maybe Type)
    instanceConstraints x = do
      runIO $ print x
      case x of
        AppT (TupleT n) r ->
          instanceConstraints r >>= \case
            Nothing -> pure . Just $ TupleT (pred n)
            Just t  -> pure . Just $ AppT (TupleT n) t

        AppT l r -> do
          l' <- instanceConstraints l
          r' <- instanceConstraints r
          pure $ AppT <$> l' <*> r'

        t@(ConT name) -> do
          instantiable <- isInstantiableClass name
          pure $
            if instantiable
            -- drop instantiable classes, we already have instances for them
            then Nothing
            else Just t

        VarT _ ->
          -- Replace free variables by opaque
          pure $ Just (ConT opaque)

        t -> pure $ Just t
