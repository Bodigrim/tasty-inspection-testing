-- |
-- Module:      Test.Tasty.Inspection.Plugin
-- Copyright:   (c) 2017 Joachim Breitner, 2021 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  andrew.lelechenko@gmail.com
--
{-# LANGUAGE CPP             #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module Test.Tasty.Inspection.Plugin (plugin) where

import Control.Monad (foldM)
import qualified Language.Haskell.TH.Syntax as TH
import System.Exit (exitFailure)

#if MIN_VERSION_ghc(9,0,0)
import GHC.Plugins
#else
import GhcPlugins
#endif

#if MIN_VERSION_ghc(9,2,0)
import GHC.Types.TyThing
#endif

#if !MIN_VERSION_ghc(9,3,0)
import Control.Monad (when)
#endif

import Test.Inspection (Obligation(..))
import qualified Test.Inspection.Plugin as P (checkProperty, CheckResult(..))
import Test.Tasty.Inspection.Internal (CheckResult(..))

-- | The plugin for inspection testing.
-- You normally do not need to touch it yourself,
-- 'Test.Tasty.Inspection.inspectTest' will enable it automatically.
plugin :: Plugin
plugin = defaultPlugin
    { installCoreToDos = install
#if MIN_VERSION_ghc(8,6,0)
    , pluginRecompile = \_args -> pure NoForceRecompile
#endif
    }

install :: a -> [CoreToDo] -> CoreM [CoreToDo]
install = const $ \passes -> pure $ passes ++
    [CoreDoPluginPass "Test.Tasty.Inspection.Plugin" proofPass]

extractObligations :: ModGuts -> (ModGuts, [(Name, Obligation)])
extractObligations guts = (guts { mg_anns = anns_clean }, obligations)
  where
    (anns_clean, obligations) = partitionMaybe findObligationAnn (mg_anns guts)

findObligationAnn :: Annotation -> Maybe (Name, Obligation)
findObligationAnn (Annotation (NamedTarget n) payload) =
    (n,) <$> fromSerialized deserializeWithData payload
findObligationAnn _ = Nothing

checkObligation :: ModGuts -> (Name, Obligation) -> CoreM ModGuts
checkObligation guts (name, obl) = do
    res <- P.checkProperty guts (target obl) (property obl)
    e   <- resultToExpr res
    pure $ updateNameInGuts name e guts

updateNameInGuts :: Name -> CoreExpr -> ModGuts -> ModGuts
updateNameInGuts n expr guts =
    guts {mg_binds = map (updateNameInGut n expr) (mg_binds guts) }

updateNameInGut :: Name -> CoreExpr -> CoreBind -> CoreBind
updateNameInGut n e = \case
    NonRec v _
        | getName v == n -> NonRec v e
    bind -> bind

fromTHName :: TH.Name -> CoreM Name
fromTHName thn = thNameToGhcName thn >>= \case
    Nothing -> do
        errorMsg $ text "Could not resolve TH name" <+> text (show thn)
        liftIO $ exitFailure -- kill the compiler. Is there a nicer way?
    Just n -> pure n

dcExpr :: TH.Name -> CoreM CoreExpr
dcExpr thn = do
    name <- fromTHName thn
    dc <- lookupDataCon name
    pure $ Var (dataConWrapId dc)

resultToExpr :: P.CheckResult -> CoreM CoreExpr
resultToExpr P.ResSuccess =
    dcExpr 'ResSuccess
resultToExpr (P.ResSuccessWithMessage sdoc) = do
    dflags <- getDynFlags
    App <$> dcExpr 'ResSuccessWithMessage <*> mkStringExpr (showSDoc dflags sdoc)
resultToExpr (P.ResFailure sdoc) = do
    dflags <- getDynFlags
    App <$> dcExpr 'ResFailure <*> mkStringExpr (showSDoc dflags sdoc)

proofPass :: ModGuts -> CoreM ModGuts
proofPass guts = do
#if !MIN_VERSION_ghc(9,3,0)
    dflags <- getDynFlags
    when (optLevel dflags < 1) $ warnMsg
#if MIN_VERSION_ghc(8,9,0)
        NoReason
#endif
        $ fsep $ map text
        $ words "Test.Inspection: Compilation without -O detected. Expect optimizations to fail."
#endif
    uncurry (foldM checkObligation) (extractObligations guts)

partitionMaybe :: (a -> Maybe b) -> [a] -> ([a], [b])
partitionMaybe f = foldr go ([], [])
  where
    go a (l, r) = case f a of
        Nothing -> (a : l, r)
        Just b  -> (l, b : r)
