module Scary
    ( plugin
    ) where

import Class (className)
import Data.Foldable (forM_)
import Data.Maybe (catMaybes)
import DynFlags (getDynFlags)
import HscTypes (ModSummary (..))
import HsExtension (GhcRn)
import HsImpExp (IE (..), IEWrappedName (..), ImportDecl (..), LIE, LIEWrappedName)
import IOEnv (IOEnv, liftIO, readMutVar)
import Name (Name, mkClsOcc, nameSrcSpan, occName)
import Outputable (Outputable, ppr, printForUser, alwaysQualify)
import Plugins (CommandLineOption, Plugin (..), defaultPlugin, purePlugin)
import PrelNames (pRELUDE_NAME)
import RdrName (GlobalRdrElt)
import RnNames (ImportDeclUsage, findImportUsage)
import SrcLoc (GenLocated (..), SrcSpan (..), srcSpanEndCol, srcSpanEndLine,
               srcSpanStartCol, srcSpanStartLine, unLoc)
import System.IO (stdout)
import TcEvidence (EvTerm(..))
import TcRnTypes
  ( Ct
  , Env
  , TcGblEnv(..)
  , TcLclEnv
  , TcPlugin(..)
  , TcPluginM
  , TcPluginResult(..)
  , TcRn
  , ctEvidence
  , ctEvPred
  , ctEvTerm
  , unsafeTcPluginTcM
  )
import Type (PredTree(..), classifyPredType)

plugin :: Plugin
plugin = defaultPlugin {typeCheckResultAction = testPlug, tcPlugin = scaryPlugin, pluginRecompile = purePlugin }

testPlug :: [CommandLineOption] -> ModSummary -> TcGblEnv -> IOEnv (Env TcGblEnv TcLclEnv) TcGblEnv
testPlug _ _ gblEnv = do
  liftIO $ print "in type check result action..."
  pure gblEnv

scaryPlugin :: [CommandLineOption] -> Maybe TcPlugin
scaryPlugin _ =
  Just $
    TcPlugin
      { tcPluginInit = unsafeTcPluginTcM (liftIO $ putStrLn "starting scary plugin...")
      , tcPluginSolve = const solver
      , tcPluginStop = const $ unsafeTcPluginTcM (liftIO $ putStrLn "\nending scary plugin.")
      }

printCt :: [Ct] -> IOEnv (Env TcGblEnv TcLclEnv) ()
printCt cts = do
  let docs = fmap ppr cts
  dynFlags <- getDynFlags
  liftIO $ forM_ docs $ printForUser dynFlags stdout alwaysQualify

solver :: [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solver _givenCts _derivedCts wantedCts = do
  unsafeTcPluginTcM (liftIO $ putStrLn "\n\nin solver...")
  -- unsafeTcPluginTcM (liftIO $ putStrLn "\ngivens:")
  -- unsafeTcPluginTcM (printCt givens)
  -- unsafeTcPluginTcM (liftIO $ putStrLn "\nderiveds:")
  -- unsafeTcPluginTcM (printCt deriveds)
  pstr "\nwantedCts:"
  pstr "-----------------------------"
  -- unsafeTcPluginTcM (printCt wantedCts)
  res <- traverse solveIOCt wantedCts
  pure $ TcPluginOk (catMaybes res) []

pp :: Outputable a => a -> TcPluginM ()
pp a =
  unsafeTcPluginTcM $ do
    dynFlags <- getDynFlags
    liftIO $ printForUser dynFlags stdout alwaysQualify (ppr a)

pstr :: String -> TcPluginM ()
pstr str =
  unsafeTcPluginTcM $ do
    dynFlags <- getDynFlags
    liftIO $ putStrLn str

solveIOCt :: Ct -> TcPluginM (Maybe (EvTerm, Ct))
solveIOCt ct = do
  let evid = ctEvidence ct
      predty = ctEvPred evid
      ter = ctEvTerm evid
  pstr "\nct:"
  pp ct
  pstr "ctEvidence ct:"
  pp evid
  pstr "etEvPred $ ctEvidence ct:"
  pp predty
  pstr "etEvTerm $ ctEvidence ct:"
  pp ter
  case ter of
    EvExpr inner -> do
      pstr "EvExpr"
      pp inner
    EvTypeable _ _ -> pstr "EvTypeable"
    EvFun{} -> pstr "EvFun"
  case classifyPredType predty of
    ClassPred klass typ -> do
      pstr "class pred"
      pstr "##########"
      pstr "klass:"
      pp klass
      pstr "klass name:"
      pp (className klass)
      pstr "klass occ name:"
      pp (occName $ className klass)
      if (occName $ className klass) /= mkClsOcc "Applicative"
        then pure Nothing
        else
          case typ of
            [f] -> do
              pstr "f:"
              pp f
              pure $ Nothing -- Just (, ct)
            _ -> pure Nothing
    EqPred _ _ _ -> do
      pstr "eq pred"
      pure $ Nothing -- Just (ctEvTerm $ ctEvidence ct, ct)
    IrredPred _ -> do
      pstr "irred pred"
      pure $ Nothing
    ForAllPred _ _ _ -> do
      pstr "forall pred"
      pure $ Nothing
  -- let ClassPred cls _types = classifyPredType . ctEvPred . ctEvidence $ ct
  -- if mkClsOcc "MonadIO" == occName (className cls)
  --   then pure $ Just (ctEvTerm . ctEvidence $ ct, ct)
  --   else pure Nothing
