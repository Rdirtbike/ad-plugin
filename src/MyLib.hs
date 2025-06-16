module MyLib (plugin, Diff (..)) where

import Data.Data
import Data.IORef
import Data.Maybe
import GHC.Plugins
import GHC.Types.Avail

newtype Diff = Diff String deriving (Typeable, Data)

plugin :: Plugin
plugin = defaultPlugin {installCoreToDos = install, pluginRecompile = purePlugin}

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todos = pure $ CoreDoPluginPass "Add greeting" pass : todos

pass :: ModGuts -> CoreM ModGuts
pass modGuts = do
  (_, anns) <- getFirstAnnotations deserializeWithData modGuts
  (newAvails, newBinds) <- fmap unzip $ sequence $ mapMaybe (modify anns) $ mg_binds modGuts
  pure
    modGuts
      { mg_exports = newAvails ++ mg_exports modGuts,
        mg_binds = newBinds ++ mg_binds modGuts
      }

modify :: NameEnv Diff -> CoreBind -> Maybe (CoreM (AvailInfo, CoreBind))
modify anns oldBind@(NonRec x f) = do
  Diff newName <- anns `lookupNameEnv` idName x
  Just $ do
    putMsg $ ppr oldBind
    uniq <- getUniqueM
    mod' <- getModule
    env <- liftIO $ newIORef emptyVarEnv
    f' <- autodiff env f
    let name = mkExternalName uniq mod' (mkVarOcc newName) generatedSrcSpan
        bind = NonRec (mkExportedVanillaId name $ mkFunctionType ManyTy doubleTy doubleTy) f'
    putMsg $ ppr bind
    pure (Avail name, bind)
modify _ _ = error "Can't handle recursive bindings yet."

autodiff :: IORef (VarEnv Var) -> CoreExpr -> CoreM CoreExpr
autodiff env (Var v) = do
  e <- liftIO $ readIORef env
  case lookupVarEnv e v of
    Just v' -> pure $ Var v'
    Nothing -> error "Not implemented yet, var"
autodiff _ i@(Lit (LitNumber t _)) = pure $ mkCoreUnboxedTuple [i, Lit $ LitNumber t 0]
autodiff _ x@(Lit (LitFloat _)) = pure $ mkCoreTup [x, Lit $ LitFloat 0]
autodiff _ x@(Lit (LitDouble _)) = pure $ mkCoreTup [x, Lit $ LitDouble 0]
autodiff env (App f x) = App <$> autodiff env f <*> autodiff env x
autodiff env (Lam b x) = do
  liftIO $ atomicModifyIORef env $ \e -> (extendVarEnv e b b, ())
  Lam b <$> autodiff env x
autodiff env (Let (NonRec b x) y) = do
  liftIO $ atomicModifyIORef' env $ \e -> (extendVarEnv e b b, ())
  x' <- autodiff env x
  Let (NonRec b x') <$> autodiff env y
autodiff _ _ = error "Not implemented yet"
