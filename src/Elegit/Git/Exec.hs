{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Elegit.Git.Exec where

import           Control.Monad.Catch  as MC
import qualified Data.Text            as T
import           Data.Text.Lazy       (stripEnd)
import           Elegit.Git.Action
import           GHC.IO.Handle        (hFlush)
import           System.IO.Error      (IOError)
import           System.Process.Typed (ExitCode (ExitSuccess), ProcessConfig, proc, readProcess, shell)
import           Universum            as U

procCmd :: Text -> [Text] -> ProcessConfig () () ()
procCmd tName args = proc (toString tName) (toString <$> args)


shellCmd :: Text -> [Text] -> ProcessConfig () () ()
shellCmd tName args = shell $ toString $ T.intercalate " " (tName:args)


class ExecutableCommand a where
  type ExecutableCommandResult a :: Type

  cmdExecArgs :: a -> [Text]
  default cmdExecArgs :: RenderGitCommand a => a -> [Text]
  cmdExecArgs = commandArgs

  cmdExecToolName :: a -> Text
  default cmdExecToolName :: RenderGitCommand a => a -> Text
  cmdExecToolName = toolName

  toProc :: a -> Text -> [Text] -> ProcessConfig () () ()

  readGitOutput :: a -> (ExitCode, LText, LText) -> ExecutableCommandResult a

instance ExecutableCommand GCurrentBranchData where
  type ExecutableCommandResult GCurrentBranchData = Maybe Text

  toProc _ = procCmd

  readGitOutput _ (ExitSuccess, gOut, _) = pure $ toStrict gOut
  readGitOutput _ _                      = Nothing

instance ExecutableCommand GBranchUpstreamData where
  type ExecutableCommandResult GBranchUpstreamData = Maybe Text

  toProc _ = procCmd

  readGitOutput _ (ExitSuccess, gOut, _) = pure $ toStrict gOut
  readGitOutput _ _                      = Nothing

instance ExecutableCommand GLogData where
  type ExecutableCommandResult GLogData = Maybe [Text]

  cmdExecArgs gc = "-c":"color.ui=always":commandArgs gc

  toProc _ = procCmd

  readGitOutput _ (ExitSuccess, gOut, _) = pure $ lines $ toStrict gOut
  readGitOutput _ _                      = Nothing

instance ExecutableCommand GStatusData where
  type ExecutableCommandResult GStatusData = Maybe [Text]

  cmdExecArgs gc = "-c":"color.status=always":commandArgs gc

  toProc _ = procCmd

  readGitOutput _ (ExitSuccess, gOut, _) = pure $ lines $ toStrict gOut
  readGitOutput _ _                      = Nothing

instance ExecutableCommand GStashListData where
  type ExecutableCommandResult GStashListData = Maybe [Text]

  toProc _ = procCmd

  readGitOutput _ (ExitSuccess, gOut, _) = pure $ lines $ toStrict gOut
  readGitOutput _ _                      = Nothing

instance ExecutableCommand GGPGKeyListData where
  type ExecutableCommandResult GGPGKeyListData = Maybe (NonEmpty Text)

  toProc _ = procCmd

  readGitOutput _ (ExitSuccess, gOut, _) = nonEmpty . lines $ toStrict gOut
  readGitOutput _ _                      = Nothing

instance ExecutableCommand GAliasesToRemoveData where
  type ExecutableCommandResult GAliasesToRemoveData = Maybe (NonEmpty Text)

  toProc _ = procCmd

  readGitOutput _ (ExitSuccess, gOut, _) = nonEmpty . lines $ toStrict gOut
  readGitOutput _ _                      = Nothing

instance ExecutableCommand GReadConfigData where
  type ExecutableCommandResult GReadConfigData = Maybe Text

  toProc _ = procCmd

  readGitOutput _ (ExitSuccess, gOut, _) = pure $ toStrict gOut
  readGitOutput _ _                      = Nothing

instance ExecutableCommand GSetConfigData where
  type ExecutableCommandResult GSetConfigData = Maybe ()

  toProc _ = procCmd

  readGitOutput _ (ExitSuccess, _, _) = pass
  readGitOutput _ _                   = Nothing

instance ExecutableCommand GUnsetConfigData where
  type ExecutableCommandResult GUnsetConfigData = Maybe ()

  toProc _ = procCmd

  readGitOutput _ (ExitSuccess, _, _) = pass
  readGitOutput _ _                   = Nothing

instance ExecutableCommand GPathToToolData where
  type ExecutableCommandResult GPathToToolData = Maybe Text

  toProc _ = shellCmd

  readGitOutput _ (ExitSuccess, gOut, _) = pure $ toStrict gOut
  readGitOutput _ _                      = Nothing


class Monad m => MonadGitExec m where
  execGit :: (ExecutableCommand a) => a -> m (ExecutableCommandResult a)
  pText :: Text -> m ()
  pTextLn :: Text -> m ()
  gLine :: m Text


newtype GitExecT m a
  = GitExecT { runGitExecT :: m a }


instance (MonadCatch m, MonadIO m) => MonadGitExec (GitExecT m) where
  execGit gc = do
    let
      cmdArgs = cmdExecArgs gc
      cmdTool = cmdExecToolName gc
    flip U.catch (\(e :: IOError) -> bug e) $ do
      (eCode, stdoutBS, stderrBS) <- readProcess $ toProc gc cmdTool cmdArgs
      return $
        readGitOutput gc
          ( eCode
          , stripEnd $ decodeUtf8 stdoutBS
          , stripEnd $ decodeUtf8 stderrBS
          )

  pText t = do
    putText t
    liftIO $ hFlush stdout

  pTextLn = putTextLn

  gLine = getLine

{- This is the boilerplate required by mtl to define custom monad transformer
 - Theoretically, mtl is the fastest library. However, it's too generic which results
 - in a huge amount of boilerplate, which is not ideal. This is also, not a full list type-classes
 - we need to support. However, for now, this is enough.
 - There are some more drawbacks to mtl, but they are not relevant yet.

 - There are many alternatives to mtl, e.g. `fused-effects` or `polisemy`, and in theory it could be better
 - to start using them instead of mtl. Custom effects are much easier to define, and don't require to
 - support every new transformer in the stack.
 -}

liftGitExecT :: m a -> GitExecT m a
liftGitExecT = GitExecT
{-# INLINE liftGitExecT #-}

mapReaderT :: (m a -> n b) -> GitExecT m a -> GitExecT n b
mapReaderT f m = GitExecT $ f $ runGitExecT m
{-# INLINE mapReaderT #-}

instance Functor m => Functor (GitExecT m) where
  fmap f = mapReaderT (fmap f)
  {-# INLINE fmap #-}

instance Applicative m => Applicative (GitExecT m) where
  pure = liftGitExecT . pure
  {-# INLINE pure #-}
  f <*> v = GitExecT $ runGitExecT f <*> runGitExecT v
  {-# INLINE (<*>) #-}

instance Monad m => Monad (GitExecT m) where
  return = pure
  {-# INLINE return #-}
  m >>= f = GitExecT $ do
    a <- runGitExecT m
    runGitExecT (f a)
  {-# INLINE (>>=) #-}

instance MonadTrans GitExecT where
  lift = liftGitExecT
  {-# INLINE lift #-}

instance MonadThrow m => MonadThrow (GitExecT m) where
  throwM e = lift $ MC.throwM e
  {-# INLINE throwM #-}

instance MonadCatch m => MonadCatch (GitExecT m) where
  catch (GitExecT m) c = GitExecT $ m `MC.catch` \e -> runGitExecT (c e)
  {-# INLINE catch #-}

instance (MonadIO m) => MonadIO (GitExecT m) where
  liftIO = liftGitExecT . liftIO
  {-# INLINE liftIO #-}
