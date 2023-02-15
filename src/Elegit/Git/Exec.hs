module Elegit.Git.Exec where

import           Control.Monad.Catch  as MC
import           Data.Text            (stripEnd)
import           Elegit.Git.Action
import           Fmt
import           GHC.IO.Handle        (hFlush)
import           System.Process.Typed (ExitCode (ExitFailure, ExitSuccess), proc, readProcess)
import           Universum


newtype GitExecT m a
  = GitExecT { runGitExecT :: m a }

-- TODO: Improve code usability by creating a generic type-class which should have instance
-- for every possible command.
data GitCommand
  = GCCB GCurrentBranchData
  | GCBU GBranchUpstreamData
  | GCL GLogData
  | GCS GStatusData
  | GCSL GStashListData
  | GCRC GReadConfigData
  | GCSC GSetConfigData
  | GCUC GUnsetConfigData
  | GCATR GAliasesToRemoveData


-- TODO: cover with tests
renderGitCommand :: GitCommand -> Text
renderGitCommand (GCCB gc)  = renderGC gc
renderGitCommand (GCBU gc)  = renderGC gc
renderGitCommand (GCL gc)   = "-c color.ui=always "+|renderGC gc|+""
renderGitCommand (GCS gc)   = "-c color.status=always "+|renderGC gc|+""
renderGitCommand (GCSL gc)  = renderGC gc
renderGitCommand (GCRC gc)  = renderGC gc
renderGitCommand (GCSC gc)  = renderGC gc
renderGitCommand (GCUC gc)  = renderGC gc
renderGitCommand (GCATR gc) = renderGC gc


class Monad m => MonadGitExec m where
  execGit :: GitCommand -> m (Maybe Text)
  pText :: Text -> m ()
  pTextLn :: Text -> m ()
  gLine :: m Text

instance MonadIO m => MonadGitExec (GitExecT m) where
  execGit gc = do
    (eCode, outputBS, _errBS) <- readProcess $ proc "git" (toString <$> words (renderGitCommand gc))
    case eCode of
      -- TODO: Handle error codes per `gc`
      ExitFailure _ -> do
        return Nothing
      ExitSuccess -> do
        let output = stripEnd $ decodeUtf8 outputBS
        return $ if null output then Nothing else Just output

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
