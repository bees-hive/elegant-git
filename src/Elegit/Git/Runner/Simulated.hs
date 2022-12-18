{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TemplateHaskell    #-}
module Elegit.Git.Runner.Simulated where

import           Control.Monad.Free.Church
import           Control.Monad.Writer.Strict
import           Data.DList                  as DList
import qualified Data.List.NonEmpty          as NE
import qualified Elegit.Git.Action           as GA
import           Fmt
import           Lens.Micro
import           Lens.Micro.Mtl
import           Lens.Micro.TH
import           Universum                   as U hiding (preuse, use, view, (%~), (^.))

-- | Describes all the metrics we collect from the git action execution
data GitCommand
  = UpdateConfigCommand Text Text
  | CloneRepositoryCommand Text
  | ReportInfo Text
  | PrintText Text
  deriving stock (Eq, Show)

-- TODO: Add commit hash.
newtype GCommit
  = GCommit { _gcName :: Text }
  deriving (Eq, Show)

makeLenses ''GCommit


data GBranch
  = GBranch
      { _gbName     :: Text
      , _gbUpstream :: Maybe Text
      , _gbCommit   :: NonEmpty GCommit
      }
  deriving (Eq, Show)

makeLenses ''GBranch


data GStash
  = GStash
      { _gsName       :: Text
      , _gsBranchName :: Text
      }
  deriving (Eq, Show)

makeLenses ''GStash


newtype GRemote
  = GRemote { _grmName :: Text }
  deriving (Eq, Show)

makeLenses ''GRemote


data GRepository
  = GRepository
      { _grRemotes       :: [GRemote]
      , _grBranches      :: [GBranch]
      , _grCurrentBranch :: Text
      , _grStashes       :: [GStash]
      , _grModifiedFiles :: [Text]
      , _grUnstagedFiles :: [Text]
      }
  deriving (Eq, Show)


-- greatestCommonAncestor :: [GCommit] -> [GCommit] -> Maybe GCommit
-- greatestCommonAncestor left right =



commitDifference :: GCommit -> [GCommit] -> [GCommit]
commitDifference _ [] = []
commitDifference bc (tc : tcs)
    | tc^.gcName == bc^.gcName = []
    | otherwise = tc: commitDifference bc tcs


makeLenses ''GRepository


-- | Collects a summary of execution as a list of `GitCommand`s.
runGitActionPure :: GRepository -> GA.FreeGit () -> (GRepository, [GitCommand])
runGitActionPure gr action =
    let
        (commands, repo) = runIdentity $ flip runStateT gr $ execWriterT $ foldF collectImpureCommandsF action
    in (repo, DList.toList commands)

-- | Interpreter of GitF which collects the summary of exection in `Writer` monad.
--
-- Each branch should return the value of type a, which can be obtained by calling
-- the `next` function.
--
-- We use `tell` function to store the command in `DList GitCommand`. `DList` can be treated
-- as just plain list (@[]@) but that has O(1) `append` operation instead of the O(n) of the @[]@.
--
-- Note that we are in the context of the `Writer` monad and we need to wrap the value `a` in
-- `Writer`. To lift any value into a monad you should use `return`.
collectImpureCommandsF :: (MonadState GRepository m, MonadWriter (DList GitCommand) m) => GA.GitF a -> m a
collectImpureCommandsF cmd = case cmd of
    GA.CurrentBranch next -> do
        currentBranchName <- use grCurrentBranch
        return $ next currentBranchName
    GA.BranchUpstream branch next -> do
        branches <- use grBranches
        return $ next (find (\b -> b^.gbName == branch) branches >>= _gbUpstream)

    GA.Log lType base target next -> do
        case lType of
            GA.LogOneLine -> do
                mBaseBranch <- preuse $ grBranches . each . filtered (\b -> b ^. gbName == base)
                mTargetBranch <- preuse $ grBranches . each . filtered (\b -> b ^. gbName == target)

                return $ next $ fromMaybe [] $ do
                    baseBranch <- mBaseBranch
                    targetBranch <- mTargetBranch
                    let baseBranchHead = baseBranch ^. gbCommit.to U.head
                    return $ view gcName <$> commitDifference baseBranchHead (NE.toList $ targetBranch^.gbCommit)

    GA.Status sType next -> do
        case sType of
            GA.StatusShort -> do
                modifiedFiles <- use grModifiedFiles
                unstagedFiles <- use grUnstagedFiles
                let
                    modified :: [Text]
                    modified = (\modifiedFile -> fmt "M "+|modifiedFile|+"") <$> modifiedFiles
                    unstaged :: [Text]
                    unstaged = (\unstagedFile -> fmt "?? "+|unstagedFile|+"") <$> unstagedFiles
                return $ next (modified <> unstaged)

    GA.StashList next -> do
        stashes <- use grStashes
        return $ next
            [ fmt "stash@{"+||i||+"}: "+|(stash^.gsName)|+" on "+|(stash^.gsBranchName)|+"" | (i, stash) <- zip [(0 :: Int)..] stashes
            ] -- this is excessive, I guess? @teggotic

    GA.ReportInfo content next -> do
        tell $ singleton $ ReportInfo content
        return next
    GA.PrintText content next -> do
        -- make each line a separate print to make it easier to write test cases
        tell $ DList.fromList (PrintText <$> lines content)
        return next
