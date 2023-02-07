{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TemplateHaskell    #-}
module Elegit.Git.Runner.Simulated where

import           Control.Monad.Free.Church
import           Control.Monad.HT            (until)
import           Control.Monad.Writer.Strict
import           Data.DList                  as DList
import qualified Data.HashMap.Strict         as HS
import qualified Data.List.NonEmpty          as NE
import qualified Data.Text                   as T
import qualified Elegit.Git.Action           as GA
import           Fmt
import           Lens.Micro
import           Lens.Micro.Mtl
import           Lens.Micro.TH
import           Universum                   as U hiding (Lens, Lens', Traversal', preuse, use, view, (%~), (^.))

-- | Describes all the metrics we collect from the git action execution
data GitCommand
  = UpdateConfigCommand Text Text
  | CloneRepositoryCommand Text
  | Prompt Text
  | PrintText Text
  deriving stock (Eq, Show)


type GConfig = HashMap Text Text

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
      , _grConfig        :: GConfig
      }
  deriving (Eq, Show)


makeLenses ''GRepository


data InMemoryGit
  = IMGit
      { _gRepository :: GRepository
      , _gConfig     :: GConfig
      }
  deriving (Eq, Show)


makeLenses ''InMemoryGit


commitDifference :: GCommit -> [GCommit] -> [GCommit]
commitDifference _ [] = []
commitDifference bc (tc : tcs)
    | tc^.gcName == bc^.gcName = []
    | otherwise = tc: commitDifference bc tcs


-- | Collects a summary of execution as a list of `GitCommand`s.
runGitActionPure :: InMemoryGit -> GA.FreeGit () -> (InMemoryGit, [GitCommand])
runGitActionPure imGit action =
  let
    (commands, imGit') = runIdentity $ flip runStateT imGit $ execWriterT $ foldF collectImpureCommandsF action
  in (imGit', DList.toList commands)


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
collectImpureCommandsF :: (MonadState InMemoryGit m, MonadWriter (DList GitCommand) m) => GA.GitF a -> m a
collectImpureCommandsF cmd = case cmd of
  GA.CurrentBranch next -> do
    currentBranchName <- use $ localRepository . grCurrentBranch
    return $ next currentBranchName
  GA.BranchUpstream branch next -> do
    branchM <- preuse $ localRepository . branchWithName branch
    return $ next (branchM >>= _gbUpstream)

  GA.Log lType base target next -> do
    case lType of
      GA.LogOneLine -> do
        mBaseBranch <- preuse $ localRepository . branchWithName base
        mTargetBranch <- preuse $ localRepository . branchWithName target

        return $ next $ fromMaybe [] $ do
          baseBranch <- mBaseBranch
          targetBranch <- mTargetBranch
          let baseBranchHead = baseBranch ^. gbCommit.to U.head
          return $ view gcName <$> commitDifference baseBranchHead (NE.toList $ targetBranch^.gbCommit)

  GA.Status sType next -> do
    case sType of
      GA.StatusShort -> do
        modifiedFiles <- use $ localRepository . grModifiedFiles
        unstagedFiles <- use $ localRepository . grUnstagedFiles
        let
          modified :: [Text]
          modified = (\modifiedFile -> fmt "M "+|modifiedFile|+"") <$> modifiedFiles
          unstaged :: [Text]
          unstaged = (\unstagedFile -> fmt "?? "+|unstagedFile|+"") <$> unstagedFiles
        return $ next (modified <> unstaged)

  GA.StashList next -> do
    stashes <- use $ localRepository . grStashes
    return $ next
      [ fmt "stash@{"+||i||+"}: "+|(stash^.gsName)|+" on "+|(stash^.gsBranchName)|+"" | (i, stash) <- zip [(0 :: Int)..] stashes
      ] -- this is excessive, I guess? @teggotic

  GA.AliasesToRemove cScope next -> do
    case cScope of
     GA.LocalConfig -> do
       cfg <- use $ localRepository . grConfig
       let aliases = sort $ keys $ HS.filterWithKey (\k v -> "alias." `T.isPrefixOf` k && "elegant " `T.isPrefixOf` v) cfg
       return $ next $ nonEmpty aliases
     GA.GlobalConfig -> do
       return $ next Nothing
     GA.AutoConfig ->
       -- TODO: Maybe work with local as default. Or narrow options
       error "scope AutoConfig is not supported by AliasesToRemove"

  GA.ReadConfig cScope cName next -> do
    localCfg <- use localConfig
    globalCfg <- use globalConfig
    let
      -- Note: Haskell is lazy and these will only be evaluated if needed depending on the branch
      localValue = HS.lookup cName localCfg
      globalValue = HS.lookup cName globalCfg
    case cScope of
          GA.LocalConfig  ->
            return $ next localValue
          GA.GlobalConfig ->
            return $ next globalValue
          GA.AutoConfig   ->
            return $ next $ localValue <|> globalValue

  GA.SetConfig cScope cName cValue next -> do
    case cScope of
     GA.LocalConfig -> do
      localConfig %= HS.insert cName cValue
     GA.GlobalConfig -> do
      globalConfig %= HS.insert cName cValue
     GA.AutoConfig  -> do
      localConfig %= HS.insert cName cValue
    return next

  GA.UnsetConfig cScope cName next -> do
    case cScope of
     GA.LocalConfig -> do
      localConfig %= HS.delete cName
     GA.GlobalConfig -> do
      globalConfig %= HS.delete cName
     GA.AutoConfig  -> do
      localConfig %= HS.delete cName
    return next

  GA.Prompt prompt pDefaultM next -> do
    let
      -- TODO: Make configurable
      hardcodedAnswer :: Text
      hardcodedAnswer = "test"

      promptAnswer = do
        tell $ singleton $ Prompt (message <> hardcodedAnswer)
        return hardcodedAnswer

      message :: Text
      message =
        case pDefaultM of
         Just pDefault -> fmt ""+|prompt|+" {"+|pDefault|+"}: "
         Nothing       -> fmt ""+|prompt|+": "

    answer <- case pDefaultM of
       Nothing -> until (not . null) promptAnswer
       Just pDefault -> do
         answer <- promptAnswer
         if null answer
           then return pDefault
           else return answer
    return $ next answer

  GA.FormatInfo content next -> do
    return $ next content
  GA.FormatCommand content next -> do
    return $ next ("==>> " <> content)
  GA.PrintText content next -> do
    -- make each line a separate print to make it easier to write test cases
    tell $ if null content
         then singleton $ PrintText ""
         else DList.fromList (PrintText <$> lines content)
    return next

localRepository :: Lens' InMemoryGit GRepository
localRepository = gRepository

localConfig :: Lens' InMemoryGit GConfig
localConfig = localRepository . grConfig

globalConfig :: Lens' InMemoryGit GConfig
globalConfig = gConfig

branchWithName :: Text -> Traversal' GRepository GBranch
branchWithName name = grBranches . each . filtered (\b -> b ^. gbName == name)
