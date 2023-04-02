{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TemplateHaskell    #-}
module Elegit.Git.Runner.Simulated where

import           Control.Monad.Free.Church
import           Control.Monad.HT            (until)
import           Control.Monad.Writer.Strict
import           Data.DList                  as DList
import qualified Data.HashMap.Strict         as HS
import qualified Data.Text                   as T
import qualified Elegit.Git.Action           as GA
import           Fmt
import           Lens.Micro
import           Lens.Micro.Mtl
import           Lens.Micro.TH
import           Universum                   as U hiding (Lens, Lens', Traversal', preuse, preview, use, view, (%~),
                                                   (.~), (^.), (^?))

-- | Describes all the metrics we collect from the git action execution
data GitCommand
  = UpdateConfigCommand Text Text
  | CloneRepositoryCommand Text
  | Prompt Text
  | PrintText Text
  deriving stock (Eq, Show)


type GConfig = HashMap Text Text

-- TODO: Add commit hash.
data GCommit
  = GCommit
      { _gcName    :: Text
      , _gcMessage :: Text
      }
  deriving (Eq, Show)

makeLenses ''GCommit


data GBranch
  = GBranch
      { _gbName     :: Text
      , _gbUpstream :: Maybe Text
      , _gbCommit   :: [GCommit]
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


newRepository :: GRepository
newRepository
  = GRepository
    { _grRemotes = []
    , _grBranches =
      [ GBranch
        { _gbName     = "master"
        , _gbUpstream = Nothing
        , _gbCommit   = []
        }
      ]
    , _grCurrentBranch = "master"
    , _grStashes = []
    , _grModifiedFiles = []
    , _grUnstagedFiles = []
    , _grConfig = mempty
    }



makeLenses ''GRepository


data InMemoryGit
  = IMGit
      { _gRepository :: Maybe GRepository
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
  GA.InitRepository _ next -> do
    gRepository .= Just newRepository
    return next
  GA.AddInitialCommit (GA.GInitialCommitData message) next -> do
    localRepository . branchWithName "master" . gbCommit %= (GCommit {_gcName = "Initial Commit", _gcMessage = message}:)
    return next

  GA.Show (GA.GShowData target) next -> do
    result <- runMaybeT $ case target of
      GA.ShowHead -> do
        currentBranchName <- MaybeT $ preuse $ localRepository . grCurrentBranch
        currentBranch <- MaybeT $ preuse $ localRepository . branchWithName currentBranchName
        GCommit {_gcName = name, _gcMessage = message} <- MaybeT $ pure $ currentBranch ^? gbCommit._head
        return $ [name, ""] ++ lines message
    return $ next $ fromMaybe [] result

  GA.CurrentBranch GA.GCurrentBranchData next -> do
    mCurrentBranchName <- preuse $ localRepository . grCurrentBranch
    return $ next mCurrentBranchName
  GA.BranchUpstream (GA.GBranchUpstreamData branch) next -> do
    branchM <- preuse $ localRepository . branchWithName branch
    return $ next (branchM >>= _gbUpstream)

  GA.Log (GA.GLogData lType base target) next -> do
    result <- runMaybeT $ case lType of
      GA.LogOneLine -> do
        baseBranch <- MaybeT $ preuse $ localRepository.branchWithName base
        targetBranch <- MaybeT $ preuse $ localRepository.branchWithName target
        baseBranchHead <- MaybeT $ pure $ baseBranch ^? gbCommit._head
        return $ view gcName <$> commitDifference baseBranchHead (targetBranch^.gbCommit)
    return $ next $ fromMaybe [] result

  GA.Status (GA.GStatusData sType) next -> do
    case sType of
      GA.StatusShort -> do
        modifiedFiles <- use $ localRepository.grModifiedFiles
        unstagedFiles <- use $ localRepository.grUnstagedFiles
        let
          modified :: [Text]
          modified = (\modifiedFile -> fmt "M "+|modifiedFile|+"") <$> modifiedFiles
          unstaged :: [Text]
          unstaged = (\unstagedFile -> fmt "?? "+|unstagedFile|+"") <$> unstagedFiles
        return $ next (modified <> unstaged)

  GA.StashList GA.GStashListData next -> do
    stashes <- use $ localRepository . grStashes
    return $ next
      [ fmt "stash@{"+||i||+"}: "+|(stash^.gsName)|+" on "+|(stash^.gsBranchName)|+"" | (i, stash) <- zip [(0 :: Int)..] stashes
      ] -- this is excessive, I guess? @teggotic

  GA.GPGListKeys (GA.GGPGKeyListData _email) next -> do
    -- Ideally we want to see this
    return $ next (Just ("3AA5C34371567BD2":|[]))

  GA.AliasesToRemove (GA.GAliasesToRemoveData cScope) next -> do
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

  GA.ReadConfig (GA.GReadConfigData cScope cName) next -> do
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

  GA.SetConfig (GA.GSetConfigData cScope cName cValue) next -> do
    case cScope of
     GA.LocalConfig -> do
      localConfig %= HS.insert cName cValue
     GA.GlobalConfig -> do
      globalConfig %= HS.insert cName cValue
     GA.AutoConfig  -> do
      localConfig %= HS.insert cName cValue
    return next

  GA.UnsetConfig (GA.GUnsetConfigData cScope cName) next -> do
    case cScope of
     GA.LocalConfig -> do
      localConfig %= HS.delete cName
     GA.GlobalConfig -> do
      globalConfig %= HS.delete cName
     GA.AutoConfig  -> do
      localConfig %= HS.delete cName
    return next

  GA.Prompt (GA.PromptConfig prompt pType) next -> do
    let
      -- TODO: Make configurable
      hardcodedAnswer :: Text
      hardcodedAnswer = "test"

      promptAnswer = do
        tell $ singleton $ Prompt (message <> hardcodedAnswer)
        return hardcodedAnswer

      message :: Text
      message =
        case pType of
          GA.PromptOneTime                 -> fmt ""+|prompt|+": "
          GA.PromptDefault (Just pDefault) -> fmt ""+|prompt|+" {"+|pDefault|+"}: "
          GA.PromptDefault Nothing         -> fmt ""+|prompt|+": "

    answer <- case pType of
        GA.PromptOneTime                 -> promptAnswer
        GA.PromptDefault Nothing         -> until (not . null) promptAnswer
        GA.PromptDefault (Just pDefault) -> do
         answer <- promptAnswer
         if null answer
           then return pDefault
           else return answer
    return $ next answer

  GA.PathToTool (GA.GPathToToolData toolName) next -> do
    return $ next $ Just ("/usr/bin/"+|toolName|+"")

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

localRepository :: Traversal' InMemoryGit GRepository
localRepository = gRepository . _Just

localConfig :: Traversal' InMemoryGit GConfig
localConfig = localRepository . grConfig

globalConfig :: Traversal' InMemoryGit GConfig
globalConfig = gConfig

branchWithName :: Text -> Traversal' GRepository GBranch
branchWithName name = grBranches . each . filtered (\b -> b ^. gbName == name)
