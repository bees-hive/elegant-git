module Elegit.Cli.Command where

import           Universum

data ElegitCommand
  = ShowWorkCommand
  | AcquireRepositoryCommand
  deriving (Eq, Show)
