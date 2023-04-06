module Elegit.Cli.Command where

import Universum

data ElegitCommand
  = ShowWorkCommand
  | AcquireRepositoryCommand
  | InitRepositoryCommand
  deriving (Eq, Show)
