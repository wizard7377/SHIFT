{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

{- |
Module      : Lift.Common.Module
License     : BSD-2-Clause
Maintainer  : Asher Frost
-}
module Lift.Common.Module (Module (..), Program (..), ParseState (..), buildModule, currentFile, programState, currentModule, loadedModules, modules, moduleName, tokens, assertions, proofs, tokens) where

import Control.Lens ((^..))
import Data.Text qualified as T
import Extra
import Lift.Common.Names
import Lift.Common.Tokens
import Rift qualified (Term, Theory (..), occurs)

-- | Represents the current state of parsing a program.
data ParseState t = ParseState
  { _currentFile :: FPath
  -- ^ The current file being parsed
  , _programState :: Program t
  -- ^ The program state containing all loaded modules
  }
  deriving (Eq, Ord, Data, Typeable, Generic)

deriving instance (Show (Program t)) => Show (ParseState t)

-- | Represents a complete program with multiple modules.
data Program t = Program
  { _currentModule :: Name
  -- ^ The name of the currently active module
  , _loadedModules :: [Name]
  -- ^ List of all modules that have been loaded
  , _modules :: Map Name (Module t)
  -- ^ Map of module names to their definitions
  }
  deriving (Eq, Ord, Data, Typeable, Generic)

deriving instance (Show (Module t)) => Show (Program t)

instance Default (Program t) where
  def =
    Program
      { _currentModule = Name "Main"
      , _loadedModules = []
      , _modules = mempty
      }

{- | A module represents a self-contained unit of code.

Modules are the primary organizational structure in the language,
containing definitions of tokens, assertions (theorems/axioms),
and proofs.
-}
data Module t = Module
  { _moduleName :: Name
  -- ^ The name of this module
  , _tokens :: Map Name (TokenValue t)
  -- ^ Map of token names to their values
  , _assertions :: TMap () Name t
  -- ^ Map of assertion names to their definitions and types
  , _proofs :: TMap Name t t
  -- ^ Map of proof names to their implementations and types
  }
  deriving (Eq, Ord, Data, Typeable, Generic)

makeLenses ''Module
makeLenses ''Program
makeLenses ''ParseState

{- | Implementation of the Theory typeclass for Module.

This allows modules to be used with the Rift term manipulation functions
by providing access to assertions and proofs.
-}
instance (Rift.Term t) => Rift.Theory (Module t) where
  type TermOf (Module t) = t
  type LabelOf (Module t) = Name
  defines = assertions
  proofs = proofs

{- | Creates a new empty module with the given name.

The module starts with empty collections of tokens, assertions, and proofs.
-}
buildModule :: Name -> Module t
buildModule name =
  Module
    { _moduleName = name
    , _tokens = mempty
    , _assertions = mempty
    , _proofs = mempty
    }

{- | Formats a module for display as Text.

This function creates a structured text representation of the module,
showing its name, tokens, assertions, and proofs.
-}
showModule :: (Show t) => Module t -> T.Text
showModule mod =
  T.concat
    [ "\n--Module-- " <> (T.pack . show) (mod ^. moduleName)
    , "\n\nTokens: \n" <> T.pack (show (mod ^. tokens))
    , "\n\nAssertions: \n" <> T.pack (show (mod ^. assertions))
    , "\n\nProofs: \n" <> T.pack (show (mod ^. proofs))
    ]

-- | Show instance for Module, uses the showModule function for text formatting.
instance (Show t) => Show (Module t) where
  show = T.unpack . showModule
