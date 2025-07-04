{-# LANGUAGE GHC2021 #-}
-- \|
-- Module      : Extra
-- License     : BSD-2-Clause
-- Maintainer  : Asher Frost
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

-- \|
-- Module: Extra

-- \|
-- Module      : Extra
-- License     : BSD-2-Clause
-- Maintainer  : Asher Frost

{- |
Module      : Extra
License     : BSD-2-Clause
Maintainer  : Asher Frost
-}
module Extra (
  module Extra.Ghci,
  module Extra.Exports,
  module Extra.Lens,
  module Extra.List,
  module Extra.Types,
  module Extra.Choice,
  module Extra.Basics,
  module Extra.Ops,
  module Extra.Debug,
  module Extra.Tuple,
  module Extra.Map,
  module Extra.Todo,
  addColor,
) where

import Extra.Basics hiding (split)
import Extra.Choice
import Extra.Color
import Extra.Debug
import Extra.Exports
import Extra.Ghci
import Extra.Lens
import Extra.List
import Extra.Map
import Extra.Ops
import Extra.Todo
import Extra.Tuple
import Extra.Types hiding (flip)
