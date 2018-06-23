-- |
-- Module: AbstractGL.Backend.Upload
-- Copyright: (c) 2018 Patrik Sandahl
-- Licence: BSD3
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: non-portable
module AbstractGL.Backend.Upload
    ( Request (..)
    ) where

import           AbstractGL.Backend.Program (Program, Shader)
import           Control.Concurrent.STM     (TQueue)
import           Data.ByteString.Char8      (ByteString)

-- | Upload requests.
data Request =
    CreateProgram !(TQueue (Either String Program)) ![(Shader, FilePath, ByteString)]
