-- |
-- Module: AbstractGL.Backend.Types
-- Copyright: (c) 2018 Patrik Sandahl
-- Licence: BSD3
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: non-portable
module AbstractGL.Backend.Types
    ( ToGLenum (..)
    ) where

import qualified Graphics.GL as GL

-- | Typeclass for conversion to GLenum.
class ToGLenum a where
    toGLenum :: a -> GL.GLenum
