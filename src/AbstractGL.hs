-- |
-- Module: AbstractGL
-- Copyright: (c) 2018 Patrik Sandahl
-- Licence: BSD3
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: non-portable
--
-- Entry module for the user API. The user API can be called from any thread.
-- The AbstractGL library does not force any particular execution model and force
-- any specific library for low level handling of OpenGL context or events.
module AbstractGL
    ( AbstractGL
    , Shader (..)
    , Program
    , createProgram
    ) where

import           AbstractGL.Backend         (AbstractGL, pushRequest)
import           AbstractGL.Backend.Program (Program, Shader (..))
import           AbstractGL.Backend.Upload  (Request (..))
import           Control.Concurrent.STM     (atomically, newTQueueIO,
                                             readTQueue, writeTQueue)
import           Control.DeepSeq            (($!!))
import           Control.Exception          (SomeException, try)
import           Data.ByteString.Char8      (ByteString)
import qualified Data.ByteString.Char8      as BS
import           Flow                       ((<|))

-- | Create a shader program from files. Must be called from another thread
-- than the GL context owning thread.
createProgram :: AbstractGL -> [(Shader, FilePath)] -> IO (Either String Program)
createProgram abstractGL shaders = do
    result <- sequence <$> mapM readShaderSource shaders
    case result of
        Right shaders' -> do
            replyQ <- newTQueueIO
            pushRequest abstractGL <| CreateProgram replyQ $!! shaders'
            reply <- atomically <| readTQueue replyQ
            either (return . Left) (return . Right) reply

        Left err -> return <| Left err

readShaderSource :: (Shader, FilePath) -> IO (Either String (Shader, FilePath, ByteString))
readShaderSource (shader, path) = do
    result <- tryReadFile path
    either (return . Left . show) (\bs -> return <| Right (shader, path, bs)) result
    where
        tryReadFile :: FilePath -> IO (Either SomeException ByteString)
        tryReadFile = try . BS.readFile
