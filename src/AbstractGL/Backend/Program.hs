{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
-- |
-- Module: AbstractGL.Backend.Program
-- Copyright: (c) 2018 Patrik Sandahl
-- Licence: BSD3
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: non-portable
module AbstractGL.Backend.Program
    ( Program
    , Shader (..)
    , create
    ) where

import           AbstractGL.Backend.Types (ToGLenum (..))
import           Control.DeepSeq          (NFData)
import           Data.ByteString.Char8    (ByteString)
import qualified Data.ByteString.Char8    as BS
import           Flow                     ((<|))
import           Foreign                  (Ptr, nullPtr, peek, with)
import           Foreign.C                (peekCString, withCString)
import           GHC.Generics             (Generic)
import qualified Graphics.GL              as GL
import           Text.Printf              (printf)

-- | Data type representing a shader program.
newtype Program = Program GL.GLuint
    deriving (Generic, NFData)

-- | Shader types.
data Shader
    = Fragment
    | Vertex
    deriving (Generic, NFData)

instance ToGLenum Shader where
    toGLenum Fragment = GL.GL_FRAGMENT_SHADER
    toGLenum Vertex   = GL.GL_VERTEX_SHADER

-- | Create a shader program.
create :: [(Shader, FilePath, ByteString)] -> IO (Either String Program)
create shaders = do
    compileResult <- sequence <$> mapM compileShader shaders
    case compileResult of
        Right shaders' -> do
            programResult <- linkShaders shaders'
            case programResult of
                Right programId -> return <| Right (Program programId)
                Left err        -> return <| Left err

        Left err       -> return <| Left err

compileShader :: (Shader, FilePath, ByteString) -> IO (Either String GL.GLuint)
compileShader (shader, path, source) = do
    shaderId <- GL.glCreateShader <| toGLenum shader
    setShaderSource shaderId source
    GL.glCompileShader shaderId

    status <- getShaderStatus <| GL.glGetShaderiv shaderId GL.GL_COMPILE_STATUS
    if status == GL.GL_TRUE
        then return <| Right shaderId
        else do
            errLog <- getInfoLog shaderId GL.glGetShaderInfoLog
            return <| Left (printf "%s: %s" path errLog)

linkShaders :: [GL.GLuint] -> IO (Either String GL.GLuint)
linkShaders shaderIds = do
    programId <- GL.glCreateProgram
    mapM_ (GL.glAttachShader programId) shaderIds
    GL.glLinkProgram programId

    status <- getShaderStatus <| GL.glGetProgramiv programId GL.GL_LINK_STATUS
    if status == GL.GL_TRUE
        then do
            mapM_ (GL.glDetachShader programId) shaderIds
            mapM_ GL.glDeleteShader shaderIds
            return <| Right programId
        else do
            errLog <- getInfoLog programId GL.glGetProgramInfoLog
            mapM_ GL.glDeleteShader shaderIds
            GL.glDeleteProgram programId
            return <| Left errLog

setShaderSource :: GL.GLuint -> ByteString -> IO ()
setShaderSource shaderId source =
    BS.useAsCString source $ \cstring ->
        with cstring $ \ptr ->
            GL.glShaderSource shaderId 1 ptr nullPtr

getShaderStatus :: (Ptr GL.GLint -> IO ()) -> IO GL.GLboolean
getShaderStatus getter =
    with 0 $ \ptr -> do
        getter ptr
        v <- peek ptr
        if v == 0
            then return GL.GL_FALSE
            else return GL.GL_TRUE

getInfoLog :: GL.GLuint
              -> (GL.GLuint -> GL.GLsizei -> Ptr GL.GLsizei -> Ptr GL.GLchar -> IO ())
              -> IO String
getInfoLog handle getter = do
    let str = replicate 500 '\0'
    withCString str $ \ptr -> do
        getter handle 500 nullPtr ptr
        peekCString ptr
