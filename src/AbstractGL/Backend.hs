-- |
-- Module: AbstractGL.Backend
-- Copyright: (c) 2018 Patrik Sandahl
-- Licence: BSD3
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: non-portable
--
-- Entry module for the backend API. The backend API can only be called from the
-- thread owning the OpenGL context.
module AbstractGL.Backend
    ( AbstractGL (..)
    , create
    , pushRequest
    ) where

import           AbstractGL.Backend.Upload (Request)
import           Control.Concurrent.STM    (TQueue, atomically, newTQueueIO,
                                            writeTQueue)
import           Flow                      ((<|))

-- | The data type that is handle towards the backend. Visible as an opaque
-- type to users of AbstractGL.
data AbstractGL = AbstractGL
    { uploadQ :: !(TQueue Request)
    }

-- | Create the 'AbstractGL' instance.
create :: IO AbstractGL
create =
    AbstractGL <$> newTQueueIO

-- | Push a new request to the upload queue.
pushRequest :: AbstractGL -> Request -> IO ()
pushRequest abstractGL request =
    atomically <| writeTQueue (uploadQ abstractGL) request
