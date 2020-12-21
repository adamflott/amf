module AMF.Types.Common where

import Relude

-- Hackage
import qualified Data.Aeson                    as Aeson
import qualified System.Posix                  as Posix
import           Codec.Serialise               as CBOR


newtype UnixSignal = UnixSignal Posix.Signal
                   deriving stock (Generic, Show)
                   deriving anyclass (Serialise)

instance Aeson.ToJSON UnixSignal where
    toJSON (UnixSignal sig) = Aeson.Number (fromIntegral sig)


newtype HostName = HostName Text
                 deriving stock (Generic, Show)
                 deriving newtype IsString

newtype UserName = UserName Text
                 deriving stock (Generic, Show)
                 deriving newtype IsString

newtype ProcessId = ProcessId Posix.ProcessID
                  deriving stock (Generic, Show)
