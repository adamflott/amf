module AMF.Types.Common where

import           Relude

-- base
import           Text.Show                      ( Show(..) )

-- Hackage
import qualified Data.Aeson                    as Aeson
import qualified System.Posix                  as Posix
import           Codec.Serialise               as CBOR
import           System.FSNotify

newtype UnixSignal = UnixSignal Posix.Signal
                   deriving stock (Generic)
                   deriving anyclass (Serialise)

instance Show UnixSignal where
    show (UnixSignal sig) = if
        | sig == Posix.sigHUP    -> "HUP"
        | sig == Posix.sigINT    -> "INT"
        | sig == Posix.sigQUIT   -> "QUIT"
        | sig == Posix.sigILL    -> "ILL"
        | sig == Posix.sigTRAP   -> "TRAP"
        | sig == Posix.sigABRT   -> "ABRT"
        | sig == Posix.sigBUS    -> "BUS"
        | sig == Posix.sigFPE    -> "FPE"
        | sig == Posix.sigKILL   -> "KILL"
        | sig == Posix.sigUSR1   -> "USR1"
        | sig == Posix.sigSEGV   -> "SEGV"
        | sig == Posix.sigUSR2   -> "USR2"
        | sig == Posix.sigPIPE   -> "PIPE"
        | sig == Posix.sigALRM   -> "ALRM"
        | sig == Posix.sigTERM   -> "TERM"
        | sig == Posix.sigCHLD   -> "CHLD"
        | sig == Posix.sigCONT   -> "CONT"
        | sig == Posix.sigSTOP   -> "STOP"
        | sig == Posix.sigTSTP   -> "TSTP"
        | sig == Posix.sigTTIN   -> "TTIN"
        | sig == Posix.sigTTOU   -> "TTOU"
        | sig == Posix.sigURG    -> "URG"
        | sig == Posix.sigXCPU   -> "XCPU"
        | sig == Posix.sigXFSZ   -> "XFSZ"
        | sig == Posix.sigVTALRM -> "VTALRM"
        | sig == Posix.sigPROF   -> "PROF"
        | sig == Posix.sigPOLL   -> "POLL"
        | otherwise              -> "?"



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

type FSEvent = Event
