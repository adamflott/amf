module AMF.Types.SystemInfo
    ( StatGrabInfo(..)
    , SystemInfo(..)
    ) where

-- prelude
import           Relude

-- base
import           Data.Version                   ( Version )

-- Hackage
import           System.Statgrab


data StatGrabInfo = StatGrabInfo
    { hostInfo   :: !Host
    , hostMemory :: !Memory
    , hostFS     :: ![FileSystem]
    , hostNet    :: ![NetworkInterface] -- ,
    -- user :: !User
    }
    deriving stock Show

data SystemInfo = SystemInfo
    { systemOS        :: !String
    , systemArch      :: !String
    , compilerName    :: !String
    , compilerVersion :: !Version
    , stats           :: !StatGrabInfo
    }
    deriving stock Show
