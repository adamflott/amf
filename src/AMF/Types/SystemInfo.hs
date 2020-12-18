module AMF.Types.SystemInfo
    ( User(..)
    , StatGrabInfo(..)
    , SystemInfo(..)
    , ResourceLimits(..)
    , HasSystemInfo(..)
    ) where

-- prelude
import           Relude

-- base
import           Data.Version                   ( Version )
import           Text.Show                      ( Show(..) )

-- Hackage
import           Control.Lens
import           Path
import qualified System.Statgrab               as Statgrab
import qualified System.Posix                  as Posix
import qualified System.Posix.Resource         as PR


data User = User
    { _userLoginName     :: Text
    , _userID            :: Posix.UserID
    , _userGroupID       :: Posix.GroupID
    , _userName          :: Text
    , _userHomeDirectory :: Maybe (Path Abs Dir)
    , _userShell         :: Maybe (Path Abs File)
    }
    deriving stock Show

makeClassy ''User

data StatGrabInfo = StatGrabInfo
    { _statGrabInfoHostInfo   :: !Statgrab.Host
    , _statGrabInfoHostMemory :: !Statgrab.Memory
    , _statGrabInfoHostFS     :: ![Statgrab.FileSystem]
    , _statGrabInfoHostNet    :: ![Statgrab.NetworkInterface]
    , _statGrabInfoUser       :: !User
    }
    deriving stock Show

makeClassy ''StatGrabInfo


data ResourceLimits = ResourceLimits
    { _resourceLimitsCoreFileSize :: (PR.ResourceLimit, PR.ResourceLimit)
    , _resourceLimitsCPUTime      :: (PR.ResourceLimit, PR.ResourceLimit)
    , _resourceLimitsDataSize     :: (PR.ResourceLimit, PR.ResourceLimit)
    , _resourceLimitsFileSize     :: (PR.ResourceLimit, PR.ResourceLimit)
    , _resourceLimitsOpenFiles    :: (PR.ResourceLimit, PR.ResourceLimit)
    , _resourceLimitsStackSize    :: (PR.ResourceLimit, PR.ResourceLimit)
    , _resourceLimitsTotalMemory  :: (PR.ResourceLimit, PR.ResourceLimit)
    }

instance Show ResourceLimits where
    show rl = mconcat
        [ "core: " <> showRLimits (_resourceLimitsCoreFileSize rl)
        , "cpu time: " <> showRLimits (_resourceLimitsCPUTime rl)
        , "data size: " <> showRLimits (_resourceLimitsDataSize rl)
        , "file size: " <> showRLimits (_resourceLimitsFileSize rl)
        , "open files: " <> showRLimits (_resourceLimitsOpenFiles rl)
        , "stack size: " <> showRLimits (_resourceLimitsStackSize rl)
        , "total memory: " <> showRLimits (_resourceLimitsTotalMemory rl)
        ]
      where
        showRLimits :: (Semigroup a, IsString a) => (PR.ResourceLimit, PR.ResourceLimit) -> a
        showRLimits (h, s) = "hard:" <> showRLimit h <> ", soft:" <> showRLimit s <> "; "

        showRLimit :: IsString a => PR.ResourceLimit -> a
        showRLimit PR.ResourceLimitInfinity = "infinity"
        showRLimit PR.ResourceLimitUnknown  = "unknown"
        showRLimit (PR.ResourceLimit other) = Relude.show other

makeClassy ''ResourceLimits

data SystemInfo = SystemInfo
    { _systemInfoSystemOS        :: !String
    , _systemInfoSystemArch      :: !String
    , _systemInfoCompilerName    :: !String
    , _systemInfoCompilerVersion :: !Version
    , _systemInfoStats           :: !StatGrabInfo
    , _systemInfoResourceLimits  :: !ResourceLimits
    }
    deriving stock Show

makeClassy ''SystemInfo
