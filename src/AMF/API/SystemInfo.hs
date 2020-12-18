module AMF.API.SystemInfo where

import           Relude

-- base
import           System.Info

-- Hackage
import qualified Network.HostName
import qualified System.Posix.Resource         as PR
import qualified System.Posix.User             as User
import qualified System.Statgrab               as Statgrab
import           Path

-- local
import           AMF.Types.SystemInfo



newSystemInfo :: MonadIO m => m SystemInfo
newSystemInfo = do
    stat_info <- getStatGrabInfo
    rls       <- getAllResourceLimits
    pure (SystemInfo System.Info.os System.Info.arch System.Info.compilerName System.Info.compilerVersion stat_info rls)

getLoginName :: MonadIO m => m Text
getLoginName = fmap toText (liftIO User.getLoginName)

getHostName :: MonadIO m => m Text
getHostName = fmap toText (liftIO Network.HostName.getHostName)

getStatGrabInfo :: MonadIO m => m StatGrabInfo
getStatGrabInfo =
    StatGrabInfo
        <$> Statgrab.runStats Statgrab.snapshot
        <*> Statgrab.runStats Statgrab.snapshot
        <*> Statgrab.runStats Statgrab.snapshots
        <*> Statgrab.runStats Statgrab.snapshots
        <*> getUser

getUser :: MonadIO m => m User
getUser = do
    uid <- liftIO User.getRealUserID
    ue  <- liftIO (User.getUserEntryForID uid)

    pure
        (User (toText (User.userName ue))
              (User.userID ue)
              (User.userGroupID ue)
              (toText (User.userGecos ue))
              (parseAbsDir (User.homeDirectory ue))
              (parseAbsFile (User.userShell ue))
        )

getSystemFileSystemStats :: MonadIO m => m [Statgrab.FileSystem]
getSystemFileSystemStats = Statgrab.runStats Statgrab.snapshots

getSystemMemoryStats :: MonadIO m => m Statgrab.Memory
getSystemMemoryStats = Statgrab.runStats Statgrab.snapshot


getAllResourceLimits :: MonadIO m => m ResourceLimits
getAllResourceLimits =
    ResourceLimits
        <$> getRL PR.ResourceCoreFileSize
        <*> getRL PR.ResourceCPUTime
        <*> getRL PR.ResourceDataSize
        <*> getRL PR.ResourceFileSize
        <*> getRL PR.ResourceOpenFiles
        <*> getRL PR.ResourceStackSize
        <*> getRL PR.ResourceTotalMemory
  where
    getRL rl = do
        l <- liftIO (PR.getResourceLimit rl)
        pure (PR.hardLimit l, PR.softLimit l)
