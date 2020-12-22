module AMF.Types.FileSystem where

-- prelude
import           Relude                  hiding ( readFile
                                                , writeFile
                                                )

-- base

-- Hackage
import qualified Data.ByteString.Lazy          as BSL
import           Path
import qualified Path.IO                       as PIO
import qualified System.IO.Utf8                as Utf8
import           Control.Exception.Safe        as X
                                                ( IOException
                                                , tryIO
                                                )
import           Control.Monad.Catch
import qualified System.Directory
import qualified System.IO


-- Read -------------------------------------------------------------------------

class Monad m => MonadFileSystemRead m where
    readFile :: Path b File -> m (Either IOException LByteString)
    listDirectory :: Path b Dir -> m (Either IOException ([Path Abs Dir], [Path Abs File]))

    default readFile :: (MonadTrans t, MonadFileSystemRead m', m ~ t m') => Path b File -> m (Either IOException LByteString)
    readFile = lift . readFile

    default listDirectory :: (MonadTrans t, MonadFileSystemRead m', m ~ t m') => Path b Dir -> m (Either IOException ([Path Abs Dir], [Path Abs File]))
    listDirectory = lift . listDirectory


    getHomeDirectory :: m (Path Abs Dir)
    default getHomeDirectory :: (MonadTrans t, MonadFileSystemRead m', m ~ t m') => m (Path Abs Dir)
    getHomeDirectory = lift getHomeDirectory

    getCurrentDirectory :: m (Path Abs Dir)
    default getCurrentDirectory :: (MonadTrans t, MonadFileSystemRead m', m ~ t m') => m (Path Abs Dir)
    getCurrentDirectory = lift getCurrentDirectory

    getTemporaryDirectory :: m (Path Abs Dir)
    default getTemporaryDirectory :: (MonadTrans t, MonadFileSystemRead m', m ~ t m') => m (Path Abs Dir)
    getTemporaryDirectory = lift getTemporaryDirectory



instance MonadFileSystemRead m => MonadFileSystemRead (ReaderT r m)
instance MonadFileSystemRead m => MonadFileSystemRead (StateT r m)
instance MonadFileSystemRead m => MonadFileSystemRead (ExceptT r m)


instance MonadFileSystemRead IO where
    readFile fn = tryIO (BSL.readFile (toFilePath fn))

    listDirectory dir = do
        r <- tryIO (PIO.listDir dir)
        case r of
            Left  e  -> pure (Left e)
            Right es -> pure (Right es)

    getHomeDirectory = do
        hd <- System.Directory.getHomeDirectory
        parseAbsDir hd

    getCurrentDirectory = do
        cwd <- System.Directory.getCurrentDirectory
        parseAbsDir cwd

    getTemporaryDirectory = do
        td <- System.Directory.getTemporaryDirectory
        parseAbsDir td

makeAbsolute :: (MonadThrow m, ToString a, MonadFileSystemRead m) => a -> m (Path Abs Dir)
makeAbsolute path = do
    b   <- parseSomeDir (toString path)
    cwd <- getCurrentDirectory
    let y = case b of
            Abs p -> p
            Rel p -> cwd </> p
    pure y

-- Write -----------------------------------------------------------------------


class MonadIO m => MonadFileSystemWrite m where
    openFile :: Path b File -> IOMode -> m (Either IOException Handle)
    closeFile :: Handle -> m (Maybe IOException)
    writeFile :: Path b File -> ByteString -> m (Either IOException Text)

    default openFile :: (MonadTrans t, MonadFileSystemWrite m', m ~ t m') => Path b File -> IOMode -> m (Either IOException Handle)
    openFile path mode = lift (openFile path mode)

    default closeFile :: (MonadTrans t, MonadFileSystemWrite m', m ~ t m') => Handle -> m (Maybe IOException)
    closeFile path = lift (closeFile path)

    default writeFile :: (MonadTrans t, MonadFileSystemWrite m', m ~ t m') => Path b File -> ByteString -> m (Either IOException Text)
    writeFile path contents = lift (writeFile path contents)

    createDirectory :: Path b Dir -> m (Either IOException ())
    default createDirectory :: (MonadTrans t, MonadFileSystemWrite m', m ~ t m') => Path b Dir -> m (Either IOException ())
    createDirectory = lift . createDirectory

    createDirIfMissing :: Bool -> Path b Dir -> m (Maybe IOException)
    default createDirIfMissing :: (MonadTrans t, MonadFileSystemWrite m', m ~ t m') => Bool -> Path b Dir -> m (Maybe IOException)
    createDirIfMissing create_parents path = lift $ createDirIfMissing create_parents path

-- TODO removeDirectoryRecursive :: FilePath -> IO ()

instance MonadFileSystemWrite m => MonadFileSystemWrite (ReaderT r m)
instance MonadFileSystemWrite m => MonadFileSystemWrite (StateT r m)
instance MonadFileSystemWrite m => MonadFileSystemWrite (ExceptT r m)

instance MonadFileSystemWrite IO where
    openFile path mode = tryIO (Utf8.openFile (toString (toFilePath path)) mode)

    closeFile fh = do
        r <- tryIO (System.IO.hClose fh)
        case r of
            Left  err -> pure (Just err)
            Right _   -> pure Nothing

    writeFile path contents = (pure (Right "write blah"))

    createDirectory dir = tryIO (PIO.createDirIfMissing True dir)

    createDirIfMissing create_parents dir = do
        r <- tryIO (PIO.createDirIfMissing create_parents dir)
        case r of
            Left err -> pure (Just err)
            _        -> pure Nothing

-- Read & Write -----------------------------------------------------------------

-- | A class of monads that can interact with the filesystem.
class (MonadFileSystemRead m, MonadFileSystemWrite m) => MonadFileSystem m

instance MonadFileSystem m => MonadFileSystem (ReaderT r m)
instance MonadFileSystem m => MonadFileSystem (StateT r m)
instance MonadFileSystem m => MonadFileSystem (ExceptT r m)

instance MonadFileSystem IO

--instance MonadFileSystem m => MonadFileSystem (LoggingT m)
--instance MonadFileSystem m => MonadFileSystem (StateT s m)
--instance (MonadFileSystem m, Monoid w) => MonadFileSystem (WriterT w m)
