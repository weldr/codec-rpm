{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import           Conduit(awaitForever, sinkFile, sourceLazy, stdinC)
import           Control.Monad(void)
import           Control.Monad.Except(MonadError, runExceptT, throwError)
import           Control.Monad.IO.Class(liftIO)
import           Control.Monad.Trans.Resource(runResourceT)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import           Data.CPIO(Entry(..), isEntryDirectory, readCPIO)
import           Data.Conduit(($$), (=$=), Conduit, Producer, yield)
import           Data.Conduit.Combinators(mapM_)
import           Data.Conduit.Lzma(decompress)
import           Prelude hiding(mapM_)
import           System.Directory(createDirectoryIfMissing)
import           System.FilePath((</>), splitFileName)

import RPM.Parse(parseRPMC)
import RPM.Types(RPM(..), RPMMonad)

-- Grab an RPM from stdin and convert it into a chunked conduit of ByteStrings.  This could
-- just as easily come from a file (using sourceFile) or over the network (see httpSink in
-- the http-conduit package, for instance).
--
-- Important note:  stdinC produces a conduit of multiple ByteStrings.  A single ByteString
-- is some chunk of the file - I don't know how much, but it feels like maybe 32k.  conduit calls
-- this chunked data, and while chunks are more efficient for conduit to work with, they don't do
-- much for us.  To examine the RPM header, we need individual bytes.  Luckily, the *CE functions
-- out of conduit let us get at individual elements out of the stream of chunks without even
-- having to think about a chunk or asking for more.  That all happens for us.
--
-- Long story short, chunks are useful for when we need to just pipe lots of data from one place
-- to another or when we need to tell conduit about types.  Elements are useful for when we need
-- to do something to the contents.
getRPM :: Producer RPMMonad BS.ByteString
getRPM =
    stdinC

-- If a cpio entry is a directory, just create it.  If it's a file, create the directory containing
-- it and then stream the contents onto disk.  I'm not worrying with permissions, file mode, timestamps,
-- or any of that stuff here.  This is just a demo.
writeCpioEntry :: Entry -> IO ()
writeCpioEntry entry@Entry{..} | isEntryDirectory entry = createDirectoryIfMissing True (BC.unpack cpioFileName)
                               | otherwise              = do
    let (d, f) = splitFileName (BC.unpack cpioFileName)
    createDirectoryIfMissing True d
    void $ runResourceT $ sourceLazy cpioFileData $$ sinkFile (d </> f)

-- parseRPMC returns an Either String RPM, because it's the result of a parse and could therefore have an
-- error.  In the error case, use throwError to propagate it up.  In the success case, just grab the
-- RPM payload out of the record and stick that back into the conduit.
payloadC :: MonadError e m => Conduit (Either e RPM) m BS.ByteString
payloadC =
    awaitForever $ \case
        Left err      -> throwError err
        Right RPM{..} -> yield rpmArchive

main :: IO ()
main = do
    -- Hopefully self-explanatory - runErrorT and runResourceT execute and unwrap the monads, giving the
    -- actual result of the whole conduit.  That's either an error message nothing, and the files are
    -- written out in the pipeline kind of as a side effect.
    result <- runExceptT $ runResourceT $
            getRPM
        =$= parseRPMC
        =$= payloadC
        =$= decompress Nothing
        =$= readCPIO
        $$  mapM_ (liftIO . writeCpioEntry)

    case result of
        Left e  -> print e
        Right _ -> return ()
