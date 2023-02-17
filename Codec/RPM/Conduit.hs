{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module: Codec.RPM.Conduit
-- Copyright: (c) 2016-2017 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: stable
-- Portability: portable
--
-- A module for interacting with an 'RPM' record using conduits.

module Codec.RPM.Conduit(parseRPMC,
                         payloadC,
                         payloadContentsC)
 where

import           Control.Monad.Catch(MonadThrow)
import           Control.Monad.Except(MonadError, throwError)
import           Control.Monad.Trans.Resource(MonadResource)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import           Data.Conduit((.|), ConduitT, awaitForever, yield)
import           Data.Conduit.Attoparsec(ParseError, conduitParserEither)
import           Data.Conduit.Lzma(decompress)
import           Data.CPIO(Entry, readCPIO)

import Codec.RPM.Parse(parseRPM)
import Codec.RPM.Types(RPM(..))

-- | Like 'parseRPM', but puts the result into a 'Conduit' as an 'Either', containing either a
-- 'ParseError' or an 'RPM'.  The result can be extracted with 'Control.Monad.Except.runExceptT',
-- like so:
--
-- > import Conduit((.|), runConduitRes, sourceFile)
-- > import Control.Monad.Except(runExceptT)
-- > result <- runExceptT $ runConduitRes $ sourceFile "some.rpm" .| parseRPMC .| someConsumer
--
-- On success, the 'RPM' record will be passed down the conduit for futher processing or
-- consumption.  On error, the rest of the conduit will be skipped and the 'ParseError' will
-- be returned as the result to be dealt with.
parseRPMC :: MonadError ParseError m => ConduitT C.ByteString RPM m ()
parseRPMC =
    conduitParserEither parseRPM .| consumer
 where
    consumer = awaitForever $ either throwError (yield . snd)

-- | Extract the package payload from an 'RPM', returning it in the conduit.
payloadC :: Monad m => ConduitT RPM BS.ByteString m ()
payloadC = awaitForever (yield . rpmArchive)

-- | Extract the package payload from an 'RPM', decompress it, and return each element of
-- the payload as a 'Data.CPIO.Entry'.
payloadContentsC :: (MonadResource m, MonadThrow m) => ConduitT RPM Entry m ()
payloadContentsC = payloadC
                .| decompress Nothing
                .| readCPIO
