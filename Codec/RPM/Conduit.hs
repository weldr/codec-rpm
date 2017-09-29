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

import           Control.Monad.Except(MonadError, throwError)
import           Control.Monad.Trans.Resource(MonadResource)
import           Conduit((.|), Conduit, awaitForever, yield)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import           Data.Conduit.Attoparsec(ParseError(..), conduitParserEither)
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
parseRPMC :: MonadError String m => Conduit C.ByteString m RPM
parseRPMC =
    conduitParserEither parseRPM .| consumer
 where
    consumer = awaitForever $ either (throwError . errorMessage) (yield . snd)

-- | Extract the package payload from an 'RPM', returning it in the conduit.
payloadC :: MonadError e m => Conduit RPM m BS.ByteString
payloadC = awaitForever (yield . rpmArchive)

-- | Extract the package payload from an 'RPM', decompress it, and return each element of
-- the payload as a 'Data.CPIO.Entry'.
payloadContentsC :: (MonadError e m, MonadResource m) => Conduit RPM m Entry
payloadContentsC = payloadC
                .| decompress Nothing
                .| readCPIO
