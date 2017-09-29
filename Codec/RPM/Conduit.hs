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

module Codec.RPM.Conduit(parseRPMC)
 where

import           Control.Monad.Except(MonadError, throwError)
import           Conduit((.|), Conduit, awaitForever, yield)
import qualified Data.ByteString.Char8 as C
import           Data.Conduit.Attoparsec(ParseError(..), conduitParserEither)

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
-- consumption.  Functions can be written to extract just one element out of the 'RPM' and
-- pass it along.  For instance:
--
-- > payloadC :: MonadError e m => Conduit RPM m BS.ByteStrin
-- > payloadC = awaitForever (yield . rpmArchive)
--
-- On error, the rest of the conduit will be skipped and the 'ParseError' will be returned
-- as the result to be dealt with.
parseRPMC :: MonadError String m => Conduit C.ByteString m RPM
parseRPMC =
    conduitParserEither parseRPM .| consumer
 where
    consumer = awaitForever $ either (throwError . errorMessage) (yield . snd)
