{-# LANGUAGE LambdaCase #-}

import Conduit(($$), (=$), awaitForever, stdinC)
import Control.Monad.IO.Class(liftIO)
import Data.Conduit(Consumer)
import Text.PrettyPrint(render)
import Text.PrettyPrint.HughesPJClass(Pretty(pPrint))

import RPM.Parse(parseRPMC)
import RPM.Types(RPM)

consumer :: Show e => Consumer (Either e RPM) IO ()
consumer = awaitForever $ \case
    Left err  -> liftIO $ print err
    Right rpm -> liftIO $ putStrLn $ render $ pPrint rpm

main :: IO ()
main =
    stdinC $$ parseRPMC =$ consumer
