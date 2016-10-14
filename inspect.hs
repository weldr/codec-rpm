import Conduit(($$), (=$), awaitForever, stdinC)
import Control.Monad.IO.Class(liftIO)
import Data.Conduit(Consumer)
import Text.PrettyPrint(render)
import Text.PrettyPrint.HughesPJClass(Pretty(pPrint))

import RPM.Parse(parseRPMC)
import RPM.Types(RPM)

consumer :: Consumer RPM IO ()
consumer = awaitForever (liftIO . putStrLn . render . pPrint)

main :: IO ()
main =
    stdinC $$ parseRPMC =$ consumer
