module Lib ( someFunc ) where

import Prelude

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data GPSMessage = 
    GGA { name :: String } 
  | GLL { name :: String }
  | GSA { name :: String }
  | GSV { name :: String }
  | MSS { name :: String }
  | RMC { name :: String }
  | VTG { name :: String }

-- if empty or not starting with a dollar sign, it is not a gps message
-- if the first two char after dollar sign are GP
-- if the next 3 char after GP are in a specified list
-- then we can parse the message
parse :: String -> Maybe GPSMessage
parse a
  | null a = Prelude.Nothing
  | head a /= '$' = Prelude.Nothing
  | otherwise = parseMessage a

parseMessage :: String -> Maybe GPSMessage
parseMessage a =
  case take 6 a of 
    "$GPGGA" -> Just GGA {name = "GGA MESSAGE"} 
    "$GPGLL" -> Just GLL {name = "GLL MESSAGE"}
    "$GPGSA" -> Just GSA {name = "GSA MESSAGE"}
    "$GPGSV" -> Just GSV {name = "GSV MESSAGE"}
    "$GPMSS" -> Just MSS {name = "MSS MESSAGE"}
    "$GPRMC" -> Just RMC {name = "RMC MESSAGE"}
    "$GPVTG" -> Just VTG {name = "VTG MESSAGE"}
    _ -> Prelude.Nothing
    
