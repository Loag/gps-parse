{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}

module Lib ( parse, describe ) where
  
import Prelude
import Data.List.Split ( splitOn )

class FromSentence a where
  fromSentence :: [String] -> Maybe GPSMessage

instance FromSentence GGA where
  fromSentence :: [String] -> Maybe GPSMessage
  fromSentence [a, b, c, d, e] = Just (A (GGA a b c d e))
  fromSentence _ = Nothing

data GGA = GGA {
  gga_name :: String,
  gga_time :: String,
  gga_latitude :: String,
  gga_nsIndicator :: String,
  gga_longitude :: String  
}

data GLL = GLL {
  ggl_name :: String
}

data GSA = GSA {
  gsa_name :: String
}

data GSV = GSV {
  gsv_name :: String
}

data GPSMessage = 
    A GGA 
  | B GSA
  | C GLL
  | D GSV


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
    "$GPGGA" ->  fromSentence @GGA (take 5 (parseString a))
    -- "$GPGLL" -> Just (fromSentence GLL (parseString a))
    -- "$GPGSA" -> Just (fromSentence GSA (parseString a))
    -- "$GPGSV" -> Just (fromSentence GSV (parseString a))
    -- "$GPMSS" -> Just (fromSentence MSS (parseString a))
    -- "$GPRMC" -> Just (fromSentence RMC (parseString a))
    -- "$GPVTG" -> Just (fromSentence VTG (parseString a))
    _ -> Prelude.Nothing
    
parseString :: String -> [[Char]]
parseString = splitOn ","


describe :: GPSMessage -> String
describe gga@(A GGA{gga_name=name}) = name