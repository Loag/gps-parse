{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib ( parse, getName ) where
  
import Prelude
import Data.List.Split

data GPSMessage = 
    GGA { name :: String } 
  | GLL { name :: String }
  | GSA { name :: String }
  | GSV { name :: String }
  | MSS { name :: String }
  | RMC { name :: String }
  | VTG { name :: String }

getName :: GPSMessage -> String
getName = name

fromSentence :: (String -> GPSMessage) -> [String] -> GPSMessage
fromSentence constructor (x:_) = constructor x
fromSentence _ [] = error "Empty list provided"

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
    "$GPGGA" -> Just (fromSentence GGA (parseString a))
    "$GPGLL" -> Just (fromSentence GLL (parseString a))
    "$GPGSA" -> Just (fromSentence GSA (parseString a))
    "$GPGSV" -> Just (fromSentence GSV (parseString a))
    "$GPMSS" -> Just (fromSentence MSS (parseString a))
    "$GPRMC" -> Just (fromSentence RMC (parseString a))
    "$GPVTG" -> Just (fromSentence VTG (parseString a))
    _ -> Prelude.Nothing
    
parseString :: String -> [[Char]]
parseString = splitOn ","

-- createGPSMessage :: [[Char]] -> GPSMessage
-- createGPSMessage a =
