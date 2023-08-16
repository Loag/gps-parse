{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}

module Lib ( parse, describe ) where
  
import Prelude
import Data.List.Split ( splitOn )

class FromSentence a where
  fromSentence :: [String] -> Maybe GPSMessage

data GGA = GGA {
  gga_name :: String,
  gga_time :: String,
  gga_latitude :: String,
  gga_nsIndicator :: String,
  gga_longitude :: String  
}

instance FromSentence GGA where
  fromSentence :: [String] -> Maybe GPSMessage
  fromSentence [a, b, c, d, e, _, _, _, _, _, _, _, _, _, _] = Just (CGGA (GGA a b c d e))
  fromSentence _ = Nothing

data GLL = GLL {
  gll_name :: String,
  gll_lat :: String,
  gll_lon :: String
}

instance FromSentence GLL where
  fromSentence :: [String] -> Maybe GPSMessage
  fromSentence [a, b, _, d, _, _, _, _, _] = Just (CGLL (GLL a b d))
  fromSentence _ = Nothing

data GSA = GSA {
  gsa_name :: String,
  gsa_channel_1 :: String,
  gsa_channel_2 :: String,
  gsa_channel_3 :: String,
  gsa_channel_4 :: String, 
  gsa_channel_5 :: String,
  gsa_channel_6 :: String,
  gsa_channel_7 :: String,
  gsa_channel_8 :: String, 
  gsa_channel_9 :: String,
  gsa_channel_10 :: String,
  gsa_channel_11 :: String,
  gsa_channel_12 :: String
}

instance FromSentence GSA where
  fromSentence :: [String] -> Maybe GPSMessage
  fromSentence [a, _, _, d, e, f, g, h, i, j, k, l, m, n, o, _, _, _] = Just (CGSA (GSA a d e f g h i j k l m n o))
  fromSentence _ = Nothing

data GSV = GSV {
  gsv_name :: String,
  gsv_number_of_messages :: String,
  gsv_message_num :: String
}

instance FromSentence GSV where
  fromSentence :: [String] -> Maybe GPSMessage
  fromSentence [a, b, c, _, _, _, _, _, _, _, _, _, _, _, _, _] = Just (CGSV (GSV a b c))
  fromSentence [a, b, c, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] = Just (CGSV (GSV a b c))
  fromSentence _ = Nothing

data MSS = MSS {
  mss_name :: String,
  mss_signal_strength :: String
}

instance FromSentence MSS where
  fromSentence :: [String] -> Maybe GPSMessage
  fromSentence [a, b, _, _, _, _, _] = Just (CMSS (MSS a b))
  fromSentence _ = Nothing

data RMC = RMC {
  rmc_name :: String,
  rmc_speed_over_ground :: String
}

instance FromSentence RMC where
  fromSentence :: [String] -> Maybe GPSMessage
  fromSentence [a, _, _, _, _, _, _, b, _, _, _, _, _] = Just (CRMC (RMC a b))
  fromSentence _ = Nothing


data VTG = VTG {
  vtg_name :: String,
  vtg_speed :: String
}

instance FromSentence VTG where
  fromSentence :: [String] -> Maybe GPSMessage
  fromSentence [a, _, _, _, _, _, _, b, _, _] = Just (CVTG (VTG a b))
  fromSentence _ = Nothing

data GPSMessage = 
    CGGA GGA 
  | CGSA GSA
  | CGLL GLL
  | CGSV GSV
  | CMSS MSS
  | CRMC RMC
  | CVTG VTG

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
    "$GPGGA" ->  fromSentence @GGA (parseString a)
    "$GPGLL" ->  fromSentence @GLL (parseString a)
    "$GPGSA" ->  fromSentence @GSA (parseString a)
    "$GPGSV" ->  fromSentence @GSV (parseString a)
    "$GPMSS" ->  fromSentence @MSS (parseString a)
    "$GPRMC" ->  fromSentence @RMC (parseString a)
    "$GPVTG" ->  fromSentence @VTG (parseString a)
    _ -> Prelude.Nothing
    
parseString :: String -> [[Char]]
parseString = splitOn ","


describe :: GPSMessage -> String
describe (CGGA GGA{gga_name=name, gga_latitude=lat, gga_longitude=lon}) = name ++ " lat:" ++ lat ++ " lon:" ++ lon
describe (CGLL GLL{gll_name=name, gll_lat=lat, gll_lon=lon}) = name ++ " lat:" ++ lat ++ " lon:" ++ lon
describe (CGSA GSA{gsa_name=name, gsa_channel_1=chan1}) = name ++ " channel1:" ++ chan1
describe (CGSV GSV{gsv_name=name, gsv_message_num=num, gsv_number_of_messages=count}) = name ++ " num:" ++ num ++ " count:" ++ count
describe (CMSS MSS{mss_name=name, mss_signal_strength=sig}) = name ++ " sig strength:" ++ sig
describe (CRMC RMC{rmc_name=name, rmc_speed_over_ground=speed}) = name ++ " speed over ground (knots):" ++ speed
describe (CVTG VTG{vtg_name=name, vtg_speed=speed}) = name ++ " mph:" ++ speed
