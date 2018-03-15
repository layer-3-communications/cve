{-# LANGUAGE OverloadedStrings #-}

module Data.Cve
  ( Cve(..)
  , encode
  , decode
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Aeson as AE

data Cve = Cve
  { cveYear :: {-# UNPACK #-} !Word
  , cveSequence :: {-# UNPACK #-} !Word
  } deriving (Show,Read,Eq,Ord)

instance AE.ToJSON Cve where
  toJSON = AE.String . encode

instance AE.FromJSON Cve where
  parseJSON = AE.withText "Cve" $ \t -> case decode t of
    Nothing -> fail "could not decode CVE"
    Just c -> return c

encode :: Cve -> Text
encode (Cve y s) = T.concat ["CVE-",T.pack (show y),"-",padding,T.pack (show s)]
  where
  padding
    | s < 10 = "000"
    | s < 100 = "00"
    | s < 1000 = "0"
    | otherwise = T.empty

decode :: Text -> Maybe Cve
decode t = case T.splitOn (T.singleton '-') t of
  [a,b,c] -> if a == "CVE" || a == "cve" || a == "!CVE" || a == "!cve"
    then Nothing
    else decodeDigitParts b c
  [b,c] -> decodeDigitParts b c
  _ -> Nothing

decodeDigitParts :: Text -> Text -> Maybe Cve
decodeDigitParts yt st = case TR.decimal yt of
  Left _ -> Nothing
  Right (y,yx) -> if T.null yx
    then case TR.decimal st of
      Left _ -> Nothing
      Right (s,sx) -> if T.null sx
        then Just (Cve y s)
        else Nothing
    else Nothing

