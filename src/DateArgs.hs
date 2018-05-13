{-# LANGUAGE OverloadedStrings #-}

module DateArgs where

import           Data.Maybe
import qualified Data.Text          as T
import qualified Data.Text.Lazy     as TL
import           Data.Text.Template
import           Data.Time
import           Data.Monoid

type Key = T.Text
type VerboseDate = T.Text
type DateDict = [(Key, VerboseDate)]
data DateRange = DateRange { start :: Maybe Day, end :: Maybe Day }
  deriving (Eq, Show)


msgSingle :: T.Text
msgSingle
  = "I will be out of the office on $start.\n\nFor immediate assistance, please reach out the UF Computing Help Desk at 352-392-4357. Their service desk will create a support ticket and will get in touch with UFIT personnel who can assist you in my absence."

msgMulti :: T.Text
msgMulti
  = "I will be out of the office from $start, through $end.\n\nFor immediate assistance, please reach out the UF Computing Help Desk at 352-392-4357. Their service desk will create a support ticket and will get in touch with UFIT personnel who can assist you in my absence."

parseArgs :: [String] -> DateRange
parseArgs []      = DateRange Nothing Nothing
parseArgs [x    ] = DateRange (parseDt x) Nothing
parseArgs (x:y:_) = DateRange (parseDt x) (parseDt y)

parseDt :: String -> Maybe Day
parseDt = parseTimeM True defaultTimeLocale "%Y-%m-%d"

getDayOfWeek :: String -> Maybe DayOfWeek
getDayOfWeek dt = dayOfWeek <$> parseDt dt

getVerboseDate :: Day -> VerboseDate
getVerboseDate d = dayOfWeekText <> ", " <> formattedTime
 where
  dayOfWeekText = T.pack . show . dayOfWeek $ d
  formattedTime = T.pack . formatTime defaultTimeLocale "%B %-d" $ d

makeDateMap :: DateRange -> Maybe DateDict
makeDateMap (DateRange Nothing _) = Nothing
makeDateMap (DateRange (Just start') Nothing) =
  Just [("start", getVerboseDate start')]
makeDateMap (DateRange (Just start') (Just end')) =
  Just [("start", getVerboseDate start'), ("end", getVerboseDate end')]

context :: DateDict -> Context
context vars x = fromMaybe err (lookup x vars)
  where err = error $ "Could not find key: " ++ T.unpack x

genMessage :: DateDict -> String
genMessage dict
  | length dict == 1 = TL.unpack $ substitute msgSingle (context dict)
  | otherwise        = TL.unpack $ substitute msgMulti (context dict)
