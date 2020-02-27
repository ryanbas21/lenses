{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wreq
import qualified Data.Aeson as A 
import Data.Char
import Data.Maybe
import Lib
import Control.Lens
import Control.Applicative 
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as BC

type API_KEY = String 

newtype Query = Query String

key :: API_KEY
key = "4k6fyyjhfmupsnhqn62pad72"

data AllRankings = AllRankings {
                            _generated_at :: String
                         ,  _rankings :: [Division]
                         } deriving (Show)

instance A.FromJSON AllRankings where
  parseJSON (A.Object v) =  AllRankings
                             <$> v A..: "generated_at" 
                             <*> v A..: "rankings"

data Division = Division {
                         _type_id :: Integer
                       , _name :: String
                       , _year :: Integer
                       , _week :: Integer
                       , _competitor_rankings :: [Rankings]
                       } deriving (Show)

instance A.FromJSON Division where
  parseJSON  (A.Object v)=  Division 
             <$>  v A..: "type_id"
             <*>  v A..: "name"
             <*>  v A..: "year"
             <*>  v A..: "week"
             <*>  v A..: "competitor_rankings" 

data Rankings = Rankings {
                           _rank :: Integer
                         , _movement :: Integer
                         , _competitor :: Competitor
                         } deriving (Show)

instance A.FromJSON Rankings where
  parseJSON (A.Object v) = Rankings
          <$> v A..: "rank"
          <*> v A..: "movement"
          <*> v A..: "competitor" 

data Competitor = Competitor {
                               _competitor_name :: String
                             , _id :: String
                             , _abbreviation :: String
                             } deriving (Show)

instance A.FromJSON Competitor where
  parseJSON (A.Object v) = Competitor
    <$> v A..: "name"
    <*> v A..: "id"
    <*> v A..: "abbreviation"

makeLenses ''Competitor                             
makeLenses ''AllRankings                             
makeLenses ''Division                             
makeLenses ''Rankings                             

getFighterData :: IO (Response B.ByteString)
getFighterData = get "http://api.sportradar.us/ufc/trial/v2/en/rankings.json?api_key=4k6fyyjhfmupsnhqn62pad72"

parseFighterData :: Response B.ByteString -> Maybe AllRankings
parseFighterData v = A.decode $ (v ^. responseBody)

getDivision :: Query -> Maybe AllRankings -> [ Division ]
getDivision (Query str) rank = rank ^.. _Just . rankings . folded . filtered (\v -> v ^. name == str)

handleQuery :: Query -> Query
handleQuery (Query "light heavyweight") = Query "light_heavyweight"
handleQuery (Query "lw") = Query "lightweight"
handleQuery (Query "lhw") = Query "light_heavyweight"
handleQuery (Query "hw") = Query "heavyweight"
handleQuery (Query "fw") = Query "featherweight"
handleQuery (Query "p4p") = Query "pound_for_pound"
handleQuery a = a


getTop10 ::  Maybe AllRankings -> [Division]
getTop10 = getDivision (Query "pound_for_pound")

getCompetitors :: [Division] -> [String]
getCompetitors d = d ^.. competitor_rankings . folded 

main :: IO ()
main = do 
        fighterData <- getFighterData
        let allRankings = parseFighterData fighterData 
        Prelude.putStrLn "Choose a division"
        division <-  Query <$> getLine
        -- print "play"
        let divvy = getDivision (handleQuery division) allRankings
        -- print $ getTop10 allRankings
        print $ getCompetitors <$> divvy




