-------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Copyright   : (c) 2009, Tom Lokhorst
-- License     : BSD3
--
-- Maintainer  : Tom Lokhorst <tom@lokhorst.eu>
-- Stability   : Experimental
--
-- Main module for the `hackage2twitter` executable.
-- Contains `rssItem2Tweet` function to parse Hackage RSS items.
--
-------------------------------------------------------------------------------
module Main where

import Text.RSS.Syntax
import Web.Feed2Twitter

import Data.Maybe
import Data.List
import System.Console.GetOpt
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  (cfg, rest) <- processArgs defaultConfig options header args
  if length rest /= 3
   then putStrLn $ usageInfo header options
   else do
        let cfg' = cfg { username  = rest !! 0
                       , password  = rest !! 1
                       , cacheFile = rest !! 2
                       }
        rss2twitter cfg' rssItem2tweet
  where
    header = "Usage: hackage2twitter USERNAME PASSWORD CACHE-FILE [OPTIONS...]"
              ++ ", with the following options:"

-- Map a RSS item to a tweet.
-- This function will fail on a few `fromJust`s if the Hackage RSS feed ever changes.
rssItem2tweet :: RSSItem -> Tweet
rssItem2tweet ri = trunc4url (title ++ ", added by " ++ uploader
                               ++ maybe "." (": " ++) blurb) ++ url
  where
    title     = filter (/='\n') . fromJust . rssItemTitle $ ri
    uploader  = take (fromJust $ elemIndex ',' uploader') uploader'
    blurb     = fmap (\x -> drop (x + 3) desc) (findSubList "<p>" desc)
    url       = fromJust . rssItemLink $ ri
    guid      = rssGuidValue . fromJust . rssItemGuid $ ri
    desc      = map (\x -> if x == '\n' then ' ' else x)
                  . fromJust . rssItemDescription $ ri
    uploader' = drop 12 desc

findSubList :: Eq a => [a] -> [a] -> Maybe Int
findSubList [] s2 = Nothing
findSubList s1 s2 = listToMaybe $ filter (\x -> isPrefixOf s1 $ drop x s2) xs
  where
    xs = elemIndices (head s1) s2

defaultConfig :: Config
defaultConfig = Config
  { feedUrl = "http://hackage.haskell.org/packages/archive/recent.rss"
  , username = ""
  , password = ""
  , cacheFile = ""
  , cacheSize = 20
  , debugMode = False
  }

options :: [OptDescr (Config -> Config)]
options =
  [ Option ['d'] ["debug-mode"] (NoArg (\c -> c { debugMode = True })) "Debug mode, send tweets to stdout."
  -- , Option ['V'] ["version"] (NoArg (\c -> c { showVersion = True })) "Show program version."
  ]

-- Seems like this function should already exist somewhere in a package.
processArgs :: a -> [OptDescr (a -> a)] -> String -> [String] -> IO (a, [String])
processArgs defaultConfig options header args =
  case getOpt Permute options args of
    (oargs, nonopts, []    ) -> return (foldl (flip ($)) defaultConfig oargs, nonopts)
    (_    , _      , errors) -> ioError $ userError $ (concat errors) ++ usageInfo header options

