{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Main where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser, parseOnly, sepBy1, option, decimal, char)
import Data.Char (isSpace)
import Data.List (groupBy, sortBy)
import Data.List.Split (splitWhen)
import Data.Text (pack)
import Data.Tree (Forest, drawForest, unfoldForest)
import System.Console.CmdArgs(Data, Typeable, cmdArgs, def, typ, help, name, summary, (&=))
import System.Exit (ExitCode(..), exitWith)

data Index = Range Int Int
           | Indicees [Int]
           deriving (Show, Eq)

indicees :: Parser [Index]
indicees = sepBy1 (range <|> index) " "
  where
    range :: Parser Index
    range = do
      a <- option 1 decimal
      _ <- char '-'
      b <- option (maxBound :: Int) decimal
      pure $ Range a b

    index :: Parser Index
    index = Indicees <$> sepBy1 decimal ","

keywords :: Char -> [Index] -> String -> [[String]]
keywords d is s = let ls = splitWhen (== d) s
                      l = length ls
                      in map (map ((!!)ls) . saveIndicees l) is
  where
    saveIndicees :: Int -> Index -> [Int]
    saveIndicees l (Range a b)  = [a - 1..min (b-1) (l-1)]
    saveIndicees l (Indicees f) = filter (<l) $ map pred f

makeForest :: (Ord a) => [[a]] -> Forest a
makeForest = unfoldForest (\(a, b) -> (a, splitHead $ orderGroups b)) . splitHead . orderGroups
  where
    orderGroups :: (Ord a) => [[a]] -> [[[a]]]
    orderGroups = groupBy (\a b -> head a == head b) . sortBy (\a b -> head a `compare` head b) . filter (not . null)

    splitHead :: [[[a]]] -> [(a, [[a]])]
    splitHead [[]]   = []
    splitHead [[[]]] = []
    splitHead ll = map (\l -> let l' = head l in (head l', map tail l)) ll

drawForest' :: Bool -> Forest String -> String
drawForest' False f = drawForest f
drawForest' True  f = unlines $ filter (not . all (\l -> isSpace l || l == '|')) $ filter (not . null) $ lines $ drawForest f

data Plant = Plant { delimiter_ :: String
                   , fields_ :: String
                   , compact_ :: Bool
                   }
                   deriving (Data, Typeable, Show, Eq)

plant :: Plant
plant = Plant { delimiter_ = def &= name "d" &= typ "CHAR" &= help "Set delimiter for input file"
              , fields_ = def &= typ "LIST" &= name "f" &= help "Selects fields for each tree level"
              , compact_ = def &= name "c" &= help "Compact drawing"
              }
              &= help "Creates trees out of lists"
              &= summary "Plant v0.1.0.0 (c) Sebastian Boettcher"
              
main :: IO ()
main = do
  (Plant delimiter fields compact) <- cmdArgs plant
  
  d <- case length $ delimiter of
            0 -> pure $ ' '
            1 -> pure $ head $ delimiter
            _ -> putStrLn ("Invalid delimiter: '" ++ delimiter ++ "'") >> (exitWith $ ExitFailure 1)
          
  f <- case parseOnly indicees (pack $ fields) of
            Left  e -> putStrLn ("Invalid field definition: " ++ e) >> (exitWith $ ExitFailure 2)
            Right r -> pure r

  interact $ (drawForest' compact) . map (fmap unwords) . makeForest . map (keywords d f) . filter (not . null) . lines
