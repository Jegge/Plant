{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Main where

import Control.Applicative ((<|>))

import Data.Attoparsec.Text
import Data.Maybe
import Data.List
import Data.List.Split (splitWhen)
import Data.Text (pack)
import Data.Tree

import System.Console.CmdArgs(Data, Typeable, cmdArgs, def, typ, help, name, summary, (&=))
import System.Exit (ExitCode(..), exitWith)

data Index = Range Int Int
           | Indicees [Int]
           deriving (Show, Eq)

keyword :: (Char -> Bool) -> String -> [Index] -> [[String]]
keyword p ss is = map (fields p ss) $ (map indicees') is
  where
    indicees' :: Index -> [Int]    
    indicees' (Range a b)  = [a..b]
    indicees' (Indicees a) = a

    fields :: (Char -> Bool) -> String -> [Int] -> [String]
    fields p ss fs = let l = splitWhen p ss in mapMaybe (\i -> if length l >= i then Just(l !! (i - 1)) else Nothing) fs

keywords :: (Char -> Bool) -> [Index] -> [String] -> [[[String]]]
keywords p is ss = map (\t -> keyword p t is) ss 

forest :: (Ord a) => [[a]] -> Forest a
forest = unfoldForest (\(a, b) -> (a, heads $ groups b)) . heads . groups
  where
    groups :: (Ord a) => [[a]] -> [[[a]]]
    groups = groupBy (\a b -> head a == head b) . sortBy (\a b -> head a `compare` head b)
    
    heads :: [[[a]]] -> [(a, [[a]])]
    heads [[]]   = []
    heads [[[]]] = []
    heads ll = map (\l -> let l' = head l in (head l', map tail l)) ll

indicees :: Parser [Index]
indicees = sepBy1 (range' <|> index') ";"
  where
    range' :: Parser Index
    range' = do
      a <- option 0 decimal
      _ <- char '-'
      b <- option (maxBound :: Int) decimal
      pure $ Range a b

    index' :: Parser Index
    index' = Indicees <$> sepBy1 decimal ","


data Plant = Plant { delimiter :: String
                   , fields :: String
                   }
                   deriving (Data, Typeable, Show, Eq)

plant :: Plant
plant = Plant { delimiter = def &= name "d" &= typ "CHAR" &= help "Set delimiter for input file"  
              , fields = def &= typ "LIST" &= name "f" &= help "Selects fields for each tree level"
              }
              &= help "A command line editor for spoolfiles." 
              &= summary "Plant v0.1.0.0 (c) Sebastian Boettcher"
              
main :: IO ()
main = do
  (Plant d f) <- cmdArgs plant
  
  delimiter <- case length d of
      0 -> pure $ ' '
      1 -> pure $ head d
      _ -> putStrLn ("Invalid delimiter: '" ++ d ++ "'") >> (exitWith $ ExitFailure 1) 
          
  fields <- case parseOnly indicees (pack f) of 
     Left  e -> putStrLn ("Invalid field definition: " ++ e) >> (exitWith $ ExitFailure 2) 
     Right r -> pure r

  interact $ drawForest . map (fmap unwords) . forest . keywords (== delimiter) fields . filter (not . null) . lines






