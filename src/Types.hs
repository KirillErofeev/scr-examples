{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
module Types where

import Text.HTML.Scalpel
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Read

type Message = String

data Theme = Theme {themeUrl :: URL, themeName :: String, themeMessages :: [Message]}
data Chapter = Chapter {chapterName :: String, chapterThemes :: [Theme]} deriving (Show)
--mapUrl f (Theme url name msgs) = Theme (f url) name msgs 

data Page  = Page {pageTheme :: String, pageOffset :: Int}

instance Show Page where
    show (Page theme offset) | offset > 0 = 'f' : theme ++ "p" ++ (show offset) ++ "-forum"
                             | otherwise  = 'f' : theme ++ "-forum"
instance Show Theme where
    show (Theme url name msgs) = name ++ " " ++ url ++ "\n=======Messages=======\n" ++
        testShow msgs ++ "\n=====================\n"

class TestShow a where
    testShow :: a -> String

instance TestShow String where
    testShow = id

instance TestShow ([] String) where
    testShow = concat . zipWith number [1..] . map (\x -> x ++ "\n") where
        number i s = show i ++ " " ++ s

instance TestShow ([] Theme) where
    testShow = concat . zipWith number [1..] . map (\x -> show x ++ "\n") where
        number i s = show i ++ " " ++ s

makePage theme strOffset = Page theme (read strOffset :: Int)

