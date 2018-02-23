{-# LANGUAGE NoMonomorphismRestriction #-}
module Types where

import Text.HTML.Scalpel
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Read

data Theme = Theme {a :: URL, name :: String}

data Page  = Page {theme :: String, offset :: Int}

instance Show Page where
    show (Page theme offset) | offset > 0 = 'f' : theme ++ "p" ++ (show offset) ++ "-forum"
                             | otherwise  = 'f' : theme ++ "-forum"



makePage theme strOffset = Page theme (read strOffset :: Int)

