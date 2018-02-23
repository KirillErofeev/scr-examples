{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Maybe 

import Control.Applicative

import Text.HTML.Scalpel
import Text.StringLike
import Text.Parsec (many1, anyChar, digit, letter, skipMany1, parse)

import Types

source = "angraal"
site = "http://www." ++ source ++ ".com"

reviews = "/f64-forum"
masters = "/f48-forum"

mastersUrl = site ++ masters
reviewsUrl = site ++ reviews

reviewsScrape = scrapeURL reviewsUrl
mastersScrape = scrapeURL mastersUrl
testScrape = scrapeStringLike test

t0 = "f48p450-forum"
t1 = "f48-forum"

readPage :: String -> Page
readPage str = case parse p "" str of
    Right page    -> page
    Left errorMsg -> error . show $ errorMsg
    where
        p = makePage <$> nextInt <*> (nextInt <|> pure "0")
        nextInt = skipMany1 letter *> many1 digit

nextPage (Page theme offset) = Page theme (50 + offset)
--themes url pages =

test = "<div id=\"emptyidcc\"> <tbody><p>1</p> <tbody><p>2</p></tbody> </tbody></div> <tbody><p>3</p></tbody>"
--
--clean1 = "div" @: ["id" @= "emptyidcc"]
--clean2 = "tbody"
--
--msgs' :: Scraper String [String]
--msgs' = do
--    let div = "div" @: ["id" @= "emptyidcc"]
--    let clean = "table" @: [hasClass "forumline"]
--    let title = "a" @: [hasClass "topictitle"]
--    innerHTML (clean1 // "tbody" // "tr")
--    chroot div $ text clean
--    texts title
--
--s = do
--    mastersScrape msgs'
--
--ps = s >>= (putStrLn . concatMap ('\n' :) . fromJust)
--
--r :: Scraper String String
----r = (,) <$> innerHTML clean2 <*> innerHTML clean2 
--r = innerHTML (clean2 // clean2)
