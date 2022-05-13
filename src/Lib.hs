{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Maybe 
import Data.List (isInfixOf) 

import Control.Applicative
import Control.Monad (join)

import Text.HTML.Scalpel (scrapeURL, hasClass, text, notP, chroot, chroots, anySelector, attr, html, scrapeStringLike, (@:), Scraper)
import Text.StringLike
import Text.Parsec (many1, anyChar, digit, letter, skipMany1, parse)

import Util ((<$$>), (<$$$>))
import Types

source = "angraal"
site = "http://www." ++ source ++ ".com"

reviews = "/f64-forum"
masters = "/f48-forum"
runes   = "/f5-forum"

mastersUrl = site ++ masters
reviewsUrl = site ++ reviews
runesUrl   = site ++ runes

reviewsScrape = scrapeURL reviewsUrl
mastersScrape = scrapeURL mastersUrl
testScrape = scrapeStringLike test

scrapeChapter chapterPath = scrapeURL (site ++ chapterPath) themesScraper
scrapeMessages themePath = scrapeURL (site ++ themePath) messagesScraper

s = printTest (scrapeMessages theme0) id
--s = writeTest (raw) (writeFile "out.txt") id
--s = printTest (scrapeChapter runes) id

printTest scraper        f = join $ ((putStrLn) . testShow . f . fromJust) <$> scraper
writeTest scraper writeF f = join $ ((writeF)   . testShow . f . fromJust) <$> scraper
raw = scrapeURL (site ++ theme0) rawScraper

readAngraal = getChapter "Руника и Язычество" runes
--test = scrapeStringLike 
--

rawScraper :: Scraper String String
rawScraper = html "html"

t0 = "f48p450-forum"
t1 = "f48-forum"
theme0 = "/t16532-topic"

readPage :: String -> Page
readPage str = case parse p "" str of
    Right page    -> page
    Left errorMsg -> error . show $ errorMsg
    where
        p = makePage <$> nextInt <*> (nextInt <|> pure "0")
        nextInt = skipMany1 letter *> many1 digit

nextPage (Page theme offset) = Page theme (50 + offset)


updateThemeMessages (Theme url name _) = Theme url name <$> msgs where
    msgs = fromJust <$> scrapeURL (site ++ url) messagesScraper

getChapter name path = Chapter name <$> getThemes path

getThemes chapterPath = do 
     themes     <- fromJust <$> scrapeChapter chapterPath
     sequence $ updateThemeMessages <$> themes


--themeNames :: Scraper String Themes
themesScraper :: Scraper String [Theme]
themesScraper = chroots ("a" @: [hasClass "topictitle"]) theme where
    theme = return Theme <*> attr "href" anySelector <*> text anySelector <*> pure [] 

messagesScraper' :: Scraper String String
messagesScraper' = chroot ("div" @: [hasClass "postbody"]) messages where
    messages = text "div" 

msgHtmlScraper :: Scraper String [String]
msgHtmlScraper = chroots ("div" @: [hasClass "postbody"]) messages where
    messages = html "div" 

messagesScraper :: Scraper String [String]
messagesScraper = fromJust . flip scrapeStringLike messagesScraper' <$$> filter p <$> msgHtmlScraper where
    p = not . isInfixOf "<div id=\"taboola-below-article-thumbnails\"></div>"

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
