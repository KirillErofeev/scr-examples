{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Maybe 

import Control.Applicative

import Text.HTML.Scalpel

import Text.StringLike

source = ""

reviewsUrl = "http://www." ++ source ++ ".com/f64-forum"
reviewsScrape = scrapeURL reviewsUrl
testScrape = scrapeStringLike test
--
--review :: IO (Maybe [String])
--review = do
--    let clean1 = "div" @: ["id" @= "emptyidcc"]
--    let clean2 = "tbody"
--    let cleanSelectors = [html clean1, innerHTML clean2, html clean2]
--    --r <- (!! i) <$> traverse reviewsScrape cleanSelectors
--    let r = testScrape (sequence cleanSelectors)
--    let r0 = map testScrape cleanSelectors
--    return (r, r0)
    
--c 2 f p a1 a2 = ((.) .) (f . p) a1 p a2
test = "<div id=\"emptyidcc\"> <tbody><p>1</p> <tbody><p>2</p></tbody> </tbody></div> <tbody><p>3</p></tbody>"

review' :: Scraper String [String]
review' = do
    let clean1 = "div" @: ["id" @= "emptyidcc"]
    let clean2 = "tbody"
    --html clean1
    htmls clean2
    --innerHTML clean2
