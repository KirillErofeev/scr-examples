{-# LANGUAGE NoMonomorphismRestriction #-}
module Lib where

import Data.Maybe 

import Control.Applicative

import Text.HTML.Scalpel

source = ""

reviewsUrl = "http://www." ++ source ++ ".com/f64-forum"
reviewsScrape = scrapeURL reviewsUrl

review = do
    let clean1 = "div" @: ["id" @= "emptyidcc"]
    let clean2 = "tbody"
    let cleanSelectors = html <$> [clean1, clean2, clean2]
    r <- traverse reviewsScrape cleanSelectors
    return r
    
c2 f p a1 a2 = ((.) .) (f . p) a1 p a2
