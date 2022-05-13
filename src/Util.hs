{-# LANGUAGE NoMonomorphismRestriction #-}
module Util where

infixl 4 <$$>
(<$$>)  = fmap . fmap

infixl 4 <$$$> 
(<$$$>) = fmap . fmap . fmap
