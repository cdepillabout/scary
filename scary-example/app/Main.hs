{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Scary #-}

module Main where

test :: Int
test = pure 3

main :: IO ()
main = print "hello scary world"
