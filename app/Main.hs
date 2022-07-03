module Main where

import HDT.Tasks

main :: IO ()
main = testBlockChain

testRunIO :: IO ()
testRunIO = runIO [ping, pong]

testBlockChain :: IO ()
testBlockChain = runIO $ clock : [node 3 nid | nid <- [0,1,2]] 