module Main (main) where

import Infer
import Parse
import Type

import System.IO
import qualified Data.ByteString as B

main :: IO ()
main = isEOF >>= \case
  True -> return ()
  False -> do
    str <- B.getLine
    case parse str of
      Nothing -> putChar '!' >> putChar '\n'
      Just e -> 
        infer str e >>= \case
          Nothing -> putChar '?' >> putChar '\n'
          Just t -> pretty t
    main
