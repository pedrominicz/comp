module Main where

import Assemble
import Convert
import Instruction
import Lex
import Parse
import Syntax

import System.Exit

import Control.Monad.RWS
import Data.Traversable
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as M
import qualified Data.Set as S

before :: S.Set String -> IO ()
before s = do
  putStrLn ".global _start"
  for (S.toList s) $ \s ->
    putStrLn $ ".extern " ++ s
  putStrLn ""
  putStrLn ".text"
  putStrLn "_start:"
  putStrLn "        call main"
  putStrLn "        mov %rax, %rdi"
  putStrLn "        mov $60, %eax"
  putStrLn "        syscall"
  putStrLn ""

compile :: String -> Maybe ([Instruction], S.Set String)
compile str = do
  ts <- scan $ B.pack str
  prog <- parseProg ts
  (ins, defs) <- evalRWST (for prog convertFunc) M.empty 0
  let fs = defs S.\\ (S.fromList $ map funcName prog)
  return (concat ins, S.map B.unpack fs)

main :: IO ()
main = do
  input <- getContents
  case compile input of
    Nothing -> exitWith (ExitFailure 1)
    Just (ins, defs) -> do
      before defs
      for ins assemble
      return ()
