{-# LANGUAGE OverloadedStrings #-}

module Assemble where

import Instruction

import Data.Traversable
import qualified Data.ByteString.Lazy.Char8 as B

assemble :: Instruction -> IO ()
assemble i = case i of
  Int i -> do
    putStrLn $ "        mov $" ++ show i ++ ", %rax"
    putStrLn $ "        push %rax"
    putStrLn $ "        nop"
  Call n -> do
    putStrLn $ "        pop %rax"
    let regs = reverse $ take n ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]
    for regs $ \reg -> do
      putStrLn $ "        pop %rbx"
      putStrLn $ "        mov %rbx, %" ++ reg
    putStrLn $ "        call *%rax"
    putStrLn $ "        nop"
  Add -> do
    putStrLn $ "        pop %rcx"
    putStrLn $ "        pop %rax"
    putStrLn $ "        add %rcx, %rax"
    putStrLn $ "        push %rax"
    putStrLn $ "        nop"
  Local i -> do
    putStrLn $ "        mov " ++ show (-8 * i) ++ "(%rbp), %rax"
    putStrLn $ "        push %rax"
    putStrLn $ "        nop"
  Global g -> do
    putStrLn $ "        mov $" ++ B.unpack g ++ ", %rax"
    putStrLn $ "        push %rax"
    putStrLn $ "        nop"
  Assign i -> do
    putStrLn $ "        pop %rax"
    putStrLn $ "        mov %rax, " ++ show (-8 * i) ++ "(%rbp)"
    putStrLn $ "        push %rax"
    putStrLn $ "        nop"
  Discard -> do
    putStrLn $ "        pop %rax"
    putStrLn $ "        nop"
  JumpZero i -> do
    putStrLn $ "        pop %rax"
    putStrLn $ "        test %rax, %rax"
    putStrLn $ "        jz _" ++ show i
    putStrLn $ "        nop"
  Jump i -> do
    putStrLn $ "        jmp _" ++ show i
    putStrLn $ "        nop"
  Label i -> do
    putStrLn $ "_" ++ show i ++ ":"
  Return -> do
    putStrLn $ "        pop %rax"
    putStrLn $ "        leave"
    putStrLn $ "        ret"
    putStrLn $ "        nop"
  Func name args decl -> do
    putStrLn $ B.unpack name ++ ":"
    putStrLn $ "        push %rbp"
    putStrLn $ "        mov %rsp, %rbp"
    putStrLn $ "        sub $" ++ show (8 * (args + decl)) ++ ", %rsp"
    let regs = take args $ zip ["rdi", "rsi", "rdx", "rcx", "r8", "r9"] [1..]
    for regs $ \(reg, pos) -> do
      putStrLn $ "        mov %" ++ reg ++ ", " ++ show (-8 * pos) ++ "(%rbp)"
    putStrLn $ "        nop"
