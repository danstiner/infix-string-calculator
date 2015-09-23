module Main where

import           StringCalculator

import           Control.Monad
import           System.Exit
import           System.IO

main :: IO ()
main = do
	putStrLn "Enter expression using whole numbers with / * + - ( ) or negation"
	repl

repl :: IO ()
repl = do
	input <- prompt "> "
	when (input == "exit") exitSuccess
	print (calculate input)
	repl

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine
