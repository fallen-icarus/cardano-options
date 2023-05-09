module Main where

import Options.Applicative

import CLI.Parsers
import CLI.Run

main :: IO ()
main = do
  let preferences = prefs $ showHelpOnError <> showHelpOnEmpty
      opts = info (parseCommand <**> helper) 
                  (fullDesc <> progDesc "A protocol for Options Contracts on Cardano.")
  customExecParser preferences opts >>= runCommand