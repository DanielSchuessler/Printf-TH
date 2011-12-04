{-# LANGUAGE NoMonomorphismRestriction, TemplateHaskell #-}
{-# OPTIONS -Wall -Werror -ddump-splices #-}
import Text.Printf.TH

-- Compiling this with Printf-TH-0.1.1 yields:
--     Warning: Defined but not used: `n0'

f :: String -> String
f = $(printf "%s")
