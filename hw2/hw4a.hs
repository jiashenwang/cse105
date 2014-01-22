module DFAcomplement where
import Data.List ((\\))
import DFA

complementDFA :: DFA st -> DFA st
complementDFA (qs, sigma, delta, q, inF) =
   let   qsc = [r | r <- qs]
         deltac r a = delta r a
         qc = q
         inFc r = not (inF r)
   in (qsc, sigma, deltac, qc, inFc)

