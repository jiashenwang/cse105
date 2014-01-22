module DFAintersect where
import DFA

intersectDFA :: DFA st1 -> DFA st2 -> DFA (st1,st2)
intersectDFA (qs1, sigma, delta1, q1, inF1) (qs2, sigma2, delta2, q2, inF2) | sigma2 == sigma = 
   let   qs = [(r1,r2) | r1 <- qs1, r2 <- qs2]
         delta (r1,r2) a = (delta1 r1 a, delta2 r2 a)
         q0 = (q1, q2)
         inF (r1,r2) = (inF1 r1) && (inF2 r2)
   in (qs, sigma, delta, q0, inF)

