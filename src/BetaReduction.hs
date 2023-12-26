module BetaReduction where
import CPS
import Util(free)

-- reduction :: Term -> Term
-- reduction a = transform f a
--   where 
-- 		f (LetCont k x t c) | k `elem` free c -> LetCont k x t 
