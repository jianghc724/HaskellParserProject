module While where

import Parser
--import Calculate
import qualified Data.Map as M

type Env = M.Map String String

procStat :: Env -> Statement -> Env
procStat env (Begin p q) = (procStats (procStat env p) q)
procStat env (Skip) = env
procStat env (Set var p) = (M.insert "a" "b" env)
procStat env (If expr p q) = if (eval expr) then (procStat env p) else (procStat env q)
procStat env (While expr p) = if (eval expr) then (procStat env p) else env
    
procStats :: Env -> Statements -> Env
procStats env Nil = env
procStats env (List p q) = (procStats (procStat env p) q)