import Control.Monad
import Control.Monad.State

func1 rest el  = do
	(accum,len) <- get 
	let total = if el == 1 then accum+1 else accum
	let distr = total/len :: Double 
	put(total,len+1) 
	return $ distr:rest
modifyToDistr a = reverse $ fst $ runState (foldM func1 [] a) (0,1)
	
main = do
	let a = [1,2,1,2,2,2,1,1,1,2,1,2] 
	print $ modifyToDistr a 
