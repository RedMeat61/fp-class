import Control.Monad.Writer
import Data.Monoid
import Data.List
import System.Environment

eps = 0.00001

sin' x = runWriter $ tSin 1 x x x (x:[])
tSin k x prev sum taylor
    | (abs (prev) < eps) = tell (reverse taylor) >> return sum
    | otherwise = return  ( (-prev * x^2 ) / ((2*k+1)*2*k))  >>= \r -> tSin (k+1) x r (sum + r) (r: taylor)

	
cos' x = runWriter $ tCos 1 x 1 1 (1:[])
tCos k x prev sum taylor    
	| (abs (prev) < eps) = tell (reverse taylor) >> return sum
    | otherwise = return  ((-prev *x^2 ) / (2*k*(2*k-1)))  >>= \r -> tCos (k+1) x r (sum + r) (r: taylor)
    

