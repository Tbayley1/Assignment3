newTake n _
	| n <= 0 = []
newTake _ [] = []
newTake n (x:xs) = x:newTake (n-1) xs