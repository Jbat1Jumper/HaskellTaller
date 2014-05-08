data B = T 
	   | F 
	   | AND B B 
	   | OR B B 
	   | NOT B
	deriving (Show, Eq)

reducir :: B -> B
reducir T = T
reducir F = F

reducir (NOT T) = F
reducir (NOT F) = T
reducir (NOT a) = reducir (NOT (reducir a))

reducir (AND F _) = F
reducir (AND _ F) = F
reducir (AND T T) = T
reducir (AND a b) = reducir (AND (reducir a) (reducir b))

reducir (OR T _) = T
reducir (OR _ T) = T
reducir (OR F F) = F
reducir (OR a b) = reducir (OR (reducir a) (reducir b))