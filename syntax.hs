lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SLEVIN"
lucky x = "Out of luck, pal"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second ( _, y, _) = y

third :: (a, b, c) -> c
third ( _, _, z) = z

head' :: [a] -> a
head' [] = error "Can't call head on an empty list"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++  show y
tell (x:y:_) = "The list is long.  The first two elements are: " ++ show x ++ " and " ++ show y

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
	| bmi <= 18.5 = "You're underweight"
	| bmi <= 25.0 = "You're normal"
	| bmi <= 30.0 = "You're fat!"
	| otherwise = "You're a whale"
	where bmi = weight / height ^ 2
