-- added this to test some of the polymorphic types in the chapter
{-# LANGUAGE InstanceSigs #-}

module ChapExp where

class ArchEnemy a where
    getArchEnemy :: a -> String

instance ArchEnemy Bool where
    getArchEnemy :: Bool -> String
    getArchEnemy True = "False"
    getArchEnemy _ = "True"

instance ArchEnemy Int where
    getArchEnemy :: Int -> String
    getArchEnemy i = case i of
        0 -> "Division"
        _ -> "Derivative"

instance ArchEnemy Double where
    getArchEnemy :: Double -> String
    getArchEnemy n
        | isNaN n = "Infinity"
        | isInfinite n = "Limit"
        | otherwise = "NaN"


testFunc :: (ArchEnemy a) => a -> String
testFunc _ = "Test"

revealArchEnemy :: (ArchEnemy a, Show a) => a -> String
revealArchEnemy a = 
    "The arch enemy of " ++ show a ++ " is " ++ getArchEnemy a

archEnemy1 = 14 :: Int

rev :: [a] -> [a]
rev = go []
  where
      go :: [a] -> [a] -> [a]
      go acc [] = acc
      go acc (x:xs) = go (x : acc) xs

revfoldl :: [a] -> [a]
revfoldl = foldl (\acc ele -> (ele : acc)) []

revfoldr :: [a] -> [a]
revfoldr = foldr (\ele acc -> acc ++ [ele]) []

-- prefixes [1,2,3,4] = [[1],[1,2],[1,2,3], [1,2,3,4]]
prefixes :: [a] -> [[a]]
prefixes = foldr (\ele acc -> [ele] : map (\x -> ele : x) acc) []


---- test ---


{-
data Fighter = 
      Knight1 { knightName :: String, knightPower :: Int} 
    | Monster1 { monsterName :: String, monsterPower :: Int}
    deriving (Show)

instance Eq Fighter where
    (==) :: Fighter -> Fighter -> Bool
    (==) (Knight1 _ kp) (Monster1 _ mp) = kp == mp
    (==) (Monster1 _ mp) (Knight1 _ kp) = kp == mp
    (==) (Monster1 _ mp) (Monster1 _ kp) = kp == mp
    (==) (Knight1 _ mp) (Knight1 _ kp) = kp == mp
-}

data Set a = Set [a] deriving (Show)

search :: Eq a => a -> Set a -> Bool
search x (Set y) = elem x y

insert :: Eq a => a -> Set a -> Set a
insert x (Set y)
    | elem x y = Set y
    | otherwise = Set (x:y)

delete :: Eq a => a -> Set a -> Set a
delete x (Set y) = Set (filter (\y -> y /= x) y)

data BTree a = Empty | Node (BTree a) a (BTree a)

size :: BTree a -> Int
size Empty = 0
size (Node tl a tr) = size tl + 1 + size tr

height :: BTree a -> Int
height Empty = 0
height (Node tl a tr) = 1 + max (height tl) (height tr)


reflect :: BTree a -> BTree a
reflect Empty = Empty
reflect (Node tl a tr) = Node (reflect tr) a (reflect tl)

levelOrderTraversal :: BTree a -> [a]
--levelOrderTraversal Empty = []
--levelOrderTraversal (Node tl a tr) = 
levelOrderTraversal = 
    let
        join :: [[a]] -> [[a]] -> [[a]]
        join xss [] = xss
        join [] yss = yss
        join (xs:xss) (ys:yss) = (xs ++ ys) : join xss yss
        lot :: BTree a -> [[a]]
        lot Empty = []
        lot (Node tl a tr) = [a] : join (lot tl) (lot tr)
    in
        concat . lot 

instance (Show a) => Show (BTree a) where
    show t = drawTree t ""

drawTree :: (Show a) => BTree a -> String -> String
drawTree Empty spaces = spaces ++ "*\n"
drawTree (Node Empty x Empty) spaces = spaces ++ show x ++ "\n"
drawTree (Node tl x tr) spaces = spaces ++ show x ++ "\n" ++  drawTree tl (' ':' ':spaces) ++ drawTree tr (' ':' ': spaces)


data MyData a = MyData a deriving (Show)

instance Functor MyData where 
    fmap :: (a -> b) -> MyData a -> MyData b
    fmap f (MyData a) = MyData (f a)

data Secret e a
    = Reward e
    | Trap a
    deriving (Show, Eq)

instance Functor (Secret e) where
    fmap :: (a -> b) -> Reward a -> Reward b
    fmap f (Reward a) = Reward (f a)
    fmap _ (Trap e) = Trap e