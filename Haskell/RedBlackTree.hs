import Text.Printf
import Data.Tree hiding (Tree )

data Tree a = T Color (Tree a) a (Tree a) | Leaf deriving Show
data Color = Red | Black deriving Show

data RBT a = Root (Tree Black a) deriving Show

--height
getTreeHeight :: Tree a -> Integer
getTreeHeight = treeCount (\l _ r -> 1 + max l r) 0

treeCount :: (r -> a -> r -> r) -> r -> Tree a -> r
treeCount f z = h
  where
    h Leaf         = z
    h (Node l x r) = f (h l) x (h r)


-- insert
insert x (Root t) = blacken (ins x t) where
     makeRootBlack (NN _ l y r) = (Root (N B l y r))

     data Node a where
  NN :: SColor c -> CT c1 a -> a -> CT c2 a -> Node a

  ins :: Ord a => a -> CT c a -> Node a

  ins x E = NN R E x E

  ins x s@(N c a y b)
  | x < y     = balanceL c (ins x a) y b
  | x > y     = balanceR c a y (ins x b)
  | otherwise = NN c a y b


--balance stuff

leftBalance :: Ord a => SColor c -> Node a -> a -> CT c1 a -> Node a
leftBalance B (NN R (N R a x b) y c) z d =
 NN R (N B a x b) y (N B c z d)
leftBalance B (NN R a x (N R b y c)) z d =
 NN R (N B a x b) y (N B c z d)
leftBalance col (NN col' a x b) y c = NN col (N col' a x b) y c

rightBalance :: Ord a => SColor c -> CT c1 a -> a -> Node a -> Node a
rightBalance B a x (NN R (N R b y c) z d) =
 NN R (N B a x b) y (N B c z d)
rightBalance B a x (NN R b y (N R c z d)) =
 NN R (N B a x b) y (N B c z d)
rightBalance col a x (NN col' b y c) = NN col a x (N col' b y c)



--pretty print tree

treeToPrint= toDataTree Tree

display = putStrLn $ drawTree treeToPrint
