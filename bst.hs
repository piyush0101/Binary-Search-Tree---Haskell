{- A binary tree can be visualized as a Node with a left subtree and a right subtree
   Left or Right subtrees can be empty if its a leaf node
		a
	       / \
	      b   c
	     / \   \
	    d   e   f
             


data Tree a = Node a (Tree a) (Tree a) | EmptyTree deriving (Show)
 		  |	   |        |
		 node	  left     right
-}

data Tree a = Node a (Tree a) (Tree a) | EmptyTree deriving (Show)

insert :: (Ord a) => a -> Tree a -> Tree a
insert elem (EmptyTree) = (Node elem (EmptyTree) (EmptyTree))
insert elem (Node a left right)
	| elem == a = (Node elem left right)
	| elem < a = (Node a (insert elem left) right)
	| elem > a = (Node a left (insert elem right))

preOrder :: (Ord a) => Tree a -> [a] -> [a]
preOrder EmptyTree xs = []
preOrder (Node a left right) xs = (xs ++ [a]) ++ (preOrder left xs) ++ (preOrder right xs)

postOrder :: (Ord a) => Tree a -> [a] -> [a]
postOrder EmptyTree xs = []
postOrder (Node a left right) xs = xs ++ postOrder left xs ++ postOrder right xs ++ [a]

inOrder :: (Ord a) => Tree a -> [a] -> [a]
inOrder EmptyTree xs = []
inOrder (Node a left right) xs = xs ++ inOrder left xs ++ [a] ++ inOrder right xs

fill :: [Int] -> Tree Int -> Tree Int
fill [] tree = tree
fill (x:xs) tree = let subtree = insert x tree in fill (xs) subtree
