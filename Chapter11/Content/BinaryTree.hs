module Chapter11.Content.BinaryTree where

data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

-- replaced the `a > b` condition with otherwise to avoid compiler warnings
insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a    = Node left a right
  | b < a     = Node (insert' b left) a right
  | otherwise = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected :: BinaryTree Integer
mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

-- acceptance test for mapTree
--
mapOkay :: IO ()
mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test failed!"

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left v right) = v : preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left v right) = inorder left ++ [v] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left v right) = postorder left ++ postorder right ++ [v]

testTree :: BinaryTree Integer
testTree = Node (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)) 4 (Node (Node Leaf 5 Leaf) 6 (Node Leaf 7 Leaf))

-- Tree
--       4
--     /   \
--    2     6
--   / \   / \
--  1   3 5   7

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [4, 2, 1, 3, 6, 5, 7]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3, 4, 5, 6, 7]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2, 5, 7, 6, 4]
  then putStrLn "Postorder fine!"
  else putStrLn "postorder failed check"

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder

-- The exercise suggest the function type as
-- foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
-- but this type will not allow to handle the accumulated values comming from both
-- tree branches, it will be able to handle just one of them.
-- Here is the updated function type
foldTree :: (b -> a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ acc Leaf = acc
foldTree f acc (Node left v right) = f (foldTree f acc left) v (foldTree f acc right)

printSum :: Show a => String -> a -> String -> String
printSum x y z = "(" ++ x ++ "+" ++ show y ++ "+" ++ z ++ ")"

-- You can try:
-- foldTree printSum "0" testTree
-- to see how a three number sum will evaluate when folding
