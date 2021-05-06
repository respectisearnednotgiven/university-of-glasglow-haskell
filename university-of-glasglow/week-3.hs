lst_ = map f lst

f x = x*(x+1)

lst = [ 1.. 10]

main = do
    print lst_
    print accl
    print accr
g :: Double -> Double -> Double
g = (/)
g' = (/)

accl = foldl g 1 lst

accr = foldr g' 1 lst

data SimpleNum = One | Two | Many deriving Show

data CricketScore = Score [Char] Int Int deriving Show

data Tree = Leaf | Node Int Tree Tree deriving Show
treeDepth :: Tree -> Int
treeDepth Leaf = 0
treeDepth (Node _ leftSubtree rightSubtree) = 1 + max (treeDepth leftSubtree) (treeDepth rightSubtree)

isSortedTree :: Tree -> Int -> Int -> Bool -- function isSortedTree has 3 args passed into it; a Tree datatype (either leaf or node), 2 integers and then returns a bool
isSortedTree Leaf _ _ = True -- if the function is called and the Tree datatype "leaf" is passed into it, then we don't care about the 2 integers, we'll just return True
isSortedTree (Node x leftSubtree rightSubtree) minVal maxVal = -- if node is passed into it
    let leftSorted  = isSortedTree leftSubtree minVal x
        rightSorted = isSortedTree rightSubtree x maxVal
    in x >= minVal && x < maxVal && leftSorted && rightSorted

addNewMax :: Tree -> Tree
-- add a new max element to tree
addNewMax Leaf = Node 0 Leaf Leaf -- input tree with no nodes
addNewMax (Node x t1 Leaf) = Node x t1 (Node (x+1) Leaf Leaf) -- this is the rightmost node
addNewMax (Node x t1 t2) = Node x t1 (addNewMax t2) -- intermediate node, go down right subtree

