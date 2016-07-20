import System.IO  
import System.Process

-- Binary Tree

data Color =  Black
            | Red
            deriving( Eq )
            
instance Show Color where
  show Black = "black"
  show Red = "red"


data Tree o =  Leaf
             | Node  {
                  object :: o ,
                  nodeId :: Int,
                  color :: Color,
                  leftNode :: (Tree o),
                  rightNode :: (Tree o),
                  parentNode :: (Tree o)
               }
             deriving (Show)  
             
 

max_id:: Tree o -> Int
max_id (Node _ id _ _ Leaf _) = id
max_id (Node _ id _ _ right _) = max_id right

min_id:: Tree o -> Int
min_id (Node _ id _ Leaf _ _) = id
min_id (Node _ id _ left _ _) = min_id left
      
 {-       
delete:: (Eq o,Ord o) => Tree o -> o -> Tree o
delete Leaf _ = Leaf
delete (Node x y left right) v 
        | v == x = reorg (Node x y left right)
        | v > x  = Node x y left (delete right v)
        | v < x  = Node x y (delete left v) right
            where 
                reorg (Node _ _ Leaf right) = right
                reorg (Node _ _ left Leaf) = left
                reorg (Node _ _ left right) = 
                    let max = max_id left in
                    Node max y (delete left max) right
-}           

list_to_tree:: [Int] -> Tree String -> Tree String
list_to_tree [] Leaf = Leaf
list_to_tree [] tree = tree
list_to_tree (x:xs) tree = list_to_tree xs (insert tree " " x)

tree_to_list:: Tree String -> String
tree_to_list Leaf = []
tree_to_list (Node _ id color left right _) =
    (tree_to_list left) ++ (show id) ++ "\n" ++ (tree_to_list right)
        
invert_tree:: Tree o -> Tree o
invert_tree Leaf = Leaf
invert_tree t@(Node o id color left right parent) =
     Node o id color (invert_tree right) (invert_tree left) t

get_node_by_id::Tree o -> Int -> Tree o
get_node_by_id Leaf _ = Leaf
get_node_by_id node@(Node o parentId color left right _) id
    | id == parentId = node
    | id > parentId  = get_node_by_id right id
    | id < parentId  = get_node_by_id left id
    


-- RedBlack Tree poperties

rotateLeft :: Tree o -> Tree o
rotateLeft t@(Node oX idX cX leftX (Node oY idY cY leftY rightY _) parent) = Node oY idY cY (Node oX idX cX leftX leftY t) rightY parent
rotateLeft n = n
  
rotateRight :: Tree o -> Tree o
rotateRight t@(Node oY idY cY (Node oX idX cX leftX rightX _) rightY parent) = Node oX idX cX leftX (Node oY idY cY rightX rightY t) parent
rotateRight n = n



insert::(Ord a)=>a->Tree a->Tree a
insert x Leaf = Node x Leaf Leaf Leaf
insert x t@(Node a l r p) = insertIt x t p
 where insertIt x Leaf (Node a l r p) = Node x Leaf Leaf (Node a Leaf Leaf Leaf)--1*
       insertIt x t@(Node a l r p) parent
        | x == a = t
        | x <  a = Node a (insertIt x l t) r p
        | x >  a = Node a l (insertIt x l t) p

insert:: Tree o -> o -> Int -> Tree o
insert  Leaf o id = Node o id Red Leaf Leaf Leaf
insert current@(Node oParent idParent color left right parent) o id = insert2 current o id parent
  where insert2 Leaf o2 id2 parent2 
        insert2 current2@(Node oP idP c l r p) o2 id2 parent2 
        | id2 == idP = current2
        | id > idParent  =  (Node oParent idParent color left (insert2 right o id current) parent)
        | id < idParent  =  (Node oParent idParent color (insert2 left o id current) right parent)

    


{-
insert_fixup node@(Node o id c left right parent) =  
    if (color parent) == Red then
      node
    else
      
      if parent == (leftNode (parentNode parent))










-}



-- To dotise
arcs :: Tree o -> [(Int,Int)]
arcs Leaf = []
arcs (Node o id c Leaf Leaf _) = []
arcs (Node o id c Leaf rightChild@(Node oRight idRight _ _ _ _) _) = [(id,idRight)] ++ (arcs rightChild)
arcs (Node o id c leftChild@(Node oLeft idLeft _ _ _ _) Leaf _) = [(id,idLeft)] ++ arcs leftChild
arcs (Node o id c leftChild@(Node oLeft idLeft _ _ _ _) rightChild@(Node oRight idRight _ _ _ _) _ ) = [(id,idLeft)] ++ [(id,idRight)] ++ arcs leftChild ++ arcs rightChild


arc :: (Int,Int) -> String
arc (a,b) = show a ++ "->" ++ show b

display_arcs :: Tree o -> String
display_arcs tree = unlines ( map arc (arcs tree) )

dotise :: Tree String -> String
dotise tree = "digraph \"complet3\" { \n node [fontname=\"DejaVu-Sans\", shape=circle] \n \n"++  (tree_to_list tree) ++ "\n" ++ (display_arcs tree) ++ " }"

makeTree = do        
    writeFile "/Users/alexisgallepe/Documents/Haskell-Binary-Tree/tree.dot" (dotise (list_to_tree ([20,19..0]) Leaf))
    
main = makeTree










