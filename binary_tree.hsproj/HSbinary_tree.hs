import System.IO  

-- Binary Tree
data Tree a = Leaf
             | Node a String (Tree a) (Tree a)
             deriving (Show, Eq, Ord)   

get_left:: Tree a -> Tree a
get_left (Node _ _ left _)    = left

get_right:: Tree a -> Tree a
get_right  (Node _ _ _ right) = right

get_id:: (Num a, Eq a,Ord a) => Tree a -> a
get_id  Leaf = -1
get_id  (Node a _ _ _) = a

max_val:: (Eq a,Ord a) => Tree a -> a
max_val (Node x _ _ Leaf) = x
max_val (Node x _ _ right) = max_val right

min_val:: (Eq a,Ord a) => Tree a -> a
min_val (Node x _ Leaf _) = x
min_val (Node x _ left _) = min_val left
      
insert:: (Num a,Eq a,Ord a, Show a) => Tree a -> a -> Tree a
insert  Leaf v = Node v (show v) Leaf Leaf
insert (Node x y left right) v 
        | v == x =  Node x y left right
        | v > x  =  (Node x y left (insert right v))
        | v < x  =  (Node x y (insert left v) right)
        
delete:: (Eq a,Ord a) => Tree a -> a -> Tree a
delete Leaf _ = Leaf
delete (Node x y left right) v 
        | v == x = reorg (Node x y left right)
        | v > x  = Node x y left (delete right v)
        | v < x  = Node x y (delete left v) right
            where 
                reorg (Node _ _ Leaf right) = right
                reorg (Node _ _ left Leaf) = left
                reorg (Node _ _ left right) = 
                    let max = max_val left in
                    Node max y (delete left max) right
            

list_to_tree:: (Num a,Eq a,Ord a, Show a) => [a] -> Tree a -> Tree a
list_to_tree [] Leaf = Leaf
list_to_tree [] tree = tree
list_to_tree (x:xs) tree = list_to_tree xs (equilibre $ insert tree x)

tree_to_list:: Tree a -> String
tree_to_list Leaf = []
tree_to_list (Node _ y left right) =
    (tree_to_list left) ++ y ++ "\n" ++ (tree_to_list right)
        
invert_tree:: Tree a -> Tree a
invert_tree Leaf = Leaf
invert_tree (Node x y left right) =
     Node x y (invert_tree right) (invert_tree left)

is_value_present:: (Eq a,Ord a) => a -> Tree a -> Bool
is_value_present _ Leaf = False
is_value_present v (Node x _ left right) 
    | x == v = True
    | v > x  = is_value_present v right
    | v < x  = is_value_present v left
    

equilibre :: (Num a, Eq a,Ord a, Show a) =>Tree a -> Tree a
equilibre (Node x y (Node lx ly lleft (Node lrx lry lrleft lrright)) right)     
        | lx < x && lx < lrx && x > lrx= Node lrx lry (Node lx ly lleft lrleft) (Node x y lrright right)    
        | otherwise = (Node x y (Node lx ly lleft (Node lrx lry lrleft lrright)) right)
        
equilibre (Node x y left (Node rx ry (Node rlx rly rlleft rlright) rright))     
        | x < rx && rx > rlx && x < rlx = Node rlx rly (Node x y left rlleft) (Node rx ry rlright rright)
        | otherwise = (Node x y left (Node rx ry (Node rlx rly rlleft rlright) rright))  
        
equilibre (Node x y (Node lx ly lleft lright) right)     
        | x > lx && lx > (get_id lleft) && ((get_id lleft) /= -1) = Node lx ly lleft (Node x y lright right)
        | otherwise = (Node x y (Node lx ly lleft lright) right) 
        
equilibre (Node x y left (Node rx ry rleft rright)) 
        | x < rx && rx < (get_id rright) && ((get_id rright) /= -1) = Node rx ry (Node x y left rleft) rright
        | otherwise = (Node x y left (Node rx ry rleft rright)) 
   
equilibre a = a
-- | otherwise = (Node x y (Node lx ly lleft lright) (Node rx ry rleft rright)) 

arcs :: Tree a -> [(String,String)]
arcs Leaf = []
arcs (Node x y Leaf Leaf) = []
arcs (Node x y  Leaf (Node x3 y3 left2 right2)) = [(y,y3)] ++ arcs Leaf ++ arcs (Node x3 y3 left2 right2)
arcs (Node x y  (Node x2 y2 left right) Leaf) = [(y,y2)] ++ arcs(Node x2 y2 left right)  ++ arcs Leaf
arcs (Node x y  (Node x2 y2 left right) (Node x3 y3 left2 right2)) = [(y,y2)] ++ [(y,y3)] ++ arcs(Node x2 y2 left right) ++ arcs(Node x3 y3 left2 right2)


arc :: (String,String) -> String
arc (a,b) = a ++ "->" ++ b

display_arcs :: Tree a -> String
display_arcs tree = unlines ( map arc (arcs tree) )

dotise :: Tree a -> String
dotise tree = "digraph \"complet3\" { \n node [fontname=\"DejaVu-Sans\", shape=circle] \n \n"++  (tree_to_list tree) ++ "\n" ++ (display_arcs tree) ++ " }"

makeTree = do        
    writeFile "tree.dot" (dotise (equilibre $ list_to_tree ([0..20]) Leaf))










