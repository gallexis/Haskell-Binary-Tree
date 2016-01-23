
-- Binary Tree
data Tree a = Leaf
             | Node a (Tree a) (Tree a)
             deriving Show 

get_left:: Tree a -> Tree a
get_left  (Node _ left _)   = left

get_right:: Tree a -> Tree a
get_right  (Node _ _ right) = right

get_value:: Tree a -> a
get_value  (Node a _ _) = a

max_val:: (Eq a,Ord a) => Tree a -> a
max_val (Node x _ Leaf) = x
max_val (Node x _ right) = max_val right

min_val:: (Eq a,Ord a) => Tree a -> a
min_val (Node x Leaf _) = x
min_val (Node x left _) = min_val left
      
insert:: (Eq a,Ord a) => Tree a -> a -> Tree a
insert (Leaf) v = Node v Leaf Leaf
insert (Node x left right) v 
        | v == x = (Node x left right)
        | v > x  = Node x left (insert right v)
        | v < x  = Node x (insert left v) right
        
delete:: (Eq a,Ord a) => Tree a -> a -> Tree a
delete Leaf _ = Leaf
delete (Node x left right) v 
        | v == x = reorg (Node x left right)
        | v > x  = Node x left (delete right v)
        | v < x  = Node x (delete left v) right
            where 
                reorg (Node _ Leaf right) = right
                reorg (Node _ left Leaf) = left
                reorg (Node _ left right) = 
                    let max = max_val left in
                        (Node max (delete left max) right)
            

list_to_tree:: (Eq a,Ord a) => [a] -> Tree a -> Tree a
list_to_tree [] Leaf = Leaf
list_to_tree [] tree = tree
list_to_tree (x:xs) tree = list_to_tree xs (insert tree x)

tree_to_list:: Tree a -> [a]
tree_to_list Leaf = []
tree_to_list (Node x left right) =
    x:(tree_to_list left)++(tree_to_list right)
        
invert_tree:: Tree a -> Tree a
invert_tree Leaf = Leaf
invert_tree (Node x left right) =
        Node x (invert_tree right) (invert_tree left)

is_value_present:: (Eq a,Ord a) => a -> Tree a -> Bool
is_value_present _ Leaf = False
is_value_present v (Node x left right) 
    | x == v = True
    | v > x  = is_value_present v right
    | v < x  = is_value_present v left
