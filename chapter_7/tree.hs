import qualified Data.Map as Map

-- the base BST implementation

data Tree d = Empty | Node d (Tree d) (Tree d) deriving (Show, Read, Eq)

insertIntoTree :: (Ord d) => d -> Tree d -> Tree d
insertIntoTree d Empty = Node d Empty Empty
insertIntoTree d (Node value l r)
    | d == value = Node d l r
    | d > value = Node value (insertIntoTree d l) r
    | d < value = Node value l (insertIntoTree d r)

existsInTree :: (Ord d) => d -> Tree d -> Bool
existsInTree d Empty = False
existsInTree d (Node value l r)
    | d == value = True
    | d > value = existsInTree d r
    | d < value = existsInTree d l

instance (Eq m) => Eq (Maybe m) where  
    Just x == Just y = x == y  
    Nothing == Nothing = True  
    _ == _ = False  

instance Functor (Map.Map k) where
    fmap f (Map.Map k v) = Map.Map k $ f v