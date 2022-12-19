Module to define the type of a maze

> module MyMaze2 (
>   Maze, 
>   makeMaze, -- :: Size -> [Wall] -> Maze
>   hasWall,  -- :: Maze -> Place -> Direction -> Bool
>   sizeOf,    -- :: Maze -> Size
>   northWalls,
>   makeTree
> )
> where

> import Geography
> import Data.List
> import Debug.Trace

We will represent a maze by its size and a list of its walls.

> data Maze = Maze Size (Tree Place) (Tree Place) (Tree Place) (Tree Place)
> data Tree a = Empty | Node a (Tree a) (Tree a)

The list of walls will be complete in the sense that we record
both sides of the wall; for example, if the list includes 
((3,4), N), then it will also include ((3,5),S).

This function creates a maze given its size and a list of walls; 
the list of walls might not be complete in the above sense.

> makeTree :: [a] -> Tree a
> makeTree [] = Empty
> makeTree (xs) = (Node (x!!0) (makeTree l) (makeTree r))
>     where (l,x,r) = split (xs)
>
> makeList :: Tree a -> [a]
> makeList Empty = []
> makeList (Node x l r) = (makeList l) ++ [x] ++ (makeList r)
> split [] = ([],[],[])
> split [x] = ([],[x],[])
> split xs = ((take (half) (xs)), [(xs)!!half], (drop (half+1) (xs)))
>     where half = div (length (xs)) 2
> printTree :: Show a => Tree a -> Int -> String
> printTree Empty layer = indent layer ++ "Empty"
> printTree (Node x l r) layer = indent layer ++ show(x) ++ 
>                                "\n" ++ (printTree l (layer + 1)) ++
>                                "\n" ++ (printTree r (layer + 1))
>
> instance Show a => Show (Tree a) where
>     show Empty = "Empty"
>     show (Node x l r) = printTree (Node x l r) 0
> indent 0 = ""
> indent n = "    " ++ indent (n-1)
>
> searchTree :: Ord a => Show a => a -> Tree a -> Bool
> searchTree n Empty = False
> searchTree n (Node x l r) | n == x    = True
>                           | n < x     = (searchTree n l)
>                           | otherwise = (searchTree n r)
> nw = northWalls smallMaze
> ww = westWalls smallMaze
> makeMaze :: Size -> [Wall] -> Maze
> makeMaze (x,y) walls =
>   let checkDir dir wall = dir == snd wall
>       nw1 = (filter (checkDir N) walls) ++ [((i,y-1), N) | i <- [0..x-1]]
>       ew1 = (filter (checkDir E) walls) ++ [((x-1,j), E) | j <- [0..y-1]]
>       sw1 = (filter (checkDir S) walls) ++ [((i,0),   S) | i <- [0..x-1]]
>       ww1 = (filter (checkDir W) walls) ++ [((0,j),   W) | j <- [0..y-1]]
>       nw = nw1 ++ (map reflect (filter ((> 0).snd.fst) sw1))
>       ew = ew1 ++ (map reflect (filter ((> 0).snd.fst) ww1))
>       sw = sw1 ++ (map reflect (filter ((< (y-1)).snd.fst) nw1))
>       ww = ww1 ++ (map reflect (filter ((< (x-1)).snd.fst) ew1))
>       northWalls = makeTree(sort [fst wall | wall <- nw])
>       eastWalls = makeTree(sort [fst wall | wall <- ew])
>       westWalls = makeTree(sort [fst wall | wall <- ww])
>       southWalls = makeTree(sort [fst wall | wall <- sw])
>  in Maze (x,y) northWalls eastWalls southWalls westWalls

The following function "reflects" a wall; i.e. gives the representation as
seen from the other side; for example, reflect ((3,4), N) = ((3,5),S)

> reflect :: Wall -> Wall
> reflect ((i,j), d) = (move d (i,j), opposite d)

The following function tests whether the maze includes a wall in a particular
direction from a particular place:

> hasWall :: Maze -> Place -> Direction -> Bool
> hasWall (Maze _ n e s w) pos N = searchTree pos n
> hasWall (Maze _ n e s w) pos E = searchTree pos e
> hasWall (Maze _ n e s w) pos S = searchTree pos s
> hasWall (Maze _ n e s w) pos W = searchTree pos w

The following function returns the size of a maze:

> sizeOf :: Maze -> Size
> sizeOf (Maze size _ _ _ _) = size
> northWalls (Maze _ n e s w) = n
> eastWalls (Maze _ n e s w) = e
> southWalls (Maze _ n e s w) = s
> westWalls (Maze _ n e s w) = w
>
> smallMaze :: Maze
> smallMaze = 
>   let walls = [((0,0), N), ((2,2), E), ((2,1),E), ((1,0),E), 
>                ((1,2), E), ((1,1), N)]
>   in makeMaze (4,3) walls
> walls1 = [((0,0), N), ((2,2), E), ((2,1),E), ((1,0),E), 
>                ((1,2), E), ((1,1), N)]
