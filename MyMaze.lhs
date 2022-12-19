Module to define the type of a maze

> module MyMaze (
>   Maze, 
>   makeMaze, -- :: Size -> [Wall] -> Maze
>   hasWall,  -- :: Maze -> Place -> Direction -> Bool
>   sizeOf,    -- :: Maze -> Size
> )
> where

> import Geography

We will represent a maze by its size and a list of its walls.

> data Maze = Maze Size [Place] [Place] [Place] [Place]

The list of walls will be complete in the sense that we record
both sides of the wall; for example, if the list includes 
((3,4), N), then it will also include ((3,5),S).

This function creates a maze given its size and a list of walls; 
the list of walls might not be complete in the above sense.

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
>       northWalls = [fst wall | wall <- nw] 
>       eastWalls = [fst wall | wall <- ew]
>       westWalls = [fst wall | wall <- ww]
>       southWalls = [fst wall | wall <- sw]
>  in Maze (x,y) northWalls eastWalls southWalls westWalls

The following function "reflects" a wall; i.e. gives the representation as
seen from the other side; for example, reflect ((3,4), N) = ((3,5),S)

> reflect :: Wall -> Wall
> reflect ((i,j), d) = (move d (i,j), opposite d)

The following function tests whether the maze includes a wall in a particular
direction from a particular place:

> hasWall :: Maze -> Place -> Direction -> Bool
> hasWall (Maze _ n e s w) pos N = elem pos n
> hasWall (Maze _ n e s w) pos E = elem pos e
> hasWall (Maze _ n e s w) pos S = elem pos s
> hasWall (Maze _ n e s w) pos W = elem pos w

The following function returns the size of a maze:

> sizeOf :: Maze -> Size
> sizeOf (Maze size _ _ _ _) = size



