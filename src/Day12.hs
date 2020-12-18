{-# LANGUAGE RecordWildCards #-}
module Day12 where

import qualified Text.Parsec as P
import           Text.Parsec (Parsec)
import Parse
import Common
import Control.Monad.State
import Debug.Trace
import Linear ((*^), V2(V2))

day12a :: String -> Int
day12a = solveA . dayInput

solveA :: [Action] -> Int
solveA as = abs x + abs y
    where V2 x y = evalState (run doActionA as) initBoat

type Ferry = State Boat Point

day12b :: String -> Int
day12b = solveB . dayInput

solveB :: [Action] -> Int
solveB as = abs x + abs y
    where V2 x y = evalState (run doActionB as) initBoat


run :: (Action -> Boat -> Boat) -> [Action] -> Ferry
run doAction = foldr (withState . doAction) (gets _point)

type Point = V2 Int 
data MvDir = N | E | W | S
    deriving (Show, Eq)
data TnDir = R | L
    deriving (Show, Eq)
data Action = Mv MvDir Int | Tn TnDir Int | Fwd Int
    deriving (Show, Eq)
data Boat = B
    { _point  :: Point
    , _facing :: MvDir
    , _waypoint :: Point
    } deriving (Show, Eq)

initBoat :: Boat
initBoat = B{_point=V2 0 0,_facing=E,_waypoint=V2 10 1}

doActionB :: Action -> Boat -> Boat
doActionB (Fwd n) b@B{..} = moveBoatTowards _waypoint n b
doActionB (Tn dir deg) b  = turnWaypoint dir deg b
doActionB (Mv dir n  ) b  = moveWaypoint (dirToPoint dir) n b

doActionA :: Action -> Boat -> Boat
doActionA (Fwd n) b@B{..} = moveBoatTowards (dirToPoint _facing) n b
doActionA (Tn dir deg) b  = turnBoat dir deg b
doActionA (Mv dir n  ) b  = moveBoatTowards (dirToPoint dir) n b

turnBoat :: TnDir -> Int -> Boat -> Boat
turnBoat dir deg b@B{..}
    | deg' == 0   = b
    | deg' == 90  = b{_facing=pointToDir $ V2   dy  (-dx)}
    | deg' == 180 = b{_facing=pointToDir $ V2 (-dx) (-dy)}
    | deg' == 270 = b{_facing=pointToDir $ V2 (-dy)   dx }
    where deg' = if dir == L then (-deg) `mod` 360 else deg `mod` 360
          V2 dx dy = dirToPoint _facing

turnWaypoint :: TnDir -> Int -> Boat -> Boat
turnWaypoint dir deg b@B{..}
    | deg' == 0   = b
    | deg' == 90  = b{_waypoint=_waypoint * V2   1  (-1)}
    | deg' == 180 = b{_waypoint=_waypoint * V2 (-1) (-1)}
    | deg' == 270 = b{_waypoint=_waypoint * V2 (-1)   1 }
    where deg' = if dir == L then (-deg) `mod` 360 else deg `mod` 360

moveBoatTowards :: Point -> Int -> Boat -> Boat
moveBoatTowards p n b@B{..} = b{_point=_point + (n *^ p)}

moveWaypoint :: Point -> Int -> Boat -> Boat
moveWaypoint p n b@B{..} = b{_waypoint=_waypoint + (n *^ p)}

toPoint :: Char -> Point
toPoint 'N' = V2   0   1
toPoint 'S' = V2   0 (-1)
toPoint 'W' = V2 (-1)  0
toPoint 'E' = V2   1   0

dirToPoint :: MvDir -> Point
dirToPoint N = V2   0   1
dirToPoint E = V2   1   0
dirToPoint S = V2   0 (-1)
dirToPoint W = V2 (-1)  0


pointToDir :: Point -> MvDir
pointToDir (V2   0   1 ) = N
pointToDir (V2   0 (-1)) = S
pointToDir (V2   1   0 ) = E
pointToDir (V2 (-1)  0 ) = W

toDeg :: MvDir -> Int
toDeg N = 0
toDeg S = 180
toDeg W = 270
toDeg E = 90

fromDeg :: Int -> MvDir
fromDeg 0 = N
fromDeg 90 = E
fromDeg 180 = S
fromDeg 270 = W

dayInput :: String -> [Action]
dayInput = unsafeParse actions

actions :: Parsec String () [Action]
actions = P.sepEndBy1 action P.newline

action :: Parsec String () Action
action = do 
    c <- P.anyChar
    d <- parseInt
    return $ case c of
        'N' -> Mv N d
        'E' -> Mv E d
        'S' -> Mv S d
        'W' -> Mv W d
        'F' -> Fwd d
        'L' -> Tn L d
        'R' -> Tn R d