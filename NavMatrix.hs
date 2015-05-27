module NavMatrix where

data Dir = North | East | South | West | None deriving (Show,Eq)

opposite :: Dir -> Dir
opposite North = South
opposite South = North
opposite East = West
opposite West = East

type Matrix a = [[a]]

getCell :: (Int,Int) -> Matrix a -> a
getCell (row,col) = (!! col) . (!! row)

setElem :: Int -> a -> [a] -> [a]
setElem i x xs = prec++x:succ where
    (prec,targ:succ) = splitAt i xs

setCell :: (Int,Int) -> a -> Matrix a -> Matrix a
setCell (row,col) x xs = setElem row (setElem col x (xs !! row)) xs

type LocId = Int

data Connections = Connections { north :: LocId
                                 ,east  :: LocId
                                 ,south :: LocId
                                 ,west  :: LocId } deriving (Show,Eq)

newtype NavMatrix = NavMatrix { conns :: [Connections] } deriving Eq

instance Show NavMatrix where
    show (NavMatrix m) = concat $ map (\x -> (show x)++"\n") m

connectTo :: Dir -> LocId -> Connections -> Connections
connectTo North to row = row{north=to}
connectTo South to row = row{south=to}
connectTo East  to row = row{east =to}
connectTo West  to row = row{west =to}

connect :: Dir -> LocId -> LocId -> NavMatrix -> NavMatrix
connect dir from to (NavMatrix mat) = NavMatrix mat'' where
    (precRows,targRow:succRows) = splitAt from mat
    newConn = connectTo dir to targRow
    mat' = precRows ++ newConn:succRows
    (precRows',targRow':succRows') = splitAt to mat'
    newConn' = connectTo (opposite dir) from targRow'
    mat'' = precRows' ++ newConn':succRows'

fromto :: LocId -> Dir -> NavMatrix -> LocId
fromto from dir = getdir . (!! from) . conns where
    getdir = case dir of North -> north
                         East  -> east
                         South -> south
                         West  -> west

no = (-1)
unconnected = Connections no no no no


emptyMap :: Int -> NavMatrix
emptyMap nlocs = NavMatrix $ take nlocs $ repeat unconnected


navmat1 = NavMatrix [
    Connections  1  2  6  9,  
    Connections  7  8  0 no,  
    Connections no  6  3  0,  
    Connections  2  5  8  4,  
    Connections  5  3  9 no,  
    Connections  1  3  5  3,  
    Connections  7  4  2  2,  
    Connections  0  8  6 no,  
    Connections  3  3  2  9,  
    Connections  4  8  1 no   
    ]
navmat2 = ((connect North 0 1)
         . (connect East  0 2)
         . (connect East  2 6)
         . (connect South 2 3)
         . (connect East  3 5)
         . (connect South 3 8)
         . (connect West  3 4)
         . (connect South 4 9)
         . (connect North 6 7)
         . (connect West  8 9)
         . emptyMap) 10
