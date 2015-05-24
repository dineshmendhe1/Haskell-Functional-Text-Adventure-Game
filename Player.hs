module Player where

import Location
import Item 
import Data.Maybe


class Container cont where
       acquire :: Item -> cont -> cont 
       release :: Item -> cont -> cont
       empty :: cont -> Bool 
       contents :: cont -> Maybe [Item] 



data Player = Player { playerName :: String
					  ,presentLocation::Int
                      ,inventory::Maybe[Item]
                     }  
