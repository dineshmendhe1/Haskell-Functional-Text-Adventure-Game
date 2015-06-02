import GameState
import Item
import Location
import Player
import World
import NavMatrix
import Data.Char
import System.IO



--- introduction of game
type Story = String 
story:: Story
story = "                     ||Story of the Game||\n"++
                    "----------------------------------------------------------------\n"++
                    "The player has lost his Satellite phone on Marooned Island.On the\n"++
                    "iseland there is Haunted House, Castle, two  caves, Lake,  Hill,\n"++
                    "Carrot farm and Vessle. There is LION in one of the Cave. Player\n"++
                    "has to collect all life necessary items from  each  location to \n"++
                    "survive. Using  satellite phone  he can call  rescue department \n"++
                    "for help. \n"++
                    "\n "++
                    "     IN ORDER TO WIN PLAYER HAS TO KILL LION AND HE HAS TO     \n"++
                    "         COLLECT ALL ITEMS BEFORE END OF HIS LIFE POINTS.       \n"++
                    "\n"++
                    "INITIAL LOCATION :: MEADOW "++"        LIFE PERCENTAGE  :: 100 %\n"++
                    
                    "----------------------------------------------------------------"
--  introduction conf line
type Intro = String
intro :: Intro 
intro =                 "\n======================================================================="
                      ++ "\n    Please Enter the Player Name to play MAROONED ON ISLAND              "
                      ++ "\n======================================================================\n"


displayIntro :: IO ()
displayIntro = do 
				hPutStr stderr story 
				hPutStr stderr "\n"
				hPutStr stderr intro 
				

displayState :: GameState -> IO ()
displayState game_state = putStrLn (show game_state)


-- exit message 
displayOutro :: IO ()
displayOutro = putStrLn $ "\n@@----------Game Over------------@@"
                      ++ "\n===================================="
                      ++ "\nCopyright (c) 2015 || Dinesh Mendhe"
                      ++ "\n====================================\n"

-- gaem intialization using main
main :: IO()
main = do
	displayIntro
--newState <- customizationState
--print_state newState 0
	displayOutro

-- Making GameState instance of Show typeclass
instance Show GameState where
	show (GameState player world) = " \n "++(show player)++""++(show  world)
	

-- Making Item instance of Show typeclass
instance Show Item where
	show (Item name desc) = (name)

-- Making World instance of Show typeclass 
instance Show World where
	show (World name desc locs)= " World => "++name
	

-- Making Location instace of Show typeclass 					
instance Show Location where
	show(Location name desc content) = "\n Location => "++(show name)++" Description => "++(show desc) ++" Contents => "++item where
		item = case content of Nothing  -> ""
 		                       (Just i) -> show content

	
	
-- Making Player instance of Show typeclass               
instance Show Player where
	show(Player name presentLocation inventory)= "\n Player => "++name++item where
		item = case inventory of Nothing  -> ""
 		                         (Just i) -> " Inventory => "++(concat $ fmap show i)
