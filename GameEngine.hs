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
	newState <- customizationState
	print_state newState 0
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

-- Making Player instance of Container class 
instance Container Player where
	acquire invItem (Player name presentLocation Nothing) = Player name presentLocation (Just [invItem])	
	acquire invItem (Player name presentLocation (Just itms)) = Player name presentLocation (Just (invItem:itms))
	release invItem container@(Player name presentLocation Nothing) = container
	release invItem container@(Player name presentLocation (Just (i:[]))) = if i==invItem then Player name presentLocation Nothing else container
	release invItem container@(Player name presentLocation (Just itms)) = if invItem `elem` itms then Player name presentLocation (Just (filter (/=invItem) itms)) 
																	  else container

	empty (Player name presentLocation Nothing) = True
	empty (Player name presentLocation (Just []))=True
	empty _ = False
	contents (Player name presentLocation i) = i

-- to show inventory which player is carrying
show_inv::GameState -> IO GameState
show_inv game_state@(GameState playr p_world)=do
						putStrLn("\n=> Inventory you are carrying :: "++(show_items $ contents playr))
						return game_state 

--- to get items from location	
loc_getItem::Location -> Maybe Item
loc_getItem (Location name desc i) = i	
	
-- to show itmes if player is carrying it.
show_items :: Maybe [Item] -> String
show_items Nothing = "None"
show_items (Just items) = helperFunction items 1 "" where
	helperFunction [] num resultSoFar = resultSoFar
	helperFunction (invItem:itms) num resultSoFar= helperFunction itms (num+1) (resultSoFar++"\n"++(show num)++". "++(itemName invItem))

-- display location info
p_locDetails::Location -> String
p_locDetails(Location name desc content) = "\n=> Your current location is "++ name++". "++ desc++item where
		item = case content of Nothing  -> " "
 		                       (Just i) -> " using "++ (show i)

-- map configuration with all locations
viewMap::NavMatrix ->String
viewMap navmat1@(NavMatrix cons) = mapHelper cons 0 topRow where
		mapHelper [] count resultSoFar = resultSoFar++"\n"
		mapHelper (con:cons) count resultSoFar = mapHelper cons (count+1) (resultSoFar++"\n"++disp (locatins!!count)++" "++(con_view con))

headRow=["\nPresent Location "," North "," East "," South "," West \n"]
topRow = concat (map disp (headRow))

con_view::Connections -> String
con_view (Connections n e s w)=concat ([if l==(-1) then disp"Nothing " else disp (locatins !! l) | l<-[n,e,s,w]])

maxLength  = maximum(map length locatins)

type Locations = [String]
locatins :: Locations
locatins = ["Haunted House ","Meadow ","Hut ","Castle ","Vessle ","Cave " ,"Cave2 ","Carrot Farm ","Lake ","Hill "]


disp::String -> String
disp loc = loc ++ (set_view (length loc))

-- map view configuration
set_view::Int -> String
set_view len =  take (maxLength - len) (repeat ' ')

change_state :: GameState -> Char -> IO GameState
change_state game_state action_performed =	if action_performed == 'm' then do
													putStr (viewMap navmat1)
													return game_state
										    else if action_performed == 'n' then update_location game_state action_performed North
											else if action_performed == 's' then update_location game_state action_performed South
											else if action_performed == 'e' then update_location game_state action_performed East
											else if action_performed == 'w' then update_location game_state action_performed West
											else if action_performed == 't' then take_item game_state
											else if action_performed == 'd' then drop_item game_state
											else if action_performed == 'i' then show_inv game_state
											else return game_state
			

take_item::GameState -> IO GameState
take_item game_state@(GameState playr p_world) = ifValid  $ loc_getItem $ ((locations p_world)!!(presentLocation $ player game_state)) where
	ifValid (Just invItem) =do
				putStrLn("\n=> The item you acquired from this location is ::  "++ (show invItem))				
				return $ GameState (acquire invItem playr) (world_takeItem p_world (presentLocation $ player game_state)) 
	ifValid Nothing = do 
				hPutStr stderr("\n=> There is no item currently present in this location." ++
								   " \n Move to other location to find one.\n")
				return game_state
	
drop_item::GameState -> IO GameState
drop_item game_state@(GameState playr p_world) = ifValid (inventory playr) where
 ifValid (Just itms) = do 
	hPutStr stderr("=> Choose item number to drop it ::")
	itemNumber <- getLine
	dropForValid itemNumber where
		dropForValid "" = do
				hPutStr stderr("\n=> No such Command exists, select from above given list of commands.")
				getLine
				return game_state
		dropForValid itemNumber = if (digitToInt (head itemNumber)) > (length itms) then do
																hPutStr stderr("\n=> Not a valid number.\n")
																return game_state
								  else do
									iDrop <- return $ getDropItem playr itemNumber
									putStrLn("\n=> The item you released/dropped is :: "++(show iDrop))
									return $ GameState (release iDrop playr) (world_addItem p_world iDrop (presentLocation playr)) 
 ifValid Nothing = do
			hPutStr stderr("\n=> Can't perform drop operation as you're not carrying anything.\n")
			return game_state

		
											
print_state :: GameState -> Int -> IO GameState
print_state game_state count = do
			dead <- end_conditions game_state
			action dead where
			action "continue"= do		 		
				hPutStr stderr(
					" \n" ++
					"***********************************\n" ++
					("* You have remaining Life:: "++show (subtract count lifePercentage)++" % *\n")++

					"***********************************\n" ++
							"Select the command to perform operation.\n" ++
		   		    "[m]ap       - To view the map.\n" ++
		  		    "[n]orth     - Move towards North Direction.\n" ++
		  		    "[s]outh     - Move towards South Direction.\n" ++
		 		    "[e]ast      - Move towards East Directiion.\n" ++
		 		    "[w]est      - Move towards West Direction.\n" ++
		   		    "[t]ake      - To take the item/object .\n" ++
		   		    "[d]rop      - To drop the itme/object.\n" ++
		  		    "[l]ook      - To check/look your surrounding.\n" ++
		   		    "[i]nventory - To see what you have in your hand.\n" ++
		  		    "[q]uit      - To terminate/quit the game.\n" ++
		  		    "     \n"++
		  		    "Command/action ::  "
		  		    )

				actionCommand <- getLine
				checkPlay actionCommand count where			
					  checkPlay actionCommand count = if count == lifePercentage then do
																					putStrLn("\n=> you don't any remaining life. try again.")
																					return game_state
														else if  actionCommand == "" then do
																hPutStr stderr("\n=> No such operation/command exists, please select from above given list of commands.")
																handle1<- getLine
																print_state game_state count
														else if (elem (head actionCommand) ['n','s','e','w']) then do
																		newState <- change_state game_state $ head actionCommand
																		outputLocDetails newState
																		print_state newState (count+5)
														else if (elem (head actionCommand) ['i','t','d','m']) then do
																		newState <- change_state game_state $ head actionCommand
																		print_state newState count
														else if (head actionCommand) =='l' then do
																	outputLocDetails game_state
																	print_state game_state count
														else if  head actionCommand == 'q' then return game_state
														else do
																hPutStr stderr("\n=> No such operation/command exists, please select from above given list of commands.")
																handle2 <- getLine
																print_state game_state count
			action _ = return game_state
						

																
getDropItem playr itemNum = getItems (inventory playr) where
		getItems (Just itms) = (itms !! ((digitToInt (head itemNum))-1))										
			

customizationState :: IO GameState
customizationState = do
			hPutStr stderr("\nEnter name =>  ")
			name <- getLine
			hPutStr stderr("\n || WELCOME " ++name++" TO MAROONED ON ISLAND GAME. ||\n")
			return (GameState (Player name 1 Nothing) worldDetails)

							   
type Life = Int
lifePercentage :: Life
lifePercentage=100

outputLocDetails::GameState -> IO ()
outputLocDetails game_state@(GameState playr p_world)=	putStrLn $ p_locDetails $ (locations p_world)!!(presentLocation playr)
update_location :: GameState -> Char -> Dir -> IO GameState
update_location game_state@(GameState (Player pname presentLocation inventory) world) action_performed toDir = do
	return $ GameState (Player pname (validateLoc $ fromto presentLocation toDir navmat1) inventory) world
	where validateLoc newLocation = if newLocation==(-1) then presentLocation else newLocation
 					
-- Adding/ removing item from world.
world_takeItem::World -> Int -> World
world_takeItem (World name desc locs) i = World name desc ((take i locs)++[Location (locName (locs!!i)) (locDesc (locs!!i)) Nothing]++ (drop (i+1) locs)) 

world_addItem::World -> Item -> Int -> World
world_addItem (World name desc locs) invItem i = World name desc ((take i locs)++[Location (locName (locs!!i)) (locDesc (locs!!i)) (Just invItem)]++ (drop (i+1) locs))

-- defined max count for inventory player carrying
type MaxItem = Int
maximumInv :: MaxItem
maximumInv= 9

-- condition to check whether player won or not.
end_conditions  :: GameState -> IO String
end_conditions game_state@(GameState playr p_world) = p_endCond ((locations p_world)!!(presentLocation playr)) where
			p_endCond loc@(Location name desc content) = if name==" Cave" then deadCondition (inventory playr) else return "continue" where	
															deadCondition Nothing = do
																	putStr "\n** But Lion ate you. You don't have sword to kill it.**\n" 
																	return "dead"						
															deadCondition (Just itms) = if  "Sword" `elem` (map itemName itms)  then winCondition else do
																					putStr "\n  ** But Lion ate you. You don't have sword to kill it.**" 
																					return "dead" where 
																						winCondition = if (length itms) == maximumInv then do
																														putStrLn "\n@***********YOU WON************\n"
																														return "You won"  else return "continue"
														
