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
