module World where


import NavMatrix
import Location
import Item


data World = World { worldName::String,
			worldDesc::String,
			locations::[Location]}

worldDetails = World { worldName="Iceland",
		worldDesc = "Iceland is marooned, isolated and dangerous.",
		locations = [
					Location "Haunted House" "There is some food here.\n   In iceland you can live longer " (Just(Item "Food" "To feed yourself")),
					Location "Meadow" "There you found lost satellite phone. \n   Take it and Call for help" (Just(Item "Satellite Phone" "Can call, to get help")), 
					Location "Hut" "You can cook food " (Just(Item "Firewood" "To cook food.")), 
					Location "Castle" "There is golden sword inside it. \n   You can kill Lion using it" (Just(Item "Sword" "To kill lion.")),
					Location "Vessle" "It has some sweet water jars. You can quench your thirst" (Just(Item "Water Jars" "Quench thirst.")),
					Location "Cave" "There is wild Lion inside cave\n and also there is Map inside it. you can get directions using" (Just(Item "Map" "To get Directions")),
					Location "Cave2" "Cave2 was house of some tribes. They left \n   the place. There are some woolen cloths. You can keep yourself\n   warm" (Just (Item "Woolen Cloths" "Help to stay warm & cover body.")),
					Location "Carrot Farm" "Take carrots with you. You can get energy" (Just (Item "Carrots" "Keep power up.")),
					Location "Lake" "You can drink water here. \n   You can satisfy your thirst " (Just (Item "Water" "For drinking.")),
					Location "Hill" "There is bag on hill. you can view large area of iceland" (Just (Item "Bag" "to carry stuff."))
				   ]}
