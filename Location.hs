module Location where

import Item


data Location = Location {
				locName:: String
			   ,locDesc:: String
			   ,locContent:: Maybe Item
				}
