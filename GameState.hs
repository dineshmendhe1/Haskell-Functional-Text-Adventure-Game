module GameState where

import Player
import Item
import Location
import World




data GameState = GameState {
			player::Player					
			,world::World
			}
