{-
	Title: Omok Game
	Created by: Luis Medina
	Description: Two players take turns placing stones on a 15 by 15 
	board. The game ends when one player has 5 of their stones placed
	consecutively in a row, column or diagonal. In the event that the 
	board is full of stones and there is no sequence of winning stones,
	the game ends in a draw.

-}

module Main where

	import System.IO
	import System.Random
	import System.Exit
	import Board
		
	--getX :: IO [(Integer, String)]
	--Parses an integer from user input string
	getNum = do 
		putStrLn "Enter thing"
		line <- getLine
		let parsed = reads line :: [(Int, String)] in
			if length parsed == 0 
			then getNum'
			else let (x,_) = head parsed in 
				if x > 0 
				then return x
				else if x == -1
				then exitFailure
				else getNum'
		where
			getNum' = do
				putStrLn "Invalid input!"
				getNum
	
	--Gets X and Y values from player input string and returns a printable tuple
	readXY :: [Int] -> Int -> IO(Int, Int)
	readXY bd p = do
		putStr ("Player " ++ show p ++ "'s turn: enter x (1-15 or -1 to quit)")
		x <- getNum
		putStr ("Player " ++ show p ++ "'s turn: enter y (1-15 or -1 to quit)")
		y <- getNum
		if isEmpty x y bd then return (x, y)
		else do
			putStrLn "There's a stone there already!"
			readXY bd p
	
	--Function that starts the game. Creates a board and manages player turns
{-	main = do
		let board = mkBoard 15
		--Player 1 turn
		turn mkPlayer board
		turn2 mkPlayer turn
		
		main
-}	
	--Logic sequence of player p's turn
	turn p bd = do
		move <- readXY bd p
		let bd' = mark(fst move) (snd move) bd p
		printBd bd'
		--checkIfWon bd' p
		return bd'
	--Prints board representation to terminal
	printBd bd = putStr (boardToStr bd 1 1)
	
	
	
	