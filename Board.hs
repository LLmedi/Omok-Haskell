{-
	Title: Omok Game
	Created by: Luis Medina
	Description: Two players take turns placing stones on a 15 by 15 
	board. The game ends when one player has 5 of their stones placed
	consecutively in a row, column or diagonal. In the event that the 
	board is full of stones and there is no sequence of winning stones,
	the game ends in a draw.

-}

module Board where
	
	import System.Exit
	--import Main
	
	--Creates a board of size n*n
	mkBoard :: Int -> [Int]
	mkBoard n = take (n*n) (repeat 0)
	
	--Denotes the first player
	mkPlayer = 1
	
	--Denotes the second player
	mkOpponent = 2
	
	--Finds the size (n) of an n*n board
	size :: [Int] -> Int
	size bd = (floor . sqrt . fromIntegral . length) bd
	
	--Finds a specific row of a board
	row :: Int -> [Int] -> [Int]
	row y bd = take (size bd) (drop index bd)
		where index = ((y * size bd)-((size bd) -1)) -1
		
	--Finds a specific column of a board
	col :: Int -> [Int] -> [Int]
	col x bd 
	--	|x<1 || x>(length bd)  = []
		|(x + (size bd)) <= (length bd) = bd !! (x-1) : col (x + size bd) bd
		|otherwise = bd !! (x-1) : []
	
	--Places a stone with the value of the given player at the specified coordinate
	mark :: Int -> Int -> [Int] -> Int -> [Int]
	mark x y bd p = 
		let (hbd, tbd) = splitAt ( (y*(size bd)) - ((size bd)- x)) bd in
			(init hbd) ++ [p] ++ tbd
	
	--Determines if a coordinate has no stone
	isEmpty :: Int -> Int -> [Int] -> Bool
	isEmpty x y bd
		| ((row y bd) !! (x-1) )== 0 = True
		| otherwise = False
	
	--Checks to see if a coordiniate is marked by a player
	isMarked :: Int -> Int -> [Int] -> Bool
	isMarked x y bd
		|((row y bd) !! (x-1) ) /= 0 = True
		|otherwise = False
	
	--Checks to see if player p marked a coordinate
	isMarkedBy :: Int -> Int -> [Int] -> Int -> Bool
	isMarkedBy x y bd p
		|((row y bd) !! (x-1) ) == p = True
		|otherwise = False
	
	--Returns the player that marked a coordinate
	marker :: Int -> Int -> [Int] -> Int
	marker x y bd 
		|((row y bd) !! (x-1) ) == 1 = 1
		|((row y bd) !! (x-1) ) == 2 = 2
		|otherwise = 0
	
	--Checks a given board to see if the board has a stone in every coordinate
	isFull :: [Int] -> Bool
	isFull (h:t)
		|t == [] && h==0 = False
		|t == [] && h/=0 = True
		|t /= [] && h == 0 = False
		|otherwise = True && isFull t

	--Checks if player p won the game
	isWonBy :: [Int] -> Int -> Int -> Bool
	isWonBy bd cur p = (checkAllRows bd cur p)||(checkAllCols bd cur p) || (checkAllDiags bd cur p)

	--Checks to see if the game ended in a draw
	isDraw :: [Int] -> Int -> Bool
	isDraw bd cur = (isWonBy bd cur 1) && (isWonBy bd cur 2)
	
	--Checks to see if a player won or the game is tied.
	isGameOver :: [Int] -> Int -> Bool
	isGameOver bd cur
		|isWonBy bd cur 1 || isDraw bd cur = True
		|isWonBy bd cur 2 || isDraw bd cur = True
		|otherwise = False
		

	--Checks for 5 consecutive '1's or '2's in a given list
	checkCons :: [Int] -> Int -> Int -> Int
	checkCons (h:t) p c
		|c >= 5 = 5
		|(h == p) && (t == []) = c+1
		|(h == p) && (t /= []) = checkCons t p (c+1)
		|(h /= p) && (t /= []) = checkCons t p 0
		|(h /= p) && (t == []) = 0

	--Returns a specified diagonal row
	getDiag :: [Int] -> Int -> Int -> [Int]
	getDiag bd x y
		|y < (size bd) = (row y bd) !! (x-1) : getDiag bd (x+1) (y+1)
		|otherwise = (row y bd) !! (x-1) : []
	
	--Checks all rows for a winning placement of stones
	checkAllRows :: [Int] -> Int -> Int -> Bool
	checkAllRows bd cur p
		|cur < (size bd) = ((checkCons (row cur bd) p 0) == 5 ) || (checkAllRows bd (cur+1) p)
		|cur == (size bd) = ((checkCons (row cur bd) p 0) == 5)
		|otherwise = False
		
	--Checks all columns for a winning placement of stones
	checkAllCols :: [Int] -> Int -> Int -> Bool
	checkAllCols bd cur p
		|cur < (size bd) = ((checkCons (col cur bd) p 0) == 5 ) || (checkAllRows bd (cur+1) p)
		|cur == (size bd) = ((checkCons (col cur bd) p 0) == 5)
		|otherwise = False
	
	--Checks all diagonals for a winning placement of stones
	checkAllDiags :: [Int] -> Int -> Int -> Bool
	checkAllDiags bd cur p
		|cur < (size bd) = ((checkCons (getDiag bd cur cur) p 0) == 5 ) || (checkAllRows bd (cur+1) p)
		|cur == (size bd) = ((checkCons (getDiag bd cur cur) p 0) == 5)
		|otherwise = False
	
-----------------------------------------------------------------------------
	--Checks a coordinate and returns the string representation of that player or empty spot
	playerToString :: [Int] -> Int -> Int -> String
	playerToString bd x y
		|marker x y bd == 1 = "O"
		|marker x y bd == 2 = "X"
		|otherwise = "."
	
	--Creates 'y-----' part of board
	prepStr2 :: [Int] -> Int -> String
	prepStr2 bd c
		|c == 0 = "y " ++ prepStr2 bd (c+1)
		|c == size bd = "--" ++ "\n"
		|c < size bd = "--" ++ prepStr2 bd (c+1)
	
	--Creates the top row denoting board place values and appends result of prepStr2
	prepStr1 :: [Int] -> Int -> String
	prepStr1 bd c
		|c == 0 = " x" ++ (prepStr1 bd (c+1))
		|c == size bd = " " ++ (show (c `mod ` 10)) ++ "\n" ++ prepStr2 bd 0
		|c < size bd = " " ++ (show (c `mod ` 10)) ++ prepStr1 bd (c+1)

	--Given a board, will return the string representation to be printed
	boardToStr :: [Int] -> Int -> Int -> String
	boardToStr bd y x
		|y == 1 && x == 1 = (prepStr1 bd 0) ++ show (y`mod`10) ++ "| " ++ (playerToString bd x y) ++ (boardToStr bd y (x+1))
		|y <  size bd && x == 1 = show (y`mod`10) ++ "| " ++ (playerToString bd x y) ++ (boardToStr bd y (x+1))
		|y <  size bd && x < size bd = " " ++ (playerToString bd x y) ++ (boardToStr bd y (x+1))
		|y <  size bd && x == size bd = " " ++ (playerToString bd x y) ++ "\n" ++ boardToStr bd (y+1) 1
		|y == size bd && x == 1 = show (y`mod`10) ++ "| " ++ (playerToString bd x y) ++ (boardToStr bd y (x+1))
		|y == size bd && x < size bd = " " ++ (playerToString bd x y) ++ (boardToStr bd y (x+1))
		|y == size bd && x == size bd = " " ++ (playerToString bd x y)		
	
	
	
	
	
	
	
	
	
	
	
	
	
