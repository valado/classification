module Main where

-- ******************************** --
-- @author: Vladimir Daskalov       --
-- @date  : 5.1.2011                --
-- ******************************** --

import System.IO             -- import standart template library import standart template libraryreading and writing data to files
import System.IO.Unsafe
import System.Random         -- import standart template library for work random generated values
import Data.List             -- import standart template library for the parser
import Control.Arrow(second) -- import standart template library for the parser
import Directory             -- import standart template library for removeFile
import RecognitionLibrary    -- import the specific data structures and the initial data

--___________________________________________________________________________________________________________
randomList :: ( Random a ) => Int -> [ a ]
randomList seed = randoms ( mkStdGen seed )

--___________________________________________________________________________________________________________
-- Remove existing file
--deleteFile file_name = do
--						exists <- unsafePerformIO $( doesFileExist file_name )
--						if ( exists )
--							then do removeFile file_name;
--									return True
--						else return False
						

--___________________________________________________________________________________________________________
-- Line parser
-- break' is like break but removes the
-- delimiter from the rest string
break' d = second (drop 1) . break d

splitLine :: String -> Maybe (String,String)
splitLine [] = Nothing
splitLine xs = Just . break' (==',') $ xs

parseLine :: String -> [Float]
parseLine = map ((*1) . read) . unfoldr splitLine

--___________________________________________________________________________________________________________
-- Rekursiv generating and writing of file with random generated floats                                                                           
generateRandoms number rand file_name = do
	let generatedFloats = take 4 ( randomList ( rand + number + 4 ) :: [ Float ] )
	if( number > 0 )
		then do 
				let newValues = Values ( generatedFloats!!0 ) ( generatedFloats!!1 ) ( generatedFloats!!2 ) ( generatedFloats!!3 );
				appendFile file_name ( show newValues );
				appendFile file_name "\n";
				generateRandoms ( number - 1 ) rand file_name;
	else return()

--___________________________________________________________________________________________________________
-- Try to recognize the given values
initiateRecognition values cases = do
							primRanges <- getPrimaryEventRanges cases
							newCases <- expandPrimaryRangesIfNeeded values cases
							if ( isRecognitionPossible values primRanges )
							then do finalEvents <- ( recognize values newCases );
									return ( finalEvents )
							else do finalEvents <- ( createNewCase values newCases );
									return ( finalEvents )
 
--___________________________________________________________________________________________________________
-- read line by line and recognize
readLoop inh cases = do
				let ineof = unsafePerformIO $(  hIsEOF inh );
				if ( ineof )
					then do return ( cases )
				else do
					let inpStr = unsafePerformIO $( hGetLine inh );
					let generatedFloats = parseLine inpStr;
					let newValues = Values ( generatedFloats!!0 ) ( generatedFloats!!1 ) ( generatedFloats!!2 ) ( generatedFloats!!3 );
					newCases <- ( initiateRecognition newValues cases );
					return ( unsafePerformIO $(  readLoop inh newCases ) )

--___________________________________________________________________________________________________________
-- MAIN
main :: IO ()
main = do
  --r <- randomIO
  
  let randomFile = "RandomValues.txt"
  let eventsFile = "output.txt"
  --deleteFile eventsFile
  
  -- Generate new random file (currently disabled)
  --removeFile randomFile;
  --generateRandoms 1000 r randomFile;
    
--  putStrLn ( "Generate new random input file?" )
--  putStrLn ( "Y/N: " )
--  line <- getLine
--  -- Generate new random file
--  if( line == "y" || line == "Y" )
--		then do 
--			putStrLn ( "Enter number of random events to be generated: " );
--			line <- getLine;
--			deleteFile randomFile;
--			generateRandoms ( read line :: Int ) r randomFile
--  else if ( doesFileExist randomFile )
--		then putStrLn ( "Opening existing file." )
--  else do putStrLn ( "File doesn't exist. Generating file with 1000 events." );
--		  generateRandoms 1000 r randomFile
  
  inh <- openFile randomFile ReadMode
  lastCase <- readLoop inh caseC
  hClose inh
  
  appendFile eventsFile ( show lastCase )
  putStrLn ( "Done." )