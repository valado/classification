import RecognitionLibrary    -- import the specific data structures and the initial data
import System.IO             -- import standart template library import standart template libraryreading and writing data to files
import System.Random         -- import standart template library for work random generated values
import Data.List             -- import standart template library for the parser
import Control.Arrow(second) -- import standart template library for the parser
import Directory             -- import standart template library for removeFile
import RecognitionLibrary    -- import the specific data structures and the initial data

break' d = second (drop 1) . break d

splitLine :: String -> Maybe (String,String)
splitLine [] = Nothing
splitLine xs = Just . break' (==',') $ xs

parseLine :: String -> [Float]
parseLine = map ((*1) . read) . unfoldr splitLine

initiateRecognition values cases = do
							primRanges <- getPrimaryEventRanges cases
							newCases <- expandPrimaryRangesIfNeeded values cases
							if ( isRecognitionPossible values primRanges )
							then do finalEvents <- ( recognize values newCases );
									return ( finalEvents )
							else do finalEvents <- ( createNewCase values newCases );
									return ( finalEvents )						
						
readLoop inh cases = do
				ineof <- hIsEOF inh
				if ( ineof )
					then do return ( cases )
				else do
					inpStr <- hGetLine inh;
					let generatedFloats = parseLine inpStr;
					let newValues = Values ( generatedFloats!!0 ) ( generatedFloats!!1 ) ( generatedFloats!!2 ) ( generatedFloats!!3 );
					let new_cases = ( initiateRecognition newValues cases );
					return ( readLoop inh new_cases )