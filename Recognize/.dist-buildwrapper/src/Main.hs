module Main where

--import random.cabal
import Random
--import random-1.0.1.1    -- import standart template library for work random generated values
--import System.Random    -- import standart template library for work random generated values
import RecognizeLibrary -- import the specific data structures and the initial data

randomList :: ( Random a ) => Int -> [ a ]
randomList seed = randoms ( mkStdGen seed )

isRecognizable values ranges = isRecognitionPossible

-- Rekursiv generating and writing of file with random generated floats                                                                           
generateRandoms number rand file_name = do
  let generatedFloats = take 4 ( randomList ( rand + number ) :: [ Float ] )
  if( number > 0 )
    then do appendFile file_name ( show generatedFloats );
                appendFile file_name "\n";
            generateRandoms ( number - 1 ) rand file_name;
    else appendFile file_name "\n"
        
--initiateRecognition file_name last_case = do
 
main :: IO ()
main = do 
  r <- randomIO;

  let randomFile = "RandomValues.txt";
  --generateRandoms 1000 r randomFile ;
    
  let lastCase  = caseC;
  let generatedFloats = take 4 ( randomList ( r + 1 ) :: [ Float ] );
  let newValues = Values ( generatedFloats!!0 ) ( generatedFloats!!1 ) ( generatedFloats!!2 ) ( generatedFloats!!3 );
  let lastCase = recognize newValues lastCase  
  appendFile "output.txt" "\n"