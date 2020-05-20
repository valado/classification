module RecognitionLibrary where

-- ******************************** --
-- @author: Vladimir Daskalov       --
-- @date  : 5.1.2011                --
-- ******************************** --

--___________________________________________________________________________________________________________
-- Temp data structure, that holds the new values
data Values = Values {
        x1      :: Float,
        x2      :: Float,
        x3      :: Float,
        x4      :: Float
}

-- Predefinition of class Show for own type Values
instance Show Values where
	show ( Values x1 x2 x3 x4 ) = show( x1 ) ++ ", " ++ show( x2 ) ++ ", " ++ show( x3 ) ++ ", " ++ show( x4 )

--___________________________________________________________________________________________________________
-- Bounds minimal and maximal value of the current range
data Bounds = Bounds {
        x      :: Float,
        y      :: Float
}

lowerBound bounds = min ( x( bounds ) ) ( y( bounds ) )
upperBound bounds = max ( x( bounds ) ) ( y( bounds ) )

-- Predefinition of class Show for own type Ranges
instance Show Bounds where
	show ( Bounds min max ) = "[" ++ show( min ) ++ ", " ++ show( max ) ++ "]"
	
-- is the value in the bounds ( +- 10%)
isInBounds value bounds = if( ( 0.9 * ( lowerBound( bounds ) ) <= value && value <= ( upperBound( bounds ) ) ) ||
                              ( 1.1 * ( upperBound( bounds ) ) >= value && value >= ( lowerBound( bounds ) ) ) )
                          then True
                          else False                                           

--___________________________________________________________________________________________________________
-- Data structure holding ranges for every value (e.g. x1,x2,x3,x4)
data Ranges = Ranges {
        x1Range :: Bounds,
        x2Range :: Bounds,
        x3Range :: Bounds,
        x4Range :: Bounds
}

-- Predefinition of class Show for own type Ranges
instance Show Ranges where
	show ( Ranges r1 r2 r3 r4 ) = "[ x1 - " ++ show( r1 ) ++ ", x2 - " ++ show( r2 ) ++ ", x3 - " ++ show( r3 ) ++ ", x4 - " ++ show( r4 ) ++ " ]"

-- Own type predefinition
type EventName = String
type NextFlag  = Bool
type NextGroup = Event

--___________________________________________________________________________________________________________
-- Data structure, that holds all data for all known cases
data Event = Event EventName Ranges NextFlag NextGroup
           | Empty
                                                            
-- Accessors
eventName   :: Event -> EventName
eventName   ( Event group_name  _       _       _          ) = group_name
eventRanges :: Event -> Ranges
eventRanges ( Event _           ranges  _       _          ) = ranges
eventNFlag  :: Event -> Bool
eventNFlag  ( Event _           _       flag    _          ) = flag
eventNext   :: Event -> Event
eventNext   ( Event _           _       _       next_group ) = next_group

-- Predefinition of class Show for own type Event
instance Show Event where
	show ( Event name range flag next_event ) = if( flag )
												then show( name ) ++ " - " ++ show( range ) ++ "\n" ++ show( next_event )
												else show( name ) ++ " - " ++ show( range ) ++ "\n"

--___________________________________________________________________________________________________________
-- Checks if the given group of values is in the range
isInRanges values ranges = if ( isInBounds ( x1( values ) ) ( x1Range( ranges ) ) ) &&
                              ( isInBounds ( x2( values ) ) ( x2Range( ranges ) ) ) &&
                              ( isInBounds ( x3( values ) ) ( x3Range( ranges ) ) ) &&
                              ( isInBounds ( x4( values ) ) ( x4Range( ranges ) ) )
                              then True
                              else False

--___________________________________________________________________________________________________________
-- Fit the bounds to the given value
expandRange value bounds = if( ( lowerBound( bounds ) ) > value )
                           then ( Bounds value ( upperBound( bounds ) ) )
                           else if ( ( upperBound( bounds ) ) < value )
                                then ( Bounds ( lowerBound( bounds ) ) value )
                           else bounds
                                        
--___________________________________________________________________________________________________________
-- Expand ranges to fit given values
expandRanges values ranges = do
                               let newX1Bounds = ( expandRange ( x1( values ) ) ( x1Range( ranges ) ) );
                               let newX2Bounds = ( expandRange ( x2( values ) ) ( x2Range( ranges ) ) );
                               let newX3Bounds = ( expandRange ( x3( values ) ) ( x3Range( ranges ) ) );
                               let newX4Bounds = ( expandRange ( x4( values ) ) ( x4Range( ranges ) ) );
                               let new_ranges  = Ranges newX1Bounds newX2Bounds newX3Bounds newX4Bounds;
                               return ( new_ranges )
          
--___________________________________________________________________________________________________________
-- Create new known case with boundaries +-20% of the unrecognized values
expandPrimaryRangesIfNeeded values event = if ( eventNFlag( event ) )
                                           then do 
                                                newEvent <- ( expandPrimaryRangesIfNeeded values ( eventNext( event ) ) );
                                                return ( Event ( eventName( event ) ) ( eventRanges( event ) ) True newEvent )
                                           else do 
                                                newRanges <- expandRanges values ( eventRanges( event ) );
                                                return ( Event ( eventName( event ) ) newRanges False Empty )

--___________________________________________________________________________________________________________
-- Checks if the recognition is posiible i.e. if there are 2 values "x1, x3" with
-- common deviation over 50% out of the appropriate bounds the recognition is impossible
isRecognitionPossible values ranges = if ( ( getDeviation ( x1( values ) ) ( x1Range( ranges ) ) ) + ( getDeviation ( x2( values ) ) ( x2Range( ranges ) ) ) ) >= 50.0 ||
                                         ( ( getDeviation ( x1( values ) ) ( x1Range( ranges ) ) ) + ( getDeviation ( x3( values ) ) ( x3Range( ranges ) ) ) ) >= 50.0 ||
                                         ( ( getDeviation ( x1( values ) ) ( x1Range( ranges ) ) ) + ( getDeviation ( x4( values ) ) ( x4Range( ranges ) ) ) ) >= 50.0 ||
                                         ( ( getDeviation ( x2( values ) ) ( x2Range( ranges ) ) ) + ( getDeviation ( x3( values ) ) ( x3Range( ranges ) ) ) ) >= 50.0 ||
                                         ( ( getDeviation ( x2( values ) ) ( x2Range( ranges ) ) ) + ( getDeviation ( x4( values ) ) ( x4Range( ranges ) ) ) ) >= 50.0 ||
                                         ( ( getDeviation ( x3( values ) ) ( x3Range( ranges ) ) ) + ( getDeviation ( x4( values ) ) ( x4Range( ranges ) ) ) ) >= 50.0
                                       then False
                                       else True

-- Returns deviation in percentage
-- if the value is in the given range the function returns 0.0
getDeviation value bounds = if  value < lowerBound( bounds )
                            then ( ( value * 100 ) / lowerBound( bounds ) )
                                else if value > upperBound( bounds )
                                        then ( ( value * 100 ) / upperBound( bounds ) )
                            else 0

--___________________________________________________________________________________________________________
-- Rekursive recognition function
recognize values event = if ( isInRanges values ( eventRanges( event ) ) )
                         -- case recognized -> return the same Ranges
                                then do return ( event )
                         else if ( eventNFlag( eventNext( event ) ) ) -- check if the next event has its own next event if so the next event is not primaryCase
                         -- if the event is not recognized by this case and there are more cases invoke rekursive calling of this function to check all other cases
                                then do newEvents <- ( recognize values ( eventNext( event ) ) );
                                        return ( Event ( eventName( event ) ) ( eventRanges( event ) ) True newEvents )
                         -- if the case cannot be recognized i.e. this is the last known case -> crate new event
                         else do newEvents <- createNewCase values event;
                                 return ( newEvents )

--___________________________________________________________________________________________________________
-- Create new known case with boundaries +-20% of the unrecognized values
createNewCase values event  = do
          let newX1Bounds = Bounds ( ( x1( values ) ) - 40/100 * ( x1( values ) ) ) ( ( x1( values ) ) + 40/100 * ( x1( values ) ) )
          let newX2Bounds = Bounds ( ( x2( values ) ) - 40/100 * ( x2( values ) ) ) ( ( x2( values ) ) + 40/100 * ( x2( values ) ) )
          let newX3Bounds = Bounds ( ( x3( values ) ) - 40/100 * ( x3( values ) ) ) ( ( x3( values ) ) + 40/100 * ( x3( values ) ) )
          let newX4Bounds = Bounds ( ( x4( values ) ) - 40/100 * ( x4( values ) ) ) ( ( x4( values ) ) + 40/100 * ( x4( values ) ) )
          let newRange    = Ranges newX1Bounds newX2Bounds newX3Bounds newX4Bounds		  
          let newName     = "NEW_EVENT: " ++ ( show $ ( lowerBound( newX1Bounds ) ))
          let newEvent    = Event newName newRange True event
          return ( newEvent )

--___________________________________________________________________________________________________________
-- getPrimaryEventRanges
getPrimaryEventRanges event = if ( eventNFlag( event ) )
								then do
									getPrimaryEventRanges ( eventNext( event ) )
								else do
									return ( eventRanges( event ) )

--___________________________________________________________________________________________________________
-- TESTING VALUES                        
vals = Values 0.24 0.3 0.3 0.65
uvals = Values 0.1 0.15 0.85 0.90

--___________________________________________________________________________________________________________
-- Initial PrimaryBounds
x1Bounds = Bounds 0.2 0.65
x2Bounds = Bounds 0.25 0.65
x3Bounds = Bounds 0.2 0.7
x4Bounds = Bounds 0.2 0.9
primaryRanges = Ranges x1Bounds x2Bounds x3Bounds x4Bounds
primaryCase :: Event
primaryCase = Event "primaryCase" primaryRanges False Empty

-- Initial Case Bounds
-- Case 'a'
aX1Bounds = Bounds 0.25 0.45
aX2Bounds = Bounds 0.3 0.4
aX3Bounds = Bounds 0.3 0.5
aX4Bounds = Bounds 0.6 0.7
aRanges = Ranges aX1Bounds aX2Bounds aX3Bounds aX4Bounds
caseA :: Event
caseA = Event "a" aRanges True primaryCase
-- Case 'b'
bX1Bounds = Bounds 0.35 0.55
bX2Bounds = Bounds 0.4 0.5
bX3Bounds = Bounds 0.4 0.6
bX4Bounds = Bounds 0.7 0.8
bRanges = Ranges bX1Bounds bX2Bounds bX3Bounds bX4Bounds
caseB :: Event
caseB = Event "b" bRanges True caseA
-- Case 'c'
cX1Bounds = Bounds 0.4 0.6
cX2Bounds = Bounds 0.45 0.55
cX3Bounds = Bounds 0.45 0.65
cX4Bounds = Bounds 0.75 0.85
cRanges = Ranges cX1Bounds cX2Bounds cX3Bounds cX4Bounds
caseC :: Event
caseC = Event "c" cRanges True caseB