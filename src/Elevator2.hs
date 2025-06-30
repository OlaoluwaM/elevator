module Elevator2 where

import Data.Bool (bool)
import Data.Either (fromLeft)
import Data.List qualified as L

data ElevatorConfig = ElevatorConfig
    { firstFloor :: Floor
    , lastFloor :: Floor
    , maxCapacity :: Int
    }
    deriving stock (Show)

newtype Floor = Floor Int
    deriving stock (Show)
    deriving newtype (Eq, Ord, Num)

data Direction = Up | Down
    deriving stock (Show, Eq, Ord)

data MoveableElevator = MoveableElevator
    { currentFloor :: Floor
    , direction :: Direction
    , currentCapacity :: Int
    , queue :: OptimizedElevatorInstructions
    , config :: ElevatorConfig
    }
    deriving stock (Show)

data StationaryElevator = StationaryElevator
    { currentFloor :: Floor
    , currentCapacity :: Int
    , config :: ElevatorConfig
    }
    deriving stock (Show)

data Elevator = Moveable MoveableElevator | Stationary StationaryElevator
    deriving stock (Show)

data FloorRequest = FloorRequest
    { from :: Floor
    , to :: Floor
    }
    deriving stock (Show)

data ElevatorInstruction = Pickup Floor | Dropoff Floor
    deriving stock (Show, Eq)

newtype OptimizedElevatorInstructions = OptimizedElevatorInstructions [ElevatorInstruction]
    deriving stock (Show)

data ElevatorError
    = InvalidFloor Floor
    | CapacityExceeded
    | EmptyElevatorDropoff
    deriving stock (Show)

-- requestElevator :: Elevator -> FloorRequest -> MoveableElevator
-- requestElevator (Stationary stationaryE) (FloorRequest from to) =
--     MoveableElevator
--         { currentFloor = from
--         , direction = if to > from then Up else Down
--         , currentCapacity = stationaryE.currentCapacity
--         , queue = OptimizedElevatorInstructions [Pickup from, Dropoff to]
--         , config = stationaryE.config
--         }
-- requestElevator (Moveable moveableE) (FloorRequest from to) =
--     MoveableElevator
--         { currentFloor = from
--         , direction = if to > from then Up else Down
--         , currentCapacity = moveableE.currentCapacity
--         , queue = OptimizedElevatorInstructions (getInstructionsList moveableE.queue <> [Pickup from, Dropoff to])
--         , config = moveableE.config
--         }

executeElevatorInstructionsThenStop :: MoveableElevator -> StationaryElevator
executeElevatorInstructionsThenStop MoveableElevator{currentFloor, currentCapacity, queue = OptimizedElevatorInstructions [], config} =
    StationaryElevator
        { currentFloor = currentFloor
        , currentCapacity = currentCapacity
        , config = config
        }
executeElevatorInstructionsThenStop moveableE = executeElevatorInstructionsThenStop (executeElevatorInstructions moveableE)

executeElevatorInstructions :: MoveableElevator -> MoveableElevator
executeElevatorInstructions moveableE@MoveableElevator{queue = OptimizedElevatorInstructions []} = moveableE
executeElevatorInstructions moveableE@MoveableElevator{direction, queue = OptimizedElevatorInstructions (targetInstruction : rest)} =
    let errM = isValidInstruction moveableE targetInstruction
     in case errM of
            Just _ -> executeElevatorInstructions moveableE{queue = OptimizedElevatorInstructions rest}
            Nothing ->
                let floorDistance = numberFromFloor (getInstructionTargetFloor targetInstruction - moveableE.currentFloor)
                 in executeElevatorInstructions $ goToFloor floorDistance direction targetInstruction moveableE{queue = OptimizedElevatorInstructions rest}
  where
    goToFloor :: Int -> Direction -> ElevatorInstruction -> MoveableElevator -> MoveableElevator
    goToFloor 0 _ currentInstruction e = case currentInstruction of
        Pickup _ -> e{currentCapacity = e.currentCapacity + 1}
        Dropoff _ -> e{currentCapacity = e.currentCapacity - 1}
    goToFloor floorDistance Up currentInstruction e = goToFloor (floorDistance - 1) Up currentInstruction e{currentFloor = e.currentFloor + 1}
    goToFloor floorDistance Down currentInstruction e = goToFloor (floorDistance - 1) Down currentInstruction e{currentFloor = e.currentFloor - 1}

simulateElevatorMovements :: Elevator -> [ElevatorInstruction] -> StationaryElevator
simulateElevatorMovements (Stationary stationaryE) [] = stationaryE
simulateElevatorMovements e@(Stationary stationaryE) instructions =
    case optimizeFloorRequestOrder (calculateCapacityPercentage e) stationaryE.currentFloor Nothing instructions of
        Left (OptimizedElevatorInstructions [], []) -> stationaryE
        Left (_, rest) -> simulateElevatorMovements e rest
        Right (optimizedInstructions, rest, newDir) ->
            let moveableE = MoveableElevator{currentFloor = stationaryE.currentFloor, direction = newDir, currentCapacity = stationaryE.currentCapacity, queue = optimizedInstructions, config = stationaryE.config}
             in simulateElevatorMovements (Moveable moveableE) rest
simulateElevatorMovements (Moveable moveableE) [] = executeElevatorInstructionsThenStop moveableE
simulateElevatorMovements e@(Moveable moveableE) instructions =
    case optimizeFloorRequestOrder (calculateCapacityPercentage e) moveableE.currentFloor (Just moveableE.direction) (getInstructionsList moveableE.queue <> instructions) of
        Left (OptimizedElevatorInstructions [], []) -> executeElevatorInstructionsThenStop moveableE
        Left (_, rest) -> simulateElevatorMovements (Moveable (executeElevatorInstructions moveableE)) rest
        Right (optimizedInstructions, rest, newDir) ->
            let newMoveableE = MoveableElevator{currentFloor = moveableE.currentFloor, direction = newDir, currentCapacity = moveableE.currentCapacity, queue = optimizedInstructions, config = moveableE.config}
             in if null rest
                    then executeElevatorInstructionsThenStop newMoveableE
                    else simulateElevatorMovements (Moveable (executeElevatorInstructions newMoveableE)) rest

optimizeFloorRequestOrder :: Double -> Floor -> Maybe Direction -> [ElevatorInstruction] -> Either (OptimizedElevatorInstructions, [ElevatorInstruction]) (OptimizedElevatorInstructions, [ElevatorInstruction], Direction)
optimizeFloorRequestOrder _ _ Nothing [] = Left (OptimizedElevatorInstructions [], [])
optimizeFloorRequestOrder _ currentFloor Nothing eIs =
    case L.uncons . sortByDistanceToCurrentFloor currentFloor $ eIs of
        Nothing -> Left (OptimizedElevatorInstructions [], [])
        Just (first, rest) -> Right (OptimizedElevatorInstructions [first], rest, calculateNewDirectionFromInstruction currentFloor first)
optimizeFloorRequestOrder _ _ (Just _) [] = Left (OptimizedElevatorInstructions [], [])
optimizeFloorRequestOrder capacityPercentage currentFloor (Just dir) eIs =
    let (inDirectionInstructions, oppositeDirectionInstructions) = L.partition (isInDirection dir) eIs
     in if capacityPercentage >= capacityThreshold
            then
                let (priorityDropoff, deferred) = L.partition isDropoff inDirectionInstructions
                 in Right (OptimizedElevatorInstructions (sortByDistanceToCurrentFloor currentFloor priorityDropoff), deferred <> oppositeDirectionInstructions, dir)
            else Right (OptimizedElevatorInstructions (sortByDistanceToCurrentFloor currentFloor inDirectionInstructions), oppositeDirectionInstructions, dir)
  where
    isDropoff :: ElevatorInstruction -> Bool
    isDropoff (Dropoff _) = True
    isDropoff _ = False

    capacityThreshold :: Double
    capacityThreshold = 75

    isInDirection :: Direction -> ElevatorInstruction -> Bool
    isInDirection Up instruction = getInstructionTargetFloor instruction >= currentFloor
    isInDirection Down instruction = getInstructionTargetFloor instruction <= currentFloor

-- optimizeFloorRequestOrder' :: Int -> Floor -> Maybe Direction -> [FloorRequest] -> (Maybe Direction, [FloorRequest], [FloorRequest])
-- optimizeFloorRequestOrder' _ _ _ [] = (Nothing, [], [])
-- optimizeFloorRequestOrder' _ currentFloor Nothing fRs = case L.uncons . sortByDistanceToCurrentFloorFR currentFloor $ fRs of
--     Nothing -> (Nothing, [], [])
--     Just (first, rest) -> (Just Up, [first], rest)
-- optimizeFloorRequestOrder' capacityPercentage currentFloor (Just direction) fRs =
--     let (inDirection, oppositeDirection) = L.partition (isInDirection direction) fRs
--      in if capacityPercentage >= 75
--             then
--                 let (priorityDropoff, deferred) = L.partition (\(FloorRequest from _) -> from >= currentFloor) inDirection
--                  in (direction, priorityDropoff, deferred <> oppositeDirection)
--             else (direction, inDirection, oppositeDirection)
--   where
--     isInDirection :: Direction -> FloorRequest -> Bool
--     isInDirection Up (FloorRequest from _) = from >= currentFloor
--     isInDirection Down (FloorRequest from _) = from <= currentFloor

-- optimizeFloorRequestOrder elevator@(Elevator{currentDirection = Nothing}) moveEvents = sortByDistanceToCurrentFloor elevator.currentFloor moveEvents
-- optimizeFloorRequestOrder elevator@(Elevator{currentDirection = Just currentDirection}) moveEvents =
--     let currentElevatorFloor = elevator.currentFloor
--         capacityPercentage = (fromIntegral @_ @Double elevator.currentCapacity / fromIntegral elevator.maxCapacity) * 100
--         ~(inDirectionEvents, oppositeDirectionEvents) = L.partition (isInCurrentDirection currentElevatorFloor currentDirection . (.targetFloor)) moveEvents
--      in if capacityPercentage >= capacityThreshold
--             then
--                 let ~(priorityDropoffEvents, deferredEvents) = L.partition isDropoffMoveEvent inDirectionEvents
--                  in sortByDistanceToCurrentFloor currentElevatorFloor priorityDropoffEvents <> deferredEvents <> oppositeDirectionEvents
--             else sortByDistanceToCurrentFloor currentElevatorFloor inDirectionEvents <> oppositeDirectionEvents
--   where
--     isInCurrentDirection :: Floor -> Direction -> Floor -> Bool
--     isInCurrentDirection currentFloor Up flr = flr >= currentFloor
--     isInCurrentDirection currentFloor Down flr = flr <= currentFloor

--     isDropoffMoveEvent :: MoveEvent -> Bool
--     isDropoffMoveEvent (MoveEvent _ Dropoff) = True
--     isDropoffMoveEvent _ = False

--     capacityThreshold :: Double
--     capacityThreshold = 75.0

-- -------------------------------------------------------------------------- --
--                                   Helpers                                  --
-- -------------------------------------------------------------------------- --

getInstructionsList :: OptimizedElevatorInstructions -> [ElevatorInstruction]
getInstructionsList (OptimizedElevatorInstructions instructions) = instructions

numberFromFloor :: Floor -> Int
numberFromFloor (Floor n) = n

sortByDistanceToCurrentFloor :: Floor -> [ElevatorInstruction] -> [ElevatorInstruction]
sortByDistanceToCurrentFloor currentFloor = L.sortBy (\a b -> compare (abs (numberFromFloor (getInstructionTargetFloor a) - numberFromFloor currentFloor)) (abs (numberFromFloor (getInstructionTargetFloor b) - numberFromFloor currentFloor)))

getInstructionTargetFloor :: ElevatorInstruction -> Floor
getInstructionTargetFloor (Pickup flr) = flr
getInstructionTargetFloor (Dropoff flr) = flr

-- sortByDistanceToCurrentFloorFR :: Floor -> [FloorRequest] -> [FloorRequest]
-- sortByDistanceToCurrentFloorFR currentFloor = L.sortBy (\a b -> compare (abs (numberFromFloor a.from - currentFloor)) (abs (numberFromFloor b.from - currentFloor)))

calculateCapacityPercentage :: Elevator -> Double
calculateCapacityPercentage (Moveable moveableE) =
    (fromIntegral moveableE.currentCapacity / fromIntegral moveableE.config.maxCapacity) * 100
calculateCapacityPercentage (Stationary stationaryE) =
    (fromIntegral stationaryE.currentCapacity / fromIntegral stationaryE.config.maxCapacity) * 100

baz :: FloorRequest -> [ElevatorInstruction]
baz (FloorRequest from to) = [Pickup from, Dropoff to]

calculateNewDirectionFromInstruction :: Floor -> ElevatorInstruction -> Direction
calculateNewDirectionFromInstruction currentFloor instruction = if getInstructionTargetFloor instruction > currentFloor then Up else Down

isValidInstruction :: MoveableElevator -> ElevatorInstruction -> Maybe ElevatorError
isValidInstruction moveableE instruction = fromLeft Nothing $ do
    let targetFloor = getInstructionTargetFloor instruction
    bool (Left . Just $ InvalidFloor targetFloor) (pure ()) $ isValidFloor moveableE targetFloor
    bool (Left . Just $ EmptyElevatorDropoff) (pure ()) $ instruction == Dropoff targetFloor && moveableE.currentCapacity <= 0
    bool (Left . Just $ CapacityExceeded) (pure ()) $ moveableE.currentCapacity >= moveableE.config.maxCapacity
    pure $ Left Nothing

isValidFloor :: MoveableElevator -> Floor -> Bool
isValidFloor moveableE targetFloor = targetFloor >= moveableE.config.firstFloor && targetFloor <= moveableE.config.lastFloor
