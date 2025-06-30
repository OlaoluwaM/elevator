module Elevator where

import Control.Concurrent (threadDelay)
import Data.Functor (($>))
import Data.List qualified as L

data Elevator = Elevator
    { currentFloor :: Floor
    , firstFloor :: Floor
    , lastFloor :: Floor
    , maxCapacity :: Int
    , currentDirection :: Maybe Direction
    , currentCapacity :: Int
    }
    deriving stock (Show)

newtype Floor = Floor Int
    deriving newtype (Show, Eq, Ord, Num)

data Direction = Up | Down
    deriving stock (Show)

data MoveEvent = MoveEvent
    { targetFloor :: Floor
    , movementType :: MovementType
    }
    deriving stock (Show)

data MovementType = Pickup | Dropoff
    deriving stock (Show, Eq)

data ElevatorError
    = InvalidFloor Floor
    | CapacityExceeded
    | EmptyElevatorDropoff
    deriving stock (Show)

-- | Creates a MoveEvent with the appropriate direction based on current and target floors.
makeMoveEvent :: Floor -> MovementType -> MoveEvent
makeMoveEvent = MoveEvent

-- | Execute a move event to transition the elevator to another state at the target floor
processMoveEvent :: Elevator -> MoveEvent -> IO (Maybe ElevatorError, Elevator)
processMoveEvent elevator (MoveEvent targetFloor movementType)
    | elevator.currentFloor == targetFloor = do
        putStrLn ("Elevator is already at floor " <> show targetFloor <> " - " <> show movementType)
        pure (Nothing, elevator)
    | elevator.currentCapacity == 0 && movementType == Dropoff = pure (Just EmptyElevatorDropoff, elevator)
    | not (isValidFloor elevator targetFloor) = pure (Just (InvalidFloor targetFloor), elevator)
    | movementType == Pickup && elevator.currentCapacity >= elevator.maxCapacity = pure (Just CapacityExceeded, elevator)
    | otherwise = do
        let floorDistance = numberFromFloor (targetFloor - elevator.currentFloor)
        let direction = if targetFloor > elevator.currentFloor then Up else Down
        putStrLn ("Elevator is moving " <> show direction <> " to floor " <> show targetFloor <> " from floor " <> show elevator.currentFloor <> " with capacity " <> show elevator.currentCapacity <> " (" <> show movementType <> ")")
        newElevator <- updateElevatorCapacity movementType <$> moveElevator direction floorDistance targetFloor (elevator{currentDirection = Just direction})
        pure (Nothing, newElevator)

moveElevator :: Direction -> Int -> Floor -> Elevator -> IO Elevator
moveElevator _ 0 targetFloor currentElevator = putStrLn ("Elevator has arrived at floor " <> show targetFloor <> "\n") $> currentElevator
moveElevator Up floorsRemaining targetFloor currentElevator = do
    let newCurrentFloor = currentElevator.currentFloor + 1
    putStrLn ("Elevator has moved up to floor " <> show newCurrentFloor)
    threadDelay floorTransitionDelayMicros
    moveElevator Up (floorsRemaining - 1) targetFloor currentElevator{currentFloor = newCurrentFloor}
moveElevator Down floorsRemaining targetFloor currentElevator = do
    let newCurrentFloor = currentElevator.currentFloor - 1
    putStrLn ("Elevator has moved down to floor " <> show newCurrentFloor)
    threadDelay floorTransitionDelayMicros
    moveElevator Down (floorsRemaining - 1) targetFloor currentElevator{currentFloor = newCurrentFloor}

processMoveEvents :: Elevator -> [MoveEvent] -> IO Elevator
processMoveEvents elevator [] = pure elevator{currentDirection = Nothing}
processMoveEvents elevator events = do
    case determineNextMoveEvent elevator events of
        [] -> processMoveEvents elevator []
        (nextEvent : rest) -> do
            (errM, updatedElevator) <- processMoveEvent elevator nextEvent
            case errM of
                Just err -> putStrLn ("Error: " <> show err <> "\n")
                Nothing -> pure ()
            processMoveEvents updatedElevator rest

determineNextMoveEvent :: Elevator -> [MoveEvent] -> [MoveEvent]
determineNextMoveEvent _ [] = []
determineNextMoveEvent elevator@(Elevator{currentDirection = Nothing}) moveEvents = sortByDistanceToCurrentFloor elevator.currentFloor moveEvents
determineNextMoveEvent elevator@(Elevator{currentDirection = Just currentDirection}) moveEvents =
    let currentElevatorFloor = elevator.currentFloor
        capacityPercentage = (fromIntegral @_ @Double elevator.currentCapacity / fromIntegral elevator.maxCapacity) * 100
        ~(inDirectionEvents, oppositeDirectionEvents) = L.partition (isInCurrentDirection currentElevatorFloor currentDirection . (.targetFloor)) moveEvents
     in if capacityPercentage >= capacityThreshold
            then
                let ~(priorityDropoffEvents, deferredEvents) = L.partition isDropoffMoveEvent inDirectionEvents
                 in sortByDistanceToCurrentFloor currentElevatorFloor priorityDropoffEvents <> deferredEvents <> oppositeDirectionEvents
            else sortByDistanceToCurrentFloor currentElevatorFloor inDirectionEvents <> oppositeDirectionEvents
  where
    isInCurrentDirection :: Floor -> Direction -> Floor -> Bool
    isInCurrentDirection currentFloor Up flr = flr >= currentFloor
    isInCurrentDirection currentFloor Down flr = flr <= currentFloor

    isDropoffMoveEvent :: MoveEvent -> Bool
    isDropoffMoveEvent (MoveEvent _ Dropoff) = True
    isDropoffMoveEvent _ = False

    capacityThreshold :: Double
    capacityThreshold = 75.0

-- -------------------------------------------------------------------------- --
--                                   Helpers                                  --
-- -------------------------------------------------------------------------- --

updateElevatorCapacity :: MovementType -> Elevator -> Elevator
updateElevatorCapacity movementType e = case movementType of
    Pickup -> e{currentCapacity = e.currentCapacity + 1}
    Dropoff -> e{currentCapacity = e.currentCapacity - 1}

floorTransitionDelayMicros :: Int
floorTransitionDelayMicros = 1_000_000

isValidFloor :: Elevator -> Floor -> Bool
isValidFloor elevator targetFloor = (targetFloor >= elevator.firstFloor) && (targetFloor <= elevator.lastFloor)

numberFromFloor :: Floor -> Int
numberFromFloor (Floor n) = n

sortByDistanceToCurrentFloor :: Floor -> [MoveEvent] -> [MoveEvent]
sortByDistanceToCurrentFloor currentFloor = L.sortBy (\a b -> compare (abs (a.targetFloor - currentFloor)) (abs (b.targetFloor - currentFloor)))

-- -------------------------------------------------------------------------- --
--                                    Main                                    --
-- -------------------------------------------------------------------------- --

main :: IO ()
main = do
    let elevator = Elevator (Floor 1) (Floor 1) (Floor 10) 5 Nothing 0
    putStrLn "Starting elevator simulation...\n"
    let moveEvents =
            [ makeMoveEvent (Floor 3) Pickup
            , makeMoveEvent (Floor 5) Dropoff
            , makeMoveEvent (Floor 2) Pickup
            , makeMoveEvent (Floor 8) Dropoff
            , makeMoveEvent (Floor 2) Dropoff
            , makeMoveEvent (Floor 6) Pickup
            , makeMoveEvent (Floor 7) Dropoff
            ]
    finalElevator <- processMoveEvents elevator moveEvents
    putStrLn ("Final elevator state: " <> show finalElevator)
