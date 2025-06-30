module Elevator0 where

import Control.Concurrent (threadDelay)
import Control.Monad (foldM)
import Data.Bool (bool)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.List qualified as L

data Elevator = Elevator
    { currentFloor :: Floor
    , firstFloor :: Floor
    , lastFloor :: Floor
    , maxCapacity :: Int
    , currentCapacity :: Int
    }
    deriving stock (Show)

newtype Floor = Floor Int
    deriving newtype (Show, Eq, Ord, Num)

data Direction = Up | Down
    deriving stock (Show)

data ElevatorInstruction
    = Pickup Floor
    | Dropoff Floor
    deriving stock (Show)

data ElevatorMotion = Moving MovingElevator | Stationary Elevator
    deriving stock (Show)

data MovingElevator = MovingElevator
    { direction :: Direction
    , targetFloor :: Floor
    , movementType :: MovementType
    , elevator :: Elevator
    }
    deriving stock (Show)

data MovementType = PickupMovement | DropoffMovement
    deriving stock (Show)

toFloorNumber :: Floor -> Int
toFloorNumber (Floor n) = n

executeElevatorInstructions :: Elevator -> [ElevatorInstruction] -> IO Elevator
executeElevatorInstructions e [] = pure e
executeElevatorInstructions elevatorState [instruction] = executeElevatorMotion (calculateNextMotionState elevatorState instruction)
executeElevatorInstructions elevatorState (firstInstruction:others) = do
    case prioritizeInstructions (Stationary elevatorState) (firstInstruction:others) of
        [] -> pure elevatorState
        (nextInstruction : rest) -> do
            newElevatorState <- executeElevatorMotion (calculateNextMotionState elevatorState nextInstruction)
            executeElevatorInstructions newElevatorState rest

prioritizeInstructions :: ElevatorMotion -> [ElevatorInstruction] -> [ElevatorInstruction]
prioritizeInstructions _ [] = []
prioritizeInstructions (Stationary elevator) instructions = sortByDistanceToCurrentFloor elevator.currentFloor instructions
prioritizeInstructions (Moving movingElevator) instructions =
    let currentDirection = movingElevator.direction
        currentElevatorFloor = movingElevator.elevator.currentFloor
        capacityPercentage = (fromIntegral @_ @Double movingElevator.elevator.currentCapacity / fromIntegral movingElevator.elevator.maxCapacity) * 100
        ~(inDirectionInstructions, oppositeDirectionInstructions) = L.partition (isInCurrentDirection currentElevatorFloor currentDirection . getInstructionTargetFloor) instructions
     in if capacityPercentage >= capacityThreshold
            then
                let ~(priorityDropoffs, deferredInstructions) = L.partition isDropoffInstruction inDirectionInstructions
                 in sortByDistanceToCurrentFloor currentElevatorFloor priorityDropoffs <> deferredInstructions <> oppositeDirectionInstructions
            else sortByDistanceToCurrentFloor currentElevatorFloor inDirectionInstructions <> oppositeDirectionInstructions
  where
    isInCurrentDirection :: Floor -> Direction -> Floor -> Bool
    isInCurrentDirection currentFloor Up flr = flr >= currentFloor
    isInCurrentDirection currentFloor Down flr = flr <= currentFloor

    isDropoffInstruction :: ElevatorInstruction -> Bool
    isDropoffInstruction (Dropoff _) = True
    isDropoffInstruction _ = False

    capacityThreshold :: Double
    capacityThreshold = 75.0

calculateNextMotionState :: Elevator -> ElevatorInstruction -> ElevatorMotion
calculateNextMotionState elevatorState instruction
    | elevatorState.currentFloor == getInstructionTargetFloor instruction = Stationary elevatorState
    | otherwise =
        let currentElevatorFloor = elevatorState.currentFloor
         in case instruction of
                (Pickup from) -> Moving $ MovingElevator (bool Down Up $ from > currentElevatorFloor) from PickupMovement elevatorState
                (Dropoff at) -> Moving $ MovingElevator (bool Down Up $ at > currentElevatorFloor) at DropoffMovement elevatorState

executeElevatorMotion :: ElevatorMotion -> IO Elevator
executeElevatorMotion (Stationary elevator) = putStrLn ("Elevator is already at floor " <> show elevator.currentFloor <> "\n") $> elevator
executeElevatorMotion (Moving (MovingElevator direction targetFloor movementType elevator)) = do
    let updateElevatorCapacity = case movementType of
            PickupMovement -> \e -> e{currentCapacity = e.currentCapacity + 1}
            DropoffMovement -> \e -> e{currentCapacity = e.currentCapacity - 1}

    if not (isValidFloor elevator targetFloor)
        then putStrLn ("Invalid floor request. Elevator is at floor " <> show elevator.currentFloor <> ", but requested floor (" <> show targetFloor <> ") is out of range.\n") $> elevator
        else do
            let distanceToMove = abs $ toFloorNumber (elevator.currentFloor - targetFloor)
            putStrLn ("Elevator is moving " <> show direction <> " to floor " <> show targetFloor <> " from " <> show elevator.currentFloor <> " with current capacity " <> show elevator.currentCapacity)
            updateElevatorCapacity <$> go direction distanceToMove elevator
  where
    go :: Direction -> Int -> Elevator -> IO Elevator
    go _ 0 currentElevator = putStrLn ("Elevator has arrived at floor " <> show targetFloor <> "\n") $> currentElevator
    go Up floorsRemaining currentElevator = do
        let newCurrentFloor = currentElevator.currentFloor + 1
        putStrLn ("Elevator has moved up to floor " <> show newCurrentFloor)
        threadDelay floorTransitionDelayMicros
        go Up (floorsRemaining - 1) currentElevator{currentFloor = newCurrentFloor}
    go Down floorsRemaining currentElevator = do
        let newCurrentFloor = currentElevator.currentFloor - 1
        putStrLn ("Elevator has moved down to floor " <> show newCurrentFloor)
        threadDelay floorTransitionDelayMicros
        go Down (floorsRemaining - 1) currentElevator{currentFloor = newCurrentFloor}

    floorTransitionDelayMicros :: Int
    floorTransitionDelayMicros = 1_000_000

-- -------------------------------------------------------------------------- --
--                                   Helpers                                  --
-- -------------------------------------------------------------------------- --
getInstructionTargetFloor :: ElevatorInstruction -> Floor
getInstructionTargetFloor (Pickup x) = x
getInstructionTargetFloor (Dropoff x) = x

getElevator :: ElevatorMotion -> Elevator
getElevator (Stationary elevator) = elevator
getElevator (Moving MovingElevator{elevator}) = elevator

isValidFloor :: Elevator -> Floor -> Bool
isValidFloor elevator targetFloor = (targetFloor >= elevator.firstFloor) && (targetFloor <= elevator.lastFloor)

sortByDistanceToCurrentFloor :: Floor -> [ElevatorInstruction] -> [ElevatorInstruction]
sortByDistanceToCurrentFloor currentFloor = L.sortBy (\a b -> compare (abs (getInstructionTargetFloor a - currentFloor)) (abs (getInstructionTargetFloor b - currentFloor)))

-- -------------------------------------------------------------------------- --
--                                    Main                                    --
-- -------------------------------------------------------------------------- --

main :: IO ()
main = do
    let initialElevator = Elevator{currentFloor = Floor 1, firstFloor = Floor 1, lastFloor = Floor 10, maxCapacity = 5, currentCapacity = 0}
    let instructions = [Pickup (Floor 3), Dropoff (Floor 5), Pickup (Floor 7), Dropoff (Floor 2), Pickup (Floor 11)]

    putStrLn "Starting Elevator Simulation..."
    finalElevatorState <- executeElevatorInstructions initialElevator instructions

    putStrLn $ "Final Elevator State: " <> show finalElevatorState
    putStrLn "Simulation complete."
