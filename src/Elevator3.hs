{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}

{- |
Description: An intelligent elevator control system with optimized instruction batching

This module implements a state machine-based elevator system that efficiently handles
floor requests through a sophisticated instruction optimization algorithm. The system
uses a three-tier batching strategy to balance efficiency and responsiveness.

Key Features:
- State machine architecture (Stationary ↔ Moving)
- Capacity-aware instruction prioritization
- Direction-based batching optimization
- Real-time execution with visual feedback

The elevator processes requests in batches, optimizing for:
1. Current elevator capacity (prioritizing dropoffs when full, pickups when empty)
2. Travel direction (grouping same-direction instructions)
3. Distance minimization (sorting by proximity to current floor)
-}
module Elevator3 where

import Crem.BaseMachine qualified as Machine
import Data.List qualified as L
import Data.Vector qualified as V

import Control.Concurrent (threadDelay)
import Crem.BaseMachine (
    BaseMachineT (..),
    InitialState (InitialState),
    pureResult,
 )
import Crem.Render.RenderableVertices (AllVertices (..), RenderableVertices)
import Crem.Topology
import Data.Foldable (foldlM)
import Data.Maybe (fromMaybe)
import Data.Singletons.Base.TH
import Data.Vector (Vector)
import GHC.Records (HasField)

$( singletons
    [d|
        data ElevatorVertex = Stationary | Moving
            deriving stock (Eq, Show, Enum, Bounded)

        elevatorTopology :: Topology ElevatorVertex
        elevatorTopology = Topology [(Stationary, [Moving]), (Moving, [Stationary])]
        |]
 )

deriving via AllVertices ElevatorVertex instance RenderableVertices ElevatorVertex

{- | Configuration parameters for an elevator system
Defines the valid floor range and maximum passenger capacity
-}
data ElevatorConfig = ElevatorConfig
    { firstFloor :: Floor
    -- ^ Lowest accessible floor
    , lastFloor :: Floor
    -- ^ Highest accessible floor
    , maxCapacity :: Int
    -- ^ Maximum number of passengers
    }
    deriving stock (Show)

{- | Type-safe wrapper for floor numbers
Prevents mixing floor numbers with other integers
-}
newtype Floor = Floor {unFloor :: Int}
    deriving newtype (Show, Eq, Ord, Num)

{- | Elevator in stationary state at a specific floor
Ready to receive new floor requests and transition to moving state
-}
data StationaryElevator = StationaryElevator
    { elevatorConfig :: ElevatorConfig
    -- ^ Elevator system configuration
    , currentFloor :: Floor
    -- ^ Current floor position
    , currentCapacity :: Int
    -- ^ Current number of passengers
    }
    deriving stock (Show)

{- | Elevator in motion with a queue of instructions to execute
Processes optimized instruction batches sequentially
-}
data MovingElevator = MovingElevator
    { currentCapacity :: Int
    -- ^ Current number of passengers
    , currentFloor :: Floor
    -- ^ Current floor position
    , direction :: Direction
    -- ^ Direction of travel
    , elevatorConfig :: ElevatorConfig
    -- ^ Elevator system configuration
    , instructionsQueue :: OptimizedElevatorInstructions
    -- ^ Batched instructions to execute
    }
    deriving stock (Show)

-- | Direction of elevator travel
data Direction = Up | Down
    deriving stock (Show, Eq)

{- | Type-indexed elevator state for the state machine
Ensures type safety by tracking whether elevator is stationary or moving
-}
data ElevatorState (vertex :: ElevatorVertex) where
    StationaryState :: StationaryElevator -> ElevatorState 'Stationary
    MovingState :: MovingElevator -> ElevatorState 'Moving

{- | A request to travel between two floors
Represents a passenger's desired journey
-}
data FloorRequest = FloorRequest
    { fromFloor :: Floor
    -- ^ Origin floor
    , toFloor :: Floor
    -- ^ Destination floor
    }
    deriving stock (Show, Eq)

{- | Atomic elevator instruction (pickup or dropoff at a specific floor)
Floor requests are decomposed into these elementary operations
-}
data ElevatorInstruction = Pickup Floor | Dropoff Floor
    deriving stock (Show, Eq)

-- | Commands that can be sent to the elevator state machine
data ElevatorCommand
    = -- | Handle a single passenger request
      SingleFloorRequest FloorRequest
    | -- | Handle multiple requests efficiently
      BatchFloorRequest (Vector FloorRequest)
    | -- | Execute the current instruction queue
      Move
    deriving stock (Show, Eq)

{- | Optimized instruction queue with two-tier prioritization
Instructions are split between immediate execution and future processing
-}
data OptimizedElevatorInstructions = OptimizedElevatorInstructions
    { optimized :: Vector ElevatorInstruction
    -- ^ Instructions for immediate execution
    , unOptimized :: Vector ElevatorInstruction
    -- ^ Instructions deferred for later
    }
    deriving stock (Show, Eq)

-- | External representation of elevator state for monitoring
data ElevatorMotionState = StationaryE StationaryElevator | MovingE MovingElevator
    deriving stock (Show)

-- -------------------------------------------------------------------------- --
--                                   Helpers                                  --
-- -------------------------------------------------------------------------- --

-- | Extract the target floor from an elevator instruction
getInstructionTargetFloor :: ElevatorInstruction -> Floor
getInstructionTargetFloor (Pickup x) = x
getInstructionTargetFloor (Dropoff x) = x

{- | Determine the overall direction for a set of instructions
Returns the direction of the first valid instruction, or Nothing if all are same-floor
-}
determineADirection :: Floor -> Vector ElevatorInstruction -> Maybe Direction
determineADirection currentFloor = V.headM . V.mapMaybe (determineInstructionDirection currentFloor)

{- | Sort instructions by distance from current floor (nearest first)
Uses Vector → List → Vector conversion for sorting (could be optimized)
-}
sortByDistanceToCurrentFloor :: Floor -> Vector ElevatorInstruction -> Vector ElevatorInstruction
sortByDistanceToCurrentFloor currentFloor = V.fromList . L.sortBy (\a b -> compare (abs (getInstructionTargetFloor a - currentFloor)) (abs (getInstructionTargetFloor b - currentFloor))) . V.toList

-- | Calculate current capacity as a percentage of maximum capacity
calculateCapacityPercentage :: (HasField "currentCapacity" a Int, HasField "elevatorConfig" a ElevatorConfig) => a -> Double
calculateCapacityPercentage e = calculatePercentage e.currentCapacity e.elevatorConfig.maxCapacity

-- | Generic percentage calculation utility
calculatePercentage :: Int -> Int -> Double
calculatePercentage currentCapacity maxCapacity =
    (fromIntegral currentCapacity / fromIntegral maxCapacity) * 100

{- | Calculate the maximum number of instructions to include in the primary batch.
This represents 70% of the elevator's maximum capacity, providing a balance
between efficiency and flexibility in instruction scheduling.
-}
calculatePrimaryBatchLimit :: ElevatorConfig -> Int
calculatePrimaryBatchLimit config =
    floor (fromIntegral config.maxCapacity * (0.70 :: Double))

-- | Determine travel direction for a single instruction relative to current floor
determineInstructionDirection :: Floor -> ElevatorInstruction -> Maybe Direction
determineInstructionDirection currentFloor instruction =
    let targetFloor = getInstructionTargetFloor instruction
     in if currentFloor < targetFloor
            then Just Up
            else
                if currentFloor > targetFloor
                    then Just Down
                    else Nothing

-- -------------------------------------------------------------------------- --
--                                   Program                                  --
-- -------------------------------------------------------------------------- --

{- | Convert a floor request into atomic elevator instructions
Validates floor bounds and generates pickup/dropoff instruction pairs
Returns empty vector for invalid requests or same-floor requests
-}
floorRequestToElevatorInstruction :: ElevatorConfig -> FloorRequest -> Vector ElevatorInstruction
floorRequestToElevatorInstruction ElevatorConfig{firstFloor, lastFloor} (FloorRequest from to) =
    let fromIsValid = from >= firstFloor && from <= lastFloor
        toIsValid = to >= firstFloor && to <= lastFloor
     in if fromIsValid && toIsValid
            then
                if from == to
                    then V.empty -- No-op for same-floor requests
                    else V.fromList [Pickup from, Dropoff to]
            else V.empty -- Invalid floor requests are ignored

-- | Convert multiple floor requests into a flat vector of instructions
floorRequestsToElevatorInstructions :: ElevatorConfig -> Vector FloorRequest -> Vector ElevatorInstruction
floorRequestsToElevatorInstructions config = V.concatMap (floorRequestToElevatorInstruction config)

{- | Initial optimization of floor requests when transitioning from stationary to moving
Entry point for the instruction optimization pipeline
-}
optimizeInitialFloorRequestOrder :: ElevatorConfig -> Int -> Floor -> Vector FloorRequest -> Maybe (Direction, OptimizedElevatorInstructions)
optimizeInitialFloorRequestOrder config currentCapacity currentFloor floorRequests =
    let instructions = floorRequestsToElevatorInstructions config floorRequests
     in if V.null instructions
            then Nothing -- No valid instructions to process
            else Just $ optimizeInstructionOrder config currentCapacity currentFloor Up instructions

{- | Optimize elevator instruction order using a three-tier batching strategy:
  1. Primary batch: Core instructions up to capacity limit (70% of max capacity)
  2. Buffer: Small overflow allowance for flexibility (3 additional instructions)
  3. Remaining: Everything else deferred to unoptimized queue

The algorithm prioritizes different instruction types based on current capacity:
- High capacity (≥60%): Prioritize dropoffs to free up space
- Empty capacity (≤0%): Prioritize pickups to utilize capacity
- Normal capacity: Process all instructions without type-based prioritization
-}
optimizeInstructionOrder :: ElevatorConfig -> Int -> Floor -> Direction -> Vector ElevatorInstruction -> (Direction, OptimizedElevatorInstructions)
-- optimizeIns/tructionOrder _ _ _ currentDirection V.empty = (currentDirection, OptimizedElevatorInstructions V.empty V.empty)
optimizeInstructionOrder config currentCapacity currentFloor currentDirection instructions
    | V.null instructions = (currentDirection, OptimizedElevatorInstructions V.empty V.empty)
    | capacityPercentage >= capacityThreshold =
        let ~(priorityInstructions, deferredInstructions) = V.partition isDropoffInstruction instructions
            targetDirection = fromMaybe currentDirection $ determineADirection currentFloor priorityInstructions -- Use the current direction if all priority instructions are to the same floor or if there are no priority instructions
            ~(inDirectionInstructions, oppositeDirectionInstructions) = V.partition (isInTargetDirection targetDirection . getInstructionTargetFloor) priorityInstructions
            -- Primary batch: Take up to 70% of capacity worth of instructions, sorted by distance
            ~(instructionsBatch, excessInstructions) = V.splitAt primaryBatchLimit . sortByDistanceToCurrentFloor currentFloor $ inDirectionInstructions
         in (targetDirection, OptimizedElevatorInstructions instructionsBatch (excessInstructions <> oppositeDirectionInstructions <> deferredInstructions))
    | capacityPercentage <= 0 =
        let ~(priorityInstructions, deferredInstructions) = V.partition isPickupInstruction instructions
            targetDirection = fromMaybe currentDirection $ determineADirection currentFloor priorityInstructions -- Use the current direction if all priority instructions are to the same floor or if there are no priority instructions
            ~(inDirectionInstructions, oppositeDirectionInstructions) = V.partition (isInTargetDirection targetDirection . getInstructionTargetFloor) priorityInstructions
            -- Primary batch: Take up to 70% of capacity worth of instructions, sorted by distance
            ~(instructionsBatch, excessInstructions) = V.splitAt primaryBatchLimit . sortByDistanceToCurrentFloor currentFloor $ inDirectionInstructions
         in (targetDirection, OptimizedElevatorInstructions instructionsBatch (excessInstructions <> oppositeDirectionInstructions <> deferredInstructions))
    | otherwise =
        let targetDirection = fromMaybe currentDirection $ determineADirection currentFloor instructions -- Use the current direction if all priority instructions are to the same floor or if there are no priority instructions
            ~(inDirectionInstructions, oppositeDirectionInstructions) = V.partition (isInTargetDirection targetDirection . getInstructionTargetFloor) instructions
            -- Primary batch: Take up to 70% of capacity worth of instructions, sorted by distance
            ~(instructionsBatch, excessInstructions) = V.splitAt primaryBatchLimit . sortByDistanceToCurrentFloor currentFloor $ inDirectionInstructions
         in (targetDirection, OptimizedElevatorInstructions instructionsBatch (excessInstructions <> oppositeDirectionInstructions))
  where
    isInTargetDirection :: Direction -> Floor -> Bool
    isInTargetDirection Up flr = flr >= currentFloor
    isInTargetDirection Down flr = flr <= currentFloor

    isDropoffInstruction :: ElevatorInstruction -> Bool
    isDropoffInstruction (Dropoff _) = True
    isDropoffInstruction _ = False

    isPickupInstruction :: ElevatorInstruction -> Bool
    isPickupInstruction (Pickup _) = True
    isPickupInstruction _ = False

    -- Capacity threshold above which we prioritize dropoffs to free up space
    capacityThreshold :: Double
    capacityThreshold = 60.0

    capacityPercentage :: Double
    capacityPercentage = calculatePercentage currentCapacity config.maxCapacity

    -- Maximum number of instructions in the primary batch (70% of max capacity)
    primaryBatchLimit :: Int
    primaryBatchLimit = calculatePrimaryBatchLimit config

{- | Execute all instructions in the elevator's queue with real-time simulation

This function recursively processes batches of optimized instructions:
1. Executes the current optimized batch with movement simulation
2. Re-optimizes remaining instructions based on new elevator state
3. Continues until all instructions are processed
4. Returns to stationary state when complete

The execution includes:
- Floor-by-floor movement simulation with delays
- Capacity updates for pickups/dropoffs
- Progress logging for monitoring
- Recursive batch processing for remaining instructions
-}
executeElevatorInstructions :: MovingElevator -> IO StationaryElevator
executeElevatorInstructions movingElevator@MovingElevator{elevatorConfig, instructionsQueue = OptimizedElevatorInstructions optimizedInstructions remainingInstructions}
    | V.null optimizedInstructions && V.null remainingInstructions = do
        let currentFloor = movingElevator.currentFloor
        pure StationaryElevator{elevatorConfig = movingElevator.elevatorConfig, currentFloor = currentFloor, currentCapacity = movingElevator.currentCapacity}
    | otherwise = do
        print optimizedInstructions
        newMovingElevator <-
            foldlM
                ( \elevatorState instruction -> do
                    let targetFloor = getInstructionTargetFloor instruction
                    if elevatorState.currentFloor == targetFloor
                        then goSameFloor elevatorState instruction
                        else do
                            let distanceToTargetFloor = (.unFloor) $ abs (elevatorState.currentFloor - targetFloor)
                            let direction = elevatorState.direction
                            putStrLn
                                ( "Elevator is moving "
                                    <> show direction
                                    <> " to floor "
                                    <> show targetFloor
                                    <> " from floor "
                                    <> show elevatorState.currentFloor
                                    <> " with a current capacity of "
                                    <> show elevatorState.currentCapacity
                                    <> " ("
                                    <> show instruction
                                    <> ")"
                                )
                            goIO distanceToTargetFloor targetFloor instruction elevatorState
                )
                movingElevator{instructionsQueue = OptimizedElevatorInstructions V.empty remainingInstructions}
                optimizedInstructions

        let (newDirection, newInstructions) =
                optimizeInstructionOrder
                    elevatorConfig
                    newMovingElevator.currentCapacity
                    newMovingElevator.currentFloor
                    newMovingElevator.direction
                    remainingInstructions
        executeElevatorInstructions (newMovingElevator{direction = newDirection, instructionsQueue = newInstructions})
  where
    -- \| Simulate elevator movement with floor-by-floor progression and delays
    -- Recursively moves the elevator one floor at a time until reaching target
    goIO :: Int -> Floor -> ElevatorInstruction -> MovingElevator -> IO MovingElevator
    goIO 0 targetFloor currentInstruction e = do
        putStrLn ("Elevator has arrived at floor " <> show targetFloor <> "\n")
        pure ((e :: MovingElevator){currentCapacity = updateElevatorCapacity currentInstruction e.currentCapacity})
    goIO remainingDistance targetFloor currentInstruction e@MovingElevator{direction = Up} = do
        let newCurrentFloor = e.currentFloor + 1
        putStrLn ("Elevator has moved up to floor " <> show newCurrentFloor)
        threadDelay floorTransitionDelayMicros
        goIO (remainingDistance - 1) targetFloor currentInstruction (moveUpOneFloor e)
    goIO remainingDistance targetFloor currentInstruction e@MovingElevator{direction = Down} = do
        let newCurrentFloor = e.currentFloor - 1
        putStrLn ("Elevator has moved down to floor " <> show newCurrentFloor)
        threadDelay floorTransitionDelayMicros
        goIO (remainingDistance - 1) targetFloor currentInstruction (moveDownOneFloor e)

    -- \| Handle instructions for the current floor (no movement needed)
    goSameFloor :: MovingElevator -> ElevatorInstruction -> IO MovingElevator
    goSameFloor e currentInstruction = do
        putStrLn ("Elevator is already at the target floor " <> show e.currentFloor <> " with a current capacity of " <> show e.currentCapacity <> " (" <> show currentInstruction <> ")\n")
        pure ((e :: MovingElevator){currentCapacity = updateElevatorCapacity currentInstruction e.currentCapacity})

    -- \| Update elevator capacity based on instruction type
    updateElevatorCapacity :: ElevatorInstruction -> Int -> Int
    updateElevatorCapacity (Pickup _) currentCapacity = currentCapacity + 1 -- Passenger enters
    updateElevatorCapacity (Dropoff _) currentCapacity = currentCapacity - 1 -- Passenger exits

    -- \| Move elevator up one floor
    moveUpOneFloor :: MovingElevator -> MovingElevator
    moveUpOneFloor e@MovingElevator{currentFloor} = e{currentFloor = currentFloor + 1}

    -- \| Move elevator down one floor
    moveDownOneFloor :: MovingElevator -> MovingElevator
    moveDownOneFloor e@MovingElevator{currentFloor} = e{currentFloor = currentFloor - 1}

    -- \| Delay between floor transitions (1 second)
    floorTransitionDelayMicros :: Int
    floorTransitionDelayMicros = 1_000_000

{- | Main elevator state machine using the Crem state machine library

Implements a two-state system:
- Stationary: Receives floor requests and transitions to Moving
- Moving: Executes instruction queues and returns to Stationary

State transitions:
- Stationary + FloorRequest(s) → Moving (with optimized instructions)
- Moving + Move → Stationary (after executing all instructions)

The state machine ensures type safety by tracking the current state at the type level.
-}
elevatorStateMachine :: ElevatorState vertex -> BaseMachineT IO ElevatorTopology ElevatorCommand ElevatorMotionState
elevatorStateMachine initialState =
    BaseMachineT
        { initialState = InitialState initialState
        , action = \case
            StationaryState stationaryElevator -> \case
                SingleFloorRequest floorRequest ->
                    let instructions = floorRequestToElevatorInstruction stationaryElevator.elevatorConfig floorRequest
                        currentFloor = stationaryElevator.currentFloor
                     in if V.null instructions
                            then pureResult (StationaryE stationaryElevator) (StationaryState stationaryElevator)
                            else
                                let direction = fromMaybe Up $ determineADirection currentFloor instructions -- Default to Up if no direction can be determined this likely means all instructions are to the same floor
                                    newMovingElevator =
                                        MovingElevator
                                            { currentCapacity = stationaryElevator.currentCapacity
                                            , currentFloor = currentFloor
                                            , direction = direction
                                            , elevatorConfig = stationaryElevator.elevatorConfig
                                            , instructionsQueue = OptimizedElevatorInstructions{optimized = instructions, unOptimized = V.empty}
                                            }
                                 in pureResult (MovingE newMovingElevator) (MovingState newMovingElevator)
                BatchFloorRequest floorRequests ->
                    case optimizeInitialFloorRequestOrder stationaryElevator.elevatorConfig stationaryElevator.currentCapacity stationaryElevator.currentFloor floorRequests of
                        Nothing -> pureResult (StationaryE stationaryElevator) (StationaryState stationaryElevator)
                        Just (direction, optimizedInstructions) ->
                            let newMovingElevator =
                                    MovingElevator
                                        { currentCapacity = stationaryElevator.currentCapacity
                                        , currentFloor = stationaryElevator.currentFloor
                                        , direction = direction
                                        , elevatorConfig = stationaryElevator.elevatorConfig
                                        , instructionsQueue = optimizedInstructions
                                        }
                             in pureResult (MovingE newMovingElevator) (MovingState newMovingElevator)
                _ -> pureResult (StationaryE stationaryElevator) (StationaryState stationaryElevator)
            MovingState movingElevator -> \case
                Move -> Machine.ActionResult $ do
                    newStationaryElevator <- executeElevatorInstructions movingElevator
                    pure (StationaryE newStationaryElevator, StationaryState newStationaryElevator)
                _ -> pureResult (MovingE movingElevator) (MovingState movingElevator)
        }

-- -------------------------------------------------------------------------- --
--                                Test Cases                                  --
-- -------------------------------------------------------------------------- --

-- | Standard batch of floor requests for testing normal operation
batchRequests :: ElevatorCommand
batchRequests =
    BatchFloorRequest $
        V.fromList
            [ FloorRequest{fromFloor = 3, toFloor = 15}
            , FloorRequest{fromFloor = 5, toFloor = 18}
            , FloorRequest{fromFloor = 2, toFloor = 12}
            , FloorRequest{fromFloor = 7, toFloor = 20}
            , FloorRequest{fromFloor = 4, toFloor = 16}
            ]

-- | Edge cases to test validation and error handling
edgeCaseRequests :: ElevatorCommand
edgeCaseRequests =
    BatchFloorRequest $
        V.fromList
            [ FloorRequest{fromFloor = 5, toFloor = 5} -- Same floor (no-op)
            , FloorRequest{fromFloor = 0, toFloor = 10} -- Invalid source floor
            , FloorRequest{fromFloor = 5, toFloor = 25} -- Invalid destination floor
            , FloorRequest{fromFloor = -1, toFloor = 5} -- Negative floor
            , FloorRequest{fromFloor = 3, toFloor = 7} -- Valid request
            ]

-- | Test case that exactly hits the instruction capacity limit (6 instructions = 3 requests)
capacityLimitRequests :: ElevatorCommand
capacityLimitRequests =
    BatchFloorRequest $
        V.fromList
            [ FloorRequest{fromFloor = 1, toFloor = 10} -- Instructions: [Pickup 1, Dropoff 10]
            , FloorRequest{fromFloor = 2, toFloor = 15} -- Instructions: [Pickup 2, Dropoff 15]
            , FloorRequest{fromFloor = 3, toFloor = 18} -- Instructions: [Pickup 3, Dropoff 18]
            ]

-- | Test case that exceeds the instruction capacity limit and forces deferrals
overCapacityRequests :: ElevatorCommand
overCapacityRequests =
    BatchFloorRequest $
        V.fromList
            [ FloorRequest{fromFloor = 1, toFloor = 8} -- Instructions: [Pickup 1, Dropoff 8]
            , FloorRequest{fromFloor = 2, toFloor = 12} -- Instructions: [Pickup 2, Dropoff 12]
            , FloorRequest{fromFloor = 3, toFloor = 15} -- Instructions: [Pickup 3, Dropoff 15]
            , FloorRequest{fromFloor = 4, toFloor = 18} -- Instructions: [Pickup 4, Dropoff 18]
            , FloorRequest{fromFloor = 5, toFloor = 20} -- Instructions: [Pickup 5, Dropoff 20]
            ] -- Total: 10 instructions (4 should be deferred)

-- | Test case simulating high capacity elevator with many dropoffs prioritized
highCapacityDropoffRequests :: ElevatorCommand
highCapacityDropoffRequests =
    BatchFloorRequest $
        V.fromList
            [ FloorRequest{fromFloor = 10, toFloor = 5} -- Going down (dropoff priority)
            , FloorRequest{fromFloor = 12, toFloor = 3} -- Going down (dropoff priority)
            , FloorRequest{fromFloor = 15, toFloor = 7} -- Going down (dropoff priority)
            , FloorRequest{fromFloor = 13, toFloor = 3} -- Going down (dropoff priority)
            , FloorRequest{fromFloor = 13, toFloor = 3} -- Going down (dropoff priority)
            , FloorRequest{fromFloor = 13, toFloor = 3} -- Going down (dropoff priority)
            , FloorRequest{fromFloor = 10, toFloor = 15} -- Going up (deferred)
            , FloorRequest{fromFloor = 8, toFloor = 18} -- Going up (deferred)
            , FloorRequest{fromFloor = 6, toFloor = 19} -- Going up (deferred)
            , FloorRequest{fromFloor = 9, toFloor = 20} -- Going up (deferred)
            ] -- When capacity > 60%, dropoffs should be prioritized

-- | Test case with mixed directions that will split optimized vs unoptimized
mixedDirectionCapacityTest :: ElevatorCommand
mixedDirectionCapacityTest =
    BatchFloorRequest $
        V.fromList
            [ FloorRequest{fromFloor = 8, toFloor = 15} -- Up from current floor 1
            , FloorRequest{fromFloor = 9, toFloor = 18} -- Up from current floor 1
            , FloorRequest{fromFloor = 11, toFloor = 20} -- Up from current floor 1
            , FloorRequest{fromFloor = 12, toFloor = 5} -- Down from pickup floor (wrong direction)
            , FloorRequest{fromFloor = 13, toFloor = 3} -- Down from pickup floor (wrong direction)
            , FloorRequest{fromFloor = 7, toFloor = 16} -- Up from current floor 1
            , FloorRequest{fromFloor = 6, toFloor = 17} -- Up from current floor 1
            ] -- Should split: up instructions optimized first, down instructions deferred

-- | Initial elevator configuration for testing
initialElevator :: StationaryElevator
initialElevator =
    StationaryElevator
        { elevatorConfig =
            ElevatorConfig
                { firstFloor = 1 -- Ground floor
                , lastFloor = 20 -- 20-story building
                , maxCapacity = 10 -- 5 passengers maximum
                }
        , currentFloor = 1 -- Start at ground floor
        , currentCapacity = 0 -- Empty initially
        }

-- | Main elevator instance ready for commands
elevator :: BaseMachineT IO ElevatorTopology ElevatorCommand ElevatorMotionState
elevator = elevatorStateMachine (StationaryState initialElevator)
