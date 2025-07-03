{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}

{- |
SMART ELEVATOR CONTROL SYSTEM

This module implements an intelligent elevator control system that optimizes passenger
requests using advanced algorithms for capacity management, direction planning, and
distance optimization.

CORE ARCHITECTURE:
The system uses a finite state machine with two primary states:
- STATIONARY: Elevator is stopped and ready to receive requests
- MOVING: Elevator is executing a planned route with optimized instructions

INTELLIGENT FEATURES:

1. CAPACITY-AWARE OPTIMIZATION:
   - Low occupancy (≤20%): Prioritizes pickups to efficiently fill the elevator
   - High occupancy (≥60%): Prioritizes dropoffs to free space for new passengers
   - Balanced occupancy (20-60%): Processes both instruction types optimally

2. MULTI-LAYERED OPTIMIZATION:
   - Layer 1: Capacity-based instruction filtering
   - Layer 2: Directional partitioning (same direction vs opposite)
   - Layer 3: Fine-grained space management with distance optimization

3. REALISTIC SIMULATION:
   - Floor-by-floor movement with configurable delays
   - Accurate passenger count tracking
   - Comprehensive progress logging

ALGORITHM EXAMPLE:
For requests: [3→10, 5→15, 7→12] starting from floor 1:
1. Optimize pickup order by distance: 3, 5, 7 (closest first)
2. Optimize dropoff order by distance: 10, 12, 15 (closest first)
3. Execute: Floor 1 → 3 (pickup) → 5 (pickup) → 7 (pickup) → 10 (dropoff) → 12 (dropoff) → 15 (dropoff)

PERFORMANCE CHARACTERISTICS:
- Uses mutable vectors with ST monad for O(1) instruction processing
- Single-pass algorithms minimize computational overhead
- Deadlock prevention ensures system reliability
- Type-safe state transitions prevent invalid operations

TESTING FRAMEWORK:
Includes comprehensive test suites for:
- Performance benchmarking with various batch sizes
- Real-world scenarios (office buildings, hospitals, apartments)
- Edge cases and boundary conditions
- Capacity optimization validation across different occupancy levels
-}
module Elevator3 (
    -- * Core Types
    ElevatorVertex (..),
    Floor (..),
    Direction (..),
    ElevatorInstruction (..),
    FloorRequest (..),
    ElevatorCommand (..),
    ElevatorConfig (..),
    StationaryElevator (..),
    MovingElevator (..),
    OptimizedElevatorInstructions (..),

    -- * State Machine Types
    ElevatorTopology,
    ElevatorMotionState (..),
    ElevatorState (..),

    -- * Core Functions
    floorRequestToElevatorInstruction,
    optimizeInitialFloorRequestOrder,
    optimizeInstructionOrder,
    partitionByCapacityOptimized,
    executeElevatorInstructions,
    elevatorStateMachine,

    -- * Test Data and Examples
    initialElevator,
    elevator,
    batchRequests,

    -- * Test Suites
    benchmarkSuite,
    realWorldSuite,
    edgeCaseSuite,
    capacityOptimizationSuite,

    -- * Individual Test Cases
    emptyBatchRequests,
    singleFloorRequestTest,
    allSameFloorRequests,
    allInvalidFloorRequests,
    boundaryFloorRequests,
    highCapacityStressTest,
    lowCapacityOptimizationTest,
    maxCapacityBreachTest,
    conflictingDirectionsStressTest,
    distanceOptimizationTest,
    officeBuilding9AmRushTest,
    apartmentBuildingEveningTest,
    hospitalEmergencyTest,
    shuttleServiceTest,
    pathologicalWorstCaseTest,
    smallBatchTest,
    mediumBatchTest,
    largeBatchTest,
    scanAlgorithmTest,
    allFromCurrentFloorTest,
    allToCurrentFloorTest,
    customBatchRequests,
    edgeCaseRequests,
    capacityLimitRequests,
    overCapacityRequests,
    highCapacityDropOffRequests,
    mixedDirectionCapacityTest,
) where

import Data.Singletons.Base.TH

import Crem.BaseMachine qualified as Machine
import Data.Text.IO qualified as TIO
import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro qualified as VA
import Data.Vector.Generic qualified as VG
import Data.Vector.Mutable qualified as MV

import Control.Concurrent (threadDelay)
import Control.Monad (foldM_)
import Control.Monad.ST (runST)
import Crem.BaseMachine (
    BaseMachineT (..),
    InitialState (InitialState),
    pureResult,
 )
import Crem.Render.RenderableVertices (AllVertices (..), RenderableVertices)
import Crem.Topology (
    STopology (STopology),
    Topology (Topology),
    TopologySym0,
 )

import Data.Foldable (foldlM)
import Data.Maybe (fromMaybe)
import Data.STRef (modifySTRef, newSTRef, readSTRef)
import Data.String (fromString)
import Data.Text (Text)
import Data.Tuple (swap)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import GHC.Records (HasField)
import Optics.Core (over)

-- ELEVATOR STATES: The elevator can be in one of two states
$( singletons
    [d|
        data ElevatorVertex = Stationary | Moving
            deriving stock (Eq, Show, Enum, Bounded)

        -- ELEVATOR STATE MACHINE: Defines how the elevator can change between states
        -- Stationary can become Moving, and Moving can become Stationary
        elevatorTopology :: Topology ElevatorVertex
        elevatorTopology = Topology [(Stationary, [Moving]), (Moving, [Stationary])]
        |]
 )

deriving via AllVertices ElevatorVertex instance RenderableVertices ElevatorVertex

{- | ELEVATOR SETUP INFORMATION
This contains the basic rules for how the elevator works:
- Which floors it can visit (lowest and highest)
- How many people it can carry at once
-}
data ElevatorConfig = ElevatorConfig
    { firstFloor :: Floor -- The bottom floor
    , lastFloor :: Floor -- The top floor
    , maxOccupancy :: Int -- Maximum passengers (like 10 people)
    }
    deriving stock (Show, Generic)

{- | Type-safe wrapper for floor numbers
Prevents mixing floor numbers with other integers
-}
newtype Floor = Floor {unFloor :: Int}
    deriving stock (Generic)
    deriving newtype (Show, Eq, Ord, Num)

{- | STOPPED ELEVATOR
This represents an elevator that is stopped at a floor and waiting.
It knows:
- Where it is (current floor)
- How many passengers are inside
- What the elevator's rules are (config)
-}
data StationaryElevator = StationaryElevator
    { elevatorConfig :: ElevatorConfig
    -- ^ Elevator system configuration
    , currentFloor :: Floor
    -- ^ Current floor position
    , currentOccupancy :: Int
    -- ^ Current number of passengers
    }
    deriving stock (Show, Generic)

{- | MOVING ELEVATOR
This represents an elevator that is currently traveling between floors.
It knows everything a stopped elevator knows, plus:
- Which direction it's going (up or down)
- What tasks it needs to do (pickup and dropoff instructions)
-}
data MovingElevator = MovingElevator
    { currentOccupancy :: Int -- How many passengers are inside
    , currentFloor :: Floor -- Which floor it's at right now
    , direction :: Direction -- Which way it's moving (up or down)
    , elevatorConfig :: ElevatorConfig -- The elevator's basic rules
    , instructionsQueue :: OptimizedElevatorInstructions -- List of tasks to do
    }
    deriving stock (Show, Generic)

-- | Direction of elevator travel
data Direction = Up | Down
    deriving stock (Show, Eq, Generic)

{- | Type-indexed elevator state for the state machine
Ensures type safety by tracking whether elevator is stationary or moving
-}
data ElevatorState (vertex :: ElevatorVertex) where
    StationaryState :: StationaryElevator -> ElevatorState 'Stationary
    MovingState :: MovingElevator -> ElevatorState 'Moving

{- | PASSENGER REQUEST
When someone wants to use the elevator, they make a request like this:
"I want to go from floor 5 to floor 12"
This is what we call a FloorRequest.
-}
data FloorRequest = FloorRequest
    { fromFloor :: Floor -- Where the passenger is now
    , toFloor :: Floor -- Where the passenger wants to go
    }
    deriving stock (Show, Eq, Generic)

{- | ELEVATOR TASK
A passenger request gets broken down into two simple tasks:
1. "Pickup": Stop at a floor and let someone in
2. "DropOff": Stop at a floor and let someone out

For example, "go from floor 5 to floor 12" becomes:
- Pickup Floor 5 (let passenger in)
- DropOff Floor 12 (let passenger out)
-}
data ElevatorInstruction = Pickup Floor | DropOff Floor
    deriving stock (Show, Eq, Generic)

-- | Commands that can be sent to the elevator state machine
data ElevatorCommand
    = SingleFloorRequest FloorRequest -- "Handle one passenger request"
    | BatchFloorRequest (Vector FloorRequest) -- "Handle multiple passenger requests at once"
    | Move -- "Start moving and do your tasks"
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
    deriving stock (Show, Eq, Generic)

-- | External representation of elevator state for monitoring
data ElevatorMotionState = StationaryE StationaryElevator | MovingE MovingElevator
    deriving stock (Show)

class (Monad m) => MonadSimulateElevator m where
    logElevatorMotion :: Text -> m ()
    logOut :: (Show a) => a -> m ()
    awaitFloorTransition :: Int -> m ()

instance MonadSimulateElevator IO where
    logElevatorMotion = TIO.putStrLn
    logOut = TIO.putStrLn . tshow
    awaitFloorTransition seconds = threadDelay (seconds * 1000000) -- Convert seconds to microseconds

-- -------------------------------------------------------------------------- --
--                              HELPER FUNCTIONS                             --
-- -------------------------------------------------------------------------- --

-- | Extract the target floor from an elevator instruction
getInstructionTargetFloor :: ElevatorInstruction -> Floor
getInstructionTargetFloor (Pickup x) = x
getInstructionTargetFloor (DropOff x) = x

{- | Determine the overall direction for a set of instructions
Returns the direction of the first valid instruction that requires movement.
Returns Nothing if all instructions are for the current floor.
-}
determineADirection :: Floor -> Vector ElevatorInstruction -> Maybe Direction
determineADirection currentFloor = safeHeadV . V.mapMaybe (determineInstructionDirection currentFloor)

-- | Safely get the first element of a vector, returning Nothing if empty
safeHeadV :: Vector a -> Maybe a
safeHeadV v
    | V.null v = Nothing
    | otherwise = Just (V.head v)

{- | SORT TASKS BY DISTANCE (CLOSEST FIRST)
This function arranges tasks in order of how close they are to our current floor.
It's like organizing a shopping list by which stores are closest to your house.

WHY THIS MATTERS:
If we're on floor 5 and need to visit floors 3, 10, and 6, it makes sense to go:
Floor 6 first (1 floor away), then floor 3 (2 floors away), then floor 10 (5 floors away).

EXAMPLE: Current floor 5, tasks for floors [10, 3, 6]
Result: [6, 3, 10] (sorted by distance: 1, 2, 5 floors away)
-}
sortByDistanceToCurrentFloor :: Floor -> Vector ElevatorInstruction -> Vector ElevatorInstruction
sortByDistanceToCurrentFloor currentFloor instructions
    | V.null instructions = V.empty -- If no tasks, return empty list
    | otherwise = VG.create $ do
        -- Otherwise, sort them by distance
        mutableVec <- VG.thaw instructions
        VA.sortBy compareDistance mutableVec -- Sort using our distance comparison
        pure mutableVec
  where
    -- Compare two tasks by their distance from current floor
    compareDistance :: ElevatorInstruction -> ElevatorInstruction -> Ordering
    compareDistance a b = compare (distanceToCurrentFloor a) (distanceToCurrentFloor b)
    -- Calculate how far a task's floor is from our current floor
    distanceToCurrentFloor :: ElevatorInstruction -> Floor
    distanceToCurrentFloor inst = abs (getInstructionTargetFloor inst - currentFloor)

-- | Calculate current capacity as a percentage of maximum capacity
calculateCapacityPercentage :: (HasField "currentOccupancy" a Int, HasField "elevatorConfig" a ElevatorConfig) => a -> Double
calculateCapacityPercentage e = calculatePercentage e.currentOccupancy e.elevatorConfig.maxOccupancy

-- | Calculate what percentage one number represents of another
calculatePercentage :: Int -> Int -> Double
calculatePercentage currentOccupancy maxOccupancy =
    (fromIntegral currentOccupancy / fromIntegral maxOccupancy) * 100

{- | Determine travel direction for a single instruction relative to current floor
Returns Nothing if the instruction is for the current floor (no movement needed)
-}
determineInstructionDirection :: Floor -> ElevatorInstruction -> Maybe Direction
determineInstructionDirection currentFloor instruction =
    let targetFloor = getInstructionTargetFloor instruction
     in if currentFloor < targetFloor
            then Just Up -- Target floor is higher, so go up
            else
                if currentFloor > targetFloor
                    then Just Down -- Target floor is lower, so go down
                    else Nothing -- Target floor is the same, no movement needed

-- Define capacity-based prioritization thresholds
lowCapacityThreshold :: Double
lowCapacityThreshold = 20.0 -- Below 20% - focus on pickups

highCapacityThreshold :: Double
highCapacityThreshold = 65.0 -- Above 65% - focus on dropoffs

tshow :: (Show a) => a -> Text
tshow = fromString . show

-- -------------------------------------------------------------------------- --
--                              MAIN PROGRAM FUNCTIONS                       --
-- -------------------------------------------------------------------------- --

{- | Convert passenger request to elevator instructions
Transforms a floor request into specific elevator tasks.

Process:
1. Validates that both source and destination floors are within building limits
2. Handles edge cases (same floor requests, invalid floors)
3. Creates pickup and dropoff instructions for valid requests

Examples:
- Request: Floor 3 → Floor 7 → Result: [Pickup Floor 3, DropOff Floor 7]
- Request: Floor 5 → Floor 5 → Result: [] (no movement needed)
- Request: Floor 0 → Floor 10 → Result: [] (floor 0 invalid)
-}
floorRequestToElevatorInstruction :: ElevatorConfig -> FloorRequest -> Vector ElevatorInstruction
floorRequestToElevatorInstruction ElevatorConfig{firstFloor, lastFloor} (FloorRequest from dest) =
    let fromIsValid = from >= firstFloor && from <= lastFloor -- Check if starting floor exists
        toIsValid = dest >= firstFloor && dest <= lastFloor -- Check if destination floor exists
     in if fromIsValid && toIsValid -- Both floors must be valid
            then
                if from == dest
                    then V.empty -- No-op for same-floor requests
                    else V.fromList [Pickup from, DropOff dest]
            else V.empty -- Invalid floor requests are ignored

{- | Convert multiple floor requests into a flat vector of instructions
Processes each request through floorRequestToElevatorInstruction and combines results
-}
floorRequestsToElevatorInstructions :: ElevatorConfig -> Vector FloorRequest -> Vector ElevatorInstruction
floorRequestsToElevatorInstructions config = V.concatMap (floorRequestToElevatorInstruction config)

{- | Initialize the elevator optimization process
This is the entry point for handling multiple passenger requests simultaneously.

Process:
1. Convert all passenger requests into elevator instructions
2. Filter out invalid requests (return Nothing if no valid tasks exist)
3. Determine optimal initial direction based on valid instructions
4. Apply the complete optimization algorithm to organize tasks efficiently

Returns Nothing if no valid instructions can be generated from the requests.
-}
optimizeInitialFloorRequestOrder :: ElevatorConfig -> Int -> Floor -> Vector FloorRequest -> Maybe (Direction, OptimizedElevatorInstructions)
optimizeInitialFloorRequestOrder config currentOccupancy currentFloor floorRequests =
    let instructions = floorRequestsToElevatorInstructions config floorRequests
     in if V.null instructions
            then Nothing -- No valid instructions to process
            else
                let optimalDirection = fromMaybe Up $ determineADirection currentFloor instructions
                 in Just $ optimizeInstructionOrder config currentOccupancy currentFloor optimalDirection instructions

{- | Optimize elevator instruction order using multi-layered capacity-aware prioritization

This function implements a sophisticated optimization algorithm with three layers:

LAYER 1 - CAPACITY-BASED INSTRUCTION FILTERING:
- Low occupancy (≤20%): Prioritize pickups to efficiently fill the elevator
- High occupancy (≥65%): Prioritize dropoffs to free up space for new passengers
- Normal occupancy (20-65%): Process all instructions without type-based filtering

LAYER 2 - DIRECTIONAL PARTITIONING:
- Separate instructions by travel direction relative to current floor
- Prioritize instructions in the target direction, defer opposite direction

LAYER 3 - FINE-GRAINED CAPACITY OPTIMIZATION:
- Apply partitionByCapacityOptimized for detailed space management
- Sort final batch by distance from current floor for efficient movement

Returns: (optimal_direction, OptimizedElevatorInstructions)
- optimal_direction: Best direction to move (Up/Down)
- OptimizedElevatorInstructions: Immediate batch + deferred instructions

This multi-layered approach ensures both capacity efficiency and movement optimization.
-}
optimizeInstructionOrder :: ElevatorConfig -> Int -> Floor -> Direction -> Vector ElevatorInstruction -> (Direction, OptimizedElevatorInstructions)
optimizeInstructionOrder config currentOccupancy currentFloor currentDirection instructions
    | V.null instructions = (currentDirection, OptimizedElevatorInstructions V.empty V.empty)
    | otherwise =
        let ~partitionedInstructions@(initialPriorityInstructions, _)
                | capacityPercentage <= lowCapacityThreshold = V.partition isPickupInstruction instructions
                | capacityPercentage >= highCapacityThreshold = V.partition isDropOffInstruction instructions
                | otherwise = V.partition (const True) instructions
            ~(priorityInstructions, otherInstructions) = if V.null initialPriorityInstructions then swap partitionedInstructions else partitionedInstructions
            availableCapacity = config.maxOccupancy - currentOccupancy
            targetDirection = fromMaybe currentDirection $ determineADirection currentFloor priorityInstructions
            ~(inDirectionInstructions, oppositeDirectionInstructions) = V.partition (isInTargetDirection targetDirection . getInstructionTargetFloor) priorityInstructions
            (instructionsBatch, deferredBatch) = partitionByCapacityOptimized config.maxOccupancy availableCapacity inDirectionInstructions
            sortedInstructionsBatch = sortByDistanceToCurrentFloor currentFloor instructionsBatch
         in (targetDirection, OptimizedElevatorInstructions sortedInstructionsBatch (deferredBatch <> oppositeDirectionInstructions <> otherInstructions))
  where
    -- \| Check if a floor is in the target direction from current floor
    isInTargetDirection :: Direction -> Floor -> Bool
    isInTargetDirection Up flr = flr >= currentFloor
    isInTargetDirection Down flr = flr <= currentFloor

    -- \| Check if instruction is a dropoff operation
    isDropOffInstruction :: ElevatorInstruction -> Bool
    isDropOffInstruction (DropOff _) = True
    isDropOffInstruction _ = False

    -- \| Check if instruction is a pickup operation
    isPickupInstruction :: ElevatorInstruction -> Bool
    isPickupInstruction (Pickup _) = True
    isPickupInstruction _ = False

    -- \| Current occupancy as percentage of maximum capacity
    capacityPercentage :: Double
    capacityPercentage = calculatePercentage currentOccupancy config.maxOccupancy

{- | Advanced capacity-aware instruction partitioning using mutable vectors

This function intelligently partitions elevator instructions based on current occupancy
using a sophisticated priority system implemented with mutable vectors for efficiency.

OCCUPANCY-BASED PRIORITIZATION RULES:
- Low occupancy (≤20%): Prioritize pickups to efficiently fill the elevator
- High occupancy (≥65%): Prioritize dropoffs to free up space for new passengers
- Medium occupancy (20-65%): Process both instruction types if space permits

ALGORITHM IMPLEMENTATION:
1. Calculate current occupancy percentage from available space
2. Initialize mutable vectors for efficient batch construction
3. Apply occupancy-based prioritization rules with space constraints
4. Handle edge cases (no space, wrong priority) by deferring instructions
5. Apply deadlock prevention: swap results if no instructions can be processed

PERFORMANCE OPTIMIZATIONS:
- Uses ST monad with mutable vectors for O(1) append operations
- Processes instructions in a single pass with constant space overhead
- Avoids expensive list concatenations and repeated vector operations

Returns: (immediate_instructions, deferred_instructions)
- immediate_instructions: Tasks that fit current capacity constraints
- deferred_instructions: Tasks to process in subsequent batches

This approach prevents capacity overflow while maximizing elevator utilization.
-}
partitionByCapacityOptimized :: Int -> Int -> Vector ElevatorInstruction -> (Vector ElevatorInstruction, Vector ElevatorInstruction)
partitionByCapacityOptimized maxOccupancy availableSpaceLeft instructions = runST $ do
    -- Initialize mutable vectors for building batches
    priorityTasksMVec <- MV.new (V.length instructions)
    deferredTasksMVec <- MV.new (V.length instructions)

    let currentOccupancy = maxOccupancy - availableSpaceLeft
        occupancyPercentage = calculatePercentage currentOccupancy maxOccupancy

    -- Initialize index counters for tracking vector positions
    priorityTaskMVecIdxRef <- newSTRef @Int 0
    deferredTaskMVecIdxRef <- newSTRef @Int 0

    -- Process each instruction according to capacity-based rules
    foldM_
        ( \remainingSpace instr -> case instr of
            -- LOW OCCUPANCY CASE: Elevator nearly empty - prioritize pickups
            -- Only process pickups when space is available and occupancy is low
            Pickup _ | occupancyPercentage <= lowCapacityThreshold && remainingSpace > 0 -> do
                currIdx <- readSTRef priorityTaskMVecIdxRef
                MV.write priorityTasksMVec currIdx instr -- Add pickup to immediate batch
                modifySTRef priorityTaskMVecIdxRef (+ 1) -- Increment position counter
                pure (remainingSpace - 1) -- Reduce available space

            -- HIGH OCCUPANCY CASE: Elevator nearly full - prioritize dropoffs
            -- Always process dropoffs when occupancy is high (they free space)
            DropOff _ | occupancyPercentage >= highCapacityThreshold -> do
                currIdx <- readSTRef priorityTaskMVecIdxRef
                MV.write priorityTasksMVec currIdx instr -- Add dropoff to immediate batch
                modifySTRef priorityTaskMVecIdxRef (+ 1) -- Increment position counter
                pure (remainingSpace + 1) -- Increase available space

            -- MEDIUM OCCUPANCY CASE: Balanced load - process both types
            -- Handle both pickups and dropoffs if space constraints allow
            instr' | remainingSpace > 0 && occupancyPercentage > lowCapacityThreshold && occupancyPercentage < highCapacityThreshold -> do
                currIdx <- readSTRef priorityTaskMVecIdxRef
                MV.write priorityTasksMVec currIdx instr' -- Add instruction to immediate batch
                modifySTRef priorityTaskMVecIdxRef (+ 1) -- Increment position counter
                case instr' of
                    Pickup _ -> pure (remainingSpace - 1) -- Pickup reduces available space
                    DropOff _ -> pure (remainingSpace + 1) -- Dropoff increases available space

            -- DEFERRAL CASE: All other scenarios
            -- Includes: no space available, wrong priority for current occupancy, edge cases
            _ -> do
                currIdx <- readSTRef deferredTaskMVecIdxRef
                MV.write deferredTasksMVec currIdx instr -- Add to deferred batch
                modifySTRef deferredTaskMVecIdxRef (+ 1) -- Increment position counter
                pure remainingSpace -- Space unchanged (deferred)
        )
        availableSpaceLeft
        instructions

    -- Finalize batch construction
    priorityTaskMVecIdx <- readSTRef priorityTaskMVecIdxRef -- Count of immediate instructions
    deferredTaskMVecIdx <- readSTRef deferredTaskMVecIdxRef -- Count of deferred instructions

    -- Convert mutable vectors to immutable vectors with exact sizing
    priorityTasks <- V.freeze . MV.take priorityTaskMVecIdx $ priorityTasksMVec
    deferredTasks <- V.freeze . MV.take deferredTaskMVecIdx $ deferredTasksMVec

    -- DEADLOCK PREVENTION: Ensure progress is always possible
    -- If no instructions can be processed immediately, try deferred instructions
    -- This prevents the elevator from getting stuck in impossible situations
    if V.null priorityTasks
        then pure (deferredTasks, priorityTasks) -- Emergency fallback: swap batches
        else pure (priorityTasks, deferredTasks) -- Normal operation: return as planned

{- | Execute elevator instructions and simulate realistic movement

This function implements the elevator's physical movement and task execution.
It processes instructions in batches, simulating realistic elevator behavior
with floor-by-floor movement and passenger capacity updates.

EXECUTION ALGORITHM:
1. Check termination condition - stop if no instructions remain
2. Process current batch of optimized instructions sequentially
3. For each instruction: simulate movement, update passenger count, log progress
4. Re-optimize remaining instructions based on new elevator state
5. Recursively continue until all tasks are completed

MOVEMENT SIMULATION:
- Moves floor-by-floor with realistic 1-second delays between floors
- Updates passenger count for pickups (+1) and dropoffs (-1)
- Provides detailed progress logging for monitoring and debugging
- Handles both same-floor operations and multi-floor travel

BATCH PROCESSING:
- Executes current optimized batch completely before re-planning
- Re-optimizes remaining instructions after each batch completion
- Considers updated elevator state (position, occupancy) for re-optimization
- Continues until no instructions remain

Returns: StationaryElevator ready for new passenger requests

Input: MovingElevator with instructions to execute
Output: StationaryElevator at final position with updated occupancy
-}
executeElevatorInstructions :: forall m. (MonadSimulateElevator m) => MovingElevator -> m StationaryElevator
executeElevatorInstructions movingElevator@MovingElevator{elevatorConfig, instructionsQueue = OptimizedElevatorInstructions optimizedInstructions remainingInstructions}
    -- STOP CONDITION: If there are no more tasks to do, stop the elevator
    | V.null optimizedInstructions && V.null remainingInstructions = do
        let currentFloor = movingElevator.currentFloor
        pure StationaryElevator{elevatorConfig = movingElevator.elevatorConfig, currentFloor = currentFloor, currentOccupancy = movingElevator.currentOccupancy}

    -- EXECUTION CONDITION: We have tasks to do, so let's do them
    | otherwise = do
        logOut optimizedInstructions -- Show what tasks we're about to do

        -- EXECUTE CURRENT BATCH: Go through each task in our optimized list
        newMovingElevator <-
            foldlM
                ( \elevatorState instruction -> do
                    let targetFloor = getInstructionTargetFloor instruction

                    -- DECISION: Are we already at the target floor?
                    if elevatorState.currentFloor == targetFloor
                        then goSameFloor elevatorState instruction
                        else do
                            let distanceToTargetFloor = (.unFloor) $ abs (elevatorState.currentFloor - targetFloor)
                            let direction = elevatorState.direction
                            logElevatorMotion
                                ( "Elevator is moving "
                                    <> tshow direction
                                    <> " to floor "
                                    <> tshow targetFloor
                                    <> " from floor "
                                    <> tshow elevatorState.currentFloor
                                    <> " with a current capacity of "
                                    <> tshow elevatorState.currentOccupancy
                                    <> " ("
                                    <> tshow instruction
                                    <> ")"
                                )
                            goIO distanceToTargetFloor targetFloor instruction elevatorState
                )
                -- STARTING STATE: Clean slate for next batch (keep remaining tasks for later)
                movingElevator{instructionsQueue = OptimizedElevatorInstructions V.empty remainingInstructions}
                optimizedInstructions

        -- PREPARE NEXT BATCH: Now that we've completed current tasks, reorganize what's left
        let (newDirection, newInstructions) =
                optimizeInstructionOrder
                    elevatorConfig
                    newMovingElevator.currentOccupancy
                    newMovingElevator.currentFloor
                    newMovingElevator.direction
                    remainingInstructions

        executeElevatorInstructions (newMovingElevator{direction = newDirection, instructionsQueue = newInstructions})
  where
    -- \| Simulate elevator movement with floor-by-floor progression and realistic delays
    -- Recursively moves the elevator one floor at a time until reaching the target floor
    goIO :: Int -> Floor -> ElevatorInstruction -> MovingElevator -> m MovingElevator
    goIO 0 targetFloor currentInstruction e = do
        logElevatorMotion ("Elevator has arrived at floor " <> tshow targetFloor <> "\n")
        pure $ over #currentOccupancy (updateElevatorCapacity currentInstruction) e
    goIO remainingDistance targetFloor currentInstruction e@MovingElevator{direction = Up} = do
        let newCurrentFloor = e.currentFloor + 1
        logElevatorMotion ("Elevator has moved up to floor " <> tshow newCurrentFloor)
        awaitFloorTransition floorTransitionDelaySeconds
        goIO (remainingDistance - 1) targetFloor currentInstruction (moveUpOneFloor e)
    goIO remainingDistance targetFloor currentInstruction e@MovingElevator{direction = Down} = do
        let newCurrentFloor = e.currentFloor - 1
        logElevatorMotion ("Elevator has moved down to floor " <> tshow newCurrentFloor)
        awaitFloorTransition floorTransitionDelaySeconds
        goIO (remainingDistance - 1) targetFloor currentInstruction (moveDownOneFloor e)

    -- \| Handle instructions for the current floor (no movement required)
    goSameFloor :: MovingElevator -> ElevatorInstruction -> m MovingElevator
    goSameFloor e currentInstruction = do
        logElevatorMotion ("Elevator is already at the target floor " <> tshow e.currentFloor <> " with a current capacity of " <> tshow e.currentOccupancy <> " (" <> tshow currentInstruction <> ")\n")
        pure $ over #currentOccupancy (updateElevatorCapacity currentInstruction) e

    -- \| Update elevator passenger count based on instruction type
    updateElevatorCapacity :: ElevatorInstruction -> Int -> Int
    updateElevatorCapacity (Pickup _) currentOccupancy = currentOccupancy + 1 -- Passenger enters
    updateElevatorCapacity (DropOff _) currentOccupancy = currentOccupancy - 1 -- Passenger exits

    -- \| Increment elevator floor position by one
    moveUpOneFloor :: MovingElevator -> MovingElevator
    moveUpOneFloor = over #currentFloor (+ 1)

    -- \| Decrement elevator floor position by one
    moveDownOneFloor :: MovingElevator -> MovingElevator
    moveDownOneFloor = over #currentFloor (subtract 1)

    -- \| Delay duration between floor transitions (1 second in microseconds)
    floorTransitionDelaySeconds :: Int
    floorTransitionDelaySeconds = 1

{- | Elevator state machine - central control system

This function implements the elevator's decision-making logic using a finite state machine.
It manages state transitions and command processing based on current elevator state.

STATE DEFINITIONS:
- STATIONARY: Elevator is stopped and ready to receive passenger requests
- MOVING: Elevator is executing a planned route with specific instructions

COMMAND PROCESSING BY STATE:

WHEN STATIONARY:
- SingleFloorRequest: Validate request, plan route, transition to MOVING
- BatchFloorRequest: Optimize multiple requests, plan route, transition to MOVING
- Move: Ignored (cannot move without a destination)

WHEN MOVING:
- Move: Execute planned route, return to STATIONARY when complete
- Request commands: Ignored (complete current route before accepting new requests)

SAFETY AND RELIABILITY FEATURES:
- Type-safe state transitions prevent invalid operations
- Invalid requests are handled gracefully (elevator remains in current state)
- Always returns to STATIONARY state after completing work
- Uses Maybe types to handle edge cases (no valid instructions)

OPTIMIZATION INTEGRATION:
- Single requests: Basic direction determination with simple instruction queue
- Batch requests: Full optimization algorithm with capacity-aware prioritization
- Automatic re-optimization during execution for remaining instructions

Example state flow:
1. Start: STATIONARY at floor 1
2. Receive: BatchFloorRequest [3→7, 5→12]
3. Optimize: Create plan [Pickup 3, Pickup 5, DropOff 7, DropOff 12]
4. Transition: STATIONARY → MOVING
5. Receive: Move command
6. Execute: Complete all instructions with real movement simulation
7. Transition: MOVING → STATIONARY at floor 12
-}
elevatorStateMachine :: (MonadSimulateElevator m) => ElevatorState vertex -> BaseMachineT m ElevatorTopology ElevatorCommand ElevatorMotionState
elevatorStateMachine initialState =
    BaseMachineT
        { initialState = InitialState initialState
        , action = \case
            StationaryState stationaryElevator -> \case
                -- SINGLE PASSENGER REQUEST: One person wants to travel
                SingleFloorRequest floorRequest ->
                    let instructions = floorRequestToElevatorInstruction stationaryElevator.elevatorConfig floorRequest
                        currentFloor = stationaryElevator.currentFloor
                     in if V.null instructions -- Check if the request is valid
                            then pureResult (StationaryE stationaryElevator) (StationaryState stationaryElevator) -- Invalid request: stay stopped
                            else -- Valid request: plan route and start moving
                                let direction = fromMaybe Up $ determineADirection currentFloor instructions -- Determine direction (default Up)
                                    newMovingElevator =
                                        MovingElevator
                                            { currentOccupancy = stationaryElevator.currentOccupancy
                                            , currentFloor = currentFloor
                                            , direction = direction
                                            , elevatorConfig = stationaryElevator.elevatorConfig
                                            , instructionsQueue = OptimizedElevatorInstructions{optimized = instructions, unOptimized = V.empty}
                                            }
                                 in pureResult (MovingE newMovingElevator) (MovingState newMovingElevator) -- Transition to MOVING

                -- MULTIPLE PASSENGER REQUESTS: Several people want to travel (use smart planning)
                BatchFloorRequest floorRequests ->
                    case optimizeInitialFloorRequestOrder stationaryElevator.elevatorConfig stationaryElevator.currentOccupancy stationaryElevator.currentFloor floorRequests of
                        Nothing -> pureResult (StationaryE stationaryElevator) (StationaryState stationaryElevator) -- No valid requests: stay stopped
                        Just (direction, optimizedInstructions) ->
                            -- Valid requests: use smart planning
                            let newMovingElevator =
                                    MovingElevator
                                        { currentOccupancy = stationaryElevator.currentOccupancy
                                        , currentFloor = stationaryElevator.currentFloor
                                        , direction = direction
                                        , elevatorConfig = stationaryElevator.elevatorConfig
                                        , instructionsQueue = optimizedInstructions
                                        }
                             in pureResult (MovingE newMovingElevator) (MovingState newMovingElevator) -- Transition to MOVING

                -- ANY OTHER COMMAND: Stay stopped (can't move without a destination)
                _ -> pureResult (StationaryE stationaryElevator) (StationaryState stationaryElevator)
            MovingState movingElevator -> \case
                -- MOVE COMMAND: Execute the planned route
                Move -> Machine.ActionResult $ do
                    newStationaryElevator <- executeElevatorInstructions movingElevator -- Do all the planned tasks
                    pure (StationaryE newStationaryElevator, StationaryState newStationaryElevator) -- Return to STOPPED when done
                _ -> pureResult (MovingE movingElevator) (MovingState movingElevator)
        }

-- -------------------------------------------------------------------------- --
--                             TEST SCENARIOS                                --
-- -------------------------------------------------------------------------- --
-- These are pre-made scenarios to test different aspects of the elevator system.
-- Think of them like practice problems to make sure the elevator works correctly.

{- | NORMAL OFFICE BUILDING SCENARIO
A typical day with several people wanting to travel to different floors
This tests the basic smart planning and optimization features
-}
batchRequests :: ElevatorCommand
batchRequests =
    BatchFloorRequest $
        V.fromList
            [ FloorRequest{fromFloor = 3, toFloor = 15} -- Person at floor 3 wants to go to floor 15
            , FloorRequest{fromFloor = 5, toFloor = 18} -- Person at floor 5 wants to go to floor 18
            , FloorRequest{fromFloor = 2, toFloor = 12} -- Person at floor 2 wants to go to floor 12
            , FloorRequest{fromFloor = 7, toFloor = 20} -- Person at floor 7 wants to go to floor 20
            , FloorRequest{fromFloor = 4, toFloor = 16} -- Person at floor 4 wants to go to floor 16
            ]

-- | Edge cases to test validation and error handling
edgeCaseRequests :: ElevatorCommand
edgeCaseRequests =
    BatchFloorRequest $
        V.fromList
            [ FloorRequest{fromFloor = 5, toFloor = 5} -- Person already at their destination (no movement needed)
            , FloorRequest{fromFloor = 0, toFloor = 10} -- Invalid: Floor 0 doesn't exist in this building
            , FloorRequest{fromFloor = 5, toFloor = 25} -- Invalid: Floor 25 doesn't exist (building only goes to 20)
            , FloorRequest{fromFloor = -1, toFloor = 5} -- Invalid: Negative floors don't exist
            , FloorRequest{fromFloor = 3, toFloor = 7} -- Valid request mixed in with invalid ones
            ]

{- | CAPACITY TESTING: Test exactly how many tasks the elevator can handle
This creates exactly 6 tasks (3 passenger requests × 2 tasks each = 6 total)
Tests the elevator's ability to handle its designed capacity
-}
capacityLimitRequests :: ElevatorCommand
capacityLimitRequests =
    BatchFloorRequest $
        V.fromList
            [ FloorRequest{fromFloor = 1, toFloor = 10} -- Instructions: [Pickup 1, DropOff 10]
            , FloorRequest{fromFloor = 2, toFloor = 15} -- Instructions: [Pickup 2, DropOff 15]
            , FloorRequest{fromFloor = 3, toFloor = 18} -- Instructions: [Pickup 3, DropOff 18]
            ]

-- | Test case that exceeds the instruction capacity limit and forces deferrals
overCapacityRequests :: ElevatorCommand
overCapacityRequests =
    BatchFloorRequest $
        V.fromList
            [ FloorRequest{fromFloor = 1, toFloor = 8} -- Instructions: [Pickup 1, DropOff 8]
            , FloorRequest{fromFloor = 2, toFloor = 12} -- Instructions: [Pickup 2, DropOff 12]
            , FloorRequest{fromFloor = 3, toFloor = 15} -- Instructions: [Pickup 3, DropOff 15]
            , FloorRequest{fromFloor = 4, toFloor = 18} -- Instructions: [Pickup 4, DropOff 18]
            , FloorRequest{fromFloor = 5, toFloor = 20} -- Instructions: [Pickup 5, DropOff 20]
            ] -- Total: 10 instructions (4 should be deferred)

-- | Test case simulating high capacity elevator with many dropOffs prioritized
highCapacityDropOffRequests :: ElevatorCommand
highCapacityDropOffRequests =
    BatchFloorRequest $
        V.fromList
            [ FloorRequest{fromFloor = 10, toFloor = 5} -- Going down (dropOff priority)
            , FloorRequest{fromFloor = 12, toFloor = 3} -- Going down (dropOff priority)
            , FloorRequest{fromFloor = 15, toFloor = 7} -- Going down (dropOff priority)
            , FloorRequest{fromFloor = 13, toFloor = 3} -- Going down (dropOff priority)
            , FloorRequest{fromFloor = 13, toFloor = 3} -- Going down (dropOff priority)
            , FloorRequest{fromFloor = 13, toFloor = 3} -- Going down (dropOff priority)
            , FloorRequest{fromFloor = 10, toFloor = 15} -- Going up (deferred)
            , FloorRequest{fromFloor = 8, toFloor = 18} -- Going up (deferred)
            , FloorRequest{fromFloor = 6, toFloor = 19} -- Going up (deferred)
            , FloorRequest{fromFloor = 9, toFloor = 20} -- Going up (deferred)
            ] -- When capacity > 60%, dropOffs should be prioritized

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

customBatchRequests :: ElevatorCommand
customBatchRequests =
    BatchFloorRequest $
        V.fromList
            [ FloorRequest{fromFloor = 9, toFloor = 1} -- Down from current floor 1
            , FloorRequest{fromFloor = 3, toFloor = 15} -- Up from current floor 1
            , FloorRequest{fromFloor = 5, toFloor = 18} -- Up from current floor 1
            ] -- Mixed direction test case

-- | Empty batch test - should handle gracefully
emptyBatchRequests :: ElevatorCommand
emptyBatchRequests = BatchFloorRequest V.empty

-- | Single floor request test - minimal case
singleFloorRequestTest :: ElevatorCommand
singleFloorRequestTest = SingleFloorRequest FloorRequest{fromFloor = 5, toFloor = 15}

-- | All same-floor requests - should result in no instructions
allSameFloorRequests :: ElevatorCommand
allSameFloorRequests =
    BatchFloorRequest $
        V.fromList
            [ FloorRequest{fromFloor = 10, toFloor = 10}
            , FloorRequest{fromFloor = 5, toFloor = 5}
            , FloorRequest{fromFloor = 15, toFloor = 15}
            , FloorRequest{fromFloor = 8, toFloor = 8}
            ]

-- | All invalid floor requests - should be filtered out
allInvalidFloorRequests :: ElevatorCommand
allInvalidFloorRequests =
    BatchFloorRequest $
        V.fromList
            [ FloorRequest{fromFloor = 0, toFloor = 10} -- Invalid source (below firstFloor)
            , FloorRequest{fromFloor = 5, toFloor = 25} -- Invalid destination (above lastFloor)
            , FloorRequest{fromFloor = -5, toFloor = 10} -- Negative source floor
            , FloorRequest{fromFloor = 10, toFloor = -3} -- Negative destination floor
            , FloorRequest{fromFloor = 22, toFloor = 25} -- Both floors invalid
            ]

-- | Boundary testing - floors at exact limits
boundaryFloorRequests :: ElevatorCommand
boundaryFloorRequests =
    BatchFloorRequest $
        V.fromList
            [ FloorRequest{fromFloor = 1, toFloor = 20} -- Min to max floor
            , FloorRequest{fromFloor = 20, toFloor = 1} -- Max to min floor
            , FloorRequest{fromFloor = 1, toFloor = 2} -- Minimal valid upward journey
            , FloorRequest{fromFloor = 20, toFloor = 19} -- Minimal valid downward journey
            , FloorRequest{fromFloor = 10, toFloor = 11} -- Single floor up
            , FloorRequest{fromFloor = 11, toFloor = 10} -- Single floor down
            ]

-- | High capacity stress test - tests dropOff prioritization at 80% capacity
highCapacityStressTest :: ElevatorCommand
highCapacityStressTest =
    BatchFloorRequest $
        V.fromList
            [ FloorRequest{fromFloor = 15, toFloor = 5} -- DropOff (should be prioritized)
            , FloorRequest{fromFloor = 18, toFloor = 3} -- DropOff (should be prioritized)
            , FloorRequest{fromFloor = 12, toFloor = 7} -- DropOff (should be prioritized)
            , FloorRequest{fromFloor = 19, toFloor = 2} -- DropOff (should be prioritized)
            , FloorRequest{fromFloor = 16, toFloor = 8} -- DropOff (should be prioritized)
            , FloorRequest{fromFloor = 2, toFloor = 18} -- Pickup (should be deferred)
            , FloorRequest{fromFloor = 4, toFloor = 19} -- Pickup (should be deferred)
            , FloorRequest{fromFloor = 6, toFloor = 17} -- Pickup (should be deferred)
            ] -- Use with high currentOccupancy (8/10) to test dropOff prioritization

-- | Low capacity optimization test - tests pickup prioritization when empty
lowCapacityOptimizationTest :: ElevatorCommand
lowCapacityOptimizationTest =
    BatchFloorRequest $
        V.fromList
            [ FloorRequest{fromFloor = 3, toFloor = 15} -- Pickup (should be prioritized)
            , FloorRequest{fromFloor = 5, toFloor = 18} -- Pickup (should be prioritized)
            , FloorRequest{fromFloor = 7, toFloor = 12} -- Pickup (should be prioritized)
            , FloorRequest{fromFloor = 2, toFloor = 20} -- Pickup (should be prioritized)
            , FloorRequest{fromFloor = 15, toFloor = 5} -- DropOff (should be deferred - can't drop off when empty)
            , FloorRequest{fromFloor = 18, toFloor = 8} -- DropOff (should be deferred)
            , FloorRequest{fromFloor = 12, toFloor = 3} -- DropOff (should be deferred)
            ] -- Use with currentOccupancy = 0 to test pickup prioritization

-- | Maximum capacity breach test - tests capacity constraint handling
maxCapacityBreachTest :: ElevatorCommand
maxCapacityBreachTest =
    BatchFloorRequest $
        V.fromList
            [ FloorRequest{fromFloor = 1, toFloor = 20} -- 15 total instructions exceed capacity
            , FloorRequest{fromFloor = 2, toFloor = 19}
            , FloorRequest{fromFloor = 3, toFloor = 18}
            , FloorRequest{fromFloor = 4, toFloor = 17}
            , FloorRequest{fromFloor = 5, toFloor = 16}
            , FloorRequest{fromFloor = 6, toFloor = 15}
            , FloorRequest{fromFloor = 7, toFloor = 14}
            , FloorRequest{fromFloor = 8, toFloor = 13}
            , FloorRequest{fromFloor = 9, toFloor = 12}
            , FloorRequest{fromFloor = 10, toFloor = 11}
            ] -- 20 instructions total, should trigger capacity partitioning

-- | Conflicting directions stress test - mixed up/down requests
conflictingDirectionsStressTest :: ElevatorCommand
conflictingDirectionsStressTest =
    BatchFloorRequest $
        V.fromList
            [ FloorRequest{fromFloor = 1, toFloor = 20} -- Long up journey
            , FloorRequest{fromFloor = 20, toFloor = 1} -- Long down journey
            , FloorRequest{fromFloor = 5, toFloor = 15} -- Medium up
            , FloorRequest{fromFloor = 15, toFloor = 5} -- Medium down
            , FloorRequest{fromFloor = 8, toFloor = 12} -- Short up
            , FloorRequest{fromFloor = 12, toFloor = 8} -- Short down
            , FloorRequest{fromFloor = 2, toFloor = 18} -- Long up
            , FloorRequest{fromFloor = 18, toFloor = 2} -- Long down
            ] -- Should test direction optimization thoroughly

-- | Distance optimization test - tests SCAN algorithm efficiency
distanceOptimizationTest :: ElevatorCommand
distanceOptimizationTest =
    BatchFloorRequest $
        V.fromList
            [ FloorRequest{fromFloor = 10, toFloor = 15} -- Start from current floor (1), so pickup at 10
            , FloorRequest{fromFloor = 12, toFloor = 18} -- Pickup at 12 (close to 10)
            , FloorRequest{fromFloor = 8, toFloor = 16} -- Pickup at 8 (close to 10)
            , FloorRequest{fromFloor = 14, toFloor = 20} -- Pickup at 14 (close to 12)
            , FloorRequest{fromFloor = 6, toFloor = 17} -- Pickup at 6 (close to 8)
            , FloorRequest{fromFloor = 19, toFloor = 5} -- Pickup at 19 (far from others)
            , FloorRequest{fromFloor = 3, toFloor = 13} -- Pickup at 3 (close to start)
            ] -- Should optimize pickup order: 3,6,8,10,12,14,19

-- | Real-world office building simulation
officeBuilding9AmRushTest :: ElevatorCommand
officeBuilding9AmRushTest =
    BatchFloorRequest $
        V.fromList
            [ FloorRequest{fromFloor = 1, toFloor = 5} -- Lobby to office floors
            , FloorRequest{fromFloor = 1, toFloor = 8}
            , FloorRequest{fromFloor = 1, toFloor = 12}
            , FloorRequest{fromFloor = 1, toFloor = 15}
            , FloorRequest{fromFloor = 1, toFloor = 18}
            , FloorRequest{fromFloor = 2, toFloor = 10} -- Parking level to offices
            , FloorRequest{fromFloor = 2, toFloor = 14}
            , FloorRequest{fromFloor = 3, toFloor = 7} -- Mixed source floors
            , FloorRequest{fromFloor = 4, toFloor = 16}
            ] -- Typical morning rush pattern

-- | Real-world apartment building evening scenario
apartmentBuildingEveningTest :: ElevatorCommand
apartmentBuildingEveningTest =
    BatchFloorRequest $
        V.fromList
            [ FloorRequest{fromFloor = 8, toFloor = 1} -- Residents going to lobby/parking
            , FloorRequest{fromFloor = 12, toFloor = 1}
            , FloorRequest{fromFloor = 15, toFloor = 2}
            , FloorRequest{fromFloor = 18, toFloor = 1}
            , FloorRequest{fromFloor = 5, toFloor = 1}
            , FloorRequest{fromFloor = 20, toFloor = 3}
            , FloorRequest{fromFloor = 9, toFloor = 1}
            , FloorRequest{fromFloor = 16, toFloor = 2}
            ] -- Evening exodus pattern - mostly downward

-- | Hospital emergency scenario - mixed urgent requests
hospitalEmergencyTest :: ElevatorCommand
hospitalEmergencyTest =
    BatchFloorRequest $
        V.fromList
            [ FloorRequest{fromFloor = 1, toFloor = 4} -- ER to surgery
            , FloorRequest{fromFloor = 8, toFloor = 1} -- Patient room to ER
            , FloorRequest{fromFloor = 12, toFloor = 6} -- Room to ICU
            , FloorRequest{fromFloor = 3, toFloor = 15} -- Lab to specialist floor
            , FloorRequest{fromFloor = 18, toFloor = 2} -- Admin to pharmacy
            , FloorRequest{fromFloor = 7, toFloor = 4} -- Room to surgery
            , FloorRequest{fromFloor = 1, toFloor = 20} -- ER to roof helipad
            ] -- Mixed critical and routine requests

-- | Shuttle service test - repetitive pattern between key floors
shuttleServiceTest :: ElevatorCommand
shuttleServiceTest =
    BatchFloorRequest $
        V.fromList
            [ FloorRequest{fromFloor = 1, toFloor = 10} -- Lobby to main office floor
            , FloorRequest{fromFloor = 10, toFloor = 1} -- Return trip
            , FloorRequest{fromFloor = 1, toFloor = 15} -- Lobby to executive floor
            , FloorRequest{fromFloor = 15, toFloor = 1} -- Return trip
            , FloorRequest{fromFloor = 5, toFloor = 10} -- Conference floor to main office
            , FloorRequest{fromFloor = 10, toFloor = 5} -- Return trip
            , FloorRequest{fromFloor = 1, toFloor = 20} -- Lobby to penthouse
            , FloorRequest{fromFloor = 20, toFloor = 1} -- Return trip
            ] -- Tests handling of bidirectional patterns

-- | Pathological worst-case scenario
pathologicalWorstCaseTest :: ElevatorCommand
pathologicalWorstCaseTest =
    BatchFloorRequest $
        V.fromList
            [ FloorRequest{fromFloor = 1, toFloor = 20} -- Maximum distance up
            , FloorRequest{fromFloor = 20, toFloor = 1} -- Maximum distance down
            , FloorRequest{fromFloor = 2, toFloor = 19} -- Nearly maximum up
            , FloorRequest{fromFloor = 19, toFloor = 2} -- Nearly maximum down
            , FloorRequest{fromFloor = 3, toFloor = 18} -- Large up
            , FloorRequest{fromFloor = 18, toFloor = 3} -- Large down
            , FloorRequest{fromFloor = 4, toFloor = 17} -- Large up
            , FloorRequest{fromFloor = 17, toFloor = 4} -- Large down
            , FloorRequest{fromFloor = 5, toFloor = 16} -- Large up
            , FloorRequest{fromFloor = 16, toFloor = 5} -- Large down
            ] -- Maximum direction changes and distance

-- | Batch size progression tests - evaluate scalability with different request volumes
smallBatchTest :: ElevatorCommand
smallBatchTest =
    BatchFloorRequest $
        V.fromList
            [ FloorRequest{fromFloor = 5, toFloor = 10}
            , FloorRequest{fromFloor = 8, toFloor = 15}
            ] -- 4 instructions total - minimal batch

mediumBatchTest :: ElevatorCommand
mediumBatchTest =
    BatchFloorRequest $
        V.fromList
            [FloorRequest{fromFloor = Floor i, toFloor = Floor (i + 8)} | i <- [2, 4, 6, 8, 10, 12, 14]] -- 14 instructions - moderate batch

largeBatchTest :: ElevatorCommand
largeBatchTest =
    BatchFloorRequest $
        V.fromList
            [FloorRequest{fromFloor = Floor i, toFloor = Floor ((i * 2) `mod` 20 + 1)} | i <- [1 .. 25]] -- 50 instructions - stress test

-- | Algorithm comparison test - specific patterns to reveal algorithm differences
scanAlgorithmTest :: ElevatorCommand
scanAlgorithmTest =
    BatchFloorRequest $
        V.fromList
            [ FloorRequest{fromFloor = 10, toFloor = 15} -- Should be processed in SCAN order
            , FloorRequest{fromFloor = 5, toFloor = 12}
            , FloorRequest{fromFloor = 15, toFloor = 18}
            , FloorRequest{fromFloor = 3, toFloor = 8}
            , FloorRequest{fromFloor = 18, toFloor = 20}
            , FloorRequest{fromFloor = 2, toFloor = 6}
            ] -- Optimal SCAN order should be: 2,3,5,10,15,18

-- | Edge case: All pickups from current floor
allFromCurrentFloorTest :: ElevatorCommand
allFromCurrentFloorTest =
    BatchFloorRequest $
        V.fromList
            [ FloorRequest{fromFloor = 1, toFloor = 5} -- All start from current floor (1)
            , FloorRequest{fromFloor = 1, toFloor = 10}
            , FloorRequest{fromFloor = 1, toFloor = 15}
            , FloorRequest{fromFloor = 1, toFloor = 20}
            , FloorRequest{fromFloor = 1, toFloor = 8}
            , FloorRequest{fromFloor = 1, toFloor = 12}
            ] -- Tests optimization when all pickups are immediate

-- | Edge case: All dropOffs to current floor
allToCurrentFloorTest :: ElevatorCommand
allToCurrentFloorTest =
    BatchFloorRequest $
        V.fromList
            [ FloorRequest{fromFloor = 5, toFloor = 1} -- All end at current floor (1)
            , FloorRequest{fromFloor = 10, toFloor = 1}
            , FloorRequest{fromFloor = 15, toFloor = 1}
            , FloorRequest{fromFloor = 20, toFloor = 1}
            , FloorRequest{fromFloor = 8, toFloor = 1}
            , FloorRequest{fromFloor = 12, toFloor = 1}
            ] -- Tests optimization when all dropOffs are immediate

-- | Test suites organized by testing category

-- | Performance benchmarking suite - measures efficiency with various scenarios
benchmarkSuite :: [ElevatorCommand]
benchmarkSuite =
    [ emptyBatchRequests
    , singleFloorRequestTest
    , smallBatchTest
    , mediumBatchTest
    , largeBatchTest
    , conflictingDirectionsStressTest
    , maxCapacityBreachTest
    , pathologicalWorstCaseTest
    ]

-- | Real-world scenario suite - simulates actual building usage patterns
realWorldSuite :: [ElevatorCommand]
realWorldSuite =
    [ officeBuilding9AmRushTest
    , apartmentBuildingEveningTest
    , hospitalEmergencyTest
    , shuttleServiceTest
    ]

-- | Edge case testing suite - validates handling of unusual scenarios
edgeCaseSuite :: [ElevatorCommand]
edgeCaseSuite =
    [ allSameFloorRequests
    , allInvalidFloorRequests
    , boundaryFloorRequests
    , allFromCurrentFloorTest
    , allToCurrentFloorTest
    ]

{- | Capacity optimization testing suite - validates occupancy-based prioritization
Note: Use with different currentOccupancy values to test capacity thresholds
-}
capacityOptimizationSuite :: [ElevatorCommand]
capacityOptimizationSuite =
    [ lowCapacityOptimizationTest -- Use with currentOccupancy = 0 (empty elevator)
    , customBatchRequests -- Use with currentOccupancy = 3-5 (normal occupancy)
    , highCapacityStressTest -- Use with currentOccupancy = 8 (80% full)
    , maxCapacityBreachTest -- Use with currentOccupancy = 9 (90% full)
    ]

-- | Default elevator configuration and instances

{- | Initial elevator configuration for testing and demonstration
Creates a 20-floor building with 10-person elevator capacity, starting at ground floor
-}
initialElevator :: StationaryElevator
initialElevator =
    StationaryElevator
        { elevatorConfig =
            ElevatorConfig
                { firstFloor = 1 -- Ground floor
                , lastFloor = 20 -- Top floor (20-story building)
                , maxOccupancy = 10 -- Maximum 10 passengers
                }
        , currentFloor = 1 -- Start at ground floor
        , currentOccupancy = 0 -- Start empty
        }

{- | Main elevator instance ready for command processing
Pre-configured state machine initialized with default elevator settings
-}
elevator :: BaseMachineT IO ElevatorTopology ElevatorCommand ElevatorMotionState
elevator = elevatorStateMachine (StationaryState initialElevator)

{- | Execute a complete elevator operation cycle

This function provides a convenient interface to run the elevator system with a single command.
It handles the full operation cycle: receiving a command, processing it, and executing movement.

OPERATION FLOW:
1. Send the command to the elevator state machine (transitions to MOVING if valid)
2. Send a Move command to execute the planned route
3. Return the final elevator state after completion

USAGE EXAMPLES:
- runElevator batchRequests >>= print  -- Process multiple floor requests
- runElevator singleFloorRequestTest   -- Handle a single passenger request
- runElevator emptyBatchRequests       -- Test empty request handling

INPUT: Any ElevatorCommand (SingleFloorRequest, BatchFloorRequest, or Move)
OUTPUT: Final ElevatorMotionState showing elevator position and status

NOTE: This function automatically sends a Move command after the initial command,
so it's designed for complete end-to-end testing rather than step-by-step control.
-}
runElevator :: ElevatorCommand -> IO ElevatorMotionState
runElevator cmd = fmap fst $ Machine.runBaseMachineT elevator cmd >>= (\(_, s) -> Machine.runBaseMachineT s Move)
