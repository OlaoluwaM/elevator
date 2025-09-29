{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}

{- |
SMART ELEVATOR CONTROL SYSTEM

This module implements an intelligent elevator control system that optimizes passenger
requests using algorithms for capacity management, direction planning, and
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

PERFORMANCE CHARACTERISTICS:
- Uses mutable vectors with ST monad for fun and low overhead instruction processing
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
    mkRunElevator,
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

-- | Basic configuration for our elevator works:
data ElevatorConfig = ElevatorConfig
    { firstFloor :: Floor -- The bottom floor
    , lastFloor :: Floor -- The top floor
    , maxOccupancy :: Int -- Maximum passengers (like 10 people)
    }
    deriving stock (Show, Generic)

newtype Floor = Floor {unFloor :: Int}
    deriving stock (Generic)
    deriving newtype (Show, Eq, Ord, Num)

{- | Stationary Elevator
This represents an elevator that is stopped at a floor and waiting.
It knows:
- Where it is (current floor)
- How many passengers are inside (if any)
- What the elevator's configurations are (config)
-}
data StationaryElevator = StationaryElevator
    { elevatorConfig :: ElevatorConfig
    -- ^ Elevator's configuration
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
    , elevatorConfig :: ElevatorConfig -- The elevator's basic config
    , instructionsQueue :: OptimizedElevatorInstructions -- List of tasks to do
    }
    deriving stock (Show, Generic)

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
    { optimizedInstructionForExecution :: Vector ElevatorInstruction
    -- ^ Instructions for immediate execution
    , unOptimizedDeferredInstructions :: Vector ElevatorInstruction
    -- ^ Instructions deferred for later
    }
    deriving stock (Show, Eq, Generic)

-- E stands for Elevator
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

getInstructionTargetFloor :: ElevatorInstruction -> Floor
getInstructionTargetFloor (Pickup x) = x
getInstructionTargetFloor (DropOff x) = x

{- | Returns the direction of the first valid instruction that requires movement.
    Returns Nothing if all instructions are for the current floor.
-}
determineADirectionForElevator :: Floor -> Vector ElevatorInstruction -> Maybe Direction
determineADirectionForElevator currentFloor = safeHeadV . V.mapMaybe (determineInstructionDirection currentFloor)

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
    compareDistance a b = compare (distanceFromCurrentFloor a) (distanceFromCurrentFloor b)
    -- Calculate how far a task's floor is from our current floor
    distanceFromCurrentFloor :: ElevatorInstruction -> Floor
    distanceFromCurrentFloor inst = abs (getInstructionTargetFloor inst - currentFloor)

-- | Calculate current capacity as a percentage of maximum capacity
calculateCapacityPercentage :: (HasField "currentOccupancy" a Int, HasField "elevatorConfig" a ElevatorConfig) => a -> Double
calculateCapacityPercentage e = calculatePercentage e.currentOccupancy e.elevatorConfig.maxOccupancy

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
                let optimalDirection = fromMaybe Up $ determineADirectionForElevator currentFloor instructions
                 in Just $ optimizeInstructionOrder config currentOccupancy currentFloor optimalDirection instructions

{- | Optimize elevator instruction order using multi-layered capacity-aware prioritization

This function implements an optimization algorithm with three layers:

LAYER 1 - CAPACITY-BASED INSTRUCTION FILTERING:
- Low occupancy (≤20%): Prioritize pickups to efficiently fill the elevator
- High occupancy (≥65%): Prioritize dropoffs to free up space for new passengers
- Normal occupancy (20-65%): Process all instructions indiscriminately

LAYER 2 - DIRECTIONAL PARTITIONING:
- Separate instructions by travel direction relative to current floor
- Prioritize instructions in the target direction, defer opposite direction

LAYER 3 - FINE-GRAINED CAPACITY OPTIMIZATION:
- Apply partitionByCapacityOptimized for detailed space management
- Sort final batch by distance from current floor for efficient movement
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
            targetDirection = fromMaybe currentDirection $ determineADirectionForElevator currentFloor priorityInstructions
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

{- | Capacity-aware instruction partitioning using mutable vectors

This function intelligently partitions elevator instructions based on current occupancy
using a priority system implemented with mutable vectors for efficiency.

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
-}
partitionByCapacityOptimized :: Int -> Int -> Vector ElevatorInstruction -> (Vector ElevatorInstruction, Vector ElevatorInstruction)
partitionByCapacityOptimized maxOccupancy availableSpaceLeft instructions = runST $ do
    priorityTasksMVec <- MV.new (V.length instructions)
    deferredTasksMVec <- MV.new (V.length instructions)

    -- Initialize counters for tracking number of elements written
    canFitCount <- newSTRef @Int 0
    mustDeferCount <- newSTRef @Int 0

    -- Process each instruction according to capacity-based rules
    foldM_
        ( \remainingSpace instr ->
            let currentOccupancy = maxOccupancy - remainingSpace
                occupancyPercentage = calculatePercentage currentOccupancy maxOccupancy
             in case instr of
                    -- LOW OCCUPANCY CASE: Elevator nearly empty - prioritize pickups
                    -- Only process pickups when space is available and occupancy is low
                    Pickup _ | occupancyPercentage <= lowCapacityThreshold -> do
                        currIdx <- readSTRef canFitCount
                        MV.write priorityTasksMVec currIdx instr -- Add pickup to immediate batch
                        modifySTRef canFitCount (+ 1) -- Increment element count
                        pure (remainingSpace - 1) -- Reduce available space

                    -- HIGH OCCUPANCY CASE: Elevator nearly full - prioritize dropoffs
                    -- Always process dropoffs when occupancy is high (they free space)
                    DropOff _ | occupancyPercentage >= highCapacityThreshold -> do
                        currIdx <- readSTRef canFitCount
                        MV.write priorityTasksMVec currIdx instr -- Add dropoff to immediate batch
                        modifySTRef canFitCount (+ 1) -- Increment element count
                        pure (remainingSpace + 1) -- Increase available space

                    -- MEDIUM OCCUPANCY CASE: Balanced load - process both types
                    -- Handle both pickups and dropoffs if space constraints allow
                    instr' | occupancyPercentage > lowCapacityThreshold && occupancyPercentage < highCapacityThreshold -> do
                        currIdx <- readSTRef canFitCount
                        MV.write priorityTasksMVec currIdx instr' -- Add instruction to immediate batch
                        modifySTRef canFitCount (+ 1) -- Increment element count
                        case instr' of
                            Pickup _ -> pure (remainingSpace - 1) -- Pickup reduces available space
                            DropOff _ -> pure (remainingSpace + 1) -- Dropoff increases available space

                    -- DEFERRAL CASE: All other scenarios
                    -- Includes: no space available, wrong priority for current occupancy, edge cases
                    _ -> do
                        currIdx <- readSTRef mustDeferCount
                        MV.write deferredTasksMVec currIdx instr -- Add to deferred batch
                        modifySTRef mustDeferCount (+ 1) -- Increment element count
                        pure remainingSpace -- Space unchanged (deferred)
        )
        availableSpaceLeft
        instructions

    -- Finalize batch construction
    numCanFit <- readSTRef canFitCount
    numMustDefer <- readSTRef mustDeferCount

    -- Convert mutable vectors to immutable vectors with exact sizing
    -- The counts represent the number of elements written to each vector
    priorityTasks <- V.freeze . MV.take numCanFit $ priorityTasksMVec
    deferredTasks <- V.freeze . MV.take numMustDefer $ deferredTasksMVec

    -- DEADLOCK PREVENTION: Ensure progress is always possible
    -- If no instructions can be processed immediately, try deferred instructions
    -- This prevents the elevator from getting stuck in impossible situations
    if V.null priorityTasks
        then pure (deferredTasks, priorityTasks) -- Emergency fallback: swap batches
        else pure (priorityTasks, deferredTasks) -- Normal operation: return as planned

{- | Execute elevator instructions and simulate realistic movement

This function implements the elevator's "physical" movement and task execution.
It processes instructions in batches, simulating realistic elevator behavior
with floor-by-floor movement and passenger capacity updates.

EXECUTION ALGORITHM:
1. Check termination condition - stop if no instructions remain
2. Process current batch of optimized instructions sequentially
3. For each instruction: simulate movement, update passenger count, log progress
4. Re-optimize remaining instructions based on new elevator state
5. Recursively continue until all tasks are completed

MOVEMENT SIMULATION:
- Moves floor-by-floor with realistic delays between floors
- Updates passenger count for pickups and dropoffs
- Provides detailed progress logging for monitoring and debugging
- Handles both same-floor operations and multi-floor travel

BATCH PROCESSING:
- Executes current optimized batch completely before re-planning
- Re-optimizes remaining instructions after each batch completion
- Considers updated elevator state (position, occupancy) for re-optimization
- Continues until no instructions remain
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
- Move: Ignored (cannot move without instructions)

WHEN MOVING:
- Move: Execute planned route, return to STATIONARY when complete
- Request commands: Ignored (complete current route before accepting new requests)

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
                SingleFloorRequest floorRequest ->
                    let instructions = floorRequestToElevatorInstruction stationaryElevator.elevatorConfig floorRequest
                        currentFloor = stationaryElevator.currentFloor
                     in if V.null instructions -- Check if the request is valid
                            then pureResult (StationaryE stationaryElevator) (StationaryState stationaryElevator) -- Invalid request: stay stopped
                            else -- Valid request: plan route and start moving
                                let direction = fromMaybe Up $ determineADirectionForElevator currentFloor instructions -- Determine direction (default Up)
                                    newMovingElevator =
                                        MovingElevator
                                            { currentOccupancy = stationaryElevator.currentOccupancy
                                            , currentFloor = currentFloor
                                            , direction = direction
                                            , elevatorConfig = stationaryElevator.elevatorConfig
                                            , instructionsQueue =
                                                OptimizedElevatorInstructions
                                                    { optimizedInstructionForExecution = instructions
                                                    , unOptimizedDeferredInstructions = V.empty
                                                    }
                                            }
                                 in pureResult (MovingE newMovingElevator) (MovingState newMovingElevator) -- Transition to MOVING
                BatchFloorRequest floorRequests ->
                    case optimizeInitialFloorRequestOrder stationaryElevator.elevatorConfig stationaryElevator.currentOccupancy stationaryElevator.currentFloor floorRequests of
                        Nothing -> pureResult (StationaryE stationaryElevator) (StationaryState stationaryElevator) -- No valid requests: stay stopped
                        Just (direction, optimizedInstructions) ->
                            let newMovingElevator =
                                    MovingElevator
                                        { currentOccupancy = stationaryElevator.currentOccupancy
                                        , currentFloor = stationaryElevator.currentFloor
                                        , direction = direction
                                        , elevatorConfig = stationaryElevator.elevatorConfig
                                        , instructionsQueue = optimizedInstructions
                                        }
                             in pureResult (MovingE newMovingElevator) (MovingState newMovingElevator) -- Transition to MOVING

                -- ANY OTHER COMMAND: Stay stopped (can't move without instructions)
                _ -> pureResult (StationaryE stationaryElevator) (StationaryState stationaryElevator)
            MovingState movingElevator -> \case
                -- MOVE COMMAND: Execute the planned route
                Move -> Machine.ActionResult $ do
                    newStationaryElevator <- executeElevatorInstructions movingElevator -- Do all the planned tasks
                    pure (StationaryE newStationaryElevator, StationaryState newStationaryElevator) -- Return to STOPPED when done
                    -- We could potentially implement commands that allow users to add floor requests on the fly to a moving elevator
                    -- If these "on-demand" requests are placed in the "deferred pile" initially things should work without much effort
                    -- It's just that with the way the state machine works, implementing this might be difficult since while the elevator is moving, state machine actions can be sent to it
                _ -> pureResult (MovingE movingElevator) (MovingState movingElevator)
        }

mkInitialElevator :: ElevatorConfig -> StationaryElevator
mkInitialElevator elevatorConfig =
    StationaryElevator
        { elevatorConfig = elevatorConfig
        , currentFloor = 1 -- Start at ground floor
        , currentOccupancy = 0 -- Start empty
        }

mkElevatorStateMachine :: StationaryElevator -> BaseMachineT IO ElevatorTopology ElevatorCommand ElevatorMotionState
mkElevatorStateMachine elevator = elevatorStateMachine (StationaryState elevator)

mkRunElevator :: ElevatorConfig -> ElevatorCommand -> IO ElevatorMotionState
mkRunElevator elevatorConfig cmd = fmap fst $ Machine.runBaseMachineT (mkElevatorStateMachine (mkInitialElevator elevatorConfig)) cmd >>= (\(_, s) -> Machine.runBaseMachineT s Move)
