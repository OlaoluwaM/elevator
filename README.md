# Elevator Control System

> [!NOTE]
> You probably want to look at [Elevator3.hs](./src/Elevator3.hs). Also, this README was AI generated so if it sounds loftier than it is, that's why

An intelligent elevator control system written in Haskell, featuring state machine architecture, optimized instruction batching, and real-time simulation.

## Features

- **State Machine Architecture**: Type-safe elevator states (Stationary ↔ Moving)
- **Intelligent Instruction Batching**: Three-tier optimization strategy for efficient floor request processing
- **Capacity-Aware Prioritization**: Dynamic instruction prioritization based on current elevator capacity
- **Real-time Simulation**: Floor-by-floor movement with realistic timing and visual feedback
- **Vector-Based Performance**: Efficient data structures for instruction processing

## Quick Start

### Building the Project

```bash
stack build
```

### Running the Simulation

```bash
cd <project-dir>/src
stack repl -- Elevator3.hs
# Inside the GHCi repl run or similar
> runElevator <one-of-the-example-elevator-commands-in-the-file>
```

## Architecture

The system uses a state machine with two main states:

- **Stationary**: Receives floor requests and transitions to Moving state
- **Moving**: Executes optimized instruction batches and returns to Stationary

### Core Data Types

```haskell
-- Elevator configuration
data ElevatorConfig = ElevatorConfig
    { firstFloor :: Floor     -- Lowest accessible floor
    , lastFloor :: Floor      -- Highest accessible floor
    , maxCapacity :: Int      -- Maximum number of passengers
    }

-- Stationary elevator ready to receive requests
data StationaryElevator = StationaryElevator
    { elevatorConfig :: ElevatorConfig
    , currentFloor :: Floor
    , currentCapacity :: Int
    }

-- Moving elevator with instruction queue
data MovingElevator = MovingElevator
    { currentCapacity :: Int
    , currentFloor :: Floor
    , direction :: Direction
    , elevatorConfig :: ElevatorConfig
    , instructionsQueue :: OptimizedElevatorInstructions
    }
```

### Optimization Algorithm

The system uses a three-tier batching strategy:

1. **Primary Batch**: Core instructions (up to 70% of capacity)
2. **Remaining**: Deferred instructions for later processing

Instructions are prioritized based on current capacity:

- **High capacity (≥60%)**: Prioritize dropoffs to free up space
- **Empty capacity (≤0%)**: Prioritize pickups to utilize capacity
- **Normal capacity**: Process all instructions without type-based prioritization

## Examples

### Single Floor Request

```haskell
-- Create initial elevator
let initialElevator = StationaryElevator
      { elevatorConfig = ElevatorConfig 1 20 5  -- floors 1-20, capacity 5
      , currentFloor = 1
      , currentCapacity = 0
      }

-- Send elevator from floor 1 to floor 10
let request = SingleFloorRequest (FloorRequest 1 10)
```

### Batch Floor Requests

```haskell
-- Multiple requests processed with intelligent batching
let batchRequests = BatchFloorRequest $ V.fromList
      [ FloorRequest 3 15    -- Pickup at 3, dropoff at 15
      , FloorRequest 5 18    -- Pickup at 5, dropoff at 18
      , FloorRequest 2 12    -- Pickup at 2, dropoff at 12
      , FloorRequest 7 20    -- Pickup at 7, dropoff at 20
      ]

-- The system will automatically:
-- 1. Sort by distance from current floor
-- 2. Group by direction (up/down)
-- 3. Apply capacity-based prioritization
-- 4. Execute in optimized batches
```

## TODO (Maybe)

- [ ] Add `main` program for `stack run`
- [ ] Add tests
