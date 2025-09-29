module Scenarios where

import Elevator3

import Data.Vector qualified as V

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
