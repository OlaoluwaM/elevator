module Main (main) where

import Elevator3
import Scenarios
import System.IO
import Text.Read (readMaybe)

-- | Display the main menu with all available test scenarios
displayMenu :: IO ()
displayMenu = do
    putStrLn "\n🚡 SMART ELEVATOR SIMULATION 🚡"
    putStrLn "================================"
    putStrLn ""
    putStrLn "Select a test scenario to run:"
    putStrLn ""
    putStrLn "📊 BENCHMARK TESTS:"
    putStrLn "  1.  Empty Batch Requests"
    putStrLn "  2.  Single Floor Request Test"
    putStrLn "  3.  Small Batch Test"
    putStrLn "  4.  Medium Batch Test"
    putStrLn "  5.  Large Batch Test"
    putStrLn "  6.  Conflicting Directions Stress Test"
    putStrLn "  7.  Max Capacity Breach Test"
    putStrLn "  8.  Pathological Worst Case Test"
    putStrLn ""
    putStrLn "🏢 REAL-WORLD SCENARIOS:"
    putStrLn "  9.  Office Building 9AM Rush"
    putStrLn "  10. Apartment Building Evening"
    putStrLn "  11. Hospital Emergency"
    putStrLn "  12. Shuttle Service"
    putStrLn ""
    putStrLn "⚠️  EDGE CASES:"
    putStrLn "  13. All Same-Floor Requests"
    putStrLn "  14. All Invalid Floor Requests"
    putStrLn "  15. Boundary Floor Requests"
    putStrLn "  16. All From Current Floor Test"
    putStrLn "  17. All To Current Floor Test"
    putStrLn ""
    putStrLn "🔧 CAPACITY OPTIMIZATION:"
    putStrLn "  18. Low Capacity Optimization Test"
    putStrLn "  19. Custom Batch Requests"
    putStrLn "  20. High Capacity Stress Test"
    putStrLn "  21. Max Capacity Breach Test (Capacity Focus)"
    putStrLn ""
    putStrLn "📋 ADDITIONAL TESTS:"
    putStrLn "  22. Normal Office Building Scenario"
    putStrLn "  23. Edge Case Requests"
    putStrLn "  24. Capacity Limit Requests"
    putStrLn "  25. Over Capacity Requests"
    putStrLn "  26. High Capacity DropOff Requests"
    putStrLn "  27. Mixed Direction Capacity Test"
    putStrLn "  28. Distance Optimization Test"
    putStrLn "  29. SCAN Algorithm Test"
    putStrLn ""
    putStrLn "  0.  Exit"
    putStrLn ""
    putStr "Enter your choice (0-29): "

-- | Get the test scenario based on user input
getScenario :: Int -> Maybe ElevatorCommand
getScenario choice = case choice of
    -- Benchmark tests
    1 -> Just emptyBatchRequests
    2 -> Just singleFloorRequestTest
    3 -> Just smallBatchTest
    4 -> Just mediumBatchTest
    5 -> Just largeBatchTest
    6 -> Just conflictingDirectionsStressTest
    7 -> Just maxCapacityBreachTest
    8 -> Just pathologicalWorstCaseTest
    -- Real-world scenarios
    9 -> Just officeBuilding9AmRushTest
    10 -> Just apartmentBuildingEveningTest
    11 -> Just hospitalEmergencyTest
    12 -> Just shuttleServiceTest
    -- Edge cases
    13 -> Just allSameFloorRequests
    14 -> Just allInvalidFloorRequests
    15 -> Just boundaryFloorRequests
    16 -> Just allFromCurrentFloorTest
    17 -> Just allToCurrentFloorTest
    -- Capacity optimization
    18 -> Just lowCapacityOptimizationTest
    19 -> Just customBatchRequests
    20 -> Just highCapacityStressTest
    21 -> Just maxCapacityBreachTest
    -- Additional tests
    22 -> Just batchRequests
    23 -> Just edgeCaseRequests
    24 -> Just capacityLimitRequests
    25 -> Just overCapacityRequests
    26 -> Just highCapacityDropOffRequests
    27 -> Just mixedDirectionCapacityTest
    28 -> Just distanceOptimizationTest
    29 -> Just scanAlgorithmTest
    _ -> Nothing

-- | Get the scenario name for display purposes
getScenarioName :: Int -> String
getScenarioName choice = case choice of
    1 -> "Empty Batch Requests"
    2 -> "Single Floor Request Test"
    3 -> "Small Batch Test"
    4 -> "Medium Batch Test"
    5 -> "Large Batch Test"
    6 -> "Conflicting Directions Stress Test"
    7 -> "Max Capacity Breach Test"
    8 -> "Pathological Worst Case Test"
    9 -> "Office Building 9AM Rush"
    10 -> "Apartment Building Evening"
    11 -> "Hospital Emergency"
    12 -> "Shuttle Service"
    13 -> "All Same-Floor Requests"
    14 -> "All Invalid Floor Requests"
    15 -> "Boundary Floor Requests"
    16 -> "All From Current Floor Test"
    17 -> "All To Current Floor Test"
    18 -> "Low Capacity Optimization Test"
    19 -> "Custom Batch Requests"
    20 -> "High Capacity Stress Test"
    21 -> "Max Capacity Breach Test (Capacity Focus)"
    22 -> "Normal Office Building Scenario"
    23 -> "Edge Case Requests"
    24 -> "Capacity Limit Requests"
    25 -> "Over Capacity Requests"
    26 -> "High Capacity DropOff Requests"
    27 -> "Mixed Direction Capacity Test"
    28 -> "Distance Optimization Test"
    29 -> "SCAN Algorithm Test"
    _ -> "Unknown"

-- | Run a selected test scenario
runScenario :: Int -> IO ()
runScenario choice =
    let elevatorConfig =
            ElevatorConfig
                { firstFloor = 1 -- Ground floor
                , lastFloor = 20 -- Top floor (20-story building)
                , maxOccupancy = 10 -- Maximum 10 passengers
                }
        runElevator = mkRunElevator elevatorConfig
     in case getScenario choice of
            Nothing -> putStrLn "❌ Invalid choice. Please try again."
            Just scenario -> do
                let name = getScenarioName choice
                putStrLn $ "\n🚀 Running: " ++ name
                putStrLn "═══════════════════════════════════════"
                putStrLn ""

                putStrLn "📋 Scenario Details:"
                print scenario
                putStrLn ""

                putStrLn "🎬 Elevator Simulation Starting..."
                putStrLn "──────────────────────────────────────"

                result <- runElevator scenario

                putStrLn "──────────────────────────────────────"
                putStrLn "✅ Simulation Complete!"
                putStrLn ""
                putStrLn "📊 Final Elevator State:"
                print result
                putStrLn ""

-- | Main program loop
mainLoop :: IO ()
mainLoop = do
    hSetBuffering stdout NoBuffering
    displayMenu
    maybeInput <- safeReadLine
    case maybeInput of
        Nothing -> do
            -- No more input available (EOF), exit gracefully
            putStrLn "\n👋 Thank you for using the Smart Elevator Simulation!"
            putStrLn "Goodbye! 🚡"
        Just input -> case readMaybe input of
            Just 0 -> do
                putStrLn "\n👋 Thank you for using the Smart Elevator Simulation!"
                putStrLn "Goodbye! 🚡"
            Just choice -> do
                runScenario choice
                putStrLn "\nPress Enter to continue..."
                safeGetLine
                mainLoop
            Nothing -> do
                putStrLn "❌ Invalid input. Please enter a number between 0 and 29."
                putStrLn "Goodbye! 🚡"

-- | Entry point
main :: IO ()
main = do
    putStrLn "Welcome to the Smart Elevator Control System!"
    putStrLn "This simulation demonstrates intelligent elevator optimization."
    mainLoop

-- | Safe function to read a line that handles EOF
safeReadLine :: IO (Maybe String)
safeReadLine = do
    eof <- isEOF
    if eof
        then return Nothing
        else Just <$> getLine

-- | Safe function to wait for user input that handles both interactive and piped input
safeGetLine :: IO ()
safeGetLine = do
    eof <- isEOF
    if eof
        then pure () -- If no more input available, just continue
        else do
            _ <- getLine
            return ()
