module WasmEdge.Statistic
  ( StatisticsContext
  , configureStatisticsSetInstructionCounting
  , configureStatisticsIsInstructionCounting
  , configureStatisticsSetCostMeasuring
  , configureStatisticsIsCostMeasuring
  , configureStatisticsSetTimeMeasuring
  , configureStatisticsIsTimeMeasuring
  , statisticsCreate
  , statisticsGetInstrCount
  , statisticsGetInstrPerSecond
  , statisticsGetTotalCost
  , statisticsSetCostTable
  , statisticsSetCostLimit
  , statisticsClear
  ) where

import WasmEdge.Internal.FFI.Bindings
