module Backtest
    ( Data (..)
    , Context (..)
    , Order (..)
    , Bar (..)
    , Frequency (..)
    , movavg
    , csvToHistory
    , backtest
    ) where 
import Data.Default
import Data.DateTime
import Data.HashMap hiding (map)
import Data.Time.Clock (NominalDiffTime)
import Data.List (sortBy, findIndex)
import Data.Ord (comparing)
import Data.Maybe (fromJust)
import Data.List.Split (splitOn)
import Data.Time.Format (parseTimeOrError, defaultTimeLocale)

data Data = Data 
                { cash :: Double
                , history :: Map String [Bar]
                , position :: Map String Integer
                }
                deriving (Show)

data Order = Order { orderSecurity :: String
                   , orderAmount :: Integer
                   }
                deriving (Show)

data Frequency = Trade | BarLength NominalDiffTime
                deriving (Show)

data Bar = Bar 
                { barDatetime :: DateTime
                , barFrequency :: Frequency
                , barOpen :: Double
                , barHigh :: Double
                , barLow :: Double
                , barClose :: Double
                , barVolume :: Integer
                }
                deriving (Show)

data Context = Context { commissionRate :: Double
                       , commissionMin :: Double
                       }
                deriving (Show)

instance Default Context 
    where
        def = Context { commissionRate = 0.0075
                      , commissionMin = 1.00
                      }

backtest :: Double -> Map String [Bar] -> DateTime -> (Data -> [Order] -> [Order]) -> Data
backtest c hist start strategy = finalData
                        where
                            mStartIndex = findIndex (\b -> (barDatetime b) <= start) (head (elems hist))
                            startIndex = fromJust mStartIndex
                            startData = Data c (getNHistory startIndex hist) empty
                            startOrders = []
                            (finalData, finalOrders) = backtest' strategy hist (startData, startOrders)

backtest' :: (Data -> [Order] -> [Order]) -> Map String [Bar] -> (Data, [Order]) -> (Data, [Order])
backtest' strategy hist (d, orders) = if (lengthHistory hist == lengthHistory (history d))
                                            then (d, orders)
                                            else backtest' 
                                                strategy 
                                                hist 
                                                (nextBar hist d (strategy d orders))

lengthHistory :: Map String [Bar] -> Int
lengthHistory = length . head . elems

csvToHistory :: Frequency -> String -> [Bar]
csvToHistory freq s = map lineToBar valuesRaw
    where 
        separated = (fmap (splitOn ",")) . lines $ s
        titles = head separated
        valuesRaw = tail separated
        lineToBar l = Bar
                        (parseTimeOrError True defaultTimeLocale "%Y-%m-%d" (l !! 0))
                        freq
                        (read (l !! 1))
                        (read (l !! 2))
                        (read (l !! 3))
                        (read (l !! 4))
                        (read (l !! 5))

getNHistory :: Int -> Map String [Bar] -> Map String [Bar]
getNHistory n hist = fmap (take n) hist

nextBar :: Map String [Bar] -> Data -> [Order] -> (Data, [Order])
nextBar hist d openOrders = (Data newCash newHistory newPosition, newOpenOrders) 
    where
        newHistory = getNHistory ((fromIntegral . lengthHistory . history $ d) + 1) hist
        now = fmap last newHistory
        c = cash d
        p = position d
        (newCash, newPosition, newOpenOrders) = foldl (executeOrder now) (c, p, []) (sortOrders now openOrders)

executeOrder :: Map String Bar -> (Double, Map String Integer, [Order]) -> Order -> (Double, Map String Integer, [Order])
executeOrder barMap (c, pos, orders) order = if (newCash < 0)
                                                then (c, pos, orders ++ [order])
                                                else ( newCash
                                                     , (if (member (orderSecurity order) pos) 
                                                        then adjust 
                                                            (+ (orderAmount order))
                                                            (orderSecurity order)
                                                            pos
                                                        else
                                                            insert
                                                            (orderSecurity order)
                                                            (orderAmount order)
                                                            pos)
                                                     , orders)
            where
                newCash = orderDifference barMap order + c

sortOrders :: Map String Bar -> [Order] -> [Order]
sortOrders barMap = map snd . reverse . sortBy (comparing fst) . map (\o -> (orderDifference barMap o, o))

orderDifference :: Map String Bar -> Order -> Double
orderDifference barMap order = cashDifference
    where
        price = barOpen (barMap ! (orderSecurity order))
        cashDifference = -(price * fromIntegral (orderAmount order))

movavg :: Int -> [Double] -> [Double]
movavg n []     = []
movavg n (x:xs) = map (/ n') sums
    where
        sums = scanl (+) (n' * x) $ zipWith (-) xs (replicate n x ++ xs)
        n'   = fromIntegral n
