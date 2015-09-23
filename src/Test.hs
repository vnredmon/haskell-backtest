import Backtest
import Data.Default
import Data.HashMap hiding (map)
import qualified Data.HashMap as HM
import Data.Time.Format (parseTimeOrError, defaultTimeLocale)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

initialize :: Context
initialize = def

security :: String
security = "AAPL"

strategy :: Data -> [Order] -> [Order]
strategy d openOrders = orders
        where 
            prices = fmap barClose $ (history d) ! security
            mavgShort = movavg 2 prices
            mavgLong = movavg 7 prices
            currentPosition = fromMaybe 0 $ HM.lookup security (position d) 
            targetPosition = if (last mavgShort > last mavgLong) then 1 else 0
            orders = if (currentPosition /= targetPosition)
                        then [Order { orderSecurity = security
                                    , orderAmount = (targetPosition - currentPosition)
                                    }]
                        else []




main :: IO ()
main = do
    inputFile <- readFile "table.csv"
    let aaplHistory = csvToHistory (BarLength (fromInteger 60*60*24)) inputFile
    let hist = insert "AAPL" aaplHistory empty
    let startDate = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" "2014-07-01"
    let result = backtest 100000 hist startDate strategy
    putStrLn $ "cash: " ++ (show $ cash result) ++ " shares: " ++ (show $ position result)
