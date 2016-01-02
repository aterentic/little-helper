import System.Environment
import Network.HTTP
import Text.Regex.Posix

main :: IO ()
main = do result <- getNbsRates
          print $ head $ head $ parseRates $ rateLines result 

sel4 :: (a, b, c, d) -> d
sel4 (_, _, _, x) = x

parseLine :: String -> (String, String, String, [String])
parseLine line =  line =~ ".*>([0-9,\\.]+)</td></tr>$"

parseRates :: [String] -> [[String]] 
parseRates xs = map (sel4 . parseLine) xs  

rateLines :: String -> [String]
rateLines page = filter (=~ "EUR") $ lines page
          
getNbsRates :: IO String
getNbsRates = let request = getRequest "http://www.nbs.rs/kursnaListaModul/srednjiKurs.faces"
              in simpleHTTP request >>= getResponseBody
