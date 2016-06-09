import System.Environment
import Control.Monad.Trans.State.Lazy
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number
import Text.ParserCombinators.Parsec.Error
import System.IO
import Data.Maybe
import Control.Monad.IO.Class

--- TODO przetestowac
-- uproszczony "parser" PDN
-- ruch Int-Int
-- bicie [Intx]Int

data PDN =   Move (Int,Int) -- pozycja startowa i koncowa
           | Kill [Int]  -- pozycja startowa to glowa, pozniej kolejne pozycje
           deriving (Show,Eq)

-- instance Show PDN where
--   show (Move (a,c)) = (show a)++ "-"++ (show c)
--   show (Kill [p]) = (show p)
--   show (Kill (p:ps)) = (show p)++ "x" ++ show (Kill ps)


parsePos :: Parser Int
parsePos = do
            x <- int
            if (x<1 || x>32) then
              unexpected "Tylko liczby od 1-32"
            else
              return x

parseMove = do
            x1 <- parsePos
            (char '-')
            x2 <- parsePos
            eof
            return $ Move (x1,x2)

parseKill = do
            x1 <- sepBy (parsePos) (char 'x')
            eof
            if (length x1) > 1 then
              return $ Kill x1
            else
              unexpected "start i koniec minimum"

parsePDN =  try parseMove <|> parseKill

type Game a = IO a

play :: String -> Game ()
play i = do
  case parse parsePDN "sPDN err" i of
    Right move -> (hPutStrLn stderr $ "ruch = " ++ (show move))
    Left x -> fail $ show x
  putStrLn "11-15" >> hFlush stdout -- konkretny ruch trzeba wygenerowac


doPlay :: Game ()
doPlay = getContents >>= (mapM_ play) . lines


main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  mapM_ putStrLn args
  putStrLn progName
  let args = ["w"] -- do zakomentowania w programmie
  case (listToMaybe args) of
    Just "b" -> doPlay
    Just "w" -> putStrLn "11-15" >> hFlush stdout>> doPlay -- biaĹe wykonujÄ pierwszy ruch
    Nothing -> doPlay

