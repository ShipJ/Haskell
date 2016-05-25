-- Jack Shipway | Haskell Coursework | 18/01/13 --
-- I integrated code from the website: "http://bonsaicode.wordpress.com/2010/01/08/programming-praxis-nim/" Because I found difficulty in implementing the exclusive or (xor) function in order to play against the computer. The general background Of the game NIM comes from the wikipedia page of NIM --

module TYPE_nim_TO_BEGIN where
import System.IO
import Data.Maybe
import Data.Bits
import Text.Printf
import System.Random

player = play [1,2,3,4,5] ["Player 1","Player 2"]
vscompHard = computer [1,2,3,4,5]
vscompEasy = computerEasy [1,2,3,4,5]
nim :: IO ()
nim = do
    putStrLn "\n /L     /|    /L    /|L      /||"
    putStrLn " || L   ||    ||    || L    / ||"
    putStrLn " ||  L  ||    ||    ||  L  /  ||"
    putStrLn " ||    L||   _||_   /|   L/   /|"
    putStrLn "  _____________________________"
    putStrLn " |   0  : Player Vs. Computer. |"  
    putStrLn " |   1  : Player Vs. Player.   |"
    putStrLn " |   2  : How To Play NIM!     |"
    putStrLn "  -----------------------------"
    putStr "-- Select An Option From Above, Are You Willing To Challenge Me? --\n"
    c <- readLn
    if c == 0
        then putStrLn "-- Oh Dear, You Will Live To Regret that! --\n" >> difficulty
    else if c == 1
        then putStrLn "-- Very Interesting, Who's Gonna Win? --\n" >> player
    else putStr "\n                                     -- HOW TO PLAY --\n" >> howToPlay

howToPlay :: IO ()
howToPlay = do 
           putStrLn "     Nim is a mathematical game of strategy in which two players take turns removing objects from distinct heaps. On each turn, a player must remove at least one object, and may remove any number of objects provided they all come from the same heap. In general the player who removes the last object from the board wins, however a variation on the game, known as Misere Nim, is that the player who removes the last object loses, which is the case when playing against another player in this game.\n"
           putStrLn "1: Type the number of the row from which you wish to remove an object."
           putStrLn "2: The Computer will then prompt you to type the number of objects you wish to remove from this pile, You may select as few or as many as you wish provided that it is physically possible."
           putStrLn "3: Follow all other on screen instructions and most importantly enjoy playing NIM, try not to take everything i say in the game seriously, I'm just a very competitive PERSON."
           putStrLn "4: There Is an Easy And A Hard Mode, I Suggest You Try Easy Mode To Begin With!\n"
           putStrLn "1 : Get Me Started!"
           putStrLn "0 : Back To Main Menu."
           putStr "-- Select an Option From Above. --\n"
           c <- readLn
           if c == 1 
               then putStrLn "-- Have Fun Playing NIM! --\n" >> vscompEasy
           else putStr "-- GoodBye For Now. --\n" >> nim
difficulty :: IO () 
difficulty = do

           putStrLn "1 : Easy - Hmm, I'll Go Easy On You."
           putStrLn "0 : Veteran - You Will Not Beat Me."
           putStr "-- Select An Option From Above, If You Dare. --\n"
           c <- readLn
           if c == 1
               then putStrLn "-- Let's See How Clever You Are, Or Not... --\n" >> vscompEasy
               else putStrLn "-- You Can Still Turn Back If You Want To... --\n" >> vscompHard
play :: [Int] -> [String] -> IO () 
play board players = do
    (newboard,loser) <- haveGoes board players
    if null loser
      then play newboard players
      else do putStrLn ("-- Haha " ++ loser ++ " Loses. I Could Tell You didn't Quite Have The Calibre.--\n") >> rematch
              return ()
rematch :: IO () 
rematch = do
          putStrLn "1 : Rematch!"
          putStrLn "0 : Back To Main Menu."
          putStr "-- What Would You Like To Do? --\n"
          c <- readLn
          if c == 0
              then putStrLn "-- I See How It Is, Better Luck Next Time! --\n" >> nim
              else putStrLn "-- This Should Be Interesting. --\n" >> player
haveGoes :: [Int] -> [String] -> IO ([Int],String)
haveGoes board [] = return (board,[])
haveGoes board (player:players) = do
    newboard <- haveGo board player
    if sum newboard == 0
      then return (newboard,player)
      else haveGoes newboard players
haveGo :: [Int] -> String -> IO [Int]
haveGo board player = do
    putStrLn ("-- Choose Wisely, Remember, Last Stone Loses! - " ++ player ++ "'s Go --")
    showboard board
    row   <- choosePile board
    count <- onlyNum "-- How many stones do you want to remove from that pile? --\n" 1 (board!!row)
    return $ take row board ++ [board!!row - count] ++ drop (row+1) board
showboard :: [Int] -> IO ()
showboard [] = return ()
showboard board = do
    putStrLn $ show (length board) ++ " | " ++ replicate (last board) 'O'
    showboard (init board)
choosePile :: [Int] -> IO Int
choosePile board = do 
    pile <- onlyNum "-- Which row do you want to take stones from? --\n" 1 (length board)
    if board!!(pile-1) == 0
      then do putStrLn "-- That Row Is Empty, Are you Blind!? --\n"
              choosePile board
      else return (pile-1)
onlyNum info min max = do  -- making sure that all entries are valid, IE integers that are within the correct range
    putStr info
    input <- getLine
    let parsed = reads input :: [(Int,String)]
    if null parsed
      then wrongNum "-- You Call That A Number!? --\n"
      else testNumber (fst (head parsed))
    where 
         wrongNum error = do putStrLn error 
                             onlyNum info min max
         testNumber number
                      | number < min = wrongNum "-- Can You Read!? You Need A Bigger Number Than 0! Try Again. --\n"
                      | number > max = wrongNum "-- Come On, That Number Is Clearly Too Big! Try Again --\n"
                      | otherwise = do return number

valid :: (Int, Int) -> [Int] -> Bool
valid (p, t) ps = and [p >= 0, p < length ps, t > 0, t <= ps !! p]
showMove :: (Int, Int) -> IO ()
showMove (p, t) = printf "-- I remove %d stone%s from pile %d --\n" t
                      (if t > 1 then "s" else "") (p + 1)
showMoveEasy :: (Int, Int) -> IO ()
showMoveEasy (p, v) = printf "-- I remove %d stone%s from pile %d --\n" v
                      (if v > 1 then "s" else "") (p + 1)
cpu :: [Int] -> IO (Int, Int)
cpu ps = do p <- randomRIO (0, length ps-1)
            t <- randomRIO (1, ps !! p)
            let n = foldl xor 0 ps
            let r = if n == 0 then (p, t) else (length a, b - xor b n)
                        where (a,b:_) = break (\x -> xor x n < x) ps
            if valid r ps then showMove r >> return r else cpu ps
cpuEasy :: [Int] -> IO (Int, Int)
cpuEasy ps = do p <- randomRIO (0, length ps)
                v <- randomRIO (1, ps !! p)
                let n = foldl xor 1 ps
                let r = if n == 4 then (p, v) else (length g, h - xor h n)
                        where (g,h:_) = break (\x -> xor x n < x) ps
                if valid r ps then showMoveEasy r >> return r else cpuEasy ps
prompt :: Read x => String -> IO x
prompt y = putStr (y ++ " ") >> fmap read getLine
human :: [Int] -> IO (Int, Int)
human ps = do p <- fmap pred $ prompt "-- Which Pile Would You Like To Remove Stones From? --"
              t <- prompt "-- How Many Stones Would You Like To Remove? --"
              if valid (p, t) ps then return (p, t) else human ps
display :: [Int] -> String
display = unlines . zipWith (printf "%d: %d") [1 :: Int ..]
makeMove :: (Int, Int) -> [Int] -> [Int]
makeMove (p, t) = (\(a,b:c) -> a ++ b - t:c) . splitAt p
turn :: [([Int] -> IO (Int, Int), [Char])] -> [Int] -> IO ()
turn ~((f, w):ms) b = if all (== 0) b then putStrLn $ w ++ " win" 
                      else do putStr $ display b
                              turn ms . flip makeMove b =<< f b 
computer :: [Int] -> IO ()
computer ps = do f <- prompt "-- Enter 1 to move first or 2 to move second: --"
                 turn (drop f $ cycle [(cpu, "You"), (human, "I")]) ps
computerEasy :: [Int] -> IO ()
computerEasy ps = do f <- prompt "-- Enter 1 to move first or 2 to move second: --"
                     turn (drop f $ cycle [(cpuEasy, "You"), (human, "I")]) ps


               


               
