module PrettyPrint where

import Data.Char (toUpper, toLower)

import Common
import Automata
import Monads
import Config

-- // Pretty printer para la consola de comandos

printCommands :: [(Env, [Comm])] -> [Comm] -> String -> String
printCommands envsP cs selName = commToString selName envsP (reverse (clearHistory cs))

clearHistory :: [Comm] -> [Comm]
clearHistory [] = []
clearHistory ((Restart e):cs) = [(Restart e)]
clearHistory (c:cs) = c : clearHistory cs


commToString :: String -> [(Env, [Comm])] -> [Comm] -> String
commToString selName envsP [] = ""
commToString selName envsP ((Restart e):cs) = do
    let comms = snd $ head $ (filter (\(env, comms) -> env == e) envsP)
        selectedName = getFirstCellName comms
    commToString selectedName envsP comms ++ commToString selectedName envsP cs
commToString selName envsP (Step:cs) = resumeSteps selName envsP cs 1
commToString selName envsP (Select (Var n):cs) = commToString n envsP cs
commToString selName envsP (Select (Id n):cs) = commToString selName envsP cs
commToString selName envsP ((DefCell name color bornL surviveL):cs) = 
    "DEFCELL " ++ name ++ " = (" ++ (map toLower color) ++ ", " ++ (show bornL) ++ ", " ++ (show surviveL) ++ ")" ++ "\n" ++ (commToString selName envsP cs)
commToString selName envsP ((UpdatePos pos):cs) = 
    "UPDATE " ++ (show pos) ++ " " ++ (map toUpper selName) ++ "\n" ++ (commToString selName envsP cs)

commToString selName envsP ((UpdateCell pos name):cs) = 
    "UPDATE " ++ (show pos) ++ " " ++ (map toUpper name) ++ "\n" ++ (commToString selName envsP cs)

commToString selName envsP (c:cs) =  (show c) ++ "\n" ++ (commToString selName envsP cs)


resumeSteps :: String -> [(Env, [Comm])] -> [Comm] -> Int -> String
resumeSteps selName envsP (Step:cs) n = resumeSteps selName envsP cs (n+1)
resumeSteps selName envsP (cs) 1 =  "Step" ++ "\n" ++ commToString selName envsP cs
resumeSteps selName envsP (cs) n = "Step (x" ++ (show n) ++ ")" ++ "\n" ++ (commToString selName envsP cs) 

getFirstCellName :: [Comm] -> String
getFirstCellName [] = ""
getFirstCellName ((DefCell name color bornL surviveL):cs) = name
getFirstCellName (c:cs) = getFirstCellName cs