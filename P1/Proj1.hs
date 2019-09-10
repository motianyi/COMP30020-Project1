    
--  File Name   : Proj1.hs
--  Author      : Tianyi MO
--  Student ID  : 875556
--  Email       : tianyim@student.unimelb.edu.au  
--  Date        : September 10, 2019
--  Purpose     : This file is the first project of COMP30020 Declarative 
--              Programming in University of Melbourne Semester 2, 2019 . 
--  
-- This file implements feedback, firstStateGuess and nextguess 
-- functions and defines GameState Data Type. It also contains several helper 
-- functions. The first palyer choose either 2,3 or 4 cards and the second 
-- guess using initialGuess. The first player then call feedback, and second
-- player guess based on feedback using nextGuess function.


module Proj1 (feedback, initialGuess, nextGuess, GameState) where

import Data.List
import Card

-- This function take target cards and guess cards and return five feedback 
-- numbers, the first number is number of correct guesses, the second number is 
-- the number of cards in target has rank lower than the lowest rank in guess.
-- the third number is the number of cards in target has same rank in guess.
-- the fourth number is the number of cards in target has rank higher than the 
-- highest rank in guess. The fifth number is the number of cards in target has
-- same suit in guess.
feedback :: [Card] -> [Card] -> (Int, Int, Int, Int, Int)
feedback target guess = (a,b,c,d,e) 
    where 
        a = correctCard target guess
        b = lowerRank target guess
        c = correctRank target guess
        d = higherRank target guess
        e = correctSuit target guess


-- This correctCard function take target cards and guess cards and return the 
-- number of correct guesses
correctCard :: [Card] -> [Card] -> Int
correctCard target guess = (length target) - (length (target \\ guess))



-- This lowerRank function take target cards and guess cards and return the 
-- number of cards in target has rank lower than the lowest rank in guess.
lowerRank :: [Card] -> [Card] -> Int
lowerRank target guess = 
    length[ k | k <- getRankList target, k < (minimum (getRankList guess))] 
    


-- This correctRank function take target cards and guess cards and return the 
-- number of cards in target has same rank in guess.
correctRank :: [Card] -> [Card] -> Int
correctRank target guess = (length target) - (length (targetRank \\ guessRank)) 
    where 
        targetRank = getRankList target 
        guessRank = getRankList guess



-- This higherRank function take target cards and guess cards and return the 
-- number of cards in target has rank higher than the highest rank in guess.
higherRank :: [Card] -> [Card] -> Int
higherRank target guess = 
    length[ k | k <-getRankList target, k > (maximum (getRankList guess))] 



-- This correctSuit function take target cards and guess cards and return the 
-- number of cards in target has same suit in guess.  
correctSuit :: [Card] -> [Card] -> Int
correctSuit target guess = (length target) - (length (targetSuit \\ guessSuit)) 
    where 
        targetSuit = getSuitList target 
        guessSuit = getSuitList guess


-- helper function to get rank of Card        
getRank :: Card -> Rank 
getRank (Card suit rank) = rank

-- helper function to get rank of List of Cards
getRankList :: [Card] -> [Rank]
getRankList [] = []
getRankList (x:xs) = [getRank x] ++ getRankList xs

-- helper function to get suit of Card 
getSuit :: Card -> Suit
getSuit (Card suit rank) = suit

-- helper function to get suit of List of Cards
getSuitList :: [Card] -> [Suit]
getSuitList [] = []
getSuitList (x:xs) = [getSuit x] ++ getSuitList xs 



-- GameState Type consistent of a list of list of Cards and an Integer
-- The list of list of Cards represents all remaining possible gusses
-- The Int represents the number of round of game
data GameState = State [[Card]] Int deriving Show


-- The initialGuess function take the number of cards in the guess as input 
-- and return a pair of initial guess and gamestate. The number of cards in 
-- range from 2 to 4.
initialGuess :: Int -> ([Card], GameState)
initialGuess length
-- the initial guesses are pre-defined and tuned to optimal
    |length == 2 = ([(Card Club R5), (Card Heart R9)], gamestate)
    |length == 3 = 
        ([(Card Club R5), (Card Heart R8), (Card Spade Queen)], gamestate)
    |length == 4 = ([(Card Club R4), 
        (Card Heart R7), (Card Spade R9), (Card Diamond Queen)], gamestate)
    where 
        gamestate = State (allCombinations length (enumFromTo first last)) 1
        first = (Card Club R2)      --first card
        last = (Card Spade Ace)     --last card


--allCombination function take an Int and a list, it return list of all 
--possible combination of items in list in length without repetition
--For example allCombinations 2 [1, 2, 3] = [[1, 2], [1, 3], [2, 3]]
allCombinations :: Int -> [a] -> [[a]]
allCombinations 0 _ = [[]]
allCombinations _ [] = []
allCombinations n (x : xs) = 
    map (x :) (allCombinations (n - 1) xs) ++ allCombinations n xs


--nextguess take previous guess, game state and feedback as 5 ints, then return 
--the new guess and new gamestate
nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess (prevguess, State cards round) (f1, f2, f3, f4, f5) 
-- handle guesses for 2 cards case
    |length prevguess ==2 = (newGuess, newState)
-- handle third and after guesses of 3 and 4 cards 
    |round > 1 = (newGuess, newState) 
-- handle second guess of 3 and 4 cards,it use quickGuess to improve efficiency
    |otherwise = (quickGuess newState, newState) 
    where 
        newState = State newCandidateState (round + 1)
        newGuess = bestGuess newState 
        newCandidateState = 
            (delete prevguess (consistent (prevguess, cards) feedback))
        feedback = (f1, f2, f3, f4, f5)

--quickGuess function take a gamestate and return list of card as guess,
--Instead of find best state in all possible states, it find best guess among
--first 1000 possible guesses to improve effiency in early guesses for 4 case.
quickGuess :: GameState -> [Card]
quickGuess (State candidateGuesses round) = 
    bestGuess (State (take 1000 candidateGuesses) round)

--consistent function return the list of consistent answers by removing 
--inconsistent answers.
consistent :: ([Card],[[Card]]) -> (Int,Int,Int,Int,Int) -> [[Card]]
consistent (prevguess, cards) (f1, f2, f3, f4, f5) = 
    [a| a<-cards, (feedback a prevguess) == (f1, f2, f3, f4, f5)]


--resultCounts take list of cards and cards and return Int List representing
--counting the number of each feedback
resultCounts :: [[Card]] -> [Card] -> [Int]
resultCounts state guess = 
    map length (group (sort [feedback a guess | a <- state]))


--calculateScore function implements the algorithms describe in the project
--specification, it take list of Int and retutn a Double score
calculateScore :: [Int] -> Double
calculateScore score = 
    (sum (map (^3) (map fromIntegral score))) / (sum (map fromIntegral score))


--bestGuess function take gamestate and return a Card List, it select best 
--guess from consistent possible answers
bestGuess :: GameState -> [Card]
bestGuess (State candidateGuesses round) = guess
    where 
        (score , guess) = (minimum scorelist)
        scorelist = 
            [(calculateScore (resultCounts candidateGuesses guess), guess)
            |guess <- candidateGuesses]


