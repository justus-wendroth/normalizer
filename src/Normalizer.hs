module Normalizer where

import Data.List (nub, subsequences, (\\), sort)

data Dependency = FD String String | MVD String String
type Relation = ([Char], [Dependency])

instance Show Dependency where
  show (FD xs ys) = show xs ++ " -> " ++ show ys
  show (MVD xs ys) = show xs ++ " ->> " ++ show ys

instance Eq Dependency where
  FD _ _ == MVD _ _ = False
  FD as bs == FD cs ds = equalAsSets as cs && equalAsSets bs ds
  MVD as bs == FD cs ds = equalAsSets as cs && equalAsSets bs ds

equalAsSets :: String -> String -> Bool
equalAsSets xs ys = nub (sort xs) == nub (sort ys)

subsetOf :: String -> String -> Bool
xs `subsetOf` ys = all (`elem` ys) xs

empty :: Relation
empty = ([], [])

insertDependency :: Dependency -> Relation -> Relation
insertDependency d@(FD xs ys) r@(attrs, fds)
  | d `elem` fds = r
  | otherwise = (nub (xs ++ ys ++ attrs), FD (nub xs) (nub ys) : fds)
insertDependency d@(MVD xs ys) r@(attrs, fds)
  | d `elem` fds = r
  | otherwise = (nub (xs ++ ys ++ attrs), MVD (nub xs) (nub ys) : fds)

deleteDependency :: Dependency -> Relation -> Relation
deleteDependency d (attrs, fds) = (attrs, filter (/= d) fds)

insertAttribute :: Char -> Relation -> Relation
insertAttribute x r@(attrs, fds)
    | x `elem` attrs = r
    | otherwise = (nub $ x:attrs, fds)

-- Delete attribute if it is in the relation
-- Delete all fds where it is on the left side
-- Delete attribute from all fds where it is on the right side
deleteAttribute :: Char -> Relation -> Relation
deleteAttribute x r@(attrs, fds) = (filter (/= x) attrs, go x fds)
  where
    go b [] = []
    go b ((FD cs ds) : xs) = let intersection = filter (/= b) ds in if b `elem` cs || null intersection then go b xs else FD cs intersection : go b xs
    go b (d@(MVD cs ds) : xs) = if b `elem` cs || b `elem` ds then go b xs else d : go b xs

closure :: String -> [Dependency] -> String
closure attrs fds = if length recursiveAttrs > length nubbedAttrs then closure recursiveAttrs fds else recursiveAttrs
  where
    go [] = []
    go (x : xs) = checkFds x fds ++ go xs
    checkFds _ [] = []
    checkFds ts ((FD as bs) : zs) = if equalAsSets ts as then bs ++ checkFds ts zs else checkFds ts zs
    checkFds ts ((MVD _ _) : zs) = checkFds ts zs
    nubbedAttrs = nub attrs
    recursiveAttrs = nub $ nubbedAttrs ++ go (subsequences $ nub attrs)

getKeys :: Relation -> [String]
getKeys r@(attrs, _) = filter (`isKey` r) $ subsequences attrs

getCandidateKeys :: Relation -> [String]
getCandidateKeys r = nub $ filter (`isCandidateKey` r) $ getKeys r

isCandidateKey :: String -> Relation -> Bool
isCandidateKey key r = isKey key r && not (any predicate (subsequences key))
  where
    predicate ys = not (equalAsSets ys key) && isKey ys r

isKey :: String -> Relation -> Bool
isKey xs r@(attrs, fds) = equalAsSets attrs (closure xs fds)

isIn1NF :: Relation -> Bool
isIn1NF _ = True

-- voll funktionale Abhängigkeit aller nicht schlüssel attribute
isIn2NF :: Relation -> Bool
isIn2NF r = isIn1NF r

-- Check for all FDs alpha -> beta (but only fds):
-- 1. Is trivial
-- 2. Is key
-- 3. beta only contains key attrs
isIn3NF :: Relation -> Bool
isIn3NF r@(attrs, fds) = isIn2NF r && go fds  
    where
        go [] = True
        go (FD as bs : xs) = (isKey as r || all (\x -> x `elem` as || x `elem` candidateKeys) bs) && go xs
        go (MVD as bs : xs) = go xs
        candidateKeys = concat $ getCandidateKeys r

-- and [isTrivial x r || isKey x r | (_ [a]) <- fds]

isTrivial :: Dependency -> Relation -> Bool
isTrivial fd _ = isTrivialFD fd
isTrivial mvd r = isTrivialMVD mvd r

isTrivialFD :: Dependency -> Bool
isTrivialFD (FD xs ys) = ys `subsetOf` xs 
isTrivialFD (MVD xs ys) = undefined

isTrivialMVD :: Dependency -> Relation -> Bool
isTrivialMVD (FD xs ys) _ = undefined 
isTrivialMVD (MVD xs ys) (attrs, _) = ys `subsetOf` xs || equalAsSets (filter (`notElem` xs) attrs) ys

isKeyAttribute :: Char -> Relation -> Bool
isKeyAttribute x r = x `elem` concat (getKeys r)

isInBCNF :: Relation -> Bool
isInBCNF = undefined

isIn4NF :: Relation -> Bool
isIn4NF = undefined

synthesisAlgorithm :: Relation -> [Relation]
synthesisAlgorithm = undefined

leftReduction :: Relation -> Relation
leftReduction = undefined

rightReduction :: Relation -> Relation
rightReduction = undefined

decompositionAlgorithm :: Relation -> [Relation]
decompositionAlgorithm = undefined