module Normalizer where

import Data.List (nub, subsequences, (\\), sort, insert)

data Dependency = FD {alpha :: String, beta :: String} | MVD {alpha :: String, beta :: String}
type Relation = ([Char], [Dependency])

instance Show Dependency where
  show (FD xs ys) = show xs ++ " -> " ++ show ys
  show (MVD xs ys) = show xs ++ " ->> " ++ show ys

instance Eq Dependency where
  FD _ _ == MVD _ _ = False
  FD as bs == FD cs ds = setEqual as cs && setEqual bs ds
  MVD as bs == FD cs ds = setEqual as cs && setEqual bs ds

setEqual :: String -> String -> Bool
setEqual xs ys = xs `subset` ys && ys `subset` xs

subset :: String -> String -> Bool
xs `subset` ys = all (`elem` ys) xs

empty :: Relation
empty = ([], [])

insertFD :: String -> String -> Relation -> Relation
insertFD xs ys = insertDependency (FD xs ys)

insertMVD :: String -> String -> Relation -> Relation
insertMVD xs ys = insertDependency (MVD xs ys)

insertDependency :: Dependency -> Relation -> Relation
insertDependency d r@(attrs, fds)
  | d `elem` fds = r
  | otherwise = (alpha d ++ beta d ++ attrs, d : fds)

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
deleteAttribute x (attrs, fds) = (filter (/= x) attrs, go x fds)
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
    checkFds ts ((FD as bs) : zs) = if setEqual ts as then bs ++ checkFds ts zs else checkFds ts zs
    checkFds ts ((MVD _ _) : zs) = checkFds ts zs
    nubbedAttrs = nub attrs
    recursiveAttrs = nub $ nubbedAttrs ++ go (subsequences $ nub attrs)

getKeys :: Relation -> [String]
getKeys r@(attrs, _) = filter (`isKey` r) $ subsequences attrs

getCandidateKeys :: Relation -> [String]
getCandidateKeys r = filter (`isCandidateKey` r) $ getKeys r

isCandidateKey :: String -> Relation -> Bool
isCandidateKey key r = isKey key r && not (any predicate (subsequences key))
  where
    predicate ys = not (setEqual ys key) && isKey ys r

isKey :: String -> Relation -> Bool
isKey xs (attrs, fds) = setEqual attrs (closure xs fds)

isIn1NF :: Relation -> Bool
isIn1NF _ = True

-- voll funktionale Abhängigkeit aller nicht schlüssel attribute
isIn2NF :: Relation -> Bool
isIn2NF r@(attrs, fds) = isIn1NF r

-- Check for all FDs alpha -> B (but only fds):
-- 1. Is trivial
-- 2. Is key
-- 3. beta only contains key attrs
isIn3NF :: Relation -> Bool
isIn3NF r@(attrs, fds) = isIn2NF r && go fds  
    where
        go [] = True
        go (FD as bs : xs) = (isKey as r || all (\x -> x `elem` as || x `elem` candidateKeyAttributes) bs) && go xs
        go (MVD as bs : xs) = go xs
        candidateKeyAttributes = concat $ getCandidateKeys r

isInBCNF :: Relation -> Bool
isInBCNF r@(_, fds) = isIn3NF r && all predicate fds
  where
    predicate fd@(FD xs _) = isTrivialFD fd || isKey xs r
    predicate (MVD xs ys) = True 

isIn4NF :: Relation -> Bool
isIn4NF r@(_, fds) = isInBCNF r && all predicate fds
  where
    predicate d = isTrivialMVD d r || isKey (alpha d) r

isTrivial :: Dependency -> Relation -> Bool
isTrivial fd@(FD xs ys) _ = isTrivialFD fd
isTrivial mvd@(MVD _ _) r = isTrivialMVD mvd r

isTrivialFD :: Dependency -> Bool
isTrivialFD (FD xs ys) = ys `subset` xs 
isTrivialFD (MVD xs ys) = undefined

isTrivialMVD :: Dependency -> Relation -> Bool
isTrivialMVD d (attrs, _) = alpha d `subset` beta d || setEqual (filter (`notElem` alpha d) attrs) (beta d)

isKeyAttribute :: Char -> Relation -> Bool
isKeyAttribute x r = x `elem` concat (getKeys r)

-- TODO
synthesisAlgorithm :: Relation -> [Relation]
synthesisAlgorithm = undefined

canonicalCover :: Relation -> Relation
canonicalCover = uniteFDs . removeEmptyFDs . rightReduction . leftReduction
  where
    removeEmptyFDs (attrs, fds) = (attrs, filter predicate fds)
    predicate (FD xs []) = False
    predicate fd = True

uniteFDs :: Relation -> Relation
uniteFDs r@(attrs, fds) = (attrs, go fds)
  where
    go [] = []
    go (mvd@(MVD _ _) : fds) = mvd : go fds
    go (fd@(FD xs ys) : fds) = 
      let
      notUniteable = filter (not . predicate xs) recursiveCall
      uniteable = map beta $ filter (predicate xs) recursiveCall
      predicate as (FD bs cs) = setEqual as bs 
      predicate _ (MVD bs cs) = False 
      recursiveCall = go fds 
      in if null uniteable then fd : recursiveCall else FD xs (ys ++ concat uniteable) : notUniteable

-- TODO
leftReduction :: Relation -> Relation
leftReduction (attrs, []) = (attrs, [])
leftReduction (attrs, FD xs ys : fds) = undefined 
leftReduction (attrs, MVD xs ys : fds) = undefined 

-- TODO
rightReduction :: Relation -> Relation
rightReduction = undefined

-- TODO
decompositionAlgorithm :: Relation -> [Relation]
decompositionAlgorithm = undefined