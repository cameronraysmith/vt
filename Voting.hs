module Voting 

where

import List
import Ratio

type Alternative = Int
type Ballot = [Alternative]
type Profile = [Int]

cycles :: Int -> [[Int]]
cycles n = map (0:) (perms [1..n-1])

listCycles :: [a] -> [[a]] 
listCycles xs = let 
   n = length xs 
   lst = cycle xs
   cls ys = take n ys : cls (drop (n+1) ys) 
  in take n (cls lst) 

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs))
  where 
    insrt :: a -> [a] -> [[a]]
    insrt x [] = [[x]]
    insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

getCycle :: Int -> Int -> [[Int]]
getCycle m k = listCycles ((cycles m) !! k)

getCycle' :: Int -> Int -> [[Char]]
getCycle' m k = map (map int2chr) (getCycle m k)

int2chr :: Int -> Char
int2chr n = toEnum (n + 97)

fac :: Int -> Int 
fac m = product [1..m]

genBallots :: Int -> [Ballot]
genBallots m = let 
    k = fac (m-1) - 1
  in concat [ getCycle m i | i <- [0..k] ]

genBallots' :: Int -> [String]
genBallots' = map (map int2chr) . genBallots

size :: Profile -> Int 
size xs = length $ fst $ findBallot xs 0

findBallot :: Profile -> Int -> (Ballot,Int)
findBallot xs n = let 
    m = length xs
    factorials = [ fac k | k <- [2..] ]
    candidates = takeWhile (\x -> x <= m) factorials
    lst        = last candidates
    k          = length candidates + 1
  in 
    if m == lst then (genBallots k!!n,xs!!n) 
    else error "incorrect profile length"

findBallot' :: Profile -> Int -> (String,Int)
findBallot' xs n = let 
    m = length xs
    factorials = [ fac k | k <- [2..] ]
    candidates = takeWhile (\x -> x <= m) factorials
    lst        = last candidates
    k          = length candidates + 1
  in 
    if m == lst then (genBallots' k!!n,xs!!n) 
    else error "incorrect profile length"

expand :: Profile -> [(Ballot,Int)]
expand xs = map (findBallot xs) [0..length xs-1]

expand' :: Profile -> [(String,Int)]
expand' xs = map (findBallot' xs) [0..length xs-1]

votes :: Profile -> Ballot -> Int
votes profile ballot = let 
    eprofile = expand profile
    Just k = lookup ballot eprofile
  in 
    k 

voteSize :: Profile -> Int
voteSize = sum 

norm :: Profile -> Profile
norm profile = 
  let 
    xs = filter (/= 0) profile
    k  = if null xs then 1 else foldl1 gcd xs
    f  = flip div k
  in map f profile 

type Nprofile = [Ratio Int]

nrm :: Profile -> Nprofile
nrm profile = let 
   total = sum profile 
 in 
   map (\ k -> (k % total)) profile

nullprofile :: Int -> Profile 
nullprofile m = take (fac m) (repeat 0)

unit :: Int -> Int -> Profile
unit m i = let  
   f = \ (x,y) -> if y == i then (x+1) else x
 in 
   map f (zip (nullprofile m) [0..])

type VotingRule = Profile -> [Alternative]
type VotingRule' = Profile -> [Char]

type ResVotingRule  = Profile -> Alternative
type ResVotingRule' = Profile -> Char

tieBreak :: [Alternative] -> VotingRule -> ResVotingRule
tieBreak tiebreaklist f profile = let 
    results = f profile 
    m       = size profile
    order   = list2ordering (take m tiebreaklist)
  in  
    if null results then error "no winners selected"
    else if length results == 1 then head results
    else head (sortBy order results)

list2ordering :: Eq a => [a] -> a -> a -> Ordering
list2ordering xs x y | x == y  = EQ
                     | elem x (dropWhile (/= y) xs) = GT
                     | otherwise = LT

type Score = [Int]

findMaxValues :: Score -> [(Int,Int)]
findMaxValues values = 
   filter (\ (_,x) -> x == maximum values) 
          (zip [0..] values)

winners :: Score -> [Alternative]
winners = map fst . findMaxValues

findMinValues :: Score -> [(Int,Int)]
findMinValues values = 
   filter (\ (_,x) -> x == minimum values) 
          (zip [0..] values)

losers :: Score -> [Alternative]
losers = map fst . findMinValues

type SF = Profile -> Score

sf2votingrule :: SF -> VotingRule
sf2votingrule sf = winners . sf 

type ScoringVector = [Int]
type ScoringVF = Int -> ScoringVector

pluralityVector :: ScoringVF
pluralityVector n = 1 : (take (n-1) $ repeat 0)

antipluralityVector ::  ScoringVF
antipluralityVector n = (take (n-1) $ repeat 1) ++ [0]

bordaVector :: ScoringVF
bordaVector n = reverse [0..n-1]

normSV :: ScoringVector -> ScoringVector
normSV ws = let 
     w = last ws
     xs = zipWith (-) ws (repeat w)
     ys = init xs
     k  = if null ys then 1 else foldl1 gcd ys
     f  = flip div k 
  in 
     map f xs 

vector2sf :: ScoringVF -> SF
vector2sf vectorfct profile = 
  let 
    m          = size profile
    vector     = vectorfct m
    eprofile   = expand profile
    vmult k b  = [ (x, k*(vector!!n)) 
                           | (x,n) <- zip b [0..] ]
    poscounts  = concat $ 
                 map (\(ys,k) -> vmult k ys) eprofile
    f x ys     = map snd (filter (\ (y,_) -> y == x) ys)
  in 
   [ sum (f x poscounts) | x <- [0..m-1] ]

svf2votingrule :: ScoringVF -> VotingRule
svf2votingrule = sf2votingrule . vector2sf

bordaSC, plurSC, vetoSC :: SF
bordaSC = vector2sf bordaVector
plurSC  = vector2sf pluralityVector
vetoSC  = vector2sf antipluralityVector

borda, plur, veto :: VotingRule
borda = svf2votingrule bordaVector
plur = svf2votingrule pluralityVector
veto = svf2votingrule antipluralityVector

bordaR, plurR, vetoR :: ResVotingRule
bordaR = tieBreak [0..] borda
plurR =  tieBreak [0..] plur 
vetoR =  tieBreak [0..] veto 

borda', plur', veto' :: VotingRule'
borda' = map int2chr . borda
plur'  = map int2chr . plur
veto'  = map int2chr . veto

bordaR', plurR', vetoR' :: ResVotingRule'
bordaR' = int2chr . bordaR
plurR'  = int2chr . plurR
vetoR'  = int2chr . vetoR

majority :: VotingRule
majority profile = let 
   m       = size profile
   score   = vector2sf pluralityVector profile
   results = findMaxValues score
   total   = sum profile
   (winner,votes) = head results
 in 
   if (fromIntegral votes) / (fromIntegral total) > 0.5 
     then [winner]
     else [0..(m-1)]

unanimity :: VotingRule
unanimity profile = let
   m       = size profile
   xs      = vector2sf pluralityVector profile
   results = findMaxValues xs
   total   = sum profile
   (winner,votes) = head results
 in 
   if votes == total && total > 0
     then [winner]
     else [0..(m-1)]

nearUnanimity :: VotingRule
nearUnanimity profile = let
   m       = size profile
   xs      = vector2sf pluralityVector profile
   results = findMaxValues xs
   total   = sum profile
   (winner,votes) = head results
 in 
   if votes+1 >= total && total > 0
     then [winner]
     else [0..(m-1)]

majority', unanimity', nearUnanimity' :: VotingRule'
majority'      = map int2chr . majority
unanimity'     = map int2chr . unanimity 
nearUnanimity' = map int2chr . nearUnanimity 

restrict :: [Alternative] -> Profile -> Profile 
restrict xs profile = let 
    m = length profile 
    eprofile = expand profile 
    f = filter (flip elem xs)
    table = zip xs [0..]
    g = map (\ x -> let Just y = lookup x table in y)
    pprofile = map (\ (xs,k) -> (g (f xs), k)) eprofile
  in 
    makeProfile pprofile

example0 = [1,2,0,3,0,2] :: Profile

fact :: Integer -> Integer -> Integer
fact n k = product [n-k+1..n]

binom :: Integer -> Integer -> Integer
binom n k = div (fact n k) (fac k)
  where fac k = product [1..k] 

beats :: Profile -> Alternative -> Alternative -> Bool
beats p x y = let 
   p' = restrict [x,y] p 
 in majority p' == [0]

type MajorityGraph = Alternative -> Alternative -> Int

p2mg :: Profile -> MajorityGraph
p2mg p x y = if beats p x y then 1 else 0

pairscore :: Profile -> [Int]
pairscore profile = let 
    m  = size profile
    as = [0..m-1]
    mg = p2mg profile
  in 
    [ sum [ mg x y | y <- as \\ [x] ] | x <- as ]

cW :: Profile -> Alternative -> Bool
cW profile x = let 
    m  = size profile
  in 
    pairscore profile !! x == m-1

condorcet :: VotingRule 
condorcet profile = let 
    m  = size profile
    as = [0..m-1]
    f [] = as
    f (x:xs) = if cW profile x then [x] else f xs
  in 
    f as 

condorcet' :: VotingRule'
condorcet' = map int2chr . condorcet 

pairscore' :: Profile -> [(Int,Int)]
pairscore' profile = let 
    m  = size profile
    as = [0..m-1]
    mg = p2mg profile
  in 
    [ (sum [ mg x y | y <- as \\ [x] ], 
       sum [ mg y x | y <- as \\ [x] ])  | x <- as ]

copelandScore :: Profile -> Score
copelandScore profile = 
   map (\(x,y) -> x-y) (pairscore' profile)

copeland :: VotingRule 
copeland = winners . copelandScore 

copeland' :: VotingRule'
copeland' = map int2chr . copeland

hare :: VotingRule 
hare profile = let 
    m  = size profile 
    ps = plurSC profile
    maxs = findMaxValues ps
    mins = findMinValues ps  
    (winner,votes) = head maxs 
    total  = sum profile
    proportion = (fromIntegral votes) / (fromIntegral total)
    as = [0..m-1]
    ws = as \\ (losers ps)
    profile' = restrict ws profile
    g = \ n -> ws !! n
  in 
    if proportion > 0.5 then [winner]
    else if ws == [] then as 
    else map g (hare profile')

hare' :: Profile -> String
hare' = map int2chr . hare

plurRO :: ResVotingRule
plurRO profile = let 
    m        = size profile 
    score    = plurSC profile
    max1     = findMaxValues score
    f        = \ (n,k) ->  if elem (n,k) max1 then 0 else k
    max2     = findMaxValues (map f (zip [0..] score))
    max3     = if length max1 > 1 
               then max1 else max1 ++ max2
    ws       = map fst max3
    profile' = restrict ws profile
    g        = \ n -> ws !! n
  in
    if m == 2 then plurR profile
    else g (plurR profile')

plurRO' :: Profile -> Char
plurRO' = int2chr . plurRO

addP :: Profile -> Profile -> Profile
addP = zipWith (+)

profile1, profile2 :: Profile
profile1 = makeProfile'
  [("abcd",5),("bacd",6),("cabd",2),("dabc",10)]
profile2 = makeProfile'
  [("abcd",4),("bacd",4),("cabd",8),("dabc",2)]

subtrP :: Profile -> Profile -> Profile
subtrP p1 p2 = let
    p = zipWith (-) p1 p2 
  in 
    if any (<0) p then error "negative number of voters" 
                  else p 

multP :: Int -> Profile -> Profile
multP k profile = if k < 1 
                  then error "wrong multiplication factor"
                  else map (k*) profile

surplus :: Profile -> Profile
surplus profile = let
   m = size profile
   eprofile = expand profile
   n = fac (m - 1)
   cls = [ getCycle m k | k <- [0..n-1] ]
   vals = [ [ nr | (xs,nr) <- eprofile, ys <- c, xs == ys ] 
                 | c <- cls ]
   mins = [ minimum list | list <- vals ]  
 in 
   [ k | xs <- genBallots m, 
         (l,k) <- zip [0..] mins, 
         elem xs (getCycle m l) ]

reduce :: Profile -> Profile 
reduce profile = subtrP profile (surplus profile)

red :: VotingRule -> VotingRule 
red f p = f (reduce p) 

cast :: Int -> Ballot -> Profile -> Profile
cast k ballot profile = let 
    m = size profile 
    p = position ballot (genBallots m) 
    f = \ (x,n) -> if x == p then n+k else n
  in 
    map f (zip [0..] profile)

position :: Eq a => a -> [a] -> Int
position x xs = let 
    Just p = lookup x (zip xs [0..])
  in p 

cast1 :: Ballot -> Profile -> Profile
cast1 = cast 1

makeProfile :: [(Ballot,Int)] -> Profile
makeProfile [] = error "no ballots"
makeProfile [(x,k)] = let 
    m = length x
  in 
    cast k x (nullprofile m)
makeProfile ((x,k):xs) = cast k x (makeProfile xs)

makeProfile1 :: [Ballot] -> Profile
makeProfile1 xs = makeProfile (zip xs (repeat 1))

makeProfile' :: [(String,Int)] -> Profile
makeProfile' = makeProfile . map f where 
  f (xs,k) = (map chr2int xs,k)

makeProfile1' :: [String] -> Profile
makeProfile1' = makeProfile1 . map (map chr2int)

chr2int :: Char -> Int
chr2int c = fromEnum c - 97

florida :: Profile
florida = makeProfile' 
   [("abc",49),("bca",20),("bac",20),("cba",11)]

withdraw :: Int -> Ballot -> Profile -> Profile
withdraw k ballot profile = let 
    m = size profile 
    p = position ballot (genBallots m) 
    f = \ (x,n) -> if x == p && n < k then 
                      error "negative ballot number"
                   else if x == p then n-k 
                   else n
  in 
    map f (zip [0..] profile)

withdraw1 :: Ballot -> Profile -> Profile
withdraw1 = withdraw 1

noshowExample :: Profile
noshowExample = makeProfile' 
   [("abc",25),("cab",46), ("bca",24)]

noshow :: Profile
noshow = withdraw 2 [0,1,2] noshowExample

change :: Int -> Ballot -> Ballot -> Profile -> Profile
change k b b' = cast k b' . withdraw k b 

change1 :: Ballot -> Ballot -> Profile -> Profile
change1 = change 1 

change1' :: String -> String -> Profile -> Profile
change1' b b' = change 1 (map chr2int b) (map chr2int b')

better1 :: Ballot -> [Alternative] ->  [Alternative] -> Bool
better1 ballot outcome1 outcome2 = let 
    order = list2ordering ballot
  in
    and [ order x y /= GT | x <- outcome1, y <- outcome2 ]
    && 
    or  [ order x y == LT | x <- outcome1, y <- outcome2 ]

stratChange :: Int 
               -> Ballot 
               -> VotingRule 
               -> Profile -> [Ballot]
stratChange k b rule profile = let 
   m    = size profile
   alts = genBallots m \\ [b]
   x    = rule profile
   f y  = rule (change k b y profile)
 in 
   [ alt | alt <- alts, better1 b (f alt) x ]

stratChange1 :: Ballot 
                -> VotingRule 
                -> Profile -> [Ballot]
stratChange1 = stratChange 1

example1 = makeProfile1' ["abcd", "bdca", "dcab","cabd"]

bordaManip = map (map int2chr) $ 
             stratChange1 (genBallots 4!!0) borda example1

plurManip = map (map int2chr) $ 
             stratChange1 (genBallots 4!!0) plur example1

powerlist :: [a] -> [[a]]
powerlist [] = [[]]
powerlist (x:xs) = powerlist xs ++ map (x:) (powerlist xs)

payoff :: Int -> Ballot -> [Alternative] -> Int
payoff _ b [] = error "no winners selected"
payoff m b ws = let 
    outcomes = powerlist [0..m-1] \\ [[]]
  in 
    length [ vs | vs <- outcomes, better1 b ws vs ]

abstain :: VotingRule -> Profile -> Ballot -> Int
abstain r p b = let 
    m = size p 
    n = length b
  in 
    if m /= n then error "wrong ballot size"
    else payoff m b (r p) 

play :: Int -> VotingRule -> Profile 
            -> Ballot -> Ballot -> Int
play k r p b b' = let 
     m = size p
     n = length b 
     n' = length b'
     p' = cast k b' p
   in 
     if m /=n || n /= n' then error "wrong ballot size"
     else payoff m b (r p')  

play1 :: VotingRule -> Profile 
         -> Ballot -> Ballot -> Int
play1 = play 1

type Agent = Int

checkProfile :: Int -> Int -> Profile -> Bool
checkProfile m i p = size p == m && voteSize p == i 

makeGame :: [Ballot] -> Agent -> VotingRule
            -> Profile -> Ballot -> Int
makeGame bs k r p castb = let 
    trueb = bs !! k 
    m = length trueb
    n = length castb
    i = length bs - 1
    ok = checkProfile m i p
  in
    if m /= n then error "wrong ballot size" 
    else if not ok then error "wrong profile"
    else play1 r p trueb castb

makeGame' :: [String] -> Agent -> VotingRule
              -> Profile -> String -> Int
makeGame' bs k r p castb = let 
    bs' = map (map chr2int) bs
    castb' = map chr2int castb
  in 
    makeGame bs' k r p castb'

type Aballot = [Alternative]

genAballots :: Int -> [Aballot]
genAballots m = sublists [0..m-1]

sublists :: [a] -> [[a]]
sublists [] = [[]]
sublists (x:xs) = map (x:) (sublists xs) ++ sublists xs

complement :: Int -> [Alternative] -> [Alternative]
complement m ys = [0..m-1] \\ ys

genAballots' :: Int -> [String]
genAballots' = map (map int2chr) . genAballots

asize :: Profile -> Int
asize profile = let 
   m = fromIntegral (length profile)
 in 
   round (logBase 2 m)

aexpand :: Profile -> [(Aballot,Int)]
aexpand profile = let 
    m = asize profile
    g = \ (n,k) -> (genAballots m !! n, k) 
 in 
    map g (zip [0..] profile)

avotes :: Profile -> Ballot -> Int
avotes profile ballot = let 
    eprofile = aexpand profile
    Just k = lookup ballot eprofile
  in 
    k 

asurplus :: Profile -> Profile
asurplus profile = let 
   m        = asize profile
 in 
   [ min (avotes profile ballot) (avotes profile cballot)  
      | ballot  <- genAballots m, 
        cballot <- [complement m ballot]  ]

areduce :: Profile -> Profile 
areduce profile = subtrP profile (asurplus profile)

approvalScore :: Profile -> Score
approvalScore profile = let 
    m = asize profile 
    eprofile = aexpand profile 
    count = \ n -> sum [ k | (ballot,k) <- eprofile,
                              elem n ballot         ]
  in
    [ count n | n  <- [0..m-1] ]

approval :: VotingRule
approval = winners . approvalScore

approval' :: VotingRule'
approval' = map int2chr . approval

announce :: VotingRule 
            -> Profile -> Profile -> Bool
announce rule p1 p2 = rule p1 == rule p2

type Matrix = [Row]
type Row    = [Ratio Int]

rows, cols :: Matrix -> Int
rows m = length m
cols m | m == []   = 0 
       | otherwise = length (head m)

sf2matrix :: Int -> SF -> Matrix
sf2matrix m f = let 
    k = fac m 
  in 
    [ nrm $ f (unit m i) | i <- [0..k-1] ]

example2 = sf2matrix 3 (vector2sf pluralityVector)

example3 = sf2matrix 3 (vector2sf antipluralityVector)

example4 = sf2matrix 3 bordaSC

