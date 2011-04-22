module Text.Generators.Markov
	(	trainFromStdin
		, burstFromStdin
		, build
		, openDBs
		, closeDBs
		, riff
		, DBS(..)
	)
	where

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS8
import Data.List
import Data.Maybe
import Database.TokyoCabinet.BDB
import Safe
import System.Environment
import System.IO
import Control.Monad
import System.Random

-- | Databases set
type DBS = (BDB, BDB)

-- | Opens databases with given basename
openDBs :: String -> IO DBS
openDBs base = do
	db <- new
	rdb <- new
	open db (base ++ ".db") [OREADER, OWRITER, OCREAT]
	open rdb (base ++ "_r.db") [OREADER, OWRITER, OCREAT]
	return (db, rdb)

-- | Closes databases
closeDBs :: DBS -> IO ()
closeDBs (db, rdb) = do
	close db
	close rdb
	return ()

randomEntry lst = do
	num <- randomRIO (0, length lst - 1)
	return (lst !! num)

toPhrase [b1, b2] = BS.concat [b1, BS8.fromString " " , b2]
fromPhrase = (\(a,b) -> (a, tailSafe b) ) . break (== ' ') . BS8.toString

triples xs | null (drop 2 xs) = Nothing
	| otherwise = Just $ (take 3 xs, tail xs)

toTriplets = unfoldr (triples)

pairs xs | null (drop 1 xs) = Nothing
	| otherwise = Just $ ((xs !! 0, xs !! 1), tail xs)

toPairs = unfoldr (pairs)

train db sentence = do
	let swords = [""] ++ sentence ++ [""]
	mapM_ (learnTriplet db) (toTriplets swords)

-- | Reads newline-separated phrases from STDIN and learns from them
trainFromStdin :: DBS -- Databases
	-> IO ()
trainFromStdin (db, rdb) = do
	eof <- hIsEOF stdin
	if not eof then do
		lineB <- BS.hGetLine stdin
		let line = BS8.toString lineB
		train db $ words line
		train rdb $ reverse $ words line
		hFlush stdout
		trainFromStdin (db, rdb)
		else
		return ()

-- | shuffles list
shuffle :: [a] -> IO [a]
shuffle xs = do
	gen <- newStdGen
	let zipd = zip xs (take (length xs) ((randoms gen) :: [Float]))
	return (map (fst) (sortBy (\a b -> (snd a) `compare` (snd b)) zipd))

-- | Attempts to build a phrase based on given one
riff :: DBS -- ^ Databases
	-> [String] -- ^ Starting phrase
	-> IO (Maybe [String]) -- ^ Maybe result
riff (db, rdb) sentence = do
	ps <- shuffle $ toPairs sentence
	ssentence <- shuffle sentence
	
	let tryBuild phs = case phs of
		(ph:rest) -> do
			phrase <- build (db, rdb) ph
			if length phrase == 2 then
				tryBuild rest
				else do
				return $ Just phrase
		[] -> return Nothing
	try1 <- tryBuild ps
	
	case try1 of
		Just _ -> return try1
		Nothing -> do -- Let's try to find similar words
			let trySimilar tdb ws = case ws of
				(w:rest) -> do
					ls <- fwmkeys tdb (BS8.fromString w) (-1) :: IO [BS8.ByteString]
					if null ls then
						trySimilar tdb rest
						else do
						candidate <- randomEntry ls
						try <- build (db, rdb) $ (\(a,b) -> (b,a)) (fromPhrase candidate)
						if length try == 2 then
							trySimilar tdb rest
							else
							return $ Just try
				[] -> return Nothing
			fwdTry <- trySimilar db ssentence
			if isJust fwdTry then
				return fwdTry
				else
				trySimilar rdb ssentence
	
-- | Reads newline-separated lines from STDIN and replies to STDOUT
burstFromStdin :: DBS -> IO ()
burstFromStdin (db, rdb) = catch (do
	line <- hGetLine stdin
	resM <- riff (db, rdb) $ words line
	case resM of
		Nothing -> return ()
		Just res -> putStrLn $ unwords res
	burstFromStdin (db, rdb)) (const $ return ())

learnTriplet db ws = do
	putdup db (BS8.fromString $ intercalate " " $ take 2 ws) (BS8.fromString $ ws !! 2)

-- | Attempts to build a phrase using given two words and databases
build :: DBS -- ^ Databases
	-> (String, String) -- ^ Starting words
	-> IO [String] -- ^ Result
build (db, rdb) (w1, w2) = do
	bl <- (build' db $ map (BS8.fromString) [w2, w1]) >>= return . reverse
	rbl <- (build' rdb $ map (BS8.fromString) [w1, w2])
	return $ map (BS8.toString) $ rbl ++ drop 2 bl

build' :: BDB -> [BS.ByteString] -> IO [BS.ByteString]
build' db sofar = do
	lst <- getlist db (BS.intercalate (BS8.fromString " ") $ reverse $ take 2 sofar)
	case lst of
		[] -> return sofar
		otherwise -> do
			rword <- randomEntry lst
			if BS.null rword then
				return sofar
				else
				build' db (rword : sofar)