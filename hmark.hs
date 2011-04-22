import Control.Monad
import Text.Generators.Markov
import Database.TokyoCabinet.BDB
import System.Environment

printHelpAndDie = do
	putStrLn "Usage: hmark <basename> learn"
	putStrLn "Fills database with phrases from STDIN, separated by newlines"
	putStrLn "       hmark <basename> burst"
	putStrLn "Reads phrases from STDIN and replies to them is possible"
	putStrLn "<basename> is base name for databases (they will be created if do not exist)"
	fail "Wrong syntax!"
	
main = do
	args <- getArgs
	when (length args < 2) (printHelpAndDie)
	db <- new
	rdb <- new
	open db (head args ++ ".db") [OREADER, OWRITER, OCREAT]
	open rdb (head args ++ "_r.db") [OREADER, OWRITER, OCREAT]
	case args !! 1 of
		"train" -> do
			trainFromStdin (db, rdb)
		"burst" -> do
			burstFromStdin (db, rdb)
		otherwise -> printHelpAndDie
	close db
	close rdb