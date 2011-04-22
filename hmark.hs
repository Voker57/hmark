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
	dbs <- openDBs (head args)
	case args !! 1 of
		"train" -> do
			trainFromStdin dbs
		"burst" -> do
			burstFromStdin dbs
		otherwise -> printHelpAndDie
	closeDBs dbs