
import SICP.DB
import SICP.LispParser

import Text.Parsec hiding (try, space)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Exception hiding (evaluate)

import System.Console.Haskeline hiding (catch)
import System.Environment

repl :: DB -> IO ()
repl db = runInputT (defaultSettings {historyFile = Just ".db"}) loop
    where

        loop :: InputT IO ()
        loop = do
            minput <- getInputLine "% "
            case minput of
                Nothing -> return ()
                Just input -> do
                    case parse (space >> expr <* eof) "" input of
                        Left e -> outputStrLn $ "Parse error: " ++ show e
                        Right x -> runQuery x
                    loop

        runQuery :: Value -> InputT IO ()
        runQuery x = do
            v <- withInterrupt $ liftIO $ tryInterrupt $ foldM (\_ s -> print s) () $ evaluate db x x
            case v of
                Left e -> outputStrLn $ show e
                Right _ -> return ()

        tryInterrupt :: IO a -> IO (Either Interrupt a)
        tryInterrupt = try

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            dbpe <- try $ parseFile filename
            case dbpe of
                Right dbp -> case dbp of
                    Right db -> repl $ compileDB db
                    Left pe -> print pe
                Left e -> printIOError e
        _ -> putStrLn "specify filename for the database in command line"
    where
        printIOError :: IOError -> IO ()
        printIOError e = putStrLn $ "### " ++ show e
