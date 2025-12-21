module Main where

import WebServer (startServer)

main :: IO ()
main = do
  putStrLn "========================================="
  putStrLn "   Tax Calculator Web Server"
  putStrLn "========================================="
  putStrLn ""
  putStrLn "Starting server..."
  putStrLn "Open your browser and go to:"
  putStrLn "  http://localhost:3000"
  putStrLn ""
  putStrLn "Press Ctrl+C to stop the server"
  putStrLn "========================================="
  startServer