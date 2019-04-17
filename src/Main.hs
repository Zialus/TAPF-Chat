{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad (forever)
import           Control.Concurrent (forkFinally)
import           Control.Concurrent.MVar
import           Control.Exception hiding (handle)
import           System.IO
import           Network.Socket
import           Text.Printf
import           Data.List (delete)

import qualified Control.Exception as E

-- | client information
type ClientName = String

data Client = Client
  { clientName   :: ClientName
  , clientHandle :: Handle
  }

instance Eq Client where
  client1 == client2  = clientName client1 == clientName client2


-- | the server state:
-- a shared variable with the list of current clients
newtype Server = Server
  { clientList :: MVar [Client]
  }

type Message = String
 
-- | initialize the server state
newServer :: IO Server
newServer = do
  clients <- newMVar []
  return Server {clientList = clients}


-- | send a message to a client
-- silently ignores I/O exceptions e.g. when the client has closed the handle
-- but the server still hasn't noticed it
sendMessage :: Client -> Message ->  IO ()
sendMessage  Client{..} msg
  = hPutStrLn clientHandle msg `catch` (\(e::IOException) -> print e )

-- | broadcast a message to many clients
broadcast :: [Client] -> Message -> IO ()
broadcast clients msg  = mapM_ (\client -> sendMessage client msg) clients

-- | broadcast a message from a client to many clients
broadcastFromClient :: Client -> [Client] -> Message -> IO ()
broadcastFromClient sender clients msg  = mapM_ (\client -> if client /= sender
    then sendMessage client msg
    else sendMessage client "Message sent") clients

-- | try to add a new client 
checkAddClient :: Server -> ClientName -> Handle -> IO (Maybe Client)
checkAddClient Server{..} name handle  = do
  clients <- takeMVar clientList
  if invalidName name clients then do
    putMVar clientList clients 
    return Nothing
    else do
      let client = Client {clientName = name, clientHandle= handle}   
      putMVar clientList (client:clients)
      sendMessage client ("Hello, " ++ name ++ ".")
      broadcast clients (name ++ " has connected") 
      return (Just client)

-- | a name is invalid if it is empty or already taken
invalidName :: ClientName -> [Client] -> Bool
invalidName name list
  = null name || name `elem` map clientName list


-- | remove a client
removeClient :: Server -> Client -> IO ()
removeClient Server{..} client@Client{..} = do
  clients <- takeMVar clientList
  let clients' = delete client clients
  putMVar clientList clients'
  printf "User %s: disconnected\n" clientName
  broadcast clients' (clientName  ++ " disconnected")


-- | handle an new client
talk :: Handle -> Server -> IO ()
talk handle server@Server{..} = do
  hSetNewlineMode handle universalNewlineMode
  hSetBuffering handle LineBuffering
  hPutStrLn handle "Welcome to the Haskell chat server."
  getName
  where
    getName = do
      hPutStrLn handle "What is your name?"
      name <- hGetLine handle
      ok <- checkAddClient server name handle
      case ok of
        Nothing -> do
          hPrintf handle "Invalid name %s, please try again\n" (show name)
          getName
        Just client -> do
          printf "%s changed name to %s.\n" (show handle) (show name)
          runClient server client `finally` removeClient server client

-- | the client read loop
-- read one line at a time and broadcast to all clients;
-- the client is removed when the connection closes.
-- This could be improved e.g. by filtering empty lines or
-- parsing special commands such as "quit", etc.
runClient :: Server -> Client -> IO ()
runClient Server{..} client@Client{..} = forever $ do
  txt <- hGetLine clientHandle
  let msg = clientName ++ ": " ++ txt
  withMVar clientList $ \clients -> broadcastFromClient client clients msg

-- | the application entry point
main :: IO ()
main = withSocketsDo $ do
  server <- newServer
  addr <- resolve (show globalPort)
  printf "Listening on globalPort %d\n" globalPort
  E.bracket (open addr) close (loop server)
  where
    resolve port = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
        return addr
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        bind sock (addrAddress addr)
        -- If the prefork technique is not used,
        -- set CloseOnExec for the security reasons.
        let fd = fdSocket sock
        setCloseOnExecIfNeeded fd
        listen sock 10
        return sock
    loop server sock = forever $ do
        (conn, peer) <- accept sock
        putStrLn $ "Connection from " ++ show peer
        handle <- socketToHandle conn ReadWriteMode
        forkFinally (talk handle server) (\_ -> close conn)

globalPort :: Int
globalPort = 3000




