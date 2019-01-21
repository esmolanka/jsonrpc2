# jsonrpc2

Simple JSON-RPC 2.0 interface.

Example JSON-RPC 2.0 server:

```
{-# LANGUAGE OverloadedStrings #-}

import System.IO
import qualified System.IO.Streams as Streams
import Control.Monad.Except
import Network.JsonRpc2

sumNumbers :: (Monad m) => [Int] -> ExceptT (RpcError ()) m Int
sumNumbers xs =
  pure (sum xs)

helloWorld :: [String] -> IO ()
helloWorld xs = hPutStrLn stderr (unwords $ "Hello," : xs)

main :: IO ()
main = do
  packetStream
    (handleStream
      ("sum" ~: sumNumbers)
      ("hello" ^: helloWorld))
    Streams.stdin
    Streams.stdout
```

Input:

```
Content-Length: 66

{"jsonrpc": "2.0", "id": 0, "method": "sum", "params": [1,10,100]}

Content-Length: 59

{"jsonrpc": "2.0", "method": "hello", "params": ["World!"]}
```

Output:

```
Hello, World!
Content-Length: 39

{"result":111,"jsonrpc":"2.0","id":0}
```
