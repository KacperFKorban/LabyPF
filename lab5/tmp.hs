import System.Environment
import System.IO
import qualified Data.ByteString as BStr

main = do
  (inFileName:outFileName:_) <- getArgs
  inpStr <- BStr.readFile inFileName
  BStr.writeFile outFileName inpStr
