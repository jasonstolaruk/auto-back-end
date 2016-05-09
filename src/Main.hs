module Main ( main
            , runMock
            , writeHtmlDoc
            , writeMarkdownDoc ) where

import InitData
import Server
import Types

import Data.IORef (IORef, newIORef)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import qualified Data.IntMap.Lazy as IM (IntMap)
import Servant.Docs (API, docs, markdown)
import Servant.Docs.Pandoc (pandoc)
import Servant.Mock (mock)
import Servant.Server (serve)
import Text.Pandoc (writeHtmlString)
import Text.Pandoc.Options (def)


main :: IO ()
main = run 8081 . app =<< newIORef vehicleTbl


app :: IORef (IM.IntMap Vehicle) -> Application
app = serve vehicleAPIWithDocs . serverWithDocs


runMock :: IO ()
runMock = run 8081 . serve vehicleAPI . mock $ vehicleAPI


-- This binding encapsulates internal info about our API spec and forms the basis for the documentation we generate.
apiDocs :: API
apiDocs = docs vehicleAPI


writeMarkdownDoc :: IO ()
writeMarkdownDoc = writeFile "/Users/stolaruk/Desktop/doc.md" . markdown $ apiDocs


writeHtmlDoc :: IO ()
writeHtmlDoc = writeFile "/Users/stolaruk/Desktop/doc.html" . writeHtmlString def . pandoc $ apiDocs
