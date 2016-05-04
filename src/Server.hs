{-# LANGUAGE OverloadedStrings #-}

module Server where

import Types

import Control.Concurrent (myThreadId)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either (EitherT, left)
import Data.ByteString.Lazy (ByteString)
import Data.IORef (IORef, atomicModifyIORef', readIORef)
import Data.List ((\\), sortBy)
import Data.Ord (comparing)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Network.HTTP.Types (ok200)
import Network.Wai (responseLBS)
import qualified Data.IntMap.Lazy as IM (IntMap, elems, insert, keys, lookup, member)
import qualified Data.Text.Lazy as T
import Servant ((:<|>)(..), Proxy(..), Server, ServantErr, err404, errBody)
import Servant.Docs (DocIntro(..), docsWithIntros)
import Servant.Docs.Pandoc (pandoc)
import Text.Pandoc (writeHtmlString)
import Text.Pandoc.Options (def)


vehicleAPI :: Proxy VehicleAPI
vehicleAPI = Proxy


vehicleAPIWithDocs :: Proxy VehicleAPIWithDocs
vehicleAPIWithDocs = Proxy


server :: IORef (IM.IntMap Vehicle) -> Server VehicleAPI
server ref = getAllVehicles
        :<|> getVehicleById
        :<|> postVehicle
        :<|> putVehicle
        -----
        :<|> getIssuesById
        :<|> putIssues
  where
    getAllVehicles :: EitherT ServantErr IO [Vehicle]
    getAllVehicles = do
        liftIO $ print =<< myThreadId
        IM.elems <$> liftIO (readIORef ref)

    getVehicleById :: CaptureInt -> EitherT ServantErr IO Vehicle
    getVehicleById (CaptureInt i) = maybe notFound return =<< IM.lookup i <$> liftIO (readIORef ref)

    notFound :: EitherT ServantErr IO a
    notFound = left err404 { errBody = "Vehicle ID not found." }

    postVehicle :: Vehicle -> EitherT ServantErr IO Vehicle
    postVehicle v = liftIO $ atomicModifyIORef' ref insertIntoTbl
      where
        insertIntoTbl :: IM.IntMap Vehicle -> (IM.IntMap Vehicle, Vehicle)
        insertIntoTbl tbl = let newUniqueId = head . ([0..] \\) $ IM.keys tbl
                                tbl'        = IM.insert newUniqueId v tbl
                            in (tbl', v)

    putVehicle :: CaptureInt -> Vehicle -> EitherT ServantErr IO Vehicle
    putVehicle (CaptureInt i) v = putHelper f
      where
        f :: IM.IntMap Vehicle -> (IM.IntMap Vehicle, Maybe Vehicle)
        f tbl | i `IM.member` tbl = let tbl' = IM.insert i v tbl
                                    in (tbl', Just v)
              | otherwise         = (tbl, Nothing)

    putHelper :: (IM.IntMap Vehicle -> (IM.IntMap Vehicle, Maybe a)) -> EitherT ServantErr IO a
    putHelper f = maybe notFound return =<< liftIO (atomicModifyIORef' ref f)

    -----

    getIssuesById :: CaptureInt -> Maybe SortBy -> EitherT ServantErr IO [Issue]
    getIssuesById ci msb = do
        unsorted <- issues <$> getVehicleById ci
        return . maybe unsorted (sortIssues unsorted) $ msb
      where
        sortIssues :: [Issue] -> SortBy -> [Issue]
        sortIssues is sb = case sb of ByType     -> sortHelper issueType is
                                      ByPriority -> sortHelper priority  is

        sortHelper :: (Ord a) => (Issue -> a) -> [Issue] -> [Issue]
        sortHelper = sortBy . comparing

    putIssues :: CaptureInt -> [Issue] -> EitherT ServantErr IO [Issue]
    putIssues (CaptureInt i) is = putHelper f
      where
        f :: IM.IntMap Vehicle -> (IM.IntMap Vehicle, Maybe [Issue])
        f tbl = maybe (tbl, Nothing) found $ IM.lookup i tbl
          where
            found :: Vehicle -> (IM.IntMap Vehicle, Maybe [Issue])
            found v = let v'   = v { issues = is }
                          tbl' = IM.insert i v' tbl
                      in (tbl', Just is)


serverWithDocs :: IORef (IM.IntMap Vehicle) -> Server VehicleAPIWithDocs
serverWithDocs ref = server ref :<|> serveDocs
  where
    serveDocs _ respond = respond . responseLBS ok200 [("Content-Type", "text/html")] $ docsBS


docsBS :: ByteString -- Raw endpoints must serve content in the lazy bytestring format.
docsBS = encodeUtf8 . T.pack . writeHtmlString def . pandoc . docsWithIntros [intro] $ vehicleAPI
  where
    intro = DocIntro "Welcome" [ "This is our mock auto API.", "Enjoy!" ]
