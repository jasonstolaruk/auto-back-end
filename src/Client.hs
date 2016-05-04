{-# LANGUAGE OverloadedStrings #-}

module Client ( tryGetAllVehicles
              , tryGetIssuesById
              , tryGetVehicleById
              , tryPostVehicle
              , tryPutIssues
              , tryPutVehicle ) where

import Server
import Types

import Control.Monad.Trans.Either (EitherT, runEitherT)
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Servant ((:<|>)(..))
import Servant.Client (BaseUrl(..), Scheme(..), ServantError, client)


getAllVehicles ::                               EitherT ServantError IO [Vehicle]
getVehicleById :: CaptureInt                 -> EitherT ServantError IO Vehicle
postVehicle    ::               Vehicle      -> EitherT ServantError IO Vehicle
putVehicle     :: CaptureInt -> Vehicle      -> EitherT ServantError IO Vehicle
-----
getIssuesById  :: CaptureInt -> Maybe SortBy -> EitherT ServantError IO [Issue]
putIssues      :: CaptureInt -> [Issue]      -> EitherT ServantError IO [Issue]


( getAllVehicles :<|>
  getVehicleById :<|>
  postVehicle    :<|>
  putVehicle     :<|>
  getIssuesById  :<|>
  putIssues ) = client vehicleAPI host
  where
    host = BaseUrl Http "localhost" 8081


tryGetAllVehicles :: IO ()
tryGetAllVehicles = tryEndpoint getAllVehicles


tryGetVehicleById :: CaptureInt -> IO ()
tryGetVehicleById = tryEndpoint . getVehicleById


tryPostVehicle :: Vehicle -> IO ()
tryPostVehicle = tryEndpoint . postVehicle


tryPutVehicle :: CaptureInt -> Vehicle -> IO ()
tryPutVehicle ci = tryEndpoint . putVehicle ci


{-
Example usage:
tryGetIssuesById (CaptureInt 1) Nothing
tryGetIssuesById (CaptureInt 1) . Just $ ByPriority
-}
tryGetIssuesById :: CaptureInt -> Maybe SortBy -> IO ()
tryGetIssuesById ci = tryEndpoint . getIssuesById ci


{-
Example usage:
tryPutIssues (CaptureInt 0) [ Issue Battery High, Issue Brakes Low ]
-}
tryPutIssues :: CaptureInt -> [Issue] -> IO ()
tryPutIssues i = tryEndpoint . putIssues i


tryEndpoint :: (Show a) => EitherT ServantError IO a -> IO ()
tryEndpoint f = do
    res <- runEitherT f
    case res of Left  err -> T.putStrLn $ "Error: " <> T.pack (show err)
                Right x   -> T.putStrLn . T.pack . show $ x
