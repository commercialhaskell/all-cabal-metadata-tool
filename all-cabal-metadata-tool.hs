{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Client (newManager, withResponse, responseBody, brConsume, Manager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import           ClassyPrelude.Conduit
import qualified Codec.Archive.Tar                     as Tar
import qualified Data.ByteString.Lazy                  as L
import           Data.Semigroup                        (Max (Max))
import           Data.Version                          (Version)
import           Distribution.Compat.ReadP             (readP_to_S)
import           Distribution.Package                  (PackageName, PackageIdentifier (..))
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse
import           Distribution.Text                     (parse, disp)
import Text.PrettyPrint (render)
import qualified Distribution.Text
import           System.Directory
import           System.FilePath
import           System.IO                             (IOMode (ReadMode),
                                                        openBinaryFile)
import Data.Yaml
import Data.Aeson
import Stackage.Install
import Codec.Compression.GZip (decompress)
import Crypto.Hash.SHA256 (hashlazy)
import qualified Data.ByteString.Base16 as B16
import Stackage.PackageIndex.Conduit
import Stackage.Metadata

main :: IO ()
main = do
    man <- newManager tlsManagerSettings
    packageLocation <- defaultPackageLocation
    indexLocation <- defaultIndexLocation
    let set = setGetManager (return man)
            $ setPackageLocation (return packageLocation)
            $ setIndexLocation (return indexLocation)
              defaultSettings

    newest <- runResourceT $ sourceAllCabalFiles (return indexLocation) $$ flip foldlC mempty
        (\m cfe ->
            let name = cfeName cfe
                version = cfeVersion cfe
             in flip (insertMap name) m $ case lookup (cfeName cfe) m of
                    Nothing -> (version, singletonSet version)
                    Just (version', s) -> (max version version', insertSet version s))

    let onlyNewest cfe =
            case lookup (cfeName cfe) $ asMap newest of
                Nothing -> assert False Nothing
                Just (latest, allVersions)
                    | cfeVersion cfe == latest -> Just (cfe, allVersions)
                    | otherwise -> Nothing

    runResourceT
        $ sourceAllCabalFiles (return indexLocation)
       $$ concatMapC onlyNewest
       =$ (do
            concatMapMC (updatePackage set packageLocation)
            liftIO $ saveDeprecated man
            )
       =$ takeC 500
       =$ sinkNull

saveDeprecated :: Manager -> IO ()
saveDeprecated man = do
    bss <- withResponse "https://hackage.haskell.org/packages/deprecated.json" man
        $ \res -> brConsume $ responseBody res
    deps <- either throwIO return $ decodeEither' $ concat bss
    encodeFile "deprecated.yaml" (deps :: [Deprecation])

updatePackage :: MonadResource m
              => Settings
              -> (String -> String -> FilePath)
              -> (CabalFileEntry, Set Version)
              -> m (Maybe ())
updatePackage set packageLocation (cfe, allVersions) = do
    epi <- liftIO $ decodeFileEither fp
    case epi of
        Right pi
            | version == piLatest pi
           && thehash == piHash pi
           -> return Nothing
        _ -> do
            putStrLn $ "Loading " ++ tshow (name, version)
            gpd <-
                case mgpd of
                    ParseFailed pe -> error $ show (name, version, pe)
                    ParseOk _ gpd -> return gpd

            let pd = packageDescription gpd
            when (package pd /= PackageIdentifier name version) $
                error $ show ("mismatch", name, version, package pd)

            let name' = renderDistText name
                version' = renderDistText version
            liftIO $ download set [(name', version')]
            let tarball = packageLocation name' version'

            (desc, desct, cl, clt) <-
                sourceTarFile True tarball $$ foldlC goEntry
                    (pack $ description pd, "haddock", "", "")

            liftIO $ do
                createDirectoryIfMissing True $ takeDirectory fp
                encodeFile fp PackageInfo
                    { piLatest = version
                    , piHash = thehash
                    , piAllVersions = allVersions
                    , piSynopsis = pack $ synopsis pd
                    , piDescription = desc
                    , piDescriptionType = desct
                    , piChangeLog = cl
                    , piChangeLogType = clt
                    }
                return $ Just ()
  where
    name = cfeName cfe
    version = cfeVersion cfe
    lbs = cfeRaw cfe
    mgpd = cfeParsed cfe

    fp = "packages" </> (take 2 $ name' ++ "XX") </> name' <.> "yaml"
    name' = renderDistText name

    thehash = decodeUtf8 $ B16.encode $ hashlazy lbs

    goEntry :: (Text, Text, Text, Text) -> Tar.Entry -> (Text, Text, Text, Text)
    goEntry orig@(desc, desct, cl, clt) e =
        case (toEntryType $ Tar.entryPath e, toText $ Tar.entryContent e) of
            (ChangeLog clt', Just cl') -> (desc, desct, cl', clt')
            (Desc desct', Just desc') -> (desc', desct', cl, clt)
            _ -> orig

    toText (Tar.NormalFile lbs _) = Just $ decodeUtf8 $ toStrict lbs
    toText _ = Nothing

data EntryType = Ignored | ChangeLog Text | Desc Text

toEntryType :: FilePath -> EntryType
toEntryType fp
    | name == "changelog" = ChangeLog t
    | name == "readme" = Desc t
    | otherwise = Ignored
  where
    (name', ext) = splitExtension fp
    name = toLower name'
    t =
        case ext of
            ".md" -> "markdown"
            _ -> "text"
