{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}
import qualified Codec.Archive.Tar                     as Tar
import           Control.Exception                     (assert)
import           Control.Monad                         (when)
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.Trans.Resource          (MonadResource,
                                                        runResourceT, throwM)
import           Crypto.Hash.SHA256                    (hashlazy)
import qualified Data.ByteString                       as S
import qualified Data.ByteString.Base16                as B16
import qualified Data.ByteString.Lazy                  as L
import           Data.Conduit                          (($$), (=$))
import qualified Data.Conduit.List                     as CL
import           Data.Map                              (Map)
import qualified Data.Map                              as Map
import           Data.Set                              (Set)
import qualified Data.Set                              as Set
import           Data.Text                             (Text, pack, toLower,
                                                        unpack)
import           Data.Text.Encoding                    (decodeUtf8With)
import qualified Data.Text.Encoding                    as TE
import           Data.Text.Encoding.Error              (lenientDecode)
import           Data.Version                          (Version)
import           Data.Yaml                             (decodeEither',
                                                        decodeFileEither,
                                                        encodeFile)
import           Distribution.Package                  (Dependency (..),
                                                        PackageIdentifier (..),
                                                        PackageName)
import           Distribution.PackageDescription       (CondTree (..),
                                                        Condition (..),
                                                        ConfVar (..),
                                                        condBenchmarks,
                                                        condExecutables,
                                                        condLibrary,
                                                        condTestSuites,
                                                        description, package,
                                                        packageDescription,
                                                        synopsis)
import           Distribution.PackageDescription.Parse (ParseResult (..))
import           Distribution.Version                  (VersionRange,
                                                        intersectVersionRanges, simplifyVersionRange)
import           Network.HTTP.Client                   (Manager, brConsume,
                                                        newManager,
                                                        responseBody,
                                                        withResponse)
import           Network.HTTP.Client.TLS               (tlsManagerSettings)
import           Prelude                               hiding (pi)
import           Stackage.Install
import           Stackage.Metadata
import           Stackage.PackageIndex.Conduit
import           Stackage.Update
import           System.Directory                      (createDirectoryIfMissing)
import           System.FilePath                       (splitExtension,
                                                        takeDirectory, (<.>),
                                                        (</>))

data Pair x y = Pair !x !y

main :: IO ()
main = do
    stackageUpdate $ setVerify False defaultStackageUpdateSettings

    man <- newManager tlsManagerSettings
    packageLocation <- defaultPackageLocation
    indexLocation <- defaultIndexLocation
    let set = setGetManager (return man)
            $ setPackageLocation (return packageLocation)
            $ setIndexLocation (return indexLocation)
              defaultSettings

    newest <- runResourceT $ sourceAllCabalFiles (return indexLocation)
        $$ flip CL.fold Map.empty
        (\m cfe ->
            let name = cfeName cfe
                version = cfeVersion cfe
             in flip (Map.insert name) m $ case Map.lookup (cfeName cfe) m of
                    Nothing -> Pair version (Set.singleton version)
                    Just (Pair version' s) -> Pair (max version version') (Set.insert version s))

    let onlyNewest cfe =
            case Map.lookup (cfeName cfe) newest of
                Nothing -> assert False Nothing
                Just (Pair latest allVersions)
                    | cfeVersion cfe == latest -> Just (cfe, allVersions)
                    | otherwise -> Nothing

    runResourceT
        $ sourceAllCabalFiles (return indexLocation)
       $$ CL.mapMaybe onlyNewest
       =$ (do
            CL.mapMaybeM (updatePackage set packageLocation)
            liftIO $ saveDeprecated man
            )
       =$ CL.isolate 500
       =$ CL.sinkNull

saveDeprecated :: Manager -> IO ()
saveDeprecated man = do
    bss <- withResponse "https://hackage.haskell.org/packages/deprecated.json" man
        $ \res -> brConsume $ responseBody res
    deps <- either throwM return $ decodeEither' $ S.concat bss
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
            liftIO $ putStrLn $ "Loading " ++ show (name, version)
            gpd <-
                case mgpd of
                    ParseFailed pe -> error $ show (name, version, pe)
                    ParseOk _ gpd -> return gpd

            let pd = packageDescription gpd
            when (package pd /= PackageIdentifier name version) $
                error $ show ("mismatch" :: String, name, version, package pd)

            let name'' = renderDistText name
                version' = renderDistText version
            liftIO $ download set [(name'', version')]
            let tarball = packageLocation name'' version'

            (desc, desct, cl, clt) <-
                sourceTarFile True tarball $$ CL.fold goEntry
                    (pack $ description pd, "haddock", "", "")

            liftIO $ do
                createDirectoryIfMissing True $ takeDirectory fp
                let checkCond = const True -- FIXME do something intelligent
                    getDeps' = getDeps checkCond
                encodeFile fp PackageInfo
                    { piLatest = version
                    , piHash = thehash
                    , piAllVersions = allVersions
                    , piSynopsis = pack $ synopsis pd
                    , piDescription = desc
                    , piDescriptionType = desct
                    , piChangeLog = cl
                    , piChangeLogType = clt
                    , piBasicDeps = combineDeps
                        $ maybe id ((:) . getDeps') (condLibrary gpd)
                        $ map (getDeps' . snd) (condExecutables gpd)
                    , piTestBenchDeps = combineDeps
                        $ map (getDeps' . snd) (condTestSuites gpd)
                       ++ map (getDeps' . snd) (condBenchmarks gpd)
                    }
                return $ Just ()
  where
    name = cfeName cfe
    version = cfeVersion cfe
    lbs = cfeRaw cfe
    mgpd = cfeParsed cfe

    fp = "packages" </> (unpack $ toLower $ pack $ take 2 $ name' ++ "XX") </> name' <.> "yaml"
    name' = renderDistText name

    thehash = TE.decodeUtf8 $ B16.encode $ hashlazy lbs

    goEntry :: (Text, Text, Text, Text) -> Tar.Entry -> (Text, Text, Text, Text)
    goEntry orig@(desc, desct, cl, clt) e =
        case (toEntryType $ Tar.entryPath e, toText $ Tar.entryContent e) of
            (ChangeLog clt', Just cl') -> (desc, desct, cl', clt')
            (Desc desct', Just desc') -> (desc', desct', cl, clt)
            _ -> orig

    toText (Tar.NormalFile lbs' _) = Just $ decodeUtf8With lenientDecode $ L.toStrict lbs'
    toText _ = Nothing

data EntryType = Ignored | ChangeLog Text | Desc Text

toEntryType :: FilePath -> EntryType
toEntryType fp
    | name == "changelog" = ChangeLog t
    | name == "changes" = ChangeLog t
    | name == "readme" = Desc t
    | otherwise = Ignored
  where
    (name', ext) = splitExtension fp
    name = unpack $ toLower $ pack name'
    t =
        case ext of
            ".md" -> "markdown"
            _ -> "text"

-- | FIXME this function should get cleaned up and merged into stackage-common
getDeps :: (Condition ConfVar -> Bool)
        -> CondTree ConfVar [Dependency] a
        -> Map PackageName VersionRange
getDeps checkCond =
    goTree
  where
    goTree (CondNode _data deps comps) = combineDeps
        $ map (\(Dependency name range) -> Map.singleton name range) deps
       ++ map goComp comps

    goComp (cond, yes, no)
        | checkCond cond = goTree yes
        | otherwise = maybe Map.empty goTree no

combineDeps :: [Map PackageName VersionRange] -> Map PackageName VersionRange
combineDeps =
    Map.unionsWith (\x y -> normalize . simplifyVersionRange $ intersectVersionRanges x y)
  where
    normalize vr =
        case parseDistText $ renderDistText vr of
            Nothing -> vr
            Just vr' -> vr'
