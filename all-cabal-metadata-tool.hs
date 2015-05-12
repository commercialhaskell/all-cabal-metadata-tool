{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import           ClassyPrelude.Conduit
import qualified Codec.Archive.Tar                     as Tar
import qualified Data.ByteString.Lazy                  as L
import           Data.Semigroup                        (Max (Max))
import           Data.Version                          (Version)
import           Distribution.Compat.ReadP             (readP_to_S)
import           Distribution.Package                  (PackageName)
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse
import           Distribution.Text                     (parse)
import           System.Directory
import           System.FilePath
import           System.IO                             (IOMode (ReadMode),
                                                        openBinaryFile)

sourceAllCabalFiles :: MonadResource m => Producer m (PackageName, Version, ParseResult GenericPackageDescription)
sourceAllCabalFiles = do
    cabal <- liftIO $ getAppUserDataDirectory "cabal"
    let tarball = cabal </> "packages" </> "hackage.haskell.org" </> "00-index.tar"
    bracketP (openBinaryFile tarball ReadMode) hClose $ \h -> do
        lbs <- liftIO $ L.hGetContents h
        loop $ Tar.read lbs
  where
    loop Tar.Done = return ()
    loop (Tar.Fail e) = throwM e
    loop (Tar.Next e es) = go e >> loop es

    go e =
        case (toPkgVer $ Tar.entryPath e, Tar.entryContent e) of
            (Just (name, version), Tar.NormalFile lbs _) ->
                yield (name, version, parsePackageDescription $ unpack $ decodeUtf8 lbs)
            _ -> return ()

    toPkgVer s0 = do
        (name', '/':s1) <- Just $ break (== '/') s0
        (version', '/':s2) <- Just $ break (== '/') s1
        guard $ s2 == (name' ++ ".cabal")
        name <- parse' name'
        version <- parse' version'
        Just (name, version)

    parse' s =
        case map fst $ filter (null . snd) $ readP_to_S parse s of
            [x] -> Just x
            _ -> Nothing

main :: IO ()
main = do
    m <- runResourceT $ sourceAllCabalFiles $$ foldMapC (\(name, version, _) -> singletonMap name (Max version, asSet $ singletonSet version))
    print $ length $ asMap m
