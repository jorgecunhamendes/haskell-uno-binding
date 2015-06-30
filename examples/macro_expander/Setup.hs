import Distribution.Simple

import Distribution.PackageDescription (PackageDescription (..), Library (..), BuildInfo (..))
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo (..))
import Distribution.Simple.Utils (die)

import Control.Monad (void, when)
import Data.List (intercalate)
import Data.Maybe (fromJust, isNothing)
import System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.Process (system)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks { confHook = myConfHook }

hs_unoidl_path = "../../hs_unoidl/hs_unoidl"

cpputypesInclude builddir = builddir </> "include" </> "cpputypes"

loCxxOptions = words "-DCPPU_ENV=gcc3 -DHAVE_GCC_VISIBILITY_FEATURE -DLINUX -DUNX"

myConfHook (pkg0, pbi) flags = do
    currentDir <- getCurrentDirectory
    mLoInstallDir <- lookupEnv "LO_INSTDIR"
    when (isNothing mLoInstallDir) $
      die "LO_INSTDIR is not set"
    hsunoidlExists <- doesFileExist hs_unoidl_path
    when (not hsunoidlExists) $
      die "hs_unoidl not found"
    -- generate the default configuration
    lbi <- confHook simpleUserHooks (pkg0, pbi) flags
    -- add paths to use the LibreOffice SDK
    let loInstallDir = fromJust mLoInstallDir
        builddir     = currentDir </> buildDir lbi
        lpd          = localPkgDescr lbi
        lib          = fromJust (library lpd)
        libbi        = libBuildInfo lib
        custom_bi    = customFieldsBI libbi
        lo_types     = (lines . fromJust) (lookup "x-lo-sdk-types" custom_bi)
    -- Generate needed types
    putStrLn ("library: " ++ show (library lpd))
    putStrLn ("LO SDK Types: " ++ show (lookup "x-lo-sdk-types" custom_bi))
    makeTypes builddir lo_types
    --
    let libbi' = libbi
          { extraLibDirs = extraLibDirs libbi
          , extraLibs    = extraLibs    libbi
          , ldOptions    = ldOptions    libbi
          , frameworks   = frameworks   libbi
          , includeDirs  = includeDirs  libbi ++
              [ cpputypesInclude builddir
              , loInstallDir </> "sdk" </> "include"
              ]
          , ccOptions    = ccOptions    libbi ++ loCxxOptions
          }
 
    let lib' = lib { libBuildInfo = libbi' }
    let lpd' = lpd { library = Just lib', extraSrcFiles = "gen" : (extraSrcFiles lpd) }
    --
    return $ lbi { localPkgDescr = lpd' }

cxxTypesFlag :: String
cxxTypesFlag = "cpputypes.cppumaker.flag"

hsTypesFlag :: String
hsTypesFlag = "hstypes.hs_unoidl.flag"

makeTypes :: FilePath -> [String] -> IO ()
makeTypes builddir types = do
    let cxxTypesFlagFile = builddir </> cxxTypesFlag
        hsTypesFlagFile = builddir </> hsTypesFlag
    cxxTypesMade <- doesFileExist cxxTypesFlagFile
    hsTypesMade  <- doesFileExist hsTypesFlagFile
    -- make C++ types
    when (not cxxTypesMade) $ do
        let out = cpputypesInclude builddir
            typelist = "-T" ++ intercalate ";" types
            typedb = "$LO_INSTDIR/program/types.rdb"
        putStrLn "Building required LibreOffice SDK C++ types"
        createDirectoryIfMissing True out
        cppumaker ("-O" ++ out) typelist typedb
        touch cxxTypesFlagFile
    -- make Haskell types
    when (not hsTypesMade) $ do
        let out = cpputypesInclude builddir
            typelist = unwords types
            typedb = "$LO_INSTDIR/program/types.rdb"
        putStrLn "Building required LibreOffice SDK Haskell types"
        createDirectoryIfMissing True out
        hs_unoidl typedb typelist
        touch hsTypesFlagFile
    return ()

cppumaker :: String -> String -> String -> IO ()
cppumaker out typelist typedb = void $ system ("$LO_INSTDIR/sdk/bin/cppumaker " ++ args)
  where args = unwords $ map quote [out, typelist, typedb]
        quote s = "\"" ++ s ++ "\""

hs_unoidl :: String -> String -> IO ()
hs_unoidl typelist typedb = void $ system (hs_unoidl_path ++ ' ' : args)
  where args = unwords $ map quote [typelist, typedb]
        quote s = "\"" ++ s ++ "\""

touch :: FilePath -> IO ()
touch path = void $ system ("touch \"" ++ path ++ "\"")
