import Distribution.Simple

import Distribution.PackageDescription (PackageDescription (..), Library (..), Executable (..), BuildInfo (..))
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo (..))
import Distribution.Simple.Utils (die, getDirectoryContentsRecursive)

import Control.Monad (void, when)
import Data.List (intercalate)
import Data.Maybe (fromJust, isNothing)
import System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>), (<.>), takeExtension)
import System.Process (system)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks { confHook = myConfHook }

hs_unoidl_path = "../../hs_unoidl/hs_unoidl"

unoExtraLibs = ["stdc++", "uno_cppu", "uno_cppuhelpergcc3", "uno_sal"]

cpputypesInclude builddir = builddir </> "include" </> "cpputypes"
hsunoInclude = "../../hs_uno/src"

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
    let unoLibDirs = [loInstallDir </> "sdk" </> "lib"]
        builddir     = currentDir </> buildDir lbi
        lpd          = localPkgDescr lbi
        exe          = head (executables lpd)
        exebi        = buildInfo exe
        custom_bi    = customFieldsBI exebi
        lo_types     = (lines . fromJust) (lookup "x-lo-sdk-types" custom_bi)
    -- Generate needed types
    makeTypes loInstallDir builddir lo_types
    --
    cxxFilePaths <- findGeneratedCxxFiles
    let exebi' = exebi
          { hsSourceDirs = hsSourceDirs exebi ++ ["gen"]
          , cSources     = cxxFilePaths
          , includeDirs  = includeDirs  exebi ++
              [ cpputypesInclude builddir
              , loInstallDir </> "sdk" </> "include"
              , hsunoInclude
              , "gen"
              ]
          , ccOptions    = ccOptions    exebi ++ loCxxOptions
          , extraLibDirs = extraLibDirs exebi ++ unoLibDirs
          , extraLibs    = extraLibs    exebi ++ unoExtraLibs
          }
    let exe' = exe { buildInfo = exebi' }
    let lpd' = lpd { executables = [exe'], extraSrcFiles = "gen" : (extraSrcFiles lpd) }
    --
    return $ lbi { localPkgDescr = lpd' }

findGeneratedCxxFiles :: IO [FilePath]
findGeneratedCxxFiles = do
  files <- getDirectoryContentsRecursive "gen"
  let cxxFiles = filter ((== ".cpp") . takeExtension) files
  return (map ("gen" </>) cxxFiles)

cxxTypesFlag :: String
cxxTypesFlag = "cpputypes.cppumaker.flag"

hsTypesFlag :: String
hsTypesFlag = "hstypes.hs_unoidl.flag"

makeTypes :: FilePath -> FilePath -> [String] -> IO ()
makeTypes loInstallDir builddir types = do
    let cxxTypesFlagFile = builddir </> cxxTypesFlag
        hsTypesFlagFile = builddir </> hsTypesFlag
    cxxTypesMade <- doesFileExist cxxTypesFlagFile
    hsTypesMade  <- doesFileExist hsTypesFlagFile
    -- make C++ types
    when (not cxxTypesMade) $ do
        let out = cpputypesInclude builddir
            typelist = "-T" ++ intercalate ";" types
            typedb = "$LO_INSTDIR/program/types.rdb"
            offapidb = "$LO_INSTDIR/program/types/offapi.rdb"
        putStrLn "Building required LibreOffice SDK C++ types"
        createDirectoryIfMissing True out
        cppumaker ("-O" ++ out) typelist typedb offapidb
        touch cxxTypesFlagFile
    -- make Haskell types
    putStrLn "NOT building required LibreOffice SDK Haskell types"
    when (not hsTypesMade) $ do
        let out = cpputypesInclude builddir
            typelist = unwords types
            typedb = "$LO_INSTDIR/program/types.rdb"
        putStrLn "Building required LibreOffice SDK Haskell types"
        createDirectoryIfMissing True out
        hs_unoidl loInstallDir typelist
        touch hsTypesFlagFile
    return ()

cppumaker :: String -> String -> String -> String -> IO ()
cppumaker out typelist typedb offapidb = void $ system ("$LO_INSTDIR/sdk/bin/cppumaker " ++ args)
  where args = unwords $ map quote [out, typedb, offapidb]
        quote s = "\"" ++ s ++ "\""

hs_unoidl :: FilePath -> String -> IO ()
hs_unoidl loInstallDir typelist = void $ system (cmd ++ ' ' : args)
  where cmd = "LD_LIBRARY_PATH='"
              ++ loInstallDir ++ "/program' " ++ hs_unoidl_path
        args = unwords (basepath : typepaths)
        basepath = loInstallDir </> "sdk" </> "idl"
        typepaths = map quote
                    $ map (unoTypeToPath basepath)
                    $ words typelist
        quote s = "\"" ++ s ++ "\""

touch :: FilePath -> IO ()
touch path = void $ system ("touch \"" ++ path ++ "\"")

unoTypeToPath :: FilePath -> String -> FilePath
unoTypeToPath basepath t = basepath </> tpath <.> "idl"
  where tpath = replace '.' '/' t -- FIXME split on points and join using </>

replace :: Char -> Char -> String -> String
replace c0 c1 = map aux
  where aux c' | c' == c0 = c1
               | otherwise = c'
