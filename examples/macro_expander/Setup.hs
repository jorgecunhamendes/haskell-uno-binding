import Distribution.Simple

import Distribution.PackageDescription (PackageDescription (..), Library (..),
           Executable (..), BuildInfo (..))
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo (..))
import Distribution.Simple.Utils (die, getDirectoryContentsRecursive)

import Control.Monad (void, when, unless)
import Data.List (intercalate)
import Data.Maybe (fromJust, isNothing)
import System.Directory (createDirectoryIfMissing, doesFileExist,
           getCurrentDirectory, getModificationTime)
import System.Environment (lookupEnv)
import System.FilePath ((</>), (<.>), takeExtension)
import System.Process (system)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks { confHook = myConfHook }

typeDbs :: [FilePath]
typeDbs =
  [ "$LO_INSTDIR/program/types.rdb"
  ]

hsUnoidlPath :: String
hsUnoidlPath = "../../hs_unoidl/hs_unoidl"

unoExtraLibs :: [String]
unoExtraLibs = ["stdc++", "uno_cppu", "uno_cppuhelpergcc3", "uno_sal"]

cpputypesInclude :: FilePath -> FilePath
cpputypesInclude builddir = builddir </> "include" </> "cpputypes"

hsunoInclude :: String
hsunoInclude = "../../hs_uno/src"

loCxxOptions :: [String]
loCxxOptions = words "-DCPPU_ENV=gcc3 -DHAVE_GCC_VISIBILITY_FEATURE -DLINUX -DUNX"

myConfHook (pkg0, pbi) flags = do
    currentDir <- getCurrentDirectory
    mLoInstallDir <- lookupEnv "LO_INSTDIR"
    when (isNothing mLoInstallDir) $
      die "LO_INSTDIR is not set"
    hsunoidlExists <- doesFileExist hsUnoidlPath
    unless hsunoidlExists $
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
        cabalFile    = unPackageName (pkgName $ package lpd) <.> "cabal"
    -- Generate needed types
    makeTypes cabalFile loInstallDir builddir typeDbs lo_types
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
    let lpd' = lpd { executables = [exe'], extraSrcFiles = "gen" : extraSrcFiles lpd }
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

makeTypes :: FilePath -> FilePath -> FilePath -> [FilePath] -> [String] -> IO ()
makeTypes cabalFile loInstallDir builddir typedbs types = do
    let cxxTypesFlagFile = builddir </> cxxTypesFlag
        hsTypesFlagFile = builddir </> hsTypesFlag
        out = cpputypesInclude builddir
    cxxTypesMade <- cxxTypesFlagFile `isNewerThan` cabalFile
    hsTypesMade  <- hsTypesFlagFile  `isNewerThan` cabalFile
    -- make C++ types
    unless cxxTypesMade $ do
        let typelist = "-T" ++ intercalate ";" types
        putStrLn "Building required LibreOffice SDK C++ types"
        createDirectoryIfMissing True out
        cppumaker ("-O" ++ out) typelist typedbs
        touch cxxTypesFlagFile
    -- make Haskell types
    unless hsTypesMade $ do
        let typelist = "-T" ++ intercalate ":" types
        putStrLn "Building required LibreOffice SDK Haskell types"
        createDirectoryIfMissing True out
        hs_unoidl loInstallDir typelist typedbs
        touch hsTypesFlagFile
    return ()

cppumaker :: String -> String -> [FilePath] -> IO ()
cppumaker out typelist typedbs = void $ system ("$LO_INSTDIR/sdk/bin/cppumaker " ++ args)
  where args = unwords $ map quote (out : typelist : typedbs)
        quote s = "\"" ++ s ++ "\""

hs_unoidl :: FilePath -> String -> [String] -> IO ()
hs_unoidl loInstallDir typelist typedbs = void $ system (cmd ++ ' ' : args)
  where cmd = "LD_LIBRARY_PATH='"
              ++ loInstallDir ++ "/program' " ++ hsUnoidlPath
        args = unwords $ map quote (typelist : typedbs)
        quote s = "\"" ++ s ++ "\""

touch :: FilePath -> IO ()
touch path = void $ system ("touch \"" ++ path ++ "\"")

isNewerThan :: FilePath -> FilePath -> IO Bool
isNewerThan f1 f2 = do
  exists1 <- doesFileExist f1
  exists2 <- doesFileExist f2
  if exists1 && exists2
    then do
      t1 <- getModificationTime f1
      t2 <- getModificationTime f2
      return (t1 > t2)
    else return False
