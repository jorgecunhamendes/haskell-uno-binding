import Distribution.Simple

import Control.Monad                      (when, void)
import Data.List                          (foldl', intercalate, nub, lookup)
import Data.Maybe                         (fromJust, isNothing)
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, localPkgDescr, withPrograms, buildDir)
import Distribution.Simple.Program        (ConfiguredProgram (..), Program (..), lookupProgram, runProgram, simpleProgram)
import Distribution.Simple.Program.Db     (ProgramDb, emptyProgramDb, defaultProgramDb)
import Distribution.Simple.Setup          (ConfigFlags, BuildFlags (..))
import Distribution.Simple.Utils          (die)
import Distribution.System                (OS (..), Arch (..), buildOS, buildArch)
import Distribution.Verbosity             (normal, verbose)
import System.Directory                   (createDirectoryIfMissing, doesFileExist, getCurrentDirectory, getModificationTime, findExecutable)
import System.Environment                 (getEnv, lookupEnv)
import System.FilePath.Posix              ((</>), (<.>), replaceExtension, takeFileName, dropFileName, addExtension)
import System.IO.Unsafe                   (unsafePerformIO)
import System.Process                     (readProcess, system)
import System.Exit                        (exitSuccess, exitFailure)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks { confHook = myConfHook }

cpputypesInclude builddir = builddir </> "include" </> "cpputypes"

loCxxOptions = words "-DCPPU_ENV=gcc3 -DHAVE_GCC_VISIBILITY_FEATURE -DLINUX -DUNX"

myConfHook (pkg0, pbi) flags = do
    currentDir <- getCurrentDirectory
    mLoInstallDir <- lookupEnv "LO_INSTDIR"
    when (isNothing mLoInstallDir) $ do
        die "LO_INSTDIR is not set"
    -- generate the default configuration
    lbi <- confHook simpleUserHooks (pkg0, pbi) flags
    -- add paths to use the LibreOffice SDK
    let builddir = currentDir </> buildDir lbi
        loInstallDir = fromJust mLoInstallDir
    -- generate needed types
    makeTypes builddir
    -- make new local build info
    let lpd        = localPkgDescr lbi
        lib        = fromJust (library lpd)
        libbi      = libBuildInfo lib
        libbi' = libbi
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
 
        lib' = lib { libBuildInfo = libbi' }
        lpd' = lpd { library = Just lib' }
    return $ lbi { localPkgDescr = lpd' }

cxxTypesFlag :: String
cxxTypesFlag = "cpputypes.cppumaker.flag"

makeTypes :: FilePath -> IO ()
makeTypes builddir = do
    let cxxTypesFlagFile = builddir </> cxxTypesFlag
    cxxTypesMade <- doesFileExist cxxTypesFlagFile
    when (not cxxTypesMade) $ do
        let out = cpputypesInclude builddir
            typelist = "-Tcom.sun.star.beans.Introspection;com.sun.star.beans.theIntrospection;com.sun.star.bridge.BridgeFactory;com.sun.star.bridge.UnoUrlResolver;com.sun.star.connection.Acceptor;com.sun.star.connection.Connector;com.sun.star.io.Pipe;com.sun.star.io.TextInputStream;com.sun.star.io.TextOutputStream;com.sun.star.java.JavaVirtualMachine;com.sun.star.lang.DisposedException;com.sun.star.lang.EventObject;com.sun.star.lang.XMain;com.sun.star.lang.XMultiComponentFactory;com.sun.star.lang.XMultiServiceFactory;com.sun.star.lang.XSingleComponentFactory;com.sun.star.lang.XSingleServiceFactory;com.sun.star.lang.XTypeProvider;com.sun.star.loader.Java;com.sun.star.loader.SharedLibrary;com.sun.star.reflection.ProxyFactory;com.sun.star.registry.ImplementationRegistration;com.sun.star.registry.SimpleRegistry;com.sun.star.registry.XRegistryKey;com.sun.star.script.Converter;com.sun.star.script.Invocation;com.sun.star.security.AccessController;com.sun.star.security.Policy;com.sun.star.uno.DeploymentException;com.sun.star.uno.Exception;com.sun.star.uno.NamingService;com.sun.star.uno.RuntimeException;com.sun.star.uno.XAggregation;com.sun.star.uno.XComponentContext;com.sun.star.uno.XCurrentContext;com.sun.star.uno.XInterface;com.sun.star.uno.XWeak;com.sun.star.uri.ExternalUriReferenceTranslator;com.sun.star.uri.UriReferenceFactory;com.sun.star.uri.VndSunStarPkgUrlReferenceFactory;com.sun.star.util.theMacroExpander"
            typedb = "$LO_INSTDIR/program/types.rdb"
        putStrLn "Building required LibreOffice SDK types"
        createDirectoryIfMissing True out
        cppumaker ("-O" ++ out) typelist typedb
        touch cxxTypesFlagFile
    return ()

cppumaker :: String -> String -> String -> IO ()
cppumaker out typelist typedb = void $ system ("$LO_INSTDIR/sdk/bin/cppumaker " ++ args)
  where args = unwords $ map quote [out, typelist, typedb]
        quote s = "\"" ++ s ++ "\""

touch :: FilePath -> IO ()
touch path = void $ system ("touch \"" ++ path ++ "\"")
