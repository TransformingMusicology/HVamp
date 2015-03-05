import Control.Monad (when)
import Data.List (foldl', intercalate, nub, lookup)
import Data.Maybe (fromJust)
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, localPkgDescr, withPrograms, buildDir)
import Distribution.Simple.Program (ConfiguredProgram (..), lookupProgram, runProgram, simpleProgram)
import Distribution.Simple.Program.Ar
import Distribution.Simple.Setup (ConfigFlags, BuildFlags)
import Distribution.System (OS (..), Arch (..), buildOS, buildArch)
import Distribution.Verbosity (normal, verbose)
import System.Cmd (system)
import System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory, getModificationTime)
import System.Environment (getEnv)
import System.FilePath (splitFileName)
import System.FilePath.Posix ((</>), (<.>), replaceExtension, takeFileName, dropFileName, addExtension, isRelative)
--import System.IO.Unsafe (unsafePerformIO)
--import System.Process (readProcess)

import System.FilePath.Posix (normalise)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks { buildHook = myBuildHook }

myBuildHook pkg_descr local_bld_info user_hooks bld_flags =
  do
    cwd <- getCurrentDirectory
    let lib       = fromJust (library pkg_descr)
        lib_bi    = libBuildInfo lib
        custom_bi = customFieldsBI lib_bi
        cpp_srcs  = (lines . fromJust) (lookup "x-cpp-sources" custom_bi)
        cc_opts   = ccOptions lib_bi
        ld_opts   = ldOptions lib_bi
        inc_dirs  = includeDirs lib_bi
        lib_dirs  = extraLibDirs lib_bi
        libs      = extraLibs lib_bi
        bld_dir   = buildDir local_bld_info
        progs     = withPrograms local_bld_info
        gcc       = fromJust (lookupProgram (simpleProgram "gcc") progs)
        ver       = (pkgVersion . package) pkg_descr
        new_libs  = lib_dirs ++ [if isRelative bld_dir then cwd </> bld_dir else bld_dir]
        new_bi    = lib_bi { extraLibDirs = new_libs }
        new_lib   = lib { libBuildInfo = new_bi }
        new_pkg_descr = pkg_descr { library = Just new_lib }

    -- Compile C/C++ sources - output directory is dist/build/c-src
    putStrLn "Building c++"
    objs <- mapM (compileCxx gcc cc_opts inc_dirs bld_dir) cpp_srcs

    -- Archive object files as a static library - output directory is dist/build
    putStrLn $ "Making static libraries: " ++ (concat objs)
    mapM_ (\o -> putStrLn ("Start object: " ++ (concat o)) >> (createArLibArchive verbose local_bld_info (bld_dir </> ("lib" ++ (replaceExtension (takeFileName (head o))) "a")) o) >> putStrLn ("Done object: " ++ (concat o))) [objs]
    putStrLn "Finished"

    -- Remove C/C++ source code from the hooked build (don't change libs)
    putStrLn "Invoke default build hook"
    putStrLn $ "Made new package description: " ++ (show new_pkg_descr)
    buildHook simpleUserHooks new_pkg_descr local_bld_info user_hooks bld_flags

compileCxx :: ConfiguredProgram  -- ^ C/C++ compiler (gcc)
           -> [String]           -- ^ Compile options from Cabal and wxConfig
           -> [String]           -- ^ Include paths from Cabal and wxConfig
           -> FilePath           -- ^ Base output directory
           -> FilePath           -- ^ Path to source file
           -> IO FilePath        -- ^ Path to generated object code
compileCxx gcc opts incls out_path cxx_src =
  do
    let includes  = map ("-I" ++) incls
        out_path' = normalise out_path
        cxx_src'  = normalise cxx_src
        out_file  = out_path' </> dropFileName cxx_src </> replaceExtension (takeFileName cxx_src) ".o"
        out       = ["-c", cxx_src', "-o", out_file]

    createDirectoryIfMissing True (dropFileName out_file) >> runProgram verbose gcc (includes ++ opts ++ out)
    return out_file
