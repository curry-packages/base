import qualified Installation as I

external_d_C_curryCompiler :: Cover -> ConstStore -> Curry_Prelude.C_String
external_d_C_curryCompiler _ _ = toCurry "kics2"

external_d_C_curryCompilerMajorVersion ::  Cover -> ConstStore -> Curry_Prelude.C_Int
external_d_C_curryCompilerMajorVersion _ _ = toCurry I.majorVersion

external_d_C_curryCompilerMinorVersion ::  Cover -> ConstStore -> Curry_Prelude.C_Int
external_d_C_curryCompilerMinorVersion _ _ = toCurry I.minorVersion

external_d_C_curryRuntime ::  Cover -> ConstStore -> Curry_Prelude.C_String
external_d_C_curryRuntime _ _ = toCurry I.runtime

external_d_C_curryRuntimeMajorVersion ::  Cover -> ConstStore -> Curry_Prelude.C_Int
external_d_C_curryRuntimeMajorVersion _ _ = toCurry I.runtimeMajor

external_d_C_curryRuntimeMinorVersion ::  Cover -> ConstStore -> Curry_Prelude.C_Int
external_d_C_curryRuntimeMinorVersion _ _ = toCurry I.runtimeMinor

external_d_C_installDir ::  Cover -> ConstStore -> Curry_Prelude.C_String
external_d_C_installDir _ _ = toCurry I.installDir
