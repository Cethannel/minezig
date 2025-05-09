/**
 * \file wasmtime/conf.h
 *
 * \brief Build-time defines for how the C API was built.
 */

#ifndef WASMTIME_CONF_H
#define WASMTIME_CONF_H

// WASMTIME_FEATURE_LIST
#define WASMTIME_FEATURE_PROFILING
#define WASMTIME_FEATURE_WAT
#define WASMTIME_FEATURE_CACHE
#define WASMTIME_FEATURE_PARALLEL_COMPILATION
#define WASMTIME_FEATURE_WASI
#define WASMTIME_FEATURE_LOGGING
/* #undef WASMTIME_FEATURE_DISABLE_LOGGING */
#define WASMTIME_FEATURE_COREDUMP
#define WASMTIME_FEATURE_ADDR2LINE
#define WASMTIME_FEATURE_DEMANGLE
#define WASMTIME_FEATURE_THREADS
#define WASMTIME_FEATURE_GC
#define WASMTIME_FEATURE_GC_DRC
#define WASMTIME_FEATURE_GC_NULL
#define WASMTIME_FEATURE_ASYNC
#define WASMTIME_FEATURE_CRANELIFT
#define WASMTIME_FEATURE_WINCH
#define WASMTIME_FEATURE_DEBUG_BUILTINS
// ... if you add a line above this be sure to change the other locations
// marked WASMTIME_FEATURE_LIST

#if defined(WASMTIME_FEATURE_CRANELIFT) || defined(WASMTIME_FEATURE_WINCH)
#define WASMTIME_FEATURE_COMPILER
#endif

#endif // WASMTIME_CONF_H
