/* neomacs_log.h — NEOMACS_LOG environment-variable-controlled logging
   Copyright (C) 2026 Free Software Foundation, Inc.

   Similar to Rust's env_logger / RUST_LOG, this provides per-module
   log level filtering controlled by the NEOMACS_LOG environment variable.

   Syntax:
     NEOMACS_LOG=debug                          # global level
     NEOMACS_LOG=warn,render=debug,input=trace  # per-module overrides
     NEOMACS_LOG=warn,neomacs*=debug            # glob patterns

   Levels: trace, debug, info, warn, error, fatal, off

   Usage in C files:
     #define NLOG_MODULE "render"
     #include "neomacs_log.h"

     nlog_debug ("frame %d ready", frame_id);
     nlog_trace ("glyph count: %d", n);
     nlog_warn ("fallback to cairo");
*/

#ifndef NEOMACS_LOG_H
#define NEOMACS_LOG_H

#include <stdbool.h>

/* Log levels, ordered by severity.  */
enum nlog_level
{
  NLOG_TRACE = 0,
  NLOG_DEBUG = 1,
  NLOG_INFO  = 2,
  NLOG_WARN  = 3,
  NLOG_ERROR = 4,
  NLOG_FATAL = 5,
  NLOG_OFF   = 6,
};

/* Initialize the logging system from NEOMACS_LOG env var.
   Called automatically on first log call; safe to call multiple times.  */
extern void nlog_init (void);

/* Check if a message at LEVEL for MODULE would be logged.
   Use this to guard expensive argument computation.  */
extern bool nlog_enabled (enum nlog_level level, const char *module);

/* Core logging function — prefer the macros below.  */
extern void nlog_log (enum nlog_level level, const char *module,
                      const char *file, int line,
                      const char *fmt, ...)
  __attribute__ ((format (printf, 5, 6)));

/* Per-file module name.  Define before including this header:
     #define NLOG_MODULE "render"
   Falls back to "unknown" if not defined.  */
#ifndef NLOG_MODULE
#define NLOG_MODULE "unknown"
#endif

/* Convenience macros — these are the primary API.  */
#define nlog_trace(...) \
  nlog_log (NLOG_TRACE, NLOG_MODULE, __FILE__, __LINE__, __VA_ARGS__)

#define nlog_debug(...) \
  nlog_log (NLOG_DEBUG, NLOG_MODULE, __FILE__, __LINE__, __VA_ARGS__)

#define nlog_info(...) \
  nlog_log (NLOG_INFO, NLOG_MODULE, __FILE__, __LINE__, __VA_ARGS__)

#define nlog_warn(...) \
  nlog_log (NLOG_WARN, NLOG_MODULE, __FILE__, __LINE__, __VA_ARGS__)

#define nlog_error(...) \
  nlog_log (NLOG_ERROR, NLOG_MODULE, __FILE__, __LINE__, __VA_ARGS__)

#define nlog_fatal(...) \
  nlog_log (NLOG_FATAL, NLOG_MODULE, __FILE__, __LINE__, __VA_ARGS__)

#endif /* NEOMACS_LOG_H */
