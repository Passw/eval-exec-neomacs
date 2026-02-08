/* neomacs_log.c — NEOMACS_LOG environment-variable-controlled logging
   Copyright (C) 2026 Free Software Foundation, Inc.

   Parses NEOMACS_LOG env var with RUST_LOG-compatible syntax:
     NEOMACS_LOG=warn,render=debug,input=trace

   See neomacs_log.h for full documentation.  */

#include <config.h>

#ifdef HAVE_NEOMACS

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <stdarg.h>
#include <stdbool.h>
#include <time.h>
#include <unistd.h>

#include "neomacs_log.h"

/* Maximum number of per-module filter rules.  */
#define NLOG_MAX_FILTERS 64

/* Maximum length of a module pattern string.  */
#define NLOG_MAX_PATTERN 64

/* A single module-level filter rule.  */
struct nlog_filter
{
  char pattern[NLOG_MAX_PATTERN];
  enum nlog_level level;
};

/* Global logging state.  */
static enum nlog_level nlog_default_level = NLOG_WARN;
static struct nlog_filter nlog_filters[NLOG_MAX_FILTERS];
static int nlog_filter_count = 0;
static bool nlog_initialized = false;
static bool nlog_use_color = false;

/* Level name strings.  */
static const char *const nlog_level_names[] = {
  "TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL"
};

/* ANSI color codes for each level.  */
static const char *const nlog_level_colors[] = {
  "\033[94m",  /* TRACE — bright blue */
  "\033[36m",  /* DEBUG — cyan */
  "\033[32m",  /* INFO  — green */
  "\033[33m",  /* WARN  — yellow */
  "\033[31m",  /* ERROR — red */
  "\033[35m",  /* FATAL — magenta */
};

/* Parse a level name string (case-insensitive).
   Returns -1 if not recognized.  */
static int
nlog_parse_level (const char *s, int len)
{
  if (len < 0)
    len = (int) strlen (s);

  /* Skip leading/trailing whitespace.  */
  while (len > 0 && s[0] == ' ')
    { s++; len--; }
  while (len > 0 && s[len - 1] == ' ')
    len--;

  if (len == 0)
    return -1;

  if (len == 5 && !strncasecmp (s, "trace", 5)) return NLOG_TRACE;
  if (len == 5 && !strncasecmp (s, "debug", 5)) return NLOG_DEBUG;
  if (len == 4 && !strncasecmp (s, "info",  4)) return NLOG_INFO;
  if (len == 4 && !strncasecmp (s, "warn",  4)) return NLOG_WARN;
  if (len == 7 && !strncasecmp (s, "warning", 7)) return NLOG_WARN;
  if (len == 5 && !strncasecmp (s, "error", 5)) return NLOG_ERROR;
  if (len == 5 && !strncasecmp (s, "fatal", 5)) return NLOG_FATAL;
  if (len == 3 && !strncasecmp (s, "off",   3)) return NLOG_OFF;

  return -1;
}

/* Simple glob matching: supports '*' as a trailing wildcard
   and '*' anywhere as a single-segment wildcard.
   Examples: "render" matches "render", "neo*" matches "neomacsterm".  */
static bool
nlog_pattern_match (const char *pattern, const char *name)
{
  while (*pattern && *name)
    {
      if (*pattern == '*')
        {
          /* '*' at end of pattern matches everything remaining.  */
          if (pattern[1] == '\0')
            return true;
          /* '*' in middle: try matching rest from every position.  */
          for (const char *p = name; *p; p++)
            if (nlog_pattern_match (pattern + 1, p))
              return true;
          return false;
        }
      if (*pattern != *name)
        return false;
      pattern++;
      name++;
    }
  /* Handle trailing '*' in pattern.  */
  while (*pattern == '*')
    pattern++;
  return *pattern == '\0' && *name == '\0';
}

/* Parse the NEOMACS_LOG environment variable.
   Format: "level,module=level,module=level,..."
   A bare level sets the default; module=level sets a per-module override.  */
static void
nlog_parse_env (const char *env)
{
  char buf[2048];
  strncpy (buf, env, sizeof (buf) - 1);
  buf[sizeof (buf) - 1] = '\0';

  char *saveptr = NULL;
  char *token = strtok_r (buf, ",", &saveptr);

  while (token)
    {
      char *eq = strchr (token, '=');
      if (eq)
        {
          /* module=level */
          *eq = '\0';
          int lvl = nlog_parse_level (eq + 1, -1);
          if (lvl >= 0 && nlog_filter_count < NLOG_MAX_FILTERS)
            {
              /* Trim whitespace from pattern.  */
              const char *pat = token;
              while (*pat == ' ') pat++;
              strncpy (nlog_filters[nlog_filter_count].pattern,
                       pat, NLOG_MAX_PATTERN - 1);
              nlog_filters[nlog_filter_count].pattern[NLOG_MAX_PATTERN - 1] = '\0';
              /* Trim trailing whitespace.  */
              int plen = (int) strlen (nlog_filters[nlog_filter_count].pattern);
              while (plen > 0 && nlog_filters[nlog_filter_count].pattern[plen - 1] == ' ')
                nlog_filters[nlog_filter_count].pattern[--plen] = '\0';
              nlog_filters[nlog_filter_count].level = (enum nlog_level) lvl;
              nlog_filter_count++;
            }
        }
      else
        {
          /* Bare level = default.  */
          int lvl = nlog_parse_level (token, -1);
          if (lvl >= 0)
            nlog_default_level = (enum nlog_level) lvl;
        }
      token = strtok_r (NULL, ",", &saveptr);
    }
}

void
nlog_init (void)
{
  if (nlog_initialized)
    return;
  nlog_initialized = true;

  /* Detect color support: use color if stderr is a terminal,
     unless NEOMACS_LOG_COLOR or NO_COLOR overrides.  */
  const char *color_env = getenv ("NEOMACS_LOG_COLOR");
  if (color_env)
    nlog_use_color = (color_env[0] == '1' || color_env[0] == 'y'
                      || color_env[0] == 'Y');
  else if (getenv ("NO_COLOR"))
    nlog_use_color = false;
  else
    nlog_use_color = isatty (STDERR_FILENO);

  const char *env = getenv ("NEOMACS_LOG");
  if (env)
    nlog_parse_env (env);
}

/* Get the effective log level for a module.
   Last matching filter wins.  */
static enum nlog_level
nlog_effective_level (const char *module)
{
  enum nlog_level level = nlog_default_level;

  for (int i = 0; i < nlog_filter_count; i++)
    {
      if (nlog_pattern_match (nlog_filters[i].pattern, module))
        level = nlog_filters[i].level;
    }

  return level;
}

bool
nlog_enabled (enum nlog_level level, const char *module)
{
  if (!nlog_initialized)
    nlog_init ();
  return level >= nlog_effective_level (module);
}

void
nlog_log (enum nlog_level level, const char *module,
          const char *file, int line,
          const char *fmt, ...)
{
  if (!nlog_initialized)
    nlog_init ();

  if (level < nlog_effective_level (module))
    return;

  /* Timestamp.  */
  time_t t = time (NULL);
  struct tm lt;
  localtime_r (&t, &lt);
  char timebuf[16];
  strftime (timebuf, sizeof (timebuf), "%H:%M:%S", &lt);

  /* Strip directory prefix from filename for readability.  */
  const char *basename = strrchr (file, '/');
  basename = basename ? basename + 1 : file;

  /* Format: "HH:MM:SS LEVEL file:line [module] message\n" */
  if (nlog_use_color)
    fprintf (stderr, "%s %s%-5s\033[0m \033[90m%s:%d\033[0m [%s] ",
             timebuf, nlog_level_colors[level],
             nlog_level_names[level], basename, line, module);
  else
    fprintf (stderr, "%s %-5s %s:%d [%s] ",
             timebuf, nlog_level_names[level], basename, line, module);

  va_list ap;
  va_start (ap, fmt);
  vfprintf (stderr, fmt, ap);
  va_end (ap);

  fputc ('\n', stderr);
  fflush (stderr);
}

#endif /* HAVE_NEOMACS */
