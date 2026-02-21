/* System thread definitions
Copyright (C) 2012-2026 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

#include <config.h>
#include <setjmp.h>
#include <stdio.h>
#include <string.h>
#include "lisp.h"


#ifdef HAVE_PTHREAD_SET_NAME_NP
#include <pthread_np.h>
#endif

#ifndef THREADS_ENABLED

void
sys_mutex_init (sys_mutex_t *m)
{
  *m = 0;
}

void
sys_mutex_lock (sys_mutex_t *m)
{
}

void
sys_mutex_unlock (sys_mutex_t *m)
{
}

void
sys_cond_init (sys_cond_t *c)
{
  *c = 0;
}

void
sys_cond_wait (sys_cond_t *c, sys_mutex_t *m)
{
}

void
sys_cond_signal (sys_cond_t *c)
{
}

void
sys_cond_broadcast (sys_cond_t *c)
{
}

void
sys_cond_destroy (sys_cond_t *c)
{
}

sys_thread_t
sys_thread_self (void)
{
  return 0;
}

bool
sys_thread_equal (sys_thread_t t, sys_thread_t u)
{
  return t == u;
}
void
sys_thread_set_name (const char *name)
{
}

bool
sys_thread_create (sys_thread_t *t, thread_creation_function *func, void *datum)
{
  return false;
}

void
sys_thread_yield (void)
{
}

#elif defined (HAVE_PTHREAD)

#include <sched.h>

void
sys_mutex_init (sys_mutex_t *sys_mutex)
{
  pthread_mutex_t *mutex = SYSTHREAD_ALIGN_PTR (pthread_mutex_t, sys_mutex);
  pthread_mutexattr_t *attr_ptr;
#ifdef ENABLE_CHECKING
  pthread_mutexattr_t attr;
  {
    int error = pthread_mutexattr_init (&attr);
    eassert (error == 0);
    error = pthread_mutexattr_settype (&attr, PTHREAD_MUTEX_ERRORCHECK);
    eassert (error == 0);
  }
  attr_ptr = &attr;
#else
  attr_ptr = NULL;
#endif
  int error = pthread_mutex_init (mutex, attr_ptr);
  /* We could get ENOMEM.  Can't do anything except aborting.  */
  if (error != 0)
    {
      fprintf (stderr, "\npthread_mutex_init failed: %s\n", strerror (error));
      emacs_abort ();
    }
#ifdef ENABLE_CHECKING
  error = pthread_mutexattr_destroy (&attr);
  eassert (error == 0);
#endif
}

void
sys_mutex_lock (sys_mutex_t *sys_mutex)
{
  pthread_mutex_t *mutex = SYSTHREAD_ALIGN_PTR (pthread_mutex_t, sys_mutex);
  int error = pthread_mutex_lock (mutex);
  eassert (error == 0);
}

void
sys_mutex_unlock (sys_mutex_t *sys_mutex)
{
  pthread_mutex_t *mutex = SYSTHREAD_ALIGN_PTR (pthread_mutex_t, sys_mutex);
  int error = pthread_mutex_unlock (mutex);
  eassert (error == 0);
}

void
sys_cond_init (sys_cond_t *sys_cond)
{
  pthread_cond_t *cond = SYSTHREAD_ALIGN_PTR (pthread_cond_t, sys_cond);
  int error = pthread_cond_init (cond, NULL);
  /* We could get ENOMEM.  Can't do anything except aborting.  */
  if (error != 0)
    {
      fprintf (stderr, "\npthread_cond_init failed: %s\n", strerror (error));
      emacs_abort ();
    }
}

void
sys_cond_wait (sys_cond_t *sys_cond, sys_mutex_t *sys_mutex)
{
  pthread_cond_t *cond = SYSTHREAD_ALIGN_PTR (pthread_cond_t, sys_cond);
  pthread_mutex_t *mutex = SYSTHREAD_ALIGN_PTR (pthread_mutex_t, sys_mutex);
  int error = pthread_cond_wait (cond, mutex);
  eassert (error == 0);
}

void
sys_cond_signal (sys_cond_t *sys_cond)
{
  pthread_cond_t *cond = SYSTHREAD_ALIGN_PTR (pthread_cond_t, sys_cond);
  int error = pthread_cond_signal (cond);
  eassert (error == 0);
}

void
sys_cond_broadcast (sys_cond_t *sys_cond)
{
  pthread_cond_t *cond = SYSTHREAD_ALIGN_PTR (pthread_cond_t, sys_cond);
  int error = pthread_cond_broadcast (cond);
  eassert (error == 0);
}

void
sys_cond_destroy (sys_cond_t *sys_cond)
{
  pthread_cond_t *cond = SYSTHREAD_ALIGN_PTR (pthread_cond_t, sys_cond);
  int error = pthread_cond_destroy (cond);
  eassert (error == 0);
}

sys_thread_t
sys_thread_self (void)
{
  return pthread_self ();
}

bool
sys_thread_equal (sys_thread_t t, sys_thread_t u)
{
  return pthread_equal (t, u);
}

void
sys_thread_set_name (const char *name)
{
#ifdef HAVE_PTHREAD_SETNAME_NP
  /* We need to truncate here otherwise pthread_setname_np
     fails to set the name.  TASK_COMM_LEN is what the length
     is called in the Linux kernel headers (Bug#38632).  */
#define TASK_COMM_LEN 16
  char p_name[TASK_COMM_LEN];
  strncpy (p_name, name, TASK_COMM_LEN - 1);
  p_name[TASK_COMM_LEN - 1] = '\0';
# ifdef HAVE_PTHREAD_SETNAME_NP_1ARG
  pthread_setname_np (p_name);
# elif defined HAVE_PTHREAD_SETNAME_NP_3ARG
  pthread_setname_np (pthread_self (), "%s", p_name);
# else
  pthread_setname_np (pthread_self (), p_name);
# endif
#elif HAVE_PTHREAD_SET_NAME_NP
  /* The name will automatically be truncated if it exceeds a
     system-specific length.  */
  pthread_set_name_np (pthread_self (), name);
#endif
}

bool
sys_thread_create (sys_thread_t *thread_ptr, thread_creation_function *func,
                   void *arg)
{
  pthread_attr_t attr;
  bool result = false;

  if (pthread_attr_init (&attr))
    return false;

  /* Avoid crash on macOS with deeply nested GC (Bug#30364).  */
  size_t stack_size;
  size_t required_stack_size = sizeof (void *) * 1024 * 1024;
  if (pthread_attr_getstacksize (&attr, &stack_size) == 0
      && stack_size < required_stack_size)
    {
      if (pthread_attr_setstacksize (&attr, required_stack_size) != 0)
        goto out;
    }

  if (!pthread_attr_setdetachstate (&attr, PTHREAD_CREATE_DETACHED))
    result = pthread_create (thread_ptr, &attr, func, arg) == 0;

 out: ;
  int error = pthread_attr_destroy (&attr);
  eassert (error == 0);

  return result;
}

void
sys_thread_yield (void)
{
  sched_yield ();
}

#else

#error port me

#endif
