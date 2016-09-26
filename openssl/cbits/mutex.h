#ifndef HSOPENSSL_MUTEX_H_INCLUDED
#define HSOPENSSL_MUTEX_H_INCLUDED

#if defined(MINGW32)
  #include <windows.h>
  typedef HANDLE mutex_t;
#elif defined(PTHREAD)
  #include <pthread.h>
  typedef pthread_mutex_t mutex_t;
#else
  #error "ERROR: This platform is not supported."
#endif

void mutex_init(mutex_t* mutex);
void mutex_destroy(mutex_t* mutex);
void mutex_lock(mutex_t* mutex);
void mutex_unlock(mutex_t* mutex);
unsigned long self();

#endif

