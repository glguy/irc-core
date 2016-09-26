#include "mutex.h"

void mutex_init(mutex_t* mutex)
{
  pthread_mutex_init(mutex, NULL);
}

void mutex_destroy(mutex_t* mutex)
{
  pthread_mutex_destroy(mutex);
}

void mutex_lock(mutex_t* mutex)
{
  pthread_mutex_lock(mutex);
}

void mutex_unlock(mutex_t* mutex)
{
  pthread_mutex_unlock(mutex);
}

unsigned long self()
{
  return (unsigned long)pthread_self();
}
