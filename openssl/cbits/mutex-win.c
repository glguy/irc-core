#include "mutex.h"

void mutex_init(mutex_t* mutex)
{
  *mutex = CreateMutex(NULL, FALSE, NULL);
}
void mutex_destroy(mutex_t* mutex)
{
  CloseHandle(*mutex);
}
void mutex_lock(mutex_t* mutex)
{
  WaitForSingleObject(mutex, INFINITE);
}
void mutex_unlock(mutex_t* mutex)
{
  ReleaseMutex(mutex);
}
unsigned long self()
{
  return GetCurrentThreadId();
}
