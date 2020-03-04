/*
** The Dining Philosophers Problem, using semaphores.
**
** From: Ben-Ari, M., "Principles of Concureent Programming", Prentice-Hall,
**       Englewood Cliffs, (1982).
*/

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <semaphore.h>
#include <time.h>

#include "The Dining Philosophers Problem.h"
#include "visualize.h"

static sem_t room;
static sem_t fork[N_PHILOSOPHERS];
static pthread_t tid[N_PHILOSOPHERS];

static void get_realtime_add(struct timespec *tp, time_t nsec)
{
   clock_gettime(CLOCK_REALTIME, tp);
   tp->tv_nsec += nsec;
   tp->tv_sec  += (tp->tv_nsec / 1000000000L);
   tp->tv_nsec = (tp->tv_nsec % 1000000000L);
}

static void rand_delay(int sec)
{
   long long ll;
   struct timespec delay;

   ll = ((long long) sec * 1000000000LL * (long long) rand()) / (long long) RAND_MAX;
   delay.tv_sec = 1 + (time_t) (ll / 1000000000LL);
   delay.tv_nsec = (ll % 1000000000LL);
   nanosleep(&delay, NULL);
}

// Macro to demonstrate the sem_trywait() function
//#define sem_wait(n) {while (sem_trywait(n));}

// Macro to demonstrate the sem_timedwait() function
#define sem_wait(n) sem_wait_and_flash_hungry(n, i)

static int sem_wait_and_flash_hungry(sem_t *sem, int i)
{
   struct timespec t;

   while (1)
   {
      visualize_update_text(i, "Hungry!");
      get_realtime_add(&t, 500000000);
      if (!sem_timedwait(sem,&t)) return 0;

      visualize_update_text(i, "");
      get_realtime_add(&t, 300000000);
      if (!sem_timedwait(sem,&t)) return 0;
   }
}

static void eat(int i)
{
   visualize_update_plate(i, true);
   visualize_update_text(i, "Eating");
   rand_delay(5);
   visualize_update_plate(i, false);
}

static void think(int i)
{
   visualize_update_text(i, "Thinking");
   rand_delay(8);
}

static void * philosopher(void* vi)
{
   int i = (int) vi;

   for (int round = 0; round < N_ROUNDS; round++)
   {
      think(i);

      sem_wait(&room);
      sem_wait(&fork[i]);
      sem_wait(&fork[(i+1) % N_PHILOSOPHERS]);

      eat(i);

      sem_post(&fork[i]);
      sem_post(&fork[(i+1) % N_PHILOSOPHERS]);
      sem_post(&room);
   }

   think(i);

   return NULL;
}

void main (void)
{
   pthread_attr_t attr;
   int i;

   srand(time(NULL));
   visualize_init();

   sem_init(&room, 0, N_PHILOSOPHERS - 1);
   for (i = 0; i < N_PHILOSOPHERS; i++) sem_init(&fork[i], 0, 1);

   // Create round-robin threads
   pthread_attr_init (&attr);
   pthread_attr_setschedpolicy (&attr, SCHED_RR);
   pthread_attr_setinheritsched (&attr, PTHREAD_EXPLICIT_SCHED);

   while (1==1)
   {
      visualize_show_startup_screen();

      for (i = 0; i < N_PHILOSOPHERS; i++)
      {
         pthread_create(&tid[i], &attr, philosopher, (void*) i);
      }

      for (i = 0; i < N_PHILOSOPHERS; i++)
      {
         pthread_join ( tid[i], NULL );
      }

      visualize_show_end_screen();
   }

   for (i = 0; i < N_PHILOSOPHERS; i++) sem_destroy(&fork[i]);
   sem_destroy(&room);

   visualize_close();
}

