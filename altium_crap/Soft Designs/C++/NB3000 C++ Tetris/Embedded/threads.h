// Thread class
//
// A baseclass for user-defined threads.

#ifndef __THREADS_CPP_H
#define __THREADS_CPP_H  1

#include <pthread.h>

class ThreadBase
{
public:
        // constructor
        ThreadBase();

        // destructor
        virtual ~ThreadBase();

        // create the thread
        virtual int Create(void* arg);

        // cancel the thread
        virtual int Cancel();

        // kill the thread
        virtual int Kill(int sig);

        // join threads
        virtual int Join(void** ret);

protected:
        // entry point for the thread. this must be a static function!
        static void* EntryPoint(void* pthis);

        // setup function
        virtual void Setup();

        // actual thread code
        virtual void* Execute(void* arg) = 0;

        // get the thread argument
        void* Arg() const;

        // set the thread argument
        virtual void Arg(void* arg);

        // get the thread attributes
        virtual const pthread_attr_t* Attr() const;

private:
        pthread_t       _thread;
        void*           _arg;
};

#endif /* !defined(__THREADS_CPP_H) */
