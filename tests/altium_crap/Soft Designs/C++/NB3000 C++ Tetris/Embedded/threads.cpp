// C++ threads

#include <stdio.h>

#include "threads.h"

ThreadBase::ThreadBase()
{
    // nothing to construct here
}

ThreadBase::~ThreadBase()
{
    // nothing to destruct here
}

int ThreadBase::Create(void * arg)
{
    // store user data
    Arg(arg);

    // setup thread code
    Setup();

    // create the thread
    return pthread_create(& _thread, Attr(), EntryPoint, this);
}

int ThreadBase::Cancel()
{
    return pthread_cancel(_thread);
}

int ThreadBase::Kill(int sig)
{
    return pthread_kill(_thread, sig);
}

int ThreadBase::Join(void ** ret)
{
    return pthread_join(_thread, ret);
}

void * ThreadBase::EntryPoint(void * pthis)
{
    ThreadBase * pt = (ThreadBase *) pthis;

    return pt->Execute(pt->Arg());
}

void ThreadBase::Setup()
{
    // basic thread setup function that should be re-implemented in a derived
    // class.
}

void * ThreadBase::Arg() const
{
    return _arg;
}

void ThreadBase::Arg(void * arg)
{
    _arg = arg;
}

const pthread_attr_t * ThreadBase::Attr() const
{
    return NULL;
}
