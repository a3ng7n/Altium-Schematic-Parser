// Logger thread
//

#ifndef __LOGGER_CPP_H
#define __LOGGER_CPP_H  1

#include "threads.h"

class LoggerThread : public ThreadBase
{
public:
    LoggerThread();

protected:
    virtual void Setup();
    virtual void* Execute(void* arg);

    virtual const pthread_attr_t* Attr() const
        {
            return &_attr;
        }

private:
    pthread_attr_t  _attr;
    char            _stack[12 * PTHREAD_STACK_MIN];
};

#endif /* !defined(__LOGGER_CPP_H) */

