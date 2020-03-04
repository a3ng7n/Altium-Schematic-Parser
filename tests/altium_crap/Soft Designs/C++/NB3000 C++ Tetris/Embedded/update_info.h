// Update info thread
//

#ifndef __UPDATE_INFO_CPP_H
#define __UPDATE_INFO_CPP_H  1

#include "threads.h"

class UpdateInfoThread : public ThreadBase
{
public:
    UpdateInfoThread();

protected:
    virtual void Setup();
    virtual void* Execute(void* arg);

    virtual const pthread_attr_t* Attr() const;

private:
    pthread_attr_t  _attr;
    char            _stack[12 * PTHREAD_STACK_MIN];
};

#endif /* !defined(__UPDATE_INFO_CPP_H) */

