// Tetris game thread
//

#ifndef __MODEL_CPP_H
#define __MODEL_CPP_H  1

#include "threads.h"

class TetrisThread : public ThreadBase
{
public:
    TetrisThread();

protected:
    virtual void Setup();
    virtual void* Execute(void* arg);

    virtual const pthread_attr_t* Attr() const
        {
            return &_attr;
        }

private:
    pthread_attr_t  _attr;
    char            _stack[24 * PTHREAD_STACK_MIN];

    bool        _initialized;
};

#endif /* !defined(__MODEL_CPP_H) */

