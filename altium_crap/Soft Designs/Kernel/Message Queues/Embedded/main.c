/*****************************************************************************
|*  SwPlatform Priority Example
|*
|* Devices:
|*  - Terminal
|*
|* Services used:
|*  - Multithreading
|*  - Message Queues
|*
|* Description:
|*  This example shows some different pair of threads sending/receiving on
|*  different message queues. Each pair of threads runs at a different priority.
|*  The sender thread sends first three messages with different priority to its
|*  message queue and then the receiver thread proceeds to read these message
|*  according to the priority of the message.
\*****************************************************************************/

#include <stdint.h>
#include <stdlib.h>
#include <limits.h>
#include <pthread.h>
#include <stdio.h>
#include <mqueue.h>
#include <fcntl.h>
#include <string.h>

#define STACKSIZE           3000
#define MSGSIZE             16
#define MSGNO               3
#define MQUEUENO            4
#define NAMESIZE            20

typedef struct {
    int ThreadID;
    mqd_t mqID;
}mq_info;

static pthread_mutex_t print_mutex = PTHREAD_MUTEX_INITIALIZER;
inline void print_lock(void)	{ pthread_mutex_lock(&print_mutex); }
inline void print_unlock(void)	{ pthread_mutex_unlock(&print_mutex);	}

int* send(void *info)
{
    int i;
    const char *s_msg_ptr[] = {"msg test 1", "msg test 2", "msg test 3"};
    mq_info send_info;
    send_info.ThreadID = ((mq_info *)info)->ThreadID;
    send_info.mqID = ((mq_info *)info)->mqID;
    print_lock();
    printf("Enter into send[%d], mq = %d \n", send_info.ThreadID, (int)send_info.mqID);
    print_unlock();
    for (i = 0; i < MSGNO; i++ ) {
        if ( -1 == mq_send(send_info.mqID, s_msg_ptr[i], MSGSIZE, i)) {
            print_lock();
            printf("mq_send doesn't return success \n");
            print_unlock();
            pthread_exit((void *) 1);
        }
        print_lock();
        printf("[%d] send '%s' in thread send[%d]. \n", i+1, s_msg_ptr[i], send_info.ThreadID);
        print_unlock();
    }
    pthread_exit((void *)0);
    return NULL;
}
int* receive(void * info)
{
    int i;
    char r_msg_ptr[MSGNO][MSGSIZE];
    mq_info recv_info;
    recv_info.ThreadID = ((mq_info *)info)->ThreadID;
    recv_info.mqID = ((mq_info *)info)->mqID;
    print_lock();
    printf("Enter into receive[%d], mq = %d \n",recv_info.ThreadID, (int)recv_info.mqID);
    print_unlock();
    for (i = 0; i< MSGNO; i++) {
        if ( -1 == mq_receive(recv_info.mqID, r_msg_ptr[i], MSGSIZE, NULL) ) {
            print_lock();
            printf("mq_receive doesn't return success \n");
            print_unlock();
            pthread_exit((void *)0);
        }
        print_lock();
        printf("[%d] receive '%s' in thread receive recv[%d]. \n", i+1, r_msg_ptr[i], recv_info.ThreadID);
        print_unlock();
    }

    pthread_exit((void *)0);
    return NULL;
}

int main(void)
{
    const char * MQ_NAME[MQUEUENO] = {"/msg1", "/msg2", "/msg3", "/msg4"};
    mqd_t mq[MQUEUENO];
    struct mq_attr mqstat;
    int oflag = O_CREAT | O_NONBLOCK | O_RDWR;
    int i;
    pthread_t sed[MQUEUENO], rev[MQUEUENO];
    mq_info info[MQUEUENO];
    pthread_attr_t attr;
    struct sched_param schedparam;

    printf("-------------------------------------\n");

    // create threads attributes
    pthread_attr_init( &attr );
    pthread_attr_setstacksize( &attr,STACKSIZE);
    pthread_attr_setinheritsched( &attr, PTHREAD_EXPLICIT_SCHED );

    memset(&mqstat, 0, sizeof(mqstat));
    mqstat.mq_maxmsg = MSGNO;
    mqstat.mq_msgsize = MSGSIZE;
    mqstat.mq_flags = 0;

    for (i = 0; i < MQUEUENO; i++) {
        if( ((mqd_t) -1) == (mq[i] = mq_open(MQ_NAME[i],oflag,0777, &mqstat)) ) {
        printf("mq_open doesn't return success \n");
        return -1;
        }
        info[i].ThreadID = i;
        info[i].mqID = mq[i];
        schedparam.sched_priority = 1 + i;
        pthread_attr_setschedparam( &attr, &schedparam );
        pthread_create(&sed[i], &attr, (void *)send, (void *)&info[i]);
        pthread_create(&rev[i], &attr, (void *)receive, (void *)&info[i]);
        printf("Message queue mq[%d] and threads[%d] created \n", i,i);
    }
    for ( i = 0; i < MQUEUENO; i++) {
        pthread_join(sed[i], NULL);
        pthread_join(rev[i], NULL);
    }
    for ( i = 0; i < MQUEUENO; i++) {
        mq_close(mq[i]);
        mq_close(mq[i]);
        mq_unlink(MQ_NAME[i]);
        mq_unlink(MQ_NAME[i]);
    }

    printf("-------------------------------------\n");
    return 0;
}




