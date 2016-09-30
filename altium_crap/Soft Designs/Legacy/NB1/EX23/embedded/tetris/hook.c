/******************************************************************************
 *  FILE:   @(#)hook.c	1.12 04/08/27
 *  DESCRIPTION:                            
 * 	Example of hook routines file.
 *****************************************************************************/
#include <osek/osek.h>
#if defined(DEMO)
#include "demo.h"
#endif
#if defined(TETRIS)
#include "tetris.h"
#endif

#if defined(STDIOTX)
#include <stdio.h>
#endif

/* OIL objects */
DeclareAppMode(APPMODE1);
DeclareAppMode(APPMODE2);

/* print-out information from error/pre/post/hook routines */
static void HookOut(char *txt, StatusType err);
/* print-out information from shutdownhook routines */
static void ShutOut(char *txt, StatusType err);

/************************************************************************
 * This hook routine is called by the operating system at the end
 * of the operating system initialisation and before the scheduler is
 * running. At this time the application can initialise device drivers
 * STARTUPHOOK        = TRUE;
 ***********************************************************************/
void StartupHook(void)
{
	/* init the io subsystem */
	ioinit();
    	return;
} 

/************************************************************************
 * This hook routine is called by the operating system when the
 * OS service ShutdownOS has been called by the user or as a followup of
 * a fatal internal error.
 * SHUTDOWNHOOK       = TRUE;
 * No services are allowed here.
 ***********************************************************************/
void ShutdownHook ( StatusType Error)
{
    	static unsigned char no = 0;

    	switch(Error)
    	{
        	/* Example of "good" application reset */
        	case E_OK: break;

		/* Internal Fatal Error */
        	case E_OS_SYS_ERROR: case E_OS_SYS_VALUE:
        	case E_OS_SYS_RTOFLW:
        	        ShutOut("System Err: ", Error);
        	        break;

        	/* Stack overflow */
        	case E_OS_SYS_UOFLW: case E_OS_SYS_SOFLW:
        	case E_OS_SYS_TOFLW:
        	       ShutOut("Stack Error: ", Error);
		       break;
	    
		default:		
			/* Application Error */ 
	    		ShutOut("APP error: ",Error);
                	ShutOut("???? Err: ", Error);
                	break;
    	}
	no++;
    	ShutOut("Shutting RTOS : ", no);
		
    	/* After returning, the RTOS will clean up all
     	* opened objects and return from the previous
     	* call to StartOS().
     	* The control will be normally given back to
     	* the application in main().
     	*/
    	return;
}

/************************************************************************
 * This hook routine is called by the operating system at the end
 * of a system service which returns StatusType not equal E_OK.
 * It is called before returning to the task level.
 * ERRORHOOK       = TRUE;
 * The only services available are:
 *    - GetTaskID,GetTaskState,SuspendAllInterrupts,ResumeAllInterrupts,
 *      GetEvent,GetAlarmBase,GetAlarm, GetActiveApplicationMode
 ***********************************************************************/
void ErrorHook (StatusType Error )
{
    signed long Param1=0,Param2=0,Param3=0;
    TaskType    calling_task;
    AppModeType mode;
    
    /* check valid running task */
    GetTaskID(&calling_task);
    if ( calling_task  == INVALID_TASK )
    {
        ShutdownOS(E_ERR_HOOK_IT);   
    } 
    
    /* operational mode */
    mode = GetActiveApplicationMode();
    if ( mode != APPMODE1 && mode != APPMODE2)
    {   
        ShutdownOS(E_ERR_HOOK_IM);   
    }
        
    /* USEGETSERVICEID    = TRUE */

    /* check failing system service */
    switch(OSErrorGetServiceId())
    {
        /* USEPARAMETERACCESS = TRUE */

        /* check in all cases the given parameters as integers */
        case OSServiceID_GetResource:
            Param1 = OSError_GetResource_ResID();
            break;
        case OSServiceID_ReleaseResource:
            Param1 = OSError_ReleaseResource_ResID();
            break;
        case OSServiceID_GetTaskID:
            break;
        case OSServiceID_StartOS:
            Param1 = OSError_StartOS_Mode();
            break;
        case OSServiceID_ActivateTask:
            Param1 = OSError_ActivateTask_TaskID();
            break;
        case OSServiceID_TerminateTask:
            break;
        case OSServiceID_GetTaskState:
            Param1 = OSError_GetTaskState_TaskID();
            Param2 = OSError_GetTaskState_State();
            break;
        case OSServiceID_Schedule:
            break;
        case OSServiceID_GetActiveApplicationMode:
            break;
        case OSServiceID_GetAlarmBase:
            Param1 =  OSError_GetAlarmBase_AlarmID();
            Param2 =  OSError_GetAlarmBase_Info();
            break;
        case OSServiceID_GetAlarm:
            Param1 =  OSError_GetAlarm_AlarmID();
            Param2 =  OSError_GetAlarm_Tick();
            break;
        case OSServiceID_SetRelAlarm:
            Param1 =  OSError_SetRelAlarm_AlarmID();
            Param2 =  OSError_SetRelAlarm_increment();
            Param3 =  OSError_SetRelAlarm_cycle();
            break;
        case OSServiceID_SetAbsAlarm:
            Param1 =  OSError_SetAbsAlarm_AlarmID();
            Param2 =  OSError_SetAbsAlarm_start();
            Param3 =  OSError_SetAbsAlarm_cycle();
            break;
        case OSServiceID_CancelAlarm:
            Param1 =  OSError_CancelAlarm_AlarmID();
            break;
        case OSServiceID_SetEvent:
            Param1 =  OSError_SetEvent_TaskID();
            Param2 =  OSError_SetEvent_Mask();
            break;
        case OSServiceID_GetEvent:
            Param1 =  OSError_GetEvent_TaskID();
            Param2 =  OSError_GetEvent_Event();
            break;
        case OSServiceID_WaitEvent:
            Param1 =  OSError_WaitEvent_Mask();
            break;
        case OSServiceID_ClearEvent:
            Param1 =  OSError_ClearEvent_Mask();
            break;
        case OSServiceID_IncrementCounter:
            Param1 =  OSError_IncrementCounter_CounterID();
            break;
        default:
            break;
    }
	HookOut("ERROR HOOK: error ",Error);
	HookOut("            sys   ",(int)OSErrorGetServiceId());
	HookOut("            task  ",(int)calling_task);

    	return;
}


/*********************************************************************
 * This hook routine is called by the operating system before
 * executing a new task, but after the transition of the task to the
 * running state (to allow evaluation of the TaskID by GetTaskID).
 *  PRETASKHOOK = TRUE
 * The only services available are:
 *    - GetTaskID,GetTaskState,SuspendAllInterrupts,ResumeAllInterrupts,
 *      GetEvent,GetAlarmBase,GetAlarm, GetActiveApplicationMode
 *********************************************************************/ 
void PreTaskHook ( void )
{
    TaskType    task;
    TaskStateType   state;

    /* check valid task */
    GetTaskID(&task);
    if (  task== INVALID_TASK )
    {
        ShutdownOS(E_ERR_HOOK_IT);   
    }
      
    /* state of task is already running */
    if ( RUNNING != GetTaskState(task,&state) )
    {
        ShutdownOS(E_ERR_HOOK_IS);   
    }
    
    /* you could make here stats with CPU usage per task,
     * number of runs, etc ..
     */
    
    return;
}

/*********************************************************************
 *  This hook routine is called by the operating system after
 *  executing the current task, but before leaving the task's running
 *  state (to allow evaluation of the TaskID by GetTaskID).
 *  POSTTASKHOOK = TRUE;
 * The only services available are:
 *    - GetTaskID,GetTaskState,SuspendAllInterrupts,ResumeAllInterrupts,
 *      GetEvent,GetAlarmBase,GetAlarm, GetActiveApplicationMode
 *********************************************************************/
void PostTaskHook ( void )
{
    TaskType    task;
    TaskStateType   state;
   
    /* check valid task */
    GetTaskID(&task);
    if (  task== INVALID_TASK )
    {
        ShutdownOS(E_ERR_HOOK_IT);   
    }
  
    /* state of task is already running */
    if ( RUNNING != GetTaskState(task,&state) )
    {
        ShutdownOS(E_ERR_HOOK_IS);   
    }

    /* you could make here stats with CPU usage per task,
     * number of runs, etc ..
     */
    
    return;

}

/***********************************************************************
 * The service COMErrorHook is provided by the application and is
 * called by OSEK COM at the end of a system service which returns a
 * status code not equal to E_OK.
 * COMERRORHOOK = TRUE in OIL file
 * The only services available are:
 *    - GetTaskID,GetTaskState,SuspendAllInterrupts,ResumeAllInterrupts,
 *      GetEvent,GetAlarmBase,GetAlarm, GetActiveApplicationMode
 **********************************************************************/
void COMErrorHook(StatusType Error)
{   
    signed long Param1=0,Param2=0;

    /*  COMUSEGETSERVICEID = TRUE in OIL file */

    /* check failing system service */
    switch(COMErrorGetServiceId())
    {
            /* COMUSEPARAMETERACCESS = TRUE in OIL file */
                
        /* check in all cases the given parameters as integers */
        case COMServiceID_StartCOM:
            Param1 = COMError_StartCOM_Mode();
            break;
        case COMServiceID_StopCOM:
            Param1 = COMError_StopCOM_Mode();
            break;
        case COMServiceID_GetCOMApplicationMode:
            break;
        case COMServiceID_InitMessage :
            Param1 = COMError_InitMessage_Message();
            break;
        case COMServiceID_SendMessage:
            Param1 = COMError_SendMessage_Message();
                    Param2 = COMError_SendMessage_DataRef();
            break;
        case COMServiceID_ReceiveMessage :
                    Param1 = COMError_ReceiveMessage_Message();
                    Param2 = COMError_ReceiveMessage_DataRef();
            break;
        case COMServiceID_GetMessageStatus:
            Param1 = COMError_GetMessageStatus_Message();
            break;
    }

	HookOut("COM HOOK: error ",Error);
	HookOut("          sys   ",(int)COMErrorGetServiceId());
    	return;
}

/************************************************************************
 * The routine StartCOMExtension is provided by the application and is
 * called by the OSEK COM implementation at the end of the StartCOM
 * routine.
 ***********************************************************************/
StatusType StartCOMExtension(void)
{
    return E_OK;
}

/****************************************************************************** 
 * 1. print-out information from error/pre/post/hook routines.
 * 2. Dont use ISR2s for interrupt driven output (theyre off).
 * 3. The only services available are:
 *    - GetTaskID,GetTaskState,SuspendAllInterrupts,ResumeAllInterrupts,
 *      GetEvent,GetAlarmBase,GetAlarm, GetActiveApplicationMode
 *****************************************************************************/
static void HookOut(char *txt, StatusType err)
{
#if defined(STDIOTX)
	printf("%s %d\n", txt, (int)err);
#else
	(void)txt;(void)err;
#endif
}

/******************************************************************************
 * 1. print-out information from shutdownhook routines.
 * 2. normally 'printf' or an ISR1-based interrupt-driven
 *    output-software (blocking, of course).
 * 3. No services allowed.
 *****************************************************************************/
static void ShutOut(char *txt, StatusType err)
{
#if defined(STDIOTX)
	printf("%s %d\n", txt, (int)err);
#else
	(void)txt;(void)err;
#endif
}
