/*******************************************************************************

   User functions

*******************************************************************************/

#include <CANopen.h>


/*******************************************************************************
   External Functions for handling PDO reception and transmission
   Must be called either from User_ProcessMain or User_ProcessTimer
*******************************************************************************/
void CO_PDOhandleReception(void);
void CO_PDOhandleTransmission(void);


/*******************************************************************************
   User_Remove - USER EXECUTION ON EXIT OF PROGRAM
   Function is called before end of program. Not used in PIC.
*******************************************************************************/
void User_Remove(void){

}


/*******************************************************************************
   User_ResetComm - USER RESET COMMUNICATION
   Function is called after start of program and after CANopen NMT command: Reset
   Communication.
*******************************************************************************/
void User_ResetComm(void){

}


/*******************************************************************************
   User_ProcessMain - USER PROCESS MAINLINE
   This function is cyclycally called from main(). It is non blocking function.
   It is asynchronous. Here is longer and time consuming code.
*******************************************************************************/
void User_ProcessMain(void){

}


/*******************************************************************************
   User_ProcessTimer - 1 ms USER TIMER FUNCTION
   Function is executed every 1 ms. It is deterministic and has priority over
   mainline functions.
*******************************************************************************/
void User_ProcessTimer(void){
/* PDO reception */
   #if CO_NO_RPDO > 0
      CO_PDOhandleReception();
   #endif


/* User Code goes here */



/* PDO transmission */
   #if CO_NO_TPDO > 0
      CO_PDOhandleTransmission();
   #endif

}

