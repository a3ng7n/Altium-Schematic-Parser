/*********************************************************************
|*
|*  This module implements a simple calculator state machine.
|*
|*  Each key entered on the user interface is forwarded to the
|*  ProcessKey() function, which will update the calculator's
|*  internal state accordingly and return the value that should
|*  be displayed on the user interface.
|*
 *********************************************************************/

#include <stdint.h>


/* Enumeration */

typedef enum tKeyType {
    e_Digit,
    e_Operator,
    e_Equals,
    e_Clear,
    e_Invalid
} KeyType;

typedef enum tOperator {
    e_Add,
    e_Subtract,
    e_Multiply,
    e_Divide,
    e_NoOp
} Operator;

typedef enum tState {
    e_EnteringOperand1,
    e_EnteredOperator,
    e_EnteringOperand2,
    e_ResultCalculated
} State;


/* Static variables to store state */

static int32_t  s_Operand1     = 0;
static int32_t  s_Operand2     = 0;
static Operator s_Operator     = e_NoOp;
static State    s_CurrentState = e_EnteringOperand1;


/* Forward declarations */

int32_t DoState_EnteringOperand1(const KeyType  a_KeyType,
                                 const uint8_t  a_Digit,
                                 const Operator a_Operator);
int32_t DoState_EnteredOperator (const KeyType  a_KeyType,
                                 const uint8_t  a_Digit,
                                 const Operator a_Operator);
int32_t DoState_EnteringOperand2(const KeyType  a_KeyType,
                                 const uint8_t  a_Digit,
                                 const Operator a_Operator);
int32_t DoState_ResultCalculated(const KeyType  a_KeyType,
                                 const uint8_t  a_Digit,
                                 const Operator a_Operator);
KeyType DecodeKey               (const uint8_t  a_ReceivedKey,
                                 uint8_t*       a_Digit,
                                 Operator*      a_Operator);
void    EvaluateResult(void);
void    Reset(void);


/* The top level exported function. Process the keys entered by the user on the
   instrument keypad. Update the current state. */

void ProcessKey(const uint8_t RECEIVED_KEY_I, int32_t* DISPLAY_VAL_O)
{
    KeyType  l_KeyType  = e_Invalid;
    uint8_t  l_Digit    = 0;
    Operator l_Operator = e_NoOp;
    *DISPLAY_VAL_O      = 0;

    l_KeyType = DecodeKey(RECEIVED_KEY_I, &l_Digit, &l_Operator);
    if (l_KeyType == e_Clear)
    {
        Reset();
    }
    else
    {
        switch (s_CurrentState)
        {
            case e_EnteringOperand1:  *DISPLAY_VAL_O = DoState_EnteringOperand1(l_KeyType, l_Digit, l_Operator);  break;
            case e_EnteredOperator :  *DISPLAY_VAL_O = DoState_EnteredOperator (l_KeyType, l_Digit, l_Operator);  break;
            case e_EnteringOperand2:  *DISPLAY_VAL_O = DoState_EnteringOperand2(l_KeyType, l_Digit, l_Operator);  break;
            case e_ResultCalculated:  *DISPLAY_VAL_O = DoState_ResultCalculated(l_KeyType, l_Digit, l_Operator);  break;
        }
    }
}


/* Update the current state: e_EnteringOperand1 */

int32_t DoState_EnteringOperand1(const KeyType  a_KeyType,
                                 const uint8_t  a_Digit,
                                 const Operator a_Operator)
{
    if (a_KeyType == e_Digit)
    {
        s_Operand1 = 10 * s_Operand1 + a_Digit;
    }
    else if (a_KeyType == e_Operator)
    {
        s_Operator     = a_Operator;
        s_CurrentState = e_EnteredOperator;
    }
    return s_Operand1;
}


/* Update the current state: e_EnteredOperator */

int32_t DoState_EnteredOperator(const KeyType  a_KeyType,
                                const uint8_t  a_Digit,
                                const Operator a_Operator)
{
    int32_t l_DisplayValue = s_Operand1;
    if (a_KeyType == e_Digit)
    {
        s_Operand2     = a_Digit;
        l_DisplayValue = s_Operand2;
        s_CurrentState = e_EnteringOperand2;
    }
    else if (a_KeyType == e_Operator)
    {
        s_Operator = a_Operator;
    }
    return l_DisplayValue;
}


/* Update the current state: e_EnteringOperand2 */

int32_t DoState_EnteringOperand2(const KeyType  a_KeyType,
                                 const uint8_t  a_Digit,
                                 const Operator a_Operator)
{
    int32_t l_DisplayValue = 0;
    if (a_KeyType == e_Digit)
    {
        s_Operand2     = 10 * s_Operand2 + a_Digit;
        l_DisplayValue = s_Operand2;
    }
    else if (a_KeyType == e_Operator)
    {
        EvaluateResult();
        s_Operator     = a_Operator;
        l_DisplayValue = s_Operand2;
        s_CurrentState = e_EnteredOperator;
    }
    else if (a_KeyType == e_Equals)
    {
        EvaluateResult();
        l_DisplayValue = s_Operand1;
        s_CurrentState = e_ResultCalculated;
    }
    return l_DisplayValue;
}


/* Update the current state: e_ResultCalculated */

int32_t DoState_ResultCalculated(const KeyType  a_KeyType,
                                 const uint8_t  a_Digit,
                                 const Operator a_Operator)
{
    if (a_KeyType == e_Digit)
    {
        Reset();
        s_Operand1 = a_Digit;
    }
    else if (a_KeyType == e_Operator)
    {
        s_Operator     = a_Operator;
        s_CurrentState = e_EnteredOperator;
    }
    else if (a_KeyType == e_Equals)
    {
        EvaluateResult();
    }
    return s_Operand1;
}


/* Decode the key entered by the user. */

KeyType DecodeKey(const uint8_t a_ReceivedKey,
                  uint8_t*      a_Digit,
                  Operator*     a_Operator)
{
    KeyType l_KeyType = e_Invalid;
    *a_Digit          = 0;
    *a_Operator       = e_NoOp;

    if (a_ReceivedKey < 10)
    {
        l_KeyType = e_Digit;
        *a_Digit  = a_ReceivedKey;
    }
    else if (a_ReceivedKey < 14)
    {
        l_KeyType = e_Operator;
        switch (a_ReceivedKey)
        {
            case 10:  *a_Operator = e_Add;       break;
            case 11:  *a_Operator = e_Subtract;  break;
            case 12:  *a_Operator = e_Multiply;  break;
            case 13:  *a_Operator = e_Divide;    break;
        }
    }
    else if (a_ReceivedKey == 14)
    {
        l_KeyType = e_Equals;
    }
    else if (a_ReceivedKey == 15)
    {
        l_KeyType = e_Clear;
    }

    return l_KeyType;
}


/* Apply the operator on the operands. The result is stored in s_Operand1. */

void EvaluateResult(void)
{
    switch (s_Operator)
    {
        case e_Add:       s_Operand1 = s_Operand1 + s_Operand2;  break;
        case e_Subtract:  s_Operand1 = s_Operand1 - s_Operand2;  break;
        case e_Multiply:  s_Operand1 = s_Operand1 * s_Operand2;  break;
        case e_Divide:    s_Operand1 = s_Operand1 / s_Operand2;  break;
    };
}


/* Reset to the initial state. */

void Reset(void)
{
   s_Operand1     = 0;
   s_Operand2     = 0;
   s_Operator     = e_NoOp;
   s_CurrentState = e_EnteringOperand1;
}
