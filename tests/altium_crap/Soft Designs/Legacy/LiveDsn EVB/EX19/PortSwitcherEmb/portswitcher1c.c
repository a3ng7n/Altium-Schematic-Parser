// Simple port switching controller
// Control is via an RS-232 port using the TKS51A embedded serial port
// A very light weight comms suite and command interpreter is implemented.
// Literal strings are used in-line - usually not an efficient practice.
//

#define NULL 0
#define FALSE 0
#define TRUE  1

// Buffer lengths are short to prevent the need for external RAM
// Only idata is used.
#define TX_BUFFER_LENGTH 16
#define RX_BUFFER_LENGTH 16

#define RX_CMD_TERMINATOR 0x0D  // <CR>
#define RX_CMD_BACKSPACE  0x08  // <CR>

#define NUMBER_OF_PORTS 4

#define CLK_FREQ     3U*11059200UL // about 33.178 MHz
#define BAUD_RATE    57600UL


// Save ram space by keeping all strings in ROM if
// possible
 #pragma romstring 

// Globals - resource limited processor and simple
// demonstration program. Globals are being used.

// version string
__rom char sVersion[] = "PrtSw Ver 1.3 ";
__rom char sDate[] = __DATE__" ";  // Date with appended space
__rom char sTime[] = __TIME__;

// RS-232 receive buffers
// These buffers are quite restricted in length and so
// overflow will have to be managed (it has to be managed
// regardless of length, but in this instance we will
// no doubt have plenty of instances of overflow).
// In the first instance overflow shall be managed by
// discarding characters up until the next command
// terminator (v. simple).
// Two buffers are provided to allow for ping-ponging
// between them as one is being dealt with at the the
// higher level and one is receiving characters from
// the receive ISR.
char rxBuffer1[RX_BUFFER_LENGTH];
char rxBuffer2[RX_BUFFER_LENGTH];
unsigned char rxBuffer1Length = 0;
unsigned char rxBuffer2Length = 0;

__bit rxBuffer1Ready = FALSE;
__bit rxBuffer2Ready = FALSE;

// RS-232 transmit buffers
// Very limited in length to conserve
// RAM space.  Note that ROM-based
// messages can be longer.
char txBuffer[TX_BUFFER_LENGTH];
unsigned char txBufferLength = 0;


// ISR variables
const char *isrTXBuf;
__rom char *isrTXBufROM;    
                      // Code space is more available than RAM
                      // so I use a separate pointer to dump
                      // ROM-based messages. Rather than copying
                      // the ROM messages into a RAM buffer.
unsigned char isrTXLength;
unsigned char isrTXIndex;
__bit isrTXFromRom;   // TRUE if transmitting a ROM-based message
char *isrRXBuf;
unsigned char isrRXLength;
__bit isrUsingRXBuffer1;


// ROM-based messaged - see also Version variables at the top
__rom const char txBufferPrompt[] = "cmd>"; 
#define PROMPT_LENGTH sizeof(txBufferPrompt)/sizeof(char)-1
__rom const char txBufferCR = 0x0d;

// Shadow registers
// Some of the I/O ports require shadow registers for correct operation
// as not all the pins are the same direction and the port out and
// in busses are not hardwired together.
// This demonstrates a possible difference between an embedded 8051
// and a traditional chip.
unsigned char P1_ShadowReg;


// Forward declarations of function prototypes
void ClearBuffer(char *cmdBuffer);
_Bool DoCommand(const char *cmdBuffer); // Simple command interpreter
void PrintHelp(void);                   // Print simple help
void PrintPrompt(void);                 // Print a small prompt
void PrintVersion(void);
_Bool Transmit(const char *msg, const unsigned char length);
_Bool TransmitRomMsg(__rom char * msg, unsigned char length);
_Bool TransmitCR(void);
_Bool TransmitPrompt(void);

void Init(void);
void SetupSerialPort(void);

_Bool Connect(unsigned char destPort, unsigned char srcPort);
_Bool SetPortState(unsigned char port, _Bool enable);
_Bool GetPortState(unsigned char port);
_Bool QueryPort(unsigned char port);
_Bool DumpPort(unsigned char port);
signed char QueryConnection(unsigned char port);
unsigned char ConvertPortToIndex(unsigned char port);


void delay(void)
{
    int i;
    for(i=0; i<0x1fff; i++)
    {
    }
}


void main(void)
{
    Init();
    SetupSerialPort();
    
    TransmitCR();
    PrintVersion();
    TransmitCR();
    TransmitPrompt();
    ClearBuffer(rxBuffer1);
    ClearBuffer(rxBuffer2);

    while(1)
    {
       
       // Simple Task Manager
       // Loop forever and check status of
       // signals from low level software.
       // Trigger high level tasks as required.

       if(rxBuffer1Ready)
       {
          DoCommand(rxBuffer1);
          ClearBuffer(rxBuffer1);
          rxBuffer1Ready = FALSE;  // allow ISR to use this buffer if it wishes
       }
       if(rxBuffer2Ready)
       {
          DoCommand(rxBuffer2);
          ClearBuffer(rxBuffer2);
          rxBuffer2Ready = FALSE;  // allow ISR to use this buffer if it wishes
       }
    }
}

void ClearBuffer(char *cmdBuffer)
{
    for(int i=0; i < RX_BUFFER_LENGTH; i++)
    {
        cmdBuffer[i] = 0x00;
    }
}

// DoCommand is the (simple) command interpreter.
// Command decoding and execution.
// Assumes fixed format commands - each command handler
// does its own parameter checking (though there is very
// limited error handling).
_Bool DoCommand(const char *cmdBuffer)
{
    _Bool bRetVal = FALSE;
    unsigned char i;
    unsigned char ch1, ch2;

    if(cmdBuffer != NULL)
    {
       TransmitCR();
       switch(cmdBuffer[0])
       {
          case '?':
          case 'h':
          case 'H':
             // Dump help
             PrintHelp();
             bRetVal = TRUE;
             break;

          case 'v':
          case 'V':
             // Dump version
             PrintVersion();
             bRetVal = TRUE;
             break;

          case 'e':
          case 'E':
             // port enable/disable command
             // we expect an integer (port)
             // followed by another integer (1 or 0)
             // Subtract '1' from port to get zero based port.
             // Subtract '0' from integer to get 0 for '0' and 1 for '1'.
             // No error checking for now
             ch1 = ConvertPortToIndex(cmdBuffer[2]);
             if( ch1 == 0xff)
             {
                // Parameter error
                TransmitRomMsg("Invalid port", 12);
             }
             else
             {
                 if( SetPortState( ch1, (cmdBuffer[4])-'0'))
                 {
                    TransmitRomMsg("OK", 2);
                 }
                 else
                 {
                    TransmitRomMsg("Fail", 4);
                 }
             }
             bRetVal = TRUE;
             break;


          case 'm':
          case 'M':
             // port mapping command
             // we expect an integer
             // followed by another integer
             // No error checking for now
             // See if we have alpha or numeric port references
             // Also deal with upper and lower case
             ch1 = ConvertPortToIndex(cmdBuffer[2]);
             ch2 = ConvertPortToIndex(cmdBuffer[4]);
             if( ch1 == 0xff)
             {
                // Parameter error
                TransmitRomMsg("Invalid destination port", 24);
             }
             if( ch2 == 0xff)
             {
                 if( ch1 == 0xff)
                 {
                    // add in <CR> if we have already printed an error message
                    TransmitCR();
                 }
                // Parameter error
                TransmitRomMsg("Invalid source port", 19);
             }

             if((ch1 != 0xff) && (ch2 != 0xff))
             {
                 if( Connect(ch1, ch2))
                 {
                    TransmitRomMsg("OK", 2);
                 }
                 else
                 {
                    TransmitRomMsg("Fail", 4);
                 }
             }
             bRetVal = TRUE;
             break;

          case 'q':
          case 'Q':
             // Query port mapping and enable state
             // We expect an integer giving the port to query
             ch1 = ConvertPortToIndex(cmdBuffer[2]);
             if( ch1 == 0xff)
             {
                // Parameter error
                TransmitRomMsg("Invalid port", 12);
             }
             else if( !DumpPort(ch1))
             {
                TransmitRomMsg("Fail", 4);
             }
             bRetVal = TRUE;
             break;

          case 'd':
          case 'D':
             // Query port mapping and enable state for all ports
             for(i=0; i<NUMBER_OF_PORTS; i++)
             {
                if( !DumpPort(i))
                {
                   TransmitRomMsg("Fail", 4);
                }
                TransmitCR();
             }
             bRetVal = TRUE;
             break;

          default:
             TransmitRomMsg("Huh?", 4);
             break;
       }
       TransmitCR();
       TransmitPrompt();
    }
    return bRetVal;
}

unsigned char ConvertPortToIndex(unsigned char port)
{
    unsigned char index = port;
    if( (port >= '1') && (port <= '4'))
    {
        index -= '1';
    }
    else if( (port >= 'a') && (port <= 'd'))
    {
        index -= 'a';
    }
    else if( (port >= 'A') && (port <= 'D'))
    {
        index -= 'A';
    }
    else
    {
        index = 0xff;  // signal error
    }

    return index;
}

void PrintVersion(void)
{
    // Dump the version number and date and time.
    // will have to wait around for the dump to occur
    // as our buffers are too small to take the
    // full dump in one go.
    //
    // This code is not all that robust.  If there is a
    // problem transmitting one part of the
    // message the rest will still be attempted - this
    // is probably not what is required.
    // Also, the wait for the TX system is infinite.
    // This should be a timed wait with error
    // recovery.
    TransmitRomMsg(sVersion, sizeof(sVersion)/sizeof(char)-1); 
    TransmitRomMsg(sDate,    sizeof(sDate)/sizeof(char)-1); 
    TransmitRomMsg(sTime,    sizeof(sTime)/sizeof(char)-1); 
}


void PrintHelp(void)
{
    // Dump the help
    //
    // This code is not all that robust.  If there is a
    // problem transmitting one part of the
    // message the rest will still be attempted - this
    // is probably not what is required.
    // Also, the wait for the TX system is infinite.
    // This should be a timed wait with error
    // recovery.
    TransmitRomMsg("v         - print version", 25); TransmitCR();
    TransmitRomMsg("? or h    - print help", 22); TransmitCR();
    TransmitRomMsg("e prt 1|0 - enable/disable prt (1=enable)", 41); TransmitCR();
    TransmitRomMsg("m dst src - drive dst outputs from src inputs", 45); TransmitCR();
    TransmitRomMsg("q prt     - query prt connection and state" , 42); TransmitCR();
    TransmitRomMsg("d         - dump all port connections and states", 48); TransmitCR();
}


_Bool TransmitCR(void)
{
    return TransmitRomMsg(&txBufferCR, 1);
}

_Bool TransmitPrompt(void)
{
    return TransmitRomMsg(txBufferPrompt, PROMPT_LENGTH);
}

_Bool Transmit(const char * msg, unsigned char length)
{
    // This is a blocking function - it will
    // wait indefinitely until the serial port
    // is free.  Not ideal but simple for now.
    
    _Bool bRetVal = TRUE;
    
    // The following access of the isrTXLength variable
    // is atomic (byte variable) so we do not need to
    // disable interrupts.
    while(isrTXLength != 0) 
    {
       // Wait until the port is free.
       // We could put a timeout in here and return FALSE if
       // the TX fails.       
    }
    isrTXBuf = msg; // not sure this will work on a ROM string?
    isrTXLength = length;
    isrTXIndex = 1;  // we will send the 0'th character below
    isrTXFromRom = FALSE;
    
    SBUF = msg[0];  // start the transmission
       
    
    return bRetVal;
}

_Bool TransmitRomMsg(__rom char * msg, unsigned char length)
{
    // This is a blocking function - it will
    // wait indefinitely until the serial port
    // is free.  Not ideal but simple for now.
    
    _Bool bRetVal = TRUE;
    
    // The following access of the isrTXLength variable
    // is atomic (byte variable) so we do not need to
    // disable interrupts.
    while(isrTXLength != 0) 
    {
       // Wait until the port is free.
       // We could put a timeout in here and return FALSE if
       // the TX fails.       
    }
    isrTXBufROM = msg; // not sure this will work on a ROM string?
    isrTXLength = length;
    isrTXIndex = 1;  // we will send the 0'th character below
    isrTXFromRom = TRUE;
    
    SBUF = msg[0];  // start the transmission
       
    return bRetVal;
}


void Init(void)
{
       // Initialise ports for loopback and disabled
       unsigned char i;
       for(i=0; i<NUMBER_OF_PORTS; i++)
       {
          SetPortState(i, FALSE);
          Connect(i, i);
       }
}

void SetupSerialPort(void)
{
    isrTXBuf = NULL;
    isrTXLength = 0;
    isrRXBuf = rxBuffer1;
    isrRXLength = 0;
    isrUsingRXBuffer1 = TRUE;

    SM0 = 0;    
    SM1 = 1;    // Mode 1 - 8-bit UART
    SM2 = 0;    // Disable multiprocessor feature
    TB8 = 0;    // 9th bit (not used)
    RB8 = 0;    // This is the rx stop bit.
    TI  = 0;    // Clr TX int
    RI  = 0;    // Clr RX int
    
    //Need to set up timer 1 for baud rate CLK_FREQ_MHZ
    
    TR1 = 0; // stop timer 1
    
    PCON_bit(7) = 1;  // SMOD = 1 -> high speed baud rate
  
    // baud rate divisor
    TL1 = 0;  // initialise to zero
    TH1 = 256U - (2 * CLK_FREQ/(12U*32U*BAUD_RATE)); 
    
    TMOD_bit(7) = 0;    // disable GATE
    TMOD_bit(6) = 0;    // operate as timer
    TMOD_bit(5) = 1;
    TMOD_bit(4) = 0;    // 8-bit timer auto-reload mode
    
    IT0 = 0;  
    IE0 = 0; 
    IT1 = 0;
    IE1 = 0;
    TR0 = 0; // timer 0 stopped
    TF0 = 0;
    TR1 = 1; // timer 1 running
    TF1 = 0;  
    
    ES = 1;
    IE_bit(7) = 1;
    
    REN = 1;  // enable reception
}

// Connect - the destination port outputs are driven from the
//           the inputs of the source port.
//           __putbit and __getbit could be used in this function.
//           Returns TRUE if both ports are within range, FALSE
//           otherwise (and no mapping change is done in this case).

_Bool Connect(unsigned char destPort, unsigned char srcPort)
{
    _Bool bRetVal;
    // Are ports within range?
    if(destPort >= NUMBER_OF_PORTS || srcPort >= NUMBER_OF_PORTS)
    {
       bRetVal = FALSE;
    }
    else
    {
       // first set save the current state of the dest port
       _Bool bSave = GetPortState(destPort);
       P0 &= ~(0x03 << (2*destPort));  // Clear the existing mapping for this port
                                   // This little hack only works when
                                   // NUMBER_OF_PORTS == 4!
                                   // Generally a more elaborate mechanism would
                                   // be required that cleared all the bits of
                                   // the port that control this destination port.
       P0 |= (srcPort << (2*destPort));
       SetPortState(destPort, bSave);  // Restore port state
       bRetVal = TRUE;
    }
    return bRetVal;
}

// SetPortState - returns FALSE (0) if port is out of range.
//                returns TRUE (1) if port is within range
//                This function will only work correctly when
//                NUMBER_OF_PORTS is 8 or less.
_Bool SetPortState(unsigned char port, _Bool enable)
{
    _Bool bRetVal;
    // Do some parameter checking
    if(port >= NUMBER_OF_PORTS)
    {
       bRetVal = FALSE;
    }
    else
    {
       if(enable)
       {
          P1_ShadowReg |= (1 << port);  // OR in the correct port
       }
       else
       {
          P1_ShadowReg &= ~(1 << port); // AND out the correct port
       
       }
       P1 = P1_ShadowReg;
       bRetVal = TRUE;
    }
    return bRetVal;
}

// GetPortState - return FALSE (0) if port enable bit is not set or
//                if port is out of range (TRUE otherwise).
_Bool GetPortState(unsigned char port)
{
    if(port >= NUMBER_OF_PORTS)
    {
       return FALSE;  // Warning - unstructured error return.
    }
    
    return (P1_ShadowReg & (1 << port));
}

_Bool DumpPort(unsigned char port)
{
    _Bool bRetVal = FALSE;
    signed char tmp;
    
    if(port < NUMBER_OF_PORTS)
    {
       tmp = port + 'A';
       Transmit((const char *)&tmp, 1);  
       TransmitRomMsg(" <- ", 4);
       // Extract the mapping
       tmp = QueryConnection(port);
       if(tmp >= 0 && tmp < NUMBER_OF_PORTS)
       {
          tmp += 'A';
          Transmit((const char *)&tmp, 1);
       }
       else
       {
          // If we received an error display a question mark
          TransmitRomMsg("?", 1);  
       }
       if(GetPortState(port))
       {
          TransmitRomMsg(" enabled", 8);
       }
       else
       {                                                 
          TransmitRomMsg(" disabled", 9);
       }
       bRetVal = TRUE;
    }
    return bRetVal;
}

signed char QueryConnection(unsigned char port)
{
    signed char retVal = -1;
    
    if(port < NUMBER_OF_PORTS)
    {
       // Extract the mapping
       retVal = P0;
       retVal >>= (2*port);
       retVal &= 0x03; // Only works for NUMBER_OF_PORTS = 4!
    }
    return retVal;
}



__interrupt(0x23) void SerialISR( void )
{
    char tempChar; // temporary variable to reduce the
                   // amount of array dereferencing.
    // Is there a TX interrupt?
    if(TI)
    {
       TI = 0;  // clear int source as soon as possible to
                // minimise chances of missing an interrupt
       // send the next character (if any) out to the port
       if(isrTXIndex < isrTXLength)
       {
          // Another char to send - determine what sort of pointer we should
          // use. When we are sending a ROM-based message use a
          // __rom char *.  This saves having to copy the message into RAM.
          // RAM is very limited in this application - only idata.
          if(isrTXFromRom)
          {
             // We are sending a ROM message so use the ROM pointer
             SBUF = isrTXBufROM[isrTXIndex++];
          }
          else
          {
              SBUF = isrTXBuf[isrTXIndex++];
          }
       }
       else
       {
          // signal that we have finished transmitting
          isrTXLength = 0;
       }
    }   // end of if(TI)  - transmit character handling
    
    // Is there an RX interrupt?
    if(RI)
    {
       RI = 0;  // clear int source as soon as possible to
                // minimise chances of missing an RX interrupt 

       tempChar = SBUF;
       // save this character away - if there is space
       if(isrRXLength < RX_BUFFER_LENGTH-1)
       {
          // OK save it
          isrRXBuf[isrRXLength++] = tempChar;
       }
       else
       {
          // Lets signal something
          // I know! We can put the count of received chars onto
          // Port 2. But we will do this outside the test so it
          // is a useful count at all times.
       }

       // Deal with back spaces - watch for underflow of the
       // buffer.
       if(tempChar == RX_CMD_BACKSPACE)
       {
          if(isrRXLength > 1)
          {
            isrRXLength -= 2; // take off two to account for the just-added
                              // backspace character
          }
          else
          {
            isrRXLength = 0; // only really useful for a signed variable
          }
          // Could print a space and then do the backspace again
          // but I don't want to have to deal with potential recursion in
          // calling the TX routines, or to possibly stuff up some outgoing
          // transmission by forceably inserting a character into the transmit
          // buffer.
       }

       // Debug ouput - Upper four bits of port 2 contain lower four bits of
       // tempChar and lower four bits of port 2 contains lower four bits of
       // isrRXLength
       P2 = ((tempChar << 4) & 0xf0)|(0x0f & isrRXLength);
       //P2 = isrRXLength; // signal the number of received characters onto port 2


       // If the character was a command terminator then we need to
       // pass it up to the high level.
       if(tempChar == RX_CMD_TERMINATOR)
       {
          // new command buffer
          if(isrUsingRXBuffer1)
          {
             rxBuffer1Length = isrRXLength;
             
             // set up for further reception in the other RX buffer
             isrRXBuf = rxBuffer2;
             isrUsingRXBuffer1 = FALSE;
           
             rxBuffer1Ready = TRUE;  // signal high level code 
          }
          else
          {
             rxBuffer2Length = isrRXLength;
             isrRXBuf = rxBuffer1;
             isrUsingRXBuffer1 = TRUE;
             rxBuffer2Ready = TRUE;
          }
          isrRXLength = 0;
       }   // end of if(tempChar == RX_CMD_TERMINATOR )
    }      // end of if(RI)  - receive character handling
}

