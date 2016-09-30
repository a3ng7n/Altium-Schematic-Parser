
typedef void (*GenerateBitSequenceFunction)();

#define BYTE unsigned char
#define TRUE 1
#define FALSE 0

#define CONTROL_PORT P0
#define STATUS_PORT P1
#define ERROR_PORT P1
#define ENABLE_NETWORK_VALUE 0xFE
#define DISABLE_NETWORK_VALUE 0x01
BYTE currentControlValue = 0;

#define EXTERNAL_INTERRUPT_0 0*8 + 3


#define FRAME_SIZE 1024
#define FRAME_BUFFER_SIZE FRAME_SIZE * 2
#define TX_FRAME_START_ADDR 0
#define RX_FRAME_START_ADDR 1024
__xdata BYTE frameBuffer[FRAME_BUFFER_SIZE] __at(0x0000);

#define RANDOM_SEED 1234
#define RANDOM_MULT 16807
#define RANDOM_MOD 0xFFFE
int randomValue = RANDOM_SEED;

int numErrors = 0;

// Prototypes
void checkReceiveBuffer(void);


void enableNetwork(void)
{
    currentControlValue &= ENABLE_NETWORK_VALUE;
    CONTROL_PORT = currentControlValue;
}

void disableNetwork(void)
{
    currentControlValue |= DISABLE_NETWORK_VALUE;
    CONTROL_PORT = currentControlValue;
}

void outputErrorCount(BYTE errorCount)
{
    ERROR_PORT = errorCount;
}

void networkTester(GenerateBitSequenceFunction generateBitSequenceFunction)
{
    outputErrorCount(numErrors);

    generateBitSequenceFunction();

    enableNetwork();
    while(!(STATUS_PORT & 0x01)); // wait for TX buff empty
    while(!(STATUS_PORT & 0x02)); // wait for RX buff full
    disableNetwork();
    checkReceiveBuffer();
    outputErrorCount(numErrors);
}

void simpleValueTest(void)
{
    frameBuffer[0] = 0xa2;
    for(int i=1; i < FRAME_SIZE; i++)
    {
       frameBuffer[i] = 0xc5;
    }
}

void randomSequenceTest(void)
{
    frameBuffer[0] = 0xa2;
    // uses the following LCG:
    // Xn = (aXn-1) mod m
    for(int i=1; i<FRAME_SIZE; i++)
    {
       randomValue = (16807 * randomValue) % RANDOM_MOD;
       // output each bit of the random value
       frameBuffer[i] = randomValue;
    }
}

void longHighSequenceTest(void)
{
    frameBuffer[0] = 0xa2;
    for(int i=1; i<FRAME_SIZE; i++)
    {
       frameBuffer[i] = 0xff;
    }
}

void longLowSequenceTest(void)
{
    frameBuffer[0] = 0xa2;
    for(int i=1; i<FRAME_SIZE; i++)
    {
       frameBuffer[i] = 0x00;
    }
}

void alternatingBitSequenceTest(void)
{
    frameBuffer[0] = 0xa2;
    for(int i=1; i<FRAME_SIZE; i++)
    {
       frameBuffer[i] = 0xa5;
    }
}

void alternatingByteSequenceTest(void)
{
    frameBuffer[0] = 0xa2;
    BYTE outVal = 0xff;
    for(int i=1; i<FRAME_SIZE; i++)
    {
       frameBuffer[i] = outVal;
       outVal = !outVal;
    }
}

void checkReceiveBuffer(void)
{
    int transmitIndex = 1;
    int receiveIndex = RX_FRAME_START_ADDR;
    for(; receiveIndex < RX_FRAME_START_ADDR + FRAME_SIZE - 1; receiveIndex++, transmitIndex++)
    {
       if(frameBuffer[receiveIndex] != frameBuffer[transmitIndex])
       {
          if(numErrors < 255)
             numErrors++;
       }
    }
}

void initialise(void)
{
    disableNetwork();
}

void main()
{
    initialise();
    while(TRUE)
    {
       networkTester(simpleValueTest);
       networkTester(randomSequenceTest);
       networkTester(longHighSequenceTest);
       networkTester(longLowSequenceTest);
       networkTester(alternatingBitSequenceTest);
       networkTester(alternatingByteSequenceTest);
    }
}
