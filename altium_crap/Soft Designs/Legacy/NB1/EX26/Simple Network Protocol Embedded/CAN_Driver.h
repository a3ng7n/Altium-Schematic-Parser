
// The following is used to make mask off non-address bits in a byte
// The address is the 5 least-significant bits.
#define NODE_ID_MASK 0x1F
#define NODE_ID_BIT_LENGTH (byte)5

// the maximum amount of data that can be transmitted/received
#define SNP_MAX_DATA_BYTES (byte)2

typedef struct _SNPData
{
    byte messageID;
    byte dataLength;
    byte data[SNP_MAX_DATA_BYTES];
} SNPData;

typedef void (*ReceivedMessageCallback)(SNPData* rxData);

// Globally accessible CAN functions
void CANInit(byte nodeID);
void transmitData(SNPData* txData);
_Bool receiveData(void);

void setReceivedMessageCallback(ReceivedMessageCallback newReceivedMessageCallback);

