

// Simple Network Protocol API
void networkInit(void);
void setNetworkRegister(byte targetNodeID, byte networkVariableAddress, byte networkVariableValue);
void getNetworkRegister(byte targetNodeID, byte networkVariableAddress);
void sendNetworkVariableValue(byte targetNodeID, byte networkVariableAddress, byte networkVariableValue);

// API Callbacks that the API client needs to implement
typedef void (*SetNetworkVariableCallback)(byte networkVariableAddress, byte networkVariableValue);
typedef byte (*GetNetworkVariableCallback)(byte networkVariableAddress);
typedef void (*NetworkVariableValueCallback)(byte networkVariableAddress, byte networkVariableValue);

// Accessors
void setNodeID(byte newNodeID);
byte getNodeID(void);
void setSetNetworkVariableCallback(SetNetworkVariableCallback newSetNetworkVariableCallback);
void setGetNetworkVariableCallback(GetNetworkVariableCallback newGetNetworkVariableCallback);
void setNetworkVariableValueCallback(NetworkVariableValueCallback newNetworkVariableValueCallback);

