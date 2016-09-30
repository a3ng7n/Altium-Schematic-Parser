#ifndef __INC_MAX1617__
#define __INC_MAX1617__

#define MAX1617_I2C_BASE  0x30                             // I2C base address

// register definition
#define RLTS     0x00                                      // Read Local Temperature
#define RRTE     0x01                                      // Read Remote Temperature
#define RSL      0x02                                      // Read Status Byte (flags, busy signal)
#define RCL      0x03                                      // Read configuration byte
#define RCRA     0x04                                      // Read Conversion rate byte
#define RLHN     0x05                                      // Read Local Thigh Limit
#define RLLI     0x06                                      // Read Local Tlow Limit
#define RRHI     0x07                                      // Read Remote Thigh Limit
#define RRLS     0x08                                      // Read Remote Tlow Limit
#define WCA      0x09                                      // Write Configuration Byte
#define WCRW     0x0A                                      // Write Conversion Rate byte
#define WLHO     0x0B                                      // Write Local Thigh Limit
#define WLLM     0x0C                                      // Write Local Tlow Limit
#define WRHA     0x0D                                      // Write Remote Thigh Limit
#define WRLN     0x0E                                      // Write Remote Tlow Limit
#define OSHT     0x0F                                      // One-shot Command (use send-byte format)
#define SPOR     0xFC                                      // Write Software POR
#define MFGID    0xFE                                      // Read Manufacturer ID Code
#define DEVID    0xFF                                      // Read Device ID Code

// Configuration byte bit assignments
#define MASK     0x80                                      // masks all ALERT interrupts when high
#define RUN_STOP 0x40                                      // Standby mode control bit. If high, device enters standby mode

// Status byte bit assignments
#define BUSY     0x80                                      // A high indicates that the ADC is busy converting
#define LHIGH    0x40                                      // A high indicates that the local high temperature
                                                           // alarm has been activated.
#define LLOW     0x20                                      // A high indicates that the local low temperature
                                                           // alarm has been activated.
#define RHIGH    0x10                                      // A high indicates that the remote high temperature
                                                           // alarm has been activated.
#define RLOW     0x08                                      // A high indicates that the remote low temperature
                                                           // alarm has been activated.
#define OPEN     0x02                                      // A high indicates open circuit remote diode connection


typedef enum {SENSOR_MAX1617, SENSOR_FPGA} SensorType;

//----------------------------------------------------------------
// read any command register in the MAX1617 and return its value
//----------------------------------------------------------------
unsigned char Max1617_ReadRegister(unsigned char command);

//--------------------------------------------------------------------------
// write 'data' to any command register in the MAX1617
//--------------------------------------------------------------------------
void Max1617_WriteRegister(unsigned char command, unsigned char data);

//---------------------------------------------------------------------------
// Command Byte: Sends Command with no data, usually for one-shot command
// returns: 'ACK' iff success, 'NACK' if not
//---------------------------------------------------------------------------
unsigned char Max1617_SendByte(unsigned char command);

//-----------------------------------------------------
// read temperature from MAX1617
// SensorType = SENSOR_MAX1617 returns local temperature
//              otherwise FPGA temperature
//-----------------------------------------------------
signed char Max1617_GetTemperature(SensorType Sensor);

// -------------------------------------------------------------
// converts Max1617 Temperatures to a string s eg +102
// required are at least 5 bytes for temperature plus trailing 0
// -------------------------------------------------------------
void Max1617_Temperature2String(unsigned char Temperature, char *s);
       
#endif  // __INC_MAX1617__
