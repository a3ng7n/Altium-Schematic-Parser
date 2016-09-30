//-------------------------------------------------------------------------
//     _   _  ____  _____        _     ____   ____     _        _   _
//    | \ | || __ )|_   _|      / \   |  _ \ |  _ \   / \      | | | |
//    |  \| ||  _ \  | |       / _ \  | | | || | | | / _ \     | |_| |
//    | |\  || |_) | | |      / ___ \ | |_| || |_| |/ ___ \  _ |  _  |
//    |_| \_||____/  |_|_____/_/   \_\|____/ |____//_/   \_\(_)|_| |_|
//                     |_____|
//
//-------------------------------------------------------------------------
// (c) 2003 Altium
// Started: 17.11.2003 Ch.Weimann
//          09.01.04 Ch.W.: changed MAX5841 I2c base address from V1.06 onwards
// MAX1037 D/A and MAX 5841 A/D Routines
// the Altium Nanoboard Tester
//-------------------------------------------------------------------------
#ifndef __NBT_ADDA_H__
#define __NBT_ADDA_H__

//--------------------------------------------------------------------------------------
//       __  __     _    __  __ ____    ___   _  _    _     ____     __ _
//      |  \/  |   / \   \ \/ /| ___|  ( _ ) | || |  / |   |  _ \   / // \
//      | |\/| |  / _ \   \  / |___ \  / _ \ | || |_ | |   | | | | / // _ \
//      | |  | | / ___ \  /  \  ___) || (_) ||__   _|| |   | |_| |/ // ___ \
//      |_|  |_|/_/   \_\/_/\_\|____/  \___/    |_|  |_|   |____//_//_/   \_\
//
//--------------------------------------------------------------------------------------

#define Max5841_LoadA_UpdateAll     0x00
#define Max5841_LoadB_UpdateAll     0x10
#define Max5841_LoadC_UpdateAll     0x20
#define Max5841_LoadD_UpdateAll     0x30

#define Max5841_LoadA               0x40
#define Max5841_LoadB               0x50
#define Max5841_LoadC               0x60
#define Max5841_LoadD               0x70

#define Max5841_UpdateAllAndLoadA   0x80
#define Max5841_UpdateAllAndLoadB   0x90
#define Max5841_UpdateAllAndLoadC   0xA0
#define Max5841_UpdateAllAndLoadD   0xB0

#define Max5841_LoadAndUpdateAll    0xC0
#define Max5841_LoadAll             0xD0
#define Max5841_UpdateAll           0xE0
                                        
#define Max5841_ExtendedCommandMode 0xF0
#define Max5841_ReadA               0xF1
#define Max5841_ReadB               0xF2
#define Max5841_ReadC               0xF4
#define Max5841_ReadD               0xF8

// Power down Command Bits
#define Max5841_PowerDown_Off       0x00
#define Max5841_PowerDown_Float     0x01
#define Max5841_PowerDown_1k        0x02
#define Max5841_PowerDown_100k      0x03

// #define Max5841_Addr                0x3D  // for MAX5841L Vadd= Vdd
#define Max5841_Addr                0x5D  // for MAX5841M Vadd= Vdd from V1.06 on wards



//----------------------------------------------------------------
// Initialises the DAC to power up all four channels
// returns : ACK for success
//         : NACK for missing ACK from DAC chip
//----------------------------------------------------------------
unsigned char DAC_Init(void);

//----------------------------------------------------------------------
// Outputs 'DAC_Value' to DAC_Cannel [0..3]
// Performs all bis mangling and calls DAC_Send_Bytes to shift them out
// Output Voltage is (Vref * 'DAC_Value')/2048
// returns : ACK for success
//         : NACK for missing ACK from DAC chip
//----------------------------------------------------------------------
unsigned char DAC_OutData(unsigned char DAC_Channel, unsigned int DAC_Value);


//--------------------------------------------------------------------------------------
//     __  __     _    __  __  _   ___  _____ _____      _        __ ____
//    |  \/  |   / \   \ \/ / / | / _ \|___ /|___  |    / \      / /|  _ \
//    | |\/| |  / _ \   \  /  | || | | | |_ \   / /    / _ \    / / | | | |
//    | |  | | / ___ \  /  \  | || |_| |___) | / /    / ___ \  / /  | |_| |
//    |_|  |_|/_/   \_\/_/\_\ |_| \___/|____/ /_/    /_/   \_\/_/   |____/
//
//--------------------------------------------------------------------------------------


#define Max1037_Addr     0x64
//--------------------------------------------------------------------------------------
//  Max1037 Setup Register Format
//  -----------------------------
//  B7 = RegisterBit 1 => Setup Byte    0 => Configuration Byte
//  B6 = Sel2        | 
//  B5 = Sel1        | Ref voltage setup
//  B4 = Sel0        |
//  B3 = CLK Bit     1 => External Clock 0 => Internal Clock
//  B2 = BIP/UNI     1 => Bipolar       0 => Unipolar
//  B1 = Reset       1 => No Action     0 => Reset configuration register to default
//  B0 = X           Don't Care

#define Max1037_Setup            0x80
#define Max1037_Clock_External   0x08
#define Max1037_Clock_Internal   0x00
#define Max1037_Mode_Bipolar     0x04
#define Max1037_Mode_UniPolar    0x00
#define Max1037_NoReset          0x02
#define Max1037_ForceResetConfig 0x00
//                                       // Ref Volts    AIN/REF              Internal Reference State 
//                                       // ----------------------------------------------------------
#define Max1037_VRef_VDD          0x00   // VDD          Analog Input         Always Off
#define Max1037_VRef_External     0x20   // External     Reference Input      Always Off 
#define Max1037_VRef_AutoShutdown 0x40   // Internal     Analog Input         Auto Shutdown
#define Max1037_VRef_AlwaysOn     0x50   // Internal     Analog Input         Always On 
#define Max1037_VRef_Generate     0x70   // Internal     Reference Output     Always On

//  Max1037 Config Register Format
//  ------------------------------
//  B7 = RegisterBit 1 => Setup Byte    0 => Configuration Byte
//  B6 = Scan1       | Scan Select Bits 
//  B5 = Scan0       | 
//  B4  CS3          | Channel select bits - only CS0 and CS1 are used on the 4-Channel MAX1037
//  B3 = CS2         | 
//  B2 = CS1         |
//  B1 = CS0         | 
//  B0 = SGL/DIF     1 => Single-Ended  0 => Pseudo-Differential

#define Max1037_Config     0x60
#define Max1037_Channel0   0x00
#define Max1037_Channel1   0x02
#define Max1037_Channel2   0x04
#define Max1037_Channel3   0x06

#define Max1037_Mode_SingleEnded    0x01
#define Max1037_Mode_Differential   0x00
//--------------------------------------------------------------------------------------

//--------------------------------------------------------------------------------------
//#define Max1037_Default_Setup       (Max1037_Setup  | Max1037_Clock_Internal | Max1037_Mode_UniPolar | Max1037_VRef_Generate | Max1037_NoReset)
#define Max1037_Default_Setup       (Max1037_Setup  | Max1037_Clock_Internal | Max1037_Mode_UniPolar | Max1037_VRef_VDD | Max1037_NoReset)
#define Max1037_Default_Config      (Max1037_Config | Max1037_Mode_SingleEnded)
//--------------------------------------------------------------------------------------

//--------------------------------------------------------------------------------------
#define Max1037_Config_Channel0     (Max1037_Default_Config | Max1037_Channel0)
#define Max1037_Config_Channel1     (Max1037_Default_Config | Max1037_Channel1)
#define Max1037_Config_Channel2     (Max1037_Default_Config | Max1037_Channel2)
#define Max1037_Config_Channel3     (Max1037_Default_Config | Max1037_Channel3)
//--------------------------------------------------------------------------------------

#define Max1037_MAX_SCL_WAIT   0x3FF  // maximum number of delay loopcycles for SCL after read

//--------------------------------------------------------------------------------------
// ADC read Error Codes
#define ADC_NO_ACK        0xFFFF  // No I2C Ack received
#define ADC_READ_TIMEOUT  0xFFFE  // timeout on SCL release


//----------------------------------------------------------------
// Initialises the ADC: 
// returns : ACK for success
//         : NACK for missing ACK from DAC chip
//----------------------------------------------------------------
unsigned char ADC_Init(void);

//----------------------------------------------------------------
// Sets ADC up for single ended inputs and selects input 'Channel' [0..3]
// returns : ACK for success
//         : NACK for missing ACK from DAC chip
//----------------------------------------------------------------
unsigned char ADC_Config(unsigned char Channel);

//----------------------------------------------------------------
// Performs single conversion on selects channel and returns value
// ADC is only 8 bits wide, so results are returns << 2
// bottom two bits are always 0
// if result if greater than 0x3FC an error has occurred:
//     ADC_NO_ACK        (0xFFFF)  -> No I2C Ack received
//     ADC_READ_TIMEOUT  (0xFFFE)  -> timeout on SCL release
//----------------------------------------------------------------
unsigned int ADC_Read(void);

//-----------------------------------------------------------------
// Assumes that ADC[0..4] is looped to DAC[0..4]
// Assumes Reference voltage is set to 3V3
// Outputs test voltages on each DS/A channel sucessively and checks that
// the corresponding A/D Channel reads back the same value within
// the tolerance +-'Tolerance' bits;
// Test voltages are stored in a 0-terminated array of integers
// 1 bit = 2mV
// All other Channels are checked to be within 'Tolerance' of 0;
// Returns: 0    : Success
//          Non-0: Bitpattern of offending channels
//                 (eg Bit2 set -> faulty Channel 2)
//---------------------------------------------------------------------------
unsigned char DAC_ADC_Test(unsigned int Tolerance, unsigned int * PTestValues);


#endif // __NBT_ADDA_H__

