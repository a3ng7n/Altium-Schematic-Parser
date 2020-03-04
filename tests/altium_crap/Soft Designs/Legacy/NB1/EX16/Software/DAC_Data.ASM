;...............................................................................
    .section data1, data
CountInner      .dsb    1
CountOuter      .dsb    1
BitCounter      .dsb    1
DacData1        .dsb    1
DacData2        .dsb    1
DataByte        .dsb    1
I2C_Flags       .dsb    1
CommandByte1    .dsb    1
CommandByte2    .dsb    1
ValueLo         .dsb    1
ValueHi         .dsb    1
DacChannel      .dsb    1     
I2C_Address     .dsb    1           
AddressByte2    .dsb    1     ; MSB Of 24 bit address
AddressByte1    .dsb    1     ;
AddressByte0    .dsb    1     ; LSB Of 24 bit address
AdcChannel      .dsb    1 
TenK            .equ    AdcChannel
Thou            .dsb    1
Hund            .dsb    1
Tens            .dsb    1
Ones            .dsb    1
TableIndex      .dsb    1 
Temp            .dsb    1 
StringOffset    .dsb    1    ; String lookup table offset
TempW           .equ    BitCounter
;...............................................................................

;...............................................................................
Scratch0        .equ    0x30           ;First Byte of second data page
Scratch1        .equ    0x31   
Scratch2        .equ    0x32   
Scratch3        .equ    0x33   
SaveValueLoA    .equ    0x34 
SaveValueLoB    .equ    0x35 
SaveValueHiA    .equ    0x36 
SaveValueHiB    .equ    0x37 
;...............................................................................

;...............................................................................
        .global CountInner    
        .global CountOuter    
        .global BitCounter    
        .global DacData1      
        .global DacData2      
        .global DataByte      
        .global I2C_Flags     
        .global CommandByte1  
        .global CommandByte2  
        .global ValueLo       
        .global ValueHi       
        .global DacChannel    
        .global I2C_Address   
        .global AddressByte2  
        .global AddressByte1  
        .global AddressByte0  
        .global AdcChannel    
        .global TableIndex
        .global Temp
        .global StringOffset
        .global TenK          
        .global Thou          
        .global Hund          
        .global Tens          
        .global Ones          
        .global TempW
        .global Scratch0            
        .global Scratch1            
        .global Scratch2            
        .global Scratch3            
;...............................................................................

END
