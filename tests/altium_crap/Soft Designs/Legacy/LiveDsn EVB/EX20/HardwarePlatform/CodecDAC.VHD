-------------------------------------------------------------------------------
Library IEEE;
Use IEEE.Std_Logic_1164.all;
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
Entity CodecDriver_DAC Is Port
   (
     CLK        : In  Std_Logic;
     RST        : In  Std_Logic;
     D          : In  Std_Logic_Vector(7 downto 0);
     Codec_DOUT : Out Std_Logic;
     Codec_SCLK : Out Std_Logic;
     Codec_nCS  : Out Std_Logic;
     Codec_DIN  : In  Std_Logic
   );
End CodecDriver_DAC;
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
Architecture RTL Of CodecDriver_DAC Is

-------------------------------------------------------------------------------
Subtype  State_TYPE is Std_Logic_Vector (3 downto 0);
Signal   State   :  State_TYPE;
Signal   Return1 :  State_TYPE;      --Return address for subroutine

Constant Configure                       : State_TYPE := "0000";
Constant Configure_Start                 : State_TYPE := "0001";
Constant Configure_SendConfigurationByte : State_TYPE := "0010";
Constant LatchDataIn                     : State_TYPE := "0011";
Constant ProcessByte                     : State_TYPE := "0100";
Constant SPI_sendReceive                 : State_TYPE := "0101";
Constant SPI_sendBit_out                 : State_TYPE := "0110";
Constant SPI_sendBit_in                  : State_TYPE := "0111";
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
subtype  Byte is Std_Logic_Vector (7 downto 0);
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
Constant cAudio_START                   : Byte := X"80";   --Start of control byte
Constant cAudio_DAC_Addressed           : Byte := X"40";   --A1  DAC_Addressed - The control byte configures the DAC
Constant cAudio_ADC_Addressed           : Byte := X"20";   --A0  ADC_Addressed - The control byte configures the ADC
Constant cAudio_ADC_InputC1             : Byte := X"10";   --C1  ADC_Input to VDD/2 to measure supply voltage (MAX1102-MAX1103 only)
Constant cAudio_ContinuousConversion    : Byte := X"08";   --C0  Mode_ContinuousConversion
Constant cAudio_EnableReferenceVoltage  : Byte := X"04";   --E2  EnableReferenceVoltage - don't care for MAX1104 so zero
Constant cAudio_ADC_Enable              : Byte := X"02";   --E1
Constant cAudio_DAC_Enable              : Byte := X"01";   --E0
Constant cAudio_ContinuousDAC           : Byte := cAudio_START                Or
                                                  cAudio_DAC_Addressed        Or
                                                  cAudio_ContinuousConversion Or
                                                  cAudio_DAC_Enable;
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
Signal CountReg          : Integer range 0 to 7;            -- Bit send/receive Counter
Signal spiReg            : Std_Logic_Vector (7 downto 0);   -- Bit In/Out shift register

Signal Codec_SCLK_buffer : Std_Logic;                       -- Internal Codec_ClK Signal;
Signal Codec_DIN_buffer  : Std_Logic;                       -- Internal DATA In Signal;
Signal Codec_DOUT_buffer : Std_Logic;                       -- Internal DATA Out Signal;
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
Begin
    Codec_SCLK       <= Codec_SCLK_buffer;
    Codec_DOUT       <= Codec_DOUT_buffer;
    Codec_DIN_buffer <= Codec_DIN;

    FSM: Process (State,RST,CLK)
    Begin
       If RST = '1' then
          State   <= Configure;
       ElsIf (CLK'event) and (CLK = '1') Then
           Case State is
               When Configure =>
                    Codec_nCS <= '1';
                    State     <= Configure_Start;

               When Configure_Start=>
                    Codec_nCS <= '0';
                    State     <= Configure_SendConfigurationByte;

               When Configure_SendConfigurationByte=>
                    spiReg    <= cAudio_ContinuousDAC;
                    State     <= SPI_sendReceive;
                    return1   <= LatchDataIn;

               When LatchDataIn =>
                    spiReg    <= D;
                    State     <= ProcessByte;

               When ProcessByte =>
                    State     <= SPI_sendReceive;
                    return1   <= LatchDataIn;

               -------------------------------------------------
               -- Subroutine:  SPI_SendReceive (spiReg) : spiReg
               -- Postcondition Codec_SCLK_buffer = '0'
               -------------------------------------------------
               When SPI_sendReceive =>
                    CountReg          <= 0;
                    Codec_DOUT_buffer <= spiReg(7);
                    State             <= SPI_sendBit_out;                -- Send MSB to DOUT

               When SPI_sendBit_out =>
                    spiReg            <= spiReg(6 DownTo 0) & Codec_DIN;  -- Shift spiReg left and put DIN Into LSB
                    Codec_SCLK_buffer <= '1';
                    CountReg          <= CountReg-1;
                    State             <= SPI_sendBit_in;

               When SPI_sendBit_in =>
                    Codec_DOUT_buffer <= spiReg(7);                      -- Next bit ready
                    Codec_SCLK_buffer <= '0';
                    If countReg = 0
                       Then State <= Return1;                            -- Exit Subroutine
                       Else State <= SPI_sendBit_out;
                    End If;
               -------------------------------------------------

               When Others =>
                   State <= Configure;
           End Case;
       End If;
    End Process;
End RTL;
-------------------------------------------------------------------------------

