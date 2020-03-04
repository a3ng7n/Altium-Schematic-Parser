Library IEEE;

--------------------------------------------------------------------------------
Use IEEE.Std_Logic_1164.all;
Use IEEE.Std_Logic_Unsigned.All;
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
Entity V_Trace is
    Port (
           ---------------------------------------------------------------------
           -- Wishbone Slave Port - Never Changes
           ---------------------------------------------------------------------
           CLK_I      : In    Std_Logic;
           RST_I      : In    Std_Logic;
           CYC_I      : In    Std_Logic;
           STB_I      : In    Std_Logic;
           ACK_O      : Out   Std_Logic;
           ADR_I      : In    Std_Logic_Vector(13 DownTo 0);
           DAT_I      : In    Std_Logic_Vector(31 DownTo 0);
           DAT_O      : Out   Std_Logic_Vector(31 DownTo 0);
           WE_I       : In    Std_Logic;
           SEL_I      : In    Std_Logic_Vector( 3 DownTo 0);

           -- MCU Port to Dual-Port RAM
           PC_ADDRA    : Out   Std_Logic_Vector(10 DownTo 0);
           PC_ByteWEA  : Out   Std_Logic_Vector( 3 DownTo 0);
           PC_DINA     : Out   Std_Logic_Vector(31 DownTo 0);
           PC_DOUTA    : In    Std_Logic_Vector(31 DownTo 0);
           PC_ENA      : Out   Std_Logic;
           PC_WEA      : Out   Std_Logic;

           -- Trace Port to Dual-Port RAM
           PC_ADDRB    : Out   Std_Logic_Vector(10 DownTo 0);
           PC_ByteWEB  : Out   Std_Logic_Vector( 3 DownTo 0);
           PC_DINB     : Out   Std_Logic_Vector(31 DownTo 0);
           PC_DOUTB    : In    Std_Logic_Vector(31 DownTo 0);
           PC_ENB      : Out   Std_Logic;
           PC_WEB      : Out   Std_Logic;

           -- MCU Port to Dual-Port RAM
           SP_ADDRA    : Out   Std_Logic_Vector(10 DownTo 0);
           SP_ByteWEA  : Out   Std_Logic_Vector( 3 DownTo 0);
           SP_DINA     : Out   Std_Logic_Vector(31 DownTo 0);
           SP_DOUTA    : In    Std_Logic_Vector(31 DownTo 0);
           SP_ENA      : Out   Std_Logic;
           SP_WEA      : Out   Std_Logic;

           -- Trace Port to Dual-Port RAM
           SP_ADDRB    : Out   Std_Logic_Vector(10 DownTo 0);
           SP_ByteWEB  : Out   Std_Logic_Vector( 3 DownTo 0);
           SP_DINB     : Out   Std_Logic_Vector(31 DownTo 0);
           SP_DOUTB    : In    Std_Logic_Vector(31 DownTo 0);
           SP_ENB      : Out   Std_Logic;
           SP_WEB      : Out   Std_Logic;

           TracePC     : In    Std_Logic_Vector(31 DownTo 0); -- Program counter in decode stage.
           TraceSP     : In    Std_Logic_Vector(31 DownTo 0); -- Stack pointer in Regs[29]
           TraceRun    : In    Std_Logic                      -- Low if the processor is stopped.
         );                                                   -- High if stepping, running etc
End V_Trace;
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
Architecture Structure of V_Trace is
    Signal BRAM_DI        : Std_Logic_Vector(31 DownTo 0);
    Signal BRAM_DO        : Std_Logic_Vector(31 DownTo 0);
    Signal BRAM_A         : Std_Logic_Vector(12-1 DownTo 0);
    Signal BRAM_WE        : Std_Logic;
    Signal BRAM_EN        : Std_Logic;
    Signal BRAM_BW        : Std_Logic_Vector( 3 DownTo 0);


    Signal TraceCounter   : Std_Logic_Vector(10 DownTo 0);
    Signal TraceAddress   : Std_Logic_Vector(31 DownTo 0);
    Signal ReadAddrZero   : Std_Logic_Vector(10 DownTo 0);
    Signal FirstDataAddr  : Std_Logic_Vector(10 DownTo 0);
    Signal LastDataAddr   : Std_Logic_Vector(10 DownTo 0);
    Signal CaptureEnable  : Std_Logic;
    Signal EN             : Std_Logic;
    Signal ACK            : Std_Logic;
Begin
    ----------------------------------------------------------------------------
    ACK_O <= ACK;
    EN    <= STB_I And CYC_I;
    ----------------------------------------------------------------------------

    ----------------------------------------------------------------------------
    -- Trace Address so host can work out when the last PC was located in the
    -- circular buffer
    TraceAddress(10 DownTo  0) <= TraceCounter;
    TraceAddress(28 DownTo 11) <= (Others => '0');
    TraceAddress(31 DownTo 29) <= (Others => '0'); --CaptureEnable & TraceRun & TraceEnableFlag;

    ReadAddrZero  <= (Others => '0');
    FirstDataAddr <= "00000000001";   -- PC data stored from 1 to End of Mem
    LastDataAddr  <= "11111111111";   -- MCU read from zero returns the trace address
    ----------------------------------------------------------------------------

    ----------------------------------------------------------------------------
    MakeACK:
    Process(CLK_I)
    Begin
        If Rising_Edge(CLK_I) Then
            If ACK = '1' Then
                ACK <= '0';
            Else
                ACK <= EN;
            End If;
        End If;
    End Process;
    ----------------------------------------------------------------------------

    ----------------------------------------------------------------------------
    DataOutMux:
    Process(ADR_I,ReadAddrZero,TraceAddress,PC_DOUTA,SP_DOUTA)
    Begin
        If ADR_I = ReadAddrZero Then
           DAT_O <= TraceAddress;
        Else
           If ADR_I(13) = '0' Then
              DAT_O <= PC_DOUTA;
           Else
              DAT_O <= SP_DOUTA;
           End If;
        End If;
    End Process;
    ----------------------------------------------------------------------------

    ----------------------------------------------------------------------------
    -- Connection between MCU and Memory Port A for PC
    PC_ADDRA               <= ADR_I(12 DownTo 2);
    PC_ByteWEA             <= SEL_I;
    PC_DINA                <= DAT_I;
    PC_ENA                 <= EN And (Not ADR_I(13));
    PC_WEA                 <= WE_I;
    ----------------------------------------------------------------------------

    ----------------------------------------------------------------------------
    -- Connection between MCU and Memory Port A for SP
    SP_ADDRA               <= ADR_I(12 DownTo 2);
    SP_ByteWEA             <= SEL_I;
    SP_DINA                <= DAT_I;
    SP_ENA                 <= EN And ADR_I(13);
    SP_WEA                 <= WE_I;
    ----------------------------------------------------------------------------

    ----------------------------------------------------------------------------
    -- Connection trace hardware and Memory Port B for PC
    PC_ADDRB               <= TraceCounter;
    PC_ByteWEB             <= "1111";
    PC_DINB                <= TracePC;
    --                     <= DOUTA;   //Not Connected - never read from this side
    PC_ENB                 <= '1';
    PC_WEB                 <= CaptureEnable;
    ----------------------------------------------------------------------------

    ----------------------------------------------------------------------------
    -- Connection trace hardware and Memory Port B for PC
    SP_ADDRB               <= TraceCounter;
    SP_ByteWEB             <= "1111";
    SP_DINB                <= TraceSP;
    --                     <= DOUTA;   //Not Connected - never read from this side
    SP_ENB                 <= '1';
    SP_WEB                 <= CaptureEnable;
    ----------------------------------------------------------------------------

    ----------------------------------------------------------------------------
    CaptureEnable <= TraceRun And (Not RST_I);
    ----------------------------------------------------------------------------

    ----------------------------------------------------------------------------
    TraceCounter_Proc:
    Process(RST_I,CLK_I,TraceRun,LastDataAddr)
    Begin
        If Rising_Edge(CLK_I) Then
           If (RST_I = '1') Or (TraceCounter = LastDataAddr) Then
              TraceCounter <= FirstDataAddr;
           Elsif CaptureEnable = '1' Then
              TraceCounter <= TraceCounter + 1;
           End If;
        End If;
    End Process;
    ----------------------------------------------------------------------------

End Structure;
--------------------------------------------------------------------------------
