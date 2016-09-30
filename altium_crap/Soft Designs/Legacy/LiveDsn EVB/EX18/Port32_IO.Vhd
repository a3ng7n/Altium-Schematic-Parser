--------------------------------------------------------------------------------
Library IEEE;
Use IEEE.Std_Logic_1164.All;
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
Entity Port32_IO Is port
   (
     CLK_I     : In  Std_Logic;
     RST_I     : In  Std_Logic;

     CYC_I     : In  Std_Logic;
     STB_I     : In  Std_Logic;
     ACK_O     : Out Std_Logic;
     WE_I      : In  Std_Logic;

     DAT_O     : Out Std_Logic_Vector(31 DownTo 0);
     DAT_I     : In  Std_Logic_Vector(31 DownTo 0);

     ADR_I     : In  Std_Logic_Vector(5 DownTo 0);
     SEL_I     : in  Std_Logic_Vector( 3 DownTo 0);

     Port_DSP_In  : In  Std_Logic_Vector(31 DownTo 0);
     Port_DSP_Out : Out Std_Logic_Vector(31 DownTo 0);
     Port_START_Out : Out Std_Logic_Vector(3 downto 0);
     Port_LED_Out :  Out Std_Logic_Vector(31 DownTo 0);

     Port_KBD_In  : In  Std_Logic_Vector(1 DownTo 0);

     Port0_SA_Out : Out Std_Logic_Vector(31 DownTo 0);     -- Sample Start Address
     Port0_EA_Out : Out Std_Logic_Vector(31 DownTo 0);     -- Sample End   Address
     Port0_SR_Out : Out Std_Logic_Vector(31 DownTo 0);     -- Repeat Start Address
     Port0_ER_Out : Out Std_Logic_Vector(31 DownTo 0);     -- Repeat End Address
     Port0_VO_Out : Out Std_Logic_Vector(5 DownTo 0);      -- Sample Volume
     Port0_CT_Out : Out Std_Logic_Vector(31 DownTo 0);     -- Sample Control
     Port0_DR_Out : Out Std_Logic_Vector(15 DownTo 0);     -- Divider Ratio

     Port1_SA_Out : Out Std_Logic_Vector(31 DownTo 0);     -- Sample Start Address
     Port1_EA_Out : Out Std_Logic_Vector(31 DownTo 0);     -- Sample End   Address
     Port1_SR_Out : Out Std_Logic_Vector(31 DownTo 0);     -- Repeat Start Address
     Port1_ER_Out : Out Std_Logic_Vector(31 DownTo 0);     -- Repeat End Address
     Port1_VO_Out : Out Std_Logic_Vector(5 DownTo 0);      -- Sample Volume
     Port1_CT_Out : Out Std_Logic_Vector(31 DownTo 0);     -- Sample Comtrol
     Port1_DR_Out : Out Std_Logic_Vector(15 DownTo 0);     -- Divider Ratio

     Port2_SA_Out : Out Std_Logic_Vector(31 DownTo 0);     -- Sample Start Address
     Port2_EA_Out : Out Std_Logic_Vector(31 DownTo 0);     -- Sample End   Address
     Port2_SR_Out : Out Std_Logic_Vector(31 DownTo 0);     -- Repeat Start Address
     Port2_ER_Out : Out Std_Logic_Vector(31 DownTo 0);     -- Repeat End Address
     Port2_VO_Out : Out Std_Logic_Vector(5 DownTo 0);      -- Sample Volume
     Port2_CT_Out : Out Std_Logic_Vector(31 DownTo 0);     -- Sample Control
     Port2_DR_Out : Out Std_Logic_Vector(15 DownTo 0);     -- Divider Ratio

     Port3_SA_Out : Out Std_Logic_Vector(31 DownTo 0);     -- Sample Start Address
     Port3_EA_Out : Out Std_Logic_Vector(31 DownTo 0);     -- Sample End   Address
     Port3_SR_Out : Out Std_Logic_Vector(31 DownTo 0);     -- Repeat Start Address
     Port3_ER_Out : Out Std_Logic_Vector(31 DownTo 0);     -- Repeat End Address
     Port3_VO_Out : Out Std_Logic_Vector(5 DownTo 0);      -- Sample Volume
     Port3_CT_Out : Out Std_Logic_Vector(31 DownTo 0);     -- Sample Comtrol
     Port3_DR_Out : Out Std_Logic_Vector(15 DownTo 0)      -- Divider Ratio
   );
End Port32_IO;
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
Architecture RTL Of Port32_IO Is
    Signal ACK          : Std_Logic;
    Signal DoRead       : Std_Logic;
    Signal DoWrite      : Std_Logic;
    Signal PortID       : Std_Logic_Vector( 5 DownTo 0);

Begin
    ACK_O      <= ACK;
    DoRead     <= STB_I And CYC_I And (Not WE_I) ;
    DoWrite    <= STB_I And CYC_I And (    WE_I) ;
    PortID     <= ADR_I(5 DownTo 0);


    GenerateACK:
    Process(CLK_I,RST_I,ACK,STB_I,CYC_I)
    Begin
        If Rising_Edge(CLK_I) Then
            If RST_I = '1' Then
               ACK <= '0';
            ElsIf ACK = '0' Then                           -- If not in a current cycle.
               ACK <= CYC_I And STB_I;                     -- If wishbone cycle started then acknowledge it.
            Else
               ACK <= '0';                                 -- Else back to zero.
            End If;
        End If;
    End Process;

    DriveOutputRegisters:
    Process(CLK_I,RST_I,DAT_I,DoWrite)
    Begin
        If Rising_Edge(CLK_I) Then
            If RST_I = '1' Then
               Port0_SA_Out <= (Others => '0');
               Port0_EA_Out <= (Others => '0');
               Port0_SR_Out <= (Others => '0');
               Port0_ER_Out <= (Others => '0');
               Port0_VO_Out <= (Others => '0');
               Port0_CT_Out <= (Others => '0');
               Port0_DR_Out <= (Others => '0');

               Port1_SA_Out <= (Others => '0');
               Port1_EA_Out <= (Others => '0');
               Port1_SR_Out <= (Others => '0');
               Port1_ER_Out <= (Others => '0');
               Port1_VO_Out <= (Others => '0');
               Port1_CT_Out <= (Others => '0');
               Port1_DR_Out <= (Others => '0');

               Port2_SA_Out <= (Others => '0');
               Port2_EA_Out <= (Others => '0');
               Port2_SR_Out <= (Others => '0');
               Port2_ER_Out <= (Others => '0');
               Port2_VO_Out <= (Others => '0');
               Port2_CT_Out <= (Others => '0');
               Port2_DR_Out <= (Others => '0');

               Port3_SA_Out <= (Others => '0');
               Port3_EA_Out <= (Others => '0');
               Port3_SR_Out <= (Others => '0');
               Port3_ER_Out <= (Others => '0');
               Port3_VO_Out <= (Others => '0');
               Port3_CT_Out <= (Others => '0');
               Port3_DR_Out <= (Others => '0');

               Port_DSP_Out   <= (Others => '0');
               Port_START_Out <= (Others => '0');
               Port_LED_Out   <= (Others => '0');

            ElsIf DoWrite = '1' Then
               Case PortID Is
                  When "000000"   => Port0_SA_Out <= DAT_I;
                  When "000001"   => Port0_EA_Out <= DAT_I;
                  When "000010"   => Port0_SR_Out <= DAT_I;
                  When "000011"   => Port0_ER_Out <= DAT_I;
                  When "000100"   => Port0_VO_Out <= DAT_I(5 downto 0);
                  When "000101"   => Port0_CT_Out <= DAT_I;
                  When "000110"   => Port0_DR_Out <= DAT_I(15 downto 0);

                  When "001000"   => Port1_SA_Out <= DAT_I;
                  When "001001"   => Port1_EA_Out <= DAT_I;
                  When "001010"   => Port1_SR_Out <= DAT_I;
                  When "001011"   => Port1_ER_Out <= DAT_I;
                  When "001100"   => Port1_VO_Out <= DAT_I(5 downto 0);
                  When "001101"   => Port1_CT_Out <= DAT_I;
                  When "001110"   => Port1_DR_Out <= DAT_I(15 downto 0);

                  When "010000"   => Port2_SA_Out <= DAT_I;
                  When "010001"   => Port2_EA_Out <= DAT_I;
                  When "010010"   => Port2_SR_Out <= DAT_I;
                  When "010011"   => Port2_ER_Out <= DAT_I;
                  When "010100"   => Port2_VO_Out <= DAT_I(5 downto 0);
                  When "010101"   => Port2_CT_Out <= DAT_I;
                  When "010110"   => Port2_DR_Out <= DAT_I(15 downto 0);

                  When "011000"   => Port3_SA_Out <= DAT_I;
                  When "011001"   => Port3_EA_Out <= DAT_I;
                  When "011010"   => Port3_SR_Out <= DAT_I;
                  When "011011"   => Port3_ER_Out <= DAT_I;
                  When "011100"   => Port3_VO_Out <= DAT_I(5 downto 0);
                  When "011101"   => Port3_CT_Out <= DAT_I;
                  When "011110"   => Port3_DR_Out <= DAT_I(15 downto 0);

                  When "100000"   => Port_DSP_Out  <= DAT_I;
                  WHEN "100010"   => Port_Start_Out <= DAT_I(3 downto 0);
                  WHEN "100011"   => Port_LED_Out <= DAT_I;
                  When Others => NULL;
               End Case;
            End If;
        End If;
    End Process;

    MuxInputs:
    Process(PortID,Port_DSP_In,Port_KBD_In)
    Begin
      -- Add Address selection for different input ports here.
      Case PortID Is
         When "100000"   => DAT_O <= Port_DSP_In;
         When "100001"   => DAT_O <= X"000000"&"000000"&Port_KBD_In;
         When Others     => DAT_O <= (others => '0');
      End Case;
    End Process;

End RTL;
--------------------------------------------------------------------------------

