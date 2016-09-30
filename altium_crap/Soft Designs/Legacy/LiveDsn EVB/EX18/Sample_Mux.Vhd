--------------------------------------------------------------------------------
-- SubModule SAMPLE_MUX
-- Created   24/11/2004 11:24:35 AM
--------------------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;
use IEEE.std_logic_arith.all;

entity SAMPLE_MUX is
port
   (
     S0_ADR_I           : in    std_logic_vector(31 downto 0);
     S0_RD_I            : in    std_logic;
     S0_VOL_I           : in    std_logic_vector(5 downto 0);
     S1_ADR_I           : in    std_logic_vector(31 downto 0);
     S1_RD_I            : in    std_logic;
     S1_VOL_I           : in    std_logic_vector(5 downto 0);
     S2_ADR_I           : in    std_logic_vector(31 downto 0);
     S2_RD_I            : in    std_logic;
     S2_VOL_I           : in    std_logic_vector(5 downto 0);
     S3_ADR_I           : in    std_logic_vector(31 downto 0);
     S3_RD_I            : in    std_logic;
     S3_VOL_I           : in    std_logic_vector(5 downto 0);

     CLK_I              : in    std_logic;
     BUSY_I             : in    std_logic;

     ENABLE             : in    std_logic_vector(3 downto 0);

     WBR_DTA8_I         : in    std_logic_vector(7 downto 0);

     WBR_ADR            : out   std_logic_vector(31 downto 0);
     WB_RD              : out   std_logic;

     DSP_OUT            : out   std_logic_vector(31 downto 0);

     AUDIO_OUT_L        : out   std_logic_vector(15 downto 0);
     AUDIO_OUT_R        : out   std_logic_vector(15 downto 0)
   );
end SAMPLE_MUX;
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
architecture structure of SAMPLE_MUX is

signal ADR : std_logic_vector(31 downto 0);
signal  RD : std_logic;

type SEQ_STATE_TYPE is ( Check0, Read0, BusyWait0, Check1, Read1, BusyWait1,
                         Check2, Read2, BusyWait2, Check3, Read3, BusyWait3);
signal SEQ_STATE: SEQ_STATE_TYPE;

type AVG_STATE_TYPE is ( Original, Calculated);
signal AVG_STATE : AVG_STATE_TYPE;

signal V0,V1,V2,V3 :std_logic_vector(15 downto 0);
signal MixedL, MixedR : std_logic_vector(15 downto 0);

signal PendingFlags    : std_logic_vector(3 downto 0);
signal ReadActiveFlags : std_logic_vector(3 downto 0);
signal OldRead         : std_logic_vector(3 downto 0);

signal RD_Latch0,RD_Latch1,RD_Latch2,RD_Latch3 : std_logic_vector(7 downto 0);
signal Out_Latch0,Out_Latch1,Out_Latch2,Out_Latch3 : std_logic_vector(7 downto 0);


 procedure HandlePendingFlag(signal CLK: in std_logic;
  RD, ReadFlag:  in std_logic; signal Old_Rd : inout std_logic; signal Pending: out std_logic;
  RD_Latch: in std_logic_vector(7 downto 0);  signal Out_Latch : out std_logic_vector(7 downto 0))  is
 begin
     if(rising_edge(CLK)) then
       if (ReadFlag = '1') then
         Pending <= '0';
       elsif (RD /= Old_RD) then
         Old_RD <= RD;
         if(RD = '1') then
           Pending <= '1';
           Out_Latch <= RD_Latch;
         end if;
       end if;
     end if;
  end procedure  HandlePendingFlag;

begin

  HandlePendingFlag(CLK_I, S0_RD_I, ReadActiveFlags(0), OldRead(0), PendingFlags(0), RD_Latch0, Out_Latch0);
  HandlePendingFlag(CLK_I, S1_RD_I, ReadActiveFlags(1), OldRead(1), PendingFlags(1), RD_Latch1, Out_Latch1);
  HandlePendingFlag(CLK_I, S0_RD_I, ReadActiveFlags(2), OldRead(2), PendingFlags(2), RD_Latch2, Out_Latch2);
  HandlePendingFlag(CLK_I, S1_RD_I, ReadActiveFlags(3), OldRead(3), PendingFlags(3), RD_Latch3, Out_Latch3);

  SEQUENCER_FSM: process (CLK_I, SEQ_STATE, PendingFlags, WBR_DTA8_I) is
  begin
    if(rising_edge(CLK_I)) then
      case SEQ_STATE is
-- channel 0
        when Check0 =>
          ReadActiveFlags <= "0000";
          if(Enable(0) = '1') then
            RD <= '0';
            ADR <= S0_ADR_I;
            if(PendingFlags(0) = '1') then                 -- read flags pending for this channel?
              RD <= '1';
              SEQ_STATE <= Read0;                          -- yes -> read
            else
              SEQ_STATE <= Check1;                         -- no -> try next channel
            end if;
          else
            SEQ_STATE <= Check1;
          end if;
        when Read0 =>                                      -- initiate read
          ReadActiveFlags <= "0001";
          RD <= '0';
          SEQ_STATE <= BusyWait0;
        when BusyWait0 =>                                  -- wait for end of read sequence
          ReadActiveFlags <= "0000";
          RD <= '0';
          if(BUSY_I = '0') then
            RD_Latch0 <= WBR_DTA8_I;
            ADR       <= S1_ADR_I;
            SEQ_STATE <= Check1;
          end if;
-- channel 1
        when Check1 =>
          ReadActiveFlags <= "0000";
          if(Enable(1) = '1') then
            RD <= '0';
            ADR <= S1_ADR_I;
            if(PendingFlags(1) = '1') then                 -- read flags pending for this channel?
              RD <= '1';
              SEQ_STATE <= Read1;                          -- yes -> read
            else
              SEQ_STATE <= Check2;                         -- no -> try next channel
            end if;
          else
            SEQ_STATE <= Check2;
          end if;
        when Read1 =>                                      -- initiate read
          ReadActiveFlags <= "0010";
          RD <= '0';                                       -- DEBUG test only
          SEQ_STATE <= BusyWait1;
        when BusyWait1 =>
                   -- wait for end of read sequence
          ReadActiveFlags <= "0000";
          RD <= '0';
          if(BUSY_I = '0') then
            RD_Latch1 <= WBR_DTA8_I;
            SEQ_STATE <= Check2;
          end if;
-- channel 2
        when Check2 =>
          ReadActiveFlags <= "0000";
          if(Enable(2) = '1') then
            RD <= '0';
            ADR <= S2_ADR_I;
            if(PendingFlags(2) = '1') then                 -- read flags pending for this channel?
              RD <= '1';
              SEQ_STATE <= Read2;                          -- yes -> read
            else
              SEQ_STATE <= Check3;                         -- no -> try next channel
            end if;
          else
            SEQ_STATE <= Check3;
          end if;
        when Read2 =>                                      -- initiate read
          ReadActiveFlags <= "0100";
          RD <= '0';                                       -- DEBUG test only
          SEQ_STATE <= BusyWait2;
        when BusyWait2 =>
                                                           -- wait for end of read sequence
          ReadActiveFlags <= "0000";
          RD <= '0';
          if(BUSY_I = '0') then
            RD_Latch2 <= WBR_DTA8_I;
            SEQ_STATE <= Check3;
          end if;
-- channel 3
        when Check3 =>
          ReadActiveFlags <= "0000";
          if(Enable(3) = '1') then
            RD <= '0';
            ADR <= S3_ADR_I;
            if(PendingFlags(3) = '1') then                 -- read flags pending for this channel?
              RD <= '1';
              SEQ_STATE <= Read3;                          -- yes -> read
            else
              SEQ_STATE <= Check0;                         -- no -> try next channel
            end if;
          else
            SEQ_STATE <= Check0;
          end if;
        when Read3 =>                                      -- initiate read
          ReadActiveFlags <= "1000";
          RD <= '0';                                       -- DEBUG test only
          SEQ_STATE <= BusyWait3;
        when BusyWait3 =>
                                                           -- wait for end of read sequence
          ReadActiveFlags <= "0000";
          RD <= '0';
          if(BUSY_I = '0') then
            RD_Latch3 <= WBR_DTA8_I;
            SEQ_STATE <= Check0;
          end if;

        when others =>
          ReadActiveFlags <= "0000";
          RD <= '0';
          SEQ_STATE <= Check0;
       end case;
    end if;
  end process SEQUENCER_FSM;

  WBR_ADR <= ADR;
  WB_RD  <= RD;

  Volume: process(CLK_I, Out_Latch0, Out_Latch1, Out_Latch2, Out_Latch3,
    S0_VOL_I, S1_VOL_I, S2_VOL_I, S3_VOL_I) is
  begin
    if(rising_edge(CLK_I)) then
      V0  <= signed(Out_Latch0) * signed("00"& S0_VOL_I);
      V1  <= signed(Out_Latch1) * signed("00"& S1_VOL_I);
      V2  <= signed(Out_Latch2) * signed("00"& S2_VOL_I);
      V3  <= signed(Out_Latch3) * signed("00"& S3_VOL_I);
    end if;
  end process;

  Mix: process(CLK_I, V0, V1, V2, V3) is
  begin
    if(rising_edge(CLK_I)) then
      MixedL <= signed(V0) + signed(V3);                   -- 15 bits
      MixedR <= signed(V2) + signed(V1);
    end if;
  end process;

  AUDIO_OUT_L <= (MixedL(14 downto 0) & '0') XOR X"8000";  -- convert to signed 16 bits
  AUDIO_OUT_R <= (MixedR(14 downto 0) & '0') XOR X"8000";

  DSP_OUT <= Out_Latch3(7 downto 0) & Out_Latch2(7 downto 0) & Out_Latch1(7 downto 0) & Out_Latch0(7 downto 0);

 end structure;
--------------------------------------------------------------------------------
