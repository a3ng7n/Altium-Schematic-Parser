------------------------------------------------------------
-- SubModule SFR_EXPANDER.VHD
-- Created   7/09/2004 3:27:06 PM
------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;

entity SFR_EXPANDER is port
   (
     ADDRIN       : in    std_logic_vector(6 downto 0);
     DATAIN       : in    std_logic_vector(7 downto 0);
     WR           : in    std_logic;
     Out0W       : out   std_logic_vector(31 downto 0);
     Out1W       : out   std_logic_vector(31 downto 0);
     Out2W       : out   std_logic_vector(31 downto 0);
     Out3W       : out   std_logic_vector(31 downto 0)
   );
end SFR_EXPANDER;
------------------------------------------------------------

------------------------------------------------------------
architecture structure of SFR_EXPANDER is

-- Component Declarations

-- Signal Declarations

Signal OutByte0,Outbyte1,OutByte2,OutByte3, OutByte4 : std_logic_vector(7 downto 0);

Signal Writepulse0, WritePulse1, WritePulse2, WritePulse3 ,WritePulse4: std_logic;

begin
  Latch0: process (WritePulse0) is
  begin
    if(rising_edge(WritePulse0)) then
      OutByte0 <= DATAIN;
    end if;
  end process Latch0;

  Latch1: process (WritePulse1) is
  begin
    if(rising_edge(WritePulse1)) then
      OutByte1 <= DATAIN;
    end if;
  end process Latch1;

  Latch2: process (WritePulse2) is
  begin
    if(rising_edge(WritePulse2)) then
      OutByte2 <= DATAIN;
    end if;
  end process Latch2;

  Latch3: process (WritePulse3) is
  begin
    if(rising_edge(WritePulse3)) then
      OutByte3 <= DATAIN;
    end if;
  end process Latch3;

  Latch4: process (WritePulse4) is
  begin
    if(rising_edge(WritePulse4)) then
      OutByte4 <= DATAIN;
    end if;

    if(falling_edge(WritePulse4)) then
      case (OutByte4) is
        when "00000000" => Out0W <= OutByte3 & OutByte2 & OutByte1 & OutByte0;
        when "00000001" => Out1W <= OutByte3 & OutByte2 & OutByte1 & OutByte0;
        when "00000010" => Out2W <= OutByte3 & OutByte2 & OutByte1 & OutByte0;
        when "00000011" => Out3W <= OutByte3 & OutByte2 & OutByte1 & OutByte0;
        when others     => null;
      end case;
    end if;

  end process Latch4;

  WritePulse0 <= '1' when (ADDRIN = "0000110") and (WR = '1') else '0';
  WritePulse1 <= '1' when (ADDRIN = "0001110") and (WR = '1') else '0';
  WritePulse2 <= '1' when (ADDRIN = "0010110") and (WR = '1') else '0';
  WritePulse3 <= '1' when (ADDRIN = "0011110") and (WR = '1') else '0';
  WritePulse4 <= '1' when (ADDRIN = "0100110") and (WR = '1') else '0';

--  Out32W <= OutByte3 & OutByte2 & OutByte1 & OutByte0;

end structure;
------------------------------------------------------------
