-----------------------------------------------------------
-- SubModule Console Controller
-- Created   24/08/2004 4:05:03 PM
-----------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;

entity ConsoleController is port
   (
     CLK             : in    std_logic;
     RST             : in    std_logic;
     -- Processor interface
     WR              : in    std_logic;
     DATA            : in    std_logic_vector(7 downto 0);
     BUSY            : out   std_logic;

     -- Console interface
     MEM_W           : out   std_logic;
     MEM_AD          : out   std_logic_vector(10 downto 0);
     CursorPos       : out   std_logic_vector(9 downto 0);
     CursorMode      : out   std_logic;
     MEM_DATA        : out   std_logic_vector(15 downto 0);
     BorderCol       : out   std_logic_vector(3 downto 0);
     PageSelect      : out   std_logic
   );
end ConsoleController;
------------------------------------------------------------

------------------------------------------------------------
architecture structure of ConsoleController is

subtype STATE_TYPE is std_ulogic_vector (3 downto 0);
constant RESET                   : STATE_TYPE := "0000";  -- reset the console
constant CLS                     : STATE_TYPE := "0001";  -- clear the console
constant CLS_NEXT                : STATE_TYPE := "0010";
constant CLS_WRITE               : STATE_TYPE := "0011";
constant READ_BYTE               : STATE_TYPE := "0100";   -- Read next byte from interface
constant READ_PARAMETER          : STATE_TYPE := "0101";   -- Read parameter following command
constant READ_PARAMETER_NEXT     : STATE_TYPE := "0110";   -- Read parameter following command
constant WRITE_BYTE              : STATE_TYPE := "0111";   -- Read next byte from interface

signal state : STATE_TYPE;

constant COMMAND_NULL               : std_logic_vector(7 downto 0) := "00000000";
constant COMMAND_CLS                : std_logic_vector(7 downto 0) := "00000001";
constant COMMAND_TEXTCOLOR          : std_logic_vector(7 downto 0) := "00000010";
constant COMMAND_HOME               : std_logic_vector(7 downto 0) := "00000011";
constant COMMAND_CURSOR_OFF         : std_logic_vector(7 downto 0) := "00000100";
constant COMMAND_CURSOR_ON          : std_logic_vector(7 downto 0) := "00000101";
constant COMMAND_GOTO_X             : std_logic_vector(7 downto 0) := "00000110";
constant COMMAND_GOTO_Y             : std_logic_vector(7 downto 0) := "00000111";
constant COMMAND_BS                 : std_logic_vector(7 downto 0) := "00001000";
constant COMMAND_LF                 : std_logic_vector(7 downto 0) := "00001010";
constant COMMAND_VT                 : std_logic_vector(7 downto 0) := "00001011";
constant COMMAND_CR                 : std_logic_vector(7 downto 0) := "00001101";
constant COMMAND_STORE              : std_logic_vector(7 downto 0) := "00001111";
constant COMMAND_RECALL             : std_logic_vector(7 downto 0) := "00010000";
constant COMMAND_TEXTBACKGROUND     : std_logic_vector(7 downto 0) := "00010001";
constant COMMAND_PAGE0              : std_logic_vector(7 downto 0) := "00010010";
constant COMMAND_PAGE1              : std_logic_vector(7 downto 0) := "00010011";
constant COMMAND_BORDER             : std_logic_vector(7 downto 0) := "00010100";

signal AC             : std_logic_vector(9 downto 0);
signal colour         : std_logic_vector(7 downto 0);
signal lastWR         : std_logic;
signal lastCommand    : std_logic_vector(7 downto 0);

signal colour_store    : std_logic_vector(7 downto 0);
signal AC_store        : std_logic_vector(9 downto 0);
signal page            : std_logic;

begin
  cpu_interface:process (WR, RST, CLK)
  begin
  if RST='1' then
     State <= RESET;
  elsif CLK'event and CLK='1' then
    case State is
           when RESET =>
                colour <= "00101010";
                BorderCol <= "0000";
                CursorMode <= '1';
                AC <= "0000000000";
                State <= CLS;
                Page <= '0';

           when CLS =>
                Busy <= '1';
                MEM_DATA <= colour & "00100000";
                AC <= "1111111111";
                State <= CLS_NEXT;

           when CLS_NEXT=>
                Mem_W <= '0';
                AC <= AC-1;
                State <= CLS_WRITE;

           when CLS_WRITE=>
                Mem_W <= '1';
                if (AC="0000000000") then
                   Busy <= '0';
                   State <= READ_BYTE;
                else
                   State <= CLS_NEXT;
                end if;

           when READ_BYTE =>
                if lastWR='0' and WR='1' then
                   case Data is
                      when COMMAND_NULL=>       -- Do nothing

                      when COMMAND_CLS=>        -- Clear Screen
                           State <= CLS;

                      when COMMAND_TEXTCOLOR=>  -- Set Text Colour
                           lastCommand <= Data;
                           State <= READ_PARAMETER;

                      when COMMAND_TEXTBACKGROUND=> -- Set Text Colour
                           lastCommand <= Data;
                           State <= READ_PARAMETER;

                      when COMMAND_HOME=>    -- Return cursor to top left screen position
                           AC <= "0000000000";

                      when COMMAND_CURSOR_OFF=>
                           CursorMode <='0';

                      when COMMAND_CURSOR_ON=>
                           CursorMode <='1';

                      when COMMAND_GOTO_X=>
                           lastCommand <= Data;
                           State <= READ_PARAMETER;

                      when COMMAND_GOTO_Y=>
                           lastCommand <= Data;
                           State <= READ_PARAMETER;

                      when COMMAND_BS=>      -- Back Space
                           AC <= AC-1;

                      when COMMAND_LF=>      -- Line feed
                           AC <= (AC(9 downto 5)+1) & "00000";

                      when COMMAND_VT=>      -- Virticle Tab
                           AC <= AC-"0000100000";

                      when COMMAND_CR=>      -- Carriage Return to left margin
                           AC(4 downto 0) <= "00000";

                      when COMMAND_STORE=>   -- Store cursor position and colour
                           AC_store <= AC;
                           colour_store <= colour;

                      when COMMAND_RECALL=>  -- Recall cursor position and colour
                           AC <= AC_store;
                           colour <= colour_store;

                      when COMMAND_PAGE0=>   -- Set Display page to 0
                           Page <= '0';

                      when COMMAND_PAGE1=>
                           Page <= '1';     -- Set Display page to 1

                      when COMMAND_BORDER=> -- Set border colour
                           lastCommand <= Data;
                           State <= READ_PARAMETER;

                      when others =>        -- Write character
                           Mem_W <= '1';
                           State <= WRITE_BYTE;
                           MEM_DATA <= colour & DATA;
                   end case;
                end if;
                CursorPos <= AC;

           when WRITE_BYTE=>
                if WR='0' then
                   Mem_W <='0';
                   AC <= AC+1; -- Autoincrement
                   State <= READ_BYTE;
                end if;

           when READ_PARAMETER =>
                if WR='0' then
                   State <= READ_PARAMETER_NEXT;
                end if;

           when READ_PARAMETER_NEXT=>
                if WR='1' then -- got the parrameter for the command
                   case lastCommand is
                      when COMMAND_TEXTCOLOR=>       -- Set Text Colour
                           Colour(3 downto 0) <= DATA(3 downto 0);

                      when COMMAND_TEXTBACKGROUND=>  -- Set Background Colour
                           Colour(7 downto 4) <= DATA(3 downto 0);

                      when COMMAND_GOTO_X=>          -- Set Cursor X position
                           AC(4 downto 0) <= Data(4 downto 0);

                      when COMMAND_GOTO_Y=>          -- Set Cursor Y position
                           AC(9 downto 5) <= Data(4 downto 0);

                      when COMMAND_BORDER=>          -- Set Border Colour
                           BorderCol <= Data(3 downto 0);

                      when others=> -- Do nothing

                   end case;
                   State <= READ_BYTE;
                end if;

           when others=>
                State <= RESET;
    end case;
   lastWR <= WR;
    end if;
  end process;

  MEM_AD <= Page & AC;
  PageSelect <= Page;
end structure;
------------------------------------------------------------
