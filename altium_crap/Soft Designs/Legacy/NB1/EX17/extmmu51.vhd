library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;

entity extmmu51 is
  port (
    memaddr : in std_logic_vector( 15 downto 0 );
    memdatai : in std_logic_vector( 7 downto 0 );
    memdatao : out std_logic_vector( 7 downto 0 );
    memrd : in std_logic;
    memwr : in std_logic;
    pswr : in std_logic;
    psrd : in std_logic;

    lcd_e        : out std_logic;
    ram_lcd_data : inout std_logic_vector( 7 DOWNTO 0 );
    ram_address  : out std_logic_vector( 16 DOWNTO 0 );
    ram_we       : out std_logic;
    ram_oe       : out std_logic;
    ram_cs       : out std_logic;

    key       : in std_logic_vector( 3 downto 0 );
    key_valid : in std_logic;
    key_rst   : out std_logic;

    -- dbg : out std_logic_vector(7 downto 0 );

    srl_control : out std_logic;
    srl_datai   : in std_logic_vector( 7 downto 0 );
    srl_datao   : out std_logic_vector( 7 downto 0 );
    srl_wr      : out std_logic;
    srl_rd      : out std_logic
  );
end extmmu51;

architecture behavioral of extmmu51 is
begin
  process(
    memaddr, memdatai, memrd, memwr, pswr, psrd,
    ram_lcd_data, key, key_valid, srl_datai
  )
  begin
    if psrd = '1' or pswr = '1' then
      -- ROM access
      ram_address <= '0' & memaddr;
      if psrd = '1' then
        memdatao <= ram_lcd_data;
      else
        memdatao <= (others => '0');
      end if;

      if pswr = '1' then
        ram_lcd_data <= memdatai;
      else
        ram_lcd_data <= (others => 'Z');
      end if;
      ram_we <= not pswr;
      ram_oe <= not psrd;
    else
      -- External data access
      --
      -- memory mappings
      -- lcd:    0xfffc - 0xffff
      -- keypad: 0xfffb - write: bit 0 = reset
      --         0xfffb - read: bit 0 = valid_key
      --         0xfffa - read: bit 0-3 = key pressed
      -- serial: 0xfff9
      --         0xfff8
      --
      --
      if memaddr( 15 downto 2 ) = "11111111111111" then
        -- LCD
        lcd_e <= '1';
        ram_cs <= '1';

        if memrd = '1' then
          memdatao <= ram_lcd_data;
        else
          memdatao <= (others => '0');
        end if;

        if memwr = '1' then
          ram_lcd_data <= memdatai;
        else
          ram_lcd_data <= (others => 'Z');
        end if;
      elsif memaddr = X"FFFB" then
        -- KEYPAD
        lcd_e <= '0';
        ram_cs <= '1';

        if memrd = '1' then
          memdatao <= "0000000" & key_valid;
        else
          memdatao <= (others => '0');
        end if;

        if memwr = '1' then
          key_rst <= memdatai(0);
        else
          key_rst <= '0';
        end if;
      elsif memaddr = X"FFFA" then
        -- KEYPAD
        lcd_e <= '0';
        ram_cs <= '1';

        if memrd = '1' then
          memdatao <= "0000" & key;
        else
          memdatao <= (others => '0');
        end if;
      elsif memaddr( 15 downto 1 ) = X"FFF" & "100" then
        -- Serial
        lcd_e <= '0';
        ram_cs <= '1';
        srl_control <= memaddr(0);

        if memrd = '1' then
          memdatao <= srl_datai;
        else
          memdatao <= (others => '0');
        end if;

        srl_datao <= memdatai;
        srl_rd <= memrd;
        srl_wr <= memwr;
      else
        -- External RAM access
        lcd_e <= '0';
        ram_cs <= '0';
        ram_address <= '1' & memaddr;

        if memrd = '1' then
          memdatao <= ram_lcd_data;
        else
          memdatao <= (others => '0');
        end if;

        if memwr = '1' then
          ram_lcd_data <= memdatai;
        else
          ram_lcd_data <= (others => 'Z');
        end if;

        ram_we <= not memwr;
        ram_oe <= not memrd;
      end if;
    end if;
    -- dbg <= key_valid & "000" & key;
  end process;
end behavioral;

