library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;

entity uart115200 is
  port (
    reset  : in std_logic;
    clk    : in std_logic;
    clk_ref : in std_logic;

    udatai : in std_logic_vector( 7 downto 0 );
    udatao : out std_logic_vector( 7 downto 0 );
    ucontrol : in std_logic;

    txd : out std_logic;
    rxd : in std_logic;

    ti : out std_logic;
    ri : out std_logic;

    urd : in std_logic;
    uwr : in std_logic
    );
end uart115200;

architecture behavioral of uart115200 is
  signal txbuf : std_logic_vector( 7 downto 0 );
  signal txrdy : std_logic;
  signal itxbdclk : std_logic;
  signal itxbusy : std_logic;
  signal iti : std_logic;

  signal rxbdclk : std_logic;
  signal rxbuf : std_logic_vector( 7 downto 0 );
  signal rxstart : std_logic;
  signal irxrdy : std_logic;
  signal iri : std_logic;
begin

  ti <= iti;
  ri <= iri;

---------------------------------------------------------------------------
--                  C l o c k   G e n e r a t i o n                      --
---------------------------------------------------------------------------
  --
  -- Generate the receive baudrate clock from the clock reference
  -- Its speed is 6x the actual baudrate to be able to sample the
  -- receive line multiple times.
  --
  proc_rxclk : process( clk_ref, reset )
    variable rxcnt : integer range 0 to 28;
  begin
    if reset = '1' then
      rxcnt := 0;
      rxbdclk <= '0';
    elsif rising_edge(clk_ref) then
      if rxcnt = 28 then
        rxcnt := 0;
        rxbdclk <= '1';
      else
        rxcnt := rxcnt + 1;
        rxbdclk <= '0';
      end if;
    end if;
  end process;

  --
  -- Generate the transmit baudrate clock from the receive baudrate clock
  --
  proc_txclk : process( clk_ref, rxbdclk, reset )
    variable txcnt : integer range 0 to 5;
  begin
    if reset = '1' then
      txcnt := 0;
      itxbdclk <= '0';
    elsif rising_edge(clk_ref) then
      if rxbdclk = '1' then
        if txcnt = 5 then
          txcnt := 0;
          itxbdclk <= '1';
        else
          txcnt := txcnt + 1;
          itxbdclk <= '0';
        end if;
      else
        itxbdclk <= '0';
      end if;
    end if;
  end process;
---------------------------------------------------------------------------
--                         S F R  H a n d l i n g                        --
---------------------------------------------------------------------------
  --
  -- Check the sfr address bus for a write to the UART transmit buffer
  --
  proc_sfr : process( clk, ucontrol, urd, uwr, txrdy, reset )
  begin
    if reset = '1' then
      itxbusy <= '0';
      iri <= '0';
      iti <= '0';
    elsif rising_edge(clk) then
      --
      -- Wait for the buffer to become empty again
      --
      if txrdy = '1' then
        iti <= '1';
        itxbusy <= '0';
      end if;
      if irxrdy = '1' then
        iri <= '1';
      end if;
      if ucontrol = '1' then
        --
        -- CONTROL register
        --
        if uwr = '1' then
          iri <= udatai(0);
          iti <= udatai(1);
        end if;
        if urd = '1' then
          udatao <= "000000" & iti & iri;
        end if;
      else
        --
        -- SBUF access
        --
        if uwr = '1' then
          txbuf <= udatai;
          itxbusy <= '1';
        end if;
        if urd = '1' then
          udatao <= rxbuf;
        end if;
      end if;
    end if;
  end process;

---------------------------------------------------------------------------
--                         T r a n s m i t t e r                         --
---------------------------------------------------------------------------
  --
  -- The transmit process
  --
  proc_tx_uart : process( clk_ref, itxbdclk, txbuf, itxbusy, reset )
    variable state : integer range 0 to 12;
  begin
    if reset = '1' then
      state := 0;
      txd <= '1';
    elsif rising_edge(clk_ref) then
      case state is
      when 0 =>
        txd <= '1';
        if (itxbusy = '1') AND (itxbdclk = '1') then
          state := 1;
        end if;
        if txrdy = '1' then
          txrdy <= '0';
        end if;
      when 1 =>
        -- Start bit
        txd <= '0';
        if itxbdclk = '1' then
          state := 2;
        end if;
      when 2 =>
        -- Bit 0
        txd <= txbuf(0);
        if itxbdclk = '1' then
          state := 3;
        end if;
      when 3 =>
        txd <= txbuf(1);
        if itxbdclk = '1' then
          state := 4;
        end if;
      when 4 =>
        txd <= txbuf(2);
        if itxbdclk = '1' then
          state := 5;
        end if;
      when 5 =>
        txd <= txbuf(3);
        if itxbdclk = '1' then
          state := 6;
        end if;
      when 6 =>
        txd <= txbuf(4);
        if itxbdclk = '1' then
          state := 7;
        end if;
      when 7 =>
        txd <= txbuf(5);
        if itxbdclk = '1' then
          state := 8;
        end if;
      when 8 =>
        txd <= txbuf(6);
        if itxbdclk = '1' then
          state := 9;
        end if;
      when 9 =>
        txd <= txbuf(7);
        if itxbdclk = '1' then
          state := 10;
        end if;
      when 10 =>
        txd <= '1';
        if itxbdclk = '1' then
          state := 11;
        end if;
      when 11 =>
        txd <= '1';
        if itxbdclk = '1' then
          state := 12;
        end if;
      when others =>
        txd <= '1';
        txrdy <= '1';
        -- wait for txbusy to go low
        if itxbusy = '0' then
          state := 0;
        end if;
      end case;
    end if;
  end process;

---------------------------------------------------------------------------
--                           R e c e i v e r                             --
---------------------------------------------------------------------------
  --
  -- The receive process, wait for a valid start bit
  --
  proc_rxstart_uart : process( clk_ref, reset )
    variable state : integer range 0 to 12;
    variable sample : std_logic_vector(2 downto 0);
  begin
    if reset = '1' then
      state := 0;
      rxstart <= '0';
    elsif rising_edge(clk_ref) then
      case state is
      when 0 =>
        --
        -- Wait for a start bit
        --
        if (rxbdclk = '1') and (rxd = '0') then
          state := 1;
          sample := "001";
        end if;
      when 1 | 2 | 3 | 4 =>
        if rxbdclk = '1' then
          if rxd = '0' then
            sample := sample + 1;
          end if;
          state := state + 1;
        end if;
      when 5 =>
        if rxbdclk = '1' then
          if rxd = '0' then
            sample := sample + 1;
          end if;
          if sample(2) = '1' then
            -- Found valid start bit
            rxstart <= '1';
            state := 6;
          else
            -- Invalid start bit
            state := 0;
          end if;
        end if;
      when others =>
        -- In this state a valid start bit has been detected
        -- now wait for all bits to have been scanned
        if irxrdy = '1' then
          rxstart <= '0';
          state := 0;
        end if;
      end case;
    end if;
  end process;

  --
  -- The receive process, triggered by reception of a valid start bit.
  -- Get all 8 data bits.
  --
  proc_rxdata_uart : process( clk_ref, reset )
    variable bitcnt : integer range 0 to 7;
    variable bitsamplecnt : integer range 0 to 5;
    variable samples : std_logic_vector(2 downto 0);
  begin
    if reset = '1' then
      rxbuf <= (others => '0');
      bitcnt := 0;
      bitsamplecnt := 0;
      samples := (others => '0');
    elsif rising_edge(clk_ref) then
      if rxstart = '1' then
        if (irxrdy = '0') AND (rxbdclk = '1') then
          if rxd = '1' then
            samples := samples + 1;
          end if;
          if bitsamplecnt = 5 then
            bitsamplecnt := 0;
            -- shift right receive buffer
            -- the high bit of the samples counter determines the
            -- actual bit value to be shifted in.
            rxbuf <= samples(2) & rxbuf(7 downto 1);
            samples := (others => '0'); -- reset samples counter
            if bitcnt = 7 then
              bitcnt := 0;
              irxrdy <= '1';
            else
              bitcnt := bitcnt + 1;
            end if;
          else
            bitsamplecnt := bitsamplecnt + 1;
          end if;
        end if;
      else
        bitcnt := 0;
        bitsamplecnt := 0;
        samples := (others => '0');
        irxrdy <= '0';
      end if;
    end if;
  end process;
end behavioral;

