library ieee;
use ieee.std_logic_1164.all ;

entity MAX1104_Driver is
port (
   clk      : in  std_logic ;
   rst      : in  std_logic ;

   -- Host interface
   DATAIN   : in  std_logic_vector(7 downto 0) ;
   DATAOUT  : out std_logic_vector(7 downto 0) ;
   BUSY     : out std_logic ;
   WR       : in  std_logic ;
   RD       : in  std_logic ;
--   CTRL     : in  std_logic ;
   TRIGGER  : in  std_logic ;

   -- SPI interface
   SPI_DOUT : out std_logic ;
   SPI_CSn  : out std_logic ;
   SPI_SCLK : out std_logic ;
   SPI_DIN  : in  std_logic
);
end ;

architecture rtl of MAX1104_Driver is

type fsm_type is (RESET, IDLE, TRANSMIT, WA  )


begin

-- main fsm, combinatorial part.
process(state)
begin
   case state is
      when RESET =>

      when others =>
   end case ;
end process;


-- main fsm, registered part
process(clk)
begin
   if rising_edge(clk) then
      if rst='1' then
         state <= RESET ;
      else
         state <= next_state ;
      end if ;
   end if ;
end process;




process(clk)
begin
   if rising_edge(clk) then
      if rst='1' then
         shift_enable <= '0' ;
      else

      end if ;
   end if ;
end process;


-- shift in/out registers
-- shift_enable is a "clock" for SPI_SCLK that should be up to 6 MHz
process(clk)
begin
   if rising_edge(clk) then
      if rst='1' then
         in_reg  <= (others=>'0') ;
      else
         if shift_enable='1' then
            in_reg   <= in_reg(6 downto 0) & SPI_DIN ;
            SPI_DOUT <= out_reg(7) ;
         end if ;
   end if ;
end process;


-- SPI_SCLK clock => must be up to 6 MHz.
process(clk)
begin
   if rising_edge(clk) then
      if rst = '1' then
         SPI_SCLK <= '0' ;
      else
         if SPI_clock_enable = '1' then
            SPI_SCLK <= '1' ;
         else
            SPI_Sclk <= '0' ;
         end if ;
      end if ;
   end if ;
end process;


process(clk)
begin
   if rising_edge(clk) then
      if rst = '1' then
         SPI_clock_enable <= '0' ;
      else
         if SCLK_counter = "000000" then
            SCLK_counter <= counter_reg ;
         else
            SCLK_counter <= SCLK_counter - '1' ;
         end if ;
      end if ;
   end if ;
end process;



SPI_DOUT <= out_reg(7) ;
DATAOUT  <= in_reg ;

end rtl ;