library ieee;
Use     iEEE.std_logic_1164.all;

entity EXTMEM is
port (
    MEMDATAI      : out   std_logic_vector(7 downto 0);
    MEMDATAO      : in    std_logic_vector(7 downto 0);
    MEMADDR       : in    std_logic_vector(15 downto 0);
    MEMWR         : in    std_logic;
    MEMRD         : in    std_logic;
    PSRD          : in    std_logic;
    PSWR          : in    std_logic;

    EXT_ADDR      : out   std_logic_vector(16 downto 0);

    EXTDATA_DATA  : inout std_logic_vector(7 downto 0);
    EXTDATA_WR    : out   std_logic;
    EXTDATA_RD    : out   std_logic;

    EXTPROG_DATA  : inout std_logic_vector(7 downto 0);
    EXTPROG_WR    : out   std_logic;
    EXTPROG_RD    : out   std_logic
);
end EXTMEM;

architecture RTL of EXTMEM is
begin
    process(MEMRD, MEMWR, PSWR, PSRD, EXTDATA_DATA, EXTPROG_DATA, MEMDATAO)
    begin
       EXTPROG_WR <= not PSWR;
       EXTPROG_RD <= not PSRD;
       EXTDATA_WR <= not MEMWR;
       EXTDATA_RD <= not MEMRD;

       if MEMWR = '1' then
          EXTDATA_DATA <= MEMDATAO;
       else
          EXTDATA_DATA <= (others => 'Z');
       end if;

       if PSWR = '1' then
          EXTPROG_DATA <= MEMDATAO;
       else
          EXTPROG_DATA <= (others => 'Z');
       end if;

       if MEMRD = '1' then
          MEMDATAI <= EXTDATA_DATA;
       elsif PSRD = '1' then
          MEMDATAI <= EXTPROG_DATA;
       else
          MEMDATAI <= (others => '-');
       end if;
    end process;

    EXT_ADDR(15 downto 0) <= MEMADDR;
    EXT_ADDR(16)          <= '0';
end;

