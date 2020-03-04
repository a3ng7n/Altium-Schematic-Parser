library IEEE;
   use IEEE.STD_LOGIC_1164.all;
   use IEEE.STD_LOGIC_UNSIGNED."+";

   entity WATCHDOG is
      port(
          -- Global control signals
          clk           : in  STD_LOGIC;  -- clock input
          wdtclk        : in  STD_LOGIC;  -- wdt clock input
          reset         : in  STD_LOGIC;  -- reset input
          wdtc          : in  STD_LOGIC;  -- WDT clear
          wdte          : in  STD_LOGIC;  -- WDT enable
          wdtprs        : in  STD_LOGIC_VECTOR(31 downto 0);  -- WDT enable
          wdto          : out  STD_LOGIC  -- WDT output (overflow)
          );
   
   end  WATCHDOG;

--*******************************************************************--

   architecture RTL of WATCHDOG is
   
      signal wdt_to_tout     : STD_LOGIC;
      signal wdte_r          : STD_LOGIC;
      signal wdtc_r          : STD_LOGIC;
      signal wdtprs_r        : STD_LOGIC_VECTOR(31 downto 0);
   
      -- WDT counter
      signal wdt_count        : STD_LOGIC_VECTOR(31 downto 0);
         
   begin
   
   
   --------------------------------------------------------------------
   -- Time Out assignment
   --------------------------------------------------------------------
   time_out_as:
   --------------------------------------------------------------------
      wdto <=
            wdt_to_tout;  
   
   
   --------------------------------------------------------------------
   -- 
   --------------------------------------------------------------------
   pin_reg:
   --------------------------------------------------------------------
      process (clk)
      begin
         if clk'event and clk='0' then
            wdte_r      <= wdte;
            wdtc_r      <= wdtc;
            wdtprs_r    <= wdtprs;
         end if;
      end process;
   
   
   --------------------------------------------------------------------
   -- WatchDog Timer
   --------------------------------------------------------------------
   watchdog_timer_proc:
   --------------------------------------------------------------------
      process (wdtclk)
      begin
      
         -------------------------------------
         -- Synchronous counter
         -------------------------------------
         if wdtclk'event and wdtclk='0' then
            if (reset = '1' or 
                wdtc_r ='1')
            then
               wdt_count <= (others => '0');
            elsif wdt_count <= wdtprs_r then
               wdt_count <= wdt_count + 1;
            else
               wdt_count <= (others => '0');
            end if;
                  
            if (reset = '1' or
                wdtc_r ='1')
            then
               wdt_to_tout  <= '0';
            elsif wdt_count>=wdtprs_r and wdte_r='1' then
               wdt_to_tout <= '1';
            else
               wdt_to_tout <= '0';
            end if;
         end if;
      end process;
   end; 

--*******************************************************************--
