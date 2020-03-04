------------------------------------------------------------
-- SubModule SineOsc
-- Created   10/05/2004 8:58:00 PM
--
-- SINE/COSINE waveforms are generated using the reaccurance relation :-
--
-- cos (theta+delta) = cos(theta) - alpha*cos(theta) + beta*sin(theta)
-- sin (theta+delta) = sin(theta) - alpha*sin(theta) - beta*cos(theta)
--
-- where theta is the initial phase angle,
--     delta is the incement angle,
--     alpha = 2 * sin²(delta/2)
--       and  beta = sin(delta)
--
-- A value for delta is chossen so that 1/alpha =  2^N  and 1/beta = 2^M, (where N and M are integers)
-- This allows the multiplications to be performed using addition and shifting,
--
-- The amplitude of the waves are scaled to allow integer arithmetic to be used in place of floats.

------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;
use ieee.std_logic_arith.all ;

use IEEE.std_logic_signed.all;

entity SineOsc is port
   (
     CLK           : in    std_logic;
     SYNC          : in    std_logic;
     Offset        : in    std_logic; -- DC offset output signal otherwise output is signed.
     SINE          : out   std_logic_vector(15 downto 0);
     COSINE        : out   std_logic_vector(15 downto 0)
   );
end SineOsc;
------------------------------------------------------------

------------------------------------------------------------
architecture structure of SineOsc is

subtype STATE_TYPE is std_ulogic_vector (1 downto 0);
signal   State   :  STATE_TYPE;
constant INIT            : STATE_TYPE := "00";
constant CALCULATE       : STATE_TYPE := "01";
constant STORE           : STATE_TYPE := "10";
constant PEAK            : std_logic_vector(17 downto 0) := "001111100000000000";

signal X : std_logic_vector(17 downto 0);
signal Y : std_logic_vector(17 downto 0);
signal newX : std_logic_vector(17 downto 0);
signal newY : std_logic_vector(17 downto 0);

begin
     process (CLK,SYNC)
       variable xr : bit_vector(17 downto 0);
     begin
          if SYNC='1' then
             State <= INIT;
          elsif (CLK'event) and CLK = '1' then
             case State is
                       when INIT => --Set initial point
                            X<= PEAK;
                            Y<= (others => '0');
                            State <= CALCULATE;
                       when CALCULATE => -- Calculate new points
                                  newX <= X - shr(X, conv_std_logic_vector(15, 4)) + shr(Y, conv_std_logic_vector(7, 4));
                                  newY <= Y - shr(Y, conv_std_logic_vector(15, 4)) - shr(X, conv_std_logic_vector(7, 4));

                                  State <= STORE;
                       when STORE => -- Update new points
                                  if X > PEAK then X<=PEAK; else X <= newX; end if;
                                  Y <= newY;
                                  SINE <=   (X(17) xor offset) & X(15 downto 1);
                                  COSINE <= (Y(17) xor offset) & X(15 downto 1);
                                  State <=  CALCULATE;
                       when OTHERS =>
                                 State <= INIT;
             end case;
          end if;
     end process;
end structure;
