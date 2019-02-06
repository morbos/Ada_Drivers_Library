with System;           use System;
with STM32_SVD.DFSDM;  use STM32_SVD.DFSDM;

with STM32.RCC;
with STM32.SYSCFG;
with System.Machine_Code;

package body STM32.DFSDM
is

   procedure Enable
     (This    : in out DFSDM_Block;
      Channel : DFSDM_Channel)
   is
   begin
      case Channel is
         when Channel_0 =>
            This.CHCFG0R1.CHEN := True;
         when Channel_1 =>
            This.CHCFG1R1.CHEN := True;
         when Channel_2 =>
            This.CHCFG2R1.CHEN := True;
         when Channel_3 =>
            This.CHCFG3R1.CHEN := True;
         when Channel_4 =>
            This.CHCFG4R1.CHEN := True;
         when Channel_5 =>
            This.CHCFG5R1.CHEN := True;
         when Channel_6 =>
            This.CHCFG6R1.CHEN := True;
         when Channel_7 =>
            This.CHCFG7R1.CHEN := True;
      end case;
   end Enable;
   procedure Disable
     (This    : in out DFSDM_Block;
      Channel : DFSDM_Channel)
   is
   begin
      case Channel is
         when Channel_0 =>
            This.CHCFG0R1.CHEN := False;
         when Channel_1 =>
            This.CHCFG1R1.CHEN := False;
         when Channel_2 =>
            This.CHCFG2R1.CHEN := False;
         when Channel_3 =>
            This.CHCFG3R1.CHEN := False;
         when Channel_4 =>
            This.CHCFG4R1.CHEN := False;
         when Channel_5 =>
            This.CHCFG5R1.CHEN := False;
         when Channel_6 =>
            This.CHCFG6R1.CHEN := False;
         when Channel_7 =>
            This.CHCFG7R1.CHEN := False;
      end case;
   end Disable;

   function Enabled
     (This    : DFSDM_Block;
      Channel : DFSDM_Channel) return Boolean
   is
   begin
      case Channel is
         when Channel_0 =>
            return This.CHCFG0R1.CHEN;
         when Channel_1 =>
            return This.CHCFG1R1.CHEN;
         when Channel_2 =>
            return This.CHCFG2R1.CHEN;
         when Channel_3 =>
            return This.CHCFG3R1.CHEN;
         when Channel_4 =>
            return This.CHCFG4R1.CHEN;
         when Channel_5 =>
            return This.CHCFG5R1.CHEN;
         when Channel_6 =>
            return This.CHCFG6R1.CHEN;
         when Channel_7 =>
            return This.CHCFG7R1.CHEN;
      end case;
   end Enabled;

--   procedure Initialize (This : DFSDM_Block)
--   is
--   begin
--      null;
--      DFSDM1.CHCFG0R1.DFSDMEN := True;
--      DFSDM1_Periph.CHCFG0R1.CKOUTSRC := True
--      DFSDM1_Periph.CHCFG0R1.CHCFG0R1.CKOUTDIV := 16#17#;

--        > DFSDM1.CHCFG0R1.DFSDMEN: 0
--  > DFSDM1.CHCFG0R1.CKOUTSRC: 0
--  > DFSDM1.CHCFG0R1.CKOUTDIV[7:0]: 0
--  2093c2093
--  < DFSDM1.CHCFG5R1.CHEN: 1
--  --  -
--  > DFSDM1.CHCFG5R1.CHEN: 0
--  2096c2096
--  < DFSDM1.CHCFG5R1.SPICKSEL[1:0]: 1
--  --  -
--  > DFSDM1.CHCFG5R1.SPICKSEL[1:0]: 0
--  2099,2101c2099,2101
--  < DFSDM1.CHCFG5R2.DTRBS[4:0]: 4
--  < DFSDM1.AWSCD5R.AWFORD[1:0]: 1
--  < DFSDM1.AWSCD5R.AWFOSR[4:0]: 9
--  --  -
--  > DFSDM1.CHCFG5R2.DTRBS[4:0]: 0
--  > DFSDM1.AWSCD5R.AWFORD[1:0]: 0
--  > DFSDM1.AWSCD5R.AWFOSR[4:0]: 0
--  2142,2144c2142,2144
--  < DFSDM1.DFSDM0_CR1.FAST: 1
--  < DFSDM1.DFSDM0_CR1.RCH[2:0]: 5
--  < DFSDM1.DFSDM0_CR1.RDMAEN: 1
--  --  -
--  > DFSDM1.DFSDM0_CR1.FAST: 0
--  > DFSDM1.DFSDM0_CR1.RCH[2:0]: 0
--  > DFSDM1.DFSDM0_CR1.RDMAEN: 0
--  2146c2146
--  < DFSDM1.DFSDM0_CR1.RCONT: 1
--  --  -
--  > DFSDM1.DFSDM0_CR1.RCONT: 0
--  2154c2154
--  < DFSDM1.DFSDM0_CR1.DFEN: 1
--  --  -
--  > DFSDM1.DFSDM0_CR1.DFEN: 0
--  2165,2166c2165,2166
--  < DFSDM1.DFSDM0_ISR.CKABF[7:0]: 0xff
--  < DFSDM1.DFSDM0_ISR.RCIP: 1
--  --  -
--  > DFSDM1.DFSDM0_ISR.CKABF[7:0]: 0
--  > DFSDM1.DFSDM0_ISR.RCIP: 0
--  2177,2179c2177,2179
--  < DFSDM1.DFSDM0_JCHGR.JCHG[7:0]: 1
--  < DFSDM1.DFSDM0_FCR.FORD[2:0]: 3
--  < DFSDM1.DFSDM0_FCR.FOSR[9:0]: 0xff
--  --  -
--  > DFSDM1.DFSDM0_JCHGR.JCHG[7:0]: 0
--  > DFSDM1.DFSDM0_FCR.FORD[2:0]: 0
--  > DFSDM1.DFSDM0_FCR.FOSR[9:0]: 0
--  2183c2183
--  < DFSDM1.DFSDM0_RDATAR.RDATA[23:0]: 0x7b1
--  --  -
--  > DFSDM1.DFSDM0_RDATAR.RDATA[23:0]: 0
--  2194c2194
--  < DFSDM1.DFSDM0_EXMAX.EXMAX[23:0]: 0x800000
--  --  -
--  > DFSDM1.DFSDM0_EXMAX.EXMAX[23:0]: 0
--  2196c2196
--  < DFSDM1.DFSDM0_EXMIN.EXMIN[23:0]: 0x7fffff
--  --  -
--  > DFSDM1.DFSDM0_EXMIN.EXMIN[23:0]: 0
--  2198c2198
--  < DFSDM1.DFSDM0_CNVTIMR.CNVCNT[27:0]: 0x2712
--  --  -
--  > DFSDM1.DFSDM0_CNVTIMR.CNVCNT[27:0]: 0
--  2235c2235
--  < DFSDM1.DFSDM1_JCHGR.JCHG[7:0]: 1
--  --  -
--  > DFSDM1.DFSDM1_JCHGR.JCHG[7:0]: 0
--  2252c2252
--  < DFSDM1.DFSDM1_EXMAX.EXMAX[23:0]: 0x800000
--  --  -
--  > DFSDM1.DFSDM1_EXMAX.EXMAX[23:0]: 0
--  2254c2254
--  < DFSDM1.DFSDM1_EXMIN.EXMIN[23:0]: 0x7fffff
--  --  -
--  > DFSDM1.DFSDM1_EXMIN.EXMIN[23:0]: 0
--   end Initialize;
end STM32.DFSDM;
