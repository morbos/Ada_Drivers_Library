with STM32_SVD.RCC;
with STM32_SVD.IPCC;     use STM32_SVD.IPCC;
with System;

package body STM32.IPCC is

   procedure IPCC_Enable
   is
      use STM32_SVD.RCC;
   begin
      RCC_Periph.AHB3ENR.IPCCEN := True;
      RCC_Periph.C2AHB3ENR.IPCCEN := True;
   end IPCC_Enable;

   procedure IPCC_Cpu1_SetFlag (Channel : IPCC_Channel_Range)
   is
   begin
      IPCC_Periph.C1SCR.CHS.Arr (Channel'Enum_Rep) := True;
   end IPCC_Cpu1_SetFlag;

   procedure IPCC_Cpu1_SetFlag (Channel_List : IPCC_Channel_List)
   is
   begin
      for Channel of Channel_List loop
         IPCC_Cpu1_SetFlag (Channel);
      end loop;
   end IPCC_Cpu1_SetFlag;

   procedure IPCC_Cpu2_SetFlag (Channel : IPCC_Channel_Range)
   is
   begin
      IPCC_Periph.C2SCR.CHS.Arr (Channel'Enum_Rep) := True;
   end IPCC_Cpu2_SetFlag;

   procedure IPCC_Cpu2_SetFlag (Channel_List : IPCC_Channel_List)
   is
   begin
      for Channel of Channel_List loop
         IPCC_Cpu2_SetFlag (Channel);
      end loop;
   end IPCC_Cpu2_SetFlag;

   procedure IPCC_Cpu1_ClearFlag (Channel : IPCC_Channel_Range)
   is
   begin
      IPCC_Periph.C1SCR.CHC.Arr (Channel'Enum_Rep) := True;
   end IPCC_Cpu1_ClearFlag;

   procedure IPCC_Cpu1_ClearFlag (Channel_List : IPCC_Channel_List)
   is
   begin
      for Channel of Channel_List loop
         IPCC_Cpu1_ClearFlag (Channel);
      end loop;
   end IPCC_Cpu1_ClearFlag;

   procedure IPCC_Cpu2_ClearFlag (Channel : IPCC_Channel_Range)
   is
   begin
      IPCC_Periph.C2SCR.CHC.Arr (Channel'Enum_Rep) := True;
   end IPCC_Cpu2_ClearFlag;

   procedure IPCC_Cpu2_ClearFlag (Channel_List : IPCC_Channel_List)
   is
   begin
      for Channel of Channel_List loop
         IPCC_Cpu2_ClearFlag (Channel);
      end loop;
   end IPCC_Cpu2_ClearFlag;

   procedure IPCC_Cpu1_EnableTransmitChannel (Channel : IPCC_Channel_Range)
   is
   begin
      IPCC_Periph.C1MR.CHFM.Arr (Channel'Enum_Rep) := False;
   end IPCC_Cpu1_EnableTransmitChannel;

   procedure IPCC_Cpu1_EnableTransmitChannel (Channel_List : IPCC_Channel_List)
   is
   begin
      for Channel of Channel_List loop
         IPCC_Cpu1_EnableTransmitChannel (Channel);
      end loop;
   end IPCC_Cpu1_EnableTransmitChannel;

   procedure IPCC_Cpu1_DisableTransmitChannel (Channel : IPCC_Channel_Range)
   is
   begin
      IPCC_Periph.C1MR.CHFM.Arr (Channel'Enum_Rep) := True;
   end IPCC_Cpu1_DisableTransmitChannel;

   procedure IPCC_Cpu1_DisableTransmitChannel (Channel_List : IPCC_Channel_List)
   is
   begin
      for Channel of Channel_List loop
         IPCC_Cpu1_DisableTransmitChannel (Channel);
      end loop;
   end IPCC_Cpu1_DisableTransmitChannel;

   procedure IPCC_Cpu1_EnableReceiveChannel (Channel : IPCC_Channel_Range)
   is
   begin
      IPCC_Periph.C1MR.CHOM.Arr (Channel'Enum_Rep) := False;
   end IPCC_Cpu1_EnableReceiveChannel;

   procedure IPCC_Cpu1_EnableReceiveChannel (Channel_List : IPCC_Channel_List)
   is
   begin
      for Channel of Channel_List loop
         IPCC_Cpu1_EnableReceiveChannel (Channel);
      end loop;
   end IPCC_Cpu1_EnableReceiveChannel;

   procedure IPCC_Cpu1_DisableReceiveChannel (Channel : IPCC_Channel_Range)
   is
   begin
      IPCC_Periph.C1MR.CHOM.Arr (Channel'Enum_Rep) := True;
   end IPCC_Cpu1_DisableReceiveChannel;

   procedure IPCC_Cpu1_DisableReceiveChannel (Channel_List : IPCC_Channel_List)
   is
   begin
      for Channel of Channel_List loop
         IPCC_Cpu1_DisableReceiveChannel (Channel);
      end loop;
   end IPCC_Cpu1_DisableReceiveChannel;

   procedure IPCC_Cpu2_EnableTransmitChannel (Channel : IPCC_Channel_Range)
   is
   begin
      IPCC_Periph.C1MR.CHFM.Arr (Channel'Enum_Rep) := False;
   end IPCC_Cpu2_EnableTransmitChannel;

   procedure IPCC_Cpu2_EnableTransmitChannel (Channel_List : IPCC_Channel_List)
   is
   begin
      for Channel of Channel_List loop
         IPCC_Cpu2_EnableTransmitChannel (Channel);
      end loop;
   end IPCC_Cpu2_EnableTransmitChannel;

   procedure IPCC_Cpu2_DisableTransmitChannel (Channel : IPCC_Channel_Range)
   is
   begin
      IPCC_Periph.C2MR.CHFM.Arr (Channel'Enum_Rep) := False;
   end IPCC_Cpu2_DisableTransmitChannel;

   procedure IPCC_Cpu2_DisableTransmitChannel (Channel_List : IPCC_Channel_List)
   is
   begin
      for Channel of Channel_List loop
         IPCC_Cpu2_DisableTransmitChannel (Channel);
      end loop;
   end IPCC_Cpu2_DisableTransmitChannel;

   procedure IPCC_Cpu2_EnableReceiveChannel (Channel : IPCC_Channel_Range)
   is
   begin
      IPCC_Periph.C2MR.CHOM.Arr (Channel'Enum_Rep) := False;
   end IPCC_Cpu2_EnableReceiveChannel;

   procedure IPCC_Cpu2_EnableReceiveChannel (Channel_List : IPCC_Channel_List)
   is
   begin
      for Channel of Channel_List loop
         IPCC_Cpu2_EnableReceiveChannel (Channel);
      end loop;
   end IPCC_Cpu2_EnableReceiveChannel;

   procedure IPCC_Cpu2_DisableReceiveChannel (Channel : IPCC_Channel_Range)
   is
   begin
      IPCC_Periph.C2MR.CHOM.Arr (Channel'Enum_Rep) := False;
   end IPCC_Cpu2_DisableReceiveChannel;

   procedure IPCC_Cpu2_DisableReceiveChannel (Channel_List : IPCC_Channel_List)
   is
   begin
      for Channel of Channel_List loop
         IPCC_Cpu2_DisableReceiveChannel (Channel);
      end loop;
   end IPCC_Cpu2_DisableReceiveChannel;

   procedure IPCC_Enable_TX_Interrupt
   is
   begin
      IPCC_Periph.C1CR.TXFIE := True;
      IPCC_Periph.C2CR.TXFIE := True;
   end IPCC_Enable_TX_Interrupt;

   procedure IPCC_Enable_RX_Interrupt
   is
   begin
      IPCC_Periph.C1CR.RXOIE := True;
      IPCC_Periph.C2CR.RXOIE := True;
   end IPCC_Enable_RX_Interrupt;

   procedure IPCC_Disable_TX_Interrupt
   is
   begin
      IPCC_Periph.C1CR.TXFIE := False;
      IPCC_Periph.C2CR.TXFIE := False;
   end IPCC_Disable_TX_Interrupt;

   procedure IPCC_Disable_RX_Interrupt
   is
   begin
      IPCC_Periph.C1CR.RXOIE := False;
      IPCC_Periph.C2CR.RXOIE := False;
   end IPCC_Disable_RX_Interrupt;

   function IPCC_TX_Pending (Channel : IPCC_Channel_Range) return Boolean
   is
   begin
      if (not IPCC_Periph.C1TOC2SR.CHF.Arr (Channel'Enum_Rep)) --  not active
        and
        (not IPCC_Periph.C1MR.CHFM.Arr (Channel'Enum_Rep)) -- and... xmit ena(not msked)
      then
         return True;
      else
         return False;
      end if;
   end IPCC_TX_Pending;

   function IPCC_RX_Pending (Channel : IPCC_Channel_Range) return Boolean
   is
   begin
      if IPCC_Periph.C2TOC1SR.CHF.Arr (Channel'Enum_Rep) --  active
        and
        (not IPCC_Periph.C2MR.CHOM.Arr (Channel'Enum_Rep)) -- and... rx ena(not msked)
      then
         return True;
      else
         return False;
      end if;
   end IPCC_RX_Pending;

   function IPCC_CPU2_Masked (Channel : IPCC_Channel_Range) return Boolean
   is
   begin
      if IPCC_Periph.C2MR.CHOM.Arr (Channel'Enum_Rep)
      then
         return True;
      else
         return False;
      end if;
   end IPCC_CPU2_Masked;

   function IPCC_CPU2_Masked (Channel_List : IPCC_Channel_List) return Boolean
   is
   begin
      for Channel of Channel_List loop
         if IPCC_CPU2_Masked (Channel) then
            return True;
         end if;
      end loop;
      return False;
   end IPCC_CPU2_Masked;

end STM32.IPCC;
