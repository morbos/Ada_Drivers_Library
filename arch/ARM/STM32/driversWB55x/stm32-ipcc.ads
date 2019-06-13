private with STM32_SVD.IPCC;

package STM32.IPCC is

   type IPCC_Channel_Range is range 1 .. 6;
   IPCC_Channel_1 : constant IPCC_Channel_Range := 1;
   IPCC_Channel_2 : constant IPCC_Channel_Range := 2;
   IPCC_Channel_3 : constant IPCC_Channel_Range := 3;
   IPCC_Channel_4 : constant IPCC_Channel_Range := 4;
   IPCC_Channel_5 : constant IPCC_Channel_Range := 5;
   IPCC_Channel_6 : constant IPCC_Channel_Range := 6;

   type IPCC_Channel_List is array (Positive range <>) of IPCC_Channel_Range;

   procedure IPCC_Enable;

   procedure IPCC_Cpu1_SetFlag (Channel : IPCC_Channel_Range);

   procedure IPCC_Cpu1_SetFlag (Channel_List : IPCC_Channel_List);

   procedure IPCC_Cpu2_SetFlag (Channel : IPCC_Channel_Range);

   procedure IPCC_Cpu2_SetFlag (Channel_List : IPCC_Channel_List);

   procedure IPCC_Cpu1_ClearFlag (Channel : IPCC_Channel_Range);

   procedure IPCC_Cpu1_ClearFlag (Channel_List : IPCC_Channel_List);

   procedure IPCC_Cpu2_ClearFlag (Channel : IPCC_Channel_Range);

   procedure IPCC_Cpu2_ClearFlag (Channel_List : IPCC_Channel_List);

   procedure IPCC_Cpu1_EnableTransmitChannel (Channel : IPCC_Channel_Range);

   procedure IPCC_Cpu1_EnableTransmitChannel (Channel_List : IPCC_Channel_List);

   procedure IPCC_Cpu1_DisableTransmitChannel (Channel : IPCC_Channel_Range);

   procedure IPCC_Cpu1_DisableTransmitChannel (Channel_List : IPCC_Channel_List);

   procedure IPCC_Cpu1_EnableReceiveChannel (Channel : IPCC_Channel_Range);

   procedure IPCC_Cpu1_EnableReceiveChannel (Channel_List : IPCC_Channel_List);

   procedure IPCC_Cpu1_DisableReceiveChannel (Channel : IPCC_Channel_Range);

   procedure IPCC_Cpu1_DisableReceiveChannel (Channel_List : IPCC_Channel_List);

   procedure IPCC_Cpu2_EnableTransmitChannel (Channel : IPCC_Channel_Range);

   procedure IPCC_Cpu2_EnableTransmitChannel (Channel_List : IPCC_Channel_List);

   procedure IPCC_Cpu2_DisableTransmitChannel (Channel : IPCC_Channel_Range);

   procedure IPCC_Cpu2_DisableTransmitChannel (Channel_List : IPCC_Channel_List);

   procedure IPCC_Cpu2_EnableReceiveChannel (Channel : IPCC_Channel_Range);

   procedure IPCC_Cpu2_EnableReceiveChannel (Channel_List : IPCC_Channel_List);

   procedure IPCC_Cpu2_DisableReceiveChannel (Channel : IPCC_Channel_Range);

   procedure IPCC_Cpu2_DisableReceiveChannel (Channel_List : IPCC_Channel_List);

   procedure IPCC_Enable_TX_Interrupt;

   procedure IPCC_Enable_RX_Interrupt;

   procedure IPCC_Disable_TX_Interrupt;

   procedure IPCC_Disable_RX_Interrupt;

   function IPCC_TX_Pending (Channel : IPCC_Channel_Range) return Boolean;

   function IPCC_RX_Pending (Channel : IPCC_Channel_Range) return Boolean;

   function IPCC_CPU2_Masked (Channel : IPCC_Channel_Range) return Boolean;

   function IPCC_CPU2_Masked (Channel_List : IPCC_Channel_List) return Boolean;

end STM32.IPCC;
