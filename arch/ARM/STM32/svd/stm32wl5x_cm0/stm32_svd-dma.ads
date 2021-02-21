--  This spec has been automatically generated from STM32WL5x_CM0P.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.DMA is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  interrupt status register
   type ISR_Register is record
      --  Read-only. global interrupt flag for channel 1
      GIF1           : Boolean;
      --  Read-only. transfer complete (TC) flag for channel 1
      TCIF1          : Boolean;
      --  Read-only. half transfer (HT) flag for channel 1
      HTIF1          : Boolean;
      --  Read-only. transfer error (TE) flag for channel 1
      TEIF1          : Boolean;
      --  Read-only. global interrupt flag for channel 2
      GIF2           : Boolean;
      --  Read-only. transfer complete (TC) flag for channel 2
      TCIF2          : Boolean;
      --  Read-only. half transfer (HT) flag for channel 2
      HTIF2          : Boolean;
      --  Read-only. transfer error (TE) flag for channel 2
      TEIF2          : Boolean;
      --  Read-only. global interrupt flag for channel 3
      GIF3           : Boolean;
      --  Read-only. transfer complete (TC) flag for channel 3
      TCIF3          : Boolean;
      --  Read-only. half transfer (HT) flag for channel 3
      HTIF3          : Boolean;
      --  Read-only. transfer error (TE) flag for channel 3
      TEIF3          : Boolean;
      --  Read-only. global interrupt flag for channel 4
      GIF4           : Boolean;
      --  Read-only. transfer complete (TC) flag for channel 4
      TCIF4          : Boolean;
      --  Read-only. half transfer (HT) flag for channel 4
      HTIF4          : Boolean;
      --  Read-only. transfer error (TE) flag for channel 4
      TEIF4          : Boolean;
      --  Read-only. global interrupt flag for channel 5
      GIF5           : Boolean;
      --  Read-only. transfer complete (TC) flag for channel 5
      TCIF5          : Boolean;
      --  Read-only. half transfer (HT) flag for channel 5
      HTIF5          : Boolean;
      --  Read-only. transfer error (TE) flag for channel 5
      TEIF5          : Boolean;
      --  Read-only. global interrupt flag for channel 6
      GIF6           : Boolean;
      --  Read-only. transfer complete (TC) flag for channel 6
      TCIF6          : Boolean;
      --  Read-only. half transfer (HT) flag for channel 6
      HTIF6          : Boolean;
      --  Read-only. transfer error (TE) flag for channel 6
      TEIF6          : Boolean;
      --  Read-only. global interrupt flag for channel 7
      GIF7           : Boolean;
      --  Read-only. transfer complete (TC) flag for channel 7
      TCIF7          : Boolean;
      --  Read-only. half transfer (HT) flag for channel 7
      HTIF7          : Boolean;
      --  Read-only. transfer error (TE) flag for channel 7
      TEIF7          : Boolean;
      --  unspecified
      Reserved_28_31 : HAL.UInt4;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISR_Register use record
      GIF1           at 0 range 0 .. 0;
      TCIF1          at 0 range 1 .. 1;
      HTIF1          at 0 range 2 .. 2;
      TEIF1          at 0 range 3 .. 3;
      GIF2           at 0 range 4 .. 4;
      TCIF2          at 0 range 5 .. 5;
      HTIF2          at 0 range 6 .. 6;
      TEIF2          at 0 range 7 .. 7;
      GIF3           at 0 range 8 .. 8;
      TCIF3          at 0 range 9 .. 9;
      HTIF3          at 0 range 10 .. 10;
      TEIF3          at 0 range 11 .. 11;
      GIF4           at 0 range 12 .. 12;
      TCIF4          at 0 range 13 .. 13;
      HTIF4          at 0 range 14 .. 14;
      TEIF4          at 0 range 15 .. 15;
      GIF5           at 0 range 16 .. 16;
      TCIF5          at 0 range 17 .. 17;
      HTIF5          at 0 range 18 .. 18;
      TEIF5          at 0 range 19 .. 19;
      GIF6           at 0 range 20 .. 20;
      TCIF6          at 0 range 21 .. 21;
      HTIF6          at 0 range 22 .. 22;
      TEIF6          at 0 range 23 .. 23;
      GIF7           at 0 range 24 .. 24;
      TCIF7          at 0 range 25 .. 25;
      HTIF7          at 0 range 26 .. 26;
      TEIF7          at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  interrupt flag clear register
   type IFCR_Register is record
      --  Write-only. global interrupt flag clear for channel 1
      GIF1           : Boolean := False;
      --  Write-only. transfer complete flag clear for channel 1
      TCIF1          : Boolean := False;
      --  Write-only. half transfer flag clear for channel 1
      HTIF1          : Boolean := False;
      --  Write-only. transfer error flag clear for channel 1
      TEIF1          : Boolean := False;
      --  Write-only. global interrupt flag clear for channel 2
      GIF2           : Boolean := False;
      --  Write-only. transfer complete flag clear for channel 2
      TCIF2          : Boolean := False;
      --  Write-only. half transfer flag clear for channel 2
      HTIF2          : Boolean := False;
      --  Write-only. transfer error flag clear for channel 2
      TEIF2          : Boolean := False;
      --  Write-only. global interrupt flag clear for channel 3
      GIF3           : Boolean := False;
      --  Write-only. transfer complete flag clear for channel 3
      TCIF3          : Boolean := False;
      --  Write-only. half transfer flag clear for channel 3
      HTIF3          : Boolean := False;
      --  Write-only. transfer error flag clear for channel 3
      TEIF3          : Boolean := False;
      --  Write-only. global interrupt flag clear for channel 4
      GIF4           : Boolean := False;
      --  Write-only. transfer complete flag clear for channel 4
      TCIF4          : Boolean := False;
      --  Write-only. half transfer flag clear for channel 4
      HTIF4          : Boolean := False;
      --  Write-only. transfer error flag clear for channel 4
      TEIF4          : Boolean := False;
      --  Write-only. global interrupt flag clear for channel 5
      GIF5           : Boolean := False;
      --  Write-only. transfer complete flag clear for channel 5
      TCIF5          : Boolean := False;
      --  Write-only. half transfer flag clear for channel 5
      HTIF5          : Boolean := False;
      --  Write-only. transfer error flag clear for channel 5
      TEIF5          : Boolean := False;
      --  Write-only. global interrupt flag clear for channel 6
      GIF6           : Boolean := False;
      --  Write-only. transfer complete flag clear for channel 6
      TCIF6          : Boolean := False;
      --  Write-only. half transfer flag clear for channel 6
      HTIF6          : Boolean := False;
      --  Write-only. transfer error flag clear for channel 6
      TEIF6          : Boolean := False;
      --  Write-only. global interrupt flag clear for channel 7
      GIF7           : Boolean := False;
      --  Write-only. transfer complete flag clear for channel 7
      TCIF7          : Boolean := False;
      --  Write-only. half transfer flag clear for channel 7
      HTIF7          : Boolean := False;
      --  Write-only. transfer error flag clear for channel 7
      TEIF7          : Boolean := False;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IFCR_Register use record
      GIF1           at 0 range 0 .. 0;
      TCIF1          at 0 range 1 .. 1;
      HTIF1          at 0 range 2 .. 2;
      TEIF1          at 0 range 3 .. 3;
      GIF2           at 0 range 4 .. 4;
      TCIF2          at 0 range 5 .. 5;
      HTIF2          at 0 range 6 .. 6;
      TEIF2          at 0 range 7 .. 7;
      GIF3           at 0 range 8 .. 8;
      TCIF3          at 0 range 9 .. 9;
      HTIF3          at 0 range 10 .. 10;
      TEIF3          at 0 range 11 .. 11;
      GIF4           at 0 range 12 .. 12;
      TCIF4          at 0 range 13 .. 13;
      HTIF4          at 0 range 14 .. 14;
      TEIF4          at 0 range 15 .. 15;
      GIF5           at 0 range 16 .. 16;
      TCIF5          at 0 range 17 .. 17;
      HTIF5          at 0 range 18 .. 18;
      TEIF5          at 0 range 19 .. 19;
      GIF6           at 0 range 20 .. 20;
      TCIF6          at 0 range 21 .. 21;
      HTIF6          at 0 range 22 .. 22;
      TEIF6          at 0 range 23 .. 23;
      GIF7           at 0 range 24 .. 24;
      TCIF7          at 0 range 25 .. 25;
      HTIF7          at 0 range 26 .. 26;
      TEIF7          at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype CCR_PSIZE_Field is HAL.UInt2;
   subtype CCR_MSIZE_Field is HAL.UInt2;
   subtype CCR_PL_Field is HAL.UInt2;

   --  channel x configuration register
   type CCR_Register is record
      --  channel enable
      EN             : Boolean := False;
      --  transfer complete interrupt enable
      TCIE           : Boolean := False;
      --  half transfer interrupt enable
      HTIE           : Boolean := False;
      --  transfer error interrupt enable
      TEIE           : Boolean := False;
      --  data transfer direction
      DIR            : Boolean := False;
      --  circular mode
      CIRC           : Boolean := False;
      --  peripheral increment mode
      PINC           : Boolean := False;
      --  memory increment mode
      MINC           : Boolean := False;
      --  peripheral size
      PSIZE          : CCR_PSIZE_Field := 16#0#;
      --  memory size
      MSIZE          : CCR_MSIZE_Field := 16#0#;
      --  priority level
      PL             : CCR_PL_Field := 16#0#;
      --  memory-to-memory mode
      MEM2MEM        : Boolean := False;
      --  unspecified
      Reserved_15_16 : HAL.UInt2 := 16#0#;
      --  ecure mode
      SECM           : Boolean := False;
      --  ecurity of the DMA transfer from the source
      SSEC           : Boolean := False;
      --  ecurity of the DMA transfer to the destination
      DSEC           : Boolean := False;
      --  rivileged mode
      PRIV           : Boolean := False;
      --  unspecified
      Reserved_21_31 : HAL.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCR_Register use record
      EN             at 0 range 0 .. 0;
      TCIE           at 0 range 1 .. 1;
      HTIE           at 0 range 2 .. 2;
      TEIE           at 0 range 3 .. 3;
      DIR            at 0 range 4 .. 4;
      CIRC           at 0 range 5 .. 5;
      PINC           at 0 range 6 .. 6;
      MINC           at 0 range 7 .. 7;
      PSIZE          at 0 range 8 .. 9;
      MSIZE          at 0 range 10 .. 11;
      PL             at 0 range 12 .. 13;
      MEM2MEM        at 0 range 14 .. 14;
      Reserved_15_16 at 0 range 15 .. 16;
      SECM           at 0 range 17 .. 17;
      SSEC           at 0 range 18 .. 18;
      DSEC           at 0 range 19 .. 19;
      PRIV           at 0 range 20 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   subtype CNDTR_NDT_Field is HAL.UInt18;

   --  channel x number of data to transfer register
   type CNDTR_Register is record
      --  number of data to transfer (0 to 218 - 1)
      NDT            : CNDTR_NDT_Field := 16#0#;
      --  unspecified
      Reserved_18_31 : HAL.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CNDTR_Register use record
      NDT            at 0 range 0 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Direct memory access controller
   type DMA_Peripheral is record
      --  interrupt status register
      ISR    : aliased ISR_Register;
      --  interrupt flag clear register
      IFCR   : aliased IFCR_Register;
      --  channel x configuration register
      CCR1   : aliased CCR_Register;
      --  channel x number of data to transfer register
      CNDTR1 : aliased CNDTR_Register;
      --  channel x peripheral address register
      CPAR1  : aliased HAL.UInt32;
      --  channel x memory address register
      CMAR1  : aliased HAL.UInt32;
      --  channel x configuration register
      CCR2   : aliased CCR_Register;
      --  channel x number of data to transfer register
      CNDTR2 : aliased CNDTR_Register;
      --  channel x peripheral address register
      CPAR2  : aliased HAL.UInt32;
      --  channel x memory address register
      CMAR2  : aliased HAL.UInt32;
      --  channel x configuration register
      CCR3   : aliased CCR_Register;
      --  channel x number of data to transfer register
      CNDTR3 : aliased CNDTR_Register;
      --  channel x peripheral address register
      CPAR3  : aliased HAL.UInt32;
      --  channel x memory address register
      CMAR3  : aliased HAL.UInt32;
      --  channel x configuration register
      CCR4   : aliased CCR_Register;
      --  channel x number of data to transfer register
      CNDTR4 : aliased CNDTR_Register;
      --  channel x peripheral address register
      CPAR4  : aliased HAL.UInt32;
      --  channel x memory address register
      CMAR4  : aliased HAL.UInt32;
      --  channel x configuration register
      CCR5   : aliased CCR_Register;
      --  channel x number of data to transfer register
      CNDTR5 : aliased CNDTR_Register;
      --  channel x peripheral address register
      CPAR5  : aliased HAL.UInt32;
      --  channel x memory address register
      CMAR5  : aliased HAL.UInt32;
      --  channel x configuration register
      CCR6   : aliased CCR_Register;
      --  channel x number of data to transfer register
      CNDTR6 : aliased CNDTR_Register;
      --  channel x peripheral address register
      CPAR6  : aliased HAL.UInt32;
      --  channel x memory address register
      CMAR6  : aliased HAL.UInt32;
      --  channel x configuration register
      CCR7   : aliased CCR_Register;
      --  channel x number of data to transfer register
      CNDTR7 : aliased CNDTR_Register;
      --  channel x peripheral address register
      CPAR7  : aliased HAL.UInt32;
      --  channel x memory address register
      CMAR7  : aliased HAL.UInt32;
   end record
     with Volatile;

   for DMA_Peripheral use record
      ISR    at 16#0# range 0 .. 31;
      IFCR   at 16#4# range 0 .. 31;
      CCR1   at 16#8# range 0 .. 31;
      CNDTR1 at 16#C# range 0 .. 31;
      CPAR1  at 16#10# range 0 .. 31;
      CMAR1  at 16#14# range 0 .. 31;
      CCR2   at 16#1C# range 0 .. 31;
      CNDTR2 at 16#20# range 0 .. 31;
      CPAR2  at 16#24# range 0 .. 31;
      CMAR2  at 16#28# range 0 .. 31;
      CCR3   at 16#30# range 0 .. 31;
      CNDTR3 at 16#34# range 0 .. 31;
      CPAR3  at 16#38# range 0 .. 31;
      CMAR3  at 16#3C# range 0 .. 31;
      CCR4   at 16#44# range 0 .. 31;
      CNDTR4 at 16#48# range 0 .. 31;
      CPAR4  at 16#4C# range 0 .. 31;
      CMAR4  at 16#50# range 0 .. 31;
      CCR5   at 16#58# range 0 .. 31;
      CNDTR5 at 16#5C# range 0 .. 31;
      CPAR5  at 16#60# range 0 .. 31;
      CMAR5  at 16#64# range 0 .. 31;
      CCR6   at 16#6C# range 0 .. 31;
      CNDTR6 at 16#70# range 0 .. 31;
      CPAR6  at 16#74# range 0 .. 31;
      CMAR6  at 16#78# range 0 .. 31;
      CCR7   at 16#80# range 0 .. 31;
      CNDTR7 at 16#84# range 0 .. 31;
      CPAR7  at 16#88# range 0 .. 31;
      CMAR7  at 16#8C# range 0 .. 31;
   end record;

   --  Direct memory access controller
   DMA1_Periph : aliased DMA_Peripheral
     with Import, Address => DMA1_Base;

   --  Direct memory access controller
   DMA2_Periph : aliased DMA_Peripheral
     with Import, Address => DMA2_Base;

end STM32_SVD.DMA;
