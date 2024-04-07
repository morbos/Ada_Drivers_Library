--  This spec has been automatically generated from STM32U073.svd

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

   --  DMA interrupt status register
   type DMA_ISR_Register is record
      --  Read-only. Global interrupt flag for channel 1
      GIF1           : Boolean;
      --  Read-only. Transfer complete (TC) flag for channel 1
      TCIF1          : Boolean;
      --  Read-only. Half transfer (HT) flag for channel 1
      HTIF1          : Boolean;
      --  Read-only. Transfer error (TE) flag for channel 1
      TEIF1          : Boolean;
      --  Read-only. Global interrupt flag for channel 2
      GIF2           : Boolean;
      --  Read-only. Transfer complete (TC) flag for channel 2
      TCIF2          : Boolean;
      --  Read-only. Half transfer (HT) flag for channel 2
      HTIF2          : Boolean;
      --  Read-only. Transfer error (TE) flag for channel 2
      TEIF2          : Boolean;
      --  Read-only. Global interrupt flag for channel 3
      GIF3           : Boolean;
      --  Read-only. Transfer complete (TC) flag for channel 3
      TCIF3          : Boolean;
      --  Read-only. Half transfer (HT) flag for channel 3
      HTIF3          : Boolean;
      --  Read-only. Transfer error (TE) flag for channel 3
      TEIF3          : Boolean;
      --  Read-only. global interrupt flag for channel 4
      GIF4           : Boolean;
      --  Read-only. Transfer complete (TC) flag for channel 4
      TCIF4          : Boolean;
      --  Read-only. Half transfer (HT) flag for channel 4
      HTIF4          : Boolean;
      --  Read-only. Transfer error (TE) flag for channel 4
      TEIF4          : Boolean;
      --  Read-only. global interrupt flag for channel 5
      GIF5           : Boolean;
      --  Read-only. Transfer complete (TC) flag for channel 5
      TCIF5          : Boolean;
      --  Read-only. Half transfer (HT) flag for channel 5
      HTIF5          : Boolean;
      --  Read-only. Transfer error (TE) flag for channel 5
      TEIF5          : Boolean;
      --  Read-only. Global interrupt flag for channel 6
      GIF6           : Boolean;
      --  Read-only. Transfer complete (TC) flag for channel 6
      TCIF6          : Boolean;
      --  Read-only. Half transfer (HT) flag for channel 6
      HTIF6          : Boolean;
      --  Read-only. Transfer error (TE) flag for channel 6
      TEIF6          : Boolean;
      --  Read-only. Global interrupt flag for channel 7
      GIF7           : Boolean;
      --  Read-only. Transfer complete (TC) flag for channel 7
      TCIF7          : Boolean;
      --  Read-only. Half transfer (HT) flag for channel 7
      HTIF7          : Boolean;
      --  Read-only. Transfer error (TE) flag for channel 7
      TEIF7          : Boolean;
      --  unspecified
      Reserved_28_31 : HAL.UInt4;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMA_ISR_Register use record
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

   --  DMA interrupt flag clear register
   type DMA_IFCR_Register is record
      --  Write-only. Global interrupt flag clear for channel 1
      CGIF1          : Boolean := False;
      --  Write-only. Transfer complete flag clear for channel 1
      CTCIF1         : Boolean := False;
      --  Write-only. Half transfer flag clear for channel 1
      CHTIF1         : Boolean := False;
      --  Write-only. Transfer error flag clear for channel 1
      CTEIF1         : Boolean := False;
      --  Write-only. Global interrupt flag clear for channel 2
      CGIF2          : Boolean := False;
      --  Write-only. Transfer complete flag clear for channel 2
      CTCIF2         : Boolean := False;
      --  Write-only. Half transfer flag clear for channel 2
      CHTIF2         : Boolean := False;
      --  Write-only. Transfer error flag clear for channel 2
      CTEIF2         : Boolean := False;
      --  Write-only. Global interrupt flag clear for channel 3
      CGIF3          : Boolean := False;
      --  Write-only. Transfer complete flag clear for channel 3
      CTCIF3         : Boolean := False;
      --  Write-only. Half transfer flag clear for channel 3
      CHTIF3         : Boolean := False;
      --  Write-only. Transfer error flag clear for channel 3
      CTEIF3         : Boolean := False;
      --  Write-only. Global interrupt flag clear for channel 4
      CGIF4          : Boolean := False;
      --  Write-only. Transfer complete flag clear for channel 4
      CTCIF4         : Boolean := False;
      --  Write-only. Half transfer flag clear for channel 4
      CHTIF4         : Boolean := False;
      --  Write-only. Transfer error flag clear for channel 4
      CTEIF4         : Boolean := False;
      --  Write-only. Global interrupt flag clear for channel 5
      CGIF5          : Boolean := False;
      --  Write-only. Transfer complete flag clear for channel 5
      CTCIF5         : Boolean := False;
      --  Write-only. Half transfer flag clear for channel 5
      CHTIF5         : Boolean := False;
      --  Write-only. Transfer error flag clear for channel 5
      CTEIF5         : Boolean := False;
      --  Write-only. Global interrupt flag clear for channel 6
      CGIF6          : Boolean := False;
      --  Write-only. Transfer complete flag clear for channel 6
      CTCIF6         : Boolean := False;
      --  Write-only. Half transfer flag clear for channel 6
      CHTIF6         : Boolean := False;
      --  Write-only. Transfer error flag clear for channel 6
      CTEIF6         : Boolean := False;
      --  Write-only. Global interrupt flag clear for channel 7
      CGIF7          : Boolean := False;
      --  Write-only. Transfer complete flag clear for channel 7
      CTCIF7         : Boolean := False;
      --  Write-only. Half transfer flag clear for channel 7
      CHTIF7         : Boolean := False;
      --  Write-only. Transfer error flag clear for channel 7
      CTEIF7         : Boolean := False;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMA_IFCR_Register use record
      CGIF1          at 0 range 0 .. 0;
      CTCIF1         at 0 range 1 .. 1;
      CHTIF1         at 0 range 2 .. 2;
      CTEIF1         at 0 range 3 .. 3;
      CGIF2          at 0 range 4 .. 4;
      CTCIF2         at 0 range 5 .. 5;
      CHTIF2         at 0 range 6 .. 6;
      CTEIF2         at 0 range 7 .. 7;
      CGIF3          at 0 range 8 .. 8;
      CTCIF3         at 0 range 9 .. 9;
      CHTIF3         at 0 range 10 .. 10;
      CTEIF3         at 0 range 11 .. 11;
      CGIF4          at 0 range 12 .. 12;
      CTCIF4         at 0 range 13 .. 13;
      CHTIF4         at 0 range 14 .. 14;
      CTEIF4         at 0 range 15 .. 15;
      CGIF5          at 0 range 16 .. 16;
      CTCIF5         at 0 range 17 .. 17;
      CHTIF5         at 0 range 18 .. 18;
      CTEIF5         at 0 range 19 .. 19;
      CGIF6          at 0 range 20 .. 20;
      CTCIF6         at 0 range 21 .. 21;
      CHTIF6         at 0 range 22 .. 22;
      CTEIF6         at 0 range 23 .. 23;
      CGIF7          at 0 range 24 .. 24;
      CTCIF7         at 0 range 25 .. 25;
      CHTIF7         at 0 range 26 .. 26;
      CTEIF7         at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype DMA_CCR_PSIZE_Field is HAL.UInt2;
   subtype DMA_CCR_MSIZE_Field is HAL.UInt2;
   subtype DMA_CCR_PL_Field is HAL.UInt2;

   --  DMA channel 1 configuration register
   type DMA_CCR_Register is record
      --  Channel enable When a channel transfer error occurs, this bit is
      --  cleared by hardware. It can not be set again by software (channel x
      --  re-activated) until the TEIFx bit of the DMA_ISR register is cleared
      --  (by1setting the CTEIFx bit of the DMA_IFCR register). Note: This bit
      --  is set and cleared by software.
      EN             : Boolean := False;
      --  Transfer complete interrupt enable Note: This bit is set and cleared
      --  by software. It must not be written when the channel is enabled (EN =
      --  1). It is not read-only when the channel is enabled (EN = 1).
      TCIE           : Boolean := False;
      --  Half transfer interrupt enable Note: This bit is set and cleared by
      --  software. It must not be written when the channel is enabled (EN =
      --  1). It is not read-only when the channel is enabled (EN = 1).
      HTIE           : Boolean := False;
      --  Transfer error interrupt enable Note: This bit is set and cleared by
      --  software. It must not be written when the channel is enabled (EN =
      --  1). It is not read-only when the channel is enabled (EN = 1).
      TEIE           : Boolean := False;
      --  Data transfer direction This bit must be set only in
      --  memory-to-peripheral and peripheral-to-memory modes. Source
      --  attributes are defined by PSIZE and PINC, plus the DMA_CPARx
      --  register. This is still valid in a memory-to-memory mode. Destination
      --  attributes are defined by MSIZE and MINC, plus the DMA_CMARx
      --  register. This is still valid in a peripheral-to-peripheral mode.
      --  Destination attributes are defined by PSIZE and PINC, plus the
      --  DMA_CPARx register. This is still valid in a memory-to-memory mode.
      --  Source attributes are defined by MSIZE and MINC, plus the DMA_CMARx
      --  register. This is still valid in a peripheral-to-peripheral mode.
      --  Note: This bit is set and cleared by software. It must not be written
      --  when the channel is enabled (EN = 1). It is read-only when the
      --  channel is enabled (EN = 1).
      DIR            : Boolean := False;
      --  Circular mode Note: This bit is set and cleared by software. It must
      --  not be written when the channel is enabled (EN = 1). It is not
      --  read-only when the channel is enabled (EN = 1).
      CIRC           : Boolean := False;
      --  Peripheral increment mode Defines the increment mode for each DMA
      --  transfer to the identified peripheral. n memory-to-memory mode, this
      --  bit identifies the memory destination if DIR = 1 and the memory
      --  source if DIR = 0. In peripheral-to-peripheral mode, this bit
      --  identifies the peripheral destination if DIR = 1 and the peripheral
      --  source if DIR = 0. Note: This bit is set and cleared by software. It
      --  must not be written when the channel is enabled (EN = 1). It is
      --  read-only when the channel is enabled (EN = 1).
      PINC           : Boolean := False;
      --  Memory increment mode Defines the increment mode for each DMA
      --  transfer to the identified memory. In memory-to-memory mode, this bit
      --  identifies the memory source if DIR = 1 and the memory destination if
      --  DIR = 0. In peripheral-to-peripheral mode, this bit identifies the
      --  peripheral source if DIR = 1 and the peripheral destination if DIR =
      --  0. Note: This bit is set and cleared by software. It must not be
      --  written when the channel is enabled (EN = 1). It is read-only when
      --  the channel is enabled (EN = 1).
      MINC           : Boolean := False;
      --  Peripheral size Defines the data size of each DMA transfer to the
      --  identified peripheral. In memory-to-memory mode, this bitfield
      --  identifies the memory destination if DIR = 1 and the memory source if
      --  DIR = 0. In peripheral-to-peripheral mode, this bitfield identifies
      --  the peripheral destination if DIR = 1 and the peripheral source if
      --  DIR = 0. Note: This bitfield is set and cleared by software. It must
      --  not be written when the channel is enabled (EN = 1). It is read-only
      --  when the channel is enabled (EN = 1).
      PSIZE          : DMA_CCR_PSIZE_Field := 16#0#;
      --  Memory size Defines the data size of each DMA transfer to the
      --  identified memory. In memory-to-memory mode, this bitfield identifies
      --  the memory source if DIR = 1 and the memory destination if DIR = 0.
      --  In peripheral-to-peripheral mode, this bitfield identifies the
      --  peripheral source if DIR = 1 and the peripheral destination if DIR =
      --  0. Note: This bitfield is set and cleared by software. It must not be
      --  written when the channel is enabled (EN = 1). It is read-only when
      --  the channel is enabled (EN = 1).
      MSIZE          : DMA_CCR_MSIZE_Field := 16#0#;
      --  Priority level Note: This bitfield is set and cleared by software. It
      --  must not be written when the channel is enabled (EN = 1). It is
      --  read-only when the channel is enabled (EN = 1).
      PL             : DMA_CCR_PL_Field := 16#0#;
      --  Memory-to-memory mode Note: This bit is set and cleared by software.
      --  It must not be written when the channel is enabled (EN = 1). It is
      --  read-only when the channel is enabled (EN = 1).
      MEM2MEM        : Boolean := False;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMA_CCR_Register use record
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
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype DMA_CNDTR_NDT_Field is HAL.UInt16;

   --  DMA channel 1 number of data to transfer register
   type DMA_CNDTR_Register is record
      --  Number of data to transfer
      NDT            : DMA_CNDTR_NDT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMA_CNDTR_Register use record
      NDT            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  DMA register bank
   type DMA_Peripheral is record
      --  DMA interrupt status register
      DMA_ISR    : aliased DMA_ISR_Register;
      --  DMA interrupt flag clear register
      DMA_IFCR   : aliased DMA_IFCR_Register;
      --  DMA channel 1 configuration register
      DMA_CCR1   : aliased DMA_CCR_Register;
      --  DMA channel 1 number of data to transfer register
      DMA_CNDTR1 : aliased DMA_CNDTR_Register;
      --  DMA channel 1 peripheral address register
      DMA_CPAR1  : aliased HAL.UInt32;
      --  DMA channel 1 memory address register
      DMA_CMAR1  : aliased HAL.UInt32;
      --  DMA channel 2 configuration register
      DMA_CCR2   : aliased DMA_CCR_Register;
      --  DMA channel 2 number of data to transfer register
      DMA_CNDTR2 : aliased DMA_CNDTR_Register;
      --  DMA channel 2 peripheral address register
      DMA_CPAR2  : aliased HAL.UInt32;
      --  DMA channel 2 memory address register
      DMA_CMAR2  : aliased HAL.UInt32;
      --  DMA channel 3 configuration register
      DMA_CCR3   : aliased DMA_CCR_Register;
      --  DMA channel 3 number of data to transfer register
      DMA_CNDTR3 : aliased DMA_CNDTR_Register;
      --  DMA channel 3 peripheral address register
      DMA_CPAR3  : aliased HAL.UInt32;
      --  DMA channel 3 memory address register
      DMA_CMAR3  : aliased HAL.UInt32;
      --  DMA channel 4 configuration register
      DMA_CCR4   : aliased DMA_CCR_Register;
      --  DMA channel 4 number of data to transfer register
      DMA_CNDTR4 : aliased DMA_CNDTR_Register;
      --  DMA channel 4 peripheral address register
      DMA_CPAR4  : aliased HAL.UInt32;
      --  DMA channel 4 memory address register
      DMA_CMAR4  : aliased HAL.UInt32;
      --  DMA channel 5 configuration register
      DMA_CCR5   : aliased DMA_CCR_Register;
      --  DMA channel 5 number of data to transfer register
      DMA_CNDTR5 : aliased DMA_CNDTR_Register;
      --  DMA channel 5 peripheral address register
      DMA_CPAR5  : aliased HAL.UInt32;
      --  DMA channel 5 memory address register
      DMA_CMAR5  : aliased HAL.UInt32;
      --  DMA channel 6 configuration register
      DMA_CCR6   : aliased DMA_CCR_Register;
      --  DMA channel 6 number of data to transfer register
      DMA_CNDTR6 : aliased DMA_CNDTR_Register;
      --  DMA channel 6 peripheral address register
      DMA_CPAR6  : aliased HAL.UInt32;
      --  DMA channel 6 memory address register
      DMA_CMAR6  : aliased HAL.UInt32;
      --  DMA channel 7 configuration register
      DMA_CCR7   : aliased DMA_CCR_Register;
      --  DMA channel 7 number of data to transfer register
      DMA_CNDTR7 : aliased DMA_CNDTR_Register;
      --  DMA channel 7 peripheral address register
      DMA_CPAR7  : aliased HAL.UInt32;
      --  DMA channel 7 memory address register
      DMA_CMAR7  : aliased HAL.UInt32;
   end record
     with Volatile;

   for DMA_Peripheral use record
      DMA_ISR    at 16#0# range 0 .. 31;
      DMA_IFCR   at 16#4# range 0 .. 31;
      DMA_CCR1   at 16#8# range 0 .. 31;
      DMA_CNDTR1 at 16#C# range 0 .. 31;
      DMA_CPAR1  at 16#10# range 0 .. 31;
      DMA_CMAR1  at 16#14# range 0 .. 31;
      DMA_CCR2   at 16#1C# range 0 .. 31;
      DMA_CNDTR2 at 16#20# range 0 .. 31;
      DMA_CPAR2  at 16#24# range 0 .. 31;
      DMA_CMAR2  at 16#28# range 0 .. 31;
      DMA_CCR3   at 16#30# range 0 .. 31;
      DMA_CNDTR3 at 16#34# range 0 .. 31;
      DMA_CPAR3  at 16#38# range 0 .. 31;
      DMA_CMAR3  at 16#3C# range 0 .. 31;
      DMA_CCR4   at 16#44# range 0 .. 31;
      DMA_CNDTR4 at 16#48# range 0 .. 31;
      DMA_CPAR4  at 16#4C# range 0 .. 31;
      DMA_CMAR4  at 16#50# range 0 .. 31;
      DMA_CCR5   at 16#58# range 0 .. 31;
      DMA_CNDTR5 at 16#5C# range 0 .. 31;
      DMA_CPAR5  at 16#60# range 0 .. 31;
      DMA_CMAR5  at 16#64# range 0 .. 31;
      DMA_CCR6   at 16#6C# range 0 .. 31;
      DMA_CNDTR6 at 16#70# range 0 .. 31;
      DMA_CPAR6  at 16#74# range 0 .. 31;
      DMA_CMAR6  at 16#78# range 0 .. 31;
      DMA_CCR7   at 16#80# range 0 .. 31;
      DMA_CNDTR7 at 16#84# range 0 .. 31;
      DMA_CPAR7  at 16#88# range 0 .. 31;
      DMA_CMAR7  at 16#8C# range 0 .. 31;
   end record;

   --  DMA register bank
   DMA1_Periph : aliased DMA_Peripheral
     with Import, Address => DMA1_Base;

   --  DMA register bank
   DMA2_Periph : aliased DMA_Peripheral
     with Import, Address => DMA2_Base;

end STM32_SVD.DMA;
