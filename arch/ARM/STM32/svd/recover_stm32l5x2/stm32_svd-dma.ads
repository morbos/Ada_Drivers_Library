--  This spec has been automatically generated from xyzzy

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
      --  Read-only. Channel x global interrupt flag (x = 1 ..7)
      GIF1  : Boolean;
      --  Read-only. Channel x transfer complete flag (x = 1 ..7)
      TCIF1 : Boolean;
      --  Read-only. Channel x half transfer flag (x = 1 ..7)
      HTIF1 : Boolean;
      --  Read-only. Channel x transfer error flag (x = 1 ..7)
      TEIF1 : Boolean;
      --  Read-only. Channel x global interrupt flag (x = 1 ..7)
      GIF2  : Boolean;
      --  Read-only. Channel x transfer complete flag (x = 1 ..7)
      TCIF2 : Boolean;
      --  Read-only. Channel x half transfer flag (x = 1 ..7)
      HTIF2 : Boolean;
      --  Read-only. Channel x transfer error flag (x = 1 ..7)
      TEIF2 : Boolean;
      --  Read-only. Channel x global interrupt flag (x = 1 ..7)
      GIF3  : Boolean;
      --  Read-only. Channel x transfer complete flag (x = 1 ..7)
      TCIF3 : Boolean;
      --  Read-only. Channel x half transfer flag (x = 1 ..7)
      HTIF3 : Boolean;
      --  Read-only. Channel x transfer error flag (x = 1 ..7)
      TEIF3 : Boolean;
      --  Read-only. Channel x global interrupt flag (x = 1 ..7)
      GIF4  : Boolean;
      --  Read-only. Channel x transfer complete flag (x = 1 ..7)
      TCIF4 : Boolean;
      --  Read-only. Channel x half transfer flag (x = 1 ..7)
      HTIF4 : Boolean;
      --  Read-only. Channel x transfer error flag (x = 1 ..7)
      TEIF4 : Boolean;
      --  Read-only. Channel x global interrupt flag (x = 1 ..7)
      GIF5  : Boolean;
      --  Read-only. Channel x transfer complete flag (x = 1 ..7)
      TCIF5 : Boolean;
      --  Read-only. Channel x half transfer flag (x = 1 ..7)
      HTIF5 : Boolean;
      --  Read-only. Channel x transfer error flag (x = 1 ..7)
      TEIF5 : Boolean;
      --  Read-only. Channel x global interrupt flag (x = 1 ..7)
      GIF6  : Boolean;
      --  Read-only. Channel x transfer complete flag (x = 1 ..7)
      TCIF6 : Boolean;
      --  Read-only. Channel x half transfer flag (x = 1 ..7)
      HTIF6 : Boolean;
      --  Read-only. Channel x transfer error flag (x = 1 ..7)
      TEIF6 : Boolean;
      --  Read-only. Channel x global interrupt flag (x = 1 ..7)
      GIF7  : Boolean;
      --  Read-only. Channel x transfer complete flag (x = 1 ..7)
      TCIF7 : Boolean;
      --  Read-only. Channel x half transfer flag (x = 1 ..7)
      HTIF7 : Boolean;
      --  Read-only. Channel x transfer error flag (x = 1 ..7)
      TEIF7 : Boolean;
      --  Read-only. global interrupt flag for channel 8
      GIF8  : Boolean;
      --  Read-only. transfer complete (TC) flag for channel 8
      TCIF8 : Boolean;
      --  Read-only. half transfer (HT) flag for channel 8
      HTIF8 : Boolean;
      --  Read-only. transfer error (TE) flag for channel 8
      TEIF8 : Boolean;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISR_Register use record
      GIF1  at 0 range 0 .. 0;
      TCIF1 at 0 range 1 .. 1;
      HTIF1 at 0 range 2 .. 2;
      TEIF1 at 0 range 3 .. 3;
      GIF2  at 0 range 4 .. 4;
      TCIF2 at 0 range 5 .. 5;
      HTIF2 at 0 range 6 .. 6;
      TEIF2 at 0 range 7 .. 7;
      GIF3  at 0 range 8 .. 8;
      TCIF3 at 0 range 9 .. 9;
      HTIF3 at 0 range 10 .. 10;
      TEIF3 at 0 range 11 .. 11;
      GIF4  at 0 range 12 .. 12;
      TCIF4 at 0 range 13 .. 13;
      HTIF4 at 0 range 14 .. 14;
      TEIF4 at 0 range 15 .. 15;
      GIF5  at 0 range 16 .. 16;
      TCIF5 at 0 range 17 .. 17;
      HTIF5 at 0 range 18 .. 18;
      TEIF5 at 0 range 19 .. 19;
      GIF6  at 0 range 20 .. 20;
      TCIF6 at 0 range 21 .. 21;
      HTIF6 at 0 range 22 .. 22;
      TEIF6 at 0 range 23 .. 23;
      GIF7  at 0 range 24 .. 24;
      TCIF7 at 0 range 25 .. 25;
      HTIF7 at 0 range 26 .. 26;
      TEIF7 at 0 range 27 .. 27;
      GIF8  at 0 range 28 .. 28;
      TCIF8 at 0 range 29 .. 29;
      HTIF8 at 0 range 30 .. 30;
      TEIF8 at 0 range 31 .. 31;
   end record;

   --  interrupt flag clear register
   type IFCR_Register is record
      --  Write-only. Channel x global interrupt clear (x = 1 ..7)
      CGIF1  : Boolean := False;
      --  Write-only. Channel x transfer complete clear (x = 1 ..7)
      CTCIF1 : Boolean := False;
      --  Write-only. Channel x half transfer clear (x = 1 ..7)
      CHTIF1 : Boolean := False;
      --  Write-only. Channel x transfer error clear (x = 1 ..7)
      CTEIF1 : Boolean := False;
      --  Write-only. Channel x global interrupt clear (x = 1 ..7)
      CGIF2  : Boolean := False;
      --  Write-only. Channel x transfer complete clear (x = 1 ..7)
      CTCIF2 : Boolean := False;
      --  Write-only. Channel x half transfer clear (x = 1 ..7)
      CHTIF2 : Boolean := False;
      --  Write-only. Channel x transfer error clear (x = 1 ..7)
      CTEIF2 : Boolean := False;
      --  Write-only. Channel x global interrupt clear (x = 1 ..7)
      CGIF3  : Boolean := False;
      --  Write-only. Channel x transfer complete clear (x = 1 ..7)
      CTCIF3 : Boolean := False;
      --  Write-only. Channel x half transfer clear (x = 1 ..7)
      CHTIF3 : Boolean := False;
      --  Write-only. Channel x transfer error clear (x = 1 ..7)
      CTEIF3 : Boolean := False;
      --  Write-only. Channel x global interrupt clear (x = 1 ..7)
      CGIF4  : Boolean := False;
      --  Write-only. Channel x transfer complete clear (x = 1 ..7)
      CTCIF4 : Boolean := False;
      --  Write-only. Channel x half transfer clear (x = 1 ..7)
      CHTIF4 : Boolean := False;
      --  Write-only. Channel x transfer error clear (x = 1 ..7)
      CTEIF4 : Boolean := False;
      --  Write-only. Channel x global interrupt clear (x = 1 ..7)
      CGIF5  : Boolean := False;
      --  Write-only. Channel x transfer complete clear (x = 1 ..7)
      CTCIF5 : Boolean := False;
      --  Write-only. Channel x half transfer clear (x = 1 ..7)
      CHTIF5 : Boolean := False;
      --  Write-only. Channel x transfer error clear (x = 1 ..7)
      CTEIF5 : Boolean := False;
      --  Write-only. Channel x global interrupt clear (x = 1 ..7)
      CGIF6  : Boolean := False;
      --  Write-only. Channel x transfer complete clear (x = 1 ..7)
      CTCIF6 : Boolean := False;
      --  Write-only. Channel x half transfer clear (x = 1 ..7)
      CHTIF6 : Boolean := False;
      --  Write-only. Channel x transfer error clear (x = 1 ..7)
      CTEIF6 : Boolean := False;
      --  Write-only. Channel x global interrupt clear (x = 1 ..7)
      CGIF7  : Boolean := False;
      --  Write-only. Channel x transfer complete clear (x = 1 ..7)
      CTCIF7 : Boolean := False;
      --  Write-only. Channel x half transfer clear (x = 1 ..7)
      CHTIF7 : Boolean := False;
      --  Write-only. Channel x transfer error clear (x = 1 ..7)
      CTEIF7 : Boolean := False;
      --  Write-only. global interrupt flag clear for channel 8
      CGIF8  : Boolean := False;
      --  Write-only. transfer complete flag clear for channel 8
      CTCIF8 : Boolean := False;
      --  Write-only. half transfer flag clear for channel 8
      CHTIF8 : Boolean := False;
      --  Write-only. transfer error flag clear for channel 8
      CTEIF8 : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IFCR_Register use record
      CGIF1  at 0 range 0 .. 0;
      CTCIF1 at 0 range 1 .. 1;
      CHTIF1 at 0 range 2 .. 2;
      CTEIF1 at 0 range 3 .. 3;
      CGIF2  at 0 range 4 .. 4;
      CTCIF2 at 0 range 5 .. 5;
      CHTIF2 at 0 range 6 .. 6;
      CTEIF2 at 0 range 7 .. 7;
      CGIF3  at 0 range 8 .. 8;
      CTCIF3 at 0 range 9 .. 9;
      CHTIF3 at 0 range 10 .. 10;
      CTEIF3 at 0 range 11 .. 11;
      CGIF4  at 0 range 12 .. 12;
      CTCIF4 at 0 range 13 .. 13;
      CHTIF4 at 0 range 14 .. 14;
      CTEIF4 at 0 range 15 .. 15;
      CGIF5  at 0 range 16 .. 16;
      CTCIF5 at 0 range 17 .. 17;
      CHTIF5 at 0 range 18 .. 18;
      CTEIF5 at 0 range 19 .. 19;
      CGIF6  at 0 range 20 .. 20;
      CTCIF6 at 0 range 21 .. 21;
      CHTIF6 at 0 range 22 .. 22;
      CTEIF6 at 0 range 23 .. 23;
      CGIF7  at 0 range 24 .. 24;
      CTCIF7 at 0 range 25 .. 25;
      CHTIF7 at 0 range 26 .. 26;
      CTEIF7 at 0 range 27 .. 27;
      CGIF8  at 0 range 28 .. 28;
      CTCIF8 at 0 range 29 .. 29;
      CHTIF8 at 0 range 30 .. 30;
      CTEIF8 at 0 range 31 .. 31;
   end record;

   ----------------------------
   -- CH cluster's Registers --
   ----------------------------

   subtype CCRx_CH_PSIZE_Field is HAL.UInt2;
   subtype CCRx_CH_MSIZE_Field is HAL.UInt2;
   subtype CCRx_CH_PL_Field is HAL.UInt2;

   --  channel x configuration register
   type CCRx_CH_Register is record
      --  Channel enable
      EN             : Boolean := False;
      --  Transfer complete interrupt enable
      TCIE           : Boolean := False;
      --  Half transfer interrupt enable
      HTIE           : Boolean := False;
      --  Transfer error interrupt enable
      TEIE           : Boolean := False;
      --  Data transfer direction
      DIR            : Boolean := False;
      --  Circular mode
      CIRC           : Boolean := False;
      --  Peripheral increment mode
      PINC           : Boolean := False;
      --  Memory increment mode
      MINC           : Boolean := False;
      --  Peripheral size
      PSIZE          : CCRx_CH_PSIZE_Field := 16#0#;
      --  Memory size
      MSIZE          : CCRx_CH_MSIZE_Field := 16#0#;
      --  Channel priority level
      PL             : CCRx_CH_PL_Field := 16#0#;
      --  Memory to memory mode
      MEM2MEM        : Boolean := False;
      --  double-buffer mode
      DBM            : Boolean := False;
      --  current target memory of DMA transfer in double-buffer mode
      CT             : Boolean := False;
      --  secure mode
      SECM           : Boolean := False;
      --  security of the DMA transfer from the source
      SSEC           : Boolean := False;
      --  security of the DMA transfer to the destination
      DSEC           : Boolean := False;
      --  privileged mode
      PRIV           : Boolean := False;
      --  unspecified
      Reserved_21_31 : HAL.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCRx_CH_Register use record
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
      DBM            at 0 range 15 .. 15;
      CT             at 0 range 16 .. 16;
      SECM           at 0 range 17 .. 17;
      SSEC           at 0 range 18 .. 18;
      DSEC           at 0 range 19 .. 19;
      PRIV           at 0 range 20 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   subtype CNDTRx_CH_NDT_Field is HAL.UInt18;

   --  channel x number of data register
   type CNDTRx_CH_Register is record
      --  Number of data to transfer
      NDT            : CNDTRx_CH_NDT_Field := 16#0#;
      --  unspecified
      Reserved_18_31 : HAL.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CNDTRx_CH_Register use record
      NDT            at 0 range 0 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   --  

   type ARy_CH_Registers is array (0 .. 1) of HAL.UInt32;

   type CH_Cluster is record
      --  channel x configuration register
      CCRx   : aliased CCRx_CH_Register;
      --  channel x number of data register
      CNDTRx : aliased CNDTRx_CH_Register;
      --  channel x peripheral address register
      CPARx  : aliased HAL.UInt32;
      ARy    : aliased ARy_CH_Registers;
   end record
     with Size => 160;

   for CH_Cluster use record
      CCRx   at 16#0# range 0 .. 31;
      CNDTRx at 16#4# range 0 .. 31;
      CPARx  at 16#8# range 0 .. 31;
      ARy    at 16#C# range 0 .. 63;
   end record;

   type CH_Clusters is array (1 .. 8) of CH_Cluster;

   ----------------------------
   -- CH cluster's Registers --
   ----------------------------

   --  

   ----------------------------
   -- CH cluster's Registers --
   ----------------------------

   --  

   ----------------------------
   -- CH cluster's Registers --
   ----------------------------

   --  

   -----------------
   -- Peripherals --
   -----------------

   --  Direct memory access controller
   type DMA_Peripheral is record
      --  interrupt status register
      ISR   : aliased ISR_Register;
      --  interrupt flag clear register
      IFCR  : aliased IFCR_Register;
      CH    : aliased CH_Clusters;
      --  channel selection register
      CSELR : aliased HAL.UInt32;
   end record
     with Volatile;

   for DMA_Peripheral use record
      ISR   at 16#0# range 0 .. 31;
      IFCR  at 16#4# range 0 .. 31;
      CH    at 16#8# range 0 .. 1279;
      CSELR at 16#A8# range 0 .. 31;
   end record;

   --  Direct memory access controller
   DMA1_Periph : aliased DMA_Peripheral
     with Import, Address => DMA1_Base;

   --  Direct memory access controller
   DMA2_Periph : aliased DMA_Peripheral
     with Import, Address => DMA2_Base;

   --  Direct memory access controller
   SEC_DMA1_Periph : aliased DMA_Peripheral
     with Import, Address => SEC_DMA1_Base;

   --  Direct memory access controller
   SEC_DMA2_Periph : aliased DMA_Peripheral
     with Import, Address => SEC_DMA2_Base;

end STM32_SVD.DMA;
