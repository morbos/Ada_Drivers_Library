--  This spec has been automatically generated from STM32L5x2.svd

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
     with Volatile_Full_Access, Size => 32,
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
     with Volatile_Full_Access, Size => 32,
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

   subtype CCR_PSIZE_Field is HAL.UInt2;
   subtype CCR_MSIZE_Field is HAL.UInt2;
   subtype CCR_PL_Field is HAL.UInt2;

   --  channel x configuration register
   type CCR_Register is record
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
      PSIZE          : CCR_PSIZE_Field := 16#0#;
      --  Memory size
      MSIZE          : CCR_MSIZE_Field := 16#0#;
      --  Channel priority level
      PL             : CCR_PL_Field := 16#0#;
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
     with Volatile_Full_Access, Size => 32,
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
      DBM            at 0 range 15 .. 15;
      CT             at 0 range 16 .. 16;
      SECM           at 0 range 17 .. 17;
      SSEC           at 0 range 18 .. 18;
      DSEC           at 0 range 19 .. 19;
      PRIV           at 0 range 20 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   subtype CNDTR_NDT_Field is HAL.UInt18;

   --  channel x number of data register
   type CNDTR_Register is record
      --  Number of data to transfer
      NDT            : CNDTR_NDT_Field := 16#0#;
      --  unspecified
      Reserved_18_31 : HAL.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CNDTR_Register use record
      NDT            at 0 range 0 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   subtype CM1AR_PSIZE_Field is HAL.UInt2;
   subtype CM1AR_MSIZE_Field is HAL.UInt2;
   subtype CM1AR_PL_Field is HAL.UInt2;

   --  channel x memory address register
   type CM1AR_Register is record
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
      PSIZE          : CM1AR_PSIZE_Field := 16#0#;
      --  Memory size
      MSIZE          : CM1AR_MSIZE_Field := 16#0#;
      --  Channel priority level
      PL             : CM1AR_PL_Field := 16#0#;
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
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CM1AR_Register use record
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

   subtype CCR_NDT_Field is HAL.UInt18;

   --  channel x configuration register
   type CCR_Register_1 is record
      --  Number of data to transfer
      NDT            : CCR_NDT_Field := 16#0#;
      --  unspecified
      Reserved_18_31 : HAL.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCR_Register_1 use record
      NDT            at 0 range 0 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   subtype CM0AR2_PSIZE_Field is HAL.UInt2;
   subtype CM0AR2_MSIZE_Field is HAL.UInt2;
   subtype CM0AR2_PL_Field is HAL.UInt2;

   --  channel x memory address register
   type CM0AR2_Register is record
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
      PSIZE          : CM0AR2_PSIZE_Field := 16#0#;
      --  Memory size
      MSIZE          : CM0AR2_MSIZE_Field := 16#0#;
      --  Channel priority level
      PL             : CM0AR2_PL_Field := 16#0#;
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
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CM0AR2_Register use record
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

   subtype CM1AR2_NDT_Field is HAL.UInt18;

   --  channel x memory address register
   type CM1AR2_Register is record
      --  Number of data to transfer
      NDT            : CM1AR2_NDT_Field := 16#0#;
      --  unspecified
      Reserved_18_31 : HAL.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CM1AR2_Register use record
      NDT            at 0 range 0 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   subtype CPAR3_PSIZE_Field is HAL.UInt2;
   subtype CPAR3_MSIZE_Field is HAL.UInt2;
   subtype CPAR3_PL_Field is HAL.UInt2;

   --  channel x peripheral address register
   type CPAR3_Register is record
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
      PSIZE          : CPAR3_PSIZE_Field := 16#0#;
      --  Memory size
      MSIZE          : CPAR3_MSIZE_Field := 16#0#;
      --  Channel priority level
      PL             : CPAR3_PL_Field := 16#0#;
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
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPAR3_Register use record
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

   subtype CM0AR3_NDT_Field is HAL.UInt18;

   --  channel x memory address register
   type CM0AR3_Register is record
      --  Number of data to transfer
      NDT            : CM0AR3_NDT_Field := 16#0#;
      --  unspecified
      Reserved_18_31 : HAL.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CM0AR3_Register use record
      NDT            at 0 range 0 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   subtype CNDTR4_PSIZE_Field is HAL.UInt2;
   subtype CNDTR4_MSIZE_Field is HAL.UInt2;
   subtype CNDTR4_PL_Field is HAL.UInt2;

   --  channel x number of data register
   type CNDTR4_Register is record
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
      PSIZE          : CNDTR4_PSIZE_Field := 16#0#;
      --  Memory size
      MSIZE          : CNDTR4_MSIZE_Field := 16#0#;
      --  Channel priority level
      PL             : CNDTR4_PL_Field := 16#0#;
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
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CNDTR4_Register use record
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

   subtype CPAR_NDT_Field is HAL.UInt18;

   --  channel x peripheral address register
   type CPAR_Register is record
      --  Number of data to transfer
      NDT            : CPAR_NDT_Field := 16#0#;
      --  unspecified
      Reserved_18_31 : HAL.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPAR_Register use record
      NDT            at 0 range 0 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   subtype CM0AR6_C1S_Field is HAL.UInt4;
   subtype CM0AR6_C2S_Field is HAL.UInt4;
   subtype CM0AR6_C3S_Field is HAL.UInt4;
   subtype CM0AR6_C4S_Field is HAL.UInt4;
   subtype CM0AR6_C5S_Field is HAL.UInt4;
   subtype CM0AR6_C6S_Field is HAL.UInt4;
   subtype CM0AR6_C7S_Field is HAL.UInt4;

   --  channel x memory address register
   type CM0AR6_Register is record
      --  DMA channel 1 selection
      C1S            : CM0AR6_C1S_Field := 16#0#;
      --  DMA channel 2 selection
      C2S            : CM0AR6_C2S_Field := 16#0#;
      --  DMA channel 3 selection
      C3S            : CM0AR6_C3S_Field := 16#0#;
      --  DMA channel 4 selection
      C4S            : CM0AR6_C4S_Field := 16#0#;
      --  DMA channel 5 selection
      C5S            : CM0AR6_C5S_Field := 16#0#;
      --  DMA channel 6 selection
      C6S            : CM0AR6_C6S_Field := 16#0#;
      --  DMA channel 7 selection
      C7S            : CM0AR6_C7S_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CM0AR6_Register use record
      C1S            at 0 range 0 .. 3;
      C2S            at 0 range 4 .. 7;
      C3S            at 0 range 8 .. 11;
      C4S            at 0 range 12 .. 15;
      C5S            at 0 range 16 .. 19;
      C6S            at 0 range 20 .. 23;
      C7S            at 0 range 24 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype CNDTR8_PSIZE_Field is HAL.UInt2;
   subtype CNDTR8_MSIZE_Field is HAL.UInt2;
   subtype CNDTR8_PL_Field is HAL.UInt2;

   --  channel x number of data register
   type CNDTR8_Register is record
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
      PSIZE          : CNDTR8_PSIZE_Field := 16#0#;
      --  memory size
      MSIZE          : CNDTR8_MSIZE_Field := 16#0#;
      --  priority level
      PL             : CNDTR8_PL_Field := 16#0#;
      --  memory-to-memory mode
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
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CNDTR8_Register use record
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
      --  channel x number of data register
      CNDTR1 : aliased CNDTR_Register;
      --  channel x peripheral address register
      CPAR1  : aliased HAL.UInt32;
      --  channel x memory address register
      CM0AR1 : aliased HAL.UInt32;
      --  channel x memory address register
      CM1AR1 : aliased CM1AR_Register;
      --  channel x configuration register
      CCR2   : aliased CCR_Register_1;
      --  channel x number of data register
      CNDTR2 : aliased HAL.UInt32;
      --  channel x peripheral address register
      CPAR2  : aliased HAL.UInt32;
      --  channel x memory address register
      CM0AR2 : aliased CM0AR2_Register;
      --  channel x memory address register
      CM1AR2 : aliased CM1AR2_Register;
      --  channel x configuration register
      CCR3   : aliased HAL.UInt32;
      --  channel x number of data register
      CNDTR3 : aliased HAL.UInt32;
      --  channel x peripheral address register
      CPAR3  : aliased CPAR3_Register;
      --  channel x memory address register
      CM0AR3 : aliased CM0AR3_Register;
      --  channel x memory address register
      CM1AR3 : aliased HAL.UInt32;
      --  channel x configuration register
      CCR4   : aliased HAL.UInt32;
      --  channel x number of data register
      CNDTR4 : aliased CNDTR4_Register;
      --  channel x peripheral address register
      CPAR4  : aliased CPAR_Register;
      --  channel x memory address register
      CM0AR4 : aliased HAL.UInt32;
      --  channel x memory address register
      CM1AR4 : aliased HAL.UInt32;
      --  channel x configuration register
      CCR5   : aliased CCR_Register;
      --  channel x number of data register
      CNDTR5 : aliased CNDTR_Register;
      --  channel x peripheral address register
      CPAR5  : aliased HAL.UInt32;
      --  channel x memory address register
      CM0AR5 : aliased HAL.UInt32;
      --  channel x memory address register
      CM1AR5 : aliased CM1AR_Register;
      --  channel x configuration register
      CCR6   : aliased CCR_Register_1;
      --  channel x number of data register
      CNDTR6 : aliased HAL.UInt32;
      --  channel x peripheral address register
      CPAR6  : aliased HAL.UInt32;
      --  channel x memory address register
      CM0AR6 : aliased CM0AR6_Register;
      --  channel x memory address register
      CM1AR6 : aliased HAL.UInt32;
      --  channel x configuration register
      CCR7   : aliased HAL.UInt32;
      --  channel x number of data register
      CNDTR7 : aliased HAL.UInt32;
      --  channel x peripheral address register
      CPAR7  : aliased HAL.UInt32;
      --  channel x memory address register
      CM0AR7 : aliased HAL.UInt32;
      --  channel x memory address register
      CM1AR7 : aliased HAL.UInt32;
      --  channel x configuration register
      CCR8   : aliased HAL.UInt32;
      --  channel x number of data register
      CNDTR8 : aliased CNDTR8_Register;
      --  channel x peripheral address register
      CPAR8  : aliased CPAR_Register;
      --  channel x peripheral address register
      CM0AR8 : aliased HAL.UInt32;
      --  channel x peripheral address register
      CM1AR8 : aliased HAL.UInt32;
      --  channel selection register
      CSELR  : aliased HAL.UInt32;
   end record
     with Volatile;

   for DMA_Peripheral use record
      ISR    at 16#0# range 0 .. 31;
      IFCR   at 16#4# range 0 .. 31;
      CCR1   at 16#8# range 0 .. 31;
      CNDTR1 at 16#C# range 0 .. 31;
      CPAR1  at 16#10# range 0 .. 31;
      CM0AR1 at 16#14# range 0 .. 31;
      CM1AR1 at 16#18# range 0 .. 31;
      CCR2   at 16#1C# range 0 .. 31;
      CNDTR2 at 16#20# range 0 .. 31;
      CPAR2  at 16#24# range 0 .. 31;
      CM0AR2 at 16#28# range 0 .. 31;
      CM1AR2 at 16#2C# range 0 .. 31;
      CCR3   at 16#30# range 0 .. 31;
      CNDTR3 at 16#34# range 0 .. 31;
      CPAR3  at 16#38# range 0 .. 31;
      CM0AR3 at 16#3C# range 0 .. 31;
      CM1AR3 at 16#40# range 0 .. 31;
      CCR4   at 16#44# range 0 .. 31;
      CNDTR4 at 16#48# range 0 .. 31;
      CPAR4  at 16#4C# range 0 .. 31;
      CM0AR4 at 16#50# range 0 .. 31;
      CM1AR4 at 16#54# range 0 .. 31;
      CCR5   at 16#58# range 0 .. 31;
      CNDTR5 at 16#5C# range 0 .. 31;
      CPAR5  at 16#60# range 0 .. 31;
      CM0AR5 at 16#64# range 0 .. 31;
      CM1AR5 at 16#68# range 0 .. 31;
      CCR6   at 16#6C# range 0 .. 31;
      CNDTR6 at 16#70# range 0 .. 31;
      CPAR6  at 16#74# range 0 .. 31;
      CM0AR6 at 16#78# range 0 .. 31;
      CM1AR6 at 16#7C# range 0 .. 31;
      CCR7   at 16#80# range 0 .. 31;
      CNDTR7 at 16#84# range 0 .. 31;
      CPAR7  at 16#88# range 0 .. 31;
      CM0AR7 at 16#8C# range 0 .. 31;
      CM1AR7 at 16#90# range 0 .. 31;
      CCR8   at 16#94# range 0 .. 31;
      CNDTR8 at 16#98# range 0 .. 31;
      CPAR8  at 16#9C# range 0 .. 31;
      CM0AR8 at 16#A0# range 0 .. 31;
      CM1AR8 at 16#A4# range 0 .. 31;
      CSELR  at 16#A8# range 0 .. 31;
   end record;

   --  Direct memory access controller
   DMA1_Periph : aliased DMA_Peripheral
     with Import, Address => System'To_Address (16#40020000#);

   --  Direct memory access controller
   DMA2_Periph : aliased DMA_Peripheral
     with Import, Address => System'To_Address (16#40020400#);

   --  Direct memory access controller
   SEC_DMA1_Periph : aliased DMA_Peripheral
     with Import, Address => System'To_Address (16#50020000#);

   --  Direct memory access controller
   SEC_DMA2_Periph : aliased DMA_Peripheral
     with Import, Address => System'To_Address (16#50020400#);

end STM32_SVD.DMA;
