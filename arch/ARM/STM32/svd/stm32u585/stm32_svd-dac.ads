--  This spec has been automatically generated from STM32U585.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.DAC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype DAC_CR_TSEL1_Field is HAL.UInt4;
   subtype DAC_CR_WAVE1_Field is HAL.UInt2;
   subtype DAC_CR_MAMP1_Field is HAL.UInt4;
   subtype DAC_CR_TSEL2_Field is HAL.UInt4;
   subtype DAC_CR_WAVE2_Field is HAL.UInt2;
   subtype DAC_CR_MAMP2_Field is HAL.UInt4;

   --  DAC control register
   type DAC_CR_Register is record
      --  DAC channel1 enable
      EN1            : Boolean := False;
      --  DAC channel1 trigger enable
      TEN1           : Boolean := False;
      --  DAC channel1 trigger selection
      TSEL1          : DAC_CR_TSEL1_Field := 16#0#;
      --  DAC channel1 noise/triangle wave generation enable
      WAVE1          : DAC_CR_WAVE1_Field := 16#0#;
      --  DAC channel1 mask/amplitude selector
      MAMP1          : DAC_CR_MAMP1_Field := 16#0#;
      --  DAC channel1 DMA enable
      DMAEN1         : Boolean := False;
      --  DAC channel1 DMA Underrun Interrupt enable
      DMAUDRIE1      : Boolean := False;
      --  DAC channel1 calibration enable
      CEN1           : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  DAC channel2 enable
      EN2            : Boolean := False;
      --  DAC channel2 trigger enable
      TEN2           : Boolean := False;
      --  DAC channel2 trigger selection
      TSEL2          : DAC_CR_TSEL2_Field := 16#0#;
      --  DAC channel2 noise/triangle wave generation enable
      WAVE2          : DAC_CR_WAVE2_Field := 16#0#;
      --  DAC channel2 mask/amplitude selector
      MAMP2          : DAC_CR_MAMP2_Field := 16#0#;
      --  DAC channel2 DMA enable
      DMAEN2         : Boolean := False;
      --  DAC channel2 DMA underrun interrupt enable
      DMAUDRIE2      : Boolean := False;
      --  DAC channel2 calibration enable
      CEN2           : Boolean := False;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DAC_CR_Register use record
      EN1            at 0 range 0 .. 0;
      TEN1           at 0 range 1 .. 1;
      TSEL1          at 0 range 2 .. 5;
      WAVE1          at 0 range 6 .. 7;
      MAMP1          at 0 range 8 .. 11;
      DMAEN1         at 0 range 12 .. 12;
      DMAUDRIE1      at 0 range 13 .. 13;
      CEN1           at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      EN2            at 0 range 16 .. 16;
      TEN2           at 0 range 17 .. 17;
      TSEL2          at 0 range 18 .. 21;
      WAVE2          at 0 range 22 .. 23;
      MAMP2          at 0 range 24 .. 27;
      DMAEN2         at 0 range 28 .. 28;
      DMAUDRIE2      at 0 range 29 .. 29;
      CEN2           at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   --  DAC_SWTRGR_SWTRIG array
   type DAC_SWTRGR_SWTRIG_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for DAC_SWTRGR_SWTRIG
   type DAC_SWTRGR_SWTRIG_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SWTRIG as a value
            Val : HAL.UInt2;
         when True =>
            --  SWTRIG as an array
            Arr : DAC_SWTRGR_SWTRIG_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for DAC_SWTRGR_SWTRIG_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  DAC software trigger register
   type DAC_SWTRGR_Register is record
      --  Write-only. DAC channel1 software trigger
      SWTRIG        : DAC_SWTRGR_SWTRIG_Field :=
                       (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DAC_SWTRGR_Register use record
      SWTRIG        at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   subtype DAC_DHR12R1_DACC1DHR_Field is HAL.UInt12;
   subtype DAC_DHR12R1_DACC1DHRB_Field is HAL.UInt12;

   --  DAC channel1 12-bit right-aligned data holding register
   type DAC_DHR12R1_Register is record
      --  DAC channel1 12-bit right-aligned data
      DACC1DHR       : DAC_DHR12R1_DACC1DHR_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  DAC channel1 12-bit right-aligned data B
      DACC1DHRB      : DAC_DHR12R1_DACC1DHRB_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DAC_DHR12R1_Register use record
      DACC1DHR       at 0 range 0 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      DACC1DHRB      at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype DAC_DHR12L1_DACC1DHR_Field is HAL.UInt12;
   subtype DAC_DHR12L1_DACC1DHRB_Field is HAL.UInt12;

   --  DAC channel1 12-bit left aligned data holding register
   type DAC_DHR12L1_Register is record
      --  unspecified
      Reserved_0_3   : HAL.UInt4 := 16#0#;
      --  DAC channel1 12-bit left-aligned data
      DACC1DHR       : DAC_DHR12L1_DACC1DHR_Field := 16#0#;
      --  unspecified
      Reserved_16_19 : HAL.UInt4 := 16#0#;
      --  DAC channel1 12-bit left-aligned data B
      DACC1DHRB      : DAC_DHR12L1_DACC1DHRB_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DAC_DHR12L1_Register use record
      Reserved_0_3   at 0 range 0 .. 3;
      DACC1DHR       at 0 range 4 .. 15;
      Reserved_16_19 at 0 range 16 .. 19;
      DACC1DHRB      at 0 range 20 .. 31;
   end record;

   subtype DAC_DHR8R1_DACC1DHR_Field is HAL.UInt8;
   subtype DAC_DHR8R1_DACC1DHRB_Field is HAL.UInt8;

   --  DAC channel1 8-bit right aligned data holding register
   type DAC_DHR8R1_Register is record
      --  DAC channel1 8-bit right-aligned data
      DACC1DHR       : DAC_DHR8R1_DACC1DHR_Field := 16#0#;
      --  DAC channel1 8-bit right-aligned Sdata
      DACC1DHRB      : DAC_DHR8R1_DACC1DHRB_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DAC_DHR8R1_Register use record
      DACC1DHR       at 0 range 0 .. 7;
      DACC1DHRB      at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DAC_DHR12R2_DACC2DHR_Field is HAL.UInt12;
   subtype DAC_DHR12R2_DACC2DHRB_Field is HAL.UInt12;

   --  DAC channel2 12-bit right aligned data holding register
   type DAC_DHR12R2_Register is record
      --  DAC channel2 12-bit right-aligned data
      DACC2DHR       : DAC_DHR12R2_DACC2DHR_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  DAC channel2 12-bit right-aligned data
      DACC2DHRB      : DAC_DHR12R2_DACC2DHRB_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DAC_DHR12R2_Register use record
      DACC2DHR       at 0 range 0 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      DACC2DHRB      at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype DAC_DHR12L2_DACC2DHR_Field is HAL.UInt12;
   subtype DAC_DHR12L2_DACC2DHRB_Field is HAL.UInt12;

   --  DAC channel2 12-bit left aligned data holding register
   type DAC_DHR12L2_Register is record
      --  unspecified
      Reserved_0_3   : HAL.UInt4 := 16#0#;
      --  DAC channel2 12-bit left-aligned data
      DACC2DHR       : DAC_DHR12L2_DACC2DHR_Field := 16#0#;
      --  unspecified
      Reserved_16_19 : HAL.UInt4 := 16#0#;
      --  DAC channel2 12-bit left-aligned data B
      DACC2DHRB      : DAC_DHR12L2_DACC2DHRB_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DAC_DHR12L2_Register use record
      Reserved_0_3   at 0 range 0 .. 3;
      DACC2DHR       at 0 range 4 .. 15;
      Reserved_16_19 at 0 range 16 .. 19;
      DACC2DHRB      at 0 range 20 .. 31;
   end record;

   subtype DAC_DHR8R2_DACC2DHR_Field is HAL.UInt8;
   subtype DAC_DHR8R2_DACC2DHRB_Field is HAL.UInt8;

   --  DAC channel2 8-bit right-aligned data holding register
   type DAC_DHR8R2_Register is record
      --  DAC channel2 8-bit right-aligned data
      DACC2DHR       : DAC_DHR8R2_DACC2DHR_Field := 16#0#;
      --  DAC channel2 8-bit right-aligned data
      DACC2DHRB      : DAC_DHR8R2_DACC2DHRB_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DAC_DHR8R2_Register use record
      DACC2DHR       at 0 range 0 .. 7;
      DACC2DHRB      at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DAC_DHR12RD_DACC1DHR_Field is HAL.UInt12;
   subtype DAC_DHR12RD_DACC2DHR_Field is HAL.UInt12;

   --  Dual DAC 12-bit right-aligned data holding register
   type DAC_DHR12RD_Register is record
      --  DAC channel1 12-bit right-aligned data
      DACC1DHR       : DAC_DHR12RD_DACC1DHR_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  DAC channel2 12-bit right-aligned data
      DACC2DHR       : DAC_DHR12RD_DACC2DHR_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DAC_DHR12RD_Register use record
      DACC1DHR       at 0 range 0 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      DACC2DHR       at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype DAC_DHR12LD_DACC1DHR_Field is HAL.UInt12;
   subtype DAC_DHR12LD_DACC2DHR_Field is HAL.UInt12;

   --  DUAL DAC 12-bit left aligned data holding register
   type DAC_DHR12LD_Register is record
      --  unspecified
      Reserved_0_3   : HAL.UInt4 := 16#0#;
      --  DAC channel1 12-bit left-aligned data
      DACC1DHR       : DAC_DHR12LD_DACC1DHR_Field := 16#0#;
      --  unspecified
      Reserved_16_19 : HAL.UInt4 := 16#0#;
      --  DAC channel2 12-bit left-aligned data
      DACC2DHR       : DAC_DHR12LD_DACC2DHR_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DAC_DHR12LD_Register use record
      Reserved_0_3   at 0 range 0 .. 3;
      DACC1DHR       at 0 range 4 .. 15;
      Reserved_16_19 at 0 range 16 .. 19;
      DACC2DHR       at 0 range 20 .. 31;
   end record;

   subtype DAC_DHR8RD_DACC1DHR_Field is HAL.UInt8;
   subtype DAC_DHR8RD_DACC2DHR_Field is HAL.UInt8;

   --  DUAL DAC 8-bit right aligned data holding register
   type DAC_DHR8RD_Register is record
      --  DAC channel1 8-bit right-aligned data
      DACC1DHR       : DAC_DHR8RD_DACC1DHR_Field := 16#0#;
      --  DAC channel2 8-bit right-aligned data
      DACC2DHR       : DAC_DHR8RD_DACC2DHR_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DAC_DHR8RD_Register use record
      DACC1DHR       at 0 range 0 .. 7;
      DACC2DHR       at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DAC_DOR1_DACC1DOR_Field is HAL.UInt12;
   subtype DAC_DOR1_DACC1DORB_Field is HAL.UInt12;

   --  DAC channel1 data output register
   type DAC_DOR1_Register is record
      --  Read-only. DAC channel1 data output
      DACC1DOR       : DAC_DOR1_DACC1DOR_Field;
      --  unspecified
      Reserved_12_15 : HAL.UInt4;
      --  Read-only. DAC channel1 data output
      DACC1DORB      : DAC_DOR1_DACC1DORB_Field;
      --  unspecified
      Reserved_28_31 : HAL.UInt4;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DAC_DOR1_Register use record
      DACC1DOR       at 0 range 0 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      DACC1DORB      at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype DAC_DOR2_DACC2DOR_Field is HAL.UInt12;
   subtype DAC_DOR2_DACC2DORB_Field is HAL.UInt12;

   --  DAC channel2 data output register
   type DAC_DOR2_Register is record
      --  Read-only. DAC channel2 data output
      DACC2DOR       : DAC_DOR2_DACC2DOR_Field;
      --  unspecified
      Reserved_12_15 : HAL.UInt4;
      --  Read-only. DAC channel2 data output
      DACC2DORB      : DAC_DOR2_DACC2DORB_Field;
      --  unspecified
      Reserved_28_31 : HAL.UInt4;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DAC_DOR2_Register use record
      DACC2DOR       at 0 range 0 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      DACC2DORB      at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  DAC status register
   type DAC_SR_Register is record
      --  unspecified
      Reserved_0_10  : HAL.UInt11 := 16#0#;
      --  Read-only. DAC channel1 ready status bit
      DAC1RDY        : Boolean := False;
      --  Read-only. DAC channel1 output register status bit
      DORSTAT1       : Boolean := False;
      --  DAC channel1 DMA underrun flag
      DMAUDR1        : Boolean := False;
      --  Read-only. DAC Channel 1 calibration offset status
      CAL_FLAG1      : Boolean := False;
      --  Read-only. DAC Channel 1 busy writing sample time flag
      BWST1          : Boolean := False;
      --  unspecified
      Reserved_16_26 : HAL.UInt11 := 16#0#;
      --  Read-only. DAC channel 2 ready status bit
      DAC2RDY        : Boolean := False;
      --  Read-only. DAC channel 2 output register status bit
      DORSTAT2       : Boolean := False;
      --  DAC channel2 DMA underrun flag
      DMAUDR2        : Boolean := False;
      --  Read-only. DAC Channel 2 calibration offset status
      CAL_FLAG2      : Boolean := False;
      --  Read-only. DAC Channel 2 busy writing sample time flag
      BWST2          : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DAC_SR_Register use record
      Reserved_0_10  at 0 range 0 .. 10;
      DAC1RDY        at 0 range 11 .. 11;
      DORSTAT1       at 0 range 12 .. 12;
      DMAUDR1        at 0 range 13 .. 13;
      CAL_FLAG1      at 0 range 14 .. 14;
      BWST1          at 0 range 15 .. 15;
      Reserved_16_26 at 0 range 16 .. 26;
      DAC2RDY        at 0 range 27 .. 27;
      DORSTAT2       at 0 range 28 .. 28;
      DMAUDR2        at 0 range 29 .. 29;
      CAL_FLAG2      at 0 range 30 .. 30;
      BWST2          at 0 range 31 .. 31;
   end record;

   subtype DAC_CCR_OTRIM1_Field is HAL.UInt5;
   subtype DAC_CCR_OTRIM2_Field is HAL.UInt5;

   --  DAC calibration control register
   type DAC_CCR_Register is record
      --  DAC Channel 1 offset trimming value
      OTRIM1         : DAC_CCR_OTRIM1_Field := 16#0#;
      --  unspecified
      Reserved_5_15  : HAL.UInt11 := 16#0#;
      --  DAC Channel 2 offset trimming value
      OTRIM2         : DAC_CCR_OTRIM2_Field := 16#0#;
      --  unspecified
      Reserved_21_31 : HAL.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DAC_CCR_Register use record
      OTRIM1         at 0 range 0 .. 4;
      Reserved_5_15  at 0 range 5 .. 15;
      OTRIM2         at 0 range 16 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   subtype DAC_MCR_MODE1_Field is HAL.UInt3;
   subtype DAC_MCR_HFSEL_Field is HAL.UInt2;
   subtype DAC_MCR_MODE2_Field is HAL.UInt3;

   --  DAC mode control register
   type DAC_MCR_Register is record
      --  DAC Channel 1 mode
      MODE1          : DAC_MCR_MODE1_Field := 16#0#;
      --  unspecified
      Reserved_3_7   : HAL.UInt5 := 16#0#;
      --  DAC Channel1 DMA double data mode
      DMADOUBLE1     : Boolean := False;
      --  Enable signed format for DAC channel1
      SINFORMAT1     : Boolean := False;
      --  unspecified
      Reserved_10_13 : HAL.UInt4 := 16#0#;
      --  High frequency interface mode selection
      HFSEL          : DAC_MCR_HFSEL_Field := 16#0#;
      --  DAC Channel 2 mode
      MODE2          : DAC_MCR_MODE2_Field := 16#0#;
      --  unspecified
      Reserved_19_23 : HAL.UInt5 := 16#0#;
      --  DAC Channel2 DMA double data mode
      DMADOUBLE2     : Boolean := False;
      --  Enable signed format for DAC channel2
      SINFORMAT2     : Boolean := False;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DAC_MCR_Register use record
      MODE1          at 0 range 0 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      DMADOUBLE1     at 0 range 8 .. 8;
      SINFORMAT1     at 0 range 9 .. 9;
      Reserved_10_13 at 0 range 10 .. 13;
      HFSEL          at 0 range 14 .. 15;
      MODE2          at 0 range 16 .. 18;
      Reserved_19_23 at 0 range 19 .. 23;
      DMADOUBLE2     at 0 range 24 .. 24;
      SINFORMAT2     at 0 range 25 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype DAC_SHSR1_TSAMPLE1_Field is HAL.UInt10;

   --  DAC Sample and Hold sample time register 1
   type DAC_SHSR1_Register is record
      --  DAC Channel 1 sample Time (only valid in sample &amp; hold mode)
      TSAMPLE1       : DAC_SHSR1_TSAMPLE1_Field := 16#0#;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DAC_SHSR1_Register use record
      TSAMPLE1       at 0 range 0 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype DAC_SHSR2_TSAMPLE2_Field is HAL.UInt10;

   --  DAC channel2 sample and hold sample time register
   type DAC_SHSR2_Register is record
      --  DAC Channel 2 sample Time (only valid in sample and hold mode)
      TSAMPLE2       : DAC_SHSR2_TSAMPLE2_Field := 16#0#;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DAC_SHSR2_Register use record
      TSAMPLE2       at 0 range 0 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype DAC_SHHR_THOLD1_Field is HAL.UInt10;
   subtype DAC_SHHR_THOLD2_Field is HAL.UInt10;

   --  DAC Sample and Hold hold time register
   type DAC_SHHR_Register is record
      --  DAC Channel 1 hold Time (only valid in sample and hold mode)
      THOLD1         : DAC_SHHR_THOLD1_Field := 16#1#;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  DAC Channel 2 hold time (only valid in sample and hold mode)
      THOLD2         : DAC_SHHR_THOLD2_Field := 16#1#;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DAC_SHHR_Register use record
      THOLD1         at 0 range 0 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      THOLD2         at 0 range 16 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype DAC_SHRR_TREFRESH1_Field is HAL.UInt8;
   subtype DAC_SHRR_TREFRESH2_Field is HAL.UInt8;

   --  DAC Sample and Hold refresh time register
   type DAC_SHRR_Register is record
      --  DAC Channel 1 refresh Time (only valid in sample and hold mode)
      TREFRESH1      : DAC_SHRR_TREFRESH1_Field := 16#1#;
      --  unspecified
      Reserved_8_15  : HAL.UInt8 := 16#0#;
      --  DAC Channel 2 refresh Time (only valid in sample and hold mode)
      TREFRESH2      : DAC_SHRR_TREFRESH2_Field := 16#1#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DAC_SHRR_Register use record
      TREFRESH1      at 0 range 0 .. 7;
      Reserved_8_15  at 0 range 8 .. 15;
      TREFRESH2      at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  Autonomous mode control register
   type DAC_AUTOCR_Register is record
      --  unspecified
      Reserved_0_21  : HAL.UInt22 := 16#0#;
      --  DAC Autonomous mode
      AUTOMODE       : Boolean := False;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DAC_AUTOCR_Register use record
      Reserved_0_21  at 0 range 0 .. 21;
      AUTOMODE       at 0 range 22 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Digital-to-analog converter
   type DAC_Peripheral is record
      --  DAC control register
      DAC_CR      : aliased DAC_CR_Register;
      --  DAC software trigger register
      DAC_SWTRGR  : aliased DAC_SWTRGR_Register;
      --  DAC channel1 12-bit right-aligned data holding register
      DAC_DHR12R1 : aliased DAC_DHR12R1_Register;
      --  DAC channel1 12-bit left aligned data holding register
      DAC_DHR12L1 : aliased DAC_DHR12L1_Register;
      --  DAC channel1 8-bit right aligned data holding register
      DAC_DHR8R1  : aliased DAC_DHR8R1_Register;
      --  DAC channel2 12-bit right aligned data holding register
      DAC_DHR12R2 : aliased DAC_DHR12R2_Register;
      --  DAC channel2 12-bit left aligned data holding register
      DAC_DHR12L2 : aliased DAC_DHR12L2_Register;
      --  DAC channel2 8-bit right-aligned data holding register
      DAC_DHR8R2  : aliased DAC_DHR8R2_Register;
      --  Dual DAC 12-bit right-aligned data holding register
      DAC_DHR12RD : aliased DAC_DHR12RD_Register;
      --  DUAL DAC 12-bit left aligned data holding register
      DAC_DHR12LD : aliased DAC_DHR12LD_Register;
      --  DUAL DAC 8-bit right aligned data holding register
      DAC_DHR8RD  : aliased DAC_DHR8RD_Register;
      --  DAC channel1 data output register
      DAC_DOR1    : aliased DAC_DOR1_Register;
      --  DAC channel2 data output register
      DAC_DOR2    : aliased DAC_DOR2_Register;
      --  DAC status register
      DAC_SR      : aliased DAC_SR_Register;
      --  DAC calibration control register
      DAC_CCR     : aliased DAC_CCR_Register;
      --  DAC mode control register
      DAC_MCR     : aliased DAC_MCR_Register;
      --  DAC Sample and Hold sample time register 1
      DAC_SHSR1   : aliased DAC_SHSR1_Register;
      --  DAC channel2 sample and hold sample time register
      DAC_SHSR2   : aliased DAC_SHSR2_Register;
      --  DAC Sample and Hold hold time register
      DAC_SHHR    : aliased DAC_SHHR_Register;
      --  DAC Sample and Hold refresh time register
      DAC_SHRR    : aliased DAC_SHRR_Register;
      --  Autonomous mode control register
      DAC_AUTOCR  : aliased DAC_AUTOCR_Register;
   end record
     with Volatile;

   for DAC_Peripheral use record
      DAC_CR      at 16#0# range 0 .. 31;
      DAC_SWTRGR  at 16#4# range 0 .. 31;
      DAC_DHR12R1 at 16#8# range 0 .. 31;
      DAC_DHR12L1 at 16#C# range 0 .. 31;
      DAC_DHR8R1  at 16#10# range 0 .. 31;
      DAC_DHR12R2 at 16#14# range 0 .. 31;
      DAC_DHR12L2 at 16#18# range 0 .. 31;
      DAC_DHR8R2  at 16#1C# range 0 .. 31;
      DAC_DHR12RD at 16#20# range 0 .. 31;
      DAC_DHR12LD at 16#24# range 0 .. 31;
      DAC_DHR8RD  at 16#28# range 0 .. 31;
      DAC_DOR1    at 16#2C# range 0 .. 31;
      DAC_DOR2    at 16#30# range 0 .. 31;
      DAC_SR      at 16#34# range 0 .. 31;
      DAC_CCR     at 16#38# range 0 .. 31;
      DAC_MCR     at 16#3C# range 0 .. 31;
      DAC_SHSR1   at 16#40# range 0 .. 31;
      DAC_SHSR2   at 16#44# range 0 .. 31;
      DAC_SHHR    at 16#48# range 0 .. 31;
      DAC_SHRR    at 16#4C# range 0 .. 31;
      DAC_AUTOCR  at 16#54# range 0 .. 31;
   end record;

   --  Digital-to-analog converter
   DAC1_Periph : aliased DAC_Peripheral
     with Import, Address => DAC1_Base;

   --  Digital-to-analog converter
   SEC_DAC1_Periph : aliased DAC_Peripheral
     with Import, Address => SEC_DAC1_Base;

end STM32_SVD.DAC;
