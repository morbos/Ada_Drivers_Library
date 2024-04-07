--  This spec has been automatically generated from STM32U073.svd

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

   --  DAC control register
   type DAC_CR_Register is record
      --  DAC channel1 enable This bit is set and cleared by software to
      --  enable/disable DAC channel1.
      EN1            : Boolean := False;
      --  DAC channel1 trigger enable This bit is set and cleared by software
      --  to enable/disable DAC channel1 trigger. Note: When software trigger
      --  is selected, the transfer from the DAC_DHR1 register to the DAC_DOR1
      --  register takes only one dac_pclk clock cycle.
      TEN1           : Boolean := False;
      --  DAC channel1 trigger selection These bits select the external event
      --  used to trigger DAC channel1 ... Refer to the trigger selection
      --  tables in Section114.4.2: DAC pins and internal signals for details
      --  on trigger configuration and mapping. Note: Only used if bit TEN11=11
      --  (DAC channel1 trigger enabled).
      TSEL1          : DAC_CR_TSEL1_Field := 16#0#;
      --  DAC channel1 noise/triangle wave generation enable These bits are set
      --  and cleared by software. 1x: Triangle wave generation enabled Only
      --  used if bit TEN11=11 (DAC channel1 trigger enabled).
      WAVE1          : DAC_CR_WAVE1_Field := 16#0#;
      --  DAC channel1 mask/amplitude selector
      MAMP1          : DAC_CR_MAMP1_Field := 16#0#;
      --  DAC channel1 DMA enable This bit is set and cleared by software.
      DMAEN1         : Boolean := False;
      --  DAC channel1 DMA Underrun Interrupt enable This bit is set and
      --  cleared by software.
      DMAUDRIE1      : Boolean := False;
      --  DAC channel1 calibration enable This bit is set and cleared by
      --  software to enable/disable DAC channel1 calibration, it can be
      --  written only if bit EN11=10 into DAC_CR (the calibration mode can be
      --  entered/exit only when the DAC channel is disabled) Otherwise, the
      --  write operation is ignored.
      CEN1           : Boolean := False;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
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
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   --  DAC software trigger register
   type DAC_SWTRGR_Register is record
      --  Write-only. DAC channel1 software trigger This bit is set by software
      --  to trigger the DAC in software trigger mode. Note: This bit is
      --  cleared by hardware (one dac_pclk clock cycle later) once the
      --  DAC_DHR1 register value has been loaded into the DAC_DOR1 register.
      SWTRIG1       : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DAC_SWTRGR_Register use record
      SWTRIG1       at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   subtype DAC_DHR12R1_DACC1DHR_Field is HAL.UInt12;

   --  DAC channel1 12-bit right-aligned data holding register
   type DAC_DHR12R1_Register is record
      --  DAC channel1 12-bit right-aligned data These bits are written by
      --  software. They specify 12-bit data for DAC channel1.
      DACC1DHR       : DAC_DHR12R1_DACC1DHR_Field := 16#0#;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DAC_DHR12R1_Register use record
      DACC1DHR       at 0 range 0 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype DAC_DHR12L1_DACC1DHR_Field is HAL.UInt12;

   --  DAC channel1 12-bit left aligned data holding register
   type DAC_DHR12L1_Register is record
      --  unspecified
      Reserved_0_3   : HAL.UInt4 := 16#0#;
      --  DAC channel1 12-bit left-aligned data These bits are written by
      --  software. They specify 12-bit data for DAC channel1.
      DACC1DHR       : DAC_DHR12L1_DACC1DHR_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DAC_DHR12L1_Register use record
      Reserved_0_3   at 0 range 0 .. 3;
      DACC1DHR       at 0 range 4 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DAC_DHR8R1_DACC1DHR_Field is HAL.UInt8;

   --  DAC channel1 8-bit right aligned data holding register
   type DAC_DHR8R1_Register is record
      --  DAC channel1 8-bit right-aligned data These bits are written by
      --  software. They specify 8-bit data for DAC channel1.
      DACC1DHR      : DAC_DHR8R1_DACC1DHR_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DAC_DHR8R1_Register use record
      DACC1DHR      at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype DAC_DOR1_DACC1DOR_Field is HAL.UInt12;

   --  DAC channel1 data output register
   type DAC_DOR1_Register is record
      --  Read-only. DAC channel1 data output These bits are read-only, they
      --  contain data output for DAC channel1.
      DACC1DOR       : DAC_DOR1_DACC1DOR_Field;
      --  unspecified
      Reserved_12_31 : HAL.UInt20;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DAC_DOR1_Register use record
      DACC1DOR       at 0 range 0 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   --  DAC status register
   type DAC_SR_Register is record
      --  unspecified
      Reserved_0_12  : HAL.UInt13 := 16#0#;
      --  DAC channel1 DMA underrun flag This bit is set by hardware and
      --  cleared by software (by writing it to 1).
      DMAUDR1        : Boolean := False;
      --  Read-only. DAC channel1 calibration offset status This bit is set and
      --  cleared by hardware
      CAL_FLAG1      : Boolean := False;
      --  Read-only. DAC channel1 busy writing sample time flag This bit is
      --  systematically set just after Sample and hold mode enable and is set
      --  each time the software writes the register DAC_SHSR1, It is cleared
      --  by hardware when the write operation of DAC_SHSR1 is complete. (It
      --  takes about 3 LSI periods of synchronization).
      BWST1          : Boolean := False;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DAC_SR_Register use record
      Reserved_0_12  at 0 range 0 .. 12;
      DMAUDR1        at 0 range 13 .. 13;
      CAL_FLAG1      at 0 range 14 .. 14;
      BWST1          at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DAC_CCR_OTRIM1_Field is HAL.UInt5;

   --  DAC calibration control register
   type DAC_CCR_Register is record
      --  DAC channel1 offset trimming value
      OTRIM1        : DAC_CCR_OTRIM1_Field := 16#0#;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DAC_CCR_Register use record
      OTRIM1        at 0 range 0 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype DAC_MCR_MODE1_Field is HAL.UInt3;

   --  DAC mode control register
   type DAC_MCR_Register is record
      --  DAC channel1 mode These bits can be written only when the DAC is
      --  disabled and not in the calibration mode (when bit EN11=10 and bit
      --  CEN11=10 in the DAC_CR register). If EN11=11 or CEN11=11 the write
      --  operation is ignored. They can be set and cleared by software to
      --  select the DAC channel1 mode: DAC channel1 in Normal mode DAC
      --  channel1 in sample & hold mode Note: This register can be modified
      --  only when EN11=10.
      MODE1         : DAC_MCR_MODE1_Field := 16#0#;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DAC_MCR_Register use record
      MODE1         at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype DAC_SHSR1_TSAMPLE1_Field is HAL.UInt10;

   --  DAC channel1 sample and hold sample time register
   type DAC_SHSR1_Register is record
      --  DAC channel1 sample time (only valid in Sample and hold mode) These
      --  bits can be written when the DAC channel1 is disabled or also during
      --  normal operation. in the latter case, the write can be done only when
      --  BWST1 of DAC_SR register is low, If BWST11=11, the write operation is
      --  ignored.
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

   subtype DAC_SHHR_THOLD1_Field is HAL.UInt10;

   --  DAC sample and hold time register
   type DAC_SHHR_Register is record
      --  DAC channel1 hold time (only valid in Sample and hold mode) Hold
      --  time1=1(THOLD[9:0]) x LSI clock period Note: This register can be
      --  modified only when EN11=10.
      THOLD1         : DAC_SHHR_THOLD1_Field := 16#1#;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#40#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DAC_SHHR_Register use record
      THOLD1         at 0 range 0 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype DAC_SHRR_TREFRESH1_Field is HAL.UInt8;

   --  DAC sample and hold refresh time register
   type DAC_SHRR_Register is record
      --  DAC channel1 refresh time (only valid in Sample and hold mode)
      --  Refresh time1=1(TREFRESH[7:0]) x LSI clock period Note: This register
      --  can be modified only when EN11=10.
      TREFRESH1     : DAC_SHRR_TREFRESH1_Field := 16#1#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#100#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DAC_SHRR_Register use record
      TREFRESH1     at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  DAC address block description
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
      --  DAC channel1 data output register
      DAC_DOR1    : aliased DAC_DOR1_Register;
      --  DAC status register
      DAC_SR      : aliased DAC_SR_Register;
      --  DAC calibration control register
      DAC_CCR     : aliased DAC_CCR_Register;
      --  DAC mode control register
      DAC_MCR     : aliased DAC_MCR_Register;
      --  DAC channel1 sample and hold sample time register
      DAC_SHSR1   : aliased DAC_SHSR1_Register;
      --  DAC sample and hold time register
      DAC_SHHR    : aliased DAC_SHHR_Register;
      --  DAC sample and hold refresh time register
      DAC_SHRR    : aliased DAC_SHRR_Register;
   end record
     with Volatile;

   for DAC_Peripheral use record
      DAC_CR      at 16#0# range 0 .. 31;
      DAC_SWTRGR  at 16#4# range 0 .. 31;
      DAC_DHR12R1 at 16#8# range 0 .. 31;
      DAC_DHR12L1 at 16#C# range 0 .. 31;
      DAC_DHR8R1  at 16#10# range 0 .. 31;
      DAC_DOR1    at 16#2C# range 0 .. 31;
      DAC_SR      at 16#34# range 0 .. 31;
      DAC_CCR     at 16#38# range 0 .. 31;
      DAC_MCR     at 16#3C# range 0 .. 31;
      DAC_SHSR1   at 16#40# range 0 .. 31;
      DAC_SHHR    at 16#48# range 0 .. 31;
      DAC_SHRR    at 16#4C# range 0 .. 31;
   end record;

   --  DAC address block description
   DAC_Periph : aliased DAC_Peripheral
     with Import, Address => DAC_Base;

end STM32_SVD.DAC;
