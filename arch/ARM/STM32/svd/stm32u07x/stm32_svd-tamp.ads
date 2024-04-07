--  This spec has been automatically generated from STM32U073.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.TAMP is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  TAMP control register 1
   type TAMP_CR1_Register is record
      --  Tamper detection on TAMP_IN1 enable
      TAMP1E         : Boolean := False;
      --  Tamper detection on TAMP_IN2 enable<sup>(1)</sup>
      TAMP2E         : Boolean := False;
      --  Tamper detection on TAMP_IN3 enable<sup>(1)</sup>
      TAMP3E         : Boolean := False;
      --  Tamper detection on TAMP_IN4 enable<sup>(1)</sup>
      TAMP4E         : Boolean := False;
      --  Tamper detection on TAMP_IN5 enable<sup>(1)</sup>
      TAMP5E         : Boolean := False;
      --  unspecified
      Reserved_5_17  : HAL.UInt13 := 16#0#;
      --  Internal tamper 3 enable
      ITAMP3E        : Boolean := False;
      --  Internal tamper 4 enable
      ITAMP4E        : Boolean := False;
      --  Internal tamper 5 enable
      ITAMP5E        : Boolean := False;
      --  Internal tamper 6 enable
      ITAMP6E        : Boolean := False;
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TAMP_CR1_Register use record
      TAMP1E         at 0 range 0 .. 0;
      TAMP2E         at 0 range 1 .. 1;
      TAMP3E         at 0 range 2 .. 2;
      TAMP4E         at 0 range 3 .. 3;
      TAMP5E         at 0 range 4 .. 4;
      Reserved_5_17  at 0 range 5 .. 17;
      ITAMP3E        at 0 range 18 .. 18;
      ITAMP4E        at 0 range 19 .. 19;
      ITAMP5E        at 0 range 20 .. 20;
      ITAMP6E        at 0 range 21 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  TAMP control register 2
   type TAMP_CR2_Register is record
      --  Tamper 1 potential mode
      TAMP1POM       : Boolean := False;
      --  Tamper 2 potential mode
      TAMP2POM       : Boolean := False;
      --  Tamper 3 potential mode
      TAMP3POM       : Boolean := False;
      --  Tamper 4 potential mode
      TAMP4POM       : Boolean := False;
      --  Tamper 5 potential mode
      TAMP5POM       : Boolean := False;
      --  unspecified
      Reserved_5_15  : HAL.UInt11 := 16#0#;
      --  Tamper 1 mask The tamper 1 interrupt must not be enabled when
      --  TAMP1MSK is set.
      TAMP1MSK       : Boolean := False;
      --  Tamper 2 mask The tamper 2 interrupt must not be enabled when
      --  TAMP2MSK is set.
      TAMP2MSK       : Boolean := False;
      --  Tamper 3 mask The tamper 3 interrupt must not be enabled when
      --  TAMP3MSK is set.
      TAMP3MSK       : Boolean := False;
      --  unspecified
      Reserved_19_21 : HAL.UInt3 := 16#0#;
      --  Backup registers and device secrets<sup>(1)</sup> access blocked
      BKBLOCK        : Boolean := False;
      --  Write-only. Backup registers and device secrets<sup>(1)</sup> erase
      --  Writing 1 to this bit reset the backup registers and device
      --  secrets<sup>(1)</sup>. Writing 0 has no effect. This bit is always
      --  read as 0.
      BKERASE        : Boolean := False;
      --  Active level for tamper 1 input If TAMPFLT1=100 tamper 1 input rising
      --  edge triggers a tamper detection event. If TAMPFLT1=100 tamper 1
      --  input falling edge triggers a tamper detection event.
      TAMP1TRG       : Boolean := False;
      --  Active level for tamper 2 input If TAMPFLT = 00 tamper 2 input rising
      --  edge triggers a tamper detection event. If TAMPFLT1=100 tamper 2
      --  input falling edge triggers a tamper detection event.
      TAMP2TRG       : Boolean := False;
      --  Active level for tamper 3 input If TAMPFLT1=100 tamper 3 input rising
      --  edge triggers a tamper detection event. If TAMPFLT1=100 tamper 3
      --  input falling edge triggers a tamper detection event.
      TAMP3TRG       : Boolean := False;
      --  Active level for tamper 4 input (active mode disabled) If
      --  TAMPFLT1=100 tamper 4 input rising edge triggers a tamper detection
      --  event. If TAMPFLT1=100 tamper 4 input falling edge triggers a tamper
      --  detection event.
      TAMP4TRG       : Boolean := False;
      --  Active level for tamper 5 input (active mode disabled) If
      --  TAMPFLT1=100 tamper 5 input rising edge triggers a tamper detection
      --  event. If TAMPFLT1=100 tamper 5 input falling edge triggers a tamper
      --  detection event.
      TAMP5TRG       : Boolean := False;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TAMP_CR2_Register use record
      TAMP1POM       at 0 range 0 .. 0;
      TAMP2POM       at 0 range 1 .. 1;
      TAMP3POM       at 0 range 2 .. 2;
      TAMP4POM       at 0 range 3 .. 3;
      TAMP5POM       at 0 range 4 .. 4;
      Reserved_5_15  at 0 range 5 .. 15;
      TAMP1MSK       at 0 range 16 .. 16;
      TAMP2MSK       at 0 range 17 .. 17;
      TAMP3MSK       at 0 range 18 .. 18;
      Reserved_19_21 at 0 range 19 .. 21;
      BKBLOCK        at 0 range 22 .. 22;
      BKERASE        at 0 range 23 .. 23;
      TAMP1TRG       at 0 range 24 .. 24;
      TAMP2TRG       at 0 range 25 .. 25;
      TAMP3TRG       at 0 range 26 .. 26;
      TAMP4TRG       at 0 range 27 .. 27;
      TAMP5TRG       at 0 range 28 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   --  TAMP control register 3
   type TAMP_CR3_Register is record
      --  unspecified
      Reserved_0_1  : HAL.UInt2 := 16#0#;
      --  Internal tamper 3 potential mode
      ITAMP3POM     : Boolean := False;
      --  Internal tamper 4 potential mode
      ITAMP4POM     : Boolean := False;
      --  Internal tamper 5 potential mode
      ITAMP5POM     : Boolean := False;
      --  Internal tamper 6 potential mode
      ITAMP6POM     : Boolean := False;
      --  unspecified
      Reserved_6_31 : HAL.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TAMP_CR3_Register use record
      Reserved_0_1  at 0 range 0 .. 1;
      ITAMP3POM     at 0 range 2 .. 2;
      ITAMP4POM     at 0 range 3 .. 3;
      ITAMP5POM     at 0 range 4 .. 4;
      ITAMP6POM     at 0 range 5 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   subtype TAMP_FLTCR_TAMPFREQ_Field is HAL.UInt3;
   subtype TAMP_FLTCR_TAMPFLT_Field is HAL.UInt2;
   subtype TAMP_FLTCR_TAMPPRCH_Field is HAL.UInt2;

   --  TAMP filter control register
   type TAMP_FLTCR_Register is record
      --  Tamper sampling frequency Determines the frequency at which each of
      --  the TAMP_INx inputs are sampled.
      TAMPFREQ      : TAMP_FLTCR_TAMPFREQ_Field := 16#0#;
      --  TAMP_INx filter count These bits determines the number of consecutive
      --  samples at the specified level (TAMP*TRG) needed to activate a tamper
      --  event. TAMPFLT is valid for each of the TAMP_INx inputs.
      TAMPFLT       : TAMP_FLTCR_TAMPFLT_Field := 16#0#;
      --  TAMP_INx precharge duration These bit determines the duration of time
      --  during which the pull-up/is activated before each sample. TAMPPRCH is
      --  valid for each of the TAMP_INx inputs.
      TAMPPRCH      : TAMP_FLTCR_TAMPPRCH_Field := 16#0#;
      --  TAMP_INx pull-up disable This bit determines if each of the TAMPx
      --  pins are precharged before each sample.
      TAMPPUDIS     : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TAMP_FLTCR_Register use record
      TAMPFREQ      at 0 range 0 .. 2;
      TAMPFLT       at 0 range 3 .. 4;
      TAMPPRCH      at 0 range 5 .. 6;
      TAMPPUDIS     at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  TAMP interrupt enable register
   type TAMP_IER_Register is record
      --  Tamper 1 interrupt enable
      TAMP1IE        : Boolean := False;
      --  Tamper 2 interrupt enable
      TAMP2IE        : Boolean := False;
      --  Tamper 3 interrupt enable
      TAMP3IE        : Boolean := False;
      --  Tamper 4 interrupt enable
      TAMP4IE        : Boolean := False;
      --  Tamper 5 interrupt enable
      TAMP5IE        : Boolean := False;
      --  unspecified
      Reserved_5_17  : HAL.UInt13 := 16#0#;
      --  Internal tamper 3 interrupt enable
      ITAMP3IE       : Boolean := False;
      --  Internal tamper 4 interrupt enable
      ITAMP4IE       : Boolean := False;
      --  Internal tamper 5 interrupt enable
      ITAMP5IE       : Boolean := False;
      --  Internal tamper 6 interrupt enable
      ITAMP6IE       : Boolean := False;
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TAMP_IER_Register use record
      TAMP1IE        at 0 range 0 .. 0;
      TAMP2IE        at 0 range 1 .. 1;
      TAMP3IE        at 0 range 2 .. 2;
      TAMP4IE        at 0 range 3 .. 3;
      TAMP5IE        at 0 range 4 .. 4;
      Reserved_5_17  at 0 range 5 .. 17;
      ITAMP3IE       at 0 range 18 .. 18;
      ITAMP4IE       at 0 range 19 .. 19;
      ITAMP5IE       at 0 range 20 .. 20;
      ITAMP6IE       at 0 range 21 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  TAMP status register
   type TAMP_SR_Register is record
      --  Read-only. TAMP1 detection flag This flag is set by hardware when a
      --  tamper detection event is detected on the TAMP1 input.
      TAMP1F         : Boolean;
      --  Read-only. TAMP2 detection flag This flag is set by hardware when a
      --  tamper detection event is detected on the TAMP2 input.
      TAMP2F         : Boolean;
      --  Read-only. TAMP3 detection flag This flag is set by hardware when a
      --  tamper detection event is detected on the TAMP3 input.
      TAMP3F         : Boolean;
      --  Read-only. TAMP4 detection flag This flag is set by hardware when a
      --  tamper detection event is detected on the TAMP4 input.
      TAMP4F         : Boolean;
      --  Read-only. TAMP5 detection flag This flag is set by hardware when a
      --  tamper detection event is detected on the TAMP5 input.
      TAMP5F         : Boolean;
      --  unspecified
      Reserved_5_17  : HAL.UInt13;
      --  Read-only. Internal tamper 3 flag This flag is set by hardware when a
      --  tamper detection event is detected on the internal tamper 3.
      ITAMP3F        : Boolean;
      --  Read-only. Internal tamper 4 flag This flag is set by hardware when a
      --  tamper detection event is detected on the internal tamper 4.
      ITAMP4F        : Boolean;
      --  Read-only. Internal tamper 5 flag This flag is set by hardware when a
      --  tamper detection event is detected on the internal tamper 5.
      ITAMP5F        : Boolean;
      --  Read-only. Internal tamper 6 flag This flag is set by hardware when a
      --  tamper detection event is detected on the internal tamper 6.
      ITAMP6F        : Boolean;
      --  unspecified
      Reserved_22_31 : HAL.UInt10;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TAMP_SR_Register use record
      TAMP1F         at 0 range 0 .. 0;
      TAMP2F         at 0 range 1 .. 1;
      TAMP3F         at 0 range 2 .. 2;
      TAMP4F         at 0 range 3 .. 3;
      TAMP5F         at 0 range 4 .. 4;
      Reserved_5_17  at 0 range 5 .. 17;
      ITAMP3F        at 0 range 18 .. 18;
      ITAMP4F        at 0 range 19 .. 19;
      ITAMP5F        at 0 range 20 .. 20;
      ITAMP6F        at 0 range 21 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  TAMP masked interrupt status register
   type TAMP_MISR_Register is record
      --  Read-only. TAMP1 interrupt masked flag This flag is set by hardware
      --  when the tamper 1 interrupt is raised.
      TAMP1MF        : Boolean;
      --  Read-only. TAMP2 interrupt masked flag This flag is set by hardware
      --  when the tamper 2 interrupt is raised.
      TAMP2MF        : Boolean;
      --  Read-only. TAMP3 interrupt masked flag This flag is set by hardware
      --  when the tamper 3 interrupt is raised.
      TAMP3MF        : Boolean;
      --  Read-only. TAMP4 interrupt masked flag This flag is set by hardware
      --  when the tamper 4 interrupt is raised.
      TAMP4MF        : Boolean;
      --  Read-only. TAMP5 interrupt masked flag This flag is set by hardware
      --  when the tamper 5 interrupt is raised.
      TAMP5MF        : Boolean;
      --  unspecified
      Reserved_5_17  : HAL.UInt13;
      --  Read-only. Internal tamper 3 interrupt masked flag This flag is set
      --  by hardware when the internal tamper 3 interrupt is raised.
      ITAMP3MF       : Boolean;
      --  Read-only. Internal tamper 4 interrupt masked flag This flag is set
      --  by hardware when the internal tamper 4 interrupt is raised.
      ITAMP4MF       : Boolean;
      --  Read-only. Internal tamper 5 interrupt masked flag This flag is set
      --  by hardware when the internal tamper 5 interrupt is raised.
      ITAMP5MF       : Boolean;
      --  Read-only. Internal tamper 6 interrupt masked flag This flag is set
      --  by hardware when the internal tamper 6 interrupt is raised.
      ITAMP6MF       : Boolean;
      --  unspecified
      Reserved_22_31 : HAL.UInt10;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TAMP_MISR_Register use record
      TAMP1MF        at 0 range 0 .. 0;
      TAMP2MF        at 0 range 1 .. 1;
      TAMP3MF        at 0 range 2 .. 2;
      TAMP4MF        at 0 range 3 .. 3;
      TAMP5MF        at 0 range 4 .. 4;
      Reserved_5_17  at 0 range 5 .. 17;
      ITAMP3MF       at 0 range 18 .. 18;
      ITAMP4MF       at 0 range 19 .. 19;
      ITAMP5MF       at 0 range 20 .. 20;
      ITAMP6MF       at 0 range 21 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  TAMP status clear register
   type TAMP_SCR_Register is record
      --  Write-only. Clear TAMP1 detection flag Writing 1 in this bit clears
      --  the TAMP1F bit in the TAMP_SR register.
      CTAMP1F        : Boolean := False;
      --  Write-only. Clear TAMP2 detection flag Writing 1 in this bit clears
      --  the TAMP2F bit in the TAMP_SR register.
      CTAMP2F        : Boolean := False;
      --  Write-only. Clear TAMP3 detection flag Writing 1 in this bit clears
      --  the TAMP3F bit in the TAMP_SR register.
      CTAMP3F        : Boolean := False;
      --  Write-only. Clear TAMP4 detection flag Writing 1 in this bit clears
      --  the TAMP4F bit in the TAMP_SR register.
      CTAMP4F        : Boolean := False;
      --  Write-only. Clear TAMP5 detection flag Writing 1 in this bit clears
      --  the TAMP5F bit in the TAMP_SR register.
      CTAMP5F        : Boolean := False;
      --  unspecified
      Reserved_5_17  : HAL.UInt13 := 16#0#;
      --  Write-only. Clear ITAMP3 detection flag Writing 1 in this bit clears
      --  the ITAMP3F bit in the TAMP_SR register.
      CITAMP3F       : Boolean := False;
      --  Write-only. Clear ITAMP4 detection flag Writing 1 in this bit clears
      --  the ITAMP4F bit in the TAMP_SR register.
      CITAMP4F       : Boolean := False;
      --  Write-only. Clear ITAMP5 detection flag Writing 1 in this bit clears
      --  the ITAMP5F bit in the TAMP_SR register.
      CITAMP5F       : Boolean := False;
      --  Write-only. Clear ITAMP6 detection flag Writing 1 in this bit clears
      --  the ITAMP6F bit in the TAMP_SR register.
      CITAMP6F       : Boolean := False;
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TAMP_SCR_Register use record
      CTAMP1F        at 0 range 0 .. 0;
      CTAMP2F        at 0 range 1 .. 1;
      CTAMP3F        at 0 range 2 .. 2;
      CTAMP4F        at 0 range 3 .. 3;
      CTAMP5F        at 0 range 4 .. 4;
      Reserved_5_17  at 0 range 5 .. 17;
      CITAMP3F       at 0 range 18 .. 18;
      CITAMP4F       at 0 range 19 .. 19;
      CITAMP5F       at 0 range 20 .. 20;
      CITAMP6F       at 0 range 21 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  TAMP register block
   type TAMP_Peripheral is record
      --  TAMP control register 1
      TAMP_CR1   : aliased TAMP_CR1_Register;
      --  TAMP control register 2
      TAMP_CR2   : aliased TAMP_CR2_Register;
      --  TAMP control register 3
      TAMP_CR3   : aliased TAMP_CR3_Register;
      --  TAMP filter control register
      TAMP_FLTCR : aliased TAMP_FLTCR_Register;
      --  TAMP interrupt enable register
      TAMP_IER   : aliased TAMP_IER_Register;
      --  TAMP status register
      TAMP_SR    : aliased TAMP_SR_Register;
      --  TAMP masked interrupt status register
      TAMP_MISR  : aliased TAMP_MISR_Register;
      --  TAMP status clear register
      TAMP_SCR   : aliased TAMP_SCR_Register;
      --  TAMP backup 0 register
      TAMP_BKP0R : aliased HAL.UInt32;
      --  TAMP backup 1 register
      TAMP_BKP1R : aliased HAL.UInt32;
      --  TAMP backup 2 register
      TAMP_BKP2R : aliased HAL.UInt32;
      --  TAMP backup 3 register
      TAMP_BKP3R : aliased HAL.UInt32;
      --  TAMP backup 4 register
      TAMP_BKP4R : aliased HAL.UInt32;
      --  TAMP backup 5 register
      TAMP_BKP5R : aliased HAL.UInt32;
      --  TAMP backup 6 register
      TAMP_BKP6R : aliased HAL.UInt32;
      --  TAMP backup 7 register
      TAMP_BKP7R : aliased HAL.UInt32;
      --  TAMP backup 8 register
      TAMP_BKP8R : aliased HAL.UInt32;
   end record
     with Volatile;

   for TAMP_Peripheral use record
      TAMP_CR1   at 16#0# range 0 .. 31;
      TAMP_CR2   at 16#4# range 0 .. 31;
      TAMP_CR3   at 16#8# range 0 .. 31;
      TAMP_FLTCR at 16#C# range 0 .. 31;
      TAMP_IER   at 16#2C# range 0 .. 31;
      TAMP_SR    at 16#30# range 0 .. 31;
      TAMP_MISR  at 16#34# range 0 .. 31;
      TAMP_SCR   at 16#3C# range 0 .. 31;
      TAMP_BKP0R at 16#100# range 0 .. 31;
      TAMP_BKP1R at 16#104# range 0 .. 31;
      TAMP_BKP2R at 16#108# range 0 .. 31;
      TAMP_BKP3R at 16#10C# range 0 .. 31;
      TAMP_BKP4R at 16#110# range 0 .. 31;
      TAMP_BKP5R at 16#114# range 0 .. 31;
      TAMP_BKP6R at 16#118# range 0 .. 31;
      TAMP_BKP7R at 16#11C# range 0 .. 31;
      TAMP_BKP8R at 16#120# range 0 .. 31;
   end record;

   --  TAMP register block
   TAMP_Periph : aliased TAMP_Peripheral
     with Import, Address => TAMP_Base;

end STM32_SVD.TAMP;
