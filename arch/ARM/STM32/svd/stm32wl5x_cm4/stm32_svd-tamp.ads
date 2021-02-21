--  This spec has been automatically generated from STM32WL5x_CM4.svd

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

   --  control register 1
   type CR1_Register is record
      --  TAMP1E
      TAMP1E         : Boolean := False;
      --  TAMP2E
      TAMP2E         : Boolean := False;
      --  TAMP2E
      TAMP3E         : Boolean := False;
      --  unspecified
      Reserved_3_17  : HAL.UInt15 := 16#6000#;
      --  ITAMP3E
      ITAMP3E        : Boolean := True;
      --  unspecified
      Reserved_19_19 : HAL.Bit := 16#1#;
      --  ITAMP5E
      ITAMP5E        : Boolean := True;
      --  ITAMP6E
      ITAMP6E        : Boolean := True;
      --  unspecified
      Reserved_22_22 : HAL.Bit := 16#1#;
      --  ITAMP8E
      ITAMP8E        : Boolean := True;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#FF#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR1_Register use record
      TAMP1E         at 0 range 0 .. 0;
      TAMP2E         at 0 range 1 .. 1;
      TAMP3E         at 0 range 2 .. 2;
      Reserved_3_17  at 0 range 3 .. 17;
      ITAMP3E        at 0 range 18 .. 18;
      Reserved_19_19 at 0 range 19 .. 19;
      ITAMP5E        at 0 range 20 .. 20;
      ITAMP6E        at 0 range 21 .. 21;
      Reserved_22_22 at 0 range 22 .. 22;
      ITAMP8E        at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  control register 2
   type CR2_Register is record
      --  TAMP1NOER
      TAMP1NOER      : Boolean := False;
      --  TAMP2NOER
      TAMP2NOER      : Boolean := False;
      --  TAMP3NOER
      TAMP3NOER      : Boolean := False;
      --  unspecified
      Reserved_3_15  : HAL.UInt13 := 16#0#;
      --  TAMP1MSK
      TAMP1MSK       : Boolean := False;
      --  TAMP2MSK
      TAMP2MSK       : Boolean := False;
      --  TAMP3MSK
      TAMP3MSK       : Boolean := False;
      --  unspecified
      Reserved_19_22 : HAL.UInt4 := 16#0#;
      --  Backup registerserase
      BKERASE        : Boolean := False;
      --  TAMP1TRG
      TAMP1TRG       : Boolean := False;
      --  TAMP2TRG
      TAMP2TRG       : Boolean := False;
      --  TAMP3TRG
      TAMP3TRG       : Boolean := False;
      --  unspecified
      Reserved_27_31 : HAL.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR2_Register use record
      TAMP1NOER      at 0 range 0 .. 0;
      TAMP2NOER      at 0 range 1 .. 1;
      TAMP3NOER      at 0 range 2 .. 2;
      Reserved_3_15  at 0 range 3 .. 15;
      TAMP1MSK       at 0 range 16 .. 16;
      TAMP2MSK       at 0 range 17 .. 17;
      TAMP3MSK       at 0 range 18 .. 18;
      Reserved_19_22 at 0 range 19 .. 22;
      BKERASE        at 0 range 23 .. 23;
      TAMP1TRG       at 0 range 24 .. 24;
      TAMP2TRG       at 0 range 25 .. 25;
      TAMP3TRG       at 0 range 26 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   --  TAMP control register 3
   type CR3_Register is record
      --  unspecified
      Reserved_0_1  : HAL.UInt2 := 16#0#;
      --  ITAMP3NOER
      ITAMP3NOER    : Boolean := False;
      --  unspecified
      Reserved_3_3  : HAL.Bit := 16#0#;
      --  ITAMP5NOER
      ITAMP5NOER    : Boolean := False;
      --  ITAMP6NOER
      ITAMP6NOER    : Boolean := False;
      --  unspecified
      Reserved_6_6  : HAL.Bit := 16#0#;
      --  ITAMP8NOER
      ITAMP8NOER    : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR3_Register use record
      Reserved_0_1  at 0 range 0 .. 1;
      ITAMP3NOER    at 0 range 2 .. 2;
      Reserved_3_3  at 0 range 3 .. 3;
      ITAMP5NOER    at 0 range 4 .. 4;
      ITAMP6NOER    at 0 range 5 .. 5;
      Reserved_6_6  at 0 range 6 .. 6;
      ITAMP8NOER    at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype FLTCR_TAMPFREQ_Field is HAL.UInt3;
   subtype FLTCR_TAMPFLT_Field is HAL.UInt2;
   subtype FLTCR_TAMPPRCH_Field is HAL.UInt2;

   --  TAMP filter control register
   type FLTCR_Register is record
      --  TAMPFREQ
      TAMPFREQ      : FLTCR_TAMPFREQ_Field := 16#0#;
      --  TAMPFLT
      TAMPFLT       : FLTCR_TAMPFLT_Field := 16#0#;
      --  TAMPPRCH
      TAMPPRCH      : FLTCR_TAMPPRCH_Field := 16#0#;
      --  TAMPPUDIS
      TAMPPUDIS     : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FLTCR_Register use record
      TAMPFREQ      at 0 range 0 .. 2;
      TAMPFLT       at 0 range 3 .. 4;
      TAMPPRCH      at 0 range 5 .. 6;
      TAMPPUDIS     at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  TAMP interrupt enable register
   type IER_Register is record
      --  TAMP1IE
      TAMP1IE        : Boolean := False;
      --  TAMP2IE
      TAMP2IE        : Boolean := False;
      --  TAMP3IE
      TAMP3IE        : Boolean := False;
      --  unspecified
      Reserved_3_17  : HAL.UInt15 := 16#0#;
      --  ITAMP3IE
      ITAMP3IE       : Boolean := False;
      --  unspecified
      Reserved_19_19 : HAL.Bit := 16#0#;
      --  ITAMP5IE
      ITAMP5IE       : Boolean := False;
      --  ITAMP6IE
      ITAMP6IE       : Boolean := False;
      --  unspecified
      Reserved_22_22 : HAL.Bit := 16#0#;
      --  ITAMP8IE
      ITAMP8IE       : Boolean := False;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IER_Register use record
      TAMP1IE        at 0 range 0 .. 0;
      TAMP2IE        at 0 range 1 .. 1;
      TAMP3IE        at 0 range 2 .. 2;
      Reserved_3_17  at 0 range 3 .. 17;
      ITAMP3IE       at 0 range 18 .. 18;
      Reserved_19_19 at 0 range 19 .. 19;
      ITAMP5IE       at 0 range 20 .. 20;
      ITAMP6IE       at 0 range 21 .. 21;
      Reserved_22_22 at 0 range 22 .. 22;
      ITAMP8IE       at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  TAMP status register
   type SR_Register is record
      --  Read-only. TAMP1F
      TAMP1F         : Boolean;
      --  Read-only. TAMP2F
      TAMP2F         : Boolean;
      --  Read-only. TAMP3F
      TAMP3F         : Boolean;
      --  unspecified
      Reserved_3_17  : HAL.UInt15;
      --  Read-only. ITAMP3F
      ITAMP3F        : Boolean;
      --  unspecified
      Reserved_19_19 : HAL.Bit;
      --  Read-only. ITAMP5F
      ITAMP5F        : Boolean;
      --  Read-only. ITAMP6F
      ITAMP6F        : Boolean;
      --  unspecified
      Reserved_22_22 : HAL.Bit;
      --  Read-only. ITAMP8F
      ITAMP8F        : Boolean;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register use record
      TAMP1F         at 0 range 0 .. 0;
      TAMP2F         at 0 range 1 .. 1;
      TAMP3F         at 0 range 2 .. 2;
      Reserved_3_17  at 0 range 3 .. 17;
      ITAMP3F        at 0 range 18 .. 18;
      Reserved_19_19 at 0 range 19 .. 19;
      ITAMP5F        at 0 range 20 .. 20;
      ITAMP6F        at 0 range 21 .. 21;
      Reserved_22_22 at 0 range 22 .. 22;
      ITAMP8F        at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  TAMP masked interrupt status register
   type MISR_Register is record
      --  Read-only. TAMP1MF:
      TAMP1MF        : Boolean;
      --  Read-only. TAMP2MF
      TAMP2MF        : Boolean;
      --  Read-only. TAMP3MF
      TAMP3MF        : Boolean;
      --  unspecified
      Reserved_3_17  : HAL.UInt15;
      --  Read-only. ITAMP3MF
      ITAMP3MF       : Boolean;
      --  unspecified
      Reserved_19_19 : HAL.Bit;
      --  Read-only. ITAMP5MF
      ITAMP5MF       : Boolean;
      --  Read-only. ITAMP6MF
      ITAMP6MF       : Boolean;
      --  unspecified
      Reserved_22_22 : HAL.Bit;
      --  Read-only. ITAMP8MF
      ITAMP8MF       : Boolean;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MISR_Register use record
      TAMP1MF        at 0 range 0 .. 0;
      TAMP2MF        at 0 range 1 .. 1;
      TAMP3MF        at 0 range 2 .. 2;
      Reserved_3_17  at 0 range 3 .. 17;
      ITAMP3MF       at 0 range 18 .. 18;
      Reserved_19_19 at 0 range 19 .. 19;
      ITAMP5MF       at 0 range 20 .. 20;
      ITAMP6MF       at 0 range 21 .. 21;
      Reserved_22_22 at 0 range 22 .. 22;
      ITAMP8MF       at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  TAMP status clear register
   type SCR_Register is record
      --  Write-only. CTAMP1F
      CTAMP1F        : Boolean := False;
      --  Write-only. CTAMP2F
      CTAMP2F        : Boolean := False;
      --  Write-only. CTAMP3F
      CTAMP3F        : Boolean := False;
      --  unspecified
      Reserved_3_17  : HAL.UInt15 := 16#0#;
      --  Write-only. CITAMP3F
      CITAMP3F       : Boolean := False;
      --  unspecified
      Reserved_19_19 : HAL.Bit := 16#0#;
      --  Write-only. CITAMP5F
      CITAMP5F       : Boolean := False;
      --  Write-only. CITAMP6F
      CITAMP6F       : Boolean := False;
      --  unspecified
      Reserved_22_22 : HAL.Bit := 16#0#;
      --  Write-only. CITAMP8F
      CITAMP8F       : Boolean := False;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SCR_Register use record
      CTAMP1F        at 0 range 0 .. 0;
      CTAMP2F        at 0 range 1 .. 1;
      CTAMP3F        at 0 range 2 .. 2;
      Reserved_3_17  at 0 range 3 .. 17;
      CITAMP3F       at 0 range 18 .. 18;
      Reserved_19_19 at 0 range 19 .. 19;
      CITAMP5F       at 0 range 20 .. 20;
      CITAMP6F       at 0 range 21 .. 21;
      Reserved_22_22 at 0 range 22 .. 22;
      CITAMP8F       at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Tamper and backup registers
   type TAMP_Peripheral is record
      --  control register 1
      CR1    : aliased CR1_Register;
      --  control register 2
      CR2    : aliased CR2_Register;
      --  TAMP control register 3
      CR3    : aliased CR3_Register;
      --  TAMP filter control register
      FLTCR  : aliased FLTCR_Register;
      --  TAMP interrupt enable register
      IER    : aliased IER_Register;
      --  TAMP status register
      SR     : aliased SR_Register;
      --  TAMP masked interrupt status register
      MISR   : aliased MISR_Register;
      --  TAMP status clear register
      SCR    : aliased SCR_Register;
      --  monotonic counter register
      COUNTR : aliased HAL.UInt32;
      --  TAMP backup register
      BKP0R  : aliased HAL.UInt32;
      --  TAMP backup register
      BKP1R  : aliased HAL.UInt32;
      --  TAMP backup register
      BKP2R  : aliased HAL.UInt32;
      --  TAMP backup register
      BKP3R  : aliased HAL.UInt32;
      --  TAMP backup register
      BKP4R  : aliased HAL.UInt32;
      --  TAMP backup register
      BKP5R  : aliased HAL.UInt32;
      --  TAMP backup register
      BKP6R  : aliased HAL.UInt32;
      --  TAMP backup register
      BKP7R  : aliased HAL.UInt32;
      --  TAMP backup register
      BKP8R  : aliased HAL.UInt32;
      --  TAMP backup register
      BKP9R  : aliased HAL.UInt32;
      --  TAMP backup register
      BKP10R : aliased HAL.UInt32;
      --  TAMP backup register
      BKP11R : aliased HAL.UInt32;
      --  TAMP backup register
      BKP12R : aliased HAL.UInt32;
      --  TAMP backup register
      BKP13R : aliased HAL.UInt32;
      --  TAMP backup register
      BKP14R : aliased HAL.UInt32;
      --  TAMP backup register
      BKP15R : aliased HAL.UInt32;
      --  TAMP backup register
      BKP16R : aliased HAL.UInt32;
      --  TAMP backup register
      BKP17R : aliased HAL.UInt32;
      --  TAMP backup register
      BKP18R : aliased HAL.UInt32;
      --  TAMP backup register
      BKP19R : aliased HAL.UInt32;
   end record
     with Volatile;

   for TAMP_Peripheral use record
      CR1    at 16#0# range 0 .. 31;
      CR2    at 16#4# range 0 .. 31;
      CR3    at 16#8# range 0 .. 31;
      FLTCR  at 16#C# range 0 .. 31;
      IER    at 16#2C# range 0 .. 31;
      SR     at 16#30# range 0 .. 31;
      MISR   at 16#34# range 0 .. 31;
      SCR    at 16#3C# range 0 .. 31;
      COUNTR at 16#40# range 0 .. 31;
      BKP0R  at 16#100# range 0 .. 31;
      BKP1R  at 16#104# range 0 .. 31;
      BKP2R  at 16#108# range 0 .. 31;
      BKP3R  at 16#10C# range 0 .. 31;
      BKP4R  at 16#110# range 0 .. 31;
      BKP5R  at 16#114# range 0 .. 31;
      BKP6R  at 16#118# range 0 .. 31;
      BKP7R  at 16#11C# range 0 .. 31;
      BKP8R  at 16#120# range 0 .. 31;
      BKP9R  at 16#124# range 0 .. 31;
      BKP10R at 16#140# range 0 .. 31;
      BKP11R at 16#144# range 0 .. 31;
      BKP12R at 16#148# range 0 .. 31;
      BKP13R at 16#14C# range 0 .. 31;
      BKP14R at 16#150# range 0 .. 31;
      BKP15R at 16#154# range 0 .. 31;
      BKP16R at 16#158# range 0 .. 31;
      BKP17R at 16#15C# range 0 .. 31;
      BKP18R at 16#160# range 0 .. 31;
      BKP19R at 16#164# range 0 .. 31;
   end record;

   --  Tamper and backup registers
   TAMP_Periph : aliased TAMP_Peripheral
     with Import, Address => TAMP_Base;

end STM32_SVD.TAMP;
