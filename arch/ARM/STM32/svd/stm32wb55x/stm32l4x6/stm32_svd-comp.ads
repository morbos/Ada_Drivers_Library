--  This spec has been automatically generated from STM32L4x6.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.COMP is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype COMP1_CSR_COMP1_PWRMODE_Field is HAL.UInt2;
   subtype COMP1_CSR_COMP1_INMSEL_Field is HAL.UInt3;
   subtype COMP1_CSR_COMP1_HYST_Field is HAL.UInt2;
   subtype COMP1_CSR_COMP1_BLANKING_Field is HAL.UInt3;

   --  Comparator 1 control and status register
   type COMP1_CSR_Register is record
      --  Comparator 1 enable bit
      COMP1_EN       : Boolean := False;
      --  unspecified
      Reserved_1_1   : HAL.Bit := 16#0#;
      --  Power Mode of the comparator 1
      COMP1_PWRMODE  : COMP1_CSR_COMP1_PWRMODE_Field := 16#0#;
      --  Comparator 1 Input Minus connection configuration bit
      COMP1_INMSEL   : COMP1_CSR_COMP1_INMSEL_Field := 16#0#;
      --  Comparator1 input plus selection bit
      COMP1_INPSEL   : Boolean := False;
      --  unspecified
      Reserved_8_14  : HAL.UInt7 := 16#0#;
      --  Comparator 1 polarity selection bit
      COMP1_POLARITY : Boolean := False;
      --  Comparator 1 hysteresis selection bits
      COMP1_HYST     : COMP1_CSR_COMP1_HYST_Field := 16#0#;
      --  Comparator 1 blanking source selection bits
      COMP1_BLANKING : COMP1_CSR_COMP1_BLANKING_Field := 16#0#;
      --  unspecified
      Reserved_21_21 : HAL.Bit := 16#0#;
      --  Scaler bridge enable
      COMP1_BRGEN    : Boolean := False;
      --  Voltage scaler enable bit
      COMP1_SCALEN   : Boolean := False;
      --  unspecified
      Reserved_24_29 : HAL.UInt6 := 16#0#;
      --  Read-only. Comparator 1 output status bit
      COMP1_VALUE    : Boolean := False;
      --  Write-only. COMP1_CSR register lock bit
      COMP1_LOCK     : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for COMP1_CSR_Register use record
      COMP1_EN       at 0 range 0 .. 0;
      Reserved_1_1   at 0 range 1 .. 1;
      COMP1_PWRMODE  at 0 range 2 .. 3;
      COMP1_INMSEL   at 0 range 4 .. 6;
      COMP1_INPSEL   at 0 range 7 .. 7;
      Reserved_8_14  at 0 range 8 .. 14;
      COMP1_POLARITY at 0 range 15 .. 15;
      COMP1_HYST     at 0 range 16 .. 17;
      COMP1_BLANKING at 0 range 18 .. 20;
      Reserved_21_21 at 0 range 21 .. 21;
      COMP1_BRGEN    at 0 range 22 .. 22;
      COMP1_SCALEN   at 0 range 23 .. 23;
      Reserved_24_29 at 0 range 24 .. 29;
      COMP1_VALUE    at 0 range 30 .. 30;
      COMP1_LOCK     at 0 range 31 .. 31;
   end record;

   subtype COMP2_CSR_COMP2_PWRMODE_Field is HAL.UInt2;
   subtype COMP2_CSR_COMP2_INMSEL_Field is HAL.UInt3;
   subtype COMP2_CSR_COMP2_HYST_Field is HAL.UInt2;
   subtype COMP2_CSR_COMP2_BLANKING_Field is HAL.UInt3;

   --  Comparator 2 control and status register
   type COMP2_CSR_Register is record
      --  Comparator 2 enable bit
      COMP2_EN       : Boolean := False;
      --  unspecified
      Reserved_1_1   : HAL.Bit := 16#0#;
      --  Power Mode of the comparator 2
      COMP2_PWRMODE  : COMP2_CSR_COMP2_PWRMODE_Field := 16#0#;
      --  Comparator 2 Input Minus connection configuration bit
      COMP2_INMSEL   : COMP2_CSR_COMP2_INMSEL_Field := 16#0#;
      --  Comparator 2 Input Plus connection configuration bit
      COMP2_INPSEL   : Boolean := False;
      --  unspecified
      Reserved_8_8   : HAL.Bit := 16#0#;
      --  Windows mode selection bit
      COMP2_WINMODE  : Boolean := False;
      --  unspecified
      Reserved_10_14 : HAL.UInt5 := 16#0#;
      --  Comparator 2 polarity selection bit
      COMP2_POLARITY : Boolean := False;
      --  Comparator 2 hysteresis selection bits
      COMP2_HYST     : COMP2_CSR_COMP2_HYST_Field := 16#0#;
      --  Comparator 2 blanking source selection bits
      COMP2_BLANKING : COMP2_CSR_COMP2_BLANKING_Field := 16#0#;
      --  unspecified
      Reserved_21_21 : HAL.Bit := 16#0#;
      --  Scaler bridge enable
      COMP2_BRGEN    : Boolean := False;
      --  Voltage scaler enable bit
      COMP2_SCALEN   : Boolean := False;
      --  unspecified
      Reserved_24_29 : HAL.UInt6 := 16#0#;
      --  Read-only. Comparator 2 output status bit
      COMP2_VALUE    : Boolean := False;
      --  Write-only. COMP2_CSR register lock bit
      COMP2_LOCK     : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for COMP2_CSR_Register use record
      COMP2_EN       at 0 range 0 .. 0;
      Reserved_1_1   at 0 range 1 .. 1;
      COMP2_PWRMODE  at 0 range 2 .. 3;
      COMP2_INMSEL   at 0 range 4 .. 6;
      COMP2_INPSEL   at 0 range 7 .. 7;
      Reserved_8_8   at 0 range 8 .. 8;
      COMP2_WINMODE  at 0 range 9 .. 9;
      Reserved_10_14 at 0 range 10 .. 14;
      COMP2_POLARITY at 0 range 15 .. 15;
      COMP2_HYST     at 0 range 16 .. 17;
      COMP2_BLANKING at 0 range 18 .. 20;
      Reserved_21_21 at 0 range 21 .. 21;
      COMP2_BRGEN    at 0 range 22 .. 22;
      COMP2_SCALEN   at 0 range 23 .. 23;
      Reserved_24_29 at 0 range 24 .. 29;
      COMP2_VALUE    at 0 range 30 .. 30;
      COMP2_LOCK     at 0 range 31 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Comparator
   type COMP_Peripheral is record
      --  Comparator 1 control and status register
      COMP1_CSR : aliased COMP1_CSR_Register;
      --  Comparator 2 control and status register
      COMP2_CSR : aliased COMP2_CSR_Register;
   end record
     with Volatile;

   for COMP_Peripheral use record
      COMP1_CSR at 16#0# range 0 .. 31;
      COMP2_CSR at 16#4# range 0 .. 31;
   end record;

   --  Comparator
   COMP_Periph : aliased COMP_Peripheral
     with Import, Address => System'To_Address (16#40010200#);

end STM32_SVD.COMP;
