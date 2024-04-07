--  This spec has been automatically generated from STM32U5xx.svd

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

   subtype COMP1_CSR_COMP1_INMSEL_Field is HAL.UInt4;
   subtype COMP1_CSR_COMP1_INPSEL_Field is HAL.UInt2;
   subtype COMP1_CSR_COMP1_HYST_Field is HAL.UInt2;
   subtype COMP1_CSR_COMP1_PWRMODE_Field is HAL.UInt2;
   subtype COMP1_CSR_COMP1_BLANKSEL_Field is HAL.UInt5;

   --  Comparator 1 control and status register
   type COMP1_CSR_Register is record
      --  Comparator 1 enable bit
      COMP1_EN       : Boolean := False;
      --  unspecified
      Reserved_1_3   : HAL.UInt3 := 16#0#;
      --  Comparator 1 Input Minus connection configuration bit
      COMP1_INMSEL   : COMP1_CSR_COMP1_INMSEL_Field := 16#0#;
      --  Comparator1 input plus selection bit
      COMP1_INPSEL   : COMP1_CSR_COMP1_INPSEL_Field := 16#0#;
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  COMP1_WINMODE
      COMP1_WINMODE  : Boolean := False;
      --  unspecified
      Reserved_12_13 : HAL.UInt2 := 16#0#;
      --  COMP1_WINOUT
      COMP1_WINOUT   : Boolean := False;
      --  Comparator 1 polarity selection bit
      COMP1_POLARITY : Boolean := False;
      --  Comparator 1 hysteresis selection bits
      COMP1_HYST     : COMP1_CSR_COMP1_HYST_Field := 16#0#;
      --  COMP1_PWRMODE
      COMP1_PWRMODE  : COMP1_CSR_COMP1_PWRMODE_Field := 16#0#;
      --  COMP1_BLANKSEL
      COMP1_BLANKSEL : COMP1_CSR_COMP1_BLANKSEL_Field := 16#0#;
      --  unspecified
      Reserved_25_29 : HAL.UInt5 := 16#0#;
      --  Read-only. Comparator 1 output status bit
      COMP1_VALUE    : Boolean := False;
      --  COMP1_CSR register lock bit
      COMP1_LOCK     : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for COMP1_CSR_Register use record
      COMP1_EN       at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      COMP1_INMSEL   at 0 range 4 .. 7;
      COMP1_INPSEL   at 0 range 8 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      COMP1_WINMODE  at 0 range 11 .. 11;
      Reserved_12_13 at 0 range 12 .. 13;
      COMP1_WINOUT   at 0 range 14 .. 14;
      COMP1_POLARITY at 0 range 15 .. 15;
      COMP1_HYST     at 0 range 16 .. 17;
      COMP1_PWRMODE  at 0 range 18 .. 19;
      COMP1_BLANKSEL at 0 range 20 .. 24;
      Reserved_25_29 at 0 range 25 .. 29;
      COMP1_VALUE    at 0 range 30 .. 30;
      COMP1_LOCK     at 0 range 31 .. 31;
   end record;

   subtype COMP2_CSR_COM2_INMSEL_Field is HAL.UInt4;
   subtype COMP2_CSR_COM2_INPSEL_Field is HAL.UInt2;
   subtype COMP2_CSR_COM2_HYST_Field is HAL.UInt2;
   subtype COMP2_CSR_COM2_PWRMODE_Field is HAL.UInt2;
   subtype COMP2_CSR_COM2_BLANKSEL_Field is HAL.UInt5;

   --  Comparator 2 control and status register
   type COMP2_CSR_Register is record
      --  Comparator 2 enable bit
      COM2_EN        : Boolean := False;
      --  unspecified
      Reserved_1_3   : HAL.UInt3 := 16#0#;
      --  Comparator 2 Input Minus connection configuration bit
      COM2_INMSEL    : COMP2_CSR_COM2_INMSEL_Field := 16#0#;
      --  Comparator 2 input plus selection bit
      COM2_INPSEL    : COMP2_CSR_COM2_INPSEL_Field := 16#0#;
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  COM2_WINMODE
      COM2_WINMODE   : Boolean := False;
      --  unspecified
      Reserved_12_13 : HAL.UInt2 := 16#0#;
      --  COM2_WINOUT
      COM2_WINOUT    : Boolean := False;
      --  Comparator 2 polarity selection bit
      COM2_POLARITY  : Boolean := False;
      --  Comparator 2 hysteresis selection bits
      COM2_HYST      : COMP2_CSR_COM2_HYST_Field := 16#0#;
      --  COM2_PWRMODE
      COM2_PWRMODE   : COMP2_CSR_COM2_PWRMODE_Field := 16#0#;
      --  COM2_BLANKSEL
      COM2_BLANKSEL  : COMP2_CSR_COM2_BLANKSEL_Field := 16#0#;
      --  unspecified
      Reserved_25_29 : HAL.UInt5 := 16#0#;
      --  Read-only. Comparator 2 output status bit
      COM2_VALUE     : Boolean := False;
      --  COMP2_CSR register lock bit
      COM2_LOCK      : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for COMP2_CSR_Register use record
      COM2_EN        at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      COM2_INMSEL    at 0 range 4 .. 7;
      COM2_INPSEL    at 0 range 8 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      COM2_WINMODE   at 0 range 11 .. 11;
      Reserved_12_13 at 0 range 12 .. 13;
      COM2_WINOUT    at 0 range 14 .. 14;
      COM2_POLARITY  at 0 range 15 .. 15;
      COM2_HYST      at 0 range 16 .. 17;
      COM2_PWRMODE   at 0 range 18 .. 19;
      COM2_BLANKSEL  at 0 range 20 .. 24;
      Reserved_25_29 at 0 range 25 .. 29;
      COM2_VALUE     at 0 range 30 .. 30;
      COM2_LOCK      at 0 range 31 .. 31;
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
     with Import, Address => COMP_Base;

   --  Comparator
   SEC_COMP_Periph : aliased COMP_Peripheral
     with Import, Address => SEC_COMP_Base;

end STM32_SVD.COMP;
