--  This spec has been automatically generated from STM32U073.svd

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

   subtype COMP1_CSR_INMSEL_Field is HAL.UInt4;
   subtype COMP1_CSR_INPSEL_Field is HAL.UInt3;
   subtype COMP1_CSR_HYST_Field is HAL.UInt2;
   subtype COMP1_CSR_PWRMODE_Field is HAL.UInt2;
   subtype COMP1_CSR_BLANKSEL_Field is HAL.UInt5;

   --  Comparator 1 control and status register
   type COMP1_CSR_Register is record
      --  Comparator 1 enable bit This bit is controlled by software (if not
      --  locked). It enables the comparator 1:
      EN             : Boolean := False;
      --  unspecified
      Reserved_1_3   : HAL.UInt3 := 16#0#;
      --  Comparator 1 signal selector for inverting input INM This bitfield is
      --  controlled by software (if not locked). It selects the signal for the
      --  inverting input COMP_INM of the comparator 1: Refer to Table176:
      --  COMP1 inverting input assignment.
      INMSEL         : COMP1_CSR_INMSEL_Field := 16#0#;
      --  Comparator 1 signal selector for noninverting input This bitfield is
      --  controlled by software (if not locked). It selects the signal for the
      --  noninverting input COMP_INP of the comparator 1 (also see the WINMODE
      --  bit): Refer to Table175: COMP1 noninverting input assignment.
      INPSEL         : COMP1_CSR_INPSEL_Field := 16#0#;
      --  Comparator 1 noninverting input selector for window mode This bit is
      --  controlled by software (if not locked). It selects the signal for
      --  COMP_INP input of the comparator 1:
      WINMODE        : Boolean := False;
      --  unspecified
      Reserved_12_13 : HAL.UInt2 := 16#0#;
      --  Comparator 1 output selector This bit is controlled by software (if
      --  not locked). It selects the comparator 1 output:
      WINOUT         : Boolean := False;
      --  Comparator 1 polarity selector This bit is controlled by software (if
      --  not locked). It selects the comparator 1 output polarity:
      POLARITY       : Boolean := False;
      --  Comparator 1 hysteresis selector This bitfield is controlled by
      --  software (if not locked). It selects the hysteresis of the comparator
      --  1:
      HYST           : COMP1_CSR_HYST_Field := 16#0#;
      --  Comparator 1 power mode selector This bitfield is controlled by
      --  software (if not locked). It selects the power consumption and as a
      --  consequence the speed of the comparator 1:
      PWRMODE        : COMP1_CSR_PWRMODE_Field := 16#0#;
      --  Comparator 1 blanking source selector This bitfield is controlled by
      --  software (if not locked). It selects the blanking source: Others:
      --  Reserved, must not be used
      BLANKSEL       : COMP1_CSR_BLANKSEL_Field := 16#0#;
      --  unspecified
      Reserved_25_29 : HAL.UInt5 := 16#0#;
      --  Read-only. Comparator 1 output status This bit is read-only. It
      --  reflects the level of the comparator 1 output after the polarity
      --  selector and blanking, as indicated in Figure163.
      VALUE          : Boolean := False;
      --  COMP_CSR register lock This bit is set by software and cleared by a
      --  system reset. It locks the comparator 3 control bits. When locked,
      --  all register bits are read-only.
      LOCK           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for COMP1_CSR_Register use record
      EN             at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      INMSEL         at 0 range 4 .. 7;
      INPSEL         at 0 range 8 .. 10;
      WINMODE        at 0 range 11 .. 11;
      Reserved_12_13 at 0 range 12 .. 13;
      WINOUT         at 0 range 14 .. 14;
      POLARITY       at 0 range 15 .. 15;
      HYST           at 0 range 16 .. 17;
      PWRMODE        at 0 range 18 .. 19;
      BLANKSEL       at 0 range 20 .. 24;
      Reserved_25_29 at 0 range 25 .. 29;
      VALUE          at 0 range 30 .. 30;
      LOCK           at 0 range 31 .. 31;
   end record;

   subtype COMP2_CSR_INMSEL_Field is HAL.UInt4;
   subtype COMP2_CSR_INPSEL_Field is HAL.UInt2;
   subtype COMP2_CSR_HYST_Field is HAL.UInt2;
   subtype COMP2_CSR_PWRMODE_Field is HAL.UInt2;
   subtype COMP2_CSR_BLANKSEL_Field is HAL.UInt5;

   --  Comparator 2 control and status register
   type COMP2_CSR_Register is record
      --  Comparator 2 enable bit This bit is controlled by software (if not
      --  locked). It enables the comparator 2:
      EN             : Boolean := False;
      --  unspecified
      Reserved_1_3   : HAL.UInt3 := 16#0#;
      --  Comparator 2 signal selector for inverting input INM This bitfield is
      --  controlled by software (if not locked). It selects the signal for the
      --  inverting input COMP_INM of the comparator 2: Refer to Table178:
      --  COMP2 inverting input assignment.
      INMSEL         : COMP2_CSR_INMSEL_Field := 16#0#;
      --  Comparator 2 signal selector for noninverting input This bitfield is
      --  controlled by software (if not locked). It selects the signal for the
      --  noninverting input COMP_INP of the comparator 2 (also see the WINMODE
      --  bit): Refer to Table177: COMP2 noninverting input assignment.
      INPSEL         : COMP2_CSR_INPSEL_Field := 16#0#;
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  Comparator 2 noninverting input selector for window mode This bit is
      --  controlled by software (if not locked). It selects the signal for
      --  COMP_INP input of the comparator 2:
      WINMODE        : Boolean := False;
      --  unspecified
      Reserved_12_13 : HAL.UInt2 := 16#0#;
      --  Comparator 2 output selector This bit is controlled by software (if
      --  not locked). It selects the comparator 2 output:
      WINOUT         : Boolean := False;
      --  Comparator 2 polarity selector This bit is controlled by software (if
      --  not locked). It selects the comparator 2 output polarity:
      POLARITY       : Boolean := False;
      --  Comparator 2 hysteresis selector This bitfield is controlled by
      --  software (if not locked). It selects the hysteresis of the comparator
      --  2:
      HYST           : COMP2_CSR_HYST_Field := 16#0#;
      --  Comparator 2 power mode selector This bitfield is controlled by
      --  software (if not locked). It selects the power consumption and as a
      --  consequence the speed of the comparator 2:
      PWRMODE        : COMP2_CSR_PWRMODE_Field := 16#0#;
      --  Comparator 2 blanking source selector This bitfield is controlled by
      --  software (if not locked). It selects the blanking source: Others:
      --  Reserved, must not be used
      BLANKSEL       : COMP2_CSR_BLANKSEL_Field := 16#0#;
      --  unspecified
      Reserved_25_29 : HAL.UInt5 := 16#0#;
      --  Read-only. Comparator 2 output status This bit is read-only. It
      --  reflects the level of the comparator 2 output after the polarity
      --  selector and blanking, as indicated in Figure163.
      VALUE          : Boolean := False;
      --  COMP_CSR register lock This bit is set by software and cleared by a
      --  system reset. It locks the comparator 3 control bits. When locked,
      --  all register bits are read-only.
      LOCK           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for COMP2_CSR_Register use record
      EN             at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      INMSEL         at 0 range 4 .. 7;
      INPSEL         at 0 range 8 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      WINMODE        at 0 range 11 .. 11;
      Reserved_12_13 at 0 range 12 .. 13;
      WINOUT         at 0 range 14 .. 14;
      POLARITY       at 0 range 15 .. 15;
      HYST           at 0 range 16 .. 17;
      PWRMODE        at 0 range 18 .. 19;
      BLANKSEL       at 0 range 20 .. 24;
      Reserved_25_29 at 0 range 25 .. 29;
      VALUE          at 0 range 30 .. 30;
      LOCK           at 0 range 31 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  COMP address block description
   type COMP1_Peripheral is record
      --  Comparator 1 control and status register
      COMP1_CSR : aliased COMP1_CSR_Register;
      --  Comparator 2 control and status register
      COMP2_CSR : aliased COMP2_CSR_Register;
   end record
     with Volatile;

   for COMP1_Peripheral use record
      COMP1_CSR at 16#0# range 0 .. 31;
      COMP2_CSR at 16#4# range 0 .. 31;
   end record;

   --  COMP address block description
   COMP1_Periph : aliased COMP1_Peripheral
     with Import, Address => COMP1_Base;

end STM32_SVD.COMP;
