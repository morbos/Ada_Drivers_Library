--  This spec has been automatically generated from STM32L151.svd

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

   subtype CSR_INSEL_Field is HAL.UInt3;
   subtype CSR_OUTSEL_Field is HAL.UInt3;

   --  CSR_FCH array
   type CSR_FCH_Field_Array is array (3 .. 4) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CSR_FCH
   type CSR_FCH_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FCH as a value
            Val : HAL.UInt2;
         when True =>
            --  FCH as an array
            Arr : CSR_FCH_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CSR_FCH_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  comparator control and status register
   type CSR_Register is record
      --  10 kO pull-up resistor
      PU10K          : Boolean := False;
      --  400 kO pull-up resistor
      PU400K         : Boolean := False;
      --  10 kO pull-down resistor
      PD10K          : Boolean := False;
      --  400 kO pull-down resistor
      PD400K         : Boolean := False;
      --  Comparator 1 enable
      CMP1EN         : Boolean := False;
      --  SW1 analog switch enable
      SW1            : Boolean := False;
      --  unspecified
      Reserved_6_6   : HAL.Bit := 16#0#;
      --  Read-only. Comparator 1 output
      CMP1OUT        : Boolean := False;
      --  unspecified
      Reserved_8_11  : HAL.UInt4 := 16#0#;
      --  Comparator 2 speed mode
      SPEED          : Boolean := False;
      --  Read-only. Comparator 2 output
      CMP2OUT        : Boolean := False;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  VREFINT output enable
      VREFOUTEN      : Boolean := False;
      --  Window mode enable
      WNDWE          : Boolean := False;
      --  Inverted input selection
      INSEL          : CSR_INSEL_Field := 16#0#;
      --  Comparator 2 output selection
      OUTSEL         : CSR_OUTSEL_Field := 16#0#;
      --  unspecified
      Reserved_24_25 : HAL.UInt2 := 16#0#;
      --  Select GPIO port PA3 as fast ADC input channel CH3.
      FCH            : CSR_FCH_Field := (As_Array => False, Val => 16#0#);
      --  Select GPIO port PC3 as re-routed ADC input channel CH13.
      RCH13          : Boolean := False;
      --  Channel Acquisition Interrupt Enable / Clear
      CAIE           : Boolean := False;
      --  Read-only. Channel acquisition interrupt flag
      CAIF           : Boolean := False;
      --  Suspend Timer Mode
      TSUSP          : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CSR_Register use record
      PU10K          at 0 range 0 .. 0;
      PU400K         at 0 range 1 .. 1;
      PD10K          at 0 range 2 .. 2;
      PD400K         at 0 range 3 .. 3;
      CMP1EN         at 0 range 4 .. 4;
      SW1            at 0 range 5 .. 5;
      Reserved_6_6   at 0 range 6 .. 6;
      CMP1OUT        at 0 range 7 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      SPEED          at 0 range 12 .. 12;
      CMP2OUT        at 0 range 13 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      VREFOUTEN      at 0 range 16 .. 16;
      WNDWE          at 0 range 17 .. 17;
      INSEL          at 0 range 18 .. 20;
      OUTSEL         at 0 range 21 .. 23;
      Reserved_24_25 at 0 range 24 .. 25;
      FCH            at 0 range 26 .. 27;
      RCH13          at 0 range 28 .. 28;
      CAIE           at 0 range 29 .. 29;
      CAIF           at 0 range 30 .. 30;
      TSUSP          at 0 range 31 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Comparators
   type COMP_Peripheral is record
      --  comparator control and status register
      CSR : aliased CSR_Register;
   end record
     with Volatile;

   for COMP_Peripheral use record
      CSR at 0 range 0 .. 31;
   end record;

   --  Comparators
   COMP_Periph : aliased COMP_Peripheral
     with Import, Address => System'To_Address (16#40007C00#);

end STM32_SVD.COMP;
