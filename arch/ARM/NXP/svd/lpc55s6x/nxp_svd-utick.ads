--  Copyright 2016-2019 NXP
--  All rights reserved.SPDX-License-Identifier: BSD-3-Clause


--  This spec has been automatically generated from LPC55S6x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NXP_SVD.UTICK is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype CTRL_DELAYVAL_Field is HAL.UInt31;

   --  Control register.
   type CTRL_Register is record
      --  Tick interval value. The delay will be equal to DELAYVAL + 1 periods
      --  of the timer clock. The minimum usable value is 1, for a delay of 2
      --  timer clocks. A value of 0 stops the timer.
      DELAYVAL : CTRL_DELAYVAL_Field := 16#0#;
      --  Repeat delay. 0 = One-time delay. 1 = Delay repeats continuously.
      REPEAT   : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CTRL_Register use record
      DELAYVAL at 0 range 0 .. 30;
      REPEAT   at 0 range 31 .. 31;
   end record;

   --  Status register.
   type STAT_Register is record
      --  Interrupt flag. 0 = No interrupt is pending. 1 = An interrupt is
      --  pending. A write of any value to this register clears this flag.
      INTR          : Boolean := False;
      --  Active flag. 0 = The Micro-Tick Timer is stopped. 1 = The Micro-Tick
      --  Timer is currently active.
      ACTIVE        : Boolean := False;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for STAT_Register use record
      INTR          at 0 range 0 .. 0;
      ACTIVE        at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  CFG_CAPEN array
   type CFG_CAPEN_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for CFG_CAPEN
   type CFG_CAPEN_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CAPEN as a value
            Val : HAL.UInt4;
         when True =>
            --  CAPEN as an array
            Arr : CFG_CAPEN_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for CFG_CAPEN_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  CFG_CAPPOL array
   type CFG_CAPPOL_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for CFG_CAPPOL
   type CFG_CAPPOL_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CAPPOL as a value
            Val : HAL.UInt4;
         when True =>
            --  CAPPOL as an array
            Arr : CFG_CAPPOL_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for CFG_CAPPOL_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  Capture configuration register.
   type CFG_Register is record
      --  Enable Capture 0. 1 = Enabled, 0 = Disabled.
      CAPEN          : CFG_CAPEN_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Capture Polarity 0. 0 = Positive edge capture, 1 = Negative edge
      --  capture.
      CAPPOL         : CFG_CAPPOL_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CFG_Register use record
      CAPEN          at 0 range 0 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      CAPPOL         at 0 range 8 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   --  CAPCLR array
   type CAPCLR_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for CAPCLR
   type CAPCLR_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CAPCLR as a value
            Val : HAL.UInt4;
         when True =>
            --  CAPCLR as an array
            Arr : CAPCLR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for CAPCLR_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  Capture clear register.
   type CAPCLR_Register is record
      --  Write-only. Clear capture 0. Writing 1 to this bit clears the CAP0
      --  register value.
      CAPCLR        : CAPCLR_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CAPCLR_Register use record
      CAPCLR        at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   subtype CAP_CAP_VALUE_Field is HAL.UInt31;

   --  Capture register .
   type CAP_Register is record
      --  Read-only. Capture value for the related capture event (UTICK_CAPn.
      --  Note: the value is 1 lower than the actual value of the Micro-tick
      --  Timer at the moment of the capture event.
      CAP_VALUE : CAP_CAP_VALUE_Field;
      --  Read-only. Capture Valid. When 1, a value has been captured based on
      --  a transition of the related UTICK_CAPn pin. Cleared by writing to the
      --  related bit in the CAPCLR register.
      VALID     : Boolean;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CAP_Register use record
      CAP_VALUE at 0 range 0 .. 30;
      VALID     at 0 range 31 .. 31;
   end record;

   --  Capture register .
   type CAP_Registers is array (0 .. 3) of CAP_Register
     with Volatile;

   -----------------
   -- Peripherals --
   -----------------

   --  Micro-tick Timer (UTICK)
   type UTICK0_Peripheral is record
      --  Control register.
      CTRL   : aliased CTRL_Register;
      --  Status register.
      STAT   : aliased STAT_Register;
      --  Capture configuration register.
      CFG    : aliased CFG_Register;
      --  Capture clear register.
      CAPCLR : aliased CAPCLR_Register;
      --  Capture register .
      CAP    : aliased CAP_Registers;
   end record
     with Volatile;

   for UTICK0_Peripheral use record
      CTRL   at 16#0# range 0 .. 31;
      STAT   at 16#4# range 0 .. 31;
      CFG    at 16#8# range 0 .. 31;
      CAPCLR at 16#C# range 0 .. 31;
      CAP    at 16#10# range 0 .. 127;
   end record;

   --  Micro-tick Timer (UTICK)
   UTICK0_Periph : aliased UTICK0_Peripheral
     with Import, Address => System'To_Address (16#4000E000#);

end NXP_SVD.UTICK;
