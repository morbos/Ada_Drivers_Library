--  Copyright 2016-2019 NXP
--  All rights reserved.SPDX-License-Identifier: BSD-3-Clause


--  This spec has been automatically generated from LPC55S6x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NXP_SVD.WWDT is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Watchdog enable bit. Once this bit is set to one and a watchdog feed is
   --  performed, the watchdog timer will run permanently.
   type MOD_WDEN_Field is
     (
      --  Stop. The watchdog timer is stopped.
      Stop,
      --  Run. The watchdog timer is running.
      Run)
     with Size => 1;
   for MOD_WDEN_Field use
     (Stop => 0,
      Run => 1);

   --  Watchdog reset enable bit. Once this bit has been written with a 1 it
   --  cannot be re-written with a 0.
   type MOD_WDRESET_Field is
     (
      --  Interrupt. A watchdog time-out will not cause a chip reset.
      Interrupt,
      --  Reset. A watchdog time-out will cause a chip reset.
      Reset)
     with Size => 1;
   for MOD_WDRESET_Field use
     (Interrupt => 0,
      Reset => 1);

   --  Watchdog update mode. This bit can be set once by software and is only
   --  cleared by a reset.
   type MOD_WDPROTECT_Field is
     (
      --  Flexible. The watchdog time-out value (TC) can be changed at any
      --  time.
      Flexible,
      --  Threshold. The watchdog time-out value (TC) can be changed only after
      --  the counter is below the value of WDWARNINT and WDWINDOW.
      Threshold)
     with Size => 1;
   for MOD_WDPROTECT_Field use
     (Flexible => 0,
      Threshold => 1);

   --  Watchdog mode register. This register contains the basic mode and status
   --  of the Watchdog Timer.
   type MOD_Register is record
      --  Watchdog enable bit. Once this bit is set to one and a watchdog feed
      --  is performed, the watchdog timer will run permanently.
      WDEN          : MOD_WDEN_Field := NXP_SVD.WWDT.Stop;
      --  Watchdog reset enable bit. Once this bit has been written with a 1 it
      --  cannot be re-written with a 0.
      WDRESET       : MOD_WDRESET_Field := NXP_SVD.WWDT.Interrupt;
      --  Watchdog time-out flag. Set when the watchdog timer times out, by a
      --  feed error, or by events associated with WDPROTECT. Cleared by
      --  software writing a 0 to this bit position. Causes a chip reset if
      --  WDRESET = 1.
      WDTOF         : Boolean := False;
      --  Warning interrupt flag. Set when the timer is at or below the value
      --  in WDWARNINT. Cleared by software writing a 1 to this bit position.
      --  Note that this bit cannot be cleared while the WARNINT value is equal
      --  to the value of the TV register. This can occur if the value of
      --  WARNINT is 0 and the WDRESET bit is 0 when TV decrements to 0.
      WDINT         : Boolean := False;
      --  Watchdog update mode. This bit can be set once by software and is
      --  only cleared by a reset.
      WDPROTECT     : MOD_WDPROTECT_Field := NXP_SVD.WWDT.Flexible;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MOD_Register use record
      WDEN          at 0 range 0 .. 0;
      WDRESET       at 0 range 1 .. 1;
      WDTOF         at 0 range 2 .. 2;
      WDINT         at 0 range 3 .. 3;
      WDPROTECT     at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype TC_COUNT_Field is HAL.UInt24;

   --  Watchdog timer constant register. This 24-bit register determines the
   --  time-out value.
   type TC_Register is record
      --  Watchdog time-out value.
      COUNT          : TC_COUNT_Field := 16#FF#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TC_Register use record
      COUNT          at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype FEED_FEED_Field is HAL.UInt8;

   --  Watchdog feed sequence register. Writing 0xAA followed by 0x55 to this
   --  register reloads the Watchdog timer with the value contained in TC.
   type FEED_Register is record
      --  Write-only. Feed value should be 0xAA followed by 0x55.
      FEED          : FEED_FEED_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FEED_Register use record
      FEED          at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype TV_COUNT_Field is HAL.UInt24;

   --  Watchdog timer value register. This 24-bit register reads out the
   --  current value of the Watchdog timer.
   type TV_Register is record
      --  Read-only. Counter timer value.
      COUNT          : TV_COUNT_Field;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TV_Register use record
      COUNT          at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype WARNINT_WARNINT_Field is HAL.UInt10;

   --  Watchdog Warning Interrupt compare value.
   type WARNINT_Register is record
      --  Watchdog warning interrupt compare value.
      WARNINT        : WARNINT_WARNINT_Field := 16#0#;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for WARNINT_Register use record
      WARNINT        at 0 range 0 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype WINDOW_WINDOW_Field is HAL.UInt24;

   --  Watchdog Window compare value.
   type WINDOW_Register is record
      --  Watchdog window value.
      WINDOW         : WINDOW_WINDOW_Field := 16#FFFFFF#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for WINDOW_Register use record
      WINDOW         at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Windowed Watchdog Timer (WWDT)
   type WWDT_Peripheral is record
      --  Watchdog mode register. This register contains the basic mode and
      --  status of the Watchdog Timer.
      MOD_k   : aliased MOD_Register;
      --  Watchdog timer constant register. This 24-bit register determines the
      --  time-out value.
      TC      : aliased TC_Register;
      --  Watchdog feed sequence register. Writing 0xAA followed by 0x55 to
      --  this register reloads the Watchdog timer with the value contained in
      --  TC.
      FEED    : aliased FEED_Register;
      --  Watchdog timer value register. This 24-bit register reads out the
      --  current value of the Watchdog timer.
      TV      : aliased TV_Register;
      --  Watchdog Warning Interrupt compare value.
      WARNINT : aliased WARNINT_Register;
      --  Watchdog Window compare value.
      WINDOW  : aliased WINDOW_Register;
   end record
     with Volatile;

   for WWDT_Peripheral use record
      MOD_k   at 16#0# range 0 .. 31;
      TC      at 16#4# range 0 .. 31;
      FEED    at 16#8# range 0 .. 31;
      TV      at 16#C# range 0 .. 31;
      WARNINT at 16#14# range 0 .. 31;
      WINDOW  at 16#18# range 0 .. 31;
   end record;

   --  Windowed Watchdog Timer (WWDT)
   WWDT_Periph : aliased WWDT_Peripheral
     with Import, Address => System'To_Address (16#4000C000#);

end NXP_SVD.WWDT;
