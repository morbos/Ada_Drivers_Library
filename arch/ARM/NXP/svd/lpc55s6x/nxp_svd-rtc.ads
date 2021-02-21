--  Copyright 2016-2019 NXP
--  All rights reserved.SPDX-License-Identifier: BSD-3-Clause


--  This spec has been automatically generated from LPC55S6x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NXP_SVD.RTC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Software reset control
   type CTRL_SWRESET_Field is
     (
      --  Not in reset. The RTC is not held in reset. This bit must be cleared
      --  prior to configuring or initiating any operation of the RTC.
      Not_In_Reset,
      --  In reset. The RTC is held in reset. All register bits within the RTC
      --  will be forced to their reset value except the OFD bit. This bit must
      --  be cleared before writing to any register in the RTC - including
      --  writes to set any of the other bits within this register. Do not
      --  attempt to write to any bits of this register at the same time that
      --  the reset bit is being cleared.
      In_Reset)
     with Size => 1;
   for CTRL_SWRESET_Field use
     (Not_In_Reset => 0,
      In_Reset => 1);

   --  RTC 1 Hz timer alarm flag status.
   type CTRL_ALARM1HZ_Field is
     (
      --  No match. No match has occurred on the 1 Hz RTC timer. Writing a 0
      --  has no effect.
      No_Match,
      --  Match. A match condition has occurred on the 1 Hz RTC timer. This
      --  flag generates an RTC alarm interrupt request RTC_ALARM which can
      --  also wake up the part from any low power mode. Writing a 1 clears
      --  this bit.
      Match)
     with Size => 1;
   for CTRL_ALARM1HZ_Field use
     (No_Match => 0,
      Match => 1);

   --  RTC 1 kHz timer wake-up flag status.
   type CTRL_WAKE1KHZ_Field is
     (
      --  Run. The RTC 1 kHz timer is running. Writing a 0 has no effect.
      Run,
      --  Time-out. The 1 kHz high-resolution/wake-up timer has timed out. This
      --  flag generates an RTC wake-up interrupt request RTC-WAKE which can
      --  also wake up the part from any low power mode. Writing a 1 clears
      --  this bit.
      Timeout)
     with Size => 1;
   for CTRL_WAKE1KHZ_Field use
     (Run => 0,
      Timeout => 1);

   --  RTC 1 Hz timer alarm enable for Deep power-down.
   type CTRL_ALARMDPD_EN_Field is
     (
      --  Disable. A match on the 1 Hz RTC timer will not bring the part out of
      --  Deep power-down mode.
      Disable,
      --  Enable. A match on the 1 Hz RTC timer bring the part out of Deep
      --  power-down mode.
      Enable)
     with Size => 1;
   for CTRL_ALARMDPD_EN_Field use
     (Disable => 0,
      Enable => 1);

   --  RTC 1 kHz timer wake-up enable for Deep power-down.
   type CTRL_WAKEDPD_EN_Field is
     (
      --  Disable. A match on the 1 kHz RTC timer will not bring the part out
      --  of Deep power-down mode.
      Disable,
      --  Enable. A match on the 1 kHz RTC timer bring the part out of Deep
      --  power-down mode.
      Enable)
     with Size => 1;
   for CTRL_WAKEDPD_EN_Field use
     (Disable => 0,
      Enable => 1);

   --  RTC 1 kHz clock enable. This bit can be set to 0 to conserve power if
   --  the 1 kHz timer is not used. This bit has no effect when the RTC is
   --  disabled (bit 7 of this register is 0).
   type CTRL_RTC1KHZ_EN_Field is
     (
      --  Disable. A match on the 1 kHz RTC timer will not bring the part out
      --  of Deep power-down mode.
      Disable,
      --  Enable. The 1 kHz RTC timer is enabled.
      Enable)
     with Size => 1;
   for CTRL_RTC1KHZ_EN_Field use
     (Disable => 0,
      Enable => 1);

   --  RTC enable.
   type CTRL_RTC_EN_Field is
     (
      --  Disable. The RTC 1 Hz and 1 kHz clocks are shut down and the RTC
      --  operation is disabled. This bit should be 0 when writing to load a
      --  value in the RTC counter register.
      Disable,
      --  Enable. The 1 Hz RTC clock is running and RTC operation is enabled.
      --  This bit must be set to initiate operation of the RTC. The first
      --  clock to the RTC counter occurs 1 s after this bit is set. To also
      --  enable the high-resolution, 1 kHz clock, set bit 6 in this register.
      Enable)
     with Size => 1;
   for CTRL_RTC_EN_Field use
     (Disable => 0,
      Enable => 1);

   --  RTC oscillator power-down control.
   type CTRL_RTC_OSC_PD_Field is
     (
      --  See RTC_OSC_BYPASS
      Power_Up,
      --  RTC oscillator is powered-down.
      Powered_Down)
     with Size => 1;
   for CTRL_RTC_OSC_PD_Field use
     (Power_Up => 0,
      Powered_Down => 1);

   --  RTC oscillator bypass control.
   type CTRL_RTC_OSC_BYPASS_Field is
     (
      --  The RTC Oscillator operates normally as a crystal oscillator with the
      --  crystal connected between the RTC_XTALIN and RTC_XTALOUT pins.
      Used,
      --  The RTC Oscillator is in bypass mode. In this mode a clock can be
      --  directly input into the RTC_XTALIN pin.
      Bypass)
     with Size => 1;
   for CTRL_RTC_OSC_BYPASS_Field use
     (Used => 0,
      Bypass => 1);

   --  RTC Sub-second counter control.
   type CTRL_RTC_SUBSEC_ENA_Field is
     (
      --  The sub-second counter (if implemented) is disabled. This bit is
      --  cleared by a system-level POR or BOD reset as well as a by the
      --  RTC_ENA bit (bit 7 in this register). On modules not equipped with a
      --  sub-second counter, this bit will always read-back as a '0'.
      Power_Up,
      --  The 32 KHz sub-second counter is enabled (if implemented). Counting
      --  commences on the start of the first one-second interval after this
      --  bit is set. Note: This bit can only be set after the RTC_ENA bit (bit
      --  7) is set by a previous write operation. Note: The RTC sub-second
      --  counter must be re-enabled whenever the chip exits deep power-down
      --  mode.
      Powered_Down)
     with Size => 1;
   for CTRL_RTC_SUBSEC_ENA_Field use
     (Power_Up => 0,
      Powered_Down => 1);

   --  RTC control register
   type CTRL_Register is record
      --  Software reset control
      SWRESET        : CTRL_SWRESET_Field := NXP_SVD.RTC.In_Reset;
      --  unspecified
      Reserved_1_1   : HAL.Bit := 16#0#;
      --  RTC 1 Hz timer alarm flag status.
      ALARM1HZ       : CTRL_ALARM1HZ_Field := NXP_SVD.RTC.No_Match;
      --  RTC 1 kHz timer wake-up flag status.
      WAKE1KHZ       : CTRL_WAKE1KHZ_Field := NXP_SVD.RTC.Run;
      --  RTC 1 Hz timer alarm enable for Deep power-down.
      ALARMDPD_EN    : CTRL_ALARMDPD_EN_Field := NXP_SVD.RTC.Disable;
      --  RTC 1 kHz timer wake-up enable for Deep power-down.
      WAKEDPD_EN     : CTRL_WAKEDPD_EN_Field := NXP_SVD.RTC.Disable;
      --  RTC 1 kHz clock enable. This bit can be set to 0 to conserve power if
      --  the 1 kHz timer is not used. This bit has no effect when the RTC is
      --  disabled (bit 7 of this register is 0).
      RTC1KHZ_EN     : CTRL_RTC1KHZ_EN_Field := NXP_SVD.RTC.Disable;
      --  RTC enable.
      RTC_EN         : CTRL_RTC_EN_Field := NXP_SVD.RTC.Disable;
      --  RTC oscillator power-down control.
      RTC_OSC_PD     : CTRL_RTC_OSC_PD_Field := NXP_SVD.RTC.Power_Up;
      --  RTC oscillator bypass control.
      RTC_OSC_BYPASS : CTRL_RTC_OSC_BYPASS_Field := NXP_SVD.RTC.Used;
      --  RTC Sub-second counter control.
      RTC_SUBSEC_ENA : CTRL_RTC_SUBSEC_ENA_Field := NXP_SVD.RTC.Power_Up;
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CTRL_Register use record
      SWRESET        at 0 range 0 .. 0;
      Reserved_1_1   at 0 range 1 .. 1;
      ALARM1HZ       at 0 range 2 .. 2;
      WAKE1KHZ       at 0 range 3 .. 3;
      ALARMDPD_EN    at 0 range 4 .. 4;
      WAKEDPD_EN     at 0 range 5 .. 5;
      RTC1KHZ_EN     at 0 range 6 .. 6;
      RTC_EN         at 0 range 7 .. 7;
      RTC_OSC_PD     at 0 range 8 .. 8;
      RTC_OSC_BYPASS at 0 range 9 .. 9;
      RTC_SUBSEC_ENA at 0 range 10 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   subtype WAKE_VAL_Field is HAL.UInt16;

   --  High-resolution/wake-up timer control register
   type WAKE_Register is record
      --  A read reflects the current value of the high-resolution/wake-up
      --  timer. A write pre-loads a start count value into the wake-up timer
      --  and initializes a count-down sequence. Do not write to this register
      --  while counting is in progress.
      VAL            : WAKE_VAL_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for WAKE_Register use record
      VAL            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype SUBSEC_SUBSEC_Field is HAL.UInt15;

   --  Sub-second counter register
   type SUBSEC_Register is record
      --  Read-only. A read reflects the current value of the 32KHz sub-second
      --  counter. This counter is cleared whenever the SUBSEC_ENA bit in the
      --  RTC_CONTROL register is low. Up-counting at a 32KHz rate commences at
      --  the start of the next one-second interval after the SUBSEC_ENA bit is
      --  set. This counter must be re-enabled after exiting deep power-down
      --  mode or after the main RTC module is disabled and re-enabled. On
      --  modules not equipped with a sub-second counter, this register will
      --  read-back as all zeroes.
      SUBSEC         : SUBSEC_SUBSEC_Field;
      --  unspecified
      Reserved_15_31 : HAL.UInt17;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SUBSEC_Register use record
      SUBSEC         at 0 range 0 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   --  General Purpose register

   --  General Purpose register
   type GPREG_Registers is array (0 .. 7) of HAL.UInt32
     with Volatile;

   -----------------
   -- Peripherals --
   -----------------

   --  Real-Time Clock (RTC)
   type RTC_Peripheral is record
      --  RTC control register
      CTRL   : aliased CTRL_Register;
      --  RTC match register
      MATCH  : aliased HAL.UInt32;
      --  RTC counter register
      COUNT  : aliased HAL.UInt32;
      --  High-resolution/wake-up timer control register
      WAKE   : aliased WAKE_Register;
      --  Sub-second counter register
      SUBSEC : aliased SUBSEC_Register;
      --  General Purpose register
      GPREG  : aliased GPREG_Registers;
   end record
     with Volatile;

   for RTC_Peripheral use record
      CTRL   at 16#0# range 0 .. 31;
      MATCH  at 16#4# range 0 .. 31;
      COUNT  at 16#8# range 0 .. 31;
      WAKE   at 16#C# range 0 .. 31;
      SUBSEC at 16#10# range 0 .. 31;
      GPREG  at 16#40# range 0 .. 255;
   end record;

   --  Real-Time Clock (RTC)
   RTC_Periph : aliased RTC_Peripheral
     with Import, Address => System'To_Address (16#4002C000#);

end NXP_SVD.RTC;
