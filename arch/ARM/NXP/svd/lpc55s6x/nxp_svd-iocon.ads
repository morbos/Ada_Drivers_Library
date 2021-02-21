--  Copyright 2016-2019 NXP
--  All rights reserved.SPDX-License-Identifier: BSD-3-Clause


--  This spec has been automatically generated from LPC55S6x_mod2.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NXP_SVD.IOCON is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   ---------------------------
   -- P cluster's Registers --
   ---------------------------

   -----------------------------
   -- PIO cluster's Registers --
   -----------------------------

   --  Selects pin function.
   type CTL_FUNC_Field is
     (--  Alternative connection 0.
      Alt0,
      --  Alternative connection 1.
      Alt1,
      --  Alternative connection 2.
      Alt2,
      --  Alternative connection 3.
      Alt3,
      --  Alternative connection 4.
      Alt4,
      --  Alternative connection 5.
      Alt5,
      --  Alternative connection 6.
      Alt6,
      --  Alternative connection 7.
      Alt7)
     with Size => 4;
   for CTL_FUNC_Field use
     (Alt0 => 0,
      Alt1 => 1,
      Alt2 => 2,
      Alt3 => 3,
      Alt4 => 4,
      Alt5 => 5,
      Alt6 => 6,
      Alt7 => 7);

   --  Selects function mode (on-chip pull-up/pull-down resistor control).
   type CTL_MODE_Field is
     (--  Inactive. Inactive (no pull-down/pull-up resistor enabled).
      Inactive,
      --  Pull-down. Pull-down resistor enabled.
      Pull_Down,
      --  Pull-up. Pull-up resistor enabled.
      Pull_Up,
      --  Repeater. Repeater mode.
      Repeater)
     with Size => 2;
   for CTL_MODE_Field use
     (Inactive => 0,
      Pull_Down => 1,
      Pull_Up => 2,
      Repeater => 3);

   --  Driver slew rate.
   type CTL_SLEW_Field is
     (--  Standard-mode, output slew rate is slower. More outputs can be switched
--  simultaneously.
      Standard_Mode,
      --  Fast-mode, output slew rate is faster. Refer to the appropriate specific
--  device data sheet for details.
      Fast_Mode)
     with Size => 1;
   for CTL_SLEW_Field use
     (Standard_Mode => 0,
      Fast_Mode => 1);

   --  Input polarity.
   type CTL_INVERT_Field is
     (--  Disabled. Input function is not inverted.
      Disabled,
      --  Enabled. Input is function inverted.
      Enabled)
     with Size => 1;
   for CTL_INVERT_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Select Digital mode.
   type CTL_DIGIMODE_Field is
     (--  Disable digital mode. Digital input set to 0.
      Analog,
      --  Enable Digital mode. Digital input is enabled.
      Digital)
     with Size => 1;
   for CTL_DIGIMODE_Field use
     (Analog => 0,
      Digital => 1);

   --  Controls open-drain mode.
   type CTL_OD_Field is
     (--  Normal. Normal push-pull output
      Normal,
      --  Open-drain. Simulated open-drain output (high drive disabled).
      Open_Drain)
     with Size => 1;
   for CTL_OD_Field use
     (Normal => 0,
      Open_Drain => 1);

   --  Analog switch input control.
   type CTL_ASW_Field is
     (--  For pins PIO0_9, PIO0_11, PIO0_12, PIO0_15, PIO0_18, PIO0_31, PIO1_0 and
--  PIO1_9, analog switch is closed (enabled). For the other pins, analog
--  switch is open (disabled).
      Value0,
      --  For all pins except PIO0_9, PIO0_11, PIO0_12, PIO0_15, PIO0_18, PIO0_31,
--  PIO1_0 and PIO1_9 analog switch is closed (enabled)
      Value1)
     with Size => 1;
   for CTL_ASW_Field use
     (Value0 => 0,
      Value1 => 1);

   type CTL_PIO_Register is record
      --  Selects pin function.
      FUNC           : HAL.UInt4 := 0;
      --  Selects function mode (on-chip pull-up/pull-down resistor control).
      MODE           : CTL_MODE_Field := NXP_SVD.IOCON.Inactive;
      --  Driver slew rate.
      SLEW           : CTL_SLEW_Field := NXP_SVD.IOCON.Standard_Mode;
      --  Input polarity.
      INVERT         : CTL_INVERT_Field := NXP_SVD.IOCON.Disabled;
      --  Select Digital mode.
      DIGIMODE       : CTL_DIGIMODE_Field := NXP_SVD.IOCON.Analog;
      --  Controls open-drain mode.
      OD             : CTL_OD_Field := NXP_SVD.IOCON.Normal;
      --  Analog switch input control.
      ASW            : CTL_ASW_Field := NXP_SVD.IOCON.Value0;
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CTL_PIO_Register use record
      FUNC           at 0 range 0 .. 3;
      MODE           at 0 range 4 .. 5;
      SLEW           at 0 range 6 .. 6;
      INVERT         at 0 range 7 .. 7;
      DIGIMODE       at 0 range 8 .. 8;
      OD             at 0 range 9 .. 9;
      ASW            at 0 range 10 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   type PIO_Cluster is record
      CTL : aliased CTL_PIO_Register;
   end record
     with Size => 32;

   for PIO_Cluster use record
      CTL at 0 range 0 .. 31;
   end record;

   type PIO_Clusters is array (0 .. 31) of PIO_Cluster;

   --  Digital I/O control for ports
   type P_Cluster is record
      PIO : aliased PIO_Clusters;
   end record
     with Size => 1024;

   for P_Cluster use record
      PIO at 0 range 0 .. 1023;
   end record;

   --  Digital I/O control for ports
   type P_Clusters is array (0 .. 1) of P_Cluster;

   -----------------
   -- Peripherals --
   -----------------

   --  I/O pin configuration (IOCON)
   type IOCON_Peripheral is record
      --  Digital I/O control for ports
      P : aliased P_Clusters;
   end record
     with Volatile;

   for IOCON_Peripheral use record
      P at 0 range 0 .. 2047;
   end record;

   --  I/O pin configuration (IOCON)
   IOCON_Periph : aliased IOCON_Peripheral
     with Import, Address => IOCON_Base;

end NXP_SVD.IOCON;
