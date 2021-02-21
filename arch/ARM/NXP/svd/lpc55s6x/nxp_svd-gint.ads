--  Copyright 2016-2019 NXP
--  All rights reserved.SPDX-License-Identifier: BSD-3-Clause


--  This spec has been automatically generated from LPC55S6x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NXP_SVD.GINT is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Group interrupt status. This bit is cleared by writing a one to it.
   --  Writing zero has no effect.
   type CTRL_INT_Field is
     (
      --  No request. No interrupt request is pending.
      No_Request,
      --  Request active. Interrupt request is active.
      Request_Active)
     with Size => 1;
   for CTRL_INT_Field use
     (No_Request => 0,
      Request_Active => 1);

   --  Combine enabled inputs for group interrupt
   type CTRL_COMB_Field is
     (
      --  Or. OR functionality: A grouped interrupt is generated when any one
      --  of the enabled inputs is active (based on its programmed polarity).
      Or_k,
      --  And. AND functionality: An interrupt is generated when all enabled
      --  bits are active (based on their programmed polarity).
      And_k)
     with Size => 1;
   for CTRL_COMB_Field use
     (Or_k => 0,
      And_k => 1);

   --  Group interrupt trigger
   type CTRL_TRIG_Field is
     (
      --  Edge-triggered.
      Edge_Triggered,
      --  Level-triggered.
      Level_Triggered)
     with Size => 1;
   for CTRL_TRIG_Field use
     (Edge_Triggered => 0,
      Level_Triggered => 1);

   --  GPIO grouped interrupt control register
   type CTRL_Register is record
      --  Group interrupt status. This bit is cleared by writing a one to it.
      --  Writing zero has no effect.
      INT           : CTRL_INT_Field := NXP_SVD.GINT.No_Request;
      --  Combine enabled inputs for group interrupt
      COMB          : CTRL_COMB_Field := NXP_SVD.GINT.Or_k;
      --  Group interrupt trigger
      TRIG          : CTRL_TRIG_Field := NXP_SVD.GINT.Edge_Triggered;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CTRL_Register use record
      INT           at 0 range 0 .. 0;
      COMB          at 0 range 1 .. 1;
      TRIG          at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  GPIO grouped interrupt port 0 polarity register

   --  GPIO grouped interrupt port 0 polarity register
   type PORT_POL_Registers is array (0 .. 1) of HAL.UInt32
     with Volatile;

   --  GPIO grouped interrupt port 0 enable register

   --  GPIO grouped interrupt port 0 enable register
   type PORT_ENA_Registers is array (0 .. 1) of HAL.UInt32
     with Volatile;

   -----------------
   -- Peripherals --
   -----------------

   --  Group GPIO input interrupt (GINT0/1)
   type GINT_Peripheral is record
      --  GPIO grouped interrupt control register
      CTRL     : aliased CTRL_Register;
      --  GPIO grouped interrupt port 0 polarity register
      PORT_POL : aliased PORT_POL_Registers;
      --  GPIO grouped interrupt port 0 enable register
      PORT_ENA : aliased PORT_ENA_Registers;
   end record
     with Volatile;

   for GINT_Peripheral use record
      CTRL     at 16#0# range 0 .. 31;
      PORT_POL at 16#20# range 0 .. 63;
      PORT_ENA at 16#40# range 0 .. 63;
   end record;

   --  Group GPIO input interrupt (GINT0/1)
   GINT0_Periph : aliased GINT_Peripheral
     with Import, Address => System'To_Address (16#40002000#);

   --  Group GPIO input interrupt (GINT0/1)
   GINT1_Periph : aliased GINT_Peripheral
     with Import, Address => System'To_Address (16#40003000#);

end NXP_SVD.GINT;
