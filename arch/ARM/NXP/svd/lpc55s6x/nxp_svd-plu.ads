--  Copyright 2016-2019 NXP
--  All rights reserved.SPDX-License-Identifier: BSD-3-Clause


--  This spec has been automatically generated from LPC55S6x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NXP_SVD.PLU is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   -----------------------------
   -- LUT cluster's Registers --
   -----------------------------

   --  Selects the input source to be connected to LUT0 input0. For each LUT,
   --  the slot associated with the output from LUTn itself is tied low.
   type LUT_INP_MUX_LUTn_INPx_Field is
     (
      --  The PLU primary inputs 0.
      PLU_INPUTS0,
      --  The PLU primary inputs 1.
      PLU_INPUTS1,
      --  The PLU primary inputs 2.
      PLU_INPUTS2,
      --  The PLU primary inputs 3.
      PLU_INPUTS3,
      --  The PLU primary inputs 4.
      PLU_INPUTS4,
      --  The PLU primary inputs 5.
      PLU_INPUTS5,
      --  The output of LUT0.
      LUT_OUTPUTS0,
      --  The output of LUT1.
      LUT_OUTPUTS1,
      --  The output of LUT2.
      LUT_OUTPUTS2,
      --  The output of LUT3.
      LUT_OUTPUTS3,
      --  The output of LUT4.
      LUT_OUTPUTS4,
      --  The output of LUT5.
      LUT_OUTPUTS5,
      --  The output of LUT6.
      LUT_OUTPUTS6,
      --  The output of LUT7.
      LUT_OUTPUTS7,
      --  The output of LUT8.
      LUT_OUTPUTS8,
      --  The output of LUT9.
      LUT_OUTPUTS9,
      --  The output of LUT10.
      LUT_OUTPUTS10,
      --  The output of LUT11.
      LUT_OUTPUTS11,
      --  The output of LUT12.
      LUT_OUTPUTS12,
      --  The output of LUT13.
      LUT_OUTPUTS13,
      --  The output of LUT14.
      LUT_OUTPUTS14,
      --  The output of LUT15.
      LUT_OUTPUTS15,
      --  The output of LUT16.
      LUT_OUTPUTS16,
      --  The output of LUT17.
      LUT_OUTPUTS17,
      --  The output of LUT18.
      LUT_OUTPUTS18,
      --  The output of LUT19.
      LUT_OUTPUTS19,
      --  The output of LUT20.
      LUT_OUTPUTS20,
      --  The output of LUT21.
      LUT_OUTPUTS21,
      --  The output of LUT22.
      LUT_OUTPUTS22,
      --  The output of LUT23.
      LUT_OUTPUTS23,
      --  The output of LUT24.
      LUT_OUTPUTS24,
      --  The output of LUT25.
      LUT_OUTPUTS25,
      --  state(0).
      STATE0,
      --  state(1).
      STATE1,
      --  state(2).
      STATE2,
      --  state(3).
      STATE3)
     with Size => 6;
   for LUT_INP_MUX_LUTn_INPx_Field use
     (PLU_INPUTS0 => 0,
      PLU_INPUTS1 => 1,
      PLU_INPUTS2 => 2,
      PLU_INPUTS3 => 3,
      PLU_INPUTS4 => 4,
      PLU_INPUTS5 => 5,
      LUT_OUTPUTS0 => 6,
      LUT_OUTPUTS1 => 7,
      LUT_OUTPUTS2 => 8,
      LUT_OUTPUTS3 => 9,
      LUT_OUTPUTS4 => 10,
      LUT_OUTPUTS5 => 11,
      LUT_OUTPUTS6 => 12,
      LUT_OUTPUTS7 => 13,
      LUT_OUTPUTS8 => 14,
      LUT_OUTPUTS9 => 15,
      LUT_OUTPUTS10 => 16,
      LUT_OUTPUTS11 => 17,
      LUT_OUTPUTS12 => 18,
      LUT_OUTPUTS13 => 19,
      LUT_OUTPUTS14 => 20,
      LUT_OUTPUTS15 => 21,
      LUT_OUTPUTS16 => 22,
      LUT_OUTPUTS17 => 23,
      LUT_OUTPUTS18 => 24,
      LUT_OUTPUTS19 => 25,
      LUT_OUTPUTS20 => 26,
      LUT_OUTPUTS21 => 27,
      LUT_OUTPUTS22 => 28,
      LUT_OUTPUTS23 => 29,
      LUT_OUTPUTS24 => 30,
      LUT_OUTPUTS25 => 31,
      STATE0 => 32,
      STATE1 => 33,
      STATE2 => 34,
      STATE3 => 35);

   --  LUTn input x MUX
   type LUT_INP_MUX_LUT_Register is record
      --  Selects the input source to be connected to LUT0 input0. For each
      --  LUT, the slot associated with the output from LUTn itself is tied
      --  low.
      LUTn_INPx     : LUT_INP_MUX_LUTn_INPx_Field := NXP_SVD.PLU.PLU_INPUTS0;
      --  unspecified
      Reserved_6_31 : HAL.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for LUT_INP_MUX_LUT_Register use record
      LUTn_INPx     at 0 range 0 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   --  LUTn input x MUX
   type LUT_INP_MUX_LUT_Registers is array (0 .. 4)
     of LUT_INP_MUX_LUT_Register
     with Volatile;

   --  no description available
   type LUT_Cluster is record
      --  LUTn input x MUX
      LUT_INP_MUX : aliased LUT_INP_MUX_LUT_Registers;
   end record
     with Volatile, Size => 256;

   for LUT_Cluster use record
      LUT_INP_MUX at 0 range 0 .. 159;
   end record;

   --  no description available
   type LUT_Clusters is array (0 .. 25) of LUT_Cluster;

   --  Specifies the Truth Table contents for LUT0

   --  Specifies the Truth Table contents for LUT0
   type LUT_TRUTH_Registers is array (0 .. 25) of HAL.UInt32
     with Volatile;

   subtype OUTPUTS_OUTPUT_STATE_Field is HAL.UInt8;

   --  Provides the current state of the 8 designated PLU Outputs.
   type OUTPUTS_Register is record
      --  Read-only. Provides the current state of the 8 designated PLU
      --  Outputs..
      OUTPUT_STATE  : OUTPUTS_OUTPUT_STATE_Field;
      --  unspecified
      Reserved_8_31 : HAL.UInt24;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OUTPUTS_Register use record
      OUTPUT_STATE  at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype WAKEINT_CTRL_MASK_Field is HAL.UInt8;

   --  control input of the PLU, add filtering for glitch.
   type WAKEINT_CTRL_FILTER_MODE_Field is
     (
      --  Bypass mode.
      Bypass,
      --  Filter 1 clock period.
      Filter1Clk,
      --  Filter 2 clock period.
      Filter2Clk,
      --  Filter 3 clock period.
      Filter3Clk)
     with Size => 2;
   for WAKEINT_CTRL_FILTER_MODE_Field use
     (Bypass => 0,
      Filter1Clk => 1,
      Filter2Clk => 2,
      Filter3Clk => 3);

   --  hclk is divided by 2**filter_clksel.
   type WAKEINT_CTRL_FILTER_CLKSEL_Field is
     (
      --  Selects the 1 MHz low-power oscillator as the filter clock.
      Fro1Mhz,
      --  Selects the 12 Mhz FRO as the filter clock.
      Fro12Mhz,
      --  Selects a third filter clock source, if provided.
      Other_Clock)
     with Size => 2;
   for WAKEINT_CTRL_FILTER_CLKSEL_Field use
     (Fro1Mhz => 0,
      Fro12Mhz => 1,
      Other_Clock => 2);

   --  Wakeup interrupt control for PLU
   type WAKEINT_CTRL_Register is record
      --  Interrupt mask (which of the 8 PLU Outputs contribute to interrupt)
      MASK           : WAKEINT_CTRL_MASK_Field := 16#0#;
      --  control input of the PLU, add filtering for glitch.
      FILTER_MODE    : WAKEINT_CTRL_FILTER_MODE_Field := NXP_SVD.PLU.Bypass;
      --  hclk is divided by 2**filter_clksel.
      FILTER_CLKSEL  : WAKEINT_CTRL_FILTER_CLKSEL_Field :=
                        NXP_SVD.PLU.Fro1Mhz;
      --  latch the interrupt , then can be cleared with next bit INTR_CLEAR
      LATCH_ENABLE   : Boolean := False;
      --  Write data bit of one shall clear (set to zero) the corresponding bit
      --  in the field. Write to clear wakeint_latched
      INTR_CLEAR     : Boolean := False;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for WAKEINT_CTRL_Register use record
      MASK           at 0 range 0 .. 7;
      FILTER_MODE    at 0 range 8 .. 9;
      FILTER_CLKSEL  at 0 range 10 .. 11;
      LATCH_ENABLE   at 0 range 12 .. 12;
      INTR_CLEAR     at 0 range 13 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   --  Selects the source to be connected to PLU Output 0.
   type OUTPUT_MUX_OUTPUTn_Field is
     (
      --  The PLU output 0.
      PLU_OUTPUT0,
      --  The PLU output 1.
      PLU_OUTPUT1,
      --  The PLU output 2.
      PLU_OUTPUT2,
      --  The PLU output 3.
      PLU_OUTPUT3,
      --  The PLU output 4.
      PLU_OUTPUT4,
      --  The PLU output 5.
      PLU_OUTPUT5,
      --  The PLU output 6.
      PLU_OUTPUT6,
      --  The PLU output 7.
      PLU_OUTPUT7,
      --  The PLU output 8.
      PLU_OUTPUT8,
      --  The PLU output 9.
      PLU_OUTPUT9,
      --  The PLU output 10.
      PLU_OUTPUT10,
      --  The PLU output 11.
      PLU_OUTPUT11,
      --  The PLU output 12.
      PLU_OUTPUT12,
      --  The PLU output 13.
      PLU_OUTPUT13,
      --  The PLU output 14.
      PLU_OUTPUT14,
      --  The PLU output 15.
      PLU_OUTPUT15,
      --  The PLU output 16.
      PLU_OUTPUT16,
      --  The PLU output 17.
      PLU_OUTPUT17,
      --  The PLU output 18.
      PLU_OUTPUT18,
      --  The PLU output 19.
      PLU_OUTPUT19,
      --  The PLU output 20.
      PLU_OUTPUT20,
      --  The PLU output 21.
      PLU_OUTPUT21,
      --  The PLU output 22.
      PLU_OUTPUT22,
      --  The PLU output 23.
      PLU_OUTPUT23,
      --  The PLU output 24.
      PLU_OUTPUT24,
      --  The PLU output 25.
      PLU_OUTPUT25,
      --  state(0).
      STATE0,
      --  state(1).
      STATE1,
      --  state(2).
      STATE2,
      --  state(3).
      STATE3)
     with Size => 5;
   for OUTPUT_MUX_OUTPUTn_Field use
     (PLU_OUTPUT0 => 0,
      PLU_OUTPUT1 => 1,
      PLU_OUTPUT2 => 2,
      PLU_OUTPUT3 => 3,
      PLU_OUTPUT4 => 4,
      PLU_OUTPUT5 => 5,
      PLU_OUTPUT6 => 6,
      PLU_OUTPUT7 => 7,
      PLU_OUTPUT8 => 8,
      PLU_OUTPUT9 => 9,
      PLU_OUTPUT10 => 10,
      PLU_OUTPUT11 => 11,
      PLU_OUTPUT12 => 12,
      PLU_OUTPUT13 => 13,
      PLU_OUTPUT14 => 14,
      PLU_OUTPUT15 => 15,
      PLU_OUTPUT16 => 16,
      PLU_OUTPUT17 => 17,
      PLU_OUTPUT18 => 18,
      PLU_OUTPUT19 => 19,
      PLU_OUTPUT20 => 20,
      PLU_OUTPUT21 => 21,
      PLU_OUTPUT22 => 22,
      PLU_OUTPUT23 => 23,
      PLU_OUTPUT24 => 24,
      PLU_OUTPUT25 => 25,
      STATE0 => 26,
      STATE1 => 27,
      STATE2 => 28,
      STATE3 => 29);

   --  Selects the source to be connected to PLU Output 0
   type OUTPUT_MUX_Register is record
      --  Selects the source to be connected to PLU Output 0.
      OUTPUTn       : OUTPUT_MUX_OUTPUTn_Field := NXP_SVD.PLU.PLU_OUTPUT0;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OUTPUT_MUX_Register use record
      OUTPUTn       at 0 range 0 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   --  Selects the source to be connected to PLU Output 0
   type OUTPUT_MUX_Registers is array (0 .. 7) of OUTPUT_MUX_Register
     with Volatile;

   -----------------
   -- Peripherals --
   -----------------

   --  LPC80X Programmable Logic Unit (PLU)
   type PLU_Peripheral is record
      --  no description available
      LUT          : aliased LUT_Clusters;
      --  Specifies the Truth Table contents for LUT0
      LUT_TRUTH    : aliased LUT_TRUTH_Registers;
      --  Provides the current state of the 8 designated PLU Outputs.
      OUTPUTS      : aliased OUTPUTS_Register;
      --  Wakeup interrupt control for PLU
      WAKEINT_CTRL : aliased WAKEINT_CTRL_Register;
      --  Selects the source to be connected to PLU Output 0
      OUTPUT_MUX   : aliased OUTPUT_MUX_Registers;
   end record
     with Volatile;

   for PLU_Peripheral use record
      LUT          at 16#0# range 0 .. 6655;
      LUT_TRUTH    at 16#800# range 0 .. 831;
      OUTPUTS      at 16#900# range 0 .. 31;
      WAKEINT_CTRL at 16#904# range 0 .. 31;
      OUTPUT_MUX   at 16#C00# range 0 .. 255;
   end record;

   --  LPC80X Programmable Logic Unit (PLU)
   PLU_Periph : aliased PLU_Peripheral
     with Import, Address => System'To_Address (16#4003D000#);

end NXP_SVD.PLU;
