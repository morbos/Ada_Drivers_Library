--  Copyright 2016-2019 NXP
--  All rights reserved.SPDX-License-Identifier: BSD-3-Clause


--  This spec has been automatically generated from LPC55S6x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NXP_SVD.OSTIMER is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype EVTIMERH_EVTIMER_COUNT_VALUE_Field is HAL.UInt10;

   --  EVTIMER High Register
   type EVTIMERH_Register is record
      --  Read-only. A read reflects the current value of the upper 32 bits of
      --  the EVTIMER. Note there is physically only one EVTimer, readable from
      --  all domains.
      EVTIMER_COUNT_VALUE : EVTIMERH_EVTIMER_COUNT_VALUE_Field;
      --  unspecified
      Reserved_10_31      : HAL.UInt22;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EVTIMERH_Register use record
      EVTIMER_COUNT_VALUE at 0 range 0 .. 9;
      Reserved_10_31      at 0 range 10 .. 31;
   end record;

   subtype CAPTUREn_H_CAPTUREn_VALUE_Field is HAL.UInt10;

   --  Local Capture High Register for CPUn
   type CAPTUREn_H_Register is record
      --  Read-only. A read reflects the value of the upper 32 bits of the
      --  central EVTIMER at the time the last capture signal was generated by
      --  the CPU. A separate pair of CAPTURE registers are implemented for
      --  each CPU. Each CPU reads its own capture value at the same pair of
      --  addresses.
      CAPTUREn_VALUE : CAPTUREn_H_CAPTUREn_VALUE_Field;
      --  unspecified
      Reserved_10_31 : HAL.UInt22;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CAPTUREn_H_Register use record
      CAPTUREn_VALUE at 0 range 0 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype MATCHn_H_MATCHn_VALUE_Field is HAL.UInt10;

   --  Match High Register for CPUn
   type MATCHn_H_Register is record
      --  The value written to the MATCH (L/H) register pair is compared
      --  against the central EVTIMER. When a match occurs, an interrupt
      --  request is generated if enabled. A separate pair of MATCH registers
      --  are implemented for each CPU. Each CPU reads its own local value at
      --  the same pair of addresses.
      MATCHn_VALUE   : MATCHn_H_MATCHn_VALUE_Field := 16#0#;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MATCHn_H_Register use record
      MATCHn_VALUE   at 0 range 0 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   --  OS_EVENT TIMER Control Register for CPUn
   type OSEVENT_CTRL_Register is record
      --  This bit is set when a match occurs between the central 64-bit
      --  EVTIMER and the value programmed in the Match-register pair for the
      --  associated CPU This bit is cleared by writing a '1'. Writes to clear
      --  this bit are asynchronous. This should be done before a new match
      --  value is written into the MATCH_L/H registers
      OSTIMER_INTRFLAG : Boolean := False;
      --  When this bit is '1' an interrupt/wakeup request to the Domainn
      --  processor will be asserted when the OSTIMER_INTR flag is set. When
      --  this bit is '0', interrupt/wakeup requests due to the OSTIMER_INTR
      --  flag are blocked.A separate OSEVENT_CTRL register is implemented for
      --  each CPU. Each CPU reads its own local value at the same address.
      OSTIMER_INTENA   : Boolean := False;
      --  unspecified
      Reserved_2_31    : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OSEVENT_CTRL_Register use record
      OSTIMER_INTRFLAG at 0 range 0 .. 0;
      OSTIMER_INTENA   at 0 range 1 .. 1;
      Reserved_2_31    at 0 range 2 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Synchronous OS/Event timer with Wakeup Timer
   type OSTIMER_Peripheral is record
      --  EVTIMER Low Register
      EVTIMERL     : aliased HAL.UInt32;
      --  EVTIMER High Register
      EVTIMERH     : aliased EVTIMERH_Register;
      --  Local Capture Low Register for CPUn
      CAPTUREn_L   : aliased HAL.UInt32;
      --  Local Capture High Register for CPUn
      CAPTUREn_H   : aliased CAPTUREn_H_Register;
      --  Local Match Low Register for CPUn
      MATCHn_L     : aliased HAL.UInt32;
      --  Match High Register for CPUn
      MATCHn_H     : aliased MATCHn_H_Register;
      --  OS_EVENT TIMER Control Register for CPUn
      OSEVENT_CTRL : aliased OSEVENT_CTRL_Register;
   end record
     with Volatile;

   for OSTIMER_Peripheral use record
      EVTIMERL     at 16#0# range 0 .. 31;
      EVTIMERH     at 16#4# range 0 .. 31;
      CAPTUREn_L   at 16#8# range 0 .. 31;
      CAPTUREn_H   at 16#C# range 0 .. 31;
      MATCHn_L     at 16#10# range 0 .. 31;
      MATCHn_H     at 16#14# range 0 .. 31;
      OSEVENT_CTRL at 16#1C# range 0 .. 31;
   end record;

   --  Synchronous OS/Event timer with Wakeup Timer
   OSTIMER_Periph : aliased OSTIMER_Peripheral
     with Import, Address => System'To_Address (16#4002D000#);

end NXP_SVD.OSTIMER;
