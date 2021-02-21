--  Copyright 2016-2019 NXP
--  All rights reserved.SPDX-License-Identifier: BSD-3-Clause


--  This spec has been automatically generated from LPC55S6x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NXP_SVD.CASPER is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Which bank-pair the offset ABOFF is within. This must be 0 if only 2-up
   type CTRL0_ABBPAIR_Field is
     (
      --  Bank-pair 0 (1st)
      Pair0,
      --  Bank-pair 1 (2nd)
      Pair1)
     with Size => 1;
   for CTRL0_ABBPAIR_Field use
     (Pair0 => 0,
      Pair1 => 1);

   --  Which bank-pair the offset CDOFF is within. This must be 0 if only 2-up
   type CTRL0_CDBPAIR_Field is
     (
      --  Bank-pair 0 (1st)
      Pair0,
      --  Bank-pair 1 (2nd)
      Pair1)
     with Size => 1;
   for CTRL0_CDBPAIR_Field use
     (Pair0 => 0,
      Pair1 => 1);

   subtype CTRL0_CDOFF_Field is HAL.UInt11;

   --  Contains the offsets of AB and CD in the RAM.
   type CTRL0_Register is record
      --  Which bank-pair the offset ABOFF is within. This must be 0 if only
      --  2-up
      ABBPAIR        : CTRL0_ABBPAIR_Field := NXP_SVD.CASPER.Pair0;
      --  unspecified
      Reserved_1_1   : HAL.Bit := 16#0#;
      --  Word or DWord Offset of AB values, with B at [2]=0 and A at [2]=1 as
      --  far as the code sees (normally will be an interleaved bank so only
      --  sequential to AHB). Word offset only allowed if 32 bit operation.
      --  Ideally not in the same RAM as the CD values if 4-up
      ABOFF          : Boolean := False;
      --  unspecified
      Reserved_3_15  : HAL.UInt13 := 16#0#;
      --  Which bank-pair the offset CDOFF is within. This must be 0 if only
      --  2-up
      CDBPAIR        : CTRL0_CDBPAIR_Field := NXP_SVD.CASPER.Pair0;
      --  unspecified
      Reserved_17_17 : HAL.Bit := 16#0#;
      --  Word or DWord Offset of CD, with D at [2]=0 and C at [2]=1 as far as
      --  the code sees (normally will be an interleaved bank so only
      --  sequential to AHB). Word offset only allowed if 32 bit operation.
      --  Ideally not in the same RAM as the AB values
      CDOFF          : CTRL0_CDOFF_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CTRL0_Register use record
      ABBPAIR        at 0 range 0 .. 0;
      Reserved_1_1   at 0 range 1 .. 1;
      ABOFF          at 0 range 2 .. 2;
      Reserved_3_15  at 0 range 3 .. 15;
      CDBPAIR        at 0 range 16 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      CDOFF          at 0 range 18 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype CTRL1_ITER_Field is HAL.UInt8;
   subtype CTRL1_MODE_Field is HAL.UInt8;

   --  Which bank-pair the offset RESOFF is within. This must be 0 if only
   --  2-up. Ideally this is not the same bank as ABBPAIR (when 4-up supported)
   type CTRL1_RESBPAIR_Field is
     (
      --  Bank-pair 0 (1st)
      Pair0,
      --  Bank-pair 1 (2nd)
      Pair1)
     with Size => 1;
   for CTRL1_RESBPAIR_Field use
     (Pair0 => 0,
      Pair1 => 1);

   subtype CTRL1_RESOFF_Field is HAL.UInt11;

   --  Skip rules on Carry if needed. This operation will be skipped based on
   --  Carry value (from previous operation) if not 0:
   type CTRL1_CSKIP_Field is
     (
      --  No Skip
      No_Skip,
      --  Skip if Carry is 1
      Skip_If_1,
      --  Skip if Carry is 0
      Skip_If_0,
      --  Set CTRLOFF to CDOFF and Skip
      Set_And_Skip)
     with Size => 2;
   for CTRL1_CSKIP_Field use
     (No_Skip => 0,
      Skip_If_1 => 1,
      Skip_If_0 => 2,
      Set_And_Skip => 3);

   --  Contains the opcode mode, iteration count, and result offset (in RAM)
   --  and also launches the accelerator. Note: with CP version: CTRL0 and
   --  CRTL1 can be written in one go with MCRR.
   type CTRL1_Register is record
      --  Iteration counter. Is number_cycles - 1. write 0 means Does one cycle
      --  - does not iterate.
      ITER           : CTRL1_ITER_Field := 16#0#;
      --  Operation mode to perform. write 0 means Accelerator is inactive.
      --  write others means accelerator is active.
      MODE           : CTRL1_MODE_Field := 16#0#;
      --  Which bank-pair the offset RESOFF is within. This must be 0 if only
      --  2-up. Ideally this is not the same bank as ABBPAIR (when 4-up
      --  supported)
      RESBPAIR       : CTRL1_RESBPAIR_Field := NXP_SVD.CASPER.Pair0;
      --  unspecified
      Reserved_17_17 : HAL.Bit := 16#0#;
      --  Word or DWord Offset of result. Word offset only allowed if 32 bit
      --  operation. Ideally not in the same RAM as the AB and CD values
      RESOFF         : CTRL1_RESOFF_Field := 16#0#;
      --  unspecified
      Reserved_29_29 : HAL.Bit := 16#0#;
      --  Skip rules on Carry if needed. This operation will be skipped based
      --  on Carry value (from previous operation) if not 0:
      CSKIP          : CTRL1_CSKIP_Field := NXP_SVD.CASPER.No_Skip;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CTRL1_Register use record
      ITER           at 0 range 0 .. 7;
      MODE           at 0 range 8 .. 15;
      RESBPAIR       at 0 range 16 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      RESOFF         at 0 range 18 .. 28;
      Reserved_29_29 at 0 range 29 .. 29;
      CSKIP          at 0 range 30 .. 31;
   end record;

   subtype LOADER_COUNT_Field is HAL.UInt8;

   --  Which bank-pair the offset CTRLOFF is within. This must be 0 if only
   --  2-up. Does not matter which bank is used as this is loaded when not
   --  performing an operation.
   type LOADER_CTRLBPAIR_Field is
     (
      --  Bank-pair 0 (1st)
      Pair0,
      --  Bank-pair 1 (2nd)
      Pair1)
     with Size => 1;
   for LOADER_CTRLBPAIR_Field use
     (Pair0 => 0,
      Pair1 => 1);

   subtype LOADER_CTRLOFF_Field is HAL.UInt11;

   --  Contains an optional loader to load into CTRL0/1 in steps to perform a
   --  set of operations.
   type LOADER_Register is record
      --  Number of control pairs to load 0 relative (so 1 means load 1). write
      --  1 means Does one op - does not iterate, write N means N control pairs
      --  to load
      COUNT          : LOADER_COUNT_Field := 16#0#;
      --  unspecified
      Reserved_8_15  : HAL.UInt8 := 16#0#;
      --  Which bank-pair the offset CTRLOFF is within. This must be 0 if only
      --  2-up. Does not matter which bank is used as this is loaded when not
      --  performing an operation.
      CTRLBPAIR      : LOADER_CTRLBPAIR_Field := NXP_SVD.CASPER.Pair0;
      --  unspecified
      Reserved_17_17 : HAL.Bit := 16#0#;
      --  DWord Offset of CTRL pair to load next.
      CTRLOFF        : LOADER_CTRLOFF_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for LOADER_Register use record
      COUNT          at 0 range 0 .. 7;
      Reserved_8_15  at 0 range 8 .. 15;
      CTRLBPAIR      at 0 range 16 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      CTRLOFF        at 0 range 18 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   --  Indicates if the accelerator has finished an operation. Write 1 to
   --  clear, or write CTRL1 to clear.
   type STATUS_DONE_Field is
     (
      --  Busy or just cleared
      Busy,
      --  Completed last operation
      Completed)
     with Size => 1;
   for STATUS_DONE_Field use
     (Busy => 0,
      Completed => 1);

   --  Last carry value if operation produced a carry bit
   type STATUS_CARRY_Field is
     (
      --  Carry was 0 or no carry
      No_Carry,
      --  Carry was 1
      Carry)
     with Size => 1;
   for STATUS_CARRY_Field use
     (No_Carry => 0,
      Carry => 1);

   --  Indicates if the accelerator is busy performing an operation
   type STATUS_BUSY_Field is
     (
      --  Not busy - is idle
      Idle,
      --  Is busy
      Busy)
     with Size => 1;
   for STATUS_BUSY_Field use
     (Idle => 0,
      Busy => 1);

   --  Indicates operational status and would contain the carry bit if used.
   type STATUS_Register is record
      --  Indicates if the accelerator has finished an operation. Write 1 to
      --  clear, or write CTRL1 to clear.
      DONE          : STATUS_DONE_Field := NXP_SVD.CASPER.Busy;
      --  unspecified
      Reserved_1_3  : HAL.UInt3 := 16#0#;
      --  Read-only. Last carry value if operation produced a carry bit
      CARRY         : STATUS_CARRY_Field := NXP_SVD.CASPER.No_Carry;
      --  Read-only. Indicates if the accelerator is busy performing an
      --  operation
      BUSY          : STATUS_BUSY_Field := NXP_SVD.CASPER.Idle;
      --  unspecified
      Reserved_6_31 : HAL.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for STATUS_Register use record
      DONE          at 0 range 0 .. 0;
      Reserved_1_3  at 0 range 1 .. 3;
      CARRY         at 0 range 4 .. 4;
      BUSY          at 0 range 5 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   --  Set if the accelerator should interrupt when done.
   type INTENSET_DONE_Field is
     (
      --  Do not interrupt when done
      No_Interrupt,
      --  Interrupt when done
      Interrupt)
     with Size => 1;
   for INTENSET_DONE_Field use
     (No_Interrupt => 0,
      Interrupt => 1);

   --  Sets interrupts
   type INTENSET_Register is record
      --  Set if the accelerator should interrupt when done.
      DONE          : INTENSET_DONE_Field := NXP_SVD.CASPER.No_Interrupt;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENSET_Register use record
      DONE          at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Written to clear an interrupt set with INTENSET.
   type INTENCLR_DONE_Field is
     (
      --  If written 0, ignored
      Ignored,
      --  If written 1, do not Interrupt when done
      No_Interrupt)
     with Size => 1;
   for INTENCLR_DONE_Field use
     (Ignored => 0,
      No_Interrupt => 1);

   --  Clears interrupts
   type INTENCLR_Register is record
      --  Write data bit of one shall clear (set to zero) the corresponding bit
      --  in the field. Written to clear an interrupt set with INTENSET.
      DONE          : INTENCLR_DONE_Field := NXP_SVD.CASPER.Ignored;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENCLR_Register use record
      DONE          at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  If set, interrupt is caused by accelerator being done.
   type INTSTAT_DONE_Field is
     (
      --  Not caused by accelerator being done
      Not_Caused,
      --  Caused by accelerator being done
      Caused)
     with Size => 1;
   for INTSTAT_DONE_Field use
     (Not_Caused => 0,
      Caused => 1);

   --  Interrupt status bits (mask of INTENSET and STATUS)
   type INTSTAT_Register is record
      --  Read-only. If set, interrupt is caused by accelerator being done.
      DONE          : INTSTAT_DONE_Field;
      --  unspecified
      Reserved_1_31 : HAL.UInt31;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTSTAT_Register use record
      DONE          at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Reads back with security level locked to, or 0. Writes as 0 to unlock, 1
   --  to lock.
   type LOCK_LOCK_Field is
     (
      --  unlock
      Unlock,
      --  Lock to current security level
      Lock)
     with Size => 1;
   for LOCK_LOCK_Field use
     (Unlock => 0,
      Lock => 1);

   --  Must be written as 0x73D to change the register.
   type LOCK_KEY_Field is
     (
      --  Reset value for the field
      Lock_Key_Field_Reset,
      --  If set during write, will allow lock or unlock
      Kwy_Value)
     with Size => 13;
   for LOCK_KEY_Field use
     (Lock_Key_Field_Reset => 0,
      Kwy_Value => 1853);

   --  Security lock register
   type LOCK_Register is record
      --  Reads back with security level locked to, or 0. Writes as 0 to
      --  unlock, 1 to lock.
      LOCK           : LOCK_LOCK_Field := NXP_SVD.CASPER.Unlock;
      --  unspecified
      Reserved_1_3   : HAL.UInt3 := 16#0#;
      --  Must be written as 0x73D to change the register.
      KEY            : LOCK_KEY_Field := Lock_Key_Field_Reset;
      --  unspecified
      Reserved_17_31 : HAL.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for LOCK_Register use record
      LOCK           at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      KEY            at 0 range 4 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  CASPER
   type CASPER_Peripheral is record
      --  Contains the offsets of AB and CD in the RAM.
      CTRL0    : aliased CTRL0_Register;
      --  Contains the opcode mode, iteration count, and result offset (in RAM)
      --  and also launches the accelerator. Note: with CP version: CTRL0 and
      --  CRTL1 can be written in one go with MCRR.
      CTRL1    : aliased CTRL1_Register;
      --  Contains an optional loader to load into CTRL0/1 in steps to perform
      --  a set of operations.
      LOADER   : aliased LOADER_Register;
      --  Indicates operational status and would contain the carry bit if used.
      STATUS   : aliased STATUS_Register;
      --  Sets interrupts
      INTENSET : aliased INTENSET_Register;
      --  Clears interrupts
      INTENCLR : aliased INTENCLR_Register;
      --  Interrupt status bits (mask of INTENSET and STATUS)
      INTSTAT  : aliased INTSTAT_Register;
      --  A register
      AREG     : aliased HAL.UInt32;
      --  B register
      BREG     : aliased HAL.UInt32;
      --  C register
      CREG     : aliased HAL.UInt32;
      --  D register
      DREG     : aliased HAL.UInt32;
      --  Result register 0
      RES0     : aliased HAL.UInt32;
      --  Result register 1
      RES1     : aliased HAL.UInt32;
      --  Result register 2
      RES2     : aliased HAL.UInt32;
      --  Result register 3
      RES3     : aliased HAL.UInt32;
      --  Optional mask register
      MASK     : aliased HAL.UInt32;
      --  Optional re-mask register
      REMASK   : aliased HAL.UInt32;
      --  Security lock register
      LOCK     : aliased LOCK_Register;
   end record
     with Volatile;

   for CASPER_Peripheral use record
      CTRL0    at 16#0# range 0 .. 31;
      CTRL1    at 16#4# range 0 .. 31;
      LOADER   at 16#8# range 0 .. 31;
      STATUS   at 16#C# range 0 .. 31;
      INTENSET at 16#10# range 0 .. 31;
      INTENCLR at 16#14# range 0 .. 31;
      INTSTAT  at 16#18# range 0 .. 31;
      AREG     at 16#20# range 0 .. 31;
      BREG     at 16#24# range 0 .. 31;
      CREG     at 16#28# range 0 .. 31;
      DREG     at 16#2C# range 0 .. 31;
      RES0     at 16#30# range 0 .. 31;
      RES1     at 16#34# range 0 .. 31;
      RES2     at 16#38# range 0 .. 31;
      RES3     at 16#3C# range 0 .. 31;
      MASK     at 16#60# range 0 .. 31;
      REMASK   at 16#64# range 0 .. 31;
      LOCK     at 16#80# range 0 .. 31;
   end record;

   --  CASPER
   CASPER_Periph : aliased CASPER_Peripheral
     with Import, Address => System'To_Address (16#400A5000#);

end NXP_SVD.CASPER;
