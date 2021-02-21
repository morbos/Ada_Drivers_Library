--  Copyright 2016-2019 NXP
--  All rights reserved.SPDX-License-Identifier: BSD-3-Clause


--  This spec has been automatically generated from LPC55S6x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NXP_SVD.SCT is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  SCT operation
   type CONFIG_UNIFY_Field is
     (
      --  The SCT operates as two 16-bit counters named COUNTER_L and
      --  COUNTER_H.
      Dual_Counter,
      --  The SCT operates as a unified 32-bit counter.
      Unified_Counter)
     with Size => 1;
   for CONFIG_UNIFY_Field use
     (Dual_Counter => 0,
      Unified_Counter => 1);

   --  SCT clock mode
   type CONFIG_CLKMODE_Field is
     (
      --  System Clock Mode. The system clock clocks the entire SCT module
      --  including the counter(s) and counter prescalers.
      System_Clock_Mode,
      --  Sampled System Clock Mode. The system clock clocks the SCT module,
      --  but the counter and prescalers are only enabled to count when the
      --  designated edge is detected on the input selected by the CKSEL field.
      --  The minimum pulse width on the selected clock-gate input is 1 bus
      --  clock period. This mode is the high-performance, sampled-clock mode.
      Sampled_System_Clock_Mode,
      --  SCT Input Clock Mode. The input/edge selected by the CKSEL field
      --  clocks the SCT module, including the counters and prescalers, after
      --  first being synchronized to the system clock. The minimum pulse width
      --  on the clock input is 1 bus clock period. This mode is the low-power,
      --  sampled-clock mode.
      Sct_Input_Clock_Mode,
      --  Asynchronous Mode. The entire SCT module is clocked directly by the
      --  input/edge selected by the CKSEL field. In this mode, the SCT outputs
      --  are switched synchronously to the SCT input clock - not the system
      --  clock. The input clock rate must be at least half the system clock
      --  rate and can be the same or faster than the system clock.
      Asynchronous_Mode)
     with Size => 2;
   for CONFIG_CLKMODE_Field use
     (System_Clock_Mode => 0,
      Sampled_System_Clock_Mode => 1,
      Sct_Input_Clock_Mode => 2,
      Asynchronous_Mode => 3);

   --  SCT clock select. The specific functionality of the designated
   --  input/edge is dependent on the CLKMODE bit selection in this register.
   type CONFIG_CKSEL_Field is
     (
      --  Rising edges on input 0.
      Input_0_Rising_Edges,
      --  Falling edges on input 0.
      Input_0_Falling_Edge,
      --  Rising edges on input 1.
      Input_1_Rising_Edges,
      --  Falling edges on input 1.
      Input_1_Falling_Edge,
      --  Rising edges on input 2.
      Input_2_Rising_Edges,
      --  Falling edges on input 2.
      Input_2_Falling_Edge,
      --  Rising edges on input 3.
      Input_3_Rising_Edges,
      --  Falling edges on input 3.
      Input_3_Falling_Edge,
      --  Rising edges on input 4.
      Input_4_Rising_Edges,
      --  Falling edges on input 4.
      Input_4_Falling_Edge,
      --  Rising edges on input 5.
      Input_5_Rising_Edges,
      --  Falling edges on input 5.
      Input_5_Falling_Edge,
      --  Rising edges on input 6.
      Input_6_Rising_Edges,
      --  Falling edges on input 6.
      Input_6_Falling_Edge,
      --  Rising edges on input 7.
      Input_7_Rising_Edges,
      --  Falling edges on input 7.
      Input_7_Falling_Edge)
     with Size => 4;
   for CONFIG_CKSEL_Field use
     (Input_0_Rising_Edges => 0,
      Input_0_Falling_Edge => 1,
      Input_1_Rising_Edges => 2,
      Input_1_Falling_Edge => 3,
      Input_2_Rising_Edges => 4,
      Input_2_Falling_Edge => 5,
      Input_3_Rising_Edges => 6,
      Input_3_Falling_Edge => 7,
      Input_4_Rising_Edges => 8,
      Input_4_Falling_Edge => 9,
      Input_5_Rising_Edges => 10,
      Input_5_Falling_Edge => 11,
      Input_6_Rising_Edges => 12,
      Input_6_Falling_Edge => 13,
      Input_7_Rising_Edges => 14,
      Input_7_Falling_Edge => 15);

   subtype CONFIG_INSYNC_Field is HAL.UInt4;

   --  SCT configuration register
   type CONFIG_Register is record
      --  SCT operation
      UNIFY          : CONFIG_UNIFY_Field := NXP_SVD.SCT.Dual_Counter;
      --  SCT clock mode
      CLKMODE        : CONFIG_CLKMODE_Field := NXP_SVD.SCT.System_Clock_Mode;
      --  SCT clock select. The specific functionality of the designated
      --  input/edge is dependent on the CLKMODE bit selection in this
      --  register.
      CKSEL          : CONFIG_CKSEL_Field := NXP_SVD.SCT.Input_0_Rising_Edges;
      --  A 1 in this bit prevents the lower match registers from being
      --  reloaded from their respective reload registers. Setting this bit
      --  eliminates the need to write to the reload registers MATCHREL if the
      --  match values are fixed. Software can write to set or clear this bit
      --  at any time. This bit applies to both the higher and lower registers
      --  when the UNIFY bit is set.
      NORELOAD_L     : Boolean := False;
      --  A 1 in this bit prevents the higher match registers from being
      --  reloaded from their respective reload registers. Setting this bit
      --  eliminates the need to write to the reload registers MATCHREL if the
      --  match values are fixed. Software can write to set or clear this bit
      --  at any time. This bit is not used when the UNIFY bit is set.
      NORELOAD_H     : Boolean := False;
      --  Synchronization for input N (bit 9 = input 0, bit 10 = input 1,, bit
      --  12 = input 3); all other bits are reserved. A 1 in one of these bits
      --  subjects the corresponding input to synchronization to the SCT clock,
      --  before it is used to create an event. If an input is known to already
      --  be synchronous to the SCT clock, this bit may be set to 0 for faster
      --  input response. (Note: The SCT clock is the system clock for CKMODEs
      --  0-2. It is the selected, asynchronous SCT input clock for CKMODE3).
      --  Note that the INSYNC field only affects inputs used for event
      --  generation. It does not apply to the clock input specified in the
      --  CKSEL field.
      INSYNC         : CONFIG_INSYNC_Field := 16#F#;
      --  unspecified
      Reserved_13_16 : HAL.UInt4 := 16#0#;
      --  A one in this bit causes a match on match register 0 to be treated as
      --  a de-facto LIMIT condition without the need to define an associated
      --  event. As with any LIMIT event, this automatic limit causes the
      --  counter to be cleared to zero in unidirectional mode or to change the
      --  direction of count in bi-directional mode. Software can write to set
      --  or clear this bit at any time. This bit applies to both the higher
      --  and lower registers when the UNIFY bit is set.
      AUTOLIMIT_L    : Boolean := False;
      --  A one in this bit will cause a match on match register 0 to be
      --  treated as a de-facto LIMIT condition without the need to define an
      --  associated event. As with any LIMIT event, this automatic limit
      --  causes the counter to be cleared to zero in unidirectional mode or to
      --  change the direction of count in bi-directional mode. Software can
      --  write to set or clear this bit at any time. This bit is not used when
      --  the UNIFY bit is set.
      AUTOLIMIT_H    : Boolean := False;
      --  unspecified
      Reserved_19_31 : HAL.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CONFIG_Register use record
      UNIFY          at 0 range 0 .. 0;
      CLKMODE        at 0 range 1 .. 2;
      CKSEL          at 0 range 3 .. 6;
      NORELOAD_L     at 0 range 7 .. 7;
      NORELOAD_H     at 0 range 8 .. 8;
      INSYNC         at 0 range 9 .. 12;
      Reserved_13_16 at 0 range 13 .. 16;
      AUTOLIMIT_L    at 0 range 17 .. 17;
      AUTOLIMIT_H    at 0 range 18 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   --  L or unified counter direction select
   type CTRL_BIDIR_L_Field is
     (
      --  Up. The counter counts up to a limit condition, then is cleared to
      --  zero.
      Up,
      --  Up-down. The counter counts up to a limit, then counts down to a
      --  limit condition or to 0.
      Up_Down)
     with Size => 1;
   for CTRL_BIDIR_L_Field use
     (Up => 0,
      Up_Down => 1);

   subtype CTRL_PRE_L_Field is HAL.UInt8;

   --  Direction select
   type CTRL_BIDIR_H_Field is
     (
      --  The H counter counts up to its limit condition, then is cleared to
      --  zero.
      Up,
      --  The H counter counts up to its limit, then counts down to a limit
      --  condition or to 0.
      Up_Down)
     with Size => 1;
   for CTRL_BIDIR_H_Field use
     (Up => 0,
      Up_Down => 1);

   subtype CTRL_PRE_H_Field is HAL.UInt8;

   --  SCT control register
   type CTRL_Register is record
      --  This bit is 1 when the L or unified counter is counting down.
      --  Hardware sets this bit when the counter is counting up, counter limit
      --  occurs, and BIDIR = 1.Hardware clears this bit when the counter is
      --  counting down and a limit condition occurs or when the counter
      --  reaches 0.
      DOWN_L         : Boolean := False;
      --  When this bit is 1 and HALT is 0, the L or unified counter does not
      --  run, but I/O events related to the counter can occur. If a designated
      --  start event occurs, this bit is cleared and counting resumes.
      STOP_L         : Boolean := False;
      --  When this bit is 1, the L or unified counter does not run and no
      --  events can occur. A reset sets this bit. When the HALT_L bit is one,
      --  the STOP_L bit is cleared. It is possible to remove the halt
      --  condition while keeping the SCT in the stop condition (not running)
      --  with a single write to this register to simultaneously clear the HALT
      --  bit and set the STOP bit. Once set, only software can clear this bit
      --  to restore counter operation. This bit is set on reset.
      HALT_L         : Boolean := True;
      --  Writing a 1 to this bit clears the L or unified counter. This bit
      --  always reads as 0.
      CLRCTR_L       : Boolean := False;
      --  L or unified counter direction select
      BIDIR_L        : CTRL_BIDIR_L_Field := NXP_SVD.SCT.Up;
      --  Specifies the factor by which the SCT clock is prescaled to produce
      --  the L or unified counter clock. The counter clock is clocked at the
      --  rate of the SCT clock divided by PRE_L+1. Clear the counter (by
      --  writing a 1 to the CLRCTR bit) whenever changing the PRE value.
      PRE_L          : CTRL_PRE_L_Field := 16#0#;
      --  unspecified
      Reserved_13_15 : HAL.UInt3 := 16#0#;
      --  This bit is 1 when the H counter is counting down. Hardware sets this
      --  bit when the counter is counting, a counter limit condition occurs,
      --  and BIDIR is 1. Hardware clears this bit when the counter is counting
      --  down and a limit condition occurs or when the counter reaches 0.
      DOWN_H         : Boolean := False;
      --  When this bit is 1 and HALT is 0, the H counter does not, run but I/O
      --  events related to the counter can occur. If such an event matches the
      --  mask in the Start register, this bit is cleared and counting resumes.
      STOP_H         : Boolean := False;
      --  When this bit is 1, the H counter does not run and no events can
      --  occur. A reset sets this bit. When the HALT_H bit is one, the STOP_H
      --  bit is cleared. It is possible to remove the halt condition while
      --  keeping the SCT in the stop condition (not running) with a single
      --  write to this register to simultaneously clear the HALT bit and set
      --  the STOP bit. Once set, this bit can only be cleared by software to
      --  restore counter operation. This bit is set on reset.
      HALT_H         : Boolean := True;
      --  Writing a 1 to this bit clears the H counter. This bit always reads
      --  as 0.
      CLRCTR_H       : Boolean := False;
      --  Direction select
      BIDIR_H        : CTRL_BIDIR_H_Field := NXP_SVD.SCT.Up;
      --  Specifies the factor by which the SCT clock is prescaled to produce
      --  the H counter clock. The counter clock is clocked at the rate of the
      --  SCT clock divided by PRELH+1. Clear the counter (by writing a 1 to
      --  the CLRCTR bit) whenever changing the PRE value.
      PRE_H          : CTRL_PRE_H_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CTRL_Register use record
      DOWN_L         at 0 range 0 .. 0;
      STOP_L         at 0 range 1 .. 1;
      HALT_L         at 0 range 2 .. 2;
      CLRCTR_L       at 0 range 3 .. 3;
      BIDIR_L        at 0 range 4 .. 4;
      PRE_L          at 0 range 5 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      DOWN_H         at 0 range 16 .. 16;
      STOP_H         at 0 range 17 .. 17;
      HALT_H         at 0 range 18 .. 18;
      CLRCTR_H       at 0 range 19 .. 19;
      BIDIR_H        at 0 range 20 .. 20;
      PRE_H          at 0 range 21 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype LIMIT_LIMMSK_L_Field is HAL.UInt16;
   subtype LIMIT_LIMMSK_H_Field is HAL.UInt16;

   --  SCT limit event select register
   type LIMIT_Register is record
      --  If bit n is one, event n is used as a counter limit for the L or
      --  unified counter (event 0 = bit 0, event 1 = bit 1, etc.). The number
      --  of bits = number of events in this SCT.
      LIMMSK_L : LIMIT_LIMMSK_L_Field := 16#0#;
      --  If bit n is one, event n is used as a counter limit for the H counter
      --  (event 0 = bit 16, event 1 = bit 17, etc.). The number of bits =
      --  number of events in this SCT.
      LIMMSK_H : LIMIT_LIMMSK_H_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for LIMIT_Register use record
      LIMMSK_L at 0 range 0 .. 15;
      LIMMSK_H at 0 range 16 .. 31;
   end record;

   subtype HALT_HALTMSK_L_Field is HAL.UInt16;
   subtype HALT_HALTMSK_H_Field is HAL.UInt16;

   --  SCT halt event select register
   type HALT_Register is record
      --  If bit n is one, event n sets the HALT_L bit in the CTRL register
      --  (event 0 = bit 0, event 1 = bit 1, etc.). The number of bits = number
      --  of events in this SCT.
      HALTMSK_L : HALT_HALTMSK_L_Field := 16#0#;
      --  If bit n is one, event n sets the HALT_H bit in the CTRL register
      --  (event 0 = bit 16, event 1 = bit 17, etc.). The number of bits =
      --  number of events in this SCT.
      HALTMSK_H : HALT_HALTMSK_H_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HALT_Register use record
      HALTMSK_L at 0 range 0 .. 15;
      HALTMSK_H at 0 range 16 .. 31;
   end record;

   subtype STOP_STOPMSK_L_Field is HAL.UInt16;
   subtype STOP_STOPMSK_H_Field is HAL.UInt16;

   --  SCT stop event select register
   type STOP_Register is record
      --  If bit n is one, event n sets the STOP_L bit in the CTRL register
      --  (event 0 = bit 0, event 1 = bit 1, etc.). The number of bits = number
      --  of events in this SCT.
      STOPMSK_L : STOP_STOPMSK_L_Field := 16#0#;
      --  If bit n is one, event n sets the STOP_H bit in the CTRL register
      --  (event 0 = bit 16, event 1 = bit 17, etc.). The number of bits =
      --  number of events in this SCT.
      STOPMSK_H : STOP_STOPMSK_H_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for STOP_Register use record
      STOPMSK_L at 0 range 0 .. 15;
      STOPMSK_H at 0 range 16 .. 31;
   end record;

   subtype START_STARTMSK_L_Field is HAL.UInt16;
   subtype START_STARTMSK_H_Field is HAL.UInt16;

   --  SCT start event select register
   type START_Register is record
      --  If bit n is one, event n clears the STOP_L bit in the CTRL register
      --  (event 0 = bit 0, event 1 = bit 1, etc.). The number of bits = number
      --  of events in this SCT.
      STARTMSK_L : START_STARTMSK_L_Field := 16#0#;
      --  If bit n is one, event n clears the STOP_H bit in the CTRL register
      --  (event 0 = bit 16, event 1 = bit 17, etc.). The number of bits =
      --  number of events in this SCT.
      STARTMSK_H : START_STARTMSK_H_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for START_Register use record
      STARTMSK_L at 0 range 0 .. 15;
      STARTMSK_H at 0 range 16 .. 31;
   end record;

   subtype COUNT_CTR_L_Field is HAL.UInt16;
   subtype COUNT_CTR_H_Field is HAL.UInt16;

   --  SCT counter register
   type COUNT_Register is record
      --  When UNIFY = 0, read or write the 16-bit L counter value. When UNIFY
      --  = 1, read or write the lower 16 bits of the 32-bit unified counter.
      CTR_L : COUNT_CTR_L_Field := 16#0#;
      --  When UNIFY = 0, read or write the 16-bit H counter value. When UNIFY
      --  = 1, read or write the upper 16 bits of the 32-bit unified counter.
      CTR_H : COUNT_CTR_H_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for COUNT_Register use record
      CTR_L at 0 range 0 .. 15;
      CTR_H at 0 range 16 .. 31;
   end record;

   subtype STATE_STATE_L_Field is HAL.UInt5;
   subtype STATE_STATE_H_Field is HAL.UInt5;

   --  SCT state register
   type STATE_Register is record
      --  State variable.
      STATE_L        : STATE_STATE_L_Field := 16#0#;
      --  unspecified
      Reserved_5_15  : HAL.UInt11 := 16#0#;
      --  State variable.
      STATE_H        : STATE_STATE_H_Field := 16#0#;
      --  unspecified
      Reserved_21_31 : HAL.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for STATE_Register use record
      STATE_L        at 0 range 0 .. 4;
      Reserved_5_15  at 0 range 5 .. 15;
      STATE_H        at 0 range 16 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   --  INPUT_AIN array
   type INPUT_AIN_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for INPUT_AIN
   type INPUT_AIN_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AIN as a value
            Val : HAL.UInt16;
         when True =>
            --  AIN as an array
            Arr : INPUT_AIN_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for INPUT_AIN_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  INPUT_SIN array
   type INPUT_SIN_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for INPUT_SIN
   type INPUT_SIN_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SIN as a value
            Val : HAL.UInt16;
         when True =>
            --  SIN as an array
            Arr : INPUT_SIN_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for INPUT_SIN_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  SCT input register
   type INPUT_Register is record
      --  Read-only. Input 0 state. Input 0 state on the last SCT clock edge.
      AIN : INPUT_AIN_Field;
      --  Read-only. Input 0 state. Input 0 state following the synchronization
      --  specified by INSYNC.
      SIN : INPUT_SIN_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INPUT_Register use record
      AIN at 0 range 0 .. 15;
      SIN at 0 range 16 .. 31;
   end record;

   subtype REGMODE_REGMOD_L_Field is HAL.UInt16;
   subtype REGMODE_REGMOD_H_Field is HAL.UInt16;

   --  SCT match/capture mode register
   type REGMODE_Register is record
      --  Each bit controls one match/capture register (register 0 = bit 0,
      --  register 1 = bit 1, etc.). The number of bits = number of
      --  match/captures in this SCT. 0 = register operates as match register.
      --  1 = register operates as capture register.
      REGMOD_L : REGMODE_REGMOD_L_Field := 16#0#;
      --  Each bit controls one match/capture register (register 0 = bit 16,
      --  register 1 = bit 17, etc.). The number of bits = number of
      --  match/captures in this SCT. 0 = register operates as match registers.
      --  1 = register operates as capture registers.
      REGMOD_H : REGMODE_REGMOD_H_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for REGMODE_Register use record
      REGMOD_L at 0 range 0 .. 15;
      REGMOD_H at 0 range 16 .. 31;
   end record;

   subtype OUTPUT_OUT_Field is HAL.UInt16;

   --  SCT output register
   type OUTPUT_Register is record
      --  Writing a 1 to bit n forces the corresponding output HIGH. Writing a
      --  0 forces the corresponding output LOW (output 0 = bit 0, output 1 =
      --  bit 1, etc.). The number of bits = number of outputs in this SCT.
      OUT_k          : OUTPUT_OUT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OUTPUT_Register use record
      OUT_k          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  Set/clear operation on output 0. Value 0x3 is reserved. Do not program
   --  this value.
   type OUTPUTDIRCTRL_SETCLR0_Field is
     (
      --  Set and clear do not depend on the direction of any counter.
      Independent,
      --  Set and clear are reversed when counter L or the unified counter is
      --  counting down.
      L_Reversed,
      --  Set and clear are reversed when counter H is counting down. Do not
      --  use if UNIFY = 1.
      H_Reversed)
     with Size => 2;
   for OUTPUTDIRCTRL_SETCLR0_Field use
     (Independent => 0,
      L_Reversed => 1,
      H_Reversed => 2);

   --  OUTPUTDIRCTRL_SETCLR array
   type OUTPUTDIRCTRL_SETCLR_Field_Array is array (0 .. 15)
     of OUTPUTDIRCTRL_SETCLR0_Field
     with Component_Size => 2, Size => 32;

   --  SCT output counter direction control register
   type OUTPUTDIRCTRL_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SETCLR as a value
            Val : HAL.UInt32;
         when True =>
            --  SETCLR as an array
            Arr : OUTPUTDIRCTRL_SETCLR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for OUTPUTDIRCTRL_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  Effect of simultaneous set and clear on output 0.
   type RES_O0RES_Field is
     (
      --  No change.
      No_Change,
      --  Set output (or clear based on the SETCLR0 field in the OUTPUTDIRCTRL
      --  register).
      Set,
      --  Clear output (or set based on the SETCLR0 field).
      Clear,
      --  Toggle output.
      Toggle_Output)
     with Size => 2;
   for RES_O0RES_Field use
     (No_Change => 0,
      Set => 1,
      Clear => 2,
      Toggle_Output => 3);

   --  Effect of simultaneous set and clear on output 1.
   type RES_O1RES_Field is
     (
      --  No change.
      No_Change,
      --  Set output (or clear based on the SETCLR1 field in the OUTPUTDIRCTRL
      --  register).
      Set,
      --  Clear output (or set based on the SETCLR1 field).
      Clear,
      --  Toggle output.
      Toggle_Output)
     with Size => 2;
   for RES_O1RES_Field use
     (No_Change => 0,
      Set => 1,
      Clear => 2,
      Toggle_Output => 3);

   --  Effect of simultaneous set and clear on output 2.
   type RES_O2RES_Field is
     (
      --  No change.
      No_Change,
      --  Set output (or clear based on the SETCLR2 field in the OUTPUTDIRCTRL
      --  register).
      Set,
      --  Clear output n (or set based on the SETCLR2 field).
      Clear,
      --  Toggle output.
      Toggle_Output)
     with Size => 2;
   for RES_O2RES_Field use
     (No_Change => 0,
      Set => 1,
      Clear => 2,
      Toggle_Output => 3);

   --  Effect of simultaneous set and clear on output 3.
   type RES_O3RES_Field is
     (
      --  No change.
      No_Change,
      --  Set output (or clear based on the SETCLR3 field in the OUTPUTDIRCTRL
      --  register).
      Set,
      --  Clear output (or set based on the SETCLR3 field).
      Clear,
      --  Toggle output.
      Toggle_Output)
     with Size => 2;
   for RES_O3RES_Field use
     (No_Change => 0,
      Set => 1,
      Clear => 2,
      Toggle_Output => 3);

   --  Effect of simultaneous set and clear on output 4.
   type RES_O4RES_Field is
     (
      --  No change.
      No_Change,
      --  Set output (or clear based on the SETCLR4 field in the OUTPUTDIRCTRL
      --  register).
      Set,
      --  Clear output (or set based on the SETCLR4 field).
      Clear,
      --  Toggle output.
      Toggle_Output)
     with Size => 2;
   for RES_O4RES_Field use
     (No_Change => 0,
      Set => 1,
      Clear => 2,
      Toggle_Output => 3);

   --  Effect of simultaneous set and clear on output 5.
   type RES_O5RES_Field is
     (
      --  No change.
      No_Change,
      --  Set output (or clear based on the SETCLR5 field in the OUTPUTDIRCTRL
      --  register).
      Set,
      --  Clear output (or set based on the SETCLR5 field).
      Clear,
      --  Toggle output.
      Toggle_Output)
     with Size => 2;
   for RES_O5RES_Field use
     (No_Change => 0,
      Set => 1,
      Clear => 2,
      Toggle_Output => 3);

   --  Effect of simultaneous set and clear on output 6.
   type RES_O6RES_Field is
     (
      --  No change.
      No_Change,
      --  Set output (or clear based on the SETCLR6 field in the OUTPUTDIRCTRL
      --  register).
      Set,
      --  Clear output (or set based on the SETCLR6 field).
      Clear,
      --  Toggle output.
      Toggle_Output)
     with Size => 2;
   for RES_O6RES_Field use
     (No_Change => 0,
      Set => 1,
      Clear => 2,
      Toggle_Output => 3);

   --  Effect of simultaneous set and clear on output 7.
   type RES_O7RES_Field is
     (
      --  No change.
      No_Change,
      --  Set output (or clear based on the SETCLR7 field in the OUTPUTDIRCTRL
      --  register).
      Set,
      --  Clear output n (or set based on the SETCLR7 field).
      Clear,
      --  Toggle output.
      Toggle_Output)
     with Size => 2;
   for RES_O7RES_Field use
     (No_Change => 0,
      Set => 1,
      Clear => 2,
      Toggle_Output => 3);

   --  Effect of simultaneous set and clear on output 8.
   type RES_O8RES_Field is
     (
      --  No change.
      No_Change,
      --  Set output (or clear based on the SETCLR8 field in the OUTPUTDIRCTRL
      --  register).
      Set,
      --  Clear output (or set based on the SETCLR8 field).
      Clear,
      --  Toggle output.
      Toggle_Output)
     with Size => 2;
   for RES_O8RES_Field use
     (No_Change => 0,
      Set => 1,
      Clear => 2,
      Toggle_Output => 3);

   --  Effect of simultaneous set and clear on output 9.
   type RES_O9RES_Field is
     (
      --  No change.
      No_Change,
      --  Set output (or clear based on the SETCLR9 field in the OUTPUTDIRCTRL
      --  register).
      Set,
      --  Clear output (or set based on the SETCLR9 field).
      Clear,
      --  Toggle output.
      Toggle_Output)
     with Size => 2;
   for RES_O9RES_Field use
     (No_Change => 0,
      Set => 1,
      Clear => 2,
      Toggle_Output => 3);

   --  Effect of simultaneous set and clear on output 10.
   type RES_O10RES_Field is
     (
      --  No change.
      No_Change,
      --  Set output (or clear based on the SETCLR10 field in the OUTPUTDIRCTRL
      --  register).
      Set,
      --  Clear output (or set based on the SETCLR10 field).
      Clear,
      --  Toggle output.
      Toggle_Output)
     with Size => 2;
   for RES_O10RES_Field use
     (No_Change => 0,
      Set => 1,
      Clear => 2,
      Toggle_Output => 3);

   --  Effect of simultaneous set and clear on output 11.
   type RES_O11RES_Field is
     (
      --  No change.
      No_Change,
      --  Set output (or clear based on the SETCLR11 field in the OUTPUTDIRCTRL
      --  register).
      Set,
      --  Clear output (or set based on the SETCLR11 field).
      Clear,
      --  Toggle output.
      Toggle_Output)
     with Size => 2;
   for RES_O11RES_Field use
     (No_Change => 0,
      Set => 1,
      Clear => 2,
      Toggle_Output => 3);

   --  Effect of simultaneous set and clear on output 12.
   type RES_O12RES_Field is
     (
      --  No change.
      No_Change,
      --  Set output (or clear based on the SETCLR12 field in the OUTPUTDIRCTRL
      --  register).
      Set,
      --  Clear output (or set based on the SETCLR12 field).
      Clear,
      --  Toggle output.
      Toggle_Output)
     with Size => 2;
   for RES_O12RES_Field use
     (No_Change => 0,
      Set => 1,
      Clear => 2,
      Toggle_Output => 3);

   --  Effect of simultaneous set and clear on output 13.
   type RES_O13RES_Field is
     (
      --  No change.
      No_Change,
      --  Set output (or clear based on the SETCLR13 field in the OUTPUTDIRCTRL
      --  register).
      Set,
      --  Clear output (or set based on the SETCLR13 field).
      Clear,
      --  Toggle output.
      Toggle_Output)
     with Size => 2;
   for RES_O13RES_Field use
     (No_Change => 0,
      Set => 1,
      Clear => 2,
      Toggle_Output => 3);

   --  Effect of simultaneous set and clear on output 14.
   type RES_O14RES_Field is
     (
      --  No change.
      No_Change,
      --  Set output (or clear based on the SETCLR14 field in the OUTPUTDIRCTRL
      --  register).
      Set,
      --  Clear output (or set based on the SETCLR14 field).
      Clear,
      --  Toggle output.
      Toggle_Output)
     with Size => 2;
   for RES_O14RES_Field use
     (No_Change => 0,
      Set => 1,
      Clear => 2,
      Toggle_Output => 3);

   --  Effect of simultaneous set and clear on output 15.
   type RES_O15RES_Field is
     (
      --  No change.
      No_Change,
      --  Set output (or clear based on the SETCLR15 field in the OUTPUTDIRCTRL
      --  register).
      Set,
      --  Clear output (or set based on the SETCLR15 field).
      Clear,
      --  Toggle output.
      Toggle_Output)
     with Size => 2;
   for RES_O15RES_Field use
     (No_Change => 0,
      Set => 1,
      Clear => 2,
      Toggle_Output => 3);

   --  SCT conflict resolution register
   type RES_Register is record
      --  Effect of simultaneous set and clear on output 0.
      O0RES  : RES_O0RES_Field := NXP_SVD.SCT.No_Change;
      --  Effect of simultaneous set and clear on output 1.
      O1RES  : RES_O1RES_Field := NXP_SVD.SCT.No_Change;
      --  Effect of simultaneous set and clear on output 2.
      O2RES  : RES_O2RES_Field := NXP_SVD.SCT.No_Change;
      --  Effect of simultaneous set and clear on output 3.
      O3RES  : RES_O3RES_Field := NXP_SVD.SCT.No_Change;
      --  Effect of simultaneous set and clear on output 4.
      O4RES  : RES_O4RES_Field := NXP_SVD.SCT.No_Change;
      --  Effect of simultaneous set and clear on output 5.
      O5RES  : RES_O5RES_Field := NXP_SVD.SCT.No_Change;
      --  Effect of simultaneous set and clear on output 6.
      O6RES  : RES_O6RES_Field := NXP_SVD.SCT.No_Change;
      --  Effect of simultaneous set and clear on output 7.
      O7RES  : RES_O7RES_Field := NXP_SVD.SCT.No_Change;
      --  Effect of simultaneous set and clear on output 8.
      O8RES  : RES_O8RES_Field := NXP_SVD.SCT.No_Change;
      --  Effect of simultaneous set and clear on output 9.
      O9RES  : RES_O9RES_Field := NXP_SVD.SCT.No_Change;
      --  Effect of simultaneous set and clear on output 10.
      O10RES : RES_O10RES_Field := NXP_SVD.SCT.No_Change;
      --  Effect of simultaneous set and clear on output 11.
      O11RES : RES_O11RES_Field := NXP_SVD.SCT.No_Change;
      --  Effect of simultaneous set and clear on output 12.
      O12RES : RES_O12RES_Field := NXP_SVD.SCT.No_Change;
      --  Effect of simultaneous set and clear on output 13.
      O13RES : RES_O13RES_Field := NXP_SVD.SCT.No_Change;
      --  Effect of simultaneous set and clear on output 14.
      O14RES : RES_O14RES_Field := NXP_SVD.SCT.No_Change;
      --  Effect of simultaneous set and clear on output 15.
      O15RES : RES_O15RES_Field := NXP_SVD.SCT.No_Change;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RES_Register use record
      O0RES  at 0 range 0 .. 1;
      O1RES  at 0 range 2 .. 3;
      O2RES  at 0 range 4 .. 5;
      O3RES  at 0 range 6 .. 7;
      O4RES  at 0 range 8 .. 9;
      O5RES  at 0 range 10 .. 11;
      O6RES  at 0 range 12 .. 13;
      O7RES  at 0 range 14 .. 15;
      O8RES  at 0 range 16 .. 17;
      O9RES  at 0 range 18 .. 19;
      O10RES at 0 range 20 .. 21;
      O11RES at 0 range 22 .. 23;
      O12RES at 0 range 24 .. 25;
      O13RES at 0 range 26 .. 27;
      O14RES at 0 range 28 .. 29;
      O15RES at 0 range 30 .. 31;
   end record;

   subtype DMAREQ0_DEV_0_Field is HAL.UInt16;

   --  SCT DMA request 0 register
   type DMAREQ0_Register is record
      --  If bit n is one, event n triggers DMA request 0 (event 0 = bit 0,
      --  event 1 = bit 1, etc.). The number of bits = number of events in this
      --  SCT.
      DEV_0          : DMAREQ0_DEV_0_Field := 16#0#;
      --  unspecified
      Reserved_16_29 : HAL.UInt14 := 16#0#;
      --  A 1 in this bit triggers DMA request 0 when it loads the
      --  MATCH_L/Unified registers from the RELOAD_L/Unified registers.
      DRL0           : Boolean := False;
      --  This read-only bit indicates the state of DMA Request 0. Note that if
      --  the related DMA channel is enabled and properly set up, it is
      --  unlikely that software will see this flag, it will be cleared rapidly
      --  by the DMA service. The flag remaining set could point to an issue
      --  with DMA setup.
      DRQ0           : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAREQ0_Register use record
      DEV_0          at 0 range 0 .. 15;
      Reserved_16_29 at 0 range 16 .. 29;
      DRL0           at 0 range 30 .. 30;
      DRQ0           at 0 range 31 .. 31;
   end record;

   subtype DMAREQ1_DEV_1_Field is HAL.UInt16;

   --  SCT DMA request 1 register
   type DMAREQ1_Register is record
      --  If bit n is one, event n triggers DMA request 1 (event 0 = bit 0,
      --  event 1 = bit 1, etc.). The number of bits = number of events in this
      --  SCT.
      DEV_1          : DMAREQ1_DEV_1_Field := 16#0#;
      --  unspecified
      Reserved_16_29 : HAL.UInt14 := 16#0#;
      --  A 1 in this bit triggers DMA request 1 when it loads the Match
      --  L/Unified registers from the Reload L/Unified registers.
      DRL1           : Boolean := False;
      --  This read-only bit indicates the state of DMA Request 1. Note that if
      --  the related DMA channel is enabled and properly set up, it is
      --  unlikely that software will see this flag, it will be cleared rapidly
      --  by the DMA service. The flag remaining set could point to an issue
      --  with DMA setup.
      DRQ1           : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAREQ1_Register use record
      DEV_1          at 0 range 0 .. 15;
      Reserved_16_29 at 0 range 16 .. 29;
      DRL1           at 0 range 30 .. 30;
      DRQ1           at 0 range 31 .. 31;
   end record;

   subtype EVEN_IEN_Field is HAL.UInt16;

   --  SCT event interrupt enable register
   type EVEN_Register is record
      --  The SCT requests an interrupt when bit n of this register and the
      --  event flag register are both one (event 0 = bit 0, event 1 = bit 1,
      --  etc.). The number of bits = number of events in this SCT.
      IEN            : EVEN_IEN_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EVEN_Register use record
      IEN            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype EVFLAG_FLAG_Field is HAL.UInt16;

   --  SCT event flag register
   type EVFLAG_Register is record
      --  Bit n is one if event n has occurred since reset or a 1 was last
      --  written to this bit (event 0 = bit 0, event 1 = bit 1, etc.). The
      --  number of bits = number of events in this SCT.
      FLAG           : EVFLAG_FLAG_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EVFLAG_Register use record
      FLAG           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CONEN_NCEN_Field is HAL.UInt16;

   --  SCT conflict interrupt enable register
   type CONEN_Register is record
      --  The SCT requests an interrupt when bit n of this register and the SCT
      --  conflict flag register are both one (output 0 = bit 0, output 1 = bit
      --  1, etc.). The number of bits = number of outputs in this SCT.
      NCEN           : CONEN_NCEN_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CONEN_Register use record
      NCEN           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CONFLAG_NCFLAG_Field is HAL.UInt16;

   --  SCT conflict flag register
   type CONFLAG_Register is record
      --  Bit n is one if a no-change conflict event occurred on output n since
      --  reset or a 1 was last written to this bit (output 0 = bit 0, output 1
      --  = bit 1, etc.). The number of bits = number of outputs in this SCT.
      NCFLAG         : CONFLAG_NCFLAG_Field := 16#0#;
      --  unspecified
      Reserved_16_29 : HAL.UInt14 := 16#0#;
      --  The most recent bus error from this SCT involved writing CTR
      --  L/Unified, STATE L/Unified, MATCH L/Unified, or the Output register
      --  when the L/U counter was not halted. A word write to certain L and H
      --  registers can be half successful and half unsuccessful.
      BUSERRL        : Boolean := False;
      --  The most recent bus error from this SCT involved writing CTR H, STATE
      --  H, MATCH H, or the Output register when the H counter was not halted.
      BUSERRH        : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CONFLAG_Register use record
      NCFLAG         at 0 range 0 .. 15;
      Reserved_16_29 at 0 range 16 .. 29;
      BUSERRL        at 0 range 30 .. 30;
      BUSERRH        at 0 range 31 .. 31;
   end record;

   subtype CAP_CAPn_L_Field is HAL.UInt16;
   subtype CAP_CAPn_H_Field is HAL.UInt16;

   --  SCT capture register of capture channel
   type CAP_Register is record
      --  When UNIFY = 0, read the 16-bit counter value at which this register
      --  was last captured. When UNIFY = 1, read the lower 16 bits of the
      --  32-bit value at which this register was last captured.
      CAPn_L : CAP_CAPn_L_Field := 16#0#;
      --  When UNIFY = 0, read the 16-bit counter value at which this register
      --  was last captured. When UNIFY = 1, read the upper 16 bits of the
      --  32-bit value at which this register was last captured.
      CAPn_H : CAP_CAPn_H_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CAP_Register use record
      CAPn_L at 0 range 0 .. 15;
      CAPn_H at 0 range 16 .. 31;
   end record;

   subtype MATCH_MATCHn_L_Field is HAL.UInt16;
   subtype MATCH_MATCHn_H_Field is HAL.UInt16;

   --  SCT match value register of match channels
   type MATCH_Register is record
      --  When UNIFY = 0, read or write the 16-bit value to be compared to the
      --  L counter. When UNIFY = 1, read or write the lower 16 bits of the
      --  32-bit value to be compared to the unified counter.
      MATCHn_L : MATCH_MATCHn_L_Field := 16#0#;
      --  When UNIFY = 0, read or write the 16-bit value to be compared to the
      --  H counter. When UNIFY = 1, read or write the upper 16 bits of the
      --  32-bit value to be compared to the unified counter.
      MATCHn_H : MATCH_MATCHn_H_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MATCH_Register use record
      MATCHn_L at 0 range 0 .. 15;
      MATCHn_H at 0 range 16 .. 31;
   end record;

   subtype CAPCTRL_CAPCONn_L_Field is HAL.UInt16;
   subtype CAPCTRL_CAPCONn_H_Field is HAL.UInt16;

   --  SCT capture control register
   type CAPCTRL_Register is record
      --  If bit m is one, event m causes the CAPn_L (UNIFY = 0) or the CAPn
      --  (UNIFY = 1) register to be loaded (event 0 = bit 0, event 1 = bit 1,
      --  etc.). The number of bits = number of match/captures in this SCT.
      CAPCONn_L : CAPCTRL_CAPCONn_L_Field := 16#0#;
      --  If bit m is one, event m causes the CAPn_H (UNIFY = 0) register to be
      --  loaded (event 0 = bit 16, event 1 = bit 17, etc.). The number of bits
      --  = number of match/captures in this SCT.
      CAPCONn_H : CAPCTRL_CAPCONn_H_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CAPCTRL_Register use record
      CAPCONn_L at 0 range 0 .. 15;
      CAPCONn_H at 0 range 16 .. 31;
   end record;

   subtype MATCHREL_RELOADn_L_Field is HAL.UInt16;
   subtype MATCHREL_RELOADn_H_Field is HAL.UInt16;

   --  SCT match reload value register
   type MATCHREL_Register is record
      --  When UNIFY = 0, specifies the 16-bit value to be loaded into the
      --  MATCHn_L register. When UNIFY = 1, specifies the lower 16 bits of the
      --  32-bit value to be loaded into the MATCHn register.
      RELOADn_L : MATCHREL_RELOADn_L_Field := 16#0#;
      --  When UNIFY = 0, specifies the 16-bit to be loaded into the MATCHn_H
      --  register. When UNIFY = 1, specifies the upper 16 bits of the 32-bit
      --  value to be loaded into the MATCHn register.
      RELOADn_H : MATCHREL_RELOADn_H_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MATCHREL_Register use record
      RELOADn_L at 0 range 0 .. 15;
      RELOADn_H at 0 range 16 .. 31;
   end record;

   ----------------------------
   -- EV cluster's Registers --
   ----------------------------

   subtype EV_STATE_EV_STATEMSKn_Field is HAL.UInt16;

   --  SCT event state register 0
   type EV_STATE_EV_Register is record
      --  If bit m is one, event n happens in state m of the counter selected
      --  by the HEVENT bit (n = event number, m = state number; state 0 = bit
      --  0, state 1= bit 1, etc.). The number of bits = number of states in
      --  this SCT.
      STATEMSKn      : EV_STATE_EV_STATEMSKn_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EV_STATE_EV_Register use record
      STATEMSKn      at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype EV_CTRL_EV_MATCHSEL_Field is HAL.UInt4;

   --  Select L/H counter. Do not set this bit if UNIFY = 1.
   type EV_CTRL_HEVENT_Field is
     (
      --  Selects the L state and the L match register selected by MATCHSEL.
      L_Counter,
      --  Selects the H state and the H match register selected by MATCHSEL.
      H_Counter)
     with Size => 1;
   for EV_CTRL_HEVENT_Field use
     (L_Counter => 0,
      H_Counter => 1);

   --  Input/output select
   type EV_CTRL_OUTSEL_Field is
     (
      --  Selects the inputs selected by IOSEL.
      Input,
      --  Selects the outputs selected by IOSEL.
      Output)
     with Size => 1;
   for EV_CTRL_OUTSEL_Field use
     (Input => 0,
      Output => 1);

   subtype EV_CTRL_EV_IOSEL_Field is HAL.UInt4;

   --  Selects the I/O condition for event n. (The detection of edges on
   --  outputs lag the conditions that switch the outputs by one SCT clock). In
   --  order to guarantee proper edge/state detection, an input must have a
   --  minimum pulse width of at least one SCT clock period .
   type EV_CTRL_IOCOND_Field is
     (
      --  LOW
      Low,
      --  Rise
      Rise,
      --  Fall
      Fall,
      --  HIGH
      High)
     with Size => 2;
   for EV_CTRL_IOCOND_Field use
     (Low => 0,
      Rise => 1,
      Fall => 2,
      High => 3);

   --  Selects how the specified match and I/O condition are used and combined.
   type EV_CTRL_COMBMODE_Field is
     (
      --  OR. The event occurs when either the specified match or I/O condition
      --  occurs.
      Or_k,
      --  MATCH. Uses the specified match only.
      Match,
      --  IO. Uses the specified I/O condition only.
      Io,
      --  AND. The event occurs when the specified match and I/O condition
      --  occur simultaneously.
      And_k)
     with Size => 2;
   for EV_CTRL_COMBMODE_Field use
     (Or_k => 0,
      Match => 1,
      Io => 2,
      And_k => 3);

   --  This bit controls how the STATEV value modifies the state selected by
   --  HEVENT when this event is the highest-numbered event occurring for that
   --  state.
   type EV_CTRL_STATELD_Field is
     (
      --  STATEV value is added into STATE (the carry-out is ignored).
      Add,
      --  STATEV value is loaded into STATE.
      Load)
     with Size => 1;
   for EV_CTRL_STATELD_Field use
     (Add => 0,
      Load => 1);

   subtype EV_CTRL_EV_STATEV_Field is HAL.UInt5;

   --  Direction qualifier for event generation. This field only applies when
   --  the counters are operating in BIDIR mode. If BIDIR = 0, the SCT ignores
   --  this field. Value 0x3 is reserved.
   type EV_CTRL_DIRECTION_Field is
     (
      --  Direction independent. This event is triggered regardless of the
      --  count direction.
      Direction_Independent,
      --  Counting up. This event is triggered only during up-counting when
      --  BIDIR = 1.
      Counting_Up,
      --  Counting down. This event is triggered only during down-counting when
      --  BIDIR = 1.
      Counting_Down)
     with Size => 2;
   for EV_CTRL_DIRECTION_Field use
     (Direction_Independent => 0,
      Counting_Up => 1,
      Counting_Down => 2);

   --  SCT event control register 0
   type EV_CTRL_EV_Register is record
      --  Selects the Match register associated with this event (if any). A
      --  match can occur only when the counter selected by the HEVENT bit is
      --  running.
      MATCHSEL       : EV_CTRL_EV_MATCHSEL_Field := 16#0#;
      --  Select L/H counter. Do not set this bit if UNIFY = 1.
      HEVENT         : EV_CTRL_HEVENT_Field := NXP_SVD.SCT.L_Counter;
      --  Input/output select
      OUTSEL         : EV_CTRL_OUTSEL_Field := NXP_SVD.SCT.Input;
      --  Selects the input or output signal number associated with this event
      --  (if any). Do not select an input in this register if CKMODE is 1x. In
      --  this case the clock input is an implicit ingredient of every event.
      IOSEL          : EV_CTRL_EV_IOSEL_Field := 16#0#;
      --  Selects the I/O condition for event n. (The detection of edges on
      --  outputs lag the conditions that switch the outputs by one SCT clock).
      --  In order to guarantee proper edge/state detection, an input must have
      --  a minimum pulse width of at least one SCT clock period .
      IOCOND         : EV_CTRL_IOCOND_Field := NXP_SVD.SCT.Low;
      --  Selects how the specified match and I/O condition are used and
      --  combined.
      COMBMODE       : EV_CTRL_COMBMODE_Field := NXP_SVD.SCT.Or_k;
      --  This bit controls how the STATEV value modifies the state selected by
      --  HEVENT when this event is the highest-numbered event occurring for
      --  that state.
      STATELD        : EV_CTRL_STATELD_Field := NXP_SVD.SCT.Add;
      --  This value is loaded into or added to the state selected by HEVENT,
      --  depending on STATELD, when this event is the highest-numbered event
      --  occurring for that state. If STATELD and STATEV are both zero, there
      --  is no change to the STATE value.
      STATEV         : EV_CTRL_EV_STATEV_Field := 16#0#;
      --  If this bit is one and the COMBMODE field specifies a match component
      --  to the triggering of this event, then a match is considered to be
      --  active whenever the counter value is GREATER THAN OR EQUAL TO the
      --  value specified in the match register when counting up, LESS THEN OR
      --  EQUAL TO the match value when counting down. If this bit is zero, a
      --  match is only be active during the cycle when the counter is equal to
      --  the match value.
      MATCHMEM       : Boolean := False;
      --  Direction qualifier for event generation. This field only applies
      --  when the counters are operating in BIDIR mode. If BIDIR = 0, the SCT
      --  ignores this field. Value 0x3 is reserved.
      DIRECTION      : EV_CTRL_DIRECTION_Field :=
                        NXP_SVD.SCT.Direction_Independent;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EV_CTRL_EV_Register use record
      MATCHSEL       at 0 range 0 .. 3;
      HEVENT         at 0 range 4 .. 4;
      OUTSEL         at 0 range 5 .. 5;
      IOSEL          at 0 range 6 .. 9;
      IOCOND         at 0 range 10 .. 11;
      COMBMODE       at 0 range 12 .. 13;
      STATELD        at 0 range 14 .. 14;
      STATEV         at 0 range 15 .. 19;
      MATCHMEM       at 0 range 20 .. 20;
      DIRECTION      at 0 range 21 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  no description available
   type EV_Cluster is record
      --  SCT event state register 0
      EV_STATE : aliased EV_STATE_EV_Register;
      --  SCT event control register 0
      EV_CTRL  : aliased EV_CTRL_EV_Register;
   end record
     with Volatile, Size => 64;

   for EV_Cluster use record
      EV_STATE at 16#0# range 0 .. 31;
      EV_CTRL  at 16#4# range 0 .. 31;
   end record;

   --  no description available
   type EV_Clusters is array (0 .. 15) of EV_Cluster;

   -----------------------------
   -- OUT cluster's Registers --
   -----------------------------

   subtype OUT_SET_OUT_SET_Field is HAL.UInt16;

   --  SCT output 0 set register
   type OUT_SET_OUT_Register is record
      --  A 1 in bit m selects event m to set output n (or clear it if SETCLRn
      --  = 0x1 or 0x2) output 0 = bit 0, output 1 = bit 1, etc. The number of
      --  bits = number of events in this SCT. When the counter is used in
      --  bi-directional mode, it is possible to reverse the action specified
      --  by the output set and clear registers when counting down, See the
      --  OUTPUTCTRL register.
      SET            : OUT_SET_OUT_SET_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OUT_SET_OUT_Register use record
      SET            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype OUT_CLR_OUT_CLR_Field is HAL.UInt16;

   --  SCT output 0 clear register
   type OUT_CLR_OUT_Register is record
      --  A 1 in bit m selects event m to clear output n (or set it if SETCLRn
      --  = 0x1 or 0x2) event 0 = bit 0, event 1 = bit 1, etc. The number of
      --  bits = number of events in this SCT. When the counter is used in
      --  bi-directional mode, it is possible to reverse the action specified
      --  by the output set and clear registers when counting down, See the
      --  OUTPUTCTRL register.
      CLR            : OUT_CLR_OUT_CLR_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OUT_CLR_OUT_Register use record
      CLR            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  no description available
   type OUT_Cluster is record
      --  SCT output 0 set register
      OUT_SET : aliased OUT_SET_OUT_Register;
      --  SCT output 0 clear register
      OUT_CLR : aliased OUT_CLR_OUT_Register;
   end record
     with Volatile, Size => 64;

   for OUT_Cluster use record
      OUT_SET at 16#0# range 0 .. 31;
      OUT_CLR at 16#4# range 0 .. 31;
   end record;

   --  no description available
   type OUT_Clusters is array (0 .. 9) of OUT_Cluster;

   -----------------
   -- Peripherals --
   -----------------

   type SCT0_Disc is
     (
      Mode_1,
      Mode_2);

   --  SCTimer/PWM (SCT)
   type SCT0_Peripheral
     (Discriminent : SCT0_Disc := Mode_1)
   is record
      --  SCT configuration register
      CONFIG        : aliased CONFIG_Register;
      --  SCT control register
      CTRL          : aliased CTRL_Register;
      --  SCT limit event select register
      LIMIT         : aliased LIMIT_Register;
      --  SCT halt event select register
      HALT          : aliased HALT_Register;
      --  SCT stop event select register
      STOP          : aliased STOP_Register;
      --  SCT start event select register
      START         : aliased START_Register;
      --  SCT counter register
      COUNT         : aliased COUNT_Register;
      --  SCT state register
      STATE         : aliased STATE_Register;
      --  SCT input register
      INPUT         : aliased INPUT_Register;
      --  SCT match/capture mode register
      REGMODE       : aliased REGMODE_Register;
      --  SCT output register
      OUTPUT        : aliased OUTPUT_Register;
      --  SCT output counter direction control register
      OUTPUTDIRCTRL : aliased OUTPUTDIRCTRL_Register;
      --  SCT conflict resolution register
      RES           : aliased RES_Register;
      --  SCT DMA request 0 register
      DMAREQ0       : aliased DMAREQ0_Register;
      --  SCT DMA request 1 register
      DMAREQ1       : aliased DMAREQ1_Register;
      --  SCT event interrupt enable register
      EVEN          : aliased EVEN_Register;
      --  SCT event flag register
      EVFLAG        : aliased EVFLAG_Register;
      --  SCT conflict interrupt enable register
      CONEN         : aliased CONEN_Register;
      --  SCT conflict flag register
      CONFLAG       : aliased CONFLAG_Register;
      --  no description available
      EV            : aliased EV_Clusters;
      --  no description available
      OUT_k         : aliased OUT_Clusters;
      case Discriminent is
         when Mode_1 =>
            --  SCT capture register of capture channel
            CAP0 : aliased CAP_Register;
            --  SCT capture register of capture channel
            CAP1 : aliased CAP_Register;
            --  SCT capture register of capture channel
            CAP2 : aliased CAP_Register;
            --  SCT capture register of capture channel
            CAP3 : aliased CAP_Register;
            --  SCT capture register of capture channel
            CAP4 : aliased CAP_Register;
            --  SCT capture register of capture channel
            CAP5 : aliased CAP_Register;
            --  SCT capture register of capture channel
            CAP6 : aliased CAP_Register;
            --  SCT capture register of capture channel
            CAP7 : aliased CAP_Register;
            --  SCT capture register of capture channel
            CAP8 : aliased CAP_Register;
            --  SCT capture register of capture channel
            CAP9 : aliased CAP_Register;
            --  SCT capture register of capture channel
            CAP10 : aliased CAP_Register;
            --  SCT capture register of capture channel
            CAP11 : aliased CAP_Register;
            --  SCT capture register of capture channel
            CAP12 : aliased CAP_Register;
            --  SCT capture register of capture channel
            CAP13 : aliased CAP_Register;
            --  SCT capture register of capture channel
            CAP14 : aliased CAP_Register;
            --  SCT capture register of capture channel
            CAP15 : aliased CAP_Register;
            --  SCT capture control register
            CAPCTRL0 : aliased CAPCTRL_Register;
            --  SCT capture control register
            CAPCTRL1 : aliased CAPCTRL_Register;
            --  SCT capture control register
            CAPCTRL2 : aliased CAPCTRL_Register;
            --  SCT capture control register
            CAPCTRL3 : aliased CAPCTRL_Register;
            --  SCT capture control register
            CAPCTRL4 : aliased CAPCTRL_Register;
            --  SCT capture control register
            CAPCTRL5 : aliased CAPCTRL_Register;
            --  SCT capture control register
            CAPCTRL6 : aliased CAPCTRL_Register;
            --  SCT capture control register
            CAPCTRL7 : aliased CAPCTRL_Register;
            --  SCT capture control register
            CAPCTRL8 : aliased CAPCTRL_Register;
            --  SCT capture control register
            CAPCTRL9 : aliased CAPCTRL_Register;
            --  SCT capture control register
            CAPCTRL10 : aliased CAPCTRL_Register;
            --  SCT capture control register
            CAPCTRL11 : aliased CAPCTRL_Register;
            --  SCT capture control register
            CAPCTRL12 : aliased CAPCTRL_Register;
            --  SCT capture control register
            CAPCTRL13 : aliased CAPCTRL_Register;
            --  SCT capture control register
            CAPCTRL14 : aliased CAPCTRL_Register;
            --  SCT capture control register
            CAPCTRL15 : aliased CAPCTRL_Register;
         when Mode_2 =>
            --  SCT match value register of match channels
            MATCH0 : aliased MATCH_Register;
            --  SCT match value register of match channels
            MATCH1 : aliased MATCH_Register;
            --  SCT match value register of match channels
            MATCH2 : aliased MATCH_Register;
            --  SCT match value register of match channels
            MATCH3 : aliased MATCH_Register;
            --  SCT match value register of match channels
            MATCH4 : aliased MATCH_Register;
            --  SCT match value register of match channels
            MATCH5 : aliased MATCH_Register;
            --  SCT match value register of match channels
            MATCH6 : aliased MATCH_Register;
            --  SCT match value register of match channels
            MATCH7 : aliased MATCH_Register;
            --  SCT match value register of match channels
            MATCH8 : aliased MATCH_Register;
            --  SCT match value register of match channels
            MATCH9 : aliased MATCH_Register;
            --  SCT match value register of match channels
            MATCH10 : aliased MATCH_Register;
            --  SCT match value register of match channels
            MATCH11 : aliased MATCH_Register;
            --  SCT match value register of match channels
            MATCH12 : aliased MATCH_Register;
            --  SCT match value register of match channels
            MATCH13 : aliased MATCH_Register;
            --  SCT match value register of match channels
            MATCH14 : aliased MATCH_Register;
            --  SCT match value register of match channels
            MATCH15 : aliased MATCH_Register;
            --  SCT match reload value register
            MATCHREL0 : aliased MATCHREL_Register;
            --  SCT match reload value register
            MATCHREL1 : aliased MATCHREL_Register;
            --  SCT match reload value register
            MATCHREL2 : aliased MATCHREL_Register;
            --  SCT match reload value register
            MATCHREL3 : aliased MATCHREL_Register;
            --  SCT match reload value register
            MATCHREL4 : aliased MATCHREL_Register;
            --  SCT match reload value register
            MATCHREL5 : aliased MATCHREL_Register;
            --  SCT match reload value register
            MATCHREL6 : aliased MATCHREL_Register;
            --  SCT match reload value register
            MATCHREL7 : aliased MATCHREL_Register;
            --  SCT match reload value register
            MATCHREL8 : aliased MATCHREL_Register;
            --  SCT match reload value register
            MATCHREL9 : aliased MATCHREL_Register;
            --  SCT match reload value register
            MATCHREL10 : aliased MATCHREL_Register;
            --  SCT match reload value register
            MATCHREL11 : aliased MATCHREL_Register;
            --  SCT match reload value register
            MATCHREL12 : aliased MATCHREL_Register;
            --  SCT match reload value register
            MATCHREL13 : aliased MATCHREL_Register;
            --  SCT match reload value register
            MATCHREL14 : aliased MATCHREL_Register;
            --  SCT match reload value register
            MATCHREL15 : aliased MATCHREL_Register;
      end case;
   end record
     with Unchecked_Union, Volatile;

   for SCT0_Peripheral use record
      CONFIG        at 16#0# range 0 .. 31;
      CTRL          at 16#4# range 0 .. 31;
      LIMIT         at 16#8# range 0 .. 31;
      HALT          at 16#C# range 0 .. 31;
      STOP          at 16#10# range 0 .. 31;
      START         at 16#14# range 0 .. 31;
      COUNT         at 16#40# range 0 .. 31;
      STATE         at 16#44# range 0 .. 31;
      INPUT         at 16#48# range 0 .. 31;
      REGMODE       at 16#4C# range 0 .. 31;
      OUTPUT        at 16#50# range 0 .. 31;
      OUTPUTDIRCTRL at 16#54# range 0 .. 31;
      RES           at 16#58# range 0 .. 31;
      DMAREQ0       at 16#5C# range 0 .. 31;
      DMAREQ1       at 16#60# range 0 .. 31;
      EVEN          at 16#F0# range 0 .. 31;
      EVFLAG        at 16#F4# range 0 .. 31;
      CONEN         at 16#F8# range 0 .. 31;
      CONFLAG       at 16#FC# range 0 .. 31;
      EV            at 16#300# range 0 .. 1023;
      OUT_k         at 16#500# range 0 .. 639;
      CAP0          at 16#100# range 0 .. 31;
      CAP1          at 16#104# range 0 .. 31;
      CAP2          at 16#108# range 0 .. 31;
      CAP3          at 16#10C# range 0 .. 31;
      CAP4          at 16#110# range 0 .. 31;
      CAP5          at 16#114# range 0 .. 31;
      CAP6          at 16#118# range 0 .. 31;
      CAP7          at 16#11C# range 0 .. 31;
      CAP8          at 16#120# range 0 .. 31;
      CAP9          at 16#124# range 0 .. 31;
      CAP10         at 16#128# range 0 .. 31;
      CAP11         at 16#12C# range 0 .. 31;
      CAP12         at 16#130# range 0 .. 31;
      CAP13         at 16#134# range 0 .. 31;
      CAP14         at 16#138# range 0 .. 31;
      CAP15         at 16#13C# range 0 .. 31;
      CAPCTRL0      at 16#200# range 0 .. 31;
      CAPCTRL1      at 16#204# range 0 .. 31;
      CAPCTRL2      at 16#208# range 0 .. 31;
      CAPCTRL3      at 16#20C# range 0 .. 31;
      CAPCTRL4      at 16#210# range 0 .. 31;
      CAPCTRL5      at 16#214# range 0 .. 31;
      CAPCTRL6      at 16#218# range 0 .. 31;
      CAPCTRL7      at 16#21C# range 0 .. 31;
      CAPCTRL8      at 16#220# range 0 .. 31;
      CAPCTRL9      at 16#224# range 0 .. 31;
      CAPCTRL10     at 16#228# range 0 .. 31;
      CAPCTRL11     at 16#22C# range 0 .. 31;
      CAPCTRL12     at 16#230# range 0 .. 31;
      CAPCTRL13     at 16#234# range 0 .. 31;
      CAPCTRL14     at 16#238# range 0 .. 31;
      CAPCTRL15     at 16#23C# range 0 .. 31;
      MATCH0        at 16#100# range 0 .. 31;
      MATCH1        at 16#104# range 0 .. 31;
      MATCH2        at 16#108# range 0 .. 31;
      MATCH3        at 16#10C# range 0 .. 31;
      MATCH4        at 16#110# range 0 .. 31;
      MATCH5        at 16#114# range 0 .. 31;
      MATCH6        at 16#118# range 0 .. 31;
      MATCH7        at 16#11C# range 0 .. 31;
      MATCH8        at 16#120# range 0 .. 31;
      MATCH9        at 16#124# range 0 .. 31;
      MATCH10       at 16#128# range 0 .. 31;
      MATCH11       at 16#12C# range 0 .. 31;
      MATCH12       at 16#130# range 0 .. 31;
      MATCH13       at 16#134# range 0 .. 31;
      MATCH14       at 16#138# range 0 .. 31;
      MATCH15       at 16#13C# range 0 .. 31;
      MATCHREL0     at 16#200# range 0 .. 31;
      MATCHREL1     at 16#204# range 0 .. 31;
      MATCHREL2     at 16#208# range 0 .. 31;
      MATCHREL3     at 16#20C# range 0 .. 31;
      MATCHREL4     at 16#210# range 0 .. 31;
      MATCHREL5     at 16#214# range 0 .. 31;
      MATCHREL6     at 16#218# range 0 .. 31;
      MATCHREL7     at 16#21C# range 0 .. 31;
      MATCHREL8     at 16#220# range 0 .. 31;
      MATCHREL9     at 16#224# range 0 .. 31;
      MATCHREL10    at 16#228# range 0 .. 31;
      MATCHREL11    at 16#22C# range 0 .. 31;
      MATCHREL12    at 16#230# range 0 .. 31;
      MATCHREL13    at 16#234# range 0 .. 31;
      MATCHREL14    at 16#238# range 0 .. 31;
      MATCHREL15    at 16#23C# range 0 .. 31;
   end record;

   --  SCTimer/PWM (SCT)
   SCT0_Periph : aliased SCT0_Peripheral
     with Import, Address => System'To_Address (16#40085000#);

end NXP_SVD.SCT;
