--  Copyright 2016-2019 NXP
--  All rights reserved.SPDX-License-Identifier: BSD-3-Clause


--  This spec has been automatically generated from LPC55S6x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NXP_SVD.CTIMER is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Interrupt Register. The IR can be written to clear interrupts. The IR
   --  can be read to identify which of eight possible interrupt sources are
   --  pending.
   type IR_Register is record
      --  Interrupt flag for match channel 0.
      MR0INT        : Boolean := False;
      --  Interrupt flag for match channel 1.
      MR1INT        : Boolean := False;
      --  Interrupt flag for match channel 2.
      MR2INT        : Boolean := False;
      --  Interrupt flag for match channel 3.
      MR3INT        : Boolean := False;
      --  Interrupt flag for capture channel 0 event.
      CR0INT        : Boolean := False;
      --  Interrupt flag for capture channel 1 event.
      CR1INT        : Boolean := False;
      --  Interrupt flag for capture channel 2 event.
      CR2INT        : Boolean := False;
      --  Interrupt flag for capture channel 3 event.
      CR3INT        : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IR_Register use record
      MR0INT        at 0 range 0 .. 0;
      MR1INT        at 0 range 1 .. 1;
      MR2INT        at 0 range 2 .. 2;
      MR3INT        at 0 range 3 .. 3;
      CR0INT        at 0 range 4 .. 4;
      CR1INT        at 0 range 5 .. 5;
      CR2INT        at 0 range 6 .. 6;
      CR3INT        at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  Counter enable.
   type TCR_CEN_Field is
     (
      --  Disabled.The counters are disabled.
      Disabled,
      --  Enabled. The Timer Counter and Prescale Counter are enabled.
      Enabled)
     with Size => 1;
   for TCR_CEN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Counter reset.
   type TCR_CRST_Field is
     (
      --  Disabled. Do nothing.
      Disabled,
      --  Enabled. The Timer Counter and the Prescale Counter are synchronously
      --  reset on the next positive edge of the APB bus clock. The counters
      --  remain reset until TCR[1] is returned to zero.
      Enabled)
     with Size => 1;
   for TCR_CRST_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Timer Control Register. The TCR is used to control the Timer Counter
   --  functions. The Timer Counter can be disabled or reset through the TCR.
   type TCR_Register is record
      --  Counter enable.
      CEN           : TCR_CEN_Field := NXP_SVD.CTIMER.Disabled;
      --  Counter reset.
      CRST          : TCR_CRST_Field := NXP_SVD.CTIMER.Disabled;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TCR_Register use record
      CEN           at 0 range 0 .. 0;
      CRST          at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  Match Control Register
   type MCR_Register is record
      --  Interrupt on MR0: an interrupt is generated when MR0 matches the
      --  value in the TC.
      MR0I           : Boolean := False;
      --  Reset on MR0: the TC will be reset if MR0 matches it.
      MR0R           : Boolean := False;
      --  Stop on MR0: the TC and PC will be stopped and TCR[0] will be set to
      --  0 if MR0 matches the TC.
      MR0S           : Boolean := False;
      --  Interrupt on MR1: an interrupt is generated when MR1 matches the
      --  value in the TC.
      MR1I           : Boolean := False;
      --  Reset on MR1: the TC will be reset if MR1 matches it.
      MR1R           : Boolean := False;
      --  Stop on MR1: the TC and PC will be stopped and TCR[0] will be set to
      --  0 if MR1 matches the TC.
      MR1S           : Boolean := False;
      --  Interrupt on MR2: an interrupt is generated when MR2 matches the
      --  value in the TC.
      MR2I           : Boolean := False;
      --  Reset on MR2: the TC will be reset if MR2 matches it.
      MR2R           : Boolean := False;
      --  Stop on MR2: the TC and PC will be stopped and TCR[0] will be set to
      --  0 if MR2 matches the TC.
      MR2S           : Boolean := False;
      --  Interrupt on MR3: an interrupt is generated when MR3 matches the
      --  value in the TC.
      MR3I           : Boolean := False;
      --  Reset on MR3: the TC will be reset if MR3 matches it.
      MR3R           : Boolean := False;
      --  Stop on MR3: the TC and PC will be stopped and TCR[0] will be set to
      --  0 if MR3 matches the TC.
      MR3S           : Boolean := False;
      --  unspecified
      Reserved_12_23 : HAL.UInt12 := 16#0#;
      --  Reload MR0 with the contents of the Match 0 Shadow Register when the
      --  TC is reset to zero (either via a match event or a write to bit 1 of
      --  the TCR).
      MR0RL          : Boolean := False;
      --  Reload MR1 with the contents of the Match 1 Shadow Register when the
      --  TC is reset to zero (either via a match event or a write to bit 1 of
      --  the TCR).
      MR1RL          : Boolean := False;
      --  Reload MR2 with the contents of the Match 2 Shadow Register when the
      --  TC is reset to zero (either via a match event or a write to bit 1 of
      --  the TCR).
      MR2RL          : Boolean := False;
      --  Reload MR3 with the contents of the Match 3 Shadow Register when the
      --  TC is reset to zero (either via a match event or a write to bit 1 of
      --  the TCR).
      MR3RL          : Boolean := False;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MCR_Register use record
      MR0I           at 0 range 0 .. 0;
      MR0R           at 0 range 1 .. 1;
      MR0S           at 0 range 2 .. 2;
      MR1I           at 0 range 3 .. 3;
      MR1R           at 0 range 4 .. 4;
      MR1S           at 0 range 5 .. 5;
      MR2I           at 0 range 6 .. 6;
      MR2R           at 0 range 7 .. 7;
      MR2S           at 0 range 8 .. 8;
      MR3I           at 0 range 9 .. 9;
      MR3R           at 0 range 10 .. 10;
      MR3S           at 0 range 11 .. 11;
      Reserved_12_23 at 0 range 12 .. 23;
      MR0RL          at 0 range 24 .. 24;
      MR1RL          at 0 range 25 .. 25;
      MR2RL          at 0 range 26 .. 26;
      MR3RL          at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  Match Register . MR can be enabled through the MCR to reset the TC, stop both the TC and PC, and/or generate an interrupt every time MR matches the TC.

   --  Match Register . MR can be enabled through the MCR to reset the TC, stop
   --  both the TC and PC, and/or generate an interrupt every time MR matches
   --  the TC.
   type MR_Registers is array (0 .. 3) of HAL.UInt32
     with Volatile;

   --  Capture Control Register. The CCR controls which edges of the capture
   --  inputs are used to load the Capture Registers and whether or not an
   --  interrupt is generated when a capture takes place.
   type CCR_Register is record
      --  Rising edge of capture channel 0: a sequence of 0 then 1 causes CR0
      --  to be loaded with the contents of TC. 0 = disabled. 1 = enabled.
      CAP0RE         : Boolean := False;
      --  Falling edge of capture channel 0: a sequence of 1 then 0 causes CR0
      --  to be loaded with the contents of TC. 0 = disabled. 1 = enabled.
      CAP0FE         : Boolean := False;
      --  Generate interrupt on channel 0 capture event: a CR0 load generates
      --  an interrupt.
      CAP0I          : Boolean := False;
      --  Rising edge of capture channel 1: a sequence of 0 then 1 causes CR1
      --  to be loaded with the contents of TC. 0 = disabled. 1 = enabled.
      CAP1RE         : Boolean := False;
      --  Falling edge of capture channel 1: a sequence of 1 then 0 causes CR1
      --  to be loaded with the contents of TC. 0 = disabled. 1 = enabled.
      CAP1FE         : Boolean := False;
      --  Generate interrupt on channel 1 capture event: a CR1 load generates
      --  an interrupt.
      CAP1I          : Boolean := False;
      --  Rising edge of capture channel 2: a sequence of 0 then 1 causes CR2
      --  to be loaded with the contents of TC. 0 = disabled. 1 = enabled.
      CAP2RE         : Boolean := False;
      --  Falling edge of capture channel 2: a sequence of 1 then 0 causes CR2
      --  to be loaded with the contents of TC. 0 = disabled. 1 = enabled.
      CAP2FE         : Boolean := False;
      --  Generate interrupt on channel 2 capture event: a CR2 load generates
      --  an interrupt.
      CAP2I          : Boolean := False;
      --  Rising edge of capture channel 3: a sequence of 0 then 1 causes CR3
      --  to be loaded with the contents of TC. 0 = disabled. 1 = enabled.
      CAP3RE         : Boolean := False;
      --  Falling edge of capture channel 3: a sequence of 1 then 0 causes CR3
      --  to be loaded with the contents of TC. 0 = disabled. 1 = enabled.
      CAP3FE         : Boolean := False;
      --  Generate interrupt on channel 3 capture event: a CR3 load generates
      --  an interrupt.
      CAP3I          : Boolean := False;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCR_Register use record
      CAP0RE         at 0 range 0 .. 0;
      CAP0FE         at 0 range 1 .. 1;
      CAP0I          at 0 range 2 .. 2;
      CAP1RE         at 0 range 3 .. 3;
      CAP1FE         at 0 range 4 .. 4;
      CAP1I          at 0 range 5 .. 5;
      CAP2RE         at 0 range 6 .. 6;
      CAP2FE         at 0 range 7 .. 7;
      CAP2I          at 0 range 8 .. 8;
      CAP3RE         at 0 range 9 .. 9;
      CAP3FE         at 0 range 10 .. 10;
      CAP3I          at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   --  Capture Register . CR is loaded with the value of TC when there is an event on the CAPn. input.

   --  Capture Register . CR is loaded with the value of TC when there is an
   --  event on the CAPn. input.
   type CR_Registers is array (0 .. 3) of HAL.UInt32
     with Volatile;

   --  EMR_EM array
   type EMR_EM_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for EMR_EM
   type EMR_EM_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EM as a value
            Val : HAL.UInt4;
         when True =>
            --  EM as an array
            Arr : EMR_EM_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for EMR_EM_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  External Match Control 0. Determines the functionality of External Match
   --  0.
   type EMR_EMC0_Field is
     (
      --  Do Nothing.
      Do_Nothing,
      --  Clear. Clear the corresponding External Match bit/output to 0 (MAT0
      --  pin is LOW if pinned out).
      Clear,
      --  Set. Set the corresponding External Match bit/output to 1 (MAT0 pin
      --  is HIGH if pinned out).
      Set,
      --  Toggle. Toggle the corresponding External Match bit/output.
      Toggle)
     with Size => 2;
   for EMR_EMC0_Field use
     (Do_Nothing => 0,
      Clear => 1,
      Set => 2,
      Toggle => 3);

   --  External Match Control 1. Determines the functionality of External Match
   --  1.
   type EMR_EMC1_Field is
     (
      --  Do Nothing.
      Do_Nothing,
      --  Clear. Clear the corresponding External Match bit/output to 0 (MAT1
      --  pin is LOW if pinned out).
      Clear,
      --  Set. Set the corresponding External Match bit/output to 1 (MAT1 pin
      --  is HIGH if pinned out).
      Set,
      --  Toggle. Toggle the corresponding External Match bit/output.
      Toggle)
     with Size => 2;
   for EMR_EMC1_Field use
     (Do_Nothing => 0,
      Clear => 1,
      Set => 2,
      Toggle => 3);

   --  External Match Control 2. Determines the functionality of External Match
   --  2.
   type EMR_EMC2_Field is
     (
      --  Do Nothing.
      Do_Nothing,
      --  Clear. Clear the corresponding External Match bit/output to 0 (MAT2
      --  pin is LOW if pinned out).
      Clear,
      --  Set. Set the corresponding External Match bit/output to 1 (MAT2 pin
      --  is HIGH if pinned out).
      Set,
      --  Toggle. Toggle the corresponding External Match bit/output.
      Toggle)
     with Size => 2;
   for EMR_EMC2_Field use
     (Do_Nothing => 0,
      Clear => 1,
      Set => 2,
      Toggle => 3);

   --  External Match Control 3. Determines the functionality of External Match
   --  3.
   type EMR_EMC3_Field is
     (
      --  Do Nothing.
      Do_Nothing,
      --  Clear. Clear the corresponding External Match bit/output to 0 (MAT3
      --  pin is LOW if pinned out).
      Clear,
      --  Set. Set the corresponding External Match bit/output to 1 (MAT3 pin
      --  is HIGH if pinned out).
      Set,
      --  Toggle. Toggle the corresponding External Match bit/output.
      Toggle)
     with Size => 2;
   for EMR_EMC3_Field use
     (Do_Nothing => 0,
      Clear => 1,
      Set => 2,
      Toggle => 3);

   --  External Match Register. The EMR controls the match function and the
   --  external match pins.
   type EMR_Register is record
      --  External Match 0. This bit reflects the state of output MAT0, whether
      --  or not this output is connected to a pin. When a match occurs between
      --  the TC and MR0, this bit can either toggle, go LOW, go HIGH, or do
      --  nothing, as selected by EMR[5:4]. This bit is driven to the MAT pins
      --  if the match function is selected via IOCON. 0 = LOW. 1 = HIGH.
      EM             : EMR_EM_Field := (As_Array => False, Val => 16#0#);
      --  External Match Control 0. Determines the functionality of External
      --  Match 0.
      EMC0           : EMR_EMC0_Field := NXP_SVD.CTIMER.Do_Nothing;
      --  External Match Control 1. Determines the functionality of External
      --  Match 1.
      EMC1           : EMR_EMC1_Field := NXP_SVD.CTIMER.Do_Nothing;
      --  External Match Control 2. Determines the functionality of External
      --  Match 2.
      EMC2           : EMR_EMC2_Field := NXP_SVD.CTIMER.Do_Nothing;
      --  External Match Control 3. Determines the functionality of External
      --  Match 3.
      EMC3           : EMR_EMC3_Field := NXP_SVD.CTIMER.Do_Nothing;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EMR_Register use record
      EM             at 0 range 0 .. 3;
      EMC0           at 0 range 4 .. 5;
      EMC1           at 0 range 6 .. 7;
      EMC2           at 0 range 8 .. 9;
      EMC3           at 0 range 10 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   --  Counter/Timer Mode This field selects which rising APB bus clock edges
   --  can increment Timer's Prescale Counter (PC), or clear PC and increment
   --  Timer Counter (TC). Timer Mode: the TC is incremented when the Prescale
   --  Counter matches the Prescale Register.
   type CTCR_CTMODE_Field is
     (
      --  Timer Mode. Incremented every rising APB bus clock edge.
      Timer,
      --  Counter Mode rising edge. TC is incremented on rising edges on the
      --  CAP input selected by bits 3:2.
      Counter_Rising_Edge,
      --  Counter Mode falling edge. TC is incremented on falling edges on the
      --  CAP input selected by bits 3:2.
      Counter_Falling_Edge,
      --  Counter Mode dual edge. TC is incremented on both edges on the CAP
      --  input selected by bits 3:2.
      Counter_Dual_Edge)
     with Size => 2;
   for CTCR_CTMODE_Field use
     (Timer => 0,
      Counter_Rising_Edge => 1,
      Counter_Falling_Edge => 2,
      Counter_Dual_Edge => 3);

   --  Count Input Select When bits 1:0 in this register are not 00, these bits
   --  select which CAP pin is sampled for clocking. Note: If Counter mode is
   --  selected for a particular CAPn input in the CTCR, the 3 bits for that
   --  input in the Capture Control Register (CCR) must be programmed as 000.
   --  However, capture and/or interrupt can be selected for the other 3 CAPn
   --  inputs in the same timer.
   type CTCR_CINSEL_Field is
     (
      --  Channel 0. CAPn.0 for CTIMERn
      Channel_0,
      --  Channel 1. CAPn.1 for CTIMERn
      Channel_1,
      --  Channel 2. CAPn.2 for CTIMERn
      Channel_2,
      --  Channel 3. CAPn.3 for CTIMERn
      Channel_3)
     with Size => 2;
   for CTCR_CINSEL_Field use
     (Channel_0 => 0,
      Channel_1 => 1,
      Channel_2 => 2,
      Channel_3 => 3);

   --  Edge select. When bit 4 is 1, these bits select which capture input edge
   --  will cause the timer and prescaler to be cleared. These bits have no
   --  effect when bit 4 is low. Values 0x2 to 0x3 and 0x6 to 0x7 are reserved.
   type CTCR_SELCC_Field is
     (
      --  Channel 0 Rising Edge. Rising edge of the signal on capture channel 0
      --  clears the timer (if bit 4 is set).
      Channel_0_Rising,
      --  Channel 0 Falling Edge. Falling edge of the signal on capture channel
      --  0 clears the timer (if bit 4 is set).
      Channel_0_Falling,
      --  Channel 1 Rising Edge. Rising edge of the signal on capture channel 1
      --  clears the timer (if bit 4 is set).
      Channel_1_Rising,
      --  Channel 1 Falling Edge. Falling edge of the signal on capture channel
      --  1 clears the timer (if bit 4 is set).
      Channel_1_Falling,
      --  Channel 2 Rising Edge. Rising edge of the signal on capture channel 2
      --  clears the timer (if bit 4 is set).
      Channel_2_Rising,
      --  Channel 2 Falling Edge. Falling edge of the signal on capture channel
      --  2 clears the timer (if bit 4 is set).
      Channel_2_Falling)
     with Size => 3;
   for CTCR_SELCC_Field use
     (Channel_0_Rising => 0,
      Channel_0_Falling => 1,
      Channel_1_Rising => 2,
      Channel_1_Falling => 3,
      Channel_2_Rising => 4,
      Channel_2_Falling => 5);

   --  Count Control Register. The CTCR selects between Timer and Counter mode,
   --  and in Counter mode selects the signal and edge(s) for counting.
   type CTCR_Register is record
      --  Counter/Timer Mode This field selects which rising APB bus clock
      --  edges can increment Timer's Prescale Counter (PC), or clear PC and
      --  increment Timer Counter (TC). Timer Mode: the TC is incremented when
      --  the Prescale Counter matches the Prescale Register.
      CTMODE        : CTCR_CTMODE_Field := NXP_SVD.CTIMER.Timer;
      --  Count Input Select When bits 1:0 in this register are not 00, these
      --  bits select which CAP pin is sampled for clocking. Note: If Counter
      --  mode is selected for a particular CAPn input in the CTCR, the 3 bits
      --  for that input in the Capture Control Register (CCR) must be
      --  programmed as 000. However, capture and/or interrupt can be selected
      --  for the other 3 CAPn inputs in the same timer.
      CINSEL        : CTCR_CINSEL_Field := NXP_SVD.CTIMER.Channel_0;
      --  Setting this bit to 1 enables clearing of the timer and the prescaler
      --  when the capture-edge event specified in bits 7:5 occurs.
      ENCC          : Boolean := False;
      --  Edge select. When bit 4 is 1, these bits select which capture input
      --  edge will cause the timer and prescaler to be cleared. These bits
      --  have no effect when bit 4 is low. Values 0x2 to 0x3 and 0x6 to 0x7
      --  are reserved.
      SELCC         : CTCR_SELCC_Field := NXP_SVD.CTIMER.Channel_0_Rising;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CTCR_Register use record
      CTMODE        at 0 range 0 .. 1;
      CINSEL        at 0 range 2 .. 3;
      ENCC          at 0 range 4 .. 4;
      SELCC         at 0 range 5 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  PWM mode enable for channel0.
   type PWMC_PWMEN0_Field is
     (
      --  Match. CTIMERn_MAT0 is controlled by EM0.
      Match,
      --  PWM. PWM mode is enabled for CTIMERn_MAT0.
      Pwm)
     with Size => 1;
   for PWMC_PWMEN0_Field use
     (Match => 0,
      Pwm => 1);

   --  PWM mode enable for channel1.
   type PWMC_PWMEN1_Field is
     (
      --  Match. CTIMERn_MAT01 is controlled by EM1.
      Match,
      --  PWM. PWM mode is enabled for CTIMERn_MAT1.
      Pwm)
     with Size => 1;
   for PWMC_PWMEN1_Field use
     (Match => 0,
      Pwm => 1);

   --  PWM mode enable for channel2.
   type PWMC_PWMEN2_Field is
     (
      --  Match. CTIMERn_MAT2 is controlled by EM2.
      Match,
      --  PWM. PWM mode is enabled for CTIMERn_MAT2.
      Pwm)
     with Size => 1;
   for PWMC_PWMEN2_Field use
     (Match => 0,
      Pwm => 1);

   --  PWM mode enable for channel3. Note: It is recommended to use match
   --  channel 3 to set the PWM cycle.
   type PWMC_PWMEN3_Field is
     (
      --  Match. CTIMERn_MAT3 is controlled by EM3.
      Match,
      --  PWM. PWM mode is enabled for CT132Bn_MAT3.
      Pwm)
     with Size => 1;
   for PWMC_PWMEN3_Field use
     (Match => 0,
      Pwm => 1);

   --  PWM Control Register. This register enables PWM mode for the external
   --  match pins.
   type PWMC_Register is record
      --  PWM mode enable for channel0.
      PWMEN0        : PWMC_PWMEN0_Field := NXP_SVD.CTIMER.Match;
      --  PWM mode enable for channel1.
      PWMEN1        : PWMC_PWMEN1_Field := NXP_SVD.CTIMER.Match;
      --  PWM mode enable for channel2.
      PWMEN2        : PWMC_PWMEN2_Field := NXP_SVD.CTIMER.Match;
      --  PWM mode enable for channel3. Note: It is recommended to use match
      --  channel 3 to set the PWM cycle.
      PWMEN3        : PWMC_PWMEN3_Field := NXP_SVD.CTIMER.Match;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWMC_Register use record
      PWMEN0        at 0 range 0 .. 0;
      PWMEN1        at 0 range 1 .. 1;
      PWMEN2        at 0 range 2 .. 2;
      PWMEN3        at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  Match Shadow Register

   --  Match Shadow Register
   type MSR_Registers is array (0 .. 3) of HAL.UInt32
     with Volatile;

   -----------------
   -- Peripherals --
   -----------------

   --  Standard counter/timers (CTIMER0 to 4)
   type CTIMER_Peripheral is record
      --  Interrupt Register. The IR can be written to clear interrupts. The IR
      --  can be read to identify which of eight possible interrupt sources are
      --  pending.
      IR   : aliased IR_Register;
      --  Timer Control Register. The TCR is used to control the Timer Counter
      --  functions. The Timer Counter can be disabled or reset through the
      --  TCR.
      TCR  : aliased TCR_Register;
      --  Timer Counter
      TC   : aliased HAL.UInt32;
      --  Prescale Register
      PR   : aliased HAL.UInt32;
      --  Prescale Counter
      PC   : aliased HAL.UInt32;
      --  Match Control Register
      MCR  : aliased MCR_Register;
      --  Match Register . MR can be enabled through the MCR to reset the TC,
      --  stop both the TC and PC, and/or generate an interrupt every time MR
      --  matches the TC.
      MR   : aliased MR_Registers;
      --  Capture Control Register. The CCR controls which edges of the capture
      --  inputs are used to load the Capture Registers and whether or not an
      --  interrupt is generated when a capture takes place.
      CCR  : aliased CCR_Register;
      --  Capture Register . CR is loaded with the value of TC when there is an
      --  event on the CAPn. input.
      CR   : aliased CR_Registers;
      --  External Match Register. The EMR controls the match function and the
      --  external match pins.
      EMR  : aliased EMR_Register;
      --  Count Control Register. The CTCR selects between Timer and Counter
      --  mode, and in Counter mode selects the signal and edge(s) for
      --  counting.
      CTCR : aliased CTCR_Register;
      --  PWM Control Register. This register enables PWM mode for the external
      --  match pins.
      PWMC : aliased PWMC_Register;
      --  Match Shadow Register
      MSR  : aliased MSR_Registers;
   end record
     with Volatile;

   for CTIMER_Peripheral use record
      IR   at 16#0# range 0 .. 31;
      TCR  at 16#4# range 0 .. 31;
      TC   at 16#8# range 0 .. 31;
      PR   at 16#C# range 0 .. 31;
      PC   at 16#10# range 0 .. 31;
      MCR  at 16#14# range 0 .. 31;
      MR   at 16#18# range 0 .. 127;
      CCR  at 16#28# range 0 .. 31;
      CR   at 16#2C# range 0 .. 127;
      EMR  at 16#3C# range 0 .. 31;
      CTCR at 16#70# range 0 .. 31;
      PWMC at 16#74# range 0 .. 31;
      MSR  at 16#78# range 0 .. 127;
   end record;

   --  Standard counter/timers (CTIMER0 to 4)
   CTIMER0_Periph : aliased CTIMER_Peripheral
     with Import, Address => System'To_Address (16#40008000#);

   --  Standard counter/timers (CTIMER0 to 4)
   CTIMER1_Periph : aliased CTIMER_Peripheral
     with Import, Address => System'To_Address (16#40009000#);

   --  Standard counter/timers (CTIMER0 to 4)
   CTIMER2_Periph : aliased CTIMER_Peripheral
     with Import, Address => System'To_Address (16#40028000#);

   --  Standard counter/timers (CTIMER0 to 4)
   CTIMER3_Periph : aliased CTIMER_Peripheral
     with Import, Address => System'To_Address (16#40029000#);

   --  Standard counter/timers (CTIMER0 to 4)
   CTIMER4_Periph : aliased CTIMER_Peripheral
     with Import, Address => System'To_Address (16#4002A000#);

end NXP_SVD.CTIMER;
