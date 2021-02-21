--  Copyright 2016-2019 NXP
--  All rights reserved.SPDX-License-Identifier: BSD-3-Clause


--  This spec has been automatically generated from LPC55S6x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NXP_SVD.INPUTMUX is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Input number to SCT0 inputs 0 to 6..
   type SCT0_INMUX_INP_N_Field is
     (
      --  SCT_GPI0 function selected from IOCON register
      VAL0,
      --  SCT_GPI1 function selected from IOCON register
      VAL1,
      --  SCT_GPI2 function selected from IOCON register
      VAL2,
      --  SCT_GPI3 function selected from IOCON register
      VAL3,
      --  SCT_GPI4 function selected from IOCON register
      VAL4,
      --  SCT_GPI5 function selected from IOCON register
      VAL5,
      --  SCT_GPI6 function selected from IOCON register
      VAL6,
      --  SCT_GPI7 function selected from IOCON register
      VAL7,
      --  T0_OUT0 ctimer 0 match[0] output
      VAL8,
      --  T1_OUT0 ctimer 1 match[0] output
      VAL9,
      --  T2_OUT0 ctimer 2 match[0] output
      VAL10,
      --  T3_OUT0 ctimer 3 match[0] output
      VAL11,
      --  T4_OUT0 ctimer 4 match[0] output
      VAL12,
      --  ADC_IRQ interrupt request from ADC
      VAL13,
      --  GPIOINT_BMATCH
      VAL14,
      --  USB0_FRAME_TOGGLE
      VAL15,
      --  USB1_FRAME_TOGGLE
      VAL16,
      --  COMP_OUTPUT output from analog comparator
      VAL17,
      --  I2S_SHARED_SCK[0] output from I2S pin sharing
      VAL18,
      --  I2S_SHARED_SCK[1] output from I2S pin sharing
      VAL19,
      --  I2S_SHARED_WS[0] output from I2S pin sharing
      VAL20,
      --  I2S_SHARED_WS[1] output from I2S pin sharing
      VAL21,
      --  ARM_TXEV interrupt event from cpu0 or cpu1
      VAL22,
      --  DEBUG_HALTED from cpu0 or cpu1
      VAL23,
      --  None
      VAL24,
      --  None
      VAL24_1,
      --  None
      VAL24_2,
      --  None
      VAL24_3,
      --  None
      VAL24_4,
      --  None
      VAL24_5,
      --  None
      VAL24_6,
      --  None
      VAL24_7)
     with Size => 5;
   for SCT0_INMUX_INP_N_Field use
     (VAL0 => 0,
      VAL1 => 1,
      VAL2 => 2,
      VAL3 => 3,
      VAL4 => 4,
      VAL5 => 5,
      VAL6 => 6,
      VAL7 => 7,
      VAL8 => 8,
      VAL9 => 9,
      VAL10 => 10,
      VAL11 => 11,
      VAL12 => 12,
      VAL13 => 13,
      VAL14 => 14,
      VAL15 => 15,
      VAL16 => 16,
      VAL17 => 17,
      VAL18 => 18,
      VAL19 => 19,
      VAL20 => 20,
      VAL21 => 21,
      VAL22 => 22,
      VAL23 => 23,
      VAL24 => 24,
      VAL24_1 => 25,
      VAL24_2 => 26,
      VAL24_3 => 27,
      VAL24_4 => 28,
      VAL24_5 => 29,
      VAL24_6 => 30,
      VAL24_7 => 31);

   --  Input mux register for SCT0 input
   type SCT0_INMUX_Register is record
      --  Input number to SCT0 inputs 0 to 6..
      INP_N         : SCT0_INMUX_INP_N_Field := NXP_SVD.INPUTMUX.VAL24_7;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SCT0_INMUX_Register use record
      INP_N         at 0 range 0 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   --  Input mux register for SCT0 input
   type SCT0_INMUX_Registers is array (0 .. 6) of SCT0_INMUX_Register
     with Volatile;

   --  Input number to TIMER0 capture inputs 0 to 4
   type TIMER0CAPTSEL_CAPTSEL_Field is
     (
      --  CT_INP0 function selected from IOCON register
      VAL0,
      --  CT_INP1 function selected from IOCON register
      VAL1,
      --  CT_INP2 function selected from IOCON register
      VAL2,
      --  CT_INP3 function selected from IOCON register
      VAL3,
      --  CT_INP4 function selected from IOCON register
      VAL4,
      --  CT_INP5 function selected from IOCON register
      VAL5,
      --  CT_INP6 function selected from IOCON register
      VAL6,
      --  CT_INP7 function selected from IOCON register
      VAL7,
      --  CT_INP8 function selected from IOCON register
      VAL8,
      --  CT_INP9 function selected from IOCON register
      VAL9,
      --  CT_INP10 function selected from IOCON register
      VAL10,
      --  CT_INP11 function selected from IOCON register
      VAL11,
      --  CT_INP12 function selected from IOCON register
      VAL12,
      --  CT_INP13 function selected from IOCON register
      VAL13,
      --  CT_INP14 function selected from IOCON register
      VAL14,
      --  CT_INP15 function selected from IOCON register
      VAL15,
      --  CT_INP16 function selected from IOCON register
      VAL16,
      --  None
      VAL17,
      --  None
      VAL18,
      --  None
      VAL19,
      --  USB0_FRAME_TOGGLE
      VAL20,
      --  USB1_FRAME_TOGGLE
      VAL21,
      --  COMP_OUTPUT output from analog comparator
      VAL22,
      --  I2S_SHARED_WS[0] output from I2S pin sharing
      VAL23,
      --  I2S_SHARED_WS[1] output from I2S pin sharing
      VAL24,
      --  None
      VAL25,
      --  None
      VAL25_1,
      --  None
      VAL25_2,
      --  None
      VAL25_3,
      --  None
      VAL25_4,
      --  None
      VAL25_5,
      --  None
      VAL25_6)
     with Size => 5;
   for TIMER0CAPTSEL_CAPTSEL_Field use
     (VAL0 => 0,
      VAL1 => 1,
      VAL2 => 2,
      VAL3 => 3,
      VAL4 => 4,
      VAL5 => 5,
      VAL6 => 6,
      VAL7 => 7,
      VAL8 => 8,
      VAL9 => 9,
      VAL10 => 10,
      VAL11 => 11,
      VAL12 => 12,
      VAL13 => 13,
      VAL14 => 14,
      VAL15 => 15,
      VAL16 => 16,
      VAL17 => 17,
      VAL18 => 18,
      VAL19 => 19,
      VAL20 => 20,
      VAL21 => 21,
      VAL22 => 22,
      VAL23 => 23,
      VAL24 => 24,
      VAL25 => 25,
      VAL25_1 => 26,
      VAL25_2 => 27,
      VAL25_3 => 28,
      VAL25_4 => 29,
      VAL25_5 => 30,
      VAL25_6 => 31);

   --  Capture select registers for TIMER0 inputs
   type TIMER0CAPTSEL_Register is record
      --  Input number to TIMER0 capture inputs 0 to 4
      CAPTSEL       : TIMER0CAPTSEL_CAPTSEL_Field := NXP_SVD.INPUTMUX.VAL25_6;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIMER0CAPTSEL_Register use record
      CAPTSEL       at 0 range 0 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   --  Capture select registers for TIMER0 inputs
   type TIMER0CAPTSEL_Registers is array (0 .. 3) of TIMER0CAPTSEL_Register
     with Volatile;

   --  Input number to TIMER1 capture inputs 0 to 4
   type TIMER1CAPTSEL_CAPTSEL_Field is
     (
      --  CT_INP0 function selected from IOCON register
      VAL0,
      --  CT_INP1 function selected from IOCON register
      VAL1,
      --  CT_INP2 function selected from IOCON register
      VAL2,
      --  CT_INP3 function selected from IOCON register
      VAL3,
      --  CT_INP4 function selected from IOCON register
      VAL4,
      --  CT_INP5 function selected from IOCON register
      VAL5,
      --  CT_INP6 function selected from IOCON register
      VAL6,
      --  CT_INP7 function selected from IOCON register
      VAL7,
      --  CT_INP8 function selected from IOCON register
      VAL8,
      --  CT_INP9 function selected from IOCON register
      VAL9,
      --  CT_INP10 function selected from IOCON register
      VAL10,
      --  CT_INP11 function selected from IOCON register
      VAL11,
      --  CT_INP12 function selected from IOCON register
      VAL12,
      --  CT_INP13 function selected from IOCON register
      VAL13,
      --  CT_INP14 function selected from IOCON register
      VAL14,
      --  CT_INP15 function selected from IOCON register
      VAL15,
      --  CT_INP16 function selected from IOCON register
      VAL16,
      --  None
      VAL17,
      --  None
      VAL18,
      --  None
      VAL19,
      --  USB0_FRAME_TOGGLE
      VAL20,
      --  USB1_FRAME_TOGGLE
      VAL21,
      --  COMP_OUTPUT output from analog comparator
      VAL22,
      --  I2S_SHARED_WS[0] output from I2S pin sharing
      VAL23,
      --  I2S_SHARED_WS[1] output from I2S pin sharing
      VAL24,
      --  None
      VAL25,
      --  None
      VAL25_1,
      --  None
      VAL25_2,
      --  None
      VAL25_3,
      --  None
      VAL25_4,
      --  None
      VAL25_5,
      --  None
      VAL25_6)
     with Size => 5;
   for TIMER1CAPTSEL_CAPTSEL_Field use
     (VAL0 => 0,
      VAL1 => 1,
      VAL2 => 2,
      VAL3 => 3,
      VAL4 => 4,
      VAL5 => 5,
      VAL6 => 6,
      VAL7 => 7,
      VAL8 => 8,
      VAL9 => 9,
      VAL10 => 10,
      VAL11 => 11,
      VAL12 => 12,
      VAL13 => 13,
      VAL14 => 14,
      VAL15 => 15,
      VAL16 => 16,
      VAL17 => 17,
      VAL18 => 18,
      VAL19 => 19,
      VAL20 => 20,
      VAL21 => 21,
      VAL22 => 22,
      VAL23 => 23,
      VAL24 => 24,
      VAL25 => 25,
      VAL25_1 => 26,
      VAL25_2 => 27,
      VAL25_3 => 28,
      VAL25_4 => 29,
      VAL25_5 => 30,
      VAL25_6 => 31);

   --  Capture select registers for TIMER1 inputs
   type TIMER1CAPTSEL_Register is record
      --  Input number to TIMER1 capture inputs 0 to 4
      CAPTSEL       : TIMER1CAPTSEL_CAPTSEL_Field := NXP_SVD.INPUTMUX.VAL25_6;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIMER1CAPTSEL_Register use record
      CAPTSEL       at 0 range 0 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   --  Capture select registers for TIMER1 inputs
   type TIMER1CAPTSEL_Registers is array (0 .. 3) of TIMER1CAPTSEL_Register
     with Volatile;

   --  Input number to TIMER2 capture inputs 0 to 4
   type TIMER2CAPTSEL_CAPTSEL_Field is
     (
      --  CT_INP0 function selected from IOCON register
      VAL0,
      --  CT_INP1 function selected from IOCON register
      VAL1,
      --  CT_INP2 function selected from IOCON register
      VAL2,
      --  CT_INP3 function selected from IOCON register
      VAL3,
      --  CT_INP4 function selected from IOCON register
      VAL4,
      --  CT_INP5 function selected from IOCON register
      VAL5,
      --  CT_INP6 function selected from IOCON register
      VAL6,
      --  CT_INP7 function selected from IOCON register
      VAL7,
      --  CT_INP8 function selected from IOCON register
      VAL8,
      --  CT_INP9 function selected from IOCON register
      VAL9,
      --  CT_INP10 function selected from IOCON register
      VAL10,
      --  CT_INP11 function selected from IOCON register
      VAL11,
      --  CT_INP12 function selected from IOCON register
      VAL12,
      --  CT_INP13 function selected from IOCON register
      VAL13,
      --  CT_INP14 function selected from IOCON register
      VAL14,
      --  CT_INP15 function selected from IOCON register
      VAL15,
      --  CT_INP16 function selected from IOCON register
      VAL16,
      --  None
      VAL17,
      --  None
      VAL18,
      --  None
      VAL19,
      --  USB0_FRAME_TOGGLE
      VAL20,
      --  USB1_FRAME_TOGGLE
      VAL21,
      --  COMP_OUTPUT output from analog comparator
      VAL22,
      --  I2S_SHARED_WS[0] output from I2S pin sharing
      VAL23,
      --  I2S_SHARED_WS[1] output from I2S pin sharing
      VAL24,
      --  None
      VAL25,
      --  None
      VAL25_1,
      --  None
      VAL25_2,
      --  None
      VAL25_3,
      --  None
      VAL25_4,
      --  None
      VAL25_5,
      --  None
      VAL25_6)
     with Size => 5;
   for TIMER2CAPTSEL_CAPTSEL_Field use
     (VAL0 => 0,
      VAL1 => 1,
      VAL2 => 2,
      VAL3 => 3,
      VAL4 => 4,
      VAL5 => 5,
      VAL6 => 6,
      VAL7 => 7,
      VAL8 => 8,
      VAL9 => 9,
      VAL10 => 10,
      VAL11 => 11,
      VAL12 => 12,
      VAL13 => 13,
      VAL14 => 14,
      VAL15 => 15,
      VAL16 => 16,
      VAL17 => 17,
      VAL18 => 18,
      VAL19 => 19,
      VAL20 => 20,
      VAL21 => 21,
      VAL22 => 22,
      VAL23 => 23,
      VAL24 => 24,
      VAL25 => 25,
      VAL25_1 => 26,
      VAL25_2 => 27,
      VAL25_3 => 28,
      VAL25_4 => 29,
      VAL25_5 => 30,
      VAL25_6 => 31);

   --  Capture select registers for TIMER2 inputs
   type TIMER2CAPTSEL_Register is record
      --  Input number to TIMER2 capture inputs 0 to 4
      CAPTSEL       : TIMER2CAPTSEL_CAPTSEL_Field := NXP_SVD.INPUTMUX.VAL25_6;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIMER2CAPTSEL_Register use record
      CAPTSEL       at 0 range 0 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   --  Capture select registers for TIMER2 inputs
   type TIMER2CAPTSEL_Registers is array (0 .. 3) of TIMER2CAPTSEL_Register
     with Volatile;

   subtype PINTSEL_INTPIN_Field is HAL.UInt7;

   --  Pin interrupt select register
   type PINTSEL_Register is record
      --  Pin number select for pin interrupt or pattern match engine input.
      --  For PIOx_y: INTPIN = (x * 32) + y. PIO0_0 to PIO1_31 correspond to
      --  numbers 0 to 63.
      INTPIN        : PINTSEL_INTPIN_Field := 16#7F#;
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PINTSEL_Register use record
      INTPIN        at 0 range 0 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   --  Pin interrupt select register
   type PINTSEL_Registers is array (0 .. 7) of PINTSEL_Register
     with Volatile;

   --  Trigger input number (decimal value) for DMA channel n (n = 0 to 22).
   type DMA0_ITRIG_INMUX_INP_Field is
     (
      --  Pin interrupt 0
      VAL0,
      --  Pin interrupt 1
      VAL1,
      --  Pin interrupt 2
      VAL2,
      --  Pin interrupt 3
      VAL3,
      --  Timer CTIMER0 Match 0
      VAL4,
      --  Timer CTIMER0 Match 1
      VAL5,
      --  Timer CTIMER1 Match 0
      VAL6,
      --  Timer CTIMER1 Match 1
      VAL7,
      --  Timer CTIMER2 Match 0
      VAL8,
      --  Timer CTIMER2 Match 1
      VAL9,
      --  Timer CTIMER3 Match 0
      VAL10,
      --  Timer CTIMER3 Match 1
      VAL11,
      --  Timer CTIMER4 Match 0
      VAL12,
      --  Timer CTIMER4 Match 1
      VAL13,
      --  COMP_OUTPUT
      VAL14,
      --  DMA0 output trigger mux 0
      VAL15,
      --  DMA0 output trigger mux 1
      VAL16,
      --  DMA0 output trigger mux 1
      VAL17,
      --  DMA0 output trigger mux 3
      VAL18,
      --  SCT0 DMA request 0
      VAL19,
      --  SCT0 DMA request 1
      VAL20,
      --  HASH DMA RX trigger
      VAL21,
      --  None
      VAL22,
      --  None
      VAL22_1,
      --  None
      VAL22_2,
      --  None
      VAL22_3,
      --  None
      VAL22_4,
      --  None
      VAL22_5,
      --  None
      VAL22_6,
      --  None
      VAL22_7,
      --  None
      VAL22_8,
      --  None
      VAL22_9)
     with Size => 5;
   for DMA0_ITRIG_INMUX_INP_Field use
     (VAL0 => 0,
      VAL1 => 1,
      VAL2 => 2,
      VAL3 => 3,
      VAL4 => 4,
      VAL5 => 5,
      VAL6 => 6,
      VAL7 => 7,
      VAL8 => 8,
      VAL9 => 9,
      VAL10 => 10,
      VAL11 => 11,
      VAL12 => 12,
      VAL13 => 13,
      VAL14 => 14,
      VAL15 => 15,
      VAL16 => 16,
      VAL17 => 17,
      VAL18 => 18,
      VAL19 => 19,
      VAL20 => 20,
      VAL21 => 21,
      VAL22 => 22,
      VAL22_1 => 23,
      VAL22_2 => 24,
      VAL22_3 => 25,
      VAL22_4 => 26,
      VAL22_5 => 27,
      VAL22_6 => 28,
      VAL22_7 => 29,
      VAL22_8 => 30,
      VAL22_9 => 31);

   --  Trigger select register for DMA0 channel
   type DMA0_ITRIG_INMUX_Register is record
      --  Trigger input number (decimal value) for DMA channel n (n = 0 to 22).
      INP           : DMA0_ITRIG_INMUX_INP_Field := NXP_SVD.INPUTMUX.VAL22_9;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMA0_ITRIG_INMUX_Register use record
      INP           at 0 range 0 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   --  Trigger select register for DMA0 channel
   type DMA0_ITRIG_INMUX_Registers is array (0 .. 22)
     of DMA0_ITRIG_INMUX_Register
     with Volatile;

   subtype DMA0_OTRIG_INMUX_INP_Field is HAL.UInt5;

   --  DMA0 output trigger selection to become DMA0 trigger
   type DMA0_OTRIG_INMUX_Register is record
      --  DMA trigger output number (decimal value) for DMA channel n (n = 0 to
      --  22).
      INP           : DMA0_OTRIG_INMUX_INP_Field := 16#1F#;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMA0_OTRIG_INMUX_Register use record
      INP           at 0 range 0 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   --  DMA0 output trigger selection to become DMA0 trigger
   type DMA0_OTRIG_INMUX_Registers is array (0 .. 3)
     of DMA0_OTRIG_INMUX_Register
     with Volatile;

   subtype FREQMEAS_REF_CLKIN_Field is HAL.UInt5;

   --  Selection for frequency measurement reference clock
   type FREQMEAS_REF_Register is record
      --  Clock source number (decimal value) for frequency measure function
      --  target clock: 0 = CLK_IN 1 = FRO 12 MHz oscillator 2 = Watchdog
      --  oscillator 3 = 32 kHz RTC oscillator 4 = Main clock (see Section
      --  4.5.23) 5 = PIO0_4 6 = PIO0_20 7 = PIO0_24 8 = PIO1_4
      CLKIN         : FREQMEAS_REF_CLKIN_Field := 16#1F#;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FREQMEAS_REF_Register use record
      CLKIN         at 0 range 0 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype FREQMEAS_TARGET_CLKIN_Field is HAL.UInt5;

   --  Selection for frequency measurement target clock
   type FREQMEAS_TARGET_Register is record
      --  Clock source number (decimal value) for frequency measure function
      --  target clock: 0 = CLK_IN 1 = FRO 12 MHz oscillator 2 = Watchdog
      --  oscillator 3 = 32 kHz RTC oscillator 4 = Main clock (see Section
      --  4.5.23) 5 = PIO0_4 6 = PIO0_20 7 = PIO0_24 8 = PIO1_4
      CLKIN         : FREQMEAS_TARGET_CLKIN_Field := 16#1F#;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FREQMEAS_TARGET_Register use record
      CLKIN         at 0 range 0 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   --  Input number to TIMER3 capture inputs 0 to 4
   type TIMER3CAPTSEL_CAPTSEL_Field is
     (
      --  CT_INP0 function selected from IOCON register
      VAL0,
      --  CT_INP1 function selected from IOCON register
      VAL1,
      --  CT_INP2 function selected from IOCON register
      VAL2,
      --  CT_INP3 function selected from IOCON register
      VAL3,
      --  CT_INP4 function selected from IOCON register
      VAL4,
      --  CT_INP5 function selected from IOCON register
      VAL5,
      --  CT_INP6 function selected from IOCON register
      VAL6,
      --  CT_INP7 function selected from IOCON register
      VAL7,
      --  CT_INP8 function selected from IOCON register
      VAL8,
      --  CT_INP9 function selected from IOCON register
      VAL9,
      --  CT_INP10 function selected from IOCON register
      VAL10,
      --  CT_INP11 function selected from IOCON register
      VAL11,
      --  CT_INP12 function selected from IOCON register
      VAL12,
      --  CT_INP13 function selected from IOCON register
      VAL13,
      --  CT_INP14 function selected from IOCON register
      VAL14,
      --  CT_INP15 function selected from IOCON register
      VAL15,
      --  CT_INP16 function selected from IOCON register
      VAL16,
      --  CT_INP17 function selected from IOCON register
      VAL17,
      --  CT_INP18 function selected from IOCON register
      VAL18,
      --  CT_INP19 function selected from IOCON register
      VAL19,
      --  USB0_FRAME_TOGGLE
      VAL20,
      --  USB1_FRAME_TOGGLE
      VAL21,
      --  COMP_OUTPUT output from analog comparator
      VAL22,
      --  I2S_SHARED_WS[0] output from I2S pin sharing
      VAL23,
      --  I2S_SHARED_WS[1] output from I2S pin sharing
      VAL24,
      --  None
      VAL25,
      --  None
      VAL25_1,
      --  None
      VAL25_2,
      --  None
      VAL25_3,
      --  None
      VAL25_4,
      --  None
      VAL25_5,
      --  None
      VAL25_6)
     with Size => 5;
   for TIMER3CAPTSEL_CAPTSEL_Field use
     (VAL0 => 0,
      VAL1 => 1,
      VAL2 => 2,
      VAL3 => 3,
      VAL4 => 4,
      VAL5 => 5,
      VAL6 => 6,
      VAL7 => 7,
      VAL8 => 8,
      VAL9 => 9,
      VAL10 => 10,
      VAL11 => 11,
      VAL12 => 12,
      VAL13 => 13,
      VAL14 => 14,
      VAL15 => 15,
      VAL16 => 16,
      VAL17 => 17,
      VAL18 => 18,
      VAL19 => 19,
      VAL20 => 20,
      VAL21 => 21,
      VAL22 => 22,
      VAL23 => 23,
      VAL24 => 24,
      VAL25 => 25,
      VAL25_1 => 26,
      VAL25_2 => 27,
      VAL25_3 => 28,
      VAL25_4 => 29,
      VAL25_5 => 30,
      VAL25_6 => 31);

   --  Capture select registers for TIMER3 inputs
   type TIMER3CAPTSEL_Register is record
      --  Input number to TIMER3 capture inputs 0 to 4
      CAPTSEL       : TIMER3CAPTSEL_CAPTSEL_Field := NXP_SVD.INPUTMUX.VAL25_6;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIMER3CAPTSEL_Register use record
      CAPTSEL       at 0 range 0 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   --  Capture select registers for TIMER3 inputs
   type TIMER3CAPTSEL_Registers is array (0 .. 3) of TIMER3CAPTSEL_Register
     with Volatile;

   --  Input number to TIMER4 capture inputs 0 to 4
   type TIMER4CAPTSEL_CAPTSEL_Field is
     (
      --  CT_INP0 function selected from IOCON register
      VAL0,
      --  CT_INP1 function selected from IOCON register
      VAL1,
      --  CT_INP2 function selected from IOCON register
      VAL2,
      --  CT_INP3 function selected from IOCON register
      VAL3,
      --  CT_INP4 function selected from IOCON register
      VAL4,
      --  CT_INP5 function selected from IOCON register
      VAL5,
      --  CT_INP6 function selected from IOCON register
      VAL6,
      --  CT_INP7 function selected from IOCON register
      VAL7,
      --  CT_INP8 function selected from IOCON register
      VAL8,
      --  CT_INP9 function selected from IOCON register
      VAL9,
      --  CT_INP10 function selected from IOCON register
      VAL10,
      --  CT_INP11 function selected from IOCON register
      VAL11,
      --  CT_INP12 function selected from IOCON register
      VAL12,
      --  CT_INP13 function selected from IOCON register
      VAL13,
      --  CT_INP14 function selected from IOCON register
      VAL14,
      --  CT_INP15 function selected from IOCON register
      VAL15,
      --  CT_INP16 function selected from IOCON register
      VAL16,
      --  CT_INP17 function selected from IOCON register
      VAL17,
      --  CT_INP18 function selected from IOCON register
      VAL18,
      --  CT_INP19 function selected from IOCON register
      VAL19,
      --  USB0_FRAME_TOGGLE
      VAL20,
      --  USB1_FRAME_TOGGLE
      VAL21,
      --  COMP_OUTPUT output from analog comparator
      VAL22,
      --  I2S_SHARED_WS[0] output from I2S pin sharing
      VAL23,
      --  I2S_SHARED_WS[1] output from I2S pin sharing
      VAL24,
      --  None
      VAL25,
      --  None
      VAL25_1,
      --  None
      VAL25_2,
      --  None
      VAL25_3,
      --  None
      VAL25_4,
      --  None
      VAL25_5,
      --  None
      VAL25_6)
     with Size => 5;
   for TIMER4CAPTSEL_CAPTSEL_Field use
     (VAL0 => 0,
      VAL1 => 1,
      VAL2 => 2,
      VAL3 => 3,
      VAL4 => 4,
      VAL5 => 5,
      VAL6 => 6,
      VAL7 => 7,
      VAL8 => 8,
      VAL9 => 9,
      VAL10 => 10,
      VAL11 => 11,
      VAL12 => 12,
      VAL13 => 13,
      VAL14 => 14,
      VAL15 => 15,
      VAL16 => 16,
      VAL17 => 17,
      VAL18 => 18,
      VAL19 => 19,
      VAL20 => 20,
      VAL21 => 21,
      VAL22 => 22,
      VAL23 => 23,
      VAL24 => 24,
      VAL25 => 25,
      VAL25_1 => 26,
      VAL25_2 => 27,
      VAL25_3 => 28,
      VAL25_4 => 29,
      VAL25_5 => 30,
      VAL25_6 => 31);

   --  Capture select registers for TIMER4 inputs
   type TIMER4CAPTSEL_Register is record
      --  Input number to TIMER4 capture inputs 0 to 4
      CAPTSEL       : TIMER4CAPTSEL_CAPTSEL_Field := NXP_SVD.INPUTMUX.VAL25_6;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIMER4CAPTSEL_Register use record
      CAPTSEL       at 0 range 0 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   --  Capture select registers for TIMER4 inputs
   type TIMER4CAPTSEL_Registers is array (0 .. 3) of TIMER4CAPTSEL_Register
     with Volatile;

   subtype PINTSECSEL_INTPIN_Field is HAL.UInt6;

   --  Pin interrupt secure select register
   type PINTSECSEL_Register is record
      --  Pin number select for pin interrupt secure or pattern match engine
      --  input. For PIO0_x: INTPIN = x. PIO0_0 to PIO0_31 correspond to
      --  numbers 0 to 31.
      INTPIN        : PINTSECSEL_INTPIN_Field := 16#3F#;
      --  unspecified
      Reserved_6_31 : HAL.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PINTSECSEL_Register use record
      INTPIN        at 0 range 0 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   --  Pin interrupt secure select register
   type PINTSECSEL_Registers is array (0 .. 1) of PINTSECSEL_Register
     with Volatile;

   --  Trigger input number (decimal value) for DMA channel n (n = 0 to 9).
   type DMA1_ITRIG_INMUX_INP_Field is
     (
      --  Pin interrupt 0
      VAL0,
      --  Pin interrupt 1
      VAL1,
      --  Pin interrupt 2
      VAL2,
      --  Pin interrupt 3
      VAL3,
      --  Timer CTIMER0 Match 0
      VAL4,
      --  Timer CTIMER0 Match 1
      VAL5,
      --  Timer CTIMER2 Match 0
      VAL6,
      --  Timer CTIMER4 Match 0
      VAL7,
      --  DMA1 output trigger mux 0
      VAL8,
      --  DMA1 output trigger mux 1
      VAL9,
      --  DMA1 output trigger mux 2
      VAL10,
      --  DMA1 output trigger mux 3
      VAL11,
      --  SCT0 DMA request 0
      VAL12,
      --  SCT0 DMA request 1
      VAL13,
      --  HASH DMA RX trigger
      VAL14,
      --  None
      VAL15)
     with Size => 4;
   for DMA1_ITRIG_INMUX_INP_Field use
     (VAL0 => 0,
      VAL1 => 1,
      VAL2 => 2,
      VAL3 => 3,
      VAL4 => 4,
      VAL5 => 5,
      VAL6 => 6,
      VAL7 => 7,
      VAL8 => 8,
      VAL9 => 9,
      VAL10 => 10,
      VAL11 => 11,
      VAL12 => 12,
      VAL13 => 13,
      VAL14 => 14,
      VAL15 => 15);

   --  Trigger select register for DMA1 channel
   type DMA1_ITRIG_INMUX_Register is record
      --  Trigger input number (decimal value) for DMA channel n (n = 0 to 9).
      INP           : DMA1_ITRIG_INMUX_INP_Field := NXP_SVD.INPUTMUX.VAL15;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMA1_ITRIG_INMUX_Register use record
      INP           at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  Trigger select register for DMA1 channel
   type DMA1_ITRIG_INMUX_Registers is array (0 .. 9)
     of DMA1_ITRIG_INMUX_Register
     with Volatile;

   subtype DMA1_OTRIG_INMUX_INP_Field is HAL.UInt4;

   --  DMA1 output trigger selection to become DMA1 trigger
   type DMA1_OTRIG_INMUX_Register is record
      --  DMA trigger output number (decimal value) for DMA channel n (n = 0 to
      --  9).
      INP           : DMA1_OTRIG_INMUX_INP_Field := 16#F#;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMA1_OTRIG_INMUX_Register use record
      INP           at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  DMA1 output trigger selection to become DMA1 trigger
   type DMA1_OTRIG_INMUX_Registers is array (0 .. 3)
     of DMA1_OTRIG_INMUX_Register
     with Volatile;

   subtype DMA0_REQ_ENA_REQ_ENA_Field is HAL.UInt23;

   --  Enable DMA0 requests
   type DMA0_REQ_ENA_Register is record
      --  Controls the 23 request inputs of DMA0. If bit i is '1' the DMA
      --  request input #i is enabled.
      REQ_ENA        : DMA0_REQ_ENA_REQ_ENA_Field := 16#7FFFFF#;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMA0_REQ_ENA_Register use record
      REQ_ENA        at 0 range 0 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   subtype DMA0_REQ_ENA_SET_SET_Field is HAL.UInt23;

   --  Set one or several bits in DMA0_REQ_ENA register
   type DMA0_REQ_ENA_SET_Register is record
      --  Write-only. Write : If bit #i = 1, bit #i in DMA0_REQ_ENA register is
      --  set to 1; if bit #i = 0 , no change in DMA0_REQ_ENA register
      SET            : DMA0_REQ_ENA_SET_SET_Field := 16#0#;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMA0_REQ_ENA_SET_Register use record
      SET            at 0 range 0 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   subtype DMA0_REQ_ENA_CLR_CLR_Field is HAL.UInt23;

   --  Clear one or several bits in DMA0_REQ_ENA register
   type DMA0_REQ_ENA_CLR_Register is record
      --  Write-only. Write : If bit #i = 1, bit #i in DMA0_REQ_ENA register is
      --  reset to 0; if bit #i = 0 , no change in DMA0_REQ_ENA register
      CLR            : DMA0_REQ_ENA_CLR_CLR_Field := 16#0#;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMA0_REQ_ENA_CLR_Register use record
      CLR            at 0 range 0 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   subtype DMA1_REQ_ENA_REQ_ENA_Field is HAL.UInt10;

   --  Enable DMA1 requests
   type DMA1_REQ_ENA_Register is record
      --  Controls the 10 request inputs of DMA1. If bit i is '1' the DMA
      --  request input #i is enabled.
      REQ_ENA        : DMA1_REQ_ENA_REQ_ENA_Field := 16#3FF#;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMA1_REQ_ENA_Register use record
      REQ_ENA        at 0 range 0 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype DMA1_REQ_ENA_SET_SET_Field is HAL.UInt10;

   --  Set one or several bits in DMA1_REQ_ENA register
   type DMA1_REQ_ENA_SET_Register is record
      --  Write-only. Write : If bit #i = 1, bit #i in DMA1_REQ_ENA register is
      --  set to 1; if bit #i = 0 , no change in DMA1_REQ_ENA register
      SET            : DMA1_REQ_ENA_SET_SET_Field := 16#0#;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMA1_REQ_ENA_SET_Register use record
      SET            at 0 range 0 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype DMA1_REQ_ENA_CLR_CLR_Field is HAL.UInt10;

   --  Clear one or several bits in DMA1_REQ_ENA register
   type DMA1_REQ_ENA_CLR_Register is record
      --  Write-only. Write : If bit #i = 1, bit #i in DMA1_REQ_ENA register is
      --  reset to 0; if bit #i = 0 , no change in DMA1_REQ_ENA register
      CLR            : DMA1_REQ_ENA_CLR_CLR_Field := 16#0#;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMA1_REQ_ENA_CLR_Register use record
      CLR            at 0 range 0 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype DMA0_ITRIG_ENA_ITRIG_ENA_Field is HAL.UInt22;

   --  Enable DMA0 triggers
   type DMA0_ITRIG_ENA_Register is record
      --  Controls the 22 trigger inputs of DMA0. If bit i is '1' the DMA
      --  trigger input #i is enabled.
      ITRIG_ENA      : DMA0_ITRIG_ENA_ITRIG_ENA_Field := 16#3FFFFF#;
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMA0_ITRIG_ENA_Register use record
      ITRIG_ENA      at 0 range 0 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   subtype DMA0_ITRIG_ENA_SET_SET_Field is HAL.UInt22;

   --  Set one or several bits in DMA0_ITRIG_ENA register
   type DMA0_ITRIG_ENA_SET_Register is record
      --  Write-only. Write : If bit #i = 1, bit #i in DMA0_ITRIG_ENA register
      --  is set to 1; if bit #i = 0 , no change in DMA0_ITRIG_ENA register
      SET            : DMA0_ITRIG_ENA_SET_SET_Field := 16#0#;
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMA0_ITRIG_ENA_SET_Register use record
      SET            at 0 range 0 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   subtype DMA0_ITRIG_ENA_CLR_CLR_Field is HAL.UInt22;

   --  Clear one or several bits in DMA0_ITRIG_ENA register
   type DMA0_ITRIG_ENA_CLR_Register is record
      --  Write-only. Write : If bit #i = 1, bit #i in DMA0_ITRIG_ENA register
      --  is reset to 0; if bit #i = 0 , no change in DMA0_ITRIG_ENA register
      CLR            : DMA0_ITRIG_ENA_CLR_CLR_Field := 16#0#;
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMA0_ITRIG_ENA_CLR_Register use record
      CLR            at 0 range 0 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   subtype DMA1_ITRIG_ENA_ITRIG_ENA_Field is HAL.UInt15;

   --  Enable DMA1 triggers
   type DMA1_ITRIG_ENA_Register is record
      --  Controls the 15 trigger inputs of DMA1. If bit i is '1' the DMA
      --  trigger input #i is enabled.
      ITRIG_ENA      : DMA1_ITRIG_ENA_ITRIG_ENA_Field := 16#7FFF#;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMA1_ITRIG_ENA_Register use record
      ITRIG_ENA      at 0 range 0 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype DMA1_ITRIG_ENA_SET_SET_Field is HAL.UInt15;

   --  Set one or several bits in DMA1_ITRIG_ENA register
   type DMA1_ITRIG_ENA_SET_Register is record
      --  Write-only. Write : If bit #i = 1, bit #i in DMA1_ITRIG_ENA register
      --  is set to 1; if bit #i = 0 , no change in DMA1_ITRIG_ENA register
      SET            : DMA1_ITRIG_ENA_SET_SET_Field := 16#0#;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMA1_ITRIG_ENA_SET_Register use record
      SET            at 0 range 0 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype DMA1_ITRIG_ENA_CLR_CLR_Field is HAL.UInt15;

   --  Clear one or several bits in DMA1_ITRIG_ENA register
   type DMA1_ITRIG_ENA_CLR_Register is record
      --  Write-only. Write : If bit #i = 1, bit #i in DMA1_ITRIG_ENA register
      --  is reset to 0; if bit #i = 0 , no change in DMA1_ITRIG_ENA register
      CLR            : DMA1_ITRIG_ENA_CLR_CLR_Field := 16#0#;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMA1_ITRIG_ENA_CLR_Register use record
      CLR            at 0 range 0 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Input multiplexing (INPUT MUX)
   type INPUTMUX_Peripheral is record
      --  Input mux register for SCT0 input
      SCT0_INMUX         : aliased SCT0_INMUX_Registers;
      --  Capture select registers for TIMER0 inputs
      TIMER0CAPTSEL      : aliased TIMER0CAPTSEL_Registers;
      --  Capture select registers for TIMER1 inputs
      TIMER1CAPTSEL      : aliased TIMER1CAPTSEL_Registers;
      --  Capture select registers for TIMER2 inputs
      TIMER2CAPTSEL      : aliased TIMER2CAPTSEL_Registers;
      --  Pin interrupt select register
      PINTSEL            : aliased PINTSEL_Registers;
      --  Trigger select register for DMA0 channel
      DMA0_ITRIG_INMUX   : aliased DMA0_ITRIG_INMUX_Registers;
      --  DMA0 output trigger selection to become DMA0 trigger
      DMA0_OTRIG_INMUX   : aliased DMA0_OTRIG_INMUX_Registers;
      --  Selection for frequency measurement reference clock
      FREQMEAS_REF       : aliased FREQMEAS_REF_Register;
      --  Selection for frequency measurement target clock
      FREQMEAS_TARGET    : aliased FREQMEAS_TARGET_Register;
      --  Capture select registers for TIMER3 inputs
      TIMER3CAPTSEL      : aliased TIMER3CAPTSEL_Registers;
      --  Capture select registers for TIMER4 inputs
      TIMER4CAPTSEL      : aliased TIMER4CAPTSEL_Registers;
      --  Pin interrupt secure select register
      PINTSECSEL         : aliased PINTSECSEL_Registers;
      --  Trigger select register for DMA1 channel
      DMA1_ITRIG_INMUX   : aliased DMA1_ITRIG_INMUX_Registers;
      --  DMA1 output trigger selection to become DMA1 trigger
      DMA1_OTRIG_INMUX   : aliased DMA1_OTRIG_INMUX_Registers;
      --  Enable DMA0 requests
      DMA0_REQ_ENA       : aliased DMA0_REQ_ENA_Register;
      --  Set one or several bits in DMA0_REQ_ENA register
      DMA0_REQ_ENA_SET   : aliased DMA0_REQ_ENA_SET_Register;
      --  Clear one or several bits in DMA0_REQ_ENA register
      DMA0_REQ_ENA_CLR   : aliased DMA0_REQ_ENA_CLR_Register;
      --  Enable DMA1 requests
      DMA1_REQ_ENA       : aliased DMA1_REQ_ENA_Register;
      --  Set one or several bits in DMA1_REQ_ENA register
      DMA1_REQ_ENA_SET   : aliased DMA1_REQ_ENA_SET_Register;
      --  Clear one or several bits in DMA1_REQ_ENA register
      DMA1_REQ_ENA_CLR   : aliased DMA1_REQ_ENA_CLR_Register;
      --  Enable DMA0 triggers
      DMA0_ITRIG_ENA     : aliased DMA0_ITRIG_ENA_Register;
      --  Set one or several bits in DMA0_ITRIG_ENA register
      DMA0_ITRIG_ENA_SET : aliased DMA0_ITRIG_ENA_SET_Register;
      --  Clear one or several bits in DMA0_ITRIG_ENA register
      DMA0_ITRIG_ENA_CLR : aliased DMA0_ITRIG_ENA_CLR_Register;
      --  Enable DMA1 triggers
      DMA1_ITRIG_ENA     : aliased DMA1_ITRIG_ENA_Register;
      --  Set one or several bits in DMA1_ITRIG_ENA register
      DMA1_ITRIG_ENA_SET : aliased DMA1_ITRIG_ENA_SET_Register;
      --  Clear one or several bits in DMA1_ITRIG_ENA register
      DMA1_ITRIG_ENA_CLR : aliased DMA1_ITRIG_ENA_CLR_Register;
   end record
     with Volatile;

   for INPUTMUX_Peripheral use record
      SCT0_INMUX         at 16#0# range 0 .. 223;
      TIMER0CAPTSEL      at 16#20# range 0 .. 127;
      TIMER1CAPTSEL      at 16#40# range 0 .. 127;
      TIMER2CAPTSEL      at 16#60# range 0 .. 127;
      PINTSEL            at 16#C0# range 0 .. 255;
      DMA0_ITRIG_INMUX   at 16#E0# range 0 .. 735;
      DMA0_OTRIG_INMUX   at 16#160# range 0 .. 127;
      FREQMEAS_REF       at 16#180# range 0 .. 31;
      FREQMEAS_TARGET    at 16#184# range 0 .. 31;
      TIMER3CAPTSEL      at 16#1A0# range 0 .. 127;
      TIMER4CAPTSEL      at 16#1C0# range 0 .. 127;
      PINTSECSEL         at 16#1E0# range 0 .. 63;
      DMA1_ITRIG_INMUX   at 16#200# range 0 .. 319;
      DMA1_OTRIG_INMUX   at 16#240# range 0 .. 127;
      DMA0_REQ_ENA       at 16#740# range 0 .. 31;
      DMA0_REQ_ENA_SET   at 16#748# range 0 .. 31;
      DMA0_REQ_ENA_CLR   at 16#750# range 0 .. 31;
      DMA1_REQ_ENA       at 16#760# range 0 .. 31;
      DMA1_REQ_ENA_SET   at 16#768# range 0 .. 31;
      DMA1_REQ_ENA_CLR   at 16#770# range 0 .. 31;
      DMA0_ITRIG_ENA     at 16#780# range 0 .. 31;
      DMA0_ITRIG_ENA_SET at 16#788# range 0 .. 31;
      DMA0_ITRIG_ENA_CLR at 16#790# range 0 .. 31;
      DMA1_ITRIG_ENA     at 16#7A0# range 0 .. 31;
      DMA1_ITRIG_ENA_SET at 16#7A8# range 0 .. 31;
      DMA1_ITRIG_ENA_CLR at 16#7B0# range 0 .. 31;
   end record;

   --  Input multiplexing (INPUT MUX)
   INPUTMUX_Periph : aliased INPUTMUX_Peripheral
     with Import, Address => System'To_Address (16#40006000#);

end NXP_SVD.INPUTMUX;
