--  Copyright 2016-2019 NXP
--  All rights reserved.SPDX-License-Identifier: BSD-3-Clause


--  This spec has been automatically generated from LPC55S6x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NXP_SVD.PMC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Wake-up from DEEP POWER DOWN reset event (either from wake up I/O or RTC
   --  or OS Event Timer).
   type RESETCTRL_DPDWAKEUPRESETENABLE_Field is
     (
      --  Reset event from DEEP POWER DOWN mode is disable.
      Disable,
      --  Reset event from DEEP POWER DOWN mode is enable.
      Enable)
     with Size => 1;
   for RESETCTRL_DPDWAKEUPRESETENABLE_Field use
     (Disable => 0,
      Enable => 1);

   --  BOD VBAT reset enable.
   type RESETCTRL_BODVBATRESETENABLE_Field is
     (
      --  BOD VBAT reset is disable.
      Disable,
      --  BOD VBAT reset is enable.
      Enable)
     with Size => 1;
   for RESETCTRL_BODVBATRESETENABLE_Field use
     (Disable => 0,
      Enable => 1);

   --  Software reset enable.
   type RESETCTRL_SWRRESETENABLE_Field is
     (
      --  Software reset is disable.
      Disable,
      --  Software reset is enable.
      Enable)
     with Size => 1;
   for RESETCTRL_SWRRESETENABLE_Field use
     (Disable => 0,
      Enable => 1);

   --  Reset Control [Reset by: PoR, Pin Reset, Brown Out Detectors Reset, Deep
   --  Power Down Reset, Software Reset]
   type RESETCTRL_Register is record
      --  Wake-up from DEEP POWER DOWN reset event (either from wake up I/O or
      --  RTC or OS Event Timer).
      DPDWAKEUPRESETENABLE : RESETCTRL_DPDWAKEUPRESETENABLE_Field :=
                              NXP_SVD.PMC.Disable;
      --  BOD VBAT reset enable.
      BODVBATRESETENABLE   : RESETCTRL_BODVBATRESETENABLE_Field :=
                              NXP_SVD.PMC.Disable;
      --  unspecified
      Reserved_2_2         : HAL.Bit := 16#0#;
      --  Software reset enable.
      SWRRESETENABLE       : RESETCTRL_SWRRESETENABLE_Field :=
                              NXP_SVD.PMC.Disable;
      --  unspecified
      Reserved_4_31        : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RESETCTRL_Register use record
      DPDWAKEUPRESETENABLE at 0 range 0 .. 0;
      BODVBATRESETENABLE   at 0 range 1 .. 1;
      Reserved_2_2         at 0 range 2 .. 2;
      SWRRESETENABLE       at 0 range 3 .. 3;
      Reserved_4_31        at 0 range 4 .. 31;
   end record;

   --  BoD trigger level.
   type BODVBAT_TRIGLVL_Field is
     (
      --  1.00 V.
      V_1P00,
      --  1.10 V.
      V_1P10,
      --  1.20 V.
      V_1P20,
      --  1.30 V.
      V_1P30,
      --  1.40 V.
      V_1P40,
      --  1.50 V.
      V_1P50,
      --  1.60 V.
      V_1P60,
      --  1.65 V.
      V_1P65,
      --  1.70 V.
      V_1P70,
      --  1.75 V.
      V_1P75,
      --  1.80 V.
      V_1P80,
      --  1.90 V.
      V_1P90,
      --  2.00 V.
      V_2P00,
      --  2.10 V.
      V_2P10,
      --  2.20 V.
      V_2P20,
      --  2.30 V.
      V_2P30,
      --  2.40 V.
      V_2P40,
      --  2.50 V.
      V_2P50,
      --  2.60 V.
      V_2P60,
      --  2.70 V.
      V_2P70,
      --  2.806 V.
      V_2P80,
      --  2.90 V.
      V_2P90,
      --  3.00 V.
      V_3P00,
      --  3.10 V.
      V_3P10,
      --  3.20 V.
      V_3P20,
      --  3.30 V.
      V_3P30_2,
      --  3.30 V.
      V_3P30_3,
      --  3.30 V.
      V_3P30_4,
      --  3.30 V.
      V_3P30_5,
      --  3.30 V.
      V_3P30_6,
      --  3.30 V.
      V_3P30_7,
      --  3.30 V.
      V_3P30_8)
     with Size => 5;
   for BODVBAT_TRIGLVL_Field use
     (V_1P00 => 0,
      V_1P10 => 1,
      V_1P20 => 2,
      V_1P30 => 3,
      V_1P40 => 4,
      V_1P50 => 5,
      V_1P60 => 6,
      V_1P65 => 7,
      V_1P70 => 8,
      V_1P75 => 9,
      V_1P80 => 10,
      V_1P90 => 11,
      V_2P00 => 12,
      V_2P10 => 13,
      V_2P20 => 14,
      V_2P30 => 15,
      V_2P40 => 16,
      V_2P50 => 17,
      V_2P60 => 18,
      V_2P70 => 19,
      V_2P80 => 20,
      V_2P90 => 21,
      V_3P00 => 22,
      V_3P10 => 23,
      V_3P20 => 24,
      V_3P30_2 => 25,
      V_3P30_3 => 26,
      V_3P30_4 => 27,
      V_3P30_5 => 28,
      V_3P30_6 => 29,
      V_3P30_7 => 30,
      V_3P30_8 => 31);

   --  BoD Hysteresis control.
   type BODVBAT_HYST_Field is
     (
      --  25 mV.
      Hyst_25Mv,
      --  50 mV.
      Hyst_50Mv,
      --  75 mV.
      Hyst_75Mv,
      --  100 mV.
      Hyst_100Mv)
     with Size => 2;
   for BODVBAT_HYST_Field use
     (Hyst_25Mv => 0,
      Hyst_50Mv => 1,
      Hyst_75Mv => 2,
      Hyst_100Mv => 3);

   --  VBAT Brown Out Dectector (BoD) control register [Reset by: PoR, Pin
   --  Reset, Software Reset]
   type BODVBAT_Register is record
      --  BoD trigger level.
      TRIGLVL       : BODVBAT_TRIGLVL_Field := NXP_SVD.PMC.V_1P65;
      --  BoD Hysteresis control.
      HYST          : BODVBAT_HYST_Field := NXP_SVD.PMC.Hyst_75Mv;
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for BODVBAT_Register use record
      TRIGLVL       at 0 range 0 .. 4;
      HYST          at 0 range 5 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   --  Hysteris when hyst = '1'.
   type COMP_HYST_Field is
     (
      --  Hysteresis is disable.
      Disable,
      --  Hysteresis is enable.
      Enable)
     with Size => 1;
   for COMP_HYST_Field use
     (Disable => 0,
      Enable => 1);

   --  Dedicated control bit to select between internal VREF and VDDA (for the
   --  resistive ladder).
   type COMP_VREFINPUT_Field is
     (
      --  Select internal VREF.
      Internalref,
      --  Select VDDA.
      Vdda)
     with Size => 1;
   for COMP_VREFINPUT_Field use
     (Internalref => 0,
      Vdda => 1);

   --  Low power mode.
   type COMP_LOWPOWER_Field is
     (
      --  High speed mode.
      Highspeed,
      --  Low power mode (Low speed).
      Lowspeed)
     with Size => 1;
   for COMP_LOWPOWER_Field use
     (Highspeed => 0,
      Lowspeed => 1);

   --  Control word for P multiplexer:.
   type COMP_PMUX_Field is
     (
      --  VREF (See fiedl VREFINPUT).
      Vref,
      --  Pin P0_0.
      Cmp0_A,
      --  Pin P0_9.
      Cmp0_B,
      --  Pin P0_18.
      Cmp0_C,
      --  Pin P1_14.
      Cmp0_D,
      --  Pin P2_23.
      Cmp0_E)
     with Size => 3;
   for COMP_PMUX_Field use
     (Vref => 0,
      Cmp0_A => 1,
      Cmp0_B => 2,
      Cmp0_C => 3,
      Cmp0_D => 4,
      Cmp0_E => 5);

   --  Control word for N multiplexer:.
   type COMP_NMUX_Field is
     (
      --  VREF (See field VREFINPUT).
      Vref,
      --  Pin P0_0.
      Cmp0_A,
      --  Pin P0_9.
      Cmp0_B,
      --  Pin P0_18.
      Cmp0_C,
      --  Pin P1_14.
      Cmp0_D,
      --  Pin P2_23.
      Cmp0_E)
     with Size => 3;
   for COMP_NMUX_Field use
     (Vref => 0,
      Cmp0_A => 1,
      Cmp0_B => 2,
      Cmp0_C => 3,
      Cmp0_D => 4,
      Cmp0_E => 5);

   subtype COMP_VREF_Field is HAL.UInt5;
   subtype COMP_FILTERCGF_SAMPLEMODE_Field is HAL.UInt2;
   subtype COMP_FILTERCGF_CLKDIV_Field is HAL.UInt3;

   --  Analog Comparator control register [Reset by: PoR, Pin Reset, Brown Out
   --  Detectors Reset, Deep Power Down Reset, Software Reset]
   type COMP_Register is record
      --  unspecified
      Reserved_0_0         : HAL.Bit := 16#0#;
      --  Hysteris when hyst = '1'.
      HYST                 : COMP_HYST_Field := NXP_SVD.PMC.Enable;
      --  Dedicated control bit to select between internal VREF and VDDA (for
      --  the resistive ladder).
      VREFINPUT            : COMP_VREFINPUT_Field := NXP_SVD.PMC.Internalref;
      --  Low power mode.
      LOWPOWER             : COMP_LOWPOWER_Field := NXP_SVD.PMC.Lowspeed;
      --  Control word for P multiplexer:.
      PMUX                 : COMP_PMUX_Field := NXP_SVD.PMC.Vref;
      --  Control word for N multiplexer:.
      NMUX                 : COMP_NMUX_Field := NXP_SVD.PMC.Vref;
      --  Control reference voltage step, per steps of (VREFINPUT/31).
      VREF                 : COMP_VREF_Field := 16#0#;
      --  unspecified
      Reserved_15_15       : HAL.Bit := 16#0#;
      --  Filter Sample mode.
      FILTERCGF_SAMPLEMODE : COMP_FILTERCGF_SAMPLEMODE_Field := 16#0#;
      --  Filter Clock div .
      FILTERCGF_CLKDIV     : COMP_FILTERCGF_CLKDIV_Field := 16#0#;
      --  unspecified
      Reserved_21_31       : HAL.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for COMP_Register use record
      Reserved_0_0         at 0 range 0 .. 0;
      HYST                 at 0 range 1 .. 1;
      VREFINPUT            at 0 range 2 .. 2;
      LOWPOWER             at 0 range 3 .. 3;
      PMUX                 at 0 range 4 .. 6;
      NMUX                 at 0 range 7 .. 9;
      VREF                 at 0 range 10 .. 14;
      Reserved_15_15       at 0 range 15 .. 15;
      FILTERCGF_SAMPLEMODE at 0 range 16 .. 17;
      FILTERCGF_CLKDIV     at 0 range 18 .. 20;
      Reserved_21_31       at 0 range 21 .. 31;
   end record;

   --  Allows to identify Wake up I/O 0 as the wake-up source from Deep Power
   --  Down mode.
   type WAKEIOCAUSE_WAKEUP0_Field is
     (
      --  Last wake up from Deep Power down mode was NOT triggred by wake up
      --  I/O 0.
      Noevent,
      --  Last wake up from Deep Power down mode was triggred by wake up I/O 0.
      Event)
     with Size => 1;
   for WAKEIOCAUSE_WAKEUP0_Field use
     (Noevent => 0,
      Event => 1);

   --  Allows to identify Wake up I/O 1 as the wake-up source from Deep Power
   --  Down mode.
   type WAKEIOCAUSE_WAKEUP1_Field is
     (
      --  Last wake up from Deep Power down mode was NOT triggred by wake up
      --  I/O 1.
      Noevent,
      --  Last wake up from Deep Power down mode was triggred by wake up I/O 1.
      Event)
     with Size => 1;
   for WAKEIOCAUSE_WAKEUP1_Field use
     (Noevent => 0,
      Event => 1);

   --  Allows to identify Wake up I/O 2 as the wake-up source from Deep Power
   --  Down mode.
   type WAKEIOCAUSE_WAKEUP2_Field is
     (
      --  Last wake up from Deep Power down mode was NOT triggred by wake up
      --  I/O 2.
      Noevent,
      --  Last wake up from Deep Power down mode was triggred by wake up I/O 2.
      Event)
     with Size => 1;
   for WAKEIOCAUSE_WAKEUP2_Field use
     (Noevent => 0,
      Event => 1);

   --  Allows to identify Wake up I/O 3 as the wake-up source from Deep Power
   --  Down mode.
   type WAKEIOCAUSE_WAKEUP3_Field is
     (
      --  Last wake up from Deep Power down mode was NOT triggred by wake up
      --  I/O 3.
      Noevent,
      --  Last wake up from Deep Power down mode was triggred by wake up I/O 3.
      Event)
     with Size => 1;
   for WAKEIOCAUSE_WAKEUP3_Field use
     (Noevent => 0,
      Event => 1);

   --  Allows to identify the Wake-up I/O source from Deep Power Down mode
   type WAKEIOCAUSE_Register is record
      --  Read-only. Allows to identify Wake up I/O 0 as the wake-up source
      --  from Deep Power Down mode.
      WAKEUP0       : WAKEIOCAUSE_WAKEUP0_Field := NXP_SVD.PMC.Noevent;
      --  Allows to identify Wake up I/O 1 as the wake-up source from Deep
      --  Power Down mode.
      WAKEUP1       : WAKEIOCAUSE_WAKEUP1_Field := NXP_SVD.PMC.Noevent;
      --  Allows to identify Wake up I/O 2 as the wake-up source from Deep
      --  Power Down mode.
      WAKEUP2       : WAKEIOCAUSE_WAKEUP2_Field := NXP_SVD.PMC.Noevent;
      --  Allows to identify Wake up I/O 3 as the wake-up source from Deep
      --  Power Down mode.
      WAKEUP3       : WAKEIOCAUSE_WAKEUP3_Field := NXP_SVD.PMC.Noevent;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for WAKEIOCAUSE_Register use record
      WAKEUP0       at 0 range 0 .. 0;
      WAKEUP1       at 0 range 1 .. 1;
      WAKEUP2       at 0 range 2 .. 2;
      WAKEUP3       at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  XTAL32 KHZ oscillator oscillation failure detection indicator.
   type STATUSCLK_XTAL32KOSCFAILURE_Field is
     (
      --  No oscillation failure has been detetced since the last time this bit
      --  has been cleared..
      Nofail,
      --  At least one oscillation failure has been detetced since the last
      --  time this bit has been cleared..
      Failure)
     with Size => 1;
   for STATUSCLK_XTAL32KOSCFAILURE_Field use
     (Nofail => 0,
      Failure => 1);

   --  FRO and XTAL status register [Reset by: PoR, Brown Out Detectors Reset]
   type STATUSCLK_Register is record
      --  Read-only. XTAL oscillator 32 K OK signal.
      XTAL32KOK         : Boolean := False;
      --  unspecified
      Reserved_1_1      : HAL.Bit := 16#1#;
      --  XTAL32 KHZ oscillator oscillation failure detection indicator.
      XTAL32KOSCFAILURE : STATUSCLK_XTAL32KOSCFAILURE_Field :=
                           NXP_SVD.PMC.Failure;
      --  unspecified
      Reserved_3_31     : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for STATUSCLK_Register use record
      XTAL32KOK         at 0 range 0 .. 0;
      Reserved_1_1      at 0 range 1 .. 1;
      XTAL32KOSCFAILURE at 0 range 2 .. 2;
      Reserved_3_31     at 0 range 3 .. 31;
   end record;

   subtype AOREG1_BOOTERRORCOUNTER_Field is HAL.UInt4;

   --  General purpose always on domain data storage [Reset by: PoR, Brown Out
   --  Detectors Reset]
   type AOREG1_Register is record
      --  unspecified
      Reserved_0_3      : HAL.UInt4 := 16#0#;
      --  The last chip reset was caused by a Power On Reset.
      POR               : Boolean := False;
      --  The last chip reset was caused by a Pin Reset.
      PADRESET          : Boolean := False;
      --  The last chip reset was caused by a Brown Out Detector (BoD), either
      --  VBAT BoD or Core Logic BoD.
      BODRESET          : Boolean := False;
      --  The last chip reset was caused by a System Reset requested by the ARM
      --  CPU.
      SYSTEMRESET       : Boolean := False;
      --  The last chip reset was caused by the Watchdog Timer.
      WDTRESET          : Boolean := False;
      --  The last chip reset was caused by a Software event.
      SWRRESET          : Boolean := False;
      --  The last chip reset was caused by a Wake-up I/O reset event during a
      --  Deep Power-Down mode.
      DPDRESET_WAKEUPIO : Boolean := False;
      --  The last chip reset was caused by an RTC (either RTC Alarm or RTC
      --  wake up) reset event during a Deep Power-Down mode.
      DPDRESET_RTC      : Boolean := False;
      --  The last chip reset was caused by an OS Event Timer reset event
      --  during a Deep Power-Down mode.
      DPDRESET_OSTIMER  : Boolean := False;
      --  unspecified
      Reserved_13_15    : HAL.UInt3 := 16#0#;
      --  ROM Boot Fatal Error Counter.
      BOOTERRORCOUNTER  : AOREG1_BOOTERRORCOUNTER_Field := 16#0#;
      --  unspecified
      Reserved_20_31    : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for AOREG1_Register use record
      Reserved_0_3      at 0 range 0 .. 3;
      POR               at 0 range 4 .. 4;
      PADRESET          at 0 range 5 .. 5;
      BODRESET          at 0 range 6 .. 6;
      SYSTEMRESET       at 0 range 7 .. 7;
      WDTRESET          at 0 range 8 .. 8;
      SWRRESET          at 0 range 9 .. 9;
      DPDRESET_WAKEUPIO at 0 range 10 .. 10;
      DPDRESET_RTC      at 0 range 11 .. 11;
      DPDRESET_OSTIMER  at 0 range 12 .. 12;
      Reserved_13_15    at 0 range 13 .. 15;
      BOOTERRORCOUNTER  at 0 range 16 .. 19;
      Reserved_20_31    at 0 range 20 .. 31;
   end record;

   --  Select the 32K oscillator to be used in Deep Power Down Mode for the RTC
   --  (either XTAL32KHz or FRO32KHz) .
   type RTCOSC32K_SEL_Field is
     (
      --  FRO 32 KHz.
      Fro32K,
      --  XTAL 32KHz.
      Xtal32K)
     with Size => 1;
   for RTCOSC32K_SEL_Field use
     (Fro32K => 0,
      Xtal32K => 1);

   subtype RTCOSC32K_CLK1KHZDIV_Field is HAL.UInt3;
   subtype RTCOSC32K_CLK1HZDIV_Field is HAL.UInt11;

   --  RTC 1 KHZ and 1 Hz clocks source control register [Reset by: PoR, Brown
   --  Out Detectors Reset]
   type RTCOSC32K_Register is record
      --  Select the 32K oscillator to be used in Deep Power Down Mode for the
      --  RTC (either XTAL32KHz or FRO32KHz) .
      SEL                 : RTCOSC32K_SEL_Field := NXP_SVD.PMC.Fro32K;
      --  Actual division ratio is : 28 + CLK1KHZDIV.
      CLK1KHZDIV          : RTCOSC32K_CLK1KHZDIV_Field := 16#4#;
      --  unspecified
      Reserved_4_14       : HAL.UInt11 := 16#0#;
      --  RTC 1KHz clock Divider status flag.
      CLK1KHZDIVUPDATEREQ : Boolean := False;
      --  Actual division ratio is : 31744 + CLK1HZDIV.
      CLK1HZDIV           : RTCOSC32K_CLK1HZDIV_Field := 16#3FF#;
      --  unspecified
      Reserved_27_29      : HAL.UInt3 := 16#0#;
      --  Halts the divider counter.
      CLK1HZDIVHALT       : Boolean := False;
      --  RTC 1Hz Divider status flag.
      CLK1HZDIVUPDATEREQ  : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTCOSC32K_Register use record
      SEL                 at 0 range 0 .. 0;
      CLK1KHZDIV          at 0 range 1 .. 3;
      Reserved_4_14       at 0 range 4 .. 14;
      CLK1KHZDIVUPDATEREQ at 0 range 15 .. 15;
      CLK1HZDIV           at 0 range 16 .. 26;
      Reserved_27_29      at 0 range 27 .. 29;
      CLK1HZDIVHALT       at 0 range 30 .. 30;
      CLK1HZDIVUPDATEREQ  at 0 range 31 .. 31;
   end record;

   --  OS Timer control register [Reset by: PoR, Brown Out Detectors Reset]
   type OSTIMER_Register is record
      --  Active high reset.
      SOFTRESET       : Boolean := False;
      --  Enable OSTIMER 32 KHz clock.
      CLOCKENABLE     : Boolean := False;
      --  Wake up enable in Deep Power Down mode (To be used in Enable Deep
      --  Power Down mode).
      DPDWAKEUPENABLE : Boolean := False;
      --  Oscilator 32KHz (either FRO32KHz or XTAL32KHz according to RTCOSC32K.
      OSC32KPD        : Boolean := True;
      --  unspecified
      Reserved_4_31   : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OSTIMER_Register use record
      SOFTRESET       at 0 range 0 .. 0;
      CLOCKENABLE     at 0 range 1 .. 1;
      DPDWAKEUPENABLE at 0 range 2 .. 2;
      OSC32KPD        at 0 range 3 .. 3;
      Reserved_4_31   at 0 range 4 .. 31;
   end record;

   --  Controls power to VBAT Brown Out Detector (BOD).
   type PDRUNCFG0_PDEN_BODVBAT_Field is
     (
      --  BOD VBAT is powered.
      Poweredon,
      --  BOD VBAT is powered down.
      Poweredoff)
     with Size => 1;
   for PDRUNCFG0_PDEN_BODVBAT_Field use
     (Poweredon => 0,
      Poweredoff => 1);

   --  Controls power to the Free Running Oscillator (FRO) 32 KHz.
   type PDRUNCFG0_PDEN_FRO32K_Field is
     (
      --  FRO32KHz is powered.
      Poweredon,
      --  FRO32KHz is powered down.
      Poweredoff)
     with Size => 1;
   for PDRUNCFG0_PDEN_FRO32K_Field use
     (Poweredon => 0,
      Poweredoff => 1);

   --  Controls power to crystal 32 KHz.
   type PDRUNCFG0_PDEN_XTAL32K_Field is
     (
      --  Crystal 32KHz is powered.
      Poweredon,
      --  Crystal 32KHz is powered down.
      Poweredoff)
     with Size => 1;
   for PDRUNCFG0_PDEN_XTAL32K_Field use
     (Poweredon => 0,
      Poweredoff => 1);

   --  Controls power to crystal 32 MHz.
   type PDRUNCFG0_PDEN_XTAL32M_Field is
     (
      --  Crystal 32MHz is powered.
      Poweredon,
      --  Crystal 32MHz is powered down.
      Poweredoff)
     with Size => 1;
   for PDRUNCFG0_PDEN_XTAL32M_Field use
     (Poweredon => 0,
      Poweredoff => 1);

   --  Controls power to System PLL (also refered as PLL0).
   type PDRUNCFG0_PDEN_PLL0_Field is
     (
      --  PLL0 is powered.
      Poweredon,
      --  PLL0 is powered down.
      Poweredoff)
     with Size => 1;
   for PDRUNCFG0_PDEN_PLL0_Field use
     (Poweredon => 0,
      Poweredoff => 1);

   --  Controls power to USB PLL (also refered as PLL1).
   type PDRUNCFG0_PDEN_PLL1_Field is
     (
      --  PLL1 is powered.
      Poweredon,
      --  PLL1 is powered down.
      Poweredoff)
     with Size => 1;
   for PDRUNCFG0_PDEN_PLL1_Field use
     (Poweredon => 0,
      Poweredoff => 1);

   --  Controls power to USB Full Speed phy.
   type PDRUNCFG0_PDEN_USBFSPHY_Field is
     (
      --  USB Full Speed phy is powered.
      Poweredon,
      --  USB Full Speed phy is powered down.
      Poweredoff)
     with Size => 1;
   for PDRUNCFG0_PDEN_USBFSPHY_Field use
     (Poweredon => 0,
      Poweredoff => 1);

   --  Controls power to USB High Speed Phy.
   type PDRUNCFG0_PDEN_USBHSPHY_Field is
     (
      --  USB HS phy is powered.
      Poweredon,
      --  USB HS phy is powered down.
      Poweredoff)
     with Size => 1;
   for PDRUNCFG0_PDEN_USBHSPHY_Field use
     (Poweredon => 0,
      Poweredoff => 1);

   --  Controls power to Analog Comparator.
   type PDRUNCFG0_PDEN_COMP_Field is
     (
      --  Analog Comparator is powered.
      Poweredon,
      --  Analog Comparator is powered down.
      Poweredoff)
     with Size => 1;
   for PDRUNCFG0_PDEN_COMP_Field use
     (Poweredon => 0,
      Poweredoff => 1);

   --  Controls power to USB high speed LDO.
   type PDRUNCFG0_PDEN_LDOUSBHS_Field is
     (
      --  USB high speed LDO is powered.
      Poweredon,
      --  USB high speed LDO is powered down.
      Poweredoff)
     with Size => 1;
   for PDRUNCFG0_PDEN_LDOUSBHS_Field use
     (Poweredon => 0,
      Poweredoff => 1);

   --  Controls power to auxiliary biasing (AUXBIAS)
   type PDRUNCFG0_PDEN_AUXBIAS_Field is
     (
      --  auxiliary biasing is powered.
      Poweredon,
      --  auxiliary biasing is powered down.
      Poweredoff)
     with Size => 1;
   for PDRUNCFG0_PDEN_AUXBIAS_Field use
     (Poweredon => 0,
      Poweredoff => 1);

   --  Controls power to crystal 32 MHz LDO.
   type PDRUNCFG0_PDEN_LDOXO32M_Field is
     (
      --  crystal 32 MHz LDO is powered.
      Poweredon,
      --  crystal 32 MHz LDO is powered down.
      Poweredoff)
     with Size => 1;
   for PDRUNCFG0_PDEN_LDOXO32M_Field use
     (Poweredon => 0,
      Poweredoff => 1);

   --  Controls power to all True Random Number Genetaor (TRNG) clock sources.
   type PDRUNCFG0_PDEN_RNG_Field is
     (
      --  TRNG clocks are powered.
      Poweredon,
      --  TRNG clocks are powered down.
      Poweredoff)
     with Size => 1;
   for PDRUNCFG0_PDEN_RNG_Field use
     (Poweredon => 0,
      Poweredoff => 1);

   --  Controls power to System PLL (PLL0) Spread Spectrum module.
   type PDRUNCFG0_PDEN_PLL0_SSCG_Field is
     (
      --  PLL0 Sread spectrum module is powered.
      Poweredon,
      --  PLL0 Sread spectrum module is powered down.
      Poweredoff)
     with Size => 1;
   for PDRUNCFG0_PDEN_PLL0_SSCG_Field use
     (Poweredon => 0,
      Poweredoff => 1);

   --  Controls the power to various analog blocks [Reset by: PoR, Pin Reset,
   --  Brown Out Detectors Reset, Deep Power Down Reset, Software Reset]
   type PDRUNCFG0_Register is record
      --  unspecified
      Reserved_0_2   : HAL.UInt3 := 16#4#;
      --  Controls power to VBAT Brown Out Detector (BOD).
      PDEN_BODVBAT   : PDRUNCFG0_PDEN_BODVBAT_Field := NXP_SVD.PMC.Poweredon;
      --  unspecified
      Reserved_4_5   : HAL.UInt2 := 16#0#;
      --  Controls power to the Free Running Oscillator (FRO) 32 KHz.
      PDEN_FRO32K    : PDRUNCFG0_PDEN_FRO32K_Field := NXP_SVD.PMC.Poweredoff;
      --  Controls power to crystal 32 KHz.
      PDEN_XTAL32K   : PDRUNCFG0_PDEN_XTAL32K_Field := NXP_SVD.PMC.Poweredoff;
      --  Controls power to crystal 32 MHz.
      PDEN_XTAL32M   : PDRUNCFG0_PDEN_XTAL32M_Field := NXP_SVD.PMC.Poweredoff;
      --  Controls power to System PLL (also refered as PLL0).
      PDEN_PLL0      : PDRUNCFG0_PDEN_PLL0_Field := NXP_SVD.PMC.Poweredoff;
      --  Controls power to USB PLL (also refered as PLL1).
      PDEN_PLL1      : PDRUNCFG0_PDEN_PLL1_Field := NXP_SVD.PMC.Poweredoff;
      --  Controls power to USB Full Speed phy.
      PDEN_USBFSPHY  : PDRUNCFG0_PDEN_USBFSPHY_Field :=
                        NXP_SVD.PMC.Poweredoff;
      --  Controls power to USB High Speed Phy.
      PDEN_USBHSPHY  : PDRUNCFG0_PDEN_USBHSPHY_Field :=
                        NXP_SVD.PMC.Poweredoff;
      --  Controls power to Analog Comparator.
      PDEN_COMP      : PDRUNCFG0_PDEN_COMP_Field := NXP_SVD.PMC.Poweredoff;
      --  unspecified
      Reserved_14_17 : HAL.UInt4 := 16#B#;
      --  Controls power to USB high speed LDO.
      PDEN_LDOUSBHS  : PDRUNCFG0_PDEN_LDOUSBHS_Field :=
                        NXP_SVD.PMC.Poweredoff;
      --  Controls power to auxiliary biasing (AUXBIAS)
      PDEN_AUXBIAS   : PDRUNCFG0_PDEN_AUXBIAS_Field := NXP_SVD.PMC.Poweredoff;
      --  Controls power to crystal 32 MHz LDO.
      PDEN_LDOXO32M  : PDRUNCFG0_PDEN_LDOXO32M_Field :=
                        NXP_SVD.PMC.Poweredoff;
      --  unspecified
      Reserved_21_21 : HAL.Bit := 16#0#;
      --  Controls power to all True Random Number Genetaor (TRNG) clock
      --  sources.
      PDEN_RNG       : PDRUNCFG0_PDEN_RNG_Field := NXP_SVD.PMC.Poweredoff;
      --  Controls power to System PLL (PLL0) Spread Spectrum module.
      PDEN_PLL0_SSCG : PDRUNCFG0_PDEN_PLL0_SSCG_Field :=
                        NXP_SVD.PMC.Poweredoff;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PDRUNCFG0_Register use record
      Reserved_0_2   at 0 range 0 .. 2;
      PDEN_BODVBAT   at 0 range 3 .. 3;
      Reserved_4_5   at 0 range 4 .. 5;
      PDEN_FRO32K    at 0 range 6 .. 6;
      PDEN_XTAL32K   at 0 range 7 .. 7;
      PDEN_XTAL32M   at 0 range 8 .. 8;
      PDEN_PLL0      at 0 range 9 .. 9;
      PDEN_PLL1      at 0 range 10 .. 10;
      PDEN_USBFSPHY  at 0 range 11 .. 11;
      PDEN_USBHSPHY  at 0 range 12 .. 12;
      PDEN_COMP      at 0 range 13 .. 13;
      Reserved_14_17 at 0 range 14 .. 17;
      PDEN_LDOUSBHS  at 0 range 18 .. 18;
      PDEN_AUXBIAS   at 0 range 19 .. 19;
      PDEN_LDOXO32M  at 0 range 20 .. 20;
      Reserved_21_21 at 0 range 21 .. 21;
      PDEN_RNG       at 0 range 22 .. 22;
      PDEN_PLL0_SSCG at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  PMC
   type PMC_Peripheral is record
      --  Reset Control [Reset by: PoR, Pin Reset, Brown Out Detectors Reset,
      --  Deep Power Down Reset, Software Reset]
      RESETCTRL    : aliased RESETCTRL_Register;
      --  VBAT Brown Out Dectector (BoD) control register [Reset by: PoR, Pin
      --  Reset, Software Reset]
      BODVBAT      : aliased BODVBAT_Register;
      --  Analog Comparator control register [Reset by: PoR, Pin Reset, Brown
      --  Out Detectors Reset, Deep Power Down Reset, Software Reset]
      COMP         : aliased COMP_Register;
      --  Allows to identify the Wake-up I/O source from Deep Power Down mode
      WAKEIOCAUSE  : aliased WAKEIOCAUSE_Register;
      --  FRO and XTAL status register [Reset by: PoR, Brown Out Detectors
      --  Reset]
      STATUSCLK    : aliased STATUSCLK_Register;
      --  General purpose always on domain data storage [Reset by: PoR, Brown
      --  Out Detectors Reset]
      AOREG1       : aliased AOREG1_Register;
      --  RTC 1 KHZ and 1 Hz clocks source control register [Reset by: PoR,
      --  Brown Out Detectors Reset]
      RTCOSC32K    : aliased RTCOSC32K_Register;
      --  OS Timer control register [Reset by: PoR, Brown Out Detectors Reset]
      OSTIMER      : aliased OSTIMER_Register;
      --  Controls the power to various analog blocks [Reset by: PoR, Pin
      --  Reset, Brown Out Detectors Reset, Deep Power Down Reset, Software
      --  Reset]
      PDRUNCFG0    : aliased PDRUNCFG0_Register;
      --  Controls the power to various analog blocks [Reset by: PoR, Pin
      --  Reset, Brown Out Detectors Reset, Deep Power Down Reset, Software
      --  Reset]
      PDRUNCFGSET0 : aliased HAL.UInt32;
      --  Controls the power to various analog blocks [Reset by: PoR, Pin
      --  Reset, Brown Out Detectors Reset, Deep Power Down Reset, Software
      --  Reset]
      PDRUNCFGCLR0 : aliased HAL.UInt32;
   end record
     with Volatile;

   for PMC_Peripheral use record
      RESETCTRL    at 16#8# range 0 .. 31;
      BODVBAT      at 16#30# range 0 .. 31;
      COMP         at 16#50# range 0 .. 31;
      WAKEIOCAUSE  at 16#68# range 0 .. 31;
      STATUSCLK    at 16#74# range 0 .. 31;
      AOREG1       at 16#84# range 0 .. 31;
      RTCOSC32K    at 16#98# range 0 .. 31;
      OSTIMER      at 16#9C# range 0 .. 31;
      PDRUNCFG0    at 16#B8# range 0 .. 31;
      PDRUNCFGSET0 at 16#C0# range 0 .. 31;
      PDRUNCFGCLR0 at 16#C8# range 0 .. 31;
   end record;

   --  PMC
   PMC_Periph : aliased PMC_Peripheral
     with Import, Address => System'To_Address (16#40020000#);

end NXP_SVD.PMC;
