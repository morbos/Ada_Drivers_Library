--  Copyright 2016-2019 NXP
--  All rights reserved.SPDX-License-Identifier: BSD-3-Clause


--  This spec has been automatically generated from LPC55S6x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NXP_SVD.ANACTRL is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Flash Power Down status.
   type ANALOG_CTRL_STATUS_FLASH_PWRDWN_Field is
     (
      --  Flash is not in power down mode.
      Pwrup,
      --  Flash is in power down mode.
      Pwrdwn)
     with Size => 1;
   for ANALOG_CTRL_STATUS_FLASH_PWRDWN_Field use
     (Pwrup => 0,
      Pwrdwn => 1);

   --  Flash initialization error status.
   type ANALOG_CTRL_STATUS_FLASH_INIT_ERROR_Field is
     (
      --  No error.
      Noerror,
      --  At least one error occured during flash initialization..
      Error)
     with Size => 1;
   for ANALOG_CTRL_STATUS_FLASH_INIT_ERROR_Field use
     (Noerror => 0,
      Error => 1);

   --  Analog Macroblock Identity registers, Flash Status registers
   type ANALOG_CTRL_STATUS_Register is record
      --  unspecified
      Reserved_0_11    : HAL.UInt12;
      --  Read-only. Flash Power Down status.
      FLASH_PWRDWN     : ANALOG_CTRL_STATUS_FLASH_PWRDWN_Field;
      --  Read-only. Flash initialization error status.
      FLASH_INIT_ERROR : ANALOG_CTRL_STATUS_FLASH_INIT_ERROR_Field;
      --  unspecified
      Reserved_14_31   : HAL.UInt18;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ANALOG_CTRL_STATUS_Register use record
      Reserved_0_11    at 0 range 0 .. 11;
      FLASH_PWRDWN     at 0 range 12 .. 12;
      FLASH_INIT_ERROR at 0 range 13 .. 13;
      Reserved_14_31   at 0 range 14 .. 31;
   end record;

   subtype FREQ_ME_CTRL_CAPVAL_SCALE_Field is HAL.UInt31;

   --  Frequency Measure function control register
   type FREQ_ME_CTRL_Register is record
      --  Frequency measure result /Frequency measur scale
      CAPVAL_SCALE : FREQ_ME_CTRL_CAPVAL_SCALE_Field := 16#0#;
      --  Set this bit to one to initiate a frequency measurement cycle.
      --  Hardware clears this bit when the measurement cycle has completed and
      --  there is valid capture data in the CAPVAL field (bits 30:0).
      PROG         : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FREQ_ME_CTRL_Register use record
      CAPVAL_SCALE at 0 range 0 .. 30;
      PROG         at 0 range 31 .. 31;
   end record;

   --  12 MHz clock control.
   type FRO192M_CTRL_ENA_12MHZCLK_Field is
     (
      --  12 MHz clock is disabled.
      Disable,
      --  12 MHz clock is enabled.
      Enable)
     with Size => 1;
   for FRO192M_CTRL_ENA_12MHZCLK_Field use
     (Disable => 0,
      Enable => 1);

   --  48 MHz clock control.
   type FRO192M_CTRL_ENA_48MHZCLK_Field is
     (
      --  48 MHz clock is enabled.
      Enable)
     with Size => 1;
   for FRO192M_CTRL_ENA_48MHZCLK_Field use
     (Enable => 1);

   subtype FRO192M_CTRL_DAC_TRIM_Field is HAL.UInt8;

   --  96 MHz clock control.
   type FRO192M_CTRL_ENA_96MHZCLK_Field is
     (
      --  96 MHz clock is disabled.
      Disable,
      --  96 MHz clock is enabled.
      Enable)
     with Size => 1;
   for FRO192M_CTRL_ENA_96MHZCLK_Field use
     (Disable => 0,
      Enable => 1);

   --  192MHz Free Running OScillator (FRO) Control register
   type FRO192M_CTRL_Register is record
      --  unspecified
      Reserved_0_13  : HAL.UInt14 := 16#101A#;
      --  12 MHz clock control.
      ENA_12MHZCLK   : FRO192M_CTRL_ENA_12MHZCLK_Field :=
                        NXP_SVD.ANACTRL.Enable;
      --  48 MHz clock control.
      ENA_48MHZCLK   : FRO192M_CTRL_ENA_48MHZCLK_Field :=
                        NXP_SVD.ANACTRL.Enable;
      --  Frequency trim.
      DAC_TRIM       : FRO192M_CTRL_DAC_TRIM_Field := 16#80#;
      --  If this bit is set and the USB peripheral is enabled into full speed
      --  device mode, the USB block will provide FRO clock adjustments to lock
      --  it to the host clock using the SOF packets.
      USBCLKADJ      : Boolean := False;
      --  Read-only. If it reads as 1 when reading the DAC_TRIM field and
      --  USBCLKADJ=1, it should be re-read until it is 0.
      USBMODCHG      : Boolean := False;
      --  unspecified
      Reserved_26_29 : HAL.UInt4 := 16#0#;
      --  96 MHz clock control.
      ENA_96MHZCLK   : FRO192M_CTRL_ENA_96MHZCLK_Field :=
                        NXP_SVD.ANACTRL.Disable;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FRO192M_CTRL_Register use record
      Reserved_0_13  at 0 range 0 .. 13;
      ENA_12MHZCLK   at 0 range 14 .. 14;
      ENA_48MHZCLK   at 0 range 15 .. 15;
      DAC_TRIM       at 0 range 16 .. 23;
      USBCLKADJ      at 0 range 24 .. 24;
      USBMODCHG      at 0 range 25 .. 25;
      Reserved_26_29 at 0 range 26 .. 29;
      ENA_96MHZCLK   at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   --  Output clock valid signal. Indicates that CCO clock has settled.
   type FRO192M_STATUS_CLK_VALID_Field is
     (
      --  No output clock present (None of 12 MHz, 48 MHz or 96 MHz clock is
      --  available).
      Noclkout,
      --  Clock is present (12 MHz, 48 MHz or 96 MHz can be output if they are
      --  enable respectively by
      --  FRO192M_CTRL.ENA_12MHZCLK/ENA_48MHZCLK/ENA_96MHZCLK).
      Clkout)
     with Size => 1;
   for FRO192M_STATUS_CLK_VALID_Field use
     (Noclkout => 0,
      Clkout => 1);

   --  192MHz Free Running OScillator (FRO) Status register
   type FRO192M_STATUS_Register is record
      --  Read-only. Output clock valid signal. Indicates that CCO clock has
      --  settled.
      CLK_VALID     : FRO192M_STATUS_CLK_VALID_Field;
      --  Read-only. CCO threshold voltage detector output (signal vcco_ok).
      --  Once the CCO voltage crosses the threshold voltage of a SLVT
      --  transistor, this output signal will go high. It is also possible to
      --  observe the clk_valid signal.
      ATB_VCTRL     : Boolean;
      --  unspecified
      Reserved_2_31 : HAL.UInt30;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FRO192M_STATUS_Register use record
      CLK_VALID     at 0 range 0 .. 0;
      ATB_VCTRL     at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  Bypass enable of XO AC buffer enable in pll and top level.
   type XO32M_CTRL_ACBUF_PASS_ENABLE_Field is
     (
      --  XO AC buffer bypass is disabled.
      Disable,
      --  XO AC buffer bypass is enabled.
      Enable)
     with Size => 1;
   for XO32M_CTRL_ACBUF_PASS_ENABLE_Field use
     (Disable => 0,
      Enable => 1);

   --  Enable High speed Crystal oscillator output to USB HS PLL.
   type XO32M_CTRL_ENABLE_PLL_USB_OUT_Field is
     (
      --  High speed Crystal oscillator output to USB HS PLL is disabled.
      Disable,
      --  High speed Crystal oscillator output to USB HS PLL is enabled.
      Enable)
     with Size => 1;
   for XO32M_CTRL_ENABLE_PLL_USB_OUT_Field use
     (Disable => 0,
      Enable => 1);

   --  Enable XO 32 MHz output to CPU system.
   type XO32M_CTRL_ENABLE_SYSTEM_CLK_OUT_Field is
     (
      --  High speed Crystal oscillator output to CPU system is disabled.
      Disable,
      --  High speed Crystal oscillator output to CPU system is enabled.
      Enable)
     with Size => 1;
   for XO32M_CTRL_ENABLE_SYSTEM_CLK_OUT_Field use
     (Disable => 0,
      Enable => 1);

   --  High speed Crystal Oscillator Control register
   type XO32M_CTRL_Register is record
      --  unspecified
      Reserved_0_21         : HAL.UInt22 := 16#21428A#;
      --  Bypass enable of XO AC buffer enable in pll and top level.
      ACBUF_PASS_ENABLE     : XO32M_CTRL_ACBUF_PASS_ENABLE_Field :=
                               NXP_SVD.ANACTRL.Disable;
      --  Enable High speed Crystal oscillator output to USB HS PLL.
      ENABLE_PLL_USB_OUT    : XO32M_CTRL_ENABLE_PLL_USB_OUT_Field :=
                               NXP_SVD.ANACTRL.Disable;
      --  Enable XO 32 MHz output to CPU system.
      ENABLE_SYSTEM_CLK_OUT : XO32M_CTRL_ENABLE_SYSTEM_CLK_OUT_Field :=
                               NXP_SVD.ANACTRL.Disable;
      --  unspecified
      Reserved_25_31        : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for XO32M_CTRL_Register use record
      Reserved_0_21         at 0 range 0 .. 21;
      ACBUF_PASS_ENABLE     at 0 range 22 .. 22;
      ENABLE_PLL_USB_OUT    at 0 range 23 .. 23;
      ENABLE_SYSTEM_CLK_OUT at 0 range 24 .. 24;
      Reserved_25_31        at 0 range 25 .. 31;
   end record;

   --  Indicates XO out frequency statibilty.
   type XO32M_STATUS_XO_READY_Field is
     (
      --  XO output frequency is not yet stable.
      Not_Stable,
      --  XO output frequency is stable.
      Stable)
     with Size => 1;
   for XO32M_STATUS_XO_READY_Field use
     (Not_Stable => 0,
      Stable => 1);

   --  High speed Crystal Oscillator Status register
   type XO32M_STATUS_Register is record
      --  Read-only. Indicates XO out frequency statibilty.
      XO_READY      : XO32M_STATUS_XO_READY_Field;
      --  unspecified
      Reserved_1_31 : HAL.UInt31;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for XO32M_STATUS_Register use record
      XO_READY      at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  BOD VBAT interrupt control.
   type BOD_DCDC_INT_CTRL_BODVBAT_INT_ENABLE_Field is
     (
      --  BOD VBAT interrupt is disabled.
      Disable,
      --  BOD VBAT interrupt is enabled.
      Enable)
     with Size => 1;
   for BOD_DCDC_INT_CTRL_BODVBAT_INT_ENABLE_Field use
     (Disable => 0,
      Enable => 1);

   --  BOD CORE interrupt control.
   type BOD_DCDC_INT_CTRL_BODCORE_INT_ENABLE_Field is
     (
      --  BOD CORE interrupt is disabled.
      Disable,
      --  BOD CORE interrupt is enabled.
      Enable)
     with Size => 1;
   for BOD_DCDC_INT_CTRL_BODCORE_INT_ENABLE_Field use
     (Disable => 0,
      Enable => 1);

   --  DCDC interrupt control.
   type BOD_DCDC_INT_CTRL_DCDC_INT_ENABLE_Field is
     (
      --  DCDC interrupt is disabled.
      Disable,
      --  DCDC interrupt is enabled.
      Enable)
     with Size => 1;
   for BOD_DCDC_INT_CTRL_DCDC_INT_ENABLE_Field use
     (Disable => 0,
      Enable => 1);

   --  Brown Out Detectors (BoDs) & DCDC interrupts generation control register
   type BOD_DCDC_INT_CTRL_Register is record
      --  BOD VBAT interrupt control.
      BODVBAT_INT_ENABLE : BOD_DCDC_INT_CTRL_BODVBAT_INT_ENABLE_Field :=
                            NXP_SVD.ANACTRL.Disable;
      --  BOD VBAT interrupt clear.1: Clear the interrupt. Self-cleared bit.
      BODVBAT_INT_CLEAR  : Boolean := False;
      --  BOD CORE interrupt control.
      BODCORE_INT_ENABLE : BOD_DCDC_INT_CTRL_BODCORE_INT_ENABLE_Field :=
                            NXP_SVD.ANACTRL.Disable;
      --  BOD CORE interrupt clear.1: Clear the interrupt. Self-cleared bit.
      BODCORE_INT_CLEAR  : Boolean := False;
      --  DCDC interrupt control.
      DCDC_INT_ENABLE    : BOD_DCDC_INT_CTRL_DCDC_INT_ENABLE_Field :=
                            NXP_SVD.ANACTRL.Disable;
      --  DCDC interrupt clear.1: Clear the interrupt. Self-cleared bit.
      DCDC_INT_CLEAR     : Boolean := False;
      --  unspecified
      Reserved_6_31      : HAL.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for BOD_DCDC_INT_CTRL_Register use record
      BODVBAT_INT_ENABLE at 0 range 0 .. 0;
      BODVBAT_INT_CLEAR  at 0 range 1 .. 1;
      BODCORE_INT_ENABLE at 0 range 2 .. 2;
      BODCORE_INT_CLEAR  at 0 range 3 .. 3;
      DCDC_INT_ENABLE    at 0 range 4 .. 4;
      DCDC_INT_CLEAR     at 0 range 5 .. 5;
      Reserved_6_31      at 0 range 6 .. 31;
   end record;

   --  BOD VBAT Interrupt status before Interrupt Enable.
   type BOD_DCDC_INT_STATUS_BODVBAT_STATUS_Field is
     (
      --  No interrupt pending..
      Not_Pending,
      --  Interrupt pending..
      Pending)
     with Size => 1;
   for BOD_DCDC_INT_STATUS_BODVBAT_STATUS_Field use
     (Not_Pending => 0,
      Pending => 1);

   --  BOD VBAT Interrupt status after Interrupt Enable.
   type BOD_DCDC_INT_STATUS_BODVBAT_INT_STATUS_Field is
     (
      --  No interrupt pending..
      Not_Pending,
      --  Interrupt pending..
      Pending)
     with Size => 1;
   for BOD_DCDC_INT_STATUS_BODVBAT_INT_STATUS_Field use
     (Not_Pending => 0,
      Pending => 1);

   --  Current value of BOD VBAT power status output.
   type BOD_DCDC_INT_STATUS_BODVBAT_VAL_Field is
     (
      --  VBAT voltage level is below the threshold.
      Not_Ok,
      --  VBAT voltage level is above the threshold.
      Ok)
     with Size => 1;
   for BOD_DCDC_INT_STATUS_BODVBAT_VAL_Field use
     (Not_Ok => 0,
      Ok => 1);

   --  BOD CORE Interrupt status before Interrupt Enable.
   type BOD_DCDC_INT_STATUS_BODCORE_STATUS_Field is
     (
      --  No interrupt pending..
      Not_Pending,
      --  Interrupt pending..
      Pending)
     with Size => 1;
   for BOD_DCDC_INT_STATUS_BODCORE_STATUS_Field use
     (Not_Pending => 0,
      Pending => 1);

   --  BOD CORE Interrupt status after Interrupt Enable.
   type BOD_DCDC_INT_STATUS_BODCORE_INT_STATUS_Field is
     (
      --  No interrupt pending..
      Not_Pending,
      --  Interrupt pending..
      Pending)
     with Size => 1;
   for BOD_DCDC_INT_STATUS_BODCORE_INT_STATUS_Field use
     (Not_Pending => 0,
      Pending => 1);

   --  Current value of BOD CORE power status output.
   type BOD_DCDC_INT_STATUS_BODCORE_VAL_Field is
     (
      --  CORE voltage level is below the threshold.
      Not_Ok,
      --  CORE voltage level is above the threshold.
      Ok)
     with Size => 1;
   for BOD_DCDC_INT_STATUS_BODCORE_VAL_Field use
     (Not_Ok => 0,
      Ok => 1);

   --  DCDC Interrupt status before Interrupt Enable.
   type BOD_DCDC_INT_STATUS_DCDC_STATUS_Field is
     (
      --  No interrupt pending..
      Not_Pending,
      --  Interrupt pending..
      Pending)
     with Size => 1;
   for BOD_DCDC_INT_STATUS_DCDC_STATUS_Field use
     (Not_Pending => 0,
      Pending => 1);

   --  DCDC Interrupt status after Interrupt Enable.
   type BOD_DCDC_INT_STATUS_DCDC_INT_STATUS_Field is
     (
      --  No interrupt pending..
      Not_Pending,
      --  Interrupt pending..
      Pending)
     with Size => 1;
   for BOD_DCDC_INT_STATUS_DCDC_INT_STATUS_Field use
     (Not_Pending => 0,
      Pending => 1);

   --  Current value of DCDC power status output.
   type BOD_DCDC_INT_STATUS_DCDC_VAL_Field is
     (
      --  DCDC output Voltage is below the targeted regulation level.
      Not_Ok,
      --  DCDC output Voltage is above the targeted regulation level.
      Ok)
     with Size => 1;
   for BOD_DCDC_INT_STATUS_DCDC_VAL_Field use
     (Not_Ok => 0,
      Ok => 1);

   --  BoDs & DCDC interrupts status register
   type BOD_DCDC_INT_STATUS_Register is record
      --  Read-only. BOD VBAT Interrupt status before Interrupt Enable.
      BODVBAT_STATUS     : BOD_DCDC_INT_STATUS_BODVBAT_STATUS_Field;
      --  Read-only. BOD VBAT Interrupt status after Interrupt Enable.
      BODVBAT_INT_STATUS : BOD_DCDC_INT_STATUS_BODVBAT_INT_STATUS_Field;
      --  Read-only. Current value of BOD VBAT power status output.
      BODVBAT_VAL        : BOD_DCDC_INT_STATUS_BODVBAT_VAL_Field;
      --  Read-only. BOD CORE Interrupt status before Interrupt Enable.
      BODCORE_STATUS     : BOD_DCDC_INT_STATUS_BODCORE_STATUS_Field;
      --  Read-only. BOD CORE Interrupt status after Interrupt Enable.
      BODCORE_INT_STATUS : BOD_DCDC_INT_STATUS_BODCORE_INT_STATUS_Field;
      --  Read-only. Current value of BOD CORE power status output.
      BODCORE_VAL        : BOD_DCDC_INT_STATUS_BODCORE_VAL_Field;
      --  Read-only. DCDC Interrupt status before Interrupt Enable.
      DCDC_STATUS        : BOD_DCDC_INT_STATUS_DCDC_STATUS_Field;
      --  Read-only. DCDC Interrupt status after Interrupt Enable.
      DCDC_INT_STATUS    : BOD_DCDC_INT_STATUS_DCDC_INT_STATUS_Field;
      --  Read-only. Current value of DCDC power status output.
      DCDC_VAL           : BOD_DCDC_INT_STATUS_DCDC_VAL_Field;
      --  unspecified
      Reserved_9_31      : HAL.UInt23;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for BOD_DCDC_INT_STATUS_Register use record
      BODVBAT_STATUS     at 0 range 0 .. 0;
      BODVBAT_INT_STATUS at 0 range 1 .. 1;
      BODVBAT_VAL        at 0 range 2 .. 2;
      BODCORE_STATUS     at 0 range 3 .. 3;
      BODCORE_INT_STATUS at 0 range 4 .. 4;
      BODCORE_VAL        at 0 range 5 .. 5;
      DCDC_STATUS        at 0 range 6 .. 6;
      DCDC_INT_STATUS    at 0 range 7 .. 7;
      DCDC_VAL           at 0 range 8 .. 8;
      Reserved_9_31      at 0 range 9 .. 31;
   end record;

   --  Select short or long ringo (for all ringos types).
   type RINGO0_CTRL_SL_Field is
     (
      --  Select short ringo (few elements).
      Short,
      --  Select long ringo (many elements).
      Long)
     with Size => 1;
   for RINGO0_CTRL_SL_Field use
     (Short => 0,
      Long => 1);

   --  Ringo frequency output divider.
   type RINGO0_CTRL_FS_Field is
     (
      --  High frequency output (frequency lower than 100 MHz).
      Fast,
      --  Low frequency output (frequency lower than 10 MHz).
      Slow)
     with Size => 1;
   for RINGO0_CTRL_FS_Field use
     (Fast => 0,
      Slow => 1);

   --  PN-Ringos (P-Transistor and N-Transistor processing) control.
   type RINGO0_CTRL_SWN_SWP_Field is
     (
      --  Normal mode.
      Normal,
      --  P-Monitor mode. Measure with weak P transistor.
      P_Monitor,
      --  P-Monitor mode. Measure with weak N transistor.
      N_Monitor,
      --  Don't use.
      Forbidden)
     with Size => 2;
   for RINGO0_CTRL_SWN_SWP_Field use
     (Normal => 0,
      P_Monitor => 1,
      N_Monitor => 2,
      Forbidden => 3);

   --  Ringo module Power control.
   type RINGO0_CTRL_PD_Field is
     (
      --  The Ringo module is enabled.
      Powered_On,
      --  The Ringo module is disabled.
      Powered_Down)
     with Size => 1;
   for RINGO0_CTRL_PD_Field use
     (Powered_On => 0,
      Powered_Down => 1);

   --  First NAND2-based ringo control.
   type RINGO0_CTRL_E_ND0_Field is
     (
      --  First NAND2-based ringo is disabled.
      Disable,
      --  First NAND2-based ringo is enabled.
      Enable)
     with Size => 1;
   for RINGO0_CTRL_E_ND0_Field use
     (Disable => 0,
      Enable => 1);

   --  Second NAND2-based ringo control.
   type RINGO0_CTRL_E_ND1_Field is
     (
      --  Second NAND2-based ringo is disabled.
      Disable,
      --  Second NAND2-based ringo is enabled.
      Enable)
     with Size => 1;
   for RINGO0_CTRL_E_ND1_Field use
     (Disable => 0,
      Enable => 1);

   --  First NOR2-based ringo control.
   type RINGO0_CTRL_E_NR0_Field is
     (
      --  First NOR2-based ringo is disabled.
      Disable,
      --  First NOR2-based ringo is enabled.
      Enable)
     with Size => 1;
   for RINGO0_CTRL_E_NR0_Field use
     (Disable => 0,
      Enable => 1);

   --  Second NOR2-based ringo control.
   type RINGO0_CTRL_E_NR1_Field is
     (
      --  Second NORD2-based ringo is disabled.
      Disable,
      --  Second NORD2-based ringo is enabled.
      Enable)
     with Size => 1;
   for RINGO0_CTRL_E_NR1_Field use
     (Disable => 0,
      Enable => 1);

   --  First Inverter-based ringo control.
   type RINGO0_CTRL_E_IV0_Field is
     (
      --  First INV-based ringo is disabled.
      Disable,
      --  First INV-based ringo is enabled.
      Enable)
     with Size => 1;
   for RINGO0_CTRL_E_IV0_Field use
     (Disable => 0,
      Enable => 1);

   --  Second Inverter-based ringo control.
   type RINGO0_CTRL_E_IV1_Field is
     (
      --  Second INV-based ringo is disabled.
      Disable,
      --  Second INV-based ringo is enabled.
      Enable)
     with Size => 1;
   for RINGO0_CTRL_E_IV1_Field use
     (Disable => 0,
      Enable => 1);

   --  First PN (P-Transistor and N-Transistor processing) monitor control.
   type RINGO0_CTRL_E_PN0_Field is
     (
      --  First PN-based ringo is disabled.
      Disable,
      --  First PN-based ringo is enabled.
      Enable)
     with Size => 1;
   for RINGO0_CTRL_E_PN0_Field use
     (Disable => 0,
      Enable => 1);

   --  Second PN (P-Transistor and N-Transistor processing) monitor control.
   type RINGO0_CTRL_E_PN1_Field is
     (
      --  Second PN-based ringo is disabled.
      Disable,
      --  Second PN-based ringo is enabled.
      Enable)
     with Size => 1;
   for RINGO0_CTRL_E_PN1_Field use
     (Disable => 0,
      Enable => 1);

   subtype RINGO0_CTRL_DIVISOR_Field is HAL.UInt4;

   --  First Ring Oscillator module control register.
   type RINGO0_CTRL_Register is record
      --  Select short or long ringo (for all ringos types).
      SL             : RINGO0_CTRL_SL_Field := NXP_SVD.ANACTRL.Short;
      --  Ringo frequency output divider.
      FS             : RINGO0_CTRL_FS_Field := NXP_SVD.ANACTRL.Fast;
      --  PN-Ringos (P-Transistor and N-Transistor processing) control.
      SWN_SWP        : RINGO0_CTRL_SWN_SWP_Field := NXP_SVD.ANACTRL.Normal;
      --  Ringo module Power control.
      PD             : RINGO0_CTRL_PD_Field := NXP_SVD.ANACTRL.Powered_On;
      --  First NAND2-based ringo control.
      E_ND0          : RINGO0_CTRL_E_ND0_Field := NXP_SVD.ANACTRL.Disable;
      --  Second NAND2-based ringo control.
      E_ND1          : RINGO0_CTRL_E_ND1_Field := NXP_SVD.ANACTRL.Enable;
      --  First NOR2-based ringo control.
      E_NR0          : RINGO0_CTRL_E_NR0_Field := NXP_SVD.ANACTRL.Disable;
      --  Second NOR2-based ringo control.
      E_NR1          : RINGO0_CTRL_E_NR1_Field := NXP_SVD.ANACTRL.Disable;
      --  First Inverter-based ringo control.
      E_IV0          : RINGO0_CTRL_E_IV0_Field := NXP_SVD.ANACTRL.Disable;
      --  Second Inverter-based ringo control.
      E_IV1          : RINGO0_CTRL_E_IV1_Field := NXP_SVD.ANACTRL.Disable;
      --  First PN (P-Transistor and N-Transistor processing) monitor control.
      E_PN0          : RINGO0_CTRL_E_PN0_Field := NXP_SVD.ANACTRL.Disable;
      --  Second PN (P-Transistor and N-Transistor processing) monitor control.
      E_PN1          : RINGO0_CTRL_E_PN1_Field := NXP_SVD.ANACTRL.Disable;
      --  unspecified
      Reserved_13_15 : HAL.UInt3 := 16#0#;
      --  Ringo out Clock divider value. Frequency Output = Frequency input /
      --  (DIViSOR+1). (minimum = Frequency input / 16)
      DIVISOR        : RINGO0_CTRL_DIVISOR_Field := 16#0#;
      --  unspecified
      Reserved_20_30 : HAL.UInt11 := 16#0#;
      --  Read-only. Ringo clock out Divider status flag. Set when a change is
      --  made to the divider value, cleared when the change is complete.
      DIV_UPDATE_REQ : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RINGO0_CTRL_Register use record
      SL             at 0 range 0 .. 0;
      FS             at 0 range 1 .. 1;
      SWN_SWP        at 0 range 2 .. 3;
      PD             at 0 range 4 .. 4;
      E_ND0          at 0 range 5 .. 5;
      E_ND1          at 0 range 6 .. 6;
      E_NR0          at 0 range 7 .. 7;
      E_NR1          at 0 range 8 .. 8;
      E_IV0          at 0 range 9 .. 9;
      E_IV1          at 0 range 10 .. 10;
      E_PN0          at 0 range 11 .. 11;
      E_PN1          at 0 range 12 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      DIVISOR        at 0 range 16 .. 19;
      Reserved_20_30 at 0 range 20 .. 30;
      DIV_UPDATE_REQ at 0 range 31 .. 31;
   end record;

   --  Select short or long ringo (for all ringos types).
   type RINGO1_CTRL_S_Field is
     (
      --  Select short ringo (few elements).
      Short,
      --  Select long ringo (many elements).
      Long)
     with Size => 1;
   for RINGO1_CTRL_S_Field use
     (Short => 0,
      Long => 1);

   --  Ringo frequency output divider.
   type RINGO1_CTRL_FS_Field is
     (
      --  High frequency output (frequency lower than 100 MHz).
      Fast,
      --  Low frequency output (frequency lower than 10 MHz).
      Slow)
     with Size => 1;
   for RINGO1_CTRL_FS_Field use
     (Fast => 0,
      Slow => 1);

   --  Ringo module Power control.
   type RINGO1_CTRL_PD_Field is
     (
      --  The Ringo module is enabled.
      Powered_On,
      --  The Ringo module is disabled.
      Powered_Down)
     with Size => 1;
   for RINGO1_CTRL_PD_Field use
     (Powered_On => 0,
      Powered_Down => 1);

   --  .
   type RINGO1_CTRL_E_R24_Field is
     (
      --  Ringo is disabled.
      Disable,
      --  Ringo is enabled.
      Enable)
     with Size => 1;
   for RINGO1_CTRL_E_R24_Field use
     (Disable => 0,
      Enable => 1);

   --  RINGO1_CTRL_E_R array
   type RINGO1_CTRL_E_R_Field_Array is array (24 .. 25)
     of RINGO1_CTRL_E_R24_Field
     with Component_Size => 1, Size => 2;

   --  Type definition for RINGO1_CTRL_E_R
   type RINGO1_CTRL_E_R_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  E_R as a value
            Val : HAL.UInt2;
         when True =>
            --  E_R as an array
            Arr : RINGO1_CTRL_E_R_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for RINGO1_CTRL_E_R_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Metal 2 (M2) monitor control.
   type RINGO1_CTRL_E_M2_Field is
     (
      --  Ringo is disabled.
      Disable,
      --  Ringo is enabled.
      Enable)
     with Size => 1;
   for RINGO1_CTRL_E_M2_Field use
     (Disable => 0,
      Enable => 1);

   --  RINGO1_CTRL_E_M array
   type RINGO1_CTRL_E_M_Field_Array is array (2 .. 5)
     of RINGO1_CTRL_E_M2_Field
     with Component_Size => 1, Size => 4;

   --  Type definition for RINGO1_CTRL_E_M
   type RINGO1_CTRL_E_M_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  E_M as a value
            Val : HAL.UInt4;
         when True =>
            --  E_M as an array
            Arr : RINGO1_CTRL_E_M_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for RINGO1_CTRL_E_M_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   subtype RINGO1_CTRL_DIVISOR_Field is HAL.UInt4;

   --  Second Ring Oscillator module control register.
   type RINGO1_CTRL_Register is record
      --  Select short or long ringo (for all ringos types).
      S              : RINGO1_CTRL_S_Field := NXP_SVD.ANACTRL.Short;
      --  Ringo frequency output divider.
      FS             : RINGO1_CTRL_FS_Field := NXP_SVD.ANACTRL.Fast;
      --  Ringo module Power control.
      PD             : RINGO1_CTRL_PD_Field := NXP_SVD.ANACTRL.Powered_On;
      --  .
      E_R            : RINGO1_CTRL_E_R_Field :=
                        (As_Array => False, Val => 16#0#);
      --  Metal 2 (M2) monitor control.
      E_M            : RINGO1_CTRL_E_M_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_9_15  : HAL.UInt7 := 16#0#;
      --  Ringo out Clock divider value. Frequency Output = Frequency input /
      --  (DIViSOR+1). (minimum = Frequency input / 16)
      DIVISOR        : RINGO1_CTRL_DIVISOR_Field := 16#0#;
      --  unspecified
      Reserved_20_30 : HAL.UInt11 := 16#0#;
      --  Read-only. Ringo clock out Divider status flag. Set when a change is
      --  made to the divider value, cleared when the change is complete.
      DIV_UPDATE_REQ : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RINGO1_CTRL_Register use record
      S              at 0 range 0 .. 0;
      FS             at 0 range 1 .. 1;
      PD             at 0 range 2 .. 2;
      E_R            at 0 range 3 .. 4;
      E_M            at 0 range 5 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      DIVISOR        at 0 range 16 .. 19;
      Reserved_20_30 at 0 range 20 .. 30;
      DIV_UPDATE_REQ at 0 range 31 .. 31;
   end record;

   --  Select short or long ringo (for all ringos types).
   type RINGO2_CTRL_S_Field is
     (
      --  Select short ringo (few elements).
      Short,
      --  Select long ringo (many elements).
      Long)
     with Size => 1;
   for RINGO2_CTRL_S_Field use
     (Short => 0,
      Long => 1);

   --  Ringo frequency output divider.
   type RINGO2_CTRL_FS_Field is
     (
      --  High frequency output (frequency lower than 100 MHz).
      Fast,
      --  Low frequency output (frequency lower than 10 MHz).
      Slow)
     with Size => 1;
   for RINGO2_CTRL_FS_Field use
     (Fast => 0,
      Slow => 1);

   --  Ringo module Power control.
   type RINGO2_CTRL_PD_Field is
     (
      --  The Ringo module is enabled.
      Powered_On,
      --  The Ringo module is disabled.
      Powered_Down)
     with Size => 1;
   for RINGO2_CTRL_PD_Field use
     (Powered_On => 0,
      Powered_Down => 1);

   --  .
   type RINGO2_CTRL_E_R24_Field is
     (
      --  Ringo is disabled.
      Disable,
      --  Ringo is enabled.
      Enable)
     with Size => 1;
   for RINGO2_CTRL_E_R24_Field use
     (Disable => 0,
      Enable => 1);

   --  RINGO2_CTRL_E_R array
   type RINGO2_CTRL_E_R_Field_Array is array (24 .. 25)
     of RINGO2_CTRL_E_R24_Field
     with Component_Size => 1, Size => 2;

   --  Type definition for RINGO2_CTRL_E_R
   type RINGO2_CTRL_E_R_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  E_R as a value
            Val : HAL.UInt2;
         when True =>
            --  E_R as an array
            Arr : RINGO2_CTRL_E_R_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for RINGO2_CTRL_E_R_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Metal 2 (M2) monitor control.
   type RINGO2_CTRL_E_M2_Field is
     (
      --  Ringo is disabled.
      Disable,
      --  Ringo is enabled.
      Enable)
     with Size => 1;
   for RINGO2_CTRL_E_M2_Field use
     (Disable => 0,
      Enable => 1);

   --  RINGO2_CTRL_E_M array
   type RINGO2_CTRL_E_M_Field_Array is array (2 .. 5)
     of RINGO2_CTRL_E_M2_Field
     with Component_Size => 1, Size => 4;

   --  Type definition for RINGO2_CTRL_E_M
   type RINGO2_CTRL_E_M_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  E_M as a value
            Val : HAL.UInt4;
         when True =>
            --  E_M as an array
            Arr : RINGO2_CTRL_E_M_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for RINGO2_CTRL_E_M_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   subtype RINGO2_CTRL_DIVISOR_Field is HAL.UInt4;

   --  Third Ring Oscillator module control register.
   type RINGO2_CTRL_Register is record
      --  Select short or long ringo (for all ringos types).
      S              : RINGO2_CTRL_S_Field := NXP_SVD.ANACTRL.Short;
      --  Ringo frequency output divider.
      FS             : RINGO2_CTRL_FS_Field := NXP_SVD.ANACTRL.Fast;
      --  Ringo module Power control.
      PD             : RINGO2_CTRL_PD_Field := NXP_SVD.ANACTRL.Powered_On;
      --  .
      E_R            : RINGO2_CTRL_E_R_Field :=
                        (As_Array => False, Val => 16#0#);
      --  Metal 2 (M2) monitor control.
      E_M            : RINGO2_CTRL_E_M_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_9_15  : HAL.UInt7 := 16#0#;
      --  Ringo out Clock divider value. Frequency Output = Frequency input /
      --  (DIViSOR+1). (minimum = Frequency input / 16)
      DIVISOR        : RINGO2_CTRL_DIVISOR_Field := 16#0#;
      --  unspecified
      Reserved_20_30 : HAL.UInt11 := 16#0#;
      --  Read-only. Ringo clock out Divider status flag. Set when a change is
      --  made to the divider value, cleared when the change is complete.
      DIV_UPDATE_REQ : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RINGO2_CTRL_Register use record
      S              at 0 range 0 .. 0;
      FS             at 0 range 1 .. 1;
      PD             at 0 range 2 .. 2;
      E_R            at 0 range 3 .. 4;
      E_M            at 0 range 5 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      DIVISOR        at 0 range 16 .. 19;
      Reserved_20_30 at 0 range 20 .. 30;
      DIV_UPDATE_REQ at 0 range 31 .. 31;
   end record;

   --  USB High Speed Phy Control
   type USBHS_PHY_CTRL_Register is record
      --  Override value for Vbus if using external detectors.
      usb_vbusvalid_ext : Boolean := False;
      --  Override value for ID if using external detectors.
      usb_id_ext        : Boolean := False;
      --  unspecified
      Reserved_2_31     : HAL.UInt30 := 16#2#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for USBHS_PHY_CTRL_Register use record
      usb_vbusvalid_ext at 0 range 0 .. 0;
      usb_id_ext        at 0 range 1 .. 1;
      Reserved_2_31     at 0 range 2 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  ANALOGCTRL
   type ANACTRL_Peripheral is record
      --  Analog Macroblock Identity registers, Flash Status registers
      ANALOG_CTRL_STATUS  : aliased ANALOG_CTRL_STATUS_Register;
      --  Frequency Measure function control register
      FREQ_ME_CTRL        : aliased FREQ_ME_CTRL_Register;
      --  192MHz Free Running OScillator (FRO) Control register
      FRO192M_CTRL        : aliased FRO192M_CTRL_Register;
      --  192MHz Free Running OScillator (FRO) Status register
      FRO192M_STATUS      : aliased FRO192M_STATUS_Register;
      --  High speed Crystal Oscillator Control register
      XO32M_CTRL          : aliased XO32M_CTRL_Register;
      --  High speed Crystal Oscillator Status register
      XO32M_STATUS        : aliased XO32M_STATUS_Register;
      --  Brown Out Detectors (BoDs) & DCDC interrupts generation control
      --  register
      BOD_DCDC_INT_CTRL   : aliased BOD_DCDC_INT_CTRL_Register;
      --  BoDs & DCDC interrupts status register
      BOD_DCDC_INT_STATUS : aliased BOD_DCDC_INT_STATUS_Register;
      --  First Ring Oscillator module control register.
      RINGO0_CTRL         : aliased RINGO0_CTRL_Register;
      --  Second Ring Oscillator module control register.
      RINGO1_CTRL         : aliased RINGO1_CTRL_Register;
      --  Third Ring Oscillator module control register.
      RINGO2_CTRL         : aliased RINGO2_CTRL_Register;
      --  USB High Speed Phy Control
      USBHS_PHY_CTRL      : aliased USBHS_PHY_CTRL_Register;
   end record
     with Volatile;

   for ANACTRL_Peripheral use record
      ANALOG_CTRL_STATUS  at 16#4# range 0 .. 31;
      FREQ_ME_CTRL        at 16#C# range 0 .. 31;
      FRO192M_CTRL        at 16#10# range 0 .. 31;
      FRO192M_STATUS      at 16#14# range 0 .. 31;
      XO32M_CTRL          at 16#20# range 0 .. 31;
      XO32M_STATUS        at 16#24# range 0 .. 31;
      BOD_DCDC_INT_CTRL   at 16#30# range 0 .. 31;
      BOD_DCDC_INT_STATUS at 16#34# range 0 .. 31;
      RINGO0_CTRL         at 16#40# range 0 .. 31;
      RINGO1_CTRL         at 16#44# range 0 .. 31;
      RINGO2_CTRL         at 16#48# range 0 .. 31;
      USBHS_PHY_CTRL      at 16#100# range 0 .. 31;
   end record;

   --  ANALOGCTRL
   ANACTRL_Periph : aliased ANACTRL_Peripheral
     with Import, Address => System'To_Address (16#40013000#);

end NXP_SVD.ANACTRL;
