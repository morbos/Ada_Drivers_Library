--  Copyright 2016-2019 NXP
--  All rights reserved.SPDX-License-Identifier: BSD-3-Clause


--  This spec has been automatically generated from LPC55S6x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NXP_SVD.SYSCON is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Select the location of the vector table :.
   type MEMORYREMAP_MAP_Field is
     (
      --  Vector Table in ROM.
      Rom0,
      --  Vector Table in RAM.
      Ram1,
      --  Vector Table in Flash.
      Flash0,
      --  Vector Table in Flash.
      Flash1)
     with Size => 2;
   for MEMORYREMAP_MAP_Field use
     (Rom0 => 0,
      Ram1 => 1,
      Flash0 => 2,
      Flash1 => 3);

   --  Memory Remap control register
   type MEMORYREMAP_Register is record
      --  Select the location of the vector table :.
      MAP           : MEMORYREMAP_MAP_Field := NXP_SVD.SYSCON.Rom0;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MEMORYREMAP_Register use record
      MAP           at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   subtype AHBMATPRIO_PRI_CPU0_CBUS_Field is HAL.UInt2;
   subtype AHBMATPRIO_PRI_CPU0_SBUS_Field is HAL.UInt2;
   subtype AHBMATPRIO_PRI_CPU1_CBUS_Field is HAL.UInt2;
   subtype AHBMATPRIO_PRI_CPU1_SBUS_Field is HAL.UInt2;
   subtype AHBMATPRIO_PRI_USB_FS_Field is HAL.UInt2;
   subtype AHBMATPRIO_PRI_SDMA0_Field is HAL.UInt2;
   subtype AHBMATPRIO_PRI_SDIO_Field is HAL.UInt2;
   subtype AHBMATPRIO_PRI_PQ_Field is HAL.UInt2;
   subtype AHBMATPRIO_PRI_HASH_AES_Field is HAL.UInt2;
   subtype AHBMATPRIO_PRI_USB_HS_Field is HAL.UInt2;
   subtype AHBMATPRIO_PRI_SDMA1_Field is HAL.UInt2;

   --  AHB Matrix priority control register Priority values are 3 = highest, 0
   --  = lowest
   type AHBMATPRIO_Register is record
      --  CPU0 C-AHB bus.
      PRI_CPU0_CBUS  : AHBMATPRIO_PRI_CPU0_CBUS_Field := 16#0#;
      --  CPU0 S-AHB bus.
      PRI_CPU0_SBUS  : AHBMATPRIO_PRI_CPU0_SBUS_Field := 16#0#;
      --  CPU1 C-AHB bus.
      PRI_CPU1_CBUS  : AHBMATPRIO_PRI_CPU1_CBUS_Field := 16#0#;
      --  CPU1 S-AHB bus.
      PRI_CPU1_SBUS  : AHBMATPRIO_PRI_CPU1_SBUS_Field := 16#0#;
      --  USB-FS.(USB0)
      PRI_USB_FS     : AHBMATPRIO_PRI_USB_FS_Field := 16#0#;
      --  DMA0 controller priority.
      PRI_SDMA0      : AHBMATPRIO_PRI_SDMA0_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  SDIO.
      PRI_SDIO       : AHBMATPRIO_PRI_SDIO_Field := 16#0#;
      --  PQ (HW Accelerator).
      PRI_PQ         : AHBMATPRIO_PRI_PQ_Field := 16#0#;
      --  HASH_AES.
      PRI_HASH_AES   : AHBMATPRIO_PRI_HASH_AES_Field := 16#0#;
      --  USB-HS.(USB1)
      PRI_USB_HS     : AHBMATPRIO_PRI_USB_HS_Field := 16#0#;
      --  DMA1 controller priority.
      PRI_SDMA1      : AHBMATPRIO_PRI_SDMA1_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for AHBMATPRIO_Register use record
      PRI_CPU0_CBUS  at 0 range 0 .. 1;
      PRI_CPU0_SBUS  at 0 range 2 .. 3;
      PRI_CPU1_CBUS  at 0 range 4 .. 5;
      PRI_CPU1_SBUS  at 0 range 6 .. 7;
      PRI_USB_FS     at 0 range 8 .. 9;
      PRI_SDMA0      at 0 range 10 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      PRI_SDIO       at 0 range 16 .. 17;
      PRI_PQ         at 0 range 18 .. 19;
      PRI_HASH_AES   at 0 range 20 .. 21;
      PRI_USB_HS     at 0 range 22 .. 23;
      PRI_SDMA1      at 0 range 24 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype CPU0STCKCAL_TENMS_Field is HAL.UInt24;

   --  System tick calibration for secure part of CPU0
   type CPU0STCKCAL_Register is record
      --  Reload value for 10ms (100Hz) timing, subject to system clock skew
      --  errors. If the value reads as zero, the calibration value is not
      --  known.
      TENMS          : CPU0STCKCAL_TENMS_Field := 16#0#;
      --  Initial value for the Systick timer.
      SKEW           : Boolean := False;
      --  Indicates whether the device provides a reference clock to the
      --  processor: 0 = reference clock provided; 1 = no reference clock
      --  provided.
      NOREF          : Boolean := False;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPU0STCKCAL_Register use record
      TENMS          at 0 range 0 .. 23;
      SKEW           at 0 range 24 .. 24;
      NOREF          at 0 range 25 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype CPU0NSTCKCAL_TENMS_Field is HAL.UInt24;

   --  System tick calibration for non-secure part of CPU0
   type CPU0NSTCKCAL_Register is record
      --  Reload value for 10 ms (100 Hz) timing, subject to system clock skew
      --  errors. If the value reads as zero, the calibration value is not
      --  known.
      TENMS          : CPU0NSTCKCAL_TENMS_Field := 16#0#;
      --  Indicates whether the TENMS value is exact: 0 = TENMS value is exact;
      --  1 = TENMS value is inexact, or not given.
      SKEW           : Boolean := False;
      --  Initial value for the Systick timer.
      NOREF          : Boolean := False;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPU0NSTCKCAL_Register use record
      TENMS          at 0 range 0 .. 23;
      SKEW           at 0 range 24 .. 24;
      NOREF          at 0 range 25 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype CPU1STCKCAL_TENMS_Field is HAL.UInt24;

   --  System tick calibration for CPU1
   type CPU1STCKCAL_Register is record
      --  Reload value for 10ms (100Hz) timing, subject to system clock skew
      --  errors. If the value reads as zero, the calibration value is not
      --  known.
      TENMS          : CPU1STCKCAL_TENMS_Field := 16#0#;
      --  Indicates whether the TENMS value is exact: 0 = TENMS value is exact;
      --  1 = TENMS value is inexact, or not given.
      SKEW           : Boolean := False;
      --  Indicates whether the device provides a reference clock to the
      --  processor: 0 = reference clock provided; 1 = no reference clock
      --  provided.
      NOREF          : Boolean := False;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPU1STCKCAL_Register use record
      TENMS          at 0 range 0 .. 23;
      SKEW           at 0 range 24 .. 24;
      NOREF          at 0 range 25 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype NMISRC_IRQCPU0_Field is HAL.UInt6;
   subtype NMISRC_IRQCPU1_Field is HAL.UInt6;

   --  NMI Source Select
   type NMISRC_Register is record
      --  The IRQ number of the interrupt that acts as the Non-Maskable
      --  Interrupt (NMI) for the CPU0, if enabled by NMIENCPU0.
      IRQCPU0        : NMISRC_IRQCPU0_Field := 16#0#;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  The IRQ number of the interrupt that acts as the Non-Maskable
      --  Interrupt (NMI) for the CPU1, if enabled by NMIENCPU1.
      IRQCPU1        : NMISRC_IRQCPU1_Field := 16#0#;
      --  unspecified
      Reserved_14_29 : HAL.UInt16 := 16#0#;
      --  Write a 1 to this bit to enable the Non-Maskable Interrupt (NMI)
      --  source selected by IRQCPU1.
      NMIENCPU1      : Boolean := False;
      --  Write a 1 to this bit to enable the Non-Maskable Interrupt (NMI)
      --  source selected by IRQCPU0.
      NMIENCPU0      : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for NMISRC_Register use record
      IRQCPU0        at 0 range 0 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      IRQCPU1        at 0 range 8 .. 13;
      Reserved_14_29 at 0 range 14 .. 29;
      NMIENCPU1      at 0 range 30 .. 30;
      NMIENCPU0      at 0 range 31 .. 31;
   end record;

   --  ROM reset control.
   type PRESETCTRL0_ROM_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL0_ROM_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  SRAM Controller 1 reset control.
   type PRESETCTRL0_SRAM_CTRL1_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL0_SRAM_CTRL1_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  SRAM Controller 2 reset control.
   type PRESETCTRL0_SRAM_CTRL2_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL0_SRAM_CTRL2_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  SRAM Controller 3 reset control.
   type PRESETCTRL0_SRAM_CTRL3_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL0_SRAM_CTRL3_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  SRAM Controller 4 reset control.
   type PRESETCTRL0_SRAM_CTRL4_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL0_SRAM_CTRL4_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  Flash controller reset control.
   type PRESETCTRL0_FLASH_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL0_FLASH_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  FMC controller reset control.
   type PRESETCTRL0_FMC_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL0_FMC_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  Input Mux reset control.
   type PRESETCTRL0_MUX_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL0_MUX_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  I/O controller reset control.
   type PRESETCTRL0_IOCON_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL0_IOCON_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  GPIO0 reset control.
   type PRESETCTRL0_GPIO0_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL0_GPIO0_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  GPIO1 reset control.
   type PRESETCTRL0_GPIO1_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL0_GPIO1_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  GPIO2 reset control.
   type PRESETCTRL0_GPIO2_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL0_GPIO2_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  GPIO3 reset control.
   type PRESETCTRL0_GPIO3_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL0_GPIO3_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  Pin interrupt (PINT) reset control.
   type PRESETCTRL0_PINT_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL0_PINT_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  Group interrupt (GINT) reset control.
   type PRESETCTRL0_GINT_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL0_GINT_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  DMA0 reset control.
   type PRESETCTRL0_DMA0_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL0_DMA0_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  CRCGEN reset control.
   type PRESETCTRL0_CRCGEN_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL0_CRCGEN_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  Watchdog Timer reset control.
   type PRESETCTRL0_WWDT_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL0_WWDT_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  Real Time Clock (RTC) reset control.
   type PRESETCTRL0_RTC_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL0_RTC_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  Inter CPU communication Mailbox reset control.
   type PRESETCTRL0_MAILBOX_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL0_MAILBOX_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  ADC reset control.
   type PRESETCTRL0_ADC_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL0_ADC_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  Peripheral reset control 0
   type PRESETCTRL0_Register is record
      --  unspecified
      Reserved_0_0   : HAL.Bit := 16#0#;
      --  ROM reset control.
      ROM_RST        : PRESETCTRL0_ROM_RST_Field := NXP_SVD.SYSCON.Released;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  SRAM Controller 1 reset control.
      SRAM_CTRL1_RST : PRESETCTRL0_SRAM_CTRL1_RST_Field :=
                        NXP_SVD.SYSCON.Released;
      --  SRAM Controller 2 reset control.
      SRAM_CTRL2_RST : PRESETCTRL0_SRAM_CTRL2_RST_Field :=
                        NXP_SVD.SYSCON.Released;
      --  SRAM Controller 3 reset control.
      SRAM_CTRL3_RST : PRESETCTRL0_SRAM_CTRL3_RST_Field :=
                        NXP_SVD.SYSCON.Released;
      --  SRAM Controller 4 reset control.
      SRAM_CTRL4_RST : PRESETCTRL0_SRAM_CTRL4_RST_Field :=
                        NXP_SVD.SYSCON.Released;
      --  Flash controller reset control.
      FLASH_RST      : PRESETCTRL0_FLASH_RST_Field := NXP_SVD.SYSCON.Released;
      --  FMC controller reset control.
      FMC_RST        : PRESETCTRL0_FMC_RST_Field := NXP_SVD.SYSCON.Released;
      --  unspecified
      Reserved_9_10  : HAL.UInt2 := 16#0#;
      --  Input Mux reset control.
      MUX_RST        : PRESETCTRL0_MUX_RST_Field := NXP_SVD.SYSCON.Released;
      --  unspecified
      Reserved_12_12 : HAL.Bit := 16#0#;
      --  I/O controller reset control.
      IOCON_RST      : PRESETCTRL0_IOCON_RST_Field := NXP_SVD.SYSCON.Released;
      --  GPIO0 reset control.
      GPIO0_RST      : PRESETCTRL0_GPIO0_RST_Field := NXP_SVD.SYSCON.Released;
      --  GPIO1 reset control.
      GPIO1_RST      : PRESETCTRL0_GPIO1_RST_Field := NXP_SVD.SYSCON.Released;
      --  GPIO2 reset control.
      GPIO2_RST      : PRESETCTRL0_GPIO2_RST_Field := NXP_SVD.SYSCON.Released;
      --  GPIO3 reset control.
      GPIO3_RST      : PRESETCTRL0_GPIO3_RST_Field := NXP_SVD.SYSCON.Released;
      --  Pin interrupt (PINT) reset control.
      PINT_RST       : PRESETCTRL0_PINT_RST_Field := NXP_SVD.SYSCON.Released;
      --  Group interrupt (GINT) reset control.
      GINT_RST       : PRESETCTRL0_GINT_RST_Field := NXP_SVD.SYSCON.Released;
      --  DMA0 reset control.
      DMA0_RST       : PRESETCTRL0_DMA0_RST_Field := NXP_SVD.SYSCON.Released;
      --  CRCGEN reset control.
      CRCGEN_RST     : PRESETCTRL0_CRCGEN_RST_Field :=
                        NXP_SVD.SYSCON.Released;
      --  Watchdog Timer reset control.
      WWDT_RST       : PRESETCTRL0_WWDT_RST_Field := NXP_SVD.SYSCON.Released;
      --  Real Time Clock (RTC) reset control.
      RTC_RST        : PRESETCTRL0_RTC_RST_Field := NXP_SVD.SYSCON.Released;
      --  unspecified
      Reserved_24_25 : HAL.UInt2 := 16#0#;
      --  Inter CPU communication Mailbox reset control.
      MAILBOX_RST    : PRESETCTRL0_MAILBOX_RST_Field :=
                        NXP_SVD.SYSCON.Released;
      --  ADC reset control.
      ADC_RST        : PRESETCTRL0_ADC_RST_Field := NXP_SVD.SYSCON.Released;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PRESETCTRL0_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      ROM_RST        at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      SRAM_CTRL1_RST at 0 range 3 .. 3;
      SRAM_CTRL2_RST at 0 range 4 .. 4;
      SRAM_CTRL3_RST at 0 range 5 .. 5;
      SRAM_CTRL4_RST at 0 range 6 .. 6;
      FLASH_RST      at 0 range 7 .. 7;
      FMC_RST        at 0 range 8 .. 8;
      Reserved_9_10  at 0 range 9 .. 10;
      MUX_RST        at 0 range 11 .. 11;
      Reserved_12_12 at 0 range 12 .. 12;
      IOCON_RST      at 0 range 13 .. 13;
      GPIO0_RST      at 0 range 14 .. 14;
      GPIO1_RST      at 0 range 15 .. 15;
      GPIO2_RST      at 0 range 16 .. 16;
      GPIO3_RST      at 0 range 17 .. 17;
      PINT_RST       at 0 range 18 .. 18;
      GINT_RST       at 0 range 19 .. 19;
      DMA0_RST       at 0 range 20 .. 20;
      CRCGEN_RST     at 0 range 21 .. 21;
      WWDT_RST       at 0 range 22 .. 22;
      RTC_RST        at 0 range 23 .. 23;
      Reserved_24_25 at 0 range 24 .. 25;
      MAILBOX_RST    at 0 range 26 .. 26;
      ADC_RST        at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  MRT reset control.
   type PRESETCTRL1_MRT_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL1_MRT_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  OS Event Timer reset control.
   type PRESETCTRL1_OSTIMER_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL1_OSTIMER_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  SCT reset control.
   type PRESETCTRL1_SCT_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL1_SCT_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  SCTIPU reset control.
   type PRESETCTRL1_SCTIPU_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL1_SCTIPU_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  UTICK reset control.
   type PRESETCTRL1_UTICK_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL1_UTICK_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  FC0 reset control.
   type PRESETCTRL1_FC0_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL1_FC0_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  FC1 reset control.
   type PRESETCTRL1_FC1_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL1_FC1_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  FC2 reset control.
   type PRESETCTRL1_FC2_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL1_FC2_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  FC3 reset control.
   type PRESETCTRL1_FC3_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL1_FC3_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  FC4 reset control.
   type PRESETCTRL1_FC4_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL1_FC4_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  FC5 reset control.
   type PRESETCTRL1_FC5_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL1_FC5_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  FC6 reset control.
   type PRESETCTRL1_FC6_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL1_FC6_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  FC7 reset control.
   type PRESETCTRL1_FC7_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL1_FC7_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  Timer 2 reset control.
   type PRESETCTRL1_TIMER2_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL1_TIMER2_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  USB0 DEV reset control.
   type PRESETCTRL1_USB0_DEV_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL1_USB0_DEV_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  Timer 0 reset control.
   type PRESETCTRL1_TIMER0_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL1_TIMER0_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  Timer 1 reset control.
   type PRESETCTRL1_TIMER1_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL1_TIMER1_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  Peripheral reset control 1
   type PRESETCTRL1_Register is record
      --  MRT reset control.
      MRT_RST        : PRESETCTRL1_MRT_RST_Field := NXP_SVD.SYSCON.Released;
      --  OS Event Timer reset control.
      OSTIMER_RST    : PRESETCTRL1_OSTIMER_RST_Field :=
                        NXP_SVD.SYSCON.Released;
      --  SCT reset control.
      SCT_RST        : PRESETCTRL1_SCT_RST_Field := NXP_SVD.SYSCON.Released;
      --  unspecified
      Reserved_3_5   : HAL.UInt3 := 16#0#;
      --  SCTIPU reset control.
      SCTIPU_RST     : PRESETCTRL1_SCTIPU_RST_Field :=
                        NXP_SVD.SYSCON.Released;
      --  unspecified
      Reserved_7_9   : HAL.UInt3 := 16#0#;
      --  UTICK reset control.
      UTICK_RST      : PRESETCTRL1_UTICK_RST_Field := NXP_SVD.SYSCON.Released;
      --  FC0 reset control.
      FC0_RST        : PRESETCTRL1_FC0_RST_Field := NXP_SVD.SYSCON.Released;
      --  FC1 reset control.
      FC1_RST        : PRESETCTRL1_FC1_RST_Field := NXP_SVD.SYSCON.Released;
      --  FC2 reset control.
      FC2_RST        : PRESETCTRL1_FC2_RST_Field := NXP_SVD.SYSCON.Released;
      --  FC3 reset control.
      FC3_RST        : PRESETCTRL1_FC3_RST_Field := NXP_SVD.SYSCON.Released;
      --  FC4 reset control.
      FC4_RST        : PRESETCTRL1_FC4_RST_Field := NXP_SVD.SYSCON.Released;
      --  FC5 reset control.
      FC5_RST        : PRESETCTRL1_FC5_RST_Field := NXP_SVD.SYSCON.Released;
      --  FC6 reset control.
      FC6_RST        : PRESETCTRL1_FC6_RST_Field := NXP_SVD.SYSCON.Released;
      --  FC7 reset control.
      FC7_RST        : PRESETCTRL1_FC7_RST_Field := NXP_SVD.SYSCON.Released;
      --  unspecified
      Reserved_19_21 : HAL.UInt3 := 16#0#;
      --  Timer 2 reset control.
      TIMER2_RST     : PRESETCTRL1_TIMER2_RST_Field :=
                        NXP_SVD.SYSCON.Released;
      --  unspecified
      Reserved_23_24 : HAL.UInt2 := 16#0#;
      --  USB0 DEV reset control.
      USB0_DEV_RST   : PRESETCTRL1_USB0_DEV_RST_Field :=
                        NXP_SVD.SYSCON.Released;
      --  Timer 0 reset control.
      TIMER0_RST     : PRESETCTRL1_TIMER0_RST_Field :=
                        NXP_SVD.SYSCON.Released;
      --  Timer 1 reset control.
      TIMER1_RST     : PRESETCTRL1_TIMER1_RST_Field :=
                        NXP_SVD.SYSCON.Released;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PRESETCTRL1_Register use record
      MRT_RST        at 0 range 0 .. 0;
      OSTIMER_RST    at 0 range 1 .. 1;
      SCT_RST        at 0 range 2 .. 2;
      Reserved_3_5   at 0 range 3 .. 5;
      SCTIPU_RST     at 0 range 6 .. 6;
      Reserved_7_9   at 0 range 7 .. 9;
      UTICK_RST      at 0 range 10 .. 10;
      FC0_RST        at 0 range 11 .. 11;
      FC1_RST        at 0 range 12 .. 12;
      FC2_RST        at 0 range 13 .. 13;
      FC3_RST        at 0 range 14 .. 14;
      FC4_RST        at 0 range 15 .. 15;
      FC5_RST        at 0 range 16 .. 16;
      FC6_RST        at 0 range 17 .. 17;
      FC7_RST        at 0 range 18 .. 18;
      Reserved_19_21 at 0 range 19 .. 21;
      TIMER2_RST     at 0 range 22 .. 22;
      Reserved_23_24 at 0 range 23 .. 24;
      USB0_DEV_RST   at 0 range 25 .. 25;
      TIMER0_RST     at 0 range 26 .. 26;
      TIMER1_RST     at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  DMA1 reset control.
   type PRESETCTRL2_DMA1_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL2_DMA1_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  Comparator reset control.
   type PRESETCTRL2_COMP_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL2_COMP_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  SDIO reset control.
   type PRESETCTRL2_SDIO_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL2_SDIO_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  USB1 Host reset control.
   type PRESETCTRL2_USB1_HOST_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL2_USB1_HOST_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  USB1 dev reset control.
   type PRESETCTRL2_USB1_DEV_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL2_USB1_DEV_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  USB1 RAM reset control.
   type PRESETCTRL2_USB1_RAM_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL2_USB1_RAM_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  USB1 PHY reset control.
   type PRESETCTRL2_USB1_PHY_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL2_USB1_PHY_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  Frequency meter reset control.
   type PRESETCTRL2_FREQME_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL2_FREQME_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  RNG reset control.
   type PRESETCTRL2_RNG_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL2_RNG_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  SYSCTL Block reset.
   type PRESETCTRL2_SYSCTL_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL2_SYSCTL_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  USB0 Host Master reset control.
   type PRESETCTRL2_USB0_HOSTM_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL2_USB0_HOSTM_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  USB0 Host Slave reset control.
   type PRESETCTRL2_USB0_HOSTS_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL2_USB0_HOSTS_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  HASH_AES reset control.
   type PRESETCTRL2_HASH_AES_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL2_HASH_AES_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  Power Quad reset control.
   type PRESETCTRL2_PQ_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL2_PQ_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  PLU LUT reset control.
   type PRESETCTRL2_PLULUT_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL2_PLULUT_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  Timer 3 reset control.
   type PRESETCTRL2_TIMER3_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL2_TIMER3_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  Timer 4 reset control.
   type PRESETCTRL2_TIMER4_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL2_TIMER4_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  PUF reset control reset control.
   type PRESETCTRL2_PUF_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL2_PUF_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  Casper reset control.
   type PRESETCTRL2_CASPER_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL2_CASPER_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  analog control reset control.
   type PRESETCTRL2_ANALOG_CTRL_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL2_ANALOG_CTRL_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  HS LSPI reset control.
   type PRESETCTRL2_HS_LSPI_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL2_HS_LSPI_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  GPIO secure reset control.
   type PRESETCTRL2_GPIO_SEC_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL2_GPIO_SEC_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  GPIO secure int reset control.
   type PRESETCTRL2_GPIO_SEC_INT_RST_Field is
     (
      --  Bloc is not reset.
      Released,
      --  Bloc is reset.
      Asserted)
     with Size => 1;
   for PRESETCTRL2_GPIO_SEC_INT_RST_Field use
     (Released => 0,
      Asserted => 1);

   --  Peripheral reset control 2
   type PRESETCTRL2_Register is record
      --  unspecified
      Reserved_0_0     : HAL.Bit := 16#0#;
      --  DMA1 reset control.
      DMA1_RST         : PRESETCTRL2_DMA1_RST_Field :=
                          NXP_SVD.SYSCON.Released;
      --  Comparator reset control.
      COMP_RST         : PRESETCTRL2_COMP_RST_Field :=
                          NXP_SVD.SYSCON.Released;
      --  SDIO reset control.
      SDIO_RST         : PRESETCTRL2_SDIO_RST_Field :=
                          NXP_SVD.SYSCON.Released;
      --  USB1 Host reset control.
      USB1_HOST_RST    : PRESETCTRL2_USB1_HOST_RST_Field :=
                          NXP_SVD.SYSCON.Released;
      --  USB1 dev reset control.
      USB1_DEV_RST     : PRESETCTRL2_USB1_DEV_RST_Field :=
                          NXP_SVD.SYSCON.Released;
      --  USB1 RAM reset control.
      USB1_RAM_RST     : PRESETCTRL2_USB1_RAM_RST_Field :=
                          NXP_SVD.SYSCON.Released;
      --  USB1 PHY reset control.
      USB1_PHY_RST     : PRESETCTRL2_USB1_PHY_RST_Field :=
                          NXP_SVD.SYSCON.Released;
      --  Frequency meter reset control.
      FREQME_RST       : PRESETCTRL2_FREQME_RST_Field :=
                          NXP_SVD.SYSCON.Released;
      --  unspecified
      Reserved_9_12    : HAL.UInt4 := 16#0#;
      --  RNG reset control.
      RNG_RST          : PRESETCTRL2_RNG_RST_Field := NXP_SVD.SYSCON.Released;
      --  unspecified
      Reserved_14_14   : HAL.Bit := 16#0#;
      --  SYSCTL Block reset.
      SYSCTL_RST       : PRESETCTRL2_SYSCTL_RST_Field :=
                          NXP_SVD.SYSCON.Released;
      --  USB0 Host Master reset control.
      USB0_HOSTM_RST   : PRESETCTRL2_USB0_HOSTM_RST_Field :=
                          NXP_SVD.SYSCON.Released;
      --  USB0 Host Slave reset control.
      USB0_HOSTS_RST   : PRESETCTRL2_USB0_HOSTS_RST_Field :=
                          NXP_SVD.SYSCON.Released;
      --  HASH_AES reset control.
      HASH_AES_RST     : PRESETCTRL2_HASH_AES_RST_Field :=
                          NXP_SVD.SYSCON.Released;
      --  Power Quad reset control.
      PQ_RST           : PRESETCTRL2_PQ_RST_Field := NXP_SVD.SYSCON.Released;
      --  PLU LUT reset control.
      PLULUT_RST       : PRESETCTRL2_PLULUT_RST_Field :=
                          NXP_SVD.SYSCON.Released;
      --  Timer 3 reset control.
      TIMER3_RST       : PRESETCTRL2_TIMER3_RST_Field :=
                          NXP_SVD.SYSCON.Released;
      --  Timer 4 reset control.
      TIMER4_RST       : PRESETCTRL2_TIMER4_RST_Field :=
                          NXP_SVD.SYSCON.Released;
      --  PUF reset control reset control.
      PUF_RST          : PRESETCTRL2_PUF_RST_Field := NXP_SVD.SYSCON.Released;
      --  Casper reset control.
      CASPER_RST       : PRESETCTRL2_CASPER_RST_Field :=
                          NXP_SVD.SYSCON.Released;
      --  unspecified
      Reserved_25_26   : HAL.UInt2 := 16#0#;
      --  analog control reset control.
      ANALOG_CTRL_RST  : PRESETCTRL2_ANALOG_CTRL_RST_Field :=
                          NXP_SVD.SYSCON.Released;
      --  HS LSPI reset control.
      HS_LSPI_RST      : PRESETCTRL2_HS_LSPI_RST_Field :=
                          NXP_SVD.SYSCON.Released;
      --  GPIO secure reset control.
      GPIO_SEC_RST     : PRESETCTRL2_GPIO_SEC_RST_Field :=
                          NXP_SVD.SYSCON.Released;
      --  GPIO secure int reset control.
      GPIO_SEC_INT_RST : PRESETCTRL2_GPIO_SEC_INT_RST_Field :=
                          NXP_SVD.SYSCON.Released;
      --  unspecified
      Reserved_31_31   : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PRESETCTRL2_Register use record
      Reserved_0_0     at 0 range 0 .. 0;
      DMA1_RST         at 0 range 1 .. 1;
      COMP_RST         at 0 range 2 .. 2;
      SDIO_RST         at 0 range 3 .. 3;
      USB1_HOST_RST    at 0 range 4 .. 4;
      USB1_DEV_RST     at 0 range 5 .. 5;
      USB1_RAM_RST     at 0 range 6 .. 6;
      USB1_PHY_RST     at 0 range 7 .. 7;
      FREQME_RST       at 0 range 8 .. 8;
      Reserved_9_12    at 0 range 9 .. 12;
      RNG_RST          at 0 range 13 .. 13;
      Reserved_14_14   at 0 range 14 .. 14;
      SYSCTL_RST       at 0 range 15 .. 15;
      USB0_HOSTM_RST   at 0 range 16 .. 16;
      USB0_HOSTS_RST   at 0 range 17 .. 17;
      HASH_AES_RST     at 0 range 18 .. 18;
      PQ_RST           at 0 range 19 .. 19;
      PLULUT_RST       at 0 range 20 .. 20;
      TIMER3_RST       at 0 range 21 .. 21;
      TIMER4_RST       at 0 range 22 .. 22;
      PUF_RST          at 0 range 23 .. 23;
      CASPER_RST       at 0 range 24 .. 24;
      Reserved_25_26   at 0 range 25 .. 26;
      ANALOG_CTRL_RST  at 0 range 27 .. 27;
      HS_LSPI_RST      at 0 range 28 .. 28;
      GPIO_SEC_RST     at 0 range 29 .. 29;
      GPIO_SEC_INT_RST at 0 range 30 .. 30;
      Reserved_31_31   at 0 range 31 .. 31;
   end record;

   --  Peripheral reset control set register

   --  Peripheral reset control set register
   type PRESETCTRLSET_Registers is array (0 .. 2) of HAL.UInt32
     with Volatile;

   --  Peripheral reset control clear register

   --  Peripheral reset control clear register
   type PRESETCTRLCLR_Registers is array (0 .. 2) of HAL.UInt32
     with Volatile;

   --  Enables the clock for the ROM.
   type AHBCLKCTRL0_ROM_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL0_ROM_Field use
     (Disable => 0,
      Enable => 1);

   --  Enables the clock for the SRAM Controller 1.
   type AHBCLKCTRL0_SRAM_CTRL1_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL0_SRAM_CTRL1_Field use
     (Disable => 0,
      Enable => 1);

   --  AHBCLKCTRL0_SRAM_CTRL array
   type AHBCLKCTRL0_SRAM_CTRL_Field_Array is array (1 .. 4)
     of AHBCLKCTRL0_SRAM_CTRL1_Field
     with Component_Size => 1, Size => 4;

   --  Type definition for AHBCLKCTRL0_SRAM_CTRL
   type AHBCLKCTRL0_SRAM_CTRL_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SRAM_CTRL as a value
            Val : HAL.UInt4;
         when True =>
            --  SRAM_CTRL as an array
            Arr : AHBCLKCTRL0_SRAM_CTRL_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for AHBCLKCTRL0_SRAM_CTRL_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  Enables the clock for the Flash controller.
   type AHBCLKCTRL0_FLASH_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL0_FLASH_Field use
     (Disable => 0,
      Enable => 1);

   --  Enables the clock for the FMC controller.
   type AHBCLKCTRL0_FMC_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL0_FMC_Field use
     (Disable => 0,
      Enable => 1);

   --  Enables the clock for the Input Mux.
   type AHBCLKCTRL0_MUX_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL0_MUX_Field use
     (Disable => 0,
      Enable => 1);

   --  Enables the clock for the I/O controller.
   type AHBCLKCTRL0_IOCON_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL0_IOCON_Field use
     (Disable => 0,
      Enable => 1);

   --  Enables the clock for the GPIO0.
   type AHBCLKCTRL0_GPIO0_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL0_GPIO0_Field use
     (Disable => 0,
      Enable => 1);

   --  AHBCLKCTRL0_GPIO array
   type AHBCLKCTRL0_GPIO_Field_Array is array (0 .. 3)
     of AHBCLKCTRL0_GPIO0_Field
     with Component_Size => 1, Size => 4;

   --  Type definition for AHBCLKCTRL0_GPIO
   type AHBCLKCTRL0_GPIO_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  GPIO as a value
            Val : HAL.UInt4;
         when True =>
            --  GPIO as an array
            Arr : AHBCLKCTRL0_GPIO_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for AHBCLKCTRL0_GPIO_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  Enables the clock for the Pin interrupt (PINT).
   type AHBCLKCTRL0_PINT_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL0_PINT_Field use
     (Disable => 0,
      Enable => 1);

   --  Enables the clock for the Group interrupt (GINT).
   type AHBCLKCTRL0_GINT_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL0_GINT_Field use
     (Disable => 0,
      Enable => 1);

   --  Enables the clock for the DMA0.
   type AHBCLKCTRL0_DMA0_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL0_DMA0_Field use
     (Disable => 0,
      Enable => 1);

   --  Enables the clock for the CRCGEN.
   type AHBCLKCTRL0_CRCGEN_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL0_CRCGEN_Field use
     (Disable => 0,
      Enable => 1);

   --  Enables the clock for the Watchdog Timer.
   type AHBCLKCTRL0_WWDT_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL0_WWDT_Field use
     (Disable => 0,
      Enable => 1);

   --  Enables the clock for the Real Time Clock (RTC).
   type AHBCLKCTRL0_RTC_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL0_RTC_Field use
     (Disable => 0,
      Enable => 1);

   --  Enables the clock for the Inter CPU communication Mailbox.
   type AHBCLKCTRL0_MAILBOX_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL0_MAILBOX_Field use
     (Disable => 0,
      Enable => 1);

   --  Enables the clock for the ADC.
   type AHBCLKCTRL0_ADC_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL0_ADC_Field use
     (Disable => 0,
      Enable => 1);

   --  AHB Clock control 0
   type AHBCLKCTRL0_Register is record
      --  unspecified
      Reserved_0_0   : HAL.Bit := 16#0#;
      --  Enables the clock for the ROM.
      ROM            : AHBCLKCTRL0_ROM_Field := NXP_SVD.SYSCON.Disable;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  Enables the clock for the SRAM Controller 1.
      SRAM_CTRL      : AHBCLKCTRL0_SRAM_CTRL_Field :=
                        (As_Array => False, Val => 16#0#);
      --  Enables the clock for the Flash controller.
      FLASH          : AHBCLKCTRL0_FLASH_Field := NXP_SVD.SYSCON.Enable;
      --  Enables the clock for the FMC controller.
      FMC            : AHBCLKCTRL0_FMC_Field := NXP_SVD.SYSCON.Enable;
      --  unspecified
      Reserved_9_10  : HAL.UInt2 := 16#0#;
      --  Enables the clock for the Input Mux.
      MUX            : AHBCLKCTRL0_MUX_Field := NXP_SVD.SYSCON.Disable;
      --  unspecified
      Reserved_12_12 : HAL.Bit := 16#0#;
      --  Enables the clock for the I/O controller.
      IOCON          : AHBCLKCTRL0_IOCON_Field := NXP_SVD.SYSCON.Disable;
      --  Enables the clock for the GPIO0.
      GPIO           : AHBCLKCTRL0_GPIO_Field :=
                        (As_Array => False, Val => 16#0#);
      --  Enables the clock for the Pin interrupt (PINT).
      PINT           : AHBCLKCTRL0_PINT_Field := NXP_SVD.SYSCON.Disable;
      --  Enables the clock for the Group interrupt (GINT).
      GINT           : AHBCLKCTRL0_GINT_Field := NXP_SVD.SYSCON.Disable;
      --  Enables the clock for the DMA0.
      DMA0           : AHBCLKCTRL0_DMA0_Field := NXP_SVD.SYSCON.Disable;
      --  Enables the clock for the CRCGEN.
      CRCGEN         : AHBCLKCTRL0_CRCGEN_Field := NXP_SVD.SYSCON.Disable;
      --  Enables the clock for the Watchdog Timer.
      WWDT           : AHBCLKCTRL0_WWDT_Field := NXP_SVD.SYSCON.Disable;
      --  Enables the clock for the Real Time Clock (RTC).
      RTC            : AHBCLKCTRL0_RTC_Field := NXP_SVD.SYSCON.Disable;
      --  unspecified
      Reserved_24_25 : HAL.UInt2 := 16#0#;
      --  Enables the clock for the Inter CPU communication Mailbox.
      MAILBOX        : AHBCLKCTRL0_MAILBOX_Field := NXP_SVD.SYSCON.Disable;
      --  Enables the clock for the ADC.
      ADC            : AHBCLKCTRL0_ADC_Field := NXP_SVD.SYSCON.Disable;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for AHBCLKCTRL0_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      ROM            at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      SRAM_CTRL      at 0 range 3 .. 6;
      FLASH          at 0 range 7 .. 7;
      FMC            at 0 range 8 .. 8;
      Reserved_9_10  at 0 range 9 .. 10;
      MUX            at 0 range 11 .. 11;
      Reserved_12_12 at 0 range 12 .. 12;
      IOCON          at 0 range 13 .. 13;
      GPIO           at 0 range 14 .. 17;
      PINT           at 0 range 18 .. 18;
      GINT           at 0 range 19 .. 19;
      DMA0           at 0 range 20 .. 20;
      CRCGEN         at 0 range 21 .. 21;
      WWDT           at 0 range 22 .. 22;
      RTC            at 0 range 23 .. 23;
      Reserved_24_25 at 0 range 24 .. 25;
      MAILBOX        at 0 range 26 .. 26;
      ADC            at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  Enables the clock for the MRT.
   type AHBCLKCTRL1_MRT_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL1_MRT_Field use
     (Disable => 0,
      Enable => 1);

   --  Enables the clock for the OS Event Timer.
   type AHBCLKCTRL1_OSTIMER_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL1_OSTIMER_Field use
     (Disable => 0,
      Enable => 1);

   --  Enables the clock for the SCT.
   type AHBCLKCTRL1_SCT_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL1_SCT_Field use
     (Disable => 0,
      Enable => 1);

   --  Enables the clock for the UTICK.
   type AHBCLKCTRL1_UTICK_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL1_UTICK_Field use
     (Disable => 0,
      Enable => 1);

   --  Enables the clock for the FC0.
   type AHBCLKCTRL1_FC0_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL1_FC0_Field use
     (Disable => 0,
      Enable => 1);

   --  AHBCLKCTRL1_FC array
   type AHBCLKCTRL1_FC_Field_Array is array (0 .. 7) of AHBCLKCTRL1_FC0_Field
     with Component_Size => 1, Size => 8;

   --  Type definition for AHBCLKCTRL1_FC
   type AHBCLKCTRL1_FC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FC as a value
            Val : HAL.UInt8;
         when True =>
            --  FC as an array
            Arr : AHBCLKCTRL1_FC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 8;

   for AHBCLKCTRL1_FC_Field use record
      Val at 0 range 0 .. 7;
      Arr at 0 range 0 .. 7;
   end record;

   --  Enables the clock for the Timer 2.
   type AHBCLKCTRL1_TIMER2_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL1_TIMER2_Field use
     (Disable => 0,
      Enable => 1);

   --  Enables the clock for the USB0 DEV.
   type AHBCLKCTRL1_USB0_DEV_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL1_USB0_DEV_Field use
     (Disable => 0,
      Enable => 1);

   --  Enables the clock for the Timer 0.
   type AHBCLKCTRL1_TIMER0_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL1_TIMER0_Field use
     (Disable => 0,
      Enable => 1);

   --  AHBCLKCTRL1_TIMER array
   type AHBCLKCTRL1_TIMER_Field_Array is array (0 .. 1)
     of AHBCLKCTRL1_TIMER0_Field
     with Component_Size => 1, Size => 2;

   --  Type definition for AHBCLKCTRL1_TIMER
   type AHBCLKCTRL1_TIMER_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMER as a value
            Val : HAL.UInt2;
         when True =>
            --  TIMER as an array
            Arr : AHBCLKCTRL1_TIMER_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for AHBCLKCTRL1_TIMER_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  AHB Clock control 1
   type AHBCLKCTRL1_Register is record
      --  Enables the clock for the MRT.
      MRT            : AHBCLKCTRL1_MRT_Field := NXP_SVD.SYSCON.Disable;
      --  Enables the clock for the OS Event Timer.
      OSTIMER        : AHBCLKCTRL1_OSTIMER_Field := NXP_SVD.SYSCON.Disable;
      --  Enables the clock for the SCT.
      SCT            : AHBCLKCTRL1_SCT_Field := NXP_SVD.SYSCON.Disable;
      --  unspecified
      Reserved_3_9   : HAL.UInt7 := 16#0#;
      --  Enables the clock for the UTICK.
      UTICK          : AHBCLKCTRL1_UTICK_Field := NXP_SVD.SYSCON.Disable;
      --  Enables the clock for the FC0.
      FC             : AHBCLKCTRL1_FC_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_19_21 : HAL.UInt3 := 16#0#;
      --  Enables the clock for the Timer 2.
      TIMER2         : AHBCLKCTRL1_TIMER2_Field := NXP_SVD.SYSCON.Disable;
      --  unspecified
      Reserved_23_24 : HAL.UInt2 := 16#0#;
      --  Enables the clock for the USB0 DEV.
      USB0_DEV       : AHBCLKCTRL1_USB0_DEV_Field := NXP_SVD.SYSCON.Disable;
      --  Enables the clock for the Timer 0.
      TIMER          : AHBCLKCTRL1_TIMER_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for AHBCLKCTRL1_Register use record
      MRT            at 0 range 0 .. 0;
      OSTIMER        at 0 range 1 .. 1;
      SCT            at 0 range 2 .. 2;
      Reserved_3_9   at 0 range 3 .. 9;
      UTICK          at 0 range 10 .. 10;
      FC             at 0 range 11 .. 18;
      Reserved_19_21 at 0 range 19 .. 21;
      TIMER2         at 0 range 22 .. 22;
      Reserved_23_24 at 0 range 23 .. 24;
      USB0_DEV       at 0 range 25 .. 25;
      TIMER          at 0 range 26 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  Enables the clock for the DMA1.
   type AHBCLKCTRL2_DMA1_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL2_DMA1_Field use
     (Disable => 0,
      Enable => 1);

   --  Enables the clock for the Comparator.
   type AHBCLKCTRL2_COMP_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL2_COMP_Field use
     (Disable => 0,
      Enable => 1);

   --  Enables the clock for the SDIO.
   type AHBCLKCTRL2_SDIO_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL2_SDIO_Field use
     (Disable => 0,
      Enable => 1);

   --  Enables the clock for the USB1 Host.
   type AHBCLKCTRL2_USB1_HOST_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL2_USB1_HOST_Field use
     (Disable => 0,
      Enable => 1);

   --  Enables the clock for the USB1 dev.
   type AHBCLKCTRL2_USB1_DEV_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL2_USB1_DEV_Field use
     (Disable => 0,
      Enable => 1);

   --  Enables the clock for the USB1 RAM.
   type AHBCLKCTRL2_USB1_RAM_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL2_USB1_RAM_Field use
     (Disable => 0,
      Enable => 1);

   --  Enables the clock for the USB1 PHY.
   type AHBCLKCTRL2_USB1_PHY_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL2_USB1_PHY_Field use
     (Disable => 0,
      Enable => 1);

   --  Enables the clock for the Frequency meter.
   type AHBCLKCTRL2_FREQME_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL2_FREQME_Field use
     (Disable => 0,
      Enable => 1);

   --  Enables the clock for the RNG.
   type AHBCLKCTRL2_RNG_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL2_RNG_Field use
     (Disable => 0,
      Enable => 1);

   --  SYSCTL block clock.
   type AHBCLKCTRL2_SYSCTL_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL2_SYSCTL_Field use
     (Disable => 0,
      Enable => 1);

   --  Enables the clock for the USB0 Host Master.
   type AHBCLKCTRL2_USB0_HOSTM_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL2_USB0_HOSTM_Field use
     (Disable => 0,
      Enable => 1);

   --  Enables the clock for the USB0 Host Slave.
   type AHBCLKCTRL2_USB0_HOSTS_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL2_USB0_HOSTS_Field use
     (Disable => 0,
      Enable => 1);

   --  Enables the clock for the HASH_AES.
   type AHBCLKCTRL2_HASH_AES_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL2_HASH_AES_Field use
     (Disable => 0,
      Enable => 1);

   --  Enables the clock for the Power Quad.
   type AHBCLKCTRL2_PQ_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL2_PQ_Field use
     (Disable => 0,
      Enable => 1);

   --  Enables the clock for the PLU LUT.
   type AHBCLKCTRL2_PLULUT_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL2_PLULUT_Field use
     (Disable => 0,
      Enable => 1);

   --  Enables the clock for the Timer 3.
   type AHBCLKCTRL2_TIMER3_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL2_TIMER3_Field use
     (Disable => 0,
      Enable => 1);

   --  AHBCLKCTRL2_TIMER array
   type AHBCLKCTRL2_TIMER_Field_Array is array (3 .. 4)
     of AHBCLKCTRL2_TIMER3_Field
     with Component_Size => 1, Size => 2;

   --  Type definition for AHBCLKCTRL2_TIMER
   type AHBCLKCTRL2_TIMER_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMER as a value
            Val : HAL.UInt2;
         when True =>
            --  TIMER as an array
            Arr : AHBCLKCTRL2_TIMER_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for AHBCLKCTRL2_TIMER_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Enables the clock for the PUF reset control.
   type AHBCLKCTRL2_PUF_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL2_PUF_Field use
     (Disable => 0,
      Enable => 1);

   --  Enables the clock for the Casper.
   type AHBCLKCTRL2_CASPER_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL2_CASPER_Field use
     (Disable => 0,
      Enable => 1);

   --  Enables the clock for the analog control.
   type AHBCLKCTRL2_ANALOG_CTRL_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL2_ANALOG_CTRL_Field use
     (Disable => 0,
      Enable => 1);

   --  Enables the clock for the HS LSPI.
   type AHBCLKCTRL2_HS_LSPI_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL2_HS_LSPI_Field use
     (Disable => 0,
      Enable => 1);

   --  Enables the clock for the GPIO secure.
   type AHBCLKCTRL2_GPIO_SEC_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL2_GPIO_SEC_Field use
     (Disable => 0,
      Enable => 1);

   --  Enables the clock for the GPIO secure int.
   type AHBCLKCTRL2_GPIO_SEC_INT_Field is
     (
      --  Disable Clock.
      Disable,
      --  Enable Clock.
      Enable)
     with Size => 1;
   for AHBCLKCTRL2_GPIO_SEC_INT_Field use
     (Disable => 0,
      Enable => 1);

   --  AHB Clock control 2
   type AHBCLKCTRL2_Register is record
      --  unspecified
      Reserved_0_0   : HAL.Bit := 16#0#;
      --  Enables the clock for the DMA1.
      DMA1           : AHBCLKCTRL2_DMA1_Field := NXP_SVD.SYSCON.Disable;
      --  Enables the clock for the Comparator.
      COMP           : AHBCLKCTRL2_COMP_Field := NXP_SVD.SYSCON.Disable;
      --  Enables the clock for the SDIO.
      SDIO           : AHBCLKCTRL2_SDIO_Field := NXP_SVD.SYSCON.Disable;
      --  Enables the clock for the USB1 Host.
      USB1_HOST      : AHBCLKCTRL2_USB1_HOST_Field := NXP_SVD.SYSCON.Disable;
      --  Enables the clock for the USB1 dev.
      USB1_DEV       : AHBCLKCTRL2_USB1_DEV_Field := NXP_SVD.SYSCON.Disable;
      --  Enables the clock for the USB1 RAM.
      USB1_RAM       : AHBCLKCTRL2_USB1_RAM_Field := NXP_SVD.SYSCON.Disable;
      --  Enables the clock for the USB1 PHY.
      USB1_PHY       : AHBCLKCTRL2_USB1_PHY_Field := NXP_SVD.SYSCON.Disable;
      --  Enables the clock for the Frequency meter.
      FREQME         : AHBCLKCTRL2_FREQME_Field := NXP_SVD.SYSCON.Disable;
      --  unspecified
      Reserved_9_12  : HAL.UInt4 := 16#0#;
      --  Enables the clock for the RNG.
      RNG            : AHBCLKCTRL2_RNG_Field := NXP_SVD.SYSCON.Disable;
      --  unspecified
      Reserved_14_14 : HAL.Bit := 16#0#;
      --  SYSCTL block clock.
      SYSCTL         : AHBCLKCTRL2_SYSCTL_Field := NXP_SVD.SYSCON.Disable;
      --  Enables the clock for the USB0 Host Master.
      USB0_HOSTM     : AHBCLKCTRL2_USB0_HOSTM_Field := NXP_SVD.SYSCON.Disable;
      --  Enables the clock for the USB0 Host Slave.
      USB0_HOSTS     : AHBCLKCTRL2_USB0_HOSTS_Field := NXP_SVD.SYSCON.Disable;
      --  Enables the clock for the HASH_AES.
      HASH_AES       : AHBCLKCTRL2_HASH_AES_Field := NXP_SVD.SYSCON.Disable;
      --  Enables the clock for the Power Quad.
      PQ             : AHBCLKCTRL2_PQ_Field := NXP_SVD.SYSCON.Disable;
      --  Enables the clock for the PLU LUT.
      PLULUT         : AHBCLKCTRL2_PLULUT_Field := NXP_SVD.SYSCON.Disable;
      --  Enables the clock for the Timer 3.
      TIMER          : AHBCLKCTRL2_TIMER_Field :=
                        (As_Array => False, Val => 16#0#);
      --  Enables the clock for the PUF reset control.
      PUF            : AHBCLKCTRL2_PUF_Field := NXP_SVD.SYSCON.Disable;
      --  Enables the clock for the Casper.
      CASPER         : AHBCLKCTRL2_CASPER_Field := NXP_SVD.SYSCON.Disable;
      --  unspecified
      Reserved_25_26 : HAL.UInt2 := 16#0#;
      --  Enables the clock for the analog control.
      ANALOG_CTRL    : AHBCLKCTRL2_ANALOG_CTRL_Field :=
                        NXP_SVD.SYSCON.Disable;
      --  Enables the clock for the HS LSPI.
      HS_LSPI        : AHBCLKCTRL2_HS_LSPI_Field := NXP_SVD.SYSCON.Disable;
      --  Enables the clock for the GPIO secure.
      GPIO_SEC       : AHBCLKCTRL2_GPIO_SEC_Field := NXP_SVD.SYSCON.Disable;
      --  Enables the clock for the GPIO secure int.
      GPIO_SEC_INT   : AHBCLKCTRL2_GPIO_SEC_INT_Field :=
                        NXP_SVD.SYSCON.Disable;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for AHBCLKCTRL2_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      DMA1           at 0 range 1 .. 1;
      COMP           at 0 range 2 .. 2;
      SDIO           at 0 range 3 .. 3;
      USB1_HOST      at 0 range 4 .. 4;
      USB1_DEV       at 0 range 5 .. 5;
      USB1_RAM       at 0 range 6 .. 6;
      USB1_PHY       at 0 range 7 .. 7;
      FREQME         at 0 range 8 .. 8;
      Reserved_9_12  at 0 range 9 .. 12;
      RNG            at 0 range 13 .. 13;
      Reserved_14_14 at 0 range 14 .. 14;
      SYSCTL         at 0 range 15 .. 15;
      USB0_HOSTM     at 0 range 16 .. 16;
      USB0_HOSTS     at 0 range 17 .. 17;
      HASH_AES       at 0 range 18 .. 18;
      PQ             at 0 range 19 .. 19;
      PLULUT         at 0 range 20 .. 20;
      TIMER          at 0 range 21 .. 22;
      PUF            at 0 range 23 .. 23;
      CASPER         at 0 range 24 .. 24;
      Reserved_25_26 at 0 range 25 .. 26;
      ANALOG_CTRL    at 0 range 27 .. 27;
      HS_LSPI        at 0 range 28 .. 28;
      GPIO_SEC       at 0 range 29 .. 29;
      GPIO_SEC_INT   at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   --  Peripheral reset control register

   --  Peripheral reset control register
   type AHBCLKCTRLSET_Registers is array (0 .. 2) of HAL.UInt32
     with Volatile;

   --  Peripheral reset control register

   --  Peripheral reset control register
   type AHBCLKCTRLCLR_Registers is array (0 .. 2) of HAL.UInt32
     with Volatile;

   --  System Tick Timer for CPU0 source select.
   type SYSTICKCLKSEL0_SEL_Field is
     (
      --  System Tick 0 divided clock.
      Enum_0X0,
      --  FRO 1MHz clock.
      Enum_0X1,
      --  Oscillator 32 kHz clock.
      Enum_0X2,
      --  No clock.
      Enum_0X3,
      --  No clock.
      Enum_0X4,
      --  No clock.
      Enum_0X5,
      --  No clock.
      Enum_0X6,
      --  No clock.
      Enum_0X7)
     with Size => 3;
   for SYSTICKCLKSEL0_SEL_Field use
     (Enum_0X0 => 0,
      Enum_0X1 => 1,
      Enum_0X2 => 2,
      Enum_0X3 => 3,
      Enum_0X4 => 4,
      Enum_0X5 => 5,
      Enum_0X6 => 6,
      Enum_0X7 => 7);

   --  System Tick Timer for CPU0 source select
   type SYSTICKCLKSEL_Register is record
      --  System Tick Timer for CPU0 source select.
      SEL           : SYSTICKCLKSEL0_SEL_Field := NXP_SVD.SYSCON.Enum_0X7;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYSTICKCLKSEL_Register use record
      SEL           at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  Trace clock source select.
   type TRACECLKSEL_SEL_Field is
     (
      --  Trace divided clock.
      Enum_0X0,
      --  FRO 1MHz clock.
      Enum_0X1,
      --  Oscillator 32 kHz clock.
      Enum_0X2,
      --  No clock.
      Enum_0X3,
      --  No clock.
      Enum_0X4,
      --  No clock.
      Enum_0X5,
      --  No clock.
      Enum_0X6,
      --  No clock.
      Enum_0X7)
     with Size => 3;
   for TRACECLKSEL_SEL_Field use
     (Enum_0X0 => 0,
      Enum_0X1 => 1,
      Enum_0X2 => 2,
      Enum_0X3 => 3,
      Enum_0X4 => 4,
      Enum_0X5 => 5,
      Enum_0X6 => 6,
      Enum_0X7 => 7);

   --  Trace clock source select
   type TRACECLKSEL_Register is record
      --  Trace clock source select.
      SEL           : TRACECLKSEL_SEL_Field := NXP_SVD.SYSCON.Enum_0X7;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TRACECLKSEL_Register use record
      SEL           at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  CTimer 0 clock source select.
   type CTIMERCLKSEL0_SEL_Field is
     (
      --  Main clock.
      Enum_0X0,
      --  PLL0 clock.
      Enum_0X1,
      --  No clock.
      Enum_0X2,
      --  FRO 96 MHz clock.
      Enum_0X3,
      --  FRO 1MHz clock.
      Enum_0X4,
      --  MCLK clock.
      Enum_0X5,
      --  Oscillator 32kHz clock.
      Enum_0X6,
      --  No clock.
      Enum_0X7)
     with Size => 3;
   for CTIMERCLKSEL0_SEL_Field use
     (Enum_0X0 => 0,
      Enum_0X1 => 1,
      Enum_0X2 => 2,
      Enum_0X3 => 3,
      Enum_0X4 => 4,
      Enum_0X5 => 5,
      Enum_0X6 => 6,
      Enum_0X7 => 7);

   --  CTimer 0 clock source select
   type CTIMERCLKSEL_Register is record
      --  CTimer 0 clock source select.
      SEL           : CTIMERCLKSEL0_SEL_Field := NXP_SVD.SYSCON.Enum_0X7;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CTIMERCLKSEL_Register use record
      SEL           at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  Main clock A source select.
   type MAINCLKSELA_SEL_Field is
     (
      --  FRO 12 MHz clock.
      Enum_0X0,
      --  CLKIN clock.
      Enum_0X1,
      --  FRO 1MHz clock.
      Enum_0X2,
      --  FRO 96 MHz clock.
      Enum_0X3)
     with Size => 3;
   for MAINCLKSELA_SEL_Field use
     (Enum_0X0 => 0,
      Enum_0X1 => 1,
      Enum_0X2 => 2,
      Enum_0X3 => 3);

   --  Main clock A source select
   type MAINCLKSELA_Register is record
      --  Main clock A source select.
      SEL           : MAINCLKSELA_SEL_Field := NXP_SVD.SYSCON.Enum_0X0;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MAINCLKSELA_Register use record
      SEL           at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  Main clock source select.
   type MAINCLKSELB_SEL_Field is
     (
      --  Main Clock A.
      Enum_0X0,
      --  PLL0 clock.
      Enum_0X1,
      --  PLL1 clock.
      Enum_0X2,
      --  Oscillator 32 kHz clock.
      Enum_0X3)
     with Size => 3;
   for MAINCLKSELB_SEL_Field use
     (Enum_0X0 => 0,
      Enum_0X1 => 1,
      Enum_0X2 => 2,
      Enum_0X3 => 3);

   --  Main clock source select
   type MAINCLKSELB_Register is record
      --  Main clock source select.
      SEL           : MAINCLKSELB_SEL_Field := NXP_SVD.SYSCON.Enum_0X0;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MAINCLKSELB_Register use record
      SEL           at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  CLKOUT clock source select.
   type CLKOUTSEL_SEL_Field is
     (
      --  Main clock.
      Enum_0X0,
      --  PLL0 clock.
      Enum_0X1,
      --  CLKIN clock.
      Enum_0X2,
      --  FRO 96 MHz clock.
      Enum_0X3,
      --  FRO 1MHz clock.
      Enum_0X4,
      --  PLL1 clock.
      Enum_0X5,
      --  Oscillator 32kHz clock.
      Enum_0X6,
      --  No clock.
      Enum_0X7)
     with Size => 3;
   for CLKOUTSEL_SEL_Field use
     (Enum_0X0 => 0,
      Enum_0X1 => 1,
      Enum_0X2 => 2,
      Enum_0X3 => 3,
      Enum_0X4 => 4,
      Enum_0X5 => 5,
      Enum_0X6 => 6,
      Enum_0X7 => 7);

   --  CLKOUT clock source select
   type CLKOUTSEL_Register is record
      --  CLKOUT clock source select.
      SEL           : CLKOUTSEL_SEL_Field := NXP_SVD.SYSCON.Enum_0X7;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CLKOUTSEL_Register use record
      SEL           at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  PLL0 clock source select.
   type PLL0CLKSEL_SEL_Field is
     (
      --  FRO 12 MHz clock.
      Enum_0X0,
      --  CLKIN clock.
      Enum_0X1,
      --  FRO 1MHz clock.
      Enum_0X2,
      --  Oscillator 32kHz clock.
      Enum_0X3,
      --  No clock.
      Enum_0X4,
      --  No clock.
      Enum_0X5,
      --  No clock.
      Enum_0X6,
      --  No clock.
      Enum_0X7)
     with Size => 3;
   for PLL0CLKSEL_SEL_Field use
     (Enum_0X0 => 0,
      Enum_0X1 => 1,
      Enum_0X2 => 2,
      Enum_0X3 => 3,
      Enum_0X4 => 4,
      Enum_0X5 => 5,
      Enum_0X6 => 6,
      Enum_0X7 => 7);

   --  PLL0 clock source select
   type PLL0CLKSEL_Register is record
      --  PLL0 clock source select.
      SEL           : PLL0CLKSEL_SEL_Field := NXP_SVD.SYSCON.Enum_0X7;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PLL0CLKSEL_Register use record
      SEL           at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  PLL1 clock source select.
   type PLL1CLKSEL_SEL_Field is
     (
      --  FRO 12 MHz clock.
      Enum_0X0,
      --  CLKIN clock.
      Enum_0X1,
      --  FRO 1MHz clock.
      Enum_0X2,
      --  Oscillator 32kHz clock.
      Enum_0X3,
      --  No clock.
      Enum_0X4,
      --  No clock.
      Enum_0X5,
      --  No clock.
      Enum_0X6,
      --  No clock.
      Enum_0X7)
     with Size => 3;
   for PLL1CLKSEL_SEL_Field use
     (Enum_0X0 => 0,
      Enum_0X1 => 1,
      Enum_0X2 => 2,
      Enum_0X3 => 3,
      Enum_0X4 => 4,
      Enum_0X5 => 5,
      Enum_0X6 => 6,
      Enum_0X7 => 7);

   --  PLL1 clock source select
   type PLL1CLKSEL_Register is record
      --  PLL1 clock source select.
      SEL           : PLL1CLKSEL_SEL_Field := NXP_SVD.SYSCON.Enum_0X7;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PLL1CLKSEL_Register use record
      SEL           at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  ADC clock source select.
   type ADCCLKSEL_SEL_Field is
     (
      --  Main clock.
      Enum_0X0,
      --  PLL0 clock.
      Enum_0X1,
      --  FRO 96 MHz clock.
      Enum_0X2,
      --  No clock.
      Enum_0X4,
      --  No clock.
      Enum_0X5,
      --  No clock.
      Enum_0X6,
      --  No clock.
      Enum_0X7)
     with Size => 3;
   for ADCCLKSEL_SEL_Field use
     (Enum_0X0 => 0,
      Enum_0X1 => 1,
      Enum_0X2 => 2,
      Enum_0X4 => 4,
      Enum_0X5 => 5,
      Enum_0X6 => 6,
      Enum_0X7 => 7);

   --  ADC clock source select
   type ADCCLKSEL_Register is record
      --  ADC clock source select.
      SEL           : ADCCLKSEL_SEL_Field := NXP_SVD.SYSCON.Enum_0X7;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADCCLKSEL_Register use record
      SEL           at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  FS USB clock source select.
   type USB0CLKSEL_SEL_Field is
     (
      --  Main clock.
      Enum_0X0,
      --  PLL0 clock.
      Enum_0X1,
      --  No clock.
      Enum_0X2,
      --  FRO 96 MHz clock.
      Enum_0X3,
      --  No clock.
      Enum_0X4,
      --  PLL1 clock.
      Enum_0X5,
      --  No clock.
      Enum_0X6,
      --  No clock.
      Enum_0X7)
     with Size => 3;
   for USB0CLKSEL_SEL_Field use
     (Enum_0X0 => 0,
      Enum_0X1 => 1,
      Enum_0X2 => 2,
      Enum_0X3 => 3,
      Enum_0X4 => 4,
      Enum_0X5 => 5,
      Enum_0X6 => 6,
      Enum_0X7 => 7);

   --  FS USB clock source select
   type USB0CLKSEL_Register is record
      --  FS USB clock source select.
      SEL           : USB0CLKSEL_SEL_Field := NXP_SVD.SYSCON.Enum_0X7;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for USB0CLKSEL_Register use record
      SEL           at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  Flexcomm Interface 0 clock source select for Fractional Rate Divider.
   type FCCLKSEL0_SEL_Field is
     (
      --  Main clock.
      Enum_0X0,
      --  system PLL divided clock.
      Enum_0X1,
      --  FRO 12 MHz clock.
      Enum_0X2,
      --  FRO 96 MHz clock.
      Enum_0X3,
      --  FRO 1MHz clock.
      Enum_0X4,
      --  MCLK clock.
      Enum_0X5,
      --  Oscillator 32 kHz clock.
      Enum_0X6,
      --  No clock.
      Enum_0X7)
     with Size => 3;
   for FCCLKSEL0_SEL_Field use
     (Enum_0X0 => 0,
      Enum_0X1 => 1,
      Enum_0X2 => 2,
      Enum_0X3 => 3,
      Enum_0X4 => 4,
      Enum_0X5 => 5,
      Enum_0X6 => 6,
      Enum_0X7 => 7);

   --  Flexcomm Interface 0 clock source select for Fractional Rate Divider
   type FCCLKSEL_Register is record
      --  Flexcomm Interface 0 clock source select for Fractional Rate Divider.
      SEL           : FCCLKSEL0_SEL_Field := NXP_SVD.SYSCON.Enum_0X7;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FCCLKSEL_Register use record
      SEL           at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  HS LSPI clock source select.
   type HSLSPICLKSEL_SEL_Field is
     (
      --  Main clock.
      Enum_0X0,
      --  system PLL divided clock.
      Enum_0X1,
      --  FRO 12 MHz clock.
      Enum_0X2,
      --  FRO 96 MHz clock.
      Enum_0X3,
      --  FRO 1MHz clock.
      Enum_0X4,
      --  No clock.
      Enum_0X5,
      --  Oscillator 32 kHz clock.
      Enum_0X6,
      --  No clock.
      Enum_0X7)
     with Size => 3;
   for HSLSPICLKSEL_SEL_Field use
     (Enum_0X0 => 0,
      Enum_0X1 => 1,
      Enum_0X2 => 2,
      Enum_0X3 => 3,
      Enum_0X4 => 4,
      Enum_0X5 => 5,
      Enum_0X6 => 6,
      Enum_0X7 => 7);

   --  HS LSPI clock source select
   type HSLSPICLKSEL_Register is record
      --  HS LSPI clock source select.
      SEL           : HSLSPICLKSEL_SEL_Field := NXP_SVD.SYSCON.Enum_0X7;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSLSPICLKSEL_Register use record
      SEL           at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  MCLK clock source select.
   type MCLKCLKSEL_SEL_Field is
     (
      --  FRO 96 MHz clock.
      Enum_0X0,
      --  PLL0 clock.
      Enum_0X1,
      --  No clock.
      Enum_0X4,
      --  No clock.
      Enum_0X5,
      --  No clock.
      Enum_0X6,
      --  No clock.
      Enum_0X7)
     with Size => 3;
   for MCLKCLKSEL_SEL_Field use
     (Enum_0X0 => 0,
      Enum_0X1 => 1,
      Enum_0X4 => 4,
      Enum_0X5 => 5,
      Enum_0X6 => 6,
      Enum_0X7 => 7);

   --  MCLK clock source select
   type MCLKCLKSEL_Register is record
      --  MCLK clock source select.
      SEL           : MCLKCLKSEL_SEL_Field := NXP_SVD.SYSCON.Enum_0X7;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MCLKCLKSEL_Register use record
      SEL           at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  SCTimer/PWM clock source select.
   type SCTCLKSEL_SEL_Field is
     (
      --  Main clock.
      Enum_0X0,
      --  PLL0 clock.
      Enum_0X1,
      --  CLKIN clock.
      Enum_0X2,
      --  FRO 96 MHz clock.
      Enum_0X3,
      --  No clock.
      Enum_0X4,
      --  MCLK clock.
      Enum_0X5,
      --  No clock.
      Enum_0X6,
      --  No clock.
      Enum_0X7)
     with Size => 3;
   for SCTCLKSEL_SEL_Field use
     (Enum_0X0 => 0,
      Enum_0X1 => 1,
      Enum_0X2 => 2,
      Enum_0X3 => 3,
      Enum_0X4 => 4,
      Enum_0X5 => 5,
      Enum_0X6 => 6,
      Enum_0X7 => 7);

   --  SCTimer/PWM clock source select
   type SCTCLKSEL_Register is record
      --  SCTimer/PWM clock source select.
      SEL           : SCTCLKSEL_SEL_Field := NXP_SVD.SYSCON.Enum_0X7;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SCTCLKSEL_Register use record
      SEL           at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  SDIO clock source select.
   type SDIOCLKSEL_SEL_Field is
     (
      --  Main clock.
      Enum_0X0,
      --  PLL0 clock.
      Enum_0X1,
      --  No clock.
      Enum_0X2,
      --  FRO 96 MHz clock.
      Enum_0X3,
      --  No clock.
      Enum_0X4,
      --  PLL1 clock.
      Enum_0X5,
      --  No clock.
      Enum_0X6,
      --  No clock.
      Enum_0X7)
     with Size => 3;
   for SDIOCLKSEL_SEL_Field use
     (Enum_0X0 => 0,
      Enum_0X1 => 1,
      Enum_0X2 => 2,
      Enum_0X3 => 3,
      Enum_0X4 => 4,
      Enum_0X5 => 5,
      Enum_0X6 => 6,
      Enum_0X7 => 7);

   --  SDIO clock source select
   type SDIOCLKSEL_Register is record
      --  SDIO clock source select.
      SEL           : SDIOCLKSEL_SEL_Field := NXP_SVD.SYSCON.Enum_0X7;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SDIOCLKSEL_Register use record
      SEL           at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype SYSTICKCLKDIV_DIV_Field is HAL.UInt8;

   --  Resets the divider counter.
   type SYSTICKCLKDIV0_RESET_Field is
     (
      --  Divider is not reset.
      Released,
      --  Divider is reset.
      Asserted)
     with Size => 1;
   for SYSTICKCLKDIV0_RESET_Field use
     (Released => 0,
      Asserted => 1);

   --  Halts the divider counter.
   type SYSTICKCLKDIV0_HALT_Field is
     (
      --  Divider clock is running.
      Run,
      --  Divider clock is stoped.
      Halt)
     with Size => 1;
   for SYSTICKCLKDIV0_HALT_Field use
     (Run => 0,
      Halt => 1);

   --  Divider status flag.
   type SYSTICKCLKDIV0_REQFLAG_Field is
     (
      --  Divider clock is stable.
      Stable,
      --  Clock frequency is not stable.
      Ongoing)
     with Size => 1;
   for SYSTICKCLKDIV0_REQFLAG_Field use
     (Stable => 0,
      Ongoing => 1);

   --  System Tick Timer divider for CPU0
   type SYSTICKCLKDIV_Register is record
      --  Clock divider value.
      DIV           : SYSTICKCLKDIV_DIV_Field := 16#0#;
      --  unspecified
      Reserved_8_28 : HAL.UInt21 := 16#0#;
      --  Write-only. Resets the divider counter.
      RESET         : SYSTICKCLKDIV0_RESET_Field := NXP_SVD.SYSCON.Released;
      --  Halts the divider counter.
      HALT          : SYSTICKCLKDIV0_HALT_Field := NXP_SVD.SYSCON.Halt;
      --  Read-only. Divider status flag.
      REQFLAG       : SYSTICKCLKDIV0_REQFLAG_Field := NXP_SVD.SYSCON.Stable;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYSTICKCLKDIV_Register use record
      DIV           at 0 range 0 .. 7;
      Reserved_8_28 at 0 range 8 .. 28;
      RESET         at 0 range 29 .. 29;
      HALT          at 0 range 30 .. 30;
      REQFLAG       at 0 range 31 .. 31;
   end record;

   subtype TRACECLKDIV_DIV_Field is HAL.UInt8;

   --  Resets the divider counter.
   type TRACECLKDIV_RESET_Field is
     (
      --  Divider is not reset.
      Released,
      --  Divider is reset.
      Asserted)
     with Size => 1;
   for TRACECLKDIV_RESET_Field use
     (Released => 0,
      Asserted => 1);

   --  Halts the divider counter.
   type TRACECLKDIV_HALT_Field is
     (
      --  Divider clock is running.
      Run,
      --  Divider clock is stoped.
      Halt)
     with Size => 1;
   for TRACECLKDIV_HALT_Field use
     (Run => 0,
      Halt => 1);

   --  Divider status flag.
   type TRACECLKDIV_REQFLAG_Field is
     (
      --  Divider clock is stable.
      Stable,
      --  Clock frequency is not stable.
      Ongoing)
     with Size => 1;
   for TRACECLKDIV_REQFLAG_Field use
     (Stable => 0,
      Ongoing => 1);

   --  TRACE clock divider
   type TRACECLKDIV_Register is record
      --  Clock divider value.
      DIV           : TRACECLKDIV_DIV_Field := 16#0#;
      --  unspecified
      Reserved_8_28 : HAL.UInt21 := 16#0#;
      --  Write-only. Resets the divider counter.
      RESET         : TRACECLKDIV_RESET_Field := NXP_SVD.SYSCON.Released;
      --  Halts the divider counter.
      HALT          : TRACECLKDIV_HALT_Field := NXP_SVD.SYSCON.Halt;
      --  Read-only. Divider status flag.
      REQFLAG       : TRACECLKDIV_REQFLAG_Field := NXP_SVD.SYSCON.Stable;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TRACECLKDIV_Register use record
      DIV           at 0 range 0 .. 7;
      Reserved_8_28 at 0 range 8 .. 28;
      RESET         at 0 range 29 .. 29;
      HALT          at 0 range 30 .. 30;
      REQFLAG       at 0 range 31 .. 31;
   end record;

   subtype FLEXFRG0CTRL_DIV_Field is HAL.UInt8;
   subtype FLEXFRG0CTRL_MULT_Field is HAL.UInt8;

   --  Fractional rate divider for flexcomm 0
   type FLEXFRG0CTRL_Register is record
      --  Denominator of the fractional rate divider.
      DIV            : FLEXFRG0CTRL_DIV_Field := 16#FF#;
      --  Numerator of the fractional rate divider.
      MULT           : FLEXFRG0CTRL_MULT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FLEXFRG0CTRL_Register use record
      DIV            at 0 range 0 .. 7;
      MULT           at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype FLEXFRG1CTRL_DIV_Field is HAL.UInt8;
   subtype FLEXFRG1CTRL_MULT_Field is HAL.UInt8;

   --  Fractional rate divider for flexcomm 1
   type FLEXFRG1CTRL_Register is record
      --  Denominator of the fractional rate divider.
      DIV            : FLEXFRG1CTRL_DIV_Field := 16#FF#;
      --  Numerator of the fractional rate divider.
      MULT           : FLEXFRG1CTRL_MULT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FLEXFRG1CTRL_Register use record
      DIV            at 0 range 0 .. 7;
      MULT           at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype FLEXFRG2CTRL_DIV_Field is HAL.UInt8;
   subtype FLEXFRG2CTRL_MULT_Field is HAL.UInt8;

   --  Fractional rate divider for flexcomm 2
   type FLEXFRG2CTRL_Register is record
      --  Denominator of the fractional rate divider.
      DIV            : FLEXFRG2CTRL_DIV_Field := 16#FF#;
      --  Numerator of the fractional rate divider.
      MULT           : FLEXFRG2CTRL_MULT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FLEXFRG2CTRL_Register use record
      DIV            at 0 range 0 .. 7;
      MULT           at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype FLEXFRG3CTRL_DIV_Field is HAL.UInt8;
   subtype FLEXFRG3CTRL_MULT_Field is HAL.UInt8;

   --  Fractional rate divider for flexcomm 3
   type FLEXFRG3CTRL_Register is record
      --  Denominator of the fractional rate divider.
      DIV            : FLEXFRG3CTRL_DIV_Field := 16#FF#;
      --  Numerator of the fractional rate divider.
      MULT           : FLEXFRG3CTRL_MULT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FLEXFRG3CTRL_Register use record
      DIV            at 0 range 0 .. 7;
      MULT           at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype FLEXFRG4CTRL_DIV_Field is HAL.UInt8;
   subtype FLEXFRG4CTRL_MULT_Field is HAL.UInt8;

   --  Fractional rate divider for flexcomm 4
   type FLEXFRG4CTRL_Register is record
      --  Denominator of the fractional rate divider.
      DIV            : FLEXFRG4CTRL_DIV_Field := 16#FF#;
      --  Numerator of the fractional rate divider.
      MULT           : FLEXFRG4CTRL_MULT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FLEXFRG4CTRL_Register use record
      DIV            at 0 range 0 .. 7;
      MULT           at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype FLEXFRG5CTRL_DIV_Field is HAL.UInt8;
   subtype FLEXFRG5CTRL_MULT_Field is HAL.UInt8;

   --  Fractional rate divider for flexcomm 5
   type FLEXFRG5CTRL_Register is record
      --  Denominator of the fractional rate divider.
      DIV            : FLEXFRG5CTRL_DIV_Field := 16#FF#;
      --  Numerator of the fractional rate divider.
      MULT           : FLEXFRG5CTRL_MULT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FLEXFRG5CTRL_Register use record
      DIV            at 0 range 0 .. 7;
      MULT           at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype FLEXFRG6CTRL_DIV_Field is HAL.UInt8;
   subtype FLEXFRG6CTRL_MULT_Field is HAL.UInt8;

   --  Fractional rate divider for flexcomm 6
   type FLEXFRG6CTRL_Register is record
      --  Denominator of the fractional rate divider.
      DIV            : FLEXFRG6CTRL_DIV_Field := 16#FF#;
      --  Numerator of the fractional rate divider.
      MULT           : FLEXFRG6CTRL_MULT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FLEXFRG6CTRL_Register use record
      DIV            at 0 range 0 .. 7;
      MULT           at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype FLEXFRG7CTRL_DIV_Field is HAL.UInt8;
   subtype FLEXFRG7CTRL_MULT_Field is HAL.UInt8;

   --  Fractional rate divider for flexcomm 7
   type FLEXFRG7CTRL_Register is record
      --  Denominator of the fractional rate divider.
      DIV            : FLEXFRG7CTRL_DIV_Field := 16#FF#;
      --  Numerator of the fractional rate divider.
      MULT           : FLEXFRG7CTRL_MULT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FLEXFRG7CTRL_Register use record
      DIV            at 0 range 0 .. 7;
      MULT           at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype AHBCLKDIV_DIV_Field is HAL.UInt8;

   --  Resets the divider counter.
   type AHBCLKDIV_RESET_Field is
     (
      --  Divider is not reset.
      Released,
      --  Divider is reset.
      Asserted)
     with Size => 1;
   for AHBCLKDIV_RESET_Field use
     (Released => 0,
      Asserted => 1);

   --  Halts the divider counter.
   type AHBCLKDIV_HALT_Field is
     (
      --  Divider clock is running.
      Run,
      --  Divider clock is stoped.
      Halt)
     with Size => 1;
   for AHBCLKDIV_HALT_Field use
     (Run => 0,
      Halt => 1);

   --  Divider status flag.
   type AHBCLKDIV_REQFLAG_Field is
     (
      --  Divider clock is stable.
      Stable,
      --  Clock frequency is not stable.
      Ongoing)
     with Size => 1;
   for AHBCLKDIV_REQFLAG_Field use
     (Stable => 0,
      Ongoing => 1);

   --  System clock divider
   type AHBCLKDIV_Register is record
      --  Clock divider value.
      DIV           : AHBCLKDIV_DIV_Field := 16#0#;
      --  unspecified
      Reserved_8_28 : HAL.UInt21 := 16#0#;
      --  Write-only. Resets the divider counter.
      RESET         : AHBCLKDIV_RESET_Field := NXP_SVD.SYSCON.Released;
      --  Halts the divider counter.
      HALT          : AHBCLKDIV_HALT_Field := NXP_SVD.SYSCON.Run;
      --  Read-only. Divider status flag.
      REQFLAG       : AHBCLKDIV_REQFLAG_Field := NXP_SVD.SYSCON.Stable;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for AHBCLKDIV_Register use record
      DIV           at 0 range 0 .. 7;
      Reserved_8_28 at 0 range 8 .. 28;
      RESET         at 0 range 29 .. 29;
      HALT          at 0 range 30 .. 30;
      REQFLAG       at 0 range 31 .. 31;
   end record;

   subtype CLKOUTDIV_DIV_Field is HAL.UInt8;

   --  Resets the divider counter.
   type CLKOUTDIV_RESET_Field is
     (
      --  Divider is not reset.
      Released,
      --  Divider is reset.
      Asserted)
     with Size => 1;
   for CLKOUTDIV_RESET_Field use
     (Released => 0,
      Asserted => 1);

   --  Halts the divider counter.
   type CLKOUTDIV_HALT_Field is
     (
      --  Divider clock is running.
      Run,
      --  Divider clock is stoped.
      Halt)
     with Size => 1;
   for CLKOUTDIV_HALT_Field use
     (Run => 0,
      Halt => 1);

   --  Divider status flag.
   type CLKOUTDIV_REQFLAG_Field is
     (
      --  Divider clock is stable.
      Stable,
      --  Clock frequency is not stable.
      Ongoing)
     with Size => 1;
   for CLKOUTDIV_REQFLAG_Field use
     (Stable => 0,
      Ongoing => 1);

   --  CLKOUT clock divider
   type CLKOUTDIV_Register is record
      --  Clock divider value.
      DIV           : CLKOUTDIV_DIV_Field := 16#0#;
      --  unspecified
      Reserved_8_28 : HAL.UInt21 := 16#0#;
      --  Write-only. Resets the divider counter.
      RESET         : CLKOUTDIV_RESET_Field := NXP_SVD.SYSCON.Released;
      --  Halts the divider counter.
      HALT          : CLKOUTDIV_HALT_Field := NXP_SVD.SYSCON.Halt;
      --  Read-only. Divider status flag.
      REQFLAG       : CLKOUTDIV_REQFLAG_Field := NXP_SVD.SYSCON.Stable;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CLKOUTDIV_Register use record
      DIV           at 0 range 0 .. 7;
      Reserved_8_28 at 0 range 8 .. 28;
      RESET         at 0 range 29 .. 29;
      HALT          at 0 range 30 .. 30;
      REQFLAG       at 0 range 31 .. 31;
   end record;

   subtype FROHFDIV_DIV_Field is HAL.UInt8;

   --  Resets the divider counter.
   type FROHFDIV_RESET_Field is
     (
      --  Divider is not reset.
      Released,
      --  Divider is reset.
      Asserted)
     with Size => 1;
   for FROHFDIV_RESET_Field use
     (Released => 0,
      Asserted => 1);

   --  Halts the divider counter.
   type FROHFDIV_HALT_Field is
     (
      --  Divider clock is running.
      Run,
      --  Divider clock is stoped.
      Halt)
     with Size => 1;
   for FROHFDIV_HALT_Field use
     (Run => 0,
      Halt => 1);

   --  Divider status flag.
   type FROHFDIV_REQFLAG_Field is
     (
      --  Divider clock is stable.
      Stable,
      --  Clock frequency is not stable.
      Ongoing)
     with Size => 1;
   for FROHFDIV_REQFLAG_Field use
     (Stable => 0,
      Ongoing => 1);

   --  FRO_HF (96MHz) clock divider
   type FROHFDIV_Register is record
      --  Clock divider value.
      DIV           : FROHFDIV_DIV_Field := 16#0#;
      --  unspecified
      Reserved_8_28 : HAL.UInt21 := 16#0#;
      --  Write-only. Resets the divider counter.
      RESET         : FROHFDIV_RESET_Field := NXP_SVD.SYSCON.Released;
      --  Halts the divider counter.
      HALT          : FROHFDIV_HALT_Field := NXP_SVD.SYSCON.Halt;
      --  Read-only. Divider status flag.
      REQFLAG       : FROHFDIV_REQFLAG_Field := NXP_SVD.SYSCON.Stable;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FROHFDIV_Register use record
      DIV           at 0 range 0 .. 7;
      Reserved_8_28 at 0 range 8 .. 28;
      RESET         at 0 range 29 .. 29;
      HALT          at 0 range 30 .. 30;
      REQFLAG       at 0 range 31 .. 31;
   end record;

   subtype WDTCLKDIV_DIV_Field is HAL.UInt6;

   --  Resets the divider counter.
   type WDTCLKDIV_RESET_Field is
     (
      --  Divider is not reset.
      Released,
      --  Divider is reset.
      Asserted)
     with Size => 1;
   for WDTCLKDIV_RESET_Field use
     (Released => 0,
      Asserted => 1);

   --  Halts the divider counter.
   type WDTCLKDIV_HALT_Field is
     (
      --  Divider clock is running.
      Run,
      --  Divider clock is stoped.
      Halt)
     with Size => 1;
   for WDTCLKDIV_HALT_Field use
     (Run => 0,
      Halt => 1);

   --  Divider status flag.
   type WDTCLKDIV_REQFLAG_Field is
     (
      --  Divider clock is stable.
      Stable,
      --  Clock frequency is not stable.
      Ongoing)
     with Size => 1;
   for WDTCLKDIV_REQFLAG_Field use
     (Stable => 0,
      Ongoing => 1);

   --  WDT clock divider
   type WDTCLKDIV_Register is record
      --  Clock divider value.
      DIV           : WDTCLKDIV_DIV_Field := 16#0#;
      --  unspecified
      Reserved_6_28 : HAL.UInt23 := 16#0#;
      --  Write-only. Resets the divider counter.
      RESET         : WDTCLKDIV_RESET_Field := NXP_SVD.SYSCON.Released;
      --  Halts the divider counter.
      HALT          : WDTCLKDIV_HALT_Field := NXP_SVD.SYSCON.Halt;
      --  Read-only. Divider status flag.
      REQFLAG       : WDTCLKDIV_REQFLAG_Field := NXP_SVD.SYSCON.Stable;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for WDTCLKDIV_Register use record
      DIV           at 0 range 0 .. 5;
      Reserved_6_28 at 0 range 6 .. 28;
      RESET         at 0 range 29 .. 29;
      HALT          at 0 range 30 .. 30;
      REQFLAG       at 0 range 31 .. 31;
   end record;

   subtype ADCCLKDIV_DIV_Field is HAL.UInt3;

   --  Resets the divider counter.
   type ADCCLKDIV_RESET_Field is
     (
      --  Divider is not reset.
      Released,
      --  Divider is reset.
      Asserted)
     with Size => 1;
   for ADCCLKDIV_RESET_Field use
     (Released => 0,
      Asserted => 1);

   --  Halts the divider counter.
   type ADCCLKDIV_HALT_Field is
     (
      --  Divider clock is running.
      Run,
      --  Divider clock is stoped.
      Halt)
     with Size => 1;
   for ADCCLKDIV_HALT_Field use
     (Run => 0,
      Halt => 1);

   --  Divider status flag.
   type ADCCLKDIV_REQFLAG_Field is
     (
      --  Divider clock is stable.
      Stable,
      --  Clock frequency is not stable.
      Ongoing)
     with Size => 1;
   for ADCCLKDIV_REQFLAG_Field use
     (Stable => 0,
      Ongoing => 1);

   --  ADC clock divider
   type ADCCLKDIV_Register is record
      --  Clock divider value.
      DIV           : ADCCLKDIV_DIV_Field := 16#0#;
      --  unspecified
      Reserved_3_28 : HAL.UInt26 := 16#0#;
      --  Write-only. Resets the divider counter.
      RESET         : ADCCLKDIV_RESET_Field := NXP_SVD.SYSCON.Released;
      --  Halts the divider counter.
      HALT          : ADCCLKDIV_HALT_Field := NXP_SVD.SYSCON.Halt;
      --  Read-only. Divider status flag.
      REQFLAG       : ADCCLKDIV_REQFLAG_Field := NXP_SVD.SYSCON.Stable;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADCCLKDIV_Register use record
      DIV           at 0 range 0 .. 2;
      Reserved_3_28 at 0 range 3 .. 28;
      RESET         at 0 range 29 .. 29;
      HALT          at 0 range 30 .. 30;
      REQFLAG       at 0 range 31 .. 31;
   end record;

   subtype USB0CLKDIV_DIV_Field is HAL.UInt8;

   --  Resets the divider counter.
   type USB0CLKDIV_RESET_Field is
     (
      --  Divider is not reset.
      Released,
      --  Divider is reset.
      Asserted)
     with Size => 1;
   for USB0CLKDIV_RESET_Field use
     (Released => 0,
      Asserted => 1);

   --  Halts the divider counter.
   type USB0CLKDIV_HALT_Field is
     (
      --  Divider clock is running.
      Run,
      --  Divider clock is stoped.
      Halt)
     with Size => 1;
   for USB0CLKDIV_HALT_Field use
     (Run => 0,
      Halt => 1);

   --  Divider status flag.
   type USB0CLKDIV_REQFLAG_Field is
     (
      --  Divider clock is stable.
      Stable,
      --  Clock frequency is not stable.
      Ongoing)
     with Size => 1;
   for USB0CLKDIV_REQFLAG_Field use
     (Stable => 0,
      Ongoing => 1);

   --  USB0 Clock divider
   type USB0CLKDIV_Register is record
      --  Clock divider value.
      DIV           : USB0CLKDIV_DIV_Field := 16#0#;
      --  unspecified
      Reserved_8_28 : HAL.UInt21 := 16#0#;
      --  Write-only. Resets the divider counter.
      RESET         : USB0CLKDIV_RESET_Field := NXP_SVD.SYSCON.Released;
      --  Halts the divider counter.
      HALT          : USB0CLKDIV_HALT_Field := NXP_SVD.SYSCON.Halt;
      --  Read-only. Divider status flag.
      REQFLAG       : USB0CLKDIV_REQFLAG_Field := NXP_SVD.SYSCON.Stable;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for USB0CLKDIV_Register use record
      DIV           at 0 range 0 .. 7;
      Reserved_8_28 at 0 range 8 .. 28;
      RESET         at 0 range 29 .. 29;
      HALT          at 0 range 30 .. 30;
      REQFLAG       at 0 range 31 .. 31;
   end record;

   subtype MCLKDIV_DIV_Field is HAL.UInt8;

   --  Resets the divider counter.
   type MCLKDIV_RESET_Field is
     (
      --  Divider is not reset.
      Released,
      --  Divider is reset.
      Asserted)
     with Size => 1;
   for MCLKDIV_RESET_Field use
     (Released => 0,
      Asserted => 1);

   --  Halts the divider counter.
   type MCLKDIV_HALT_Field is
     (
      --  Divider clock is running.
      Run,
      --  Divider clock is stoped.
      Halt)
     with Size => 1;
   for MCLKDIV_HALT_Field use
     (Run => 0,
      Halt => 1);

   --  Divider status flag.
   type MCLKDIV_REQFLAG_Field is
     (
      --  Divider clock is stable.
      Stable,
      --  Clock frequency is not stable.
      Ongoing)
     with Size => 1;
   for MCLKDIV_REQFLAG_Field use
     (Stable => 0,
      Ongoing => 1);

   --  I2S MCLK clock divider
   type MCLKDIV_Register is record
      --  Clock divider value.
      DIV           : MCLKDIV_DIV_Field := 16#0#;
      --  unspecified
      Reserved_8_28 : HAL.UInt21 := 16#0#;
      --  Write-only. Resets the divider counter.
      RESET         : MCLKDIV_RESET_Field := NXP_SVD.SYSCON.Released;
      --  Halts the divider counter.
      HALT          : MCLKDIV_HALT_Field := NXP_SVD.SYSCON.Halt;
      --  Read-only. Divider status flag.
      REQFLAG       : MCLKDIV_REQFLAG_Field := NXP_SVD.SYSCON.Stable;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MCLKDIV_Register use record
      DIV           at 0 range 0 .. 7;
      Reserved_8_28 at 0 range 8 .. 28;
      RESET         at 0 range 29 .. 29;
      HALT          at 0 range 30 .. 30;
      REQFLAG       at 0 range 31 .. 31;
   end record;

   subtype SCTCLKDIV_DIV_Field is HAL.UInt8;

   --  Resets the divider counter.
   type SCTCLKDIV_RESET_Field is
     (
      --  Divider is not reset.
      Released,
      --  Divider is reset.
      Asserted)
     with Size => 1;
   for SCTCLKDIV_RESET_Field use
     (Released => 0,
      Asserted => 1);

   --  Halts the divider counter.
   type SCTCLKDIV_HALT_Field is
     (
      --  Divider clock is running.
      Run,
      --  Divider clock is stoped.
      Halt)
     with Size => 1;
   for SCTCLKDIV_HALT_Field use
     (Run => 0,
      Halt => 1);

   --  Divider status flag.
   type SCTCLKDIV_REQFLAG_Field is
     (
      --  Divider clock is stable.
      Stable,
      --  Clock frequency is not stable.
      Ongoing)
     with Size => 1;
   for SCTCLKDIV_REQFLAG_Field use
     (Stable => 0,
      Ongoing => 1);

   --  SCT/PWM clock divider
   type SCTCLKDIV_Register is record
      --  Clock divider value.
      DIV           : SCTCLKDIV_DIV_Field := 16#0#;
      --  unspecified
      Reserved_8_28 : HAL.UInt21 := 16#0#;
      --  Write-only. Resets the divider counter.
      RESET         : SCTCLKDIV_RESET_Field := NXP_SVD.SYSCON.Released;
      --  Halts the divider counter.
      HALT          : SCTCLKDIV_HALT_Field := NXP_SVD.SYSCON.Halt;
      --  Read-only. Divider status flag.
      REQFLAG       : SCTCLKDIV_REQFLAG_Field := NXP_SVD.SYSCON.Stable;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SCTCLKDIV_Register use record
      DIV           at 0 range 0 .. 7;
      Reserved_8_28 at 0 range 8 .. 28;
      RESET         at 0 range 29 .. 29;
      HALT          at 0 range 30 .. 30;
      REQFLAG       at 0 range 31 .. 31;
   end record;

   subtype SDIOCLKDIV_DIV_Field is HAL.UInt8;

   --  Resets the divider counter.
   type SDIOCLKDIV_RESET_Field is
     (
      --  Divider is not reset.
      Released,
      --  Divider is reset.
      Asserted)
     with Size => 1;
   for SDIOCLKDIV_RESET_Field use
     (Released => 0,
      Asserted => 1);

   --  Halts the divider counter.
   type SDIOCLKDIV_HALT_Field is
     (
      --  Divider clock is running.
      Run,
      --  Divider clock is stoped.
      Halt)
     with Size => 1;
   for SDIOCLKDIV_HALT_Field use
     (Run => 0,
      Halt => 1);

   --  Divider status flag.
   type SDIOCLKDIV_REQFLAG_Field is
     (
      --  Divider clock is stable.
      Stable,
      --  Clock frequency is not stable.
      Ongoing)
     with Size => 1;
   for SDIOCLKDIV_REQFLAG_Field use
     (Stable => 0,
      Ongoing => 1);

   --  SDIO clock divider
   type SDIOCLKDIV_Register is record
      --  Clock divider value.
      DIV           : SDIOCLKDIV_DIV_Field := 16#0#;
      --  unspecified
      Reserved_8_28 : HAL.UInt21 := 16#0#;
      --  Write-only. Resets the divider counter.
      RESET         : SDIOCLKDIV_RESET_Field := NXP_SVD.SYSCON.Released;
      --  Halts the divider counter.
      HALT          : SDIOCLKDIV_HALT_Field := NXP_SVD.SYSCON.Halt;
      --  Read-only. Divider status flag.
      REQFLAG       : SDIOCLKDIV_REQFLAG_Field := NXP_SVD.SYSCON.Stable;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SDIOCLKDIV_Register use record
      DIV           at 0 range 0 .. 7;
      Reserved_8_28 at 0 range 8 .. 28;
      RESET         at 0 range 29 .. 29;
      HALT          at 0 range 30 .. 30;
      REQFLAG       at 0 range 31 .. 31;
   end record;

   subtype PLL0CLKDIV_DIV_Field is HAL.UInt8;

   --  Resets the divider counter.
   type PLL0CLKDIV_RESET_Field is
     (
      --  Divider is not reset.
      Released,
      --  Divider is reset.
      Asserted)
     with Size => 1;
   for PLL0CLKDIV_RESET_Field use
     (Released => 0,
      Asserted => 1);

   --  Halts the divider counter.
   type PLL0CLKDIV_HALT_Field is
     (
      --  Divider clock is running.
      Run,
      --  Divider clock is stoped.
      Halt)
     with Size => 1;
   for PLL0CLKDIV_HALT_Field use
     (Run => 0,
      Halt => 1);

   --  Divider status flag.
   type PLL0CLKDIV_REQFLAG_Field is
     (
      --  Divider clock is stable.
      Stable,
      --  Clock frequency is not stable.
      Ongoing)
     with Size => 1;
   for PLL0CLKDIV_REQFLAG_Field use
     (Stable => 0,
      Ongoing => 1);

   --  PLL0 clock divider
   type PLL0CLKDIV_Register is record
      --  Clock divider value.
      DIV           : PLL0CLKDIV_DIV_Field := 16#0#;
      --  unspecified
      Reserved_8_28 : HAL.UInt21 := 16#0#;
      --  Write-only. Resets the divider counter.
      RESET         : PLL0CLKDIV_RESET_Field := NXP_SVD.SYSCON.Released;
      --  Halts the divider counter.
      HALT          : PLL0CLKDIV_HALT_Field := NXP_SVD.SYSCON.Halt;
      --  Read-only. Divider status flag.
      REQFLAG       : PLL0CLKDIV_REQFLAG_Field := NXP_SVD.SYSCON.Stable;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PLL0CLKDIV_Register use record
      DIV           at 0 range 0 .. 7;
      Reserved_8_28 at 0 range 8 .. 28;
      RESET         at 0 range 29 .. 29;
      HALT          at 0 range 30 .. 30;
      REQFLAG       at 0 range 31 .. 31;
   end record;

   --  Flash memory access time.
   type FMCCR_FLASHTIM_Field is
     (
      --  1 system clock flash access time (for system clock rates up to 11
      --  MHz).
      Flashtim0,
      --  2 system clocks flash access time (for system clock rates up to 22
      --  MHz).
      Flashtim1,
      --  3 system clocks flash access time (for system clock rates up to 33
      --  MHz).
      Flashtim2,
      --  4 system clocks flash access time (for system clock rates up to 44
      --  MHz).
      Flashtim3,
      --  5 system clocks flash access time (for system clock rates up to 55
      --  MHz).
      Flashtim4,
      --  6 system clocks flash access time (for system clock rates up to 66
      --  MHz).
      Flashtim5,
      --  7 system clocks flash access time (for system clock rates up to 77
      --  MHz).
      Flashtim6,
      --  8 system clocks flash access time (for system clock rates up to 88
      --  MHz).
      Flashtim7,
      --  9 system clocks flash access time (for system clock rates up to 100
      --  MHz).
      Flashtim8)
     with Size => 4;
   for FMCCR_FLASHTIM_Field use
     (Flashtim0 => 0,
      Flashtim1 => 1,
      Flashtim2 => 2,
      Flashtim3 => 3,
      Flashtim4 => 4,
      Flashtim5 => 5,
      Flashtim6 => 6,
      Flashtim7 => 7,
      Flashtim8 => 8);

   --  FMC configuration register
   type FMCCR_Register is record
      --  unspecified
      Reserved_0_11  : HAL.UInt12 := 16#0#;
      --  Flash memory access time.
      FLASHTIM       : FMCCR_FLASHTIM_Field := NXP_SVD.SYSCON.Flashtim2;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FMCCR_Register use record
      Reserved_0_11  at 0 range 0 .. 11;
      FLASHTIM       at 0 range 12 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  USB0 Device USB0_NEEDCLK signal control:.
   type USB0NEEDCLKCTRL_AP_FS_DEV_NEEDCLK_Field is
     (
      --  Under hardware control.
      Hw_Ctrl,
      --  Forced high.
      Forced)
     with Size => 1;
   for USB0NEEDCLKCTRL_AP_FS_DEV_NEEDCLK_Field use
     (Hw_Ctrl => 0,
      Forced => 1);

   --  USB0 Device USB0_NEEDCLK polarity for triggering the USB0 wake-up
   --  interrupt:.
   type USB0NEEDCLKCTRL_POL_FS_DEV_NEEDCLK_Field is
     (
      --  Falling edge of device USB0_NEEDCLK triggers wake-up.
      Falling,
      --  Rising edge of device USB0_NEEDCLK triggers wake-up.
      Rising)
     with Size => 1;
   for USB0NEEDCLKCTRL_POL_FS_DEV_NEEDCLK_Field use
     (Falling => 0,
      Rising => 1);

   --  USB0 Host USB0_NEEDCLK signal control:.
   type USB0NEEDCLKCTRL_AP_FS_HOST_NEEDCLK_Field is
     (
      --  Under hardware control.
      Hw_Ctrl,
      --  Forced high.
      Forced)
     with Size => 1;
   for USB0NEEDCLKCTRL_AP_FS_HOST_NEEDCLK_Field use
     (Hw_Ctrl => 0,
      Forced => 1);

   --  USB0 Host USB0_NEEDCLK polarity for triggering the USB0 wake-up
   --  interrupt:.
   type USB0NEEDCLKCTRL_POL_FS_HOST_NEEDCLK_Field is
     (
      --  Falling edge of device USB0_NEEDCLK triggers wake-up.
      Falling,
      --  Rising edge of device USB0_NEEDCLK triggers wake-up.
      Rising)
     with Size => 1;
   for USB0NEEDCLKCTRL_POL_FS_HOST_NEEDCLK_Field use
     (Falling => 0,
      Rising => 1);

   --  USB0 need clock control
   type USB0NEEDCLKCTRL_Register is record
      --  USB0 Device USB0_NEEDCLK signal control:.
      AP_FS_DEV_NEEDCLK   : USB0NEEDCLKCTRL_AP_FS_DEV_NEEDCLK_Field :=
                             NXP_SVD.SYSCON.Hw_Ctrl;
      --  USB0 Device USB0_NEEDCLK polarity for triggering the USB0 wake-up
      --  interrupt:.
      POL_FS_DEV_NEEDCLK  : USB0NEEDCLKCTRL_POL_FS_DEV_NEEDCLK_Field :=
                             NXP_SVD.SYSCON.Falling;
      --  USB0 Host USB0_NEEDCLK signal control:.
      AP_FS_HOST_NEEDCLK  : USB0NEEDCLKCTRL_AP_FS_HOST_NEEDCLK_Field :=
                             NXP_SVD.SYSCON.Hw_Ctrl;
      --  USB0 Host USB0_NEEDCLK polarity for triggering the USB0 wake-up
      --  interrupt:.
      POL_FS_HOST_NEEDCLK : USB0NEEDCLKCTRL_POL_FS_HOST_NEEDCLK_Field :=
                             NXP_SVD.SYSCON.Falling;
      --  unspecified
      Reserved_4_31       : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for USB0NEEDCLKCTRL_Register use record
      AP_FS_DEV_NEEDCLK   at 0 range 0 .. 0;
      POL_FS_DEV_NEEDCLK  at 0 range 1 .. 1;
      AP_FS_HOST_NEEDCLK  at 0 range 2 .. 2;
      POL_FS_HOST_NEEDCLK at 0 range 3 .. 3;
      Reserved_4_31       at 0 range 4 .. 31;
   end record;

   --  USB0 Device USB0_NEEDCLK signal status:.
   type USB0NEEDCLKSTAT_DEV_NEEDCLK_Field is
     (
      --  USB0 Device clock is low.
      Low,
      --  USB0 Device clock is high.
      High)
     with Size => 1;
   for USB0NEEDCLKSTAT_DEV_NEEDCLK_Field use
     (Low => 0,
      High => 1);

   --  USB0 Host USB0_NEEDCLK signal status:.
   type USB0NEEDCLKSTAT_HOST_NEEDCLK_Field is
     (
      --  USB0 Host clock is low.
      Low,
      --  USB0 Host clock is high.
      High)
     with Size => 1;
   for USB0NEEDCLKSTAT_HOST_NEEDCLK_Field use
     (Low => 0,
      High => 1);

   --  USB0 need clock status
   type USB0NEEDCLKSTAT_Register is record
      --  Read-only. USB0 Device USB0_NEEDCLK signal status:.
      DEV_NEEDCLK   : USB0NEEDCLKSTAT_DEV_NEEDCLK_Field;
      --  Read-only. USB0 Host USB0_NEEDCLK signal status:.
      HOST_NEEDCLK  : USB0NEEDCLKSTAT_HOST_NEEDCLK_Field;
      --  unspecified
      Reserved_2_31 : HAL.UInt30;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for USB0NEEDCLKSTAT_Register use record
      DEV_NEEDCLK   at 0 range 0 .. 0;
      HOST_NEEDCLK  at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  Flush control
   type FMCFLUSH_FLUSH_Field is
     (
      --  No action is performed.
      No_Flush,
      --  Flush the FMC buffer contents.
      Flush)
     with Size => 1;
   for FMCFLUSH_FLUSH_Field use
     (No_Flush => 0,
      Flush => 1);

   --  FMCflush control
   type FMCFLUSH_Register is record
      --  Write-only. Flush control
      FLUSH         : FMCFLUSH_FLUSH_Field := NXP_SVD.SYSCON.No_Flush;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FMCFLUSH_Register use record
      FLUSH         at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  USB1 Device need_clock signal control:
   type USB1NEEDCLKCTRL_AP_HS_DEV_NEEDCLK_Field is
     (
      --  HOST_NEEDCLK is under hardware control.
      Hw_Ctrl,
      --  HOST_NEEDCLK is forced high.
      Forced)
     with Size => 1;
   for USB1NEEDCLKCTRL_AP_HS_DEV_NEEDCLK_Field use
     (Hw_Ctrl => 0,
      Forced => 1);

   --  USB1 device need clock polarity for triggering the USB1_NEEDCLK wake-up
   --  interrupt:
   type USB1NEEDCLKCTRL_POL_HS_DEV_NEEDCLK_Field is
     (
      --  Falling edge of DEV_NEEDCLK triggers wake-up.
      Falling,
      --  Rising edge of DEV_NEEDCLK triggers wake-up.
      Rising)
     with Size => 1;
   for USB1NEEDCLKCTRL_POL_HS_DEV_NEEDCLK_Field use
     (Falling => 0,
      Rising => 1);

   --  USB1 Host need clock signal control:
   type USB1NEEDCLKCTRL_AP_HS_HOST_NEEDCLK_Field is
     (
      --  HOST_NEEDCLK is under hardware control.
      Hw_Ctrl,
      --  HOST_NEEDCLK is forced high.
      Forced)
     with Size => 1;
   for USB1NEEDCLKCTRL_AP_HS_HOST_NEEDCLK_Field use
     (Hw_Ctrl => 0,
      Forced => 1);

   --  USB1 host need clock polarity for triggering the USB1_NEEDCLK wake-up
   --  interrupt.
   type USB1NEEDCLKCTRL_POL_HS_HOST_NEEDCLK_Field is
     (
      --  Falling edge of HOST_NEEDCLK triggers wake-up.
      Falling,
      --  Rising edge of HOST_NEEDCLK triggers wake-up.
      Rising)
     with Size => 1;
   for USB1NEEDCLKCTRL_POL_HS_HOST_NEEDCLK_Field use
     (Falling => 0,
      Rising => 1);

   --  Software override of device controller PHY wake up logic.
   type USB1NEEDCLKCTRL_HS_DEV_WAKEUP_N_Field is
     (
      --  Forces USB1_PHY to wake-up.
      Force_Wup,
      --  Normal USB1_PHY behavior.
      Normal_Wup)
     with Size => 1;
   for USB1NEEDCLKCTRL_HS_DEV_WAKEUP_N_Field use
     (Force_Wup => 0,
      Normal_Wup => 1);

   --  USB1 need clock control
   type USB1NEEDCLKCTRL_Register is record
      --  USB1 Device need_clock signal control:
      AP_HS_DEV_NEEDCLK   : USB1NEEDCLKCTRL_AP_HS_DEV_NEEDCLK_Field :=
                             NXP_SVD.SYSCON.Hw_Ctrl;
      --  USB1 device need clock polarity for triggering the USB1_NEEDCLK
      --  wake-up interrupt:
      POL_HS_DEV_NEEDCLK  : USB1NEEDCLKCTRL_POL_HS_DEV_NEEDCLK_Field :=
                             NXP_SVD.SYSCON.Falling;
      --  USB1 Host need clock signal control:
      AP_HS_HOST_NEEDCLK  : USB1NEEDCLKCTRL_AP_HS_HOST_NEEDCLK_Field :=
                             NXP_SVD.SYSCON.Hw_Ctrl;
      --  USB1 host need clock polarity for triggering the USB1_NEEDCLK wake-up
      --  interrupt.
      POL_HS_HOST_NEEDCLK : USB1NEEDCLKCTRL_POL_HS_HOST_NEEDCLK_Field :=
                             NXP_SVD.SYSCON.Falling;
      --  Software override of device controller PHY wake up logic.
      HS_DEV_WAKEUP_N     : USB1NEEDCLKCTRL_HS_DEV_WAKEUP_N_Field :=
                             NXP_SVD.SYSCON.Normal_Wup;
      --  unspecified
      Reserved_5_31       : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for USB1NEEDCLKCTRL_Register use record
      AP_HS_DEV_NEEDCLK   at 0 range 0 .. 0;
      POL_HS_DEV_NEEDCLK  at 0 range 1 .. 1;
      AP_HS_HOST_NEEDCLK  at 0 range 2 .. 2;
      POL_HS_HOST_NEEDCLK at 0 range 3 .. 3;
      HS_DEV_WAKEUP_N     at 0 range 4 .. 4;
      Reserved_5_31       at 0 range 5 .. 31;
   end record;

   --  USB1 Device need_clock signal status:.
   type USB1NEEDCLKSTAT_DEV_NEEDCLK_Field is
     (
      --  DEV_NEEDCLK is low.
      Low,
      --  DEV_NEEDCLK is high.
      High)
     with Size => 1;
   for USB1NEEDCLKSTAT_DEV_NEEDCLK_Field use
     (Low => 0,
      High => 1);

   --  USB1 Host need_clock signal status:.
   type USB1NEEDCLKSTAT_HOST_NEEDCLK_Field is
     (
      --  HOST_NEEDCLK is low.
      Low,
      --  HOST_NEEDCLK is high.
      High)
     with Size => 1;
   for USB1NEEDCLKSTAT_HOST_NEEDCLK_Field use
     (Low => 0,
      High => 1);

   --  USB1 need clock status
   type USB1NEEDCLKSTAT_Register is record
      --  Read-only. USB1 Device need_clock signal status:.
      DEV_NEEDCLK   : USB1NEEDCLKSTAT_DEV_NEEDCLK_Field;
      --  Read-only. USB1 Host need_clock signal status:.
      HOST_NEEDCLK  : USB1NEEDCLKSTAT_HOST_NEEDCLK_Field;
      --  unspecified
      Reserved_2_31 : HAL.UInt30;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for USB1NEEDCLKSTAT_Register use record
      DEV_NEEDCLK   at 0 range 0 .. 0;
      HOST_NEEDCLK  at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  Programmable delay value by which cclk_in_drv is phase-shifted with
   --  regard to cclk_in.
   type SDIOCLKCTRL_CCLK_DRV_PHASE_Field is
     (
      --  0 degree shift.
      Enum_0_Deg,
      --  90 degree shift.
      Enum_90_Deg,
      --  180 degree shift.
      Enum_180_Deg,
      --  270 degree shift.
      Enum_270_Deg)
     with Size => 2;
   for SDIOCLKCTRL_CCLK_DRV_PHASE_Field use
     (Enum_0_Deg => 0,
      Enum_90_Deg => 1,
      Enum_180_Deg => 2,
      Enum_270_Deg => 3);

   --  Programmable delay value by which cclk_in_sample is delayed with regard
   --  to cclk_in.
   type SDIOCLKCTRL_CCLK_SAMPLE_PHASE_Field is
     (
      --  0 degree shift.
      Enum_0_Deg,
      --  90 degree shift.
      Enum_90_Deg,
      --  180 degree shift.
      Enum_180_Deg,
      --  270 degree shift.
      Enum_270_Deg)
     with Size => 2;
   for SDIOCLKCTRL_CCLK_SAMPLE_PHASE_Field use
     (Enum_0_Deg => 0,
      Enum_90_Deg => 1,
      Enum_180_Deg => 2,
      Enum_270_Deg => 3);

   --  Enables the delays CCLK_DRV_PHASE and CCLK_SAMPLE_PHASE.
   type SDIOCLKCTRL_PHASE_ACTIVE_Field is
     (
      --  Bypassed.
      Bypassed,
      --  Activates phase shift logic. When active, the clock divider is active
      --  and phase delays are enabled.
      Ph_Shift)
     with Size => 1;
   for SDIOCLKCTRL_PHASE_ACTIVE_Field use
     (Bypassed => 0,
      Ph_Shift => 1);

   subtype SDIOCLKCTRL_CCLK_DRV_DELAY_Field is HAL.UInt5;

   --  Enables drive delay, as controlled by the CCLK_DRV_DELAY field.
   type SDIOCLKCTRL_CCLK_DRV_DELAY_ACTIVE_Field is
     (
      --  Disable drive delay.
      Disable,
      --  Enable drive delay.
      Enable)
     with Size => 1;
   for SDIOCLKCTRL_CCLK_DRV_DELAY_ACTIVE_Field use
     (Disable => 0,
      Enable => 1);

   subtype SDIOCLKCTRL_CCLK_SAMPLE_DELAY_Field is HAL.UInt5;

   --  Enables sample delay, as controlled by the CCLK_SAMPLE_DELAY field.
   type SDIOCLKCTRL_CCLK_SAMPLE_DELAY_ACTIVE_Field is
     (
      --  Disables sample delay.
      Disable,
      --  Enables sample delay.
      Enable)
     with Size => 1;
   for SDIOCLKCTRL_CCLK_SAMPLE_DELAY_ACTIVE_Field use
     (Disable => 0,
      Enable => 1);

   --  SDIO CCLKIN phase and delay control
   type SDIOCLKCTRL_Register is record
      --  Programmable delay value by which cclk_in_drv is phase-shifted with
      --  regard to cclk_in.
      CCLK_DRV_PHASE           : SDIOCLKCTRL_CCLK_DRV_PHASE_Field :=
                                  NXP_SVD.SYSCON.Enum_0_Deg;
      --  Programmable delay value by which cclk_in_sample is delayed with
      --  regard to cclk_in.
      CCLK_SAMPLE_PHASE        : SDIOCLKCTRL_CCLK_SAMPLE_PHASE_Field :=
                                  NXP_SVD.SYSCON.Enum_0_Deg;
      --  unspecified
      Reserved_4_6             : HAL.UInt3 := 16#0#;
      --  Enables the delays CCLK_DRV_PHASE and CCLK_SAMPLE_PHASE.
      PHASE_ACTIVE             : SDIOCLKCTRL_PHASE_ACTIVE_Field :=
                                  NXP_SVD.SYSCON.Bypassed;
      --  unspecified
      Reserved_8_15            : HAL.UInt8 := 16#0#;
      --  Programmable delay value by which cclk_in_drv is delayed with regard
      --  to cclk_in.
      CCLK_DRV_DELAY           : SDIOCLKCTRL_CCLK_DRV_DELAY_Field := 16#0#;
      --  unspecified
      Reserved_21_22           : HAL.UInt2 := 16#0#;
      --  Enables drive delay, as controlled by the CCLK_DRV_DELAY field.
      CCLK_DRV_DELAY_ACTIVE    : SDIOCLKCTRL_CCLK_DRV_DELAY_ACTIVE_Field :=
                                  NXP_SVD.SYSCON.Disable;
      --  Programmable delay value by which cclk_in_sample is delayed with
      --  regard to cclk_in.
      CCLK_SAMPLE_DELAY        : SDIOCLKCTRL_CCLK_SAMPLE_DELAY_Field := 16#0#;
      --  unspecified
      Reserved_29_30           : HAL.UInt2 := 16#0#;
      --  Enables sample delay, as controlled by the CCLK_SAMPLE_DELAY field.
      CCLK_SAMPLE_DELAY_ACTIVE : SDIOCLKCTRL_CCLK_SAMPLE_DELAY_ACTIVE_Field :=
                                  NXP_SVD.SYSCON.Disable;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SDIOCLKCTRL_Register use record
      CCLK_DRV_PHASE           at 0 range 0 .. 1;
      CCLK_SAMPLE_PHASE        at 0 range 2 .. 3;
      Reserved_4_6             at 0 range 4 .. 6;
      PHASE_ACTIVE             at 0 range 7 .. 7;
      Reserved_8_15            at 0 range 8 .. 15;
      CCLK_DRV_DELAY           at 0 range 16 .. 20;
      Reserved_21_22           at 0 range 21 .. 22;
      CCLK_DRV_DELAY_ACTIVE    at 0 range 23 .. 23;
      CCLK_SAMPLE_DELAY        at 0 range 24 .. 28;
      Reserved_29_30           at 0 range 29 .. 30;
      CCLK_SAMPLE_DELAY_ACTIVE at 0 range 31 .. 31;
   end record;

   subtype PLL1CTRL_SELR_Field is HAL.UInt4;
   subtype PLL1CTRL_SELI_Field is HAL.UInt6;
   subtype PLL1CTRL_SELP_Field is HAL.UInt5;

   --  Bypass PLL input clock is sent directly to the PLL output (default).
   type PLL1CTRL_BYPASSPLL_Field is
     (
      --  use PLL.
      Used,
      --  PLL input clock is sent directly to the PLL output.
      Bypassed)
     with Size => 1;
   for PLL1CTRL_BYPASSPLL_Field use
     (Used => 0,
      Bypassed => 1);

   --  bypass of the divide-by-2 divider in the post-divider.
   type PLL1CTRL_BYPASSPOSTDIV2_Field is
     (
      --  use the divide-by-2 divider in the post-divider.
      Used,
      --  bypass of the divide-by-2 divider in the post-divider.
      Bypassed)
     with Size => 1;
   for PLL1CTRL_BYPASSPOSTDIV2_Field use
     (Used => 0,
      Bypassed => 1);

   --  control of the bandwidth of the PLL.
   type PLL1CTRL_BWDIRECT_Field is
     (
      --  the bandwidth is changed synchronously with the feedback-divider.
      Sync,
      --  modify the bandwidth of the PLL directly.
      Direct)
     with Size => 1;
   for PLL1CTRL_BWDIRECT_Field use
     (Sync => 0,
      Direct => 1);

   --  bypass of the pre-divider.
   type PLL1CTRL_BYPASSPREDIV_Field is
     (
      --  use the pre-divider.
      Used,
      --  bypass of the pre-divider.
      Bypassed)
     with Size => 1;
   for PLL1CTRL_BYPASSPREDIV_Field use
     (Used => 0,
      Bypassed => 1);

   --  bypass of the post-divider.
   type PLL1CTRL_BYPASSPOSTDIV_Field is
     (
      --  use the post-divider.
      Used,
      --  bypass of the post-divider.
      Bypassed)
     with Size => 1;
   for PLL1CTRL_BYPASSPOSTDIV_Field use
     (Used => 0,
      Bypassed => 1);

   --  enable the output clock.
   type PLL1CTRL_CLKEN_Field is
     (
      --  Disable the output clock.
      Disable,
      --  Enable the output clock.
      Enable)
     with Size => 1;
   for PLL1CTRL_CLKEN_Field use
     (Disable => 0,
      Enable => 1);

   --  Skew mode.
   type PLL1CTRL_SKEWEN_Field is
     (
      --  skewmode is disable.
      Disable,
      --  skewmode is enable.
      Enable)
     with Size => 1;
   for PLL1CTRL_SKEWEN_Field use
     (Disable => 0,
      Enable => 1);

   --  PLL1 550m control
   type PLL1CTRL_Register is record
      --  Bandwidth select R value.
      SELR           : PLL1CTRL_SELR_Field := 16#0#;
      --  Bandwidth select I value.
      SELI           : PLL1CTRL_SELI_Field := 16#0#;
      --  Bandwidth select P value.
      SELP           : PLL1CTRL_SELP_Field := 16#0#;
      --  Bypass PLL input clock is sent directly to the PLL output (default).
      BYPASSPLL      : PLL1CTRL_BYPASSPLL_Field := NXP_SVD.SYSCON.Used;
      --  bypass of the divide-by-2 divider in the post-divider.
      BYPASSPOSTDIV2 : PLL1CTRL_BYPASSPOSTDIV2_Field := NXP_SVD.SYSCON.Used;
      --  limup_off = 1 in spread spectrum and fractional PLL applications.
      LIMUPOFF       : Boolean := False;
      --  control of the bandwidth of the PLL.
      BWDIRECT       : PLL1CTRL_BWDIRECT_Field := NXP_SVD.SYSCON.Sync;
      --  bypass of the pre-divider.
      BYPASSPREDIV   : PLL1CTRL_BYPASSPREDIV_Field := NXP_SVD.SYSCON.Used;
      --  bypass of the post-divider.
      BYPASSPOSTDIV  : PLL1CTRL_BYPASSPOSTDIV_Field := NXP_SVD.SYSCON.Used;
      --  enable the output clock.
      CLKEN          : PLL1CTRL_CLKEN_Field := NXP_SVD.SYSCON.Disable;
      --  1: free running mode.
      FRMEN          : Boolean := False;
      --  free running mode clockstable: Warning: Only make frm_clockstable = 1
      --  after the PLL output frequency is stable.
      FRMCLKSTABLE   : Boolean := False;
      --  Skew mode.
      SKEWEN         : PLL1CTRL_SKEWEN_Field := NXP_SVD.SYSCON.Disable;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PLL1CTRL_Register use record
      SELR           at 0 range 0 .. 3;
      SELI           at 0 range 4 .. 9;
      SELP           at 0 range 10 .. 14;
      BYPASSPLL      at 0 range 15 .. 15;
      BYPASSPOSTDIV2 at 0 range 16 .. 16;
      LIMUPOFF       at 0 range 17 .. 17;
      BWDIRECT       at 0 range 18 .. 18;
      BYPASSPREDIV   at 0 range 19 .. 19;
      BYPASSPOSTDIV  at 0 range 20 .. 20;
      CLKEN          at 0 range 21 .. 21;
      FRMEN          at 0 range 22 .. 22;
      FRMCLKSTABLE   at 0 range 23 .. 23;
      SKEWEN         at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  PLL1 550m status
   type PLL1STAT_Register is record
      --  Read-only. lock detector output (active high) Warning: The lock
      --  signal is only reliable between fref[2] :100 kHz to 20 MHz.
      LOCK          : Boolean;
      --  Read-only. pre-divider ratio change acknowledge.
      PREDIVACK     : Boolean;
      --  Read-only. feedback divider ratio change acknowledge.
      FEEDDIVACK    : Boolean;
      --  Read-only. post-divider ratio change acknowledge.
      POSTDIVACK    : Boolean;
      --  Read-only. free running detector output (active high).
      FRMDET        : Boolean;
      --  unspecified
      Reserved_5_31 : HAL.UInt27;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PLL1STAT_Register use record
      LOCK          at 0 range 0 .. 0;
      PREDIVACK     at 0 range 1 .. 1;
      FEEDDIVACK    at 0 range 2 .. 2;
      POSTDIVACK    at 0 range 3 .. 3;
      FRMDET        at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype PLL1NDEC_NDIV_Field is HAL.UInt8;

   --  PLL1 550m N divider
   type PLL1NDEC_Register is record
      --  pre-divider divider ratio (N-divider).
      NDIV          : PLL1NDEC_NDIV_Field := 16#0#;
      --  pre-divider ratio change request.
      NREQ          : Boolean := False;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PLL1NDEC_Register use record
      NDIV          at 0 range 0 .. 7;
      NREQ          at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   subtype PLL1MDEC_MDIV_Field is HAL.UInt16;

   --  PLL1 550m M divider
   type PLL1MDEC_Register is record
      --  feedback divider divider ratio (M-divider).
      MDIV           : PLL1MDEC_MDIV_Field := 16#0#;
      --  feedback ratio change request.
      MREQ           : Boolean := False;
      --  unspecified
      Reserved_17_31 : HAL.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PLL1MDEC_Register use record
      MDIV           at 0 range 0 .. 15;
      MREQ           at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype PLL1PDEC_PDIV_Field is HAL.UInt5;

   --  PLL1 550m P divider
   type PLL1PDEC_Register is record
      --  post-divider divider ratio (P-divider)
      PDIV          : PLL1PDEC_PDIV_Field := 16#0#;
      --  feedback ratio change request.
      PREQ          : Boolean := False;
      --  unspecified
      Reserved_6_31 : HAL.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PLL1PDEC_Register use record
      PDIV          at 0 range 0 .. 4;
      PREQ          at 0 range 5 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   subtype PLL0CTRL_SELR_Field is HAL.UInt4;
   subtype PLL0CTRL_SELI_Field is HAL.UInt6;
   subtype PLL0CTRL_SELP_Field is HAL.UInt5;

   --  Bypass PLL input clock is sent directly to the PLL output (default).
   type PLL0CTRL_BYPASSPLL_Field is
     (
      --  use PLL.
      Used,
      --  Bypass PLL input clock is sent directly to the PLL output.
      Bypassed)
     with Size => 1;
   for PLL0CTRL_BYPASSPLL_Field use
     (Used => 0,
      Bypassed => 1);

   --  bypass of the divide-by-2 divider in the post-divider.
   type PLL0CTRL_BYPASSPOSTDIV2_Field is
     (
      --  use the divide-by-2 divider in the post-divider.
      Used,
      --  bypass of the divide-by-2 divider in the post-divider.
      Bypassed)
     with Size => 1;
   for PLL0CTRL_BYPASSPOSTDIV2_Field use
     (Used => 0,
      Bypassed => 1);

   --  Control of the bandwidth of the PLL.
   type PLL0CTRL_BWDIRECT_Field is
     (
      --  the bandwidth is changed synchronously with the feedback-divider.
      Sync,
      --  modify the bandwidth of the PLL directly.
      Direct)
     with Size => 1;
   for PLL0CTRL_BWDIRECT_Field use
     (Sync => 0,
      Direct => 1);

   --  bypass of the pre-divider.
   type PLL0CTRL_BYPASSPREDIV_Field is
     (
      --  use the pre-divider.
      Used,
      --  bypass of the pre-divider.
      Bypassed)
     with Size => 1;
   for PLL0CTRL_BYPASSPREDIV_Field use
     (Used => 0,
      Bypassed => 1);

   --  bypass of the post-divider.
   type PLL0CTRL_BYPASSPOSTDIV_Field is
     (
      --  use the post-divider.
      Used,
      --  bypass of the post-divider.
      Bypassed)
     with Size => 1;
   for PLL0CTRL_BYPASSPOSTDIV_Field use
     (Used => 0,
      Bypassed => 1);

   --  enable the output clock.
   type PLL0CTRL_CLKEN_Field is
     (
      --  disable the output clock.
      Disable,
      --  enable the output clock.
      Enable)
     with Size => 1;
   for PLL0CTRL_CLKEN_Field use
     (Disable => 0,
      Enable => 1);

   --  free running mode.
   type PLL0CTRL_FRMEN_Field is
     (
      --  free running mode is disable.
      Disable,
      --  free running mode is enable.
      Enable)
     with Size => 1;
   for PLL0CTRL_FRMEN_Field use
     (Disable => 0,
      Enable => 1);

   --  skew mode.
   type PLL0CTRL_SKEWEN_Field is
     (
      --  skew mode is disable.
      Disable,
      --  skew mode is enable.
      Enable)
     with Size => 1;
   for PLL0CTRL_SKEWEN_Field use
     (Disable => 0,
      Enable => 1);

   --  PLL0 550m control
   type PLL0CTRL_Register is record
      --  Bandwidth select R value.
      SELR           : PLL0CTRL_SELR_Field := 16#0#;
      --  Bandwidth select I value.
      SELI           : PLL0CTRL_SELI_Field := 16#0#;
      --  Bandwidth select P value.
      SELP           : PLL0CTRL_SELP_Field := 16#0#;
      --  Bypass PLL input clock is sent directly to the PLL output (default).
      BYPASSPLL      : PLL0CTRL_BYPASSPLL_Field := NXP_SVD.SYSCON.Used;
      --  bypass of the divide-by-2 divider in the post-divider.
      BYPASSPOSTDIV2 : PLL0CTRL_BYPASSPOSTDIV2_Field := NXP_SVD.SYSCON.Used;
      --  limup_off = 1 in spread spectrum and fractional PLL applications.
      LIMUPOFF       : Boolean := False;
      --  Control of the bandwidth of the PLL.
      BWDIRECT       : PLL0CTRL_BWDIRECT_Field := NXP_SVD.SYSCON.Sync;
      --  bypass of the pre-divider.
      BYPASSPREDIV   : PLL0CTRL_BYPASSPREDIV_Field := NXP_SVD.SYSCON.Used;
      --  bypass of the post-divider.
      BYPASSPOSTDIV  : PLL0CTRL_BYPASSPOSTDIV_Field := NXP_SVD.SYSCON.Used;
      --  enable the output clock.
      CLKEN          : PLL0CTRL_CLKEN_Field := NXP_SVD.SYSCON.Disable;
      --  free running mode.
      FRMEN          : PLL0CTRL_FRMEN_Field := NXP_SVD.SYSCON.Disable;
      --  free running mode clockstable: Warning: Only make frm_clockstable =1
      --  after the PLL output frequency is stable.
      FRMCLKSTABLE   : Boolean := False;
      --  skew mode.
      SKEWEN         : PLL0CTRL_SKEWEN_Field := NXP_SVD.SYSCON.Disable;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PLL0CTRL_Register use record
      SELR           at 0 range 0 .. 3;
      SELI           at 0 range 4 .. 9;
      SELP           at 0 range 10 .. 14;
      BYPASSPLL      at 0 range 15 .. 15;
      BYPASSPOSTDIV2 at 0 range 16 .. 16;
      LIMUPOFF       at 0 range 17 .. 17;
      BWDIRECT       at 0 range 18 .. 18;
      BYPASSPREDIV   at 0 range 19 .. 19;
      BYPASSPOSTDIV  at 0 range 20 .. 20;
      CLKEN          at 0 range 21 .. 21;
      FRMEN          at 0 range 22 .. 22;
      FRMCLKSTABLE   at 0 range 23 .. 23;
      SKEWEN         at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  PLL0 550m status
   type PLL0STAT_Register is record
      --  Read-only. lock detector output (active high) Warning: The lock
      --  signal is only reliable between fref[2] :100 kHz to 20 MHz.
      LOCK          : Boolean;
      --  Read-only. pre-divider ratio change acknowledge.
      PREDIVACK     : Boolean;
      --  Read-only. feedback divider ratio change acknowledge.
      FEEDDIVACK    : Boolean;
      --  Read-only. post-divider ratio change acknowledge.
      POSTDIVACK    : Boolean;
      --  Read-only. free running detector output (active high).
      FRMDET        : Boolean;
      --  unspecified
      Reserved_5_31 : HAL.UInt27;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PLL0STAT_Register use record
      LOCK          at 0 range 0 .. 0;
      PREDIVACK     at 0 range 1 .. 1;
      FEEDDIVACK    at 0 range 2 .. 2;
      POSTDIVACK    at 0 range 3 .. 3;
      FRMDET        at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype PLL0NDEC_NDIV_Field is HAL.UInt8;

   --  PLL0 550m N divider
   type PLL0NDEC_Register is record
      --  pre-divider divider ratio (N-divider).
      NDIV          : PLL0NDEC_NDIV_Field := 16#0#;
      --  pre-divider ratio change request.
      NREQ          : Boolean := False;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PLL0NDEC_Register use record
      NDIV          at 0 range 0 .. 7;
      NREQ          at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   subtype PLL0PDEC_PDIV_Field is HAL.UInt5;

   --  PLL0 550m P divider
   type PLL0PDEC_Register is record
      --  post-divider divider ratio (P-divider)
      PDIV          : PLL0PDEC_PDIV_Field := 16#0#;
      --  feedback ratio change request.
      PREQ          : Boolean := False;
      --  unspecified
      Reserved_6_31 : HAL.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PLL0PDEC_Register use record
      PDIV          at 0 range 0 .. 4;
      PREQ          at 0 range 5 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   subtype PLL0SSCG1_MF_Field is HAL.UInt3;
   subtype PLL0SSCG1_MR_Field is HAL.UInt3;
   subtype PLL0SSCG1_MC_Field is HAL.UInt2;
   subtype PLL0SSCG1_MDIV_EXT_Field is HAL.UInt16;

   --  PLL0 Spread Spectrum Wrapper control register 1
   type PLL0SSCG1_Register is record
      --  input word of the wrapper bit 32.
      MD_MBS         : Boolean := False;
      --  md change request.
      MD_REQ         : Boolean := False;
      --  programmable modulation frequency fm = Fref/Nss mf[2:0] = 000 =>
      --  Nss=512 (fm ~ 3.
      MF             : PLL0SSCG1_MF_Field := 16#0#;
      --  programmable frequency modulation depth Dfmodpk-pk = Fref*kss/Fcco =
      --  kss/(2*md[32:25]dec) mr[2:0] = 000 => kss = 0 (no spread spectrum)
      --  mr[2:0] = 001 => kss ~ 1 mr[2:0] = 010 => kss ~ 1.
      MR             : PLL0SSCG1_MR_Field := 16#0#;
      --  modulation waveform control Compensation for low pass filtering of
      --  the PLL to get a triangular modulation at the output of the PLL,
      --  giving a flat frequency spectrum.
      MC             : PLL0SSCG1_MC_Field := 16#0#;
      --  to select an external mdiv value.
      MDIV_EXT       : PLL0SSCG1_MDIV_EXT_Field := 16#0#;
      --  to select an external mreq value.
      MREQ           : Boolean := False;
      --  dithering between two modulation frequencies in a random way or in a
      --  pseudo random way (white noise), in order to decrease the probability
      --  that the modulated waveform will occur with the same phase on a
      --  particular point on the screen.
      DITHER         : Boolean := False;
      --  to select mdiv_ext and mreq_ext sel_ext = 0: mdiv ~ md[32:0], mreq =
      --  1 sel_ext = 1 : mdiv = mdiv_ext, mreq = mreq_ext.
      SEL_EXT        : Boolean := False;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PLL0SSCG1_Register use record
      MD_MBS         at 0 range 0 .. 0;
      MD_REQ         at 0 range 1 .. 1;
      MF             at 0 range 2 .. 4;
      MR             at 0 range 5 .. 7;
      MC             at 0 range 8 .. 9;
      MDIV_EXT       at 0 range 10 .. 25;
      MREQ           at 0 range 26 .. 26;
      DITHER         at 0 range 27 .. 27;
      SEL_EXT        at 0 range 28 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   --  CPU1 clock enable.
   type CPUCTRL_CPU1CLKEN_Field is
     (
      --  The CPU1 clock is not enabled.
      Disable,
      --  The CPU1 clock is enabled.
      Enable)
     with Size => 1;
   for CPUCTRL_CPU1CLKEN_Field use
     (Disable => 0,
      Enable => 1);

   --  CPU1 reset.
   type CPUCTRL_CPU1RSTEN_Field is
     (
      --  The CPU1 is not being reset.
      Released,
      --  The CPU1 is being reset.
      Asserted)
     with Size => 1;
   for CPUCTRL_CPU1RSTEN_Field use
     (Released => 0,
      Asserted => 1);

   --  CPU Control for multiple processors
   type CPUCTRL_Register is record
      --  unspecified
      Reserved_0_2  : HAL.UInt3 := 16#4#;
      --  CPU1 clock enable.
      CPU1CLKEN     : CPUCTRL_CPU1CLKEN_Field := NXP_SVD.SYSCON.Enable;
      --  unspecified
      Reserved_4_4  : HAL.Bit := 16#0#;
      --  CPU1 reset.
      CPU1RSTEN     : CPUCTRL_CPU1RSTEN_Field := NXP_SVD.SYSCON.Asserted;
      --  unspecified
      Reserved_6_31 : HAL.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPUCTRL_Register use record
      Reserved_0_2  at 0 range 0 .. 2;
      CPU1CLKEN     at 0 range 3 .. 3;
      Reserved_4_4  at 0 range 4 .. 4;
      CPU1RSTEN     at 0 range 5 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   --  The CPU0 sleeping state.
   type CPSTAT_CPU0SLEEPING_Field is
     (
      --  the CPU is not sleeping.
      Awake,
      --  the CPU is sleeping.
      Sleeping)
     with Size => 1;
   for CPSTAT_CPU0SLEEPING_Field use
     (Awake => 0,
      Sleeping => 1);

   --  The CPU1 sleeping state.
   type CPSTAT_CPU1SLEEPING_Field is
     (
      --  the CPU is not sleeping.
      Awake,
      --  the CPU is sleeping.
      Sleeping)
     with Size => 1;
   for CPSTAT_CPU1SLEEPING_Field use
     (Awake => 0,
      Sleeping => 1);

   --  The CPU0 lockup state.
   type CPSTAT_CPU0LOCKUP_Field is
     (
      --  the CPU is not in lockup.
      Awake,
      --  the CPU is in lockup.
      Sleeping)
     with Size => 1;
   for CPSTAT_CPU0LOCKUP_Field use
     (Awake => 0,
      Sleeping => 1);

   --  The CPU1 lockup state.
   type CPSTAT_CPU1LOCKUP_Field is
     (
      --  the CPU is not in lockup.
      Awake,
      --  the CPU is in lockup.
      Sleeping)
     with Size => 1;
   for CPSTAT_CPU1LOCKUP_Field use
     (Awake => 0,
      Sleeping => 1);

   --  CPU Status
   type CPSTAT_Register is record
      --  Read-only. The CPU0 sleeping state.
      CPU0SLEEPING  : CPSTAT_CPU0SLEEPING_Field;
      --  Read-only. The CPU1 sleeping state.
      CPU1SLEEPING  : CPSTAT_CPU1SLEEPING_Field;
      --  Read-only. The CPU0 lockup state.
      CPU0LOCKUP    : CPSTAT_CPU0LOCKUP_Field;
      --  Read-only. The CPU1 lockup state.
      CPU1LOCKUP    : CPSTAT_CPU1LOCKUP_Field;
      --  unspecified
      Reserved_4_31 : HAL.UInt28;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPSTAT_Register use record
      CPU0SLEEPING  at 0 range 0 .. 0;
      CPU1SLEEPING  at 0 range 1 .. 1;
      CPU0LOCKUP    at 0 range 2 .. 2;
      CPU1LOCKUP    at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  Enable XTAL32MHz clock for Frequency Measure module.
   type CLOCK_CTRL_XTAL32MHZ_FREQM_ENA_Field is
     (
      --  The clock is not enabled.
      Disable,
      --  The clock is enabled.
      Enable)
     with Size => 1;
   for CLOCK_CTRL_XTAL32MHZ_FREQM_ENA_Field use
     (Disable => 0,
      Enable => 1);

   --  Enable FRO 1MHz clock for Frequency Measure module and for UTICK.
   type CLOCK_CTRL_FRO1MHZ_UTICK_ENA_Field is
     (
      --  The clock is not enabled.
      Disable,
      --  The clock is enabled.
      Enable)
     with Size => 1;
   for CLOCK_CTRL_FRO1MHZ_UTICK_ENA_Field use
     (Disable => 0,
      Enable => 1);

   --  Enable FRO 12MHz clock for Frequency Measure module.
   type CLOCK_CTRL_FRO12MHZ_FREQM_ENA_Field is
     (
      --  The clock is not enabled.
      Disable,
      --  The clock is enabled.
      Enable)
     with Size => 1;
   for CLOCK_CTRL_FRO12MHZ_FREQM_ENA_Field use
     (Disable => 0,
      Enable => 1);

   --  Enable FRO 96MHz clock for Frequency Measure module.
   type CLOCK_CTRL_FRO_HF_FREQM_ENA_Field is
     (
      --  The clock is not enabled.
      Disable,
      --  The clock is enabled.
      Enable)
     with Size => 1;
   for CLOCK_CTRL_FRO_HF_FREQM_ENA_Field use
     (Disable => 0,
      Enable => 1);

   --  Enable clock_in clock for clock module.
   type CLOCK_CTRL_CLKIN_ENA_Field is
     (
      --  The clock is not enabled.
      Disable,
      --  The clock is enabled.
      Enable)
     with Size => 1;
   for CLOCK_CTRL_CLKIN_ENA_Field use
     (Disable => 0,
      Enable => 1);

   --  Enable FRO 1MHz clock for clock muxing in clock gen.
   type CLOCK_CTRL_FRO1MHZ_CLK_ENA_Field is
     (
      --  The clock is not enabled.
      Disable,
      --  The clock is enabled.
      Enable)
     with Size => 1;
   for CLOCK_CTRL_FRO1MHZ_CLK_ENA_Field use
     (Disable => 0,
      Enable => 1);

   --  Enable FRO 12MHz clock for analog control of the FRO 192MHz.
   type CLOCK_CTRL_ANA_FRO12M_CLK_ENA_Field is
     (
      --  The clock is not enabled.
      Disable,
      --  The clock is enabled.
      Enable)
     with Size => 1;
   for CLOCK_CTRL_ANA_FRO12M_CLK_ENA_Field use
     (Disable => 0,
      Enable => 1);

   --  Enable clock for cristal oscilator calibration.
   type CLOCK_CTRL_XO_CAL_CLK_ENA_Field is
     (
      --  The clock is not enabled.
      Disable,
      --  The clock is enabled.
      Enable)
     with Size => 1;
   for CLOCK_CTRL_XO_CAL_CLK_ENA_Field use
     (Disable => 0,
      Enable => 1);

   --  Enable clocks FRO_1MHz and FRO_12MHz for PLU deglitching.
   type CLOCK_CTRL_PLU_DEGLITCH_CLK_ENA_Field is
     (
      --  The clock is not enabled.
      Disable,
      --  The clock is enabled.
      Enable)
     with Size => 1;
   for CLOCK_CTRL_PLU_DEGLITCH_CLK_ENA_Field use
     (Disable => 0,
      Enable => 1);

   --  Various system clock controls : Flash clock (48 MHz) control, clocks to
   --  Frequency Measures
   type CLOCK_CTRL_Register is record
      --  unspecified
      Reserved_0_0         : HAL.Bit := 16#1#;
      --  Enable XTAL32MHz clock for Frequency Measure module.
      XTAL32MHZ_FREQM_ENA  : CLOCK_CTRL_XTAL32MHZ_FREQM_ENA_Field :=
                              NXP_SVD.SYSCON.Disable;
      --  Enable FRO 1MHz clock for Frequency Measure module and for UTICK.
      FRO1MHZ_UTICK_ENA    : CLOCK_CTRL_FRO1MHZ_UTICK_ENA_Field :=
                              NXP_SVD.SYSCON.Disable;
      --  Enable FRO 12MHz clock for Frequency Measure module.
      FRO12MHZ_FREQM_ENA   : CLOCK_CTRL_FRO12MHZ_FREQM_ENA_Field :=
                              NXP_SVD.SYSCON.Disable;
      --  Enable FRO 96MHz clock for Frequency Measure module.
      FRO_HF_FREQM_ENA     : CLOCK_CTRL_FRO_HF_FREQM_ENA_Field :=
                              NXP_SVD.SYSCON.Disable;
      --  Enable clock_in clock for clock module.
      CLKIN_ENA            : CLOCK_CTRL_CLKIN_ENA_Field :=
                              NXP_SVD.SYSCON.Disable;
      --  Enable FRO 1MHz clock for clock muxing in clock gen.
      FRO1MHZ_CLK_ENA      : CLOCK_CTRL_FRO1MHZ_CLK_ENA_Field :=
                              NXP_SVD.SYSCON.Disable;
      --  Enable FRO 12MHz clock for analog control of the FRO 192MHz.
      ANA_FRO12M_CLK_ENA   : CLOCK_CTRL_ANA_FRO12M_CLK_ENA_Field :=
                              NXP_SVD.SYSCON.Disable;
      --  Enable clock for cristal oscilator calibration.
      XO_CAL_CLK_ENA       : CLOCK_CTRL_XO_CAL_CLK_ENA_Field :=
                              NXP_SVD.SYSCON.Disable;
      --  Enable clocks FRO_1MHz and FRO_12MHz for PLU deglitching.
      PLU_DEGLITCH_CLK_ENA : CLOCK_CTRL_PLU_DEGLITCH_CLK_ENA_Field :=
                              NXP_SVD.SYSCON.Disable;
      --  unspecified
      Reserved_10_31       : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CLOCK_CTRL_Register use record
      Reserved_0_0         at 0 range 0 .. 0;
      XTAL32MHZ_FREQM_ENA  at 0 range 1 .. 1;
      FRO1MHZ_UTICK_ENA    at 0 range 2 .. 2;
      FRO12MHZ_FREQM_ENA   at 0 range 3 .. 3;
      FRO_HF_FREQM_ENA     at 0 range 4 .. 4;
      CLKIN_ENA            at 0 range 5 .. 5;
      FRO1MHZ_CLK_ENA      at 0 range 6 .. 6;
      ANA_FRO12M_CLK_ENA   at 0 range 7 .. 7;
      XO_CAL_CLK_ENA       at 0 range 8 .. 8;
      PLU_DEGLITCH_CLK_ENA at 0 range 9 .. 9;
      Reserved_10_31       at 0 range 10 .. 31;
   end record;

   --  Analog Comparator interrupt enable control:.
   type COMP_INT_CTRL_INT_ENABLE_Field is
     (
      --  interrupt disable.
      Int_Disable,
      --  interrupt enable.
      Int_Enable)
     with Size => 1;
   for COMP_INT_CTRL_INT_ENABLE_Field use
     (Int_Disable => 0,
      Int_Enable => 1);

   --  Analog Comparator interrupt clear.
   type COMP_INT_CTRL_INT_CLEAR_Field is
     (
      --  No effect.
      None,
      --  Clear the interrupt. Self-cleared bit.
      Clear)
     with Size => 1;
   for COMP_INT_CTRL_INT_CLEAR_Field use
     (None => 0,
      Clear => 1);

   --  Comparator interrupt type selector:.
   type COMP_INT_CTRL_INT_CTRL_Field is
     (
      --  The analog comparator interrupt edge sensitive is disabled.
      Edge_Disable,
      --  The analog comparator interrupt level sensitive is disabled.
      Lvl_Disable,
      --  analog comparator interrupt is rising edge sensitive.
      Edge_Rising,
      --  Analog Comparator interrupt is high level sensitive.
      Lvl_High,
      --  analog comparator interrupt is falling edge sensitive.
      Edge_Falling,
      --  Analog Comparator interrupt is low level sensitive.
      Lvl_Low,
      --  analog comparator interrupt is rising and falling edge sensitive.
      Edge_Both,
      --  The analog comparator interrupt level sensitive is disabled.
      Lvl_Dis2)
     with Size => 3;
   for COMP_INT_CTRL_INT_CTRL_Field use
     (Edge_Disable => 0,
      Lvl_Disable => 1,
      Edge_Rising => 2,
      Lvl_High => 3,
      Edge_Falling => 4,
      Lvl_Low => 5,
      Edge_Both => 6,
      Lvl_Dis2 => 7);

   --  Select which Analog comparator output (filtered our un-filtered) is used
   --  for interrupt detection.
   type COMP_INT_CTRL_INT_SOURCE_Field is
     (
      --  Select Analog Comparator filtered output as input for interrupt
      --  detection.
      Filter_Int,
      --  Select Analog Comparator raw output (unfiltered) as input for
      --  interrupt detection. Must be used when Analog comparator is used as
      --  wake up source in Power down mode.
      Raw_Int)
     with Size => 1;
   for COMP_INT_CTRL_INT_SOURCE_Field use
     (Filter_Int => 0,
      Raw_Int => 1);

   --  Comparator Interrupt control
   type COMP_INT_CTRL_Register is record
      --  Analog Comparator interrupt enable control:.
      INT_ENABLE    : COMP_INT_CTRL_INT_ENABLE_Field :=
                       NXP_SVD.SYSCON.Int_Disable;
      --  Analog Comparator interrupt clear.
      INT_CLEAR     : COMP_INT_CTRL_INT_CLEAR_Field := NXP_SVD.SYSCON.None;
      --  Comparator interrupt type selector:.
      INT_CTRL      : COMP_INT_CTRL_INT_CTRL_Field :=
                       NXP_SVD.SYSCON.Edge_Disable;
      --  Select which Analog comparator output (filtered our un-filtered) is
      --  used for interrupt detection.
      INT_SOURCE    : COMP_INT_CTRL_INT_SOURCE_Field :=
                       NXP_SVD.SYSCON.Filter_Int;
      --  unspecified
      Reserved_6_31 : HAL.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for COMP_INT_CTRL_Register use record
      INT_ENABLE    at 0 range 0 .. 0;
      INT_CLEAR     at 0 range 1 .. 1;
      INT_CTRL      at 0 range 2 .. 4;
      INT_SOURCE    at 0 range 5 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   --  Interrupt status BEFORE Interrupt Enable.
   type COMP_INT_STATUS_STATUS_Field is
     (
      --  no interrupt pending.
      No_Int,
      --  interrupt pending.
      Pending)
     with Size => 1;
   for COMP_INT_STATUS_STATUS_Field use
     (No_Int => 0,
      Pending => 1);

   --  Interrupt status AFTER Interrupt Enable.
   type COMP_INT_STATUS_INT_STATUS_Field is
     (
      --  no interrupt pending.
      No_Int,
      --  interrupt pending.
      Pending)
     with Size => 1;
   for COMP_INT_STATUS_INT_STATUS_Field use
     (No_Int => 0,
      Pending => 1);

   --  comparator analog output.
   type COMP_INT_STATUS_VAL_Field is
     (
      --  P+ is smaller than P-.
      Smaller,
      --  P+ is greater than P-.
      Greater)
     with Size => 1;
   for COMP_INT_STATUS_VAL_Field use
     (Smaller => 0,
      Greater => 1);

   --  Comparator Interrupt status
   type COMP_INT_STATUS_Register is record
      --  Read-only. Interrupt status BEFORE Interrupt Enable.
      STATUS        : COMP_INT_STATUS_STATUS_Field;
      --  Read-only. Interrupt status AFTER Interrupt Enable.
      INT_STATUS    : COMP_INT_STATUS_INT_STATUS_Field;
      --  Read-only. comparator analog output.
      VAL           : COMP_INT_STATUS_VAL_Field;
      --  unspecified
      Reserved_3_31 : HAL.UInt29;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for COMP_INT_STATUS_Register use record
      STATUS        at 0 range 0 .. 0;
      INT_STATUS    at 0 range 1 .. 1;
      VAL           at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  Control automatic clock gating of ROM controller.
   type AUTOCLKGATEOVERRIDE_ROM_Field is
     (
      --  Automatic clock gating is not overridden.
      Disable,
      --  Automatic clock gating is overridden (Clock gating is disabled).
      Enable)
     with Size => 1;
   for AUTOCLKGATEOVERRIDE_ROM_Field use
     (Disable => 0,
      Enable => 1);

   --  Control automatic clock gating of RAMX controller.
   type AUTOCLKGATEOVERRIDE_RAMX_CTRL_Field is
     (
      --  Automatic clock gating is not overridden.
      Disable,
      --  Automatic clock gating is overridden (Clock gating is disabled).
      Enable)
     with Size => 1;
   for AUTOCLKGATEOVERRIDE_RAMX_CTRL_Field use
     (Disable => 0,
      Enable => 1);

   --  Control automatic clock gating of RAM0 controller.
   type AUTOCLKGATEOVERRIDE_RAM0_CTRL_Field is
     (
      --  Automatic clock gating is not overridden.
      Disable,
      --  Automatic clock gating is overridden (Clock gating is disabled).
      Enable)
     with Size => 1;
   for AUTOCLKGATEOVERRIDE_RAM0_CTRL_Field use
     (Disable => 0,
      Enable => 1);

   --  Control automatic clock gating of RAM1 controller.
   type AUTOCLKGATEOVERRIDE_RAM1_CTRL_Field is
     (
      --  Automatic clock gating is not overridden.
      Disable,
      --  Automatic clock gating is overridden (Clock gating is disabled).
      Enable)
     with Size => 1;
   for AUTOCLKGATEOVERRIDE_RAM1_CTRL_Field use
     (Disable => 0,
      Enable => 1);

   --  Control automatic clock gating of RAM2 controller.
   type AUTOCLKGATEOVERRIDE_RAM2_CTRL_Field is
     (
      --  Automatic clock gating is not overridden.
      Disable,
      --  Automatic clock gating is overridden (Clock gating is disabled).
      Enable)
     with Size => 1;
   for AUTOCLKGATEOVERRIDE_RAM2_CTRL_Field use
     (Disable => 0,
      Enable => 1);

   --  Control automatic clock gating of RAM3 controller.
   type AUTOCLKGATEOVERRIDE_RAM3_CTRL_Field is
     (
      --  Automatic clock gating is not overridden.
      Disable,
      --  Automatic clock gating is overridden (Clock gating is disabled).
      Enable)
     with Size => 1;
   for AUTOCLKGATEOVERRIDE_RAM3_CTRL_Field use
     (Disable => 0,
      Enable => 1);

   --  Control automatic clock gating of RAM4 controller.
   type AUTOCLKGATEOVERRIDE_RAM4_CTRL_Field is
     (
      --  Automatic clock gating is not overridden.
      Disable,
      --  Automatic clock gating is overridden (Clock gating is disabled).
      Enable)
     with Size => 1;
   for AUTOCLKGATEOVERRIDE_RAM4_CTRL_Field use
     (Disable => 0,
      Enable => 1);

   --  Control automatic clock gating of synchronous bridge controller 0.
   type AUTOCLKGATEOVERRIDE_SYNC0_APB_Field is
     (
      --  Automatic clock gating is not overridden.
      Disable,
      --  Automatic clock gating is overridden (Clock gating is disabled).
      Enable)
     with Size => 1;
   for AUTOCLKGATEOVERRIDE_SYNC0_APB_Field use
     (Disable => 0,
      Enable => 1);

   --  Control automatic clock gating of synchronous bridge controller 1.
   type AUTOCLKGATEOVERRIDE_SYNC1_APB_Field is
     (
      --  Automatic clock gating is not overridden.
      Disable,
      --  Automatic clock gating is overridden (Clock gating is disabled).
      Enable)
     with Size => 1;
   for AUTOCLKGATEOVERRIDE_SYNC1_APB_Field use
     (Disable => 0,
      Enable => 1);

   --  Control automatic clock gating of CRCGEN controller.
   type AUTOCLKGATEOVERRIDE_CRCGEN_Field is
     (
      --  Automatic clock gating is not overridden.
      Disable,
      --  Automatic clock gating is overridden (Clock gating is disabled).
      Enable)
     with Size => 1;
   for AUTOCLKGATEOVERRIDE_CRCGEN_Field use
     (Disable => 0,
      Enable => 1);

   --  Control automatic clock gating of DMA0 controller.
   type AUTOCLKGATEOVERRIDE_SDMA0_Field is
     (
      --  Automatic clock gating is not overridden.
      Disable,
      --  Automatic clock gating is overridden (Clock gating is disabled).
      Enable)
     with Size => 1;
   for AUTOCLKGATEOVERRIDE_SDMA0_Field use
     (Disable => 0,
      Enable => 1);

   --  AUTOCLKGATEOVERRIDE_SDMA array
   type AUTOCLKGATEOVERRIDE_SDMA_Field_Array is array (0 .. 1)
     of AUTOCLKGATEOVERRIDE_SDMA0_Field
     with Component_Size => 1, Size => 2;

   --  Type definition for AUTOCLKGATEOVERRIDE_SDMA
   type AUTOCLKGATEOVERRIDE_SDMA_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SDMA as a value
            Val : HAL.UInt2;
         when True =>
            --  SDMA as an array
            Arr : AUTOCLKGATEOVERRIDE_SDMA_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for AUTOCLKGATEOVERRIDE_SDMA_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Control automatic clock gating of USB controller.
   type AUTOCLKGATEOVERRIDE_USB0_Field is
     (
      --  Automatic clock gating is not overridden.
      Disable,
      --  Automatic clock gating is overridden (Clock gating is disabled).
      Enable)
     with Size => 1;
   for AUTOCLKGATEOVERRIDE_USB0_Field use
     (Disable => 0,
      Enable => 1);

   --  Control automatic clock gating of synchronous system controller
   --  registers bank.
   type AUTOCLKGATEOVERRIDE_SYSCON_Field is
     (
      --  Automatic clock gating is not overridden.
      Disable,
      --  Automatic clock gating is overridden (Clock gating is disabled).
      Enable)
     with Size => 1;
   for AUTOCLKGATEOVERRIDE_SYSCON_Field use
     (Disable => 0,
      Enable => 1);

   --  The value 0xC0DE must be written for AUTOCLKGATEOVERRIDE registers
   --  fields updates to have effect.
   type AUTOCLKGATEOVERRIDE_ENABLEUPDATE_Field is
     (
      --  Bit Fields 0 - 15 of this register are not updated
      Disable,
      --  Bit Fields 0 - 15 of this register are updated
      Enable)
     with Size => 16;
   for AUTOCLKGATEOVERRIDE_ENABLEUPDATE_Field use
     (Disable => 0,
      Enable => 49374);

   --  Control automatic clock gating
   type AUTOCLKGATEOVERRIDE_Register is record
      --  Control automatic clock gating of ROM controller.
      ROM           : AUTOCLKGATEOVERRIDE_ROM_Field := NXP_SVD.SYSCON.Enable;
      --  Control automatic clock gating of RAMX controller.
      RAMX_CTRL     : AUTOCLKGATEOVERRIDE_RAMX_CTRL_Field :=
                       NXP_SVD.SYSCON.Enable;
      --  Control automatic clock gating of RAM0 controller.
      RAM0_CTRL     : AUTOCLKGATEOVERRIDE_RAM0_CTRL_Field :=
                       NXP_SVD.SYSCON.Enable;
      --  Control automatic clock gating of RAM1 controller.
      RAM1_CTRL     : AUTOCLKGATEOVERRIDE_RAM1_CTRL_Field :=
                       NXP_SVD.SYSCON.Enable;
      --  Control automatic clock gating of RAM2 controller.
      RAM2_CTRL     : AUTOCLKGATEOVERRIDE_RAM2_CTRL_Field :=
                       NXP_SVD.SYSCON.Enable;
      --  Control automatic clock gating of RAM3 controller.
      RAM3_CTRL     : AUTOCLKGATEOVERRIDE_RAM3_CTRL_Field :=
                       NXP_SVD.SYSCON.Enable;
      --  Control automatic clock gating of RAM4 controller.
      RAM4_CTRL     : AUTOCLKGATEOVERRIDE_RAM4_CTRL_Field :=
                       NXP_SVD.SYSCON.Enable;
      --  Control automatic clock gating of synchronous bridge controller 0.
      SYNC0_APB     : AUTOCLKGATEOVERRIDE_SYNC0_APB_Field :=
                       NXP_SVD.SYSCON.Enable;
      --  Control automatic clock gating of synchronous bridge controller 1.
      SYNC1_APB     : AUTOCLKGATEOVERRIDE_SYNC1_APB_Field :=
                       NXP_SVD.SYSCON.Enable;
      --  unspecified
      Reserved_9_10 : HAL.UInt2 := 16#3#;
      --  Control automatic clock gating of CRCGEN controller.
      CRCGEN        : AUTOCLKGATEOVERRIDE_CRCGEN_Field :=
                       NXP_SVD.SYSCON.Enable;
      --  Control automatic clock gating of DMA0 controller.
      SDMA          : AUTOCLKGATEOVERRIDE_SDMA_Field :=
                       (As_Array => False, Val => 16#1#);
      --  Control automatic clock gating of USB controller.
      USB0          : AUTOCLKGATEOVERRIDE_USB0_Field := NXP_SVD.SYSCON.Enable;
      --  Control automatic clock gating of synchronous system controller
      --  registers bank.
      SYSCON        : AUTOCLKGATEOVERRIDE_SYSCON_Field :=
                       NXP_SVD.SYSCON.Enable;
      --  Write-only. The value 0xC0DE must be written for AUTOCLKGATEOVERRIDE
      --  registers fields updates to have effect.
      ENABLEUPDATE  : AUTOCLKGATEOVERRIDE_ENABLEUPDATE_Field :=
                       NXP_SVD.SYSCON.Disable;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for AUTOCLKGATEOVERRIDE_Register use record
      ROM           at 0 range 0 .. 0;
      RAMX_CTRL     at 0 range 1 .. 1;
      RAM0_CTRL     at 0 range 2 .. 2;
      RAM1_CTRL     at 0 range 3 .. 3;
      RAM2_CTRL     at 0 range 4 .. 4;
      RAM3_CTRL     at 0 range 5 .. 5;
      RAM4_CTRL     at 0 range 6 .. 6;
      SYNC0_APB     at 0 range 7 .. 7;
      SYNC1_APB     at 0 range 8 .. 8;
      Reserved_9_10 at 0 range 9 .. 10;
      CRCGEN        at 0 range 11 .. 11;
      SDMA          at 0 range 12 .. 13;
      USB0          at 0 range 14 .. 14;
      SYSCON        at 0 range 15 .. 15;
      ENABLEUPDATE  at 0 range 16 .. 31;
   end record;

   --  Enable bypass of the first stage of synchonization inside GPIO_INT
   --  module.
   type GPIOPSYNC_PSYNC_Field is
     (
      --  use the first stage of synchonization inside GPIO_INT module.
      Used,
      --  bypass of the first stage of synchonization inside GPIO_INT module.
      Bypass)
     with Size => 1;
   for GPIOPSYNC_PSYNC_Field use
     (Used => 0,
      Bypass => 1);

   --  Enable bypass of the first stage of synchonization inside GPIO_INT
   --  module
   type GPIOPSYNC_Register is record
      --  Enable bypass of the first stage of synchonization inside GPIO_INT
      --  module.
      PSYNC         : GPIOPSYNC_PSYNC_Field := NXP_SVD.SYSCON.Used;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPIOPSYNC_Register use record
      PSYNC         at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Control write access to CODESECURITYPROTTEST, CODESECURITYPROTCPU0,
   --  CODESECURITYPROTCPU1, CPU0_DEBUG_FEATURES, CPU1_DEBUG_FEATURES and
   --  DBG_AUTH_SCRATCH registers.
   type DEBUG_LOCK_EN_LOCK_ALL_Field is
     (
      --  Any other value than b1010: disable write access to all 6 registers.
      Disable,
      --  Reset value for the field
      Debug_Lock_En_Lock_All_Field_Reset,
      --  1010: Enable write access to all 6 registers.
      Enable)
     with Size => 4;
   for DEBUG_LOCK_EN_LOCK_ALL_Field use
     (Disable => 0,
      Debug_Lock_En_Lock_All_Field_Reset => 5,
      Enable => 10);

   --  Control write access to security registers.
   type DEBUG_LOCK_EN_Register is record
      --  Control write access to CODESECURITYPROTTEST, CODESECURITYPROTCPU0,
      --  CODESECURITYPROTCPU1, CPU0_DEBUG_FEATURES, CPU1_DEBUG_FEATURES and
      --  DBG_AUTH_SCRATCH registers.
      LOCK_ALL      : DEBUG_LOCK_EN_LOCK_ALL_Field :=
                       Debug_Lock_En_Lock_All_Field_Reset;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DEBUG_LOCK_EN_Register use record
      LOCK_ALL      at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  CPU0 Invasive debug control:.
   type DEBUG_FEATURES_CPU0_DBGEN_Field is
     (
      --  Reset value for the field
      Debug_Features_Cpu0_Dbgen_Field_Reset,
      --  Any other value than b10: invasive debug is disable.
      Disable,
      --  10: Invasive debug is enabled.
      Enable)
     with Size => 2;
   for DEBUG_FEATURES_CPU0_DBGEN_Field use
     (Debug_Features_Cpu0_Dbgen_Field_Reset => 0,
      Disable => 1,
      Enable => 2);

   --  CPU0 Non Invasive debug control:.
   type DEBUG_FEATURES_CPU0_NIDEN_Field is
     (
      --  Reset value for the field
      Debug_Features_Cpu0_Niden_Field_Reset,
      --  Any other value than b10: invasive debug is disable.
      Disable,
      --  10: Invasive debug is enabled.
      Enable)
     with Size => 2;
   for DEBUG_FEATURES_CPU0_NIDEN_Field use
     (Debug_Features_Cpu0_Niden_Field_Reset => 0,
      Disable => 1,
      Enable => 2);

   --  CPU0 Secure Invasive debug control:.
   type DEBUG_FEATURES_CPU0_SPIDEN_Field is
     (
      --  Reset value for the field
      Debug_Features_Cpu0_Spiden_Field_Reset,
      --  Any other value than b10: invasive debug is disable.
      Disable,
      --  10: Invasive debug is enabled.
      Enable)
     with Size => 2;
   for DEBUG_FEATURES_CPU0_SPIDEN_Field use
     (Debug_Features_Cpu0_Spiden_Field_Reset => 0,
      Disable => 1,
      Enable => 2);

   --  CPU0 Secure Non Invasive debug control:.
   type DEBUG_FEATURES_CPU0_SPNIDEN_Field is
     (
      --  Reset value for the field
      Debug_Features_Cpu0_Spniden_Field_Reset,
      --  Any other value than b10: invasive debug is disable.
      Disable,
      --  10: Invasive debug is enabled.
      Enable)
     with Size => 2;
   for DEBUG_FEATURES_CPU0_SPNIDEN_Field use
     (Debug_Features_Cpu0_Spniden_Field_Reset => 0,
      Disable => 1,
      Enable => 2);

   --  CPU1 Invasive debug control:.
   type DEBUG_FEATURES_CPU1_DBGEN_Field is
     (
      --  Reset value for the field
      Debug_Features_Cpu1_Dbgen_Field_Reset,
      --  Any other value than b10: invasive debug is disable.
      Disable,
      --  10: Invasive debug is enabled.
      Enable)
     with Size => 2;
   for DEBUG_FEATURES_CPU1_DBGEN_Field use
     (Debug_Features_Cpu1_Dbgen_Field_Reset => 0,
      Disable => 1,
      Enable => 2);

   --  CPU1 Non Invasive debug control:.
   type DEBUG_FEATURES_CPU1_NIDEN_Field is
     (
      --  Reset value for the field
      Debug_Features_Cpu1_Niden_Field_Reset,
      --  Any other value than b10: invasive debug is disable.
      Disable,
      --  10: Invasive debug is enabled.
      Enable)
     with Size => 2;
   for DEBUG_FEATURES_CPU1_NIDEN_Field use
     (Debug_Features_Cpu1_Niden_Field_Reset => 0,
      Disable => 1,
      Enable => 2);

   --  Cortex M33 (CPU0) and micro Cortex M33 (CPU1) debug features control.
   type DEBUG_FEATURES_Register is record
      --  CPU0 Invasive debug control:.
      CPU0_DBGEN     : DEBUG_FEATURES_CPU0_DBGEN_Field :=
                        Debug_Features_Cpu0_Dbgen_Field_Reset;
      --  CPU0 Non Invasive debug control:.
      CPU0_NIDEN     : DEBUG_FEATURES_CPU0_NIDEN_Field :=
                        Debug_Features_Cpu0_Niden_Field_Reset;
      --  CPU0 Secure Invasive debug control:.
      CPU0_SPIDEN    : DEBUG_FEATURES_CPU0_SPIDEN_Field :=
                        Debug_Features_Cpu0_Spiden_Field_Reset;
      --  CPU0 Secure Non Invasive debug control:.
      CPU0_SPNIDEN   : DEBUG_FEATURES_CPU0_SPNIDEN_Field :=
                        Debug_Features_Cpu0_Spniden_Field_Reset;
      --  CPU1 Invasive debug control:.
      CPU1_DBGEN     : DEBUG_FEATURES_CPU1_DBGEN_Field :=
                        Debug_Features_Cpu1_Dbgen_Field_Reset;
      --  CPU1 Non Invasive debug control:.
      CPU1_NIDEN     : DEBUG_FEATURES_CPU1_NIDEN_Field :=
                        Debug_Features_Cpu1_Niden_Field_Reset;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DEBUG_FEATURES_Register use record
      CPU0_DBGEN     at 0 range 0 .. 1;
      CPU0_NIDEN     at 0 range 2 .. 3;
      CPU0_SPIDEN    at 0 range 4 .. 5;
      CPU0_SPNIDEN   at 0 range 6 .. 7;
      CPU1_DBGEN     at 0 range 8 .. 9;
      CPU1_NIDEN     at 0 range 10 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   --  CPU0 (CPU0) Invasive debug control:.
   type DEBUG_FEATURES_DP_CPU0_DBGEN_Field is
     (
      --  Any other value than b10: invasive debug is disable.
      Disable,
      --  10: Invasive debug is enabled.
      Enable)
     with Size => 2;
   for DEBUG_FEATURES_DP_CPU0_DBGEN_Field use
     (Disable => 1,
      Enable => 2);

   --  CPU0 Non Invasive debug control:.
   type DEBUG_FEATURES_DP_CPU0_NIDEN_Field is
     (
      --  Any other value than b10: invasive debug is disable.
      Disable,
      --  10: Invasive debug is enabled.
      Enable)
     with Size => 2;
   for DEBUG_FEATURES_DP_CPU0_NIDEN_Field use
     (Disable => 1,
      Enable => 2);

   --  CPU0 Secure Invasive debug control:.
   type DEBUG_FEATURES_DP_CPU0_SPIDEN_Field is
     (
      --  Any other value than b10: invasive debug is disable.
      Disable,
      --  10: Invasive debug is enabled.
      Enable)
     with Size => 2;
   for DEBUG_FEATURES_DP_CPU0_SPIDEN_Field use
     (Disable => 1,
      Enable => 2);

   --  CPU0 Secure Non Invasive debug control:.
   type DEBUG_FEATURES_DP_CPU0_SPNIDEN_Field is
     (
      --  Any other value than b10: invasive debug is disable.
      Disable,
      --  10: Invasive debug is enabled.
      Enable)
     with Size => 2;
   for DEBUG_FEATURES_DP_CPU0_SPNIDEN_Field use
     (Disable => 1,
      Enable => 2);

   --  CPU1 Invasive debug control:.
   type DEBUG_FEATURES_DP_CPU1_DBGEN_Field is
     (
      --  Any other value than b10: invasive debug is disable.
      Disable,
      --  10: Invasive debug is enabled.
      Enable)
     with Size => 2;
   for DEBUG_FEATURES_DP_CPU1_DBGEN_Field use
     (Disable => 1,
      Enable => 2);

   --  CPU1 Non Invasive debug control:.
   type DEBUG_FEATURES_DP_CPU1_NIDEN_Field is
     (
      --  Any other value than b10: invasive debug is disable.
      Disable,
      --  10: Invasive debug is enabled.
      Enable)
     with Size => 2;
   for DEBUG_FEATURES_DP_CPU1_NIDEN_Field use
     (Disable => 1,
      Enable => 2);

   --  Cortex M33 (CPU0) and micro Cortex M33 (CPU1) debug features control
   --  DUPLICATE register.
   type DEBUG_FEATURES_DP_Register is record
      --  CPU0 (CPU0) Invasive debug control:.
      CPU0_DBGEN     : DEBUG_FEATURES_DP_CPU0_DBGEN_Field :=
                        NXP_SVD.SYSCON.Disable;
      --  CPU0 Non Invasive debug control:.
      CPU0_NIDEN     : DEBUG_FEATURES_DP_CPU0_NIDEN_Field :=
                        NXP_SVD.SYSCON.Disable;
      --  CPU0 Secure Invasive debug control:.
      CPU0_SPIDEN    : DEBUG_FEATURES_DP_CPU0_SPIDEN_Field :=
                        NXP_SVD.SYSCON.Disable;
      --  CPU0 Secure Non Invasive debug control:.
      CPU0_SPNIDEN   : DEBUG_FEATURES_DP_CPU0_SPNIDEN_Field :=
                        NXP_SVD.SYSCON.Disable;
      --  CPU1 Invasive debug control:.
      CPU1_DBGEN     : DEBUG_FEATURES_DP_CPU1_DBGEN_Field :=
                        NXP_SVD.SYSCON.Disable;
      --  CPU1 Non Invasive debug control:.
      CPU1_NIDEN     : DEBUG_FEATURES_DP_CPU1_NIDEN_Field :=
                        NXP_SVD.SYSCON.Disable;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DEBUG_FEATURES_DP_Register use record
      CPU0_DBGEN     at 0 range 0 .. 1;
      CPU0_NIDEN     at 0 range 2 .. 3;
      CPU0_SPIDEN    at 0 range 4 .. 5;
      CPU0_SPNIDEN   at 0 range 6 .. 7;
      CPU1_DBGEN     at 0 range 8 .. 9;
      CPU1_NIDEN     at 0 range 10 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   --  Enable CPU1.
   type CPUCFG_CPU1ENABLE_Field is
     (
      --  CPU1 is disable (Processor in reset).
      Disable,
      --  CPU1 is enable.
      Enable)
     with Size => 1;
   for CPUCFG_CPU1ENABLE_Field use
     (Disable => 0,
      Enable => 1);

   --  CPUs configuration register
   type CPUCFG_Register is record
      --  unspecified
      Reserved_0_1  : HAL.UInt2 := 16#2#;
      --  Enable CPU1.
      CPU1ENABLE    : CPUCFG_CPU1ENABLE_Field := NXP_SVD.SYSCON.Disable;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPUCFG_Register use record
      Reserved_0_1  at 0 range 0 .. 1;
      CPU1ENABLE    at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype DEVICE_ID0_ROM_REV_MINOR_Field is HAL.UInt4;

   --  Device ID
   type DEVICE_ID0_Register is record
      --  unspecified
      Reserved_0_19  : HAL.UInt20;
      --  Read-only. ROM revision.
      ROM_REV_MINOR  : DEVICE_ID0_ROM_REV_MINOR_Field;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DEVICE_ID0_Register use record
      Reserved_0_19  at 0 range 0 .. 19;
      ROM_REV_MINOR  at 0 range 20 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype DIEID_REV_ID_Field is HAL.UInt4;
   subtype DIEID_MCO_NUM_IN_DIE_ID_Field is HAL.UInt20;

   --  Chip revision ID and Number
   type DIEID_Register is record
      --  Read-only. Chip Metal Revision ID.
      REV_ID            : DIEID_REV_ID_Field;
      --  Read-only. Chip Number 0x426B.
      MCO_NUM_IN_DIE_ID : DIEID_MCO_NUM_IN_DIE_ID_Field;
      --  unspecified
      Reserved_24_31    : HAL.UInt8;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DIEID_Register use record
      REV_ID            at 0 range 0 .. 3;
      MCO_NUM_IN_DIE_ID at 0 range 4 .. 23;
      Reserved_24_31    at 0 range 24 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   type SYSCON_Disc is
     (
      SYSCON_Disc_0,
      X0,
      SYSCON_Disc_1,
      X1,
      SYSCON_Disc_2,
      X2,
      SYSCON_Disc_3,
      X3,
      SYSCON_Disc_4,
      X4,
      SYSCON_Disc_5,
      X5,
      SYSCON_Disc_6,
      X6,
      SYSCON_Disc_7,
      X7,
      SYSCON_Disc_0Ctrl,
      Xctrl0,
      SYSCON_Disc_1Ctrl,
      Xctrl1,
      SYSCON_Disc_2Ctrl,
      Xctrl2,
      SYSCON_Disc_3Ctrl,
      Xctrl3,
      SYSCON_Disc_4Ctrl,
      Xctrl4,
      SYSCON_Disc_5Ctrl,
      Xctrl5,
      SYSCON_Disc_6Ctrl,
      Xctrl6,
      SYSCON_Disc_7Ctrl,
      Xctrl7);

   --  SYSCON
   type SYSCON_Peripheral
     (Discriminent : SYSCON_Disc := SYSCON_Disc_0)
   is record
      --  Memory Remap control register
      MEMORYREMAP           : aliased MEMORYREMAP_Register;
      --  AHB Matrix priority control register Priority values are 3 = highest,
      --  0 = lowest
      AHBMATPRIO            : aliased AHBMATPRIO_Register;
      --  System tick calibration for secure part of CPU0
      CPU0STCKCAL           : aliased CPU0STCKCAL_Register;
      --  System tick calibration for non-secure part of CPU0
      CPU0NSTCKCAL          : aliased CPU0NSTCKCAL_Register;
      --  System tick calibration for CPU1
      CPU1STCKCAL           : aliased CPU1STCKCAL_Register;
      --  NMI Source Select
      NMISRC                : aliased NMISRC_Register;
      --  Peripheral reset control set register
      PRESETCTRLSET         : aliased PRESETCTRLSET_Registers;
      --  Peripheral reset control clear register
      PRESETCTRLCLR         : aliased PRESETCTRLCLR_Registers;
      --  generate a software_reset
      SWR_RESET             : aliased HAL.UInt32;
      --  Peripheral reset control register
      AHBCLKCTRLSET         : aliased AHBCLKCTRLSET_Registers;
      --  Peripheral reset control register
      AHBCLKCTRLCLR         : aliased AHBCLKCTRLCLR_Registers;
      --  Trace clock source select
      TRACECLKSEL           : aliased TRACECLKSEL_Register;
      --  Main clock A source select
      MAINCLKSELA           : aliased MAINCLKSELA_Register;
      --  Main clock source select
      MAINCLKSELB           : aliased MAINCLKSELB_Register;
      --  CLKOUT clock source select
      CLKOUTSEL             : aliased CLKOUTSEL_Register;
      --  PLL0 clock source select
      PLL0CLKSEL            : aliased PLL0CLKSEL_Register;
      --  PLL1 clock source select
      PLL1CLKSEL            : aliased PLL1CLKSEL_Register;
      --  ADC clock source select
      ADCCLKSEL             : aliased ADCCLKSEL_Register;
      --  FS USB clock source select
      USB0CLKSEL            : aliased USB0CLKSEL_Register;
      --  HS LSPI clock source select
      HSLSPICLKSEL          : aliased HSLSPICLKSEL_Register;
      --  MCLK clock source select
      MCLKCLKSEL            : aliased MCLKCLKSEL_Register;
      --  SCTimer/PWM clock source select
      SCTCLKSEL             : aliased SCTCLKSEL_Register;
      --  SDIO clock source select
      SDIOCLKSEL            : aliased SDIOCLKSEL_Register;
      --  System Tick Timer divider for CPU0
      SYSTICKCLKDIV0        : aliased SYSTICKCLKDIV_Register;
      --  System Tick Timer divider for CPU1
      SYSTICKCLKDIV1        : aliased SYSTICKCLKDIV_Register;
      --  TRACE clock divider
      TRACECLKDIV           : aliased TRACECLKDIV_Register;
      --  System clock divider
      AHBCLKDIV             : aliased AHBCLKDIV_Register;
      --  CLKOUT clock divider
      CLKOUTDIV             : aliased CLKOUTDIV_Register;
      --  FRO_HF (96MHz) clock divider
      FROHFDIV              : aliased FROHFDIV_Register;
      --  WDT clock divider
      WDTCLKDIV             : aliased WDTCLKDIV_Register;
      --  ADC clock divider
      ADCCLKDIV             : aliased ADCCLKDIV_Register;
      --  USB0 Clock divider
      USB0CLKDIV            : aliased USB0CLKDIV_Register;
      --  I2S MCLK clock divider
      MCLKDIV               : aliased MCLKDIV_Register;
      --  SCT/PWM clock divider
      SCTCLKDIV             : aliased SCTCLKDIV_Register;
      --  SDIO clock divider
      SDIOCLKDIV            : aliased SDIOCLKDIV_Register;
      --  PLL0 clock divider
      PLL0CLKDIV            : aliased PLL0CLKDIV_Register;
      --  Control clock configuration registers access (like xxxDIV, xxxSEL)
      CLOCKGENUPDATELOCKOUT : aliased HAL.UInt32;
      --  FMC configuration register
      FMCCR                 : aliased FMCCR_Register;
      --  USB0 need clock control
      USB0NEEDCLKCTRL       : aliased USB0NEEDCLKCTRL_Register;
      --  USB0 need clock status
      USB0NEEDCLKSTAT       : aliased USB0NEEDCLKSTAT_Register;
      --  FMCflush control
      FMCFLUSH              : aliased FMCFLUSH_Register;
      --  MCLK control
      MCLKIO                : aliased HAL.UInt32;
      --  USB1 need clock control
      USB1NEEDCLKCTRL       : aliased USB1NEEDCLKCTRL_Register;
      --  USB1 need clock status
      USB1NEEDCLKSTAT       : aliased USB1NEEDCLKSTAT_Register;
      --  SDIO CCLKIN phase and delay control
      SDIOCLKCTRL           : aliased SDIOCLKCTRL_Register;
      --  PLL1 550m control
      PLL1CTRL              : aliased PLL1CTRL_Register;
      --  PLL1 550m status
      PLL1STAT              : aliased PLL1STAT_Register;
      --  PLL1 550m N divider
      PLL1NDEC              : aliased PLL1NDEC_Register;
      --  PLL1 550m M divider
      PLL1MDEC              : aliased PLL1MDEC_Register;
      --  PLL1 550m P divider
      PLL1PDEC              : aliased PLL1PDEC_Register;
      --  PLL0 550m control
      PLL0CTRL              : aliased PLL0CTRL_Register;
      --  PLL0 550m status
      PLL0STAT              : aliased PLL0STAT_Register;
      --  PLL0 550m N divider
      PLL0NDEC              : aliased PLL0NDEC_Register;
      --  PLL0 550m P divider
      PLL0PDEC              : aliased PLL0PDEC_Register;
      --  PLL0 Spread Spectrum Wrapper control register 0
      PLL0SSCG0             : aliased HAL.UInt32;
      --  PLL0 Spread Spectrum Wrapper control register 1
      PLL0SSCG1             : aliased PLL0SSCG1_Register;
      --  CPU Control for multiple processors
      CPUCTRL               : aliased CPUCTRL_Register;
      --  Coprocessor Boot Address
      CPBOOT                : aliased HAL.UInt32;
      --  CPU Status
      CPSTAT                : aliased CPSTAT_Register;
      --  Various system clock controls : Flash clock (48 MHz) control, clocks
      --  to Frequency Measures
      CLOCK_CTRL            : aliased CLOCK_CTRL_Register;
      --  Comparator Interrupt control
      COMP_INT_CTRL         : aliased COMP_INT_CTRL_Register;
      --  Comparator Interrupt status
      COMP_INT_STATUS       : aliased COMP_INT_STATUS_Register;
      --  Control automatic clock gating
      AUTOCLKGATEOVERRIDE   : aliased AUTOCLKGATEOVERRIDE_Register;
      --  Enable bypass of the first stage of synchonization inside GPIO_INT
      --  module
      GPIOPSYNC             : aliased GPIOPSYNC_Register;
      --  Control write access to security registers.
      DEBUG_LOCK_EN         : aliased DEBUG_LOCK_EN_Register;
      --  Cortex M33 (CPU0) and micro Cortex M33 (CPU1) debug features control.
      DEBUG_FEATURES        : aliased DEBUG_FEATURES_Register;
      --  Cortex M33 (CPU0) and micro Cortex M33 (CPU1) debug features control
      --  DUPLICATE register.
      DEBUG_FEATURES_DP     : aliased DEBUG_FEATURES_DP_Register;
      --  block quiddikey/PUF all index.
      KEY_BLOCK             : aliased HAL.UInt32;
      --  Debug authentication BEACON register
      DEBUG_AUTH_BEACON     : aliased HAL.UInt32;
      --  CPUs configuration register
      CPUCFG                : aliased CPUCFG_Register;
      --  Device ID
      DEVICE_ID0            : aliased DEVICE_ID0_Register;
      --  Chip revision ID and Number
      DIEID                 : aliased DIEID_Register;
      case Discriminent is
         when SYSCON_Disc_0 =>
            --  Peripheral reset control 0
            PRESETCTRL0 : aliased PRESETCTRL0_Register;
            --  AHB Clock control 0
            AHBCLKCTRL0 : aliased AHBCLKCTRL0_Register;
            --  System Tick Timer for CPU0 source select
            SYSTICKCLKSEL0 : aliased SYSTICKCLKSEL_Register;
            --  CTimer 0 clock source select
            CTIMERCLKSEL0 : aliased CTIMERCLKSEL_Register;
            --  Flexcomm Interface 0 clock source select for Fractional Rate
            --  Divider
            FCCLKSEL0 : aliased FCCLKSEL_Register;
         when X0 =>
            --  Peripheral reset control register
            PRESETCTRLX0 : aliased HAL.UInt32;
            --  Peripheral reset control register
            AHBCLKCTRLX0 : aliased HAL.UInt32;
            --  Peripheral reset control register
            SYSTICKCLKSELX0 : aliased HAL.UInt32;
            --  Peripheral reset control register
            CTIMERCLKSELX0 : aliased HAL.UInt32;
            --  Peripheral reset control register
            FCCLKSELX0 : aliased HAL.UInt32;
         when SYSCON_Disc_1 =>
            --  Peripheral reset control 1
            PRESETCTRL1 : aliased PRESETCTRL1_Register;
            --  AHB Clock control 1
            AHBCLKCTRL1 : aliased AHBCLKCTRL1_Register;
            --  System Tick Timer for CPU1 source select
            SYSTICKCLKSEL1 : aliased SYSTICKCLKSEL_Register;
            --  CTimer 1 clock source select
            CTIMERCLKSEL1 : aliased CTIMERCLKSEL_Register;
            --  Flexcomm Interface 1 clock source select for Fractional Rate
            --  Divider
            FCCLKSEL1 : aliased FCCLKSEL_Register;
         when X1 =>
            --  Peripheral reset control register
            PRESETCTRLX1 : aliased HAL.UInt32;
            --  Peripheral reset control register
            AHBCLKCTRLX1 : aliased HAL.UInt32;
            --  Peripheral reset control register
            SYSTICKCLKSELX1 : aliased HAL.UInt32;
            --  Peripheral reset control register
            CTIMERCLKSELX1 : aliased HAL.UInt32;
            --  Peripheral reset control register
            FCCLKSELX1 : aliased HAL.UInt32;
         when SYSCON_Disc_2 =>
            --  Peripheral reset control 2
            PRESETCTRL2 : aliased PRESETCTRL2_Register;
            --  AHB Clock control 2
            AHBCLKCTRL2 : aliased AHBCLKCTRL2_Register;
            --  CTimer 2 clock source select
            CTIMERCLKSEL2 : aliased CTIMERCLKSEL_Register;
            --  Flexcomm Interface 2 clock source select for Fractional Rate
            --  Divider
            FCCLKSEL2 : aliased FCCLKSEL_Register;
         when X2 =>
            --  Peripheral reset control register
            PRESETCTRLX2 : aliased HAL.UInt32;
            --  Peripheral reset control register
            AHBCLKCTRLX2 : aliased HAL.UInt32;
            --  Peripheral reset control register
            CTIMERCLKSELX2 : aliased HAL.UInt32;
            --  Peripheral reset control register
            FCCLKSELX2 : aliased HAL.UInt32;
         when SYSCON_Disc_3 =>
            --  CTimer 3 clock source select
            CTIMERCLKSEL3 : aliased CTIMERCLKSEL_Register;
            --  Flexcomm Interface 3 clock source select for Fractional Rate
            --  Divider
            FCCLKSEL3 : aliased FCCLKSEL_Register;
         when X3 =>
            --  Peripheral reset control register
            CTIMERCLKSELX3 : aliased HAL.UInt32;
            --  Peripheral reset control register
            FCCLKSELX3 : aliased HAL.UInt32;
         when SYSCON_Disc_4 =>
            --  CTimer 4 clock source select
            CTIMERCLKSEL4 : aliased CTIMERCLKSEL_Register;
            --  Flexcomm Interface 4 clock source select for Fractional Rate
            --  Divider
            FCCLKSEL4 : aliased FCCLKSEL_Register;
         when X4 =>
            --  Peripheral reset control register
            CTIMERCLKSELX4 : aliased HAL.UInt32;
            --  Peripheral reset control register
            FCCLKSELX4 : aliased HAL.UInt32;
         when SYSCON_Disc_5 =>
            --  Flexcomm Interface 5 clock source select for Fractional Rate
            --  Divider
            FCCLKSEL5 : aliased FCCLKSEL_Register;
         when X5 =>
            --  Peripheral reset control register
            FCCLKSELX5 : aliased HAL.UInt32;
         when SYSCON_Disc_6 =>
            --  Flexcomm Interface 6 clock source select for Fractional Rate
            --  Divider
            FCCLKSEL6 : aliased FCCLKSEL_Register;
         when X6 =>
            --  Peripheral reset control register
            FCCLKSELX6 : aliased HAL.UInt32;
         when SYSCON_Disc_7 =>
            --  Flexcomm Interface 7 clock source select for Fractional Rate
            --  Divider
            FCCLKSEL7 : aliased FCCLKSEL_Register;
         when X7 =>
            --  Peripheral reset control register
            FCCLKSELX7 : aliased HAL.UInt32;
         when SYSCON_Disc_0Ctrl =>
            --  Fractional rate divider for flexcomm 0
            FLEXFRG0CTRL : aliased FLEXFRG0CTRL_Register;
         when Xctrl0 =>
            --  Peripheral reset control register
            FLEXFRGXCTRL0 : aliased HAL.UInt32;
         when SYSCON_Disc_1Ctrl =>
            --  Fractional rate divider for flexcomm 1
            FLEXFRG1CTRL : aliased FLEXFRG1CTRL_Register;
         when Xctrl1 =>
            --  Peripheral reset control register
            FLEXFRGXCTRL1 : aliased HAL.UInt32;
         when SYSCON_Disc_2Ctrl =>
            --  Fractional rate divider for flexcomm 2
            FLEXFRG2CTRL : aliased FLEXFRG2CTRL_Register;
         when Xctrl2 =>
            --  Peripheral reset control register
            FLEXFRGXCTRL2 : aliased HAL.UInt32;
         when SYSCON_Disc_3Ctrl =>
            --  Fractional rate divider for flexcomm 3
            FLEXFRG3CTRL : aliased FLEXFRG3CTRL_Register;
         when Xctrl3 =>
            --  Peripheral reset control register
            FLEXFRGXCTRL3 : aliased HAL.UInt32;
         when SYSCON_Disc_4Ctrl =>
            --  Fractional rate divider for flexcomm 4
            FLEXFRG4CTRL : aliased FLEXFRG4CTRL_Register;
         when Xctrl4 =>
            --  Peripheral reset control register
            FLEXFRGXCTRL4 : aliased HAL.UInt32;
         when SYSCON_Disc_5Ctrl =>
            --  Fractional rate divider for flexcomm 5
            FLEXFRG5CTRL : aliased FLEXFRG5CTRL_Register;
         when Xctrl5 =>
            --  Peripheral reset control register
            FLEXFRGXCTRL5 : aliased HAL.UInt32;
         when SYSCON_Disc_6Ctrl =>
            --  Fractional rate divider for flexcomm 6
            FLEXFRG6CTRL : aliased FLEXFRG6CTRL_Register;
         when Xctrl6 =>
            --  Peripheral reset control register
            FLEXFRGXCTRL6 : aliased HAL.UInt32;
         when SYSCON_Disc_7Ctrl =>
            --  Fractional rate divider for flexcomm 7
            FLEXFRG7CTRL : aliased FLEXFRG7CTRL_Register;
         when Xctrl7 =>
            --  Peripheral reset control register
            FLEXFRGXCTRL7 : aliased HAL.UInt32;
      end case;
   end record
     with Unchecked_Union, Volatile;

   for SYSCON_Peripheral use record
      MEMORYREMAP           at 16#0# range 0 .. 31;
      AHBMATPRIO            at 16#10# range 0 .. 31;
      CPU0STCKCAL           at 16#38# range 0 .. 31;
      CPU0NSTCKCAL          at 16#3C# range 0 .. 31;
      CPU1STCKCAL           at 16#40# range 0 .. 31;
      NMISRC                at 16#48# range 0 .. 31;
      PRESETCTRLSET         at 16#120# range 0 .. 95;
      PRESETCTRLCLR         at 16#140# range 0 .. 95;
      SWR_RESET             at 16#160# range 0 .. 31;
      AHBCLKCTRLSET         at 16#220# range 0 .. 95;
      AHBCLKCTRLCLR         at 16#240# range 0 .. 95;
      TRACECLKSEL           at 16#268# range 0 .. 31;
      MAINCLKSELA           at 16#280# range 0 .. 31;
      MAINCLKSELB           at 16#284# range 0 .. 31;
      CLKOUTSEL             at 16#288# range 0 .. 31;
      PLL0CLKSEL            at 16#290# range 0 .. 31;
      PLL1CLKSEL            at 16#294# range 0 .. 31;
      ADCCLKSEL             at 16#2A4# range 0 .. 31;
      USB0CLKSEL            at 16#2A8# range 0 .. 31;
      HSLSPICLKSEL          at 16#2D0# range 0 .. 31;
      MCLKCLKSEL            at 16#2E0# range 0 .. 31;
      SCTCLKSEL             at 16#2F0# range 0 .. 31;
      SDIOCLKSEL            at 16#2F8# range 0 .. 31;
      SYSTICKCLKDIV0        at 16#300# range 0 .. 31;
      SYSTICKCLKDIV1        at 16#304# range 0 .. 31;
      TRACECLKDIV           at 16#308# range 0 .. 31;
      AHBCLKDIV             at 16#380# range 0 .. 31;
      CLKOUTDIV             at 16#384# range 0 .. 31;
      FROHFDIV              at 16#388# range 0 .. 31;
      WDTCLKDIV             at 16#38C# range 0 .. 31;
      ADCCLKDIV             at 16#394# range 0 .. 31;
      USB0CLKDIV            at 16#398# range 0 .. 31;
      MCLKDIV               at 16#3AC# range 0 .. 31;
      SCTCLKDIV             at 16#3B4# range 0 .. 31;
      SDIOCLKDIV            at 16#3BC# range 0 .. 31;
      PLL0CLKDIV            at 16#3C4# range 0 .. 31;
      CLOCKGENUPDATELOCKOUT at 16#3FC# range 0 .. 31;
      FMCCR                 at 16#400# range 0 .. 31;
      USB0NEEDCLKCTRL       at 16#40C# range 0 .. 31;
      USB0NEEDCLKSTAT       at 16#410# range 0 .. 31;
      FMCFLUSH              at 16#41C# range 0 .. 31;
      MCLKIO                at 16#420# range 0 .. 31;
      USB1NEEDCLKCTRL       at 16#424# range 0 .. 31;
      USB1NEEDCLKSTAT       at 16#428# range 0 .. 31;
      SDIOCLKCTRL           at 16#460# range 0 .. 31;
      PLL1CTRL              at 16#560# range 0 .. 31;
      PLL1STAT              at 16#564# range 0 .. 31;
      PLL1NDEC              at 16#568# range 0 .. 31;
      PLL1MDEC              at 16#56C# range 0 .. 31;
      PLL1PDEC              at 16#570# range 0 .. 31;
      PLL0CTRL              at 16#580# range 0 .. 31;
      PLL0STAT              at 16#584# range 0 .. 31;
      PLL0NDEC              at 16#588# range 0 .. 31;
      PLL0PDEC              at 16#58C# range 0 .. 31;
      PLL0SSCG0             at 16#590# range 0 .. 31;
      PLL0SSCG1             at 16#594# range 0 .. 31;
      CPUCTRL               at 16#800# range 0 .. 31;
      CPBOOT                at 16#804# range 0 .. 31;
      CPSTAT                at 16#80C# range 0 .. 31;
      CLOCK_CTRL            at 16#A18# range 0 .. 31;
      COMP_INT_CTRL         at 16#B10# range 0 .. 31;
      COMP_INT_STATUS       at 16#B14# range 0 .. 31;
      AUTOCLKGATEOVERRIDE   at 16#E04# range 0 .. 31;
      GPIOPSYNC             at 16#E08# range 0 .. 31;
      DEBUG_LOCK_EN         at 16#FA0# range 0 .. 31;
      DEBUG_FEATURES        at 16#FA4# range 0 .. 31;
      DEBUG_FEATURES_DP     at 16#FA8# range 0 .. 31;
      KEY_BLOCK             at 16#FBC# range 0 .. 31;
      DEBUG_AUTH_BEACON     at 16#FC0# range 0 .. 31;
      CPUCFG                at 16#FD4# range 0 .. 31;
      DEVICE_ID0            at 16#FF8# range 0 .. 31;
      DIEID                 at 16#FFC# range 0 .. 31;
      PRESETCTRL0           at 16#100# range 0 .. 31;
      AHBCLKCTRL0           at 16#200# range 0 .. 31;
      SYSTICKCLKSEL0        at 16#260# range 0 .. 31;
      CTIMERCLKSEL0         at 16#26C# range 0 .. 31;
      FCCLKSEL0             at 16#2B0# range 0 .. 31;
      PRESETCTRLX0          at 16#100# range 0 .. 31;
      AHBCLKCTRLX0          at 16#200# range 0 .. 31;
      SYSTICKCLKSELX0       at 16#260# range 0 .. 31;
      CTIMERCLKSELX0        at 16#26C# range 0 .. 31;
      FCCLKSELX0            at 16#2B0# range 0 .. 31;
      PRESETCTRL1           at 16#104# range 0 .. 31;
      AHBCLKCTRL1           at 16#204# range 0 .. 31;
      SYSTICKCLKSEL1        at 16#264# range 0 .. 31;
      CTIMERCLKSEL1         at 16#270# range 0 .. 31;
      FCCLKSEL1             at 16#2B4# range 0 .. 31;
      PRESETCTRLX1          at 16#104# range 0 .. 31;
      AHBCLKCTRLX1          at 16#204# range 0 .. 31;
      SYSTICKCLKSELX1       at 16#264# range 0 .. 31;
      CTIMERCLKSELX1        at 16#270# range 0 .. 31;
      FCCLKSELX1            at 16#2B4# range 0 .. 31;
      PRESETCTRL2           at 16#108# range 0 .. 31;
      AHBCLKCTRL2           at 16#208# range 0 .. 31;
      CTIMERCLKSEL2         at 16#274# range 0 .. 31;
      FCCLKSEL2             at 16#2B8# range 0 .. 31;
      PRESETCTRLX2          at 16#108# range 0 .. 31;
      AHBCLKCTRLX2          at 16#208# range 0 .. 31;
      CTIMERCLKSELX2        at 16#274# range 0 .. 31;
      FCCLKSELX2            at 16#2B8# range 0 .. 31;
      CTIMERCLKSEL3         at 16#278# range 0 .. 31;
      FCCLKSEL3             at 16#2BC# range 0 .. 31;
      CTIMERCLKSELX3        at 16#278# range 0 .. 31;
      FCCLKSELX3            at 16#2BC# range 0 .. 31;
      CTIMERCLKSEL4         at 16#27C# range 0 .. 31;
      FCCLKSEL4             at 16#2C0# range 0 .. 31;
      CTIMERCLKSELX4        at 16#27C# range 0 .. 31;
      FCCLKSELX4            at 16#2C0# range 0 .. 31;
      FCCLKSEL5             at 16#2C4# range 0 .. 31;
      FCCLKSELX5            at 16#2C4# range 0 .. 31;
      FCCLKSEL6             at 16#2C8# range 0 .. 31;
      FCCLKSELX6            at 16#2C8# range 0 .. 31;
      FCCLKSEL7             at 16#2CC# range 0 .. 31;
      FCCLKSELX7            at 16#2CC# range 0 .. 31;
      FLEXFRG0CTRL          at 16#320# range 0 .. 31;
      FLEXFRGXCTRL0         at 16#320# range 0 .. 31;
      FLEXFRG1CTRL          at 16#324# range 0 .. 31;
      FLEXFRGXCTRL1         at 16#324# range 0 .. 31;
      FLEXFRG2CTRL          at 16#328# range 0 .. 31;
      FLEXFRGXCTRL2         at 16#328# range 0 .. 31;
      FLEXFRG3CTRL          at 16#32C# range 0 .. 31;
      FLEXFRGXCTRL3         at 16#32C# range 0 .. 31;
      FLEXFRG4CTRL          at 16#330# range 0 .. 31;
      FLEXFRGXCTRL4         at 16#330# range 0 .. 31;
      FLEXFRG5CTRL          at 16#334# range 0 .. 31;
      FLEXFRGXCTRL5         at 16#334# range 0 .. 31;
      FLEXFRG6CTRL          at 16#338# range 0 .. 31;
      FLEXFRGXCTRL6         at 16#338# range 0 .. 31;
      FLEXFRG7CTRL          at 16#33C# range 0 .. 31;
      FLEXFRGXCTRL7         at 16#33C# range 0 .. 31;
   end record;

   --  SYSCON
   SYSCON_Periph : aliased SYSCON_Peripheral
     with Import, Address => System'To_Address (16#40000000#);

end NXP_SVD.SYSCON;
