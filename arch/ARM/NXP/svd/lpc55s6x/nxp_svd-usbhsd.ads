--  Copyright 2016-2019 NXP
--  All rights reserved.SPDX-License-Identifier: BSD-3-Clause


--  This spec has been automatically generated from LPC55S6x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NXP_SVD.USBHSD is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype DEVCMDSTAT_DEV_ADDR_Field is HAL.UInt7;
   subtype DEVCMDSTAT_Speed_Field is HAL.UInt2;
   subtype DEVCMDSTAT_PHY_TEST_MODE_Field is HAL.UInt3;

   --  USB Device Command/Status register
   type DEVCMDSTAT_Register is record
      --  USB device address.
      DEV_ADDR       : DEVCMDSTAT_DEV_ADDR_Field := 16#0#;
      --  USB device enable.
      DEV_EN         : Boolean := False;
      --  SETUP token received.
      SETUP          : Boolean := False;
      --  Forces the NEEDCLK output to always be on:.
      FORCE_NEEDCLK  : Boolean := False;
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  LPM Supported:.
      LPM_SUP        : Boolean := True;
      --  Interrupt on NAK for interrupt and bulk OUT EP:.
      INTONNAK_AO    : Boolean := False;
      --  Interrupt on NAK for interrupt and bulk IN EP:.
      INTONNAK_AI    : Boolean := False;
      --  Interrupt on NAK for control OUT EP:.
      INTONNAK_CO    : Boolean := False;
      --  Interrupt on NAK for control IN EP:.
      INTONNAK_CI    : Boolean := False;
      --  Device status - connect.
      DCON           : Boolean := False;
      --  Device status - suspend.
      DSUS           : Boolean := False;
      --  unspecified
      Reserved_18_18 : HAL.Bit := 16#0#;
      --  Device status - LPM Suspend.
      LPM_SUS        : Boolean := False;
      --  Read-only. LPM Remote Wake-up Enabled by USB host.
      LPM_REWP       : Boolean := False;
      --  unspecified
      Reserved_21_21 : HAL.Bit := 16#0#;
      --  Read-only. This field indicates the speed at which the device
      --  operates: 00b: reserved 01b: full-speed 10b: high-speed 11b:
      --  super-speed (reserved for future use).
      Speed          : DEVCMDSTAT_Speed_Field := 16#0#;
      --  Device status - connect change.
      DCON_C         : Boolean := False;
      --  Device status - suspend change.
      DSUS_C         : Boolean := False;
      --  Device status - reset change.
      DRES_C         : Boolean := False;
      --  unspecified
      Reserved_27_27 : HAL.Bit := 16#0#;
      --  Read-only. This bit indicates if VBUS is detected or not.
      VBUS_DEBOUNCED : Boolean := False;
      --  This field is written by firmware to put the PHY into a test mode as
      --  defined by the USB2.0 specification
      PHY_TEST_MODE  : DEVCMDSTAT_PHY_TEST_MODE_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DEVCMDSTAT_Register use record
      DEV_ADDR       at 0 range 0 .. 6;
      DEV_EN         at 0 range 7 .. 7;
      SETUP          at 0 range 8 .. 8;
      FORCE_NEEDCLK  at 0 range 9 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      LPM_SUP        at 0 range 11 .. 11;
      INTONNAK_AO    at 0 range 12 .. 12;
      INTONNAK_AI    at 0 range 13 .. 13;
      INTONNAK_CO    at 0 range 14 .. 14;
      INTONNAK_CI    at 0 range 15 .. 15;
      DCON           at 0 range 16 .. 16;
      DSUS           at 0 range 17 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      LPM_SUS        at 0 range 19 .. 19;
      LPM_REWP       at 0 range 20 .. 20;
      Reserved_21_21 at 0 range 21 .. 21;
      Speed          at 0 range 22 .. 23;
      DCON_C         at 0 range 24 .. 24;
      DSUS_C         at 0 range 25 .. 25;
      DRES_C         at 0 range 26 .. 26;
      Reserved_27_27 at 0 range 27 .. 27;
      VBUS_DEBOUNCED at 0 range 28 .. 28;
      PHY_TEST_MODE  at 0 range 29 .. 31;
   end record;

   subtype INFO_FRAME_NR_Field is HAL.UInt11;
   subtype INFO_ERR_CODE_Field is HAL.UInt4;
   subtype INFO_MINREV_Field is HAL.UInt8;
   subtype INFO_MAJREV_Field is HAL.UInt8;

   --  USB Info register
   type INFO_Register is record
      --  Read-only. Frame number.
      FRAME_NR       : INFO_FRAME_NR_Field;
      --  Read-only. The error code which last occurred:.
      ERR_CODE       : INFO_ERR_CODE_Field;
      --  unspecified
      Reserved_15_15 : HAL.Bit;
      --  Read-only. Minor revision.
      MINREV         : INFO_MINREV_Field;
      --  Read-only. Major revision.
      MAJREV         : INFO_MAJREV_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INFO_Register use record
      FRAME_NR       at 0 range 0 .. 10;
      ERR_CODE       at 0 range 11 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      MINREV         at 0 range 16 .. 23;
      MAJREV         at 0 range 24 .. 31;
   end record;

   subtype EPLISTSTART_EP_LIST_PRG_Field is HAL.UInt12;
   subtype EPLISTSTART_EP_LIST_FIXED_Field is HAL.UInt12;

   --  USB EP Command/Status List start address
   type EPLISTSTART_Register is record
      --  unspecified
      Reserved_0_7  : HAL.UInt8 := 16#0#;
      --  Programmable portion of the USB EP Command/Status List address.
      EP_LIST_PRG   : EPLISTSTART_EP_LIST_PRG_Field := 16#0#;
      --  Read-only. Fixed portion of USB EP Command/Status List address.
      EP_LIST_FIXED : EPLISTSTART_EP_LIST_FIXED_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EPLISTSTART_Register use record
      Reserved_0_7  at 0 range 0 .. 7;
      EP_LIST_PRG   at 0 range 8 .. 19;
      EP_LIST_FIXED at 0 range 20 .. 31;
   end record;

   subtype LPM_HIRD_HW_Field is HAL.UInt4;
   subtype LPM_HIRD_SW_Field is HAL.UInt4;

   --  USB Link Power Management register
   type LPM_Register is record
      --  Read-only. Host Initiated Resume Duration - HW.
      HIRD_HW       : LPM_HIRD_HW_Field := 16#0#;
      --  Host Initiated Resume Duration - SW.
      HIRD_SW       : LPM_HIRD_SW_Field := 16#0#;
      --  As long as this bit is set to one and LPM supported bit is set to
      --  one, HW will return a NYET handshake on every LPM token it receives.
      DATA_PENDING  : Boolean := False;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPM_Register use record
      HIRD_HW       at 0 range 0 .. 3;
      HIRD_SW       at 0 range 4 .. 7;
      DATA_PENDING  at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   subtype EPSKIP_SKIP_Field is HAL.UInt12;

   --  USB Endpoint skip
   type EPSKIP_Register is record
      --  Endpoint skip: Writing 1 to one of these bits, will indicate to HW
      --  that it must deactivate the buffer assigned to this endpoint and
      --  return control back to software.
      SKIP           : EPSKIP_SKIP_Field := 16#0#;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EPSKIP_Register use record
      SKIP           at 0 range 0 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype EPINUSE_BUF_Field is HAL.UInt10;

   --  USB Endpoint Buffer in use
   type EPINUSE_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  Buffer in use: This register has one bit per physical endpoint.
      BUF            : EPINUSE_BUF_Field := 16#0#;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EPINUSE_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      BUF            at 0 range 2 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype EPBUFCFG_BUF_SB_Field is HAL.UInt10;

   --  USB Endpoint Buffer Configuration register
   type EPBUFCFG_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  Buffer usage: This register has one bit per physical endpoint.
      BUF_SB         : EPBUFCFG_BUF_SB_Field := 16#0#;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EPBUFCFG_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      BUF_SB         at 0 range 2 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   --  USB interrupt status register
   type INTSTAT_Register is record
      --  Interrupt status register bit for the Control EP0 OUT direction.
      EP0OUT         : Boolean := False;
      --  Interrupt status register bit for the Control EP0 IN direction.
      EP0IN          : Boolean := False;
      --  Interrupt status register bit for the EP1 OUT direction.
      EP1OUT         : Boolean := False;
      --  Interrupt status register bit for the EP1 IN direction.
      EP1IN          : Boolean := False;
      --  Interrupt status register bit for the EP2 OUT direction.
      EP2OUT         : Boolean := False;
      --  Interrupt status register bit for the EP2 IN direction.
      EP2IN          : Boolean := False;
      --  Interrupt status register bit for the EP3 OUT direction.
      EP3OUT         : Boolean := False;
      --  Interrupt status register bit for the EP3 IN direction.
      EP3IN          : Boolean := False;
      --  Interrupt status register bit for the EP4 OUT direction.
      EP4OUT         : Boolean := False;
      --  Interrupt status register bit for the EP4 IN direction.
      EP4IN          : Boolean := False;
      --  Interrupt status register bit for the EP5 OUT direction.
      EP5OUT         : Boolean := False;
      --  Interrupt status register bit for the EP5 IN direction.
      EP5IN          : Boolean := False;
      --  unspecified
      Reserved_12_29 : HAL.UInt18 := 16#0#;
      --  Frame interrupt.
      FRAME_INT      : Boolean := False;
      --  Device status interrupt.
      DEV_INT        : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTSTAT_Register use record
      EP0OUT         at 0 range 0 .. 0;
      EP0IN          at 0 range 1 .. 1;
      EP1OUT         at 0 range 2 .. 2;
      EP1IN          at 0 range 3 .. 3;
      EP2OUT         at 0 range 4 .. 4;
      EP2IN          at 0 range 5 .. 5;
      EP3OUT         at 0 range 6 .. 6;
      EP3IN          at 0 range 7 .. 7;
      EP4OUT         at 0 range 8 .. 8;
      EP4IN          at 0 range 9 .. 9;
      EP5OUT         at 0 range 10 .. 10;
      EP5IN          at 0 range 11 .. 11;
      Reserved_12_29 at 0 range 12 .. 29;
      FRAME_INT      at 0 range 30 .. 30;
      DEV_INT        at 0 range 31 .. 31;
   end record;

   subtype INTEN_EP_INT_EN_Field is HAL.UInt12;

   --  USB interrupt enable register
   type INTEN_Register is record
      --  If this bit is set and the corresponding USB interrupt status bit is
      --  set, a HW interrupt is generated on the interrupt line.
      EP_INT_EN      : INTEN_EP_INT_EN_Field := 16#0#;
      --  unspecified
      Reserved_12_29 : HAL.UInt18 := 16#0#;
      --  If this bit is set and the corresponding USB interrupt status bit is
      --  set, a HW interrupt is generated on the interrupt line.
      FRAME_INT_EN   : Boolean := False;
      --  If this bit is set and the corresponding USB interrupt status bit is
      --  set, a HW interrupt is generated on the interrupt line.
      DEV_INT_EN     : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTEN_Register use record
      EP_INT_EN      at 0 range 0 .. 11;
      Reserved_12_29 at 0 range 12 .. 29;
      FRAME_INT_EN   at 0 range 30 .. 30;
      DEV_INT_EN     at 0 range 31 .. 31;
   end record;

   subtype INTSETSTAT_EP_SET_INT_Field is HAL.UInt12;

   --  USB set interrupt status register
   type INTSETSTAT_Register is record
      --  If software writes a one to one of these bits, the corresponding USB
      --  interrupt status bit is set.
      EP_SET_INT     : INTSETSTAT_EP_SET_INT_Field := 16#0#;
      --  unspecified
      Reserved_12_29 : HAL.UInt18 := 16#0#;
      --  If software writes a one to one of these bits, the corresponding USB
      --  interrupt status bit is set.
      FRAME_SET_INT  : Boolean := False;
      --  If software writes a one to one of these bits, the corresponding USB
      --  interrupt status bit is set.
      DEV_SET_INT    : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTSETSTAT_Register use record
      EP_SET_INT     at 0 range 0 .. 11;
      Reserved_12_29 at 0 range 12 .. 29;
      FRAME_SET_INT  at 0 range 30 .. 30;
      DEV_SET_INT    at 0 range 31 .. 31;
   end record;

   subtype EPTOGGLE_TOGGLE_Field is HAL.UInt30;

   --  USB Endpoint toggle register
   type EPTOGGLE_Register is record
      --  Read-only. Endpoint data toggle: This field indicates the current
      --  value of the data toggle for the corresponding endpoint.
      TOGGLE         : EPTOGGLE_TOGGLE_Field;
      --  unspecified
      Reserved_30_31 : HAL.UInt2;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EPTOGGLE_Register use record
      TOGGLE         at 0 range 0 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  USB1 High-speed Device Controller
   type USBHSD_Peripheral is record
      --  USB Device Command/Status register
      DEVCMDSTAT   : aliased DEVCMDSTAT_Register;
      --  USB Info register
      INFO         : aliased INFO_Register;
      --  USB EP Command/Status List start address
      EPLISTSTART  : aliased EPLISTSTART_Register;
      --  USB Data buffer start address
      DATABUFSTART : aliased HAL.UInt32;
      --  USB Link Power Management register
      LPM          : aliased LPM_Register;
      --  USB Endpoint skip
      EPSKIP       : aliased EPSKIP_Register;
      --  USB Endpoint Buffer in use
      EPINUSE      : aliased EPINUSE_Register;
      --  USB Endpoint Buffer Configuration register
      EPBUFCFG     : aliased EPBUFCFG_Register;
      --  USB interrupt status register
      INTSTAT      : aliased INTSTAT_Register;
      --  USB interrupt enable register
      INTEN        : aliased INTEN_Register;
      --  USB set interrupt status register
      INTSETSTAT   : aliased INTSETSTAT_Register;
      --  USB Endpoint toggle register
      EPTOGGLE     : aliased EPTOGGLE_Register;
   end record
     with Volatile;

   for USBHSD_Peripheral use record
      DEVCMDSTAT   at 16#0# range 0 .. 31;
      INFO         at 16#4# range 0 .. 31;
      EPLISTSTART  at 16#8# range 0 .. 31;
      DATABUFSTART at 16#C# range 0 .. 31;
      LPM          at 16#10# range 0 .. 31;
      EPSKIP       at 16#14# range 0 .. 31;
      EPINUSE      at 16#18# range 0 .. 31;
      EPBUFCFG     at 16#1C# range 0 .. 31;
      INTSTAT      at 16#20# range 0 .. 31;
      INTEN        at 16#24# range 0 .. 31;
      INTSETSTAT   at 16#28# range 0 .. 31;
      EPTOGGLE     at 16#34# range 0 .. 31;
   end record;

   --  USB1 High-speed Device Controller
   USBHSD_Periph : aliased USBHSD_Peripheral
     with Import, Address => System'To_Address (16#40094000#);

end NXP_SVD.USBHSD;
