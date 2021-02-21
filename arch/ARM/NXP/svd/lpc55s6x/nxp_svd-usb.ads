--  Copyright 2016-2019 NXP
--  All rights reserved.SPDX-License-Identifier: BSD-3-Clause
--  This spec has been automatically generated from LPC55S6x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NXP_SVD.USB is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype DEVCMDSTAT_DEV_ADDR_Field is HAL.UInt7;

   --  Forces the NEEDCLK output to always be on:
   type DEVCMDSTAT_FORCE_NEEDCLK_Field is
     (
      --  USB_NEEDCLK has normal function.
      Normal,
      --  USB_NEEDCLK always 1. Clock will not be stopped in case of suspend.
      Always_On)
     with Size => 1;
   for DEVCMDSTAT_FORCE_NEEDCLK_Field use
     (Normal => 0,
      Always_On => 1);

   --  LPM Supported:
   type DEVCMDSTAT_LPM_SUP_Field is
     (
      --  LPM not supported.
      No,
      --  LPM supported.
      Yes)
     with Size => 1;
   for DEVCMDSTAT_LPM_SUP_Field use
     (No => 0,
      Yes => 1);

   --  Interrupt on NAK for interrupt and bulk OUT EP
   type DEVCMDSTAT_INTONNAK_AO_Field is
     (
      --  Only acknowledged packets generate an interrupt
      Disabled,
      --  Both acknowledged and NAKed packets generate interrupts.
      Enabled)
     with Size => 1;
   for DEVCMDSTAT_INTONNAK_AO_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Interrupt on NAK for interrupt and bulk IN EP
   type DEVCMDSTAT_INTONNAK_AI_Field is
     (
      --  Only acknowledged packets generate an interrupt
      Disabled,
      --  Both acknowledged and NAKed packets generate interrupts.
      Enabled)
     with Size => 1;
   for DEVCMDSTAT_INTONNAK_AI_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Interrupt on NAK for control OUT EP
   type DEVCMDSTAT_INTONNAK_CO_Field is
     (
      --  Only acknowledged packets generate an interrupt
      Disabled,
      --  Both acknowledged and NAKed packets generate interrupts.
      Enabled)
     with Size => 1;
   for DEVCMDSTAT_INTONNAK_CO_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Interrupt on NAK for control IN EP
   type DEVCMDSTAT_INTONNAK_CI_Field is
     (
      --  Only acknowledged packets generate an interrupt
      Disabled,
      --  Both acknowledged and NAKed packets generate interrupts.
      Enabled)
     with Size => 1;
   for DEVCMDSTAT_INTONNAK_CI_Field use
     (Disabled => 0,
      Enabled => 1);

   --  USB Device Command/Status register
   type DEVCMDSTAT_Register is record
      --  USB device address. After bus reset, the address is reset to 0x00. If
      --  the enable bit is set, the device will respond on packets for
      --  function address DEV_ADDR. When receiving a SetAddress Control
      --  Request from the USB host, software must program the new address
      --  before completing the status phase of the SetAddress Control Request.
      DEV_ADDR       : DEVCMDSTAT_DEV_ADDR_Field := 16#0#;
      --  USB device enable. If this bit is set, the HW will start responding
      --  on packets for function address DEV_ADDR.
      DEV_EN         : Boolean := False;
      --  SETUP token received. If a SETUP token is received and acknowledged
      --  by the device, this bit is set. As long as this bit is set all
      --  received IN and OUT tokens will be NAKed by HW. SW must clear this
      --  bit by writing a one. If this bit is zero, HW will handle the tokens
      --  to the CTRL EP0 as indicated by the CTRL EP0 IN and OUT data
      --  information programmed by SW.
      SETUP          : Boolean := False;
      --  Forces the NEEDCLK output to always be on:
      FORCE_NEEDCLK  : DEVCMDSTAT_FORCE_NEEDCLK_Field := NXP_SVD.USB.Normal;
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  LPM Supported:
      LPM_SUP        : DEVCMDSTAT_LPM_SUP_Field := NXP_SVD.USB.Yes;
      --  Interrupt on NAK for interrupt and bulk OUT EP
      INTONNAK_AO    : DEVCMDSTAT_INTONNAK_AO_Field := NXP_SVD.USB.Disabled;
      --  Interrupt on NAK for interrupt and bulk IN EP
      INTONNAK_AI    : DEVCMDSTAT_INTONNAK_AI_Field := NXP_SVD.USB.Disabled;
      --  Interrupt on NAK for control OUT EP
      INTONNAK_CO    : DEVCMDSTAT_INTONNAK_CO_Field := NXP_SVD.USB.Disabled;
      --  Interrupt on NAK for control IN EP
      INTONNAK_CI    : DEVCMDSTAT_INTONNAK_CI_Field := NXP_SVD.USB.Disabled;
      --  Device status - connect. The connect bit must be set by SW to
      --  indicate that the device must signal a connect. The pull-up resistor
      --  on USB_DP will be enabled when this bit is set and the VBUSDEBOUNCED
      --  bit is one.
      DCON           : Boolean := False;
      --  Device status - suspend. The suspend bit indicates the current
      --  suspend state. It is set to 1 when the device hasn't seen any
      --  activity on its upstream port for more than 3 milliseconds. It is
      --  reset to 0 on any activity. When the device is suspended (Suspend bit
      --  DSUS = 1) and the software writes a 0 to it, the device will generate
      --  a remote wake-up. This will only happen when the device is connected
      --  (Connect bit = 1). When the device is not connected or not suspended,
      --  a writing a 0 has no effect. Writing a 1 never has an effect.
      DSUS           : Boolean := False;
      --  unspecified
      Reserved_18_18 : HAL.Bit := 16#0#;
      --  Device status - LPM Suspend. This bit represents the current LPM
      --  suspend state. It is set to 1 by HW when the device has acknowledged
      --  the LPM request from the USB host and the Token Retry Time of 10 ms
      --  has elapsed. When the device is in the LPM suspended state (LPM
      --  suspend bit = 1) and the software writes a zero to this bit, the
      --  device will generate a remote walk-up. Software can only write a zero
      --  to this bit when the LPM_REWP bit is set to 1. HW resets this bit
      --  when it receives a host initiated resume. HW only updates the LPM_SUS
      --  bit when the LPM_SUPP bit is equal to one.
      LPM_SUS        : Boolean := False;
      --  Read-only. LPM Remote Wake-up Enabled by USB host. HW sets this bit
      --  to one when the bRemoteWake bit in the LPM extended token is set to
      --  1. HW will reset this bit to 0 when it receives the host initiated
      --  LPM resume, when a remote wake-up is sent by the device or when a USB
      --  bus reset is received. Software can use this bit to check if the
      --  remote wake-up feature is enabled by the host for the LPM
      --  transaction.
      LPM_REWP       : Boolean := False;
      --  unspecified
      Reserved_21_23 : HAL.UInt3 := 16#0#;
      --  Device status - connect change. The Connect Change bit is set when
      --  the device's pull-up resistor is disconnected because VBus
      --  disappeared. The bit is reset by writing a one to it.
      DCON_C         : Boolean := False;
      --  Device status - suspend change. The suspend change bit is set to 1
      --  when the suspend bit toggles. The suspend bit can toggle because: -
      --  The device goes in the suspended state - The device is disconnected -
      --  The device receives resume signaling on its upstream port. The bit is
      --  reset by writing a one to it.
      DSUS_C         : Boolean := False;
      --  Device status - reset change. This bit is set when the device
      --  received a bus reset. On a bus reset the device will automatically go
      --  to the default state (unconfigured and responding to address 0). The
      --  bit is reset by writing a one to it.
      DRES_C         : Boolean := False;
      --  unspecified
      Reserved_27_27 : HAL.Bit := 16#0#;
      --  Read-only. This bit indicates if Vbus is detected or not. The bit
      --  raises immediately when Vbus becomes high. It drops to zero if Vbus
      --  is low for at least 3 ms. If this bit is high and the DCon bit is
      --  set, the HW will enable the pull-up resistor to signal a connect.
      VBUSDEBOUNCED  : Boolean := False;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
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
      Reserved_21_23 at 0 range 21 .. 23;
      DCON_C         at 0 range 24 .. 24;
      DSUS_C         at 0 range 25 .. 25;
      DRES_C         at 0 range 26 .. 26;
      Reserved_27_27 at 0 range 27 .. 27;
      VBUSDEBOUNCED  at 0 range 28 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype INFO_FRAME_NR_Field is HAL.UInt11;

   --  The error code which last occurred:
   type INFO_ERR_CODE_Field is
     (
      --  No error
      No_Error,
      --  PID encoding error
      Pid_Encoding_Error,
      --  PID unknown
      Pid_Unknown,
      --  Packet unexpected
      Packet_Unexpected,
      --  Token CRC error
      Token_Crc_Error,
      --  Data CRC error
      Data_Crc_Error,
      --  Time out
      Timeout,
      --  Babble
      Babble,
      --  Truncated EOP
      Truncated_Eop,
      --  Sent/Received NAK
      Sent_Received_Nak,
      --  Sent Stall
      Sent_Stall,
      --  Overrun
      Overrun,
      --  Sent empty packet
      Sent_Empty_Packet,
      --  Bitstuff error
      Bitstuff_Error,
      --  Sync error
      Sync_Error,
      --  Wrong data toggle
      Wrong_Data_Toggle)
     with Size => 4;
   for INFO_ERR_CODE_Field use
     (No_Error => 0,
      Pid_Encoding_Error => 1,
      Pid_Unknown => 2,
      Packet_Unexpected => 3,
      Token_Crc_Error => 4,
      Data_Crc_Error => 5,
      Timeout => 6,
      Babble => 7,
      Truncated_Eop => 8,
      Sent_Received_Nak => 9,
      Sent_Stall => 10,
      Overrun => 11,
      Sent_Empty_Packet => 12,
      Bitstuff_Error => 13,
      Sync_Error => 14,
      Wrong_Data_Toggle => 15);

   subtype INFO_MINREV_Field is HAL.UInt8;
   subtype INFO_MAJREV_Field is HAL.UInt8;

   --  USB Info register
   type INFO_Register is record
      --  Read-only. Frame number. This contains the frame number of the last
      --  successfully received SOF. In case no SOF was received by the device
      --  at the beginning of a frame, the frame number returned is that of the
      --  last successfully received SOF. In case the SOF frame number
      --  contained a CRC error, the frame number returned will be the
      --  corrupted frame number as received by the device.
      FRAME_NR       : INFO_FRAME_NR_Field := 16#0#;
      --  The error code which last occurred:
      ERR_CODE       : INFO_ERR_CODE_Field := NXP_SVD.USB.No_Error;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Read-only. Minor Revision.
      MINREV         : INFO_MINREV_Field := 16#0#;
      --  Read-only. Major Revision.
      MAJREV         : INFO_MAJREV_Field := 16#0#;
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

   subtype EPLISTSTART_EP_LIST_Field is HAL.UInt24;

   --  USB EP Command/Status List start address
   type EPLISTSTART_Register is record
      --  unspecified
      Reserved_0_7 : HAL.UInt8 := 16#0#;
      --  Start address of the USB EP Command/Status List.
      EP_LIST      : EPLISTSTART_EP_LIST_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EPLISTSTART_Register use record
      Reserved_0_7 at 0 range 0 .. 7;
      EP_LIST      at 0 range 8 .. 31;
   end record;

   subtype DATABUFSTART_DA_BUF_Field is HAL.UInt10;

   --  USB Data buffer start address
   type DATABUFSTART_Register is record
      --  unspecified
      Reserved_0_21 : HAL.UInt22 := 16#0#;
      --  Start address of the buffer pointer page where all endpoint data
      --  buffers are located.
      DA_BUF        : DATABUFSTART_DA_BUF_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DATABUFSTART_Register use record
      Reserved_0_21 at 0 range 0 .. 21;
      DA_BUF        at 0 range 22 .. 31;
   end record;

   subtype LPM_HIRD_HW_Field is HAL.UInt4;
   subtype LPM_HIRD_SW_Field is HAL.UInt4;

   --  USB Link Power Management register
   type LPM_Register is record
      --  Read-only. Host Initiated Resume Duration - HW. This is the HIRD
      --  value from the last received LPM token
      HIRD_HW       : LPM_HIRD_HW_Field := 16#0#;
      --  Host Initiated Resume Duration - SW. This is the time duration
      --  required by the USB device system to come out of LPM initiated
      --  suspend after receiving the host initiated LPM resume.
      HIRD_SW       : LPM_HIRD_SW_Field := 16#0#;
      --  As long as this bit is set to one and LPM supported bit is set to
      --  one, HW will return a NYET handshake on every LPM token it receives.
      --  If LPM supported bit is set to one and this bit is zero, HW will
      --  return an ACK handshake on every LPM token it receives. If SW has
      --  still data pending and LPM is supported, it must set this bit to 1.
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

   subtype EPSKIP_SKIP_Field is HAL.UInt10;

   --  USB Endpoint skip
   type EPSKIP_Register is record
      --  Endpoint skip: Writing 1 to one of these bits, will indicate to HW
      --  that it must deactivate the buffer assigned to this endpoint and
      --  return control back to software. When HW has deactivated the
      --  endpoint, it will clear this bit, but it will not modify the EPINUSE
      --  bit. An interrupt will be generated when the Active bit goes from 1
      --  to 0. Note: In case of double-buffering, HW will only clear the
      --  Active bit of the buffer indicated by the EPINUSE bit.
      SKIP           : EPSKIP_SKIP_Field := 16#0#;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EPSKIP_Register use record
      SKIP           at 0 range 0 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype EPINUSE_BUF_Field is HAL.UInt8;

   --  USB Endpoint Buffer in use
   type EPINUSE_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  Buffer in use: This register has one bit per physical endpoint. 0: HW
      --  is accessing buffer 0. 1: HW is accessing buffer 1.
      BUF            : EPINUSE_BUF_Field := 16#0#;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EPINUSE_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      BUF            at 0 range 2 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype EPBUFCFG_BUF_SB_Field is HAL.UInt8;

   --  USB Endpoint Buffer Configuration register
   type EPBUFCFG_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  Buffer usage: This register has one bit per physical endpoint. 0:
      --  Single-buffer. 1: Double-buffer. If the bit is set to single-buffer
      --  (0), it will not toggle the corresponding EPINUSE bit when it clears
      --  the active bit. If the bit is set to double-buffer (1), HW will
      --  toggle the EPINUSE bit when it clears the Active bit for the buffer.
      BUF_SB         : EPBUFCFG_BUF_SB_Field := 16#0#;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EPBUFCFG_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      BUF_SB         at 0 range 2 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   --  USB interrupt status register
   type INTSTAT_Register is record
      --  Interrupt status register bit for the Control EP0 OUT direction. This
      --  bit will be set if NBytes transitions to zero or the skip bit is set
      --  by software or a SETUP packet is successfully received for the
      --  control EP0. If the IntOnNAK_CO is set, this bit will also be set
      --  when a NAK is transmitted for the Control EP0 OUT direction. Software
      --  can clear this bit by writing a one to it.
      EP0OUT         : Boolean := False;
      --  Interrupt status register bit for the Control EP0 IN direction. This
      --  bit will be set if NBytes transitions to zero or the skip bit is set
      --  by software. If the IntOnNAK_CI is set, this bit will also be set
      --  when a NAK is transmitted for the Control EP0 IN direction. Software
      --  can clear this bit by writing a one to it.
      EP0IN          : Boolean := False;
      --  Interrupt status register bit for the EP1 OUT direction. This bit
      --  will be set if the corresponding Active bit is cleared by HW. This is
      --  done in case the programmed NBytes transitions to zero or the skip
      --  bit is set by software. If the IntOnNAK_AO is set, this bit will also
      --  be set when a NAK is transmitted for the EP1 OUT direction. Software
      --  can clear this bit by writing a one to it.
      EP1OUT         : Boolean := False;
      --  Interrupt status register bit for the EP1 IN direction. This bit will
      --  be set if the corresponding Active bit is cleared by HW. This is done
      --  in case the programmed NBytes transitions to zero or the skip bit is
      --  set by software. If the IntOnNAK_AI is set, this bit will also be set
      --  when a NAK is transmitted for the EP1 IN direction. Software can
      --  clear this bit by writing a one to it.
      EP1IN          : Boolean := False;
      --  Interrupt status register bit for the EP2 OUT direction. This bit
      --  will be set if the corresponding Active bit is cleared by HW. This is
      --  done in case the programmed NBytes transitions to zero or the skip
      --  bit is set by software. If the IntOnNAK_AO is set, this bit will also
      --  be set when a NAK is transmitted for the EP2 OUT direction. Software
      --  can clear this bit by writing a one to it.
      EP2OUT         : Boolean := False;
      --  Interrupt status register bit for the EP2 IN direction. This bit will
      --  be set if the corresponding Active bit is cleared by HW. This is done
      --  in case the programmed NBytes transitions to zero or the skip bit is
      --  set by software. If the IntOnNAK_AI is set, this bit will also be set
      --  when a NAK is transmitted for the EP2 IN direction. Software can
      --  clear this bit by writing a one to it.
      EP2IN          : Boolean := False;
      --  Interrupt status register bit for the EP3 OUT direction. This bit
      --  will be set if the corresponding Active bit is cleared by HW. This is
      --  done in case the programmed NBytes transitions to zero or the skip
      --  bit is set by software. If the IntOnNAK_AO is set, this bit will also
      --  be set when a NAK is transmitted for the EP3 OUT direction. Software
      --  can clear this bit by writing a one to it.
      EP3OUT         : Boolean := False;
      --  Interrupt status register bit for the EP3 IN direction. This bit will
      --  be set if the corresponding Active bit is cleared by HW. This is done
      --  in case the programmed NBytes transitions to zero or the skip bit is
      --  set by software. If the IntOnNAK_AI is set, this bit will also be set
      --  when a NAK is transmitted for the EP3 IN direction. Software can
      --  clear this bit by writing a one to it.
      EP3IN          : Boolean := False;
      --  Interrupt status register bit for the EP4 OUT direction. This bit
      --  will be set if the corresponding Active bit is cleared by HW. This is
      --  done in case the programmed NBytes transitions to zero or the skip
      --  bit is set by software. If the IntOnNAK_AO is set, this bit will also
      --  be set when a NAK is transmitted for the EP4 OUT direction. Software
      --  can clear this bit by writing a one to it.
      EP4OUT         : Boolean := False;
      --  Interrupt status register bit for the EP4 IN direction. This bit will
      --  be set if the corresponding Active bit is cleared by HW. This is done
      --  in case the programmed NBytes transitions to zero or the skip bit is
      --  set by software. If the IntOnNAK_AI is set, this bit will also be set
      --  when a NAK is transmitted for the EP4 IN direction. Software can
      --  clear this bit by writing a one to it.
      EP4IN          : Boolean := False;
      --  unspecified
      Reserved_10_29 : HAL.UInt20 := 16#0#;
      --  Frame interrupt. This bit is set to one every millisecond when the
      --  VbusDebounced bit and the DCON bit are set. This bit can be used by
      --  software when handling isochronous endpoints. Software can clear this
      --  bit by writing a one to it.
      FRAME_INT      : Boolean := False;
      --  Device status interrupt. This bit is set by HW when one of the bits
      --  in the Device Status Change register are set. Software can clear this
      --  bit by writing a one to it.
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
      Reserved_10_29 at 0 range 10 .. 29;
      FRAME_INT      at 0 range 30 .. 30;
      DEV_INT        at 0 range 31 .. 31;
   end record;

   subtype INTEN_EP_INT_EN_Field is HAL.UInt10;

   --  USB interrupt enable register
   type INTEN_Register is record
      --  If this bit is set and the corresponding USB interrupt status bit is
      --  set, a HW interrupt is generated on the interrupt line indicated by
      --  the corresponding USB interrupt routing bit.
      EP_INT_EN      : INTEN_EP_INT_EN_Field := 16#0#;
      --  unspecified
      Reserved_10_29 : HAL.UInt20 := 16#0#;
      --  If this bit is set and the corresponding USB interrupt status bit is
      --  set, a HW interrupt is generated on the interrupt line indicated by
      --  the corresponding USB interrupt routing bit.
      FRAME_INT_EN   : Boolean := False;
      --  If this bit is set and the corresponding USB interrupt status bit is
      --  set, a HW interrupt is generated on the interrupt line indicated by
      --  the corresponding USB interrupt routing bit.
      DEV_INT_EN     : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTEN_Register use record
      EP_INT_EN      at 0 range 0 .. 9;
      Reserved_10_29 at 0 range 10 .. 29;
      FRAME_INT_EN   at 0 range 30 .. 30;
      DEV_INT_EN     at 0 range 31 .. 31;
   end record;

   subtype INTSETSTAT_EP_SET_INT_Field is HAL.UInt10;

   --  USB set interrupt status register
   type INTSETSTAT_Register is record
      --  If software writes a one to one of these bits, the corresponding USB
      --  interrupt status bit is set. When this register is read, the same
      --  value as the USB interrupt status register is returned.
      EP_SET_INT     : INTSETSTAT_EP_SET_INT_Field := 16#0#;
      --  unspecified
      Reserved_10_29 : HAL.UInt20 := 16#0#;
      --  If software writes a one to one of these bits, the corresponding USB
      --  interrupt status bit is set. When this register is read, the same
      --  value as the USB interrupt status register is returned.
      FRAME_SET_INT  : Boolean := False;
      --  If software writes a one to one of these bits, the corresponding USB
      --  interrupt status bit is set. When this register is read, the same
      --  value as the USB interrupt status register is returned.
      DEV_SET_INT    : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTSETSTAT_Register use record
      EP_SET_INT     at 0 range 0 .. 9;
      Reserved_10_29 at 0 range 10 .. 29;
      FRAME_SET_INT  at 0 range 30 .. 30;
      DEV_SET_INT    at 0 range 31 .. 31;
   end record;

   subtype EPTOGGLE_TOGGLE_Field is HAL.UInt10;

   --  USB Endpoint toggle register
   type EPTOGGLE_Register is record
      --  Endpoint data toggle: This field indicates the current value of the
      --  data toggle for the corresponding endpoint.
      TOGGLE         : EPTOGGLE_TOGGLE_Field := 16#0#;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EPTOGGLE_Register use record
      TOGGLE         at 0 range 0 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  USB 2.0 Device Controller
   type USB0_Peripheral is record
      --  USB Device Command/Status register
      DEVCMDSTAT   : aliased DEVCMDSTAT_Register;
      --  USB Info register
      INFO         : aliased INFO_Register;
      --  USB EP Command/Status List start address
      EPLISTSTART  : aliased EPLISTSTART_Register;
      --  USB Data buffer start address
      DATABUFSTART : aliased DATABUFSTART_Register;
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

   for USB0_Peripheral use record
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

   --  USB 2.0 Device Controller
   USB0_Periph : aliased USB0_Peripheral
     with Import, Address => System'To_Address (16#40084000#);

end NXP_SVD.USB;
