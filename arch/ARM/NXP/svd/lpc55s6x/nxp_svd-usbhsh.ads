--  Copyright 2016-2019 NXP
--  All rights reserved.SPDX-License-Identifier: BSD-3-Clause


--  This spec has been automatically generated from LPC55S6x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NXP_SVD.USBHSH is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype CAPLENGTH_CHIPID_CAPLENGTH_Field is HAL.UInt8;
   subtype CAPLENGTH_CHIPID_CHIPID_Field is HAL.UInt16;

   --  This register contains the offset value towards the start of the
   --  operational register space and the version number of the IP block
   type CAPLENGTH_CHIPID_Register is record
      --  Read-only. Capability Length: This is used as an offset.
      CAPLENGTH     : CAPLENGTH_CHIPID_CAPLENGTH_Field;
      --  unspecified
      Reserved_8_15 : HAL.UInt8;
      --  Read-only. Chip identification: indicates major and minor revision of
      --  the IP: [31:24] = Major revision [23:16] = Minor revision Major
      --  revisions used: 0x01: USB2.
      CHIPID        : CAPLENGTH_CHIPID_CHIPID_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CAPLENGTH_CHIPID_Register use record
      CAPLENGTH     at 0 range 0 .. 7;
      Reserved_8_15 at 0 range 8 .. 15;
      CHIPID        at 0 range 16 .. 31;
   end record;

   subtype HCSPARAMS_N_PORTS_Field is HAL.UInt4;

   --  Host Controller Structural Parameters
   type HCSPARAMS_Register is record
      --  Read-only. This register specifies the number of physical downstream
      --  ports implemented on this host controller.
      N_PORTS        : HCSPARAMS_N_PORTS_Field;
      --  Read-only. This field indicates whether the host controller
      --  implementation includes port power control.
      PPC            : Boolean;
      --  unspecified
      Reserved_5_15  : HAL.UInt11;
      --  Read-only. This bit indicates whether the ports support port
      --  indicator control.
      P_INDICATOR    : Boolean;
      --  unspecified
      Reserved_17_31 : HAL.UInt15;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HCSPARAMS_Register use record
      N_PORTS        at 0 range 0 .. 3;
      PPC            at 0 range 4 .. 4;
      Reserved_5_15  at 0 range 5 .. 15;
      P_INDICATOR    at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype FLADJ_FRINDEX_FLADJ_Field is HAL.UInt6;
   subtype FLADJ_FRINDEX_FRINDEX_Field is HAL.UInt14;

   --  Frame Length Adjustment
   type FLADJ_FRINDEX_Register is record
      --  Frame Length Timing Value.
      FLADJ          : FLADJ_FRINDEX_FLADJ_Field := 16#20#;
      --  unspecified
      Reserved_6_15  : HAL.UInt10 := 16#0#;
      --  Frame Index: Bits 29 to16 in this register are used for the frame
      --  number field in the SOF packet.
      FRINDEX        : FLADJ_FRINDEX_FRINDEX_Field := 16#0#;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FLADJ_FRINDEX_Register use record
      FLADJ          at 0 range 0 .. 5;
      Reserved_6_15  at 0 range 6 .. 15;
      FRINDEX        at 0 range 16 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype ATLPTD_ATL_CUR_Field is HAL.UInt5;
   subtype ATLPTD_ATL_BASE_Field is HAL.UInt23;

   --  Memory base address where ATL PTD0 is stored
   type ATLPTD_Register is record
      --  unspecified
      Reserved_0_3 : HAL.UInt4 := 16#0#;
      --  This indicates the current PTD that is used by the hardware when it
      --  is processing the ATL list.
      ATL_CUR      : ATLPTD_ATL_CUR_Field := 16#0#;
      --  Base address to be used by the hardware to find the start of the ATL
      --  list.
      ATL_BASE     : ATLPTD_ATL_BASE_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ATLPTD_Register use record
      Reserved_0_3 at 0 range 0 .. 3;
      ATL_CUR      at 0 range 4 .. 8;
      ATL_BASE     at 0 range 9 .. 31;
   end record;

   subtype ISOPTD_ISO_FIRST_Field is HAL.UInt5;
   subtype ISOPTD_ISO_BASE_Field is HAL.UInt22;

   --  Memory base address where ISO PTD0 is stored
   type ISOPTD_Register is record
      --  unspecified
      Reserved_0_4 : HAL.UInt5 := 16#0#;
      --  This indicates the first PTD that is used by the hardware when it is
      --  processing the ISO list.
      ISO_FIRST    : ISOPTD_ISO_FIRST_Field := 16#0#;
      --  Base address to be used by the hardware to find the start of the ISO
      --  list.
      ISO_BASE     : ISOPTD_ISO_BASE_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISOPTD_Register use record
      Reserved_0_4 at 0 range 0 .. 4;
      ISO_FIRST    at 0 range 5 .. 9;
      ISO_BASE     at 0 range 10 .. 31;
   end record;

   subtype INTPTD_INT_FIRST_Field is HAL.UInt5;
   subtype INTPTD_INT_BASE_Field is HAL.UInt22;

   --  Memory base address where INT PTD0 is stored
   type INTPTD_Register is record
      --  unspecified
      Reserved_0_4 : HAL.UInt5 := 16#0#;
      --  This indicates the first PTD that is used by the hardware when it is
      --  processing the INT list.
      INT_FIRST    : INTPTD_INT_FIRST_Field := 16#0#;
      --  Base address to be used by the hardware to find the start of the INT
      --  list.
      INT_BASE     : INTPTD_INT_BASE_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTPTD_Register use record
      Reserved_0_4 at 0 range 0 .. 4;
      INT_FIRST    at 0 range 5 .. 9;
      INT_BASE     at 0 range 10 .. 31;
   end record;

   subtype DATAPAYLOAD_DAT_BASE_Field is HAL.UInt16;

   --  Memory base address that indicates the start of the data payload buffers
   type DATAPAYLOAD_Register is record
      --  unspecified
      Reserved_0_15 : HAL.UInt16 := 16#0#;
      --  Base address to be used by the hardware to find the start of the data
      --  payload section.
      DAT_BASE      : DATAPAYLOAD_DAT_BASE_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DATAPAYLOAD_Register use record
      Reserved_0_15 at 0 range 0 .. 15;
      DAT_BASE      at 0 range 16 .. 31;
   end record;

   subtype USBCMD_FLS_Field is HAL.UInt2;

   --  USB Command register
   type USBCMD_Register is record
      --  Run/Stop: 1b = Run.
      RS             : Boolean := False;
      --  Host Controller Reset: This control bit is used by the software to
      --  reset the host controller.
      HCRESET        : Boolean := False;
      --  Frame List Size: This field specifies the size of the frame list.
      FLS            : USBCMD_FLS_Field := 16#0#;
      --  unspecified
      Reserved_4_6   : HAL.UInt3 := 16#0#;
      --  Light Host Controller Reset: This bit allows the driver software to
      --  reset the host controller without affecting the state of the ports.
      LHCR           : Boolean := False;
      --  ATL List enabled.
      ATL_EN         : Boolean := False;
      --  ISO List enabled.
      ISO_EN         : Boolean := False;
      --  INT List enabled.
      INT_EN         : Boolean := False;
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for USBCMD_Register use record
      RS             at 0 range 0 .. 0;
      HCRESET        at 0 range 1 .. 1;
      FLS            at 0 range 2 .. 3;
      Reserved_4_6   at 0 range 4 .. 6;
      LHCR           at 0 range 7 .. 7;
      ATL_EN         at 0 range 8 .. 8;
      ISO_EN         at 0 range 9 .. 9;
      INT_EN         at 0 range 10 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   --  USB Interrupt Status register
   type USBSTS_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  Port Change Detect: The host controller sets this bit to logic 1 when
      --  any port has a change bit transition from a 0 to a one or a Force
      --  Port Resume bit transition from a 0 to a 1 as a result of a J-K
      --  transition detected on a suspended port.
      PCD            : Boolean := False;
      --  Frame List Rollover: The host controller sets this bit to logic 1
      --  when the frame list index rolls over its maximum value to 0.
      FLR            : Boolean := False;
      --  unspecified
      Reserved_4_15  : HAL.UInt12 := 16#0#;
      --  ATL IRQ: Indicates that an ATL PTD (with I-bit set) was completed.
      ATL_IRQ        : Boolean := False;
      --  ISO IRQ: Indicates that an ISO PTD (with I-bit set) was completed.
      ISO_IRQ        : Boolean := False;
      --  INT IRQ: Indicates that an INT PTD (with I-bit set) was completed.
      INT_IRQ        : Boolean := False;
      --  SOF interrupt: Every time when the host sends a Start of Frame token
      --  on the USB bus, this bit is set.
      SOF_IRQ        : Boolean := False;
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for USBSTS_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      PCD            at 0 range 2 .. 2;
      FLR            at 0 range 3 .. 3;
      Reserved_4_15  at 0 range 4 .. 15;
      ATL_IRQ        at 0 range 16 .. 16;
      ISO_IRQ        at 0 range 17 .. 17;
      INT_IRQ        at 0 range 18 .. 18;
      SOF_IRQ        at 0 range 19 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  USB Interrupt Enable register
   type USBINTR_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  Port Change Detect Interrupt Enable: 1: enable 0: disable.
      PCDE           : Boolean := False;
      --  Frame List Rollover Interrupt Enable: 1: enable 0: disable.
      FLRE           : Boolean := False;
      --  unspecified
      Reserved_4_15  : HAL.UInt12 := 16#0#;
      --  ATL IRQ Enable bit: 1: enable 0: disable.
      ATL_IRQ_E      : Boolean := False;
      --  ISO IRQ Enable bit: 1: enable 0: disable.
      ISO_IRQ_E      : Boolean := False;
      --  INT IRQ Enable bit: 1: enable 0: disable.
      INT_IRQ_E      : Boolean := False;
      --  SOF Interrupt Enable bit: 1: enable 0: disable.
      SOF_E          : Boolean := False;
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for USBINTR_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      PCDE           at 0 range 2 .. 2;
      FLRE           at 0 range 3 .. 3;
      Reserved_4_15  at 0 range 4 .. 15;
      ATL_IRQ_E      at 0 range 16 .. 16;
      ISO_IRQ_E      at 0 range 17 .. 17;
      INT_IRQ_E      at 0 range 18 .. 18;
      SOF_E          at 0 range 19 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   subtype PORTSC1_LS_Field is HAL.UInt2;
   subtype PORTSC1_PIC_Field is HAL.UInt2;
   subtype PORTSC1_PTC_Field is HAL.UInt4;
   subtype PORTSC1_PSPD_Field is HAL.UInt2;

   --  Port Status and Control register
   type PORTSC1_Register is record
      --  Current Connect Status: Logic 1 indicates a device is present on the
      --  port.
      CCS            : Boolean := False;
      --  Connect Status Change: Logic 1 means that the value of CCS has
      --  changed.
      CSC            : Boolean := False;
      --  Port Enabled/Disabled.
      PED            : Boolean := False;
      --  Port Enabled/Disabled Change: Logic 1 means that the value of PED has
      --  changed.
      PEDC           : Boolean := False;
      --  Over-current active: Logic 1 means that this port has an over-current
      --  condition.
      OCA            : Boolean := False;
      --  Over-current change: Logic 1 means that the value of OCA has changed.
      OCC            : Boolean := False;
      --  Force Port Resume: Logic 1 means resume (K-state) detected or driven
      --  on the port.
      FPR            : Boolean := False;
      --  Suspend: Logic 1 means port is in the suspend state.
      SUSP           : Boolean := False;
      --  Port Reset: Logic 1 means the port is in the reset state.
      PR             : Boolean := False;
      --  unspecified
      Reserved_9_9   : HAL.Bit := 16#0#;
      --  Read-only. Line Status: This field reflects the current logical
      --  levels of the DP (bit 11) and DM (bit 10) signal lines.
      LS             : PORTSC1_LS_Field := 16#0#;
      --  Port Power: The function of this bit depends on the value of the Port
      --  Power Control (PPC) bit in the HCSPARAMS register.
      PP             : Boolean := False;
      --  unspecified
      Reserved_13_13 : HAL.Bit := 16#0#;
      --  Port Indicator Control : Writing to this field has no effect if the
      --  P_INDICATOR bit in the HCSPARAMS register is logic 0.
      PIC            : PORTSC1_PIC_Field := 16#0#;
      --  Port Test Control: A non-zero value indicates that the port is
      --  operating in the test mode as indicated by the value.
      PTC            : PORTSC1_PTC_Field := 16#0#;
      --  Port Speed: 00b: Low-speed 01b: Full-speed 10b: High-speed 11b:
      --  Reserved.
      PSPD           : PORTSC1_PSPD_Field := 16#0#;
      --  Wake on overcurrent enable: Writing this bit to a one enables the
      --  port to be sensitive to overcurrent conditions as wake-up events.
      WOO            : Boolean := False;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PORTSC1_Register use record
      CCS            at 0 range 0 .. 0;
      CSC            at 0 range 1 .. 1;
      PED            at 0 range 2 .. 2;
      PEDC           at 0 range 3 .. 3;
      OCA            at 0 range 4 .. 4;
      OCC            at 0 range 5 .. 5;
      FPR            at 0 range 6 .. 6;
      SUSP           at 0 range 7 .. 7;
      PR             at 0 range 8 .. 8;
      Reserved_9_9   at 0 range 9 .. 9;
      LS             at 0 range 10 .. 11;
      PP             at 0 range 12 .. 12;
      Reserved_13_13 at 0 range 13 .. 13;
      PIC            at 0 range 14 .. 15;
      PTC            at 0 range 16 .. 19;
      PSPD           at 0 range 20 .. 21;
      WOO            at 0 range 22 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   subtype LASTPTD_ATL_LAST_Field is HAL.UInt5;
   subtype LASTPTD_ISO_LAST_Field is HAL.UInt5;
   subtype LASTPTD_INT_LAST_Field is HAL.UInt5;

   --  Marks the last PTD in the list for ISO, INT and ATL
   type LASTPTD_Register is record
      --  If hardware has reached this PTD and the J bit is not set, it will go
      --  to PTD0 as the next PTD to be processed.
      ATL_LAST       : LASTPTD_ATL_LAST_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  This indicates the last PTD in the ISO list.
      ISO_LAST       : LASTPTD_ISO_LAST_Field := 16#0#;
      --  unspecified
      Reserved_13_15 : HAL.UInt3 := 16#0#;
      --  This indicates the last PTD in the INT list.
      INT_LAST       : LASTPTD_INT_LAST_Field := 16#0#;
      --  unspecified
      Reserved_21_31 : HAL.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for LASTPTD_Register use record
      ATL_LAST       at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      ISO_LAST       at 0 range 8 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      INT_LAST       at 0 range 16 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   --  Controls the port if it is attached to the host block or the device
   --  block
   type PORTMODE_Register is record
      --  unspecified
      Reserved_0_15  : HAL.UInt16 := 16#0#;
      --  If this bit is set to one, one of the ports will behave as a USB
      --  device.
      DEV_ENABLE     : Boolean := False;
      --  unspecified
      Reserved_17_17 : HAL.Bit := 16#0#;
      --  This bit indicates if the PHY power-down input is controlled by
      --  software or by hardware.
      SW_CTRL_PDCOM  : Boolean := True;
      --  This bit is only used when SW_CTRL_PDCOM is set to 1b.
      SW_PDCOM       : Boolean := False;
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PORTMODE_Register use record
      Reserved_0_15  at 0 range 0 .. 15;
      DEV_ENABLE     at 0 range 16 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      SW_CTRL_PDCOM  at 0 range 18 .. 18;
      SW_PDCOM       at 0 range 19 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  USB1 High-speed Host Controller
   type USBHSH_Peripheral is record
      --  This register contains the offset value towards the start of the
      --  operational register space and the version number of the IP block
      CAPLENGTH_CHIPID : aliased CAPLENGTH_CHIPID_Register;
      --  Host Controller Structural Parameters
      HCSPARAMS        : aliased HCSPARAMS_Register;
      --  Frame Length Adjustment
      FLADJ_FRINDEX    : aliased FLADJ_FRINDEX_Register;
      --  Memory base address where ATL PTD0 is stored
      ATLPTD           : aliased ATLPTD_Register;
      --  Memory base address where ISO PTD0 is stored
      ISOPTD           : aliased ISOPTD_Register;
      --  Memory base address where INT PTD0 is stored
      INTPTD           : aliased INTPTD_Register;
      --  Memory base address that indicates the start of the data payload
      --  buffers
      DATAPAYLOAD      : aliased DATAPAYLOAD_Register;
      --  USB Command register
      USBCMD           : aliased USBCMD_Register;
      --  USB Interrupt Status register
      USBSTS           : aliased USBSTS_Register;
      --  USB Interrupt Enable register
      USBINTR          : aliased USBINTR_Register;
      --  Port Status and Control register
      PORTSC1          : aliased PORTSC1_Register;
      --  Done map for each ATL PTD
      ATLPTDD          : aliased HAL.UInt32;
      --  Skip map for each ATL PTD
      ATLPTDS          : aliased HAL.UInt32;
      --  Done map for each ISO PTD
      ISOPTDD          : aliased HAL.UInt32;
      --  Skip map for each ISO PTD
      ISOPTDS          : aliased HAL.UInt32;
      --  Done map for each INT PTD
      INTPTDD          : aliased HAL.UInt32;
      --  Skip map for each INT PTD
      INTPTDS          : aliased HAL.UInt32;
      --  Marks the last PTD in the list for ISO, INT and ATL
      LASTPTD          : aliased LASTPTD_Register;
      --  Controls the port if it is attached to the host block or the device
      --  block
      PORTMODE         : aliased PORTMODE_Register;
   end record
     with Volatile;

   for USBHSH_Peripheral use record
      CAPLENGTH_CHIPID at 16#0# range 0 .. 31;
      HCSPARAMS        at 16#4# range 0 .. 31;
      FLADJ_FRINDEX    at 16#C# range 0 .. 31;
      ATLPTD           at 16#10# range 0 .. 31;
      ISOPTD           at 16#14# range 0 .. 31;
      INTPTD           at 16#18# range 0 .. 31;
      DATAPAYLOAD      at 16#1C# range 0 .. 31;
      USBCMD           at 16#20# range 0 .. 31;
      USBSTS           at 16#24# range 0 .. 31;
      USBINTR          at 16#28# range 0 .. 31;
      PORTSC1          at 16#2C# range 0 .. 31;
      ATLPTDD          at 16#30# range 0 .. 31;
      ATLPTDS          at 16#34# range 0 .. 31;
      ISOPTDD          at 16#38# range 0 .. 31;
      ISOPTDS          at 16#3C# range 0 .. 31;
      INTPTDD          at 16#40# range 0 .. 31;
      INTPTDS          at 16#44# range 0 .. 31;
      LASTPTD          at 16#48# range 0 .. 31;
      PORTMODE         at 16#50# range 0 .. 31;
   end record;

   --  USB1 High-speed Host Controller
   USBHSH_Periph : aliased USBHSH_Peripheral
     with Import, Address => System'To_Address (16#400A3000#);

end NXP_SVD.USBHSH;
