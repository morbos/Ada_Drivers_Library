--  This spec has been automatically generated from xyzzy

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.USB is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype EPR_EA_Field is HAL.UInt4;
   subtype EPR_STAT_TX_Field is HAL.UInt2;
   subtype EPR_EP_TYPE_Field is HAL.UInt2;
   subtype EPR_STAT_RX_Field is HAL.UInt2;

   type EPR_Register is record
      --  Endpoint address
      EA             : EPR_EA_Field := 16#0#;
      --  Status bits, for transmission transfers
      STAT_TX        : EPR_STAT_TX_Field := 16#0#;
      --  Data Toggle, for transmission transfers
      DTOG_TX        : Boolean := False;
      --  Correct Transfer for transmission
      CTR_TX         : Boolean := False;
      --  Endpoint kind
      EP_KIND        : Boolean := False;
      --  Endpoint type
      EP_TYPE        : EPR_EP_TYPE_Field := 16#0#;
      --  Setup transaction completed
      SETUP          : Boolean := False;
      --  Status bits, for reception transfers
      STAT_RX        : EPR_STAT_RX_Field := 16#0#;
      --  Data Toggle, for reception transfers
      DTOG_RX        : Boolean := False;
      --  Correct transfer for reception
      CTR_RX         : Boolean := False;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EPR_Register use record
      EA             at 0 range 0 .. 3;
      STAT_TX        at 0 range 4 .. 5;
      DTOG_TX        at 0 range 6 .. 6;
      CTR_TX         at 0 range 7 .. 7;
      EP_KIND        at 0 range 8 .. 8;
      EP_TYPE        at 0 range 9 .. 10;
      SETUP          at 0 range 11 .. 11;
      STAT_RX        at 0 range 12 .. 13;
      DTOG_RX        at 0 range 14 .. 14;
      CTR_RX         at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   type EPR_Registers is array (0 .. 7) of EPR_Register;

   --  control register
   type CNTR_Register is record
      --  Force USB Reset
      FRES           : Boolean := True;
      --  Power down
      PDWN           : Boolean := True;
      --  Low-power mode
      LPMODE         : Boolean := False;
      --  Force suspend
      FSUSP          : Boolean := False;
      --  Resume request
      RESUME         : Boolean := False;
      --  LPM L1 Resume request
      L1RESUME       : Boolean := False;
      --  unspecified
      Reserved_6_6   : HAL.Bit := 16#0#;
      --  LPM L1 state request interrupt mask
      L1REQM         : Boolean := False;
      --  Expected start of frame interrupt mask
      ESOFM          : Boolean := False;
      --  Start of frame interrupt mask
      SOFM           : Boolean := False;
      --  USB reset interrupt mask
      RESETM         : Boolean := False;
      --  Suspend mode interrupt mask
      SUSPM          : Boolean := False;
      --  Wakeup interrupt mask
      WKUPM          : Boolean := False;
      --  Error interrupt mask
      ERRM           : Boolean := False;
      --  Packet memory area over / underrun interrupt mask
      PMAOVRM        : Boolean := False;
      --  Correct transfer interrupt mask
      CTRM           : Boolean := False;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CNTR_Register use record
      FRES           at 0 range 0 .. 0;
      PDWN           at 0 range 1 .. 1;
      LPMODE         at 0 range 2 .. 2;
      FSUSP          at 0 range 3 .. 3;
      RESUME         at 0 range 4 .. 4;
      L1RESUME       at 0 range 5 .. 5;
      Reserved_6_6   at 0 range 6 .. 6;
      L1REQM         at 0 range 7 .. 7;
      ESOFM          at 0 range 8 .. 8;
      SOFM           at 0 range 9 .. 9;
      RESETM         at 0 range 10 .. 10;
      SUSPM          at 0 range 11 .. 11;
      WKUPM          at 0 range 12 .. 12;
      ERRM           at 0 range 13 .. 13;
      PMAOVRM        at 0 range 14 .. 14;
      CTRM           at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype ISTR_EP_ID_Field is HAL.UInt4;

   --  interrupt status register
   type ISTR_Register is record
      --  Read-only. Endpoint Identifier
      EP_ID          : ISTR_EP_ID_Field := 16#0#;
      --  Read-only. Direction of transaction
      DIR            : Boolean := False;
      --  unspecified
      Reserved_5_6   : HAL.UInt2 := 16#0#;
      --  LPM L1 state request
      L1REQ          : Boolean := False;
      --  Expected start frame
      ESOF           : Boolean := False;
      --  start of frame
      SOF            : Boolean := False;
      --  reset request
      RESET          : Boolean := False;
      --  Suspend mode request
      SUSP           : Boolean := False;
      --  Wakeup
      WKUP           : Boolean := False;
      --  Error
      ERR            : Boolean := False;
      --  Packet memory area over / underrun
      PMAOVR         : Boolean := False;
      --  Read-only. Correct transfer
      CTR            : Boolean := False;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISTR_Register use record
      EP_ID          at 0 range 0 .. 3;
      DIR            at 0 range 4 .. 4;
      Reserved_5_6   at 0 range 5 .. 6;
      L1REQ          at 0 range 7 .. 7;
      ESOF           at 0 range 8 .. 8;
      SOF            at 0 range 9 .. 9;
      RESET          at 0 range 10 .. 10;
      SUSP           at 0 range 11 .. 11;
      WKUP           at 0 range 12 .. 12;
      ERR            at 0 range 13 .. 13;
      PMAOVR         at 0 range 14 .. 14;
      CTR            at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype FNR_FN_Field is HAL.UInt11;
   subtype FNR_LSOF_Field is HAL.UInt2;

   --  frame number register
   type FNR_Register is record
      --  Read-only. Frame number
      FN             : FNR_FN_Field;
      --  Read-only. Lost SOF
      LSOF           : FNR_LSOF_Field;
      --  Read-only. Locked
      LCK            : Boolean;
      --  Read-only. Receive data - line status
      RXDM           : Boolean;
      --  Read-only. Receive data + line status
      RXDP           : Boolean;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FNR_Register use record
      FN             at 0 range 0 .. 10;
      LSOF           at 0 range 11 .. 12;
      LCK            at 0 range 13 .. 13;
      RXDM           at 0 range 14 .. 14;
      RXDP           at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DADDR_ADD_Field is HAL.UInt7;

   --  device address
   type DADDR_Register is record
      --  Device address
      ADD           : DADDR_ADD_Field := 16#0#;
      --  Enable function
      EF            : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DADDR_Register use record
      ADD           at 0 range 0 .. 6;
      EF            at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype BTABLE_BTABLE_Field is HAL.UInt13;

   --  Buffer table address
   type BTABLE_Register is record
      --  unspecified
      Reserved_0_2   : HAL.UInt3 := 16#0#;
      --  Buffer table
      BTABLE         : BTABLE_BTABLE_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for BTABLE_Register use record
      Reserved_0_2   at 0 range 0 .. 2;
      BTABLE         at 0 range 3 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype LPMCSR_BESL_Field is HAL.UInt4;

   --  LPM control and status register
   type LPMCSR_Register is record
      --  LPM support enable
      LPMEN         : Boolean := False;
      --  LPM Token acknowledge enable
      LPMACK        : Boolean := False;
      --  unspecified
      Reserved_2_2  : HAL.Bit := 16#0#;
      --  RemoteWake value
      REMWAKE       : Boolean := False;
      --  BESL value
      BESL          : LPMCSR_BESL_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPMCSR_Register use record
      LPMEN         at 0 range 0 .. 0;
      LPMACK        at 0 range 1 .. 1;
      Reserved_2_2  at 0 range 2 .. 2;
      REMWAKE       at 0 range 3 .. 3;
      BESL          at 0 range 4 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  Battery charging detector
   type BCDR_Register is record
      --  Battery charging detector (BCD) enable
      BCDEN          : Boolean := False;
      --  Data contact detection (DCD) mode enable
      DCDEN          : Boolean := False;
      --  Primary detection (PD) mode enable
      PDEN           : Boolean := False;
      --  Secondary detection (SD) mode enable
      SDEN           : Boolean := False;
      --  Data contact detection (DCD) status
      DCDET          : Boolean := False;
      --  Primary detection (PD) status
      PDET           : Boolean := False;
      --  Secondary detection (SD) status
      SDET           : Boolean := False;
      --  DM pull-up detection status
      PS2DET         : Boolean := False;
      --  unspecified
      Reserved_8_14  : HAL.UInt7 := 16#0#;
      --  DP pull-up control
      DPPU           : Boolean := False;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for BCDR_Register use record
      BCDEN          at 0 range 0 .. 0;
      DCDEN          at 0 range 1 .. 1;
      PDEN           at 0 range 2 .. 2;
      SDEN           at 0 range 3 .. 3;
      DCDET          at 0 range 4 .. 4;
      PDET           at 0 range 5 .. 5;
      SDET           at 0 range 6 .. 6;
      PS2DET         at 0 range 7 .. 7;
      Reserved_8_14  at 0 range 8 .. 14;
      DPPU           at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Universal serial bus full-speed device interface
   type USB_Peripheral is record
      EPR    : aliased EPR_Registers;
      --  control register
      CNTR   : aliased CNTR_Register;
      --  interrupt status register
      ISTR   : aliased ISTR_Register;
      --  frame number register
      FNR    : aliased FNR_Register;
      --  device address
      DADDR  : aliased DADDR_Register;
      --  Buffer table address
      BTABLE : aliased BTABLE_Register;
      --  LPM control and status register
      LPMCSR : aliased LPMCSR_Register;
      --  Battery charging detector
      BCDR   : aliased BCDR_Register;
   end record
     with Volatile;

   for USB_Peripheral use record
      EPR    at 16#0# range 0 .. 255;
      CNTR   at 16#40# range 0 .. 31;
      ISTR   at 16#44# range 0 .. 31;
      FNR    at 16#48# range 0 .. 31;
      DADDR  at 16#4C# range 0 .. 31;
      BTABLE at 16#50# range 0 .. 31;
      LPMCSR at 16#54# range 0 .. 31;
      BCDR   at 16#58# range 0 .. 31;
   end record;

   --  Universal serial bus full-speed device interface
   SEC_USB_Periph : aliased USB_Peripheral
     with Import, Address => SEC_USB_Base;

   --  Universal serial bus full-speed device interface
   USB_Periph : aliased USB_Peripheral
     with Import, Address => USB_Base;

end STM32_SVD.USB;
