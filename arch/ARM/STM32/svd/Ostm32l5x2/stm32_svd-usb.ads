--  This spec has been automatically generated from STM32L5x2.svd

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

   subtype EP0R_EA_Field is HAL.UInt4;
   subtype EP0R_STAT_TX_Field is HAL.UInt2;
   subtype EP0R_EP_TYPE_Field is HAL.UInt2;
   subtype EP0R_STAT_RX_Field is HAL.UInt2;

   --  endpoint 0 register
   type EP0R_Register is record
      --  Endpoint address
      EA      : EP0R_EA_Field := 16#0#;
      --  Status bits, for transmission transfers
      STAT_TX : EP0R_STAT_TX_Field := 16#0#;
      --  Data Toggle, for transmission transfers
      DTOG_TX : Boolean := False;
      --  Correct Transfer for transmission
      CTR_TX  : Boolean := False;
      --  Endpoint kind
      EP_KIND : Boolean := False;
      --  Endpoint type
      EP_TYPE : EP0R_EP_TYPE_Field := 16#0#;
      --  Setup transaction completed
      SETUP   : Boolean := False;
      --  Status bits, for reception transfers
      STAT_RX : EP0R_STAT_RX_Field := 16#0#;
      --  Data Toggle, for reception transfers
      DTOG_RX : Boolean := False;
      --  Correct transfer for reception
      CTR_RX  : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for EP0R_Register use record
      EA      at 0 range 0 .. 3;
      STAT_TX at 0 range 4 .. 5;
      DTOG_TX at 0 range 6 .. 6;
      CTR_TX  at 0 range 7 .. 7;
      EP_KIND at 0 range 8 .. 8;
      EP_TYPE at 0 range 9 .. 10;
      SETUP   at 0 range 11 .. 11;
      STAT_RX at 0 range 12 .. 13;
      DTOG_RX at 0 range 14 .. 14;
      CTR_RX  at 0 range 15 .. 15;
   end record;

   subtype EP1R_EA_Field is HAL.UInt4;
   subtype EP1R_STAT_TX_Field is HAL.UInt2;
   subtype EP1R_EP_TYPE_Field is HAL.UInt2;
   subtype EP1R_STAT_RX_Field is HAL.UInt2;

   --  endpoint 1 register
   type EP1R_Register is record
      --  Endpoint address
      EA      : EP1R_EA_Field := 16#0#;
      --  Status bits, for transmission transfers
      STAT_TX : EP1R_STAT_TX_Field := 16#0#;
      --  Data Toggle, for transmission transfers
      DTOG_TX : Boolean := False;
      --  Correct Transfer for transmission
      CTR_TX  : Boolean := False;
      --  Endpoint kind
      EP_KIND : Boolean := False;
      --  Endpoint type
      EP_TYPE : EP1R_EP_TYPE_Field := 16#0#;
      --  Setup transaction completed
      SETUP   : Boolean := False;
      --  Status bits, for reception transfers
      STAT_RX : EP1R_STAT_RX_Field := 16#0#;
      --  Data Toggle, for reception transfers
      DTOG_RX : Boolean := False;
      --  Correct transfer for reception
      CTR_RX  : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for EP1R_Register use record
      EA      at 0 range 0 .. 3;
      STAT_TX at 0 range 4 .. 5;
      DTOG_TX at 0 range 6 .. 6;
      CTR_TX  at 0 range 7 .. 7;
      EP_KIND at 0 range 8 .. 8;
      EP_TYPE at 0 range 9 .. 10;
      SETUP   at 0 range 11 .. 11;
      STAT_RX at 0 range 12 .. 13;
      DTOG_RX at 0 range 14 .. 14;
      CTR_RX  at 0 range 15 .. 15;
   end record;

   subtype EP2R_EA_Field is HAL.UInt4;
   subtype EP2R_STAT_TX_Field is HAL.UInt2;
   subtype EP2R_EP_TYPE_Field is HAL.UInt2;
   subtype EP2R_STAT_RX_Field is HAL.UInt2;

   --  endpoint 2 register
   type EP2R_Register is record
      --  Endpoint address
      EA      : EP2R_EA_Field := 16#0#;
      --  Status bits, for transmission transfers
      STAT_TX : EP2R_STAT_TX_Field := 16#0#;
      --  Data Toggle, for transmission transfers
      DTOG_TX : Boolean := False;
      --  Correct Transfer for transmission
      CTR_TX  : Boolean := False;
      --  Endpoint kind
      EP_KIND : Boolean := False;
      --  Endpoint type
      EP_TYPE : EP2R_EP_TYPE_Field := 16#0#;
      --  Setup transaction completed
      SETUP   : Boolean := False;
      --  Status bits, for reception transfers
      STAT_RX : EP2R_STAT_RX_Field := 16#0#;
      --  Data Toggle, for reception transfers
      DTOG_RX : Boolean := False;
      --  Correct transfer for reception
      CTR_RX  : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for EP2R_Register use record
      EA      at 0 range 0 .. 3;
      STAT_TX at 0 range 4 .. 5;
      DTOG_TX at 0 range 6 .. 6;
      CTR_TX  at 0 range 7 .. 7;
      EP_KIND at 0 range 8 .. 8;
      EP_TYPE at 0 range 9 .. 10;
      SETUP   at 0 range 11 .. 11;
      STAT_RX at 0 range 12 .. 13;
      DTOG_RX at 0 range 14 .. 14;
      CTR_RX  at 0 range 15 .. 15;
   end record;

   subtype EP3R_EA_Field is HAL.UInt4;
   subtype EP3R_STAT_TX_Field is HAL.UInt2;
   subtype EP3R_EP_TYPE_Field is HAL.UInt2;
   subtype EP3R_STAT_RX_Field is HAL.UInt2;

   --  endpoint 3 register
   type EP3R_Register is record
      --  Endpoint address
      EA      : EP3R_EA_Field := 16#0#;
      --  Status bits, for transmission transfers
      STAT_TX : EP3R_STAT_TX_Field := 16#0#;
      --  Data Toggle, for transmission transfers
      DTOG_TX : Boolean := False;
      --  Correct Transfer for transmission
      CTR_TX  : Boolean := False;
      --  Endpoint kind
      EP_KIND : Boolean := False;
      --  Endpoint type
      EP_TYPE : EP3R_EP_TYPE_Field := 16#0#;
      --  Setup transaction completed
      SETUP   : Boolean := False;
      --  Status bits, for reception transfers
      STAT_RX : EP3R_STAT_RX_Field := 16#0#;
      --  Data Toggle, for reception transfers
      DTOG_RX : Boolean := False;
      --  Correct transfer for reception
      CTR_RX  : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for EP3R_Register use record
      EA      at 0 range 0 .. 3;
      STAT_TX at 0 range 4 .. 5;
      DTOG_TX at 0 range 6 .. 6;
      CTR_TX  at 0 range 7 .. 7;
      EP_KIND at 0 range 8 .. 8;
      EP_TYPE at 0 range 9 .. 10;
      SETUP   at 0 range 11 .. 11;
      STAT_RX at 0 range 12 .. 13;
      DTOG_RX at 0 range 14 .. 14;
      CTR_RX  at 0 range 15 .. 15;
   end record;

   subtype EP4R_EA_Field is HAL.UInt4;
   subtype EP4R_STAT_TX_Field is HAL.UInt2;
   subtype EP4R_EP_TYPE_Field is HAL.UInt2;
   subtype EP4R_STAT_RX_Field is HAL.UInt2;

   --  endpoint 4 register
   type EP4R_Register is record
      --  Endpoint address
      EA      : EP4R_EA_Field := 16#0#;
      --  Status bits, for transmission transfers
      STAT_TX : EP4R_STAT_TX_Field := 16#0#;
      --  Data Toggle, for transmission transfers
      DTOG_TX : Boolean := False;
      --  Correct Transfer for transmission
      CTR_TX  : Boolean := False;
      --  Endpoint kind
      EP_KIND : Boolean := False;
      --  Endpoint type
      EP_TYPE : EP4R_EP_TYPE_Field := 16#0#;
      --  Setup transaction completed
      SETUP   : Boolean := False;
      --  Status bits, for reception transfers
      STAT_RX : EP4R_STAT_RX_Field := 16#0#;
      --  Data Toggle, for reception transfers
      DTOG_RX : Boolean := False;
      --  Correct transfer for reception
      CTR_RX  : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for EP4R_Register use record
      EA      at 0 range 0 .. 3;
      STAT_TX at 0 range 4 .. 5;
      DTOG_TX at 0 range 6 .. 6;
      CTR_TX  at 0 range 7 .. 7;
      EP_KIND at 0 range 8 .. 8;
      EP_TYPE at 0 range 9 .. 10;
      SETUP   at 0 range 11 .. 11;
      STAT_RX at 0 range 12 .. 13;
      DTOG_RX at 0 range 14 .. 14;
      CTR_RX  at 0 range 15 .. 15;
   end record;

   subtype EP5R_EA_Field is HAL.UInt4;
   subtype EP5R_STAT_TX_Field is HAL.UInt2;
   subtype EP5R_EP_TYPE_Field is HAL.UInt2;
   subtype EP5R_STAT_RX_Field is HAL.UInt2;

   --  endpoint 5 register
   type EP5R_Register is record
      --  Endpoint address
      EA      : EP5R_EA_Field := 16#0#;
      --  Status bits, for transmission transfers
      STAT_TX : EP5R_STAT_TX_Field := 16#0#;
      --  Data Toggle, for transmission transfers
      DTOG_TX : Boolean := False;
      --  Correct Transfer for transmission
      CTR_TX  : Boolean := False;
      --  Endpoint kind
      EP_KIND : Boolean := False;
      --  Endpoint type
      EP_TYPE : EP5R_EP_TYPE_Field := 16#0#;
      --  Setup transaction completed
      SETUP   : Boolean := False;
      --  Status bits, for reception transfers
      STAT_RX : EP5R_STAT_RX_Field := 16#0#;
      --  Data Toggle, for reception transfers
      DTOG_RX : Boolean := False;
      --  Correct transfer for reception
      CTR_RX  : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for EP5R_Register use record
      EA      at 0 range 0 .. 3;
      STAT_TX at 0 range 4 .. 5;
      DTOG_TX at 0 range 6 .. 6;
      CTR_TX  at 0 range 7 .. 7;
      EP_KIND at 0 range 8 .. 8;
      EP_TYPE at 0 range 9 .. 10;
      SETUP   at 0 range 11 .. 11;
      STAT_RX at 0 range 12 .. 13;
      DTOG_RX at 0 range 14 .. 14;
      CTR_RX  at 0 range 15 .. 15;
   end record;

   subtype EP6R_EA_Field is HAL.UInt4;
   subtype EP6R_STAT_TX_Field is HAL.UInt2;
   subtype EP6R_EP_TYPE_Field is HAL.UInt2;
   subtype EP6R_STAT_RX_Field is HAL.UInt2;

   --  endpoint 6 register
   type EP6R_Register is record
      --  Endpoint address
      EA      : EP6R_EA_Field := 16#0#;
      --  Status bits, for transmission transfers
      STAT_TX : EP6R_STAT_TX_Field := 16#0#;
      --  Data Toggle, for transmission transfers
      DTOG_TX : Boolean := False;
      --  Correct Transfer for transmission
      CTR_TX  : Boolean := False;
      --  Endpoint kind
      EP_KIND : Boolean := False;
      --  Endpoint type
      EP_TYPE : EP6R_EP_TYPE_Field := 16#0#;
      --  Setup transaction completed
      SETUP   : Boolean := False;
      --  Status bits, for reception transfers
      STAT_RX : EP6R_STAT_RX_Field := 16#0#;
      --  Data Toggle, for reception transfers
      DTOG_RX : Boolean := False;
      --  Correct transfer for reception
      CTR_RX  : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for EP6R_Register use record
      EA      at 0 range 0 .. 3;
      STAT_TX at 0 range 4 .. 5;
      DTOG_TX at 0 range 6 .. 6;
      CTR_TX  at 0 range 7 .. 7;
      EP_KIND at 0 range 8 .. 8;
      EP_TYPE at 0 range 9 .. 10;
      SETUP   at 0 range 11 .. 11;
      STAT_RX at 0 range 12 .. 13;
      DTOG_RX at 0 range 14 .. 14;
      CTR_RX  at 0 range 15 .. 15;
   end record;

   subtype EP7R_EA_Field is HAL.UInt4;
   subtype EP7R_STAT_TX_Field is HAL.UInt2;
   subtype EP7R_EP_TYPE_Field is HAL.UInt2;
   subtype EP7R_STAT_RX_Field is HAL.UInt2;

   --  endpoint 7 register
   type EP7R_Register is record
      --  Endpoint address
      EA      : EP7R_EA_Field := 16#0#;
      --  Status bits, for transmission transfers
      STAT_TX : EP7R_STAT_TX_Field := 16#0#;
      --  Data Toggle, for transmission transfers
      DTOG_TX : Boolean := False;
      --  Correct Transfer for transmission
      CTR_TX  : Boolean := False;
      --  Endpoint kind
      EP_KIND : Boolean := False;
      --  Endpoint type
      EP_TYPE : EP7R_EP_TYPE_Field := 16#0#;
      --  Setup transaction completed
      SETUP   : Boolean := False;
      --  Status bits, for reception transfers
      STAT_RX : EP7R_STAT_RX_Field := 16#0#;
      --  Data Toggle, for reception transfers
      DTOG_RX : Boolean := False;
      --  Correct transfer for reception
      CTR_RX  : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for EP7R_Register use record
      EA      at 0 range 0 .. 3;
      STAT_TX at 0 range 4 .. 5;
      DTOG_TX at 0 range 6 .. 6;
      CTR_TX  at 0 range 7 .. 7;
      EP_KIND at 0 range 8 .. 8;
      EP_TYPE at 0 range 9 .. 10;
      SETUP   at 0 range 11 .. 11;
      STAT_RX at 0 range 12 .. 13;
      DTOG_RX at 0 range 14 .. 14;
      CTR_RX  at 0 range 15 .. 15;
   end record;

   --  control register
   type CNTR_Register is record
      --  Force USB Reset
      FRES         : Boolean := True;
      --  Power down
      PDWN         : Boolean := True;
      --  Low-power mode
      LPMODE       : Boolean := False;
      --  Force suspend
      FSUSP        : Boolean := False;
      --  Resume request
      RESUME       : Boolean := False;
      --  LPM L1 Resume request
      L1RESUME     : Boolean := False;
      --  unspecified
      Reserved_6_6 : HAL.Bit := 16#0#;
      --  LPM L1 state request interrupt mask
      L1REQM       : Boolean := False;
      --  Expected start of frame interrupt mask
      ESOFM        : Boolean := False;
      --  Start of frame interrupt mask
      SOFM         : Boolean := False;
      --  USB reset interrupt mask
      RESETM       : Boolean := False;
      --  Suspend mode interrupt mask
      SUSPM        : Boolean := False;
      --  Wakeup interrupt mask
      WKUPM        : Boolean := False;
      --  Error interrupt mask
      ERRM         : Boolean := False;
      --  Packet memory area over / underrun interrupt mask
      PMAOVRM      : Boolean := False;
      --  Correct transfer interrupt mask
      CTRM         : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for CNTR_Register use record
      FRES         at 0 range 0 .. 0;
      PDWN         at 0 range 1 .. 1;
      LPMODE       at 0 range 2 .. 2;
      FSUSP        at 0 range 3 .. 3;
      RESUME       at 0 range 4 .. 4;
      L1RESUME     at 0 range 5 .. 5;
      Reserved_6_6 at 0 range 6 .. 6;
      L1REQM       at 0 range 7 .. 7;
      ESOFM        at 0 range 8 .. 8;
      SOFM         at 0 range 9 .. 9;
      RESETM       at 0 range 10 .. 10;
      SUSPM        at 0 range 11 .. 11;
      WKUPM        at 0 range 12 .. 12;
      ERRM         at 0 range 13 .. 13;
      PMAOVRM      at 0 range 14 .. 14;
      CTRM         at 0 range 15 .. 15;
   end record;

   subtype ISTR_EP_ID_Field is HAL.UInt4;

   --  interrupt status register
   type ISTR_Register is record
      --  Read-only. Endpoint Identifier
      EP_ID        : ISTR_EP_ID_Field := 16#0#;
      --  Read-only. Direction of transaction
      DIR          : Boolean := False;
      --  unspecified
      Reserved_5_6 : HAL.UInt2 := 16#0#;
      --  LPM L1 state request
      L1REQ        : Boolean := False;
      --  Expected start frame
      ESOF         : Boolean := False;
      --  start of frame
      SOF          : Boolean := False;
      --  reset request
      RESET        : Boolean := False;
      --  Suspend mode request
      SUSP         : Boolean := False;
      --  Wakeup
      WKUP         : Boolean := False;
      --  Error
      ERR          : Boolean := False;
      --  Packet memory area over / underrun
      PMAOVR       : Boolean := False;
      --  Read-only. Correct transfer
      CTR          : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for ISTR_Register use record
      EP_ID        at 0 range 0 .. 3;
      DIR          at 0 range 4 .. 4;
      Reserved_5_6 at 0 range 5 .. 6;
      L1REQ        at 0 range 7 .. 7;
      ESOF         at 0 range 8 .. 8;
      SOF          at 0 range 9 .. 9;
      RESET        at 0 range 10 .. 10;
      SUSP         at 0 range 11 .. 11;
      WKUP         at 0 range 12 .. 12;
      ERR          at 0 range 13 .. 13;
      PMAOVR       at 0 range 14 .. 14;
      CTR          at 0 range 15 .. 15;
   end record;

   subtype FNR_FN_Field is HAL.UInt11;
   subtype FNR_LSOF_Field is HAL.UInt2;

   --  frame number register
   type FNR_Register is record
      --  Read-only. Frame number
      FN   : FNR_FN_Field;
      --  Read-only. Lost SOF
      LSOF : FNR_LSOF_Field;
      --  Read-only. Locked
      LCK  : Boolean;
      --  Read-only. Receive data - line status
      RXDM : Boolean;
      --  Read-only. Receive data + line status
      RXDP : Boolean;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for FNR_Register use record
      FN   at 0 range 0 .. 10;
      LSOF at 0 range 11 .. 12;
      LCK  at 0 range 13 .. 13;
      RXDM at 0 range 14 .. 14;
      RXDP at 0 range 15 .. 15;
   end record;

   subtype DADDR_ADD_Field is HAL.UInt7;

   --  device address
   type DADDR_Register is record
      --  Device address
      ADD           : DADDR_ADD_Field := 16#0#;
      --  Enable function
      EF            : Boolean := False;
      --  unspecified
      Reserved_8_15 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for DADDR_Register use record
      ADD           at 0 range 0 .. 6;
      EF            at 0 range 7 .. 7;
      Reserved_8_15 at 0 range 8 .. 15;
   end record;

   subtype BTABLE_BTABLE_Field is HAL.UInt13;

   --  Buffer table address
   type BTABLE_Register is record
      --  unspecified
      Reserved_0_2 : HAL.UInt3 := 16#0#;
      --  Buffer table
      BTABLE       : BTABLE_BTABLE_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for BTABLE_Register use record
      Reserved_0_2 at 0 range 0 .. 2;
      BTABLE       at 0 range 3 .. 15;
   end record;

   subtype COUNT0_TX_COUNT0_TX_Field is HAL.UInt10;

   --  Transmission byte count 0
   type COUNT0_TX_Register is record
      --  Transmission byte count
      COUNT0_TX      : COUNT0_TX_COUNT0_TX_Field := 16#0#;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for COUNT0_TX_Register use record
      COUNT0_TX      at 0 range 0 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
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
      Reserved_8_15 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for LPMCSR_Register use record
      LPMEN         at 0 range 0 .. 0;
      LPMACK        at 0 range 1 .. 1;
      Reserved_2_2  at 0 range 2 .. 2;
      REMWAKE       at 0 range 3 .. 3;
      BESL          at 0 range 4 .. 7;
      Reserved_8_15 at 0 range 8 .. 15;
   end record;

   subtype COUNT0_RX_COUNT0_RX_Field is HAL.UInt10;
   subtype COUNT0_RX_NUM_BLOCK_Field is HAL.UInt5;

   --  Reception byte count 0
   type COUNT0_RX_Register is record
      --  Read-only. Reception byte count
      COUNT0_RX : COUNT0_RX_COUNT0_RX_Field := 16#0#;
      --  Number of blocks
      NUM_BLOCK : COUNT0_RX_NUM_BLOCK_Field := 16#0#;
      --  Block size
      BL_SIZE   : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for COUNT0_RX_Register use record
      COUNT0_RX at 0 range 0 .. 9;
      NUM_BLOCK at 0 range 10 .. 14;
      BL_SIZE   at 0 range 15 .. 15;
   end record;

   --  Battery charging detector
   type BCDR_Register is record
      --  Battery charging detector (BCD) enable
      BCDEN         : Boolean := False;
      --  Data contact detection (DCD) mode enable
      DCDEN         : Boolean := False;
      --  Primary detection (PD) mode enable
      PDEN          : Boolean := False;
      --  Secondary detection (SD) mode enable
      SDEN          : Boolean := False;
      --  Data contact detection (DCD) status
      DCDET         : Boolean := False;
      --  Primary detection (PD) status
      PDET          : Boolean := False;
      --  Secondary detection (SD) status
      SDET          : Boolean := False;
      --  DM pull-up detection status
      PS2DET        : Boolean := False;
      --  unspecified
      Reserved_8_14 : HAL.UInt7 := 16#0#;
      --  DP pull-up control
      DPPU          : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for BCDR_Register use record
      BCDEN         at 0 range 0 .. 0;
      DCDEN         at 0 range 1 .. 1;
      PDEN          at 0 range 2 .. 2;
      SDEN          at 0 range 3 .. 3;
      DCDET         at 0 range 4 .. 4;
      PDET          at 0 range 5 .. 5;
      SDET          at 0 range 6 .. 6;
      PS2DET        at 0 range 7 .. 7;
      Reserved_8_14 at 0 range 8 .. 14;
      DPPU          at 0 range 15 .. 15;
   end record;

   subtype COUNT1_TX_COUNT1_TX_Field is HAL.UInt10;

   --  Transmission byte count 0
   type COUNT1_TX_Register is record
      --  Transmission byte count
      COUNT1_TX      : COUNT1_TX_COUNT1_TX_Field := 16#0#;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for COUNT1_TX_Register use record
      COUNT1_TX      at 0 range 0 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
   end record;

   subtype ADDR0_RX_ADDR0_RX_Field is HAL.UInt15;

   --  Reception buffer address 0
   type ADDR0_RX_Register is record
      --  unspecified
      Reserved_0_0 : HAL.Bit := 16#0#;
      --  Reception buffer address
      ADDR0_RX     : ADDR0_RX_ADDR0_RX_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for ADDR0_RX_Register use record
      Reserved_0_0 at 0 range 0 .. 0;
      ADDR0_RX     at 0 range 1 .. 15;
   end record;

   subtype COUNT1_RX_COUNT1_RX_Field is HAL.UInt10;
   subtype COUNT1_RX_NUM_BLOCK_Field is HAL.UInt5;

   --  Reception byte count 0
   type COUNT1_RX_Register is record
      --  Read-only. Reception byte count
      COUNT1_RX : COUNT1_RX_COUNT1_RX_Field := 16#0#;
      --  Number of blocks
      NUM_BLOCK : COUNT1_RX_NUM_BLOCK_Field := 16#0#;
      --  Block size
      BL_SIZE   : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for COUNT1_RX_Register use record
      COUNT1_RX at 0 range 0 .. 9;
      NUM_BLOCK at 0 range 10 .. 14;
      BL_SIZE   at 0 range 15 .. 15;
   end record;

   subtype COUNT2_TX_COUNT2_TX_Field is HAL.UInt10;

   --  Transmission byte count 0
   type COUNT2_TX_Register is record
      --  Transmission byte count
      COUNT2_TX      : COUNT2_TX_COUNT2_TX_Field := 16#0#;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for COUNT2_TX_Register use record
      COUNT2_TX      at 0 range 0 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
   end record;

   subtype ADDR1_RX_ADDR1_RX_Field is HAL.UInt15;

   --  Reception buffer address 0
   type ADDR1_RX_Register is record
      --  unspecified
      Reserved_0_0 : HAL.Bit := 16#0#;
      --  Reception buffer address
      ADDR1_RX     : ADDR1_RX_ADDR1_RX_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for ADDR1_RX_Register use record
      Reserved_0_0 at 0 range 0 .. 0;
      ADDR1_RX     at 0 range 1 .. 15;
   end record;

   subtype COUNT2_RX_COUNT2_RX_Field is HAL.UInt10;
   subtype COUNT2_RX_NUM_BLOCK_Field is HAL.UInt5;

   --  Reception byte count 0
   type COUNT2_RX_Register is record
      --  Read-only. Reception byte count
      COUNT2_RX : COUNT2_RX_COUNT2_RX_Field := 16#0#;
      --  Number of blocks
      NUM_BLOCK : COUNT2_RX_NUM_BLOCK_Field := 16#0#;
      --  Block size
      BL_SIZE   : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for COUNT2_RX_Register use record
      COUNT2_RX at 0 range 0 .. 9;
      NUM_BLOCK at 0 range 10 .. 14;
      BL_SIZE   at 0 range 15 .. 15;
   end record;

   subtype COUNT3_TX_COUNT3_TX_Field is HAL.UInt10;

   --  Transmission byte count 0
   type COUNT3_TX_Register is record
      --  Transmission byte count
      COUNT3_TX      : COUNT3_TX_COUNT3_TX_Field := 16#0#;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for COUNT3_TX_Register use record
      COUNT3_TX      at 0 range 0 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
   end record;

   subtype ADDR2_RX_ADDR2_RX_Field is HAL.UInt15;

   --  Reception buffer address 0
   type ADDR2_RX_Register is record
      --  unspecified
      Reserved_0_0 : HAL.Bit := 16#0#;
      --  Reception buffer address
      ADDR2_RX     : ADDR2_RX_ADDR2_RX_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for ADDR2_RX_Register use record
      Reserved_0_0 at 0 range 0 .. 0;
      ADDR2_RX     at 0 range 1 .. 15;
   end record;

   subtype COUNT3_RX_COUNT3_RX_Field is HAL.UInt10;
   subtype COUNT3_RX_NUM_BLOCK_Field is HAL.UInt5;

   --  Reception byte count 0
   type COUNT3_RX_Register is record
      --  Read-only. Reception byte count
      COUNT3_RX : COUNT3_RX_COUNT3_RX_Field := 16#0#;
      --  Number of blocks
      NUM_BLOCK : COUNT3_RX_NUM_BLOCK_Field := 16#0#;
      --  Block size
      BL_SIZE   : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for COUNT3_RX_Register use record
      COUNT3_RX at 0 range 0 .. 9;
      NUM_BLOCK at 0 range 10 .. 14;
      BL_SIZE   at 0 range 15 .. 15;
   end record;

   subtype COUNT4_TX_COUNT4_TX_Field is HAL.UInt10;

   --  Transmission byte count 0
   type COUNT4_TX_Register is record
      --  Transmission byte count
      COUNT4_TX      : COUNT4_TX_COUNT4_TX_Field := 16#0#;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for COUNT4_TX_Register use record
      COUNT4_TX      at 0 range 0 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
   end record;

   subtype ADDR3_RX_ADDR3_RX_Field is HAL.UInt15;

   --  Reception buffer address 0
   type ADDR3_RX_Register is record
      --  unspecified
      Reserved_0_0 : HAL.Bit := 16#0#;
      --  Reception buffer address
      ADDR3_RX     : ADDR3_RX_ADDR3_RX_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for ADDR3_RX_Register use record
      Reserved_0_0 at 0 range 0 .. 0;
      ADDR3_RX     at 0 range 1 .. 15;
   end record;

   subtype COUNT4_RX_COUNT4_RX_Field is HAL.UInt10;
   subtype COUNT4_RX_NUM_BLOCK_Field is HAL.UInt5;

   --  Reception byte count 0
   type COUNT4_RX_Register is record
      --  Read-only. Reception byte count
      COUNT4_RX : COUNT4_RX_COUNT4_RX_Field := 16#0#;
      --  Number of blocks
      NUM_BLOCK : COUNT4_RX_NUM_BLOCK_Field := 16#0#;
      --  Block size
      BL_SIZE   : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for COUNT4_RX_Register use record
      COUNT4_RX at 0 range 0 .. 9;
      NUM_BLOCK at 0 range 10 .. 14;
      BL_SIZE   at 0 range 15 .. 15;
   end record;

   subtype COUNT5_TX_COUNT5_TX_Field is HAL.UInt10;

   --  Transmission byte count 0
   type COUNT5_TX_Register is record
      --  Transmission byte count
      COUNT5_TX      : COUNT5_TX_COUNT5_TX_Field := 16#0#;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for COUNT5_TX_Register use record
      COUNT5_TX      at 0 range 0 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
   end record;

   subtype ADDR4_RX_ADDR4_RX_Field is HAL.UInt15;

   --  Reception buffer address 0
   type ADDR4_RX_Register is record
      --  unspecified
      Reserved_0_0 : HAL.Bit := 16#0#;
      --  Reception buffer address
      ADDR4_RX     : ADDR4_RX_ADDR4_RX_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for ADDR4_RX_Register use record
      Reserved_0_0 at 0 range 0 .. 0;
      ADDR4_RX     at 0 range 1 .. 15;
   end record;

   subtype COUNT5_RX_COUNT5_RX_Field is HAL.UInt10;
   subtype COUNT5_RX_NUM_BLOCK_Field is HAL.UInt5;

   --  Reception byte count 0
   type COUNT5_RX_Register is record
      --  Read-only. Reception byte count
      COUNT5_RX : COUNT5_RX_COUNT5_RX_Field := 16#0#;
      --  Number of blocks
      NUM_BLOCK : COUNT5_RX_NUM_BLOCK_Field := 16#0#;
      --  Block size
      BL_SIZE   : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for COUNT5_RX_Register use record
      COUNT5_RX at 0 range 0 .. 9;
      NUM_BLOCK at 0 range 10 .. 14;
      BL_SIZE   at 0 range 15 .. 15;
   end record;

   subtype COUNT6_TX_COUNT6_TX_Field is HAL.UInt10;

   --  Transmission byte count 0
   type COUNT6_TX_Register is record
      --  Transmission byte count
      COUNT6_TX      : COUNT6_TX_COUNT6_TX_Field := 16#0#;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for COUNT6_TX_Register use record
      COUNT6_TX      at 0 range 0 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
   end record;

   subtype ADDR5_RX_ADDR5_RX_Field is HAL.UInt15;

   --  Reception buffer address 0
   type ADDR5_RX_Register is record
      --  unspecified
      Reserved_0_0 : HAL.Bit := 16#0#;
      --  Reception buffer address
      ADDR5_RX     : ADDR5_RX_ADDR5_RX_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for ADDR5_RX_Register use record
      Reserved_0_0 at 0 range 0 .. 0;
      ADDR5_RX     at 0 range 1 .. 15;
   end record;

   subtype COUNT6_RX_COUNT6_RX_Field is HAL.UInt10;
   subtype COUNT6_RX_NUM_BLOCK_Field is HAL.UInt5;

   --  Reception byte count 0
   type COUNT6_RX_Register is record
      --  Read-only. Reception byte count
      COUNT6_RX : COUNT6_RX_COUNT6_RX_Field := 16#0#;
      --  Number of blocks
      NUM_BLOCK : COUNT6_RX_NUM_BLOCK_Field := 16#0#;
      --  Block size
      BL_SIZE   : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for COUNT6_RX_Register use record
      COUNT6_RX at 0 range 0 .. 9;
      NUM_BLOCK at 0 range 10 .. 14;
      BL_SIZE   at 0 range 15 .. 15;
   end record;

   subtype COUNT7_TX_COUNT7_TX_Field is HAL.UInt10;

   --  Transmission byte count 0
   type COUNT7_TX_Register is record
      --  Transmission byte count
      COUNT7_TX      : COUNT7_TX_COUNT7_TX_Field := 16#0#;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for COUNT7_TX_Register use record
      COUNT7_TX      at 0 range 0 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
   end record;

   subtype ADDR6_RX_ADDR6_RX_Field is HAL.UInt15;

   --  Reception buffer address 0
   type ADDR6_RX_Register is record
      --  unspecified
      Reserved_0_0 : HAL.Bit := 16#0#;
      --  Reception buffer address
      ADDR6_RX     : ADDR6_RX_ADDR6_RX_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for ADDR6_RX_Register use record
      Reserved_0_0 at 0 range 0 .. 0;
      ADDR6_RX     at 0 range 1 .. 15;
   end record;

   subtype COUNT7_RX_COUNT7_RX_Field is HAL.UInt10;
   subtype COUNT7_RX_NUM_BLOCK_Field is HAL.UInt5;

   --  Reception byte count 0
   type COUNT7_RX_Register is record
      --  Read-only. Reception byte count
      COUNT7_RX : COUNT7_RX_COUNT7_RX_Field := 16#0#;
      --  Number of blocks
      NUM_BLOCK : COUNT7_RX_NUM_BLOCK_Field := 16#0#;
      --  Block size
      BL_SIZE   : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for COUNT7_RX_Register use record
      COUNT7_RX at 0 range 0 .. 9;
      NUM_BLOCK at 0 range 10 .. 14;
      BL_SIZE   at 0 range 15 .. 15;
   end record;

   subtype ADDR7_RX_ADDR7_RX_Field is HAL.UInt15;

   --  Reception buffer address 0
   type ADDR7_RX_Register is record
      --  unspecified
      Reserved_0_0 : HAL.Bit := 16#0#;
      --  Reception buffer address
      ADDR7_RX     : ADDR7_RX_ADDR7_RX_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for ADDR7_RX_Register use record
      Reserved_0_0 at 0 range 0 .. 0;
      ADDR7_RX     at 0 range 1 .. 15;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Universal serial bus full-speed device interface
   type USB_Peripheral is record
      --  endpoint 0 register
      EP0R      : aliased EP0R_Register;
      --  endpoint 1 register
      EP1R      : aliased EP1R_Register;
      --  endpoint 2 register
      EP2R      : aliased EP2R_Register;
      --  endpoint 3 register
      EP3R      : aliased EP3R_Register;
      --  endpoint 4 register
      EP4R      : aliased EP4R_Register;
      --  endpoint 5 register
      EP5R      : aliased EP5R_Register;
      --  endpoint 6 register
      EP6R      : aliased EP6R_Register;
      --  endpoint 7 register
      EP7R      : aliased EP7R_Register;
      --  control register
      CNTR      : aliased CNTR_Register;
      --  interrupt status register
      ISTR      : aliased ISTR_Register;
      --  frame number register
      FNR       : aliased FNR_Register;
      --  device address
      DADDR     : aliased DADDR_Register;
      --  Buffer table address
      BTABLE    : aliased BTABLE_Register;
      --  Transmission byte count 0
      COUNT0_TX : aliased COUNT0_TX_Register;
      --  LPM control and status register
      LPMCSR    : aliased LPMCSR_Register;
      --  Reception byte count 0
      COUNT0_RX : aliased COUNT0_RX_Register;
      --  Battery charging detector
      BCDR      : aliased BCDR_Register;
      --  Transmission byte count 0
      COUNT1_TX : aliased COUNT1_TX_Register;
      --  Reception buffer address 0
      ADDR0_RX  : aliased ADDR0_RX_Register;
      --  Reception byte count 0
      COUNT1_RX : aliased COUNT1_RX_Register;
      --  Transmission byte count 0
      COUNT2_TX : aliased COUNT2_TX_Register;
      --  Reception buffer address 0
      ADDR1_RX  : aliased ADDR1_RX_Register;
      --  Reception byte count 0
      COUNT2_RX : aliased COUNT2_RX_Register;
      --  Transmission byte count 0
      COUNT3_TX : aliased COUNT3_TX_Register;
      --  Reception buffer address 0
      ADDR2_RX  : aliased ADDR2_RX_Register;
      --  Reception byte count 0
      COUNT3_RX : aliased COUNT3_RX_Register;
      --  Transmission byte count 0
      COUNT4_TX : aliased COUNT4_TX_Register;
      --  Reception buffer address 0
      ADDR3_RX  : aliased ADDR3_RX_Register;
      --  Reception byte count 0
      COUNT4_RX : aliased COUNT4_RX_Register;
      --  Transmission byte count 0
      COUNT5_TX : aliased COUNT5_TX_Register;
      --  Reception buffer address 0
      ADDR4_RX  : aliased ADDR4_RX_Register;
      --  Reception byte count 0
      COUNT5_RX : aliased COUNT5_RX_Register;
      --  Transmission byte count 0
      COUNT6_TX : aliased COUNT6_TX_Register;
      --  Reception buffer address 0
      ADDR5_RX  : aliased ADDR5_RX_Register;
      --  Reception byte count 0
      COUNT6_RX : aliased COUNT6_RX_Register;
      --  Transmission byte count 0
      COUNT7_TX : aliased COUNT7_TX_Register;
      --  Reception buffer address 0
      ADDR6_RX  : aliased ADDR6_RX_Register;
      --  Reception byte count 0
      COUNT7_RX : aliased COUNT7_RX_Register;
      --  Reception buffer address 0
      ADDR7_RX  : aliased ADDR7_RX_Register;
   end record
     with Volatile;

   for USB_Peripheral use record
      EP0R      at 16#0# range 0 .. 15;
      EP1R      at 16#4# range 0 .. 15;
      EP2R      at 16#8# range 0 .. 15;
      EP3R      at 16#C# range 0 .. 15;
      EP4R      at 16#10# range 0 .. 15;
      EP5R      at 16#14# range 0 .. 15;
      EP6R      at 16#18# range 0 .. 15;
      EP7R      at 16#1C# range 0 .. 15;
      CNTR      at 16#40# range 0 .. 15;
      ISTR      at 16#44# range 0 .. 15;
      FNR       at 16#48# range 0 .. 15;
      DADDR     at 16#4C# range 0 .. 15;
      BTABLE    at 16#50# range 0 .. 15;
      COUNT0_TX at 16#52# range 0 .. 15;
      LPMCSR    at 16#54# range 0 .. 15;
      COUNT0_RX at 16#56# range 0 .. 15;
      BCDR      at 16#58# range 0 .. 15;
      COUNT1_TX at 16#5A# range 0 .. 15;
      ADDR0_RX  at 16#5C# range 0 .. 15;
      COUNT1_RX at 16#5E# range 0 .. 15;
      COUNT2_TX at 16#62# range 0 .. 15;
      ADDR1_RX  at 16#64# range 0 .. 15;
      COUNT2_RX at 16#66# range 0 .. 15;
      COUNT3_TX at 16#6A# range 0 .. 15;
      ADDR2_RX  at 16#6C# range 0 .. 15;
      COUNT3_RX at 16#6E# range 0 .. 15;
      COUNT4_TX at 16#72# range 0 .. 15;
      ADDR3_RX  at 16#74# range 0 .. 15;
      COUNT4_RX at 16#76# range 0 .. 15;
      COUNT5_TX at 16#7A# range 0 .. 15;
      ADDR4_RX  at 16#7C# range 0 .. 15;
      COUNT5_RX at 16#7E# range 0 .. 15;
      COUNT6_TX at 16#82# range 0 .. 15;
      ADDR5_RX  at 16#84# range 0 .. 15;
      COUNT6_RX at 16#86# range 0 .. 15;
      COUNT7_TX at 16#8A# range 0 .. 15;
      ADDR6_RX  at 16#8C# range 0 .. 15;
      COUNT7_RX at 16#8E# range 0 .. 15;
      ADDR7_RX  at 16#94# range 0 .. 15;
   end record;

   --  Universal serial bus full-speed device interface
   SEC_USB_Periph : aliased USB_Peripheral
     with Import, Address => System'To_Address (16#5000D400#);

   --  Universal serial bus full-speed device interface
   USB_Periph : aliased USB_Peripheral
     with Import, Address => System'To_Address (16#4000D400#);

end STM32_SVD.USB;
