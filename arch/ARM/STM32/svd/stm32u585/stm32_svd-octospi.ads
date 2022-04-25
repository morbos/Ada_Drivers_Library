--  This spec has been automatically generated from STM32U5xx.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.OctoSPI is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype CR_FTHRES_Field is HAL.UInt5;
   subtype CR_FMODE_Field is HAL.UInt2;

   --  control register
   type CR_Register is record
      --  Enable
      EN             : Boolean := False;
      --  Abort request
      ABORT_k        : Boolean := False;
      --  DMA enable
      DMAEN          : Boolean := False;
      --  Timeout counter enable
      TCEN           : Boolean := False;
      --  unspecified
      Reserved_4_5   : HAL.UInt2 := 16#0#;
      --  Dual-quad mode
      DQM            : Boolean := False;
      --  FLASH memory selection
      FSEL           : Boolean := False;
      --  IFO threshold level
      FTHRES         : CR_FTHRES_Field := 16#0#;
      --  unspecified
      Reserved_13_15 : HAL.UInt3 := 16#0#;
      --  Transfer error interrupt enable
      TEIE           : Boolean := False;
      --  Transfer complete interrupt enable
      TCIE           : Boolean := False;
      --  FIFO threshold interrupt enable
      FTIE           : Boolean := False;
      --  Status match interrupt enable
      SMIE           : Boolean := False;
      --  TimeOut interrupt enable
      TOIE           : Boolean := False;
      --  unspecified
      Reserved_21_21 : HAL.Bit := 16#0#;
      --  Automatic poll mode stop
      APMS           : Boolean := False;
      --  Polling match mode
      PMM            : Boolean := False;
      --  unspecified
      Reserved_24_27 : HAL.UInt4 := 16#0#;
      --  Functional mode
      FMODE          : CR_FMODE_Field := 16#0#;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      EN             at 0 range 0 .. 0;
      ABORT_k        at 0 range 1 .. 1;
      DMAEN          at 0 range 2 .. 2;
      TCEN           at 0 range 3 .. 3;
      Reserved_4_5   at 0 range 4 .. 5;
      DQM            at 0 range 6 .. 6;
      FSEL           at 0 range 7 .. 7;
      FTHRES         at 0 range 8 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      TEIE           at 0 range 16 .. 16;
      TCIE           at 0 range 17 .. 17;
      FTIE           at 0 range 18 .. 18;
      SMIE           at 0 range 19 .. 19;
      TOIE           at 0 range 20 .. 20;
      Reserved_21_21 at 0 range 21 .. 21;
      APMS           at 0 range 22 .. 22;
      PMM            at 0 range 23 .. 23;
      Reserved_24_27 at 0 range 24 .. 27;
      FMODE          at 0 range 28 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype DCR1_CSHT_Field is HAL.UInt6;
   subtype DCR1_DEVSIZE_Field is HAL.UInt5;
   subtype DCR1_MTYP_Field is HAL.UInt3;

   --  device configuration register 1
   type DCR1_Register is record
      --  Mode 0 / mode 3
      CKMODE         : Boolean := False;
      --  Free running clock
      FRCK           : Boolean := False;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  Delay block bypass
      DLYBYP         : Boolean := False;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Chip-select high time
      CSHT           : DCR1_CSHT_Field := 16#0#;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  Device size
      DEVSIZE        : DCR1_DEVSIZE_Field := 16#0#;
      --  unspecified
      Reserved_21_23 : HAL.UInt3 := 16#0#;
      --  Memory type
      MTYP           : DCR1_MTYP_Field := 16#0#;
      --  unspecified
      Reserved_27_31 : HAL.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DCR1_Register use record
      CKMODE         at 0 range 0 .. 0;
      FRCK           at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      DLYBYP         at 0 range 3 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      CSHT           at 0 range 8 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      DEVSIZE        at 0 range 16 .. 20;
      Reserved_21_23 at 0 range 21 .. 23;
      MTYP           at 0 range 24 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   subtype DCR2_PRESCALER_Field is HAL.UInt8;
   subtype DCR2_WRAPSIZE_Field is HAL.UInt3;

   --  device configuration register 2
   type DCR2_Register is record
      --  Clock prescaler
      PRESCALER      : DCR2_PRESCALER_Field := 16#0#;
      --  unspecified
      Reserved_8_15  : HAL.UInt8 := 16#0#;
      --  Wrap size
      WRAPSIZE       : DCR2_WRAPSIZE_Field := 16#0#;
      --  unspecified
      Reserved_19_31 : HAL.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DCR2_Register use record
      PRESCALER      at 0 range 0 .. 7;
      Reserved_8_15  at 0 range 8 .. 15;
      WRAPSIZE       at 0 range 16 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   subtype DCR3_MAXTRAN_Field is HAL.UInt8;
   subtype DCR3_CSBOUND_Field is HAL.UInt5;

   --  device configuration register 3
   type DCR3_Register is record
      --  Maximum transfer
      MAXTRAN        : DCR3_MAXTRAN_Field := 16#0#;
      --  unspecified
      Reserved_8_15  : HAL.UInt8 := 16#0#;
      --  CS boundary
      CSBOUND        : DCR3_CSBOUND_Field := 16#0#;
      --  unspecified
      Reserved_21_31 : HAL.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DCR3_Register use record
      MAXTRAN        at 0 range 0 .. 7;
      Reserved_8_15  at 0 range 8 .. 15;
      CSBOUND        at 0 range 16 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   subtype SR_FLEVEL_Field is HAL.UInt6;

   --  status register
   type SR_Register is record
      --  Read-only. Transfer error flag
      TEF            : Boolean;
      --  Read-only. transfer complete flag
      TCF            : Boolean;
      --  Read-only. FIFO threshold flag
      FTF            : Boolean;
      --  Read-only. status match flag
      SMF            : Boolean;
      --  Read-only. timeout flag
      TOF            : Boolean;
      --  Read-only. BUSY
      BUSY           : Boolean;
      --  unspecified
      Reserved_6_7   : HAL.UInt2;
      --  Read-only. FIFO level
      FLEVEL         : SR_FLEVEL_Field;
      --  unspecified
      Reserved_14_31 : HAL.UInt18;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register use record
      TEF            at 0 range 0 .. 0;
      TCF            at 0 range 1 .. 1;
      FTF            at 0 range 2 .. 2;
      SMF            at 0 range 3 .. 3;
      TOF            at 0 range 4 .. 4;
      BUSY           at 0 range 5 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      FLEVEL         at 0 range 8 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   --  flag clear register
   type FCR_Register is record
      --  Write-only. Clear Transfer error flag
      CTEF          : Boolean := False;
      --  Write-only. Clear transfer complete flag
      CTCF          : Boolean := False;
      --  unspecified
      Reserved_2_2  : HAL.Bit := 16#0#;
      --  Write-only. Clear status match flag
      CSMF          : Boolean := False;
      --  Write-only. Clear timeout flag
      CTOF          : Boolean := False;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FCR_Register use record
      CTEF          at 0 range 0 .. 0;
      CTCF          at 0 range 1 .. 1;
      Reserved_2_2  at 0 range 2 .. 2;
      CSMF          at 0 range 3 .. 3;
      CTOF          at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype PIR_INTERVAL_Field is HAL.UInt16;

   --  polling interval register
   type PIR_Register is record
      --  polling interval
      INTERVAL       : PIR_INTERVAL_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIR_Register use record
      INTERVAL       at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CCR_IMODE_Field is HAL.UInt3;
   subtype CCR_ISIZE_Field is HAL.UInt2;
   subtype CCR_ADMODE_Field is HAL.UInt3;
   subtype CCR_ADSIZE_Field is HAL.UInt2;
   subtype CCR_ABMODE_Field is HAL.UInt3;
   subtype CCR_ABSIZE_Field is HAL.UInt2;
   subtype CCR_DMODE_Field is HAL.UInt3;

   --  communication configuration register
   type CCR_Register is record
      --  Instruction mode
      IMODE          : CCR_IMODE_Field := 16#0#;
      --  Instruction double transfer rate
      IDTR           : Boolean := False;
      --  Instruction size
      ISIZE          : CCR_ISIZE_Field := 16#0#;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Address mode
      ADMODE         : CCR_ADMODE_Field := 16#0#;
      --  Address double transfer rate
      ADDTR          : Boolean := False;
      --  Address size
      ADSIZE         : CCR_ADSIZE_Field := 16#0#;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  Alternate byte mode
      ABMODE         : CCR_ABMODE_Field := 16#0#;
      --  Alternate bytes double transfer rate
      ABDTR          : Boolean := False;
      --  Alternate bytes size
      ABSIZE         : CCR_ABSIZE_Field := 16#0#;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  Data mode
      DMODE          : CCR_DMODE_Field := 16#0#;
      --  Alternate bytes double transfer rate
      DDTR           : Boolean := False;
      --  unspecified
      Reserved_28_28 : HAL.Bit := 16#0#;
      --  DQS enable
      DQSE           : Boolean := False;
      --  unspecified
      Reserved_30_30 : HAL.Bit := 16#0#;
      --  Send instruction only once mode
      SIOO           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCR_Register use record
      IMODE          at 0 range 0 .. 2;
      IDTR           at 0 range 3 .. 3;
      ISIZE          at 0 range 4 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      ADMODE         at 0 range 8 .. 10;
      ADDTR          at 0 range 11 .. 11;
      ADSIZE         at 0 range 12 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      ABMODE         at 0 range 16 .. 18;
      ABDTR          at 0 range 19 .. 19;
      ABSIZE         at 0 range 20 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      DMODE          at 0 range 24 .. 26;
      DDTR           at 0 range 27 .. 27;
      Reserved_28_28 at 0 range 28 .. 28;
      DQSE           at 0 range 29 .. 29;
      Reserved_30_30 at 0 range 30 .. 30;
      SIOO           at 0 range 31 .. 31;
   end record;

   subtype TCR_DCYC_Field is HAL.UInt5;

   --  timing configuration register
   type TCR_Register is record
      --  Number of dummy cycles
      DCYC           : TCR_DCYC_Field := 16#0#;
      --  unspecified
      Reserved_5_27  : HAL.UInt23 := 16#0#;
      --  Delay hold quarter cycle
      DHQC           : Boolean := False;
      --  unspecified
      Reserved_29_29 : HAL.Bit := 16#0#;
      --  Sample shift
      SSHIFT         : Boolean := False;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TCR_Register use record
      DCYC           at 0 range 0 .. 4;
      Reserved_5_27  at 0 range 5 .. 27;
      DHQC           at 0 range 28 .. 28;
      Reserved_29_29 at 0 range 29 .. 29;
      SSHIFT         at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype LPTR_TIMEOUT_Field is HAL.UInt16;

   --  low-power timeout register
   type LPTR_Register is record
      --  Timeout period
      TIMEOUT        : LPTR_TIMEOUT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTR_Register use record
      TIMEOUT        at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype WPCCR_IMODE_Field is HAL.UInt3;
   subtype WPCCR_ISIZE_Field is HAL.UInt2;
   subtype WPCCR_ADMODE_Field is HAL.UInt3;
   subtype WPCCR_ADSIZE_Field is HAL.UInt2;
   subtype WPCCR_ABMODE_Field is HAL.UInt3;
   subtype WPCCR_ABSIZE_Field is HAL.UInt2;
   subtype WPCCR_DMODE_Field is HAL.UInt3;

   --  wrap communication configuration register
   type WPCCR_Register is record
      --  Instruction mode
      IMODE          : WPCCR_IMODE_Field := 16#0#;
      --  Instruction double transfer rate
      IDTR           : Boolean := False;
      --  Instruction size
      ISIZE          : WPCCR_ISIZE_Field := 16#0#;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Address mode
      ADMODE         : WPCCR_ADMODE_Field := 16#0#;
      --  Address double transfer rate
      ADDTR          : Boolean := False;
      --  Address size
      ADSIZE         : WPCCR_ADSIZE_Field := 16#0#;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  Alternate byte mode
      ABMODE         : WPCCR_ABMODE_Field := 16#0#;
      --  Alternate bytes double transfer rate
      ABDTR          : Boolean := False;
      --  Alternate bytes size
      ABSIZE         : WPCCR_ABSIZE_Field := 16#0#;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  Data mode
      DMODE          : WPCCR_DMODE_Field := 16#0#;
      --  alternate bytes double transfer rate
      DDTR           : Boolean := False;
      --  unspecified
      Reserved_28_28 : HAL.Bit := 16#0#;
      --  DQS enable
      DQSE           : Boolean := False;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for WPCCR_Register use record
      IMODE          at 0 range 0 .. 2;
      IDTR           at 0 range 3 .. 3;
      ISIZE          at 0 range 4 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      ADMODE         at 0 range 8 .. 10;
      ADDTR          at 0 range 11 .. 11;
      ADSIZE         at 0 range 12 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      ABMODE         at 0 range 16 .. 18;
      ABDTR          at 0 range 19 .. 19;
      ABSIZE         at 0 range 20 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      DMODE          at 0 range 24 .. 26;
      DDTR           at 0 range 27 .. 27;
      Reserved_28_28 at 0 range 28 .. 28;
      DQSE           at 0 range 29 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype WPTCR_DCYC_Field is HAL.UInt5;

   --  wrap timing configuration register
   type WPTCR_Register is record
      --  Number of dummy cycles
      DCYC           : WPTCR_DCYC_Field := 16#0#;
      --  unspecified
      Reserved_5_27  : HAL.UInt23 := 16#0#;
      --  Delay hold quarter cycle
      DHQC           : Boolean := False;
      --  unspecified
      Reserved_29_29 : HAL.Bit := 16#0#;
      --  Sample shift
      SSHIFT         : Boolean := False;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for WPTCR_Register use record
      DCYC           at 0 range 0 .. 4;
      Reserved_5_27  at 0 range 5 .. 27;
      DHQC           at 0 range 28 .. 28;
      Reserved_29_29 at 0 range 29 .. 29;
      SSHIFT         at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype WCCR_IMODE_Field is HAL.UInt3;
   subtype WCCR_ISIZE_Field is HAL.UInt2;
   subtype WCCR_ADMODE_Field is HAL.UInt3;
   subtype WCCR_ADSIZE_Field is HAL.UInt2;
   subtype WCCR_ABMODE_Field is HAL.UInt3;
   subtype WCCR_ABSIZE_Field is HAL.UInt2;
   subtype WCCR_DMODE_Field is HAL.UInt3;

   --  write communication configuration register
   type WCCR_Register is record
      --  Instruction mode
      IMODE          : WCCR_IMODE_Field := 16#0#;
      --  Instruction double transfer rate
      IDTR           : Boolean := False;
      --  Instruction size
      ISIZE          : WCCR_ISIZE_Field := 16#0#;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Address mode
      ADMODE         : WCCR_ADMODE_Field := 16#0#;
      --  Address double transfer rate
      ADDTR          : Boolean := False;
      --  Address size
      ADSIZE         : WCCR_ADSIZE_Field := 16#0#;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  Alternate byte mode
      ABMODE         : WCCR_ABMODE_Field := 16#0#;
      --  Alternate bytes double transfer rate
      ABDTR          : Boolean := False;
      --  Alternate bytes size
      ABSIZE         : WCCR_ABSIZE_Field := 16#0#;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  Data mode
      DMODE          : WCCR_DMODE_Field := 16#0#;
      --  alternate bytes double transfer rate
      DDTR           : Boolean := False;
      --  unspecified
      Reserved_28_28 : HAL.Bit := 16#0#;
      --  DQS enable
      DQSE           : Boolean := False;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for WCCR_Register use record
      IMODE          at 0 range 0 .. 2;
      IDTR           at 0 range 3 .. 3;
      ISIZE          at 0 range 4 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      ADMODE         at 0 range 8 .. 10;
      ADDTR          at 0 range 11 .. 11;
      ADSIZE         at 0 range 12 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      ABMODE         at 0 range 16 .. 18;
      ABDTR          at 0 range 19 .. 19;
      ABSIZE         at 0 range 20 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      DMODE          at 0 range 24 .. 26;
      DDTR           at 0 range 27 .. 27;
      Reserved_28_28 at 0 range 28 .. 28;
      DQSE           at 0 range 29 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype WTCR_DCYC_Field is HAL.UInt5;

   --  write timing configuration register
   type WTCR_Register is record
      --  Number of dummy cycles
      DCYC          : WTCR_DCYC_Field := 16#0#;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for WTCR_Register use record
      DCYC          at 0 range 0 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype HLCR_TACC_Field is HAL.UInt8;
   subtype HLCR_TRWR_Field is HAL.UInt8;

   --  HyperBus latency configuration register
   type HLCR_Register is record
      --  Latency mode
      LM             : Boolean := False;
      --  Write zero latency
      WZL            : Boolean := False;
      --  unspecified
      Reserved_2_7   : HAL.UInt6 := 16#0#;
      --  Access time
      TACC           : HLCR_TACC_Field := 16#0#;
      --  Read write recovery time
      TRWR           : HLCR_TRWR_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for HLCR_Register use record
      LM             at 0 range 0 .. 0;
      WZL            at 0 range 1 .. 1;
      Reserved_2_7   at 0 range 2 .. 7;
      TACC           at 0 range 8 .. 15;
      TRWR           at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype CR_REQ2ACK_TIME_Field is HAL.UInt8;

   --  control register
   type CR_Register_1 is record
      --  Multiplexed mode enable
      MUXEN          : Boolean := False;
      --  unspecified
      Reserved_1_15  : HAL.UInt15 := 16#0#;
      --  REQ to ACK time
      REQ2ACK_TIME   : CR_REQ2ACK_TIME_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register_1 use record
      MUXEN          at 0 range 0 .. 0;
      Reserved_1_15  at 0 range 1 .. 15;
      REQ2ACK_TIME   at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype P1CR_IOLSRC_Field is HAL.UInt2;
   subtype P1CR_IOHSRC_Field is HAL.UInt2;

   --  OCTOSPI I/O manager Port 1 configuration register
   type P1CR_Register is record
      --  CLKEN
      CLKEN          : Boolean := True;
      --  CLKSRC
      CLKSRC         : Boolean := False;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  DQSEN
      DQSEN          : Boolean := True;
      --  DQSSRC
      DQSSRC         : Boolean := False;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  NCSEN
      NCSEN          : Boolean := True;
      --  NCSSRC
      NCSSRC         : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  IOLEN
      IOLEN          : Boolean := True;
      --  IOLSRC
      IOLSRC         : P1CR_IOLSRC_Field := 16#0#;
      --  unspecified
      Reserved_19_23 : HAL.UInt5 := 16#0#;
      --  IOHEN
      IOHEN          : Boolean := True;
      --  IOHSR
      IOHSRC         : P1CR_IOHSRC_Field := 16#1#;
      --  unspecified
      Reserved_27_31 : HAL.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for P1CR_Register use record
      CLKEN          at 0 range 0 .. 0;
      CLKSRC         at 0 range 1 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      DQSEN          at 0 range 4 .. 4;
      DQSSRC         at 0 range 5 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      NCSEN          at 0 range 8 .. 8;
      NCSSRC         at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      IOLEN          at 0 range 16 .. 16;
      IOLSRC         at 0 range 17 .. 18;
      Reserved_19_23 at 0 range 19 .. 23;
      IOHEN          at 0 range 24 .. 24;
      IOHSRC         at 0 range 25 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   subtype P2CR_IOLSRC_Field is HAL.UInt2;
   subtype P2CR_IOHSRC_Field is HAL.UInt2;

   --  OCTOSPI I/O manager Port 2 configuration register
   type P2CR_Register is record
      --  CLKEN
      CLKEN          : Boolean := True;
      --  CLKSRC
      CLKSRC         : Boolean := True;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  DQSEN
      DQSEN          : Boolean := True;
      --  DQSSRC
      DQSSRC         : Boolean := True;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  NCSEN
      NCSEN          : Boolean := True;
      --  NCSSRC
      NCSSRC         : Boolean := True;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  IOLEN
      IOLEN          : Boolean := True;
      --  IOLSRC
      IOLSRC         : P2CR_IOLSRC_Field := 16#2#;
      --  unspecified
      Reserved_19_23 : HAL.UInt5 := 16#0#;
      --  IOHEN
      IOHEN          : Boolean := True;
      --  IOHSR
      IOHSRC         : P2CR_IOHSRC_Field := 16#3#;
      --  unspecified
      Reserved_27_31 : HAL.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for P2CR_Register use record
      CLKEN          at 0 range 0 .. 0;
      CLKSRC         at 0 range 1 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      DQSEN          at 0 range 4 .. 4;
      DQSSRC         at 0 range 5 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      NCSEN          at 0 range 8 .. 8;
      NCSSRC         at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      IOLEN          at 0 range 16 .. 16;
      IOLSRC         at 0 range 17 .. 18;
      Reserved_19_23 at 0 range 19 .. 23;
      IOHEN          at 0 range 24 .. 24;
      IOHSRC         at 0 range 25 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  OctoSPI
   type OCTOSPI1_Peripheral is record
      --  control register
      CR    : aliased CR_Register;
      --  device configuration register 1
      DCR1  : aliased DCR1_Register;
      --  device configuration register 2
      DCR2  : aliased DCR2_Register;
      --  device configuration register 3
      DCR3  : aliased DCR3_Register;
      --  DCR4
      DCR4  : aliased HAL.UInt32;
      --  status register
      SR    : aliased SR_Register;
      --  flag clear register
      FCR   : aliased FCR_Register;
      --  data length register
      DLR   : aliased HAL.UInt32;
      --  address register
      AR    : aliased HAL.UInt32;
      --  data register
      DR    : aliased HAL.UInt32;
      --  polling status mask register
      PSMKR : aliased HAL.UInt32;
      --  polling status match register
      PSMAR : aliased HAL.UInt32;
      --  polling interval register
      PIR   : aliased PIR_Register;
      --  communication configuration register
      CCR   : aliased CCR_Register;
      --  timing configuration register
      TCR   : aliased TCR_Register;
      --  instruction register
      IR    : aliased HAL.UInt32;
      --  alternate bytes register
      ABR   : aliased HAL.UInt32;
      --  low-power timeout register
      LPTR  : aliased LPTR_Register;
      --  wrap communication configuration register
      WPCCR : aliased WPCCR_Register;
      --  wrap timing configuration register
      WPTCR : aliased WPTCR_Register;
      --  wrap instruction register
      WPIR  : aliased HAL.UInt32;
      --  wrap alternate bytes register
      WPABR : aliased HAL.UInt32;
      --  write communication configuration register
      WCCR  : aliased WCCR_Register;
      --  write timing configuration register
      WTCR  : aliased WTCR_Register;
      --  write instruction register
      WIR   : aliased HAL.UInt32;
      --  write alternate bytes register
      WABR  : aliased HAL.UInt32;
      --  HyperBus latency configuration register
      HLCR  : aliased HLCR_Register;
   end record
     with Volatile;

   for OCTOSPI1_Peripheral use record
      CR    at 16#0# range 0 .. 31;
      DCR1  at 16#8# range 0 .. 31;
      DCR2  at 16#C# range 0 .. 31;
      DCR3  at 16#10# range 0 .. 31;
      DCR4  at 16#14# range 0 .. 31;
      SR    at 16#20# range 0 .. 31;
      FCR   at 16#24# range 0 .. 31;
      DLR   at 16#40# range 0 .. 31;
      AR    at 16#48# range 0 .. 31;
      DR    at 16#50# range 0 .. 31;
      PSMKR at 16#80# range 0 .. 31;
      PSMAR at 16#88# range 0 .. 31;
      PIR   at 16#90# range 0 .. 31;
      CCR   at 16#100# range 0 .. 31;
      TCR   at 16#108# range 0 .. 31;
      IR    at 16#110# range 0 .. 31;
      ABR   at 16#120# range 0 .. 31;
      LPTR  at 16#130# range 0 .. 31;
      WPCCR at 16#140# range 0 .. 31;
      WPTCR at 16#148# range 0 .. 31;
      WPIR  at 16#150# range 0 .. 31;
      WPABR at 16#160# range 0 .. 31;
      WCCR  at 16#180# range 0 .. 31;
      WTCR  at 16#188# range 0 .. 31;
      WIR   at 16#190# range 0 .. 31;
      WABR  at 16#1A0# range 0 .. 31;
      HLCR  at 16#200# range 0 .. 31;
   end record;

   --  OctoSPI
   OCTOSPI1_Periph : aliased OCTOSPI1_Peripheral
     with Import, Address => OCTOSPI1_Base;

   --  OctoSPI
   OCTOSPI2_Periph : aliased OCTOSPI1_Peripheral
     with Import, Address => OCTOSPI2_Base;

   --  OctoSPI
   SEC_OCTOSPI1_Periph : aliased OCTOSPI1_Peripheral
     with Import, Address => SEC_OCTOSPI1_Base;

   --  OctoSPI
   SEC_OCTOSPI2_Periph : aliased OCTOSPI1_Peripheral
     with Import, Address => SEC_OCTOSPI2_Base;

   --  OCTOSPI I/O manager
   type OCTOSPIM_Peripheral is record
      --  control register
      CR   : aliased CR_Register_1;
      --  OCTOSPI I/O manager Port 1 configuration register
      P1CR : aliased P1CR_Register;
      --  OCTOSPI I/O manager Port 2 configuration register
      P2CR : aliased P2CR_Register;
   end record
     with Volatile;

   for OCTOSPIM_Peripheral use record
      CR   at 16#0# range 0 .. 31;
      P1CR at 16#4# range 0 .. 31;
      P2CR at 16#8# range 0 .. 31;
   end record;

   --  OCTOSPI I/O manager
   OCTOSPIM_Periph : aliased OCTOSPIM_Peripheral
     with Import, Address => OCTOSPIM_Base;

   --  OCTOSPI I/O manager
   SEC_OCTOSPIM_Periph : aliased OCTOSPIM_Peripheral
     with Import, Address => SEC_OCTOSPIM_Base;

end STM32_SVD.OctoSPI;
