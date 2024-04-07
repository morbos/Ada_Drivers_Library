--  This spec has been automatically generated from STM32L562.svd

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
     with Volatile_Full_Access, Size => 32,
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

   subtype DCR1_CSHT_Field is HAL.UInt3;
   subtype DCR1_DEVSIZE_Field is HAL.UInt5;
   subtype DCR1_MTYP_Field is HAL.UInt2;

   --  device configuration register
   type DCR1_Register is record
      --  Mode 0 / mode 3
      CKMODE         : Boolean := False;
      --  Free running clock
      FRCK           : Boolean := False;
      --  unspecified
      Reserved_2_7   : HAL.UInt6 := 16#0#;
      --  Chip-select high time
      CSHT           : DCR1_CSHT_Field := 16#0#;
      --  unspecified
      Reserved_11_15 : HAL.UInt5 := 16#0#;
      --  Device size
      DEVSIZE        : DCR1_DEVSIZE_Field := 16#0#;
      --  unspecified
      Reserved_21_23 : HAL.UInt3 := 16#0#;
      --  Memory type
      MTYP           : DCR1_MTYP_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DCR1_Register use record
      CKMODE         at 0 range 0 .. 0;
      FRCK           at 0 range 1 .. 1;
      Reserved_2_7   at 0 range 2 .. 7;
      CSHT           at 0 range 8 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
      DEVSIZE        at 0 range 16 .. 20;
      Reserved_21_23 at 0 range 21 .. 23;
      MTYP           at 0 range 24 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
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
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DCR2_Register use record
      PRESCALER      at 0 range 0 .. 7;
      Reserved_8_15  at 0 range 8 .. 15;
      WRAPSIZE       at 0 range 16 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   subtype DCR3_CSBOUND_Field is HAL.UInt5;

   --  device configuration register 3
   type DCR3_Register is record
      --  unspecified
      Reserved_0_15  : HAL.UInt16 := 16#0#;
      --  CS boundary
      CSBOUND        : DCR3_CSBOUND_Field := 16#0#;
      --  unspecified
      Reserved_21_31 : HAL.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DCR3_Register use record
      Reserved_0_15  at 0 range 0 .. 15;
      CSBOUND        at 0 range 16 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   subtype DCR4_FLEVEL_Field is HAL.UInt6;

   --  DCR4
   type DCR4_Register is record
      --  Transfer error flag
      TEF            : Boolean := False;
      --  Transfer complete flag
      TCF            : Boolean := False;
      --  FIFO threshold flag
      FTF            : Boolean := False;
      --  Status match flag
      SMF            : Boolean := False;
      --  Timeout flag
      TOF            : Boolean := False;
      --  BUSY
      BUSY           : Boolean := False;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  FIFO level
      FLEVEL         : DCR4_FLEVEL_Field := 16#0#;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DCR4_Register use record
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

   --  status register
   type SR_Register is record
      --  Write-only. Clear transfer error flag
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
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register use record
      CTEF          at 0 range 0 .. 0;
      CTCF          at 0 range 1 .. 1;
      Reserved_2_2  at 0 range 2 .. 2;
      CSMF          at 0 range 3 .. 3;
      CTOF          at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype PSMAR_INTERVAL_Field is HAL.UInt16;

   --  polling status match register
   type PSMAR_Register is record
      --  Polling interval
      INTERVAL       : PSMAR_INTERVAL_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PSMAR_Register use record
      INTERVAL       at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype PIR_IMODE_Field is HAL.UInt3;
   subtype PIR_ISIZE_Field is HAL.UInt2;
   subtype PIR_ADMODE_Field is HAL.UInt3;
   subtype PIR_ADSIZE_Field is HAL.UInt2;
   subtype PIR_ABMODE_Field is HAL.UInt3;
   subtype PIR_ABSIZE_Field is HAL.UInt2;
   subtype PIR_DMODE_Field is HAL.UInt3;

   --  polling interval register
   type PIR_Register is record
      --  Instruction mode
      IMODE          : PIR_IMODE_Field := 16#0#;
      --  Instruction double transfer rate
      IDTR           : Boolean := False;
      --  Instruction size
      ISIZE          : PIR_ISIZE_Field := 16#0#;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Address mode
      ADMODE         : PIR_ADMODE_Field := 16#0#;
      --  Address double transfer rate
      ADDTR          : Boolean := False;
      --  Address size
      ADSIZE         : PIR_ADSIZE_Field := 16#0#;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  Alternate byte mode
      ABMODE         : PIR_ABMODE_Field := 16#0#;
      --  Alternate bytes double transfer rate
      ABDTR          : Boolean := False;
      --  Alternate bytes size
      ABSIZE         : PIR_ABSIZE_Field := 16#0#;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  Data mode
      DMODE          : PIR_DMODE_Field := 16#0#;
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
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIR_Register use record
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

   subtype CCR_DCYC_Field is HAL.UInt5;

   --  communication configuration register
   type CCR_Register is record
      --  Number of dummy cycles
      DCYC           : CCR_DCYC_Field := 16#0#;
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
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCR_Register use record
      DCYC           at 0 range 0 .. 4;
      Reserved_5_27  at 0 range 5 .. 27;
      DHQC           at 0 range 28 .. 28;
      Reserved_29_29 at 0 range 29 .. 29;
      SSHIFT         at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype ABR_TIMEOUT_Field is HAL.UInt16;

   --  alternate bytes register
   type ABR_Register is record
      --  Timeout period
      TIMEOUT        : ABR_TIMEOUT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ABR_Register use record
      TIMEOUT        at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype LPTR_IMODE_Field is HAL.UInt3;
   subtype LPTR_ISIZE_Field is HAL.UInt2;
   subtype LPTR_ADMODE_Field is HAL.UInt3;
   subtype LPTR_ADSIZE_Field is HAL.UInt2;
   subtype LPTR_ABMODE_Field is HAL.UInt3;
   subtype LPTR_ABSIZE_Field is HAL.UInt2;
   subtype LPTR_DMODE_Field is HAL.UInt3;

   --  low-power timeout register
   type LPTR_Register is record
      --  Instruction mode
      IMODE          : LPTR_IMODE_Field := 16#0#;
      --  Instruction double transfer rate
      IDTR           : Boolean := False;
      --  Instruction size
      ISIZE          : LPTR_ISIZE_Field := 16#0#;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Address mode
      ADMODE         : LPTR_ADMODE_Field := 16#0#;
      --  Address double transfer rate
      ADDTR          : Boolean := False;
      --  Address size
      ADSIZE         : LPTR_ADSIZE_Field := 16#0#;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  Alternate byte mode
      ABMODE         : LPTR_ABMODE_Field := 16#0#;
      --  Alternate bytes double transfer rate
      ABDTR          : Boolean := False;
      --  Alternate bytes size
      ABSIZE         : LPTR_ABSIZE_Field := 16#0#;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  Data mode
      DMODE          : LPTR_DMODE_Field := 16#0#;
      --  alternate bytes double transfer rate
      DDTR           : Boolean := False;
      --  unspecified
      Reserved_28_28 : HAL.Bit := 16#0#;
      --  DQS enable
      DQSE           : Boolean := False;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTR_Register use record
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

   subtype WPCCR_DCYC_Field is HAL.UInt5;

   --  write communication configuration register
   type WPCCR_Register is record
      --  Number of dummy cycles
      DCYC           : WPCCR_DCYC_Field := 16#0#;
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
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for WPCCR_Register use record
      DCYC           at 0 range 0 .. 4;
      Reserved_5_27  at 0 range 5 .. 27;
      DHQC           at 0 range 28 .. 28;
      Reserved_29_29 at 0 range 29 .. 29;
      SSHIFT         at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype WPABR_TACC_Field is HAL.UInt8;
   subtype WPABR_TRWR_Field is HAL.UInt8;

   --  write alternate bytes register
   type WPABR_Register is record
      --  Latency mode
      LM             : Boolean := False;
      --  Write zero latency
      WZL            : Boolean := False;
      --  unspecified
      Reserved_2_7   : HAL.UInt6 := 16#0#;
      --  Access time
      TACC           : WPABR_TACC_Field := 16#0#;
      --  Read write recovery time
      TRWR           : WPABR_TRWR_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for WPABR_Register use record
      LM             at 0 range 0 .. 0;
      WZL            at 0 range 1 .. 1;
      Reserved_2_7   at 0 range 2 .. 7;
      TACC           at 0 range 8 .. 15;
      TRWR           at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype WCCR_REFRESH_Field is HAL.UInt16;

   --  WCCR
   type WCCR_Register is record
      --  REFRESH
      REFRESH        : WCCR_REFRESH_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for WCCR_Register use record
      REFRESH        at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype WTCR_IMODE_Field is HAL.UInt3;
   subtype WTCR_ISIZE_Field is HAL.UInt2;
   subtype WTCR_ADMODE_Field is HAL.UInt3;
   subtype WTCR_ADSIZE_Field is HAL.UInt2;
   subtype WTCR_ABMODE_Field is HAL.UInt3;
   subtype WTCR_ABSIZE_Field is HAL.UInt2;
   subtype WTCR_DMODE_Field is HAL.UInt3;

   --  WTCR
   type WTCR_Register is record
      --  IMODE
      IMODE          : WTCR_IMODE_Field := 16#0#;
      --  IDTR
      IDTR           : Boolean := False;
      --  ISIZE
      ISIZE          : WTCR_ISIZE_Field := 16#0#;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  ADMODE
      ADMODE         : WTCR_ADMODE_Field := 16#0#;
      --  ADDTR
      ADDTR          : Boolean := False;
      --  ADSIZE
      ADSIZE         : WTCR_ADSIZE_Field := 16#0#;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  ABMODE
      ABMODE         : WTCR_ABMODE_Field := 16#0#;
      --  ABDTR
      ABDTR          : Boolean := False;
      --  ABSIZE
      ABSIZE         : WTCR_ABSIZE_Field := 16#0#;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  DMODE
      DMODE          : WTCR_DMODE_Field := 16#0#;
      --  DDTR
      DDTR           : Boolean := False;
      --  unspecified
      Reserved_28_28 : HAL.Bit := 16#0#;
      --  DQSE
      DQSE           : Boolean := False;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for WTCR_Register use record
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

   subtype WIR_DCYC_Field is HAL.UInt5;

   --  WIR
   type WIR_Register is record
      --  DCYC
      DCYC          : WIR_DCYC_Field := 16#0#;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for WIR_Register use record
      DCYC          at 0 range 0 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  OctoSPI
   type OctoSPI_Peripheral is record
      --  control register
      CR    : aliased CR_Register;
      --  device configuration register
      DCR1  : aliased DCR1_Register;
      --  device configuration register 2
      DCR2  : aliased DCR2_Register;
      --  device configuration register 3
      DCR3  : aliased DCR3_Register;
      --  DCR4
      DCR4  : aliased DCR4_Register;
      --  status register
      SR    : aliased SR_Register;
      --  flag clear register
      FCR   : aliased HAL.UInt32;
      --  data length register
      DLR   : aliased HAL.UInt32;
      --  address register
      AR    : aliased HAL.UInt32;
      --  data register
      DR    : aliased HAL.UInt32;
      --  polling status mask register
      PSMKR : aliased HAL.UInt32;
      --  polling status match register
      PSMAR : aliased PSMAR_Register;
      --  polling interval register
      PIR   : aliased PIR_Register;
      --  communication configuration register
      CCR   : aliased CCR_Register;
      --  timing configuration register
      TCR   : aliased HAL.UInt32;
      --  instruction register
      IR    : aliased HAL.UInt32;
      --  alternate bytes register
      ABR   : aliased ABR_Register;
      --  low-power timeout register
      LPTR  : aliased LPTR_Register;
      --  write communication configuration register
      WPCCR : aliased WPCCR_Register;
      --  write timing configuration register
      WPTCR : aliased HAL.UInt32;
      --  write instruction register
      WPIR  : aliased HAL.UInt32;
      --  write alternate bytes register
      WPABR : aliased WPABR_Register;
      --  WCCR
      WCCR  : aliased WCCR_Register;
      --  WTCR
      WTCR  : aliased WTCR_Register;
      --  WIR
      WIR   : aliased WIR_Register;
      --  WABR
      WABR  : aliased HAL.UInt32;
      --  HyperBusTM latency configuration register
      HLCR  : aliased HAL.UInt32;
   end record
     with Volatile;

   for OctoSPI_Peripheral use record
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
   OCTOSPI1_Periph : aliased OctoSPI_Peripheral
     with Import, Address => System'To_Address (16#44021000#);

   --  OctoSPI
   SEC_OCTOSPI1_Periph : aliased OctoSPI_Peripheral
     with Import, Address => System'To_Address (16#54021000#);

end STM32_SVD.OctoSPI;
