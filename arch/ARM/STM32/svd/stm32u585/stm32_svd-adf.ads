--  This spec has been automatically generated from STM32U5xx.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.ADF is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  ADF Global Control Register
   type ADF_GCR_Register is record
      --  Trigger output control Set by software and reset by
      TRGO          : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADF_GCR_Register use record
      TRGO          at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   subtype ADF_CKGCR_TRGSRC_Field is HAL.UInt4;
   subtype ADF_CKGCR_CCKDIV_Field is HAL.UInt4;
   subtype ADF_CKGCR_PROCDIV_Field is HAL.UInt7;

   --  ADF clock generator control register
   type ADF_CKGCR_Register is record
      --  CKGEN dividers enable
      CKGDEN         : Boolean := False;
      --  ADF_CCK0 clock enable
      CCK0EN         : Boolean := False;
      --  ADF_CCK1 clock enable
      CCK1EN         : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Clock generator mode
      CKGMOD         : Boolean := False;
      --  ADF_CCK0 direction
      CCK0DIR        : Boolean := False;
      --  ADF_CCK1 direction
      CCK1DIR        : Boolean := False;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  CKGEN trigger sensitivity selection
      TRGSENS        : Boolean := False;
      --  unspecified
      Reserved_9_11  : HAL.UInt3 := 16#0#;
      --  Digital filter trigger signal selection
      TRGSRC         : ADF_CKGCR_TRGSRC_Field := 16#0#;
      --  Divider to control the ADF_CCK clock
      CCKDIV         : ADF_CKGCR_CCKDIV_Field := 16#0#;
      --  unspecified
      Reserved_20_23 : HAL.UInt4 := 16#0#;
      --  Divider to control the serial interface clock
      PROCDIV        : ADF_CKGCR_PROCDIV_Field := 16#0#;
      --  Clock generator active flag
      CKGACTIVE      : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADF_CKGCR_Register use record
      CKGDEN         at 0 range 0 .. 0;
      CCK0EN         at 0 range 1 .. 1;
      CCK1EN         at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      CKGMOD         at 0 range 4 .. 4;
      CCK0DIR        at 0 range 5 .. 5;
      CCK1DIR        at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      TRGSENS        at 0 range 8 .. 8;
      Reserved_9_11  at 0 range 9 .. 11;
      TRGSRC         at 0 range 12 .. 15;
      CCKDIV         at 0 range 16 .. 19;
      Reserved_20_23 at 0 range 20 .. 23;
      PROCDIV        at 0 range 24 .. 30;
      CKGACTIVE      at 0 range 31 .. 31;
   end record;

   subtype ADF_SITF0CR_SCKSRC_Field is HAL.UInt2;
   subtype ADF_SITF0CR_SITFMOD_Field is HAL.UInt2;
   subtype ADF_SITF0CR_STH_Field is HAL.UInt5;

   --  ADF serial interface control register 0
   type ADF_SITF0CR_Register is record
      --  SITFEN
      SITFEN         : Boolean := False;
      --  SCKSRC
      SCKSRC         : ADF_SITF0CR_SCKSRC_Field := 16#0#;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  SITFMOD
      SITFMOD        : ADF_SITF0CR_SITFMOD_Field := 16#0#;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  STH
      STH            : ADF_SITF0CR_STH_Field := 16#1F#;
      --  unspecified
      Reserved_13_30 : HAL.UInt18 := 16#0#;
      --  SITFACTIVE
      SITFACTIVE     : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADF_SITF0CR_Register use record
      SITFEN         at 0 range 0 .. 0;
      SCKSRC         at 0 range 1 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      SITFMOD        at 0 range 4 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      STH            at 0 range 8 .. 12;
      Reserved_13_30 at 0 range 13 .. 30;
      SITFACTIVE     at 0 range 31 .. 31;
   end record;

   subtype ADF_BSMX0CR_BSSEL_Field is HAL.UInt5;

   --  ADF bitstream matrix control register 0
   type ADF_BSMX0CR_Register is record
      --  Bitstream selection
      BSSEL         : ADF_BSMX0CR_BSSEL_Field := 16#0#;
      --  unspecified
      Reserved_5_30 : HAL.UInt26 := 16#0#;
      --  BSMX active flag
      BSMXACTIVE    : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADF_BSMX0CR_Register use record
      BSSEL         at 0 range 0 .. 4;
      Reserved_5_30 at 0 range 5 .. 30;
      BSMXACTIVE    at 0 range 31 .. 31;
   end record;

   subtype ADF_DFLT0CR_ACQMOD_Field is HAL.UInt3;
   subtype ADF_DFLT0CR_TRGSRC_Field is HAL.UInt4;
   subtype ADF_DFLT0CR_NBDIS_Field is HAL.UInt8;

   --  ADF digital filter control register 0
   type ADF_DFLT0CR_Register is record
      --  DFLT0 enable
      DFLTEN         : Boolean := False;
      --  DMA requests enable
      DMAEN          : Boolean := False;
      --  RXFIFO threshold selection
      FTH            : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  DFLT0 trigger mode
      ACQMOD         : ADF_DFLT0CR_ACQMOD_Field := 16#0#;
      --  unspecified
      Reserved_7_11  : HAL.UInt5 := 16#0#;
      --  DFLT0 trigger signal selection
      TRGSRC         : ADF_DFLT0CR_TRGSRC_Field := 16#0#;
      --  unspecified
      Reserved_16_19 : HAL.UInt4 := 16#0#;
      --  Number of samples to be discarded
      NBDIS          : ADF_DFLT0CR_NBDIS_Field := 16#0#;
      --  unspecified
      Reserved_28_29 : HAL.UInt2 := 16#0#;
      --  DFLT0 run status flag
      DFLTRUN        : Boolean := False;
      --  DFLT0 active flag
      DFLTACTIVE     : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADF_DFLT0CR_Register use record
      DFLTEN         at 0 range 0 .. 0;
      DMAEN          at 0 range 1 .. 1;
      FTH            at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      ACQMOD         at 0 range 4 .. 6;
      Reserved_7_11  at 0 range 7 .. 11;
      TRGSRC         at 0 range 12 .. 15;
      Reserved_16_19 at 0 range 16 .. 19;
      NBDIS          at 0 range 20 .. 27;
      Reserved_28_29 at 0 range 28 .. 29;
      DFLTRUN        at 0 range 30 .. 30;
      DFLTACTIVE     at 0 range 31 .. 31;
   end record;

   subtype ADF_DFLT0CICR_DATSRC_Field is HAL.UInt2;
   subtype ADF_DFLT0CICR_CICMOD_Field is HAL.UInt3;
   subtype ADF_DFLT0CICR_MCICD_Field is HAL.UInt9;
   subtype ADF_DFLT0CICR_SCALE_Field is HAL.UInt6;

   --  ADF digital filer configuration register 0
   type ADF_DFLT0CICR_Register is record
      --  Source data for the digital filter
      DATSRC         : ADF_DFLT0CICR_DATSRC_Field := 16#0#;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  Select the CIC order
      CICMOD         : ADF_DFLT0CICR_CICMOD_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  CIC decimation ratio selection
      MCICD          : ADF_DFLT0CICR_MCICD_Field := 16#0#;
      --  unspecified
      Reserved_17_19 : HAL.UInt3 := 16#0#;
      --  Scaling factor selection
      SCALE          : ADF_DFLT0CICR_SCALE_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADF_DFLT0CICR_Register use record
      DATSRC         at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      CICMOD         at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      MCICD          at 0 range 8 .. 16;
      Reserved_17_19 at 0 range 17 .. 19;
      SCALE          at 0 range 20 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype ADF_DFLT0RSFR_HPFC_Field is HAL.UInt2;

   --  ADF reshape filter configuration register 0
   type ADF_DFLT0RSFR_Register is record
      --  Reshaper filter bypass
      RSFLTBYP       : Boolean := False;
      --  unspecified
      Reserved_1_3   : HAL.UInt3 := 16#0#;
      --  Reshaper filter decimation ratio
      RSFLTD         : Boolean := False;
      --  unspecified
      Reserved_5_6   : HAL.UInt2 := 16#0#;
      --  High-pass filter bypass
      HPFBYP         : Boolean := False;
      --  High-pass filter cut-off frequency
      HPFC           : ADF_DFLT0RSFR_HPFC_Field := 16#0#;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADF_DFLT0RSFR_Register use record
      RSFLTBYP       at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      RSFLTD         at 0 range 4 .. 4;
      Reserved_5_6   at 0 range 5 .. 6;
      HPFBYP         at 0 range 7 .. 7;
      HPFC           at 0 range 8 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype ADF_DLY0CR_SKPDLY_Field is HAL.UInt7;

   --  ADF delay control register 0
   type ADF_DLY0CR_Register is record
      --  Delay to apply to a bitstream
      SKPDLY        : ADF_DLY0CR_SKPDLY_Field := 16#0#;
      --  unspecified
      Reserved_7_30 : HAL.UInt24 := 16#0#;
      --  Skip busy flag
      SKPBF         : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADF_DLY0CR_Register use record
      SKPDLY        at 0 range 0 .. 6;
      Reserved_7_30 at 0 range 7 .. 30;
      SKPBF         at 0 range 31 .. 31;
   end record;

   --  ADF DFLT0 interrupt enable register
   type ADF_DFLT0IER_Register is record
      --  RXFIFO threshold interrupt enable
      FTHIE          : Boolean := False;
      --  Data overflow interrupt enable
      DOVRIE         : Boolean := False;
      --  unspecified
      Reserved_2_8   : HAL.UInt7 := 16#0#;
      --  Saturation detection interrupt enable
      SATIE          : Boolean := False;
      --  Clock absence detection interrupt enable
      CKABIE         : Boolean := False;
      --  Reshape filter overrun interrupt enable
      RFOVRIE        : Boolean := False;
      --  Sound activity detection interrupt enable
      SDDETIE        : Boolean := False;
      --  SAD sound-level value ready enable
      SDLVLIE        : Boolean := False;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADF_DFLT0IER_Register use record
      FTHIE          at 0 range 0 .. 0;
      DOVRIE         at 0 range 1 .. 1;
      Reserved_2_8   at 0 range 2 .. 8;
      SATIE          at 0 range 9 .. 9;
      CKABIE         at 0 range 10 .. 10;
      RFOVRIE        at 0 range 11 .. 11;
      SDDETIE        at 0 range 12 .. 12;
      SDLVLIE        at 0 range 13 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   --  ADF DFLT0 interrupt status register 0
   type ADF_DFLT0ISR_Register is record
      --  Read-only. RXFIFO threshold flag
      FTHF           : Boolean := False;
      --  Data overflow flag
      DOVRF          : Boolean := False;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  Read-only. RXFIFO not empty flag
      RXNEF          : Boolean := False;
      --  unspecified
      Reserved_4_8   : HAL.UInt5 := 16#0#;
      --  Saturation detection flag
      SATF           : Boolean := False;
      --  Clock absence detection flag
      CKABF          : Boolean := False;
      --  Reshape filter overrun detection flag
      RFOVRF         : Boolean := False;
      --  Sound activity detection flag
      SDDETF         : Boolean := False;
      --  Sound level value ready flag
      SDLVLF         : Boolean := False;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADF_DFLT0ISR_Register use record
      FTHF           at 0 range 0 .. 0;
      DOVRF          at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      RXNEF          at 0 range 3 .. 3;
      Reserved_4_8   at 0 range 4 .. 8;
      SATF           at 0 range 9 .. 9;
      CKABF          at 0 range 10 .. 10;
      RFOVRF         at 0 range 11 .. 11;
      SDDETF         at 0 range 12 .. 12;
      SDLVLF         at 0 range 13 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   subtype ADF_SADCR_DATCAP_Field is HAL.UInt2;
   subtype ADF_SADCR_SADST_Field is HAL.UInt2;
   subtype ADF_SADCR_FRSIZE_Field is HAL.UInt3;
   subtype ADF_SADCR_SADMOD_Field is HAL.UInt2;

   --  ADF SAD control register
   type ADF_SADCR_Register is record
      --  Sound activity detector enable
      SADEN          : Boolean := False;
      --  Data capture mode
      DATCAP         : ADF_SADCR_DATCAP_Field := 16#0#;
      --  Sound trigger event configuration
      DETCFG         : Boolean := False;
      --  Read-only. SAD state
      SADST          : ADF_SADCR_SADST_Field := 16#0#;
      --  unspecified
      Reserved_6_6   : HAL.Bit := 16#0#;
      --  Hysteresis enable
      HYSTEN         : Boolean := False;
      --  Frame size
      FRSIZE         : ADF_SADCR_FRSIZE_Field := 16#0#;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  SAD working mode
      SADMOD         : ADF_SADCR_SADMOD_Field := 16#0#;
      --  unspecified
      Reserved_14_30 : HAL.UInt17 := 16#0#;
      --  Read-only. SAD Active flag
      SADACTIVE      : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADF_SADCR_Register use record
      SADEN          at 0 range 0 .. 0;
      DATCAP         at 0 range 1 .. 2;
      DETCFG         at 0 range 3 .. 3;
      SADST          at 0 range 4 .. 5;
      Reserved_6_6   at 0 range 6 .. 6;
      HYSTEN         at 0 range 7 .. 7;
      FRSIZE         at 0 range 8 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      SADMOD         at 0 range 12 .. 13;
      Reserved_14_30 at 0 range 14 .. 30;
      SADACTIVE      at 0 range 31 .. 31;
   end record;

   subtype ADF_SADCFGR_SNTHR_Field is HAL.UInt4;
   subtype ADF_SADCFGR_ANSLP_Field is HAL.UInt3;
   subtype ADF_SADCFGR_LFRNB_Field is HAL.UInt3;
   subtype ADF_SADCFGR_HGOVR_Field is HAL.UInt3;
   subtype ADF_SADCFGR_ANMIN_Field is HAL.UInt13;

   --  ADF SAD configuration register
   type ADF_SADCFGR_Register is record
      --  SNTHR
      SNTHR          : ADF_SADCFGR_SNTHR_Field := 16#0#;
      --  ANSLP
      ANSLP          : ADF_SADCFGR_ANSLP_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  LFRNB
      LFRNB          : ADF_SADCFGR_LFRNB_Field := 16#0#;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  Hangover time window
      HGOVR          : ADF_SADCFGR_HGOVR_Field := 16#0#;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  ANMIN
      ANMIN          : ADF_SADCFGR_ANMIN_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADF_SADCFGR_Register use record
      SNTHR          at 0 range 0 .. 3;
      ANSLP          at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      LFRNB          at 0 range 8 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      HGOVR          at 0 range 12 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      ANMIN          at 0 range 16 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype ADF_SADSDLVR_SDLVL_Field is HAL.UInt15;

   --  ADF SAD sound level register
   type ADF_SADSDLVR_Register is record
      --  Read-only. SDLVL
      SDLVL          : ADF_SADSDLVR_SDLVL_Field;
      --  unspecified
      Reserved_15_31 : HAL.UInt17;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADF_SADSDLVR_Register use record
      SDLVL          at 0 range 0 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype ADF_SADANLVR_ANLVL_Field is HAL.UInt15;

   --  ADF SAD ambient noise level register
   type ADF_SADANLVR_Register is record
      --  Read-only. ANLVL
      ANLVL          : ADF_SADANLVR_ANLVL_Field;
      --  unspecified
      Reserved_15_31 : HAL.UInt17;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADF_SADANLVR_Register use record
      ANLVL          at 0 range 0 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype ADF_DFLT0DR_DR_Field is HAL.UInt24;

   --  ADF digital filter data register 0
   type ADF_DFLT0DR_Register is record
      --  unspecified
      Reserved_0_7 : HAL.UInt8;
      --  Read-only. DR
      DR           : ADF_DFLT0DR_DR_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADF_DFLT0DR_Register use record
      Reserved_0_7 at 0 range 0 .. 7;
      DR           at 0 range 8 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  ADF1
   type ADF_Peripheral is record
      --  ADF Global Control Register
      ADF_GCR       : aliased ADF_GCR_Register;
      --  ADF clock generator control register
      ADF_CKGCR     : aliased ADF_CKGCR_Register;
      --  ADF serial interface control register 0
      ADF_SITF0CR   : aliased ADF_SITF0CR_Register;
      --  ADF bitstream matrix control register 0
      ADF_BSMX0CR   : aliased ADF_BSMX0CR_Register;
      --  ADF digital filter control register 0
      ADF_DFLT0CR   : aliased ADF_DFLT0CR_Register;
      --  ADF digital filer configuration register 0
      ADF_DFLT0CICR : aliased ADF_DFLT0CICR_Register;
      --  ADF reshape filter configuration register 0
      ADF_DFLT0RSFR : aliased ADF_DFLT0RSFR_Register;
      --  ADF delay control register 0
      ADF_DLY0CR    : aliased ADF_DLY0CR_Register;
      --  ADF DFLT0 interrupt enable register
      ADF_DFLT0IER  : aliased ADF_DFLT0IER_Register;
      --  ADF DFLT0 interrupt status register 0
      ADF_DFLT0ISR  : aliased ADF_DFLT0ISR_Register;
      --  ADF SAD control register
      ADF_SADCR     : aliased ADF_SADCR_Register;
      --  ADF SAD configuration register
      ADF_SADCFGR   : aliased ADF_SADCFGR_Register;
      --  ADF SAD sound level register
      ADF_SADSDLVR  : aliased ADF_SADSDLVR_Register;
      --  ADF SAD ambient noise level register
      ADF_SADANLVR  : aliased ADF_SADANLVR_Register;
      --  ADF digital filter data register 0
      ADF_DFLT0DR   : aliased ADF_DFLT0DR_Register;
   end record
     with Volatile;

   for ADF_Peripheral use record
      ADF_GCR       at 16#0# range 0 .. 31;
      ADF_CKGCR     at 16#4# range 0 .. 31;
      ADF_SITF0CR   at 16#80# range 0 .. 31;
      ADF_BSMX0CR   at 16#84# range 0 .. 31;
      ADF_DFLT0CR   at 16#88# range 0 .. 31;
      ADF_DFLT0CICR at 16#8C# range 0 .. 31;
      ADF_DFLT0RSFR at 16#90# range 0 .. 31;
      ADF_DLY0CR    at 16#A4# range 0 .. 31;
      ADF_DFLT0IER  at 16#AC# range 0 .. 31;
      ADF_DFLT0ISR  at 16#B0# range 0 .. 31;
      ADF_SADCR     at 16#B8# range 0 .. 31;
      ADF_SADCFGR   at 16#BC# range 0 .. 31;
      ADF_SADSDLVR  at 16#C0# range 0 .. 31;
      ADF_SADANLVR  at 16#C4# range 0 .. 31;
      ADF_DFLT0DR   at 16#F0# range 0 .. 31;
   end record;

   --  ADF1
   ADF1_Periph : aliased ADF_Peripheral
     with Import, Address => ADF1_Base;

   --  ADF1
   SEC_ADF1_Periph : aliased ADF_Peripheral
     with Import, Address => SEC_ADF1_Base;

end STM32_SVD.ADF;
