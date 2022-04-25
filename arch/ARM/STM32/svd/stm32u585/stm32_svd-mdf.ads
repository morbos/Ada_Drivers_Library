--  This spec has been automatically generated from STM32U5xx.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.MDF is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype GCR_ILVNB_Field is HAL.UInt4;

   --  MDF global control register
   type GCR_Register is record
      --  TRGO
      TRGO          : Boolean := False;
      --  unspecified
      Reserved_1_3  : HAL.UInt3 := 16#0#;
      --  ILVNB
      ILVNB         : GCR_ILVNB_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GCR_Register use record
      TRGO          at 0 range 0 .. 0;
      Reserved_1_3  at 0 range 1 .. 3;
      ILVNB         at 0 range 4 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype CKGCR_TRGSRC_Field is HAL.UInt4;
   subtype CKGCR_CCKDIV_Field is HAL.UInt4;
   subtype CKGCR_PROCDIV_Field is HAL.UInt7;

   --  MDF clock generator control register
   type CKGCR_Register is record
      --  CKGDEN
      CKGDEN         : Boolean := False;
      --  CCK0EN
      CCK0EN         : Boolean := False;
      --  CCK1EN
      CCK1EN         : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  CKGMOD
      CKGMOD         : Boolean := False;
      --  CCK0DIR
      CCK0DIR        : Boolean := False;
      --  CCK1DIR
      CCK1DIR        : Boolean := False;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  TRGSENS
      TRGSENS        : Boolean := False;
      --  unspecified
      Reserved_9_11  : HAL.UInt3 := 16#0#;
      --  TRGSRC
      TRGSRC         : CKGCR_TRGSRC_Field := 16#0#;
      --  CCKDIV
      CCKDIV         : CKGCR_CCKDIV_Field := 16#0#;
      --  unspecified
      Reserved_20_23 : HAL.UInt4 := 16#0#;
      --  PROCDIV
      PROCDIV        : CKGCR_PROCDIV_Field := 16#0#;
      --  CKGACTIVE
      CKGACTIVE      : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CKGCR_Register use record
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

   subtype MDF_SITF0CR_SCKSRC_Field is HAL.UInt2;
   subtype MDF_SITF0CR_SITFMOD_Field is HAL.UInt2;
   subtype MDF_SITF0CR_STH_Field is HAL.UInt5;

   --  This register is used to control the serial interfaces (SITFx).
   type MDF_SITF0CR_Register is record
      --  Serial interface enable Set and cleared by software. This bit is used
      --  to enable/disable the serial interface. - 0: Serial interface
      --  disabled - 1: Serial interface enabled
      SITFEN         : Boolean := False;
      --  Serial clock source Set and cleared by software. This bit is used to
      --  select the clock source of the serial interface. - 00: Serial clock
      --  source is MDF_CCK0 - 01: Serial clock source is MDF_CCK1 1x: Serial
      --  clock source is MDF_CKIx, not allowed in LF_MASTER SPI mode This
      --  field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      SCKSRC         : MDF_SITF0CR_SCKSRC_Field := 16#0#;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Serial interface type Set and cleared by software. This field is used
      --  to defined the serial interface type. - 00: LF_MASTER (Low-Frequency
      --  MASTER) SPI mode - 01: Normal SPI mode - 10: Manchester mode: rising
      --  edge = logic 0, falling edge = logic 1 - 11: Manchester mode: rising
      --  edge = logic 1, falling edge = logic 0 This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      SITFMOD        : MDF_SITF0CR_SITFMOD_Field := 16#0#;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Manchester Symbol threshold / SPI threshold Set and cleared by
      --  software. This field is used for Manchester mode, in order to define
      --  the expected symbol threshold levels. Please refer to Section :
      --  Manchester mode for details on computation. In addition this field is
      --  used to define the timeout value for the clock absence detection in
      --  Normal SPI mode. Values of STH[4:0] lower than 4 are invalid. This
      --  field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      STH            : MDF_SITF0CR_STH_Field := 16#1F#;
      --  unspecified
      Reserved_13_30 : HAL.UInt18 := 16#0#;
      --  Read-only. Serial interface Active flag Set and cleared by hardware.
      --  This flag must be used by the application in order to check if the
      --  serial interface is effectively enabled (active) or not. The
      --  protected fields of this function can only be updated when the
      --  SITFACTIVE is set , please refer to Section 1.4.15: Register
      --  protection for details. The delay between a transition on SITFEN and
      --  a transition on SITFACTIVE is 2 periods of AHB clock and 2 periods of
      --  mdf_proc_ck. - 0: The serial interface is not active, and can be
      --  configured if needed - 1: The serial interface is active, and
      --  protected fields cannot be configured.
      SITFACTIVE     : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_SITF0CR_Register use record
      SITFEN         at 0 range 0 .. 0;
      SCKSRC         at 0 range 1 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      SITFMOD        at 0 range 4 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      STH            at 0 range 8 .. 12;
      Reserved_13_30 at 0 range 13 .. 30;
      SITFACTIVE     at 0 range 31 .. 31;
   end record;

   subtype MDF_BSMX0CR_BSSEL_Field is HAL.UInt5;

   --  This register is used to select the bitstream to be provided to the
   --  corresponding digital filter and to the SCD.
   type MDF_BSMX0CR_Register is record
      --  Bitstream Selection Set and cleared by software. This field is used
      --  to select the bitstream to be processed for the digital filter x and
      --  for the SCDx. The size of this field depends on the number of DFLTx
      --  instantiated. If the BSSEL is selecting an input which is not
      --  instantiated, the MDF will select the valid stream bs[x]_F having the
      --  higher index number. - 00000: The bitstream bs[0]_R is provided to
      --  DFLTx and SCDx - 00001: The bitstream bs[0]_F is provided to DFLTx
      --  and SCDx - 00010: The bitstream bs[1]_R is provided to DFLTx and SCDx
      --  (if instantiated) - 00011: The bitstream bs[1]_F is provided to DFLTx
      --  and SCDx (if instantiated) ... - 11110: The bitstream bs[15]_R is
      --  provided to DFLTx and SCDx (if instantiated) - 11111: The bitstream
      --  bs[15]_F is provided to DFLTx and SCDx (if instantiated) This field
      --  can be write-protected, please refer to Section 1.4.15: Register
      --  protection for details.
      BSSEL         : MDF_BSMX0CR_BSSEL_Field := 16#0#;
      --  unspecified
      Reserved_5_30 : HAL.UInt26 := 16#0#;
      --  Read-only. BSMX Active flag Set and cleared by hardware. This flag
      --  must be used by the application in order to check if the BSMX is
      --  effectively enabled (active) or not. BSSEL[4:0] can only be updated
      --  when the BSMXACTIVE is set . The BSMXACTIVE flag is a logical between
      --  OLDACTIVE, DFLTACTIVE, and SCDACTIVE flags. Both of them must be set
      --  in order update BSSEL[4:0] field. - 0: The BSMX is not active, and
      --  can be configured if needed - 1: The BSMX is active, and protected
      --  fields cannot be configured.
      BSMXACTIVE    : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_BSMX0CR_Register use record
      BSSEL         at 0 range 0 .. 4;
      Reserved_5_30 at 0 range 5 .. 30;
      BSMXACTIVE    at 0 range 31 .. 31;
   end record;

   subtype MDF_DFLT0CR_ACQMOD_Field is HAL.UInt3;
   subtype MDF_DFLT0CR_TRGSRC_Field is HAL.UInt4;
   subtype MDF_DFLT0CR_NBDIS_Field is HAL.UInt8;

   --  This register is used to control the digital filter x.
   type MDF_DFLT0CR_Register is record
      --  Write-only. Digital Filter Enable Set and cleared by software. This
      --  bit is used to control the start of acquisition of the corresponding
      --  digital filter path. The behavior of this bit depends on ACQMOD and
      --  external events. or the acquisition starts when the proper trigger
      --  event occurs if ACQMOD = 01x . The serial or parallel interface
      --  delivering the samples shall be enabled as well. - 0: The acquisition
      --  is stopped immediately - 1: The acquisition is immediately started if
      --  ACQMOD = 00x or 1xx ,
      DFLTEN         : Boolean := False;
      --  DMA Requests Enable Set and cleared by software. This bit is used to
      --  control the generation of DMA request in order to transfer the
      --  processed samples into the memory. - 0: The DMA interface for the
      --  corresponding digital filter is disabled - 1: The DMA interface for
      --  the corresponding digital filter is enabled This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      DMAEN          : Boolean := False;
      --  RXFIFO Threshold selection Set and cleared by software.
      FTH            : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Digital filter Trigger mode Set and cleared by software. This field
      --  is used to select the filter trigger mode. - 000: Asynchronous,
      --  continuous acquisition mode - 001: Asynchronous, single-shot
      --  acquisition mode - 010: Synchronous, continuous acquisition mode -
      --  011: Synchronous, single-shot acquisition mode - 100: Window,
      --  continuous acquisition mode - 101: Synchronous, snapshot mode others:
      --  same a 000 This field can be write-protected, please refer to Section
      --  1.4.15: Register protection for details.
      ACQMOD         : MDF_DFLT0CR_ACQMOD_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Digital filter Trigger sensitivity selection Set and cleared by
      --  software. This field is used to select the trigger sensitivity of the
      --  external signals - 0: A rising edge event triggers the acquisition -
      --  1: A falling edge even triggers the acquisition Note that when the
      --  trigger source is TRGO or OLDx event, TRGSENS value is not taken into
      --  account. When TRGO is selected, the sensitivity is forced to falling
      --  edge, when OLDx event is selected, the sensitivity is forced to
      --  rising edge. This field can be write-protected, please refer to
      --  Section 1.4.15: Register protection for details.
      TRGSENS        : Boolean := False;
      --  unspecified
      Reserved_9_11  : HAL.UInt3 := 16#0#;
      --  Digital filter Trigger signal selection, Set and cleared by software.
      --  This field is used to select which external signals is used as
      --  trigger for the corresponding filter. - 0000: TRGO is selected -
      --  0001: OLDx event is selected - 0010: mdf_trg[0] is selected ... -
      --  1111: mdf_trg[13] is selected This field can be write-protected,
      --  please refer to Section 1.4.15: Register protection for details.
      TRGSRC         : MDF_DFLT0CR_TRGSRC_Field := 16#0#;
      --  Snapshot data format Set and cleared by software. This field is used
      --  to select the data format for the snapshot mode. - 0: The integrator
      --  counter (INT_CNT) is not inserted into the MDF_SNPSxDR register,
      --  leaving a data resolution of 23 bits. - 1: The integrator counter
      --  (INT_CNT) is inserted at position [15:9] of MDF_SNPSxDR register,
      --  leaving a data resolution of 16 bits. This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      SNPSFMT        : Boolean := False;
      --  unspecified
      Reserved_17_19 : HAL.UInt3 := 16#0#;
      --  Number of samples to be discarded Set and cleared by software. This
      --  field is used to define the number of samples to be discarded every
      --  time the DFLTx is re-started. - 0: no sample discarded - 1: 1 sample
      --  discarded - 2: 2 samples discarded ... - 255: 255 samples discarded
      --  This field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      NBDIS          : MDF_DFLT0CR_NBDIS_Field := 16#0#;
      --  unspecified
      Reserved_28_29 : HAL.UInt2 := 16#0#;
      --  Read-only. Digital filter Run Status Flag Set and cleared by
      --  hardware. This bit indicates if the digital filter is running or not.
      --  - 0: The digital filter is not running, and ready to accept a new
      --  trigger event - 1: The digital filter is running
      DFLTRUN        : Boolean := False;
      --  Read-only. Digital filter Active Flag Set and cleared by hardware.
      --  This bit indicates if the digital filter is active: can be running or
      --  waiting for events. - 0: The digital filter is not active, and can be
      --  re-enabled again (via DFLTEN bit) if needed - 1: The digital filter
      --  is active
      DFLTACTIVE     : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT0CR_Register use record
      DFLTEN         at 0 range 0 .. 0;
      DMAEN          at 0 range 1 .. 1;
      FTH            at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      ACQMOD         at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      TRGSENS        at 0 range 8 .. 8;
      Reserved_9_11  at 0 range 9 .. 11;
      TRGSRC         at 0 range 12 .. 15;
      SNPSFMT        at 0 range 16 .. 16;
      Reserved_17_19 at 0 range 17 .. 19;
      NBDIS          at 0 range 20 .. 27;
      Reserved_28_29 at 0 range 28 .. 29;
      DFLTRUN        at 0 range 30 .. 30;
      DFLTACTIVE     at 0 range 31 .. 31;
   end record;

   subtype MDF_DFLT0CICR_DATSRC_Field is HAL.UInt2;
   subtype MDF_DFLT0CICR_CICMOD_Field is HAL.UInt3;
   subtype MDF_DFLT0CICR_MCICD_Field is HAL.UInt9;
   subtype MDF_DFLT0CICR_SCALE_Field is HAL.UInt6;

   --  This register is used to control the main CIC filter.
   type MDF_DFLT0CICR_Register is record
      --  Source data for the digital filter Set and cleared by software. 0x:
      --  Select the stream coming from the BSMX - 10: Select the stream coming
      --  from the ADCITF1 - 11: Select the stream coming from the ADCITF2 This
      --  field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      DATSRC         : MDF_DFLT0CICR_DATSRC_Field := 16#0#;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  Select the CIC mode Set and cleared by software. This field allows
      --  the application to select the configuration and the order of the
      --  MCIC. When CICMOD[2:0] is equal to 0xx , the CIC is split into two
      --  filters: - The main CIC (MCIC) - The auxiliary CIC (ACIC), used for
      --  the out-off limit detector - 000: The CIC is split into 2 filters,
      --  and the main CIC (MCIC) is configured in FastSinc filter - 001: The
      --  CIC is split into 2 filters, and the main CIC (MCIC) is configured in
      --  Sinc1 filter - 010: The CIC is split into 2 filters, and the main CIC
      --  (MCIC) is configured in Sinc2 filter - 011: The CIC is split into 2
      --  filters, and the main CIC (MCIC) is configured in Sinc3 filter - 100:
      --  The CIC is configured in single sinc4 filter others: The CIC is
      --  configured in single sinc5 filter This field can be write-protected,
      --  please refer to Section 1.4.15: Register protection for details.
      CICMOD         : MDF_DFLT0CICR_CICMOD_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  CIC decimation ratio selection Set and cleared by software. This bit
      --  is used to allow the application to select the decimation ratio of
      --  the CIC. Decimation ratio smaller than 2 is not allowed. The
      --  decimation ratio is given by (CICDEC+1). - 0: Decimation ratio is 2 -
      --  1: Decimation ratio is 2 - 2: Decimation ratio is 3 - 3: Decimation
      --  ratio is 4 ... - 511: Decimation ratio is 512 This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      MCICD          : MDF_DFLT0CICR_MCICD_Field := 16#0#;
      --  unspecified
      Reserved_17_19 : HAL.UInt3 := 16#0#;
      --  Scaling factor selection Set and cleared by software. This field is
      --  used to allow the application to select the gain to be applied at CIC
      --  output. Please refer to Table 13: Possible gain values for details.
      --  If the application attempts to write a new gain value while the
      --  previous one is not yet applied, this new gain value is ignored.
      --  Reading back the SCALE[5:0] field will inform the application on the
      --  current gain value. - 100000: - 48.2 dB, or shift right by 8 bits
      --  (default value) - 100001: - 44.6 dB, - 100010: - 42.1 dB, or shift
      --  right by 7 bits - 100011: - 38.6 dB, ... - 101110: -6 dB, or shift
      --  right by 1 bit - 101111: -2.5 dB, - 000000: 0 dB - 000001: + 3.5 dB,
      --  - 000010: + 6 dB, or shift left by 1 bit ... - 011000: + 72 dB, or
      --  shift left by 12 bits
      SCALE          : MDF_DFLT0CICR_SCALE_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT0CICR_Register use record
      DATSRC         at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      CICMOD         at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      MCICD          at 0 range 8 .. 16;
      Reserved_17_19 at 0 range 17 .. 19;
      SCALE          at 0 range 20 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype MDF_DFLT0RSFR_HPFC_Field is HAL.UInt2;

   --  This register is used to control the reshape and HPF filters.
   type MDF_DFLT0RSFR_Register is record
      --  Reshaper filter bypass Set and cleared by software. This bit is used
      --  to bypass the reshape filter and its decimation block. - 0: The
      --  reshape filter is not bypassed (Default value) - 1: The reshape
      --  filter is bypassed This field can be write-protected, please refer to
      --  Section 1.4.15: Register protection for details.
      RSFLTBYP       : Boolean := False;
      --  unspecified
      Reserved_1_3   : HAL.UInt3 := 16#0#;
      --  Reshaper filter decimation ratio Set and cleared by software. This
      --  bit is used to select the decimation ratio for the reshape filter -
      --  0: Decimation ratio is 4 (Default value) - 1: Decimation ratio is 1
      --  This field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      RSFLTD         : Boolean := False;
      --  unspecified
      Reserved_5_6   : HAL.UInt2 := 16#0#;
      --  High-Pass Filter bypass Set and cleared by software. This bit is used
      --  to bypass the high-pass filter. - 0: The high pass filter is not
      --  bypassed (Default value) - 1: The high pass filter is bypassed This
      --  field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      HPFBYP         : Boolean := False;
      --  High-pass filter cut-off frequency Set and cleared by software. This
      --  field is used to select the cut-off frequency of the high-pass
      --  filter. FPCM represents the sampling frequency at HPF input. - 00:
      --  Cut-off frequency = 0.000625 x FPCM - 01: Cut-off frequency = 0.00125
      --  x FPCM - 10: Cut-off frequency = 0.00250 x FPCM - 11: Cut-off
      --  frequency = 0.00950 x FPCM This field can be write-protected, please
      --  refer to Section 1.4.15: Register protection for details.
      HPFC           : MDF_DFLT0RSFR_HPFC_Field := 16#0#;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT0RSFR_Register use record
      RSFLTBYP       at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      RSFLTD         at 0 range 4 .. 4;
      Reserved_5_6   at 0 range 5 .. 6;
      HPFBYP         at 0 range 7 .. 7;
      HPFC           at 0 range 8 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype MDF_DFLT0INTR_INTDIV_Field is HAL.UInt2;
   subtype MDF_DFLT0INTR_INTVAL_Field is HAL.UInt7;

   --  This register is used to the integrator (INT) settings.
   type MDF_DFLT0INTR_Register is record
      --  Integrator output division Set and cleared by software. This bit is
      --  used to rescale the signal at the integrator output in order keep the
      --  data width lower than 24 bits. - 00: The integrator data outputs are
      --  divided by 128 (Default value) - 01: The integrator data outputs are
      --  divided by 32 - 10: The integrator data outputs are divided by 4 -
      --  11: The integrator data outputs are not divided This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      INTDIV         : MDF_DFLT0INTR_INTDIV_Field := 16#0#;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  Integration value selection Set and cleared by software. This field
      --  is used to select the integration value. - 0: The integration value
      --  is 1, meaning bypass mode (default after reset) - 1: The integration
      --  value is 2 - 2: The integration value is 3 ... - 127: The integration
      --  value is 128 This field can be write-protected, please refer to
      --  Section 1.4.15: Register protection for details.
      INTVAL         : MDF_DFLT0INTR_INTVAL_Field := 16#0#;
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT0INTR_Register use record
      INTDIV         at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      INTVAL         at 0 range 4 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   subtype MDF_OLD0CR_BKOLD_Field is HAL.UInt4;
   subtype MDF_OLD0CR_ACICN_Field is HAL.UInt2;
   subtype MDF_OLD0CR_ACICD_Field is HAL.UInt5;

   --  This register is used to configure the Out-of Limit Detector function.
   type MDF_OLD0CR_Register is record
      --  Over-Current Detector Enable Set and cleared by software. - 0: The
      --  OLD is disabled (Default value) - 1: The OLD is enabled, including
      --  the ACIC filter working in continuous mode.
      OLDEN          : Boolean := False;
      --  Threshold In band Set and cleared by software. - 0: The OLD generates
      --  an event if the signal is lower than OLDTHL OR higher than OLDTHH
      --  (Default value) - 1: The OLD generates an event if the signal is
      --  lower than OLDTHH AND higher than OLDTHL This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      THINB          : Boolean := False;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  Break signal assignment for out-of limit detector Set and cleared by
      --  software. BKOLD[i] = 0: Break signal (mdf_break[i]) is not assigned
      --  to threshold event BKOLD[i] = 1: Break signal (mdf_break[i]) is
      --  assigned to threshold event This field can be write-protected, please
      --  refer to Section 1.4.15: Register protection for details.
      BKOLD          : MDF_OLD0CR_BKOLD_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : HAL.UInt4 := 16#0#;
      --  OLD CIC order selection Set and cleared by software. This field
      --  allows the application to select the type, and the order of the ACIC.
      --  This field is only taken into account by the MDF when CICMOD[2:0] =
      --  0xx . - 00: FastSinc filter type - 01: Sinc1 filter type - 10: Sinc2
      --  filter type - 11: Sinc3 filter type This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      ACICN          : MDF_OLD0CR_ACICN_Field := 16#0#;
      --  unspecified
      Reserved_14_16 : HAL.UInt3 := 16#0#;
      --  OLD CIC decimation ratio selection Set and cleared by software. This
      --  field is used to allow the application to select the decimation ratio
      --  of the ACIC. This field is only taken into account by the MDF when
      --  CICMOD[2:0] = 0xx . The decimation ratio is given by (ACICD+1). - 0:
      --  Decimation ratio is 1 - 1: Decimation ratio is 2 - 2: Decimation
      --  ratio is 3 - 3: Decimation ratio is 4 ... - 31: Decimation ratio is
      --  32 This field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      ACICD          : MDF_OLD0CR_ACICD_Field := 16#0#;
      --  unspecified
      Reserved_22_30 : HAL.UInt9 := 16#0#;
      --  Read-only. OLD Active flag Set and cleared by hardware. This flag
      --  must be used by the application in order to check if the OLD is
      --  effectively enabled (active) or not. The protected fields and
      --  registers of this function can only be updated when the OLDACTIVE is
      --  set to , please refer to Section 1.4.15: Register protection for
      --  details. The delay between a transition on OLDEN and a transition on
      --  OLDACTIVE is 2 periods of AHB clock and 2 periods of mdf_proc_ck. -
      --  0: The OLD is not active, and can be configured if needed - 1: The
      --  OLD is active, and protected fields cannot be configured.
      OLDACTIVE      : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_OLD0CR_Register use record
      OLDEN          at 0 range 0 .. 0;
      THINB          at 0 range 1 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      BKOLD          at 0 range 4 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      ACICN          at 0 range 12 .. 13;
      Reserved_14_16 at 0 range 14 .. 16;
      ACICD          at 0 range 17 .. 21;
      Reserved_22_30 at 0 range 22 .. 30;
      OLDACTIVE      at 0 range 31 .. 31;
   end record;

   subtype MDF_OLD0THLR_OLDTHL_Field is HAL.UInt26;

   --  This register is used for the adjustment of the Out-off Limit low
   --  threshold.
   type MDF_OLD0THLR_Register is record
      --  OLD Low Threshold Value Set and cleared by software. OLDTHL
      --  represents a 26-bit signed value. The real threshold compared to the
      --  signal provided by the filter is OLDTHL. This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      OLDTHL         : MDF_OLD0THLR_OLDTHL_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_OLD0THLR_Register use record
      OLDTHL         at 0 range 0 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype MDF_OLD0THHR_OLDTHH_Field is HAL.UInt26;

   --  This register is used for the adjustment of the Out-off Limit high
   --  threshold.
   type MDF_OLD0THHR_Register is record
      --  OLD High Threshold Value Set and cleared by software. OLDTHH
      --  represents a 26-bit signed value. The real threshold compared to the
      --  signal provided by the filter is OLDTHH. This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details
      OLDTHH         : MDF_OLD0THHR_OLDTHH_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_OLD0THHR_Register use record
      OLDTHH         at 0 range 0 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype MDF_DLY0CR_SKPDLY_Field is HAL.UInt7;

   --  This register is used for the adjustment stream delays.
   type MDF_DLY0CR_Register is record
      --  Delay to apply to a bitstream Set and cleared by software. Defines
      --  the number of input samples that will be skipped. Skipping is applied
      --  immediately after writing to this field, if SKPBF = 0 , and the
      --  corresponding bit DFLTEN = 1 . If SKPBF = 1 the value written into
      --  the register is ignored by the delay state machine. - 0: No input
      --  sample skipped, - 1: 1 input sample skipped, ... - 127: 127 input
      --  sample skipped,
      SKPDLY        : MDF_DLY0CR_SKPDLY_Field := 16#0#;
      --  unspecified
      Reserved_7_30 : HAL.UInt24 := 16#0#;
      --  Read-only. Skip Busy flag Set and cleared by hardware. Shall be used
      --  in order to control if the delay sequence is completed. - 0: Reading
      --  0 means that the MDF is ready to accept a new value into SKPDLY[6:0].
      --  - 1: Reading 1 means that last valid SKPDLY[6:0] is still under
      --  precessing.
      SKPBF         : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DLY0CR_Register use record
      SKPDLY        at 0 range 0 .. 6;
      Reserved_7_30 at 0 range 7 .. 30;
      SKPBF         at 0 range 31 .. 31;
   end record;

   subtype MDF_SCD0CR_BKSCD_Field is HAL.UInt4;
   subtype MDF_SCD0CR_SCDT_Field is HAL.UInt8;

   --  This register is used for the adjustment stream delays.
   type MDF_SCD0CR_Register is record
      --  Short circuit detector enable Set and cleared by software. - 0: The
      --  short circuit detector is disabled, - 1: The short circuit detector
      --  is enabled,
      SCDEN          : Boolean := False;
      --  unspecified
      Reserved_1_3   : HAL.UInt3 := 16#0#;
      --  Break signal assignment for short circuit detector Set and cleared by
      --  software. BKSCD[i] = 0: Break signal (mdf_break[i]) is not assigned
      --  to this SCD event BKSCD[i] = 1: Break signal (mdf_break[i]) is
      --  assigned to this SCD event This field can be write-protected, please
      --  refer to Section 1.4.15: Register protection for details.
      BKSCD          : MDF_SCD0CR_BKSCD_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : HAL.UInt4 := 16#0#;
      --  Short-circuit detector threshold Set and cleared by software. These
      --  bits are written by software to define the threshold counter for the
      --  short-circuit detector. If this value is reached, then a
      --  short-circuit detector event occurs on a given input stream. - 0: 2
      --  consecutive 1 s or 0 s will generate an event, - 1: 2 consecutive 1 s
      --  or 0 s will generate an event - 2: 3 consecutive 1 s or 0 s will
      --  generate an event, ... - 255: 256 consecutive 1 s or 0 s will
      --  generate an event, This field can be write-protected, please refer to
      --  Section 1.4.15: Register protection for details.
      SCDT           : MDF_SCD0CR_SCDT_Field := 16#0#;
      --  unspecified
      Reserved_20_30 : HAL.UInt11 := 16#0#;
      --  Read-only. SCD Active flag Set and cleared by hardware. This flag
      --  must be used by the application in order to check if the SCD is
      --  effectively enabled (active) or not. The protected fields of this
      --  function can only be updated when the SCDACTIVE is set to a , please
      --  refer to Section 1.4.15: Register protection for details. The delay
      --  between a transition on SCDEN and a transition on SCDACTIVE is 2
      --  periods of AHB clock and 2 periods of mdf_proc_ck. - 0: The SCD is
      --  not active, and can be configured if needed - 1: The SCD is active,
      --  and protected fields cannot be configured.
      SCDACTIVE      : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_SCD0CR_Register use record
      SCDEN          at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      BKSCD          at 0 range 4 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      SCDT           at 0 range 12 .. 19;
      Reserved_20_30 at 0 range 20 .. 30;
      SCDACTIVE      at 0 range 31 .. 31;
   end record;

   --  This register is used for allowing or not the events to generate an
   --  interrupt.
   type MDF_DFLT0IER_Register is record
      --  RXFIFO threshold interrupt enable Set and cleared by software. - 0:
      --  RXFIFO threshold interrupt disabled - 1: RXFIFO threshold interrupt
      --  enabled
      FTHIE          : Boolean := False;
      --  Data overflow interrupt enable Set and cleared by software. - 0: Data
      --  overflow interrupt disabled - 1: Data overflow interrupt enabled
      DOVRIE         : Boolean := False;
      --  Snapshot data ready interrupt enable Set and cleared by software. -
      --  0: Snapshot data ready interrupt disabled - 1: Snapshot data ready
      --  interrupt enabled
      SSDRIE         : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Out-of Limit interrupt enable Set and cleared by software. - 0: OLD
      --  event interrupt disabled - 1: OLD event interrupt enabled
      OLDIE          : Boolean := False;
      --  unspecified
      Reserved_5_6   : HAL.UInt2 := 16#0#;
      --  Snapshot overrun interrupt enable Set and cleared by software. - 0:
      --  Snapshot overrun interrupt disabled - 1: Snapshot overrun interrupt
      --  enabled
      SSOVRIE        : Boolean := False;
      --  Short-Circuit Detector interrupt enable Set and cleared by software.
      --  - 0: SCD interrupt disabled - 1: SCD interrupt enabled
      SCDIE          : Boolean := False;
      --  Saturation detection interrupt enable Set and cleared by software. -
      --  0: Saturation interrupt disabled - 1: Saturation interrupt enabled
      SATIE          : Boolean := False;
      --  Clock absence detection interrupt enable Set and cleared by software.
      --  - 0: Clock absence interrupt disabled - 1: Clock absence interrupt
      --  enabled
      CKABIE         : Boolean := False;
      --  Reshape Filter Overrun interrupt enable Set and cleared by software.
      --  - 0: Reshape filter overrun interrupt disabled - 1: Reshape filter
      --  overrun interrupt enabled
      RFOVRIE        : Boolean := False;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT0IER_Register use record
      FTHIE          at 0 range 0 .. 0;
      DOVRIE         at 0 range 1 .. 1;
      SSDRIE         at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      OLDIE          at 0 range 4 .. 4;
      Reserved_5_6   at 0 range 5 .. 6;
      SSOVRIE        at 0 range 7 .. 7;
      SCDIE          at 0 range 8 .. 8;
      SATIE          at 0 range 9 .. 9;
      CKABIE         at 0 range 10 .. 10;
      RFOVRIE        at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   --  MDF DFLT0 interrupt status register 0
   type MDF_DFLT0ISR_Register is record
      --  Read-only. FTHF
      FTHF           : Boolean := False;
      --  Data overflow flag Set by hardware, and cleared by software by
      --  writing this bit to 1 . - 0: Reading 0 means that no overflow is
      --  detected, writing 0 has no effect. - 1: Reading 1 means that an
      --  overflow is detected, writing 1 clears this flag.
      DOVRF          : Boolean := False;
      --  Snapshot data ready flag Set by hardware, and cleared by software by
      --  writing this bit to 1 . - 0: Reading 0 means that no data is
      --  available on , writing 0 has no effect. - 1: Reading 1 means that a
      --  new data is available on , writing 1 clears this flag.
      SSDRF          : Boolean := False;
      --  Read-only. RXFIFO Not Empty flag Set and cleared by hardware
      --  according to the RXFIFO level. - 0: Reading 0 means that the RXFIFO
      --  is empty. - 1: Reading 1 means that the RXFIFO is not empty.
      RXNEF          : Boolean := False;
      --  Out-of Limit Detector flag Set by hardware, and cleared by software
      --  by writing this bit to 1 . - 0: Reading 0 means that no OLD event is
      --  detected, writing 0 has no effect. - 1: Reading 1 means that an OLD
      --  event is detected, writing 1 clears THHF, THLF and OLDF flags.
      OLDF           : Boolean := False;
      --  Read-only. Low threshold status flag Set by hardware, and cleared by
      --  software by writing this bit to 1 . This flag indicates the status of
      --  the low threshold comparator when the last OLD event occurred. This
      --  bit gives additional information on the conditions triggering the
      --  last OLD event. It can be cleared by writing OLDF flag to a 1. - 0:
      --  The signal was higher than OLDTHL when the last OLD event occurred. -
      --  1: The signal was lower than OLDTHL when the last OLD event occurred.
      THLF           : Boolean := False;
      --  Read-only. High threshold status flag Set by hardware, and cleared by
      --  software by writing this bit to 1 . This flag indicates the status of
      --  the high threshold comparator when the last OLD event occurred. This
      --  bit gives additional information on the conditions triggering the
      --  last OLD event. It can be cleared by writing OLDF flag to a 1. - 0:
      --  The signal was lower than OLDTHH when the last OLD event occurred. -
      --  1: The signal was higher than OLDTHH when the last OLD event
      --  occurred.
      THHF           : Boolean := False;
      --  Snapshot overrun flag Set by hardware, and cleared by software by
      --  writing this bit to 1 . - 0: Reading 0 means that no snapshot overrun
      --  event is detected, writing 0 has no effect. - 1: Reading 1 means that
      --  a snapshot overrun event is detected, writing 1 clears this flag.
      SSOVRF         : Boolean := False;
      --  Short-Circuit Detector flag Set by hardware, and cleared by software
      --  by writing this bit to 1 . - 0: Reading 0 means that no SCD event is
      --  detected, writing 0 has no effect. - 1: Reading 1 means that a SCD
      --  event is detected, writing 1 clears this flag.
      SCDF           : Boolean := False;
      --  Saturation detection flag Set by hardware, and cleared by software by
      --  writing this bit to 1 . - 0: Reading 0 means that no saturation is
      --  detected, writing 0 has no effect. - 1: Reading 1 means that a
      --  saturation is detected, writing 1 clears this flag.
      SATF           : Boolean := False;
      --  Clock absence detection flag Set by hardware, and cleared by software
      --  by writing this bit to 1 . - 0: Reading 0 means that no clock absence
      --  is detected, writing 0 has no effect. - 1: Reading 1 means that a
      --  clock absence is detected, writing 1 clears this flag.
      CKABF          : Boolean := False;
      --  Reshape Filter Overrun detection flag Set by hardware, and cleared by
      --  software by writing this bit to 1 . - 0: Reading 0 means that no
      --  reshape filter overrun is detected, writing 0 has no effect. - 1:
      --  Reading 1 means that reshape filter overrun is detected, writing 1
      --  clears this flag.
      RFOVRF         : Boolean := False;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT0ISR_Register use record
      FTHF           at 0 range 0 .. 0;
      DOVRF          at 0 range 1 .. 1;
      SSDRF          at 0 range 2 .. 2;
      RXNEF          at 0 range 3 .. 3;
      OLDF           at 0 range 4 .. 4;
      THLF           at 0 range 5 .. 5;
      THHF           at 0 range 6 .. 6;
      SSOVRF         at 0 range 7 .. 7;
      SCDF           at 0 range 8 .. 8;
      SATF           at 0 range 9 .. 9;
      CKABF          at 0 range 10 .. 10;
      RFOVRF         at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype MDF_OEC0CR_OFFSET_Field is HAL.UInt26;

   --  This register contains the offset compensation value.
   type MDF_OEC0CR_Register is record
      --  Offset error compensation Set and cleared by software. If the
      --  application attempts to write a new offset value while the previous
      --  one is not yet applied, this new offset value is ignored. Reading
      --  back the OFFSET[25:0] field will inform the application on the
      --  current offset value. OFFSET[25:0] represents the value to be
      --  subtracted to the signal before going to the SCALE.
      OFFSET         : MDF_OEC0CR_OFFSET_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_OEC0CR_Register use record
      OFFSET         at 0 range 0 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype MDF_SNPS0DR_MCICDC_Field is HAL.UInt9;
   subtype MDF_SNPS0DR_EXTSDR_Field is HAL.UInt7;
   subtype MDF_SNPS0DR_SDR_Field is HAL.UInt16;

   --  This register is used to read the data processed by each digital filter
   --  in snapshot mode.
   type MDF_SNPS0DR_Register is record
      --  Read-only. Contains the MCIC decimation counter value at the moment
      --  of the last trigger event occurs (MCIC_CNT)
      MCICDC : MDF_SNPS0DR_MCICDC_Field;
      --  Read-only. Extended data size If SNPSFMT = 0 , EXTSDR[6:0] contains
      --  the bit 7 to 1 of the last valid data processed by the digital
      --  filter, If SNPSFMT = 1 , this field contains the INT accumulator
      --  counter value at the moment of the last trigger event occurs
      --  (INT_CNT).
      EXTSDR : MDF_SNPS0DR_EXTSDR_Field;
      --  Read-only. Contains the 16 MSB of the last valid data processed by
      --  the digital filter.
      SDR    : MDF_SNPS0DR_SDR_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_SNPS0DR_Register use record
      MCICDC at 0 range 0 .. 8;
      EXTSDR at 0 range 9 .. 15;
      SDR    at 0 range 16 .. 31;
   end record;

   subtype MDF_DFLT0DR_DR_Field is HAL.UInt24;

   --  This register is used to read the data processed by each digital filter.
   type MDF_DFLT0DR_Register is record
      --  unspecified
      Reserved_0_7 : HAL.UInt8;
      --  Read-only. Data processed by digital filter.
      DR           : MDF_DFLT0DR_DR_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT0DR_Register use record
      Reserved_0_7 at 0 range 0 .. 7;
      DR           at 0 range 8 .. 31;
   end record;

   subtype MDF_SITF1CR_SCKSRC_Field is HAL.UInt2;
   subtype MDF_SITF1CR_SITFMOD_Field is HAL.UInt2;
   subtype MDF_SITF1CR_STH_Field is HAL.UInt5;

   --  This register is used to control the serial interfaces (SITFx).
   type MDF_SITF1CR_Register is record
      --  Serial interface enable Set and cleared by software. This bit is used
      --  to enable/disable the serial interface. - 0: Serial interface
      --  disabled - 1: Serial interface enabled
      SITFEN         : Boolean := False;
      --  Serial clock source Set and cleared by software. This bit is used to
      --  select the clock source of the serial interface. - 00: Serial clock
      --  source is MDF_CCK0 - 01: Serial clock source is MDF_CCK1 1x: Serial
      --  clock source is MDF_CKIx, not allowed in LF_MASTER SPI mode This
      --  field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      SCKSRC         : MDF_SITF1CR_SCKSRC_Field := 16#0#;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Serial interface type Set and cleared by software. This field is used
      --  to defined the serial interface type. - 00: LF_MASTER (Low-Frequency
      --  MASTER) SPI mode - 01: Normal SPI mode - 10: Manchester mode: rising
      --  edge = logic 0, falling edge = logic 1 - 11: Manchester mode: rising
      --  edge = logic 1, falling edge = logic 0 This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      SITFMOD        : MDF_SITF1CR_SITFMOD_Field := 16#0#;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Manchester Symbol threshold / SPI threshold Set and cleared by
      --  software. This field is used for Manchester mode, in order to define
      --  the expected symbol threshold levels. Please refer to Section :
      --  Manchester mode for details on computation. In addition this field is
      --  used to define the timeout value for the clock absence detection in
      --  Normal SPI mode. Values of STH[4:0] lower than 4 are invalid. This
      --  field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      STH            : MDF_SITF1CR_STH_Field := 16#1F#;
      --  unspecified
      Reserved_13_30 : HAL.UInt18 := 16#0#;
      --  Read-only. Serial interface Active flag
      SITFACTIVE     : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_SITF1CR_Register use record
      SITFEN         at 0 range 0 .. 0;
      SCKSRC         at 0 range 1 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      SITFMOD        at 0 range 4 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      STH            at 0 range 8 .. 12;
      Reserved_13_30 at 0 range 13 .. 30;
      SITFACTIVE     at 0 range 31 .. 31;
   end record;

   subtype MDF_BSMX1CR_BSSEL_Field is HAL.UInt5;

   --  This register is used to select the bitstream to be provided to the
   --  corresponding digital filter and to the SCD.
   type MDF_BSMX1CR_Register is record
      --  Bitstream Selection Set and cleared by software. This field is used
      --  to select the bitstream to be processed for the digital filter x and
      --  for the SCDx. The size of this field depends on the number of DFLTx
      --  instantiated. If the BSSEL is selecting an input which is not
      --  instantiated, the MDF will select the valid stream bs[x]_F having the
      --  higher index number. - 00000: The bitstream bs[0]_R is provided to
      --  DFLTx and SCDx - 00001: The bitstream bs[0]_F is provided to DFLTx
      --  and SCDx - 00010: The bitstream bs[1]_R is provided to DFLTx and SCDx
      --  (if instantiated) - 00011: The bitstream bs[1]_F is provided to DFLTx
      --  and SCDx (if instantiated) ... - 11110: The bitstream bs[15]_R is
      --  provided to DFLTx and SCDx (if instantiated) - 11111: The bitstream
      --  bs[15]_F is provided to DFLTx and SCDx (if instantiated) This field
      --  can be write-protected, please refer to Section 1.4.15: Register
      --  protection for details.
      BSSEL         : MDF_BSMX1CR_BSSEL_Field := 16#0#;
      --  unspecified
      Reserved_5_30 : HAL.UInt26 := 16#0#;
      --  Read-only. BSMX Active flag Set and cleared by hardware. This flag
      --  must be used by the application in order to check if the BSMX is
      --  effectively enabled (active) or not. BSSEL[4:0] can only be updated
      --  when the BSMXACTIVE is set . The BSMXACTIVE flag is a logical between
      --  OLDACTIVE, DFLTACTIVE, and SCDACTIVE flags. Both of them must be set
      --  in order update BSSEL[4:0] field. - 0: The BSMX is not active, and
      --  can be configured if needed - 1: The BSMX is active, and protected
      --  fields cannot be configured.
      BSMXACTIVE    : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_BSMX1CR_Register use record
      BSSEL         at 0 range 0 .. 4;
      Reserved_5_30 at 0 range 5 .. 30;
      BSMXACTIVE    at 0 range 31 .. 31;
   end record;

   subtype MDF_DFLT1CR_ACQMOD_Field is HAL.UInt3;
   subtype MDF_DFLT1CR_TRGSRC_Field is HAL.UInt4;
   subtype MDF_DFLT1CR_NBDIS_Field is HAL.UInt8;

   --  This register is used to control the digital filter x.
   type MDF_DFLT1CR_Register is record
      --  Write-only. Digital Filter Enable Set and cleared by software. This
      --  bit is used to control the start of acquisition of the corresponding
      --  digital filter path. The behavior of this bit depends on ACQMOD and
      --  external events. or the acquisition starts when the proper trigger
      --  event occurs if ACQMOD = 01x . The serial or parallel interface
      --  delivering the samples shall be enabled as well. - 0: The acquisition
      --  is stopped immediately - 1: The acquisition is immediately started if
      --  ACQMOD = 00x or 1xx ,
      DFLTEN         : Boolean := False;
      --  DMA Requests Enable Set and cleared by software. This bit is used to
      --  control the generation of DMA request in order to transfer the
      --  processed samples into the memory. - 0: The DMA interface for the
      --  corresponding digital filter is disabled - 1: The DMA interface for
      --  the corresponding digital filter is enabled This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      DMAEN          : Boolean := False;
      --  RXFIFO Threshold selection Set and cleared by software. This bit is
      --  used to select the RXFIFO threshold. This bit is not significant for
      --  RXFIFOs working in interleaved transfer mode. Refer to Section
      --  1.4.13.4: Using the interleaved transfer mode for details. - 0:
      --  RXFIFO threshold event generated when the RXFIFO is not empty - 1:
      --  RXFIFO threshold event generated when the RXFIFO is half-full This
      --  field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      FTH            : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Digital filter Trigger mode Set and cleared by software. This field
      --  is used to select the filter trigger mode. - 000: Asynchronous,
      --  continuous acquisition mode - 001: Asynchronous, single-shot
      --  acquisition mode - 010: Synchronous, continuous acquisition mode -
      --  011: Synchronous, single-shot acquisition mode - 100: Window,
      --  continuous acquisition mode - 101: Synchronous, snapshot mode others:
      --  same a 000 This field can be write-protected, please refer to Section
      --  1.4.15: Register protection for details.
      ACQMOD         : MDF_DFLT1CR_ACQMOD_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Digital filter Trigger sensitivity selection Set and cleared by
      --  software. This field is used to select the trigger sensitivity of the
      --  external signals - 0: A rising edge event triggers the acquisition -
      --  1: A falling edge even triggers the acquisition Note that when the
      --  trigger source is TRGO or OLDx event, TRGSENS value is not taken into
      --  account. When TRGO is selected, the sensitivity is forced to falling
      --  edge, when OLDx event is selected, the sensitivity is forced to
      --  rising edge. This field can be write-protected, please refer to
      --  Section 1.4.15: Register protection for details.
      TRGSENS        : Boolean := False;
      --  unspecified
      Reserved_9_11  : HAL.UInt3 := 16#0#;
      --  Digital filter Trigger signal selection, Set and cleared by software.
      --  This field is used to select which external signals is used as
      --  trigger for the corresponding filter. - 0000: TRGO is selected -
      --  0001: OLDx event is selected - 0010: mdf_trg[0] is selected ... -
      --  1111: mdf_trg[13] is selected This field can be write-protected,
      --  please refer to Section 1.4.15: Register protection for details.
      TRGSRC         : MDF_DFLT1CR_TRGSRC_Field := 16#0#;
      --  Snapshot data format Set and cleared by software. This field is used
      --  to select the data format for the snapshot mode. - 0: The integrator
      --  counter (INT_CNT) is not inserted into the MDF_SNPSxDR register,
      --  leaving a data resolution of 23 bits. - 1: The integrator counter
      --  (INT_CNT) is inserted at position [15:9] of MDF_SNPSxDR register,
      --  leaving a data resolution of 16 bits. This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      SNPSFMT        : Boolean := False;
      --  unspecified
      Reserved_17_19 : HAL.UInt3 := 16#0#;
      --  Number of samples to be discarded Set and cleared by software. This
      --  field is used to define the number of samples to be discarded every
      --  time the DFLTx is re-started. - 0: no sample discarded - 1: 1 sample
      --  discarded - 2: 2 samples discarded ... - 255: 255 samples discarded
      --  This field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      NBDIS          : MDF_DFLT1CR_NBDIS_Field := 16#0#;
      --  unspecified
      Reserved_28_29 : HAL.UInt2 := 16#0#;
      --  Read-only. Digital filter Run Status Flag Set and cleared by
      --  hardware. This bit indicates if the digital filter is running or not.
      --  - 0: The digital filter is not running, and ready to accept a new
      --  trigger event - 1: The digital filter is running
      DFLTRUN        : Boolean := False;
      --  Read-only. Digital filter Active Flag Set and cleared by hardware.
      --  This bit indicates if the digital filter is active: can be running or
      --  waiting for events. - 0: The digital filter is not active, and can be
      --  re-enabled again (via DFLTEN bit) if needed - 1: The digital filter
      --  is active
      DFLTACTIVE     : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT1CR_Register use record
      DFLTEN         at 0 range 0 .. 0;
      DMAEN          at 0 range 1 .. 1;
      FTH            at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      ACQMOD         at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      TRGSENS        at 0 range 8 .. 8;
      Reserved_9_11  at 0 range 9 .. 11;
      TRGSRC         at 0 range 12 .. 15;
      SNPSFMT        at 0 range 16 .. 16;
      Reserved_17_19 at 0 range 17 .. 19;
      NBDIS          at 0 range 20 .. 27;
      Reserved_28_29 at 0 range 28 .. 29;
      DFLTRUN        at 0 range 30 .. 30;
      DFLTACTIVE     at 0 range 31 .. 31;
   end record;

   subtype MDF_DFLT1CICR_DATSRC_Field is HAL.UInt2;
   subtype MDF_DFLT1CICR_CICMOD_Field is HAL.UInt3;
   subtype MDF_DFLT1CICR_MCICD_Field is HAL.UInt9;
   subtype MDF_DFLT1CICR_SCALE_Field is HAL.UInt6;

   --  This register is used to control the main CIC filter.
   type MDF_DFLT1CICR_Register is record
      --  Source data for the digital filter Set and cleared by software. 0x:
      --  Select the stream coming from the BSMX - 10: Select the stream coming
      --  from the ADCITF1 - 11: Select the stream coming from the ADCITF2 This
      --  field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      DATSRC         : MDF_DFLT1CICR_DATSRC_Field := 16#0#;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  Select the CIC mode Set and cleared by software. This field allows
      --  the application to select the configuration and the order of the
      --  MCIC. When CICMOD[2:0] is equal to 0xx , the CIC is split into two
      --  filters: - The main CIC (MCIC) - The auxiliary CIC (ACIC), used for
      --  the out-off limit detector - 000: The CIC is split into 2 filters,
      --  and the main CIC (MCIC) is configured in FastSinc filter - 001: The
      --  CIC is split into 2 filters, and the main CIC (MCIC) is configured in
      --  Sinc1 filter - 010: The CIC is split into 2 filters, and the main CIC
      --  (MCIC) is configured in Sinc2 filter - 011: The CIC is split into 2
      --  filters, and the main CIC (MCIC) is configured in Sinc3 filter - 100:
      --  The CIC is configured in single sinc4 filter others: The CIC is
      --  configured in single sinc5 filter This field can be write-protected,
      --  please refer to Section 1.4.15: Register protection for details.
      CICMOD         : MDF_DFLT1CICR_CICMOD_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  CIC decimation ratio selection Set and cleared by software. This bit
      --  is used to allow the application to select the decimation ratio of
      --  the CIC. Decimation ratio smaller than 2 is not allowed. The
      --  decimation ratio is given by (CICDEC+1). - 0: Decimation ratio is 2 -
      --  1: Decimation ratio is 2 - 2: Decimation ratio is 3 - 3: Decimation
      --  ratio is 4 ... - 511: Decimation ratio is 512 This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      MCICD          : MDF_DFLT1CICR_MCICD_Field := 16#0#;
      --  unspecified
      Reserved_17_19 : HAL.UInt3 := 16#0#;
      --  Scaling factor selection Set and cleared by software. This field is
      --  used to allow the application to select the gain to be applied at CIC
      --  output. Please refer to Table 13: Possible gain values for details.
      --  If the application attempts to write a new gain value while the
      --  previous one is not yet applied, this new gain value is ignored.
      --  Reading back the SCALE[5:0] field will inform the application on the
      --  current gain value. - 100000: - 48.2 dB, or shift right by 8 bits
      --  (default value) - 100001: - 44.6 dB, - 100010: - 42.1 dB, or shift
      --  right by 7 bits - 100011: - 38.6 dB, ... - 101110: -6 dB, or shift
      --  right by 1 bit - 101111: -2.5 dB, - 000000: 0 dB - 000001: + 3.5 dB,
      --  - 000010: + 6 dB, or shift left by 1 bit ... - 011000: + 72 dB, or
      --  shift left by 12 bits
      SCALE          : MDF_DFLT1CICR_SCALE_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT1CICR_Register use record
      DATSRC         at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      CICMOD         at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      MCICD          at 0 range 8 .. 16;
      Reserved_17_19 at 0 range 17 .. 19;
      SCALE          at 0 range 20 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype MDF_DFLT1RSFR_HPFC_Field is HAL.UInt2;

   --  This register is used to control the reshape and HPF filters.
   type MDF_DFLT1RSFR_Register is record
      --  Reshaper filter bypass Set and cleared by software. This bit is used
      --  to bypass the reshape filter and its decimation block. - 0: The
      --  reshape filter is not bypassed (Default value) - 1: The reshape
      --  filter is bypassed This field can be write-protected, please refer to
      --  Section 1.4.15: Register protection for details.
      RSFLTBYP       : Boolean := False;
      --  unspecified
      Reserved_1_3   : HAL.UInt3 := 16#0#;
      --  Reshaper filter decimation ratio Set and cleared by software. This
      --  bit is used to select the decimation ratio for the reshape filter -
      --  0: Decimation ratio is 4 (Default value) - 1: Decimation ratio is 1
      --  This field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      RSFLTD         : Boolean := False;
      --  unspecified
      Reserved_5_6   : HAL.UInt2 := 16#0#;
      --  High-Pass Filter bypass Set and cleared by software. This bit is used
      --  to bypass the high-pass filter. - 0: The high pass filter is not
      --  bypassed (Default value) - 1: The high pass filter is bypassed This
      --  field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      HPFBYP         : Boolean := False;
      --  High-pass filter cut-off frequency Set and cleared by software. This
      --  field is used to select the cut-off frequency of the high-pass
      --  filter. FPCM represents the sampling frequency at HPF input. - 00:
      --  Cut-off frequency = 0.000625 x FPCM - 01: Cut-off frequency = 0.00125
      --  x FPCM - 10: Cut-off frequency = 0.00250 x FPCM - 11: Cut-off
      --  frequency = 0.00950 x FPCM This field can be write-protected, please
      --  refer to Section 1.4.15: Register protection for details.
      HPFC           : MDF_DFLT1RSFR_HPFC_Field := 16#0#;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT1RSFR_Register use record
      RSFLTBYP       at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      RSFLTD         at 0 range 4 .. 4;
      Reserved_5_6   at 0 range 5 .. 6;
      HPFBYP         at 0 range 7 .. 7;
      HPFC           at 0 range 8 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype MDF_DFLT1INTR_INTDIV_Field is HAL.UInt2;
   subtype MDF_DFLT1INTR_INTVAL_Field is HAL.UInt7;

   --  This register is used to the integrator (INT) settings.
   type MDF_DFLT1INTR_Register is record
      --  Integrator output division Set and cleared by software. This bit is
      --  used to rescale the signal at the integrator output in order keep the
      --  data width lower than 24 bits. - 00: The integrator data outputs are
      --  divided by 128 (Default value) - 01: The integrator data outputs are
      --  divided by 32 - 10: The integrator data outputs are divided by 4 -
      --  11: The integrator data outputs are not divided This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      INTDIV         : MDF_DFLT1INTR_INTDIV_Field := 16#0#;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  Integration value selection Set and cleared by software. This field
      --  is used to select the integration value. - 0: The integration value
      --  is 1, meaning bypass mode (default after reset) - 1: The integration
      --  value is 2 - 2: The integration value is 3 ... - 127: The integration
      --  value is 128 This field can be write-protected, please refer to
      --  Section 1.4.15: Register protection for details.
      INTVAL         : MDF_DFLT1INTR_INTVAL_Field := 16#0#;
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT1INTR_Register use record
      INTDIV         at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      INTVAL         at 0 range 4 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   subtype MDF_OLD1CR_BKOLD_Field is HAL.UInt4;
   subtype MDF_OLD1CR_ACICN_Field is HAL.UInt2;
   subtype MDF_OLD1CR_ACICD_Field is HAL.UInt5;

   --  This register is used to configure the Out-of Limit Detector function.
   type MDF_OLD1CR_Register is record
      --  Over-Current Detector Enable Set and cleared by software. - 0: The
      --  OLD is disabled (Default value) - 1: The OLD is enabled, including
      --  the ACIC filter working in continuous mode.
      OLDEN          : Boolean := False;
      --  Threshold In band Set and cleared by software. - 0: The OLD generates
      --  an event if the signal is lower than OLDTHL OR higher than OLDTHH
      --  (Default value) - 1: The OLD generates an event if the signal is
      --  lower than OLDTHH AND higher than OLDTHL This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      THINB          : Boolean := False;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  Break signal assignment for out-of limit detector Set and cleared by
      --  software. BKOLD[i] = 0: Break signal (mdf_break[i]) is not assigned
      --  to threshold event BKOLD[i] = 1: Break signal (mdf_break[i]) is
      --  assigned to threshold event This field can be write-protected, please
      --  refer to Section 1.4.15: Register protection for details.
      BKOLD          : MDF_OLD1CR_BKOLD_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : HAL.UInt4 := 16#0#;
      --  OLD CIC order selection Set and cleared by software. This field
      --  allows the application to select the type, and the order of the ACIC.
      --  This field is only taken into account by the MDF when CICMOD[2:0] =
      --  0xx . - 00: FastSinc filter type - 01: Sinc1 filter type - 10: Sinc2
      --  filter type - 11: Sinc3 filter type This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      ACICN          : MDF_OLD1CR_ACICN_Field := 16#0#;
      --  unspecified
      Reserved_14_16 : HAL.UInt3 := 16#0#;
      --  OLD CIC decimation ratio selection Set and cleared by software. This
      --  field is used to allow the application to select the decimation ratio
      --  of the ACIC. This field is only taken into account by the MDF when
      --  CICMOD[2:0] = 0xx . The decimation ratio is given by (ACICD+1). - 0:
      --  Decimation ratio is 1 - 1: Decimation ratio is 2 - 2: Decimation
      --  ratio is 3 - 3: Decimation ratio is 4 ... - 31: Decimation ratio is
      --  32 This field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      ACICD          : MDF_OLD1CR_ACICD_Field := 16#0#;
      --  unspecified
      Reserved_22_30 : HAL.UInt9 := 16#0#;
      --  Read-only. OLD Active flag Set and cleared by hardware. This flag
      --  must be used by the application in order to check if the OLD is
      --  effectively enabled (active) or not. The protected fields and
      --  registers of this function can only be updated when the OLDACTIVE is
      --  set to , please refer to Section 1.4.15: Register protection for
      --  details. The delay between a transition on OLDEN and a transition on
      --  OLDACTIVE is 2 periods of AHB clock and 2 periods of mdf_proc_ck. -
      --  0: The OLD is not active, and can be configured if needed - 1: The
      --  OLD is active, and protected fields cannot be configured.
      OLDACTIVE      : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_OLD1CR_Register use record
      OLDEN          at 0 range 0 .. 0;
      THINB          at 0 range 1 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      BKOLD          at 0 range 4 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      ACICN          at 0 range 12 .. 13;
      Reserved_14_16 at 0 range 14 .. 16;
      ACICD          at 0 range 17 .. 21;
      Reserved_22_30 at 0 range 22 .. 30;
      OLDACTIVE      at 0 range 31 .. 31;
   end record;

   subtype MDF_OLD1THLR_OLDTHL_Field is HAL.UInt26;

   --  This register is used for the adjustment of the Out-off Limit low
   --  threshold.
   type MDF_OLD1THLR_Register is record
      --  OLD Low Threshold Value Set and cleared by software. OLDTHL
      --  represents a 26-bit signed value. The real threshold compared to the
      --  signal provided by the filter is OLDTHL. This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      OLDTHL         : MDF_OLD1THLR_OLDTHL_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_OLD1THLR_Register use record
      OLDTHL         at 0 range 0 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype MDF_OLD1THHR_OLDTHH_Field is HAL.UInt26;

   --  This register is used for the adjustment of the Out-off Limit high
   --  threshold.
   type MDF_OLD1THHR_Register is record
      --  OLD High Threshold Value Set and cleared by software. OLDTHH
      --  represents a 26-bit signed value. The real threshold compared to the
      --  signal provided by the filter is OLDTHH. This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details
      OLDTHH         : MDF_OLD1THHR_OLDTHH_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_OLD1THHR_Register use record
      OLDTHH         at 0 range 0 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype MDF_DLY1CR_SKPDLY_Field is HAL.UInt7;

   --  This register is used for the adjustment stream delays.
   type MDF_DLY1CR_Register is record
      --  Delay to apply to a bitstream Set and cleared by software. Defines
      --  the number of input samples that will be skipped. Skipping is applied
      --  immediately after writing to this field, if SKPBF = 0 , and the
      --  corresponding bit DFLTEN = 1 . If SKPBF = 1 the value written into
      --  the register is ignored by the delay state machine. - 0: No input
      --  sample skipped, - 1: 1 input sample skipped, ... - 127: 127 input
      --  sample skipped,
      SKPDLY        : MDF_DLY1CR_SKPDLY_Field := 16#0#;
      --  unspecified
      Reserved_7_30 : HAL.UInt24 := 16#0#;
      --  Read-only. Skip Busy flag Set and cleared by hardware. Shall be used
      --  in order to control if the delay sequence is completed. - 0: Reading
      --  0 means that the MDF is ready to accept a new value into SKPDLY[6:0].
      --  - 1: Reading 1 means that last valid SKPDLY[6:0] is still under
      --  precessing.
      SKPBF         : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DLY1CR_Register use record
      SKPDLY        at 0 range 0 .. 6;
      Reserved_7_30 at 0 range 7 .. 30;
      SKPBF         at 0 range 31 .. 31;
   end record;

   subtype MDF_SCD1CR_BKSCD_Field is HAL.UInt4;
   subtype MDF_SCD1CR_SCDT_Field is HAL.UInt8;

   --  This register is used for the adjustment stream delays.
   type MDF_SCD1CR_Register is record
      --  Short circuit detector enable Set and cleared by software. - 0: The
      --  short circuit detector is disabled, - 1: The short circuit detector
      --  is enabled,
      SCDEN          : Boolean := False;
      --  unspecified
      Reserved_1_3   : HAL.UInt3 := 16#0#;
      --  Break signal assignment for short circuit detector Set and cleared by
      --  software. BKSCD[i] = 0: Break signal (mdf_break[i]) is not assigned
      --  to this SCD event BKSCD[i] = 1: Break signal (mdf_break[i]) is
      --  assigned to this SCD event This field can be write-protected, please
      --  refer to Section 1.4.15: Register protection for details.
      BKSCD          : MDF_SCD1CR_BKSCD_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : HAL.UInt4 := 16#0#;
      --  Short-circuit detector threshold Set and cleared by software. These
      --  bits are written by software to define the threshold counter for the
      --  short-circuit detector. If this value is reached, then a
      --  short-circuit detector event occurs on a given input stream. - 0: 2
      --  consecutive 1 s or 0 s will generate an event, - 1: 2 consecutive 1 s
      --  or 0 s will generate an event - 2: 3 consecutive 1 s or 0 s will
      --  generate an event, ... - 255: 256 consecutive 1 s or 0 s will
      --  generate an event, This field can be write-protected, please refer to
      --  Section 1.4.15: Register protection for details.
      SCDT           : MDF_SCD1CR_SCDT_Field := 16#0#;
      --  unspecified
      Reserved_20_30 : HAL.UInt11 := 16#0#;
      --  Read-only. SCD Active flag Set and cleared by hardware. This flag
      --  must be used by the application in order to check if the SCD is
      --  effectively enabled (active) or not. The protected fields of this
      --  function can only be updated when the SCDACTIVE is set to a , please
      --  refer to Section 1.4.15: Register protection for details. The delay
      --  between a transition on SCDEN and a transition on SCDACTIVE is 2
      --  periods of AHB clock and 2 periods of mdf_proc_ck. - 0: The SCD is
      --  not active, and can be configured if needed - 1: The SCD is active,
      --  and protected fields cannot be configured.
      SCDACTIVE      : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_SCD1CR_Register use record
      SCDEN          at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      BKSCD          at 0 range 4 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      SCDT           at 0 range 12 .. 19;
      Reserved_20_30 at 0 range 20 .. 30;
      SCDACTIVE      at 0 range 31 .. 31;
   end record;

   --  MDF DFLTx interrupt enable register x
   type MDF_DFLT1IER_Register is record
      --  RXFIFO threshold interrupt enable Set and cleared by software. - 0:
      --  RXFIFO threshold interrupt disabled - 1: RXFIFO threshold interrupt
      --  enabled
      FTHIE          : Boolean := False;
      --  Data overflow interrupt enable Set and cleared by software. - 0: Data
      --  overflow interrupt disabled - 1: Data overflow interrupt enabled
      DOVRIE         : Boolean := False;
      --  Snapshot data ready interrupt enable Set and cleared by software. -
      --  0: Snapshot data ready interrupt disabled - 1: Snapshot data ready
      --  interrupt enabled
      SSDRIE         : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Out-of Limit interrupt enable Set and cleared by software. - 0: OLD
      --  event interrupt disabled - 1: OLD event interrupt enabled
      OLDIE          : Boolean := False;
      --  unspecified
      Reserved_5_6   : HAL.UInt2 := 16#0#;
      --  Snapshot overrun interrupt enable Set and cleared by software. - 0:
      --  Snapshot overrun interrupt disabled - 1: Snapshot overrun interrupt
      --  enabled
      SSOVRIE        : Boolean := False;
      --  Short-Circuit Detector interrupt enable Set and cleared by software.
      --  - 0: SCD interrupt disabled - 1: SCD interrupt enabled
      SCDIE          : Boolean := False;
      --  Saturation detection interrupt enable Set and cleared by software. -
      --  0: Saturation interrupt disabled - 1: Saturation interrupt enabled
      SATIE          : Boolean := False;
      --  Clock absence detection interrupt enable Set and cleared by software.
      --  - 0: Clock absence interrupt disabled - 1: Clock absence interrupt
      --  enabled
      CKABIE         : Boolean := False;
      --  Reshape Filter Overrun interrupt enable Set and cleared by software.
      --  - 0: Reshape filter overrun interrupt disabled - 1: Reshape filter
      --  overrun interrupt enabled
      RFOVRIE        : Boolean := False;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT1IER_Register use record
      FTHIE          at 0 range 0 .. 0;
      DOVRIE         at 0 range 1 .. 1;
      SSDRIE         at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      OLDIE          at 0 range 4 .. 4;
      Reserved_5_6   at 0 range 5 .. 6;
      SSOVRIE        at 0 range 7 .. 7;
      SCDIE          at 0 range 8 .. 8;
      SATIE          at 0 range 9 .. 9;
      CKABIE         at 0 range 10 .. 10;
      RFOVRIE        at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   --  This register contains the status flags for each digital filter path.
   type MDF_DFLT1ISR_Register is record
      --  Read-only. RXFIFO threshold flag Set by hardware, and cleared by
      --  software by writing this bit to 1 . - 0: Reading 0 means that the
      --  RXFIFO threshold is not reached, writing 0 has no effect. - 1:
      --  Reading 1 means that the RXFIFO reached the threshold, writing 1
      --  clears this flag.
      FTHF           : Boolean := False;
      --  Data overflow flag Set by hardware, and cleared by software by
      --  writing this bit to 1 . - 0: Reading 0 means that no overflow is
      --  detected, writing 0 has no effect. - 1: Reading 1 means that an
      --  overflow is detected, writing 1 clears this flag.
      DOVRF          : Boolean := False;
      --  Snapshot data ready flag Set by hardware, and cleared by software by
      --  writing this bit to 1 . - 0: Reading 0 means that no data is
      --  available on MDF_SNPSxDR, writing 0 has no effect. - 1: Reading 1
      --  means that a new data is available on MDF_SNPSxDR, writing 1 clears
      --  this flag.
      SSDRF          : Boolean := False;
      --  Read-only. RXFIFO Not Empty flag Set and cleared by hardware
      --  according to the RXFIFO level. - 0: Reading 0 means that the RXFIFO
      --  is empty. - 1: Reading 1 means that the RXFIFO is not empty.
      RXNEF          : Boolean := False;
      --  Out-of Limit Detector flag Set by hardware, and cleared by software
      --  by writing this bit to 1 . - 0: Reading 0 means that no OLD event is
      --  detected, writing 0 has no effect. - 1: Reading 1 means that an OLD
      --  event is detected, writing 1 clears THHF, THLF and OLDF flags.
      OLDF           : Boolean := False;
      --  Read-only. Low threshold status flag Set by hardware, and cleared by
      --  software by writing OLDF bit to 1 . This flag indicates the status of
      --  the low threshold comparator when the last OLD event occurred. This
      --  bit gives additional information on the conditions triggering the
      --  last OLD event. It can be cleared by writing OLDF flag to a 1. - 0:
      --  The signal was lower than OLDTHL, when the last OLD event occurred -
      --  1: The signal was higher than OLDTHL, when the last OLD event
      --  occurred
      THLF           : Boolean := False;
      --  Read-only. High threshold status flag Set by hardware, and cleared by
      --  software by writing OLDF bit to 1 . This flag indicates the status of
      --  the high threshold comparator when the last OLD event occurred. This
      --  bit gives additional information on the conditions triggering the
      --  last OLD event. It can be cleared by writing OLDF flag to a 1. - 0:
      --  The signal was lower than OLDTHH, when the last OLD event occurred -
      --  1: The signal was higher than OLDTHH, when the last OLD event
      --  occurred
      THHF           : Boolean := False;
      --  Snapshot overrun flag Set by hardware, and cleared by software by
      --  writing this bit to 1 . - 0: Reading 0 means that no snapshot overrun
      --  event is detected, writing 0 has no effect. - 1: Reading 1 means that
      --  a snapshot overrun event is detected, writing 1 clears this flag.
      SSOVRF         : Boolean := False;
      --  Short-Circuit Detector flag Set by hardware, and cleared by software
      --  by writing this bit to 1 . - 0: Reading 0 means that no SCD event is
      --  detected, writing 0 has no effect. - 1: Reading 1 means that a SCD
      --  event is detected, writing 1 clears this flag.
      SCDF           : Boolean := False;
      --  Saturation detection flag Set by hardware, and cleared by software by
      --  writing this bit to 1 . - 0: Reading 0 means that no saturation is
      --  detected, writing 0 has no effect. - 1: Reading 1 means that a
      --  saturation is detected, writing 1 clears this flag.
      SATF           : Boolean := False;
      --  Clock absence detection flag Set by hardware, and cleared by software
      --  by writing this bit to 1 . - 0: Reading 0 means that no clock absence
      --  is detected, writing 0 has no effect. - 1: Reading 1 means that a
      --  clock absence is detected, writing 1 clears this flag.
      CKABF          : Boolean := False;
      --  Reshape Filter Overrun detection flag Set by hardware, and cleared by
      --  software by writing this bit to 1 . - 0: Reading 0 means that no
      --  reshape filter overrun is detected, writing 0 has no effect. - 1:
      --  Reading 1 means that reshape filter overrun is detected, writing 1
      --  clears this flag.
      RFOVRF         : Boolean := False;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT1ISR_Register use record
      FTHF           at 0 range 0 .. 0;
      DOVRF          at 0 range 1 .. 1;
      SSDRF          at 0 range 2 .. 2;
      RXNEF          at 0 range 3 .. 3;
      OLDF           at 0 range 4 .. 4;
      THLF           at 0 range 5 .. 5;
      THHF           at 0 range 6 .. 6;
      SSOVRF         at 0 range 7 .. 7;
      SCDF           at 0 range 8 .. 8;
      SATF           at 0 range 9 .. 9;
      CKABF          at 0 range 10 .. 10;
      RFOVRF         at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype MDF_OEC1CR_OFFSET_Field is HAL.UInt26;

   --  This register contains the offset compensation value.
   type MDF_OEC1CR_Register is record
      --  Offset error compensation Set and cleared by software. If the
      --  application attempts to write a new offset value while the previous
      --  one is not yet applied, this new offset value is ignored. Reading
      --  back the OFFSET[25:0] field will inform the application on the
      --  current offset value. OFFSET[25:0] represents the value to be
      --  subtracted to the signal before going to the SCALE.
      OFFSET         : MDF_OEC1CR_OFFSET_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_OEC1CR_Register use record
      OFFSET         at 0 range 0 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype MDF_SNPS1DR_MCICDC_Field is HAL.UInt9;
   subtype MDF_SNPS1DR_EXTSDR_Field is HAL.UInt7;
   subtype MDF_SNPS1DR_SDR_Field is HAL.UInt16;

   --  This register is used to read the data processed by each digital filter
   --  in snapshot mode.
   type MDF_SNPS1DR_Register is record
      --  Read-only. Contains the MCIC decimation counter value at the moment
      --  of the last trigger event occurs (MCIC_CNT)
      MCICDC : MDF_SNPS1DR_MCICDC_Field;
      --  Read-only. Extended data size If SNPSFMT = 0 , EXTSDR[6:0] contains
      --  the bit 7 to 1 of the last valid data processed by the digital
      --  filter, If SNPSFMT = 1 , this field contains the INT accumulator
      --  counter value at the moment of the last trigger event occurs
      --  (INT_CNT).
      EXTSDR : MDF_SNPS1DR_EXTSDR_Field;
      --  Read-only. Contains the 16 MSB of the last valid data processed by
      --  the digital filter.
      SDR    : MDF_SNPS1DR_SDR_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_SNPS1DR_Register use record
      MCICDC at 0 range 0 .. 8;
      EXTSDR at 0 range 9 .. 15;
      SDR    at 0 range 16 .. 31;
   end record;

   subtype MDF_DFLT1DR_DR_Field is HAL.UInt24;

   --  This register is used to read the data processed by each digital filter.
   type MDF_DFLT1DR_Register is record
      --  unspecified
      Reserved_0_7 : HAL.UInt8;
      --  Read-only. Data processed by digital filter.
      DR           : MDF_DFLT1DR_DR_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT1DR_Register use record
      Reserved_0_7 at 0 range 0 .. 7;
      DR           at 0 range 8 .. 31;
   end record;

   subtype MDF_SITF2CR_SCKSRC_Field is HAL.UInt2;
   subtype MDF_SITF2CR_SITFMOD_Field is HAL.UInt2;
   subtype MDF_SITF2CR_STH_Field is HAL.UInt5;

   --  This register is used to control the serial interfaces (SITFx).
   type MDF_SITF2CR_Register is record
      --  Serial interface enable Set and cleared by software. This bit is used
      --  to enable/disable the serial interface. - 0: Serial interface
      --  disabled - 1: Serial interface enabled
      SITFEN         : Boolean := False;
      --  Serial clock source Set and cleared by software. This bit is used to
      --  select the clock source of the serial interface. - 00: Serial clock
      --  source is MDF_CCK0 - 01: Serial clock source is MDF_CCK1 1x: Serial
      --  clock source is MDF_CKIx, not allowed in LF_MASTER SPI mode This
      --  field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      SCKSRC         : MDF_SITF2CR_SCKSRC_Field := 16#0#;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Serial interface type Set and cleared by software. This field is used
      --  to defined the serial interface type. - 00: LF_MASTER (Low-Frequency
      --  MASTER) SPI mode - 01: Normal SPI mode - 10: Manchester mode: rising
      --  edge = logic 0, falling edge = logic 1 - 11: Manchester mode: rising
      --  edge = logic 1, falling edge = logic 0 This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      SITFMOD        : MDF_SITF2CR_SITFMOD_Field := 16#0#;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Manchester Symbol threshold / SPI threshold Set and cleared by
      --  software. This field is used for Manchester mode, in order to define
      --  the expected symbol threshold levels. Please refer to Section :
      --  Manchester mode for details on computation. In addition this field is
      --  used to define the timeout value for the clock absence detection in
      --  Normal SPI mode. Values of STH[4:0] lower than 4 are invalid. This
      --  field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      STH            : MDF_SITF2CR_STH_Field := 16#1F#;
      --  unspecified
      Reserved_13_30 : HAL.UInt18 := 16#0#;
      --  Read-only. Serial interface Active flag
      SITFACTIVE     : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_SITF2CR_Register use record
      SITFEN         at 0 range 0 .. 0;
      SCKSRC         at 0 range 1 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      SITFMOD        at 0 range 4 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      STH            at 0 range 8 .. 12;
      Reserved_13_30 at 0 range 13 .. 30;
      SITFACTIVE     at 0 range 31 .. 31;
   end record;

   subtype MDF_BSMX2CR_BSSEL_Field is HAL.UInt5;

   --  This register is used to select the bitstream to be provided to the
   --  corresponding digital filter and to the SCD.
   type MDF_BSMX2CR_Register is record
      --  Bitstream Selection Set and cleared by software. This field is used
      --  to select the bitstream to be processed for the digital filter x and
      --  for the SCDx. The size of this field depends on the number of DFLTx
      --  instantiated. If the BSSEL is selecting an input which is not
      --  instantiated, the MDF will select the valid stream bs[x]_F having the
      --  higher index number. - 00000: The bitstream bs[0]_R is provided to
      --  DFLTx and SCDx - 00001: The bitstream bs[0]_F is provided to DFLTx
      --  and SCDx - 00010: The bitstream bs[1]_R is provided to DFLTx and SCDx
      --  (if instantiated) - 00011: The bitstream bs[1]_F is provided to DFLTx
      --  and SCDx (if instantiated) ... - 11110: The bitstream bs[15]_R is
      --  provided to DFLTx and SCDx (if instantiated) - 11111: The bitstream
      --  bs[15]_F is provided to DFLTx and SCDx (if instantiated) This field
      --  can be write-protected, please refer to Section 1.4.15: Register
      --  protection for details.
      BSSEL         : MDF_BSMX2CR_BSSEL_Field := 16#0#;
      --  unspecified
      Reserved_5_30 : HAL.UInt26 := 16#0#;
      --  Read-only. BSMX Active flag Set and cleared by hardware. This flag
      --  must be used by the application in order to check if the BSMX is
      --  effectively enabled (active) or not. BSSEL[4:0] can only be updated
      --  when the BSMXACTIVE is set to a . The BSMXACTIVE flag is a logical
      --  between OLDACTIVE, DFLTACTIVE, and SCDACTIVE flags. Both of them must
      --  be set to in order update BSSEL[4:0] field. - 0: The BSMX is not
      --  active, and can be configured if needed - 1: The BSMX is active, and
      --  protected fields cannot be configured.
      BSMXACTIVE    : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_BSMX2CR_Register use record
      BSSEL         at 0 range 0 .. 4;
      Reserved_5_30 at 0 range 5 .. 30;
      BSMXACTIVE    at 0 range 31 .. 31;
   end record;

   subtype MDF_DFLT2CR_ACQMOD_Field is HAL.UInt3;
   subtype MDF_DFLT2CR_TRGSRC_Field is HAL.UInt4;
   subtype MDF_DFLT2CR_NBDIS_Field is HAL.UInt8;

   --  This register is used to control the digital filter 2.
   type MDF_DFLT2CR_Register is record
      --  Write-only. Digital Filter Enable Set and cleared by software. This
      --  bit is used to control the start of acquisition of the corresponding
      --  digital filter path. The behavior of this bit depends on ACQMOD and
      --  external events. or the acquisition starts when the proper trigger
      --  event occurs if ACQMOD = 01x . The serial or parallel interface
      --  delivering the samples shall be enabled as well. - 0: The acquisition
      --  is stopped immediately - 1: The acquisition is immediately started if
      --  ACQMOD = 00x or 1xx ,
      DFLTEN         : Boolean := False;
      --  DMA Requests Enable Set and cleared by software. This bit is used to
      --  control the generation of DMA request in order to transfer the
      --  processed samples into the memory. - 0: The DMA interface for the
      --  corresponding digital filter is disabled - 1: The DMA interface for
      --  the corresponding digital filter is enabled This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      DMAEN          : Boolean := False;
      --  RXFIFO Threshold selection Set and cleared by software. This bit is
      --  used to select the RXFIFO threshold. This bit is not significant for
      --  RXFIFOs working in a interleaved transfer mode. Refer to Section
      --  1.4.13.4: Using the interleaved transfer mode for details. - 0:
      --  RXFIFO threshold event generated when the RXFIFO is not empty - 1:
      --  RXFIFO threshold event generated when the RXFIFO is half-full This
      --  field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      FTH            : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Digital filter Trigger mode Set and cleared by software. This field
      --  is used to select the filter trigger mode. - 000: Asynchronous,
      --  continuous acquisition mode - 001: Asynchronous, single-shot
      --  acquisition mode - 010: Synchronous, continuous acquisition mode -
      --  011: Synchronous, single-shot acquisition mode - 100: Window,
      --  continuous acquisition mode - 101: Synchronous, snapshot mode others:
      --  same a 000 This field can be write-protected, please refer to Section
      --  1.4.15: Register protection for details.
      ACQMOD         : MDF_DFLT2CR_ACQMOD_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Digital filter Trigger sensitivity selection Set and cleared by
      --  software. This field is used to select the trigger sensitivity of the
      --  external signals - 0: A rising edge event triggers the acquisition -
      --  1: A falling edge even triggers the acquisition Note that when the
      --  trigger source is TRGO or OLDx event, TRGSENS value is not taken into
      --  account. When TRGO is selected, the sensitivity is forced to falling
      --  edge, when OLDx event is selected, the sensitivity is forced to
      --  rising edge. This field can be write-protected, please refer to
      --  Section 1.4.15: Register protection for details.
      TRGSENS        : Boolean := False;
      --  unspecified
      Reserved_9_11  : HAL.UInt3 := 16#0#;
      --  Digital filter Trigger signal selection, Set and cleared by software.
      --  This field is used to select which external signals is used as
      --  trigger for the corresponding filter. - 0000: TRGO is selected -
      --  0001: OLDx event is selected - 0010: mdf_trg[0] is selected ... -
      --  1111: mdf_trg[13] is selected This field can be write-protected,
      --  please refer to Section 1.4.15: Register protection for details.
      TRGSRC         : MDF_DFLT2CR_TRGSRC_Field := 16#0#;
      --  Snapshot data format Set and cleared by software. This field is used
      --  to select the data format for the snapshot mode. - 0: The integrator
      --  counter (INT_CNT) is not inserted into the MDF_SNPSxDR register,
      --  leaving a data resolution of 23 bits. - 1: The integrator counter
      --  (INT_CNT) is inserted at position [15:9] of MDF_SNPSxDR register,
      --  leaving a data resolution of 16 bits. This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      SNPSFMT        : Boolean := False;
      --  unspecified
      Reserved_17_19 : HAL.UInt3 := 16#0#;
      --  Number of samples to be discarded Set and cleared by software. This
      --  field is used to define the number of samples to be discarded every
      --  time the DFLTx is re-started. - 0: no sample discarded - 1: 1 sample
      --  discarded - 2: 2 samples discarded ... - 255: 255 samples discarded
      --  This field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      NBDIS          : MDF_DFLT2CR_NBDIS_Field := 16#0#;
      --  unspecified
      Reserved_28_29 : HAL.UInt2 := 16#0#;
      --  Read-only. Digital filter Run Status Flag Set and cleared by
      --  hardware. This bit indicates if the digital filter is running or not.
      --  - 0: The digital filter is not running, and ready to accept a new
      --  trigger event - 1: The digital filter is running
      DFLTRUN        : Boolean := False;
      --  Read-only. Digital filter Active Flag Set and cleared by hardware.
      --  This bit indicates if the digital filter is active: can be running or
      --  waiting for events. - 0: The digital filter is not active, and can be
      --  re-enabled again (via DFLTEN bit) if needed - 1: The digital filter
      --  is active
      DFLTACTIVE     : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT2CR_Register use record
      DFLTEN         at 0 range 0 .. 0;
      DMAEN          at 0 range 1 .. 1;
      FTH            at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      ACQMOD         at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      TRGSENS        at 0 range 8 .. 8;
      Reserved_9_11  at 0 range 9 .. 11;
      TRGSRC         at 0 range 12 .. 15;
      SNPSFMT        at 0 range 16 .. 16;
      Reserved_17_19 at 0 range 17 .. 19;
      NBDIS          at 0 range 20 .. 27;
      Reserved_28_29 at 0 range 28 .. 29;
      DFLTRUN        at 0 range 30 .. 30;
      DFLTACTIVE     at 0 range 31 .. 31;
   end record;

   subtype MDF_DFLT2CICR_DATSRC_Field is HAL.UInt2;
   subtype MDF_DFLT2CICR_CICMOD_Field is HAL.UInt3;
   subtype MDF_DFLT2CICR_MCICD_Field is HAL.UInt9;
   subtype MDF_DFLT2CICR_SCALE_Field is HAL.UInt6;

   --  This register is used to control the main CIC filter.
   type MDF_DFLT2CICR_Register is record
      --  Source data for the digital filter Set and cleared by software. 0x:
      --  Select the stream coming from the BSMX - 10: Select the stream coming
      --  from the ADCITF1 - 11: Select the stream coming from the ADCITF2 This
      --  field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      DATSRC         : MDF_DFLT2CICR_DATSRC_Field := 16#0#;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  Select the CIC mode Set and cleared by software. This field allows
      --  the application to select the configuration and the order of the
      --  MCIC. When CICMOD[2:0] is equal to 0xx , the CIC is split into two
      --  filters: - The main CIC (MCIC) - The auxiliary CIC (ACIC), used for
      --  the out-off limit detector - 000: The CIC is split into 2 filters,
      --  and the main CIC (MCIC) is configured in FastSinc filter - 001: The
      --  CIC is split into 2 filters, and the main CIC (MCIC) is configured in
      --  Sinc1 filter - 010: The CIC is split into 2 filters, and the main CIC
      --  (MCIC) is configured in Sinc2 filter - 011: The CIC is split into 2
      --  filters, and the main CIC (MCIC) is configured in Sinc3 filter - 100:
      --  The CIC is configured in single sinc4 filter others: The CIC is
      --  configured in single sinc5 filter This field can be write-protected,
      --  please refer to Section 1.4.15: Register protection for details.
      CICMOD         : MDF_DFLT2CICR_CICMOD_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  CIC decimation ratio selection Set and cleared by software. This bit
      --  is used to allow the application to select the decimation ratio of
      --  the CIC. Decimation ratio smaller than 2 is not allowed. The
      --  decimation ratio is given by (CICDEC+1). - 0: Decimation ratio is 2 -
      --  1: Decimation ratio is 2 - 2: Decimation ratio is 3 - 3: Decimation
      --  ratio is 4 ... - 511: Decimation ratio is 512 This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      MCICD          : MDF_DFLT2CICR_MCICD_Field := 16#0#;
      --  unspecified
      Reserved_17_19 : HAL.UInt3 := 16#0#;
      --  Scaling factor selection Set and cleared by software. This field is
      --  used to allow the application to select the gain to be applied at CIC
      --  output. Please refer to Table 13: Possible gain values for details.
      --  If the application attempts to write a new gain value while the
      --  previous one is not yet applied, this new gain value is ignored.
      --  Reading back the SCALE[5:0] field will inform the application on the
      --  current gain value. - 100000: - 48.2 dB, or shift right by 8 bits
      --  (default value) - 100001: - 44.6 dB, - 100010: - 42.1 dB, or shift
      --  right by 7 bits - 100011: - 38.6 dB, ... - 101110: -6 dB, or shift
      --  right by 1 bit - 101111: -2.5 dB, - 000000: 0 dB - 000001: + 3.5 dB,
      --  - 000010: + 6 dB, or shift left by 1 bit ... - 011000: + 72 dB, or
      --  shift left by 12 bits
      SCALE          : MDF_DFLT2CICR_SCALE_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT2CICR_Register use record
      DATSRC         at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      CICMOD         at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      MCICD          at 0 range 8 .. 16;
      Reserved_17_19 at 0 range 17 .. 19;
      SCALE          at 0 range 20 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype MDF_DFLT2RSFR_HPFC_Field is HAL.UInt2;

   --  This register is used to control the reshape and HPF filters.
   type MDF_DFLT2RSFR_Register is record
      --  Reshaper filter bypass Set and cleared by software. This bit is used
      --  to bypass the reshape filter and its decimation block. - 0: The
      --  reshape filter is not bypassed (Default value) - 1: The reshape
      --  filter is bypassed This field can be write-protected, please refer to
      --  Section 1.4.15: Register protection for details.
      RSFLTBYP       : Boolean := False;
      --  unspecified
      Reserved_1_3   : HAL.UInt3 := 16#0#;
      --  Reshaper filter decimation ratio Set and cleared by software. This
      --  bit is used to select the decimation ratio for the reshape filter -
      --  0: Decimation ratio is 4 (Default value) - 1: Decimation ratio is 1
      --  This field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      RSFLTD         : Boolean := False;
      --  unspecified
      Reserved_5_6   : HAL.UInt2 := 16#0#;
      --  High-Pass Filter bypass Set and cleared by software. This bit is used
      --  to bypass the high-pass filter. - 0: The high pass filter is not
      --  bypassed (Default value) - 1: The high pass filter is bypassed This
      --  field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      HPFBYP         : Boolean := False;
      --  High-pass filter cut-off frequency Set and cleared by software. This
      --  field is used to select the cut-off frequency of the high-pass
      --  filter. FPCM represents the sampling frequency at HPF input. - 00:
      --  Cut-off frequency = 0.000625 x FPCM - 01: Cut-off frequency = 0.00125
      --  x FPCM - 10: Cut-off frequency = 0.00250 x FPCM - 11: Cut-off
      --  frequency = 0.00950 x FPCM This field can be write-protected, please
      --  refer to Section 1.4.15: Register protection for details.
      HPFC           : MDF_DFLT2RSFR_HPFC_Field := 16#0#;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT2RSFR_Register use record
      RSFLTBYP       at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      RSFLTD         at 0 range 4 .. 4;
      Reserved_5_6   at 0 range 5 .. 6;
      HPFBYP         at 0 range 7 .. 7;
      HPFC           at 0 range 8 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype MDF_DFLT2INTR_INTDIV_Field is HAL.UInt2;
   subtype MDF_DFLT2INTR_INTVAL_Field is HAL.UInt7;

   --  This register is used to the integrator (INT) settings.
   type MDF_DFLT2INTR_Register is record
      --  Integrator output division Set and cleared by software. This bit is
      --  used to rescale the signal at the integrator output in order keep the
      --  data width lower than 24 bits. - 00: The integrator data outputs are
      --  divided by 128 (Default value) - 01: The integrator data outputs are
      --  divided by 32 - 10: The integrator data outputs are divided by 4 -
      --  11: The integrator data outputs are not divided This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      INTDIV         : MDF_DFLT2INTR_INTDIV_Field := 16#0#;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  Integration value selection Set and cleared by software. This field
      --  is used to select the integration value. - 0: The integration value
      --  is 1, meaning bypass mode (default after reset) - 1: The integration
      --  value is 2 - 2: The integration value is 3 ... - 127: The integration
      --  value is 128 This field can be write-protected, please refer to
      --  Section 1.4.15: Register protection for details.
      INTVAL         : MDF_DFLT2INTR_INTVAL_Field := 16#0#;
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT2INTR_Register use record
      INTDIV         at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      INTVAL         at 0 range 4 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   subtype MDF_OLD2CR_BKOLD_Field is HAL.UInt4;
   subtype MDF_OLD2CR_ACICN_Field is HAL.UInt2;
   subtype MDF_OLD2CR_ACICD_Field is HAL.UInt5;

   --  This register is used to configure the Out-of Limit Detector function.
   type MDF_OLD2CR_Register is record
      --  Over-Current Detector Enable Set and cleared by software. - 0: The
      --  OLD is disabled (Default value) - 1: The OLD is enabled, including
      --  the ACIC filter working in continuous mode.
      OLDEN          : Boolean := False;
      --  Threshold In band Set and cleared by software. - 0: The OLD generates
      --  an event if the signal is lower than OLDTHL OR higher than OLDTHH
      --  (Default value) - 1: The OLD generates an event if the signal is
      --  lower than OLDTHH AND higher than OLDTHL This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      THINB          : Boolean := False;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  Break signal assignment for out-of limit detector Set and cleared by
      --  software. BKOLD[i] = 0: Break signal (mdf_break[i]) is not assigned
      --  to threshold event BKOLD[i] = 1: Break signal (mdf_break[i]) is
      --  assigned to threshold event This field can be write-protected, please
      --  refer to Section 1.4.15: Register protection for details.
      BKOLD          : MDF_OLD2CR_BKOLD_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : HAL.UInt4 := 16#0#;
      --  OLD CIC order selection Set and cleared by software. This field
      --  allows the application to select the type, and the order of the ACIC.
      --  This field is only taken into account by the MDF when CICMOD[2:0] =
      --  0xx . - 00: FastSinc filter type - 01: Sinc1 filter type - 10: Sinc2
      --  filter type - 11: Sinc3 filter type This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      ACICN          : MDF_OLD2CR_ACICN_Field := 16#0#;
      --  unspecified
      Reserved_14_16 : HAL.UInt3 := 16#0#;
      --  OLD CIC decimation ratio selection Set and cleared by software. This
      --  field is used to allow the application to select the decimation ratio
      --  of the ACIC. This field is only taken into account by the MDF when
      --  CICMOD[2:0] = 0xx . The decimation ratio is given by (ACICD+1). - 0:
      --  Decimation ratio is 1 - 1: Decimation ratio is 2 - 2: Decimation
      --  ratio is 3 - 3: Decimation ratio is 4 ... - 31: Decimation ratio is
      --  32 This field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      ACICD          : MDF_OLD2CR_ACICD_Field := 16#0#;
      --  unspecified
      Reserved_22_30 : HAL.UInt9 := 16#0#;
      --  Read-only. OLD Active flag Set and cleared by hardware. This flag
      --  must be used by the application in order to check if the OLD is
      --  effectively enabled (active) or not. The protected fields and
      --  registers of this function can only be updated when the OLDACTIVE is
      --  set to , please refer to Section 1.4.15: Register protection for
      --  details. The delay between a transition on OLDEN and a transition on
      --  OLDACTIVE is 2 periods of AHB clock and 2 periods of mdf_proc_ck. -
      --  0: The OLD is not active, and can be configured if needed - 1: The
      --  OLD is active, and protected fields cannot be configured.
      OLDACTIVE      : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_OLD2CR_Register use record
      OLDEN          at 0 range 0 .. 0;
      THINB          at 0 range 1 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      BKOLD          at 0 range 4 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      ACICN          at 0 range 12 .. 13;
      Reserved_14_16 at 0 range 14 .. 16;
      ACICD          at 0 range 17 .. 21;
      Reserved_22_30 at 0 range 22 .. 30;
      OLDACTIVE      at 0 range 31 .. 31;
   end record;

   subtype MDF_OLD2THLR_OLDTHL_Field is HAL.UInt26;

   --  This register is used for the adjustment of the Out-off Limit low
   --  threshold.
   type MDF_OLD2THLR_Register is record
      --  OLD Low Threshold Value Set and cleared by software. OLDTHL
      --  represents a 26-bit signed value. The real threshold compared to the
      --  signal provided by the filter is OLDTHL. This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      OLDTHL         : MDF_OLD2THLR_OLDTHL_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_OLD2THLR_Register use record
      OLDTHL         at 0 range 0 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype MDF_OLD2THHR_OLDTHH_Field is HAL.UInt26;

   --  This register is used for the adjustment of the Out-off Limit high
   --  threshold.
   type MDF_OLD2THHR_Register is record
      --  OLD High Threshold Value Set and cleared by software. OLDTHH
      --  represents a 26-bit signed value. The real threshold compared to the
      --  signal provided by the filter is OLDTHH. This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details
      OLDTHH         : MDF_OLD2THHR_OLDTHH_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_OLD2THHR_Register use record
      OLDTHH         at 0 range 0 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype MDF_DLY2CR_SKPDLY_Field is HAL.UInt7;

   --  This register is used for the adjustment stream delays.
   type MDF_DLY2CR_Register is record
      --  Delay to apply to a bitstream Set and cleared by software. Defines
      --  the number of input samples that will be skipped. Skipping is applied
      --  immediately after writing to this field, if SKPBF = 0 , and the
      --  corresponding bit DFLTEN = 1 . If SKPBF = 1 the value written into
      --  the register is ignored by the delay state machine. - 0: No input
      --  sample skipped, - 1: 1 input sample skipped, ... - 127: 127 input
      --  sample skipped,
      SKPDLY        : MDF_DLY2CR_SKPDLY_Field := 16#0#;
      --  unspecified
      Reserved_7_30 : HAL.UInt24 := 16#0#;
      --  Read-only. Skip Busy flag Set and cleared by hardware. Shall be used
      --  in order to control if the delay sequence is completed. - 0: Reading
      --  0 means that the MDF is ready to accept a new value into SKPDLY[6:0].
      --  - 1: Reading 1 means that last valid SKPDLY[6:0] is still under
      --  precessing.
      SKPBF         : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DLY2CR_Register use record
      SKPDLY        at 0 range 0 .. 6;
      Reserved_7_30 at 0 range 7 .. 30;
      SKPBF         at 0 range 31 .. 31;
   end record;

   subtype MDF_SCD2CR_BKSCD_Field is HAL.UInt4;
   subtype MDF_SCD2CR_SCDT_Field is HAL.UInt8;

   --  This register is used for the adjustment stream delays.
   type MDF_SCD2CR_Register is record
      --  Short circuit detector enable Set and cleared by software. - 0: The
      --  short circuit detector is disabled, - 1: The short circuit detector
      --  is enabled,
      SCDEN          : Boolean := False;
      --  unspecified
      Reserved_1_3   : HAL.UInt3 := 16#0#;
      --  Break signal assignment for short circuit detector Set and cleared by
      --  software. BKSCD[i] = 0: Break signal (mdf_break[i]) is not assigned
      --  to this SCD event BKSCD[i] = 1: Break signal (mdf_break[i]) is
      --  assigned to this SCD event This field can be write-protected, please
      --  refer to Section 1.4.15: Register protection for details.
      BKSCD          : MDF_SCD2CR_BKSCD_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : HAL.UInt4 := 16#0#;
      --  Short-circuit detector threshold Set and cleared by software. These
      --  bits are written by software to define the threshold counter for the
      --  short-circuit detector. If this value is reached, then a
      --  short-circuit detector event occurs on a given input stream. - 0: 2
      --  consecutive 1 s or 0 s will generate an event, - 1: 2 consecutive 1 s
      --  or 0 s will generate an event - 2: 3 consecutive 1 s or 0 s will
      --  generate an event, ... - 255: 256 consecutive 1 s or 0 s will
      --  generate an event, This field can be write-protected, please refer to
      --  Section 1.4.15: Register protection for details.
      SCDT           : MDF_SCD2CR_SCDT_Field := 16#0#;
      --  unspecified
      Reserved_20_30 : HAL.UInt11 := 16#0#;
      --  Read-only. SCD Active flag Set and cleared by hardware. This flag
      --  must be used by the application in order to check if the SCD is
      --  effectively enabled (active) or not. The protected fields of this
      --  function can only be updated when the SCDACTIVE is set to a , please
      --  refer to Section 1.4.15: Register protection for details. The delay
      --  between a transition on SCDEN and a transition on SCDACTIVE is 2
      --  periods of AHB clock and 2 periods of mdf_proc_ck. - 0: The SCD is
      --  not active, and can be configured if needed - 1: The SCD is active,
      --  and protected fields cannot be configured.
      SCDACTIVE      : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_SCD2CR_Register use record
      SCDEN          at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      BKSCD          at 0 range 4 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      SCDT           at 0 range 12 .. 19;
      Reserved_20_30 at 0 range 20 .. 30;
      SCDACTIVE      at 0 range 31 .. 31;
   end record;

   --  MDF DFLTx interrupt enable register x
   type MDF_DFLT2IER_Register is record
      --  RXFIFO threshold interrupt enable Set and cleared by software. - 0:
      --  RXFIFO threshold interrupt disabled - 1: RXFIFO threshold interrupt
      --  enabled
      FTHIE          : Boolean := False;
      --  Data overflow interrupt enable Set and cleared by software. - 0: Data
      --  overflow interrupt disabled - 1: Data overflow interrupt enabled
      DOVRIE         : Boolean := False;
      --  Snapshot data ready interrupt enable Set and cleared by software. -
      --  0: Snapshot data ready interrupt disabled - 1: Snapshot data ready
      --  interrupt enabled
      SSDRIE         : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Out-of Limit interrupt enable Set and cleared by software. - 0: OLD
      --  event interrupt disabled - 1: OLD event interrupt enabled
      OLDIE          : Boolean := False;
      --  unspecified
      Reserved_5_6   : HAL.UInt2 := 16#0#;
      --  Snapshot overrun interrupt enable Set and cleared by software. - 0:
      --  Snapshot overrun interrupt disabled - 1: Snapshot overrun interrupt
      --  enabled
      SSOVRIE        : Boolean := False;
      --  Short-Circuit Detector interrupt enable Set and cleared by software.
      --  - 0: SCD interrupt disabled - 1: SCD interrupt enabled
      SCDIE          : Boolean := False;
      --  Saturation detection interrupt enable Set and cleared by software. -
      --  0: Saturation interrupt disabled - 1: Saturation interrupt enabled
      SATIE          : Boolean := False;
      --  Clock absence detection interrupt enable Set and cleared by software.
      --  - 0: Clock absence interrupt disabled - 1: Clock absence interrupt
      --  enabled
      CKABIE         : Boolean := False;
      --  Reshape Filter Overrun interrupt enable Set and cleared by software.
      --  - 0: Reshape filter overrun interrupt disabled - 1: Reshape filter
      --  overrun interrupt enabled
      RFOVRIE        : Boolean := False;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT2IER_Register use record
      FTHIE          at 0 range 0 .. 0;
      DOVRIE         at 0 range 1 .. 1;
      SSDRIE         at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      OLDIE          at 0 range 4 .. 4;
      Reserved_5_6   at 0 range 5 .. 6;
      SSOVRIE        at 0 range 7 .. 7;
      SCDIE          at 0 range 8 .. 8;
      SATIE          at 0 range 9 .. 9;
      CKABIE         at 0 range 10 .. 10;
      RFOVRIE        at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   --  This register contains the status flags for each digital filter path.
   type MDF_DFLT2ISR_Register is record
      --  Read-only. RXFIFO threshold flag Set by hardware, and cleared by
      --  software by writing this bit to 1 . - 0: Reading 0 means that the
      --  RXFIFO threshold is not reached, writing 0 has no effect. - 1:
      --  Reading 1 means that the RXFIFO reached the threshold, writing 1
      --  clears this flag.
      FTHF           : Boolean := False;
      --  Data overflow flag Set by hardware, and cleared by software by
      --  writing this bit to 1 . - 0: Reading 0 means that no overflow is
      --  detected, writing 0 has no effect. - 1: Reading 1 means that an
      --  overflow is detected, writing 1 clears this flag.
      DOVRF          : Boolean := False;
      --  Snapshot data ready flag Set by hardware, and cleared by software by
      --  writing this bit to 1 . - 0: Reading 0 means that no data is
      --  available on MDF_SNPSxDR, writing 0 has no effect. - 1: Reading 1
      --  means that a new data is available on MDF_SNPSxDR, writing 1 clears
      --  this flag.
      SSDRF          : Boolean := False;
      --  Read-only. RXFIFO Not Empty flag Set and cleared by hardware
      --  according to the RXFIFO level. - 0: Reading 0 means that the RXFIFO
      --  is empty. - 1: Reading 1 means that the RXFIFO is not empty.
      RXNEF          : Boolean := False;
      --  Out-of Limit Detector flag Set by hardware, and cleared by software
      --  by writing this bit to 1 . - 0: Reading 0 means that no OLD event is
      --  detected, writing 0 has no effect. - 1: Reading 1 means that an OLD
      --  event is detected, writing 1 clears THHF, THLF and OLDF flags.
      OLDF           : Boolean := False;
      --  Read-only. Low threshold status flag Set by hardware, and cleared by
      --  software by writing OLDF bit to 1 . This flag indicates the status of
      --  the low threshold comparator when the last OLD event occurred. This
      --  bit gives additional information on the conditions triggering the
      --  last OLD event. It can be cleared by writing OLDF flag to a 1. - 0:
      --  The signal was lower than OLDTHL, when the last OLD event occurred -
      --  1: The signal was higher than OLDTHL, when the last OLD event
      --  occurred
      THLF           : Boolean := False;
      --  Read-only. High threshold status flag Set by hardware, and cleared by
      --  software by writing OLDF bit to 1 . This flag indicates the status of
      --  the high threshold comparator when the last OLD event occurred. This
      --  bit gives additional information on the conditions triggering the
      --  last OLD event. It can be cleared by writing OLDF flag to a 1. - 0:
      --  The signal was lower than OLDTHH, when the last OLD event occurred -
      --  1: The signal was higher than OLDTHH, when the last OLD event
      --  occurred
      THHF           : Boolean := False;
      --  Snapshot overrun flag Set by hardware, and cleared by software by
      --  writing this bit to 1 . - 0: Reading 0 means that no snapshot overrun
      --  event is detected, writing 0 has no effect. - 1: Reading 1 means that
      --  a snapshot overrun event is detected, writing 1 clears this flag.
      SSOVRF         : Boolean := False;
      --  Short-Circuit Detector flag Set by hardware, and cleared by software
      --  by writing this bit to 1 . - 0: Reading 0 means that no SCD event is
      --  detected, writing 0 has no effect. - 1: Reading 1 means that a SCD
      --  event is detected, writing 1 clears this flag.
      SCDF           : Boolean := False;
      --  Saturation detection flag Set by hardware, and cleared by software by
      --  writing this bit to 1 . - 0: Reading 0 means that no saturation is
      --  detected, writing 0 has no effect. - 1: Reading 1 means that a
      --  saturation is detected, writing 1 clears this flag.
      SATF           : Boolean := False;
      --  Clock absence detection flag Set by hardware, and cleared by software
      --  by writing this bit to 1 . - 0: Reading 0 means that no clock absence
      --  is detected, writing 0 has no effect. - 1: Reading 1 means that a
      --  clock absence is detected, writing 1 clears this flag.
      CKABF          : Boolean := False;
      --  Reshape Filter Overrun detection flag Set by hardware, and cleared by
      --  software by writing this bit to 1 . - 0: Reading 0 means that no
      --  reshape filter overrun is detected, writing 0 has no effect. - 1:
      --  Reading 1 means that reshape filter overrun is detected, writing 1
      --  clears this flag.
      RFOVRF         : Boolean := False;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT2ISR_Register use record
      FTHF           at 0 range 0 .. 0;
      DOVRF          at 0 range 1 .. 1;
      SSDRF          at 0 range 2 .. 2;
      RXNEF          at 0 range 3 .. 3;
      OLDF           at 0 range 4 .. 4;
      THLF           at 0 range 5 .. 5;
      THHF           at 0 range 6 .. 6;
      SSOVRF         at 0 range 7 .. 7;
      SCDF           at 0 range 8 .. 8;
      SATF           at 0 range 9 .. 9;
      CKABF          at 0 range 10 .. 10;
      RFOVRF         at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype MDF_OEC2CR_OFFSET_Field is HAL.UInt26;

   --  This register contains the offset compensation value.
   type MDF_OEC2CR_Register is record
      --  Offset error compensation Set and cleared by software. If the
      --  application attempts to write a new offset value while the previous
      --  one is not yet applied, this new offset value is ignored. Reading
      --  back the OFFSET[25:0] field will inform the application on the
      --  current offset value. OFFSET[25:0] represents the value to be
      --  subtracted to the signal before going to the SCALE.
      OFFSET         : MDF_OEC2CR_OFFSET_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_OEC2CR_Register use record
      OFFSET         at 0 range 0 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype MDF_SNPS2DR_MCICDC_Field is HAL.UInt9;
   subtype MDF_SNPS2DR_EXTSDR_Field is HAL.UInt7;
   subtype MDF_SNPS2DR_SDR_Field is HAL.UInt16;

   --  This register is used to read the data processed by each digital filter
   --  in snapshot mode.
   type MDF_SNPS2DR_Register is record
      --  Read-only. Contains the MCIC decimation counter value at the moment
      --  of the last trigger event occurs (MCIC_CNT)
      MCICDC : MDF_SNPS2DR_MCICDC_Field;
      --  Read-only. Extended data size If SNPSFMT = 0 , EXTSDR[6:0] contains
      --  the bit 7 to 1 of the last valid data processed by the digital
      --  filter, If SNPSFMT = 1 , this field contains the INT accumulator
      --  counter value at the moment of the last trigger event occurs
      --  (INT_CNT).
      EXTSDR : MDF_SNPS2DR_EXTSDR_Field;
      --  Read-only. Contains the 16 MSB of the last valid data processed by
      --  the digital filter.
      SDR    : MDF_SNPS2DR_SDR_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_SNPS2DR_Register use record
      MCICDC at 0 range 0 .. 8;
      EXTSDR at 0 range 9 .. 15;
      SDR    at 0 range 16 .. 31;
   end record;

   subtype MDF_DFLT2DR_DR_Field is HAL.UInt24;

   --  This register is used to read the data processed by each digital filter.
   type MDF_DFLT2DR_Register is record
      --  unspecified
      Reserved_0_7 : HAL.UInt8;
      --  Read-only. Data processed by digital filter.
      DR           : MDF_DFLT2DR_DR_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT2DR_Register use record
      Reserved_0_7 at 0 range 0 .. 7;
      DR           at 0 range 8 .. 31;
   end record;

   subtype MDF_SITF3CR_SCKSRC_Field is HAL.UInt2;
   subtype MDF_SITF3CR_SITFMOD_Field is HAL.UInt2;
   subtype MDF_SITF3CR_STH_Field is HAL.UInt5;

   --  This register is used to control the serial interfaces (SITFx).
   type MDF_SITF3CR_Register is record
      --  Serial interface enable Set and cleared by software. This bit is used
      --  to enable/disable the serial interface. - 0: Serial interface
      --  disabled - 1: Serial interface enabled
      SITFEN         : Boolean := False;
      --  Serial clock source Set and cleared by software. This bit is used to
      --  select the clock source of the serial interface. - 00: Serial clock
      --  source is MDF_CCK0 - 01: Serial clock source is MDF_CCK1 1x: Serial
      --  clock source is MDF_CKIx, not allowed in LF_MASTER SPI mode This
      --  field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      SCKSRC         : MDF_SITF3CR_SCKSRC_Field := 16#0#;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Serial interface type Set and cleared by software. This field is used
      --  to defined the serial interface type. - 00: LF_MASTER (Low-Frequency
      --  MASTER) SPI mode - 01: Normal SPI mode - 10: Manchester mode: rising
      --  edge = logic 0, falling edge = logic 1 - 11: Manchester mode: rising
      --  edge = logic 1, falling edge = logic 0 This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      SITFMOD        : MDF_SITF3CR_SITFMOD_Field := 16#0#;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Manchester Symbol threshold / SPI threshold Set and cleared by
      --  software. This field is used for Manchester mode, in order to define
      --  the expected symbol threshold levels. Please refer to Section :
      --  Manchester mode for details on computation. In addition this field is
      --  used to define the timeout value for the clock absence detection in
      --  Normal SPI mode. Values of STH[4:0] lower than 4 are invalid. This
      --  field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      STH            : MDF_SITF3CR_STH_Field := 16#1F#;
      --  unspecified
      Reserved_13_30 : HAL.UInt18 := 16#0#;
      --  Read-only. Serial interface Active flag Set and cleared by hardware.
      --  This flag must be used by the application in order to check if the
      --  serial interface is effectively enabled (active) or not. The
      --  protected fields of this function can only be updated when the
      --  SITFACTIVE is set , please refer to Section 1.4.15: Register
      --  protection for details. The delay between a transition on SITFEN and
      --  a transition on SITFACTIVE is 2 periods of AHB clock and 2 periods of
      --  mdf_proc_ck. - 0: The serial interface is not active, and can be
      --  configured if needed - 1: The serial interface is active, and
      --  protected fields cannot be configured.
      SITFACTIVE     : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_SITF3CR_Register use record
      SITFEN         at 0 range 0 .. 0;
      SCKSRC         at 0 range 1 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      SITFMOD        at 0 range 4 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      STH            at 0 range 8 .. 12;
      Reserved_13_30 at 0 range 13 .. 30;
      SITFACTIVE     at 0 range 31 .. 31;
   end record;

   subtype MDF_BSMX3CR_BSSEL_Field is HAL.UInt5;

   --  This register is used to select the bitstream to be provided to the
   --  corresponding digital filter and to the SCD.
   type MDF_BSMX3CR_Register is record
      --  Bitstream Selection Set and cleared by software. This field is used
      --  to select the bitstream to be processed for the digital filter x and
      --  for the SCDx. The size of this field depends on the number of DFLTx
      --  instantiated. If the BSSEL is selecting an input which is not
      --  instantiated, the MDF will select the valid stream bs[x]_F having the
      --  higher index number. - 00000: The bitstream bs[0]_R is provided to
      --  DFLTx and SCDx - 00001: The bitstream bs[0]_F is provided to DFLTx
      --  and SCDx - 00010: The bitstream bs[1]_R is provided to DFLTx and SCDx
      --  (if instantiated) - 00011: The bitstream bs[1]_F is provided to DFLTx
      --  and SCDx (if instantiated) ... - 11110: The bitstream bs[15]_R is
      --  provided to DFLTx and SCDx (if instantiated) - 11111: The bitstream
      --  bs[15]_F is provided to DFLTx and SCDx (if instantiated) This field
      --  can be write-protected, please refer to Section 1.4.15: Register
      --  protection for details.
      BSSEL         : MDF_BSMX3CR_BSSEL_Field := 16#0#;
      --  unspecified
      Reserved_5_30 : HAL.UInt26 := 16#0#;
      --  Read-only. BSMX Active flag Set and cleared by hardware. This flag
      --  must be used by the application in order to check if the BSMX is
      --  effectively enabled (active) or not. BSSEL[4:0] can only be updated
      --  when the BSMXACTIVE is set to a . The BSMXACTIVE flag is a logical
      --  between OLDACTIVE, DFLTACTIVE, and SCDACTIVE flags. Both of them must
      --  be set to a in order update BSSEL[4:0] field. - 0: The BSMX is not
      --  active, and can be configured if needed - 1: The BSMX is active, and
      --  protected fields cannot be configured.
      BSMXACTIVE    : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_BSMX3CR_Register use record
      BSSEL         at 0 range 0 .. 4;
      Reserved_5_30 at 0 range 5 .. 30;
      BSMXACTIVE    at 0 range 31 .. 31;
   end record;

   subtype MDF_DFLT3CR_ACQMOD_Field is HAL.UInt3;
   subtype MDF_DFLT3CR_TRGSRC_Field is HAL.UInt4;
   subtype MDF_DFLT3CR_NBDIS_Field is HAL.UInt8;

   --  This register is used to control the digital filter 3.
   type MDF_DFLT3CR_Register is record
      --  Write-only. Digital Filter Enable Set and cleared by software. This
      --  bit is used to control the start of acquisition of the corresponding
      --  digital filter path. The behavior of this bit depends on ACQMOD and
      --  external events. or the acquisition starts when the proper trigger
      --  event occurs if ACQMOD = 01x . The serial or parallel interface
      --  delivering the samples shall be enabled as well. - 0: The acquisition
      --  is stopped immediately - 1: The acquisition is immediately started if
      --  ACQMOD = 00x or 1xx ,
      DFLTEN         : Boolean := False;
      --  DMA Requests Enable Set and cleared by software. This bit is used to
      --  control the generation of DMA request in order to transfer the
      --  processed samples into the memory. - 0: The DMA interface for the
      --  corresponding digital filter is disabled - 1: The DMA interface for
      --  the corresponding digital filter is enabled This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      DMAEN          : Boolean := False;
      --  RXFIFO Threshold selection Set and cleared by software. This bit is
      --  used to select the RXFIFO threshold. This bit is not significant for
      --  RXFIFOs working in a interleaved transfer mode. Refer to Section
      --  1.4.13.4: Using the interleaved transfer mode for details. - 0:
      --  RXFIFO threshold event generated when the RXFIFO is not empty - 1:
      --  RXFIFO threshold event generated when the RXFIFO is half-full This
      --  field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      FTH            : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Digital filter Trigger mode Set and cleared by software. This field
      --  is used to select the filter trigger mode. - 000: Asynchronous,
      --  continuous acquisition mode - 001: Asynchronous, single-shot
      --  acquisition mode - 010: Synchronous, continuous acquisition mode -
      --  011: Synchronous, single-shot acquisition mode - 100: Window,
      --  continuous acquisition mode - 101: Synchronous, snapshot mode others:
      --  same a 000 This field can be write-protected, please refer to Section
      --  1.4.15: Register protection for details.
      ACQMOD         : MDF_DFLT3CR_ACQMOD_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Digital filter Trigger sensitivity selection Set and cleared by
      --  software. This field is used to select the trigger sensitivity of the
      --  external signals - 0: A rising edge event triggers the acquisition -
      --  1: A falling edge even triggers the acquisition Note that when the
      --  trigger source is TRGO or OLDx event, TRGSENS value is not taken into
      --  account. When TRGO is selected, the sensitivity is forced to falling
      --  edge, when OLDx event is selected, the sensitivity is forced to
      --  rising edge. This field can be write-protected, please refer to
      --  Section 1.4.15: Register protection for details.
      TRGSENS        : Boolean := False;
      --  unspecified
      Reserved_9_11  : HAL.UInt3 := 16#0#;
      --  Digital filter Trigger signal selection, Set and cleared by software.
      --  This field is used to select which external signals is used as
      --  trigger for the corresponding filter. - 0000: TRGO is selected -
      --  0001: OLDx event is selected - 0010: mdf_trg[0] is selected ... -
      --  1111: mdf_trg[13] is selected This field can be write-protected,
      --  please refer to Section 1.4.15: Register protection for details.
      TRGSRC         : MDF_DFLT3CR_TRGSRC_Field := 16#0#;
      --  Snapshot data format Set and cleared by software. This field is used
      --  to select the data format for the snapshot mode. - 0: The integrator
      --  counter (INT_CNT) is not inserted into the MDF_SNPSxDR register,
      --  leaving a data resolution of 23 bits. - 1: The integrator counter
      --  (INT_CNT) is inserted at position [15:9] of MDF_SNPSxDR register,
      --  leaving a data resolution of 16 bits. This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      SNPSFMT        : Boolean := False;
      --  unspecified
      Reserved_17_19 : HAL.UInt3 := 16#0#;
      --  Number of samples to be discarded Set and cleared by software. This
      --  field is used to define the number of samples to be discarded every
      --  time the DFLTx is re-started. - 0: no sample discarded - 1: 1 sample
      --  discarded - 2: 2 samples discarded ... - 255: 255 samples discarded
      --  This field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      NBDIS          : MDF_DFLT3CR_NBDIS_Field := 16#0#;
      --  unspecified
      Reserved_28_29 : HAL.UInt2 := 16#0#;
      --  Read-only. Digital filter Run Status Flag Set and cleared by
      --  hardware. This bit indicates if the digital filter is running or not.
      --  - 0: The digital filter is not running, and ready to accept a new
      --  trigger event - 1: The digital filter is running
      DFLTRUN        : Boolean := False;
      --  Read-only. Digital filter Active Flag Set and cleared by hardware.
      --  This bit indicates if the digital filter is active: can be running or
      --  waiting for events. - 0: The digital filter is not active, and can be
      --  re-enabled again (via DFLTEN bit) if needed - 1: The digital filter
      --  is active
      DFLTACTIVE     : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT3CR_Register use record
      DFLTEN         at 0 range 0 .. 0;
      DMAEN          at 0 range 1 .. 1;
      FTH            at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      ACQMOD         at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      TRGSENS        at 0 range 8 .. 8;
      Reserved_9_11  at 0 range 9 .. 11;
      TRGSRC         at 0 range 12 .. 15;
      SNPSFMT        at 0 range 16 .. 16;
      Reserved_17_19 at 0 range 17 .. 19;
      NBDIS          at 0 range 20 .. 27;
      Reserved_28_29 at 0 range 28 .. 29;
      DFLTRUN        at 0 range 30 .. 30;
      DFLTACTIVE     at 0 range 31 .. 31;
   end record;

   subtype MDF_DFLT3CICR_DATSRC_Field is HAL.UInt2;
   subtype MDF_DFLT3CICR_CICMOD_Field is HAL.UInt3;
   subtype MDF_DFLT3CICR_MCICD_Field is HAL.UInt9;
   subtype MDF_DFLT3CICR_SCALE_Field is HAL.UInt6;

   --  This register is used to control the main CIC filter.
   type MDF_DFLT3CICR_Register is record
      --  Source data for the digital filter Set and cleared by software. 0x:
      --  Select the stream coming from the BSMX - 10: Select the stream coming
      --  from the ADCITF1 - 11: Select the stream coming from the ADCITF2 This
      --  field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      DATSRC         : MDF_DFLT3CICR_DATSRC_Field := 16#0#;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  Select the CIC mode Set and cleared by software. This field allows
      --  the application to select the configuration and the order of the
      --  MCIC. When CICMOD[2:0] is equal to 0xx , the CIC is split into two
      --  filters: - The main CIC (MCIC) - The auxiliary CIC (ACIC), used for
      --  the out-off limit detector - 000: The CIC is split into 2 filters,
      --  and the main CIC (MCIC) is configured in FastSinc filter - 001: The
      --  CIC is split into 2 filters, and the main CIC (MCIC) is configured in
      --  Sinc1 filter - 010: The CIC is split into 2 filters, and the main CIC
      --  (MCIC) is configured in Sinc2 filter - 011: The CIC is split into 2
      --  filters, and the main CIC (MCIC) is configured in Sinc3 filter - 100:
      --  The CIC is configured in single sinc4 filter others: The CIC is
      --  configured in single sinc5 filter This field can be write-protected,
      --  please refer to Section 1.4.15: Register protection for details.
      CICMOD         : MDF_DFLT3CICR_CICMOD_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  CIC decimation ratio selection Set and cleared by software. This bit
      --  is used to allow the application to select the decimation ratio of
      --  the CIC. Decimation ratio smaller than 2 is not allowed. The
      --  decimation ratio is given by (CICDEC+1). - 0: Decimation ratio is 2 -
      --  1: Decimation ratio is 2 - 2: Decimation ratio is 3 - 3: Decimation
      --  ratio is 4 ... - 511: Decimation ratio is 512 This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      MCICD          : MDF_DFLT3CICR_MCICD_Field := 16#0#;
      --  unspecified
      Reserved_17_19 : HAL.UInt3 := 16#0#;
      --  Scaling factor selection Set and cleared by software. This field is
      --  used to allow the application to select the gain to be applied at CIC
      --  output. Please refer to Table 13: Possible gain values for details.
      --  If the application attempts to write a new gain value while the
      --  previous one is not yet applied, this new gain value is ignored.
      --  Reading back the SCALE[5:0] field will inform the application on the
      --  current gain value. - 100000: - 48.2 dB, or shift right by 8 bits
      --  (default value) - 100001: - 44.6 dB, - 100010: - 42.1 dB, or shift
      --  right by 7 bits - 100011: - 38.6 dB, ... - 101110: -6 dB, or shift
      --  right by 1 bit - 101111: -2.5 dB, - 000000: 0 dB - 000001: + 3.5 dB,
      --  - 000010: + 6 dB, or shift left by 1 bit ... - 011000: + 72 dB, or
      --  shift left by 12 bits
      SCALE          : MDF_DFLT3CICR_SCALE_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT3CICR_Register use record
      DATSRC         at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      CICMOD         at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      MCICD          at 0 range 8 .. 16;
      Reserved_17_19 at 0 range 17 .. 19;
      SCALE          at 0 range 20 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype MDF_DFLT3RSFR_HPFC_Field is HAL.UInt2;

   --  This register is used to control the reshape and HPF filters.
   type MDF_DFLT3RSFR_Register is record
      --  Reshaper filter bypass Set and cleared by software. This bit is used
      --  to bypass the reshape filter and its decimation block. - 0: The
      --  reshape filter is not bypassed (Default value) - 1: The reshape
      --  filter is bypassed This field can be write-protected, please refer to
      --  Section 1.4.15: Register protection for details.
      RSFLTBYP       : Boolean := False;
      --  unspecified
      Reserved_1_3   : HAL.UInt3 := 16#0#;
      --  Reshaper filter decimation ratio Set and cleared by software. This
      --  bit is used to select the decimation ratio for the reshape filter -
      --  0: Decimation ratio is 4 (Default value) - 1: Decimation ratio is 1
      --  This field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      RSFLTD         : Boolean := False;
      --  unspecified
      Reserved_5_6   : HAL.UInt2 := 16#0#;
      --  High-Pass Filter bypass Set and cleared by software. This bit is used
      --  to bypass the high-pass filter. - 0: The high pass filter is not
      --  bypassed (Default value) - 1: The high pass filter is bypassed This
      --  field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      HPFBYP         : Boolean := False;
      --  High-pass filter cut-off frequency Set and cleared by software. This
      --  field is used to select the cut-off frequency of the high-pass
      --  filter. FPCM represents the sampling frequency at HPF input. - 00:
      --  Cut-off frequency = 0.000625 x FPCM - 01: Cut-off frequency = 0.00125
      --  x FPCM - 10: Cut-off frequency = 0.00250 x FPCM - 11: Cut-off
      --  frequency = 0.00950 x FPCM This field can be write-protected, please
      --  refer to Section 1.4.15: Register protection for details.
      HPFC           : MDF_DFLT3RSFR_HPFC_Field := 16#0#;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT3RSFR_Register use record
      RSFLTBYP       at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      RSFLTD         at 0 range 4 .. 4;
      Reserved_5_6   at 0 range 5 .. 6;
      HPFBYP         at 0 range 7 .. 7;
      HPFC           at 0 range 8 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype MDF_DFLT3INTR_INTDIV_Field is HAL.UInt2;
   subtype MDF_DFLT3INTR_INTVAL_Field is HAL.UInt7;

   --  This register is used to the integrator (INT) settings.
   type MDF_DFLT3INTR_Register is record
      --  Integrator output division Set and cleared by software. This bit is
      --  used to rescale the signal at the integrator output in order keep the
      --  data width lower than 24 bits. - 00: The integrator data outputs are
      --  divided by 128 (Default value) - 01: The integrator data outputs are
      --  divided by 32 - 10: The integrator data outputs are divided by 4 -
      --  11: The integrator data outputs are not divided This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      INTDIV         : MDF_DFLT3INTR_INTDIV_Field := 16#0#;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  Integration value selection Set and cleared by software. This field
      --  is used to select the integration value. - 0: The integration value
      --  is 1, meaning bypass mode (default after reset) - 1: The integration
      --  value is 2 - 2: The integration value is 3 ... - 127: The integration
      --  value is 128 This field can be write-protected, please refer to
      --  Section 1.4.15: Register protection for details.
      INTVAL         : MDF_DFLT3INTR_INTVAL_Field := 16#0#;
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT3INTR_Register use record
      INTDIV         at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      INTVAL         at 0 range 4 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   subtype MDF_OLD3CR_BKOLD_Field is HAL.UInt4;
   subtype MDF_OLD3CR_ACICN_Field is HAL.UInt2;
   subtype MDF_OLD3CR_ACICD_Field is HAL.UInt5;

   --  This register is used to configure the Out-of Limit Detector function.
   type MDF_OLD3CR_Register is record
      --  Over-Current Detector Enable Set and cleared by software. - 0: The
      --  OLD is disabled (Default value) - 1: The OLD is enabled, including
      --  the ACIC filter working in continuous mode.
      OLDEN          : Boolean := False;
      --  Threshold In band Set and cleared by software. - 0: The OLD generates
      --  an event if the signal is lower than OLDTHL OR higher than OLDTHH
      --  (Default value) - 1: The OLD generates an event if the signal is
      --  lower than OLDTHH AND higher than OLDTHL This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      THINB          : Boolean := False;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  Break signal assignment for out-of limit detector Set and cleared by
      --  software. BKOLD[i] = 0: Break signal (mdf_break[i]) is not assigned
      --  to threshold event BKOLD[i] = 1: Break signal (mdf_break[i]) is
      --  assigned to threshold event This field can be write-protected, please
      --  refer to Section 1.4.15: Register protection for details.
      BKOLD          : MDF_OLD3CR_BKOLD_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : HAL.UInt4 := 16#0#;
      --  OLD CIC order selection Set and cleared by software. This field
      --  allows the application to select the type, and the order of the ACIC.
      --  This field is only taken into account by the MDF when CICMOD[2:0] =
      --  0xx . - 00: FastSinc filter type - 01: Sinc1 filter type - 10: Sinc2
      --  filter type - 11: Sinc3 filter type This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      ACICN          : MDF_OLD3CR_ACICN_Field := 16#0#;
      --  unspecified
      Reserved_14_16 : HAL.UInt3 := 16#0#;
      --  OLD CIC decimation ratio selection Set and cleared by software. This
      --  field is used to allow the application to select the decimation ratio
      --  of the ACIC. This field is only taken into account by the MDF when
      --  CICMOD[2:0] = 0xx . The decimation ratio is given by (ACICD+1). - 0:
      --  Decimation ratio is 1 - 1: Decimation ratio is 2 - 2: Decimation
      --  ratio is 3 - 3: Decimation ratio is 4 ... - 31: Decimation ratio is
      --  32 This field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      ACICD          : MDF_OLD3CR_ACICD_Field := 16#0#;
      --  unspecified
      Reserved_22_30 : HAL.UInt9 := 16#0#;
      --  Read-only. OLD Active flag Set and cleared by hardware. This flag
      --  must be used by the application in order to check if the OLD is
      --  effectively enabled (active) or not. The protected fields and
      --  registers of this function can only be updated when the OLDACTIVE is
      --  set to , please refer to Section 1.4.15: Register protection for
      --  details. The delay between a transition on OLDEN and a transition on
      --  OLDACTIVE is 2 periods of AHB clock and 2 periods of mdf_proc_ck. -
      --  0: The OLD is not active, and can be configured if needed - 1: The
      --  OLD is active, and protected fields cannot be configured.
      OLDACTIVE      : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_OLD3CR_Register use record
      OLDEN          at 0 range 0 .. 0;
      THINB          at 0 range 1 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      BKOLD          at 0 range 4 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      ACICN          at 0 range 12 .. 13;
      Reserved_14_16 at 0 range 14 .. 16;
      ACICD          at 0 range 17 .. 21;
      Reserved_22_30 at 0 range 22 .. 30;
      OLDACTIVE      at 0 range 31 .. 31;
   end record;

   subtype MDF_OLD3THLR_OLDTHL_Field is HAL.UInt26;

   --  This register is used for the adjustment of the Out-off Limit low
   --  threshold.
   type MDF_OLD3THLR_Register is record
      --  OLD Low Threshold Value Set and cleared by software. OLDTHL
      --  represents a 26-bit signed value. The real threshold compared to the
      --  signal provided by the filter is OLDTHL. This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      OLDTHL         : MDF_OLD3THLR_OLDTHL_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_OLD3THLR_Register use record
      OLDTHL         at 0 range 0 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype MDF_OLD3THHR_OLDTHH_Field is HAL.UInt26;

   --  This register is used for the adjustment of the Out-off Limit high
   --  threshold.
   type MDF_OLD3THHR_Register is record
      --  OLD High Threshold Value Set and cleared by software. OLDTHH
      --  represents a 26-bit signed value. The real threshold compared to the
      --  signal provided by the filter is OLDTHH. This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details
      OLDTHH         : MDF_OLD3THHR_OLDTHH_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_OLD3THHR_Register use record
      OLDTHH         at 0 range 0 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype MDF_DLY3CR_SKPDLY_Field is HAL.UInt7;

   --  This register is used for the adjustment stream delays.
   type MDF_DLY3CR_Register is record
      --  Delay to apply to a bitstream Set and cleared by software. Defines
      --  the number of input samples that will be skipped. Skipping is applied
      --  immediately after writing to this field, if SKPBF = 0 , and the
      --  corresponding bit DFLTEN = 1 . If SKPBF = 1 the value written into
      --  the register is ignored by the delay state machine. - 0: No input
      --  sample skipped, - 1: 1 input sample skipped, ... - 127: 127 input
      --  sample skipped,
      SKPDLY        : MDF_DLY3CR_SKPDLY_Field := 16#0#;
      --  unspecified
      Reserved_7_30 : HAL.UInt24 := 16#0#;
      --  Read-only. Skip Busy flag Set and cleared by hardware. Shall be used
      --  in order to control if the delay sequence is completed. - 0: Reading
      --  0 means that the MDF is ready to accept a new value into SKPDLY[6:0].
      --  - 1: Reading 1 means that last valid SKPDLY[6:0] is still under
      --  precessing.
      SKPBF         : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DLY3CR_Register use record
      SKPDLY        at 0 range 0 .. 6;
      Reserved_7_30 at 0 range 7 .. 30;
      SKPBF         at 0 range 31 .. 31;
   end record;

   subtype MDF_SCD3CR_BKSCD_Field is HAL.UInt4;
   subtype MDF_SCD3CR_SCDT_Field is HAL.UInt8;

   --  This register is used for the adjustment stream delays.
   type MDF_SCD3CR_Register is record
      --  Short circuit detector enable Set and cleared by software. - 0: The
      --  short circuit detector is disabled, - 1: The short circuit detector
      --  is enabled,
      SCDEN          : Boolean := False;
      --  unspecified
      Reserved_1_3   : HAL.UInt3 := 16#0#;
      --  Break signal assignment for short circuit detector Set and cleared by
      --  software. BKSCD[i] = 0: Break signal (mdf_break[i]) is not assigned
      --  to this SCD event BKSCD[i] = 1: Break signal (mdf_break[i]) is
      --  assigned to this SCD event This field can be write-protected, please
      --  refer to Section 1.4.15: Register protection for details.
      BKSCD          : MDF_SCD3CR_BKSCD_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : HAL.UInt4 := 16#0#;
      --  Short-circuit detector threshold Set and cleared by software. These
      --  bits are written by software to define the threshold counter for the
      --  short-circuit detector. If this value is reached, then a
      --  short-circuit detector event occurs on a given input stream. - 0: 2
      --  consecutive 1 s or 0 s will generate an event, - 1: 2 consecutive 1 s
      --  or 0 s will generate an event - 2: 3 consecutive 1 s or 0 s will
      --  generate an event, ... - 255: 256 consecutive 1 s or 0 s will
      --  generate an event, This field can be write-protected, please refer to
      --  Section 1.4.15: Register protection for details.
      SCDT           : MDF_SCD3CR_SCDT_Field := 16#0#;
      --  unspecified
      Reserved_20_30 : HAL.UInt11 := 16#0#;
      --  Read-only. SCD Active flag Set and cleared by hardware. This flag
      --  must be used by the application in order to check if the SCD is
      --  effectively enabled (active) or not. The protected fields of this
      --  function can only be updated when the SCDACTIVE is set to a , please
      --  refer to Section 1.4.15: Register protection for details. The delay
      --  between a transition on SCDEN and a transition on SCDACTIVE is 2
      --  periods of AHB clock and 2 periods of mdf_proc_ck. - 0: The SCD is
      --  not active, and can be configured if needed - 1: The SCD is active,
      --  and protected fields cannot be configured.
      SCDACTIVE      : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_SCD3CR_Register use record
      SCDEN          at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      BKSCD          at 0 range 4 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      SCDT           at 0 range 12 .. 19;
      Reserved_20_30 at 0 range 20 .. 30;
      SCDACTIVE      at 0 range 31 .. 31;
   end record;

   --  MDF DFLTx interrupt enable register x
   type MDF_DFLT3IER_Register is record
      --  RXFIFO threshold interrupt enable Set and cleared by software. - 0:
      --  RXFIFO threshold interrupt disabled - 1: RXFIFO threshold interrupt
      --  enabled
      FTHIE          : Boolean := False;
      --  Data overflow interrupt enable Set and cleared by software. - 0: Data
      --  overflow interrupt disabled - 1: Data overflow interrupt enabled
      DOVRIE         : Boolean := False;
      --  Snapshot data ready interrupt enable Set and cleared by software. -
      --  0: Snapshot data ready interrupt disabled - 1: Snapshot data ready
      --  interrupt enabled
      SSDRIE         : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Out-of Limit interrupt enable Set and cleared by software. - 0: OLD
      --  event interrupt disabled - 1: OLD event interrupt enabled
      OLDIE          : Boolean := False;
      --  unspecified
      Reserved_5_6   : HAL.UInt2 := 16#0#;
      --  Snapshot overrun interrupt enable Set and cleared by software. - 0:
      --  Snapshot overrun interrupt disabled - 1: Snapshot overrun interrupt
      --  enabled
      SSOVRIE        : Boolean := False;
      --  Short-Circuit Detector interrupt enable Set and cleared by software.
      --  - 0: SCD interrupt disabled - 1: SCD interrupt enabled
      SCDIE          : Boolean := False;
      --  Saturation detection interrupt enable Set and cleared by software. -
      --  0: Saturation interrupt disabled - 1: Saturation interrupt enabled
      SATIE          : Boolean := False;
      --  Clock absence detection interrupt enable Set and cleared by software.
      --  - 0: Clock absence interrupt disabled - 1: Clock absence interrupt
      --  enabled
      CKABIE         : Boolean := False;
      --  Reshape Filter Overrun interrupt enable Set and cleared by software.
      --  - 0: Reshape filter overrun interrupt disabled - 1: Reshape filter
      --  overrun interrupt enabled
      RFOVRIE        : Boolean := False;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT3IER_Register use record
      FTHIE          at 0 range 0 .. 0;
      DOVRIE         at 0 range 1 .. 1;
      SSDRIE         at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      OLDIE          at 0 range 4 .. 4;
      Reserved_5_6   at 0 range 5 .. 6;
      SSOVRIE        at 0 range 7 .. 7;
      SCDIE          at 0 range 8 .. 8;
      SATIE          at 0 range 9 .. 9;
      CKABIE         at 0 range 10 .. 10;
      RFOVRIE        at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   --  This register contains the status flags for each digital filter path.
   type MDF_DFLT3ISR_Register is record
      --  Read-only. RXFIFO threshold flag Set by hardware, and cleared by
      --  software by writing this bit to 1 . - 0: Reading 0 means that the
      --  RXFIFO threshold is not reached, writing 0 has no effect. - 1:
      --  Reading 1 means that the RXFIFO reached the threshold, writing 1
      --  clears this flag.
      FTHF           : Boolean := False;
      --  Data overflow flag Set by hardware, and cleared by software by
      --  writing this bit to 1 . - 0: Reading 0 means that no overflow is
      --  detected, writing 0 has no effect. - 1: Reading 1 means that an
      --  overflow is detected, writing 1 clears this flag.
      DOVRF          : Boolean := False;
      --  Snapshot data ready flag Set by hardware, and cleared by software by
      --  writing this bit to 1 . - 0: Reading 0 means that no data is
      --  available on MDF_SNPSxDR, writing 0 has no effect. - 1: Reading 1
      --  means that a new data is available on MDF_SNPSxDR, writing 1 clears
      --  this flag.
      SSDRF          : Boolean := False;
      --  Read-only. RXFIFO Not Empty flag Set and cleared by hardware
      --  according to the RXFIFO level. - 0: Reading 0 means that the RXFIFO
      --  is empty. - 1: Reading 1 means that the RXFIFO is not empty.
      RXNEF          : Boolean := False;
      --  Out-of Limit Detector flag Set by hardware, and cleared by software
      --  by writing this bit to 1 . - 0: Reading 0 means that no OLD event is
      --  detected, writing 0 has no effect. - 1: Reading 1 means that an OLD
      --  event is detected, writing 1 clears THHF, THLF and OLDF flags.
      OLDF           : Boolean := False;
      --  Read-only. Low threshold status flag Set by hardware, and cleared by
      --  software by writing OLDF bit to 1 . This flag indicates the status of
      --  the low threshold comparator when the last OLD event occurred. This
      --  bit gives additional information on the conditions triggering the
      --  last OLD event. It can be cleared by writing OLDF flag to a 1. - 0:
      --  The signal was lower than OLDTHL, when the last OLD event occurred -
      --  1: The signal was higher than OLDTHL, when the last OLD event
      --  occurred
      THLF           : Boolean := False;
      --  Read-only. High threshold status flag Set by hardware, and cleared by
      --  software by writing OLDF bit to 1 . This flag indicates the status of
      --  the high threshold comparator when the last OLD event occurred. This
      --  bit gives additional information on the conditions triggering the
      --  last OLD event. It can be cleared by writing OLDF flag to a 1. - 0:
      --  The signal was lower than OLDTHH, when the last OLD event occurred -
      --  1: The signal was higher than OLDTHH, when the last OLD event
      --  occurred
      THHF           : Boolean := False;
      --  Snapshot overrun flag Set by hardware, and cleared by software by
      --  writing this bit to 1 . - 0: Reading 0 means that no snapshot overrun
      --  event is detected, writing 0 has no effect. - 1: Reading 1 means that
      --  a snapshot overrun event is detected, writing 1 clears this flag.
      SSOVRF         : Boolean := False;
      --  Short-Circuit Detector flag Set by hardware, and cleared by software
      --  by writing this bit to 1 . - 0: Reading 0 means that no SCD event is
      --  detected, writing 0 has no effect. - 1: Reading 1 means that a SCD
      --  event is detected, writing 1 clears this flag.
      SCDF           : Boolean := False;
      --  Saturation detection flag Set by hardware, and cleared by software by
      --  writing this bit to 1 . - 0: Reading 0 means that no saturation is
      --  detected, writing 0 has no effect. - 1: Reading 1 means that a
      --  saturation is detected, writing 1 clears this flag.
      SATF           : Boolean := False;
      --  Clock absence detection flag Set by hardware, and cleared by software
      --  by writing this bit to 1 . - 0: Reading 0 means that no clock absence
      --  is detected, writing 0 has no effect. - 1: Reading 1 means that a
      --  clock absence is detected, writing 1 clears this flag.
      CKABF          : Boolean := False;
      --  Reshape Filter Overrun detection flag Set by hardware, and cleared by
      --  software by writing this bit to 1 . - 0: Reading 0 means that no
      --  reshape filter overrun is detected, writing 0 has no effect. - 1:
      --  Reading 1 means that reshape filter overrun is detected, writing 1
      --  clears this flag.
      RFOVRF         : Boolean := False;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT3ISR_Register use record
      FTHF           at 0 range 0 .. 0;
      DOVRF          at 0 range 1 .. 1;
      SSDRF          at 0 range 2 .. 2;
      RXNEF          at 0 range 3 .. 3;
      OLDF           at 0 range 4 .. 4;
      THLF           at 0 range 5 .. 5;
      THHF           at 0 range 6 .. 6;
      SSOVRF         at 0 range 7 .. 7;
      SCDF           at 0 range 8 .. 8;
      SATF           at 0 range 9 .. 9;
      CKABF          at 0 range 10 .. 10;
      RFOVRF         at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype MDF_OEC3CR_OFFSET_Field is HAL.UInt26;

   --  This register contains the offset compensation value.
   type MDF_OEC3CR_Register is record
      --  Offset error compensation Set and cleared by software. If the
      --  application attempts to write a new offset value while the previous
      --  one is not yet applied, this new offset value is ignored. Reading
      --  back the OFFSET[25:0] field will inform the application on the
      --  current offset value. OFFSET[25:0] represents the value to be
      --  subtracted to the signal before going to the SCALE.
      OFFSET         : MDF_OEC3CR_OFFSET_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_OEC3CR_Register use record
      OFFSET         at 0 range 0 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype MDF_SNPS3DR_MCICDC_Field is HAL.UInt9;
   subtype MDF_SNPS3DR_EXTSDR_Field is HAL.UInt7;
   subtype MDF_SNPS3DR_SDR_Field is HAL.UInt16;

   --  This register is used to read the data processed by each digital filter
   --  in snapshot mode.
   type MDF_SNPS3DR_Register is record
      --  Read-only. Contains the MCIC decimation counter value at the moment
      --  of the last trigger event occurs (MCIC_CNT)
      MCICDC : MDF_SNPS3DR_MCICDC_Field;
      --  Read-only. Extended data size If SNPSFMT = 0 , EXTSDR[6:0] contains
      --  the bit 7 to 1 of the last valid data processed by the digital
      --  filter, If SNPSFMT = 1 , this field contains the INT accumulator
      --  counter value at the moment of the last trigger event occurs
      --  (INT_CNT).
      EXTSDR : MDF_SNPS3DR_EXTSDR_Field;
      --  Read-only. Contains the 16 MSB of the last valid data processed by
      --  the digital filter.
      SDR    : MDF_SNPS3DR_SDR_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_SNPS3DR_Register use record
      MCICDC at 0 range 0 .. 8;
      EXTSDR at 0 range 9 .. 15;
      SDR    at 0 range 16 .. 31;
   end record;

   subtype MDF_DFLT3DR_DR_Field is HAL.UInt24;

   --  This register is used to read the data processed by each digital filter.
   type MDF_DFLT3DR_Register is record
      --  unspecified
      Reserved_0_7 : HAL.UInt8;
      --  Read-only. Data processed by digital filter.
      DR           : MDF_DFLT3DR_DR_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT3DR_Register use record
      Reserved_0_7 at 0 range 0 .. 7;
      DR           at 0 range 8 .. 31;
   end record;

   subtype MDF_SITF4CR_SCKSRC_Field is HAL.UInt2;
   subtype MDF_SITF4CR_SITFMOD_Field is HAL.UInt2;
   subtype MDF_SITF4CR_STH_Field is HAL.UInt5;

   --  This register is used to control the serial interfaces (SITFx).
   type MDF_SITF4CR_Register is record
      --  Serial interface enable Set and cleared by software. This bit is used
      --  to enable/disable the serial interface. - 0: Serial interface
      --  disabled - 1: Serial interface enabled
      SITFEN         : Boolean := False;
      --  Serial clock source Set and cleared by software. This bit is used to
      --  select the clock source of the serial interface. - 00: Serial clock
      --  source is MDF_CCK0 - 01: Serial clock source is MDF_CCK1 1x: Serial
      --  clock source is MDF_CKIx, not allowed in LF_MASTER SPI mode This
      --  field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      SCKSRC         : MDF_SITF4CR_SCKSRC_Field := 16#0#;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Serial interface type Set and cleared by software. This field is used
      --  to defined the serial interface type. - 00: LF_MASTER (Low-Frequency
      --  MASTER) SPI mode - 01: Normal SPI mode - 10: Manchester mode: rising
      --  edge = logic 0, falling edge = logic 1 - 11: Manchester mode: rising
      --  edge = logic 1, falling edge = logic 0 This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      SITFMOD        : MDF_SITF4CR_SITFMOD_Field := 16#0#;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Manchester Symbol threshold / SPI threshold Set and cleared by
      --  software. This field is used for Manchester mode, in order to define
      --  the expected symbol threshold levels. Please refer to Section :
      --  Manchester mode for details on computation. In addition this field is
      --  used to define the timeout value for the clock absence detection in
      --  Normal SPI mode. Values of STH[4:0] lower than 4 are invalid. This
      --  field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      STH            : MDF_SITF4CR_STH_Field := 16#1F#;
      --  unspecified
      Reserved_13_30 : HAL.UInt18 := 16#0#;
      --  Read-only. Serial interface Active flag Set and cleared by hardware.
      --  This flag must be used by the application in order to check if the
      --  serial interface is effectively enabled (active) or not. The
      --  protected fields of this function can only be updated when the
      --  SITFACTIVE is set , please refer to Section 1.4.15: Register
      --  protection for details. The delay between a transition on SITFEN and
      --  a transition on SITFACTIVE is 2 periods of AHB clock and 2 periods of
      --  mdf_proc_ck. - 0: The serial interface is not active, and can be
      --  configured if needed - 1: The serial interface is active, and
      --  protected fields cannot be configured.
      SITFACTIVE     : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_SITF4CR_Register use record
      SITFEN         at 0 range 0 .. 0;
      SCKSRC         at 0 range 1 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      SITFMOD        at 0 range 4 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      STH            at 0 range 8 .. 12;
      Reserved_13_30 at 0 range 13 .. 30;
      SITFACTIVE     at 0 range 31 .. 31;
   end record;

   subtype MDF_BSMX4CR_BSSEL_Field is HAL.UInt5;

   --  This register is used to select the bitstream to be provided to the
   --  corresponding digital filter and to the SCD.
   type MDF_BSMX4CR_Register is record
      --  Bitstream Selection Set and cleared by software. This field is used
      --  to select the bitstream to be processed for the digital filter x and
      --  for the SCDx. The size of this field depends on the number of DFLTx
      --  instantiated. If the BSSEL is selecting an input which is not
      --  instantiated, the MDF will select the valid stream bs[x]_F having the
      --  higher index number. - 00000: The bitstream bs[0]_R is provided to
      --  DFLTx and SCDx - 00001: The bitstream bs[0]_F is provided to DFLTx
      --  and SCDx - 00010: The bitstream bs[1]_R is provided to DFLTx and SCDx
      --  (if instantiated) - 00011: The bitstream bs[1]_F is provided to DFLTx
      --  and SCDx (if instantiated) ... - 11110: The bitstream bs[15]_R is
      --  provided to DFLTx and SCDx (if instantiated) - 11111: The bitstream
      --  bs[15]_F is provided to DFLTx and SCDx (if instantiated) This field
      --  can be write-protected, please refer to Section 1.4.15: Register
      --  protection for details.
      BSSEL         : MDF_BSMX4CR_BSSEL_Field := 16#0#;
      --  unspecified
      Reserved_5_30 : HAL.UInt26 := 16#0#;
      --  Read-only. BSMX Active flag Set and cleared by hardware. This flag
      --  must be used by the application in order to check if the BSMX is
      --  effectively enabled (active) or not. BSSEL[4:0] can only be updated
      --  when the BSMXACTIVE is set to . The BSMXACTIVE flag is a logical
      --  between OLDACTIVE, DFLTACTIVE, and SCDACTIVE flags. Both of them must
      --  be set to in order update BSSEL[4:0] field. - 0: The BSMX is not
      --  active, and can be configured if needed - 1: The BSMX is active, and
      --  protected fields cannot be configured.
      BSMXACTIVE    : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_BSMX4CR_Register use record
      BSSEL         at 0 range 0 .. 4;
      Reserved_5_30 at 0 range 5 .. 30;
      BSMXACTIVE    at 0 range 31 .. 31;
   end record;

   subtype MDF_DFLT4CR_ACQMOD_Field is HAL.UInt3;
   subtype MDF_DFLT4CR_TRGSRC_Field is HAL.UInt4;
   subtype MDF_DFLT4CR_NBDIS_Field is HAL.UInt8;

   --  This register is used to control the digital filter 4.
   type MDF_DFLT4CR_Register is record
      --  Write-only. Digital Filter Enable Set and cleared by software. This
      --  bit is used to control the start of acquisition of the corresponding
      --  digital filter path. The behavior of this bit depends on ACQMOD and
      --  external events. or the acquisition starts when the proper trigger
      --  event occurs if ACQMOD = 01x . The serial or parallel interface
      --  delivering the samples shall be enabled as well. - 0: The acquisition
      --  is stopped immediately - 1: The acquisition is immediately started if
      --  ACQMOD = 00x or 1xx ,
      DFLTEN         : Boolean := False;
      --  DMA Requests Enable Set and cleared by software. This bit is used to
      --  control the generation of DMA request in order to transfer the
      --  processed samples into the memory. - 0: The DMA interface for the
      --  corresponding digital filter is disabled - 1: The DMA interface for
      --  the corresponding digital filter is enabled This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      DMAEN          : Boolean := False;
      --  RXFIFO Threshold selection Set and cleared by software. This bit is
      --  used to select the RXFIFO threshold. This bit is not significant for
      --  RXFIFOs working in a interleaved transfer mode. Refer to Section
      --  1.4.13.4: Using the interleaved transfer mode for details. - 0:
      --  RXFIFO threshold event generated when the RXFIFO is not empty - 1:
      --  RXFIFO threshold event generated when the RXFIFO is half-full This
      --  field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      FTH            : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Digital filter Trigger mode Set and cleared by software. This field
      --  is used to select the filter trigger mode. - 000: Asynchronous,
      --  continuous acquisition mode - 001: Asynchronous, single-shot
      --  acquisition mode - 010: Synchronous, continuous acquisition mode -
      --  011: Synchronous, single-shot acquisition mode - 100: Window,
      --  continuous acquisition mode - 101: Synchronous, snapshot mode others:
      --  same a 000 This field can be write-protected, please refer to Section
      --  1.4.15: Register protection for details.
      ACQMOD         : MDF_DFLT4CR_ACQMOD_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Digital filter Trigger sensitivity selection Set and cleared by
      --  software. This field is used to select the trigger sensitivity of the
      --  external signals - 0: A rising edge event triggers the acquisition -
      --  1: A falling edge even triggers the acquisition Note that when the
      --  trigger source is TRGO or OLDx event, TRGSENS value is not taken into
      --  account. When TRGO is selected, the sensitivity is forced to falling
      --  edge, when OLDx event is selected, the sensitivity is forced to
      --  rising edge. This field can be write-protected, please refer to
      --  Section 1.4.15: Register protection for details.
      TRGSENS        : Boolean := False;
      --  unspecified
      Reserved_9_11  : HAL.UInt3 := 16#0#;
      --  Digital filter Trigger signal selection, Set and cleared by software.
      --  This field is used to select which external signals is used as
      --  trigger for the corresponding filter. - 0000: TRGO is selected -
      --  0001: OLDx event is selected - 0010: mdf_trg[0] is selected ... -
      --  1111: mdf_trg[13] is selected This field can be write-protected,
      --  please refer to Section 1.4.15: Register protection for details.
      TRGSRC         : MDF_DFLT4CR_TRGSRC_Field := 16#0#;
      --  Snapshot data format Set and cleared by software. This field is used
      --  to select the data format for the snapshot mode. - 0: The integrator
      --  counter (INT_CNT) is not inserted into the MDF_SNPSxDR register,
      --  leaving a data resolution of 23 bits. - 1: The integrator counter
      --  (INT_CNT) is inserted at position [15:9] of MDF_SNPSxDR register,
      --  leaving a data resolution of 16 bits. This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      SNPSFMT        : Boolean := False;
      --  unspecified
      Reserved_17_19 : HAL.UInt3 := 16#0#;
      --  Number of samples to be discarded Set and cleared by software. This
      --  field is used to define the number of samples to be discarded every
      --  time the DFLTx is re-started. - 0: no sample discarded - 1: 1 sample
      --  discarded - 2: 2 samples discarded ... - 255: 255 samples discarded
      --  This field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      NBDIS          : MDF_DFLT4CR_NBDIS_Field := 16#0#;
      --  unspecified
      Reserved_28_29 : HAL.UInt2 := 16#0#;
      --  Read-only. Digital filter Run Status Flag Set and cleared by
      --  hardware. This bit indicates if the digital filter is running or not.
      --  - 0: The digital filter is not running, and ready to accept a new
      --  trigger event - 1: The digital filter is running
      DFLTRUN        : Boolean := False;
      --  Read-only. Digital filter Active Flag Set and cleared by hardware.
      --  This bit indicates if the digital filter is active: can be running or
      --  waiting for events. - 0: The digital filter is not active, and can be
      --  re-enabled again (via DFLTEN bit) if needed - 1: The digital filter
      --  is active
      DFLTACTIVE     : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT4CR_Register use record
      DFLTEN         at 0 range 0 .. 0;
      DMAEN          at 0 range 1 .. 1;
      FTH            at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      ACQMOD         at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      TRGSENS        at 0 range 8 .. 8;
      Reserved_9_11  at 0 range 9 .. 11;
      TRGSRC         at 0 range 12 .. 15;
      SNPSFMT        at 0 range 16 .. 16;
      Reserved_17_19 at 0 range 17 .. 19;
      NBDIS          at 0 range 20 .. 27;
      Reserved_28_29 at 0 range 28 .. 29;
      DFLTRUN        at 0 range 30 .. 30;
      DFLTACTIVE     at 0 range 31 .. 31;
   end record;

   subtype MDF_DFLT4CICR_DATSRC_Field is HAL.UInt2;
   subtype MDF_DFLT4CICR_CICMOD_Field is HAL.UInt3;
   subtype MDF_DFLT4CICR_MCICD_Field is HAL.UInt9;
   subtype MDF_DFLT4CICR_SCALE_Field is HAL.UInt6;

   --  This register is used to control the main CIC filter.
   type MDF_DFLT4CICR_Register is record
      --  Source data for the digital filter Set and cleared by software. 0x:
      --  Select the stream coming from the BSMX - 10: Select the stream coming
      --  from the ADCITF1 - 11: Select the stream coming from the ADCITF2 This
      --  field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      DATSRC         : MDF_DFLT4CICR_DATSRC_Field := 16#0#;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  Select the CIC mode Set and cleared by software. This field allows
      --  the application to select the configuration and the order of the
      --  MCIC. When CICMOD[2:0] is equal to 0xx , the CIC is split into two
      --  filters: - The main CIC (MCIC) - The auxiliary CIC (ACIC), used for
      --  the out-off limit detector - 000: The CIC is split into 2 filters,
      --  and the main CIC (MCIC) is configured in FastSinc filter - 001: The
      --  CIC is split into 2 filters, and the main CIC (MCIC) is configured in
      --  Sinc1 filter - 010: The CIC is split into 2 filters, and the main CIC
      --  (MCIC) is configured in Sinc2 filter - 011: The CIC is split into 2
      --  filters, and the main CIC (MCIC) is configured in Sinc3 filter - 100:
      --  The CIC is configured in single sinc4 filter others: The CIC is
      --  configured in single sinc5 filter This field can be write-protected,
      --  please refer to Section 1.4.15: Register protection for details.
      CICMOD         : MDF_DFLT4CICR_CICMOD_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  CIC decimation ratio selection Set and cleared by software. This bit
      --  is used to allow the application to select the decimation ratio of
      --  the CIC. Decimation ratio smaller than 2 is not allowed. The
      --  decimation ratio is given by (CICDEC+1). - 0: Decimation ratio is 2 -
      --  1: Decimation ratio is 2 - 2: Decimation ratio is 3 - 3: Decimation
      --  ratio is 4 ... - 511: Decimation ratio is 512 This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      MCICD          : MDF_DFLT4CICR_MCICD_Field := 16#0#;
      --  unspecified
      Reserved_17_19 : HAL.UInt3 := 16#0#;
      --  Scaling factor selection Set and cleared by software. This field is
      --  used to allow the application to select the gain to be applied at CIC
      --  output. Please refer to Table 13: Possible gain values for details.
      --  If the application attempts to write a new gain value while the
      --  previous one is not yet applied, this new gain value is ignored.
      --  Reading back the SCALE[5:0] field will inform the application on the
      --  current gain value. - 100000: - 48.2 dB, or shift right by 8 bits
      --  (default value) - 100001: - 44.6 dB, - 100010: - 42.1 dB, or shift
      --  right by 7 bits - 100011: - 38.6 dB, ... - 101110: -6 dB, or shift
      --  right by 1 bit - 101111: -2.5 dB, - 000000: 0 dB - 000001: + 3.5 dB,
      --  - 000010: + 6 dB, or shift left by 1 bit ... - 011000: + 72 dB, or
      --  shift left by 12 bits
      SCALE          : MDF_DFLT4CICR_SCALE_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT4CICR_Register use record
      DATSRC         at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      CICMOD         at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      MCICD          at 0 range 8 .. 16;
      Reserved_17_19 at 0 range 17 .. 19;
      SCALE          at 0 range 20 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype MDF_DFLT4RSFR_HPFC_Field is HAL.UInt2;

   --  This register is used to control the reshape and HPF filters.
   type MDF_DFLT4RSFR_Register is record
      --  Reshaper filter bypass Set and cleared by software. This bit is used
      --  to bypass the reshape filter and its decimation block. - 0: The
      --  reshape filter is not bypassed (Default value) - 1: The reshape
      --  filter is bypassed This field can be write-protected, please refer to
      --  Section 1.4.15: Register protection for details.
      RSFLTBYP       : Boolean := False;
      --  unspecified
      Reserved_1_3   : HAL.UInt3 := 16#0#;
      --  Reshaper filter decimation ratio Set and cleared by software. This
      --  bit is used to select the decimation ratio for the reshape filter -
      --  0: Decimation ratio is 4 (Default value) - 1: Decimation ratio is 1
      --  This field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      RSFLTD         : Boolean := False;
      --  unspecified
      Reserved_5_6   : HAL.UInt2 := 16#0#;
      --  High-Pass Filter bypass Set and cleared by software. This bit is used
      --  to bypass the high-pass filter. - 0: The high pass filter is not
      --  bypassed (Default value) - 1: The high pass filter is bypassed This
      --  field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      HPFBYP         : Boolean := False;
      --  High-pass filter cut-off frequency Set and cleared by software. This
      --  field is used to select the cut-off frequency of the high-pass
      --  filter. FPCM represents the sampling frequency at HPF input. - 00:
      --  Cut-off frequency = 0.000625 x FPCM - 01: Cut-off frequency = 0.00125
      --  x FPCM - 10: Cut-off frequency = 0.00250 x FPCM - 11: Cut-off
      --  frequency = 0.00950 x FPCM This field can be write-protected, please
      --  refer to Section 1.4.15: Register protection for details.
      HPFC           : MDF_DFLT4RSFR_HPFC_Field := 16#0#;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT4RSFR_Register use record
      RSFLTBYP       at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      RSFLTD         at 0 range 4 .. 4;
      Reserved_5_6   at 0 range 5 .. 6;
      HPFBYP         at 0 range 7 .. 7;
      HPFC           at 0 range 8 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype MDF_DFLT4INTR_INTDIV_Field is HAL.UInt2;
   subtype MDF_DFLT4INTR_INTVAL_Field is HAL.UInt7;

   --  This register is used to the integrator (INT) settings.
   type MDF_DFLT4INTR_Register is record
      --  Integrator output division Set and cleared by software. This bit is
      --  used to rescale the signal at the integrator output in order keep the
      --  data width lower than 24 bits. - 00: The integrator data outputs are
      --  divided by 128 (Default value) - 01: The integrator data outputs are
      --  divided by 32 - 10: The integrator data outputs are divided by 4 -
      --  11: The integrator data outputs are not divided This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      INTDIV         : MDF_DFLT4INTR_INTDIV_Field := 16#0#;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  Integration value selection Set and cleared by software. This field
      --  is used to select the integration value. - 0: The integration value
      --  is 1, meaning bypass mode (default after reset) - 1: The integration
      --  value is 2 - 2: The integration value is 3 ... - 127: The integration
      --  value is 128 This field can be write-protected, please refer to
      --  Section 1.4.15: Register protection for details.
      INTVAL         : MDF_DFLT4INTR_INTVAL_Field := 16#0#;
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT4INTR_Register use record
      INTDIV         at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      INTVAL         at 0 range 4 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   subtype MDF_OLD4CR_BKOLD_Field is HAL.UInt4;
   subtype MDF_OLD4CR_ACICN_Field is HAL.UInt2;
   subtype MDF_OLD4CR_ACICD_Field is HAL.UInt5;

   --  This register is used to configure the Out-of Limit Detector function.
   type MDF_OLD4CR_Register is record
      --  Over-Current Detector Enable Set and cleared by software. - 0: The
      --  OLD is disabled (Default value) - 1: The OLD is enabled, including
      --  the ACIC filter working in continuous mode.
      OLDEN          : Boolean := False;
      --  Threshold In band Set and cleared by software. - 0: The OLD generates
      --  an event if the signal is lower than OLDTHL OR higher than OLDTHH
      --  (Default value) - 1: The OLD generates an event if the signal is
      --  lower than OLDTHH AND higher than OLDTHL This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      THINB          : Boolean := False;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  Break signal assignment for out-of limit detector Set and cleared by
      --  software. BKOLD[i] = 0: Break signal (mdf_break[i]) is not assigned
      --  to threshold event BKOLD[i] = 1: Break signal (mdf_break[i]) is
      --  assigned to threshold event This field can be write-protected, please
      --  refer to Section 1.4.15: Register protection for details.
      BKOLD          : MDF_OLD4CR_BKOLD_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : HAL.UInt4 := 16#0#;
      --  OLD CIC order selection Set and cleared by software. This field
      --  allows the application to select the type, and the order of the ACIC.
      --  This field is only taken into account by the MDF when CICMOD[2:0] =
      --  0xx . - 00: FastSinc filter type - 01: Sinc1 filter type - 10: Sinc2
      --  filter type - 11: Sinc3 filter type This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      ACICN          : MDF_OLD4CR_ACICN_Field := 16#0#;
      --  unspecified
      Reserved_14_16 : HAL.UInt3 := 16#0#;
      --  OLD CIC decimation ratio selection Set and cleared by software. This
      --  field is used to allow the application to select the decimation ratio
      --  of the ACIC. This field is only taken into account by the MDF when
      --  CICMOD[2:0] = 0xx . The decimation ratio is given by (ACICD+1). - 0:
      --  Decimation ratio is 1 - 1: Decimation ratio is 2 - 2: Decimation
      --  ratio is 3 - 3: Decimation ratio is 4 ... - 31: Decimation ratio is
      --  32 This field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      ACICD          : MDF_OLD4CR_ACICD_Field := 16#0#;
      --  unspecified
      Reserved_22_30 : HAL.UInt9 := 16#0#;
      --  Read-only. OLD Active flag Set and cleared by hardware. This flag
      --  must be used by the application in order to check if the OLD is
      --  effectively enabled (active) or not. The protected fields and
      --  registers of this function can only be updated when the OLDACTIVE is
      --  set to , please refer to Section 1.4.15: Register protection for
      --  details. The delay between a transition on OLDEN and a transition on
      --  OLDACTIVE is 2 periods of AHB clock and 2 periods of mdf_proc_ck. -
      --  0: The OLD is not active, and can be configured if needed - 1: The
      --  OLD is active, and protected fields cannot be configured.
      OLDACTIVE      : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_OLD4CR_Register use record
      OLDEN          at 0 range 0 .. 0;
      THINB          at 0 range 1 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      BKOLD          at 0 range 4 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      ACICN          at 0 range 12 .. 13;
      Reserved_14_16 at 0 range 14 .. 16;
      ACICD          at 0 range 17 .. 21;
      Reserved_22_30 at 0 range 22 .. 30;
      OLDACTIVE      at 0 range 31 .. 31;
   end record;

   subtype MDF_OLD4THLR_OLDTHL_Field is HAL.UInt26;

   --  This register is used for the adjustment of the Out-off Limit low
   --  threshold.
   type MDF_OLD4THLR_Register is record
      --  OLD Low Threshold Value Set and cleared by software. OLDTHL
      --  represents a 26-bit signed value. The real threshold compared to the
      --  signal provided by the filter is OLDTHL. This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      OLDTHL         : MDF_OLD4THLR_OLDTHL_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_OLD4THLR_Register use record
      OLDTHL         at 0 range 0 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype MDF_OLD4THHR_OLDTHH_Field is HAL.UInt26;

   --  This register is used for the adjustment of the Out-off Limit high
   --  threshold.
   type MDF_OLD4THHR_Register is record
      --  OLD High Threshold Value Set and cleared by software. OLDTHH
      --  represents a 26-bit signed value. The real threshold compared to the
      --  signal provided by the filter is OLDTHH. This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details
      OLDTHH         : MDF_OLD4THHR_OLDTHH_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_OLD4THHR_Register use record
      OLDTHH         at 0 range 0 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype MDF_DLY4CR_SKPDLY_Field is HAL.UInt7;

   --  This register is used for the adjustment stream delays.
   type MDF_DLY4CR_Register is record
      --  Delay to apply to a bitstream Set and cleared by software. Defines
      --  the number of input samples that will be skipped. Skipping is applied
      --  immediately after writing to this field, if SKPBF = 0 , and the
      --  corresponding bit DFLTEN = 1 . If SKPBF = 1 the value written into
      --  the register is ignored by the delay state machine. - 0: No input
      --  sample skipped, - 1: 1 input sample skipped, ... - 127: 127 input
      --  sample skipped,
      SKPDLY        : MDF_DLY4CR_SKPDLY_Field := 16#0#;
      --  unspecified
      Reserved_7_30 : HAL.UInt24 := 16#0#;
      --  Read-only. Skip Busy flag Set and cleared by hardware. Shall be used
      --  in order to control if the delay sequence is completed. - 0: Reading
      --  0 means that the MDF is ready to accept a new value into SKPDLY[6:0].
      --  - 1: Reading 1 means that last valid SKPDLY[6:0] is still under
      --  precessing.
      SKPBF         : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DLY4CR_Register use record
      SKPDLY        at 0 range 0 .. 6;
      Reserved_7_30 at 0 range 7 .. 30;
      SKPBF         at 0 range 31 .. 31;
   end record;

   subtype MDF_SCD4CR_BKSCD_Field is HAL.UInt4;
   subtype MDF_SCD4CR_SCDT_Field is HAL.UInt8;

   --  This register is used for the adjustment stream delays.
   type MDF_SCD4CR_Register is record
      --  Short circuit detector enable Set and cleared by software. - 0: The
      --  short circuit detector is disabled, - 1: The short circuit detector
      --  is enabled,
      SCDEN          : Boolean := False;
      --  unspecified
      Reserved_1_3   : HAL.UInt3 := 16#0#;
      --  Break signal assignment for short circuit detector Set and cleared by
      --  software. BKSCD[i] = 0: Break signal (mdf_break[i]) is not assigned
      --  to this SCD event BKSCD[i] = 1: Break signal (mdf_break[i]) is
      --  assigned to this SCD event This field can be write-protected, please
      --  refer to Section 1.4.15: Register protection for details.
      BKSCD          : MDF_SCD4CR_BKSCD_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : HAL.UInt4 := 16#0#;
      --  Short-circuit detector threshold Set and cleared by software. These
      --  bits are written by software to define the threshold counter for the
      --  short-circuit detector. If this value is reached, then a
      --  short-circuit detector event occurs on a given input stream. - 0: 2
      --  consecutive 1 s or 0 s will generate an event, - 1: 2 consecutive 1 s
      --  or 0 s will generate an event - 2: 3 consecutive 1 s or 0 s will
      --  generate an event, ... - 255: 256 consecutive 1 s or 0 s will
      --  generate an event, This field can be write-protected, please refer to
      --  Section 1.4.15: Register protection for details.
      SCDT           : MDF_SCD4CR_SCDT_Field := 16#0#;
      --  unspecified
      Reserved_20_30 : HAL.UInt11 := 16#0#;
      --  Read-only. SCD Active flag Set and cleared by hardware. This flag
      --  must be used by the application in order to check if the SCD is
      --  effectively enabled (active) or not. The protected fields of this
      --  function can only be updated when the SCDACTIVE is set to a a ,
      --  please refer to Section 1.4.15: Register protection for details. The
      --  delay between a transition on SCDEN and a transition on SCDACTIVE is
      --  2 periods of AHB clock and 2 periods of mdf_proc_ck. - 0: The SCD is
      --  not active, and can be configured if needed - 1: The SCD is active,
      --  and protected fields cannot be configured.
      SCDACTIVE      : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_SCD4CR_Register use record
      SCDEN          at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      BKSCD          at 0 range 4 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      SCDT           at 0 range 12 .. 19;
      Reserved_20_30 at 0 range 20 .. 30;
      SCDACTIVE      at 0 range 31 .. 31;
   end record;

   --  MDF DFLTx interrupt enable register x
   type MDF_DFLT4IER_Register is record
      --  RXFIFO threshold interrupt enable Set and cleared by software. - 0:
      --  RXFIFO threshold interrupt disabled - 1: RXFIFO threshold interrupt
      --  enabled
      FTHIE          : Boolean := False;
      --  Data overflow interrupt enable Set and cleared by software. - 0: Data
      --  overflow interrupt disabled - 1: Data overflow interrupt enabled
      DOVRIE         : Boolean := False;
      --  Snapshot data ready interrupt enable Set and cleared by software. -
      --  0: Snapshot data ready interrupt disabled - 1: Snapshot data ready
      --  interrupt enabled
      SSDRIE         : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Out-of Limit interrupt enable Set and cleared by software. - 0: OLD
      --  event interrupt disabled - 1: OLD event interrupt enabled
      OLDIE          : Boolean := False;
      --  unspecified
      Reserved_5_6   : HAL.UInt2 := 16#0#;
      --  Snapshot overrun interrupt enable Set and cleared by software. - 0:
      --  Snapshot overrun interrupt disabled - 1: Snapshot overrun interrupt
      --  enabled
      SSOVRIE        : Boolean := False;
      --  Short-Circuit Detector interrupt enable Set and cleared by software.
      --  - 0: SCD interrupt disabled - 1: SCD interrupt enabled
      SCDIE          : Boolean := False;
      --  Saturation detection interrupt enable Set and cleared by software. -
      --  0: Saturation interrupt disabled - 1: Saturation interrupt enabled
      SATIE          : Boolean := False;
      --  Clock absence detection interrupt enable Set and cleared by software.
      --  - 0: Clock absence interrupt disabled - 1: Clock absence interrupt
      --  enabled
      CKABIE         : Boolean := False;
      --  Reshape Filter Overrun interrupt enable Set and cleared by software.
      --  - 0: Reshape filter overrun interrupt disabled - 1: Reshape filter
      --  overrun interrupt enabled
      RFOVRIE        : Boolean := False;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT4IER_Register use record
      FTHIE          at 0 range 0 .. 0;
      DOVRIE         at 0 range 1 .. 1;
      SSDRIE         at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      OLDIE          at 0 range 4 .. 4;
      Reserved_5_6   at 0 range 5 .. 6;
      SSOVRIE        at 0 range 7 .. 7;
      SCDIE          at 0 range 8 .. 8;
      SATIE          at 0 range 9 .. 9;
      CKABIE         at 0 range 10 .. 10;
      RFOVRIE        at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   --  This register contains the status flags for each digital filter path.
   type MDF_DFLT4ISR_Register is record
      --  Read-only. RXFIFO threshold flag Set by hardware, and cleared by
      --  software by writing this bit to 1 . - 0: Reading 0 means that the
      --  RXFIFO threshold is not reached, writing 0 has no effect. - 1:
      --  Reading 1 means that the RXFIFO reached the threshold, writing 1
      --  clears this flag.
      FTHF           : Boolean := False;
      --  Data overflow flag Set by hardware, and cleared by software by
      --  writing this bit to 1 . - 0: Reading 0 means that no overflow is
      --  detected, writing 0 has no effect. - 1: Reading 1 means that an
      --  overflow is detected, writing 1 clears this flag.
      DOVRF          : Boolean := False;
      --  Snapshot data ready flag Set by hardware, and cleared by software by
      --  writing this bit to 1 . - 0: Reading 0 means that no data is
      --  available on MDF_SNPSxDR, writing 0 has no effect. - 1: Reading 1
      --  means that a new data is available on MDF_SNPSxDR, writing 1 clears
      --  this flag.
      SSDRF          : Boolean := False;
      --  Read-only. RXFIFO Not Empty flag Set and cleared by hardware
      --  according to the RXFIFO level. - 0: Reading 0 means that the RXFIFO
      --  is empty. - 1: Reading 1 means that the RXFIFO is not empty.
      RXNEF          : Boolean := False;
      --  Out-of Limit Detector flag Set by hardware, and cleared by software
      --  by writing this bit to 1 . - 0: Reading 0 means that no OLD event is
      --  detected, writing 0 has no effect. - 1: Reading 1 means that an OLD
      --  event is detected, writing 1 clears THHF, THLF and OLDF flags.
      OLDF           : Boolean := False;
      --  Read-only. Low threshold status flag Set by hardware, and cleared by
      --  software by writing OLDF bit to 1 . This flag indicates the status of
      --  the low threshold comparator when the last OLD event occurred. This
      --  bit gives additional information on the conditions triggering the
      --  last OLD event. It can be cleared by writing OLDF flag to a 1. - 0:
      --  The signal was lower than OLDTHL, when the last OLD event occurred -
      --  1: The signal was higher than OLDTHL, when the last OLD event
      --  occurred
      THLF           : Boolean := False;
      --  Read-only. High threshold status flag Set by hardware, and cleared by
      --  software by writing OLDF bit to 1 . This flag indicates the status of
      --  the high threshold comparator when the last OLD event occurred. This
      --  bit gives additional information on the conditions triggering the
      --  last OLD event. It can be cleared by writing OLDF flag to a 1. - 0:
      --  The signal was lower than OLDTHH, when the last OLD event occurred -
      --  1: The signal was higher than OLDTHH, when the last OLD event
      --  occurred
      THHF           : Boolean := False;
      --  Snapshot overrun flag Set by hardware, and cleared by software by
      --  writing this bit to 1 . - 0: Reading 0 means that no snapshot overrun
      --  event is detected, writing 0 has no effect. - 1: Reading 1 means that
      --  a snapshot overrun event is detected, writing 1 clears this flag.
      SSOVRF         : Boolean := False;
      --  Short-Circuit Detector flag Set by hardware, and cleared by software
      --  by writing this bit to 1 . - 0: Reading 0 means that no SCD event is
      --  detected, writing 0 has no effect. - 1: Reading 1 means that a SCD
      --  event is detected, writing 1 clears this flag.
      SCDF           : Boolean := False;
      --  Saturation detection flag Set by hardware, and cleared by software by
      --  writing this bit to 1 . - 0: Reading 0 means that no saturation is
      --  detected, writing 0 has no effect. - 1: Reading 1 means that a
      --  saturation is detected, writing 1 clears this flag.
      SATF           : Boolean := False;
      --  Clock absence detection flag Set by hardware, and cleared by software
      --  by writing this bit to 1 . - 0: Reading 0 means that no clock absence
      --  is detected, writing 0 has no effect. - 1: Reading 1 means that a
      --  clock absence is detected, writing 1 clears this flag.
      CKABF          : Boolean := False;
      --  Reshape Filter Overrun detection flag Set by hardware, and cleared by
      --  software by writing this bit to 1 . - 0: Reading 0 means that no
      --  reshape filter overrun is detected, writing 0 has no effect. - 1:
      --  Reading 1 means that reshape filter overrun is detected, writing 1
      --  clears this flag.
      RFOVRF         : Boolean := False;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT4ISR_Register use record
      FTHF           at 0 range 0 .. 0;
      DOVRF          at 0 range 1 .. 1;
      SSDRF          at 0 range 2 .. 2;
      RXNEF          at 0 range 3 .. 3;
      OLDF           at 0 range 4 .. 4;
      THLF           at 0 range 5 .. 5;
      THHF           at 0 range 6 .. 6;
      SSOVRF         at 0 range 7 .. 7;
      SCDF           at 0 range 8 .. 8;
      SATF           at 0 range 9 .. 9;
      CKABF          at 0 range 10 .. 10;
      RFOVRF         at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype MDF_OEC4CR_OFFSET_Field is HAL.UInt26;

   --  This register contains the offset compensation value.
   type MDF_OEC4CR_Register is record
      --  Offset error compensation Set and cleared by software. If the
      --  application attempts to write a new offset value while the previous
      --  one is not yet applied, this new offset value is ignored. Reading
      --  back the OFFSET[25:0] field will inform the application on the
      --  current offset value. OFFSET[25:0] represents the value to be
      --  subtracted to the signal before going to the SCALE.
      OFFSET         : MDF_OEC4CR_OFFSET_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_OEC4CR_Register use record
      OFFSET         at 0 range 0 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype MDF_SNPS4DR_MCICDC_Field is HAL.UInt9;
   subtype MDF_SNPS4DR_EXTSDR_Field is HAL.UInt7;
   subtype MDF_SNPS4DR_SDR_Field is HAL.UInt16;

   --  This register is used to read the data processed by each digital filter
   --  in snapshot mode.
   type MDF_SNPS4DR_Register is record
      --  Read-only. Contains the MCIC decimation counter value at the moment
      --  of the last trigger event occurs (MCIC_CNT)
      MCICDC : MDF_SNPS4DR_MCICDC_Field;
      --  Read-only. Extended data size If SNPSFMT = 0 , EXTSDR[6:0] contains
      --  the bit 7 to 1 of the last valid data processed by the digital
      --  filter, If SNPSFMT = 1 , this field contains the INT accumulator
      --  counter value at the moment of the last trigger event occurs
      --  (INT_CNT).
      EXTSDR : MDF_SNPS4DR_EXTSDR_Field;
      --  Read-only. Contains the 16 MSB of the last valid data processed by
      --  the digital filter.
      SDR    : MDF_SNPS4DR_SDR_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_SNPS4DR_Register use record
      MCICDC at 0 range 0 .. 8;
      EXTSDR at 0 range 9 .. 15;
      SDR    at 0 range 16 .. 31;
   end record;

   subtype MDF_DFLT4DR_DR_Field is HAL.UInt24;

   --  This register is used to read the data processed by each digital filter.
   type MDF_DFLT4DR_Register is record
      --  unspecified
      Reserved_0_7 : HAL.UInt8;
      --  Read-only. Data processed by digital filter.
      DR           : MDF_DFLT4DR_DR_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT4DR_Register use record
      Reserved_0_7 at 0 range 0 .. 7;
      DR           at 0 range 8 .. 31;
   end record;

   subtype MDF_SITF5CR_SCKSRC_Field is HAL.UInt2;
   subtype MDF_SITF5CR_SITFMOD_Field is HAL.UInt2;
   subtype MDF_SITF5CR_STH_Field is HAL.UInt5;

   --  This register is used to control the serial interfaces (SITFx).
   type MDF_SITF5CR_Register is record
      --  Serial interface enable Set and cleared by software. This bit is used
      --  to enable/disable the serial interface. - 0: Serial interface
      --  disabled - 1: Serial interface enabled
      SITFEN         : Boolean := False;
      --  Serial clock source Set and cleared by software. This bit is used to
      --  select the clock source of the serial interface. - 00: Serial clock
      --  source is MDF_CCK0 - 01: Serial clock source is MDF_CCK1 1x: Serial
      --  clock source is MDF_CKIx, not allowed in LF_MASTER SPI mode This
      --  field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      SCKSRC         : MDF_SITF5CR_SCKSRC_Field := 16#0#;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Serial interface type Set and cleared by software. This field is used
      --  to defined the serial interface type. - 00: LF_MASTER (Low-Frequency
      --  MASTER) SPI mode - 01: Normal SPI mode - 10: Manchester mode: rising
      --  edge = logic 0, falling edge = logic 1 - 11: Manchester mode: rising
      --  edge = logic 1, falling edge = logic 0 This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      SITFMOD        : MDF_SITF5CR_SITFMOD_Field := 16#0#;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Manchester Symbol threshold / SPI threshold Set and cleared by
      --  software. This field is used for Manchester mode, in order to define
      --  the expected symbol threshold levels. Please refer to Section :
      --  Manchester mode for details on computation. In addition this field is
      --  used to define the timeout value for the clock absence detection in
      --  Normal SPI mode. Values of STH[4:0] lower than 4 are invalid. This
      --  field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      STH            : MDF_SITF5CR_STH_Field := 16#1F#;
      --  unspecified
      Reserved_13_30 : HAL.UInt18 := 16#0#;
      --  Read-only. Serial interface Active flag Set and cleared by hardware.
      --  This flag must be used by the application in order to check if the
      --  serial interface is effectively enabled (active) or not. The
      --  protected fields of this function can only be updated when the
      --  SITFACTIVE is set , please refer to Section 1.4.15: Register
      --  protection for details. The delay between a transition on SITFEN and
      --  a transition on SITFACTIVE is 2 periods of AHB clock and 2 periods of
      --  mdf_proc_ck. - 0: The serial interface is not active, and can be
      --  configured if needed - 1: The serial interface is active, and
      --  protected fields cannot be configured.
      SITFACTIVE     : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_SITF5CR_Register use record
      SITFEN         at 0 range 0 .. 0;
      SCKSRC         at 0 range 1 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      SITFMOD        at 0 range 4 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      STH            at 0 range 8 .. 12;
      Reserved_13_30 at 0 range 13 .. 30;
      SITFACTIVE     at 0 range 31 .. 31;
   end record;

   subtype MDF_BSMX5CR_BSSEL_Field is HAL.UInt5;

   --  This register is used to select the bitstream to be provided to the
   --  corresponding digital filter and to the SCD.
   type MDF_BSMX5CR_Register is record
      --  Bitstream Selection Set and cleared by software. This field is used
      --  to select the bitstream to be processed for the digital filter x and
      --  for the SCDx. The size of this field depends on the number of DFLTx
      --  instantiated. If the BSSEL is selecting an input which is not
      --  instantiated, the MDF will select the valid stream bs[x]_F having the
      --  higher index number. - 00000: The bitstream bs[0]_R is provided to
      --  DFLTx and SCDx - 00001: The bitstream bs[0]_F is provided to DFLTx
      --  and SCDx - 00010: The bitstream bs[1]_R is provided to DFLTx and SCDx
      --  (if instantiated) - 00011: The bitstream bs[1]_F is provided to DFLTx
      --  and SCDx (if instantiated) ... - 11110: The bitstream bs[15]_R is
      --  provided to DFLTx and SCDx (if instantiated) - 11111: The bitstream
      --  bs[15]_F is provided to DFLTx and SCDx (if instantiated) This field
      --  can be write-protected, please refer to Section 1.4.15: Register
      --  protection for details.
      BSSEL         : MDF_BSMX5CR_BSSEL_Field := 16#0#;
      --  unspecified
      Reserved_5_30 : HAL.UInt26 := 16#0#;
      --  Read-only. BSMX Active flag Set and cleared by hardware. This flag
      --  must be used by the application in order to check if the BSMX is
      --  effectively enabled (active) or not. BSSEL[4:0] can only be updated
      --  when the BSMXACTIVE is set to . The BSMXACTIVE flag is a logical
      --  between OLDACTIVE, DFLTACTIVE, and SCDACTIVE flags. Both of them must
      --  be set to in order update BSSEL[4:0] field. - 0: The BSMX is not
      --  active, and can be configured if needed - 1: The BSMX is active, and
      --  protected fields cannot be configured.
      BSMXACTIVE    : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_BSMX5CR_Register use record
      BSSEL         at 0 range 0 .. 4;
      Reserved_5_30 at 0 range 5 .. 30;
      BSMXACTIVE    at 0 range 31 .. 31;
   end record;

   subtype MDF_DFLT5CR_ACQMOD_Field is HAL.UInt3;
   subtype MDF_DFLT5CR_TRGSRC_Field is HAL.UInt4;
   subtype MDF_DFLT5CR_NBDIS_Field is HAL.UInt8;

   --  This register is used to control the digital filter x.
   type MDF_DFLT5CR_Register is record
      --  Write-only. Digital Filter Enable Set and cleared by software. This
      --  bit is used to control the start of acquisition of the corresponding
      --  digital filter path. The behavior of this bit depends on ACQMOD and
      --  external events. or the acquisition starts when the proper trigger
      --  event occurs if ACQMOD = 01x . The serial or parallel interface
      --  delivering the samples shall be enabled as well. - 0: The acquisition
      --  is stopped immediately - 1: The acquisition is immediately started if
      --  ACQMOD = 00x or 1xx ,
      DFLTEN         : Boolean := False;
      --  DMA Requests Enable Set and cleared by software. This bit is used to
      --  control the generation of DMA request in order to transfer the
      --  processed samples into the memory. - 0: The DMA interface for the
      --  corresponding digital filter is disabled - 1: The DMA interface for
      --  the corresponding digital filter is enabled This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      DMAEN          : Boolean := False;
      --  RXFIFO Threshold selection Set and cleared by software. This bit is
      --  used to select the RXFIFO threshold. This bit is not significant for
      --  RXFIFOs working in interleaved transfer mode. Refer to Section
      --  1.4.13.4: Using the interleaved transfer mode for details. - 0:
      --  RXFIFO threshold event generated when the RXFIFO is not empty - 1:
      --  RXFIFO threshold event generated when the RXFIFO is half-full This
      --  field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      FTH            : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Digital filter Trigger mode Set and cleared by software. This field
      --  is used to select the filter trigger mode. - 000: Asynchronous,
      --  continuous acquisition mode - 001: Asynchronous, single-shot
      --  acquisition mode - 010: Synchronous, continuous acquisition mode -
      --  011: Synchronous, single-shot acquisition mode - 100: Window,
      --  continuous acquisition mode - 101: Synchronous, snapshot mode others:
      --  same a 000 This field can be write-protected, please refer to Section
      --  1.4.15: Register protection for details.
      ACQMOD         : MDF_DFLT5CR_ACQMOD_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Digital filter Trigger sensitivity selection Set and cleared by
      --  software. This field is used to select the trigger sensitivity of the
      --  external signals - 0: A rising edge event triggers the acquisition -
      --  1: A falling edge even triggers the acquisition Note that when the
      --  trigger source is TRGO or OLDx event, TRGSENS value is not taken into
      --  account. When TRGO is selected, the sensitivity is forced to falling
      --  edge, when OLDx event is selected, the sensitivity is forced to
      --  rising edge. This field can be write-protected, please refer to
      --  Section 1.4.15: Register protection for details.
      TRGSENS        : Boolean := False;
      --  unspecified
      Reserved_9_11  : HAL.UInt3 := 16#0#;
      --  Digital filter Trigger signal selection, Set and cleared by software.
      --  This field is used to select which external signals is used as
      --  trigger for the corresponding filter. - 0000: TRGO is selected -
      --  0001: OLDx event is selected - 0010: mdf_trg[0] is selected ... -
      --  1111: mdf_trg[13] is selected This field can be write-protected,
      --  please refer to Section 1.4.15: Register protection for details.
      TRGSRC         : MDF_DFLT5CR_TRGSRC_Field := 16#0#;
      --  Snapshot data format Set and cleared by software. This field is used
      --  to select the data format for the snapshot mode. - 0: The integrator
      --  counter (INT_CNT) is not inserted into the MDF_SNPSxDR register,
      --  leaving a data resolution of 23 bits. - 1: The integrator counter
      --  (INT_CNT) is inserted at position [15:9] of MDF_SNPSxDR register,
      --  leaving a data resolution of 16 bits. This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      SNPSFMT        : Boolean := False;
      --  unspecified
      Reserved_17_19 : HAL.UInt3 := 16#0#;
      --  Number of samples to be discarded Set and cleared by software. This
      --  field is used to define the number of samples to be discarded every
      --  time the DFLTx is re-started. - 0: no sample discarded - 1: 1 sample
      --  discarded - 2: 2 samples discarded ... - 255: 255 samples discarded
      --  This field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      NBDIS          : MDF_DFLT5CR_NBDIS_Field := 16#0#;
      --  unspecified
      Reserved_28_29 : HAL.UInt2 := 16#0#;
      --  Read-only. Digital filter Run Status Flag Set and cleared by
      --  hardware. This bit indicates if the digital filter is running or not.
      --  - 0: The digital filter is not running, and ready to accept a new
      --  trigger event - 1: The digital filter is running
      DFLTRUN        : Boolean := False;
      --  Read-only. Digital filter Active Flag Set and cleared by hardware.
      --  This bit indicates if the digital filter is active: can be running or
      --  waiting for events. - 0: The digital filter is not active, and can be
      --  re-enabled again (via DFLTEN bit) if needed - 1: The digital filter
      --  is active
      DFLTACTIVE     : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT5CR_Register use record
      DFLTEN         at 0 range 0 .. 0;
      DMAEN          at 0 range 1 .. 1;
      FTH            at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      ACQMOD         at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      TRGSENS        at 0 range 8 .. 8;
      Reserved_9_11  at 0 range 9 .. 11;
      TRGSRC         at 0 range 12 .. 15;
      SNPSFMT        at 0 range 16 .. 16;
      Reserved_17_19 at 0 range 17 .. 19;
      NBDIS          at 0 range 20 .. 27;
      Reserved_28_29 at 0 range 28 .. 29;
      DFLTRUN        at 0 range 30 .. 30;
      DFLTACTIVE     at 0 range 31 .. 31;
   end record;

   subtype MDF_DFLT5CICR_DATSRC_Field is HAL.UInt2;
   subtype MDF_DFLT5CICR_CICMOD_Field is HAL.UInt3;
   subtype MDF_DFLT5CICR_MCICD_Field is HAL.UInt9;
   subtype MDF_DFLT5CICR_SCALE_Field is HAL.UInt6;

   --  This register is used to control the main CIC filter.
   type MDF_DFLT5CICR_Register is record
      --  Source data for the digital filter Set and cleared by software. 0x:
      --  Select the stream coming from the BSMX - 10: Select the stream coming
      --  from the ADCITF1 - 11: Select the stream coming from the ADCITF2 This
      --  field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      DATSRC         : MDF_DFLT5CICR_DATSRC_Field := 16#0#;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  Select the CIC mode Set and cleared by software. This field allows
      --  the application to select the configuration and the order of the
      --  MCIC. When CICMOD[2:0] is equal to 0xx , the CIC is split into two
      --  filters: - The main CIC (MCIC) - The auxiliary CIC (ACIC), used for
      --  the out-off limit detector - 000: The CIC is split into 2 filters,
      --  and the main CIC (MCIC) is configured in FastSinc filter - 001: The
      --  CIC is split into 2 filters, and the main CIC (MCIC) is configured in
      --  Sinc1 filter - 010: The CIC is split into 2 filters, and the main CIC
      --  (MCIC) is configured in Sinc2 filter - 011: The CIC is split into 2
      --  filters, and the main CIC (MCIC) is configured in Sinc3 filter - 100:
      --  The CIC is configured in single sinc4 filter others: The CIC is
      --  configured in single sinc5 filter This field can be write-protected,
      --  please refer to Section 1.4.15: Register protection for details.
      CICMOD         : MDF_DFLT5CICR_CICMOD_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  CIC decimation ratio selection Set and cleared by software. This bit
      --  is used to allow the application to select the decimation ratio of
      --  the CIC. Decimation ratio smaller than 2 is not allowed. The
      --  decimation ratio is given by (CICDEC+1). - 0: Decimation ratio is 2 -
      --  1: Decimation ratio is 2 - 2: Decimation ratio is 3 - 3: Decimation
      --  ratio is 4 ... - 511: Decimation ratio is 512 This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      MCICD          : MDF_DFLT5CICR_MCICD_Field := 16#0#;
      --  unspecified
      Reserved_17_19 : HAL.UInt3 := 16#0#;
      --  Scaling factor selection Set and cleared by software. This field is
      --  used to allow the application to select the gain to be applied at CIC
      --  output. Please refer to Table 13: Possible gain values for details.
      --  If the application attempts to write a new gain value while the
      --  previous one is not yet applied, this new gain value is ignored.
      --  Reading back the SCALE[5:0] field will inform the application on the
      --  current gain value. - 100000: - 48.2 dB, or shift right by 8 bits
      --  (default value) - 100001: - 44.6 dB, - 100010: - 42.1 dB, or shift
      --  right by 7 bits - 100011: - 38.6 dB, ... - 101110: -6 dB, or shift
      --  right by 1 bit - 101111: -2.5 dB, - 000000: 0 dB - 000001: + 3.5 dB,
      --  - 000010: + 6 dB, or shift left by 1 bit ... - 011000: + 72 dB, or
      --  shift left by 12 bits
      SCALE          : MDF_DFLT5CICR_SCALE_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT5CICR_Register use record
      DATSRC         at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      CICMOD         at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      MCICD          at 0 range 8 .. 16;
      Reserved_17_19 at 0 range 17 .. 19;
      SCALE          at 0 range 20 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype MDF_DFLT5RSFR_HPFC_Field is HAL.UInt2;

   --  This register is used to control the reshape and HPF filters.
   type MDF_DFLT5RSFR_Register is record
      --  Reshaper filter bypass Set and cleared by software. This bit is used
      --  to bypass the reshape filter and its decimation block. - 0: The
      --  reshape filter is not bypassed (Default value) - 1: The reshape
      --  filter is bypassed This field can be write-protected, please refer to
      --  Section 1.4.15: Register protection for details.
      RSFLTBYP       : Boolean := False;
      --  unspecified
      Reserved_1_3   : HAL.UInt3 := 16#0#;
      --  Reshaper filter decimation ratio Set and cleared by software. This
      --  bit is used to select the decimation ratio for the reshape filter -
      --  0: Decimation ratio is 4 (Default value) - 1: Decimation ratio is 1
      --  This field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      RSFLTD         : Boolean := False;
      --  unspecified
      Reserved_5_6   : HAL.UInt2 := 16#0#;
      --  High-Pass Filter bypass Set and cleared by software. This bit is used
      --  to bypass the high-pass filter. - 0: The high pass filter is not
      --  bypassed (Default value) - 1: The high pass filter is bypassed This
      --  field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      HPFBYP         : Boolean := False;
      --  High-pass filter cut-off frequency Set and cleared by software. This
      --  field is used to select the cut-off frequency of the high-pass
      --  filter. FPCM represents the sampling frequency at HPF input. - 00:
      --  Cut-off frequency = 0.000625 x FPCM - 01: Cut-off frequency = 0.00125
      --  x FPCM - 10: Cut-off frequency = 0.00250 x FPCM - 11: Cut-off
      --  frequency = 0.00950 x FPCM This field can be write-protected, please
      --  refer to Section 1.4.15: Register protection for details.
      HPFC           : MDF_DFLT5RSFR_HPFC_Field := 16#0#;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT5RSFR_Register use record
      RSFLTBYP       at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      RSFLTD         at 0 range 4 .. 4;
      Reserved_5_6   at 0 range 5 .. 6;
      HPFBYP         at 0 range 7 .. 7;
      HPFC           at 0 range 8 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype MDF_DFLT5INTR_INTDIV_Field is HAL.UInt2;
   subtype MDF_DFLT5INTR_INTVAL_Field is HAL.UInt7;

   --  This register is used to the integrator (INT) settings.
   type MDF_DFLT5INTR_Register is record
      --  Integrator output division Set and cleared by software. This bit is
      --  used to rescale the signal at the integrator output in order keep the
      --  data width lower than 24 bits. - 00: The integrator data outputs are
      --  divided by 128 (Default value) - 01: The integrator data outputs are
      --  divided by 32 - 10: The integrator data outputs are divided by 4 -
      --  11: The integrator data outputs are not divided This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      INTDIV         : MDF_DFLT5INTR_INTDIV_Field := 16#0#;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  Integration value selection Set and cleared by software. This field
      --  is used to select the integration value. - 0: The integration value
      --  is 1, meaning bypass mode (default after reset) - 1: The integration
      --  value is 2 - 2: The integration value is 3 ... - 127: The integration
      --  value is 128 This field can be write-protected, please refer to
      --  Section 1.4.15: Register protection for details.
      INTVAL         : MDF_DFLT5INTR_INTVAL_Field := 16#0#;
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT5INTR_Register use record
      INTDIV         at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      INTVAL         at 0 range 4 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   subtype MDF_OLD5CR_BKOLD_Field is HAL.UInt4;
   subtype MDF_OLD5CR_ACICN_Field is HAL.UInt2;
   subtype MDF_OLD5CR_ACICD_Field is HAL.UInt5;

   --  This register is used to configure the Out-of Limit Detector function.
   type MDF_OLD5CR_Register is record
      --  Over-Current Detector Enable Set and cleared by software. - 0: The
      --  OLD is disabled (Default value) - 1: The OLD is enabled, including
      --  the ACIC filter working in continuous mode.
      OLDEN          : Boolean := False;
      --  Threshold In band Set and cleared by software. - 0: The OLD generates
      --  an event if the signal is lower than OLDTHL OR higher than OLDTHH
      --  (Default value) - 1: The OLD generates an event if the signal is
      --  lower than OLDTHH AND higher than OLDTHL This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      THINB          : Boolean := False;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  Break signal assignment for out-of limit detector Set and cleared by
      --  software. BKOLD[i] = 0: Break signal (mdf_break[i]) is not assigned
      --  to threshold event BKOLD[i] = 1: Break signal (mdf_break[i]) is
      --  assigned to threshold event This field can be write-protected, please
      --  refer to Section 1.4.15: Register protection for details.
      BKOLD          : MDF_OLD5CR_BKOLD_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : HAL.UInt4 := 16#0#;
      --  OLD CIC order selection Set and cleared by software. This field
      --  allows the application to select the type, and the order of the ACIC.
      --  This field is only taken into account by the MDF when CICMOD[2:0] =
      --  0xx . - 00: FastSinc filter type - 01: Sinc1 filter type - 10: Sinc2
      --  filter type - 11: Sinc3 filter type This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      ACICN          : MDF_OLD5CR_ACICN_Field := 16#0#;
      --  unspecified
      Reserved_14_16 : HAL.UInt3 := 16#0#;
      --  OLD CIC decimation ratio selection Set and cleared by software. This
      --  field is used to allow the application to select the decimation ratio
      --  of the ACIC. This field is only taken into account by the MDF when
      --  CICMOD[2:0] = 0xx . The decimation ratio is given by (ACICD+1). - 0:
      --  Decimation ratio is 1 - 1: Decimation ratio is 2 - 2: Decimation
      --  ratio is 3 - 3: Decimation ratio is 4 ... - 31: Decimation ratio is
      --  32 This field can be write-protected, please refer to Section 1.4.15:
      --  Register protection for details.
      ACICD          : MDF_OLD5CR_ACICD_Field := 16#0#;
      --  unspecified
      Reserved_22_30 : HAL.UInt9 := 16#0#;
      --  Read-only. OLD Active flag Set and cleared by hardware. This flag
      --  must be used by the application in order to check if the OLD is
      --  effectively enabled (active) or not. The protected fields and
      --  registers of this function can only be updated when the OLDACTIVE is
      --  set to , please refer to Section 1.4.15: Register protection for
      --  details. The delay between a transition on OLDEN and a transition on
      --  OLDACTIVE is 2 periods of AHB clock and 2 periods of mdf_proc_ck. -
      --  0: The OLD is not active, and can be configured if needed - 1: The
      --  OLD is active, and protected fields cannot be configured.
      OLDACTIVE      : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_OLD5CR_Register use record
      OLDEN          at 0 range 0 .. 0;
      THINB          at 0 range 1 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      BKOLD          at 0 range 4 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      ACICN          at 0 range 12 .. 13;
      Reserved_14_16 at 0 range 14 .. 16;
      ACICD          at 0 range 17 .. 21;
      Reserved_22_30 at 0 range 22 .. 30;
      OLDACTIVE      at 0 range 31 .. 31;
   end record;

   subtype MDF_OLD5THLR_OLDTHL_Field is HAL.UInt26;

   --  This register is used for the adjustment of the Out-off Limit low
   --  threshold.
   type MDF_OLD5THLR_Register is record
      --  OLD Low Threshold Value Set and cleared by software. OLDTHL
      --  represents a 26-bit signed value. The real threshold compared to the
      --  signal provided by the filter is OLDTHL. This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details.
      OLDTHL         : MDF_OLD5THLR_OLDTHL_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_OLD5THLR_Register use record
      OLDTHL         at 0 range 0 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype MDF_OLD5THHR_OLDTHH_Field is HAL.UInt26;

   --  This register is used for the adjustment of the Out-off Limit high
   --  threshold.
   type MDF_OLD5THHR_Register is record
      --  OLD High Threshold Value Set and cleared by software. OLDTHH
      --  represents a 26-bit signed value. The real threshold compared to the
      --  signal provided by the filter is OLDTHH. This field can be
      --  write-protected, please refer to Section 1.4.15: Register protection
      --  for details
      OLDTHH         : MDF_OLD5THHR_OLDTHH_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_OLD5THHR_Register use record
      OLDTHH         at 0 range 0 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype MDF_DLY5CR_SKPDLY_Field is HAL.UInt7;

   --  This register is used for the adjustment stream delays.
   type MDF_DLY5CR_Register is record
      --  Delay to apply to a bitstream Set and cleared by software. Defines
      --  the number of input samples that will be skipped. Skipping is applied
      --  immediately after writing to this field, if SKPBF = 0 , and the
      --  corresponding bit DFLTEN = 1 . If SKPBF = 1 the value written into
      --  the register is ignored by the delay state machine. - 0: No input
      --  sample skipped, - 1: 1 input sample skipped, ... - 127: 127 input
      --  sample skipped,
      SKPDLY        : MDF_DLY5CR_SKPDLY_Field := 16#0#;
      --  unspecified
      Reserved_7_30 : HAL.UInt24 := 16#0#;
      --  Read-only. Skip Busy flag Set and cleared by hardware. Shall be used
      --  in order to control if the delay sequence is completed. - 0: Reading
      --  0 means that the MDF is ready to accept a new value into SKPDLY[6:0].
      --  - 1: Reading 1 means that last valid SKPDLY[6:0] is still under
      --  precessing.
      SKPBF         : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DLY5CR_Register use record
      SKPDLY        at 0 range 0 .. 6;
      Reserved_7_30 at 0 range 7 .. 30;
      SKPBF         at 0 range 31 .. 31;
   end record;

   subtype MDF_SCD5CR_BKSCD_Field is HAL.UInt4;
   subtype MDF_SCD5CR_SCDT_Field is HAL.UInt8;

   --  This register is used for the adjustment stream delays.
   type MDF_SCD5CR_Register is record
      --  Short circuit detector enable Set and cleared by software. - 0: The
      --  short circuit detector is disabled, - 1: The short circuit detector
      --  is enabled,
      SCDEN          : Boolean := False;
      --  unspecified
      Reserved_1_3   : HAL.UInt3 := 16#0#;
      --  Break signal assignment for short circuit detector Set and cleared by
      --  software. BKSCD[i] = 0: Break signal (mdf_break[i]) is not assigned
      --  to this SCD event BKSCD[i] = 1: Break signal (mdf_break[i]) is
      --  assigned to this SCD event This field can be write-protected, please
      --  refer to Section 1.4.15: Register protection for details.
      BKSCD          : MDF_SCD5CR_BKSCD_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : HAL.UInt4 := 16#0#;
      --  Short-circuit detector threshold Set and cleared by software. These
      --  bits are written by software to define the threshold counter for the
      --  short-circuit detector. If this value is reached, then a
      --  short-circuit detector event occurs on a given input stream. - 0: 2
      --  consecutive 1 s or 0 s will generate an event, - 1: 2 consecutive 1 s
      --  or 0 s will generate an event - 2: 3 consecutive 1 s or 0 s will
      --  generate an event, ... - 255: 256 consecutive 1 s or 0 s will
      --  generate an event, This field can be write-protected, please refer to
      --  Section 1.4.15: Register protection for details.
      SCDT           : MDF_SCD5CR_SCDT_Field := 16#0#;
      --  unspecified
      Reserved_20_30 : HAL.UInt11 := 16#0#;
      --  Read-only. SCD Active flag Set and cleared by hardware. This flag
      --  must be used by the application in order to check if the SCD is
      --  effectively enabled (active) or not. The protected fields of this
      --  function can only be updated when the SCDACTIVE is set to a a ,
      --  please refer to Section 1.4.15: Register protection for details. The
      --  delay between a transition on SCDEN and a transition on SCDACTIVE is
      --  2 periods of AHB clock and 2 periods of mdf_proc_ck. - 0: The SCD is
      --  not active, and can be configured if needed - 1: The SCD is active,
      --  and protected fields cannot be configured.
      SCDACTIVE      : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_SCD5CR_Register use record
      SCDEN          at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      BKSCD          at 0 range 4 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      SCDT           at 0 range 12 .. 19;
      Reserved_20_30 at 0 range 20 .. 30;
      SCDACTIVE      at 0 range 31 .. 31;
   end record;

   --  MDF DFLTx interrupt enable register x
   type MDF_DFLT5IER_Register is record
      --  RXFIFO threshold interrupt enable Set and cleared by software. - 0:
      --  RXFIFO threshold interrupt disabled - 1: RXFIFO threshold interrupt
      --  enabled
      FTHIE          : Boolean := False;
      --  Data overflow interrupt enable Set and cleared by software. - 0: Data
      --  overflow interrupt disabled - 1: Data overflow interrupt enabled
      DOVRIE         : Boolean := False;
      --  Snapshot data ready interrupt enable Set and cleared by software. -
      --  0: Snapshot data ready interrupt disabled - 1: Snapshot data ready
      --  interrupt enabled
      SSDRIE         : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Out-of Limit interrupt enable Set and cleared by software. - 0: OLD
      --  event interrupt disabled - 1: OLD event interrupt enabled
      OLDIE          : Boolean := False;
      --  unspecified
      Reserved_5_6   : HAL.UInt2 := 16#0#;
      --  Snapshot overrun interrupt enable Set and cleared by software. - 0:
      --  Snapshot overrun interrupt disabled - 1: Snapshot overrun interrupt
      --  enabled
      SSOVRIE        : Boolean := False;
      --  Short-Circuit Detector interrupt enable Set and cleared by software.
      --  - 0: SCD interrupt disabled - 1: SCD interrupt enabled
      SCDIE          : Boolean := False;
      --  Saturation detection interrupt enable Set and cleared by software. -
      --  0: Saturation interrupt disabled - 1: Saturation interrupt enabled
      SATIE          : Boolean := False;
      --  Clock absence detection interrupt enable Set and cleared by software.
      --  - 0: Clock absence interrupt disabled - 1: Clock absence interrupt
      --  enabled
      CKABIE         : Boolean := False;
      --  Reshape Filter Overrun interrupt enable Set and cleared by software.
      --  - 0: Reshape filter overrun interrupt disabled - 1: Reshape filter
      --  overrun interrupt enabled
      RFOVRIE        : Boolean := False;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT5IER_Register use record
      FTHIE          at 0 range 0 .. 0;
      DOVRIE         at 0 range 1 .. 1;
      SSDRIE         at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      OLDIE          at 0 range 4 .. 4;
      Reserved_5_6   at 0 range 5 .. 6;
      SSOVRIE        at 0 range 7 .. 7;
      SCDIE          at 0 range 8 .. 8;
      SATIE          at 0 range 9 .. 9;
      CKABIE         at 0 range 10 .. 10;
      RFOVRIE        at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   --  This register contains the status flags for each digital filter path.
   type MDF_DFLT5ISR_Register is record
      --  Read-only. RXFIFO threshold flag Set by hardware, and cleared by
      --  software by writing this bit to 1 . - 0: Reading 0 means that the
      --  RXFIFO threshold is not reached, writing 0 has no effect. - 1:
      --  Reading 1 means that the RXFIFO reached the threshold, writing 1
      --  clears this flag.
      FTHF           : Boolean := False;
      --  Data overflow flag Set by hardware, and cleared by software by
      --  writing this bit to 1 . - 0: Reading 0 means that no overflow is
      --  detected, writing 0 has no effect. - 1: Reading 1 means that an
      --  overflow is detected, writing 1 clears this flag.
      DOVRF          : Boolean := False;
      --  Snapshot data ready flag Set by hardware, and cleared by software by
      --  writing this bit to 1 . - 0: Reading 0 means that no data is
      --  available on MDF_SNPSxDR, writing 0 has no effect. - 1: Reading 1
      --  means that a new data is available on MDF_SNPSxDR, writing 1 clears
      --  this flag.
      SSDRF          : Boolean := False;
      --  Read-only. RXFIFO Not Empty flag Set and cleared by hardware
      --  according to the RXFIFO level. - 0: Reading 0 means that the RXFIFO
      --  is empty. - 1: Reading 1 means that the RXFIFO is not empty.
      RXNEF          : Boolean := False;
      --  Out-of Limit Detector flag Set by hardware, and cleared by software
      --  by writing this bit to 1 . - 0: Reading 0 means that no OLD event is
      --  detected, writing 0 has no effect. - 1: Reading 1 means that an OLD
      --  event is detected, writing 1 clears THHF, THLF and OLDF flags.
      OLDF           : Boolean := False;
      --  Read-only. Low threshold status flag Set by hardware, and cleared by
      --  software by writing OLDF bit to 1 . This flag indicates the status of
      --  the low threshold comparator when the last OLD event occurred. This
      --  bit gives additional information on the conditions triggering the
      --  last OLD event. It can be cleared by writing OLDF flag to a 1. - 0:
      --  The signal was lower than OLDTHL, when the last OLD event occurred -
      --  1: The signal was higher than OLDTHL, when the last OLD event
      --  occurred
      THLF           : Boolean := False;
      --  Read-only. High threshold status flag Set by hardware, and cleared by
      --  software by writing OLDF bit to 1 . This flag indicates the status of
      --  the high threshold comparator when the last OLD event occurred. This
      --  bit gives additional information on the conditions triggering the
      --  last OLD event. It can be cleared by writing OLDF flag to a 1. - 0:
      --  The signal was lower than OLDTHH, when the last OLD event occurred -
      --  1: The signal was higher than OLDTHH, when the last OLD event
      --  occurred
      THHF           : Boolean := False;
      --  Snapshot overrun flag Set by hardware, and cleared by software by
      --  writing this bit to 1 . - 0: Reading 0 means that no snapshot overrun
      --  event is detected, writing 0 has no effect. - 1: Reading 1 means that
      --  a snapshot overrun event is detected, writing 1 clears this flag.
      SSOVRF         : Boolean := False;
      --  Short-Circuit Detector flag Set by hardware, and cleared by software
      --  by writing this bit to 1 . - 0: Reading 0 means that no SCD event is
      --  detected, writing 0 has no effect. - 1: Reading 1 means that a SCD
      --  event is detected, writing 1 clears this flag.
      SCDF           : Boolean := False;
      --  Saturation detection flag Set by hardware, and cleared by software by
      --  writing this bit to 1 . - 0: Reading 0 means that no saturation is
      --  detected, writing 0 has no effect. - 1: Reading 1 means that a
      --  saturation is detected, writing 1 clears this flag.
      SATF           : Boolean := False;
      --  Clock absence detection flag Set by hardware, and cleared by software
      --  by writing this bit to 1 . - 0: Reading 0 means that no clock absence
      --  is detected, writing 0 has no effect. - 1: Reading 1 means that a
      --  clock absence is detected, writing 1 clears this flag.
      CKABF          : Boolean := False;
      --  Reshape Filter Overrun detection flag Set by hardware, and cleared by
      --  software by writing this bit to 1 . - 0: Reading 0 means that no
      --  reshape filter overrun is detected, writing 0 has no effect. - 1:
      --  Reading 1 means that reshape filter overrun is detected, writing 1
      --  clears this flag.
      RFOVRF         : Boolean := False;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT5ISR_Register use record
      FTHF           at 0 range 0 .. 0;
      DOVRF          at 0 range 1 .. 1;
      SSDRF          at 0 range 2 .. 2;
      RXNEF          at 0 range 3 .. 3;
      OLDF           at 0 range 4 .. 4;
      THLF           at 0 range 5 .. 5;
      THHF           at 0 range 6 .. 6;
      SSOVRF         at 0 range 7 .. 7;
      SCDF           at 0 range 8 .. 8;
      SATF           at 0 range 9 .. 9;
      CKABF          at 0 range 10 .. 10;
      RFOVRF         at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype MDF_OEC5CR_OFFSET_Field is HAL.UInt26;

   --  This register contains the offset compensation value.
   type MDF_OEC5CR_Register is record
      --  Offset error compensation Set and cleared by software. If the
      --  application attempts to write a new offset value while the previous
      --  one is not yet applied, this new offset value is ignored. Reading
      --  back the OFFSET[25:0] field will inform the application on the
      --  current offset value. OFFSET[25:0] represents the value to be
      --  subtracted to the signal before going to the SCALE.
      OFFSET         : MDF_OEC5CR_OFFSET_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_OEC5CR_Register use record
      OFFSET         at 0 range 0 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype MDF_SNPS5DR_MCICDC_Field is HAL.UInt9;
   subtype MDF_SNPS5DR_EXTSDR_Field is HAL.UInt7;
   subtype MDF_SNPS5DR_SDR_Field is HAL.UInt16;

   --  This register is used to read the data processed by each digital filter
   --  in snapshot mode.
   type MDF_SNPS5DR_Register is record
      --  Read-only. Contains the MCIC decimation counter value at the moment
      --  of the last trigger event occurs (MCIC_CNT)
      MCICDC : MDF_SNPS5DR_MCICDC_Field;
      --  Read-only. Extended data size If SNPSFMT = 0 , EXTSDR[6:0] contains
      --  the bit 7 to 1 of the last valid data processed by the digital
      --  filter, If SNPSFMT = 1 , this field contains the INT accumulator
      --  counter value at the moment of the last trigger event occurs
      --  (INT_CNT).
      EXTSDR : MDF_SNPS5DR_EXTSDR_Field;
      --  Read-only. Contains the 16 MSB of the last valid data processed by
      --  the digital filter.
      SDR    : MDF_SNPS5DR_SDR_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_SNPS5DR_Register use record
      MCICDC at 0 range 0 .. 8;
      EXTSDR at 0 range 9 .. 15;
      SDR    at 0 range 16 .. 31;
   end record;

   subtype MDF_DFLT5DR_DR_Field is HAL.UInt24;

   --  This register is used to read the data processed by each digital filter.
   type MDF_DFLT5DR_Register is record
      --  unspecified
      Reserved_0_7 : HAL.UInt8;
      --  Read-only. Data processed by digital filter.
      DR           : MDF_DFLT5DR_DR_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDF_DFLT5DR_Register use record
      Reserved_0_7 at 0 range 0 .. 7;
      DR           at 0 range 8 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Multi-function digital filter
   type MDF_Peripheral is record
      --  MDF global control register
      GCR           : aliased GCR_Register;
      --  MDF clock generator control register
      CKGCR         : aliased CKGCR_Register;
      --  This register is used to control the serial interfaces (SITFx).
      MDF_SITF0CR   : aliased MDF_SITF0CR_Register;
      --  This register is used to select the bitstream to be provided to the
      --  corresponding digital filter and to the SCD.
      MDF_BSMX0CR   : aliased MDF_BSMX0CR_Register;
      --  This register is used to control the digital filter x.
      MDF_DFLT0CR   : aliased MDF_DFLT0CR_Register;
      --  This register is used to control the main CIC filter.
      MDF_DFLT0CICR : aliased MDF_DFLT0CICR_Register;
      --  This register is used to control the reshape and HPF filters.
      MDF_DFLT0RSFR : aliased MDF_DFLT0RSFR_Register;
      --  This register is used to the integrator (INT) settings.
      MDF_DFLT0INTR : aliased MDF_DFLT0INTR_Register;
      --  This register is used to configure the Out-of Limit Detector
      --  function.
      MDF_OLD0CR    : aliased MDF_OLD0CR_Register;
      --  This register is used for the adjustment of the Out-off Limit low
      --  threshold.
      MDF_OLD0THLR  : aliased MDF_OLD0THLR_Register;
      --  This register is used for the adjustment of the Out-off Limit high
      --  threshold.
      MDF_OLD0THHR  : aliased MDF_OLD0THHR_Register;
      --  This register is used for the adjustment stream delays.
      MDF_DLY0CR    : aliased MDF_DLY0CR_Register;
      --  This register is used for the adjustment stream delays.
      MDF_SCD0CR    : aliased MDF_SCD0CR_Register;
      --  This register is used for allowing or not the events to generate an
      --  interrupt.
      MDF_DFLT0IER  : aliased MDF_DFLT0IER_Register;
      --  MDF DFLT0 interrupt status register 0
      MDF_DFLT0ISR  : aliased MDF_DFLT0ISR_Register;
      --  This register contains the offset compensation value.
      MDF_OEC0CR    : aliased MDF_OEC0CR_Register;
      --  This register is used to read the data processed by each digital
      --  filter in snapshot mode.
      MDF_SNPS0DR   : aliased MDF_SNPS0DR_Register;
      --  This register is used to read the data processed by each digital
      --  filter.
      MDF_DFLT0DR   : aliased MDF_DFLT0DR_Register;
      --  This register is used to control the serial interfaces (SITFx).
      MDF_SITF1CR   : aliased MDF_SITF1CR_Register;
      --  This register is used to select the bitstream to be provided to the
      --  corresponding digital filter and to the SCD.
      MDF_BSMX1CR   : aliased MDF_BSMX1CR_Register;
      --  This register is used to control the digital filter x.
      MDF_DFLT1CR   : aliased MDF_DFLT1CR_Register;
      --  This register is used to control the main CIC filter.
      MDF_DFLT1CICR : aliased MDF_DFLT1CICR_Register;
      --  This register is used to control the reshape and HPF filters.
      MDF_DFLT1RSFR : aliased MDF_DFLT1RSFR_Register;
      --  This register is used to the integrator (INT) settings.
      MDF_DFLT1INTR : aliased MDF_DFLT1INTR_Register;
      --  This register is used to configure the Out-of Limit Detector
      --  function.
      MDF_OLD1CR    : aliased MDF_OLD1CR_Register;
      --  This register is used for the adjustment of the Out-off Limit low
      --  threshold.
      MDF_OLD1THLR  : aliased MDF_OLD1THLR_Register;
      --  This register is used for the adjustment of the Out-off Limit high
      --  threshold.
      MDF_OLD1THHR  : aliased MDF_OLD1THHR_Register;
      --  This register is used for the adjustment stream delays.
      MDF_DLY1CR    : aliased MDF_DLY1CR_Register;
      --  This register is used for the adjustment stream delays.
      MDF_SCD1CR    : aliased MDF_SCD1CR_Register;
      --  MDF DFLTx interrupt enable register x
      MDF_DFLT1IER  : aliased MDF_DFLT1IER_Register;
      --  This register contains the status flags for each digital filter path.
      MDF_DFLT1ISR  : aliased MDF_DFLT1ISR_Register;
      --  This register contains the offset compensation value.
      MDF_OEC1CR    : aliased MDF_OEC1CR_Register;
      --  This register is used to read the data processed by each digital
      --  filter in snapshot mode.
      MDF_SNPS1DR   : aliased MDF_SNPS1DR_Register;
      --  This register is used to read the data processed by each digital
      --  filter.
      MDF_DFLT1DR   : aliased MDF_DFLT1DR_Register;
      --  This register is used to control the serial interfaces (SITFx).
      MDF_SITF2CR   : aliased MDF_SITF2CR_Register;
      --  This register is used to select the bitstream to be provided to the
      --  corresponding digital filter and to the SCD.
      MDF_BSMX2CR   : aliased MDF_BSMX2CR_Register;
      --  This register is used to control the digital filter 2.
      MDF_DFLT2CR   : aliased MDF_DFLT2CR_Register;
      --  This register is used to control the main CIC filter.
      MDF_DFLT2CICR : aliased MDF_DFLT2CICR_Register;
      --  This register is used to control the reshape and HPF filters.
      MDF_DFLT2RSFR : aliased MDF_DFLT2RSFR_Register;
      --  This register is used to the integrator (INT) settings.
      MDF_DFLT2INTR : aliased MDF_DFLT2INTR_Register;
      --  This register is used to configure the Out-of Limit Detector
      --  function.
      MDF_OLD2CR    : aliased MDF_OLD2CR_Register;
      --  This register is used for the adjustment of the Out-off Limit low
      --  threshold.
      MDF_OLD2THLR  : aliased MDF_OLD2THLR_Register;
      --  This register is used for the adjustment of the Out-off Limit high
      --  threshold.
      MDF_OLD2THHR  : aliased MDF_OLD2THHR_Register;
      --  This register is used for the adjustment stream delays.
      MDF_DLY2CR    : aliased MDF_DLY2CR_Register;
      --  This register is used for the adjustment stream delays.
      MDF_SCD2CR    : aliased MDF_SCD2CR_Register;
      --  MDF DFLTx interrupt enable register x
      MDF_DFLT2IER  : aliased MDF_DFLT2IER_Register;
      --  This register contains the status flags for each digital filter path.
      MDF_DFLT2ISR  : aliased MDF_DFLT2ISR_Register;
      --  This register contains the offset compensation value.
      MDF_OEC2CR    : aliased MDF_OEC2CR_Register;
      --  This register is used to read the data processed by each digital
      --  filter in snapshot mode.
      MDF_SNPS2DR   : aliased MDF_SNPS2DR_Register;
      --  This register is used to read the data processed by each digital
      --  filter.
      MDF_DFLT2DR   : aliased MDF_DFLT2DR_Register;
      --  This register is used to control the serial interfaces (SITFx).
      MDF_SITF3CR   : aliased MDF_SITF3CR_Register;
      --  This register is used to select the bitstream to be provided to the
      --  corresponding digital filter and to the SCD.
      MDF_BSMX3CR   : aliased MDF_BSMX3CR_Register;
      --  This register is used to control the digital filter 3.
      MDF_DFLT3CR   : aliased MDF_DFLT3CR_Register;
      --  This register is used to control the main CIC filter.
      MDF_DFLT3CICR : aliased MDF_DFLT3CICR_Register;
      --  This register is used to control the reshape and HPF filters.
      MDF_DFLT3RSFR : aliased MDF_DFLT3RSFR_Register;
      --  This register is used to the integrator (INT) settings.
      MDF_DFLT3INTR : aliased MDF_DFLT3INTR_Register;
      --  This register is used to configure the Out-of Limit Detector
      --  function.
      MDF_OLD3CR    : aliased MDF_OLD3CR_Register;
      --  This register is used for the adjustment of the Out-off Limit low
      --  threshold.
      MDF_OLD3THLR  : aliased MDF_OLD3THLR_Register;
      --  This register is used for the adjustment of the Out-off Limit high
      --  threshold.
      MDF_OLD3THHR  : aliased MDF_OLD3THHR_Register;
      --  This register is used for the adjustment stream delays.
      MDF_DLY3CR    : aliased MDF_DLY3CR_Register;
      --  This register is used for the adjustment stream delays.
      MDF_SCD3CR    : aliased MDF_SCD3CR_Register;
      --  MDF DFLTx interrupt enable register x
      MDF_DFLT3IER  : aliased MDF_DFLT3IER_Register;
      --  This register contains the status flags for each digital filter path.
      MDF_DFLT3ISR  : aliased MDF_DFLT3ISR_Register;
      --  This register contains the offset compensation value.
      MDF_OEC3CR    : aliased MDF_OEC3CR_Register;
      --  This register is used to read the data processed by each digital
      --  filter in snapshot mode.
      MDF_SNPS3DR   : aliased MDF_SNPS3DR_Register;
      --  This register is used to read the data processed by each digital
      --  filter.
      MDF_DFLT3DR   : aliased MDF_DFLT3DR_Register;
      --  This register is used to control the serial interfaces (SITFx).
      MDF_SITF4CR   : aliased MDF_SITF4CR_Register;
      --  This register is used to select the bitstream to be provided to the
      --  corresponding digital filter and to the SCD.
      MDF_BSMX4CR   : aliased MDF_BSMX4CR_Register;
      --  This register is used to control the digital filter 4.
      MDF_DFLT4CR   : aliased MDF_DFLT4CR_Register;
      --  This register is used to control the main CIC filter.
      MDF_DFLT4CICR : aliased MDF_DFLT4CICR_Register;
      --  This register is used to control the reshape and HPF filters.
      MDF_DFLT4RSFR : aliased MDF_DFLT4RSFR_Register;
      --  This register is used to the integrator (INT) settings.
      MDF_DFLT4INTR : aliased MDF_DFLT4INTR_Register;
      --  This register is used to configure the Out-of Limit Detector
      --  function.
      MDF_OLD4CR    : aliased MDF_OLD4CR_Register;
      --  This register is used for the adjustment of the Out-off Limit low
      --  threshold.
      MDF_OLD4THLR  : aliased MDF_OLD4THLR_Register;
      --  This register is used for the adjustment of the Out-off Limit high
      --  threshold.
      MDF_OLD4THHR  : aliased MDF_OLD4THHR_Register;
      --  This register is used for the adjustment stream delays.
      MDF_DLY4CR    : aliased MDF_DLY4CR_Register;
      --  This register is used for the adjustment stream delays.
      MDF_SCD4CR    : aliased MDF_SCD4CR_Register;
      --  MDF DFLTx interrupt enable register x
      MDF_DFLT4IER  : aliased MDF_DFLT4IER_Register;
      --  This register contains the status flags for each digital filter path.
      MDF_DFLT4ISR  : aliased MDF_DFLT4ISR_Register;
      --  This register contains the offset compensation value.
      MDF_OEC4CR    : aliased MDF_OEC4CR_Register;
      --  This register is used to read the data processed by each digital
      --  filter in snapshot mode.
      MDF_SNPS4DR   : aliased MDF_SNPS4DR_Register;
      --  This register is used to read the data processed by each digital
      --  filter.
      MDF_DFLT4DR   : aliased MDF_DFLT4DR_Register;
      --  This register is used to control the serial interfaces (SITFx).
      MDF_SITF5CR   : aliased MDF_SITF5CR_Register;
      --  This register is used to select the bitstream to be provided to the
      --  corresponding digital filter and to the SCD.
      MDF_BSMX5CR   : aliased MDF_BSMX5CR_Register;
      --  This register is used to control the digital filter x.
      MDF_DFLT5CR   : aliased MDF_DFLT5CR_Register;
      --  This register is used to control the main CIC filter.
      MDF_DFLT5CICR : aliased MDF_DFLT5CICR_Register;
      --  This register is used to control the reshape and HPF filters.
      MDF_DFLT5RSFR : aliased MDF_DFLT5RSFR_Register;
      --  This register is used to the integrator (INT) settings.
      MDF_DFLT5INTR : aliased MDF_DFLT5INTR_Register;
      --  This register is used to configure the Out-of Limit Detector
      --  function.
      MDF_OLD5CR    : aliased MDF_OLD5CR_Register;
      --  This register is used for the adjustment of the Out-off Limit low
      --  threshold.
      MDF_OLD5THLR  : aliased MDF_OLD5THLR_Register;
      --  This register is used for the adjustment of the Out-off Limit high
      --  threshold.
      MDF_OLD5THHR  : aliased MDF_OLD5THHR_Register;
      --  This register is used for the adjustment stream delays.
      MDF_DLY5CR    : aliased MDF_DLY5CR_Register;
      --  This register is used for the adjustment stream delays.
      MDF_SCD5CR    : aliased MDF_SCD5CR_Register;
      --  MDF DFLTx interrupt enable register x
      MDF_DFLT5IER  : aliased MDF_DFLT5IER_Register;
      --  This register contains the status flags for each digital filter path.
      MDF_DFLT5ISR  : aliased MDF_DFLT5ISR_Register;
      --  This register contains the offset compensation value.
      MDF_OEC5CR    : aliased MDF_OEC5CR_Register;
      --  This register is used to read the data processed by each digital
      --  filter in snapshot mode.
      MDF_SNPS5DR   : aliased MDF_SNPS5DR_Register;
      --  This register is used to read the data processed by each digital
      --  filter.
      MDF_DFLT5DR   : aliased MDF_DFLT5DR_Register;
   end record
     with Volatile;

   for MDF_Peripheral use record
      GCR           at 16#0# range 0 .. 31;
      CKGCR         at 16#4# range 0 .. 31;
      MDF_SITF0CR   at 16#80# range 0 .. 31;
      MDF_BSMX0CR   at 16#84# range 0 .. 31;
      MDF_DFLT0CR   at 16#88# range 0 .. 31;
      MDF_DFLT0CICR at 16#8C# range 0 .. 31;
      MDF_DFLT0RSFR at 16#90# range 0 .. 31;
      MDF_DFLT0INTR at 16#94# range 0 .. 31;
      MDF_OLD0CR    at 16#98# range 0 .. 31;
      MDF_OLD0THLR  at 16#9C# range 0 .. 31;
      MDF_OLD0THHR  at 16#A0# range 0 .. 31;
      MDF_DLY0CR    at 16#A4# range 0 .. 31;
      MDF_SCD0CR    at 16#A8# range 0 .. 31;
      MDF_DFLT0IER  at 16#AC# range 0 .. 31;
      MDF_DFLT0ISR  at 16#B0# range 0 .. 31;
      MDF_OEC0CR    at 16#B4# range 0 .. 31;
      MDF_SNPS0DR   at 16#EC# range 0 .. 31;
      MDF_DFLT0DR   at 16#F0# range 0 .. 31;
      MDF_SITF1CR   at 16#100# range 0 .. 31;
      MDF_BSMX1CR   at 16#104# range 0 .. 31;
      MDF_DFLT1CR   at 16#108# range 0 .. 31;
      MDF_DFLT1CICR at 16#10C# range 0 .. 31;
      MDF_DFLT1RSFR at 16#110# range 0 .. 31;
      MDF_DFLT1INTR at 16#114# range 0 .. 31;
      MDF_OLD1CR    at 16#118# range 0 .. 31;
      MDF_OLD1THLR  at 16#11C# range 0 .. 31;
      MDF_OLD1THHR  at 16#120# range 0 .. 31;
      MDF_DLY1CR    at 16#124# range 0 .. 31;
      MDF_SCD1CR    at 16#128# range 0 .. 31;
      MDF_DFLT1IER  at 16#12C# range 0 .. 31;
      MDF_DFLT1ISR  at 16#130# range 0 .. 31;
      MDF_OEC1CR    at 16#134# range 0 .. 31;
      MDF_SNPS1DR   at 16#16C# range 0 .. 31;
      MDF_DFLT1DR   at 16#170# range 0 .. 31;
      MDF_SITF2CR   at 16#180# range 0 .. 31;
      MDF_BSMX2CR   at 16#184# range 0 .. 31;
      MDF_DFLT2CR   at 16#188# range 0 .. 31;
      MDF_DFLT2CICR at 16#18C# range 0 .. 31;
      MDF_DFLT2RSFR at 16#190# range 0 .. 31;
      MDF_DFLT2INTR at 16#194# range 0 .. 31;
      MDF_OLD2CR    at 16#198# range 0 .. 31;
      MDF_OLD2THLR  at 16#19C# range 0 .. 31;
      MDF_OLD2THHR  at 16#1A0# range 0 .. 31;
      MDF_DLY2CR    at 16#1A4# range 0 .. 31;
      MDF_SCD2CR    at 16#1A8# range 0 .. 31;
      MDF_DFLT2IER  at 16#1AC# range 0 .. 31;
      MDF_DFLT2ISR  at 16#1B0# range 0 .. 31;
      MDF_OEC2CR    at 16#1B4# range 0 .. 31;
      MDF_SNPS2DR   at 16#1EC# range 0 .. 31;
      MDF_DFLT2DR   at 16#1F0# range 0 .. 31;
      MDF_SITF3CR   at 16#200# range 0 .. 31;
      MDF_BSMX3CR   at 16#204# range 0 .. 31;
      MDF_DFLT3CR   at 16#208# range 0 .. 31;
      MDF_DFLT3CICR at 16#20C# range 0 .. 31;
      MDF_DFLT3RSFR at 16#210# range 0 .. 31;
      MDF_DFLT3INTR at 16#214# range 0 .. 31;
      MDF_OLD3CR    at 16#218# range 0 .. 31;
      MDF_OLD3THLR  at 16#21C# range 0 .. 31;
      MDF_OLD3THHR  at 16#220# range 0 .. 31;
      MDF_DLY3CR    at 16#224# range 0 .. 31;
      MDF_SCD3CR    at 16#228# range 0 .. 31;
      MDF_DFLT3IER  at 16#22C# range 0 .. 31;
      MDF_DFLT3ISR  at 16#230# range 0 .. 31;
      MDF_OEC3CR    at 16#234# range 0 .. 31;
      MDF_SNPS3DR   at 16#26C# range 0 .. 31;
      MDF_DFLT3DR   at 16#270# range 0 .. 31;
      MDF_SITF4CR   at 16#280# range 0 .. 31;
      MDF_BSMX4CR   at 16#284# range 0 .. 31;
      MDF_DFLT4CR   at 16#288# range 0 .. 31;
      MDF_DFLT4CICR at 16#28C# range 0 .. 31;
      MDF_DFLT4RSFR at 16#290# range 0 .. 31;
      MDF_DFLT4INTR at 16#294# range 0 .. 31;
      MDF_OLD4CR    at 16#298# range 0 .. 31;
      MDF_OLD4THLR  at 16#29C# range 0 .. 31;
      MDF_OLD4THHR  at 16#2A0# range 0 .. 31;
      MDF_DLY4CR    at 16#2A4# range 0 .. 31;
      MDF_SCD4CR    at 16#2A8# range 0 .. 31;
      MDF_DFLT4IER  at 16#2AC# range 0 .. 31;
      MDF_DFLT4ISR  at 16#2B0# range 0 .. 31;
      MDF_OEC4CR    at 16#2B4# range 0 .. 31;
      MDF_SNPS4DR   at 16#2EC# range 0 .. 31;
      MDF_DFLT4DR   at 16#2F0# range 0 .. 31;
      MDF_SITF5CR   at 16#300# range 0 .. 31;
      MDF_BSMX5CR   at 16#304# range 0 .. 31;
      MDF_DFLT5CR   at 16#308# range 0 .. 31;
      MDF_DFLT5CICR at 16#30C# range 0 .. 31;
      MDF_DFLT5RSFR at 16#310# range 0 .. 31;
      MDF_DFLT5INTR at 16#314# range 0 .. 31;
      MDF_OLD5CR    at 16#318# range 0 .. 31;
      MDF_OLD5THLR  at 16#31C# range 0 .. 31;
      MDF_OLD5THHR  at 16#320# range 0 .. 31;
      MDF_DLY5CR    at 16#324# range 0 .. 31;
      MDF_SCD5CR    at 16#328# range 0 .. 31;
      MDF_DFLT5IER  at 16#32C# range 0 .. 31;
      MDF_DFLT5ISR  at 16#330# range 0 .. 31;
      MDF_OEC5CR    at 16#334# range 0 .. 31;
      MDF_SNPS5DR   at 16#36C# range 0 .. 31;
      MDF_DFLT5DR   at 16#370# range 0 .. 31;
   end record;

   --  Multi-function digital filter
   MDF1_Periph : aliased MDF_Peripheral
     with Import, Address => MDF1_Base;

   --  Multi-function digital filter
   SEC_MDF1_Periph : aliased MDF_Peripheral
     with Import, Address => SEC_MDF1_Base;

end STM32_SVD.MDF;
