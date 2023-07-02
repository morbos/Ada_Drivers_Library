--  This spec has been automatically generated from STM32U585.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.ADC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  ADC_ISR_AWD array
   type ADC_ISR_AWD_Field_Array is array (1 .. 3) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for ADC_ISR_AWD
   type ADC_ISR_AWD_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AWD as a value
            Val : HAL.UInt3;
         when True =>
            --  AWD as an array
            Arr : ADC_ISR_AWD_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for ADC_ISR_AWD_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  ADC interrupt and status register
   type ADC_ISR_Register is record
      --  ADC ready This bit is set by hardware after the ADC has been enabled
      --  (bit ADEN = 1) and when the ADC reaches a state where it is ready
      --  to accept conversion requests. It is cleared by software writing 1 to
      --  it.
      ADRDY          : Boolean := False;
      --  End of sampling flag This bit is set by hardware during the
      --  conversion of any channel (only for regular channels), at the end of
      --  the sampling phase.
      EOSMP          : Boolean := False;
      --  End of conversion flag This bit is set by hardware at the end of each
      --  regular conversion of a channel when a new data is available in the
      --  ADC_DR register. It is cleared by software writing 1 to it or by
      --  reading the ADC_DR register
      EOC            : Boolean := False;
      --  End of regular sequence flag This bit is set by hardware at the end
      --  of the conversions of a regular sequence of channels. It is cleared
      --  by software writing 1 to it.
      EOS            : Boolean := False;
      --  ADC overrun This bit is set by hardware when an overrun occurs on a
      --  regular channel, meaning that a new conversion has completed while
      --  the EOC flag was already set. It is cleared by software writing 1 to
      --  it.
      OVR            : Boolean := False;
      --  Injected channel end of conversion flag This bit is set by hardware
      --  at the end of each injected conversion of a channel when a new data
      --  is available in the corresponding ADC_JDRy register. It is cleared by
      --  software writing 1 to it or by reading the corresponding ADC_JDRy
      --  register
      JEOC           : Boolean := False;
      --  Injected channel end of sequence flag This bit is set by hardware at
      --  the end of the conversions of all injected channels in the group. It
      --  is cleared by software writing 1 to it.
      JEOS           : Boolean := False;
      --  Analog watchdog 1 flag This bit is set by hardware when the converted
      --  voltage crosses the values programmed in the fields LT1[11:0] and
      --  HT1[11:0] of ADC_LTR1, & ADC_HTR1 register. It is cleared by
      --  software. writing 1 to it.
      AWD            : ADC_ISR_AWD_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_10_11 : HAL.UInt2 := 16#0#;
      --  Read-only. ADC voltage regulator ready This bit is set by hardware.
      --  It indicates that the ADC internal supply is ready. The ADC is
      --  available after tADCVREG_SETUP time.
      LDORDY         : Boolean := False;
      --  unspecified
      Reserved_13_31 : HAL.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_ISR_Register use record
      ADRDY          at 0 range 0 .. 0;
      EOSMP          at 0 range 1 .. 1;
      EOC            at 0 range 2 .. 2;
      EOS            at 0 range 3 .. 3;
      OVR            at 0 range 4 .. 4;
      JEOC           at 0 range 5 .. 5;
      JEOS           at 0 range 6 .. 6;
      AWD            at 0 range 7 .. 9;
      Reserved_10_11 at 0 range 10 .. 11;
      LDORDY         at 0 range 12 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   --  ADC interrupt enable register
   type ADC_IER_Register is record
      --  ADC ready interrupt enable This bit is set and cleared by software to
      --  enable/disable the ADC Ready interrupt. Note: Software is allowed to
      --  write this bit only when ADSTART = 0 and JADSTART = 0 (which
      --  ensures that no conversion is ongoing).
      ADRDYIE        : Boolean := False;
      --  End of sampling flag interrupt enable for regular conversions This
      --  bit is set and cleared by software to enable/disable the end of the
      --  sampling phase interrupt for regular conversions. Note: Software is
      --  allowed to write this bit only when ADSTART = 0 (which ensures that
      --  no regular conversion is ongoing).
      EOSMPIE        : Boolean := False;
      --  End of regular conversion interrupt enable This bit is set and
      --  cleared by software to enable/disable the end of a regular conversion
      --  interrupt. Note: Software is allowed to write this bit only when
      --  ADSTART = 0 (which ensures that no regular conversion is ongoing).
      EOCIE          : Boolean := False;
      --  End of regular sequence of conversions interrupt enable This bit is
      --  set and cleared by software to enable/disable the end of regular
      --  sequence of conversions interrupt. Note: Software is allowed to write
      --  this bit only when ADSTART = 0 (which ensures that no regular
      --  conversion is ongoing).
      EOSIE          : Boolean := False;
      --  Overrun interrupt enable This bit is set and cleared by software to
      --  enable/disable the Overrun interrupt of a regular conversion. Note:
      --  Software is allowed to write this bit only when ADSTART = 0 (which
      --  ensures that no regular conversion is ongoing).
      OVRIE          : Boolean := False;
      --  End of injected conversion interrupt enable This bit is set and
      --  cleared by software to enable/disable the end of an injected
      --  conversion interrupt. Note: Software is allowed to write this bit
      --  only when JADSTART = 0 (which ensures that no regular conversion is
      --  ongoing).
      JEOCIE         : Boolean := False;
      --  End of injected sequence of conversions interrupt enable This bit is
      --  set and cleared by software to enable/disable the end of injected
      --  sequence of conversions interrupt. Note: Software is allowed to write
      --  this bit only when JADSTART = 0 (which ensures that no injected
      --  conversion is ongoing).
      JEOSIE         : Boolean := False;
      --  Analog watchdog 1 interrupt enable This bit is set and cleared by
      --  software to enable/disable the analog watchdog 1 interrupt. Note:
      --  Software is allowed to write this bit only when ADSTART = 0 and
      --  JADSTART = 0 (which ensures that no conversion is ongoing).
      AWD1IE         : Boolean := False;
      --  Analog watchdog 2 interrupt enable This bit is set and cleared by
      --  software to enable/disable the analog watchdog 2 interrupt. Note:
      --  Software is allowed to write this bit only when ADSTART = 0 and
      --  JADSTART = 0 (which ensures that no conversion is ongoing).
      AWD2IE         : Boolean := False;
      --  Analog watchdog 3 interrupt enable This bit is set and cleared by
      --  software to enable/disable the analog watchdog 2 interrupt. Note:
      --  Software is allowed to write this bit only when ADSTART = 0 and
      --  JADSTART = 0 (which ensures that no conversion is ongoing).
      AWD3IE         : Boolean := False;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_IER_Register use record
      ADRDYIE        at 0 range 0 .. 0;
      EOSMPIE        at 0 range 1 .. 1;
      EOCIE          at 0 range 2 .. 2;
      EOSIE          at 0 range 3 .. 3;
      OVRIE          at 0 range 4 .. 4;
      JEOCIE         at 0 range 5 .. 5;
      JEOSIE         at 0 range 6 .. 6;
      AWD1IE         at 0 range 7 .. 7;
      AWD2IE         at 0 range 8 .. 8;
      AWD3IE         at 0 range 9 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype ADC_CR_CALINDEX_Field is HAL.UInt4;

   --  ADC control register
   type ADC_CR_Register is record
      --  ADC enable control This bit is set by software to enable the ADC. The
      --  ADC is effectively ready to operate once the flag ADRDY has been set.
      --  It is cleared by hardware when the ADC is disabled, after the
      --  execution of the ADDIS command. Note: The software is allowed to set
      --  ADEN only when all bits of ADC_CR registers are 0 (ADCAL = 0,
      --  JADSTART = 0, ADSTART = 0, ADSTP = 0, ADDIS = 0 and
      --  ADEN = 0) except for bit ADVREGEN which must be 1 (and the software
      --  must have wait for the startup time of the voltage regulator)
      ADEN           : Boolean := False;
      --  ADC disable command This bit is set by software to disable the ADC
      --  (ADDIS command) and put it into power-down state (OFF state). It is
      --  cleared by hardware once the ADC is effectively disabled (ADEN is
      --  also cleared by hardware at this time). Note: The software is allowed
      --  to set ADDIS only when ADEN = 1 and both ADSTART = 0 and
      --  JADSTART = 0 (which ensures that no conversion is ongoing)
      ADDIS          : Boolean := False;
      --  ADC start of regular conversion This bit is set by software to start
      --  ADC conversion of regular channels. Depending on the configuration
      --  bits EXTEN[1:0], a conversion starts immediately (software trigger
      --  configuration) or once a regular hardware trigger event occurs
      --  (hardware trigger configuration). It is cleared by hardware:  in
      --  Single conversion mode (CONT = 0, DISCEN = 0) when software
      --  trigger is selected (EXTEN[1:0] = 0x0): at the assertion of the end
      --  of regular conversion sequence (EOS) flag. In Discontinuous
      --  conversion mode (CONT = 0, DISCEN = 1), when the software trigger
      --  is selected (EXTEN[1:0] = 0x0): at the end of conversion (EOC)
      --  flag. in all other cases: after the execution of the ADSTP command,
      --  at the same time that ADSTP is cleared by hardware. Note: The
      --  software is allowed to set ADSTART only when ADEN = 1 and
      --  ADDIS = 0 (ADC is enabled and there is no pending request to
      --  disable the ADC) In Auto-injection mode (JAUTO = 1), regular and
      --  auto-injected conversions are started by setting bit ADSTART
      --  (JADSTART must be kept cleared)
      ADSTART        : Boolean := False;
      --  ADC start of injected conversion This bit is set by software to start
      --  ADC conversion of injected channels. Depending on the configuration
      --  bits JEXTEN[1:0], a conversion starts immediately (software trigger
      --  configuration) or once an injected hardware trigger event occurs
      --  (hardware trigger configuration). It is cleared by hardware: in
      --  Single conversion mode when software trigger is selected
      --  (JEXTSEL = 0x0): at the assertion of the end of injected conversion
      --  sequence (JEOS) flag. in all cases: after the execution of the JADSTP
      --  command, at the same time as JADSTP is cleared by hardware. Note: The
      --  software is allowed to set JADSTART only when ADEN = 1 and
      --  ADDIS = 0 (ADC is enabled and there is no pending request to
      --  disable the ADC). In Auto-injection mode (JAUTO = 1), regular and
      --  auto-injected conversions are started by setting bit ADSTART
      --  (JADSTART must be kept cleared)
      JADSTART       : Boolean := False;
      --  ADC stop of regular conversion command This bit is set by software to
      --  stop and discard an ongoing regular conversion (ADSTP Command). It is
      --  cleared by hardware when the conversion is effectively discarded and
      --  the ADC regular sequence and triggers can be re-configured. The ADC
      --  is then ready to accept a new start of regular conversions (ADSTART
      --  command). Note: The software is allowed to set ADSTP only when
      --  ADSTART = 1 and ADDIS = 0 (ADC is enabled and eventually
      --  converting a regular conversion and there is no pending request to
      --  disable the ADC). In Auto-injection mode (JAUTO = 1), setting ADSTP
      --  bit aborts both regular and injected conversions (do not use JADSTP).
      ADSTP          : Boolean := False;
      --  ADC stop of injected conversion command This bit is set by software
      --  to stop and discard an ongoing injected conversion (JADSTP Command).
      --  It is cleared by hardware when the conversion is effectively
      --  discarded and the ADC injected sequence and triggers can be
      --  re-configured. The ADC is then ready to accept a new start of
      --  injected conversions (JADSTART command). Note: The software is
      --  allowed to set JADSTP only when JADSTART = 1 and ADDIS = 0 (ADC
      --  is enabled and eventually converting an injected conversion and there
      --  is no pending request to disable the ADC). In Auto-injection mode
      --  (JAUTO = 1), setting ADSTP bit aborts both regular and injected
      --  conversions (do not use JADSTP)
      JADSTP         : Boolean := False;
      --  unspecified
      Reserved_6_15  : HAL.UInt10 := 16#0#;
      --  Linearity calibration This bit is set and cleared by software to
      --  enable the linearity calibration. Note: The software is allowed to
      --  write this bit only when the ADC is disabled and is not calibrating
      --  (ADCAL = 0, JADSTART = 0, JADSTP = 0, ADSTART = 0,
      --  ADSTP = 0, ADDIS = 0 and ADEN = 0).
      ADCALLIN       : Boolean := False;
      --  unspecified
      Reserved_17_23 : HAL.UInt7 := 16#0#;
      --  Calibration factor This bitfield controls the calibration factor to
      --  be read or written. Calibration index 0 is dedicated to single-ended
      --  and differential offsets, calibration index 1 to 7 to the linearity
      --  calibration factors, and index 8 to the internal offset: Others:
      --  Reserved, must not be used Note: ADC_CALFACT2[31:0] correspond to the
      --  location of CALINDEX[3:0] calibration factor data (see for details).
      CALINDEX       : ADC_CR_CALINDEX_Field := 16#0#;
      --  ADC voltage regulator enable This bits is set by software to enable
      --  the ADC voltage regulator. Before performing any operation such as
      --  launching a calibration or enabling the ADC, the ADC voltage
      --  regulator must first be enabled and the software must wait for the
      --  regulator start-up time. For more details about the ADC voltage
      --  regulator enable and disable sequences, refer to (ADVREGEN). The
      --  software can program this bit field only when the ADC is disabled
      --  (ADCAL = 0, JADSTART = 0, ADSTART = 0, ADSTP = 0, ADDIS = 0
      --  and ADEN = 0).
      ADVREGEN       : Boolean := False;
      --  Deep-power-down enable This bit is set and cleared by software to put
      --  the ADC in Deep-power-down mode. Note: The software is allowed to
      --  write this bit only when the ADC is disabled (ADCAL = 0,
      --  JADSTART = 0, JADSTP = 0, ADSTART = 0, ADSTP = 0, ADDIS = 0
      --  and ADEN = 0).
      DEEPPWD        : Boolean := True;
      --  unspecified
      Reserved_30_30 : HAL.Bit := 16#0#;
      --  ADC calibration This bit is set by software to start the ADC
      --  calibration. It is cleared by hardware after calibration is complete.
      --  Note: The software is allowed to launch a calibration by setting
      --  ADCAL only when ADEN = 0.
      ADCAL          : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_CR_Register use record
      ADEN           at 0 range 0 .. 0;
      ADDIS          at 0 range 1 .. 1;
      ADSTART        at 0 range 2 .. 2;
      JADSTART       at 0 range 3 .. 3;
      ADSTP          at 0 range 4 .. 4;
      JADSTP         at 0 range 5 .. 5;
      Reserved_6_15  at 0 range 6 .. 15;
      ADCALLIN       at 0 range 16 .. 16;
      Reserved_17_23 at 0 range 17 .. 23;
      CALINDEX       at 0 range 24 .. 27;
      ADVREGEN       at 0 range 28 .. 28;
      DEEPPWD        at 0 range 29 .. 29;
      Reserved_30_30 at 0 range 30 .. 30;
      ADCAL          at 0 range 31 .. 31;
   end record;

   subtype ADC_CFGR1_DMNGT_Field is HAL.UInt2;
   subtype ADC_CFGR1_RES_Field is HAL.UInt2;
   subtype ADC_CFGR1_EXTSEL_Field is HAL.UInt5;
   subtype ADC_CFGR1_EXTEN_Field is HAL.UInt2;
   subtype ADC_CFGR1_DISCNUM_Field is HAL.UInt3;
   subtype ADC_CFGR1_AWD1CH_Field is HAL.UInt5;

   --  ADC configuration register
   type ADC_CFGR1_Register is record
      --  Data management configuration This bit is set and cleared by software
      --  to select how the ADC interface output data are managed. Note: The
      --  software is allowed to write this bit only when ADSTART = 0 and
      --  JADSTART = 0 (which ensures that no conversion is ongoing).
      DMNGT          : ADC_CFGR1_DMNGT_Field := 16#0#;
      --  Data resolution These bits are written by software to select the
      --  resolution of the conversion. Note: The software is allowed to write
      --  these bits only when ADSTART = 0 and JADSTART = 0 (which ensures
      --  that no conversion is ongoing).
      RES            : ADC_CFGR1_RES_Field := 16#0#;
      --  unspecified
      Reserved_4_4   : HAL.Bit := 16#0#;
      --  External trigger selection for regular group These bits select the
      --  external event used to trigger the start of conversion of a regular
      --  group: ... Refer to the ADC external trigger for regular channels in
      --  signals for details on trigger mapping. Note: The software is allowed
      --  to write these bits only when ADSTART = 0 (which ensures that no
      --  regular conversion is ongoing).
      EXTSEL         : ADC_CFGR1_EXTSEL_Field := 16#0#;
      --  External trigger enable and polarity selection for regular channels
      --  These bits are set and cleared by software to select the external
      --  trigger polarity and enable the trigger of a regular group. Note: The
      --  software is allowed to write these bits only when ADSTART = 0
      --  (which ensures that no regular conversion is ongoing).
      EXTEN          : ADC_CFGR1_EXTEN_Field := 16#0#;
      --  Overrun Mode This bit is set and cleared by software and configure
      --  the way data overrun is managed. Note: The software is allowed to
      --  write this bit only when ADSTART = 0 (which ensures that no regular
      --  conversion is ongoing).
      OVRMOD         : Boolean := False;
      --  Single / continuous conversion mode for regular conversions This bit
      --  is set and cleared by software. If it is set, regular conversion
      --  takes place continuously until it is cleared. Note: It is not
      --  possible to have both discontinuous mode and continuous mode enabled:
      --  it is forbidden to set both DISCEN = 1 and CONT = 1. The software
      --  is allowed to write this bit only when ADSTART = 0 (which ensures
      --  that no regular conversion is ongoing).
      CONT           : Boolean := False;
      --  Delayed conversion mode This bit is set and cleared by software to
      --  enable/disable the Auto Delayed Conversion mode.. Note: The software
      --  is allowed to write this bit only when ADSTART = 0 and
      --  JADSTART = 0 (which ensures that no conversion is ongoing).
      AUTDLY         : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Discontinuous mode for regular channels This bit is set and cleared
      --  by software to enable/disable Discontinuous mode for regular
      --  channels. Note: It is not possible to have both discontinuous mode
      --  and continuous mode enabled: it is forbidden to set both DISCEN = 1
      --  and CONT = 1. It is not possible to use both auto-injected mode and
      --  discontinuous mode simultaneously: the bits DISCEN and JDISCEN must
      --  be kept cleared by software when JAUTO is set. The software is
      --  allowed to write this bit only when ADSTART = 0 (which ensures that
      --  no regular conversion is ongoing).
      DISCEN         : Boolean := False;
      --  Discontinuous mode channel count These bits are written by software
      --  to define the number of regular channels to be converted in
      --  discontinuous mode, after receiving an external trigger. ... Note:
      --  The software is allowed to write these bits only when ADSTART = 0
      --  (which ensures that no regular conversion is ongoing).
      DISCNUM        : ADC_CFGR1_DISCNUM_Field := 16#0#;
      --  Discontinuous mode on injected channels This bit is set and cleared
      --  by software to enable/disable discontinuous mode on the injected
      --  channels of a group. Note: The software is allowed to write this bit
      --  only when JADSTART = 0 (which ensures that no injected conversion
      --  is ongoing). It is not possible to use both auto-injected mode and
      --  discontinuous mode simultaneously: the bits DISCEN and JDISCEN must
      --  be kept cleared by software when JAUTO is set.
      JDISCEN        : Boolean := False;
      --  unspecified
      Reserved_21_21 : HAL.Bit := 16#0#;
      --  Enable the watchdog 1 on a single channel or on all channels This bit
      --  is set and cleared by software to enable the analog watchdog on the
      --  channel identified by the AWD1CH[4:0] bits or on all the channels
      --  Note: The software is allowed to write these bits only when
      --  ADSTART = 0 and JADSTART = 0 (which ensures that no conversion is
      --  ongoing).
      AWD1SGL        : Boolean := False;
      --  Analog watchdog 1 enable on regular channels This bit is set and
      --  cleared by software Note: The software is allowed to write this bit
      --  only when ADSTART = 0 (which ensures that no regular conversion is
      --  ongoing).
      AWD1EN         : Boolean := False;
      --  Analog watchdog 1 enable on injected channels This bit is set and
      --  cleared by software Note: The software is allowed to write this bit
      --  only when JADSTART = 0 (which ensures that no injected conversion
      --  is ongoing).
      JAWD1EN        : Boolean := False;
      --  Automatic injected group conversion This bit is set and cleared by
      --  software to enable/disable automatic injected group conversion after
      --  regular group conversion. Note: The software is allowed to write this
      --  bit only when ADSTART = 0 and JADSTART = 0 (which ensures that no
      --  regular nor injected conversion is ongoing).
      JAUTO          : Boolean := False;
      --  Analog watchdog 1 channel selection These bits are set and cleared by
      --  software. They select the input channel to be guarded by the analog
      --  watchdog. ..... Others: Reserved, must not be used Note: The channel
      --  selected by AWD1CH must be also selected into the SQRi or JSQRi
      --  registers. Software is allowed to write these bits only when
      --  ADSTART = 0 and JADSTART = 0 (which ensures that no conversion is
      --  ongoing).
      AWD1CH         : ADC_CFGR1_AWD1CH_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#1#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_CFGR1_Register use record
      DMNGT          at 0 range 0 .. 1;
      RES            at 0 range 2 .. 3;
      Reserved_4_4   at 0 range 4 .. 4;
      EXTSEL         at 0 range 5 .. 9;
      EXTEN          at 0 range 10 .. 11;
      OVRMOD         at 0 range 12 .. 12;
      CONT           at 0 range 13 .. 13;
      AUTDLY         at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      DISCEN         at 0 range 16 .. 16;
      DISCNUM        at 0 range 17 .. 19;
      JDISCEN        at 0 range 20 .. 20;
      Reserved_21_21 at 0 range 21 .. 21;
      AWD1SGL        at 0 range 22 .. 22;
      AWD1EN         at 0 range 23 .. 23;
      JAWD1EN        at 0 range 24 .. 24;
      JAUTO          at 0 range 25 .. 25;
      AWD1CH         at 0 range 26 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype ADC_CFGR2_OVSS_Field is HAL.UInt4;
   subtype ADC_CFGR2_OSR_Field is HAL.UInt10;
   subtype ADC_CFGR2_LSHIFT_Field is HAL.UInt4;

   --  ADC configuration register 2
   type ADC_CFGR2_Register is record
      --  Regular Oversampling Enable This bit is set and cleared by software
      --  to enable regular oversampling. Note: The software is allowed to
      --  write this bit only when ADSTART = 0 and JADSTART = 0 (which ensures
      --  that no conversion is ongoing)
      ROVSE          : Boolean := False;
      --  Injected Oversampling Enable This bit is set and cleared by software
      --  to enable injected oversampling. Note: The software is allowed to
      --  write this bit only when ADSTART = 0 and JADSTART = 0 (which ensures
      --  that no conversion is ongoing)
      JOVSE          : Boolean := False;
      --  unspecified
      Reserved_2_4   : HAL.UInt3 := 16#0#;
      --  Oversampling right shift This bit field is set and cleared by
      --  software to define the right shifting applied to the raw oversampling
      --  result. Others: Reserved, must not be used. Note: The software is
      --  allowed to write these bits only when ADSTART = 0 (which ensures that
      --  no conversion is ongoing).
      OVSS           : ADC_CFGR2_OVSS_Field := 16#0#;
      --  Triggered Regular Oversampling This bit is set and cleared by
      --  software to enable triggered oversampling Note: The software is
      --  allowed to write this bit only when ADSTART = 0 (which ensures that
      --  no conversion is ongoing).
      TROVS          : Boolean := False;
      --  Regular Oversampling mode This bit is set and cleared by software to
      --  select the regular oversampling mode. Note: The software is allowed
      --  to write this bit only when ADSTART = 0 (which ensures that no
      --  conversion is ongoing).
      ROVSM          : Boolean := False;
      --  unspecified
      Reserved_11_12 : HAL.UInt2 := 16#0#;
      --  Bulb sampling mode This bit is set and cleared by software to select
      --  the bulb sampling mode. SMPTRIG bit must not be set when the BULB bit
      --  is set. The very first ADC conversion is performed with the sampling
      --  time specified in SMPx bits. Note: The software is allowed to write
      --  this bit only when ADSTART = 0 (which ensures that no conversion is
      --  ongoing).
      BULB           : Boolean := False;
      --  Software trigger bit for sampling time control trigger mode This bit
      --  is set and cleared by software to enable the bulb sampling mode.
      --  Note: The software is allowed to write this bit only when
      --  ADSTART = 0 (which ensures that no conversion is ongoing).
      SWTRIG         : Boolean := False;
      --  Sampling time control trigger mode This bit is set and cleared by
      --  software to enable the sampling time control trigger mode. The
      --  sampling time starts on the trigger rising edge, and the conversion
      --  on the trigger falling edge. EXTEN[1:0] bits must be set to 01. BULB
      --  bit must not be set when the SMPTRIG bit is set. When EXTEN[1:0] bits
      --  is set to 00, set SWTRIG to start the sampling and clear SWTRIG bit
      --  to start the conversion. Note: The software is allowed to write this
      --  bit only when ADSTART = 0 (which ensures that no conversion is
      --  ongoing).
      SMPTRIG        : Boolean := False;
      --  Oversampling ratio This bitfield is set and cleared by software to
      --  define the oversampling ratio. 2: 3x ... 1023: 1024x Note: The
      --  software is allowed to write this bit only when ADSTART = 0 (which
      --  ensures that no conversion is ongoing).
      OSR            : ADC_CFGR2_OSR_Field := 16#0#;
      --  unspecified
      Reserved_26_26 : HAL.Bit := 16#0#;
      --  Low-frequency trigger This bit is set and cleared by software Note:
      --  The software is allowed to write this bit only when ADSTART = 0
      --  (which ensures that no conversion is ongoing).
      LFTRIG         : Boolean := False;
      --  Left shift factor This bitfield is set and cleared by software to
      --  define the left shifting applied to the final result with or without
      --  oversampling. Note: The software is allowed to write this bit only
      --  when ADSTART = 0 (which ensures that no conversion is ongoing).
      LSHIFT         : ADC_CFGR2_LSHIFT_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_CFGR2_Register use record
      ROVSE          at 0 range 0 .. 0;
      JOVSE          at 0 range 1 .. 1;
      Reserved_2_4   at 0 range 2 .. 4;
      OVSS           at 0 range 5 .. 8;
      TROVS          at 0 range 9 .. 9;
      ROVSM          at 0 range 10 .. 10;
      Reserved_11_12 at 0 range 11 .. 12;
      BULB           at 0 range 13 .. 13;
      SWTRIG         at 0 range 14 .. 14;
      SMPTRIG        at 0 range 15 .. 15;
      OSR            at 0 range 16 .. 25;
      Reserved_26_26 at 0 range 26 .. 26;
      LFTRIG         at 0 range 27 .. 27;
      LSHIFT         at 0 range 28 .. 31;
   end record;

   --  ADC_SMPR1_SMP array element
   subtype ADC_SMPR1_SMP_Element is HAL.UInt3;

   --  ADC_SMPR1_SMP array
   type ADC_SMPR1_SMP_Field_Array is array (0 .. 9) of ADC_SMPR1_SMP_Element
     with Component_Size => 3, Size => 30;

   --  Type definition for ADC_SMPR1_SMP
   type ADC_SMPR1_SMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SMP as a value
            Val : HAL.UInt30;
         when True =>
            --  SMP as an array
            Arr : ADC_SMPR1_SMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 30;

   for ADC_SMPR1_SMP_Field use record
      Val at 0 range 0 .. 29;
      Arr at 0 range 0 .. 29;
   end record;

   --  ADC sample time register 1
   type ADC_SMPR1_Register is record
      --  Channel x sampling time selection (x = 0 to 9) These bits are written
      --  by software to select the sampling time individually for each
      --  channel. During sample cycles, the channel selection bits must remain
      --  unchanged. Note: The software is allowed to write these bits only
      --  when ADSTART = 0 and JADSTART = 0 (which ensures that no conversion
      --  is ongoing).
      SMP            : ADC_SMPR1_SMP_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_SMPR1_Register use record
      SMP            at 0 range 0 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   --  ADC_SMPR2_SMP array element
   subtype ADC_SMPR2_SMP_Element is HAL.UInt3;

   --  ADC_SMPR2_SMP array
   type ADC_SMPR2_SMP_Field_Array is array (10 .. 19)
     of ADC_SMPR2_SMP_Element
     with Component_Size => 3, Size => 30;

   --  Type definition for ADC_SMPR2_SMP
   type ADC_SMPR2_SMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SMP as a value
            Val : HAL.UInt30;
         when True =>
            --  SMP as an array
            Arr : ADC_SMPR2_SMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 30;

   for ADC_SMPR2_SMP_Field use record
      Val at 0 range 0 .. 29;
      Arr at 0 range 0 .. 29;
   end record;

   --  ADC sample time register 2
   type ADC_SMPR2_Register is record
      --  Channel x sampling time selection (x = 0 to 9) These bits are written
      --  by software to select the sampling time individually for each
      --  channel. During sampling cycles, the channel selection bits must
      --  remain unchanged. Note: The software is allowed to write these bits
      --  only when ADSTART = 0 and JADSTART = 0 (which ensures that no
      --  conversion is ongoing).
      SMP            : ADC_SMPR2_SMP_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_SMPR2_Register use record
      SMP            at 0 range 0 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   --  ADC_PCSEL_PCSEL array
   type ADC_PCSEL_PCSEL_Field_Array is array (0 .. 19) of Boolean
     with Component_Size => 1, Size => 20;

   --  Type definition for ADC_PCSEL_PCSEL
   type ADC_PCSEL_PCSEL_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PCSEL as a value
            Val : HAL.UInt20;
         when True =>
            --  PCSEL as an array
            Arr : ADC_PCSEL_PCSEL_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 20;

   for ADC_PCSEL_PCSEL_Field use record
      Val at 0 range 0 .. 19;
      Arr at 0 range 0 .. 19;
   end record;

   --  ADC channel preselection register
   type ADC_PCSEL_Register is record
      --  Channel i (VINP[i]) preselection These bits are written by software
      --  to preselect the input channel I/O instance to be converted. Note:
      --  The software is allowed to write these bits only when ADSTART = 0
      --  and JADSTART = 0 (which ensures that no conversion is ongoing).
      PCSEL          : ADC_PCSEL_PCSEL_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_PCSEL_Register use record
      PCSEL          at 0 range 0 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   subtype ADC_SQR1_L_Field is HAL.UInt4;
   subtype ADC_SQR1_SQ1_Field is HAL.UInt5;
   subtype ADC_SQR1_SQ2_Field is HAL.UInt5;
   subtype ADC_SQR1_SQ3_Field is HAL.UInt5;
   subtype ADC_SQR1_SQ4_Field is HAL.UInt5;

   --  ADC regular sequence register 1
   type ADC_SQR1_Register is record
      --  Regular channel sequence length These bits are written by software to
      --  define the total number of conversions in the regular channel
      --  conversion sequence. ... Note: The software is allowed to write these
      --  bits only when ADSTART = 0 (which ensures that no regular
      --  conversion is ongoing).
      L              : ADC_SQR1_L_Field := 16#0#;
      --  unspecified
      Reserved_4_5   : HAL.UInt2 := 16#0#;
      --  1st conversion in regular sequence These bits are written by software
      --  with the channel number (0..19) assigned as the 1st in the regular
      --  conversion sequence. Note: The software is allowed to write these
      --  bits only when ADSTART = 0 (which ensures that no regular
      --  conversion is ongoing).
      SQ1            : ADC_SQR1_SQ1_Field := 16#0#;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  2nd conversion in regular sequence These bits are written by software
      --  with the channel number (0..19) assigned as the 2nd in the regular
      --  conversion sequence. Note: The software is allowed to write these
      --  bits only when ADSTART = 0 (which ensures that no regular
      --  conversion is ongoing).
      SQ2            : ADC_SQR1_SQ2_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : HAL.Bit := 16#0#;
      --  3rd conversion in regular sequence These bits are written by software
      --  with the channel number (0..19) assigned as the 3rd in the regular
      --  conversion sequence. Note: The software is allowed to write these
      --  bits only when ADSTART = 0 (which ensures that no regular
      --  conversion is ongoing).
      SQ3            : ADC_SQR1_SQ3_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : HAL.Bit := 16#0#;
      --  4th conversion in regular sequence These bits are written by software
      --  with the channel number (0..19) assigned as the 4th in the regular
      --  conversion sequence.
      SQ4            : ADC_SQR1_SQ4_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_SQR1_Register use record
      L              at 0 range 0 .. 3;
      Reserved_4_5   at 0 range 4 .. 5;
      SQ1            at 0 range 6 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      SQ2            at 0 range 12 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      SQ3            at 0 range 18 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      SQ4            at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype ADC_SQR2_SQ5_Field is HAL.UInt5;
   subtype ADC_SQR2_SQ6_Field is HAL.UInt5;
   subtype ADC_SQR2_SQ7_Field is HAL.UInt5;
   subtype ADC_SQR2_SQ8_Field is HAL.UInt5;
   subtype ADC_SQR2_SQ9_Field is HAL.UInt5;

   --  ADC regular sequence register 2
   type ADC_SQR2_Register is record
      --  5th conversion in regular sequence These bits are written by software
      --  with the channel number (0..19) assigned as the 5th in the regular
      --  conversion sequence. Note: The software is allowed to write these
      --  bits only when ADSTART = 0 (which ensures that no regular
      --  conversion is ongoing).
      SQ5            : ADC_SQR2_SQ5_Field := 16#0#;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  6th conversion in regular sequence These bits are written by software
      --  with the channel number (0..19) assigned as the 6th in the regular
      --  conversion sequence. Note: The software is allowed to write these
      --  bits only when ADSTART = 0 (which ensures that no regular
      --  conversion is ongoing).
      SQ6            : ADC_SQR2_SQ6_Field := 16#0#;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  7th conversion in regular sequence These bits are written by software
      --  with the channel number (0..19) assigned as the 7th in the regular
      --  conversion sequence. Note: The software is allowed to write these
      --  bits only when ADSTART = 0 (which ensures that no regular
      --  conversion is ongoing).
      SQ7            : ADC_SQR2_SQ7_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : HAL.Bit := 16#0#;
      --  8th conversion in regular sequence These bits are written by software
      --  with the channel number (0..19) assigned as the 8th in the regular
      --  conversion sequence Note: The software is allowed to write these bits
      --  only when ADSTART = 0 (which ensures that no regular conversion is
      --  ongoing).
      SQ8            : ADC_SQR2_SQ8_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : HAL.Bit := 16#0#;
      --  9th conversion in regular sequence These bits are written by software
      --  with the channel number (0..19) assigned as the 9th in the regular
      --  conversion sequence. Note: The software is allowed to write these
      --  bits only when ADSTART = 0 (which ensures that no regular
      --  conversion is ongoing).
      SQ9            : ADC_SQR2_SQ9_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_SQR2_Register use record
      SQ5            at 0 range 0 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      SQ6            at 0 range 6 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      SQ7            at 0 range 12 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      SQ8            at 0 range 18 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      SQ9            at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype ADC_SQR3_SQ10_Field is HAL.UInt5;
   subtype ADC_SQR3_SQ11_Field is HAL.UInt5;
   subtype ADC_SQR3_SQ12_Field is HAL.UInt5;
   subtype ADC_SQR3_SQ13_Field is HAL.UInt5;
   subtype ADC_SQR3_SQ14_Field is HAL.UInt5;

   --  ADC regular sequence register 3
   type ADC_SQR3_Register is record
      --  10th conversion in regular sequence These bits are written by
      --  software with the channel number (0..19) assigned as the 10th in the
      --  regular conversion sequence. Note: The software is allowed to write
      --  these bits only when ADSTART = 0 (which ensures that no regular
      --  conversion is ongoing).
      SQ10           : ADC_SQR3_SQ10_Field := 16#0#;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  11th conversion in regular sequence These bits are written by
      --  software with the channel number (0..19) assigned as the 11th in the
      --  regular conversion sequence. Note: The software is allowed to write
      --  these bits only when ADSTART = 0 (which ensures that no regular
      --  conversion is ongoing).
      SQ11           : ADC_SQR3_SQ11_Field := 16#0#;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  12th conversion in regular sequence These bits are written by
      --  software with the channel number (0..19) assigned as the 12th in the
      --  regular conversion sequence. Note: The software is allowed to write
      --  these bits only when ADSTART = 0 (which ensures that no regular
      --  conversion is ongoing).
      SQ12           : ADC_SQR3_SQ12_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : HAL.Bit := 16#0#;
      --  13th conversion in regular sequence These bits are written by
      --  software with the channel number (0..19) assigned as the 13th in the
      --  regular conversion sequence. Note: The software is allowed to write
      --  these bits only when ADSTART = 0 (which ensures that no regular
      --  conversion is ongoing).
      SQ13           : ADC_SQR3_SQ13_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : HAL.Bit := 16#0#;
      --  14th conversion in regular sequence These bits are written by
      --  software with the channel number (0..19) assigned as the 14th in the
      --  regular conversion sequence. Note: The software is allowed to write
      --  these bits only when ADSTART = 0 (which ensures that no regular
      --  conversion is ongoing).
      SQ14           : ADC_SQR3_SQ14_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_SQR3_Register use record
      SQ10           at 0 range 0 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      SQ11           at 0 range 6 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      SQ12           at 0 range 12 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      SQ13           at 0 range 18 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      SQ14           at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype ADC_SQR4_SQ15_Field is HAL.UInt5;
   subtype ADC_SQR4_SQ16_Field is HAL.UInt5;

   --  ADC regular sequence register 4
   type ADC_SQR4_Register is record
      --  15th conversion in regular sequence These bits are written by
      --  software with the channel number (0..19) assigned as the 15th in the
      --  regular conversion sequence. Note: The software is allowed to write
      --  these bits only when ADSTART = 0 (which ensures that no regular
      --  conversion is ongoing).
      SQ15           : ADC_SQR4_SQ15_Field := 16#0#;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  16th conversion in regular sequence These bits are written by
      --  software with the channel number (0..19) assigned as the 16th in the
      --  regular conversion sequence. Note: The software is allowed to write
      --  these bits only when ADSTART = 0 (which ensures that no regular
      --  conversion is ongoing).
      SQ16           : ADC_SQR4_SQ16_Field := 16#0#;
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_SQR4_Register use record
      SQ15           at 0 range 0 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      SQ16           at 0 range 6 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   subtype ADC_JSQR_JL_Field is HAL.UInt2;
   subtype ADC_JSQR_JEXTSEL_Field is HAL.UInt5;
   subtype ADC_JSQR_JEXTEN_Field is HAL.UInt2;
   subtype ADC_JSQR_JSQ1_Field is HAL.UInt5;
   subtype ADC_JSQR_JSQ2_Field is HAL.UInt5;
   subtype ADC_JSQR_JSQ3_Field is HAL.UInt5;
   subtype ADC_JSQR_JSQ4_Field is HAL.UInt5;

   --  ADC injected sequence register
   type ADC_JSQR_Register is record
      --  Injected channel sequence length These bits are written by software
      --  to define the total number of conversions in the injected channel
      --  conversion sequence. Note: The software is allowed to write these
      --  bits only when JADSTART = 0 (which ensures that no injected
      --  conversion is ongoing.
      JL             : ADC_JSQR_JL_Field := 16#0#;
      --  External trigger selection for injected group These bits select the
      --  external event used to trigger the start of conversion of an injected
      --  group: ... Refer to the ADC external trigger for injected channels in
      --  internal signals for details on trigger mapping. Note: The software
      --  is allowed to write these bits only when JADSTART = 0 (which
      --  ensures that no injected conversion is ongoing.
      JEXTSEL        : ADC_JSQR_JEXTSEL_Field := 16#0#;
      --  External trigger enable and polarity selection for injected channels
      --  These bits are set and cleared by software to select the external
      --  trigger polarity and enable the trigger of an injected group. Note:
      --  The software is allowed to write these bits only when JADSTART = 0
      --  (which ensures that no injected conversion is ongoing.
      JEXTEN         : ADC_JSQR_JEXTEN_Field := 16#0#;
      --  1st conversion in the injected sequence These bits are written by
      --  software with the channel number (0..19) assigned as the 1st in the
      --  injected conversion sequence. Note: The software is allowed to write
      --  these bits only when JADSTART = 0 (which ensures that no injected
      --  conversion is ongoing.
      JSQ1           : ADC_JSQR_JSQ1_Field := 16#0#;
      --  unspecified
      Reserved_14_14 : HAL.Bit := 16#0#;
      --  2nd conversion in the injected sequence These bits are written by
      --  software with the channel number (0..19) assigned as the 2nd in the
      --  injected conversion sequence. Note: The software is allowed to write
      --  these bits only when JADSTART = 0 (which ensures that no injected
      --  conversion is ongoing.
      JSQ2           : ADC_JSQR_JSQ2_Field := 16#0#;
      --  unspecified
      Reserved_20_20 : HAL.Bit := 16#0#;
      --  3rd conversion in the injected sequence These bits are written by
      --  software with the channel number (0..19) assigned as the 3rd in the
      --  injected conversion sequence. Note: The software is allowed to write
      --  these bits only when JADSTART = 0 (which ensures that no injected
      --  conversion is ongoing.
      JSQ3           : ADC_JSQR_JSQ3_Field := 16#0#;
      --  unspecified
      Reserved_26_26 : HAL.Bit := 16#0#;
      --  4th conversion in the injected sequence These bits are written by
      --  software with the channel number (0..19) assigned as the 4th in the
      --  injected conversion sequence. Note: The software is allowed to write
      --  these bits only when JADSTART = 0 (which ensures that no injected
      --  conversion is ongoing.
      JSQ4           : ADC_JSQR_JSQ4_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_JSQR_Register use record
      JL             at 0 range 0 .. 1;
      JEXTSEL        at 0 range 2 .. 6;
      JEXTEN         at 0 range 7 .. 8;
      JSQ1           at 0 range 9 .. 13;
      Reserved_14_14 at 0 range 14 .. 14;
      JSQ2           at 0 range 15 .. 19;
      Reserved_20_20 at 0 range 20 .. 20;
      JSQ3           at 0 range 21 .. 25;
      Reserved_26_26 at 0 range 26 .. 26;
      JSQ4           at 0 range 27 .. 31;
   end record;

   subtype ADC_OFR_OFFSET_Field is HAL.UInt24;
   subtype ADC_OFR_OFFSET_CH_Field is HAL.UInt5;

   --  ADC offset register
   type ADC_OFR_Register is record
      --  Data offset y for the channel programmed into OFFSETy_CH[4:0] bits
      --  These bits are written by software to define the offset y to be
      --  subtracted from the raw converted data when converting a channel
      --  (regular or injected). The channel to which the data offset y applies
      --  must be programmed to the OFFSETy_CH[4:0] bits. The conversion result
      --  can be read from in the ADC_DR (regular conversion) or from in the
      --  ADC_JDRyi registers (injected conversion). When OFFSETy[21:0]
      --  bitfield is reset, the offset compensation is disabled. Note: The
      --  software is allowed to write these bits only when ADSTART = 0 and
      --  JADSTART = 0 (which ensures that no conversion is ongoing). If
      --  several offsets (OFFSETy) point to the same channel, only the offset
      --  with the lowest y value is considered for the subtraction. For
      --  example, if OFFSET1_CH[4:0] = 4 and OFFSET2_CH[4:0] = 4, this is
      --  OFFSET1[25:0] that is subtracted when converting channel 4.
      OFFSET    : ADC_OFR_OFFSET_Field := 16#0#;
      --  offset sign This bit is set and cleared by software to enable the
      --  positive offset. Note: The software is allowed to write these bits
      --  only when ADSTART = 0 and JADSTART = 0 (which ensures that no
      --  conversion is ongoing).
      POSOFF    : Boolean := False;
      --  Unsigned saturation enable This bit is written by software to enable
      --  or disable the unsigned saturation feature. Note: The software is
      --  allowed to write this bit only when ADSTART = 0 and JADSTART = 0
      --  (which ensures that no conversion is ongoing).
      USAT      : Boolean := False;
      --  Signed saturation enable This bit is written by software to enable or
      --  disable the Signed saturation feature. (see OFFSETy_CH, OVSS, LSHIFT,
      --  USAT, SSAT) for details). Note: The software is allowed to write this
      --  bit only when ADSTART = 0 and JADSTART = 0 (which ensures that no
      --  conversion is ongoing).
      SSAT      : Boolean := False;
      --  Channel selection for the data offset y These bits are written by
      --  software to define the channel to which the offset programmed into
      --  OFFSETy[25:0] bits applies. Note: The software is allowed to write
      --  these bits only when ADSTART = 0 and JADSTART = 0 (which ensures
      --  that no conversion is ongoing). If OFFSETy_EN bit is set, it is not
      --  allowed to select the same channel in different ADC_OFRy registers.
      OFFSET_CH : ADC_OFR_OFFSET_CH_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_OFR_Register use record
      OFFSET    at 0 range 0 .. 23;
      POSOFF    at 0 range 24 .. 24;
      USAT      at 0 range 25 .. 25;
      SSAT      at 0 range 26 .. 26;
      OFFSET_CH at 0 range 27 .. 31;
   end record;

   subtype ADC_GCOMP_GCOMPCOEFF_Field is HAL.UInt14;

   --  ADC gain compensation register
   type ADC_GCOMP_Register is record
      --  Gain compensation coefficient These bits are set and cleared by
      --  software to program the gain compensation coefficient. ... ... The
      --  coefficient is divided by 4096 to get the gain factor ranging from 0
      --  to 3.999756. Note: This gain compensation is only applied when GCOMP
      --  bit of ADCx_CFGR2 register is 1.
      GCOMPCOEFF     : ADC_GCOMP_GCOMPCOEFF_Field := 16#0#;
      --  unspecified
      Reserved_14_30 : HAL.UInt17 := 16#0#;
      --  Gain compensation mode This bit is set and cleared by software to
      --  enable the gain compensation mode. Note: The software is allowed to
      --  write this bit only when ADSTART = 0 (which ensures that no
      --  conversion is ongoing).
      GCOMP          : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_GCOMP_Register use record
      GCOMPCOEFF     at 0 range 0 .. 13;
      Reserved_14_30 at 0 range 14 .. 30;
      GCOMP          at 0 range 31 .. 31;
   end record;

   subtype ADC_AWD2CR_AWD2CH_Field is HAL.UInt20;

   --  ADC analog watchdog 2 configuration register	
   type ADC_AWD2CR_Register is record
      --  Analog watchdog 2 channel selection These bits are set and cleared by
      --  software. They enable and select the input channels to be guarded by
      --  the analog watchdog 2. AWD2CH[i] = 0: ADC analog input channel-i is
      --  not monitored by AWD2 AWD2CH[i] = 1: ADC analog input channel-i is
      --  monitored by AWD2 When AWD2CH[19:0] = 000..0, the analog Watchdog 2
      --  is disabled Note: The channels selected by AWD2CH must be also
      --  selected into the SQRi or JSQRi registers. Software is allowed to
      --  write these bits only when ADSTART = 0 and JADSTART = 0 (which
      --  ensures that no conversion is ongoing).
      AWD2CH         : ADC_AWD2CR_AWD2CH_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_AWD2CR_Register use record
      AWD2CH         at 0 range 0 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   subtype ADC_AWD3CR_AWD3CH_Field is HAL.UInt20;

   --  ADC analog watchdog 3 configuration register	
   type ADC_AWD3CR_Register is record
      --  Analog watchdog 3 channel selection These bits are set and cleared by
      --  software. They enable and select the input channels to be guarded by
      --  the analog watchdog 3. AWD3CH[i] = 0: ADC analog input channel-i is
      --  not monitored by AWD3 AWD3CH[i] = 1: ADC analog input channel-i is
      --  monitored by AWD3 When AWD3CH[19:0] = 000..0, the analog Watchdog 3
      --  is disabled Note: The channels selected by AWD3CH must be also
      --  selected into the SQRi or JSQRi registers. The software is allowed to
      --  write these bits only when ADSTART = 0 and JADSTART = 0 (which
      --  ensures that no conversion is ongoing).
      AWD3CH         : ADC_AWD3CR_AWD3CH_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_AWD3CR_Register use record
      AWD3CH         at 0 range 0 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   subtype ADC_LTR1_LTR1_Field is HAL.UInt25;

   --  ADC watchdog threshold register 1
   type ADC_LTR1_Register is record
      --  Analog watchdog 1 lower threshold These bits are written by software
      --  to define the lower threshold for the analog watchdog 1. Refer to
      --  AWD2CH, AWD3CH, AWD_HTRy, AWD_LTRy, AWDy).
      LTR1           : ADC_LTR1_LTR1_Field := 16#0#;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_LTR1_Register use record
      LTR1           at 0 range 0 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype ADC_HTR1_HTR1_Field is HAL.UInt25;
   subtype ADC_HTR1_AWDFILT1_Field is HAL.UInt3;

   --  ADC watchdog threshold register 1
   type ADC_HTR1_Register is record
      --  Analog watchdog 1 higher threshold These bits are written by software
      --  to define the higher threshold for the analog watchdog 1. Refer to
      --  AWD2CH, AWD3CH, AWD_HTRy, AWD_LTRy, AWDy).
      HTR1           : ADC_HTR1_HTR1_Field := 16#1FFFFFF#;
      --  unspecified
      Reserved_25_28 : HAL.UInt4 := 16#0#;
      --  Analog watchdog filtering parameter This bit is set and cleared by
      --  software. ... Note: The software is allowed to write this bit only
      --  when ADSTART = 0 and JADSTART = 0 (which ensures that no
      --  conversion is ongoing).
      AWDFILT1       : ADC_HTR1_AWDFILT1_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_HTR1_Register use record
      HTR1           at 0 range 0 .. 24;
      Reserved_25_28 at 0 range 25 .. 28;
      AWDFILT1       at 0 range 29 .. 31;
   end record;

   subtype ADC_LTR2_LTR2_Field is HAL.UInt25;

   --  ADC watchdog lower threshold register 2
   type ADC_LTR2_Register is record
      --  Analog watchdog 2 lower threshold These bits are written by software
      --  to define the lower threshold for the analog watchdog 2. Refer to
      --  AWD2CH, AWD3CH, AWD_HTRy, AWD_LTRy, AWDy).
      LTR2           : ADC_LTR2_LTR2_Field := 16#0#;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_LTR2_Register use record
      LTR2           at 0 range 0 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype ADC_HTR2_HTR2_Field is HAL.UInt25;

   --  ADC watchdog higher threshold register 2
   type ADC_HTR2_Register is record
      --  Analog watchdog 2 higher threshold These bits are written by software
      --  to define the higher threshold for the analog watchdog 2. Refer to
      --  AWD2CH, AWD3CH, AWD_HTRy, AWD_LTRy, AWDy).
      HTR2           : ADC_HTR2_HTR2_Field := 16#1FFFFFF#;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_HTR2_Register use record
      HTR2           at 0 range 0 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype ADC_LTR3_LTR3_Field is HAL.UInt25;

   --  ADC watchdog lower threshold register 3
   type ADC_LTR3_Register is record
      --  Analog watchdog 3 lower threshold These bits are written by software
      --  to define the lower threshold for the analog watchdog 3. Refer to
      --  AWD2CH, AWD3CH, AWD_HTRy, AWD_LTRy, AWDy).
      LTR3           : ADC_LTR3_LTR3_Field := 16#0#;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_LTR3_Register use record
      LTR3           at 0 range 0 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype ADC_HTR3_HTR3_Field is HAL.UInt25;

   --  ADC watchdog higher threshold register 3
   type ADC_HTR3_Register is record
      --  Analog watchdog 3 higher threshold These bits are written by software
      --  to define the higher threshold for the analog watchdog 3. Refer to
      --  AWD2CH, AWD3CH, AWD_HTRy, AWD_LTRy, AWDy).
      HTR3           : ADC_HTR3_HTR3_Field := 16#1FFFFFF#;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_HTR3_Register use record
      HTR3           at 0 range 0 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype ADC_DIFSEL_DIFSEL_Field is HAL.UInt20;

   --  ADC differential mode selection register
   type ADC_DIFSEL_Register is record
      --  Differential mode for channels 19 to 0 These bits are set and cleared
      --  by software. They allow selecting if a channel is configured as
      --  single ended or differential mode. DIFSEL[i] = 0: ADC analog input
      --  channel-i is configured in single ended mode DIFSEL[i] = 1: ADC
      --  analog input channel-i is configured in differential mode Note: The
      --  software is allowed to write these bits only when the ADC is disabled
      --  (ADCAL = 0, JADSTART = 0, JADSTP = 0, ADSTART = 0,
      --  ADSTP = 0, ADDIS = 0 and ADEN = 0).
      DIFSEL         : ADC_DIFSEL_DIFSEL_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_DIFSEL_Register use record
      DIFSEL         at 0 range 0 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   subtype ADC_CALFACT_I_APB_ADDR_Field is HAL.UInt8;
   subtype ADC_CALFACT_I_APB_DATA_Field is HAL.UInt8;

   --  ADC user control register
   type ADC_CALFACT_Register is record
      --  Read-only. Delayed write access address This bitfield contains the
      --  address that is being written during delayed write accesses.
      I_APB_ADDR     : ADC_CALFACT_I_APB_ADDR_Field := 16#0#;
      --  Read-only. Delayed write access data This bitfield contains the data
      --  that are being written during delayed write accesses.
      I_APB_DATA     : ADC_CALFACT_I_APB_DATA_Field := 16#0#;
      --  Read-only. Delayed write access status bit This bit indicates the
      --  communication status between the ADC digital and analog blocks.
      VALIDITY       : Boolean := False;
      --  unspecified
      Reserved_17_23 : HAL.UInt7 := 16#0#;
      --  Calibration factor latch enable bit This bit latches the calibration
      --  factor in the CALFACT[31:0] bits.
      LATCH_COEF     : Boolean := False;
      --  Calibration factor capture enable bit This bit enables the internal
      --  calibration factor capture.
      CAPTURE_COEF   : Boolean := False;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_CALFACT_Register use record
      I_APB_ADDR     at 0 range 0 .. 7;
      I_APB_DATA     at 0 range 8 .. 15;
      VALIDITY       at 0 range 16 .. 16;
      Reserved_17_23 at 0 range 17 .. 23;
      LATCH_COEF     at 0 range 24 .. 24;
      CAPTURE_COEF   at 0 range 25 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   --  ADC interrupt and status register
   type ADC_ISR_Register_1 is record
      --  ADRDY
      ADRDY          : Boolean := False;
      --  EOSMP
      EOSMP          : Boolean := False;
      --  EOC
      EOC            : Boolean := False;
      --  EOS
      EOS            : Boolean := False;
      --  OVR
      OVR            : Boolean := False;
      --  unspecified
      Reserved_5_6   : HAL.UInt2 := 16#0#;
      --  AWD1
      AWD            : ADC_ISR_AWD_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  EOCAL
      EOCAL          : Boolean := False;
      --  LDORDY
      LDORDY         : Boolean := False;
      --  unspecified
      Reserved_13_31 : HAL.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_ISR_Register_1 use record
      ADRDY          at 0 range 0 .. 0;
      EOSMP          at 0 range 1 .. 1;
      EOC            at 0 range 2 .. 2;
      EOS            at 0 range 3 .. 3;
      OVR            at 0 range 4 .. 4;
      Reserved_5_6   at 0 range 5 .. 6;
      AWD            at 0 range 7 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      EOCAL          at 0 range 11 .. 11;
      LDORDY         at 0 range 12 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   --  ADC interrupt enable register
   type ADC_IER_Register_1 is record
      --  ADRDYIE
      ADRDYIE        : Boolean := False;
      --  EOSMPIE
      EOSMPIE        : Boolean := False;
      --  EOCIE
      EOCIE          : Boolean := False;
      --  EOSIE
      EOSIE          : Boolean := False;
      --  OVRIE
      OVRIE          : Boolean := False;
      --  unspecified
      Reserved_5_6   : HAL.UInt2 := 16#0#;
      --  AWD1IE
      AWD1IE         : Boolean := False;
      --  AWD2IE
      AWD2IE         : Boolean := False;
      --  AWD3IE
      AWD3IE         : Boolean := False;
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  EOCALIE
      EOCALIE        : Boolean := False;
      --  LDORDYIE
      LDORDYIE       : Boolean := False;
      --  unspecified
      Reserved_13_31 : HAL.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_IER_Register_1 use record
      ADRDYIE        at 0 range 0 .. 0;
      EOSMPIE        at 0 range 1 .. 1;
      EOCIE          at 0 range 2 .. 2;
      EOSIE          at 0 range 3 .. 3;
      OVRIE          at 0 range 4 .. 4;
      Reserved_5_6   at 0 range 5 .. 6;
      AWD1IE         at 0 range 7 .. 7;
      AWD2IE         at 0 range 8 .. 8;
      AWD3IE         at 0 range 9 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      EOCALIE        at 0 range 11 .. 11;
      LDORDYIE       at 0 range 12 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   --  ADC control register
   type ADC_CR_Register_1 is record
      --  Read-only. ADEN
      ADEN           : Boolean := False;
      --  Read-only. ADDIS
      ADDIS          : Boolean := False;
      --  Read-only. ADSTART
      ADSTART        : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Read-only. ADSTP
      ADSTP          : Boolean := False;
      --  unspecified
      Reserved_5_27  : HAL.UInt23 := 16#0#;
      --  ADVREGEN
      ADVREGEN       : Boolean := False;
      --  unspecified
      Reserved_29_30 : HAL.UInt2 := 16#0#;
      --  Read-only. ADCAL
      ADCAL          : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_CR_Register_1 use record
      ADEN           at 0 range 0 .. 0;
      ADDIS          at 0 range 1 .. 1;
      ADSTART        at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      ADSTP          at 0 range 4 .. 4;
      Reserved_5_27  at 0 range 5 .. 27;
      ADVREGEN       at 0 range 28 .. 28;
      Reserved_29_30 at 0 range 29 .. 30;
      ADCAL          at 0 range 31 .. 31;
   end record;

   subtype ADC_CFGR1_EXTSEL_Field_1 is HAL.UInt3;

   --  ADC configuration register
   type ADC_CFGR1_Register_1 is record
      --  DMAEN
      DMAEN          : Boolean := False;
      --  DMACFG
      DMACFG         : Boolean := False;
      --  RES
      RES            : ADC_CFGR1_RES_Field := 16#0#;
      --  SCANDIR
      SCANDIR        : Boolean := False;
      --  ALIGN
      ALIGN          : Boolean := False;
      --  EXTSEL
      EXTSEL         : ADC_CFGR1_EXTSEL_Field_1 := 16#0#;
      --  unspecified
      Reserved_9_9   : HAL.Bit := 16#0#;
      --  EXTEN
      EXTEN          : ADC_CFGR1_EXTEN_Field := 16#0#;
      --  OVRMOD
      OVRMOD         : Boolean := False;
      --  CONT
      CONT           : Boolean := False;
      --  WAIT
      WAIT           : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  DISCEN
      DISCEN         : Boolean := False;
      --  unspecified
      Reserved_17_20 : HAL.UInt4 := 16#0#;
      --  CHSELRMOD
      CHSELRMOD      : Boolean := False;
      --  AWD1SGL
      AWD1SGL        : Boolean := False;
      --  AWD1EN
      AWD1EN         : Boolean := False;
      --  unspecified
      Reserved_24_25 : HAL.UInt2 := 16#0#;
      --  AWD1CH
      AWD1CH         : ADC_CFGR1_AWD1CH_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_CFGR1_Register_1 use record
      DMAEN          at 0 range 0 .. 0;
      DMACFG         at 0 range 1 .. 1;
      RES            at 0 range 2 .. 3;
      SCANDIR        at 0 range 4 .. 4;
      ALIGN          at 0 range 5 .. 5;
      EXTSEL         at 0 range 6 .. 8;
      Reserved_9_9   at 0 range 9 .. 9;
      EXTEN          at 0 range 10 .. 11;
      OVRMOD         at 0 range 12 .. 12;
      CONT           at 0 range 13 .. 13;
      WAIT           at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      DISCEN         at 0 range 16 .. 16;
      Reserved_17_20 at 0 range 17 .. 20;
      CHSELRMOD      at 0 range 21 .. 21;
      AWD1SGL        at 0 range 22 .. 22;
      AWD1EN         at 0 range 23 .. 23;
      Reserved_24_25 at 0 range 24 .. 25;
      AWD1CH         at 0 range 26 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype ADC_CFGR2_OVSR_Field is HAL.UInt3;

   --  ADC configuration register 2
   type ADC_CFGR2_Register_1 is record
      --  OVSE
      OVSE           : Boolean := False;
      --  unspecified
      Reserved_1_1   : HAL.Bit := 16#0#;
      --  OVSR
      OVSR           : ADC_CFGR2_OVSR_Field := 16#0#;
      --  OVSS
      OVSS           : ADC_CFGR2_OVSS_Field := 16#0#;
      --  TOVS
      TOVS           : Boolean := False;
      --  unspecified
      Reserved_10_28 : HAL.UInt19 := 16#0#;
      --  LFTRIG
      LFTRIG         : Boolean := False;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_CFGR2_Register_1 use record
      OVSE           at 0 range 0 .. 0;
      Reserved_1_1   at 0 range 1 .. 1;
      OVSR           at 0 range 2 .. 4;
      OVSS           at 0 range 5 .. 8;
      TOVS           at 0 range 9 .. 9;
      Reserved_10_28 at 0 range 10 .. 28;
      LFTRIG         at 0 range 29 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype ADC_SMPR_SMP1_Field is HAL.UInt3;
   subtype ADC_SMPR_SMP2_Field is HAL.UInt3;

   --  ADC_SMPR_SMPSEL array
   type ADC_SMPR_SMPSEL_Field_Array is array (0 .. 23) of Boolean
     with Component_Size => 1, Size => 24;

   --  Type definition for ADC_SMPR_SMPSEL
   type ADC_SMPR_SMPSEL_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SMPSEL as a value
            Val : HAL.UInt24;
         when True =>
            --  SMPSEL as an array
            Arr : ADC_SMPR_SMPSEL_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 24;

   for ADC_SMPR_SMPSEL_Field use record
      Val at 0 range 0 .. 23;
      Arr at 0 range 0 .. 23;
   end record;

   --  ADC sample time register
   type ADC_SMPR_Register is record
      --  SMP1
      SMP1         : ADC_SMPR_SMP1_Field := 16#0#;
      --  unspecified
      Reserved_3_3 : HAL.Bit := 16#0#;
      --  SMP2
      SMP2         : ADC_SMPR_SMP2_Field := 16#0#;
      --  unspecified
      Reserved_7_7 : HAL.Bit := 16#0#;
      --  SMPSEL0
      SMPSEL       : ADC_SMPR_SMPSEL_Field :=
                      (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_SMPR_Register use record
      SMP1         at 0 range 0 .. 2;
      Reserved_3_3 at 0 range 3 .. 3;
      SMP2         at 0 range 4 .. 6;
      Reserved_7_7 at 0 range 7 .. 7;
      SMPSEL       at 0 range 8 .. 31;
   end record;

   subtype ADC_AWD1TR_LT1_Field is HAL.UInt12;
   subtype ADC_AWD1TR_HT1_Field is HAL.UInt12;

   --  ADC watchdog threshold register
   type ADC_AWD1TR_Register is record
      --  LT1
      LT1            : ADC_AWD1TR_LT1_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  HT1
      HT1            : ADC_AWD1TR_HT1_Field := 16#FFF#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_AWD1TR_Register use record
      LT1            at 0 range 0 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      HT1            at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype ADC_AWD2TR_LT2_Field is HAL.UInt12;
   subtype ADC_AWD2TR_HT2_Field is HAL.UInt12;

   --  ADC watchdog threshold register
   type ADC_AWD2TR_Register is record
      --  LT2
      LT2            : ADC_AWD2TR_LT2_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  HT2
      HT2            : ADC_AWD2TR_HT2_Field := 16#FFF#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_AWD2TR_Register use record
      LT2            at 0 range 0 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      HT2            at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype ADC_CHSELRMOD0_CHSEL_Field is HAL.UInt24;

   --  ADC channel selection register [alternate]
   type ADC_CHSELRMOD0_Register is record
      --  CHSEL
      CHSEL          : ADC_CHSELRMOD0_CHSEL_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_CHSELRMOD0_Register use record
      CHSEL          at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  ADC_CHSELRMOD1_SQ array element
   subtype ADC_CHSELRMOD1_SQ_Element is HAL.UInt4;

   --  ADC_CHSELRMOD1_SQ array
   type ADC_CHSELRMOD1_SQ_Field_Array is array (1 .. 8)
     of ADC_CHSELRMOD1_SQ_Element
     with Component_Size => 4, Size => 32;

   --  ADC channel selection register [alternate]
   type ADC_CHSELRMOD1_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SQ as a value
            Val : HAL.UInt32;
         when True =>
            --  SQ as an array
            Arr : ADC_CHSELRMOD1_SQ_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_CHSELRMOD1_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   subtype ADC_AWD3TR_LT3_Field is HAL.UInt12;
   subtype ADC_AWD3TR_HT3_Field is HAL.UInt12;

   --  ADC watchdog threshold register
   type ADC_AWD3TR_Register is record
      --  LT3
      LT3            : ADC_AWD3TR_LT3_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  HT3
      HT3            : ADC_AWD3TR_HT3_Field := 16#FFF#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_AWD3TR_Register use record
      LT3            at 0 range 0 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      HT3            at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype ADC_DR_DATA_Field is HAL.UInt16;

   --  ADC data register
   type ADC_DR_Register is record
      --  Read-only. DATA
      DATA           : ADC_DR_DATA_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_DR_Register use record
      DATA           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  ADC data register
   type ADC_PWR_Register is record
      --  AUTOFF
      AUTOFF        : Boolean := False;
      --  DPD
      DPD           : Boolean := False;
      --  VREFPROT
      VREFPROT      : Boolean := False;
      --  VREFSECSMP
      VREFSECSMP    : Boolean := False;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_PWR_Register use record
      AUTOFF        at 0 range 0 .. 0;
      DPD           at 0 range 1 .. 1;
      VREFPROT      at 0 range 2 .. 2;
      VREFSECSMP    at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  ADC_AWD2CR_AWD2CH array
   type ADC_AWD2CR_AWD2CH_Field_Array is array (0 .. 23) of Boolean
     with Component_Size => 1, Size => 24;

   --  Type definition for ADC_AWD2CR_AWD2CH
   type ADC_AWD2CR_AWD2CH_Field_1
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AWD2CH as a value
            Val : HAL.UInt24;
         when True =>
            --  AWD2CH as an array
            Arr : ADC_AWD2CR_AWD2CH_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 24;

   for ADC_AWD2CR_AWD2CH_Field_1 use record
      Val at 0 range 0 .. 23;
      Arr at 0 range 0 .. 23;
   end record;

   --  ADC Analog Watchdog 2 Configuration register
   type ADC_AWD2CR_Register_1 is record
      --  AWD2CH0
      AWD2CH         : ADC_AWD2CR_AWD2CH_Field_1 :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_AWD2CR_Register_1 use record
      AWD2CH         at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  ADC_AWD3CR_AWD3CH array
   type ADC_AWD3CR_AWD3CH_Field_Array is array (0 .. 23) of Boolean
     with Component_Size => 1, Size => 24;

   --  Type definition for ADC_AWD3CR_AWD3CH
   type ADC_AWD3CR_AWD3CH_Field_1
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AWD3CH as a value
            Val : HAL.UInt24;
         when True =>
            --  AWD3CH as an array
            Arr : ADC_AWD3CR_AWD3CH_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 24;

   for ADC_AWD3CR_AWD3CH_Field_1 use record
      Val at 0 range 0 .. 23;
      Arr at 0 range 0 .. 23;
   end record;

   --  ADC Analog Watchdog 3 Configuration register
   type ADC_AWD3CR_Register_1 is record
      --  AWD3CH0
      AWD3CH         : ADC_AWD3CR_AWD3CH_Field_1 :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_AWD3CR_Register_1 use record
      AWD3CH         at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype ADC_CALFACT_CALFACT_Field is HAL.UInt7;

   --  ADC Calibration factor
   type ADC_CALFACT_Register_1 is record
      --  CALFACT
      CALFACT       : ADC_CALFACT_CALFACT_Field := 16#0#;
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_CALFACT_Register_1 use record
      CALFACT       at 0 range 0 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   --  ADC option register
   type ADC_OR_Register is record
      --  CHN21SEL
      CHN21SEL      : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_OR_Register use record
      CHN21SEL      at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   subtype ADC_CCR_PRESC_Field is HAL.UInt4;

   --  ADC common configuration register
   type ADC_CCR_Register is record
      --  unspecified
      Reserved_0_17  : HAL.UInt18 := 16#0#;
      --  PRESC
      PRESC          : ADC_CCR_PRESC_Field := 16#0#;
      --  VREFEN
      VREFEN         : Boolean := False;
      --  VSENSESEL
      VSENSESEL      : Boolean := False;
      --  VBATEN
      VBATEN         : Boolean := False;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_CCR_Register use record
      Reserved_0_17  at 0 range 0 .. 17;
      PRESC          at 0 range 18 .. 21;
      VREFEN         at 0 range 22 .. 22;
      VSENSESEL      at 0 range 23 .. 23;
      VBATEN         at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype ADC12_CCR_PRESC_Field is HAL.UInt4;

   --  ADC_CCR system control register
   type ADC12_CCR_Register is record
      --  unspecified
      Reserved_0_17  : HAL.UInt18 := 16#0#;
      --  ADC prescaler These bits are set and cleared by software to select
      --  the frequency of the ADC clock. The clock is common to all ADCs.
      --  Others: Reserved, must not be used Note: The software is allowed to
      --  write this bit only when the ADCs are disabled (ADCAL = 0,
      --  JADSTART = 0, ADSTART = 0, ADSTP = 0, ADDIS = 0 and
      --  ADEN = 0).
      PRESC          : ADC12_CCR_PRESC_Field := 16#0#;
      --  VREFINT enable This bit is set and cleared by software to
      --  enable/disable the VREFINT buffer. Note: The software is allowed to
      --  write this bit only when the ADCs are disabled (ADCAL = 0,
      --  JADSTART = 0, ADSTART = 0, ADSTP = 0, ADDIS = 0 and
      --  ADEN = 0).
      VREFEN         : Boolean := False;
      --  Temperature sensor voltage selection This bit is set and cleared by
      --  software to control the temperature sensor channel. Note: The
      --  software is allowed to write this bit only when the ADCs are disabled
      --  (ADCAL = 0, JADSTART = 0, ADSTART = 0, ADSTP = 0, ADDIS = 0
      --  and ADEN = 0).
      VSENSESEL      : Boolean := False;
      --  VBAT enable This bit is set and cleared by software to control the
      --  VBAT channel. Note: The software is allowed to write this bit only
      --  when the ADCs are disabled (ADCAL = 0, JADSTART = 0,
      --  ADSTART = 0, ADSTP = 0, ADDIS = 0 and ADEN = 0).
      VBATEN         : Boolean := False;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC12_CCR_Register use record
      Reserved_0_17  at 0 range 0 .. 17;
      PRESC          at 0 range 18 .. 21;
      VREFEN         at 0 range 22 .. 22;
      VSENSESEL      at 0 range 23 .. 23;
      VBATEN         at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  ADC1
   type ADC1_Peripheral is record
      --  ADC interrupt and status register
      ADC_ISR      : aliased ADC_ISR_Register;
      --  ADC interrupt enable register
      ADC_IER      : aliased ADC_IER_Register;
      --  ADC control register
      ADC_CR       : aliased ADC_CR_Register;
      --  ADC configuration register
      ADC_CFGR1    : aliased ADC_CFGR1_Register;
      --  ADC configuration register 2
      ADC_CFGR2    : aliased ADC_CFGR2_Register;
      --  ADC sample time register 1
      ADC_SMPR1    : aliased ADC_SMPR1_Register;
      --  ADC sample time register 2
      ADC_SMPR2    : aliased ADC_SMPR2_Register;
      --  ADC channel preselection register
      ADC_PCSEL    : aliased ADC_PCSEL_Register;
      --  ADC regular sequence register 1
      ADC_SQR1     : aliased ADC_SQR1_Register;
      --  ADC regular sequence register 2
      ADC_SQR2     : aliased ADC_SQR2_Register;
      --  ADC regular sequence register 3
      ADC_SQR3     : aliased ADC_SQR3_Register;
      --  ADC regular sequence register 4
      ADC_SQR4     : aliased ADC_SQR4_Register;
      --  ADC regular Data Register
      ADC_DR       : aliased HAL.UInt32;
      --  ADC injected sequence register
      ADC_JSQR     : aliased ADC_JSQR_Register;
      --  ADC offset register
      ADC_OFR1     : aliased ADC_OFR_Register;
      --  ADC offset register
      ADC_OFR2     : aliased ADC_OFR_Register;
      --  ADC offset register
      ADC_OFR3     : aliased ADC_OFR_Register;
      --  ADC offset register
      ADC_OFR4     : aliased ADC_OFR_Register;
      --  ADC gain compensation register
      ADC_GCOMP    : aliased ADC_GCOMP_Register;
      --  ADC injected data register
      ADC_JDR1     : aliased HAL.UInt32;
      --  ADC injected data register
      ADC_JDR2     : aliased HAL.UInt32;
      --  ADC injected data register
      ADC_JDR3     : aliased HAL.UInt32;
      --  ADC injected data register
      ADC_JDR4     : aliased HAL.UInt32;
      --  ADC analog watchdog 2 configuration register	
      ADC_AWD2CR   : aliased ADC_AWD2CR_Register;
      --  ADC analog watchdog 3 configuration register	
      ADC_AWD3CR   : aliased ADC_AWD3CR_Register;
      --  ADC watchdog threshold register 1
      ADC_LTR1     : aliased ADC_LTR1_Register;
      --  ADC watchdog threshold register 1
      ADC_HTR1     : aliased ADC_HTR1_Register;
      --  ADC watchdog lower threshold register 2
      ADC_LTR2     : aliased ADC_LTR2_Register;
      --  ADC watchdog higher threshold register 2
      ADC_HTR2     : aliased ADC_HTR2_Register;
      --  ADC watchdog lower threshold register 3
      ADC_LTR3     : aliased ADC_LTR3_Register;
      --  ADC watchdog higher threshold register 3
      ADC_HTR3     : aliased ADC_HTR3_Register;
      --  ADC differential mode selection register
      ADC_DIFSEL   : aliased ADC_DIFSEL_Register;
      --  ADC user control register
      ADC_CALFACT  : aliased ADC_CALFACT_Register;
      --  ADC calibration factor register
      ADC_CALFACT2 : aliased HAL.UInt32;
   end record
     with Volatile;

   for ADC1_Peripheral use record
      ADC_ISR      at 16#0# range 0 .. 31;
      ADC_IER      at 16#4# range 0 .. 31;
      ADC_CR       at 16#8# range 0 .. 31;
      ADC_CFGR1    at 16#C# range 0 .. 31;
      ADC_CFGR2    at 16#10# range 0 .. 31;
      ADC_SMPR1    at 16#14# range 0 .. 31;
      ADC_SMPR2    at 16#18# range 0 .. 31;
      ADC_PCSEL    at 16#1C# range 0 .. 31;
      ADC_SQR1     at 16#30# range 0 .. 31;
      ADC_SQR2     at 16#34# range 0 .. 31;
      ADC_SQR3     at 16#38# range 0 .. 31;
      ADC_SQR4     at 16#3C# range 0 .. 31;
      ADC_DR       at 16#40# range 0 .. 31;
      ADC_JSQR     at 16#4C# range 0 .. 31;
      ADC_OFR1     at 16#60# range 0 .. 31;
      ADC_OFR2     at 16#64# range 0 .. 31;
      ADC_OFR3     at 16#68# range 0 .. 31;
      ADC_OFR4     at 16#6C# range 0 .. 31;
      ADC_GCOMP    at 16#70# range 0 .. 31;
      ADC_JDR1     at 16#80# range 0 .. 31;
      ADC_JDR2     at 16#84# range 0 .. 31;
      ADC_JDR3     at 16#88# range 0 .. 31;
      ADC_JDR4     at 16#8C# range 0 .. 31;
      ADC_AWD2CR   at 16#A0# range 0 .. 31;
      ADC_AWD3CR   at 16#A4# range 0 .. 31;
      ADC_LTR1     at 16#A8# range 0 .. 31;
      ADC_HTR1     at 16#AC# range 0 .. 31;
      ADC_LTR2     at 16#B0# range 0 .. 31;
      ADC_HTR2     at 16#B4# range 0 .. 31;
      ADC_LTR3     at 16#B8# range 0 .. 31;
      ADC_HTR3     at 16#BC# range 0 .. 31;
      ADC_DIFSEL   at 16#C0# range 0 .. 31;
      ADC_CALFACT  at 16#C4# range 0 .. 31;
      ADC_CALFACT2 at 16#C8# range 0 .. 31;
   end record;

   --  ADC1
   ADC1_Periph : aliased ADC1_Peripheral
     with Import, Address => ADC1_Base;

   --  ADC1
   SEC_ADC1_Periph : aliased ADC1_Peripheral
     with Import, Address => SEC_ADC1_Base;

   type ADC4_Disc is
     (Val_0,
      Val_1);

   --  ADC4
   type ADC4_Peripheral
     (Discriminent : ADC4_Disc := Val_0)
   is record
      --  ADC interrupt and status register
      ADC_ISR        : aliased ADC_ISR_Register_1;
      --  ADC interrupt enable register
      ADC_IER        : aliased ADC_IER_Register_1;
      --  ADC control register
      ADC_CR         : aliased ADC_CR_Register_1;
      --  ADC configuration register
      ADC_CFGR1      : aliased ADC_CFGR1_Register_1;
      --  ADC configuration register 2
      ADC_CFGR2      : aliased ADC_CFGR2_Register_1;
      --  ADC sample time register
      ADC_SMPR       : aliased ADC_SMPR_Register;
      --  ADC watchdog threshold register
      ADC_AWD1TR     : aliased ADC_AWD1TR_Register;
      --  ADC watchdog threshold register
      ADC_AWD2TR     : aliased ADC_AWD2TR_Register;
      --  ADC watchdog threshold register
      ADC_AWD3TR     : aliased ADC_AWD3TR_Register;
      --  ADC data register
      ADC_DR         : aliased ADC_DR_Register;
      --  ADC data register
      ADC_PWR        : aliased ADC_PWR_Register;
      --  ADC Analog Watchdog 2 Configuration register
      ADC_AWD2CR     : aliased ADC_AWD2CR_Register_1;
      --  ADC Analog Watchdog 3 Configuration register
      ADC_AWD3CR     : aliased ADC_AWD3CR_Register_1;
      --  ADC Calibration factor
      ADC_CALFACT    : aliased ADC_CALFACT_Register_1;
      --  ADC option register
      ADC_OR         : aliased ADC_OR_Register;
      --  ADC common configuration register
      ADC_CCR        : aliased ADC_CCR_Register;
      case Discriminent is
         when Val_0 =>
            --  ADC channel selection register [alternate]
            ADC_CHSELRMOD0 : aliased ADC_CHSELRMOD0_Register;
         when Val_1 =>
            --  ADC channel selection register [alternate]
            ADC_CHSELRMOD1 : aliased ADC_CHSELRMOD1_Register;
      end case;
   end record
     with Unchecked_Union, Volatile;

   for ADC4_Peripheral use record
      ADC_ISR        at 16#0# range 0 .. 31;
      ADC_IER        at 16#4# range 0 .. 31;
      ADC_CR         at 16#8# range 0 .. 31;
      ADC_CFGR1      at 16#C# range 0 .. 31;
      ADC_CFGR2      at 16#10# range 0 .. 31;
      ADC_SMPR       at 16#14# range 0 .. 31;
      ADC_AWD1TR     at 16#20# range 0 .. 31;
      ADC_AWD2TR     at 16#24# range 0 .. 31;
      ADC_AWD3TR     at 16#2C# range 0 .. 31;
      ADC_DR         at 16#40# range 0 .. 31;
      ADC_PWR        at 16#44# range 0 .. 31;
      ADC_AWD2CR     at 16#A0# range 0 .. 31;
      ADC_AWD3CR     at 16#A4# range 0 .. 31;
      ADC_CALFACT    at 16#C4# range 0 .. 31;
      ADC_OR         at 16#D0# range 0 .. 31;
      ADC_CCR        at 16#308# range 0 .. 31;
      ADC_CHSELRMOD0 at 16#28# range 0 .. 31;
      ADC_CHSELRMOD1 at 16#28# range 0 .. 31;
   end record;

   --  ADC4
   ADC4_Periph : aliased ADC4_Peripheral
     with Import, Address => ADC4_Base;

   --  ADC4
   SEC_ADC4_Periph : aliased ADC4_Peripheral
     with Import, Address => SEC_ADC4_Base;

   --  ADC common registers
   type ADC12_Peripheral is record
      --  ADC_CCR system control register
      ADC12_CCR : aliased ADC12_CCR_Register;
   end record
     with Volatile;

   for ADC12_Peripheral use record
      ADC12_CCR at 16#8# range 0 .. 31;
   end record;

   --  ADC common registers
   ADC12_Periph : aliased ADC12_Peripheral
     with Import, Address => ADC12_Base;

   --  ADC common registers
   SEC_ADC12_Periph : aliased ADC12_Peripheral
     with Import, Address => SEC_ADC12_Base;

end STM32_SVD.ADC;
