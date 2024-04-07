--  This spec has been automatically generated from STM32U073.svd

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
      --  (ADEN+1) and when the ADC reaches a state where it is ready to accept
      --  conversion requests. It is cleared by software writing 1 to it.
      ADRDY          : Boolean := False;
      --  End of sampling flag This bit is set by hardware during the
      --  conversion, at the end of the sampling phase.It is cleared by
      --  software by programming it to 1.
      EOSMP          : Boolean := False;
      --  End of conversion flag This bit is set by hardware at the end of each
      --  conversion of a channel when a new data result is available in the
      --  ADC_DR register. It is cleared by software writing 1 to it or by
      --  reading the ADC_DR register.
      EOC            : Boolean := False;
      --  End of sequence flag This bit is set by hardware at the end of the
      --  conversion of a sequence of channels selected by the CHSEL bits. It
      --  is cleared by software writing 1 to it.
      EOS            : Boolean := False;
      --  ADC overrun This bit is set by hardware when an overrun occurs,
      --  meaning that a new conversion has complete while the EOC flag was
      --  already set. It is cleared by software writing 1 to it.
      OVR            : Boolean := False;
      --  unspecified
      Reserved_5_6   : HAL.UInt2 := 16#0#;
      --  Analog watchdog 1 flag This bit is set by hardware when the converted
      --  voltage crosses the values programmed in ADC_TR1 and ADC_HR1
      --  registers. It is cleared by software by programming it to 1.
      AWD            : ADC_ISR_AWD_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  End Of Calibration flag This bit is set by hardware when calibration
      --  is complete. It is cleared by software writing 1 to it.
      EOCAL          : Boolean := False;
      --  unspecified
      Reserved_12_12 : HAL.Bit := 16#0#;
      --  Channel Configuration Ready flag This flag bit is set by hardware
      --  when the channel configuration is applied after programming to
      --  ADC_CHSELR register or changing CHSELRMOD or SCANDIR. It is cleared
      --  by software by programming it to it. Note: When the software
      --  configures the channels (by programming ADC_CHSELR or changing
      --  CHSELRMOD or SCANDIR), it must wait until the CCRDY flag rises before
      --  configuring again or starting conversions, otherwise the new
      --  configuration (or the START bit) is ignored. Once the flag is
      --  asserted, if the software needs to configure again the channels, it
      --  must clear the CCRDY flag before proceeding with a new configuration.
      CCRDY          : Boolean := False;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_ISR_Register use record
      ADRDY          at 0 range 0 .. 0;
      EOSMP          at 0 range 1 .. 1;
      EOC            at 0 range 2 .. 2;
      EOS            at 0 range 3 .. 3;
      OVR            at 0 range 4 .. 4;
      Reserved_5_6   at 0 range 5 .. 6;
      AWD            at 0 range 7 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      EOCAL          at 0 range 11 .. 11;
      Reserved_12_12 at 0 range 12 .. 12;
      CCRDY          at 0 range 13 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   --  ADC interrupt enable register
   type ADC_IER_Register is record
      --  ADC ready interrupt enable This bit is set and cleared by software to
      --  enable/disable the ADC Ready interrupt. Note: The software is allowed
      --  to write this bit only when ADSTART bit is cleared (this ensures that
      --  no conversion is ongoing).
      ADRDYIE        : Boolean := False;
      --  End of sampling flag interrupt enable This bit is set and cleared by
      --  software to enable/disable the end of the sampling phase interrupt.
      --  Note: The software is allowed to write this bit only when ADSTART bit
      --  is cleared (this ensures that no conversion is ongoing).
      EOSMPIE        : Boolean := False;
      --  End of conversion interrupt enable This bit is set and cleared by
      --  software to enable/disable the end of conversion interrupt. Note: The
      --  software is allowed to write this bit only when ADSTART bit is
      --  cleared (this ensures that no conversion is ongoing).
      EOCIE          : Boolean := False;
      --  End of conversion sequence interrupt enable This bit is set and
      --  cleared by software to enable/disable the end of sequence of
      --  conversions interrupt. Note: The software is allowed to write this
      --  bit only when ADSTART bit is cleared (this ensures that no conversion
      --  is ongoing).
      EOSIE          : Boolean := False;
      --  Overrun interrupt enable This bit is set and cleared by software to
      --  enable/disable the overrun interrupt. Note: The software is allowed
      --  to write this bit only when ADSTART bit is cleared (this ensures that
      --  no conversion is ongoing).
      OVRIE          : Boolean := False;
      --  unspecified
      Reserved_5_6   : HAL.UInt2 := 16#0#;
      --  Analog watchdog 1 interrupt enable This bit is set and cleared by
      --  software to enable/disable the analog watchdog interrupt. Note: The
      --  Software is allowed to write this bit only when ADSTART bit is
      --  cleared (this ensures that no conversion is ongoing).
      AWD1IE         : Boolean := False;
      --  Analog watchdog 2 interrupt enable This bit is set and cleared by
      --  software to enable/disable the analog watchdog interrupt. Note: The
      --  Software is allowed to write this bit only when ADSTART bit is
      --  cleared (this ensures that no conversion is ongoing).
      AWD2IE         : Boolean := False;
      --  Analog watchdog 3 interrupt enable This bit is set and cleared by
      --  software to enable/disable the analog watchdog interrupt. Note: The
      --  Software is allowed to write this bit only when ADSTART bit is
      --  cleared (this ensures that no conversion is ongoing).
      AWD3IE         : Boolean := False;
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  End of calibration interrupt enable This bit is set and cleared by
      --  software to enable/disable the end of calibration interrupt. Note:
      --  The software is allowed to write this bit only when ADSTART bit is
      --  cleared (this ensures that no conversion is ongoing).
      EOCALIE        : Boolean := False;
      --  unspecified
      Reserved_12_12 : HAL.Bit := 16#0#;
      --  Channel Configuration Ready Interrupt enable This bit is set and
      --  cleared by software to enable/disable the channel configuration ready
      --  interrupt. Note: The software is allowed to write this bit only when
      --  ADSTART bit is cleared (this ensures that no conversion is ongoing).
      CCRDYIE        : Boolean := False;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_IER_Register use record
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
      Reserved_12_12 at 0 range 12 .. 12;
      CCRDYIE        at 0 range 13 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   --  ADC control register
   type ADC_CR_Register is record
      --  ADC enable command This bit is set by software to enable the ADC. The
      --  ADC is effectively ready to operate once the ADRDY flag has been set.
      --  It is cleared by hardware when the ADC is disabled, after the
      --  execution of the ADDIS command.
      ADEN           : Boolean := False;
      --  ADC disable command
      ADDIS          : Boolean := False;
      --  ADC start conversion command
      ADSTART        : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  ADC stop conversion command
      ADSTP          : Boolean := False;
      --  unspecified
      Reserved_5_27  : HAL.UInt23 := 16#0#;
      --  ADC Voltage Regulator Enable
      ADVREGEN       : Boolean := False;
      --  unspecified
      Reserved_29_30 : HAL.UInt2 := 16#0#;
      --  ADC calibration This bit is set by software to start the calibration
      --  of the ADC.
      ADCAL          : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_CR_Register use record
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

   --  Direct memory access configuration This bit is set and cleared by
   --  software to select between two DMA modes of operation and is effective
   --  only when DMAEN1=11. For more details, refer to Section113.6.5: Managing
   --  converted data using the DMA on page1333.
   type ADC_CFGR1_DMACFG_Field is
     (--  DMA one shot mode selected
      B_0X0,
      --  DMA circular mode selected
      B_0X1)
     with Size => 1;
   for ADC_CFGR1_DMACFG_Field use
     (B_0X0 => 0,
      B_0X1 => 1);

   subtype ADC_CFGR1_RES_Field is HAL.UInt2;
   subtype ADC_CFGR1_EXTSEL_Field is HAL.UInt3;
   subtype ADC_CFGR1_EXTEN_Field is HAL.UInt2;
   subtype ADC_CFGR1_AWD1CH_Field is HAL.UInt5;

   --  ADC configuration register 1
   type ADC_CFGR1_Register is record
      --  Direct memory access enable This bit is set and cleared by software
      --  to enable the generation of DMA requests. This allows the DMA
      --  controller to be used to manage automatically the converted data. For
      --  more details, refer to Section113.6.5: Managing converted data using
      --  the DMA on page1333.
      DMAEN          : Boolean := False;
      --  Direct memory access configuration This bit is set and cleared by
      --  software to select between two DMA modes of operation and is
      --  effective only when DMAEN1=11. For more details, refer to
      --  Section113.6.5: Managing converted data using the DMA on page1333.
      DMACFG         : ADC_CFGR1_DMACFG_Field := STM32_SVD.ADC.B_0X0;
      --  Scan sequence direction This bit is set and cleared by software to
      --  select the direction in which the channels is scanned in the
      --  sequence. It is effective only if CHSELMOD bit is cleared. Note: If
      --  CCRDY is not yet asserted after channel configuration (writing
      --  ADC_CHSELR register or changing CHSELRMOD or SCANDIR), the value
      --  written to this bit is ignored.
      SCANDIR        : Boolean := False;
      --  Data resolution These bits are written by software to select the
      --  resolution of the conversion.
      RES            : ADC_CFGR1_RES_Field := 16#0#;
      --  Data alignment This bit is set and cleared by software to select
      --  right or left alignment. Refer to Figure141: Data alignment and
      --  resolution (oversampling disabled: OVSE = 0) on page1332
      ALIGN          : Boolean := False;
      --  External trigger selection These bits select the external event used
      --  to trigger the start of conversion (refer to Table160: External
      --  triggers for details):
      EXTSEL         : ADC_CFGR1_EXTSEL_Field := 16#0#;
      --  unspecified
      Reserved_9_9   : HAL.Bit := 16#0#;
      --  External trigger enable and polarity selection These bits are set and
      --  cleared by software to select the external trigger polarity and
      --  enable the trigger.
      EXTEN          : ADC_CFGR1_EXTEN_Field := 16#0#;
      --  Overrun management mode This bit is set and cleared by software and
      --  configure the way data overruns are managed.
      OVRMOD         : Boolean := False;
      --  Single / continuous conversion mode This bit is set and cleared by
      --  software. If it is set, conversion takes place continuously until it
      --  is cleared. Note: It is not possible to have both discontinuous mode
      --  and continuous mode enabled: it is forbidden to set both bits
      --  DISCEN1=11 and CONT1=11.
      CONT           : Boolean := False;
      --  Wait conversion mode This bit is set and cleared by software to
      --  enable/disable wait conversion mode.<sup>.</sup>
      WAIT           : Boolean := False;
      --  Auto-off mode This bit is set and cleared by software to
      --  enable/disable auto-off mode.<sup>.</sup>
      AUTOFF         : Boolean := False;
      --  Discontinuous mode This bit is set and cleared by software to
      --  enable/disable discontinuous mode. Note: It is not possible to have
      --  both discontinuous mode and continuous mode enabled: it is forbidden
      --  to set both bits DISCEN1=11 and CONT1=11.
      DISCEN         : Boolean := False;
      --  unspecified
      Reserved_17_20 : HAL.UInt4 := 16#0#;
      --  Mode selection of the ADC_CHSELR register This bit is set and cleared
      --  by software to control the ADC_CHSELR feature: Note: If CCRDY is not
      --  yet asserted after channel configuration (writing ADC_CHSELR register
      --  or changing CHSELRMOD or SCANDIR), the value written to this bit is
      --  ignored.
      CHSELRMOD      : Boolean := False;
      --  Enable the watchdog on a single channel or on all channels This bit
      --  is set and cleared by software to enable the analog watchdog on the
      --  channel identified by the AWDCH[4:0] bits or on all the channels
      AWD1SGL        : Boolean := False;
      --  Analog watchdog enable This bit is set and cleared by software.
      AWD1EN         : Boolean := False;
      --  unspecified
      Reserved_24_25 : HAL.UInt2 := 16#0#;
      --  Analog watchdog channel selection These bits are set and cleared by
      --  software. They select the input channel to be guarded by the analog
      --  watchdog. ..... Others: Reserved Note: The channel selected by the
      --  AWDCH[4:0] bits must be also set into the CHSELR register.
      AWD1CH         : ADC_CFGR1_AWD1CH_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_CFGR1_Register use record
      DMAEN          at 0 range 0 .. 0;
      DMACFG         at 0 range 1 .. 1;
      SCANDIR        at 0 range 2 .. 2;
      RES            at 0 range 3 .. 4;
      ALIGN          at 0 range 5 .. 5;
      EXTSEL         at 0 range 6 .. 8;
      Reserved_9_9   at 0 range 9 .. 9;
      EXTEN          at 0 range 10 .. 11;
      OVRMOD         at 0 range 12 .. 12;
      CONT           at 0 range 13 .. 13;
      WAIT           at 0 range 14 .. 14;
      AUTOFF         at 0 range 15 .. 15;
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
   subtype ADC_CFGR2_OVSS_Field is HAL.UInt4;
   subtype ADC_CFGR2_CKMODE_Field is HAL.UInt2;

   --  ADC configuration register 2
   type ADC_CFGR2_Register is record
      --  Oversampler Enable This bit is set and cleared by software. Note: The
      --  software is allowed to write this bit only when ADEN bit is cleared.
      OVSE           : Boolean := False;
      --  unspecified
      Reserved_1_1   : HAL.Bit := 16#0#;
      --  Oversampling ratio This bit filed defines the number of oversampling
      --  ratio. Note: The software is allowed to write this bit only when ADEN
      --  bit is cleared.
      OVSR           : ADC_CFGR2_OVSR_Field := 16#0#;
      --  Oversampling shift This bit is set and cleared by software. Others:
      --  Reserved Note: The software is allowed to write this bit only when
      --  ADEN bit is cleared.
      OVSS           : ADC_CFGR2_OVSS_Field := 16#0#;
      --  Triggered Oversampling This bit is set and cleared by software. Note:
      --  The software is allowed to write this bit only when ADEN bit is
      --  cleared.
      TOVS           : Boolean := False;
      --  unspecified
      Reserved_10_28 : HAL.UInt19 := 16#0#;
      --  Low frequency trigger mode enable This bit is set and cleared by
      --  software. Note: The software is allowed to write this bit only when
      --  ADEN bit is cleared.
      LFTRIG         : Boolean := False;
      --  ADC clock mode These bits are set and cleared by software to define
      --  how the analog ADC is clocked: In all synchronous clock modes, there
      --  is no jitter in the delay from a timer trigger to the start of a
      --  conversion. Note: The software is allowed to write these bits only
      --  when the ADC is disabled (ADCAL1=10, ADSTART1=10, ADSTP1=10,
      --  ADDIS1=10 and ADEN1=10).
      CKMODE         : ADC_CFGR2_CKMODE_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_CFGR2_Register use record
      OVSE           at 0 range 0 .. 0;
      Reserved_1_1   at 0 range 1 .. 1;
      OVSR           at 0 range 2 .. 4;
      OVSS           at 0 range 5 .. 8;
      TOVS           at 0 range 9 .. 9;
      Reserved_10_28 at 0 range 10 .. 28;
      LFTRIG         at 0 range 29 .. 29;
      CKMODE         at 0 range 30 .. 31;
   end record;

   subtype ADC_SMPR_SMP1_Field is HAL.UInt3;
   subtype ADC_SMPR_SMP2_Field is HAL.UInt3;

   --  ADC_SMPR_SMPSEL array
   type ADC_SMPR_SMPSEL_Field_Array is array (0 .. 19) of Boolean
     with Component_Size => 1, Size => 20;

   --  Type definition for ADC_SMPR_SMPSEL
   type ADC_SMPR_SMPSEL_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SMPSEL as a value
            Val : HAL.UInt20;
         when True =>
            --  SMPSEL as an array
            Arr : ADC_SMPR_SMPSEL_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 20;

   for ADC_SMPR_SMPSEL_Field use record
      Val at 0 range 0 .. 19;
      Arr at 0 range 0 .. 19;
   end record;

   --  ADC sampling time register
   type ADC_SMPR_Register is record
      --  Sampling time selection 1 These bits are written by software to
      --  select the sampling time that applies to all channels. Note: The
      --  software is allowed to write this bit only when ADSTART=0 (which
      --  ensures that no conversion is ongoing).
      SMP1           : ADC_SMPR_SMP1_Field := 16#0#;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Sampling time selection 2 These bits are written by software to
      --  select the sampling time that applies to all channels. Note: The
      --  software is allowed to write this bit only when ADSTART=0 (which
      --  ensures that no conversion is ongoing).
      SMP2           : ADC_SMPR_SMP2_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Channel-x sampling time selection (x1=119 to 0) These bits are
      --  written by software to define which sampling time is used. Note: The
      --  software is allowed to write this bit only when ADSTART1=10 (which
      --  ensures that no conversion is ongoing).
      SMPSEL         : ADC_SMPR_SMPSEL_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_SMPR_Register use record
      SMP1           at 0 range 0 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      SMP2           at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SMPSEL         at 0 range 8 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype ADC_AWD1TR_LT1_Field is HAL.UInt12;
   subtype ADC_AWD1TR_HT1_Field is HAL.UInt12;

   --  ADC watchdog threshold register
   type ADC_AWD1TR_Register is record
      --  Analog watchdog 1 lower threshold These bits are written by software
      --  to define the lower threshold for the analog watchdog. Refer to
      --  Section113.8: Analog window watchdogs on page1337.
      LT1            : ADC_AWD1TR_LT1_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Analog watchdog 1 higher threshold These bits are written by software
      --  to define the higher threshold for the analog watchdog. Refer to
      --  Section113.8: Analog window watchdogs on page1337.
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
      --  Analog watchdog 2 lower threshold These bits are written by software
      --  to define the lower threshold for the analog watchdog. Refer to
      --  Section113.8: Analog window watchdogs on page1337.
      LT2            : ADC_AWD2TR_LT2_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Analog watchdog 2 higher threshold These bits are written by software
      --  to define the higher threshold for the analog watchdog. Refer to
      --  Section113.8: Analog window watchdogs on page1337.
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

   --  ADC_CHSELR_CHSEL array
   type ADC_CHSELR_CHSEL_Field_Array is array (0 .. 19) of Boolean
     with Component_Size => 1, Size => 20;

   --  Type definition for ADC_CHSELR_CHSEL
   type ADC_CHSELR_CHSEL_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CHSEL as a value
            Val : HAL.UInt20;
         when True =>
            --  CHSEL as an array
            Arr : ADC_CHSELR_CHSEL_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 20;

   for ADC_CHSELR_CHSEL_Field use record
      Val at 0 range 0 .. 19;
      Arr at 0 range 0 .. 19;
   end record;

   --  ADC channel selection register
   type ADC_CHSELR_Register is record
      --  Channel-x selection These bits are written by software and define
      --  which channels are part of the sequence of channels to be converted.
      --  Note: The software is allowed to write this bit only when ADSTART1=10
      --  (which ensures that no conversion is ongoing). Note: If CCRDY is not
      --  yet asserted after channel configuration (writing ADC_CHSELR register
      --  or changing CHSELRMOD or SCANDIR), the value written to this bit is
      --  ignored.
      CHSEL          : ADC_CHSELR_CHSEL_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_CHSELR_Register use record
      CHSEL          at 0 range 0 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  ADC_CHSELR_ALTERNATE_SQ array element
   subtype ADC_CHSELR_ALTERNATE_SQ_Element is HAL.UInt4;

   --  ADC_CHSELR_ALTERNATE_SQ array
   type ADC_CHSELR_ALTERNATE_SQ_Field_Array is array (1 .. 8)
     of ADC_CHSELR_ALTERNATE_SQ_Element
     with Component_Size => 4, Size => 32;

   --  ADC channel selection register
   type ADC_CHSELR_ALTERNATE_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SQ as a value
            Val : HAL.UInt32;
         when True =>
            --  SQ as an array
            Arr : ADC_CHSELR_ALTERNATE_SQ_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_CHSELR_ALTERNATE_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   subtype ADC_AWD3TR_LT3_Field is HAL.UInt12;
   subtype ADC_AWD3TR_HT3_Field is HAL.UInt12;

   --  ADC watchdog threshold register
   type ADC_AWD3TR_Register is record
      --  Analog watchdog 3lower threshold These bits are written by software
      --  to define the lower threshold for the analog watchdog. Refer to
      --  Section113.8: Analog window watchdogs on page1337.
      LT3            : ADC_AWD3TR_LT3_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Analog watchdog 3 higher threshold These bits are written by software
      --  to define the higher threshold for the analog watchdog. Refer to
      --  Section113.8: Analog window watchdogs on page1337.
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
      --  Read-only. Converted data These bits are read-only. They contain the
      --  conversion result from the last converted channel. The data are left-
      --  or right-aligned as shown in Figure141: Data alignment and resolution
      --  (oversampling disabled: OVSE = 0) on page1332. Just after a
      --  calibration is complete, DATA[6:0] contains the calibration factor.
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

   --  ADC_AWD2CR_AWD2CH array
   type ADC_AWD2CR_AWD2CH_Field_Array is array (0 .. 19) of Boolean
     with Component_Size => 1, Size => 20;

   --  Type definition for ADC_AWD2CR_AWD2CH
   type ADC_AWD2CR_AWD2CH_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AWD2CH as a value
            Val : HAL.UInt20;
         when True =>
            --  AWD2CH as an array
            Arr : ADC_AWD2CR_AWD2CH_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 20;

   for ADC_AWD2CR_AWD2CH_Field use record
      Val at 0 range 0 .. 19;
      Arr at 0 range 0 .. 19;
   end record;

   --  ADC analog watchdog 2 configuration register
   type ADC_AWD2CR_Register is record
      --  Analog watchdog channel selection These bits are set and cleared by
      --  software. They enable and select the input channels to be guarded by
      --  analog watchdog 2 (AWD2). Note: The channels selected through
      --  ADC_AWD2CR must be also configured into the ADC_CHSELR registers. The
      --  software is allowed to write this bit only when ADSTART1=10 (which
      --  ensures that no conversion is ongoing).
      AWD2CH         : ADC_AWD2CR_AWD2CH_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_AWD2CR_Register use record
      AWD2CH         at 0 range 0 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  ADC_AWD3CR_AWD3CH array
   type ADC_AWD3CR_AWD3CH_Field_Array is array (0 .. 19) of Boolean
     with Component_Size => 1, Size => 20;

   --  Type definition for ADC_AWD3CR_AWD3CH
   type ADC_AWD3CR_AWD3CH_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AWD3CH as a value
            Val : HAL.UInt20;
         when True =>
            --  AWD3CH as an array
            Arr : ADC_AWD3CR_AWD3CH_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 20;

   for ADC_AWD3CR_AWD3CH_Field use record
      Val at 0 range 0 .. 19;
      Arr at 0 range 0 .. 19;
   end record;

   --  ADC Analog Watchdog 3 Configuration register
   type ADC_AWD3CR_Register is record
      --  Analog watchdog channel selection These bits are set and cleared by
      --  software. They enable and select the input channels to be guarded by
      --  analog watchdog 3 (AWD3). Note: The channels selected through
      --  ADC_AWD3CR must be also configured into the ADC_CHSELR registers. The
      --  software is allowed to write this bit only when ADSTART=0 (which
      --  ensures that no conversion is ongoing).
      AWD3CH         : ADC_AWD3CR_AWD3CH_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_AWD3CR_Register use record
      AWD3CH         at 0 range 0 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   subtype ADC_CALFACT_CALFACT_Field is HAL.UInt7;

   --  ADC calibration factor
   type ADC_CALFACT_Register is record
      --  Calibration factor These bits are written by hardware or by software.
      --  Once a calibration is complete,1they are updated by hardware with the
      --  calibration factors. Software can write these bits with a new
      --  calibration factor. If the new calibration factor is different from
      --  the current one stored into the analog ADC, it is then applied once a
      --  new conversion is launched. Just after a calibration is complete,
      --  DATA[6:0] contains the calibration factor. Note: Software can write
      --  these bits only when ADEN=1 (ADC is enabled and no calibration is
      --  ongoing and no conversion is ongoing).
      CALFACT       : ADC_CALFACT_CALFACT_Field := 16#0#;
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC_CALFACT_Register use record
      CALFACT       at 0 range 0 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   subtype ADC_CCR_PRESC_Field is HAL.UInt4;

   --  ADC common configuration register
   type ADC_CCR_Register is record
      --  unspecified
      Reserved_0_17  : HAL.UInt18 := 16#0#;
      --  ADC prescaler Set and cleared by software to select the frequency of
      --  the clock to the ADC. Other: Reserved Note: Software is allowed to
      --  write these bits only when the ADC is disabled (ADCAL1=10,
      --  ADSTART1=10, ADSTP1=10, ADDIS1=10 and ADEN1=10).
      PRESC          : ADC_CCR_PRESC_Field := 16#0#;
      --  V<sub>REFINT</sub> enable This bit is set and cleared by software to
      --  enable/disable the V<sub>REFINT</sub>. Note: Software is allowed to
      --  write this bit only when ADSTART1=10 (which ensures that no
      --  conversion is ongoing).
      VREFEN         : Boolean := False;
      --  Temperature sensor enable This bit is set and cleared by software to
      --  enable/disable the temperature sensor. Note: Software is allowed to
      --  write this bit only when ADSTART1=10 (which ensures that no
      --  conversion is ongoing).
      TSEN           : Boolean := False;
      --  V<sub>BAT</sub> enable This bit is set and cleared by software to
      --  enable/disable the V<sub>BAT</sub> channel. Note: The software is
      --  allowed to write this bit only when ADSTART1=10 (which ensures that
      --  no conversion is ongoing)
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
      TSEN           at 0 range 23 .. 23;
      VBATEN         at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   type ADC_Disc is
     (Default,
      Alternate);

   --  ADC address block description
   type ADC_Peripheral
     (Discriminent : ADC_Disc := Default)
   is record
      --  ADC interrupt and status register
      ADC_ISR              : aliased ADC_ISR_Register;
      --  ADC interrupt enable register
      ADC_IER              : aliased ADC_IER_Register;
      --  ADC control register
      ADC_CR               : aliased ADC_CR_Register;
      --  ADC configuration register 1
      ADC_CFGR1            : aliased ADC_CFGR1_Register;
      --  ADC configuration register 2
      ADC_CFGR2            : aliased ADC_CFGR2_Register;
      --  ADC sampling time register
      ADC_SMPR             : aliased ADC_SMPR_Register;
      --  ADC watchdog threshold register
      ADC_AWD1TR           : aliased ADC_AWD1TR_Register;
      --  ADC watchdog threshold register
      ADC_AWD2TR           : aliased ADC_AWD2TR_Register;
      --  ADC watchdog threshold register
      ADC_AWD3TR           : aliased ADC_AWD3TR_Register;
      --  ADC data register
      ADC_DR               : aliased ADC_DR_Register;
      --  ADC analog watchdog 2 configuration register
      ADC_AWD2CR           : aliased ADC_AWD2CR_Register;
      --  ADC Analog Watchdog 3 Configuration register
      ADC_AWD3CR           : aliased ADC_AWD3CR_Register;
      --  ADC calibration factor
      ADC_CALFACT          : aliased ADC_CALFACT_Register;
      --  ADC common configuration register
      ADC_CCR              : aliased ADC_CCR_Register;
      case Discriminent is
         when Default =>
            --  ADC channel selection register
            ADC_CHSELR : aliased ADC_CHSELR_Register;
         when Alternate =>
            --  ADC channel selection register
            ADC_CHSELR_ALTERNATE : aliased ADC_CHSELR_ALTERNATE_Register;
      end case;
   end record
     with Unchecked_Union, Volatile;

   for ADC_Peripheral use record
      ADC_ISR              at 16#0# range 0 .. 31;
      ADC_IER              at 16#4# range 0 .. 31;
      ADC_CR               at 16#8# range 0 .. 31;
      ADC_CFGR1            at 16#C# range 0 .. 31;
      ADC_CFGR2            at 16#10# range 0 .. 31;
      ADC_SMPR             at 16#14# range 0 .. 31;
      ADC_AWD1TR           at 16#20# range 0 .. 31;
      ADC_AWD2TR           at 16#24# range 0 .. 31;
      ADC_AWD3TR           at 16#2C# range 0 .. 31;
      ADC_DR               at 16#40# range 0 .. 31;
      ADC_AWD2CR           at 16#A0# range 0 .. 31;
      ADC_AWD3CR           at 16#A4# range 0 .. 31;
      ADC_CALFACT          at 16#B4# range 0 .. 31;
      ADC_CCR              at 16#308# range 0 .. 31;
      ADC_CHSELR           at 16#28# range 0 .. 31;
      ADC_CHSELR_ALTERNATE at 16#28# range 0 .. 31;
   end record;

   --  ADC address block description
   ADC_Periph : aliased ADC_Peripheral
     with Import, Address => ADC_Base;

end STM32_SVD.ADC;
