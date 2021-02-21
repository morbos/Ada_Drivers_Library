--  This spec has been automatically generated from STM32WL5x_CM4.svd

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

   --  ISR_AWD array
   type ISR_AWD_Field_Array is array (1 .. 3) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for ISR_AWD
   type ISR_AWD_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AWD as a value
            Val : HAL.UInt3;
         when True =>
            --  AWD as an array
            Arr : ISR_AWD_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for ISR_AWD_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  ADC interrupt and status register
   type ISR_Register is record
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
      AWD            : ISR_AWD_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  EOCAL
      EOCAL          : Boolean := False;
      --  unspecified
      Reserved_12_12 : HAL.Bit := 16#0#;
      --  CCRDY
      CCRDY          : Boolean := False;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISR_Register use record
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
   type IER_Register is record
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
      --  unspecified
      Reserved_12_12 : HAL.Bit := 16#0#;
      --  CCRDYIE
      CCRDYIE        : Boolean := False;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IER_Register use record
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
   type CR_Register is record
      --  ADEN
      ADEN           : Boolean := False;
      --  ADDIS
      ADDIS          : Boolean := False;
      --  ADSTART
      ADSTART        : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  ADSTP
      ADSTP          : Boolean := False;
      --  unspecified
      Reserved_5_27  : HAL.UInt23 := 16#0#;
      --  ADVREGEN
      ADVREGEN       : Boolean := False;
      --  unspecified
      Reserved_29_30 : HAL.UInt2 := 16#0#;
      --  ADCAL
      ADCAL          : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
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

   subtype CFGR1_RES_Field is HAL.UInt2;
   subtype CFGR1_EXTSEL_Field is HAL.UInt3;
   subtype CFGR1_EXTEN_Field is HAL.UInt2;
   subtype CFGR1_AWD1CH_Field is HAL.UInt5;

   --  ADC configuration register 1
   type CFGR1_Register is record
      --  DMAEN
      DMAEN          : Boolean := False;
      --  DMACFG
      DMACFG         : Boolean := False;
      --  SCANDIR
      SCANDIR        : Boolean := False;
      --  RES
      RES            : CFGR1_RES_Field := 16#0#;
      --  ALIGN
      ALIGN          : Boolean := False;
      --  EXTSEL
      EXTSEL         : CFGR1_EXTSEL_Field := 16#0#;
      --  unspecified
      Reserved_9_9   : HAL.Bit := 16#0#;
      --  EXTEN
      EXTEN          : CFGR1_EXTEN_Field := 16#0#;
      --  OVRMOD
      OVRMOD         : Boolean := False;
      --  CONT
      CONT           : Boolean := False;
      --  WAIT
      WAIT           : Boolean := False;
      --  AUTOFF
      AUTOFF         : Boolean := False;
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
      AWD1CH         : CFGR1_AWD1CH_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CFGR1_Register use record
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

   --  CFGR2_OVSR array
   type CFGR2_OVSR_Field_Array is array (0 .. 2) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for CFGR2_OVSR
   type CFGR2_OVSR_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  OVSR as a value
            Val : HAL.UInt3;
         when True =>
            --  OVSR as an array
            Arr : CFGR2_OVSR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for CFGR2_OVSR_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  CFGR2_OVSS array
   type CFGR2_OVSS_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for CFGR2_OVSS
   type CFGR2_OVSS_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  OVSS as a value
            Val : HAL.UInt4;
         when True =>
            --  OVSS as an array
            Arr : CFGR2_OVSS_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for CFGR2_OVSS_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   subtype CFGR2_CKMODE_Field is HAL.UInt2;

   --  ADC configuration register 2
   type CFGR2_Register is record
      --  OVSE
      OVSE           : Boolean := False;
      --  unspecified
      Reserved_1_1   : HAL.Bit := 16#0#;
      --  OVSR0
      OVSR           : CFGR2_OVSR_Field := (As_Array => False, Val => 16#0#);
      --  OVSS0
      OVSS           : CFGR2_OVSS_Field := (As_Array => False, Val => 16#0#);
      --  TOVS
      TOVS           : Boolean := False;
      --  unspecified
      Reserved_10_28 : HAL.UInt19 := 16#0#;
      --  LFTRIG
      LFTRIG         : Boolean := False;
      --  CKMODE
      CKMODE         : CFGR2_CKMODE_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CFGR2_Register use record
      OVSE           at 0 range 0 .. 0;
      Reserved_1_1   at 0 range 1 .. 1;
      OVSR           at 0 range 2 .. 4;
      OVSS           at 0 range 5 .. 8;
      TOVS           at 0 range 9 .. 9;
      Reserved_10_28 at 0 range 10 .. 28;
      LFTRIG         at 0 range 29 .. 29;
      CKMODE         at 0 range 30 .. 31;
   end record;

   subtype SMPR_SMP1_Field is HAL.UInt3;
   subtype SMPR_SMP2_Field is HAL.UInt3;
   subtype SMPR_SMPSEL_Field is HAL.UInt18;

   --  ADC sampling time register
   type SMPR_Register is record
      --  SMP1
      SMP1           : SMPR_SMP1_Field := 16#0#;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  SMP2
      SMP2           : SMPR_SMP2_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  SMPSEL
      SMPSEL         : SMPR_SMPSEL_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SMPR_Register use record
      SMP1           at 0 range 0 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      SMP2           at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SMPSEL         at 0 range 8 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype AWD1TR_LT1_Field is HAL.UInt12;
   subtype AWD1TR_HT1_Field is HAL.UInt12;

   --  ADC watchdog threshold register
   type AWD1TR_Register is record
      --  LT1
      LT1            : AWD1TR_LT1_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  HT1
      HT1            : AWD1TR_HT1_Field := 16#FFF#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AWD1TR_Register use record
      LT1            at 0 range 0 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      HT1            at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype AWD2TR_LT2_Field is HAL.UInt12;
   subtype AWD2TR_HT2_Field is HAL.UInt12;

   --  ADC watchdog threshold register
   type AWD2TR_Register is record
      --  LT2
      LT2            : AWD2TR_LT2_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  HT2
      HT2            : AWD2TR_HT2_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AWD2TR_Register use record
      LT2            at 0 range 0 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      HT2            at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype CHSELR0_CHSEL_Field is HAL.UInt18;

   --  channel selection register
   type CHSELR0_Register is record
      --  CHSEL
      CHSEL          : CHSELR0_CHSEL_Field := 16#0#;
      --  unspecified
      Reserved_18_31 : HAL.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CHSELR0_Register use record
      CHSEL          at 0 range 0 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   --  CHSELR1_SQ array element
   subtype CHSELR1_SQ_Element is HAL.UInt4;

   --  CHSELR1_SQ array
   type CHSELR1_SQ_Field_Array is array (1 .. 8) of CHSELR1_SQ_Element
     with Component_Size => 4, Size => 32;

   --  channel selection register
   type CHSELR1_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SQ as a value
            Val : HAL.UInt32;
         when True =>
            --  SQ as an array
            Arr : CHSELR1_SQ_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CHSELR1_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   subtype AWD3TR_LT3_Field is HAL.UInt12;
   subtype AWD3TR_HT3_Field is HAL.UInt12;

   --  ADC watchdog threshold register
   type AWD3TR_Register is record
      --  LT3
      LT3            : AWD3TR_LT3_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  HT3
      HT3            : AWD3TR_HT3_Field := 16#FFF#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AWD3TR_Register use record
      LT3            at 0 range 0 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      HT3            at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype DR_DATA_Field is HAL.UInt16;

   --  ADC data register
   type DR_Register is record
      --  Read-only. DATA
      DATA           : DR_DATA_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR_Register use record
      DATA           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype AWD2CR_AWD2CH_Field is HAL.UInt18;

   --  ADC Analog Watchdog 2 Configuration register
   type AWD2CR_Register is record
      --  AWD2CH
      AWD2CH         : AWD2CR_AWD2CH_Field := 16#0#;
      --  unspecified
      Reserved_18_31 : HAL.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AWD2CR_Register use record
      AWD2CH         at 0 range 0 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   subtype AWD3CR_AWD3CH_Field is HAL.UInt18;

   --  ADC Analog Watchdog 3 Configuration register
   type AWD3CR_Register is record
      --  AWD3CH
      AWD3CH         : AWD3CR_AWD3CH_Field := 16#0#;
      --  unspecified
      Reserved_18_31 : HAL.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AWD3CR_Register use record
      AWD3CH         at 0 range 0 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   subtype CALFACT_CALFACT_Field is HAL.UInt7;

   --  ADC Calibration factor
   type CALFACT_Register is record
      --  CALFACT
      CALFACT       : CALFACT_CALFACT_Field := 16#0#;
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CALFACT_Register use record
      CALFACT       at 0 range 0 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   --  CCR_PRESC array
   type CCR_PRESC_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for CCR_PRESC
   type CCR_PRESC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PRESC as a value
            Val : HAL.UInt4;
         when True =>
            --  PRESC as an array
            Arr : CCR_PRESC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for CCR_PRESC_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  ADC common configuration register
   type CCR_Register is record
      --  unspecified
      Reserved_0_17  : HAL.UInt18 := 16#0#;
      --  PRESC0
      PRESC          : CCR_PRESC_Field := (As_Array => False, Val => 16#0#);
      --  VREFEN
      VREFEN         : Boolean := False;
      --  TSEN
      TSEN           : Boolean := False;
      --  VBATEN
      VBATEN         : Boolean := False;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCR_Register use record
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
     (Val_0,
      Val_1);

   --  Analog to digital convertor
   type ADC_Peripheral
     (Discriminent : ADC_Disc := Val_0)
   is record
      --  ADC interrupt and status register
      ISR     : aliased ISR_Register;
      --  ADC interrupt enable register
      IER     : aliased IER_Register;
      --  ADC control register
      CR      : aliased CR_Register;
      --  ADC configuration register 1
      CFGR1   : aliased CFGR1_Register;
      --  ADC configuration register 2
      CFGR2   : aliased CFGR2_Register;
      --  ADC sampling time register
      SMPR    : aliased SMPR_Register;
      --  ADC watchdog threshold register
      AWD1TR  : aliased AWD1TR_Register;
      --  ADC watchdog threshold register
      AWD2TR  : aliased AWD2TR_Register;
      --  ADC watchdog threshold register
      AWD3TR  : aliased AWD3TR_Register;
      --  ADC data register
      DR      : aliased DR_Register;
      --  ADC Analog Watchdog 2 Configuration register
      AWD2CR  : aliased AWD2CR_Register;
      --  ADC Analog Watchdog 3 Configuration register
      AWD3CR  : aliased AWD3CR_Register;
      --  ADC Calibration factor
      CALFACT : aliased CALFACT_Register;
      --  ADC common configuration register
      CCR     : aliased CCR_Register;
      case Discriminent is
         when Val_0 =>
            --  channel selection register
            CHSELR0 : aliased CHSELR0_Register;
         when Val_1 =>
            --  channel selection register
            CHSELR1 : aliased CHSELR1_Register;
      end case;
   end record
     with Unchecked_Union, Volatile;

   for ADC_Peripheral use record
      ISR     at 16#0# range 0 .. 31;
      IER     at 16#4# range 0 .. 31;
      CR      at 16#8# range 0 .. 31;
      CFGR1   at 16#C# range 0 .. 31;
      CFGR2   at 16#10# range 0 .. 31;
      SMPR    at 16#14# range 0 .. 31;
      AWD1TR  at 16#20# range 0 .. 31;
      AWD2TR  at 16#24# range 0 .. 31;
      AWD3TR  at 16#2C# range 0 .. 31;
      DR      at 16#40# range 0 .. 31;
      AWD2CR  at 16#A0# range 0 .. 31;
      AWD3CR  at 16#A4# range 0 .. 31;
      CALFACT at 16#B4# range 0 .. 31;
      CCR     at 16#308# range 0 .. 31;
      CHSELR0 at 16#28# range 0 .. 31;
      CHSELR1 at 16#28# range 0 .. 31;
   end record;

   --  Analog to digital convertor
   ADC_Periph : aliased ADC_Peripheral
     with Import, Address => ADC_Base;

end STM32_SVD.ADC;
