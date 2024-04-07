--  This spec has been automatically generated from xyzzy

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

   --  interrupt and status register
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
      --  JEOC
      JEOC           : Boolean := False;
      --  JEOS
      JEOS           : Boolean := False;
      --  AWD1
      AWD            : ISR_AWD_Field := (As_Array => False, Val => 16#0#);
      --  JQOVF
      JQOVF          : Boolean := False;
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISR_Register use record
      ADRDY          at 0 range 0 .. 0;
      EOSMP          at 0 range 1 .. 1;
      EOC            at 0 range 2 .. 2;
      EOS            at 0 range 3 .. 3;
      OVR            at 0 range 4 .. 4;
      JEOC           at 0 range 5 .. 5;
      JEOS           at 0 range 6 .. 6;
      AWD            at 0 range 7 .. 9;
      JQOVF          at 0 range 10 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   --  interrupt enable register
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
      --  JEOCIE
      JEOCIE         : Boolean := False;
      --  JEOSIE
      JEOSIE         : Boolean := False;
      --  AWD1IE
      AWD1IE         : Boolean := False;
      --  AWD2IE
      AWD2IE         : Boolean := False;
      --  AWD3IE
      AWD3IE         : Boolean := False;
      --  JQOVFIE
      JQOVFIE        : Boolean := False;
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IER_Register use record
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
      JQOVFIE        at 0 range 10 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   --  control register
   type CR_Register is record
      --  ADEN
      ADEN          : Boolean := False;
      --  ADDIS
      ADDIS         : Boolean := False;
      --  ADSTART
      ADSTART       : Boolean := False;
      --  JADSTART
      JADSTART      : Boolean := False;
      --  ADSTP
      ADSTP         : Boolean := False;
      --  JADSTP
      JADSTP        : Boolean := False;
      --  unspecified
      Reserved_6_27 : HAL.UInt22 := 16#0#;
      --  ADVREGEN
      ADVREGEN      : Boolean := False;
      --  DEEPPWD
      DEEPPWD       : Boolean := False;
      --  ADCALDIF
      ADCALDIF      : Boolean := False;
      --  ADCAL
      ADCAL         : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      ADEN          at 0 range 0 .. 0;
      ADDIS         at 0 range 1 .. 1;
      ADSTART       at 0 range 2 .. 2;
      JADSTART      at 0 range 3 .. 3;
      ADSTP         at 0 range 4 .. 4;
      JADSTP        at 0 range 5 .. 5;
      Reserved_6_27 at 0 range 6 .. 27;
      ADVREGEN      at 0 range 28 .. 28;
      DEEPPWD       at 0 range 29 .. 29;
      ADCALDIF      at 0 range 30 .. 30;
      ADCAL         at 0 range 31 .. 31;
   end record;

   subtype CFGR_RES_Field is HAL.UInt2;
   subtype CFGR_EXTSEL_Field is HAL.UInt4;
   subtype CFGR_EXTEN_Field is HAL.UInt2;
   subtype CFGR_DISCNUM_Field is HAL.UInt3;
   subtype CFGR_AWDCH1CH_Field is HAL.UInt5;

   --  configuration register
   type CFGR_Register is record
      --  DMAEN
      DMAEN          : Boolean := False;
      --  DMACFG
      DMACFG         : Boolean := False;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  RES
      RES            : CFGR_RES_Field := 16#0#;
      --  ALIGN
      ALIGN          : Boolean := False;
      --  EXTSEL
      EXTSEL         : CFGR_EXTSEL_Field := 16#0#;
      --  EXTEN
      EXTEN          : CFGR_EXTEN_Field := 16#0#;
      --  OVRMOD
      OVRMOD         : Boolean := False;
      --  CONT
      CONT           : Boolean := False;
      --  AUTDLY
      AUTDLY         : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  DISCEN
      DISCEN         : Boolean := False;
      --  DISCNUM
      DISCNUM        : CFGR_DISCNUM_Field := 16#0#;
      --  JDISCEN
      JDISCEN        : Boolean := False;
      --  JQM
      JQM            : Boolean := False;
      --  AWD1SGL
      AWD1SGL        : Boolean := False;
      --  AWD1EN
      AWD1EN         : Boolean := False;
      --  JAWD1EN
      JAWD1EN        : Boolean := False;
      --  JAUTO
      JAUTO          : Boolean := False;
      --  AWDCH1CH
      AWDCH1CH       : CFGR_AWDCH1CH_Field := 16#0#;
      --  JQDIS
      JQDIS          : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CFGR_Register use record
      DMAEN          at 0 range 0 .. 0;
      DMACFG         at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      RES            at 0 range 3 .. 4;
      ALIGN          at 0 range 5 .. 5;
      EXTSEL         at 0 range 6 .. 9;
      EXTEN          at 0 range 10 .. 11;
      OVRMOD         at 0 range 12 .. 12;
      CONT           at 0 range 13 .. 13;
      AUTDLY         at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      DISCEN         at 0 range 16 .. 16;
      DISCNUM        at 0 range 17 .. 19;
      JDISCEN        at 0 range 20 .. 20;
      JQM            at 0 range 21 .. 21;
      AWD1SGL        at 0 range 22 .. 22;
      AWD1EN         at 0 range 23 .. 23;
      JAWD1EN        at 0 range 24 .. 24;
      JAUTO          at 0 range 25 .. 25;
      AWDCH1CH       at 0 range 26 .. 30;
      JQDIS          at 0 range 31 .. 31;
   end record;

   subtype CFGR2_OVSR_Field is HAL.UInt3;
   subtype CFGR2_OVSS_Field is HAL.UInt4;

   --  configuration register
   type CFGR2_Register is record
      --  DMAEN
      ROVSE          : Boolean := False;
      --  DMACFG
      JOVSE          : Boolean := False;
      --  RES
      OVSR           : CFGR2_OVSR_Field := 16#0#;
      --  ALIGN
      OVSS           : CFGR2_OVSS_Field := 16#0#;
      --  EXTSEL
      TOVS           : Boolean := False;
      --  EXTEN
      ROVSM          : Boolean := False;
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CFGR2_Register use record
      ROVSE          at 0 range 0 .. 0;
      JOVSE          at 0 range 1 .. 1;
      OVSR           at 0 range 2 .. 4;
      OVSS           at 0 range 5 .. 8;
      TOVS           at 0 range 9 .. 9;
      ROVSM          at 0 range 10 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   --  SMPR1_SMP array element
   subtype SMPR1_SMP_Element is HAL.UInt3;

   --  SMPR1_SMP array
   type SMPR1_SMP_Field_Array is array (0 .. 9) of SMPR1_SMP_Element
     with Component_Size => 3, Size => 30;

   --  Type definition for SMPR1_SMP
   type SMPR1_SMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SMP as a value
            Val : HAL.UInt30;
         when True =>
            --  SMP as an array
            Arr : SMPR1_SMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 30;

   for SMPR1_SMP_Field use record
      Val at 0 range 0 .. 29;
      Arr at 0 range 0 .. 29;
   end record;

   --  sample time register 1
   type SMPR1_Register is record
      --  SMP0
      SMP            : SMPR1_SMP_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SMPR1_Register use record
      SMP            at 0 range 0 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   --  SMPR2_SMP array element
   subtype SMPR2_SMP_Element is HAL.UInt3;

   --  SMPR2_SMP array
   type SMPR2_SMP_Field_Array is array (10 .. 18) of SMPR2_SMP_Element
     with Component_Size => 3, Size => 27;

   --  Type definition for SMPR2_SMP
   type SMPR2_SMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SMP as a value
            Val : HAL.UInt27;
         when True =>
            --  SMP as an array
            Arr : SMPR2_SMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 27;

   for SMPR2_SMP_Field use record
      Val at 0 range 0 .. 26;
      Arr at 0 range 0 .. 26;
   end record;

   --  sample time register 2
   type SMPR2_Register is record
      --  SMP10
      SMP            : SMPR2_SMP_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_27_31 : HAL.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SMPR2_Register use record
      SMP            at 0 range 0 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   subtype TR_LT1_Field is HAL.UInt12;
   subtype TR_HT1_Field is HAL.UInt12;

   type TR_Register is record
      --  LT1
      LT1            : TR_LT1_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  HT1
      HT1            : TR_HT1_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TR_Register use record
      LT1            at 0 range 0 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      HT1            at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   type TR_Registers is array (1 .. 3) of TR_Register;

   subtype SQR_L_Field is HAL.UInt4;
   subtype SQR_SQ1_Field is HAL.UInt5;
   subtype SQR_SQ2_Field is HAL.UInt5;
   subtype SQR_SQ3_Field is HAL.UInt5;
   subtype SQR_SQ4_Field is HAL.UInt5;

   type SQR_Register is record
      --  L
      L              : SQR_L_Field := 16#0#;
      --  unspecified
      Reserved_4_5   : HAL.UInt2 := 16#0#;
      --  SQ1
      SQ1            : SQR_SQ1_Field := 16#0#;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  SQ2
      SQ2            : SQR_SQ2_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : HAL.Bit := 16#0#;
      --  SQ3
      SQ3            : SQR_SQ3_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : HAL.Bit := 16#0#;
      --  SQ4
      SQ4            : SQR_SQ4_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SQR_Register use record
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

   type SQR_Registers is array (1 .. 4) of SQR_Register;

   subtype DR_RDATA_Field is HAL.UInt16;

   --  regular Data Register
   type DR_Register is record
      --  Read-only. regularDATA
      RDATA          : DR_RDATA_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR_Register use record
      RDATA          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype JSQR_JL_Field is HAL.UInt2;
   subtype JSQR_JEXTSEL_Field is HAL.UInt4;
   subtype JSQR_JEXTEN_Field is HAL.UInt2;
   subtype JSQR_JSQ1_Field is HAL.UInt5;
   subtype JSQR_JSQ2_Field is HAL.UInt5;
   subtype JSQR_JSQ3_Field is HAL.UInt5;
   subtype JSQR_JSQ4_Field is HAL.UInt5;

   --  injected sequence register
   type JSQR_Register is record
      --  JL
      JL             : JSQR_JL_Field := 16#0#;
      --  JEXTSEL
      JEXTSEL        : JSQR_JEXTSEL_Field := 16#0#;
      --  JEXTEN
      JEXTEN         : JSQR_JEXTEN_Field := 16#0#;
      --  JSQ1
      JSQ1           : JSQR_JSQ1_Field := 16#0#;
      --  unspecified
      Reserved_13_13 : HAL.Bit := 16#0#;
      --  JSQ2
      JSQ2           : JSQR_JSQ2_Field := 16#0#;
      --  unspecified
      Reserved_19_19 : HAL.Bit := 16#0#;
      --  JSQ3
      JSQ3           : JSQR_JSQ3_Field := 16#0#;
      --  unspecified
      Reserved_25_25 : HAL.Bit := 16#0#;
      --  JSQ4
      JSQ4           : JSQR_JSQ4_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for JSQR_Register use record
      JL             at 0 range 0 .. 1;
      JEXTSEL        at 0 range 2 .. 5;
      JEXTEN         at 0 range 6 .. 7;
      JSQ1           at 0 range 8 .. 12;
      Reserved_13_13 at 0 range 13 .. 13;
      JSQ2           at 0 range 14 .. 18;
      Reserved_19_19 at 0 range 19 .. 19;
      JSQ3           at 0 range 20 .. 24;
      Reserved_25_25 at 0 range 25 .. 25;
      JSQ4           at 0 range 26 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype OFR_OFFSET1_Field is HAL.UInt12;
   subtype OFR_OFFSET1_CH_Field is HAL.UInt5;

   type OFR_Register is record
      --  OFFSET1
      OFFSET1        : OFR_OFFSET1_Field := 16#0#;
      --  unspecified
      Reserved_12_25 : HAL.UInt14 := 16#0#;
      --  OFFSET1_CH
      OFFSET1_CH     : OFR_OFFSET1_CH_Field := 16#0#;
      --  OFFSET1_EN
      OFFSET1_EN     : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OFR_Register use record
      OFFSET1        at 0 range 0 .. 11;
      Reserved_12_25 at 0 range 12 .. 25;
      OFFSET1_CH     at 0 range 26 .. 30;
      OFFSET1_EN     at 0 range 31 .. 31;
   end record;

   type OFR_Registers is array (1 .. 4) of OFR_Register;

   subtype JDR_JDATA_Field is HAL.UInt16;

   type JDR_Register is record
      --  JDATA1
      JDATA          : JDR_JDATA_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for JDR_Register use record
      JDATA          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   type JDR_Registers is array (1 .. 4) of JDR_Register;

   subtype AWDCR_AWD2CH_Field is HAL.UInt19;

   type AWDCR_Register is record
      --  AWD2CH
      AWD2CH         : AWDCR_AWD2CH_Field := 16#0#;
      --  unspecified
      Reserved_19_31 : HAL.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AWDCR_Register use record
      AWD2CH         at 0 range 0 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   type AWDCR_Registers is array (2 .. 3) of AWDCR_Register;

   subtype DIFSEL_DIFSEL_1_15_Field is HAL.UInt15;
   subtype DIFSEL_DIFSEL_16_18_Field is HAL.UInt3;

   --  Differential Mode Selection Register 2
   type DIFSEL_Register is record
      --  Read-only. Differential mode for channel 0
      DIFSEL_0       : Boolean := False;
      --  Differential mode for channels 15 to 1
      DIFSEL_1_15    : DIFSEL_DIFSEL_1_15_Field := 16#0#;
      --  Read-only. Differential mode for channels 18 to 16
      DIFSEL_16_18   : DIFSEL_DIFSEL_16_18_Field := 16#0#;
      --  unspecified
      Reserved_19_31 : HAL.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DIFSEL_Register use record
      DIFSEL_0       at 0 range 0 .. 0;
      DIFSEL_1_15    at 0 range 1 .. 15;
      DIFSEL_16_18   at 0 range 16 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   subtype CALFACT_CALFACT_S_Field is HAL.UInt7;
   subtype CALFACT_CALFACT_D_Field is HAL.UInt7;

   --  Calibration Factors
   type CALFACT_Register is record
      --  CALFACT_S
      CALFACT_S      : CALFACT_CALFACT_S_Field := 16#0#;
      --  unspecified
      Reserved_7_15  : HAL.UInt9 := 16#0#;
      --  CALFACT_D
      CALFACT_D      : CALFACT_CALFACT_D_Field := 16#0#;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CALFACT_Register use record
      CALFACT_S      at 0 range 0 .. 6;
      Reserved_7_15  at 0 range 7 .. 15;
      CALFACT_D      at 0 range 16 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  ADC Common status register
   type CSR_Register is record
      --  Read-only. ADDRDY_MST
      ADDRDY_MST     : Boolean;
      --  Read-only. EOSMP_MST
      EOSMP_MST      : Boolean;
      --  Read-only. EOC_MST
      EOC_MST        : Boolean;
      --  Read-only. EOS_MST
      EOS_MST        : Boolean;
      --  Read-only. OVR_MST
      OVR_MST        : Boolean;
      --  Read-only. JEOC_MST
      JEOC_MST       : Boolean;
      --  Read-only. JEOS_MST
      JEOS_MST       : Boolean;
      --  Read-only. AWD1_MST
      AWD1_MST       : Boolean;
      --  Read-only. AWD2_MST
      AWD2_MST       : Boolean;
      --  Read-only. AWD3_MST
      AWD3_MST       : Boolean;
      --  Read-only. JQOVF_MST
      JQOVF_MST      : Boolean;
      --  unspecified
      Reserved_11_15 : HAL.UInt5;
      --  Read-only. ADRDY_SLV
      ADRDY_SLV      : Boolean;
      --  Read-only. EOSMP_SLV
      EOSMP_SLV      : Boolean;
      --  Read-only. EOC_SLV
      EOC_SLV        : Boolean;
      --  Read-only. EOS_SLV
      EOS_SLV        : Boolean;
      --  Read-only. OVR_SLV
      OVR_SLV        : Boolean;
      --  Read-only. JEOC_SLV
      JEOC_SLV       : Boolean;
      --  Read-only. JEOS_SLV
      JEOS_SLV       : Boolean;
      --  Read-only. AWD1_SLV
      AWD1_SLV       : Boolean;
      --  Read-only. AWD2_SLV
      AWD2_SLV       : Boolean;
      --  Read-only. AWD3_SLV
      AWD3_SLV       : Boolean;
      --  Read-only. JQOVF_SLV
      JQOVF_SLV      : Boolean;
      --  unspecified
      Reserved_27_31 : HAL.UInt5;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CSR_Register use record
      ADDRDY_MST     at 0 range 0 .. 0;
      EOSMP_MST      at 0 range 1 .. 1;
      EOC_MST        at 0 range 2 .. 2;
      EOS_MST        at 0 range 3 .. 3;
      OVR_MST        at 0 range 4 .. 4;
      JEOC_MST       at 0 range 5 .. 5;
      JEOS_MST       at 0 range 6 .. 6;
      AWD1_MST       at 0 range 7 .. 7;
      AWD2_MST       at 0 range 8 .. 8;
      AWD3_MST       at 0 range 9 .. 9;
      JQOVF_MST      at 0 range 10 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
      ADRDY_SLV      at 0 range 16 .. 16;
      EOSMP_SLV      at 0 range 17 .. 17;
      EOC_SLV        at 0 range 18 .. 18;
      EOS_SLV        at 0 range 19 .. 19;
      OVR_SLV        at 0 range 20 .. 20;
      JEOC_SLV       at 0 range 21 .. 21;
      JEOS_SLV       at 0 range 22 .. 22;
      AWD1_SLV       at 0 range 23 .. 23;
      AWD2_SLV       at 0 range 24 .. 24;
      AWD3_SLV       at 0 range 25 .. 25;
      JQOVF_SLV      at 0 range 26 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   subtype CCR_DUAL_Field is HAL.UInt5;
   subtype CCR_DELAY_Field is HAL.UInt3;
   subtype CCR_MDMA_Field is HAL.UInt2;
   subtype CCR_CKMODE_Field is HAL.UInt2;
   subtype CCR_PRESC_Field is HAL.UInt4;

   --  ADC common control register
   type CCR_Register is record
      --  DUAL
      DUAL           : CCR_DUAL_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  DELAY
      DELAY_k        : CCR_DELAY_Field := 16#0#;
      --  unspecified
      Reserved_11_12 : HAL.UInt2 := 16#0#;
      --  DMACFG
      DMACFG         : Boolean := False;
      --  MDMA
      MDMA           : CCR_MDMA_Field := 16#0#;
      --  ADC clock mode
      CKMODE         : CCR_CKMODE_Field := 16#0#;
      --  ADC prescaler
      PRESC          : CCR_PRESC_Field := 16#0#;
      --  VREFINT enable
      VREFEN         : Boolean := False;
      --  CH17SEL
      CH17SEL        : Boolean := False;
      --  CH18SEL
      CH18SEL        : Boolean := False;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCR_Register use record
      DUAL           at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      DELAY_k        at 0 range 8 .. 10;
      Reserved_11_12 at 0 range 11 .. 12;
      DMACFG         at 0 range 13 .. 13;
      MDMA           at 0 range 14 .. 15;
      CKMODE         at 0 range 16 .. 17;
      PRESC          at 0 range 18 .. 21;
      VREFEN         at 0 range 22 .. 22;
      CH17SEL        at 0 range 23 .. 23;
      CH18SEL        at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype CDR_RDATA_MST_Field is HAL.UInt16;
   subtype CDR_RDATA_SLV_Field is HAL.UInt16;

   --  Common regular data register for dual mode
   type CDR_Register is record
      --  Read-only. RDATA_MST
      RDATA_MST : CDR_RDATA_MST_Field;
      --  Read-only. RDATA_SLV
      RDATA_SLV : CDR_RDATA_SLV_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CDR_Register use record
      RDATA_MST at 0 range 0 .. 15;
      RDATA_SLV at 0 range 16 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Analog-to-Digital Converter
   type ADC_Peripheral is record
      --  interrupt and status register
      ISR     : aliased ISR_Register;
      --  interrupt enable register
      IER     : aliased IER_Register;
      --  control register
      CR      : aliased CR_Register;
      --  configuration register
      CFGR    : aliased CFGR_Register;
      --  configuration register
      CFGR2   : aliased CFGR2_Register;
      --  sample time register 1
      SMPR1   : aliased SMPR1_Register;
      --  sample time register 2
      SMPR2   : aliased SMPR2_Register;
      TR      : aliased TR_Registers;
      SQR     : aliased SQR_Registers;
      --  regular Data Register
      DR      : aliased DR_Register;
      --  injected sequence register
      JSQR    : aliased JSQR_Register;
      OFR     : aliased OFR_Registers;
      JDR     : aliased JDR_Registers;
      AWDCR   : aliased AWDCR_Registers;
      --  Differential Mode Selection Register 2
      DIFSEL  : aliased DIFSEL_Register;
      --  Calibration Factors
      CALFACT : aliased CALFACT_Register;
   end record
     with Volatile;

   for ADC_Peripheral use record
      ISR     at 16#0# range 0 .. 31;
      IER     at 16#4# range 0 .. 31;
      CR      at 16#8# range 0 .. 31;
      CFGR    at 16#C# range 0 .. 31;
      CFGR2   at 16#10# range 0 .. 31;
      SMPR1   at 16#14# range 0 .. 31;
      SMPR2   at 16#18# range 0 .. 31;
      TR      at 16#20# range 0 .. 95;
      SQR     at 16#30# range 0 .. 127;
      DR      at 16#40# range 0 .. 31;
      JSQR    at 16#4C# range 0 .. 31;
      OFR     at 16#60# range 0 .. 127;
      JDR     at 16#80# range 0 .. 127;
      AWDCR   at 16#A0# range 0 .. 63;
      DIFSEL  at 16#B0# range 0 .. 31;
      CALFACT at 16#B4# range 0 .. 31;
   end record;

   --  Analog-to-Digital Converter
   ADC_Periph : aliased ADC_Peripheral
     with Import, Address => ADC_Base;

   --  Analog-to-Digital Converter
   SEC_ADC_Periph : aliased ADC_Peripheral
     with Import, Address => SEC_ADC_Base;

   --  Analog-to-Digital Converter
   type ADC_Common_Peripheral is record
      --  ADC Common status register
      CSR : aliased CSR_Register;
      --  ADC common control register
      CCR : aliased CCR_Register;
      --  Common regular data register for dual mode
      CDR : aliased CDR_Register;
   end record
     with Volatile;

   for ADC_Common_Peripheral use record
      CSR at 16#0# range 0 .. 31;
      CCR at 16#8# range 0 .. 31;
      CDR at 16#C# range 0 .. 31;
   end record;

   --  Analog-to-Digital Converter
   ADC_Common_Periph : aliased ADC_Common_Peripheral
     with Import, Address => ADC_Common_Base;

   --  Analog-to-Digital Converter
   SEC_ADC_Common_Periph : aliased ADC_Common_Peripheral
     with Import, Address => SEC_ADC_Common_Base;

end STM32_SVD.ADC;
