--  This spec has been automatically generated from STM32U5xx.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.LPTIM is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Interrupt and Status Register (output mode)
   type ISR_output_Register is record
      --  Read-only. Compare 1 interrupt flag
      CC1IF          : Boolean;
      --  Read-only. Autoreload match
      ARRM           : Boolean;
      --  Read-only. External trigger edge event
      EXTTRIG        : Boolean;
      --  Read-only. Compare register 1 update OK
      CMP1OK         : Boolean;
      --  Read-only. Autoreload register update OK
      ARROK          : Boolean;
      --  Read-only. Counter direction change down to up
      UP             : Boolean;
      --  Read-only. Counter direction change up to down
      DOWN           : Boolean;
      --  Read-only. LPTIM update event occurred
      UE             : Boolean;
      --  Read-only. Repetition register update Ok
      REPOK          : Boolean;
      --  Read-only. Compare 2 interrupt flag
      CC2IF          : Boolean;
      --  unspecified
      Reserved_10_18 : HAL.UInt9;
      --  Read-only. Compare register 2 update OK
      CMP2OK         : Boolean;
      --  unspecified
      Reserved_20_23 : HAL.UInt4;
      --  Read-only. Interrupt enable register update OK
      DIEROK         : Boolean;
      --  unspecified
      Reserved_25_31 : HAL.UInt7;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISR_output_Register use record
      CC1IF          at 0 range 0 .. 0;
      ARRM           at 0 range 1 .. 1;
      EXTTRIG        at 0 range 2 .. 2;
      CMP1OK         at 0 range 3 .. 3;
      ARROK          at 0 range 4 .. 4;
      UP             at 0 range 5 .. 5;
      DOWN           at 0 range 6 .. 6;
      UE             at 0 range 7 .. 7;
      REPOK          at 0 range 8 .. 8;
      CC2IF          at 0 range 9 .. 9;
      Reserved_10_18 at 0 range 10 .. 18;
      CMP2OK         at 0 range 19 .. 19;
      Reserved_20_23 at 0 range 20 .. 23;
      DIEROK         at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  Interrupt and Status Register (intput mode)
   type ISR_intput_Register is record
      --  Read-only. Compare 1 interrupt flag
      CC1IF          : Boolean;
      --  Read-only. Autoreload match
      ARRM           : Boolean;
      --  Read-only. External trigger edge event
      EXTTRIG        : Boolean;
      --  unspecified
      Reserved_3_3   : HAL.Bit;
      --  Read-only. Autoreload register update OK
      ARROK          : Boolean;
      --  Read-only. Counter direction change down to up
      UP             : Boolean;
      --  Read-only. Counter direction change up to down
      DOWN           : Boolean;
      --  Read-only. LPTIM update event occurred
      UE             : Boolean;
      --  Read-only. Repetition register update Ok
      REPOK          : Boolean;
      --  Read-only. Capture 2 interrupt flag
      CC2IF          : Boolean;
      --  unspecified
      Reserved_10_11 : HAL.UInt2;
      --  Read-only. Capture 1 over-capture flag
      CC1OF          : Boolean;
      --  Read-only. Capture 2 over-capture flag
      CC2OF          : Boolean;
      --  unspecified
      Reserved_14_23 : HAL.UInt10;
      --  Read-only. Interrupt enable register update OK
      DIEROK         : Boolean;
      --  unspecified
      Reserved_25_31 : HAL.UInt7;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISR_intput_Register use record
      CC1IF          at 0 range 0 .. 0;
      ARRM           at 0 range 1 .. 1;
      EXTTRIG        at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      ARROK          at 0 range 4 .. 4;
      UP             at 0 range 5 .. 5;
      DOWN           at 0 range 6 .. 6;
      UE             at 0 range 7 .. 7;
      REPOK          at 0 range 8 .. 8;
      CC2IF          at 0 range 9 .. 9;
      Reserved_10_11 at 0 range 10 .. 11;
      CC1OF          at 0 range 12 .. 12;
      CC2OF          at 0 range 13 .. 13;
      Reserved_14_23 at 0 range 14 .. 23;
      DIEROK         at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  Interrupt Clear Register (output mode)
   type ICR_output_Register is record
      --  Write-only. Capture/compare 1 clear flag
      CC1IF          : Boolean := False;
      --  Write-only. Autoreload match Clear Flag
      ARRMCF         : Boolean := False;
      --  Write-only. External trigger valid edge Clear Flag
      EXTTRIGCF      : Boolean := False;
      --  Write-only. Compare register 1 update OK Clear Flag
      CMP1OKCF       : Boolean := False;
      --  Write-only. Autoreload register update OK Clear Flag
      ARROKCF        : Boolean := False;
      --  Write-only. Direction change to UP Clear Flag
      UPCF           : Boolean := False;
      --  Write-only. Direction change to down Clear Flag
      DOWNCF         : Boolean := False;
      --  Write-only. Update event clear flag
      UECF           : Boolean := False;
      --  Write-only. Repetition register update OK clear flag
      REPOKCF        : Boolean := False;
      --  Write-only. Capture/compare 2 clear flag
      CC2CF          : Boolean := False;
      --  unspecified
      Reserved_10_18 : HAL.UInt9 := 16#0#;
      --  Write-only. Compare register 2 update OK clear flag
      CMP2OKCF       : Boolean := False;
      --  unspecified
      Reserved_20_23 : HAL.UInt4 := 16#0#;
      --  Write-only. Interrupt enable register update OK clear flag
      DIEROKCF       : Boolean := False;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ICR_output_Register use record
      CC1IF          at 0 range 0 .. 0;
      ARRMCF         at 0 range 1 .. 1;
      EXTTRIGCF      at 0 range 2 .. 2;
      CMP1OKCF       at 0 range 3 .. 3;
      ARROKCF        at 0 range 4 .. 4;
      UPCF           at 0 range 5 .. 5;
      DOWNCF         at 0 range 6 .. 6;
      UECF           at 0 range 7 .. 7;
      REPOKCF        at 0 range 8 .. 8;
      CC2CF          at 0 range 9 .. 9;
      Reserved_10_18 at 0 range 10 .. 18;
      CMP2OKCF       at 0 range 19 .. 19;
      Reserved_20_23 at 0 range 20 .. 23;
      DIEROKCF       at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  Interrupt Clear Register (intput mode)
   type ICR_intput_Register is record
      --  Write-only. Capture/compare 1 clear flag
      CC1IF          : Boolean := False;
      --  Write-only. Autoreload match Clear Flag
      ARRMCF         : Boolean := False;
      --  Write-only. External trigger valid edge Clear Flag
      EXTTRIGCF      : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Write-only. Autoreload register update OK Clear Flag
      ARROKCF        : Boolean := False;
      --  Write-only. Direction change to UP Clear Flag
      UPCF           : Boolean := False;
      --  Write-only. Direction change to down Clear Flag
      DOWNCF         : Boolean := False;
      --  Write-only. Update event clear flag
      UECF           : Boolean := False;
      --  Write-only. Repetition register update OK clear flag
      REPOKCF        : Boolean := False;
      --  Write-only. Capture/compare 2 clear flag
      CC2CF          : Boolean := False;
      --  unspecified
      Reserved_10_11 : HAL.UInt2 := 16#0#;
      --  Write-only. Capture/compare 1 over-capture clear flag
      CC1OCF         : Boolean := False;
      --  Write-only. Capture/compare 2 over-capture clear flag
      CC2OCF         : Boolean := False;
      --  unspecified
      Reserved_14_23 : HAL.UInt10 := 16#0#;
      --  Write-only. Interrupt enable register update OK clear flag
      DIEROKCF       : Boolean := False;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ICR_intput_Register use record
      CC1IF          at 0 range 0 .. 0;
      ARRMCF         at 0 range 1 .. 1;
      EXTTRIGCF      at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      ARROKCF        at 0 range 4 .. 4;
      UPCF           at 0 range 5 .. 5;
      DOWNCF         at 0 range 6 .. 6;
      UECF           at 0 range 7 .. 7;
      REPOKCF        at 0 range 8 .. 8;
      CC2CF          at 0 range 9 .. 9;
      Reserved_10_11 at 0 range 10 .. 11;
      CC1OCF         at 0 range 12 .. 12;
      CC2OCF         at 0 range 13 .. 13;
      Reserved_14_23 at 0 range 14 .. 23;
      DIEROKCF       at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  LPTIM interrupt Enable Register (output mode)
   type DIER_output_Register is record
      --  Capture/compare 1 clear flag
      CC1IF          : Boolean := False;
      --  Autoreload match Interrupt Enable
      ARRMIE         : Boolean := False;
      --  External trigger valid edge Interrupt Enable
      EXTTRIGIE      : Boolean := False;
      --  Compare register 1 update OK Interrupt Enable
      CMP1OKIE       : Boolean := False;
      --  Autoreload register update OK Interrupt Enable
      ARROKIE        : Boolean := False;
      --  Direction change to UP Interrupt Enable
      UPIE           : Boolean := False;
      --  Direction change to down Interrupt Enable
      DOWNIE         : Boolean := False;
      --  Update event interrupt enable
      UEIE           : Boolean := False;
      --  REPOKIE
      REPOKIE        : Boolean := False;
      --  Capture/compare 2 interrupt enable
      CC2IE          : Boolean := False;
      --  unspecified
      Reserved_10_18 : HAL.UInt9 := 16#0#;
      --  Compare register 2 update OK interrupt enable
      CMP2OKIE       : Boolean := False;
      --  unspecified
      Reserved_20_22 : HAL.UInt3 := 16#0#;
      --  Update event DMA request enable
      UEDE           : Boolean := False;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DIER_output_Register use record
      CC1IF          at 0 range 0 .. 0;
      ARRMIE         at 0 range 1 .. 1;
      EXTTRIGIE      at 0 range 2 .. 2;
      CMP1OKIE       at 0 range 3 .. 3;
      ARROKIE        at 0 range 4 .. 4;
      UPIE           at 0 range 5 .. 5;
      DOWNIE         at 0 range 6 .. 6;
      UEIE           at 0 range 7 .. 7;
      REPOKIE        at 0 range 8 .. 8;
      CC2IE          at 0 range 9 .. 9;
      Reserved_10_18 at 0 range 10 .. 18;
      CMP2OKIE       at 0 range 19 .. 19;
      Reserved_20_22 at 0 range 20 .. 22;
      UEDE           at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  LPTIM interrupt Enable Register (intput mode)
   type DIER_intput_Register is record
      --  Capture/compare 1 clear flag
      CC1IF          : Boolean := False;
      --  Autoreload match Interrupt Enable
      ARRMIE         : Boolean := False;
      --  External trigger valid edge Interrupt Enable
      EXTTRIGIE      : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Autoreload register update OK Interrupt Enable
      ARROKIE        : Boolean := False;
      --  Direction change to UP Interrupt Enable
      UPIE           : Boolean := False;
      --  Direction change to down Interrupt Enable
      DOWNIE         : Boolean := False;
      --  Update event interrupt enable
      UEIE           : Boolean := False;
      --  REPOKIE
      REPOKIE        : Boolean := False;
      --  Capture/compare 2 interrupt enable
      CC2IE          : Boolean := False;
      --  unspecified
      Reserved_10_11 : HAL.UInt2 := 16#0#;
      --  Capture/compare 1 over-capture interrupt enable
      CC1OIE         : Boolean := False;
      --  Capture/compare 2 over-capture interrupt enable
      CC2OIE         : Boolean := False;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  Capture/compare 1 DMA request enable
      CC1DE          : Boolean := False;
      --  unspecified
      Reserved_17_24 : HAL.UInt8 := 16#0#;
      --  Capture/compare 2 DMA request enable
      CC2DE          : Boolean := False;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DIER_intput_Register use record
      CC1IF          at 0 range 0 .. 0;
      ARRMIE         at 0 range 1 .. 1;
      EXTTRIGIE      at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      ARROKIE        at 0 range 4 .. 4;
      UPIE           at 0 range 5 .. 5;
      DOWNIE         at 0 range 6 .. 6;
      UEIE           at 0 range 7 .. 7;
      REPOKIE        at 0 range 8 .. 8;
      CC2IE          at 0 range 9 .. 9;
      Reserved_10_11 at 0 range 10 .. 11;
      CC1OIE         at 0 range 12 .. 12;
      CC2OIE         at 0 range 13 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      CC1DE          at 0 range 16 .. 16;
      Reserved_17_24 at 0 range 17 .. 24;
      CC2DE          at 0 range 25 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype CFGR_CKPOL_Field is HAL.UInt2;
   subtype CFGR_CKFLT_Field is HAL.UInt2;
   subtype CFGR_TRGFLT_Field is HAL.UInt2;
   subtype CFGR_PRESC_Field is HAL.UInt3;
   subtype CFGR_TRIGSEL_Field is HAL.UInt3;
   subtype CFGR_TRIGEN_Field is HAL.UInt2;

   --  Configuration Register
   type CFGR_Register is record
      --  Clock selector
      CKSEL          : Boolean := False;
      --  Clock Polarity
      CKPOL          : CFGR_CKPOL_Field := 16#0#;
      --  Configurable digital filter for external clock
      CKFLT          : CFGR_CKFLT_Field := 16#0#;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  Configurable digital filter for trigger
      TRGFLT         : CFGR_TRGFLT_Field := 16#0#;
      --  unspecified
      Reserved_8_8   : HAL.Bit := 16#0#;
      --  Clock prescaler
      PRESC          : CFGR_PRESC_Field := 16#0#;
      --  unspecified
      Reserved_12_12 : HAL.Bit := 16#0#;
      --  Trigger selector
      TRIGSEL        : CFGR_TRIGSEL_Field := 16#0#;
      --  unspecified
      Reserved_16_16 : HAL.Bit := 16#0#;
      --  Trigger enable and polarity
      TRIGEN         : CFGR_TRIGEN_Field := 16#0#;
      --  Timeout enable
      TIMOUT         : Boolean := False;
      --  Waveform shape
      WAVE           : Boolean := False;
      --  Waveform shape polarity
      WAVPOL         : Boolean := False;
      --  Registers update mode
      PRELOAD        : Boolean := False;
      --  counter mode enabled
      COUNTMODE      : Boolean := False;
      --  Encoder mode enable
      ENC            : Boolean := False;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CFGR_Register use record
      CKSEL          at 0 range 0 .. 0;
      CKPOL          at 0 range 1 .. 2;
      CKFLT          at 0 range 3 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      TRGFLT         at 0 range 6 .. 7;
      Reserved_8_8   at 0 range 8 .. 8;
      PRESC          at 0 range 9 .. 11;
      Reserved_12_12 at 0 range 12 .. 12;
      TRIGSEL        at 0 range 13 .. 15;
      Reserved_16_16 at 0 range 16 .. 16;
      TRIGEN         at 0 range 17 .. 18;
      TIMOUT         at 0 range 19 .. 19;
      WAVE           at 0 range 20 .. 20;
      WAVPOL         at 0 range 21 .. 21;
      PRELOAD        at 0 range 22 .. 22;
      COUNTMODE      at 0 range 23 .. 23;
      ENC            at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  Control Register
   type CR_Register is record
      --  LPTIM Enable
      ENABLE        : Boolean := False;
      --  LPTIM start in single mode
      SNGSTRT       : Boolean := False;
      --  Timer start in continuous mode
      CNTSTRT       : Boolean := False;
      --  Counter reset
      COUNTRST      : Boolean := False;
      --  Reset after read enable
      RSTARE        : Boolean := False;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      ENABLE        at 0 range 0 .. 0;
      SNGSTRT       at 0 range 1 .. 1;
      CNTSTRT       at 0 range 2 .. 2;
      COUNTRST      at 0 range 3 .. 3;
      RSTARE        at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype CCR1_CCR1_Field is HAL.UInt16;

   --  Compare Register
   type CCR1_Register is record
      --  Capture/compare 1 value
      CCR1           : CCR1_CCR1_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCR1_Register use record
      CCR1           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype ARR_ARR_Field is HAL.UInt16;

   --  Autoreload Register
   type ARR_Register is record
      --  Auto reload value
      ARR            : ARR_ARR_Field := 16#1#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ARR_Register use record
      ARR            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CNT_CNT_Field is HAL.UInt16;

   --  Counter Register
   type CNT_Register is record
      --  Read-only. Counter value
      CNT            : CNT_CNT_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CNT_Register use record
      CNT            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CFGR2_IN1SEL_Field is HAL.UInt2;
   subtype CFGR2_IN2SEL_Field is HAL.UInt2;
   subtype CFGR2_IC1SEL_Field is HAL.UInt2;
   subtype CFGR2_IC2SEL_Field is HAL.UInt2;

   --  LPTIM configuration register 2
   type CFGR2_Register is record
      --  LPTIM input 1 selection
      IN1SEL         : CFGR2_IN1SEL_Field := 16#0#;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  LPTIM input 2 selection
      IN2SEL         : CFGR2_IN2SEL_Field := 16#0#;
      --  unspecified
      Reserved_6_15  : HAL.UInt10 := 16#0#;
      --  LPTIM input capture 1 selection
      IC1SEL         : CFGR2_IC1SEL_Field := 16#0#;
      --  unspecified
      Reserved_18_19 : HAL.UInt2 := 16#0#;
      --  LPTIM input capture 2 selection
      IC2SEL         : CFGR2_IC2SEL_Field := 16#0#;
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CFGR2_Register use record
      IN1SEL         at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      IN2SEL         at 0 range 4 .. 5;
      Reserved_6_15  at 0 range 6 .. 15;
      IC1SEL         at 0 range 16 .. 17;
      Reserved_18_19 at 0 range 18 .. 19;
      IC2SEL         at 0 range 20 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   subtype RCR_REP_Field is HAL.UInt8;

   --  LPTIM repetition register
   type RCR_Register is record
      --  Repetition register value
      REP           : RCR_REP_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RCR_Register use record
      REP           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype CCMR1_CC1P_Field is HAL.UInt2;
   subtype CCMR1_IC1PSC_Field is HAL.UInt2;
   subtype CCMR1_IC1F_Field is HAL.UInt2;
   subtype CCMR1_CC2P_Field is HAL.UInt2;
   subtype CCMR1_IC2PSC_Field is HAL.UInt2;
   subtype CCMR1_IC2F_Field is HAL.UInt2;

   --  LPTIM capture/compare mode register 1
   type CCMR1_Register is record
      --  Capture/compare 1 selection
      CC1SEL         : Boolean := False;
      --  Capture/compare 1 output enable
      CC1E           : Boolean := False;
      --  Capture/compare 1 output polarity
      CC1P           : CCMR1_CC1P_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Input capture 1 prescaler
      IC1PSC         : CCMR1_IC1PSC_Field := 16#0#;
      --  unspecified
      Reserved_10_11 : HAL.UInt2 := 16#0#;
      --  Input capture 1 filter
      IC1F           : CCMR1_IC1F_Field := 16#0#;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  Capture/compare 2 selection
      CC2SEL         : Boolean := False;
      --  Capture/compare 2 output enable
      CC2E           : Boolean := False;
      --  Capture/compare 2 output polarity
      CC2P           : CCMR1_CC2P_Field := 16#0#;
      --  unspecified
      Reserved_20_23 : HAL.UInt4 := 16#0#;
      --  Input capture 2 prescaler
      IC2PSC         : CCMR1_IC2PSC_Field := 16#0#;
      --  unspecified
      Reserved_26_27 : HAL.UInt2 := 16#0#;
      --  Input capture 2 filter
      IC2F           : CCMR1_IC2F_Field := 16#0#;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCMR1_Register use record
      CC1SEL         at 0 range 0 .. 0;
      CC1E           at 0 range 1 .. 1;
      CC1P           at 0 range 2 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      IC1PSC         at 0 range 8 .. 9;
      Reserved_10_11 at 0 range 10 .. 11;
      IC1F           at 0 range 12 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      CC2SEL         at 0 range 16 .. 16;
      CC2E           at 0 range 17 .. 17;
      CC2P           at 0 range 18 .. 19;
      Reserved_20_23 at 0 range 20 .. 23;
      IC2PSC         at 0 range 24 .. 25;
      Reserved_26_27 at 0 range 26 .. 27;
      IC2F           at 0 range 28 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype CCR2_CCR2_Field is HAL.UInt16;

   --  LPTIM Compare Register 2
   type CCR2_Register is record
      --  Capture/compare 2 value
      CCR2           : CCR2_CCR2_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCR2_Register use record
      CCR2           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype HWCFGR2_CFG1_Field is HAL.UInt4;
   subtype HWCFGR2_CFG2_Field is HAL.UInt8;

   --  LPTIM peripheral hardware configuration register 2
   type HWCFGR2_Register is record
      --  Read-only. peripheral hardware configuration 1
      CFG1           : HWCFGR2_CFG1_Field;
      --  unspecified
      Reserved_4_7   : HAL.UInt4;
      --  Read-only. peripheral hardware configuration 2
      CFG2           : HWCFGR2_CFG2_Field;
      --  Read-only. peripheral hardware configuration 3
      CFG3           : Boolean;
      --  unspecified
      Reserved_17_31 : HAL.UInt15;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for HWCFGR2_Register use record
      CFG1           at 0 range 0 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      CFG2           at 0 range 8 .. 15;
      CFG3           at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   --  HWCFGR1_CFG array element
   subtype HWCFGR1_CFG_Element is HAL.UInt8;

   --  HWCFGR1_CFG array
   type HWCFGR1_CFG_Field_Array is array (1 .. 2) of HWCFGR1_CFG_Element
     with Component_Size => 8, Size => 16;

   --  Type definition for HWCFGR1_CFG
   type HWCFGR1_CFG_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CFG as a value
            Val : HAL.UInt16;
         when True =>
            --  CFG as an array
            Arr : HWCFGR1_CFG_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for HWCFGR1_CFG_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   subtype HWCFGR1_CFG3_Field is HAL.UInt4;
   subtype HWCFGR1_CFG4_Field is HAL.UInt8;

   --  LPTIM peripheral hardware configuration register 1
   type HWCFGR1_Register is record
      --  Read-only. peripheral hardware configuration 1
      CFG            : HWCFGR1_CFG_Field;
      --  Read-only. peripheral hardware configuration 3
      CFG3           : HWCFGR1_CFG3_Field;
      --  unspecified
      Reserved_20_23 : HAL.UInt4;
      --  Read-only. peripheral hardware configuration 4
      CFG4           : HWCFGR1_CFG4_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for HWCFGR1_Register use record
      CFG            at 0 range 0 .. 15;
      CFG3           at 0 range 16 .. 19;
      Reserved_20_23 at 0 range 20 .. 23;
      CFG4           at 0 range 24 .. 31;
   end record;

   --  Interrupt and Status Register
   type ISR_Register is record
      --  Read-only. Compare 1 interrupt flag
      CC1IF          : Boolean;
      --  Read-only. Autoreload match
      ARRM           : Boolean;
      --  Read-only. External trigger edge event
      EXTTRIG        : Boolean;
      --  Read-only. Compare register 1 update OK
      CMP1OK         : Boolean;
      --  Read-only. Autoreload register update OK
      ARROK          : Boolean;
      --  Read-only. Counter direction change down to up
      UP             : Boolean;
      --  Read-only. Counter direction change up to down
      DOWN           : Boolean;
      --  Read-only. LPTIM update event occurred
      UE             : Boolean;
      --  Read-only. Repetition register update Ok
      REPOK          : Boolean;
      --  unspecified
      Reserved_9_23  : HAL.UInt15;
      --  Read-only. Interrupt enable register update OK
      DIEROK         : Boolean;
      --  unspecified
      Reserved_25_31 : HAL.UInt7;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISR_Register use record
      CC1IF          at 0 range 0 .. 0;
      ARRM           at 0 range 1 .. 1;
      EXTTRIG        at 0 range 2 .. 2;
      CMP1OK         at 0 range 3 .. 3;
      ARROK          at 0 range 4 .. 4;
      UP             at 0 range 5 .. 5;
      DOWN           at 0 range 6 .. 6;
      UE             at 0 range 7 .. 7;
      REPOK          at 0 range 8 .. 8;
      Reserved_9_23  at 0 range 9 .. 23;
      DIEROK         at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  Interrupt Clear Register
   type ICR_Register is record
      --  Write-only. Capture/compare 1 clear flag
      CC1IF          : Boolean := False;
      --  Write-only. Autoreload match Clear Flag
      ARRMCF         : Boolean := False;
      --  Write-only. External trigger valid edge Clear Flag
      EXTTRIGCF      : Boolean := False;
      --  Write-only. Compare register 1 update OK Clear Flag
      CMP1OKCF       : Boolean := False;
      --  Write-only. Autoreload register update OK Clear Flag
      ARROKCF        : Boolean := False;
      --  Write-only. Direction change to UP Clear Flag
      UPCF           : Boolean := False;
      --  Write-only. Direction change to down Clear Flag
      DOWNCF         : Boolean := False;
      --  Write-only. Update event clear flag
      UECF           : Boolean := False;
      --  Write-only. Repetition register update OK clear flag
      REPOKCF        : Boolean := False;
      --  unspecified
      Reserved_9_23  : HAL.UInt15 := 16#0#;
      --  Write-only. Interrupt enable register update OK clear flag
      DIEROKCF       : Boolean := False;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ICR_Register use record
      CC1IF          at 0 range 0 .. 0;
      ARRMCF         at 0 range 1 .. 1;
      EXTTRIGCF      at 0 range 2 .. 2;
      CMP1OKCF       at 0 range 3 .. 3;
      ARROKCF        at 0 range 4 .. 4;
      UPCF           at 0 range 5 .. 5;
      DOWNCF         at 0 range 6 .. 6;
      UECF           at 0 range 7 .. 7;
      REPOKCF        at 0 range 8 .. 8;
      Reserved_9_23  at 0 range 9 .. 23;
      DIEROKCF       at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  LPTIM interrupt Enable Register
   type DIER_Register is record
      --  Capture/compare 1 clear flag
      CC1IF         : Boolean := False;
      --  Autoreload match Interrupt Enable
      ARRMIE        : Boolean := False;
      --  External trigger valid edge Interrupt Enable
      EXTTRIGIE     : Boolean := False;
      --  Compare register 1 update OK Interrupt Enable
      CMP1OKIE      : Boolean := False;
      --  Autoreload register update OK Interrupt Enable
      ARROKIE       : Boolean := False;
      --  Direction change to UP Interrupt Enable
      UPIE          : Boolean := False;
      --  Direction change to down Interrupt Enable
      DOWNIE        : Boolean := False;
      --  Update event interrupt enable
      UEIE          : Boolean := False;
      --  REPOKIE
      REPOKIE       : Boolean := False;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DIER_Register use record
      CC1IF         at 0 range 0 .. 0;
      ARRMIE        at 0 range 1 .. 1;
      EXTTRIGIE     at 0 range 2 .. 2;
      CMP1OKIE      at 0 range 3 .. 3;
      ARROKIE       at 0 range 4 .. 4;
      UPIE          at 0 range 5 .. 5;
      DOWNIE        at 0 range 6 .. 6;
      UEIE          at 0 range 7 .. 7;
      REPOKIE       at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   type LPTIM1_Disc is
     (OUTPUT,
      INTPUT);

   --  Low power timer
   type LPTIM1_Peripheral
     (Discriminent : LPTIM1_Disc := OUTPUT)
   is record
      --  Configuration Register
      CFGR        : aliased CFGR_Register;
      --  Control Register
      CR          : aliased CR_Register;
      --  Compare Register
      CCR1        : aliased CCR1_Register;
      --  Autoreload Register
      ARR         : aliased ARR_Register;
      --  Counter Register
      CNT         : aliased CNT_Register;
      --  LPTIM configuration register 2
      CFGR2       : aliased CFGR2_Register;
      --  LPTIM repetition register
      RCR         : aliased RCR_Register;
      --  LPTIM capture/compare mode register 1
      CCMR1       : aliased CCMR1_Register;
      --  LPTIM Compare Register 2
      CCR2        : aliased CCR2_Register;
      --  LPTIM peripheral hardware configuration register 2
      HWCFGR2     : aliased HWCFGR2_Register;
      --  LPTIM peripheral hardware configuration register 1
      HWCFGR1     : aliased HWCFGR1_Register;
      case Discriminent is
         when OUTPUT =>
            --  Interrupt and Status Register (output mode)
            ISR_output : aliased ISR_output_Register;
            --  Interrupt Clear Register (output mode)
            ICR_output : aliased ICR_output_Register;
            --  LPTIM interrupt Enable Register (output mode)
            DIER_output : aliased DIER_output_Register;
         when INTPUT =>
            --  Interrupt and Status Register (intput mode)
            ISR_intput : aliased ISR_intput_Register;
            --  Interrupt Clear Register (intput mode)
            ICR_intput : aliased ICR_intput_Register;
            --  LPTIM interrupt Enable Register (intput mode)
            DIER_intput : aliased DIER_intput_Register;
      end case;
   end record
     with Unchecked_Union, Volatile;

   for LPTIM1_Peripheral use record
      CFGR        at 16#C# range 0 .. 31;
      CR          at 16#10# range 0 .. 31;
      CCR1        at 16#14# range 0 .. 31;
      ARR         at 16#18# range 0 .. 31;
      CNT         at 16#1C# range 0 .. 31;
      CFGR2       at 16#24# range 0 .. 31;
      RCR         at 16#28# range 0 .. 31;
      CCMR1       at 16#2C# range 0 .. 31;
      CCR2        at 16#34# range 0 .. 31;
      HWCFGR2     at 16#3EC# range 0 .. 31;
      HWCFGR1     at 16#3F0# range 0 .. 31;
      ISR_output  at 16#0# range 0 .. 31;
      ICR_output  at 16#4# range 0 .. 31;
      DIER_output at 16#8# range 0 .. 31;
      ISR_intput  at 16#0# range 0 .. 31;
      ICR_intput  at 16#4# range 0 .. 31;
      DIER_intput at 16#8# range 0 .. 31;
   end record;

   --  Low power timer
   LPTIM1_Periph : aliased LPTIM1_Peripheral
     with Import, Address => LPTIM1_Base;

   --  Low power timer
   LPTIM2_Periph : aliased LPTIM1_Peripheral
     with Import, Address => LPTIM2_Base;

   --  Low power timer
   LPTIM3_Periph : aliased LPTIM1_Peripheral
     with Import, Address => LPTIM3_Base;

   --  Low power timer
   SEC_LPTIM1_Periph : aliased LPTIM1_Peripheral
     with Import, Address => SEC_LPTIM1_Base;

   --  Low power timer
   SEC_LPTIM2_Periph : aliased LPTIM1_Peripheral
     with Import, Address => SEC_LPTIM2_Base;

   --  Low power timer
   SEC_LPTIM3_Periph : aliased LPTIM1_Peripheral
     with Import, Address => SEC_LPTIM3_Base;

   --  Low power timer
   type LPTIM4_Peripheral is record
      --  Interrupt and Status Register
      ISR     : aliased ISR_Register;
      --  Interrupt Clear Register
      ICR     : aliased ICR_Register;
      --  LPTIM interrupt Enable Register
      DIER    : aliased DIER_Register;
      --  Configuration Register
      CFGR    : aliased CFGR_Register;
      --  Control Register
      CR      : aliased CR_Register;
      --  Compare Register
      CCR1    : aliased CCR1_Register;
      --  Autoreload Register
      ARR     : aliased ARR_Register;
      --  Counter Register
      CNT     : aliased CNT_Register;
      --  LPTIM configuration register 2
      CFGR2   : aliased CFGR2_Register;
      --  LPTIM repetition register
      RCR     : aliased RCR_Register;
      --  LPTIM capture/compare mode register 1
      CCMR1   : aliased CCMR1_Register;
      --  LPTIM Compare Register 2
      CCR2    : aliased CCR2_Register;
      --  LPTIM peripheral hardware configuration register 2
      HWCFGR2 : aliased HWCFGR2_Register;
      --  LPTIM peripheral hardware configuration register 1
      HWCFGR1 : aliased HWCFGR1_Register;
   end record
     with Volatile;

   for LPTIM4_Peripheral use record
      ISR     at 16#0# range 0 .. 31;
      ICR     at 16#4# range 0 .. 31;
      DIER    at 16#8# range 0 .. 31;
      CFGR    at 16#C# range 0 .. 31;
      CR      at 16#10# range 0 .. 31;
      CCR1    at 16#14# range 0 .. 31;
      ARR     at 16#18# range 0 .. 31;
      CNT     at 16#1C# range 0 .. 31;
      CFGR2   at 16#24# range 0 .. 31;
      RCR     at 16#28# range 0 .. 31;
      CCMR1   at 16#2C# range 0 .. 31;
      CCR2    at 16#34# range 0 .. 31;
      HWCFGR2 at 16#3EC# range 0 .. 31;
      HWCFGR1 at 16#3F0# range 0 .. 31;
   end record;

   --  Low power timer
   LPTIM4_Periph : aliased LPTIM4_Peripheral
     with Import, Address => LPTIM4_Base;

   --  Low power timer
   SEC_LPTIM4_Periph : aliased LPTIM4_Peripheral
     with Import, Address => SEC_LPTIM4_Base;

end STM32_SVD.LPTIM;
