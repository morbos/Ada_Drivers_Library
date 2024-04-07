--  This spec has been automatically generated from STM32U073.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.TIM is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype TIM1_CR1_CMS_Field is HAL.UInt2;
   subtype TIM1_CR1_CKD_Field is HAL.UInt2;

   --  TIM1 control register 1
   type TIM1_CR1_Register is record
      --  Counter enable Note: External clock, gated mode and encoder mode can
      --  work only if the CEN bit has been previously set by software. However
      --  trigger mode can set the CEN bit automatically by hardware.
      CEN            : Boolean := False;
      --  Update disable This bit is set and cleared by software to
      --  enable/disable UEV event generation. Counter overflow/underflow
      --  Setting the UG bit Update generation through the slave mode
      --  controller Buffered registers are then loaded with their preload
      --  values.
      UDIS           : Boolean := False;
      --  Update request source This bit is set and cleared by software to
      --  select the UEV event sources. Counter overflow/underflow Setting the
      --  UG bit Update generation through the slave mode controller
      URS            : Boolean := False;
      --  One pulse mode
      OPM            : Boolean := False;
      --  Direction Note: This bit is read only when the timer is configured in
      --  Center-aligned mode or Encoder mode.
      DIR            : Boolean := False;
      --  Center-aligned mode selection Note: Switch from edge-aligned mode to
      --  center-aligned mode as long as the counter is enabled (CEN=1) is not
      --  allowed
      CMS            : TIM1_CR1_CMS_Field := 16#0#;
      --  Auto-reload preload enable
      ARPE           : Boolean := False;
      --  Clock division This bit-field indicates the division ratio between
      --  the timer clock (CK_INT) frequency and the dead-time and sampling
      --  clock (t<sub>DTS</sub>)used by the dead-time generators and the
      --  digital filters (ETR, TIx): Note: t<sub>DTS</sub>
      CKD            : TIM1_CR1_CKD_Field := 16#0#;
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  UIF status bit remapping
      UIFREMAP       : Boolean := False;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 16,
          Bit_Order => System.Low_Order_First;

   for TIM1_CR1_Register use record
      CEN            at 0 range 0 .. 0;
      UDIS           at 0 range 1 .. 1;
      URS            at 0 range 2 .. 2;
      OPM            at 0 range 3 .. 3;
      DIR            at 0 range 4 .. 4;
      CMS            at 0 range 5 .. 6;
      ARPE           at 0 range 7 .. 7;
      CKD            at 0 range 8 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      UIFREMAP       at 0 range 11 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
   end record;

   subtype TIM1_CR2_MMS_Field is HAL.UInt3;
   subtype TIM1_CR2_MMS2_Field is HAL.UInt4;

   --  TIM1 control register 2
   type TIM1_CR2_Register is record
      --  Capture/compare preloaded control Note: This bit acts only on
      --  channels that have a complementary output.
      CCPC           : Boolean := False;
      --  unspecified
      Reserved_1_1   : HAL.Bit := 16#0#;
      --  Capture/compare control update selection Note: This bit acts only on
      --  channels that have a complementary output.
      CCUS           : Boolean := False;
      --  Capture/compare DMA selection
      CCDS           : Boolean := False;
      --  Master mode selection These bits allow selected information to be
      --  sent in master mode to slave timers for synchronization (TRGO). The
      --  combination is as follows: Note: The clock of the slave timer or ADC
      --  must be enabled prior to receive events from the master timer, and
      --  must not be changed on-the-fly while triggers are received from the
      --  master timer.
      MMS            : TIM1_CR2_MMS_Field := 16#0#;
      --  TI1 selection
      TI1S           : Boolean := False;
      --  Output Idle state 1 (OC1 output) Note: This bit can not be modified
      --  as long as LOCK level 1, 2 or 3 has been programmed (LOCK bits in
      --  TIMx_BDTR register).
      OIS1           : Boolean := False;
      --  Output Idle state 1 (OC1N output) Note: This bit can not be modified
      --  as long as LOCK level 1, 2 or 3 has been programmed (LOCK bits in
      --  TIMx_BDTR register).
      OIS1N          : Boolean := False;
      --  Output Idle state 2 (OC2 output) Refer to OIS1 bit
      OIS2           : Boolean := False;
      --  Output Idle state 2 (OC2N output) Refer to OIS1N bit
      OIS2N          : Boolean := False;
      --  Output Idle state 3 (OC3 output) Refer to OIS1 bit
      OIS3           : Boolean := False;
      --  Output Idle state 3 (OC3N output) Refer to OIS1N bit
      OIS3N          : Boolean := False;
      --  Output Idle state 4 (OC4 output) Refer to OIS1 bit
      OIS4           : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Output Idle state 5 (OC5 output) Refer to OIS1 bit
      OIS5           : Boolean := False;
      --  unspecified
      Reserved_17_17 : HAL.Bit := 16#0#;
      --  Output Idle state 6 (OC6 output) Refer to OIS1 bit
      OIS6           : Boolean := False;
      --  unspecified
      Reserved_19_19 : HAL.Bit := 16#0#;
      --  Master mode selection 2 These bits allow the information to be sent
      --  to ADC for synchronization (TRGO2) to be selected. The combination is
      --  as follows: Note: The clock of the slave timer or ADC must be enabled
      --  prior to receive events from the master timer, and must not be
      --  changed on-the-fly while triggers are received from the master timer.
      MMS2           : TIM1_CR2_MMS2_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIM1_CR2_Register use record
      CCPC           at 0 range 0 .. 0;
      Reserved_1_1   at 0 range 1 .. 1;
      CCUS           at 0 range 2 .. 2;
      CCDS           at 0 range 3 .. 3;
      MMS            at 0 range 4 .. 6;
      TI1S           at 0 range 7 .. 7;
      OIS1           at 0 range 8 .. 8;
      OIS1N          at 0 range 9 .. 9;
      OIS2           at 0 range 10 .. 10;
      OIS2N          at 0 range 11 .. 11;
      OIS3           at 0 range 12 .. 12;
      OIS3N          at 0 range 13 .. 13;
      OIS4           at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      OIS5           at 0 range 16 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      OIS6           at 0 range 18 .. 18;
      Reserved_19_19 at 0 range 19 .. 19;
      MMS2           at 0 range 20 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype TIM1_SMCR_SMS_Field is HAL.UInt3;
   subtype TIM1_SMCR_TS_Field is HAL.UInt3;
   subtype TIM1_SMCR_ETF_Field is HAL.UInt4;
   subtype TIM1_SMCR_ETPS_Field is HAL.UInt2;
   subtype TIM1_SMCR_TS_1_Field is HAL.UInt2;

   --  TIM1 slave mode control register
   type TIM1_SMCR_Register is record
      --  SMS[0]: Slave mode selection When external signals are selected the
      --  active edge of the trigger signal (TRGI) is linked to the polarity
      --  selected on the external input (see Input Control register and
      --  Control Register description. Codes above 1000: Reserved. Note: The
      --  gated mode must not be used if TI1F_ED is selected as the trigger
      --  input (TS=00100). Indeed, TI1F_ED outputs 1 pulse for each transition
      --  on TI1F, whereas the gated mode checks the level of the trigger
      --  signal. Note: The clock of the slave peripherals (timer, ADC, ...)
      --  receiving the TRGO or the TRGO2 signals must be enabled prior to
      --  receive events from the master timer, and the clock frequency
      --  (prescaler) must not be changed on-the-fly while triggers are
      --  received from the master timer.
      SMS            : TIM1_SMCR_SMS_Field := 16#0#;
      --  OCREF clear selection This bit is used to select the OCREF clear
      --  source.
      OCCS           : Boolean := False;
      --  TS[0]: Trigger selection This bit-field selects the trigger input to
      --  be used to synchronize the counter. Others: Reserved See Table1118:
      --  TIM1 internal trigger connection on page1561 for more details on ITRx
      --  meaning for each Timer. Note: These bits must be changed only when
      --  they are not used (e.g. when SMS=000) to avoid wrong edge detections
      --  at the transition.
      TS             : TIM1_SMCR_TS_Field := 16#0#;
      --  Master/slave mode
      MSM            : Boolean := False;
      --  External trigger filter This bit-field then defines the frequency
      --  used to sample ETRP signal and the length of the digital filter
      --  applied to ETRP. The digital filter is made of an event counter in
      --  which N consecutive events are needed to validate a transition on the
      --  output:
      ETF            : TIM1_SMCR_ETF_Field := 16#0#;
      --  External trigger prescaler External trigger signal ETRP frequency
      --  must be at most 1/4 of f<sub>CK_INT</sub> frequency. A prescaler can
      --  be enabled to reduce ETRP frequency. It is useful when inputting fast
      --  external clocks.
      ETPS           : TIM1_SMCR_ETPS_Field := 16#0#;
      --  External clock enable This bit enables External clock mode 2. Note:
      --  Setting the ECE bit has the same effect as selecting external clock
      --  mode 1 with TRGI connected to ETRF (SMS=111 and TS=00111). It is
      --  possible to simultaneously use external clock mode 2 with the
      --  following slave modes: reset mode, gated mode and trigger mode.
      --  Nevertheless, TRGI must not be connected to ETRF in this case (TS
      --  bits must not be 00111). Note: If external clock mode 1 and external
      --  clock mode 2 are enabled at the same time, the external clock input
      --  is ETRF.
      ECE            : Boolean := False;
      --  External trigger polarity This bit selects whether ETR or ETR is used
      --  for trigger operations
      ETP            : Boolean := False;
      --  SMS[3]
      SMS_1          : Boolean := False;
      --  unspecified
      Reserved_17_19 : HAL.UInt3 := 16#0#;
      --  TS[4:3]
      TS_1           : TIM1_SMCR_TS_1_Field := 16#0#;
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIM1_SMCR_Register use record
      SMS            at 0 range 0 .. 2;
      OCCS           at 0 range 3 .. 3;
      TS             at 0 range 4 .. 6;
      MSM            at 0 range 7 .. 7;
      ETF            at 0 range 8 .. 11;
      ETPS           at 0 range 12 .. 13;
      ECE            at 0 range 14 .. 14;
      ETP            at 0 range 15 .. 15;
      SMS_1          at 0 range 16 .. 16;
      Reserved_17_19 at 0 range 17 .. 19;
      TS_1           at 0 range 20 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  TIM1 DMA/interrupt enable register
   type TIM1_DIER_Register is record
      --  Update interrupt enable
      UIE            : Boolean := False;
      --  Capture/Compare 1 interrupt enable
      CC1IE          : Boolean := False;
      --  Capture/Compare 2 interrupt enable
      CC2IE          : Boolean := False;
      --  Capture/Compare 3 interrupt enable
      CC3IE          : Boolean := False;
      --  Capture/Compare 4 interrupt enable
      CC4IE          : Boolean := False;
      --  COM interrupt enable
      COMIE          : Boolean := False;
      --  Trigger interrupt enable
      TIE            : Boolean := False;
      --  Break interrupt enable
      BIE            : Boolean := False;
      --  Update DMA request enable
      UDE            : Boolean := False;
      --  Capture/Compare 1 DMA request enable
      CC1DE          : Boolean := False;
      --  Capture/Compare 2 DMA request enable
      CC2DE          : Boolean := False;
      --  Capture/Compare 3 DMA request enable
      CC3DE          : Boolean := False;
      --  Capture/Compare 4 DMA request enable
      CC4DE          : Boolean := False;
      --  COM DMA request enable
      COMDE          : Boolean := False;
      --  Trigger DMA request enable
      TDE            : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 16,
          Bit_Order => System.Low_Order_First;

   for TIM1_DIER_Register use record
      UIE            at 0 range 0 .. 0;
      CC1IE          at 0 range 1 .. 1;
      CC2IE          at 0 range 2 .. 2;
      CC3IE          at 0 range 3 .. 3;
      CC4IE          at 0 range 4 .. 4;
      COMIE          at 0 range 5 .. 5;
      TIE            at 0 range 6 .. 6;
      BIE            at 0 range 7 .. 7;
      UDE            at 0 range 8 .. 8;
      CC1DE          at 0 range 9 .. 9;
      CC2DE          at 0 range 10 .. 10;
      CC3DE          at 0 range 11 .. 11;
      CC4DE          at 0 range 12 .. 12;
      COMDE          at 0 range 13 .. 13;
      TDE            at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
   end record;

   --  TIM1 status register
   type TIM1_SR_Register is record
      --  Update interrupt flag This bit is set by hardware on an update event.
      --  It is cleared by software. At overflow or underflow regarding the
      --  repetition counter value (update if repetition counter = 0) and if
      --  the UDIS=0 in the TIMx_CR1 register. When CNT is reinitialized by
      --  software using the UG bit in TIMx_EGR register, if URS=0 and UDIS=0
      --  in the TIMx_CR1 register. When CNT is reinitialized by a trigger
      --  event (refer to Section122.4.3: TIM1 slave mode control register
      --  (TIM1_SMCR)), if URS=0 and UDIS=0 in the TIMx_CR1 register.
      UIF            : Boolean := False;
      --  Capture/Compare 1 interrupt flag This flag is set by hardware. It is
      --  cleared by software (input capture or output compare mode) or by
      --  reading the TIMx_CCR1 register (input capture mode only). If channel
      --  CC1 is configured as output: this flag is set when the content of the
      --  counter TIMx_CNT matches the content of the TIMx_CCR1 register. When
      --  the content of TIMx_CCR1 is greater than the content of TIMx_ARR, the
      --  CC1IF bit goes high on the counter overflow (in up-counting and
      --  up/down-counting modes) or underflow (in down-counting mode). There
      --  are 3 possible options for flag setting in center-aligned mode, refer
      --  to the CMS bits in the TIMx_CR1 register for the full description. If
      --  channel CC1 is configured as input: this bit is set when counter
      --  value has been captured in TIMx_CCR1 register (an edge has been
      --  detected on IC1, as per the edge sensitivity defined with the CC1P
      --  and CC1NP bits setting, in TIMx_CCER).
      CC1IF          : Boolean := False;
      --  Capture/Compare 2 interrupt flag Refer to CC1IF description
      CC2IF          : Boolean := False;
      --  Capture/Compare 3 interrupt flag Refer to CC1IF description
      CC3IF          : Boolean := False;
      --  Capture/Compare 4 interrupt flag Refer to CC1IF description
      CC4IF          : Boolean := False;
      --  COM interrupt flag This flag is set by hardware on COM event (when
      --  Capture/compare Control bits - CCxE, CCxNE, OCxM - have been
      --  updated). It is cleared by software.
      COMIF          : Boolean := False;
      --  Trigger interrupt flag This flag is set by hardware on the TRG
      --  trigger event (active edge detected on TRGI input when the slave mode
      --  controller is enabled in all modes but gated mode. It is set when the
      --  counter starts or stops when gated mode is selected. It is cleared by
      --  software.
      TIF            : Boolean := False;
      --  Break interrupt flag This flag is set by hardware as soon as the
      --  break input goes active. It can be cleared by software if the break
      --  input is not active.
      BIF            : Boolean := False;
      --  Break 2 interrupt flag This flag is set by hardware as soon as the
      --  break 2 input goes active. It can be cleared by software if the break
      --  2 input is not active.
      B2IF           : Boolean := False;
      --  Capture/Compare 1 overcapture flag This flag is set by hardware only
      --  when the corresponding channel is configured in input capture mode.
      --  It is cleared by software by writing it to 0.
      CC1OF          : Boolean := False;
      --  Capture/Compare 2 overcapture flag Refer to CC1OF description
      CC2OF          : Boolean := False;
      --  Capture/Compare 3 overcapture flag Refer to CC1OF description
      CC3OF          : Boolean := False;
      --  Capture/Compare 4 overcapture flag Refer to CC1OF description
      CC4OF          : Boolean := False;
      --  System Break interrupt flag This flag is set by hardware as soon as
      --  the system break input goes active. It can be cleared by software if
      --  the system break input is not active. This flag must be reset to
      --  re-start PWM operation.
      SBIF           : Boolean := False;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  Compare 5 interrupt flag Refer to CC1IF description (Note: Channel 5
      --  can only be configured as output)
      CC5IF          : Boolean := False;
      --  Compare 6 interrupt flag Refer to CC1IF description (Note: Channel 6
      --  can only be configured as output)
      CC6IF          : Boolean := False;
      --  unspecified
      Reserved_18_31 : HAL.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIM1_SR_Register use record
      UIF            at 0 range 0 .. 0;
      CC1IF          at 0 range 1 .. 1;
      CC2IF          at 0 range 2 .. 2;
      CC3IF          at 0 range 3 .. 3;
      CC4IF          at 0 range 4 .. 4;
      COMIF          at 0 range 5 .. 5;
      TIF            at 0 range 6 .. 6;
      BIF            at 0 range 7 .. 7;
      B2IF           at 0 range 8 .. 8;
      CC1OF          at 0 range 9 .. 9;
      CC2OF          at 0 range 10 .. 10;
      CC3OF          at 0 range 11 .. 11;
      CC4OF          at 0 range 12 .. 12;
      SBIF           at 0 range 13 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      CC5IF          at 0 range 16 .. 16;
      CC6IF          at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   --  TIM1 event generation register
   type TIM1_EGR_Register is record
      --  Write-only. Update generation This bit can be set by software, it is
      --  automatically cleared by hardware.
      UG            : Boolean := False;
      --  Write-only. Capture/Compare 1 generation This bit is set by software
      --  in order to generate an event, it is automatically cleared by
      --  hardware. If channel CC1 is configured as output: CC1IF flag is set,
      --  Corresponding interrupt or DMA request is sent if enabled. If channel
      --  CC1 is configured as input: The current value of the counter is
      --  captured in TIMx_CCR1 register. The CC1IF flag is set, the
      --  corresponding interrupt or DMA request is sent if enabled. The CC1OF
      --  flag is set if the CC1IF flag was already high.
      CC1G          : Boolean := False;
      --  Write-only. Capture/Compare 2 generation Refer to CC1G description
      CC2G          : Boolean := False;
      --  Write-only. Capture/Compare 3 generation Refer to CC1G description
      CC3G          : Boolean := False;
      --  Write-only. Capture/Compare 4 generation Refer to CC1G description
      CC4G          : Boolean := False;
      --  Write-only. Capture/Compare control update generation This bit can be
      --  set by software, it is automatically cleared by hardware Note: This
      --  bit acts only on channels having a complementary output.
      COMG          : Boolean := False;
      --  Write-only. Trigger generation This bit is set by software in order
      --  to generate an event, it is automatically cleared by hardware.
      TG            : Boolean := False;
      --  Write-only. Break generation This bit is set by software in order to
      --  generate an event, it is automatically cleared by hardware.
      BG            : Boolean := False;
      --  Write-only. Break 2 generation This bit is set by software in order
      --  to generate an event, it is automatically cleared by hardware.
      B2G           : Boolean := False;
      --  unspecified
      Reserved_9_15 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 16,
          Bit_Order => System.Low_Order_First;

   for TIM1_EGR_Register use record
      UG            at 0 range 0 .. 0;
      CC1G          at 0 range 1 .. 1;
      CC2G          at 0 range 2 .. 2;
      CC3G          at 0 range 3 .. 3;
      CC4G          at 0 range 4 .. 4;
      COMG          at 0 range 5 .. 5;
      TG            at 0 range 6 .. 6;
      BG            at 0 range 7 .. 7;
      B2G           at 0 range 8 .. 8;
      Reserved_9_15 at 0 range 9 .. 15;
   end record;

   subtype TIM1_CCMR1_INPUT_CC1S_Field is HAL.UInt2;
   subtype TIM1_CCMR1_INPUT_IC1PSC_Field is HAL.UInt2;
   subtype TIM1_CCMR1_INPUT_IC1F_Field is HAL.UInt4;
   subtype TIM1_CCMR1_INPUT_CC2S_Field is HAL.UInt2;
   subtype TIM1_CCMR1_INPUT_IC2PSC_Field is HAL.UInt2;
   subtype TIM1_CCMR1_INPUT_IC2F_Field is HAL.UInt4;

   --  TIM1 capture/compare mode register 1
   type TIM1_CCMR1_INPUT_Register is record
      --  Capture/Compare 1 Selection This bit-field defines the direction of
      --  the channel (input/output) as well as the used input. Note: CC1S bits
      --  are writable only when the channel is OFF (CC1E = 0 in TIMx_CCER).
      CC1S           : TIM1_CCMR1_INPUT_CC1S_Field := 16#0#;
      --  Input capture 1 prescaler This bit-field defines the ratio of the
      --  prescaler acting on CC1 input (IC1). The prescaler is reset as soon
      --  as CC1E=0 (TIMx_CCER register).
      IC1PSC         : TIM1_CCMR1_INPUT_IC1PSC_Field := 16#0#;
      --  Input capture 1 filter This bit-field defines the frequency used to
      --  sample TI1 input and the length of the digital filter applied to TI1.
      --  The digital filter is made of an event counter in which N consecutive
      --  events are needed to validate a transition on the output:
      IC1F           : TIM1_CCMR1_INPUT_IC1F_Field := 16#0#;
      --  Capture/Compare 2 selection This bit-field defines the direction of
      --  the channel (input/output) as well as the used input. Note: CC2S bits
      --  are writable only when the channel is OFF (CC2E = 0 in TIMx_CCER).
      CC2S           : TIM1_CCMR1_INPUT_CC2S_Field := 16#0#;
      --  Input capture 2 prescaler Refer to IC1PSC[1:0] description.
      IC2PSC         : TIM1_CCMR1_INPUT_IC2PSC_Field := 16#0#;
      --  Input capture 2 filter Refer to IC1F[3:0] description.
      IC2F           : TIM1_CCMR1_INPUT_IC2F_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIM1_CCMR1_INPUT_Register use record
      CC1S           at 0 range 0 .. 1;
      IC1PSC         at 0 range 2 .. 3;
      IC1F           at 0 range 4 .. 7;
      CC2S           at 0 range 8 .. 9;
      IC2PSC         at 0 range 10 .. 11;
      IC2F           at 0 range 12 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype TIM1_CCMR1_OUTPUT_CC1S_Field is HAL.UInt2;
   subtype TIM1_CCMR1_OUTPUT_OC1M_Field is HAL.UInt3;
   subtype TIM1_CCMR1_OUTPUT_CC2S_Field is HAL.UInt2;
   subtype TIM1_CCMR1_OUTPUT_OC2M_Field is HAL.UInt3;

   --  TIM1 capture/compare mode register 1
   type TIM1_CCMR1_OUTPUT_Register is record
      --  Capture/Compare 1 selection This bit-field defines the direction of
      --  the channel (input/output) as well as the used input. Note: CC1S bits
      --  are writable only when the channel is OFF (CC1E = 0 in TIMx_CCER).
      CC1S           : TIM1_CCMR1_OUTPUT_CC1S_Field := 16#0#;
      --  Output Compare 1 fast enable This bit decreases the latency between a
      --  trigger event and a transition on the timer output. It must be used
      --  in one-pulse mode (OPM bit set in TIMx_CR1 register), to have the
      --  output pulse starting as soon as possible after the starting trigger.
      OC1FE          : Boolean := False;
      --  Output Compare 1 preload enable Note: These bits can not be modified
      --  as long as LOCK level 3 has been programmed (LOCK bits in TIMx_BDTR
      --  register) and CC1S=00 (the channel is configured in output).
      OC1PE          : Boolean := False;
      --  OC1M[2:0]: Output Compare 1 mode These bits define the behavior of
      --  the output reference signal OC1REF from which OC1 and OC1N are
      --  derived. OC1REF is active high whereas OC1 and OC1N active level
      --  depends on CC1P and CC1NP bits. Note: These bits can not be modified
      --  as long as LOCK level 3 has been programmed (LOCK bits in TIMx_BDTR
      --  register) and CC1S=00 (the channel is configured in output). Note: In
      --  PWM mode, the OCREF level changes only when the result of the
      --  comparison changes or when the output compare mode switches from
      --  frozen mode to PWM mode. Note: On channels having a complementary
      --  output, this bit field is preloaded. If the CCPC bit is set in the
      --  TIMx_CR2 register then the OC1M active bits take the new value from
      --  the preloaded bits only when a COM event is generated. Note: The
      --  OC1M[3] bit is not contiguous, located in bit 16.
      OC1M           : TIM1_CCMR1_OUTPUT_OC1M_Field := 16#0#;
      --  Output Compare 1 clear enable
      OC1CE          : Boolean := False;
      --  Capture/Compare 2 selection This bit-field defines the direction of
      --  the channel (input/output) as well as the used input. Note: CC2S bits
      --  are writable only when the channel is OFF (CC2E = 0 in TIMx_CCER).
      CC2S           : TIM1_CCMR1_OUTPUT_CC2S_Field := 16#0#;
      --  Output Compare 2 fast enable Refer to OC1FE description.
      OC2FE          : Boolean := False;
      --  Output Compare 2 preload enable Refer to OC1PE description.
      OC2PE          : Boolean := False;
      --  OC2M[2:0]: Output Compare 2 mode Refer to OC1M[3:0] description.
      OC2M           : TIM1_CCMR1_OUTPUT_OC2M_Field := 16#0#;
      --  Output Compare 2 clear enable Refer to OC1CE description.
      OC2CE          : Boolean := False;
      --  OC1M[3]
      OC1M_1         : Boolean := False;
      --  unspecified
      Reserved_17_23 : HAL.UInt7 := 16#0#;
      --  OC2M[3]
      OC2M_1         : Boolean := False;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIM1_CCMR1_OUTPUT_Register use record
      CC1S           at 0 range 0 .. 1;
      OC1FE          at 0 range 2 .. 2;
      OC1PE          at 0 range 3 .. 3;
      OC1M           at 0 range 4 .. 6;
      OC1CE          at 0 range 7 .. 7;
      CC2S           at 0 range 8 .. 9;
      OC2FE          at 0 range 10 .. 10;
      OC2PE          at 0 range 11 .. 11;
      OC2M           at 0 range 12 .. 14;
      OC2CE          at 0 range 15 .. 15;
      OC1M_1         at 0 range 16 .. 16;
      Reserved_17_23 at 0 range 17 .. 23;
      OC2M_1         at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype TIM1_CCMR2_INPUT_CC3S_Field is HAL.UInt2;
   subtype TIM1_CCMR2_INPUT_IC3PSC_Field is HAL.UInt2;
   subtype TIM1_CCMR2_INPUT_IC3F_Field is HAL.UInt4;
   subtype TIM1_CCMR2_INPUT_CC4S_Field is HAL.UInt2;
   subtype TIM1_CCMR2_INPUT_IC4PSC_Field is HAL.UInt2;
   subtype TIM1_CCMR2_INPUT_IC4F_Field is HAL.UInt4;

   --  TIM1 capture/compare mode register 2
   type TIM1_CCMR2_INPUT_Register is record
      --  Capture/compare 3 selection This bit-field defines the direction of
      --  the channel (input/output) as well as the used input. Note: CC3S bits
      --  are writable only when the channel is OFF (CC3E = 0 in TIMx_CCER).
      CC3S           : TIM1_CCMR2_INPUT_CC3S_Field := 16#0#;
      --  Input capture 3 prescaler Refer to IC1PSC[1:0] description.
      IC3PSC         : TIM1_CCMR2_INPUT_IC3PSC_Field := 16#0#;
      --  Input capture 3 filter Refer to IC1F[3:0] description.
      IC3F           : TIM1_CCMR2_INPUT_IC3F_Field := 16#0#;
      --  Capture/Compare 4 selection This bit-field defines the direction of
      --  the channel (input/output) as well as the used input. Note: CC4S bits
      --  are writable only when the channel is OFF (CC4E = 0 in TIMx_CCER).
      CC4S           : TIM1_CCMR2_INPUT_CC4S_Field := 16#0#;
      --  Input capture 4 prescaler Refer to IC1PSC[1:0] description.
      IC4PSC         : TIM1_CCMR2_INPUT_IC4PSC_Field := 16#0#;
      --  Input capture 4 filter Refer to IC1F[3:0] description.
      IC4F           : TIM1_CCMR2_INPUT_IC4F_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIM1_CCMR2_INPUT_Register use record
      CC3S           at 0 range 0 .. 1;
      IC3PSC         at 0 range 2 .. 3;
      IC3F           at 0 range 4 .. 7;
      CC4S           at 0 range 8 .. 9;
      IC4PSC         at 0 range 10 .. 11;
      IC4F           at 0 range 12 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype TIM1_CCMR2_OUTPUT_CC3S_Field is HAL.UInt2;
   subtype TIM1_CCMR2_OUTPUT_OC3M_Field is HAL.UInt3;
   subtype TIM1_CCMR2_OUTPUT_CC4S_Field is HAL.UInt2;
   subtype TIM1_CCMR2_OUTPUT_OC4M_Field is HAL.UInt3;

   --  TIM1 capture/compare mode register 1
   type TIM1_CCMR2_OUTPUT_Register is record
      --  Capture/Compare 3 selection This bit-field defines the direction of
      --  the channel (input/output) as well as the used input. Note: CC3S bits
      --  are writable only when the channel is OFF (CC3E = 0 in TIMx_CCER).
      CC3S           : TIM1_CCMR2_OUTPUT_CC3S_Field := 16#0#;
      --  Output compare 3 fast enable Refer to OC1FE description.
      OC3FE          : Boolean := False;
      --  Output compare 3 preload enable Refer to OC1PE description.
      OC3PE          : Boolean := False;
      --  OC3M[2:0]: Output compare 3 mode Refer to OC1M[3:0] description.
      OC3M           : TIM1_CCMR2_OUTPUT_OC3M_Field := 16#0#;
      --  Output compare 3 clear enable Refer to OC1CE description.
      OC3CE          : Boolean := False;
      --  Capture/Compare 4 selection This bit-field defines the direction of
      --  the channel (input/output) as well as the used input. Note: CC4S bits
      --  are writable only when the channel is OFF (CC4E = 0 in TIMx_CCER).
      CC4S           : TIM1_CCMR2_OUTPUT_CC4S_Field := 16#0#;
      --  Output compare 4 fast enable Refer to OC1FE description.
      OC4FE          : Boolean := False;
      --  Output compare 4 preload enable Refer to OC1PE description.
      OC4PE          : Boolean := False;
      --  OC4M[2:0]: Output compare 4 mode Refer to OC3M[3:0] description.
      OC4M           : TIM1_CCMR2_OUTPUT_OC4M_Field := 16#0#;
      --  Output compare 4 clear enable Refer to OC1CE description.
      OC4CE          : Boolean := False;
      --  OC3M[3]
      OC3M_1         : Boolean := False;
      --  unspecified
      Reserved_17_23 : HAL.UInt7 := 16#0#;
      --  OC4M[3]
      OC4M_1         : Boolean := False;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIM1_CCMR2_OUTPUT_Register use record
      CC3S           at 0 range 0 .. 1;
      OC3FE          at 0 range 2 .. 2;
      OC3PE          at 0 range 3 .. 3;
      OC3M           at 0 range 4 .. 6;
      OC3CE          at 0 range 7 .. 7;
      CC4S           at 0 range 8 .. 9;
      OC4FE          at 0 range 10 .. 10;
      OC4PE          at 0 range 11 .. 11;
      OC4M           at 0 range 12 .. 14;
      OC4CE          at 0 range 15 .. 15;
      OC3M_1         at 0 range 16 .. 16;
      Reserved_17_23 at 0 range 17 .. 23;
      OC4M_1         at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  TIM1 capture/compare enable register
   type TIM1_CCER_Register is record
      --  Capture/Compare 1 output enable When CC1 channel is configured as
      --  output, the OC1 level depends on MOE, OSSI, OSSR, OIS1, OIS1N and
      --  CC1NE bits, regardless of the CC1E bits state. Refer to Table1119 for
      --  details. Note: On channels having a complementary output, this bit is
      --  preloaded. If the CCPC bit is set in the TIMx_CR2 register then the
      --  CC1E active bit takes the new value from the preloaded bit only when
      --  a Commutation event is generated.
      CC1E           : Boolean := False;
      --  Capture/Compare 1 output polarity When CC1 channel is configured as
      --  input, both CC1NP/CC1P bits select the active polarity of TI1FP1 and
      --  TI2FP1 for trigger or capture operations. CC1NP=0,
      --  CC1P=0:	non-inverted/rising edge. The circuit is sensitive to TIxFP1
      --  rising edge (capture or trigger operations in reset, external clock
      --  or trigger mode), TIxFP1 is not inverted (trigger operation in gated
      --  mode or encoder mode). CC1NP=0, CC1P=1:	inverted/falling edge. The
      --  circuit is sensitive to TIxFP1 falling edge (capture or trigger
      --  operations in reset, external clock or trigger mode), TIxFP1 is
      --  inverted (trigger operation in gated mode or encoder mode). CC1NP=1,
      --  CC1P=1:	non-inverted/both edges/ The circuit is sensitive to both
      --  TIxFP1 rising and falling edges (capture or trigger operations in
      --  reset, external clock or trigger mode), TIxFP1is not inverted
      --  (trigger operation in gated mode). This configuration must not be
      --  used in encoder mode. CC1NP=1, CC1P=0:	The configuration is reserved,
      --  it must not be used. Note: This bit is not writable as soon as LOCK
      --  level 2 or 3 has been programmed (LOCK bits in TIMx_BDTR register).
      --  Note: On channels having a complementary output, this bit is
      --  preloaded. If the CCPC bit is set in the TIMx_CR2 register then the
      --  CC1P active bit takes the new value from the preloaded bit only when
      --  a Commutation event is generated.
      CC1P           : Boolean := False;
      --  Capture/Compare 1 complementary output enable On channels having a
      --  complementary output, this bit is preloaded. If the CCPC bit is set
      --  in the TIMx_CR2 register then the CC1NE active bit takes the new
      --  value from the preloaded bit only when a Commutation event is
      --  generated.
      CC1NE          : Boolean := False;
      --  Capture/Compare 1 complementary output polarity CC1 channel
      --  configured as output: CC1 channel configured as input: This bit is
      --  used in conjunction with CC1P to define the polarity of TI1FP1 and
      --  TI2FP1. Refer to CC1P description. Note: This bit is not writable as
      --  soon as LOCK level 2 or 3 has been programmed (LOCK bits in TIMx_BDTR
      --  register) and CC1S=00 (channel configured as output). On channels
      --  having a complementary output, this bit is preloaded. If the CCPC bit
      --  is set in the TIMx_CR2 register then the CC1NP active bit takes the
      --  new value from the preloaded bit only when a Commutation event is
      --  generated.
      CC1NP          : Boolean := False;
      --  Capture/Compare 2 output enable Refer to CC1E description
      CC2E           : Boolean := False;
      --  Capture/Compare 2 output polarity Refer to CC1P description
      CC2P           : Boolean := False;
      --  Capture/Compare 2 complementary output enable Refer to CC1NE
      --  description
      CC2NE          : Boolean := False;
      --  Capture/Compare 2 complementary output polarity Refer to CC1NP
      --  description
      CC2NP          : Boolean := False;
      --  Capture/Compare 3 output enable Refer to CC1E description
      CC3E           : Boolean := False;
      --  Capture/Compare 3 output polarity Refer to CC1P description
      CC3P           : Boolean := False;
      --  Capture/Compare 3 complementary output enable Refer to CC1NE
      --  description
      CC3NE          : Boolean := False;
      --  Capture/Compare 3 complementary output polarity Refer to CC1NP
      --  description
      CC3NP          : Boolean := False;
      --  Capture/Compare 4 output enable Refer to CC1E description
      CC4E           : Boolean := False;
      --  Capture/Compare 4 output polarity Refer to CC1P description
      CC4P           : Boolean := False;
      --  unspecified
      Reserved_14_14 : HAL.Bit := 16#0#;
      --  Capture/Compare 4 complementary output polarity Refer to CC1NP
      --  description
      CC4NP          : Boolean := False;
      --  Capture/Compare 5 output enable Refer to CC1E description
      CC5E           : Boolean := False;
      --  Capture/Compare 5 output polarity Refer to CC1P description
      CC5P           : Boolean := False;
      --  unspecified
      Reserved_18_19 : HAL.UInt2 := 16#0#;
      --  Capture/Compare 6 output enable Refer to CC1E description
      CC6E           : Boolean := False;
      --  Capture/Compare 6 output polarity Refer to CC1P description
      CC6P           : Boolean := False;
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIM1_CCER_Register use record
      CC1E           at 0 range 0 .. 0;
      CC1P           at 0 range 1 .. 1;
      CC1NE          at 0 range 2 .. 2;
      CC1NP          at 0 range 3 .. 3;
      CC2E           at 0 range 4 .. 4;
      CC2P           at 0 range 5 .. 5;
      CC2NE          at 0 range 6 .. 6;
      CC2NP          at 0 range 7 .. 7;
      CC3E           at 0 range 8 .. 8;
      CC3P           at 0 range 9 .. 9;
      CC3NE          at 0 range 10 .. 10;
      CC3NP          at 0 range 11 .. 11;
      CC4E           at 0 range 12 .. 12;
      CC4P           at 0 range 13 .. 13;
      Reserved_14_14 at 0 range 14 .. 14;
      CC4NP          at 0 range 15 .. 15;
      CC5E           at 0 range 16 .. 16;
      CC5P           at 0 range 17 .. 17;
      Reserved_18_19 at 0 range 18 .. 19;
      CC6E           at 0 range 20 .. 20;
      CC6P           at 0 range 21 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   subtype TIM1_CNT_CNT_Field is HAL.UInt16;

   --  TIM1 counter
   type TIM1_CNT_Register is record
      --  Counter value
      CNT            : TIM1_CNT_CNT_Field := 16#0#;
      --  unspecified
      Reserved_16_30 : HAL.UInt15 := 16#0#;
      --  Read-only. UIF copy This bit is a read-only copy of the UIF bit of
      --  the TIMx_ISR register. If the UIFREMAP bit in the TIMxCR1 is reset,
      --  bit 31 is reserved and read at 0.
      UIFCPY         : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIM1_CNT_Register use record
      CNT            at 0 range 0 .. 15;
      Reserved_16_30 at 0 range 16 .. 30;
      UIFCPY         at 0 range 31 .. 31;
   end record;

   subtype TIM1_BDTR_DTG_Field is HAL.UInt8;
   subtype TIM1_BDTR_LOCK_Field is HAL.UInt2;
   subtype TIM1_BDTR_BKF_Field is HAL.UInt4;
   subtype TIM1_BDTR_BK2F_Field is HAL.UInt4;

   --  TIM1 break and dead-time register
   type TIM1_BDTR_Register is record
      --  Dead-time generator setup
      DTG            : TIM1_BDTR_DTG_Field := 16#0#;
      --  Lock configuration These bits offer a write protection against
      --  software errors. Note: The LOCK bits can be written only once after
      --  the reset. Once the TIMx_BDTR register has been written, their
      --  content is frozen until the next reset.
      LOCK           : TIM1_BDTR_LOCK_Field := 16#0#;
      --  Off-state selection for Idle mode This bit is used when MOE=0 due to
      --  a break event or by a software write, on channels configured as
      --  outputs. See OC/OCN enable description for more details
      --  (Section122.4.11: TIM1 capture/compare enable register (TIM1_CCER)).
      --  Note: This bit can not be modified as soon as the LOCK level 2 has
      --  been programmed (LOCK bits in TIMx_BDTR register).
      OSSI           : Boolean := False;
      --  Off-state selection for Run mode This bit is used when MOE=1 on
      --  channels having a complementary output which are configured as
      --  outputs. OSSR is not implemented if no complementary output is
      --  implemented in the timer. See OC/OCN enable description for more
      --  details (Section122.4.11: TIM1 capture/compare enable register
      --  (TIM1_CCER)). Note: This bit can not be modified as soon as the LOCK
      --  level 2 has been programmed (LOCK bits in TIMx_BDTR register).
      OSSR           : Boolean := False;
      --  Break enable This bit enables the complete break protection
      --  (including all sources connected to bk_acth and BRK sources, as per
      --  Figure1152: Break and Break2 circuitry overview). Note: This bit
      --  cannot be modified when LOCK level 1 has been programmed (LOCK bits
      --  in TIMx_BDTR register). Note: Any write operation to this bit takes a
      --  delay of 1 APB clock cycle to become effective.
      BKE            : Boolean := False;
      --  Break polarity Note: This bit can not be modified as long as LOCK
      --  level 1 has been programmed (LOCK bits in TIMx_BDTR register). Note:
      --  Any write operation to this bit takes a delay of 1 APB clock cycle to
      --  become effective.
      BKP            : Boolean := False;
      --  Automatic output enable Note: This bit can not be modified as long as
      --  LOCK level 1 has been programmed (LOCK bits in TIMx_BDTR register).
      AOE            : Boolean := False;
      --  Main output enable This bit is cleared asynchronously by hardware as
      --  soon as one of the break inputs is active (BRK or BRK2). It is set by
      --  software or automatically depending on the AOE bit. It is acting only
      --  on the channels which are configured in output. In response to a
      --  break event or if MOE is written to 0: OC and OCN outputs are
      --  disabled or forced to idle state depending on the OSSI bit. See
      --  OC/OCN enable description for more details (Section122.4.11: TIM1
      --  capture/compare enable register (TIM1_CCER)).
      MOE            : Boolean := False;
      --  Break filter This bit-field defines the frequency used to sample BRK
      --  input and the length of the digital filter applied to BRK. The
      --  digital filter is made of an event counter in which N consecutive
      --  events are needed to validate a transition on the output: Note: This
      --  bit cannot be modified when LOCK level 1 has been programmed (LOCK
      --  bits in TIMx_BDTR register).
      BKF            : TIM1_BDTR_BKF_Field := 16#0#;
      --  Break 2 filter This bit-field defines the frequency used to sample
      --  BRK2 input and the length of the digital filter applied to BRK2. The
      --  digital filter is made of an event counter in which N consecutive
      --  events are needed to validate a transition on the output: Note: This
      --  bit cannot be modified when LOCK level 1 has been programmed (LOCK
      --  bits in TIMx_BDTR register).
      BK2F           : TIM1_BDTR_BK2F_Field := 16#0#;
      --  Break 2 enable Note: The BRK2 must only be used with OSSR = OSSI = 1.
      --  Note: This bit cannot be modified when LOCK level 1 has been
      --  programmed (LOCK bits in TIMx_BDTR register). Note: Any write
      --  operation to this bit takes a delay of 1 APB clock cycle to become
      --  effective.
      BK2E           : Boolean := False;
      --  Break 2 polarity Note: This bit cannot be modified as long as LOCK
      --  level 1 has been programmed (LOCK bits in TIMx_BDTR register). Note:
      --  Any write operation to this bit takes a delay of 1 APB clock cycle to
      --  become effective.
      BK2P           : Boolean := False;
      --  Break Disarm This bit is cleared by hardware when no break source is
      --  active. The BKDSRM bit must be set by software to release the
      --  bidirectional output control (open-drain output in Hi-Z state) and
      --  then be polled it until it is reset by hardware, indicating that the
      --  fault condition has disappeared. Note: Any write operation to this
      --  bit takes a delay of 1 APB clock cycle to become effective.
      BKDSRM         : Boolean := False;
      --  Break2 Disarm Refer to BKDSRM description
      BK2DSRM        : Boolean := False;
      --  Break Bidirectional In the bidirectional mode (BKBID bit set to 1),
      --  the break input is configured both in input mode and in open drain
      --  output mode. Any active break event asserts a low logic level on the
      --  Break input to indicate an internal break event to external devices.
      --  Note: This bit cannot be modified as long as LOCK level 1 has been
      --  programmed (LOCK bits in TIMx_BDTR register). Note: Any write
      --  operation to this bit takes a delay of 1 APB clock cycle to become
      --  effective.
      BKBID          : Boolean := False;
      --  Break2 bidirectional Refer to BKBID description
      BK2BID         : Boolean := False;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIM1_BDTR_Register use record
      DTG            at 0 range 0 .. 7;
      LOCK           at 0 range 8 .. 9;
      OSSI           at 0 range 10 .. 10;
      OSSR           at 0 range 11 .. 11;
      BKE            at 0 range 12 .. 12;
      BKP            at 0 range 13 .. 13;
      AOE            at 0 range 14 .. 14;
      MOE            at 0 range 15 .. 15;
      BKF            at 0 range 16 .. 19;
      BK2F           at 0 range 20 .. 23;
      BK2E           at 0 range 24 .. 24;
      BK2P           at 0 range 25 .. 25;
      BKDSRM         at 0 range 26 .. 26;
      BK2DSRM        at 0 range 27 .. 27;
      BKBID          at 0 range 28 .. 28;
      BK2BID         at 0 range 29 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype TIM1_DCR_DBA_Field is HAL.UInt5;
   subtype TIM1_DCR_DBL_Field is HAL.UInt5;

   --  TIM1 DMA control register
   type TIM1_DCR_Register is record
      --  DMA base address This 5-bits vector defines the base-address for DMA
      --  transfers (when read/write access are done through the TIMx_DMAR
      --  address). DBA is defined as an offset starting from the address of
      --  the TIMx_CR1 register. Example: ...
      DBA            : TIM1_DCR_DBA_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  DMA burst length This 5-bit vector defines the length of DMA
      --  transfers (the timer recognizes a burst transfer when a read or a
      --  write access is done to the TIMx_DMAR address), i.e. the number of
      --  transfers. Transfers can be in half-words or in bytes (see example
      --  below). ... Example: Let us consider the following transfer: DBL = 7
      --  bytes & DBA = TIMx_CR1. If DBL = 7 bytes and DBA = TIMx_CR1
      --  represents the address of the byte to be transferred, the address of
      --  the transfer should be given by the following equation: (TIMx_CR1
      --  address) + DBA + (DMA index), where DMA index = DBL In this example,
      --  7 bytes are added to (TIMx_CR1 address) + DBA, which gives us the
      --  address from/to which the data is copied. In this case, the transfer
      --  is done to 7 registers starting from the following address: (TIMx_CR1
      --  address) + DBA According to the configuration of the DMA Data Size,
      --  several cases may occur: If the DMA Data Size is configured in
      --  half-words, 16-bit data is transferred to each of the 7 registers. If
      --  the DMA Data Size is configured in bytes, the data is also
      --  transferred to 7 registers: the first register contains the first MSB
      --  byte, the second register, the first LSB byte and so on. So with the
      --  transfer Timer, one also has to specify the size of data transferred
      --  by DMA.
      DBL            : TIM1_DCR_DBL_Field := 16#0#;
      --  unspecified
      Reserved_13_15 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 16,
          Bit_Order => System.Low_Order_First;

   for TIM1_DCR_Register use record
      DBA            at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      DBL            at 0 range 8 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
   end record;

   subtype TIM1_OR1_OCREF_CLR_Field is HAL.UInt2;

   --  TIM1 option register 1
   type TIM1_OR1_Register is record
      --  Ocref_clr source selection This bit selects the ocref_clr input
      --  source. Others: Reserved Note: COMP3 is available on STM32G0B1xx and
      --  STM32G0C1xx salestypes only.
      OCREF_CLR     : TIM1_OR1_OCREF_CLR_Field := 16#0#;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIM1_OR1_Register use record
      OCREF_CLR     at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   subtype TIM1_CCMR3_OC5M_Field is HAL.UInt3;
   subtype TIM1_CCMR3_OC6M_Field is HAL.UInt3;

   --  TIM1 capture/compare mode register 3
   type TIM1_CCMR3_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  Output compare 5 fast enable Refer to OC1FE description.
      OC5FE          : Boolean := False;
      --  Output compare 5 preload enable Refer to OC1PE description.
      OC5PE          : Boolean := False;
      --  OC5M[0]: Output compare 5 mode Refer to OC1M description.
      OC5M           : TIM1_CCMR3_OC5M_Field := 16#0#;
      --  Output compare 5 clear enable Refer to OC1CE description.
      OC5CE          : Boolean := False;
      --  unspecified
      Reserved_8_9   : HAL.UInt2 := 16#0#;
      --  Output compare 6 fast enable Refer to OC1FE description.
      OC6FE          : Boolean := False;
      --  Output compare 6 preload enable Refer to OC1PE description.
      OC6PE          : Boolean := False;
      --  OC6M[0]: Output compare 6 mode Refer to OC1M description.
      OC6M           : TIM1_CCMR3_OC6M_Field := 16#0#;
      --  Output compare 6 clear enable Refer to OC1CE description.
      OC6CE          : Boolean := False;
      --  OC5M[3]
      OC5M_1         : Boolean := False;
      --  unspecified
      Reserved_17_23 : HAL.UInt7 := 16#0#;
      --  OC6M[3]
      OC6M_1         : Boolean := False;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIM1_CCMR3_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      OC5FE          at 0 range 2 .. 2;
      OC5PE          at 0 range 3 .. 3;
      OC5M           at 0 range 4 .. 6;
      OC5CE          at 0 range 7 .. 7;
      Reserved_8_9   at 0 range 8 .. 9;
      OC6FE          at 0 range 10 .. 10;
      OC6PE          at 0 range 11 .. 11;
      OC6M           at 0 range 12 .. 14;
      OC6CE          at 0 range 15 .. 15;
      OC5M_1         at 0 range 16 .. 16;
      Reserved_17_23 at 0 range 17 .. 23;
      OC6M_1         at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype TIM1_CCR5_CCR5_Field is HAL.UInt16;

   --  TIM1_CCR5_GC5C array
   type TIM1_CCR5_GC5C_Field_Array is array (1 .. 3) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for TIM1_CCR5_GC5C
   type TIM1_CCR5_GC5C_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  GC5C as a value
            Val : HAL.UInt3;
         when True =>
            --  GC5C as an array
            Arr : TIM1_CCR5_GC5C_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for TIM1_CCR5_GC5C_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  TIM1 capture/compare register 5
   type TIM1_CCR5_Register is record
      --  Capture/Compare 5 value CCR5 is the value to be loaded in the actual
      --  capture/compare 5 register (preload value). It is loaded permanently
      --  if the preload feature is not selected in the TIMx_CCMR3 register
      --  (bit OC5PE). Else the preload value is copied in the active
      --  capture/compare 5 register when an update event occurs. The active
      --  capture/compare register contains the value to be compared to the
      --  counter TIMx_CNT and signaled on OC5 output.
      CCR5           : TIM1_CCR5_CCR5_Field := 16#0#;
      --  unspecified
      Reserved_16_28 : HAL.UInt13 := 16#0#;
      --  Group Channel 5 and Channel 1 Distortion on Channel 1 output: This
      --  bit can either have immediate effect or be preloaded and taken into
      --  account after an update event (if preload feature is selected in
      --  TIMxCCMR1). Note: it is also possible to apply this distortion on
      --  combined PWM signals.
      GC5C           : TIM1_CCR5_GC5C_Field :=
                        (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIM1_CCR5_Register use record
      CCR5           at 0 range 0 .. 15;
      Reserved_16_28 at 0 range 16 .. 28;
      GC5C           at 0 range 29 .. 31;
   end record;

   subtype TIM1_AF1_ETRSEL_Field is HAL.UInt4;

   --  TIM1 alternate function option register 1
   type TIM1_AF1_Register is record
      --  BRK BKIN input enable This bit enables the BKIN alternate function
      --  input for the timers BRK input. BKIN input is ORed with the other BRK
      --  sources. Note: This bit can not be modified as long as LOCK level 1
      --  has been programmed (LOCK bits in TIMx_BDTR register).
      BKINE          : Boolean := True;
      --  BRK COMP1 enable This bit enables the COMP1 for the timers BRK input.
      --  COMP1 output is ORed with the other BRK sources. Note: This bit can
      --  not be modified as long as LOCK level 1 has been programmed (LOCK
      --  bits in TIMx_BDTR register).
      BKCMP1E        : Boolean := False;
      --  BRK COMP2 enable This bit enables the COMP2 for the timers BRK input.
      --  COMP2 output is ORed with the other BRK sources. Note: This bit can
      --  not be modified as long as LOCK level 1 has been programmed (LOCK
      --  bits in TIMx_BDTR register).
      BKCMP2E        : Boolean := False;
      --  unspecified
      Reserved_3_8   : HAL.UInt6 := 16#0#;
      --  BRK BKIN input polarity This bit selects the BKIN alternate function
      --  input sensitivity. It must be programmed together with the BKP
      --  polarity bit. Note: This bit can not be modified as long as LOCK
      --  level 1 has been programmed (LOCK bits in TIMx_BDTR register).
      BKINP          : Boolean := False;
      --  BRK COMP1 input polarity This bit selects the COMP1 input
      --  sensitivity. It must be programmed together with the BKP polarity
      --  bit. Note: This bit can not be modified as long as LOCK level 1 has
      --  been programmed (LOCK bits in TIMx_BDTR register).
      BKCMP1P        : Boolean := False;
      --  BRK COMP2 input polarity This bit selects the COMP2 input
      --  sensitivity. It must be programmed together with the BKP polarity
      --  bit. Note: This bit can not be modified as long as LOCK level 1 has
      --  been programmed (LOCK bits in TIMx_BDTR register).
      BKCMP2P        : Boolean := False;
      --  unspecified
      Reserved_12_13 : HAL.UInt2 := 16#0#;
      --  ETR source selection These bits select the ETR input source. Others:
      --  Reserved Note: These bits can not be modified as long as LOCK level 1
      --  has been programmed (LOCK bits in TIMx_BDTR register).
      ETRSEL         : TIM1_AF1_ETRSEL_Field := 16#0#;
      --  unspecified
      Reserved_18_31 : HAL.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIM1_AF1_Register use record
      BKINE          at 0 range 0 .. 0;
      BKCMP1E        at 0 range 1 .. 1;
      BKCMP2E        at 0 range 2 .. 2;
      Reserved_3_8   at 0 range 3 .. 8;
      BKINP          at 0 range 9 .. 9;
      BKCMP1P        at 0 range 10 .. 10;
      BKCMP2P        at 0 range 11 .. 11;
      Reserved_12_13 at 0 range 12 .. 13;
      ETRSEL         at 0 range 14 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   --  TIM1 Alternate function register 2
   type TIM1_AF2_Register is record
      --  BRK2 BKIN input enable This bit enables the BKIN2 alternate function
      --  input for the timers BRK2 input. BKIN2 input is ORed with the other
      --  BRK2 sources. Note: This bit can not be modified as long as LOCK
      --  level 1 has been programmed (LOCK bits in TIMx_BDTR register).
      BK2INE         : Boolean := True;
      --  BRK2 COMP1 enable This bit enables the COMP1 for the timers BRK2
      --  input. COMP1 output is ORed with the other BRK2 sources. Note: This
      --  bit can not be modified as long as LOCK level 1 has been programmed
      --  (LOCK bits in TIMx_BDTR register).
      BK2CMP1E       : Boolean := False;
      --  BRK2 COMP2 enable This bit enables the COMP2 for the timers BRK2
      --  input. COMP2 output is ORed with the other BRK2 sources. Note: This
      --  bit can not be modified as long as LOCK level 1 has been programmed
      --  (LOCK bits in TIMx_BDTR register).
      BK2CMP2E       : Boolean := False;
      --  unspecified
      Reserved_3_8   : HAL.UInt6 := 16#0#;
      --  BRK2 BKIN2 input polarity This bit selects the BKIN2 alternate
      --  function input sensitivity. It must be programmed together with the
      --  BK2P polarity bit. Note: This bit can not be modified as long as LOCK
      --  level 1 has been programmed (LOCK bits in TIMx_BDTR register).
      BK2INP         : Boolean := False;
      --  BRK2 COMP1 input polarity This bit selects the COMP1 input
      --  sensitivity. It must be programmed together with the BK2P polarity
      --  bit. Note: This bit can not be modified as long as LOCK level 1 has
      --  been programmed (LOCK bits in TIMx_BDTR register).
      BK2CMP1P       : Boolean := False;
      --  BRK2 COMP2 input polarity This bit selects the COMP2 input
      --  sensitivity. It must be programmed together with the BK2P polarity
      --  bit. Note: This bit can not be modified as long as LOCK level 1 has
      --  been programmed (LOCK bits in TIMx_BDTR register).
      BK2CMP2P       : Boolean := False;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIM1_AF2_Register use record
      BK2INE         at 0 range 0 .. 0;
      BK2CMP1E       at 0 range 1 .. 1;
      BK2CMP2E       at 0 range 2 .. 2;
      Reserved_3_8   at 0 range 3 .. 8;
      BK2INP         at 0 range 9 .. 9;
      BK2CMP1P       at 0 range 10 .. 10;
      BK2CMP2P       at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype TIM1_TISEL_TI1SEL_Field is HAL.UInt4;
   subtype TIM1_TISEL_TI2SEL_Field is HAL.UInt4;
   subtype TIM1_TISEL_TI3SEL_Field is HAL.UInt4;
   subtype TIM1_TISEL_TI4SEL_Field is HAL.UInt4;

   --  TIM1 timer input selection register
   type TIM1_TISEL_Register is record
      --  selects TI1[0] to TI1[15] input Others: Reserved
      TI1SEL         : TIM1_TISEL_TI1SEL_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  selects TI2[0] to TI2[15] input Others: Reserved
      TI2SEL         : TIM1_TISEL_TI2SEL_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  selects TI3[0] to TI3[15] input Others: Reserved
      TI3SEL         : TIM1_TISEL_TI3SEL_Field := 16#0#;
      --  unspecified
      Reserved_20_23 : HAL.UInt4 := 16#0#;
      --  selects TI4[0] to TI4[15] input Others: Reserved
      TI4SEL         : TIM1_TISEL_TI4SEL_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIM1_TISEL_Register use record
      TI1SEL         at 0 range 0 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      TI2SEL         at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      TI3SEL         at 0 range 16 .. 19;
      Reserved_20_23 at 0 range 20 .. 23;
      TI4SEL         at 0 range 24 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  TIM6 control register 1
   type TIM6_CR1_Register is record
      --  Counter enable Note: Gated mode can work only if the CEN bit has been
      --  previously set by software. However trigger mode can set the CEN bit
      --  automatically by hardware. CEN is cleared automatically in one-pulse
      --  mode, when an update event occurs.
      CEN            : Boolean := False;
      --  Update disable This bit is set and cleared by software to
      --  enable/disable UEV event generation. Counter overflow/underflow
      --  Setting the UG bit Update generation through the slave mode
      --  controller Buffered registers are then loaded with their preload
      --  values.
      UDIS           : Boolean := False;
      --  Update request source This bit is set and cleared by software to
      --  select the UEV event sources. Counter overflow/underflow Setting the
      --  UG bit Update generation through the slave mode controller
      URS            : Boolean := False;
      --  One-pulse mode
      OPM            : Boolean := False;
      --  unspecified
      Reserved_4_6   : HAL.UInt3 := 16#0#;
      --  Auto-reload preload enable
      ARPE           : Boolean := False;
      --  unspecified
      Reserved_8_10  : HAL.UInt3 := 16#0#;
      --  UIF status bit remapping
      UIFREMAP       : Boolean := False;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 16,
          Bit_Order => System.Low_Order_First;

   for TIM6_CR1_Register use record
      CEN            at 0 range 0 .. 0;
      UDIS           at 0 range 1 .. 1;
      URS            at 0 range 2 .. 2;
      OPM            at 0 range 3 .. 3;
      Reserved_4_6   at 0 range 4 .. 6;
      ARPE           at 0 range 7 .. 7;
      Reserved_8_10  at 0 range 8 .. 10;
      UIFREMAP       at 0 range 11 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
   end record;

   subtype TIM6_CR2_MMS_Field is HAL.UInt3;

   --  TIM6 control register 2
   type TIM6_CR2_Register is record
      --  unspecified
      Reserved_0_3  : HAL.UInt4 := 16#0#;
      --  Master mode selection These bits are used to select the information
      --  to be sent in master mode to slave timers for synchronization (TRGO).
      --  The combination is as follows: When the Counter Enable signal is
      --  controlled by the trigger input, there is a delay on TRGO, except if
      --  the master/slave mode is selected (see the MSM bit description in the
      --  TIMx_SMCR register). Note: The clock of the slave timer or ADC must
      --  be enabled prior to receive events from the master timer, and must
      --  not be changed on-the-fly while triggers are received from the master
      --  timer.
      MMS           : TIM6_CR2_MMS_Field := 16#0#;
      --  unspecified
      Reserved_7_15 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 16,
          Bit_Order => System.Low_Order_First;

   for TIM6_CR2_Register use record
      Reserved_0_3  at 0 range 0 .. 3;
      MMS           at 0 range 4 .. 6;
      Reserved_7_15 at 0 range 7 .. 15;
   end record;

   --  TIM6 DMA/Interrupt enable register
   type TIM6_DIER_Register is record
      --  Update interrupt enable
      UIE           : Boolean := False;
      --  unspecified
      Reserved_1_7  : HAL.UInt7 := 16#0#;
      --  Update DMA request enable
      UDE           : Boolean := False;
      --  unspecified
      Reserved_9_15 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 16,
          Bit_Order => System.Low_Order_First;

   for TIM6_DIER_Register use record
      UIE           at 0 range 0 .. 0;
      Reserved_1_7  at 0 range 1 .. 7;
      UDE           at 0 range 8 .. 8;
      Reserved_9_15 at 0 range 9 .. 15;
   end record;

   --  TIM6 status register
   type TIM6_SR_Register is record
      --  Update interrupt flag This bit is set by hardware on an update event.
      --  It is cleared by software. At overflow or underflow regarding the
      --  repetition counter value and if UDIS = 0 in the TIMx_CR1 register.
      --  When CNT is reinitialized by software using the UG bit in the
      --  TIMx_EGR register, if URS1=10 and UDIS1=10 in the TIMx_CR1 register.
      UIF           : Boolean := False;
      --  unspecified
      Reserved_1_15 : HAL.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 16,
          Bit_Order => System.Low_Order_First;

   for TIM6_SR_Register use record
      UIF           at 0 range 0 .. 0;
      Reserved_1_15 at 0 range 1 .. 15;
   end record;

   --  TIM6 event generation register
   type TIM6_EGR_Register is record
      --  Write-only. Update generation This bit can be set by software, it is
      --  automatically cleared by hardware.
      UG            : Boolean := False;
      --  unspecified
      Reserved_1_15 : HAL.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 16,
          Bit_Order => System.Low_Order_First;

   for TIM6_EGR_Register use record
      UG            at 0 range 0 .. 0;
      Reserved_1_15 at 0 range 1 .. 15;
   end record;

   subtype TIM6_CNT_CNT_Field is HAL.UInt16;

   --  TIM6 counter
   type TIM6_CNT_Register is record
      --  Counter value
      CNT            : TIM6_CNT_CNT_Field := 16#0#;
      --  unspecified
      Reserved_16_30 : HAL.UInt15 := 16#0#;
      --  Read-only. UIF Copy This bit is a read-only copy of the UIF bit of
      --  the TIMx_ISR register. If the UIFREMAP bit in TIMx_CR1 is reset, bit
      --  31 is reserved and read as 0.
      UIFCPY         : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIM6_CNT_Register use record
      CNT            at 0 range 0 .. 15;
      Reserved_16_30 at 0 range 16 .. 30;
      UIFCPY         at 0 range 31 .. 31;
   end record;

   --  TIM7 control register 1
   type TIM7_CR1_Register is record
      --  Counter enable Note: Gated mode can work only if the CEN bit has been
      --  previously set by software. However trigger mode can set the CEN bit
      --  automatically by hardware. CEN is cleared automatically in one-pulse
      --  mode, when an update event occurs.
      CEN            : Boolean := False;
      --  Update disable This bit is set and cleared by software to
      --  enable/disable UEV event generation. Counter overflow/underflow
      --  Setting the UG bit Update generation through the slave mode
      --  controller Buffered registers are then loaded with their preload
      --  values.
      UDIS           : Boolean := False;
      --  Update request source This bit is set and cleared by software to
      --  select the UEV event sources. Counter overflow/underflow Setting the
      --  UG bit Update generation through the slave mode controller
      URS            : Boolean := False;
      --  One-pulse mode
      OPM            : Boolean := False;
      --  unspecified
      Reserved_4_6   : HAL.UInt3 := 16#0#;
      --  Auto-reload preload enable
      ARPE           : Boolean := False;
      --  unspecified
      Reserved_8_10  : HAL.UInt3 := 16#0#;
      --  UIF status bit remapping
      UIFREMAP       : Boolean := False;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 16,
          Bit_Order => System.Low_Order_First;

   for TIM7_CR1_Register use record
      CEN            at 0 range 0 .. 0;
      UDIS           at 0 range 1 .. 1;
      URS            at 0 range 2 .. 2;
      OPM            at 0 range 3 .. 3;
      Reserved_4_6   at 0 range 4 .. 6;
      ARPE           at 0 range 7 .. 7;
      Reserved_8_10  at 0 range 8 .. 10;
      UIFREMAP       at 0 range 11 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
   end record;

   subtype TIM7_CR2_MMS_Field is HAL.UInt3;

   --  TIM7 control register 2
   type TIM7_CR2_Register is record
      --  unspecified
      Reserved_0_3  : HAL.UInt4 := 16#0#;
      --  Master mode selection These bits are used to select the information
      --  to be sent in master mode to slave timers for synchronization (TRGO).
      --  The combination is as follows: When the Counter Enable signal is
      --  controlled by the trigger input, there is a delay on TRGO, except if
      --  the master/slave mode is selected (see the MSM bit description in the
      --  TIMx_SMCR register). Note: The clock of the slave timer or ADC must
      --  be enabled prior to receive events from the master timer, and must
      --  not be changed on-the-fly while triggers are received from the master
      --  timer.
      MMS           : TIM7_CR2_MMS_Field := 16#0#;
      --  unspecified
      Reserved_7_15 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 16,
          Bit_Order => System.Low_Order_First;

   for TIM7_CR2_Register use record
      Reserved_0_3  at 0 range 0 .. 3;
      MMS           at 0 range 4 .. 6;
      Reserved_7_15 at 0 range 7 .. 15;
   end record;

   --  TIM7 DMA/Interrupt enable register
   type TIM7_DIER_Register is record
      --  Update interrupt enable
      UIE           : Boolean := False;
      --  unspecified
      Reserved_1_7  : HAL.UInt7 := 16#0#;
      --  Update DMA request enable
      UDE           : Boolean := False;
      --  unspecified
      Reserved_9_15 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 16,
          Bit_Order => System.Low_Order_First;

   for TIM7_DIER_Register use record
      UIE           at 0 range 0 .. 0;
      Reserved_1_7  at 0 range 1 .. 7;
      UDE           at 0 range 8 .. 8;
      Reserved_9_15 at 0 range 9 .. 15;
   end record;

   --  TIM7 status register
   type TIM7_SR_Register is record
      --  Update interrupt flag This bit is set by hardware on an update event.
      --  It is cleared by software. At overflow or underflow regarding the
      --  repetition counter value and if UDIS = 0 in the TIMx_CR1 register.
      --  When CNT is reinitialized by software using the UG bit in the
      --  TIMx_EGR register, if URS1=10 and UDIS1=10 in the TIMx_CR1 register.
      UIF           : Boolean := False;
      --  unspecified
      Reserved_1_15 : HAL.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 16,
          Bit_Order => System.Low_Order_First;

   for TIM7_SR_Register use record
      UIF           at 0 range 0 .. 0;
      Reserved_1_15 at 0 range 1 .. 15;
   end record;

   --  TIM7 event generation register
   type TIM7_EGR_Register is record
      --  Write-only. Update generation This bit can be set by software, it is
      --  automatically cleared by hardware.
      UG            : Boolean := False;
      --  unspecified
      Reserved_1_15 : HAL.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 16,
          Bit_Order => System.Low_Order_First;

   for TIM7_EGR_Register use record
      UG            at 0 range 0 .. 0;
      Reserved_1_15 at 0 range 1 .. 15;
   end record;

   subtype TIM7_CNT_CNT_Field is HAL.UInt16;

   --  TIM7 counter
   type TIM7_CNT_Register is record
      --  Counter value
      CNT            : TIM7_CNT_CNT_Field := 16#0#;
      --  unspecified
      Reserved_16_30 : HAL.UInt15 := 16#0#;
      --  Read-only. UIF Copy This bit is a read-only copy of the UIF bit of
      --  the TIMx_ISR register. If the UIFREMAP bit in TIMx_CR1 is reset, bit
      --  31 is reserved and read as 0.
      UIFCPY         : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIM7_CNT_Register use record
      CNT            at 0 range 0 .. 15;
      Reserved_16_30 at 0 range 16 .. 30;
      UIFCPY         at 0 range 31 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   type TIM1_Disc is
     (Input,
      Output);

   --  TIM1 address block description
   type TIM1_Peripheral
     (Discriminent : TIM1_Disc := Input)
   is record
      --  TIM1 control register 1
      TIM1_CR1          : aliased TIM1_CR1_Register;
      --  TIM1 control register 2
      TIM1_CR2          : aliased TIM1_CR2_Register;
      --  TIM1 slave mode control register
      TIM1_SMCR         : aliased TIM1_SMCR_Register;
      --  TIM1 DMA/interrupt enable register
      TIM1_DIER         : aliased TIM1_DIER_Register;
      --  TIM1 status register
      TIM1_SR           : aliased TIM1_SR_Register;
      --  TIM1 event generation register
      TIM1_EGR          : aliased TIM1_EGR_Register;
      --  TIM1 capture/compare enable register
      TIM1_CCER         : aliased TIM1_CCER_Register;
      --  TIM1 counter
      TIM1_CNT          : aliased TIM1_CNT_Register;
      --  TIM1 prescaler
      TIM1_PSC          : aliased HAL.UInt16;
      --  TIM1 auto-reload register
      TIM1_ARR          : aliased HAL.UInt16;
      --  TIM1 repetition counter register
      TIM1_RCR          : aliased HAL.UInt16;
      --  TIM1 capture/compare register 1
      TIM1_CCR1         : aliased HAL.UInt16;
      --  TIM1 capture/compare register 2
      TIM1_CCR2         : aliased HAL.UInt16;
      --  TIM1 capture/compare register 3
      TIM1_CCR3         : aliased HAL.UInt16;
      --  TIM1 capture/compare register 4
      TIM1_CCR4         : aliased HAL.UInt16;
      --  TIM1 break and dead-time register
      TIM1_BDTR         : aliased TIM1_BDTR_Register;
      --  TIM1 DMA control register
      TIM1_DCR          : aliased TIM1_DCR_Register;
      --  TIM1 DMA address for full transfer
      TIM1_DMAR         : aliased HAL.UInt32;
      --  TIM1 option register 1
      TIM1_OR1          : aliased TIM1_OR1_Register;
      --  TIM1 capture/compare mode register 3
      TIM1_CCMR3        : aliased TIM1_CCMR3_Register;
      --  TIM1 capture/compare register 5
      TIM1_CCR5         : aliased TIM1_CCR5_Register;
      --  TIM1 capture/compare register 6
      TIM1_CCR6         : aliased HAL.UInt16;
      --  TIM1 alternate function option register 1
      TIM1_AF1          : aliased TIM1_AF1_Register;
      --  TIM1 Alternate function register 2
      TIM1_AF2          : aliased TIM1_AF2_Register;
      --  TIM1 timer input selection register
      TIM1_TISEL        : aliased TIM1_TISEL_Register;
      case Discriminent is
         when Input =>
            --  TIM1 capture/compare mode register 1
            TIM1_CCMR1_INPUT : aliased TIM1_CCMR1_INPUT_Register;
            --  TIM1 capture/compare mode register 2
            TIM1_CCMR2_INPUT : aliased TIM1_CCMR2_INPUT_Register;
         when Output =>
            --  TIM1 capture/compare mode register 1
            TIM1_CCMR1_OUTPUT : aliased TIM1_CCMR1_OUTPUT_Register;
            --  TIM1 capture/compare mode register 1
            TIM1_CCMR2_OUTPUT : aliased TIM1_CCMR2_OUTPUT_Register;
      end case;
   end record
     with Unchecked_Union, Volatile;

   for TIM1_Peripheral use record
      TIM1_CR1          at 16#0# range 0 .. 15;
      TIM1_CR2          at 16#4# range 0 .. 31;
      TIM1_SMCR         at 16#8# range 0 .. 31;
      TIM1_DIER         at 16#C# range 0 .. 15;
      TIM1_SR           at 16#10# range 0 .. 31;
      TIM1_EGR          at 16#14# range 0 .. 15;
      TIM1_CCER         at 16#20# range 0 .. 31;
      TIM1_CNT          at 16#24# range 0 .. 31;
      TIM1_PSC          at 16#28# range 0 .. 15;
      TIM1_ARR          at 16#2C# range 0 .. 15;
      TIM1_RCR          at 16#30# range 0 .. 15;
      TIM1_CCR1         at 16#34# range 0 .. 15;
      TIM1_CCR2         at 16#38# range 0 .. 15;
      TIM1_CCR3         at 16#3C# range 0 .. 15;
      TIM1_CCR4         at 16#40# range 0 .. 15;
      TIM1_BDTR         at 16#44# range 0 .. 31;
      TIM1_DCR          at 16#48# range 0 .. 15;
      TIM1_DMAR         at 16#4C# range 0 .. 31;
      TIM1_OR1          at 16#50# range 0 .. 31;
      TIM1_CCMR3        at 16#54# range 0 .. 31;
      TIM1_CCR5         at 16#58# range 0 .. 31;
      TIM1_CCR6         at 16#5C# range 0 .. 15;
      TIM1_AF1          at 16#60# range 0 .. 31;
      TIM1_AF2          at 16#64# range 0 .. 31;
      TIM1_TISEL        at 16#68# range 0 .. 31;
      TIM1_CCMR1_INPUT  at 16#18# range 0 .. 31;
      TIM1_CCMR2_INPUT  at 16#1C# range 0 .. 31;
      TIM1_CCMR1_OUTPUT at 16#18# range 0 .. 31;
      TIM1_CCMR2_OUTPUT at 16#1C# range 0 .. 31;
   end record;

   --  TIM1 address block description
   TIM1_Periph : aliased TIM1_Peripheral
     with Import, Address => TIM1_Base;

   --  TIM6 address block description
   type TIM6_Peripheral is record
      --  TIM6 control register 1
      TIM6_CR1  : aliased TIM6_CR1_Register;
      --  TIM6 control register 2
      TIM6_CR2  : aliased TIM6_CR2_Register;
      --  TIM6 DMA/Interrupt enable register
      TIM6_DIER : aliased TIM6_DIER_Register;
      --  TIM6 status register
      TIM6_SR   : aliased TIM6_SR_Register;
      --  TIM6 event generation register
      TIM6_EGR  : aliased TIM6_EGR_Register;
      --  TIM6 counter
      TIM6_CNT  : aliased TIM6_CNT_Register;
      --  TIM6 prescaler
      TIM6_PSC  : aliased HAL.UInt16;
      --  TIM6 auto-reload register
      TIM6_ARR  : aliased HAL.UInt16;
   end record
     with Volatile;

   for TIM6_Peripheral use record
      TIM6_CR1  at 16#0# range 0 .. 15;
      TIM6_CR2  at 16#4# range 0 .. 15;
      TIM6_DIER at 16#C# range 0 .. 15;
      TIM6_SR   at 16#10# range 0 .. 15;
      TIM6_EGR  at 16#14# range 0 .. 15;
      TIM6_CNT  at 16#24# range 0 .. 31;
      TIM6_PSC  at 16#28# range 0 .. 15;
      TIM6_ARR  at 16#2C# range 0 .. 15;
   end record;

   --  TIM6 address block description
   TIM6_Periph : aliased TIM6_Peripheral
     with Import, Address => TIM6_Base;

   --  TIM7 address block description
   type TIM7_Peripheral is record
      --  TIM7 control register 1
      TIM7_CR1  : aliased TIM7_CR1_Register;
      --  TIM7 control register 2
      TIM7_CR2  : aliased TIM7_CR2_Register;
      --  TIM7 DMA/Interrupt enable register
      TIM7_DIER : aliased TIM7_DIER_Register;
      --  TIM7 status register
      TIM7_SR   : aliased TIM7_SR_Register;
      --  TIM7 event generation register
      TIM7_EGR  : aliased TIM7_EGR_Register;
      --  TIM7 counter
      TIM7_CNT  : aliased TIM7_CNT_Register;
      --  TIM7 prescaler
      TIM7_PSC  : aliased HAL.UInt16;
      --  TIM7 auto-reload register
      TIM7_ARR  : aliased HAL.UInt16;
   end record
     with Volatile;

   for TIM7_Peripheral use record
      TIM7_CR1  at 16#0# range 0 .. 15;
      TIM7_CR2  at 16#4# range 0 .. 15;
      TIM7_DIER at 16#C# range 0 .. 15;
      TIM7_SR   at 16#10# range 0 .. 15;
      TIM7_EGR  at 16#14# range 0 .. 15;
      TIM7_CNT  at 16#24# range 0 .. 31;
      TIM7_PSC  at 16#28# range 0 .. 15;
      TIM7_ARR  at 16#2C# range 0 .. 15;
   end record;

   --  TIM7 address block description
   TIM7_Periph : aliased TIM7_Peripheral
     with Import, Address => TIM7_Base;

end STM32_SVD.TIM;
