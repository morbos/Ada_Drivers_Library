--  This spec has been automatically generated from STM32U073.svd

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

   --  LPTIM1 interrupt and status register [alternate]
   type LPTIM1_ISR_OUTPUT_Register is record
      --  Read-only. Compare 1 interrupt flag If channel CC1 is configured as
      --  output: The CC1IF flag is set by hardware to inform application that
      --  LPTIM_CNT register value matches the compare register's value. CC1IF
      --  flag can be cleared by writing 1 to the CC1CF bit in the LPTIM_ICR
      --  register.
      CC1IF          : Boolean;
      --  Read-only. Autoreload match ARRM is set by hardware to inform
      --  application that LPTIM_CNT registers value reached the LPTIM_ARR
      --  registers value. ARRM flag can be cleared by writing 1 to the ARRMCF
      --  bit in the LPTIM_ICR register.
      ARRM           : Boolean;
      --  Read-only. External trigger edge event EXTTRIG is set by hardware to
      --  inform application that a valid edge on the selected external trigger
      --  input has occurred. If the trigger is ignored because the timer has
      --  already started, then this flag is not set. EXTTRIG flag can be
      --  cleared by writing 1 to the EXTTRIGCF bit in the LPTIM_ICR register.
      EXTTRIG        : Boolean;
      --  Read-only. Compare register 1 update OK CMP1OK is set by hardware to
      --  inform application that the APB bus write operation to the LPTIM_CCR1
      --  register has been successfully completed. CMP1OK flag can be cleared
      --  by writing 1 to the CMP1OKCF bit in the LPTIM_ICR register.
      CMP1OK         : Boolean;
      --  Read-only. Autoreload register update OK ARROK is set by hardware to
      --  inform application that the APB bus write operation to the LPTIM_ARR
      --  register has been successfully completed. ARROK flag can be cleared
      --  by writing 1 to the ARROKCF bit in the LPTIM_ICR register.
      ARROK          : Boolean;
      --  Read-only. Counter direction change down to up In Encoder mode, UP
      --  bit is set by hardware to inform application that the counter
      --  direction has changed from down to up. UP flag can be cleared by
      --  writing 1 to the UPCF bit in the LPTIM_ICR register. Note: If the
      --  LPTIM does not support encoder mode feature, this bit is reserved.
      --  Refer to Section125.3.
      UP             : Boolean;
      --  Read-only. Counter direction change up to down In Encoder mode, DOWN
      --  bit is set by hardware to inform application that the counter
      --  direction has changed from up to down. DOWN flag can be cleared by
      --  writing 1 to the DOWNCF bit in the LPTIM_ICR register. Note: If the
      --  LPTIM does not support encoder mode feature, this bit is reserved.
      --  Refer to Section125.3.
      DOWN           : Boolean;
      --  Read-only. LPTIM update event occurred UE is set by hardware to
      --  inform application that an update event was generated. The
      --  corresponding interrupt or DMA request is generated if enabled. UE
      --  flag can be cleared by writing 1 to the UECF bit in the LPTIM_ICR
      --  register. The UE flag is automatically cleared by hardware once the
      --  LPTIM_ARR register is written by any bus master like CPU or DMA.
      UE             : Boolean;
      --  Read-only. Repetition register update OK REPOK is set by hardware to
      --  inform application that the APB bus write operation to the LPTIM_RCR
      --  register has been successfully completed. REPOK flag can be cleared
      --  by writing 1 to the REPOKCF bit in the LPTIM_ICR register.
      REPOK          : Boolean;
      --  Read-only. Compare 2 interrupt flag If channel CC2 is configured as
      --  output: The CC2IF flag is set by hardware to inform application that
      --  LPTIM_CNT register value matches the compare register's value. CC2IF
      --  flag can be cleared by writing 1 to the CC2CF bit in the LPTIM_ICR
      --  register. Note: If LPTIM does not implement at least 2 channels this
      --  bit is reserved. Refer to Section125.3.
      CC2IF          : Boolean;
      --  Read-only. Compare 3 interrupt flag If channel CC3 is configured as
      --  output: The CC3IF flag is set by hardware to inform application that
      --  LPTIM_CNT register value matches the compare register's value. CC3IF
      --  flag can be cleared by writing 1 to the CC3CF bit in the LPTIM_ICR
      --  register. Note: If LPTIM does not implement at least 3 channels this
      --  bit is reserved. Refer to Section125.3.
      CC3IF          : Boolean;
      --  Read-only. Compare 4 interrupt flag If channel CC4 is configured as
      --  output: The CC4IF flag is set by hardware to inform application that
      --  LPTIM_CNT register value matches the compare register's value. CC4IF
      --  flag can be cleared by writing 1 to the CC4CF bit in the LPTIM_ICR
      --  register. Note: If LPTIM does not implement at least 4 channels this
      --  bit is reserved. Refer to Section125.3.
      CC4IF          : Boolean;
      --  unspecified
      Reserved_12_18 : HAL.UInt7;
      --  Read-only. Compare register 2 update OK CMP2OK is set by hardware to
      --  inform application that the APB bus write operation to the LPTIM_CCR2
      --  register has been successfully completed. CMP2OK flag can be cleared
      --  by writing 1 to the CMP2OKCF bit in the LPTIM_ICR register. Note: If
      --  LPTIM does not implement at least 2 channels this bit is reserved.
      --  Refer to Section125.3.
      CMP2OK         : Boolean;
      --  Read-only. Compare register 3 update OK CMP3OK is set by hardware to
      --  inform application that the APB bus write operation to the LPTIM_CCR3
      --  register has been successfully completed. CMP3OK flag can be cleared
      --  by writing 1 to the CMP3OKCF bit in the LPTIM_ICR register. Note: If
      --  LPTIM does not implement at least 3 channels this bit is reserved.
      --  Refer to Section125.3.
      CMP3OK         : Boolean;
      --  Read-only. Compare register 4 update OK CMP4OK is set by hardware to
      --  inform application that the APB bus write operation to the LPTIM_CCR4
      --  register has been successfully completed. CMP4OK flag can be cleared
      --  by writing 1 to the CMP4OKCF bit in the LPTIM_ICR register. Note: If
      --  LPTIM does not implement at least 4 channels this bit is reserved.
      --  Refer to Section125.3.
      CMP4OK         : Boolean;
      --  unspecified
      Reserved_22_23 : HAL.UInt2;
      --  Read-only. Interrupt enable register update OK DIEROK is set by
      --  hardware to inform application that the APB bus write operation to
      --  the LPTIM_DIER register has been successfully completed. DIEROK flag
      --  can be cleared by writing 1 to the DIEROKCF bit in the LPTIM_ICR
      --  register.
      DIEROK         : Boolean;
      --  unspecified
      Reserved_25_31 : HAL.UInt7;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM1_ISR_OUTPUT_Register use record
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
      CC3IF          at 0 range 10 .. 10;
      CC4IF          at 0 range 11 .. 11;
      Reserved_12_18 at 0 range 12 .. 18;
      CMP2OK         at 0 range 19 .. 19;
      CMP3OK         at 0 range 20 .. 20;
      CMP4OK         at 0 range 21 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      DIEROK         at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  LPTIM1 interrupt and status register [alternate]
   type LPTIM1_ISR_INPUT_Register is record
      --  Read-only. capture 1 interrupt flag If channel CC1 is configured as
      --  input: CC1IF is set by hardware to inform application that the
      --  current value of the counter is captured in LPTIM_CCR1 register. The
      --  corresponding interrupt or DMA request is generated if enabled. The
      --  CC1OF flag is set if the CC1IF flag was already high.
      CC1IF          : Boolean;
      --  Read-only. Autoreload match ARRM is set by hardware to inform
      --  application that LPTIM_CNT registers value reached the LPTIM_ARR
      --  registers value. ARRM flag can be cleared by writing 1 to the ARRMCF
      --  bit in the LPTIM_ICR register.
      ARRM           : Boolean;
      --  Read-only. External trigger edge event EXTTRIG is set by hardware to
      --  inform application that a valid edge on the selected external trigger
      --  input has occurred. If the trigger is ignored because the timer has
      --  already started, then this flag is not set. EXTTRIG flag can be
      --  cleared by writing 1 to the EXTTRIGCF bit in the LPTIM_ICR register.
      EXTTRIG        : Boolean;
      --  unspecified
      Reserved_3_3   : HAL.Bit;
      --  Read-only. Autoreload register update OK ARROK is set by hardware to
      --  inform application that the APB bus write operation to the LPTIM_ARR
      --  register has been successfully completed. ARROK flag can be cleared
      --  by writing 1 to the ARROKCF bit in the LPTIM_ICR register.
      ARROK          : Boolean;
      --  Read-only. Counter direction change down to up In Encoder mode, UP
      --  bit is set by hardware to inform application that the counter
      --  direction has changed from down to up. UP flag can be cleared by
      --  writing 1 to the UPCF bit in the LPTIM_ICR register. Note: If the
      --  LPTIM does not support encoder mode feature, this bit is reserved.
      --  Refer to Section125.3.
      UP             : Boolean;
      --  Read-only. Counter direction change up to down In Encoder mode, DOWN
      --  bit is set by hardware to inform application that the counter
      --  direction has changed from up to down. DOWN flag can be cleared by
      --  writing 1 to the DOWNCF bit in the LPTIM_ICR register. Note: If the
      --  LPTIM does not support encoder mode feature, this bit is reserved.
      --  Refer to Section125.3.
      DOWN           : Boolean;
      --  Read-only. LPTIM update event occurred UE is set by hardware to
      --  inform application that an update event was generated. UE flag can be
      --  cleared by writing 1 to the UECF bit in the LPTIM_ICR register.
      UE             : Boolean;
      --  Read-only. Repetition register update OK REPOK is set by hardware to
      --  inform application that the APB bus write operation to the LPTIM_RCR
      --  register has been successfully completed. REPOK flag can be cleared
      --  by writing 1 to the REPOKCF bit in the LPTIM_ICR register.
      REPOK          : Boolean;
      --  Read-only. Capture 2 interrupt flag If channel CC2 is configured as
      --  input: CC2IF is set by hardware to inform application that the
      --  current value of the counter is captured in LPTIM_CCR2 register. The
      --  corresponding interrupt or DMA request is generated if enabled. The
      --  CC2OF flag is set if the CC2IF flag was already high. Note: If LPTIM
      --  does not implement at least 2 channels this bit is reserved. Refer to
      --  Section125.3.
      CC2IF          : Boolean;
      --  Read-only. Capture 3 interrupt flag If channel CC3 is configured as
      --  input: CC3IF is set by hardware to inform application that the
      --  current value of the counter is captured in LPTIM_CCR3 register. The
      --  corresponding interrupt or DMA request is generated if enabled. The
      --  CC3OF flag is set if the CC3IF flag was already high. Note: If LPTIM
      --  does not implement at least 3 channels this bit is reserved. Refer to
      --  Section125.3.
      CC3IF          : Boolean;
      --  Read-only. Capture 4 interrupt flag If channel CC4 is configured as
      --  input: CC4IF is set by hardware to inform application that the
      --  current value of the counter is captured in LPTIM_CCR4 register. The
      --  corresponding interrupt or DMA request is generated if enabled. The
      --  CC4OF flag is set if the CC4IF flag was already high. Note: If LPTIM
      --  does not implement at least 4 channels this bit is reserved. Refer to
      --  Section125.3.
      CC4IF          : Boolean;
      --  Read-only. Capture 1 over-capture flag This flag is set by hardware
      --  only when the corresponding channel is configured in input capture
      --  mode. It is cleared by software by writing 1 to the CC1OCF bit in the
      --  LPTIM_ICR register. Note: If LPTIM does not implement at least 1
      --  channel this bit is reserved. Refer to Section125.3.
      CC1OF          : Boolean;
      --  Read-only. Capture 2 over-capture flag This flag is set by hardware
      --  only when the corresponding channel is configured in input capture
      --  mode. It is cleared by software by writing 1 to the CC2OCF bit in the
      --  LPTIM_ICR register. Note: If LPTIM does not implement at least 2
      --  channels this bit is reserved. Refer to Section125.3.
      CC2OF          : Boolean;
      --  Read-only. Capture 3 over-capture flag This flag is set by hardware
      --  only when the corresponding channel is configured in input capture
      --  mode. It is cleared by software by writing 1 to the CC3OCF bit in the
      --  LPTIM_ICR register. Note: If LPTIM does not implement at least 3
      --  channels this bit is reserved. Refer to Section125.3.
      CC3OF          : Boolean;
      --  Read-only. Capture 4 over-capture flag This flag is set by hardware
      --  only when the corresponding channel is configured in input capture
      --  mode. It is cleared by software by writing 1 to the CC4OCF bit in the
      --  LPTIM_ICR register. Note: If LPTIM does not implement at least 4
      --  channels this bit is reserved. Refer to Section125.3.
      CC4OF          : Boolean;
      --  unspecified
      Reserved_16_23 : HAL.UInt8;
      --  Read-only. Interrupt enable register update OK DIEROK is set by
      --  hardware to inform application that the APB bus write operation to
      --  the LPTIM_DIER register has been successfully completed. DIEROK flag
      --  can be cleared by writing 1 to the DIEROKCF bit in the LPTIM_ICR
      --  register.
      DIEROK         : Boolean;
      --  unspecified
      Reserved_25_31 : HAL.UInt7;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM1_ISR_INPUT_Register use record
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
      CC3IF          at 0 range 10 .. 10;
      CC4IF          at 0 range 11 .. 11;
      CC1OF          at 0 range 12 .. 12;
      CC2OF          at 0 range 13 .. 13;
      CC3OF          at 0 range 14 .. 14;
      CC4OF          at 0 range 15 .. 15;
      Reserved_16_23 at 0 range 16 .. 23;
      DIEROK         at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  LPTIM1 interrupt clear register [alternate]
   type LPTIM1_ICR_OUTPUT_Register is record
      --  Write-only. Capture/compare 1 clear flag Writing 1 to this bit clears
      --  the CC1IF flag in the LPTIM_ISR register.
      CC1CF          : Boolean := False;
      --  Write-only. Autoreload match clear flag Writing 1 to this bit clears
      --  the ARRM flag in the LPTIM_ISR register
      ARRMCF         : Boolean := False;
      --  Write-only. External trigger valid edge clear flag Writing 1 to this
      --  bit clears the EXTTRIG flag in the LPTIM_ISR register
      EXTTRIGCF      : Boolean := False;
      --  Write-only. Compare register 1 update OK clear flag Writing 1 to this
      --  bit clears the CMP1OK flag in the LPTIM_ISR register.
      CMP1OKCF       : Boolean := False;
      --  Write-only. Autoreload register update OK clear flag Writing 1 to
      --  this bit clears the ARROK flag in the LPTIM_ISR register
      ARROKCF        : Boolean := False;
      --  Write-only. Direction change to UP clear flag Writing 1 to this bit
      --  clear the UP flag in the LPTIM_ISR register. Note: If the LPTIM does
      --  not support encoder mode feature, this bit is reserved. Refer to
      --  Section125.3.
      UPCF           : Boolean := False;
      --  Write-only. Direction change to down clear flag Writing 1 to this bit
      --  clear the DOWN flag in the LPTIM_ISR register. Note: If the LPTIM
      --  does not support encoder mode feature, this bit is reserved. Refer to
      --  Section125.3.
      DOWNCF         : Boolean := False;
      --  Write-only. Update event clear flag Writing 1 to this bit clear the
      --  UE flag in the LPTIM_ISR register.
      UECF           : Boolean := False;
      --  Write-only. Repetition register update OK clear flag Writing 1 to
      --  this bit clears the REPOK flag in the LPTIM_ISR register.
      REPOKCF        : Boolean := False;
      --  Write-only. Capture/compare 2 clear flag Writing 1 to this bit clears
      --  the CC2IF flag in the LPTIM_ISR register. Note: If LPTIM does not
      --  implement at least 2 channels this bit is reserved. Refer to
      --  Section125.3.
      CC2CF          : Boolean := False;
      --  Write-only. Capture/compare 3 clear flag Writing 1 to this bit clears
      --  the CC3IF flag in the LPTIM_ISR register. Note: If LPTIM does not
      --  implement at least 3 channels this bit is reserved. Refer to
      --  Section125.3.
      CC3CF          : Boolean := False;
      --  Write-only. Capture/compare 4 clear flag Writing 1 to this bit clears
      --  the CC4IF flag in the LPTIM_ISR register. Note: If LPTIM does not
      --  implement at least 4 channels this bit is reserved. Refer to
      --  Section125.3.
      CC4CF          : Boolean := False;
      --  unspecified
      Reserved_12_18 : HAL.UInt7 := 16#0#;
      --  Write-only. Compare register 2 update OK clear flag Writing 1 to this
      --  bit clears the CMP2OK flag in the LPTIM_ISR register. Note: If LPTIM
      --  does not implement at least 2 channels this bit is reserved. Refer to
      --  Section125.3.
      CMP2OKCF       : Boolean := False;
      --  Write-only. Compare register 3 update OK clear flag Writing 1 to this
      --  bit clears the CMP3OK flag in the LPTIM_ISR register. Note: If LPTIM
      --  does not implement at least 3 channels this bit is reserved. Refer to
      --  Section125.3.
      CMP3OKCF       : Boolean := False;
      --  Write-only. Compare register 4 update OK clear flag Writing 1 to this
      --  bit clears the CMP4OK flag in the LPTIM_ISR register. Note: If LPTIM
      --  does not implement at least 4 channels this bit is reserved. Refer to
      --  Section125.3.
      CMP4OKCF       : Boolean := False;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  Write-only. Interrupt enable register update OK clear flag Writing 1
      --  to this bit clears the DIEROK flag in the LPTIM_ISR register.
      DIEROKCF       : Boolean := False;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM1_ICR_OUTPUT_Register use record
      CC1CF          at 0 range 0 .. 0;
      ARRMCF         at 0 range 1 .. 1;
      EXTTRIGCF      at 0 range 2 .. 2;
      CMP1OKCF       at 0 range 3 .. 3;
      ARROKCF        at 0 range 4 .. 4;
      UPCF           at 0 range 5 .. 5;
      DOWNCF         at 0 range 6 .. 6;
      UECF           at 0 range 7 .. 7;
      REPOKCF        at 0 range 8 .. 8;
      CC2CF          at 0 range 9 .. 9;
      CC3CF          at 0 range 10 .. 10;
      CC4CF          at 0 range 11 .. 11;
      Reserved_12_18 at 0 range 12 .. 18;
      CMP2OKCF       at 0 range 19 .. 19;
      CMP3OKCF       at 0 range 20 .. 20;
      CMP4OKCF       at 0 range 21 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      DIEROKCF       at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  LPTIM1 interrupt clear register [alternate]
   type LPTIM1_ICR_INPUT_Register is record
      --  Write-only. Capture/compare 1 clear flag Writing 1 to this bit clears
      --  the CC1IF flag in the LPTIM_ISR register.
      CC1CF          : Boolean := False;
      --  Write-only. Autoreload match clear flag Writing 1 to this bit clears
      --  the ARRM flag in the LPTIM_ISR register
      ARRMCF         : Boolean := False;
      --  Write-only. External trigger valid edge clear flag Writing 1 to this
      --  bit clears the EXTTRIG flag in the LPTIM_ISR register
      EXTTRIGCF      : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Write-only. Autoreload register update OK clear flag Writing 1 to
      --  this bit clears the ARROK flag in the LPTIM_ISR register
      ARROKCF        : Boolean := False;
      --  Write-only. Direction change to UP clear flag Writing 1 to this bit
      --  clear the UP flag in the LPTIM_ISR register. Note: If the LPTIM does
      --  not support encoder mode feature, this bit is reserved. Refer to
      --  Section125.3.
      UPCF           : Boolean := False;
      --  Write-only. Direction change to down clear flag Writing 1 to this bit
      --  clear the DOWN flag in the LPTIM_ISR register. Note: If the LPTIM
      --  does not support encoder mode feature, this bit is reserved. Refer to
      --  Section125.3.
      DOWNCF         : Boolean := False;
      --  Write-only. Update event clear flag Writing 1 to this bit clear the
      --  UE flag in the LPTIM_ISR register.
      UECF           : Boolean := False;
      --  Write-only. Repetition register update OK clear flag Writing 1 to
      --  this bit clears the REPOK flag in the LPTIM_ISR register.
      REPOKCF        : Boolean := False;
      --  Write-only. Capture/compare 2 clear flag Writing 1 to this bit clears
      --  the CC2IF flag in the LPTIM_ISR register. Note: If LPTIM does not
      --  implement at least 2 channels this bit is reserved. Refer to
      --  Section125.3.
      CC2CF          : Boolean := False;
      --  Write-only. Capture/compare 3 clear flag Writing 1 to this bit clears
      --  the CC3IF flag in the LPTIM_ISR register. Note: If LPTIM does not
      --  implement at least 3 channels this bit is reserved. Refer to
      --  Section125.3.
      CC3CF          : Boolean := False;
      --  Write-only. Capture/compare 4 clear flag Writing 1 to this bit clears
      --  the CC4IF flag in the LPTIM_ISR register. Note: If LPTIM does not
      --  implement at least 4 channels this bit is reserved. Refer to
      --  Section125.3.
      CC4CF          : Boolean := False;
      --  Write-only. Capture/compare 1 over-capture clear flag Writing 1 to
      --  this bit clears the CC1OF flag in the LPTIM_ISR register. Note: If
      --  LPTIM does not implement at least 1 channel this bit is reserved.
      --  Refer to Section125.3.
      CC1OCF         : Boolean := False;
      --  Write-only. Capture/compare 2 over-capture clear flag Writing 1 to
      --  this bit clears the CC2OF flag in the LPTIM_ISR register. Note: If
      --  LPTIM does not implement at least 2 channels this bit is reserved.
      --  Refer to Section125.3.
      CC2OCF         : Boolean := False;
      --  Write-only. Capture/compare 3 over-capture clear flag Writing 1 to
      --  this bit clears the CC3OF flag in the LPTIM_ISR register. Note: If
      --  LPTIM does not implement at least 3 channels this bit is reserved.
      --  Refer to Section125.3.
      CC3OCF         : Boolean := False;
      --  Write-only. Capture/compare 4 over-capture clear flag Writing 1 to
      --  this bit clears the CC4OF flag in the LPTIM_ISR register. Note: If
      --  LPTIM does not implement at least 4 channels this bit is reserved.
      --  Refer to Section125.3.
      CC4OCF         : Boolean := False;
      --  unspecified
      Reserved_16_23 : HAL.UInt8 := 16#0#;
      --  Write-only. Interrupt enable register update OK clear flag Writing 1
      --  to this bit clears the DIEROK flag in the LPTIM_ISR register.
      DIEROKCF       : Boolean := False;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM1_ICR_INPUT_Register use record
      CC1CF          at 0 range 0 .. 0;
      ARRMCF         at 0 range 1 .. 1;
      EXTTRIGCF      at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      ARROKCF        at 0 range 4 .. 4;
      UPCF           at 0 range 5 .. 5;
      DOWNCF         at 0 range 6 .. 6;
      UECF           at 0 range 7 .. 7;
      REPOKCF        at 0 range 8 .. 8;
      CC2CF          at 0 range 9 .. 9;
      CC3CF          at 0 range 10 .. 10;
      CC4CF          at 0 range 11 .. 11;
      CC1OCF         at 0 range 12 .. 12;
      CC2OCF         at 0 range 13 .. 13;
      CC3OCF         at 0 range 14 .. 14;
      CC4OCF         at 0 range 15 .. 15;
      Reserved_16_23 at 0 range 16 .. 23;
      DIEROKCF       at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  LPTIM1 interrupt enable register [alternate]
   type LPTIM1_DIER_OUTPUT_Register is record
      --  Capture/compare 1 interrupt enable
      CC1IE          : Boolean := False;
      --  Autoreload match Interrupt Enable
      ARRMIE         : Boolean := False;
      --  External trigger valid edge Interrupt Enable
      EXTTRIGIE      : Boolean := False;
      --  Compare register 1 update OK interrupt enable
      CMP1OKIE       : Boolean := False;
      --  Autoreload register update OK Interrupt Enable
      ARROKIE        : Boolean := False;
      --  Direction change to UP Interrupt Enable Note: If the LPTIM does not
      --  support encoder mode feature, this bit is reserved. Refer to
      --  Section125.3.
      UPIE           : Boolean := False;
      --  Direction change to down Interrupt Enable Note: If the LPTIM does not
      --  support encoder mode feature, this bit is reserved. Refer to
      --  Section125.3.
      DOWNIE         : Boolean := False;
      --  Update event interrupt enable
      UEIE           : Boolean := False;
      --  Repetition register update OK interrupt Enable
      REPOKIE        : Boolean := False;
      --  Capture/compare 2 interrupt enable Note: If LPTIM does not implement
      --  at least 2 channels this bit is reserved. Refer to Section125.3.
      CC2IE          : Boolean := False;
      --  Capture/compare 3 interrupt enable Note: If LPTIM does not implement
      --  at least 3 channels this bit is reserved. Refer to Section125.3.
      CC3IE          : Boolean := False;
      --  Capture/compare 4 interrupt enable Note: If LPTIM does not implement
      --  at least 4 channels this bit is reserved. Refer to Section125.3.
      CC4IE          : Boolean := False;
      --  unspecified
      Reserved_12_18 : HAL.UInt7 := 16#0#;
      --  Compare register 2 update OK interrupt enable Note: If LPTIM does not
      --  implement at least 2 channels this bit is reserved. Refer to
      --  Section125.3.
      CMP2OKIE       : Boolean := False;
      --  Compare register 3 update OK interrupt enable Note: If LPTIM does not
      --  implement at least 3 channels this bit is reserved. Refer to
      --  Section125.3.
      CMP3OKIE       : Boolean := False;
      --  Compare register 4 update OK interrupt enable Note: If LPTIM does not
      --  implement at least 4 channels this bit is reserved. Refer to
      --  Section125.3.
      CMP4OKIE       : Boolean := False;
      --  unspecified
      Reserved_22_22 : HAL.Bit := 16#0#;
      --  Update event DMA request enable Note: If LPTIM does not implement at
      --  least 1 channel this bit is reserved. Refer to Section125.3.
      UEDE           : Boolean := False;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM1_DIER_OUTPUT_Register use record
      CC1IE          at 0 range 0 .. 0;
      ARRMIE         at 0 range 1 .. 1;
      EXTTRIGIE      at 0 range 2 .. 2;
      CMP1OKIE       at 0 range 3 .. 3;
      ARROKIE        at 0 range 4 .. 4;
      UPIE           at 0 range 5 .. 5;
      DOWNIE         at 0 range 6 .. 6;
      UEIE           at 0 range 7 .. 7;
      REPOKIE        at 0 range 8 .. 8;
      CC2IE          at 0 range 9 .. 9;
      CC3IE          at 0 range 10 .. 10;
      CC4IE          at 0 range 11 .. 11;
      Reserved_12_18 at 0 range 12 .. 18;
      CMP2OKIE       at 0 range 19 .. 19;
      CMP3OKIE       at 0 range 20 .. 20;
      CMP4OKIE       at 0 range 21 .. 21;
      Reserved_22_22 at 0 range 22 .. 22;
      UEDE           at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  LPTIM1 interrupt enable register [alternate]
   type LPTIM1_DIER_INPUT_Register is record
      --  Capture/compare 1 interrupt enable
      CC1IE          : Boolean := False;
      --  Autoreload match Interrupt Enable
      ARRMIE         : Boolean := False;
      --  External trigger valid edge Interrupt Enable
      EXTTRIGIE      : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Autoreload register update OK Interrupt Enable
      ARROKIE        : Boolean := False;
      --  Direction change to UP Interrupt Enable Note: If the LPTIM does not
      --  support encoder mode feature, this bit is reserved. Refer to
      --  Section125.3.
      UPIE           : Boolean := False;
      --  Direction change to down Interrupt Enable Note: If the LPTIM does not
      --  support encoder mode feature, this bit is reserved. Refer to
      --  Section125.3.
      DOWNIE         : Boolean := False;
      --  Update event interrupt enable
      UEIE           : Boolean := False;
      --  Repetition register update OK interrupt Enable
      REPOKIE        : Boolean := False;
      --  Capture/compare 2 interrupt enable Note: If LPTIM does not implement
      --  at least 2 channels this bit is reserved. Refer to Section125.3.
      CC2IE          : Boolean := False;
      --  Capture/compare 3 interrupt enable Note: If LPTIM does not implement
      --  at least 3 channels this bit is reserved. Refer to Section125.3.
      CC3IE          : Boolean := False;
      --  Capture/compare 4 interrupt enable Note: If LPTIM does not implement
      --  at least 4 channels this bit is reserved. Refer to Section125.3.
      CC4IE          : Boolean := False;
      --  Capture/compare 1 over-capture interrupt enable Note: If LPTIM does
      --  not implement at least 1 channel this bit is reserved. Refer to
      --  Section125.3.
      CC1OIE         : Boolean := False;
      --  Capture/compare 2 over-capture interrupt enable Note: If LPTIM does
      --  not implement at least 2 channels this bit is reserved. Refer to
      --  Section125.3.
      CC2OIE         : Boolean := False;
      --  Capture/compare 3 over-capture interrupt enable Note: If LPTIM does
      --  not implement at least 3 channels this bit is reserved. Refer to
      --  Section125.3.
      CC3OIE         : Boolean := False;
      --  Capture/compare 4 over-capture interrupt enable Note: If LPTIM does
      --  not implement at least 4 channels this bit is reserved. Refer to
      --  Section125.3.
      CC4OIE         : Boolean := False;
      --  Capture/compare 1 DMA request enable Note: If LPTIM does not
      --  implement at least 1 channel this bit is reserved. Refer to
      --  Section125.3.
      CC1DE          : Boolean := False;
      --  unspecified
      Reserved_17_22 : HAL.UInt6 := 16#0#;
      --  Update event DMA request enable Note: If LPTIM does not implement at
      --  least 1 channel this bit is reserved. Refer to Section125.3.
      UEDE           : Boolean := False;
      --  unspecified
      Reserved_24_24 : HAL.Bit := 16#0#;
      --  Capture/compare 2 DMA request enable Note: If LPTIM does not
      --  implement at least 2 channels this bit is reserved. Refer to
      --  Section125.3.
      CC2DE          : Boolean := False;
      --  Capture/compare 3 DMA request enable Note: If LPTIM does not
      --  implement at least 3 channels this bit is reserved. Refer to
      --  Section125.3.
      CC3DE          : Boolean := False;
      --  Capture/compare 4 DMA request enable Note: If LPTIM does not
      --  implement at least 4 channels this bit is reserved. Refer to
      --  Section125.3.
      CC4DE          : Boolean := False;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM1_DIER_INPUT_Register use record
      CC1IE          at 0 range 0 .. 0;
      ARRMIE         at 0 range 1 .. 1;
      EXTTRIGIE      at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      ARROKIE        at 0 range 4 .. 4;
      UPIE           at 0 range 5 .. 5;
      DOWNIE         at 0 range 6 .. 6;
      UEIE           at 0 range 7 .. 7;
      REPOKIE        at 0 range 8 .. 8;
      CC2IE          at 0 range 9 .. 9;
      CC3IE          at 0 range 10 .. 10;
      CC4IE          at 0 range 11 .. 11;
      CC1OIE         at 0 range 12 .. 12;
      CC2OIE         at 0 range 13 .. 13;
      CC3OIE         at 0 range 14 .. 14;
      CC4OIE         at 0 range 15 .. 15;
      CC1DE          at 0 range 16 .. 16;
      Reserved_17_22 at 0 range 17 .. 22;
      UEDE           at 0 range 23 .. 23;
      Reserved_24_24 at 0 range 24 .. 24;
      CC2DE          at 0 range 25 .. 25;
      CC3DE          at 0 range 26 .. 26;
      CC4DE          at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype LPTIM1_CFGR_CKPOL_Field is HAL.UInt2;
   subtype LPTIM1_CFGR_CKFLT_Field is HAL.UInt2;
   subtype LPTIM1_CFGR_TRGFLT_Field is HAL.UInt2;
   subtype LPTIM1_CFGR_PRESC_Field is HAL.UInt3;
   subtype LPTIM1_CFGR_TRIGSEL_Field is HAL.UInt3;
   subtype LPTIM1_CFGR_TRIGEN_Field is HAL.UInt2;

   --  LPTIM configuration register
   type LPTIM1_CFGR_Register is record
      --  Clock selector The CKSEL bit selects which clock source the LPTIM
      --  uses:
      CKSEL          : Boolean := False;
      --  Clock Polarity When the LPTIM is clocked by an external clock source,
      --  CKPOL bits is used to configure the active edge or edges used by the
      --  counter: If the LPTIM is configured in Encoder mode (ENC bit is set),
      --  the encoder sub-mode 1 is active. If the LPTIM is configured in
      --  Encoder mode (ENC bit is set), the encoder sub-mode 2 is active. If
      --  the LPTIM is configured in Encoder mode (ENC bit is set), the encoder
      --  sub-mode 3 is active. Refer to Section125.4.15: Encoder mode for more
      --  details about Encoder mode sub-modes.
      CKPOL          : LPTIM1_CFGR_CKPOL_Field := 16#0#;
      --  Configurable digital filter for external clock The CKFLT value sets
      --  the number of consecutive equal samples that are detected when a
      --  level change occurs on an external clock signal before it is
      --  considered as a valid level transition. An internal clock source must
      --  be present to use this feature
      CKFLT          : LPTIM1_CFGR_CKFLT_Field := 16#0#;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  Configurable digital filter for trigger The TRGFLT value sets the
      --  number of consecutive equal samples that are detected when a level
      --  change occurs on an internal trigger before it is considered as a
      --  valid level transition. An internal clock source must be present to
      --  use this feature
      TRGFLT         : LPTIM1_CFGR_TRGFLT_Field := 16#0#;
      --  unspecified
      Reserved_8_8   : HAL.Bit := 16#0#;
      --  Clock prescaler The PRESC bits configure the prescaler division
      --  factor. It can be one among the following division factors:
      PRESC          : LPTIM1_CFGR_PRESC_Field := 16#0#;
      --  unspecified
      Reserved_12_12 : HAL.Bit := 16#0#;
      --  Trigger selector The TRIGSEL bits select the trigger source that
      --  serves as a trigger event for the LPTIM among the below 8 available
      --  sources: See Section125.4.3: LPTIM input and trigger mapping for
      --  details.
      TRIGSEL        : LPTIM1_CFGR_TRIGSEL_Field := 16#0#;
      --  unspecified
      Reserved_16_16 : HAL.Bit := 16#0#;
      --  Trigger enable and polarity The TRIGEN bits controls whether the
      --  LPTIM counter is started by an external trigger or not. If the
      --  external trigger option is selected, three configurations are
      --  possible for the trigger active edge:
      TRIGEN         : LPTIM1_CFGR_TRIGEN_Field := 16#0#;
      --  Timeout enable The TIMOUT bit controls the Timeout feature
      TIMOUT         : Boolean := False;
      --  Waveform shape The WAVE bit controls the output shape
      WAVE           : Boolean := False;
      --  unspecified
      Reserved_21_21 : HAL.Bit := 16#0#;
      --  Registers update mode The PRELOAD bit controls the LPTIM1_ARR,
      --  LPTIM1_RCR and the LPTIM1_CCRx registers update modality
      PRELOAD        : Boolean := False;
      --  counter mode enabled The COUNTMODE bit selects which clock source is
      --  used by the LPTIM to clock the counter:
      COUNTMODE      : Boolean := False;
      --  Encoder mode enable The ENC bit controls the Encoder mode Note: If
      --  the LPTIM does not support encoder mode feature, this bit is
      --  reserved. Refer to Section125.3.
      ENC            : Boolean := False;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM1_CFGR_Register use record
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
      Reserved_21_21 at 0 range 21 .. 21;
      PRELOAD        at 0 range 22 .. 22;
      COUNTMODE      at 0 range 23 .. 23;
      ENC            at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  LPTIM control register
   type LPTIM1_CR_Register is record
      --  LPTIM enable The ENABLE bit is set and cleared by software.
      ENABLE        : Boolean := False;
      --  LPTIM start in Single mode This bit is set by software and cleared by
      --  hardware. In case of software start (TRIGEN[1:0] = 00), setting this
      --  bit starts the LPTIM in single pulse mode. If the software start is
      --  disabled (TRIGEN[1:0] different than 00), setting this bit starts the
      --  LPTIM in single pulse mode as soon as an external trigger is
      --  detected. If this bit is set when the LPTIM is in continuous counting
      --  mode, then the LPTIM stops at the following match between LPTIM1_ARR
      --  and LPTIM1_CNT registers. This bit can only be set when the LPTIM is
      --  enabled. It is automatically reset by hardware.
      SNGSTRT       : Boolean := False;
      --  Timer start in Continuous mode This bit is set by software and
      --  cleared by hardware. In case of software start (TRIGEN[1:0] = 00),
      --  setting this bit starts the LPTIM in Continuous mode. If the software
      --  start is disabled (TRIGEN[1:0] different than 00), setting this bit
      --  starts the timer in Continuous mode as soon as an external trigger is
      --  detected. If this bit is set when a single pulse mode counting is
      --  ongoing, then the timer does not stop at the next match between the
      --  LPTIM1_ARR and LPTIM1_CNT registers and the LPTIM counter keeps
      --  counting in Continuous mode. This bit can be set only when the LPTIM
      --  is enabled. It is automatically reset by hardware.
      CNTSTRT       : Boolean := False;
      --  Counter reset This bit is set by software and cleared by hardware.
      --  When set to '1' this bit triggers a synchronous reset of the
      --  LPTIM1_CNT counter register. Due to the synchronous nature of this
      --  reset, it only takes place after a synchronization delay of 3 LPTimer
      --  core clock cycles (LPTimer core clock may be different from APB
      --  clock). This bit can be set only when the LPTIM is enabled. It is
      --  automatically reset by hardware. COUNTRST must never be set to '1' by
      --  software before it is already cleared to '0' by hardware. Software
      --  must consequently check that COUNTRST bit is already cleared to '0'
      --  before attempting to set it to '1'.
      COUNTRST      : Boolean := False;
      --  Reset after read enable This bit is set and cleared by software. When
      --  RSTARE is set to '1', any read access to LPTIM1_CNT register
      --  asynchronously resets LPTIM1_CNT register content. This bit can be
      --  set only when the LPTIM is enabled.
      RSTARE        : Boolean := False;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM1_CR_Register use record
      ENABLE        at 0 range 0 .. 0;
      SNGSTRT       at 0 range 1 .. 1;
      CNTSTRT       at 0 range 2 .. 2;
      COUNTRST      at 0 range 3 .. 3;
      RSTARE        at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype LPTIM1_CCR1_CCR1_Field is HAL.UInt16;

   --  LPTIM compare register 1
   type LPTIM1_CCR1_Register is record
      --  Capture/compare 1 value If channel CC1 is configured as output: CCR1
      --  is the value to be loaded in the capture/compare 1 register.
      --  Depending on the PRELOAD option, the CCR1 register is immediately
      --  updated if the PRELOAD bit is reset and updated at next LPTIM update
      --  event if PREOAD bit is reset. The capture/compare register 1 contains
      --  the value to be compared to the counter LPTIM1_CNT and signaled on
      --  OC1 output. If channel CC1 is configured as input: CCR1 becomes
      --  read-only, it contains the counter value transferred by the last
      --  input capture 1 event. The LPTIM1_CCR1 register is read-only and
      --  cannot be programmed.
      CCR1           : LPTIM1_CCR1_CCR1_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM1_CCR1_Register use record
      CCR1           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype LPTIM1_ARR_ARR_Field is HAL.UInt16;

   --  LPTIM autoreload register
   type LPTIM1_ARR_Register is record
      --  Auto reload value ARR is the autoreload value for the LPTIM. This
      --  value must be strictly greater than the CCRx[15:0] value.
      ARR            : LPTIM1_ARR_ARR_Field := 16#1#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM1_ARR_Register use record
      ARR            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype LPTIM1_CNT_CNT_Field is HAL.UInt16;

   --  LPTIM counter register
   type LPTIM1_CNT_Register is record
      --  Read-only. Counter value When the LPTIM is running with an
      --  asynchronous clock, reading the LPTIM1_CNT register may return
      --  unreliable values. So in this case it is necessary to perform two
      --  consecutive read accesses and verify that the two returned values are
      --  identical.
      CNT            : LPTIM1_CNT_CNT_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM1_CNT_Register use record
      CNT            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype LPTIM1_CFGR2_IN1SEL_Field is HAL.UInt2;
   subtype LPTIM1_CFGR2_IN2SEL_Field is HAL.UInt2;
   subtype LPTIM1_CFGR2_IC1SEL_Field is HAL.UInt2;
   subtype LPTIM1_CFGR2_IC2SEL_Field is HAL.UInt2;

   --  LPTIM configuration register 2
   type LPTIM1_CFGR2_Register is record
      --  LPTIM input 1 selection The IN1SEL bits control the LPTIM input 1
      --  multiplexer, which connects LPTIM input 1 to one of the available
      --  inputs. For connection details refer to Section125.4.3: LPTIM input
      --  and trigger mapping.
      IN1SEL         : LPTIM1_CFGR2_IN1SEL_Field := 16#0#;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  LPTIM input 2 selection The IN2SEL bits control the LPTIM input 2
      --  multiplexer, which connects LPTIM input 2 to one of the available
      --  inputs. For connection details refer to Section125.4.3: LPTIM input
      --  and trigger mapping.
      IN2SEL         : LPTIM1_CFGR2_IN2SEL_Field := 16#0#;
      --  unspecified
      Reserved_6_15  : HAL.UInt10 := 16#0#;
      --  LPTIM input capture 1 selection The IC1SEL bits control the LPTIM
      --  Input capture 1 multiplexer, which connects LPTIM Input capture 1 to
      --  one of the available inputs. For connection details refer to
      --  Section125.4.3: LPTIM input and trigger mapping.
      IC1SEL         : LPTIM1_CFGR2_IC1SEL_Field := 16#0#;
      --  unspecified
      Reserved_18_19 : HAL.UInt2 := 16#0#;
      --  LPTIM input capture 2 selection The IC2SEL bits control the LPTIM
      --  Input capture 2 multiplexer, which connects LPTIM Input capture 2 to
      --  one of the available inputs. For connection details refer to
      --  Section125.4.3: LPTIM input and trigger mapping.
      IC2SEL         : LPTIM1_CFGR2_IC2SEL_Field := 16#0#;
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM1_CFGR2_Register use record
      IN1SEL         at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      IN2SEL         at 0 range 4 .. 5;
      Reserved_6_15  at 0 range 6 .. 15;
      IC1SEL         at 0 range 16 .. 17;
      Reserved_18_19 at 0 range 18 .. 19;
      IC2SEL         at 0 range 20 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   subtype LPTIM1_RCR_REP_Field is HAL.UInt8;

   --  LPTIM repetition register
   type LPTIM1_RCR_Register is record
      --  Repetition register value REP is the repetition value for the LPTIM.
      REP           : LPTIM1_RCR_REP_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM1_RCR_Register use record
      REP           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype LPTIM1_CCMR1_CC1P_Field is HAL.UInt2;
   subtype LPTIM1_CCMR1_IC1PSC_Field is HAL.UInt2;
   subtype LPTIM1_CCMR1_IC1F_Field is HAL.UInt2;
   subtype LPTIM1_CCMR1_CC2P_Field is HAL.UInt2;
   subtype LPTIM1_CCMR1_IC2PSC_Field is HAL.UInt2;
   subtype LPTIM1_CCMR1_IC2F_Field is HAL.UInt2;

   --  LPTIM capture/compare mode register 1
   type LPTIM1_CCMR1_Register is record
      --  Capture/compare 1 selection This bitfield defines the direction of
      --  the channel input (capture) or output mode.
      CC1SEL         : Boolean := False;
      --  Capture/compare 1 output enable. This bit determines if a capture of
      --  the counter value can actually be done into the input capture/compare
      --  register 1 (LPTIM1_CCR1) or not.
      CC1E           : Boolean := False;
      --  Capture/compare 1 output polarity. Only bit2 is used to set polarity
      --  when output mode is enabled, bit3 is don't care. This field is used
      --  to select the IC1 polarity for capture operations.
      CC1P           : LPTIM1_CCMR1_CC1P_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Input capture 1 prescaler This bitfield defines the ratio of the
      --  prescaler acting on the CC1 input (IC1).
      IC1PSC         : LPTIM1_CCMR1_IC1PSC_Field := 16#0#;
      --  unspecified
      Reserved_10_11 : HAL.UInt2 := 16#0#;
      --  Input capture 1 filter This bitfield defines the number of
      --  consecutive equal samples that are detected when a level change
      --  occurs on an external input capture signal before it is considered as
      --  a valid level transition. An internal clock source must be present to
      --  use this feature.
      IC1F           : LPTIM1_CCMR1_IC1F_Field := 16#0#;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  Capture/compare 2 selection This bitfield defines the direction of
      --  the channel, input (capture) or output mode.
      CC2SEL         : Boolean := False;
      --  Capture/compare 2 output enable. This bit determines if a capture of
      --  the counter value can actually be done into the input capture/compare
      --  register 2 (LPTIM1_CCR2) or not.
      CC2E           : Boolean := False;
      --  Capture/compare 2 output polarity. Only bit2 is used to set polarity
      --  when output mode is enabled, bit3 is don't care. This field is used
      --  to select the IC2 polarity for capture operations.
      CC2P           : LPTIM1_CCMR1_CC2P_Field := 16#0#;
      --  unspecified
      Reserved_20_23 : HAL.UInt4 := 16#0#;
      --  Input capture 2 prescaler This bitfield defines the ratio of the
      --  prescaler acting on the CC2 input (IC2).
      IC2PSC         : LPTIM1_CCMR1_IC2PSC_Field := 16#0#;
      --  unspecified
      Reserved_26_27 : HAL.UInt2 := 16#0#;
      --  Input capture 2 filter This bitfield defines the number of
      --  consecutive equal samples that are detected when a level change
      --  occurs on an external input capture signal before it is considered as
      --  a valid level transition. An internal clock source must be present to
      --  use this feature.
      IC2F           : LPTIM1_CCMR1_IC2F_Field := 16#0#;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM1_CCMR1_Register use record
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

   subtype LPTIM1_CCMR2_CC3P_Field is HAL.UInt2;
   subtype LPTIM1_CCMR2_IC3PSC_Field is HAL.UInt2;
   subtype LPTIM1_CCMR2_IC3F_Field is HAL.UInt2;
   subtype LPTIM1_CCMR2_CC4P_Field is HAL.UInt2;
   subtype LPTIM1_CCMR2_IC4PSC_Field is HAL.UInt2;
   subtype LPTIM1_CCMR2_IC4F_Field is HAL.UInt2;

   --  LPTIM capture/compare mode register 2
   type LPTIM1_CCMR2_Register is record
      --  Capture/compare 3 selection This bitfield defines the direction of
      --  the channel input (capture) or output mode.
      CC3SEL         : Boolean := False;
      --  Capture/compare 3 output enable. Condition: CC3 as output: Condition:
      --  CC3 as input: This bit determines if a capture of the counter value
      --  can actually be done into the input capture/compare register 3
      --  (LPTIM1_CCR3) or not.
      CC3E           : Boolean := False;
      --  Capture/compare 3 output polarity. Condition: CC3 as output: Only
      --  bit2 is used to set polarity when output mode is enabled, bit3 is
      --  don't care. Condition: CC3 as input: This field is used to select the
      --  IC3 polarity for capture operations.
      CC3P           : LPTIM1_CCMR2_CC3P_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Input capture 3 prescaler This bitfield defines the ratio of the
      --  prescaler acting on the CC3 input (IC3).
      IC3PSC         : LPTIM1_CCMR2_IC3PSC_Field := 16#0#;
      --  unspecified
      Reserved_10_11 : HAL.UInt2 := 16#0#;
      --  Input capture 3 filter This bitfield defines the number of
      --  consecutive equal samples that should be detected when a level change
      --  occurs on an external input capture signal before it is considered as
      --  a valid level transition. An internal clock source must be present to
      --  use this feature.
      IC3F           : LPTIM1_CCMR2_IC3F_Field := 16#0#;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  Capture/compare 4 selection This bitfield defines the direction of
      --  the channel, input (capture) or output mode.
      CC4SEL         : Boolean := False;
      --  Capture/compare 4 output enable. Condition: CC4 as output: Condition:
      --  CC4 as input: This bit determines if a capture of the counter value
      --  can actually be done into the input capture/compare register 4
      --  (LPTIM1_CCR4) or not.
      CC4E           : Boolean := False;
      --  Capture/compare 4 output polarity. Condition: CC4 as output: Only
      --  bit2 is used to set polarity when output mode is enabled, bit3 is
      --  don't care. Condition: CC4 as input: This field is used to select the
      --  IC4 polarity for capture operations.
      CC4P           : LPTIM1_CCMR2_CC4P_Field := 16#0#;
      --  unspecified
      Reserved_20_23 : HAL.UInt4 := 16#0#;
      --  Input capture 4 prescaler This bitfield defines the ratio of the
      --  prescaler acting on the CC4 input (IC4).
      IC4PSC         : LPTIM1_CCMR2_IC4PSC_Field := 16#0#;
      --  unspecified
      Reserved_26_27 : HAL.UInt2 := 16#0#;
      --  Input capture 4 filter This bitfield defines the number of
      --  consecutive equal samples that should be detected when a level change
      --  occurs on an external input capture signal before it is considered as
      --  a valid level transition. An internal clock source must be present to
      --  use this feature.
      IC4F           : LPTIM1_CCMR2_IC4F_Field := 16#0#;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM1_CCMR2_Register use record
      CC3SEL         at 0 range 0 .. 0;
      CC3E           at 0 range 1 .. 1;
      CC3P           at 0 range 2 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      IC3PSC         at 0 range 8 .. 9;
      Reserved_10_11 at 0 range 10 .. 11;
      IC3F           at 0 range 12 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      CC4SEL         at 0 range 16 .. 16;
      CC4E           at 0 range 17 .. 17;
      CC4P           at 0 range 18 .. 19;
      Reserved_20_23 at 0 range 20 .. 23;
      IC4PSC         at 0 range 24 .. 25;
      Reserved_26_27 at 0 range 26 .. 27;
      IC4F           at 0 range 28 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype LPTIM1_CCR2_CCR2_Field is HAL.UInt16;

   --  LPTIM compare register 2
   type LPTIM1_CCR2_Register is record
      --  Capture/compare 2 value If channel CC2 is configured as output: CCR2
      --  is the value to be loaded in the capture/compare 2 register.
      --  Depending on the PRELOAD option, the CCR2 register is immediately
      --  updated if the PRELOAD bit is reset and updated at next LPTIM update
      --  event if PREOAD bit is reset. The capture/compare register 2 contains
      --  the value to be compared to the counter LPTIM1_CNT and signaled on
      --  OC2 output. If channel CC2 is configured as input: CCR2 becomes
      --  read-only, it contains the counter value transferred by the last
      --  input capture 2 event. The LPTIM1_CCR2 register is read-only and
      --  cannot be programmed.
      CCR2           : LPTIM1_CCR2_CCR2_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM1_CCR2_Register use record
      CCR2           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype LPTIM1_CCR3_CCR3_Field is HAL.UInt16;

   --  LPTIM compare register 3
   type LPTIM1_CCR3_Register is record
      --  Capture/compare 3 value If channel CC3 is configured as output: CCR3
      --  is the value to be loaded in the capture/compare 3 register.
      --  Depending on the PRELOAD option, the CCR3 register is immediately
      --  updated if the PRELOAD bit is reset and updated at next LPTIM update
      --  event if PREOAD bit is reset. The capture/compare register 3 contains
      --  the value to be compared to the counter LPTIM1_CNT and signaled on
      --  OC3 output. If channel CC3 is configured as input: CCR3 becomes
      --  read-only, it contains the counter value transferred by the last
      --  input capture 3 event. The LPTIM1_CCR3 register is read-only and
      --  cannot be programmed.
      CCR3           : LPTIM1_CCR3_CCR3_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM1_CCR3_Register use record
      CCR3           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype LPTIM1_CCR4_CCR4_Field is HAL.UInt16;

   --  LPTIM compare register 4
   type LPTIM1_CCR4_Register is record
      --  Capture/compare 4 value If channel CC4 is configured as output: CCR4
      --  is the value to be loaded in the capture/compare 4 register.
      --  Depending on the PRELOAD option, the CCR4 register is immediately
      --  updated if the PRELOAD bit is reset and updated at next LPTIM update
      --  event if PREOAD bit is reset. The capture/compare register 4 contains
      --  the value to be compared to the counter LPTIM1_CNT and signaled on
      --  OC4 output. If channel CC4 is configured as input: CCR4 becomes
      --  read-only, it contains the counter value transferred by the last
      --  input capture 4 event. The LPTIM1_CCR4 register is read-only and
      --  cannot be programmed.
      CCR4           : LPTIM1_CCR4_CCR4_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM1_CCR4_Register use record
      CCR4           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  LPTIM2 interrupt and status register [alternate]
   type LPTIM2_ISR_OUTPUT_Register is record
      --  Read-only. Compare 1 interrupt flag If channel CC1 is configured as
      --  output: The CC1IF flag is set by hardware to inform application that
      --  LPTIM_CNT register value matches the compare register's value. CC1IF
      --  flag can be cleared by writing 1 to the CC1CF bit in the LPTIM_ICR
      --  register.
      CC1IF          : Boolean;
      --  Read-only. Autoreload match ARRM is set by hardware to inform
      --  application that LPTIM_CNT registers value reached the LPTIM_ARR
      --  registers value. ARRM flag can be cleared by writing 1 to the ARRMCF
      --  bit in the LPTIM_ICR register.
      ARRM           : Boolean;
      --  Read-only. External trigger edge event EXTTRIG is set by hardware to
      --  inform application that a valid edge on the selected external trigger
      --  input has occurred. If the trigger is ignored because the timer has
      --  already started, then this flag is not set. EXTTRIG flag can be
      --  cleared by writing 1 to the EXTTRIGCF bit in the LPTIM_ICR register.
      EXTTRIG        : Boolean;
      --  Read-only. Compare register 1 update OK CMP1OK is set by hardware to
      --  inform application that the APB bus write operation to the LPTIM_CCR1
      --  register has been successfully completed. CMP1OK flag can be cleared
      --  by writing 1 to the CMP1OKCF bit in the LPTIM_ICR register.
      CMP1OK         : Boolean;
      --  Read-only. Autoreload register update OK ARROK is set by hardware to
      --  inform application that the APB bus write operation to the LPTIM_ARR
      --  register has been successfully completed. ARROK flag can be cleared
      --  by writing 1 to the ARROKCF bit in the LPTIM_ICR register.
      ARROK          : Boolean;
      --  Read-only. Counter direction change down to up In Encoder mode, UP
      --  bit is set by hardware to inform application that the counter
      --  direction has changed from down to up. UP flag can be cleared by
      --  writing 1 to the UPCF bit in the LPTIM_ICR register. Note: If the
      --  LPTIM does not support encoder mode feature, this bit is reserved.
      --  Refer to Section125.3.
      UP             : Boolean;
      --  Read-only. Counter direction change up to down In Encoder mode, DOWN
      --  bit is set by hardware to inform application that the counter
      --  direction has changed from up to down. DOWN flag can be cleared by
      --  writing 1 to the DOWNCF bit in the LPTIM_ICR register. Note: If the
      --  LPTIM does not support encoder mode feature, this bit is reserved.
      --  Refer to Section125.3.
      DOWN           : Boolean;
      --  Read-only. LPTIM update event occurred UE is set by hardware to
      --  inform application that an update event was generated. The
      --  corresponding interrupt or DMA request is generated if enabled. UE
      --  flag can be cleared by writing 1 to the UECF bit in the LPTIM_ICR
      --  register. The UE flag is automatically cleared by hardware once the
      --  LPTIM_ARR register is written by any bus master like CPU or DMA.
      UE             : Boolean;
      --  Read-only. Repetition register update OK REPOK is set by hardware to
      --  inform application that the APB bus write operation to the LPTIM_RCR
      --  register has been successfully completed. REPOK flag can be cleared
      --  by writing 1 to the REPOKCF bit in the LPTIM_ICR register.
      REPOK          : Boolean;
      --  Read-only. Compare 2 interrupt flag If channel CC2 is configured as
      --  output: The CC2IF flag is set by hardware to inform application that
      --  LPTIM_CNT register value matches the compare register's value. CC2IF
      --  flag can be cleared by writing 1 to the CC2CF bit in the LPTIM_ICR
      --  register. Note: If LPTIM does not implement at least 2 channels this
      --  bit is reserved. Refer to Section125.3.
      CC2IF          : Boolean;
      --  Read-only. Compare 3 interrupt flag If channel CC3 is configured as
      --  output: The CC3IF flag is set by hardware to inform application that
      --  LPTIM_CNT register value matches the compare register's value. CC3IF
      --  flag can be cleared by writing 1 to the CC3CF bit in the LPTIM_ICR
      --  register. Note: If LPTIM does not implement at least 3 channels this
      --  bit is reserved. Refer to Section125.3.
      CC3IF          : Boolean;
      --  Read-only. Compare 4 interrupt flag If channel CC4 is configured as
      --  output: The CC4IF flag is set by hardware to inform application that
      --  LPTIM_CNT register value matches the compare register's value. CC4IF
      --  flag can be cleared by writing 1 to the CC4CF bit in the LPTIM_ICR
      --  register. Note: If LPTIM does not implement at least 4 channels this
      --  bit is reserved. Refer to Section125.3.
      CC4IF          : Boolean;
      --  unspecified
      Reserved_12_18 : HAL.UInt7;
      --  Read-only. Compare register 2 update OK CMP2OK is set by hardware to
      --  inform application that the APB bus write operation to the LPTIM_CCR2
      --  register has been successfully completed. CMP2OK flag can be cleared
      --  by writing 1 to the CMP2OKCF bit in the LPTIM_ICR register. Note: If
      --  LPTIM does not implement at least 2 channels this bit is reserved.
      --  Refer to Section125.3.
      CMP2OK         : Boolean;
      --  Read-only. Compare register 3 update OK CMP3OK is set by hardware to
      --  inform application that the APB bus write operation to the LPTIM_CCR3
      --  register has been successfully completed. CMP3OK flag can be cleared
      --  by writing 1 to the CMP3OKCF bit in the LPTIM_ICR register. Note: If
      --  LPTIM does not implement at least 3 channels this bit is reserved.
      --  Refer to Section125.3.
      CMP3OK         : Boolean;
      --  Read-only. Compare register 4 update OK CMP4OK is set by hardware to
      --  inform application that the APB bus write operation to the LPTIM_CCR4
      --  register has been successfully completed. CMP4OK flag can be cleared
      --  by writing 1 to the CMP4OKCF bit in the LPTIM_ICR register. Note: If
      --  LPTIM does not implement at least 4 channels this bit is reserved.
      --  Refer to Section125.3.
      CMP4OK         : Boolean;
      --  unspecified
      Reserved_22_23 : HAL.UInt2;
      --  Read-only. Interrupt enable register update OK DIEROK is set by
      --  hardware to inform application that the APB bus write operation to
      --  the LPTIM_DIER register has been successfully completed. DIEROK flag
      --  can be cleared by writing 1 to the DIEROKCF bit in the LPTIM_ICR
      --  register.
      DIEROK         : Boolean;
      --  unspecified
      Reserved_25_31 : HAL.UInt7;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM2_ISR_OUTPUT_Register use record
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
      CC3IF          at 0 range 10 .. 10;
      CC4IF          at 0 range 11 .. 11;
      Reserved_12_18 at 0 range 12 .. 18;
      CMP2OK         at 0 range 19 .. 19;
      CMP3OK         at 0 range 20 .. 20;
      CMP4OK         at 0 range 21 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      DIEROK         at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  LPTIM2 interrupt and status register [alternate]
   type LPTIM2_ISR_INPUT_Register is record
      --  Read-only. capture 1 interrupt flag If channel CC1 is configured as
      --  input: CC1IF is set by hardware to inform application that the
      --  current value of the counter is captured in LPTIM_CCR1 register. The
      --  corresponding interrupt or DMA request is generated if enabled. The
      --  CC1OF flag is set if the CC1IF flag was already high.
      CC1IF          : Boolean;
      --  Read-only. Autoreload match ARRM is set by hardware to inform
      --  application that LPTIM_CNT registers value reached the LPTIM_ARR
      --  registers value. ARRM flag can be cleared by writing 1 to the ARRMCF
      --  bit in the LPTIM_ICR register.
      ARRM           : Boolean;
      --  Read-only. External trigger edge event EXTTRIG is set by hardware to
      --  inform application that a valid edge on the selected external trigger
      --  input has occurred. If the trigger is ignored because the timer has
      --  already started, then this flag is not set. EXTTRIG flag can be
      --  cleared by writing 1 to the EXTTRIGCF bit in the LPTIM_ICR register.
      EXTTRIG        : Boolean;
      --  unspecified
      Reserved_3_3   : HAL.Bit;
      --  Read-only. Autoreload register update OK ARROK is set by hardware to
      --  inform application that the APB bus write operation to the LPTIM_ARR
      --  register has been successfully completed. ARROK flag can be cleared
      --  by writing 1 to the ARROKCF bit in the LPTIM_ICR register.
      ARROK          : Boolean;
      --  Read-only. Counter direction change down to up In Encoder mode, UP
      --  bit is set by hardware to inform application that the counter
      --  direction has changed from down to up. UP flag can be cleared by
      --  writing 1 to the UPCF bit in the LPTIM_ICR register. Note: If the
      --  LPTIM does not support encoder mode feature, this bit is reserved.
      --  Refer to Section125.3.
      UP             : Boolean;
      --  Read-only. Counter direction change up to down In Encoder mode, DOWN
      --  bit is set by hardware to inform application that the counter
      --  direction has changed from up to down. DOWN flag can be cleared by
      --  writing 1 to the DOWNCF bit in the LPTIM_ICR register. Note: If the
      --  LPTIM does not support encoder mode feature, this bit is reserved.
      --  Refer to Section125.3.
      DOWN           : Boolean;
      --  Read-only. LPTIM update event occurred UE is set by hardware to
      --  inform application that an update event was generated. UE flag can be
      --  cleared by writing 1 to the UECF bit in the LPTIM_ICR register.
      UE             : Boolean;
      --  Read-only. Repetition register update OK REPOK is set by hardware to
      --  inform application that the APB bus write operation to the LPTIM_RCR
      --  register has been successfully completed. REPOK flag can be cleared
      --  by writing 1 to the REPOKCF bit in the LPTIM_ICR register.
      REPOK          : Boolean;
      --  Read-only. Capture 2 interrupt flag If channel CC2 is configured as
      --  input: CC2IF is set by hardware to inform application that the
      --  current value of the counter is captured in LPTIM_CCR2 register. The
      --  corresponding interrupt or DMA request is generated if enabled. The
      --  CC2OF flag is set if the CC2IF flag was already high. Note: If LPTIM
      --  does not implement at least 2 channels this bit is reserved. Refer to
      --  Section125.3.
      CC2IF          : Boolean;
      --  Read-only. Capture 3 interrupt flag If channel CC3 is configured as
      --  input: CC3IF is set by hardware to inform application that the
      --  current value of the counter is captured in LPTIM_CCR3 register. The
      --  corresponding interrupt or DMA request is generated if enabled. The
      --  CC3OF flag is set if the CC3IF flag was already high. Note: If LPTIM
      --  does not implement at least 3 channels this bit is reserved. Refer to
      --  Section125.3.
      CC3IF          : Boolean;
      --  Read-only. Capture 4 interrupt flag If channel CC4 is configured as
      --  input: CC4IF is set by hardware to inform application that the
      --  current value of the counter is captured in LPTIM_CCR4 register. The
      --  corresponding interrupt or DMA request is generated if enabled. The
      --  CC4OF flag is set if the CC4IF flag was already high. Note: If LPTIM
      --  does not implement at least 4 channels this bit is reserved. Refer to
      --  Section125.3.
      CC4IF          : Boolean;
      --  Read-only. Capture 1 over-capture flag This flag is set by hardware
      --  only when the corresponding channel is configured in input capture
      --  mode. It is cleared by software by writing 1 to the CC1OCF bit in the
      --  LPTIM_ICR register. Note: If LPTIM does not implement at least 1
      --  channel this bit is reserved. Refer to Section125.3.
      CC1OF          : Boolean;
      --  Read-only. Capture 2 over-capture flag This flag is set by hardware
      --  only when the corresponding channel is configured in input capture
      --  mode. It is cleared by software by writing 1 to the CC2OCF bit in the
      --  LPTIM_ICR register. Note: If LPTIM does not implement at least 2
      --  channels this bit is reserved. Refer to Section125.3.
      CC2OF          : Boolean;
      --  Read-only. Capture 3 over-capture flag This flag is set by hardware
      --  only when the corresponding channel is configured in input capture
      --  mode. It is cleared by software by writing 1 to the CC3OCF bit in the
      --  LPTIM_ICR register. Note: If LPTIM does not implement at least 3
      --  channels this bit is reserved. Refer to Section125.3.
      CC3OF          : Boolean;
      --  Read-only. Capture 4 over-capture flag This flag is set by hardware
      --  only when the corresponding channel is configured in input capture
      --  mode. It is cleared by software by writing 1 to the CC4OCF bit in the
      --  LPTIM_ICR register. Note: If LPTIM does not implement at least 4
      --  channels this bit is reserved. Refer to Section125.3.
      CC4OF          : Boolean;
      --  unspecified
      Reserved_16_23 : HAL.UInt8;
      --  Read-only. Interrupt enable register update OK DIEROK is set by
      --  hardware to inform application that the APB bus write operation to
      --  the LPTIM_DIER register has been successfully completed. DIEROK flag
      --  can be cleared by writing 1 to the DIEROKCF bit in the LPTIM_ICR
      --  register.
      DIEROK         : Boolean;
      --  unspecified
      Reserved_25_31 : HAL.UInt7;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM2_ISR_INPUT_Register use record
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
      CC3IF          at 0 range 10 .. 10;
      CC4IF          at 0 range 11 .. 11;
      CC1OF          at 0 range 12 .. 12;
      CC2OF          at 0 range 13 .. 13;
      CC3OF          at 0 range 14 .. 14;
      CC4OF          at 0 range 15 .. 15;
      Reserved_16_23 at 0 range 16 .. 23;
      DIEROK         at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  LPTIM2 interrupt clear register [alternate]
   type LPTIM2_ICR_OUTPUT_Register is record
      --  Write-only. Capture/compare 1 clear flag Writing 1 to this bit clears
      --  the CC1IF flag in the LPTIM_ISR register.
      CC1CF          : Boolean := False;
      --  Write-only. Autoreload match clear flag Writing 1 to this bit clears
      --  the ARRM flag in the LPTIM_ISR register
      ARRMCF         : Boolean := False;
      --  Write-only. External trigger valid edge clear flag Writing 1 to this
      --  bit clears the EXTTRIG flag in the LPTIM_ISR register
      EXTTRIGCF      : Boolean := False;
      --  Write-only. Compare register 1 update OK clear flag Writing 1 to this
      --  bit clears the CMP1OK flag in the LPTIM_ISR register.
      CMP1OKCF       : Boolean := False;
      --  Write-only. Autoreload register update OK clear flag Writing 1 to
      --  this bit clears the ARROK flag in the LPTIM_ISR register
      ARROKCF        : Boolean := False;
      --  Write-only. Direction change to UP clear flag Writing 1 to this bit
      --  clear the UP flag in the LPTIM_ISR register. Note: If the LPTIM does
      --  not support encoder mode feature, this bit is reserved. Refer to
      --  Section125.3.
      UPCF           : Boolean := False;
      --  Write-only. Direction change to down clear flag Writing 1 to this bit
      --  clear the DOWN flag in the LPTIM_ISR register. Note: If the LPTIM
      --  does not support encoder mode feature, this bit is reserved. Refer to
      --  Section125.3.
      DOWNCF         : Boolean := False;
      --  Write-only. Update event clear flag Writing 1 to this bit clear the
      --  UE flag in the LPTIM_ISR register.
      UECF           : Boolean := False;
      --  Write-only. Repetition register update OK clear flag Writing 1 to
      --  this bit clears the REPOK flag in the LPTIM_ISR register.
      REPOKCF        : Boolean := False;
      --  Write-only. Capture/compare 2 clear flag Writing 1 to this bit clears
      --  the CC2IF flag in the LPTIM_ISR register. Note: If LPTIM does not
      --  implement at least 2 channels this bit is reserved. Refer to
      --  Section125.3.
      CC2CF          : Boolean := False;
      --  Write-only. Capture/compare 3 clear flag Writing 1 to this bit clears
      --  the CC3IF flag in the LPTIM_ISR register. Note: If LPTIM does not
      --  implement at least 3 channels this bit is reserved. Refer to
      --  Section125.3.
      CC3CF          : Boolean := False;
      --  Write-only. Capture/compare 4 clear flag Writing 1 to this bit clears
      --  the CC4IF flag in the LPTIM_ISR register. Note: If LPTIM does not
      --  implement at least 4 channels this bit is reserved. Refer to
      --  Section125.3.
      CC4CF          : Boolean := False;
      --  unspecified
      Reserved_12_18 : HAL.UInt7 := 16#0#;
      --  Write-only. Compare register 2 update OK clear flag Writing 1 to this
      --  bit clears the CMP2OK flag in the LPTIM_ISR register. Note: If LPTIM
      --  does not implement at least 2 channels this bit is reserved. Refer to
      --  Section125.3.
      CMP2OKCF       : Boolean := False;
      --  Write-only. Compare register 3 update OK clear flag Writing 1 to this
      --  bit clears the CMP3OK flag in the LPTIM_ISR register. Note: If LPTIM
      --  does not implement at least 3 channels this bit is reserved. Refer to
      --  Section125.3.
      CMP3OKCF       : Boolean := False;
      --  Write-only. Compare register 4 update OK clear flag Writing 1 to this
      --  bit clears the CMP4OK flag in the LPTIM_ISR register. Note: If LPTIM
      --  does not implement at least 4 channels this bit is reserved. Refer to
      --  Section125.3.
      CMP4OKCF       : Boolean := False;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  Write-only. Interrupt enable register update OK clear flag Writing 1
      --  to this bit clears the DIEROK flag in the LPTIM_ISR register.
      DIEROKCF       : Boolean := False;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM2_ICR_OUTPUT_Register use record
      CC1CF          at 0 range 0 .. 0;
      ARRMCF         at 0 range 1 .. 1;
      EXTTRIGCF      at 0 range 2 .. 2;
      CMP1OKCF       at 0 range 3 .. 3;
      ARROKCF        at 0 range 4 .. 4;
      UPCF           at 0 range 5 .. 5;
      DOWNCF         at 0 range 6 .. 6;
      UECF           at 0 range 7 .. 7;
      REPOKCF        at 0 range 8 .. 8;
      CC2CF          at 0 range 9 .. 9;
      CC3CF          at 0 range 10 .. 10;
      CC4CF          at 0 range 11 .. 11;
      Reserved_12_18 at 0 range 12 .. 18;
      CMP2OKCF       at 0 range 19 .. 19;
      CMP3OKCF       at 0 range 20 .. 20;
      CMP4OKCF       at 0 range 21 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      DIEROKCF       at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  LPTIM2 interrupt clear register [alternate]
   type LPTIM2_ICR_INPUT_Register is record
      --  Write-only. Capture/compare 1 clear flag Writing 1 to this bit clears
      --  the CC1IF flag in the LPTIM_ISR register.
      CC1CF          : Boolean := False;
      --  Write-only. Autoreload match clear flag Writing 1 to this bit clears
      --  the ARRM flag in the LPTIM_ISR register
      ARRMCF         : Boolean := False;
      --  Write-only. External trigger valid edge clear flag Writing 1 to this
      --  bit clears the EXTTRIG flag in the LPTIM_ISR register
      EXTTRIGCF      : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Write-only. Autoreload register update OK clear flag Writing 1 to
      --  this bit clears the ARROK flag in the LPTIM_ISR register
      ARROKCF        : Boolean := False;
      --  Write-only. Direction change to UP clear flag Writing 1 to this bit
      --  clear the UP flag in the LPTIM_ISR register. Note: If the LPTIM does
      --  not support encoder mode feature, this bit is reserved. Refer to
      --  Section125.3.
      UPCF           : Boolean := False;
      --  Write-only. Direction change to down clear flag Writing 1 to this bit
      --  clear the DOWN flag in the LPTIM_ISR register. Note: If the LPTIM
      --  does not support encoder mode feature, this bit is reserved. Refer to
      --  Section125.3.
      DOWNCF         : Boolean := False;
      --  Write-only. Update event clear flag Writing 1 to this bit clear the
      --  UE flag in the LPTIM_ISR register.
      UECF           : Boolean := False;
      --  Write-only. Repetition register update OK clear flag Writing 1 to
      --  this bit clears the REPOK flag in the LPTIM_ISR register.
      REPOKCF        : Boolean := False;
      --  Write-only. Capture/compare 2 clear flag Writing 1 to this bit clears
      --  the CC2IF flag in the LPTIM_ISR register. Note: If LPTIM does not
      --  implement at least 2 channels this bit is reserved. Refer to
      --  Section125.3.
      CC2CF          : Boolean := False;
      --  Write-only. Capture/compare 3 clear flag Writing 1 to this bit clears
      --  the CC3IF flag in the LPTIM_ISR register. Note: If LPTIM does not
      --  implement at least 3 channels this bit is reserved. Refer to
      --  Section125.3.
      CC3CF          : Boolean := False;
      --  Write-only. Capture/compare 4 clear flag Writing 1 to this bit clears
      --  the CC4IF flag in the LPTIM_ISR register. Note: If LPTIM does not
      --  implement at least 4 channels this bit is reserved. Refer to
      --  Section125.3.
      CC4CF          : Boolean := False;
      --  Write-only. Capture/compare 1 over-capture clear flag Writing 1 to
      --  this bit clears the CC1OF flag in the LPTIM_ISR register. Note: If
      --  LPTIM does not implement at least 1 channel this bit is reserved.
      --  Refer to Section125.3.
      CC1OCF         : Boolean := False;
      --  Write-only. Capture/compare 2 over-capture clear flag Writing 1 to
      --  this bit clears the CC2OF flag in the LPTIM_ISR register. Note: If
      --  LPTIM does not implement at least 2 channels this bit is reserved.
      --  Refer to Section125.3.
      CC2OCF         : Boolean := False;
      --  Write-only. Capture/compare 3 over-capture clear flag Writing 1 to
      --  this bit clears the CC3OF flag in the LPTIM_ISR register. Note: If
      --  LPTIM does not implement at least 3 channels this bit is reserved.
      --  Refer to Section125.3.
      CC3OCF         : Boolean := False;
      --  Write-only. Capture/compare 4 over-capture clear flag Writing 1 to
      --  this bit clears the CC4OF flag in the LPTIM_ISR register. Note: If
      --  LPTIM does not implement at least 4 channels this bit is reserved.
      --  Refer to Section125.3.
      CC4OCF         : Boolean := False;
      --  unspecified
      Reserved_16_23 : HAL.UInt8 := 16#0#;
      --  Write-only. Interrupt enable register update OK clear flag Writing 1
      --  to this bit clears the DIEROK flag in the LPTIM_ISR register.
      DIEROKCF       : Boolean := False;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM2_ICR_INPUT_Register use record
      CC1CF          at 0 range 0 .. 0;
      ARRMCF         at 0 range 1 .. 1;
      EXTTRIGCF      at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      ARROKCF        at 0 range 4 .. 4;
      UPCF           at 0 range 5 .. 5;
      DOWNCF         at 0 range 6 .. 6;
      UECF           at 0 range 7 .. 7;
      REPOKCF        at 0 range 8 .. 8;
      CC2CF          at 0 range 9 .. 9;
      CC3CF          at 0 range 10 .. 10;
      CC4CF          at 0 range 11 .. 11;
      CC1OCF         at 0 range 12 .. 12;
      CC2OCF         at 0 range 13 .. 13;
      CC3OCF         at 0 range 14 .. 14;
      CC4OCF         at 0 range 15 .. 15;
      Reserved_16_23 at 0 range 16 .. 23;
      DIEROKCF       at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  LPTIM2 interrupt enable register [alternate]
   type LPTIM2_DIER_OUTPUT_Register is record
      --  Capture/compare 1 interrupt enable
      CC1IE          : Boolean := False;
      --  Autoreload match Interrupt Enable
      ARRMIE         : Boolean := False;
      --  External trigger valid edge Interrupt Enable
      EXTTRIGIE      : Boolean := False;
      --  Compare register 1 update OK interrupt enable
      CMP1OKIE       : Boolean := False;
      --  Autoreload register update OK Interrupt Enable
      ARROKIE        : Boolean := False;
      --  Direction change to UP Interrupt Enable Note: If the LPTIM does not
      --  support encoder mode feature, this bit is reserved. Refer to
      --  Section125.3.
      UPIE           : Boolean := False;
      --  Direction change to down Interrupt Enable Note: If the LPTIM does not
      --  support encoder mode feature, this bit is reserved. Refer to
      --  Section125.3.
      DOWNIE         : Boolean := False;
      --  Update event interrupt enable
      UEIE           : Boolean := False;
      --  Repetition register update OK interrupt Enable
      REPOKIE        : Boolean := False;
      --  Capture/compare 2 interrupt enable Note: If LPTIM does not implement
      --  at least 2 channels this bit is reserved. Refer to Section125.3.
      CC2IE          : Boolean := False;
      --  Capture/compare 3 interrupt enable Note: If LPTIM does not implement
      --  at least 3 channels this bit is reserved. Refer to Section125.3.
      CC3IE          : Boolean := False;
      --  Capture/compare 4 interrupt enable Note: If LPTIM does not implement
      --  at least 4 channels this bit is reserved. Refer to Section125.3.
      CC4IE          : Boolean := False;
      --  unspecified
      Reserved_12_18 : HAL.UInt7 := 16#0#;
      --  Compare register 2 update OK interrupt enable Note: If LPTIM does not
      --  implement at least 2 channels this bit is reserved. Refer to
      --  Section125.3.
      CMP2OKIE       : Boolean := False;
      --  Compare register 3 update OK interrupt enable Note: If LPTIM does not
      --  implement at least 3 channels this bit is reserved. Refer to
      --  Section125.3.
      CMP3OKIE       : Boolean := False;
      --  Compare register 4 update OK interrupt enable Note: If LPTIM does not
      --  implement at least 4 channels this bit is reserved. Refer to
      --  Section125.3.
      CMP4OKIE       : Boolean := False;
      --  unspecified
      Reserved_22_22 : HAL.Bit := 16#0#;
      --  Update event DMA request enable Note: If LPTIM does not implement at
      --  least 1 channel this bit is reserved. Refer to Section125.3.
      UEDE           : Boolean := False;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM2_DIER_OUTPUT_Register use record
      CC1IE          at 0 range 0 .. 0;
      ARRMIE         at 0 range 1 .. 1;
      EXTTRIGIE      at 0 range 2 .. 2;
      CMP1OKIE       at 0 range 3 .. 3;
      ARROKIE        at 0 range 4 .. 4;
      UPIE           at 0 range 5 .. 5;
      DOWNIE         at 0 range 6 .. 6;
      UEIE           at 0 range 7 .. 7;
      REPOKIE        at 0 range 8 .. 8;
      CC2IE          at 0 range 9 .. 9;
      CC3IE          at 0 range 10 .. 10;
      CC4IE          at 0 range 11 .. 11;
      Reserved_12_18 at 0 range 12 .. 18;
      CMP2OKIE       at 0 range 19 .. 19;
      CMP3OKIE       at 0 range 20 .. 20;
      CMP4OKIE       at 0 range 21 .. 21;
      Reserved_22_22 at 0 range 22 .. 22;
      UEDE           at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  LPTIM2 interrupt enable register [alternate]
   type LPTIM2_DIER_INPUT_Register is record
      --  Capture/compare 1 interrupt enable
      CC1IE          : Boolean := False;
      --  Autoreload match Interrupt Enable
      ARRMIE         : Boolean := False;
      --  External trigger valid edge Interrupt Enable
      EXTTRIGIE      : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Autoreload register update OK Interrupt Enable
      ARROKIE        : Boolean := False;
      --  Direction change to UP Interrupt Enable Note: If the LPTIM does not
      --  support encoder mode feature, this bit is reserved. Refer to
      --  Section125.3.
      UPIE           : Boolean := False;
      --  Direction change to down Interrupt Enable Note: If the LPTIM does not
      --  support encoder mode feature, this bit is reserved. Refer to
      --  Section125.3.
      DOWNIE         : Boolean := False;
      --  Update event interrupt enable
      UEIE           : Boolean := False;
      --  Repetition register update OK interrupt Enable
      REPOKIE        : Boolean := False;
      --  Capture/compare 2 interrupt enable Note: If LPTIM does not implement
      --  at least 2 channels this bit is reserved. Refer to Section125.3.
      CC2IE          : Boolean := False;
      --  Capture/compare 3 interrupt enable Note: If LPTIM does not implement
      --  at least 3 channels this bit is reserved. Refer to Section125.3.
      CC3IE          : Boolean := False;
      --  Capture/compare 4 interrupt enable Note: If LPTIM does not implement
      --  at least 4 channels this bit is reserved. Refer to Section125.3.
      CC4IE          : Boolean := False;
      --  Capture/compare 1 over-capture interrupt enable Note: If LPTIM does
      --  not implement at least 1 channel this bit is reserved. Refer to
      --  Section125.3.
      CC1OIE         : Boolean := False;
      --  Capture/compare 2 over-capture interrupt enable Note: If LPTIM does
      --  not implement at least 2 channels this bit is reserved. Refer to
      --  Section125.3.
      CC2OIE         : Boolean := False;
      --  Capture/compare 3 over-capture interrupt enable Note: If LPTIM does
      --  not implement at least 3 channels this bit is reserved. Refer to
      --  Section125.3.
      CC3OIE         : Boolean := False;
      --  Capture/compare 4 over-capture interrupt enable Note: If LPTIM does
      --  not implement at least 4 channels this bit is reserved. Refer to
      --  Section125.3.
      CC4OIE         : Boolean := False;
      --  Capture/compare 1 DMA request enable Note: If LPTIM does not
      --  implement at least 1 channel this bit is reserved. Refer to
      --  Section125.3.
      CC1DE          : Boolean := False;
      --  unspecified
      Reserved_17_22 : HAL.UInt6 := 16#0#;
      --  Update event DMA request enable Note: If LPTIM does not implement at
      --  least 1 channel this bit is reserved. Refer to Section125.3.
      UEDE           : Boolean := False;
      --  unspecified
      Reserved_24_24 : HAL.Bit := 16#0#;
      --  Capture/compare 2 DMA request enable Note: If LPTIM does not
      --  implement at least 2 channels this bit is reserved. Refer to
      --  Section125.3.
      CC2DE          : Boolean := False;
      --  Capture/compare 3 DMA request enable Note: If LPTIM does not
      --  implement at least 3 channels this bit is reserved. Refer to
      --  Section125.3.
      CC3DE          : Boolean := False;
      --  Capture/compare 4 DMA request enable Note: If LPTIM does not
      --  implement at least 4 channels this bit is reserved. Refer to
      --  Section125.3.
      CC4DE          : Boolean := False;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM2_DIER_INPUT_Register use record
      CC1IE          at 0 range 0 .. 0;
      ARRMIE         at 0 range 1 .. 1;
      EXTTRIGIE      at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      ARROKIE        at 0 range 4 .. 4;
      UPIE           at 0 range 5 .. 5;
      DOWNIE         at 0 range 6 .. 6;
      UEIE           at 0 range 7 .. 7;
      REPOKIE        at 0 range 8 .. 8;
      CC2IE          at 0 range 9 .. 9;
      CC3IE          at 0 range 10 .. 10;
      CC4IE          at 0 range 11 .. 11;
      CC1OIE         at 0 range 12 .. 12;
      CC2OIE         at 0 range 13 .. 13;
      CC3OIE         at 0 range 14 .. 14;
      CC4OIE         at 0 range 15 .. 15;
      CC1DE          at 0 range 16 .. 16;
      Reserved_17_22 at 0 range 17 .. 22;
      UEDE           at 0 range 23 .. 23;
      Reserved_24_24 at 0 range 24 .. 24;
      CC2DE          at 0 range 25 .. 25;
      CC3DE          at 0 range 26 .. 26;
      CC4DE          at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype LPTIM2_CFGR_CKPOL_Field is HAL.UInt2;
   subtype LPTIM2_CFGR_CKFLT_Field is HAL.UInt2;
   subtype LPTIM2_CFGR_TRGFLT_Field is HAL.UInt2;
   subtype LPTIM2_CFGR_PRESC_Field is HAL.UInt3;
   subtype LPTIM2_CFGR_TRIGSEL_Field is HAL.UInt3;
   subtype LPTIM2_CFGR_TRIGEN_Field is HAL.UInt2;

   --  LPTIM configuration register
   type LPTIM2_CFGR_Register is record
      --  Clock selector The CKSEL bit selects which clock source the LPTIM
      --  uses:
      CKSEL          : Boolean := False;
      --  Clock Polarity When the LPTIM is clocked by an external clock source,
      --  CKPOL bits is used to configure the active edge or edges used by the
      --  counter: If the LPTIM is configured in Encoder mode (ENC bit is set),
      --  the encoder sub-mode 1 is active. If the LPTIM is configured in
      --  Encoder mode (ENC bit is set), the encoder sub-mode 2 is active. If
      --  the LPTIM is configured in Encoder mode (ENC bit is set), the encoder
      --  sub-mode 3 is active. Refer to Section125.4.15: Encoder mode for more
      --  details about Encoder mode sub-modes.
      CKPOL          : LPTIM2_CFGR_CKPOL_Field := 16#0#;
      --  Configurable digital filter for external clock The CKFLT value sets
      --  the number of consecutive equal samples that are detected when a
      --  level change occurs on an external clock signal before it is
      --  considered as a valid level transition. An internal clock source must
      --  be present to use this feature
      CKFLT          : LPTIM2_CFGR_CKFLT_Field := 16#0#;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  Configurable digital filter for trigger The TRGFLT value sets the
      --  number of consecutive equal samples that are detected when a level
      --  change occurs on an internal trigger before it is considered as a
      --  valid level transition. An internal clock source must be present to
      --  use this feature
      TRGFLT         : LPTIM2_CFGR_TRGFLT_Field := 16#0#;
      --  unspecified
      Reserved_8_8   : HAL.Bit := 16#0#;
      --  Clock prescaler The PRESC bits configure the prescaler division
      --  factor. It can be one among the following division factors:
      PRESC          : LPTIM2_CFGR_PRESC_Field := 16#0#;
      --  unspecified
      Reserved_12_12 : HAL.Bit := 16#0#;
      --  Trigger selector The TRIGSEL bits select the trigger source that
      --  serves as a trigger event for the LPTIM among the below 8 available
      --  sources: See Section125.4.3: LPTIM input and trigger mapping for
      --  details.
      TRIGSEL        : LPTIM2_CFGR_TRIGSEL_Field := 16#0#;
      --  unspecified
      Reserved_16_16 : HAL.Bit := 16#0#;
      --  Trigger enable and polarity The TRIGEN bits controls whether the
      --  LPTIM counter is started by an external trigger or not. If the
      --  external trigger option is selected, three configurations are
      --  possible for the trigger active edge:
      TRIGEN         : LPTIM2_CFGR_TRIGEN_Field := 16#0#;
      --  Timeout enable The TIMOUT bit controls the Timeout feature
      TIMOUT         : Boolean := False;
      --  Waveform shape The WAVE bit controls the output shape
      WAVE           : Boolean := False;
      --  unspecified
      Reserved_21_21 : HAL.Bit := 16#0#;
      --  Registers update mode The PRELOAD bit controls the LPTIM2_ARR,
      --  LPTIM2_RCR and the LPTIM2_CCRx registers update modality
      PRELOAD        : Boolean := False;
      --  counter mode enabled The COUNTMODE bit selects which clock source is
      --  used by the LPTIM to clock the counter:
      COUNTMODE      : Boolean := False;
      --  Encoder mode enable The ENC bit controls the Encoder mode Note: If
      --  the LPTIM does not support encoder mode feature, this bit is
      --  reserved. Refer to Section125.3.
      ENC            : Boolean := False;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM2_CFGR_Register use record
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
      Reserved_21_21 at 0 range 21 .. 21;
      PRELOAD        at 0 range 22 .. 22;
      COUNTMODE      at 0 range 23 .. 23;
      ENC            at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  LPTIM control register
   type LPTIM2_CR_Register is record
      --  LPTIM enable The ENABLE bit is set and cleared by software.
      ENABLE        : Boolean := False;
      --  LPTIM start in Single mode This bit is set by software and cleared by
      --  hardware. In case of software start (TRIGEN[1:0] = 00), setting this
      --  bit starts the LPTIM in single pulse mode. If the software start is
      --  disabled (TRIGEN[1:0] different than 00), setting this bit starts the
      --  LPTIM in single pulse mode as soon as an external trigger is
      --  detected. If this bit is set when the LPTIM is in continuous counting
      --  mode, then the LPTIM stops at the following match between LPTIM2_ARR
      --  and LPTIM2_CNT registers. This bit can only be set when the LPTIM is
      --  enabled. It is automatically reset by hardware.
      SNGSTRT       : Boolean := False;
      --  Timer start in Continuous mode This bit is set by software and
      --  cleared by hardware. In case of software start (TRIGEN[1:0] = 00),
      --  setting this bit starts the LPTIM in Continuous mode. If the software
      --  start is disabled (TRIGEN[1:0] different than 00), setting this bit
      --  starts the timer in Continuous mode as soon as an external trigger is
      --  detected. If this bit is set when a single pulse mode counting is
      --  ongoing, then the timer does not stop at the next match between the
      --  LPTIM2_ARR and LPTIM2_CNT registers and the LPTIM counter keeps
      --  counting in Continuous mode. This bit can be set only when the LPTIM
      --  is enabled. It is automatically reset by hardware.
      CNTSTRT       : Boolean := False;
      --  Counter reset This bit is set by software and cleared by hardware.
      --  When set to '1' this bit triggers a synchronous reset of the
      --  LPTIM2_CNT counter register. Due to the synchronous nature of this
      --  reset, it only takes place after a synchronization delay of 3 LPTimer
      --  core clock cycles (LPTimer core clock may be different from APB
      --  clock). This bit can be set only when the LPTIM is enabled. It is
      --  automatically reset by hardware. COUNTRST must never be set to '1' by
      --  software before it is already cleared to '0' by hardware. Software
      --  must consequently check that COUNTRST bit is already cleared to '0'
      --  before attempting to set it to '1'.
      COUNTRST      : Boolean := False;
      --  Reset after read enable This bit is set and cleared by software. When
      --  RSTARE is set to '1', any read access to LPTIM2_CNT register
      --  asynchronously resets LPTIM2_CNT register content. This bit can be
      --  set only when the LPTIM is enabled.
      RSTARE        : Boolean := False;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM2_CR_Register use record
      ENABLE        at 0 range 0 .. 0;
      SNGSTRT       at 0 range 1 .. 1;
      CNTSTRT       at 0 range 2 .. 2;
      COUNTRST      at 0 range 3 .. 3;
      RSTARE        at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype LPTIM2_CCR1_CCR1_Field is HAL.UInt16;

   --  LPTIM compare register 1
   type LPTIM2_CCR1_Register is record
      --  Capture/compare 1 value If channel CC1 is configured as output: CCR1
      --  is the value to be loaded in the capture/compare 1 register.
      --  Depending on the PRELOAD option, the CCR1 register is immediately
      --  updated if the PRELOAD bit is reset and updated at next LPTIM update
      --  event if PREOAD bit is reset. The capture/compare register 1 contains
      --  the value to be compared to the counter LPTIM2_CNT and signaled on
      --  OC1 output. If channel CC1 is configured as input: CCR1 becomes
      --  read-only, it contains the counter value transferred by the last
      --  input capture 1 event. The LPTIM2_CCR1 register is read-only and
      --  cannot be programmed.
      CCR1           : LPTIM2_CCR1_CCR1_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM2_CCR1_Register use record
      CCR1           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype LPTIM2_ARR_ARR_Field is HAL.UInt16;

   --  LPTIM autoreload register
   type LPTIM2_ARR_Register is record
      --  Auto reload value ARR is the autoreload value for the LPTIM. This
      --  value must be strictly greater than the CCRx[15:0] value.
      ARR            : LPTIM2_ARR_ARR_Field := 16#1#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM2_ARR_Register use record
      ARR            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype LPTIM2_CNT_CNT_Field is HAL.UInt16;

   --  LPTIM counter register
   type LPTIM2_CNT_Register is record
      --  Read-only. Counter value When the LPTIM is running with an
      --  asynchronous clock, reading the LPTIM2_CNT register may return
      --  unreliable values. So in this case it is necessary to perform two
      --  consecutive read accesses and verify that the two returned values are
      --  identical.
      CNT            : LPTIM2_CNT_CNT_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM2_CNT_Register use record
      CNT            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype LPTIM2_CFGR2_IN1SEL_Field is HAL.UInt2;
   subtype LPTIM2_CFGR2_IN2SEL_Field is HAL.UInt2;
   subtype LPTIM2_CFGR2_IC1SEL_Field is HAL.UInt2;
   subtype LPTIM2_CFGR2_IC2SEL_Field is HAL.UInt2;

   --  LPTIM configuration register 2
   type LPTIM2_CFGR2_Register is record
      --  LPTIM input 1 selection The IN1SEL bits control the LPTIM input 1
      --  multiplexer, which connects LPTIM input 1 to one of the available
      --  inputs. For connection details refer to Section125.4.3: LPTIM input
      --  and trigger mapping.
      IN1SEL         : LPTIM2_CFGR2_IN1SEL_Field := 16#0#;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  LPTIM input 2 selection The IN2SEL bits control the LPTIM input 2
      --  multiplexer, which connects LPTIM input 2 to one of the available
      --  inputs. For connection details refer to Section125.4.3: LPTIM input
      --  and trigger mapping.
      IN2SEL         : LPTIM2_CFGR2_IN2SEL_Field := 16#0#;
      --  unspecified
      Reserved_6_15  : HAL.UInt10 := 16#0#;
      --  LPTIM input capture 1 selection The IC1SEL bits control the LPTIM
      --  Input capture 1 multiplexer, which connects LPTIM Input capture 1 to
      --  one of the available inputs. For connection details refer to
      --  Section125.4.3: LPTIM input and trigger mapping.
      IC1SEL         : LPTIM2_CFGR2_IC1SEL_Field := 16#0#;
      --  unspecified
      Reserved_18_19 : HAL.UInt2 := 16#0#;
      --  LPTIM input capture 2 selection The IC2SEL bits control the LPTIM
      --  Input capture 2 multiplexer, which connects LPTIM Input capture 2 to
      --  one of the available inputs. For connection details refer to
      --  Section125.4.3: LPTIM input and trigger mapping.
      IC2SEL         : LPTIM2_CFGR2_IC2SEL_Field := 16#0#;
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM2_CFGR2_Register use record
      IN1SEL         at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      IN2SEL         at 0 range 4 .. 5;
      Reserved_6_15  at 0 range 6 .. 15;
      IC1SEL         at 0 range 16 .. 17;
      Reserved_18_19 at 0 range 18 .. 19;
      IC2SEL         at 0 range 20 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   subtype LPTIM2_RCR_REP_Field is HAL.UInt8;

   --  LPTIM repetition register
   type LPTIM2_RCR_Register is record
      --  Repetition register value REP is the repetition value for the LPTIM.
      REP           : LPTIM2_RCR_REP_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM2_RCR_Register use record
      REP           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype LPTIM2_CCMR1_CC1P_Field is HAL.UInt2;
   subtype LPTIM2_CCMR1_IC1PSC_Field is HAL.UInt2;
   subtype LPTIM2_CCMR1_IC1F_Field is HAL.UInt2;
   subtype LPTIM2_CCMR1_CC2P_Field is HAL.UInt2;
   subtype LPTIM2_CCMR1_IC2PSC_Field is HAL.UInt2;
   subtype LPTIM2_CCMR1_IC2F_Field is HAL.UInt2;

   --  LPTIM capture/compare mode register 1
   type LPTIM2_CCMR1_Register is record
      --  Capture/compare 1 selection This bitfield defines the direction of
      --  the channel input (capture) or output mode.
      CC1SEL         : Boolean := False;
      --  Capture/compare 1 output enable. This bit determines if a capture of
      --  the counter value can actually be done into the input capture/compare
      --  register 1 (LPTIM2_CCR1) or not.
      CC1E           : Boolean := False;
      --  Capture/compare 1 output polarity. Only bit2 is used to set polarity
      --  when output mode is enabled, bit3 is don't care. This field is used
      --  to select the IC1 polarity for capture operations.
      CC1P           : LPTIM2_CCMR1_CC1P_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Input capture 1 prescaler This bitfield defines the ratio of the
      --  prescaler acting on the CC1 input (IC1).
      IC1PSC         : LPTIM2_CCMR1_IC1PSC_Field := 16#0#;
      --  unspecified
      Reserved_10_11 : HAL.UInt2 := 16#0#;
      --  Input capture 1 filter This bitfield defines the number of
      --  consecutive equal samples that are detected when a level change
      --  occurs on an external input capture signal before it is considered as
      --  a valid level transition. An internal clock source must be present to
      --  use this feature.
      IC1F           : LPTIM2_CCMR1_IC1F_Field := 16#0#;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  Capture/compare 2 selection This bitfield defines the direction of
      --  the channel, input (capture) or output mode.
      CC2SEL         : Boolean := False;
      --  Capture/compare 2 output enable. This bit determines if a capture of
      --  the counter value can actually be done into the input capture/compare
      --  register 2 (LPTIM2_CCR2) or not.
      CC2E           : Boolean := False;
      --  Capture/compare 2 output polarity. Only bit2 is used to set polarity
      --  when output mode is enabled, bit3 is don't care. This field is used
      --  to select the IC2 polarity for capture operations.
      CC2P           : LPTIM2_CCMR1_CC2P_Field := 16#0#;
      --  unspecified
      Reserved_20_23 : HAL.UInt4 := 16#0#;
      --  Input capture 2 prescaler This bitfield defines the ratio of the
      --  prescaler acting on the CC2 input (IC2).
      IC2PSC         : LPTIM2_CCMR1_IC2PSC_Field := 16#0#;
      --  unspecified
      Reserved_26_27 : HAL.UInt2 := 16#0#;
      --  Input capture 2 filter This bitfield defines the number of
      --  consecutive equal samples that are detected when a level change
      --  occurs on an external input capture signal before it is considered as
      --  a valid level transition. An internal clock source must be present to
      --  use this feature.
      IC2F           : LPTIM2_CCMR1_IC2F_Field := 16#0#;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM2_CCMR1_Register use record
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

   subtype LPTIM2_CCMR2_CC3P_Field is HAL.UInt2;
   subtype LPTIM2_CCMR2_IC3PSC_Field is HAL.UInt2;
   subtype LPTIM2_CCMR2_IC3F_Field is HAL.UInt2;
   subtype LPTIM2_CCMR2_CC4P_Field is HAL.UInt2;
   subtype LPTIM2_CCMR2_IC4PSC_Field is HAL.UInt2;
   subtype LPTIM2_CCMR2_IC4F_Field is HAL.UInt2;

   --  LPTIM capture/compare mode register 2
   type LPTIM2_CCMR2_Register is record
      --  Capture/compare 3 selection This bitfield defines the direction of
      --  the channel input (capture) or output mode.
      CC3SEL         : Boolean := False;
      --  Capture/compare 3 output enable. Condition: CC3 as output: Condition:
      --  CC3 as input: This bit determines if a capture of the counter value
      --  can actually be done into the input capture/compare register 3
      --  (LPTIM2_CCR3) or not.
      CC3E           : Boolean := False;
      --  Capture/compare 3 output polarity. Condition: CC3 as output: Only
      --  bit2 is used to set polarity when output mode is enabled, bit3 is
      --  don't care. Condition: CC3 as input: This field is used to select the
      --  IC3 polarity for capture operations.
      CC3P           : LPTIM2_CCMR2_CC3P_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Input capture 3 prescaler This bitfield defines the ratio of the
      --  prescaler acting on the CC3 input (IC3).
      IC3PSC         : LPTIM2_CCMR2_IC3PSC_Field := 16#0#;
      --  unspecified
      Reserved_10_11 : HAL.UInt2 := 16#0#;
      --  Input capture 3 filter This bitfield defines the number of
      --  consecutive equal samples that should be detected when a level change
      --  occurs on an external input capture signal before it is considered as
      --  a valid level transition. An internal clock source must be present to
      --  use this feature.
      IC3F           : LPTIM2_CCMR2_IC3F_Field := 16#0#;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  Capture/compare 4 selection This bitfield defines the direction of
      --  the channel, input (capture) or output mode.
      CC4SEL         : Boolean := False;
      --  Capture/compare 4 output enable. Condition: CC4 as output: Condition:
      --  CC4 as input: This bit determines if a capture of the counter value
      --  can actually be done into the input capture/compare register 4
      --  (LPTIM2_CCR4) or not.
      CC4E           : Boolean := False;
      --  Capture/compare 4 output polarity. Condition: CC4 as output: Only
      --  bit2 is used to set polarity when output mode is enabled, bit3 is
      --  don't care. Condition: CC4 as input: This field is used to select the
      --  IC4 polarity for capture operations.
      CC4P           : LPTIM2_CCMR2_CC4P_Field := 16#0#;
      --  unspecified
      Reserved_20_23 : HAL.UInt4 := 16#0#;
      --  Input capture 4 prescaler This bitfield defines the ratio of the
      --  prescaler acting on the CC4 input (IC4).
      IC4PSC         : LPTIM2_CCMR2_IC4PSC_Field := 16#0#;
      --  unspecified
      Reserved_26_27 : HAL.UInt2 := 16#0#;
      --  Input capture 4 filter This bitfield defines the number of
      --  consecutive equal samples that should be detected when a level change
      --  occurs on an external input capture signal before it is considered as
      --  a valid level transition. An internal clock source must be present to
      --  use this feature.
      IC4F           : LPTIM2_CCMR2_IC4F_Field := 16#0#;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM2_CCMR2_Register use record
      CC3SEL         at 0 range 0 .. 0;
      CC3E           at 0 range 1 .. 1;
      CC3P           at 0 range 2 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      IC3PSC         at 0 range 8 .. 9;
      Reserved_10_11 at 0 range 10 .. 11;
      IC3F           at 0 range 12 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      CC4SEL         at 0 range 16 .. 16;
      CC4E           at 0 range 17 .. 17;
      CC4P           at 0 range 18 .. 19;
      Reserved_20_23 at 0 range 20 .. 23;
      IC4PSC         at 0 range 24 .. 25;
      Reserved_26_27 at 0 range 26 .. 27;
      IC4F           at 0 range 28 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype LPTIM2_CCR2_CCR2_Field is HAL.UInt16;

   --  LPTIM compare register 2
   type LPTIM2_CCR2_Register is record
      --  Capture/compare 2 value If channel CC2 is configured as output: CCR2
      --  is the value to be loaded in the capture/compare 2 register.
      --  Depending on the PRELOAD option, the CCR2 register is immediately
      --  updated if the PRELOAD bit is reset and updated at next LPTIM update
      --  event if PREOAD bit is reset. The capture/compare register 2 contains
      --  the value to be compared to the counter LPTIM2_CNT and signaled on
      --  OC2 output. If channel CC2 is configured as input: CCR2 becomes
      --  read-only, it contains the counter value transferred by the last
      --  input capture 2 event. The LPTIM2_CCR2 register is read-only and
      --  cannot be programmed.
      CCR2           : LPTIM2_CCR2_CCR2_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM2_CCR2_Register use record
      CCR2           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype LPTIM2_CCR3_CCR3_Field is HAL.UInt16;

   --  LPTIM compare register 3
   type LPTIM2_CCR3_Register is record
      --  Capture/compare 3 value If channel CC3 is configured as output: CCR3
      --  is the value to be loaded in the capture/compare 3 register.
      --  Depending on the PRELOAD option, the CCR3 register is immediately
      --  updated if the PRELOAD bit is reset and updated at next LPTIM update
      --  event if PREOAD bit is reset. The capture/compare register 3 contains
      --  the value to be compared to the counter LPTIM2_CNT and signaled on
      --  OC3 output. If channel CC3 is configured as input: CCR3 becomes
      --  read-only, it contains the counter value transferred by the last
      --  input capture 3 event. The LPTIM2_CCR3 register is read-only and
      --  cannot be programmed.
      CCR3           : LPTIM2_CCR3_CCR3_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM2_CCR3_Register use record
      CCR3           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype LPTIM2_CCR4_CCR4_Field is HAL.UInt16;

   --  LPTIM compare register 4
   type LPTIM2_CCR4_Register is record
      --  Capture/compare 4 value If channel CC4 is configured as output: CCR4
      --  is the value to be loaded in the capture/compare 4 register.
      --  Depending on the PRELOAD option, the CCR4 register is immediately
      --  updated if the PRELOAD bit is reset and updated at next LPTIM update
      --  event if PREOAD bit is reset. The capture/compare register 4 contains
      --  the value to be compared to the counter LPTIM2_CNT and signaled on
      --  OC4 output. If channel CC4 is configured as input: CCR4 becomes
      --  read-only, it contains the counter value transferred by the last
      --  input capture 4 event. The LPTIM2_CCR4 register is read-only and
      --  cannot be programmed.
      CCR4           : LPTIM2_CCR4_CCR4_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM2_CCR4_Register use record
      CCR4           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  LPTIM3 interrupt and status register [alternate]
   type LPTIM3_ISR_OUTPUT_Register is record
      --  Read-only. Compare 1 interrupt flag If channel CC1 is configured as
      --  output: The CC1IF flag is set by hardware to inform application that
      --  LPTIM_CNT register value matches the compare register's value. CC1IF
      --  flag can be cleared by writing 1 to the CC1CF bit in the LPTIM_ICR
      --  register.
      CC1IF          : Boolean;
      --  Read-only. Autoreload match ARRM is set by hardware to inform
      --  application that LPTIM_CNT registers value reached the LPTIM_ARR
      --  registers value. ARRM flag can be cleared by writing 1 to the ARRMCF
      --  bit in the LPTIM_ICR register.
      ARRM           : Boolean;
      --  Read-only. External trigger edge event EXTTRIG is set by hardware to
      --  inform application that a valid edge on the selected external trigger
      --  input has occurred. If the trigger is ignored because the timer has
      --  already started, then this flag is not set. EXTTRIG flag can be
      --  cleared by writing 1 to the EXTTRIGCF bit in the LPTIM_ICR register.
      EXTTRIG        : Boolean;
      --  Read-only. Compare register 1 update OK CMP1OK is set by hardware to
      --  inform application that the APB bus write operation to the LPTIM_CCR1
      --  register has been successfully completed. CMP1OK flag can be cleared
      --  by writing 1 to the CMP1OKCF bit in the LPTIM_ICR register.
      CMP1OK         : Boolean;
      --  Read-only. Autoreload register update OK ARROK is set by hardware to
      --  inform application that the APB bus write operation to the LPTIM_ARR
      --  register has been successfully completed. ARROK flag can be cleared
      --  by writing 1 to the ARROKCF bit in the LPTIM_ICR register.
      ARROK          : Boolean;
      --  Read-only. Counter direction change down to up In Encoder mode, UP
      --  bit is set by hardware to inform application that the counter
      --  direction has changed from down to up. UP flag can be cleared by
      --  writing 1 to the UPCF bit in the LPTIM_ICR register. Note: If the
      --  LPTIM does not support encoder mode feature, this bit is reserved.
      --  Refer to Section125.3.
      UP             : Boolean;
      --  Read-only. Counter direction change up to down In Encoder mode, DOWN
      --  bit is set by hardware to inform application that the counter
      --  direction has changed from up to down. DOWN flag can be cleared by
      --  writing 1 to the DOWNCF bit in the LPTIM_ICR register. Note: If the
      --  LPTIM does not support encoder mode feature, this bit is reserved.
      --  Refer to Section125.3.
      DOWN           : Boolean;
      --  Read-only. LPTIM update event occurred UE is set by hardware to
      --  inform application that an update event was generated. The
      --  corresponding interrupt or DMA request is generated if enabled. UE
      --  flag can be cleared by writing 1 to the UECF bit in the LPTIM_ICR
      --  register. The UE flag is automatically cleared by hardware once the
      --  LPTIM_ARR register is written by any bus master like CPU or DMA.
      UE             : Boolean;
      --  Read-only. Repetition register update OK REPOK is set by hardware to
      --  inform application that the APB bus write operation to the LPTIM_RCR
      --  register has been successfully completed. REPOK flag can be cleared
      --  by writing 1 to the REPOKCF bit in the LPTIM_ICR register.
      REPOK          : Boolean;
      --  Read-only. Compare 2 interrupt flag If channel CC2 is configured as
      --  output: The CC2IF flag is set by hardware to inform application that
      --  LPTIM_CNT register value matches the compare register's value. CC2IF
      --  flag can be cleared by writing 1 to the CC2CF bit in the LPTIM_ICR
      --  register. Note: If LPTIM does not implement at least 2 channels this
      --  bit is reserved. Refer to Section125.3.
      CC2IF          : Boolean;
      --  Read-only. Compare 3 interrupt flag If channel CC3 is configured as
      --  output: The CC3IF flag is set by hardware to inform application that
      --  LPTIM_CNT register value matches the compare register's value. CC3IF
      --  flag can be cleared by writing 1 to the CC3CF bit in the LPTIM_ICR
      --  register. Note: If LPTIM does not implement at least 3 channels this
      --  bit is reserved. Refer to Section125.3.
      CC3IF          : Boolean;
      --  Read-only. Compare 4 interrupt flag If channel CC4 is configured as
      --  output: The CC4IF flag is set by hardware to inform application that
      --  LPTIM_CNT register value matches the compare register's value. CC4IF
      --  flag can be cleared by writing 1 to the CC4CF bit in the LPTIM_ICR
      --  register. Note: If LPTIM does not implement at least 4 channels this
      --  bit is reserved. Refer to Section125.3.
      CC4IF          : Boolean;
      --  unspecified
      Reserved_12_18 : HAL.UInt7;
      --  Read-only. Compare register 2 update OK CMP2OK is set by hardware to
      --  inform application that the APB bus write operation to the LPTIM_CCR2
      --  register has been successfully completed. CMP2OK flag can be cleared
      --  by writing 1 to the CMP2OKCF bit in the LPTIM_ICR register. Note: If
      --  LPTIM does not implement at least 2 channels this bit is reserved.
      --  Refer to Section125.3.
      CMP2OK         : Boolean;
      --  Read-only. Compare register 3 update OK CMP3OK is set by hardware to
      --  inform application that the APB bus write operation to the LPTIM_CCR3
      --  register has been successfully completed. CMP3OK flag can be cleared
      --  by writing 1 to the CMP3OKCF bit in the LPTIM_ICR register. Note: If
      --  LPTIM does not implement at least 3 channels this bit is reserved.
      --  Refer to Section125.3.
      CMP3OK         : Boolean;
      --  Read-only. Compare register 4 update OK CMP4OK is set by hardware to
      --  inform application that the APB bus write operation to the LPTIM_CCR4
      --  register has been successfully completed. CMP4OK flag can be cleared
      --  by writing 1 to the CMP4OKCF bit in the LPTIM_ICR register. Note: If
      --  LPTIM does not implement at least 4 channels this bit is reserved.
      --  Refer to Section125.3.
      CMP4OK         : Boolean;
      --  unspecified
      Reserved_22_23 : HAL.UInt2;
      --  Read-only. Interrupt enable register update OK DIEROK is set by
      --  hardware to inform application that the APB bus write operation to
      --  the LPTIM_DIER register has been successfully completed. DIEROK flag
      --  can be cleared by writing 1 to the DIEROKCF bit in the LPTIM_ICR
      --  register.
      DIEROK         : Boolean;
      --  unspecified
      Reserved_25_31 : HAL.UInt7;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM3_ISR_OUTPUT_Register use record
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
      CC3IF          at 0 range 10 .. 10;
      CC4IF          at 0 range 11 .. 11;
      Reserved_12_18 at 0 range 12 .. 18;
      CMP2OK         at 0 range 19 .. 19;
      CMP3OK         at 0 range 20 .. 20;
      CMP4OK         at 0 range 21 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      DIEROK         at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  LPTIM3 interrupt and status register [alternate]
   type LPTIM3_ISR_INPUT_Register is record
      --  Read-only. capture 1 interrupt flag If channel CC1 is configured as
      --  input: CC1IF is set by hardware to inform application that the
      --  current value of the counter is captured in LPTIM_CCR1 register. The
      --  corresponding interrupt or DMA request is generated if enabled. The
      --  CC1OF flag is set if the CC1IF flag was already high.
      CC1IF          : Boolean;
      --  Read-only. Autoreload match ARRM is set by hardware to inform
      --  application that LPTIM_CNT registers value reached the LPTIM_ARR
      --  registers value. ARRM flag can be cleared by writing 1 to the ARRMCF
      --  bit in the LPTIM_ICR register.
      ARRM           : Boolean;
      --  Read-only. External trigger edge event EXTTRIG is set by hardware to
      --  inform application that a valid edge on the selected external trigger
      --  input has occurred. If the trigger is ignored because the timer has
      --  already started, then this flag is not set. EXTTRIG flag can be
      --  cleared by writing 1 to the EXTTRIGCF bit in the LPTIM_ICR register.
      EXTTRIG        : Boolean;
      --  unspecified
      Reserved_3_3   : HAL.Bit;
      --  Read-only. Autoreload register update OK ARROK is set by hardware to
      --  inform application that the APB bus write operation to the LPTIM_ARR
      --  register has been successfully completed. ARROK flag can be cleared
      --  by writing 1 to the ARROKCF bit in the LPTIM_ICR register.
      ARROK          : Boolean;
      --  Read-only. Counter direction change down to up In Encoder mode, UP
      --  bit is set by hardware to inform application that the counter
      --  direction has changed from down to up. UP flag can be cleared by
      --  writing 1 to the UPCF bit in the LPTIM_ICR register. Note: If the
      --  LPTIM does not support encoder mode feature, this bit is reserved.
      --  Refer to Section125.3.
      UP             : Boolean;
      --  Read-only. Counter direction change up to down In Encoder mode, DOWN
      --  bit is set by hardware to inform application that the counter
      --  direction has changed from up to down. DOWN flag can be cleared by
      --  writing 1 to the DOWNCF bit in the LPTIM_ICR register. Note: If the
      --  LPTIM does not support encoder mode feature, this bit is reserved.
      --  Refer to Section125.3.
      DOWN           : Boolean;
      --  Read-only. LPTIM update event occurred UE is set by hardware to
      --  inform application that an update event was generated. UE flag can be
      --  cleared by writing 1 to the UECF bit in the LPTIM_ICR register.
      UE             : Boolean;
      --  Read-only. Repetition register update OK REPOK is set by hardware to
      --  inform application that the APB bus write operation to the LPTIM_RCR
      --  register has been successfully completed. REPOK flag can be cleared
      --  by writing 1 to the REPOKCF bit in the LPTIM_ICR register.
      REPOK          : Boolean;
      --  Read-only. Capture 2 interrupt flag If channel CC2 is configured as
      --  input: CC2IF is set by hardware to inform application that the
      --  current value of the counter is captured in LPTIM_CCR2 register. The
      --  corresponding interrupt or DMA request is generated if enabled. The
      --  CC2OF flag is set if the CC2IF flag was already high. Note: If LPTIM
      --  does not implement at least 2 channels this bit is reserved. Refer to
      --  Section125.3.
      CC2IF          : Boolean;
      --  Read-only. Capture 3 interrupt flag If channel CC3 is configured as
      --  input: CC3IF is set by hardware to inform application that the
      --  current value of the counter is captured in LPTIM_CCR3 register. The
      --  corresponding interrupt or DMA request is generated if enabled. The
      --  CC3OF flag is set if the CC3IF flag was already high. Note: If LPTIM
      --  does not implement at least 3 channels this bit is reserved. Refer to
      --  Section125.3.
      CC3IF          : Boolean;
      --  Read-only. Capture 4 interrupt flag If channel CC4 is configured as
      --  input: CC4IF is set by hardware to inform application that the
      --  current value of the counter is captured in LPTIM_CCR4 register. The
      --  corresponding interrupt or DMA request is generated if enabled. The
      --  CC4OF flag is set if the CC4IF flag was already high. Note: If LPTIM
      --  does not implement at least 4 channels this bit is reserved. Refer to
      --  Section125.3.
      CC4IF          : Boolean;
      --  Read-only. Capture 1 over-capture flag This flag is set by hardware
      --  only when the corresponding channel is configured in input capture
      --  mode. It is cleared by software by writing 1 to the CC1OCF bit in the
      --  LPTIM_ICR register. Note: If LPTIM does not implement at least 1
      --  channel this bit is reserved. Refer to Section125.3.
      CC1OF          : Boolean;
      --  Read-only. Capture 2 over-capture flag This flag is set by hardware
      --  only when the corresponding channel is configured in input capture
      --  mode. It is cleared by software by writing 1 to the CC2OCF bit in the
      --  LPTIM_ICR register. Note: If LPTIM does not implement at least 2
      --  channels this bit is reserved. Refer to Section125.3.
      CC2OF          : Boolean;
      --  Read-only. Capture 3 over-capture flag This flag is set by hardware
      --  only when the corresponding channel is configured in input capture
      --  mode. It is cleared by software by writing 1 to the CC3OCF bit in the
      --  LPTIM_ICR register. Note: If LPTIM does not implement at least 3
      --  channels this bit is reserved. Refer to Section125.3.
      CC3OF          : Boolean;
      --  Read-only. Capture 4 over-capture flag This flag is set by hardware
      --  only when the corresponding channel is configured in input capture
      --  mode. It is cleared by software by writing 1 to the CC4OCF bit in the
      --  LPTIM_ICR register. Note: If LPTIM does not implement at least 4
      --  channels this bit is reserved. Refer to Section125.3.
      CC4OF          : Boolean;
      --  unspecified
      Reserved_16_23 : HAL.UInt8;
      --  Read-only. Interrupt enable register update OK DIEROK is set by
      --  hardware to inform application that the APB bus write operation to
      --  the LPTIM_DIER register has been successfully completed. DIEROK flag
      --  can be cleared by writing 1 to the DIEROKCF bit in the LPTIM_ICR
      --  register.
      DIEROK         : Boolean;
      --  unspecified
      Reserved_25_31 : HAL.UInt7;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM3_ISR_INPUT_Register use record
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
      CC3IF          at 0 range 10 .. 10;
      CC4IF          at 0 range 11 .. 11;
      CC1OF          at 0 range 12 .. 12;
      CC2OF          at 0 range 13 .. 13;
      CC3OF          at 0 range 14 .. 14;
      CC4OF          at 0 range 15 .. 15;
      Reserved_16_23 at 0 range 16 .. 23;
      DIEROK         at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  LPTIM3 interrupt clear register [alternate]
   type LPTIM3_ICR_OUTPUT_Register is record
      --  Write-only. Capture/compare 1 clear flag Writing 1 to this bit clears
      --  the CC1IF flag in the LPTIM_ISR register.
      CC1CF          : Boolean := False;
      --  Write-only. Autoreload match clear flag Writing 1 to this bit clears
      --  the ARRM flag in the LPTIM_ISR register
      ARRMCF         : Boolean := False;
      --  Write-only. External trigger valid edge clear flag Writing 1 to this
      --  bit clears the EXTTRIG flag in the LPTIM_ISR register
      EXTTRIGCF      : Boolean := False;
      --  Write-only. Compare register 1 update OK clear flag Writing 1 to this
      --  bit clears the CMP1OK flag in the LPTIM_ISR register.
      CMP1OKCF       : Boolean := False;
      --  Write-only. Autoreload register update OK clear flag Writing 1 to
      --  this bit clears the ARROK flag in the LPTIM_ISR register
      ARROKCF        : Boolean := False;
      --  Write-only. Direction change to UP clear flag Writing 1 to this bit
      --  clear the UP flag in the LPTIM_ISR register. Note: If the LPTIM does
      --  not support encoder mode feature, this bit is reserved. Refer to
      --  Section125.3.
      UPCF           : Boolean := False;
      --  Write-only. Direction change to down clear flag Writing 1 to this bit
      --  clear the DOWN flag in the LPTIM_ISR register. Note: If the LPTIM
      --  does not support encoder mode feature, this bit is reserved. Refer to
      --  Section125.3.
      DOWNCF         : Boolean := False;
      --  Write-only. Update event clear flag Writing 1 to this bit clear the
      --  UE flag in the LPTIM_ISR register.
      UECF           : Boolean := False;
      --  Write-only. Repetition register update OK clear flag Writing 1 to
      --  this bit clears the REPOK flag in the LPTIM_ISR register.
      REPOKCF        : Boolean := False;
      --  Write-only. Capture/compare 2 clear flag Writing 1 to this bit clears
      --  the CC2IF flag in the LPTIM_ISR register. Note: If LPTIM does not
      --  implement at least 2 channels this bit is reserved. Refer to
      --  Section125.3.
      CC2CF          : Boolean := False;
      --  Write-only. Capture/compare 3 clear flag Writing 1 to this bit clears
      --  the CC3IF flag in the LPTIM_ISR register. Note: If LPTIM does not
      --  implement at least 3 channels this bit is reserved. Refer to
      --  Section125.3.
      CC3CF          : Boolean := False;
      --  Write-only. Capture/compare 4 clear flag Writing 1 to this bit clears
      --  the CC4IF flag in the LPTIM_ISR register. Note: If LPTIM does not
      --  implement at least 4 channels this bit is reserved. Refer to
      --  Section125.3.
      CC4CF          : Boolean := False;
      --  unspecified
      Reserved_12_18 : HAL.UInt7 := 16#0#;
      --  Write-only. Compare register 2 update OK clear flag Writing 1 to this
      --  bit clears the CMP2OK flag in the LPTIM_ISR register. Note: If LPTIM
      --  does not implement at least 2 channels this bit is reserved. Refer to
      --  Section125.3.
      CMP2OKCF       : Boolean := False;
      --  Write-only. Compare register 3 update OK clear flag Writing 1 to this
      --  bit clears the CMP3OK flag in the LPTIM_ISR register. Note: If LPTIM
      --  does not implement at least 3 channels this bit is reserved. Refer to
      --  Section125.3.
      CMP3OKCF       : Boolean := False;
      --  Write-only. Compare register 4 update OK clear flag Writing 1 to this
      --  bit clears the CMP4OK flag in the LPTIM_ISR register. Note: If LPTIM
      --  does not implement at least 4 channels this bit is reserved. Refer to
      --  Section125.3.
      CMP4OKCF       : Boolean := False;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  Write-only. Interrupt enable register update OK clear flag Writing 1
      --  to this bit clears the DIEROK flag in the LPTIM_ISR register.
      DIEROKCF       : Boolean := False;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM3_ICR_OUTPUT_Register use record
      CC1CF          at 0 range 0 .. 0;
      ARRMCF         at 0 range 1 .. 1;
      EXTTRIGCF      at 0 range 2 .. 2;
      CMP1OKCF       at 0 range 3 .. 3;
      ARROKCF        at 0 range 4 .. 4;
      UPCF           at 0 range 5 .. 5;
      DOWNCF         at 0 range 6 .. 6;
      UECF           at 0 range 7 .. 7;
      REPOKCF        at 0 range 8 .. 8;
      CC2CF          at 0 range 9 .. 9;
      CC3CF          at 0 range 10 .. 10;
      CC4CF          at 0 range 11 .. 11;
      Reserved_12_18 at 0 range 12 .. 18;
      CMP2OKCF       at 0 range 19 .. 19;
      CMP3OKCF       at 0 range 20 .. 20;
      CMP4OKCF       at 0 range 21 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      DIEROKCF       at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  LPTIM3 interrupt clear register [alternate]
   type LPTIM3_ICR_INPUT_Register is record
      --  Write-only. Capture/compare 1 clear flag Writing 1 to this bit clears
      --  the CC1IF flag in the LPTIM_ISR register.
      CC1CF          : Boolean := False;
      --  Write-only. Autoreload match clear flag Writing 1 to this bit clears
      --  the ARRM flag in the LPTIM_ISR register
      ARRMCF         : Boolean := False;
      --  Write-only. External trigger valid edge clear flag Writing 1 to this
      --  bit clears the EXTTRIG flag in the LPTIM_ISR register
      EXTTRIGCF      : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Write-only. Autoreload register update OK clear flag Writing 1 to
      --  this bit clears the ARROK flag in the LPTIM_ISR register
      ARROKCF        : Boolean := False;
      --  Write-only. Direction change to UP clear flag Writing 1 to this bit
      --  clear the UP flag in the LPTIM_ISR register. Note: If the LPTIM does
      --  not support encoder mode feature, this bit is reserved. Refer to
      --  Section125.3.
      UPCF           : Boolean := False;
      --  Write-only. Direction change to down clear flag Writing 1 to this bit
      --  clear the DOWN flag in the LPTIM_ISR register. Note: If the LPTIM
      --  does not support encoder mode feature, this bit is reserved. Refer to
      --  Section125.3.
      DOWNCF         : Boolean := False;
      --  Write-only. Update event clear flag Writing 1 to this bit clear the
      --  UE flag in the LPTIM_ISR register.
      UECF           : Boolean := False;
      --  Write-only. Repetition register update OK clear flag Writing 1 to
      --  this bit clears the REPOK flag in the LPTIM_ISR register.
      REPOKCF        : Boolean := False;
      --  Write-only. Capture/compare 2 clear flag Writing 1 to this bit clears
      --  the CC2IF flag in the LPTIM_ISR register. Note: If LPTIM does not
      --  implement at least 2 channels this bit is reserved. Refer to
      --  Section125.3.
      CC2CF          : Boolean := False;
      --  Write-only. Capture/compare 3 clear flag Writing 1 to this bit clears
      --  the CC3IF flag in the LPTIM_ISR register. Note: If LPTIM does not
      --  implement at least 3 channels this bit is reserved. Refer to
      --  Section125.3.
      CC3CF          : Boolean := False;
      --  Write-only. Capture/compare 4 clear flag Writing 1 to this bit clears
      --  the CC4IF flag in the LPTIM_ISR register. Note: If LPTIM does not
      --  implement at least 4 channels this bit is reserved. Refer to
      --  Section125.3.
      CC4CF          : Boolean := False;
      --  Write-only. Capture/compare 1 over-capture clear flag Writing 1 to
      --  this bit clears the CC1OF flag in the LPTIM_ISR register. Note: If
      --  LPTIM does not implement at least 1 channel this bit is reserved.
      --  Refer to Section125.3.
      CC1OCF         : Boolean := False;
      --  Write-only. Capture/compare 2 over-capture clear flag Writing 1 to
      --  this bit clears the CC2OF flag in the LPTIM_ISR register. Note: If
      --  LPTIM does not implement at least 2 channels this bit is reserved.
      --  Refer to Section125.3.
      CC2OCF         : Boolean := False;
      --  Write-only. Capture/compare 3 over-capture clear flag Writing 1 to
      --  this bit clears the CC3OF flag in the LPTIM_ISR register. Note: If
      --  LPTIM does not implement at least 3 channels this bit is reserved.
      --  Refer to Section125.3.
      CC3OCF         : Boolean := False;
      --  Write-only. Capture/compare 4 over-capture clear flag Writing 1 to
      --  this bit clears the CC4OF flag in the LPTIM_ISR register. Note: If
      --  LPTIM does not implement at least 4 channels this bit is reserved.
      --  Refer to Section125.3.
      CC4OCF         : Boolean := False;
      --  unspecified
      Reserved_16_23 : HAL.UInt8 := 16#0#;
      --  Write-only. Interrupt enable register update OK clear flag Writing 1
      --  to this bit clears the DIEROK flag in the LPTIM_ISR register.
      DIEROKCF       : Boolean := False;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM3_ICR_INPUT_Register use record
      CC1CF          at 0 range 0 .. 0;
      ARRMCF         at 0 range 1 .. 1;
      EXTTRIGCF      at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      ARROKCF        at 0 range 4 .. 4;
      UPCF           at 0 range 5 .. 5;
      DOWNCF         at 0 range 6 .. 6;
      UECF           at 0 range 7 .. 7;
      REPOKCF        at 0 range 8 .. 8;
      CC2CF          at 0 range 9 .. 9;
      CC3CF          at 0 range 10 .. 10;
      CC4CF          at 0 range 11 .. 11;
      CC1OCF         at 0 range 12 .. 12;
      CC2OCF         at 0 range 13 .. 13;
      CC3OCF         at 0 range 14 .. 14;
      CC4OCF         at 0 range 15 .. 15;
      Reserved_16_23 at 0 range 16 .. 23;
      DIEROKCF       at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  LPTIM3 interrupt enable register [alternate]
   type LPTIM3_DIER_OUTPUT_Register is record
      --  Capture/compare 1 interrupt enable
      CC1IE          : Boolean := False;
      --  Autoreload match Interrupt Enable
      ARRMIE         : Boolean := False;
      --  External trigger valid edge Interrupt Enable
      EXTTRIGIE      : Boolean := False;
      --  Compare register 1 update OK interrupt enable
      CMP1OKIE       : Boolean := False;
      --  Autoreload register update OK Interrupt Enable
      ARROKIE        : Boolean := False;
      --  Direction change to UP Interrupt Enable Note: If the LPTIM does not
      --  support encoder mode feature, this bit is reserved. Refer to
      --  Section125.3.
      UPIE           : Boolean := False;
      --  Direction change to down Interrupt Enable Note: If the LPTIM does not
      --  support encoder mode feature, this bit is reserved. Refer to
      --  Section125.3.
      DOWNIE         : Boolean := False;
      --  Update event interrupt enable
      UEIE           : Boolean := False;
      --  Repetition register update OK interrupt Enable
      REPOKIE        : Boolean := False;
      --  Capture/compare 2 interrupt enable Note: If LPTIM does not implement
      --  at least 2 channels this bit is reserved. Refer to Section125.3.
      CC2IE          : Boolean := False;
      --  Capture/compare 3 interrupt enable Note: If LPTIM does not implement
      --  at least 3 channels this bit is reserved. Refer to Section125.3.
      CC3IE          : Boolean := False;
      --  Capture/compare 4 interrupt enable Note: If LPTIM does not implement
      --  at least 4 channels this bit is reserved. Refer to Section125.3.
      CC4IE          : Boolean := False;
      --  unspecified
      Reserved_12_18 : HAL.UInt7 := 16#0#;
      --  Compare register 2 update OK interrupt enable Note: If LPTIM does not
      --  implement at least 2 channels this bit is reserved. Refer to
      --  Section125.3.
      CMP2OKIE       : Boolean := False;
      --  Compare register 3 update OK interrupt enable Note: If LPTIM does not
      --  implement at least 3 channels this bit is reserved. Refer to
      --  Section125.3.
      CMP3OKIE       : Boolean := False;
      --  Compare register 4 update OK interrupt enable Note: If LPTIM does not
      --  implement at least 4 channels this bit is reserved. Refer to
      --  Section125.3.
      CMP4OKIE       : Boolean := False;
      --  unspecified
      Reserved_22_22 : HAL.Bit := 16#0#;
      --  Update event DMA request enable Note: If LPTIM does not implement at
      --  least 1 channel this bit is reserved. Refer to Section125.3.
      UEDE           : Boolean := False;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM3_DIER_OUTPUT_Register use record
      CC1IE          at 0 range 0 .. 0;
      ARRMIE         at 0 range 1 .. 1;
      EXTTRIGIE      at 0 range 2 .. 2;
      CMP1OKIE       at 0 range 3 .. 3;
      ARROKIE        at 0 range 4 .. 4;
      UPIE           at 0 range 5 .. 5;
      DOWNIE         at 0 range 6 .. 6;
      UEIE           at 0 range 7 .. 7;
      REPOKIE        at 0 range 8 .. 8;
      CC2IE          at 0 range 9 .. 9;
      CC3IE          at 0 range 10 .. 10;
      CC4IE          at 0 range 11 .. 11;
      Reserved_12_18 at 0 range 12 .. 18;
      CMP2OKIE       at 0 range 19 .. 19;
      CMP3OKIE       at 0 range 20 .. 20;
      CMP4OKIE       at 0 range 21 .. 21;
      Reserved_22_22 at 0 range 22 .. 22;
      UEDE           at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  LPTIM3 interrupt enable register [alternate]
   type LPTIM3_DIER_INPUT_Register is record
      --  Capture/compare 1 interrupt enable
      CC1IE          : Boolean := False;
      --  Autoreload match Interrupt Enable
      ARRMIE         : Boolean := False;
      --  External trigger valid edge Interrupt Enable
      EXTTRIGIE      : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Autoreload register update OK Interrupt Enable
      ARROKIE        : Boolean := False;
      --  Direction change to UP Interrupt Enable Note: If the LPTIM does not
      --  support encoder mode feature, this bit is reserved. Refer to
      --  Section125.3.
      UPIE           : Boolean := False;
      --  Direction change to down Interrupt Enable Note: If the LPTIM does not
      --  support encoder mode feature, this bit is reserved. Refer to
      --  Section125.3.
      DOWNIE         : Boolean := False;
      --  Update event interrupt enable
      UEIE           : Boolean := False;
      --  Repetition register update OK interrupt Enable
      REPOKIE        : Boolean := False;
      --  Capture/compare 2 interrupt enable Note: If LPTIM does not implement
      --  at least 2 channels this bit is reserved. Refer to Section125.3.
      CC2IE          : Boolean := False;
      --  Capture/compare 3 interrupt enable Note: If LPTIM does not implement
      --  at least 3 channels this bit is reserved. Refer to Section125.3.
      CC3IE          : Boolean := False;
      --  Capture/compare 4 interrupt enable Note: If LPTIM does not implement
      --  at least 4 channels this bit is reserved. Refer to Section125.3.
      CC4IE          : Boolean := False;
      --  Capture/compare 1 over-capture interrupt enable Note: If LPTIM does
      --  not implement at least 1 channel this bit is reserved. Refer to
      --  Section125.3.
      CC1OIE         : Boolean := False;
      --  Capture/compare 2 over-capture interrupt enable Note: If LPTIM does
      --  not implement at least 2 channels this bit is reserved. Refer to
      --  Section125.3.
      CC2OIE         : Boolean := False;
      --  Capture/compare 3 over-capture interrupt enable Note: If LPTIM does
      --  not implement at least 3 channels this bit is reserved. Refer to
      --  Section125.3.
      CC3OIE         : Boolean := False;
      --  Capture/compare 4 over-capture interrupt enable Note: If LPTIM does
      --  not implement at least 4 channels this bit is reserved. Refer to
      --  Section125.3.
      CC4OIE         : Boolean := False;
      --  Capture/compare 1 DMA request enable Note: If LPTIM does not
      --  implement at least 1 channel this bit is reserved. Refer to
      --  Section125.3.
      CC1DE          : Boolean := False;
      --  unspecified
      Reserved_17_22 : HAL.UInt6 := 16#0#;
      --  Update event DMA request enable Note: If LPTIM does not implement at
      --  least 1 channel this bit is reserved. Refer to Section125.3.
      UEDE           : Boolean := False;
      --  unspecified
      Reserved_24_24 : HAL.Bit := 16#0#;
      --  Capture/compare 2 DMA request enable Note: If LPTIM does not
      --  implement at least 2 channels this bit is reserved. Refer to
      --  Section125.3.
      CC2DE          : Boolean := False;
      --  Capture/compare 3 DMA request enable Note: If LPTIM does not
      --  implement at least 3 channels this bit is reserved. Refer to
      --  Section125.3.
      CC3DE          : Boolean := False;
      --  Capture/compare 4 DMA request enable Note: If LPTIM does not
      --  implement at least 4 channels this bit is reserved. Refer to
      --  Section125.3.
      CC4DE          : Boolean := False;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM3_DIER_INPUT_Register use record
      CC1IE          at 0 range 0 .. 0;
      ARRMIE         at 0 range 1 .. 1;
      EXTTRIGIE      at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      ARROKIE        at 0 range 4 .. 4;
      UPIE           at 0 range 5 .. 5;
      DOWNIE         at 0 range 6 .. 6;
      UEIE           at 0 range 7 .. 7;
      REPOKIE        at 0 range 8 .. 8;
      CC2IE          at 0 range 9 .. 9;
      CC3IE          at 0 range 10 .. 10;
      CC4IE          at 0 range 11 .. 11;
      CC1OIE         at 0 range 12 .. 12;
      CC2OIE         at 0 range 13 .. 13;
      CC3OIE         at 0 range 14 .. 14;
      CC4OIE         at 0 range 15 .. 15;
      CC1DE          at 0 range 16 .. 16;
      Reserved_17_22 at 0 range 17 .. 22;
      UEDE           at 0 range 23 .. 23;
      Reserved_24_24 at 0 range 24 .. 24;
      CC2DE          at 0 range 25 .. 25;
      CC3DE          at 0 range 26 .. 26;
      CC4DE          at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype LPTIM3_CFGR_CKPOL_Field is HAL.UInt2;
   subtype LPTIM3_CFGR_CKFLT_Field is HAL.UInt2;
   subtype LPTIM3_CFGR_TRGFLT_Field is HAL.UInt2;
   subtype LPTIM3_CFGR_PRESC_Field is HAL.UInt3;
   subtype LPTIM3_CFGR_TRIGSEL_Field is HAL.UInt3;
   subtype LPTIM3_CFGR_TRIGEN_Field is HAL.UInt2;

   --  LPTIM configuration register
   type LPTIM3_CFGR_Register is record
      --  Clock selector The CKSEL bit selects which clock source the LPTIM
      --  uses:
      CKSEL          : Boolean := False;
      --  Clock Polarity When the LPTIM is clocked by an external clock source,
      --  CKPOL bits is used to configure the active edge or edges used by the
      --  counter: If the LPTIM is configured in Encoder mode (ENC bit is set),
      --  the encoder sub-mode 1 is active. If the LPTIM is configured in
      --  Encoder mode (ENC bit is set), the encoder sub-mode 2 is active. If
      --  the LPTIM is configured in Encoder mode (ENC bit is set), the encoder
      --  sub-mode 3 is active. Refer to Section125.4.15: Encoder mode for more
      --  details about Encoder mode sub-modes.
      CKPOL          : LPTIM3_CFGR_CKPOL_Field := 16#0#;
      --  Configurable digital filter for external clock The CKFLT value sets
      --  the number of consecutive equal samples that are detected when a
      --  level change occurs on an external clock signal before it is
      --  considered as a valid level transition. An internal clock source must
      --  be present to use this feature
      CKFLT          : LPTIM3_CFGR_CKFLT_Field := 16#0#;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  Configurable digital filter for trigger The TRGFLT value sets the
      --  number of consecutive equal samples that are detected when a level
      --  change occurs on an internal trigger before it is considered as a
      --  valid level transition. An internal clock source must be present to
      --  use this feature
      TRGFLT         : LPTIM3_CFGR_TRGFLT_Field := 16#0#;
      --  unspecified
      Reserved_8_8   : HAL.Bit := 16#0#;
      --  Clock prescaler The PRESC bits configure the prescaler division
      --  factor. It can be one among the following division factors:
      PRESC          : LPTIM3_CFGR_PRESC_Field := 16#0#;
      --  unspecified
      Reserved_12_12 : HAL.Bit := 16#0#;
      --  Trigger selector The TRIGSEL bits select the trigger source that
      --  serves as a trigger event for the LPTIM among the below 8 available
      --  sources: See Section125.4.3: LPTIM input and trigger mapping for
      --  details.
      TRIGSEL        : LPTIM3_CFGR_TRIGSEL_Field := 16#0#;
      --  unspecified
      Reserved_16_16 : HAL.Bit := 16#0#;
      --  Trigger enable and polarity The TRIGEN bits controls whether the
      --  LPTIM counter is started by an external trigger or not. If the
      --  external trigger option is selected, three configurations are
      --  possible for the trigger active edge:
      TRIGEN         : LPTIM3_CFGR_TRIGEN_Field := 16#0#;
      --  Timeout enable The TIMOUT bit controls the Timeout feature
      TIMOUT         : Boolean := False;
      --  Waveform shape The WAVE bit controls the output shape
      WAVE           : Boolean := False;
      --  unspecified
      Reserved_21_21 : HAL.Bit := 16#0#;
      --  Registers update mode The PRELOAD bit controls the LPTIM3_ARR,
      --  LPTIM3_RCR and the LPTIM3_CCRx registers update modality
      PRELOAD        : Boolean := False;
      --  counter mode enabled The COUNTMODE bit selects which clock source is
      --  used by the LPTIM to clock the counter:
      COUNTMODE      : Boolean := False;
      --  Encoder mode enable The ENC bit controls the Encoder mode Note: If
      --  the LPTIM does not support encoder mode feature, this bit is
      --  reserved. Refer to Section125.3.
      ENC            : Boolean := False;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM3_CFGR_Register use record
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
      Reserved_21_21 at 0 range 21 .. 21;
      PRELOAD        at 0 range 22 .. 22;
      COUNTMODE      at 0 range 23 .. 23;
      ENC            at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  LPTIM control register
   type LPTIM3_CR_Register is record
      --  LPTIM enable The ENABLE bit is set and cleared by software.
      ENABLE        : Boolean := False;
      --  LPTIM start in Single mode This bit is set by software and cleared by
      --  hardware. In case of software start (TRIGEN[1:0] = 00), setting this
      --  bit starts the LPTIM in single pulse mode. If the software start is
      --  disabled (TRIGEN[1:0] different than 00), setting this bit starts the
      --  LPTIM in single pulse mode as soon as an external trigger is
      --  detected. If this bit is set when the LPTIM is in continuous counting
      --  mode, then the LPTIM stops at the following match between LPTIM3_ARR
      --  and LPTIM3_CNT registers. This bit can only be set when the LPTIM is
      --  enabled. It is automatically reset by hardware.
      SNGSTRT       : Boolean := False;
      --  Timer start in Continuous mode This bit is set by software and
      --  cleared by hardware. In case of software start (TRIGEN[1:0] = 00),
      --  setting this bit starts the LPTIM in Continuous mode. If the software
      --  start is disabled (TRIGEN[1:0] different than 00), setting this bit
      --  starts the timer in Continuous mode as soon as an external trigger is
      --  detected. If this bit is set when a single pulse mode counting is
      --  ongoing, then the timer does not stop at the next match between the
      --  LPTIM3_ARR and LPTIM3_CNT registers and the LPTIM counter keeps
      --  counting in Continuous mode. This bit can be set only when the LPTIM
      --  is enabled. It is automatically reset by hardware.
      CNTSTRT       : Boolean := False;
      --  Counter reset This bit is set by software and cleared by hardware.
      --  When set to '1' this bit triggers a synchronous reset of the
      --  LPTIM3_CNT counter register. Due to the synchronous nature of this
      --  reset, it only takes place after a synchronization delay of 3 LPTimer
      --  core clock cycles (LPTimer core clock may be different from APB
      --  clock). This bit can be set only when the LPTIM is enabled. It is
      --  automatically reset by hardware. COUNTRST must never be set to '1' by
      --  software before it is already cleared to '0' by hardware. Software
      --  must consequently check that COUNTRST bit is already cleared to '0'
      --  before attempting to set it to '1'.
      COUNTRST      : Boolean := False;
      --  Reset after read enable This bit is set and cleared by software. When
      --  RSTARE is set to '1', any read access to LPTIM3_CNT register
      --  asynchronously resets LPTIM3_CNT register content. This bit can be
      --  set only when the LPTIM is enabled.
      RSTARE        : Boolean := False;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM3_CR_Register use record
      ENABLE        at 0 range 0 .. 0;
      SNGSTRT       at 0 range 1 .. 1;
      CNTSTRT       at 0 range 2 .. 2;
      COUNTRST      at 0 range 3 .. 3;
      RSTARE        at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype LPTIM3_CCR1_CCR1_Field is HAL.UInt16;

   --  LPTIM compare register 1
   type LPTIM3_CCR1_Register is record
      --  Capture/compare 1 value If channel CC1 is configured as output: CCR1
      --  is the value to be loaded in the capture/compare 1 register.
      --  Depending on the PRELOAD option, the CCR1 register is immediately
      --  updated if the PRELOAD bit is reset and updated at next LPTIM update
      --  event if PREOAD bit is reset. The capture/compare register 1 contains
      --  the value to be compared to the counter LPTIM3_CNT and signaled on
      --  OC1 output. If channel CC1 is configured as input: CCR1 becomes
      --  read-only, it contains the counter value transferred by the last
      --  input capture 1 event. The LPTIM3_CCR1 register is read-only and
      --  cannot be programmed.
      CCR1           : LPTIM3_CCR1_CCR1_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM3_CCR1_Register use record
      CCR1           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype LPTIM3_ARR_ARR_Field is HAL.UInt16;

   --  LPTIM autoreload register
   type LPTIM3_ARR_Register is record
      --  Auto reload value ARR is the autoreload value for the LPTIM. This
      --  value must be strictly greater than the CCRx[15:0] value.
      ARR            : LPTIM3_ARR_ARR_Field := 16#1#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM3_ARR_Register use record
      ARR            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype LPTIM3_CNT_CNT_Field is HAL.UInt16;

   --  LPTIM counter register
   type LPTIM3_CNT_Register is record
      --  Read-only. Counter value When the LPTIM is running with an
      --  asynchronous clock, reading the LPTIM3_CNT register may return
      --  unreliable values. So in this case it is necessary to perform two
      --  consecutive read accesses and verify that the two returned values are
      --  identical.
      CNT            : LPTIM3_CNT_CNT_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM3_CNT_Register use record
      CNT            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype LPTIM3_CFGR2_IN1SEL_Field is HAL.UInt2;
   subtype LPTIM3_CFGR2_IN2SEL_Field is HAL.UInt2;
   subtype LPTIM3_CFGR2_IC1SEL_Field is HAL.UInt2;
   subtype LPTIM3_CFGR2_IC2SEL_Field is HAL.UInt2;

   --  LPTIM configuration register 2
   type LPTIM3_CFGR2_Register is record
      --  LPTIM input 1 selection The IN1SEL bits control the LPTIM input 1
      --  multiplexer, which connects LPTIM input 1 to one of the available
      --  inputs. For connection details refer to Section125.4.3: LPTIM input
      --  and trigger mapping.
      IN1SEL         : LPTIM3_CFGR2_IN1SEL_Field := 16#0#;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  LPTIM input 2 selection The IN2SEL bits control the LPTIM input 2
      --  multiplexer, which connects LPTIM input 2 to one of the available
      --  inputs. For connection details refer to Section125.4.3: LPTIM input
      --  and trigger mapping.
      IN2SEL         : LPTIM3_CFGR2_IN2SEL_Field := 16#0#;
      --  unspecified
      Reserved_6_15  : HAL.UInt10 := 16#0#;
      --  LPTIM input capture 1 selection The IC1SEL bits control the LPTIM
      --  Input capture 1 multiplexer, which connects LPTIM Input capture 1 to
      --  one of the available inputs. For connection details refer to
      --  Section125.4.3: LPTIM input and trigger mapping.
      IC1SEL         : LPTIM3_CFGR2_IC1SEL_Field := 16#0#;
      --  unspecified
      Reserved_18_19 : HAL.UInt2 := 16#0#;
      --  LPTIM input capture 2 selection The IC2SEL bits control the LPTIM
      --  Input capture 2 multiplexer, which connects LPTIM Input capture 2 to
      --  one of the available inputs. For connection details refer to
      --  Section125.4.3: LPTIM input and trigger mapping.
      IC2SEL         : LPTIM3_CFGR2_IC2SEL_Field := 16#0#;
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM3_CFGR2_Register use record
      IN1SEL         at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      IN2SEL         at 0 range 4 .. 5;
      Reserved_6_15  at 0 range 6 .. 15;
      IC1SEL         at 0 range 16 .. 17;
      Reserved_18_19 at 0 range 18 .. 19;
      IC2SEL         at 0 range 20 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   subtype LPTIM3_RCR_REP_Field is HAL.UInt8;

   --  LPTIM repetition register
   type LPTIM3_RCR_Register is record
      --  Repetition register value REP is the repetition value for the LPTIM.
      REP           : LPTIM3_RCR_REP_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM3_RCR_Register use record
      REP           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype LPTIM3_CCMR1_CC1P_Field is HAL.UInt2;
   subtype LPTIM3_CCMR1_IC1PSC_Field is HAL.UInt2;
   subtype LPTIM3_CCMR1_IC1F_Field is HAL.UInt2;
   subtype LPTIM3_CCMR1_CC2P_Field is HAL.UInt2;
   subtype LPTIM3_CCMR1_IC2PSC_Field is HAL.UInt2;
   subtype LPTIM3_CCMR1_IC2F_Field is HAL.UInt2;

   --  LPTIM capture/compare mode register 1
   type LPTIM3_CCMR1_Register is record
      --  Capture/compare 1 selection This bitfield defines the direction of
      --  the channel input (capture) or output mode.
      CC1SEL         : Boolean := False;
      --  Capture/compare 1 output enable. This bit determines if a capture of
      --  the counter value can actually be done into the input capture/compare
      --  register 1 (LPTIM3_CCR1) or not.
      CC1E           : Boolean := False;
      --  Capture/compare 1 output polarity. Only bit2 is used to set polarity
      --  when output mode is enabled, bit3 is don't care. This field is used
      --  to select the IC1 polarity for capture operations.
      CC1P           : LPTIM3_CCMR1_CC1P_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Input capture 1 prescaler This bitfield defines the ratio of the
      --  prescaler acting on the CC1 input (IC1).
      IC1PSC         : LPTIM3_CCMR1_IC1PSC_Field := 16#0#;
      --  unspecified
      Reserved_10_11 : HAL.UInt2 := 16#0#;
      --  Input capture 1 filter This bitfield defines the number of
      --  consecutive equal samples that are detected when a level change
      --  occurs on an external input capture signal before it is considered as
      --  a valid level transition. An internal clock source must be present to
      --  use this feature.
      IC1F           : LPTIM3_CCMR1_IC1F_Field := 16#0#;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  Capture/compare 2 selection This bitfield defines the direction of
      --  the channel, input (capture) or output mode.
      CC2SEL         : Boolean := False;
      --  Capture/compare 2 output enable. This bit determines if a capture of
      --  the counter value can actually be done into the input capture/compare
      --  register 2 (LPTIM3_CCR2) or not.
      CC2E           : Boolean := False;
      --  Capture/compare 2 output polarity. Only bit2 is used to set polarity
      --  when output mode is enabled, bit3 is don't care. This field is used
      --  to select the IC2 polarity for capture operations.
      CC2P           : LPTIM3_CCMR1_CC2P_Field := 16#0#;
      --  unspecified
      Reserved_20_23 : HAL.UInt4 := 16#0#;
      --  Input capture 2 prescaler This bitfield defines the ratio of the
      --  prescaler acting on the CC2 input (IC2).
      IC2PSC         : LPTIM3_CCMR1_IC2PSC_Field := 16#0#;
      --  unspecified
      Reserved_26_27 : HAL.UInt2 := 16#0#;
      --  Input capture 2 filter This bitfield defines the number of
      --  consecutive equal samples that are detected when a level change
      --  occurs on an external input capture signal before it is considered as
      --  a valid level transition. An internal clock source must be present to
      --  use this feature.
      IC2F           : LPTIM3_CCMR1_IC2F_Field := 16#0#;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM3_CCMR1_Register use record
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

   subtype LPTIM3_CCMR2_CC3P_Field is HAL.UInt2;
   subtype LPTIM3_CCMR2_IC3PSC_Field is HAL.UInt2;
   subtype LPTIM3_CCMR2_IC3F_Field is HAL.UInt2;
   subtype LPTIM3_CCMR2_CC4P_Field is HAL.UInt2;
   subtype LPTIM3_CCMR2_IC4PSC_Field is HAL.UInt2;
   subtype LPTIM3_CCMR2_IC4F_Field is HAL.UInt2;

   --  LPTIM capture/compare mode register 2
   type LPTIM3_CCMR2_Register is record
      --  Capture/compare 3 selection This bitfield defines the direction of
      --  the channel input (capture) or output mode.
      CC3SEL         : Boolean := False;
      --  Capture/compare 3 output enable. Condition: CC3 as output: Condition:
      --  CC3 as input: This bit determines if a capture of the counter value
      --  can actually be done into the input capture/compare register 3
      --  (LPTIM3_CCR3) or not.
      CC3E           : Boolean := False;
      --  Capture/compare 3 output polarity. Condition: CC3 as output: Only
      --  bit2 is used to set polarity when output mode is enabled, bit3 is
      --  don't care. Condition: CC3 as input: This field is used to select the
      --  IC3 polarity for capture operations.
      CC3P           : LPTIM3_CCMR2_CC3P_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Input capture 3 prescaler This bitfield defines the ratio of the
      --  prescaler acting on the CC3 input (IC3).
      IC3PSC         : LPTIM3_CCMR2_IC3PSC_Field := 16#0#;
      --  unspecified
      Reserved_10_11 : HAL.UInt2 := 16#0#;
      --  Input capture 3 filter This bitfield defines the number of
      --  consecutive equal samples that should be detected when a level change
      --  occurs on an external input capture signal before it is considered as
      --  a valid level transition. An internal clock source must be present to
      --  use this feature.
      IC3F           : LPTIM3_CCMR2_IC3F_Field := 16#0#;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  Capture/compare 4 selection This bitfield defines the direction of
      --  the channel, input (capture) or output mode.
      CC4SEL         : Boolean := False;
      --  Capture/compare 4 output enable. Condition: CC4 as output: Condition:
      --  CC4 as input: This bit determines if a capture of the counter value
      --  can actually be done into the input capture/compare register 4
      --  (LPTIM3_CCR4) or not.
      CC4E           : Boolean := False;
      --  Capture/compare 4 output polarity. Condition: CC4 as output: Only
      --  bit2 is used to set polarity when output mode is enabled, bit3 is
      --  don't care. Condition: CC4 as input: This field is used to select the
      --  IC4 polarity for capture operations.
      CC4P           : LPTIM3_CCMR2_CC4P_Field := 16#0#;
      --  unspecified
      Reserved_20_23 : HAL.UInt4 := 16#0#;
      --  Input capture 4 prescaler This bitfield defines the ratio of the
      --  prescaler acting on the CC4 input (IC4).
      IC4PSC         : LPTIM3_CCMR2_IC4PSC_Field := 16#0#;
      --  unspecified
      Reserved_26_27 : HAL.UInt2 := 16#0#;
      --  Input capture 4 filter This bitfield defines the number of
      --  consecutive equal samples that should be detected when a level change
      --  occurs on an external input capture signal before it is considered as
      --  a valid level transition. An internal clock source must be present to
      --  use this feature.
      IC4F           : LPTIM3_CCMR2_IC4F_Field := 16#0#;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM3_CCMR2_Register use record
      CC3SEL         at 0 range 0 .. 0;
      CC3E           at 0 range 1 .. 1;
      CC3P           at 0 range 2 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      IC3PSC         at 0 range 8 .. 9;
      Reserved_10_11 at 0 range 10 .. 11;
      IC3F           at 0 range 12 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      CC4SEL         at 0 range 16 .. 16;
      CC4E           at 0 range 17 .. 17;
      CC4P           at 0 range 18 .. 19;
      Reserved_20_23 at 0 range 20 .. 23;
      IC4PSC         at 0 range 24 .. 25;
      Reserved_26_27 at 0 range 26 .. 27;
      IC4F           at 0 range 28 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype LPTIM3_CCR2_CCR2_Field is HAL.UInt16;

   --  LPTIM compare register 2
   type LPTIM3_CCR2_Register is record
      --  Capture/compare 2 value If channel CC2 is configured as output: CCR2
      --  is the value to be loaded in the capture/compare 2 register.
      --  Depending on the PRELOAD option, the CCR2 register is immediately
      --  updated if the PRELOAD bit is reset and updated at next LPTIM update
      --  event if PREOAD bit is reset. The capture/compare register 2 contains
      --  the value to be compared to the counter LPTIM3_CNT and signaled on
      --  OC2 output. If channel CC2 is configured as input: CCR2 becomes
      --  read-only, it contains the counter value transferred by the last
      --  input capture 2 event. The LPTIM3_CCR2 register is read-only and
      --  cannot be programmed.
      CCR2           : LPTIM3_CCR2_CCR2_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM3_CCR2_Register use record
      CCR2           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype LPTIM3_CCR3_CCR3_Field is HAL.UInt16;

   --  LPTIM compare register 3
   type LPTIM3_CCR3_Register is record
      --  Capture/compare 3 value If channel CC3 is configured as output: CCR3
      --  is the value to be loaded in the capture/compare 3 register.
      --  Depending on the PRELOAD option, the CCR3 register is immediately
      --  updated if the PRELOAD bit is reset and updated at next LPTIM update
      --  event if PREOAD bit is reset. The capture/compare register 3 contains
      --  the value to be compared to the counter LPTIM3_CNT and signaled on
      --  OC3 output. If channel CC3 is configured as input: CCR3 becomes
      --  read-only, it contains the counter value transferred by the last
      --  input capture 3 event. The LPTIM3_CCR3 register is read-only and
      --  cannot be programmed.
      CCR3           : LPTIM3_CCR3_CCR3_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM3_CCR3_Register use record
      CCR3           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype LPTIM3_CCR4_CCR4_Field is HAL.UInt16;

   --  LPTIM compare register 4
   type LPTIM3_CCR4_Register is record
      --  Capture/compare 4 value If channel CC4 is configured as output: CCR4
      --  is the value to be loaded in the capture/compare 4 register.
      --  Depending on the PRELOAD option, the CCR4 register is immediately
      --  updated if the PRELOAD bit is reset and updated at next LPTIM update
      --  event if PREOAD bit is reset. The capture/compare register 4 contains
      --  the value to be compared to the counter LPTIM3_CNT and signaled on
      --  OC4 output. If channel CC4 is configured as input: CCR4 becomes
      --  read-only, it contains the counter value transferred by the last
      --  input capture 4 event. The LPTIM3_CCR4 register is read-only and
      --  cannot be programmed.
      CCR4           : LPTIM3_CCR4_CCR4_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTIM3_CCR4_Register use record
      CCR4           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   type LPTIM1_Disc is
     (Output,
      Input);

   --  LPTIM1 address block description
   type LPTIM1_Peripheral
     (Discriminent : LPTIM1_Disc := Output)
   is record
      --  LPTIM configuration register
      LPTIM1_CFGR        : aliased LPTIM1_CFGR_Register;
      --  LPTIM control register
      LPTIM1_CR          : aliased LPTIM1_CR_Register;
      --  LPTIM compare register 1
      LPTIM1_CCR1        : aliased LPTIM1_CCR1_Register;
      --  LPTIM autoreload register
      LPTIM1_ARR         : aliased LPTIM1_ARR_Register;
      --  LPTIM counter register
      LPTIM1_CNT         : aliased LPTIM1_CNT_Register;
      --  LPTIM configuration register 2
      LPTIM1_CFGR2       : aliased LPTIM1_CFGR2_Register;
      --  LPTIM repetition register
      LPTIM1_RCR         : aliased LPTIM1_RCR_Register;
      --  LPTIM capture/compare mode register 1
      LPTIM1_CCMR1       : aliased LPTIM1_CCMR1_Register;
      --  LPTIM capture/compare mode register 2
      LPTIM1_CCMR2       : aliased LPTIM1_CCMR2_Register;
      --  LPTIM compare register 2
      LPTIM1_CCR2        : aliased LPTIM1_CCR2_Register;
      --  LPTIM compare register 3
      LPTIM1_CCR3        : aliased LPTIM1_CCR3_Register;
      --  LPTIM compare register 4
      LPTIM1_CCR4        : aliased LPTIM1_CCR4_Register;
      case Discriminent is
         when Output =>
            --  LPTIM1 interrupt and status register [alternate]
            LPTIM1_ISR_OUTPUT : aliased LPTIM1_ISR_OUTPUT_Register;
            --  LPTIM1 interrupt clear register [alternate]
            LPTIM1_ICR_OUTPUT : aliased LPTIM1_ICR_OUTPUT_Register;
            --  LPTIM1 interrupt enable register [alternate]
            LPTIM1_DIER_OUTPUT : aliased LPTIM1_DIER_OUTPUT_Register;
         when Input =>
            --  LPTIM1 interrupt and status register [alternate]
            LPTIM1_ISR_INPUT : aliased LPTIM1_ISR_INPUT_Register;
            --  LPTIM1 interrupt clear register [alternate]
            LPTIM1_ICR_INPUT : aliased LPTIM1_ICR_INPUT_Register;
            --  LPTIM1 interrupt enable register [alternate]
            LPTIM1_DIER_INPUT : aliased LPTIM1_DIER_INPUT_Register;
      end case;
   end record
     with Unchecked_Union, Volatile;

   for LPTIM1_Peripheral use record
      LPTIM1_CFGR        at 16#C# range 0 .. 31;
      LPTIM1_CR          at 16#10# range 0 .. 31;
      LPTIM1_CCR1        at 16#14# range 0 .. 31;
      LPTIM1_ARR         at 16#18# range 0 .. 31;
      LPTIM1_CNT         at 16#1C# range 0 .. 31;
      LPTIM1_CFGR2       at 16#24# range 0 .. 31;
      LPTIM1_RCR         at 16#28# range 0 .. 31;
      LPTIM1_CCMR1       at 16#2C# range 0 .. 31;
      LPTIM1_CCMR2       at 16#30# range 0 .. 31;
      LPTIM1_CCR2        at 16#34# range 0 .. 31;
      LPTIM1_CCR3        at 16#38# range 0 .. 31;
      LPTIM1_CCR4        at 16#3C# range 0 .. 31;
      LPTIM1_ISR_OUTPUT  at 16#0# range 0 .. 31;
      LPTIM1_ICR_OUTPUT  at 16#4# range 0 .. 31;
      LPTIM1_DIER_OUTPUT at 16#8# range 0 .. 31;
      LPTIM1_ISR_INPUT   at 16#0# range 0 .. 31;
      LPTIM1_ICR_INPUT   at 16#4# range 0 .. 31;
      LPTIM1_DIER_INPUT  at 16#8# range 0 .. 31;
   end record;

   --  LPTIM1 address block description
   LPTIM1_Periph : aliased LPTIM1_Peripheral
     with Import, Address => LPTIM1_Base;

   type LPTIM2_Disc is
     (Output,
      Input);

   --  LPTIM2 address block description
   type LPTIM2_Peripheral
     (Discriminent : LPTIM2_Disc := Output)
   is record
      --  LPTIM configuration register
      LPTIM2_CFGR        : aliased LPTIM2_CFGR_Register;
      --  LPTIM control register
      LPTIM2_CR          : aliased LPTIM2_CR_Register;
      --  LPTIM compare register 1
      LPTIM2_CCR1        : aliased LPTIM2_CCR1_Register;
      --  LPTIM autoreload register
      LPTIM2_ARR         : aliased LPTIM2_ARR_Register;
      --  LPTIM counter register
      LPTIM2_CNT         : aliased LPTIM2_CNT_Register;
      --  LPTIM configuration register 2
      LPTIM2_CFGR2       : aliased LPTIM2_CFGR2_Register;
      --  LPTIM repetition register
      LPTIM2_RCR         : aliased LPTIM2_RCR_Register;
      --  LPTIM capture/compare mode register 1
      LPTIM2_CCMR1       : aliased LPTIM2_CCMR1_Register;
      --  LPTIM capture/compare mode register 2
      LPTIM2_CCMR2       : aliased LPTIM2_CCMR2_Register;
      --  LPTIM compare register 2
      LPTIM2_CCR2        : aliased LPTIM2_CCR2_Register;
      --  LPTIM compare register 3
      LPTIM2_CCR3        : aliased LPTIM2_CCR3_Register;
      --  LPTIM compare register 4
      LPTIM2_CCR4        : aliased LPTIM2_CCR4_Register;
      case Discriminent is
         when Output =>
            --  LPTIM2 interrupt and status register [alternate]
            LPTIM2_ISR_OUTPUT : aliased LPTIM2_ISR_OUTPUT_Register;
            --  LPTIM2 interrupt clear register [alternate]
            LPTIM2_ICR_OUTPUT : aliased LPTIM2_ICR_OUTPUT_Register;
            --  LPTIM2 interrupt enable register [alternate]
            LPTIM2_DIER_OUTPUT : aliased LPTIM2_DIER_OUTPUT_Register;
         when Input =>
            --  LPTIM2 interrupt and status register [alternate]
            LPTIM2_ISR_INPUT : aliased LPTIM2_ISR_INPUT_Register;
            --  LPTIM2 interrupt clear register [alternate]
            LPTIM2_ICR_INPUT : aliased LPTIM2_ICR_INPUT_Register;
            --  LPTIM2 interrupt enable register [alternate]
            LPTIM2_DIER_INPUT : aliased LPTIM2_DIER_INPUT_Register;
      end case;
   end record
     with Unchecked_Union, Volatile;

   for LPTIM2_Peripheral use record
      LPTIM2_CFGR        at 16#C# range 0 .. 31;
      LPTIM2_CR          at 16#10# range 0 .. 31;
      LPTIM2_CCR1        at 16#14# range 0 .. 31;
      LPTIM2_ARR         at 16#18# range 0 .. 31;
      LPTIM2_CNT         at 16#1C# range 0 .. 31;
      LPTIM2_CFGR2       at 16#24# range 0 .. 31;
      LPTIM2_RCR         at 16#28# range 0 .. 31;
      LPTIM2_CCMR1       at 16#2C# range 0 .. 31;
      LPTIM2_CCMR2       at 16#30# range 0 .. 31;
      LPTIM2_CCR2        at 16#34# range 0 .. 31;
      LPTIM2_CCR3        at 16#38# range 0 .. 31;
      LPTIM2_CCR4        at 16#3C# range 0 .. 31;
      LPTIM2_ISR_OUTPUT  at 16#0# range 0 .. 31;
      LPTIM2_ICR_OUTPUT  at 16#4# range 0 .. 31;
      LPTIM2_DIER_OUTPUT at 16#8# range 0 .. 31;
      LPTIM2_ISR_INPUT   at 16#0# range 0 .. 31;
      LPTIM2_ICR_INPUT   at 16#4# range 0 .. 31;
      LPTIM2_DIER_INPUT  at 16#8# range 0 .. 31;
   end record;

   --  LPTIM2 address block description
   LPTIM2_Periph : aliased LPTIM2_Peripheral
     with Import, Address => LPTIM2_Base;

   type LPTIM3_Disc is
     (Output,
      Input);

   --  LPTIM3 address block description
   type LPTIM3_Peripheral
     (Discriminent : LPTIM3_Disc := Output)
   is record
      --  LPTIM configuration register
      LPTIM3_CFGR        : aliased LPTIM3_CFGR_Register;
      --  LPTIM control register
      LPTIM3_CR          : aliased LPTIM3_CR_Register;
      --  LPTIM compare register 1
      LPTIM3_CCR1        : aliased LPTIM3_CCR1_Register;
      --  LPTIM autoreload register
      LPTIM3_ARR         : aliased LPTIM3_ARR_Register;
      --  LPTIM counter register
      LPTIM3_CNT         : aliased LPTIM3_CNT_Register;
      --  LPTIM configuration register 2
      LPTIM3_CFGR2       : aliased LPTIM3_CFGR2_Register;
      --  LPTIM repetition register
      LPTIM3_RCR         : aliased LPTIM3_RCR_Register;
      --  LPTIM capture/compare mode register 1
      LPTIM3_CCMR1       : aliased LPTIM3_CCMR1_Register;
      --  LPTIM capture/compare mode register 2
      LPTIM3_CCMR2       : aliased LPTIM3_CCMR2_Register;
      --  LPTIM compare register 2
      LPTIM3_CCR2        : aliased LPTIM3_CCR2_Register;
      --  LPTIM compare register 3
      LPTIM3_CCR3        : aliased LPTIM3_CCR3_Register;
      --  LPTIM compare register 4
      LPTIM3_CCR4        : aliased LPTIM3_CCR4_Register;
      case Discriminent is
         when Output =>
            --  LPTIM3 interrupt and status register [alternate]
            LPTIM3_ISR_OUTPUT : aliased LPTIM3_ISR_OUTPUT_Register;
            --  LPTIM3 interrupt clear register [alternate]
            LPTIM3_ICR_OUTPUT : aliased LPTIM3_ICR_OUTPUT_Register;
            --  LPTIM3 interrupt enable register [alternate]
            LPTIM3_DIER_OUTPUT : aliased LPTIM3_DIER_OUTPUT_Register;
         when Input =>
            --  LPTIM3 interrupt and status register [alternate]
            LPTIM3_ISR_INPUT : aliased LPTIM3_ISR_INPUT_Register;
            --  LPTIM3 interrupt clear register [alternate]
            LPTIM3_ICR_INPUT : aliased LPTIM3_ICR_INPUT_Register;
            --  LPTIM3 interrupt enable register [alternate]
            LPTIM3_DIER_INPUT : aliased LPTIM3_DIER_INPUT_Register;
      end case;
   end record
     with Unchecked_Union, Volatile;

   for LPTIM3_Peripheral use record
      LPTIM3_CFGR        at 16#C# range 0 .. 31;
      LPTIM3_CR          at 16#10# range 0 .. 31;
      LPTIM3_CCR1        at 16#14# range 0 .. 31;
      LPTIM3_ARR         at 16#18# range 0 .. 31;
      LPTIM3_CNT         at 16#1C# range 0 .. 31;
      LPTIM3_CFGR2       at 16#24# range 0 .. 31;
      LPTIM3_RCR         at 16#28# range 0 .. 31;
      LPTIM3_CCMR1       at 16#2C# range 0 .. 31;
      LPTIM3_CCMR2       at 16#30# range 0 .. 31;
      LPTIM3_CCR2        at 16#34# range 0 .. 31;
      LPTIM3_CCR3        at 16#38# range 0 .. 31;
      LPTIM3_CCR4        at 16#3C# range 0 .. 31;
      LPTIM3_ISR_OUTPUT  at 16#0# range 0 .. 31;
      LPTIM3_ICR_OUTPUT  at 16#4# range 0 .. 31;
      LPTIM3_DIER_OUTPUT at 16#8# range 0 .. 31;
      LPTIM3_ISR_INPUT   at 16#0# range 0 .. 31;
      LPTIM3_ICR_INPUT   at 16#4# range 0 .. 31;
      LPTIM3_DIER_INPUT  at 16#8# range 0 .. 31;
   end record;

   --  LPTIM3 address block description
   LPTIM3_Periph : aliased LPTIM3_Peripheral
     with Import, Address => LPTIM3_Base;

end STM32_SVD.LPTIM;
