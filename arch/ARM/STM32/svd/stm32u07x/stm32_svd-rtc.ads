--  This spec has been automatically generated from STM32U073.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.RTC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype RTC_TR_SU_Field is HAL.UInt4;
   subtype RTC_TR_ST_Field is HAL.UInt3;
   subtype RTC_TR_MNU_Field is HAL.UInt4;
   subtype RTC_TR_MNT_Field is HAL.UInt3;
   subtype RTC_TR_HU_Field is HAL.UInt4;
   subtype RTC_TR_HT_Field is HAL.UInt2;

   --  RTC time register
   type RTC_TR_Register is record
      --  Second units in BCD format
      SU             : RTC_TR_SU_Field := 16#0#;
      --  Second tens in BCD format
      ST             : RTC_TR_ST_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Minute units in BCD format
      MNU            : RTC_TR_MNU_Field := 16#0#;
      --  Minute tens in BCD format
      MNT            : RTC_TR_MNT_Field := 16#0#;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Hour units in BCD format
      HU             : RTC_TR_HU_Field := 16#0#;
      --  Hour tens in BCD format
      HT             : RTC_TR_HT_Field := 16#0#;
      --  AM/PM notation
      PM             : Boolean := False;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_TR_Register use record
      SU             at 0 range 0 .. 3;
      ST             at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      MNU            at 0 range 8 .. 11;
      MNT            at 0 range 12 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      HU             at 0 range 16 .. 19;
      HT             at 0 range 20 .. 21;
      PM             at 0 range 22 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   subtype RTC_DR_DU_Field is HAL.UInt4;
   subtype RTC_DR_DT_Field is HAL.UInt2;
   subtype RTC_DR_MU_Field is HAL.UInt4;
   subtype RTC_DR_WDU_Field is HAL.UInt3;
   subtype RTC_DR_YU_Field is HAL.UInt4;
   subtype RTC_DR_YT_Field is HAL.UInt4;

   --  RTC date register
   type RTC_DR_Register is record
      --  Date units in BCD format
      DU             : RTC_DR_DU_Field := 16#1#;
      --  Date tens in BCD format
      DT             : RTC_DR_DT_Field := 16#0#;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Month units in BCD format
      MU             : RTC_DR_MU_Field := 16#1#;
      --  Month tens in BCD format
      MT             : Boolean := False;
      --  Week day units ...
      WDU            : RTC_DR_WDU_Field := 16#1#;
      --  Year units in BCD format
      YU             : RTC_DR_YU_Field := 16#0#;
      --  Year tens in BCD format
      YT             : RTC_DR_YT_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_DR_Register use record
      DU             at 0 range 0 .. 3;
      DT             at 0 range 4 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      MU             at 0 range 8 .. 11;
      MT             at 0 range 12 .. 12;
      WDU            at 0 range 13 .. 15;
      YU             at 0 range 16 .. 19;
      YT             at 0 range 20 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype RTC_ICSR_BIN_Field is HAL.UInt2;
   subtype RTC_ICSR_BCDU_Field is HAL.UInt3;

   --  RTC initialization control and status register
   type RTC_ICSR_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#3#;
      --  Read-only. Wake-up timer write flag This bit is set by hardware when
      --  WUT value can be changed, after the WUTE bit has been set to 0 in
      --  RTC_CR. It is cleared by hardware in initialization mode.
      WUTWF          : Boolean := True;
      --  Read-only. Shift operation pending This flag is set by hardware as
      --  soon as a shift operation is initiated by a write to the RTC_SHIFTR
      --  register. It is cleared by hardware when the corresponding shift
      --  operation has been executed. Writing to the SHPF bit has no effect.
      SHPF           : Boolean := False;
      --  Read-only. Initialization status flag This bit is set by hardware
      --  when the calendar year field is different from 0 (Backup domain reset
      --  state).
      INITS          : Boolean := False;
      --  Registers synchronization flag This bit is set by hardware each time
      --  the calendar registers are copied into the shadow registers (RTC_SSR,
      --  RTC_TR and RTC_DR). This bit is cleared by hardware in initialization
      --  mode, while a shift operation is pending (SHPF = 1), or when in
      --  bypass shadow register mode (BYPSHAD = 1). This bit can also be
      --  cleared by software. It is cleared either by software or by hardware
      --  in initialization mode.
      RSF            : Boolean := False;
      --  Read-only. Initialization flag When this bit is set to 1, the RTC is
      --  in initialization state, and the time, date and prescaler registers
      --  can be updated.
      INITF          : Boolean := False;
      --  Initialization mode
      INIT           : Boolean := False;
      --  Binary mode
      BIN            : RTC_ICSR_BIN_Field := 16#0#;
      --  BCD update (BIN = 10 or 11) In mixed mode when both BCD calendar and
      --  binary extended counter are used (BIN = 10 or 11), the calendar
      --  second is incremented using the SSR Least Significant Bits.
      BCDU           : RTC_ICSR_BCDU_Field := 16#0#;
      --  unspecified
      Reserved_13_15 : HAL.UInt3 := 16#0#;
      --  Read-only. Recalibration pending Flag The RECALPF status flag is
      --  automatically set to 1 when software writes to the RTC_CALR register,
      --  indicating that the RTC_CALR register is blocked. When the new
      --  calibration settings are taken into account, this bit returns to 0.
      --  Refer to Re-calibration on-the-fly.
      RECALPF        : Boolean := False;
      --  unspecified
      Reserved_17_31 : HAL.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_ICSR_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      WUTWF          at 0 range 2 .. 2;
      SHPF           at 0 range 3 .. 3;
      INITS          at 0 range 4 .. 4;
      RSF            at 0 range 5 .. 5;
      INITF          at 0 range 6 .. 6;
      INIT           at 0 range 7 .. 7;
      BIN            at 0 range 8 .. 9;
      BCDU           at 0 range 10 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      RECALPF        at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype RTC_PRER_PREDIV_S_Field is HAL.UInt15;
   subtype RTC_PRER_PREDIV_A_Field is HAL.UInt7;

   --  RTC prescaler register
   type RTC_PRER_Register is record
      --  Synchronous prescaler factor This is the synchronous division factor:
      --  ck_spre frequency = ck_apre frequency/(PREDIV_S+1)
      PREDIV_S       : RTC_PRER_PREDIV_S_Field := 16#FF#;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Asynchronous prescaler factor This is the asynchronous division
      --  factor: ck_apre frequency = RTCCLK frequency/(PREDIV_A+1)
      PREDIV_A       : RTC_PRER_PREDIV_A_Field := 16#7F#;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_PRER_Register use record
      PREDIV_S       at 0 range 0 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      PREDIV_A       at 0 range 16 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   subtype RTC_WUTR_WUT_Field is HAL.UInt16;
   subtype RTC_WUTR_WUTOCLR_Field is HAL.UInt16;

   --  RTC wake-up timer register
   type RTC_WUTR_Register is record
      --  Wake-up auto-reload value bits When the wake-up timer is enabled
      --  (WUTE set to 1), the WUTF flag is set every (WUT[15:0]1+11) ck_wut
      --  cycles. The ck_wut period is selected through WUCKSEL[2:0] bits of
      --  the RTC_CR register. When WUCKSEL[2] = 1, the wake-up timer becomes
      --  17-bits and WUCKSEL[1] effectively becomes WUT[16] the
      --  most-significant bit to be reloaded into the timer. The first
      --  assertion of WUTF occurs between WUT and (WUT + 2) ck_wut cycles
      --  after WUTE is set. Setting WUT[15:0] to 0x0000 with WUCKSEL[2:0] =
      --  011 (RTCCLK/2) is forbidden.
      WUT     : RTC_WUTR_WUT_Field := 16#FFFF#;
      --  Wake-up auto-reload output clear value When WUTOCLR[15:0] is
      --  different from 0x0000, WUTF is set by hardware when the auto-reload
      --  down-counter reaches 0 and is cleared by hardware when the
      --  auto-reload downcounter reaches WUTOCLR[15:0]. When WUTOCLR[15:0] =
      --  0x0000, WUTF is set by hardware when the WUT down-counter reaches 0
      --  and is cleared by software.
      WUTOCLR : RTC_WUTR_WUTOCLR_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_WUTR_Register use record
      WUT     at 0 range 0 .. 15;
      WUTOCLR at 0 range 16 .. 31;
   end record;

   subtype RTC_CR_WUCKSEL_Field is HAL.UInt3;
   subtype RTC_CR_OSEL_Field is HAL.UInt2;

   --  RTC control register
   type RTC_CR_Register is record
      --  ck_wut wake-up clock selection 10x: ck_spre (usually 11Hz) clock is
      --  selected in BCD mode. In binary or mixed mode, this is the clock
      --  selected by BCDU. 11x: ck_spre (usually 1 Hz) clock is selected in
      --  BCD mode. In binary or mixed mode, this is the clock selected by
      --  BCDU. Furthermore, 2<sup>16</sup> is added to the WUT counter value.
      WUCKSEL       : RTC_CR_WUCKSEL_Field := 16#0#;
      --  Timestamp event active edge TSE must be reset when TSEDGE is changed
      --  to avoid unwanted TSF setting.
      TSEDGE        : Boolean := False;
      --  RTC_REFIN reference clock detection enable (50 or 601Hz) Note: BIN
      --  must be 0x00 and PREDIV_S must be 0x00FF.
      REFCKON       : Boolean := False;
      --  Bypass the shadow registers Note: If the frequency of the APB clock
      --  is less than seven times the frequency of RTCCLK, BYPSHAD must be set
      --  to 1.
      BYPSHAD       : Boolean := False;
      --  Hour format
      FMT           : Boolean := False;
      --  SSR underflow interrupt enable
      SSRUIE        : Boolean := False;
      --  Alarm A enable
      ALRAE         : Boolean := False;
      --  Alarm B enable
      ALRBE         : Boolean := False;
      --  Wake-up timer enable Note: When the wake-up timer is disabled, wait
      --  for WUTWF = 1 before enabling it again.
      WUTE          : Boolean := False;
      --  timestamp enable
      TSE           : Boolean := False;
      --  Alarm A interrupt enable
      ALRAIE        : Boolean := False;
      --  Alarm B interrupt enable
      ALRBIE        : Boolean := False;
      --  Wake-up timer interrupt enable
      WUTIE         : Boolean := False;
      --  Timestamp interrupt enable
      TSIE          : Boolean := False;
      --  Write-only. Add 1 hour (summer time change) When this bit is set
      --  outside initialization mode, 1 hour is added to the calendar time.
      --  This bit is always read as 0.
      ADD1H         : Boolean := False;
      --  Write-only. Subtract 1 hour (winter time change) When this bit is set
      --  outside initialization mode, 1 hour is subtracted to the calendar
      --  time if the current hour is not 0. This bit is always read as 0.
      --  Setting this bit has no effect when current hour is 0.
      SUB1H         : Boolean := False;
      --  Backup This bit can be written by the user to memorize whether the
      --  daylight saving time change has been performed or not.
      BKP           : Boolean := False;
      --  Calibration output selection When COE = 1, this bit selects which
      --  signal is output on CALIB. These frequencies are valid for RTCCLK at
      --  32.7681kHz and prescalers at their default values (PREDIV_A = 127 and
      --  PREDIV_S = 255). Refer to Section128.3.16: Calibration clock output.
      COSEL         : Boolean := False;
      --  Output polarity This bit is used to configure the polarity of
      --  TAMPALRM output.
      POL           : Boolean := False;
      --  Output selection These bits are used to select the flag to be routed
      --  to TAMPALRM output.
      OSEL          : RTC_CR_OSEL_Field := 16#0#;
      --  Calibration output enable This bit enables the CALIB output
      COE           : Boolean := False;
      --  timestamp on internal event enable
      ITSE          : Boolean := False;
      --  Activate timestamp on tamper detection event TAMPTS is valid even if
      --  TSE = 0 in the RTC_CR register. Timestamp flag is set up to 3 ck_apre
      --  cycles after the tamper flags. Note: TAMPTS must be cleared before
      --  entering RTC initialization mode.
      TAMPTS        : Boolean := False;
      --  Tamper detection output enable on TAMPALRM
      TAMPOE        : Boolean := False;
      --  Alarm A flag automatic clear
      ALRAFCLR      : Boolean := False;
      --  Alarm B flag automatic clear
      ALRBFCLR      : Boolean := False;
      --  TAMPALRM pull-up enable
      TAMPALRM_PU   : Boolean := False;
      --  TAMPALRM output type
      TAMPALRM_TYPE : Boolean := False;
      --  RTC_OUT2 output enable
      OUT2EN        : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_CR_Register use record
      WUCKSEL       at 0 range 0 .. 2;
      TSEDGE        at 0 range 3 .. 3;
      REFCKON       at 0 range 4 .. 4;
      BYPSHAD       at 0 range 5 .. 5;
      FMT           at 0 range 6 .. 6;
      SSRUIE        at 0 range 7 .. 7;
      ALRAE         at 0 range 8 .. 8;
      ALRBE         at 0 range 9 .. 9;
      WUTE          at 0 range 10 .. 10;
      TSE           at 0 range 11 .. 11;
      ALRAIE        at 0 range 12 .. 12;
      ALRBIE        at 0 range 13 .. 13;
      WUTIE         at 0 range 14 .. 14;
      TSIE          at 0 range 15 .. 15;
      ADD1H         at 0 range 16 .. 16;
      SUB1H         at 0 range 17 .. 17;
      BKP           at 0 range 18 .. 18;
      COSEL         at 0 range 19 .. 19;
      POL           at 0 range 20 .. 20;
      OSEL          at 0 range 21 .. 22;
      COE           at 0 range 23 .. 23;
      ITSE          at 0 range 24 .. 24;
      TAMPTS        at 0 range 25 .. 25;
      TAMPOE        at 0 range 26 .. 26;
      ALRAFCLR      at 0 range 27 .. 27;
      ALRBFCLR      at 0 range 28 .. 28;
      TAMPALRM_PU   at 0 range 29 .. 29;
      TAMPALRM_TYPE at 0 range 30 .. 30;
      OUT2EN        at 0 range 31 .. 31;
   end record;

   subtype RTC_WPR_KEY_Field is HAL.UInt8;

   --  RTC write protection register
   type RTC_WPR_Register is record
      --  Write-only. Write protection key This byte is written by software.
      --  Reading this byte always returns 0x00. Refer to RTC register write
      --  protection for a description of how to unlock RTC register write
      --  protection.
      KEY           : RTC_WPR_KEY_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_WPR_Register use record
      KEY           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype RTC_CALR_CALM_Field is HAL.UInt9;

   --  RTC calibration register
   type RTC_CALR_Register is record
      --  Calibration minus The frequency of the calendar is reduced by masking
      --  CALM out of 2<sup>20</sup> RTCCLK pulses (321seconds if the input
      --  frequency is 327681Hz). This decreases the frequency of the calendar
      --  with a resolution of 0.95371ppm. To increase the frequency of the
      --  calendar, this feature should be used in conjunction with CALP. See
      --  Section128.3.14: RTC smooth digital calibration on page1733.
      CALM           : RTC_CALR_CALM_Field := 16#0#;
      --  unspecified
      Reserved_9_11  : HAL.UInt3 := 16#0#;
      --  RTC low-power mode
      LPCAL          : Boolean := False;
      --  Use a 16-second calibration cycle period When CALW16 is set to 1, the
      --  16-second calibration cycle period is selected. This bit must not be
      --  set to 1 if CALW8 = 1. Note: CALM[0] is stuck at 0 when CALW16 = 1.
      --  Refer to Section128.3.14: RTC smooth digital calibration.
      CALW16         : Boolean := False;
      --  Use an 8-second calibration cycle period When CALW8 is set to 1, the
      --  8-second calibration cycle period is selected. Note: CALM[1:0] are
      --  stuck at 00 when CALW8 = 1. Refer to Section128.3.14: RTC smooth
      --  digital calibration.
      CALW8          : Boolean := False;
      --  Increase frequency of RTC by 488.51ppm.
      CALP           : Boolean := False;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_CALR_Register use record
      CALM           at 0 range 0 .. 8;
      Reserved_9_11  at 0 range 9 .. 11;
      LPCAL          at 0 range 12 .. 12;
      CALW16         at 0 range 13 .. 13;
      CALW8          at 0 range 14 .. 14;
      CALP           at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype RTC_SHIFTR_SUBFS_Field is HAL.UInt15;

   --  RTC shift control register
   type RTC_SHIFTR_Register is record
      --  Write-only. Subtract a fraction of a second These bits are write only
      --  and is always read as zero. Writing to this bit has no effect when a
      --  shift operation is pending (when SHPF = 1, in RTC_ICSR). The value
      --  which is written to SUBFS is added to the synchronous prescaler
      --  counter. Since this counter counts down, this operation effectively
      --  subtracts from (delays) the clock by: Delay (seconds) = SUBFS /
      --  (PREDIV_S + 1) A fraction of a second can effectively be added to the
      --  clock (advancing the clock) when the ADD1S function is used in
      --  conjunction with SUBFS, effectively advancing the clock by: Advance
      --  (seconds) = (1 - (SUBFS / (PREDIV_S + 1))). In mixed BCD-binary mode
      --  (BIN=10 or 11), the SUBFS[14:BCDU+8] must be written with 0. Note:
      --  Writing to SUBFS causes RSF to be cleared. Software can then wait
      --  until RSF = 1 to be sure that the shadow registers have been updated
      --  with the shifted time.
      SUBFS          : RTC_SHIFTR_SUBFS_Field := 16#0#;
      --  unspecified
      Reserved_15_30 : HAL.UInt16 := 16#0#;
      --  Write-only. Add one second This bit is write only and is always read
      --  as zero. Writing to this bit has no effect when a shift operation is
      --  pending (when SHPF = 1, in RTC_ICSR). This function is intended to be
      --  used with SUBFS (see description below) in order to effectively add a
      --  fraction of a second to the clock in an atomic operation.
      ADD1S          : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_SHIFTR_Register use record
      SUBFS          at 0 range 0 .. 14;
      Reserved_15_30 at 0 range 15 .. 30;
      ADD1S          at 0 range 31 .. 31;
   end record;

   subtype RTC_TSTR_SU_Field is HAL.UInt4;
   subtype RTC_TSTR_ST_Field is HAL.UInt3;
   subtype RTC_TSTR_MNU_Field is HAL.UInt4;
   subtype RTC_TSTR_MNT_Field is HAL.UInt3;
   subtype RTC_TSTR_HU_Field is HAL.UInt4;
   subtype RTC_TSTR_HT_Field is HAL.UInt2;

   --  RTC timestamp time register
   type RTC_TSTR_Register is record
      --  Read-only. Second units in BCD format.
      SU             : RTC_TSTR_SU_Field;
      --  Read-only. Second tens in BCD format.
      ST             : RTC_TSTR_ST_Field;
      --  unspecified
      Reserved_7_7   : HAL.Bit;
      --  Read-only. Minute units in BCD format.
      MNU            : RTC_TSTR_MNU_Field;
      --  Read-only. Minute tens in BCD format.
      MNT            : RTC_TSTR_MNT_Field;
      --  unspecified
      Reserved_15_15 : HAL.Bit;
      --  Read-only. Hour units in BCD format.
      HU             : RTC_TSTR_HU_Field;
      --  Read-only. Hour tens in BCD format.
      HT             : RTC_TSTR_HT_Field;
      --  Read-only. AM/PM notation
      PM             : Boolean;
      --  unspecified
      Reserved_23_31 : HAL.UInt9;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_TSTR_Register use record
      SU             at 0 range 0 .. 3;
      ST             at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      MNU            at 0 range 8 .. 11;
      MNT            at 0 range 12 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      HU             at 0 range 16 .. 19;
      HT             at 0 range 20 .. 21;
      PM             at 0 range 22 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   subtype RTC_TSDR_DU_Field is HAL.UInt4;
   subtype RTC_TSDR_DT_Field is HAL.UInt2;
   subtype RTC_TSDR_MU_Field is HAL.UInt4;
   subtype RTC_TSDR_WDU_Field is HAL.UInt3;

   --  RTC timestamp date register
   type RTC_TSDR_Register is record
      --  Read-only. Date units in BCD format
      DU             : RTC_TSDR_DU_Field;
      --  Read-only. Date tens in BCD format
      DT             : RTC_TSDR_DT_Field;
      --  unspecified
      Reserved_6_7   : HAL.UInt2;
      --  Read-only. Month units in BCD format
      MU             : RTC_TSDR_MU_Field;
      --  Read-only. Month tens in BCD format
      MT             : Boolean;
      --  Read-only. Week day units
      WDU            : RTC_TSDR_WDU_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_TSDR_Register use record
      DU             at 0 range 0 .. 3;
      DT             at 0 range 4 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      MU             at 0 range 8 .. 11;
      MT             at 0 range 12 .. 12;
      WDU            at 0 range 13 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype RTC_ALRMAR_SU_Field is HAL.UInt4;
   subtype RTC_ALRMAR_ST_Field is HAL.UInt3;
   subtype RTC_ALRMAR_MNU_Field is HAL.UInt4;
   subtype RTC_ALRMAR_MNT_Field is HAL.UInt3;
   subtype RTC_ALRMAR_HU_Field is HAL.UInt4;
   subtype RTC_ALRMAR_HT_Field is HAL.UInt2;
   subtype RTC_ALRMAR_DU_Field is HAL.UInt4;
   subtype RTC_ALRMAR_DT_Field is HAL.UInt2;

   --  RTC alarm A register
   type RTC_ALRMAR_Register is record
      --  Second units in BCD format.
      SU    : RTC_ALRMAR_SU_Field := 16#0#;
      --  Second tens in BCD format.
      ST    : RTC_ALRMAR_ST_Field := 16#0#;
      --  Alarm A seconds mask
      MSK1  : Boolean := False;
      --  Minute units in BCD format
      MNU   : RTC_ALRMAR_MNU_Field := 16#0#;
      --  Minute tens in BCD format
      MNT   : RTC_ALRMAR_MNT_Field := 16#0#;
      --  Alarm A minutes mask
      MSK2  : Boolean := False;
      --  Hour units in BCD format
      HU    : RTC_ALRMAR_HU_Field := 16#0#;
      --  Hour tens in BCD format
      HT    : RTC_ALRMAR_HT_Field := 16#0#;
      --  AM/PM notation
      PM    : Boolean := False;
      --  Alarm A hours mask
      MSK3  : Boolean := False;
      --  Date units or day in BCD format
      DU    : RTC_ALRMAR_DU_Field := 16#0#;
      --  Date tens in BCD format
      DT    : RTC_ALRMAR_DT_Field := 16#0#;
      --  Week day selection
      WDSEL : Boolean := False;
      --  Alarm A date mask
      MSK4  : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_ALRMAR_Register use record
      SU    at 0 range 0 .. 3;
      ST    at 0 range 4 .. 6;
      MSK1  at 0 range 7 .. 7;
      MNU   at 0 range 8 .. 11;
      MNT   at 0 range 12 .. 14;
      MSK2  at 0 range 15 .. 15;
      HU    at 0 range 16 .. 19;
      HT    at 0 range 20 .. 21;
      PM    at 0 range 22 .. 22;
      MSK3  at 0 range 23 .. 23;
      DU    at 0 range 24 .. 27;
      DT    at 0 range 28 .. 29;
      WDSEL at 0 range 30 .. 30;
      MSK4  at 0 range 31 .. 31;
   end record;

   subtype RTC_ALRMASSR_SS_Field is HAL.UInt15;
   subtype RTC_ALRMASSR_MASKSS_Field is HAL.UInt6;

   --  RTC alarm A subsecond register
   type RTC_ALRMASSR_Register is record
      --  Subseconds value This value is compared with the contents of the
      --  synchronous prescaler counter to determine if alarm A is to be
      --  activated. Only bits 0 up MASKSS-1 are compared. This field is the
      --  mirror of SS[14:0] in the RTC_ALRABINR, and so can also be read or
      --  written through RTC_ALRABINR.
      SS             : RTC_ALRMASSR_SS_Field := 16#0#;
      --  unspecified
      Reserved_15_23 : HAL.UInt9 := 16#0#;
      --  Mask the most-significant bits starting at this bit ... From 32 to
      --  63: All 32 SS bits are compared and must match to activate alarm.
      --  Note: In BCD mode (BIN=00) the overflow bits of the synchronous
      --  counter (bits 31:15) are never compared. These bits can be different
      --  from 0 only after a shift operation.
      MASKSS         : RTC_ALRMASSR_MASKSS_Field := 16#0#;
      --  unspecified
      Reserved_30_30 : HAL.Bit := 16#0#;
      --  Clear synchronous counter on alarm (Binary mode only) Note: SSCLR
      --  must be kept to 0 when BCD or mixed mode is used (BIN = 00, 10 or
      --  11).
      SSCLR          : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_ALRMASSR_Register use record
      SS             at 0 range 0 .. 14;
      Reserved_15_23 at 0 range 15 .. 23;
      MASKSS         at 0 range 24 .. 29;
      Reserved_30_30 at 0 range 30 .. 30;
      SSCLR          at 0 range 31 .. 31;
   end record;

   subtype RTC_ALRMBR_SU_Field is HAL.UInt4;
   subtype RTC_ALRMBR_ST_Field is HAL.UInt3;
   subtype RTC_ALRMBR_MNU_Field is HAL.UInt4;
   subtype RTC_ALRMBR_MNT_Field is HAL.UInt3;
   subtype RTC_ALRMBR_HU_Field is HAL.UInt4;
   subtype RTC_ALRMBR_HT_Field is HAL.UInt2;
   subtype RTC_ALRMBR_DU_Field is HAL.UInt4;
   subtype RTC_ALRMBR_DT_Field is HAL.UInt2;

   --  RTC alarm B register
   type RTC_ALRMBR_Register is record
      --  Second units in BCD format
      SU    : RTC_ALRMBR_SU_Field := 16#0#;
      --  Second tens in BCD format
      ST    : RTC_ALRMBR_ST_Field := 16#0#;
      --  Alarm B seconds mask
      MSK1  : Boolean := False;
      --  Minute units in BCD format
      MNU   : RTC_ALRMBR_MNU_Field := 16#0#;
      --  Minute tens in BCD format
      MNT   : RTC_ALRMBR_MNT_Field := 16#0#;
      --  Alarm B minutes mask
      MSK2  : Boolean := False;
      --  Hour units in BCD format
      HU    : RTC_ALRMBR_HU_Field := 16#0#;
      --  Hour tens in BCD format
      HT    : RTC_ALRMBR_HT_Field := 16#0#;
      --  AM/PM notation
      PM    : Boolean := False;
      --  Alarm B hours mask
      MSK3  : Boolean := False;
      --  Date units or day in BCD format
      DU    : RTC_ALRMBR_DU_Field := 16#0#;
      --  Date tens in BCD format
      DT    : RTC_ALRMBR_DT_Field := 16#0#;
      --  Week day selection
      WDSEL : Boolean := False;
      --  Alarm B date mask
      MSK4  : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_ALRMBR_Register use record
      SU    at 0 range 0 .. 3;
      ST    at 0 range 4 .. 6;
      MSK1  at 0 range 7 .. 7;
      MNU   at 0 range 8 .. 11;
      MNT   at 0 range 12 .. 14;
      MSK2  at 0 range 15 .. 15;
      HU    at 0 range 16 .. 19;
      HT    at 0 range 20 .. 21;
      PM    at 0 range 22 .. 22;
      MSK3  at 0 range 23 .. 23;
      DU    at 0 range 24 .. 27;
      DT    at 0 range 28 .. 29;
      WDSEL at 0 range 30 .. 30;
      MSK4  at 0 range 31 .. 31;
   end record;

   subtype RTC_ALRMBSSR_SS_Field is HAL.UInt15;
   subtype RTC_ALRMBSSR_MASKSS_Field is HAL.UInt6;

   --  RTC alarm B subsecond register
   type RTC_ALRMBSSR_Register is record
      --  Subseconds value This value is compared with the contents of the
      --  synchronous prescaler counter to determine if alarm B is to be
      --  activated. Only bits 0 up to MASKSS-1 are compared. This field is the
      --  mirror of SS[14:0] in the RTC_ALRBBINR, and so can also be read or
      --  written through RTC_ALRBBINR.
      SS             : RTC_ALRMBSSR_SS_Field := 16#0#;
      --  unspecified
      Reserved_15_23 : HAL.UInt9 := 16#0#;
      --  Mask the most-significant bits starting at this bit ... From 32 to
      --  63: All 32 SS bits are compared and must match to activate alarm.
      --  Note: In BCD mode (BIN=00)The overflow bits of the synchronous
      --  counter (bits 15) is never compared. This bit can be different from 0
      --  only after a shift operation.
      MASKSS         : RTC_ALRMBSSR_MASKSS_Field := 16#0#;
      --  unspecified
      Reserved_30_30 : HAL.Bit := 16#0#;
      --  Clear synchronous counter on alarm (Binary mode only) Note: SSCLR
      --  must be kept to 0 when BCD or mixed mode is used (BIN = 00, 10 or
      --  11).
      SSCLR          : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_ALRMBSSR_Register use record
      SS             at 0 range 0 .. 14;
      Reserved_15_23 at 0 range 15 .. 23;
      MASKSS         at 0 range 24 .. 29;
      Reserved_30_30 at 0 range 30 .. 30;
      SSCLR          at 0 range 31 .. 31;
   end record;

   --  RTC status register
   type RTC_SR_Register is record
      --  Read-only. Alarm A flag This flag is set by hardware when the
      --  time/date registers (RTC_TR and RTC_DR) match the alarm A register
      --  (RTC_ALRMAR).
      ALRAF         : Boolean;
      --  Read-only. Alarm B flag This flag is set by hardware when the
      --  time/date registers (RTC_TR and RTC_DR) match the alarm B register
      --  (RTC_ALRMBR).
      ALRBF         : Boolean;
      --  Read-only. Wake-up timer flag This flag is set by hardware when the
      --  wake-up auto-reload counter reaches 0. If WUTOCLR[15:0] is different
      --  from 0x0000, WUTF is cleared by hardware when the wake-up auto-reload
      --  counter reaches WUTOCLR value. If WUTOCLR[15:0] is 0x0000, WUTF must
      --  be cleared by software. This flag must be cleared by software at
      --  least 1.5 RTCCLK periods before WUTF is set to 1 again.
      WUTF          : Boolean;
      --  Read-only. Timestamp flag This flag is set by hardware when a
      --  timestamp event occurs. If ITSF flag is set, TSF must be cleared
      --  together with ITSF. Note: TSF is not set if TAMPTS1=11 and the tamper
      --  flag is read during the 3 ck_apre cycles following tamper event.
      --  Refer to Timestamp on tamper event for more details.
      TSF           : Boolean;
      --  Read-only. Timestamp overflow flag This flag is set by hardware when
      --  a timestamp event occurs while TSF is already set. It is recommended
      --  to check and then clear TSOVF only after clearing the TSF bit.
      --  Otherwise, an overflow might not be noticed if a timestamp event
      --  occurs immediately before the TSF bit is cleared.
      TSOVF         : Boolean;
      --  Read-only. Internal timestamp flag This flag is set by hardware when
      --  a timestamp on the internal event occurs.
      ITSF          : Boolean;
      --  Read-only. SSR underflow flag This flag is set by hardware when the
      --  SSR rolls under 0. SSRUF is not set when SSCLR=1.
      SSRUF         : Boolean;
      --  unspecified
      Reserved_7_31 : HAL.UInt25;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_SR_Register use record
      ALRAF         at 0 range 0 .. 0;
      ALRBF         at 0 range 1 .. 1;
      WUTF          at 0 range 2 .. 2;
      TSF           at 0 range 3 .. 3;
      TSOVF         at 0 range 4 .. 4;
      ITSF          at 0 range 5 .. 5;
      SSRUF         at 0 range 6 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   --  RTC masked interrupt status register
   type RTC_MISR_Register is record
      --  Read-only. Alarm A masked flag This flag is set by hardware when the
      --  alarm A interrupt occurs.
      ALRAMF        : Boolean;
      --  Read-only. Alarm B masked flag This flag is set by hardware when the
      --  alarm B interrupt occurs.
      ALRBMF        : Boolean;
      --  Read-only. Wake-up timer masked flag This flag is set by hardware
      --  when the wake-up timer interrupt occurs. This flag must be cleared by
      --  software at least 1.5 RTCCLK periods before WUTF is set to 1 again.
      WUTMF         : Boolean;
      --  Read-only. Timestamp masked flag This flag is set by hardware when a
      --  timestamp interrupt occurs. If ITSF flag is set, TSF must be cleared
      --  together with ITSF.
      TSMF          : Boolean;
      --  Read-only. Timestamp overflow masked flag This flag is set by
      --  hardware when a timestamp interrupt occurs while TSMF is already set.
      --  It is recommended to check and then clear TSOVF only after clearing
      --  the TSF bit. Otherwise, an overflow might not be noticed if a
      --  timestamp event occurs immediately before the TSF bit is cleared.
      TSOVMF        : Boolean;
      --  Read-only. Internal timestamp masked flag This flag is set by
      --  hardware when a timestamp on the internal event occurs and
      --  timestampinterrupt is raised.
      ITSMF         : Boolean;
      --  Read-only. SSR underflow masked flag This flag is set by hardware
      --  when the SSR underflow interrupt occurs.
      SSRUMF        : Boolean;
      --  unspecified
      Reserved_7_31 : HAL.UInt25;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_MISR_Register use record
      ALRAMF        at 0 range 0 .. 0;
      ALRBMF        at 0 range 1 .. 1;
      WUTMF         at 0 range 2 .. 2;
      TSMF          at 0 range 3 .. 3;
      TSOVMF        at 0 range 4 .. 4;
      ITSMF         at 0 range 5 .. 5;
      SSRUMF        at 0 range 6 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   --  RTC status clear register
   type RTC_SCR_Register is record
      --  Write-only. Clear alarm A flag Writing 1 in this bit clears the ALRAF
      --  bit in the RTC_SR register.
      CALRAF        : Boolean := False;
      --  Write-only. Clear alarm B flag Writing 1 in this bit clears the ALRBF
      --  bit in the RTC_SR register.
      CALRBF        : Boolean := False;
      --  Write-only. Clear wake-up timer flag Writing 1 in this bit clears the
      --  WUTF bit in the RTC_SR register.
      CWUTF         : Boolean := False;
      --  Write-only. Clear timestamp flag Writing 1 in this bit clears the TSF
      --  bit in the RTC_SR register. If ITSF flag is set, TSF must be cleared
      --  together with ITSF by setting CRSF and CITSF.
      CTSF          : Boolean := False;
      --  Write-only. Clear timestamp overflow flag Writing 1 in this bit
      --  clears the TSOVF bit in the RTC_SR register. It is recommended to
      --  check and then clear TSOVF only after clearing the TSF bit.
      --  Otherwise, an overflow might not be noticed if a timestamp event
      --  occurs immediately before the TSF bit is cleared.
      CTSOVF        : Boolean := False;
      --  Write-only. Clear internal timestamp flag Writing 1 in this bit
      --  clears the ITSF bit in the RTC_SR register.
      CITSF         : Boolean := False;
      --  Write-only. Clear SSR underflow flag Writing 1 in this bit clears the
      --  SSRUF in the RTC_SR register.
      CSSRUF        : Boolean := False;
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_SCR_Register use record
      CALRAF        at 0 range 0 .. 0;
      CALRBF        at 0 range 1 .. 1;
      CWUTF         at 0 range 2 .. 2;
      CTSF          at 0 range 3 .. 3;
      CTSOVF        at 0 range 4 .. 4;
      CITSF         at 0 range 5 .. 5;
      CSSRUF        at 0 range 6 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  RTC register block
   type RTC_Peripheral is record
      --  RTC time register
      RTC_TR       : aliased RTC_TR_Register;
      --  RTC date register
      RTC_DR       : aliased RTC_DR_Register;
      --  RTC subsecond register
      RTC_SSR      : aliased HAL.UInt32;
      --  RTC initialization control and status register
      RTC_ICSR     : aliased RTC_ICSR_Register;
      --  RTC prescaler register
      RTC_PRER     : aliased RTC_PRER_Register;
      --  RTC wake-up timer register
      RTC_WUTR     : aliased RTC_WUTR_Register;
      --  RTC control register
      RTC_CR       : aliased RTC_CR_Register;
      --  RTC write protection register
      RTC_WPR      : aliased RTC_WPR_Register;
      --  RTC calibration register
      RTC_CALR     : aliased RTC_CALR_Register;
      --  RTC shift control register
      RTC_SHIFTR   : aliased RTC_SHIFTR_Register;
      --  RTC timestamp time register
      RTC_TSTR     : aliased RTC_TSTR_Register;
      --  RTC timestamp date register
      RTC_TSDR     : aliased RTC_TSDR_Register;
      --  RTC timestamp subsecond register
      RTC_TSSSR    : aliased HAL.UInt32;
      --  RTC alarm A register
      RTC_ALRMAR   : aliased RTC_ALRMAR_Register;
      --  RTC alarm A subsecond register
      RTC_ALRMASSR : aliased RTC_ALRMASSR_Register;
      --  RTC alarm B register
      RTC_ALRMBR   : aliased RTC_ALRMBR_Register;
      --  RTC alarm B subsecond register
      RTC_ALRMBSSR : aliased RTC_ALRMBSSR_Register;
      --  RTC status register
      RTC_SR       : aliased RTC_SR_Register;
      --  RTC masked interrupt status register
      RTC_MISR     : aliased RTC_MISR_Register;
      --  RTC status clear register
      RTC_SCR      : aliased RTC_SCR_Register;
      --  RTC alarm A binary mode register
      RTC_ALRABINR : aliased HAL.UInt32;
      --  RTC alarm B binary mode register
      RTC_ALRBBINR : aliased HAL.UInt32;
   end record
     with Volatile;

   for RTC_Peripheral use record
      RTC_TR       at 16#0# range 0 .. 31;
      RTC_DR       at 16#4# range 0 .. 31;
      RTC_SSR      at 16#8# range 0 .. 31;
      RTC_ICSR     at 16#C# range 0 .. 31;
      RTC_PRER     at 16#10# range 0 .. 31;
      RTC_WUTR     at 16#14# range 0 .. 31;
      RTC_CR       at 16#18# range 0 .. 31;
      RTC_WPR      at 16#24# range 0 .. 31;
      RTC_CALR     at 16#28# range 0 .. 31;
      RTC_SHIFTR   at 16#2C# range 0 .. 31;
      RTC_TSTR     at 16#30# range 0 .. 31;
      RTC_TSDR     at 16#34# range 0 .. 31;
      RTC_TSSSR    at 16#38# range 0 .. 31;
      RTC_ALRMAR   at 16#40# range 0 .. 31;
      RTC_ALRMASSR at 16#44# range 0 .. 31;
      RTC_ALRMBR   at 16#48# range 0 .. 31;
      RTC_ALRMBSSR at 16#4C# range 0 .. 31;
      RTC_SR       at 16#50# range 0 .. 31;
      RTC_MISR     at 16#54# range 0 .. 31;
      RTC_SCR      at 16#5C# range 0 .. 31;
      RTC_ALRABINR at 16#70# range 0 .. 31;
      RTC_ALRBBINR at 16#74# range 0 .. 31;
   end record;

   --  RTC register block
   RTC_Periph : aliased RTC_Peripheral
     with Import, Address => RTC_Base;

end STM32_SVD.RTC;
