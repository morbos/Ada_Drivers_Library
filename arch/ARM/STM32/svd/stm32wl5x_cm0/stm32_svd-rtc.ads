--  This spec has been automatically generated from STM32WL5x_CM0P.svd

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

   subtype TR_SU_Field is HAL.UInt4;
   subtype TR_ST_Field is HAL.UInt3;
   subtype TR_MNU_Field is HAL.UInt4;
   subtype TR_MNT_Field is HAL.UInt3;
   subtype TR_HU_Field is HAL.UInt4;
   subtype TR_HT_Field is HAL.UInt2;

   --  TR
   type TR_Register is record
      --  Second units in BCD format
      SU             : TR_SU_Field := 16#0#;
      --  Second tens in BCD format
      ST             : TR_ST_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Minute units in BCD format
      MNU            : TR_MNU_Field := 16#0#;
      --  Minute tens in BCD format
      MNT            : TR_MNT_Field := 16#0#;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Hour units in BCD format
      HU             : TR_HU_Field := 16#0#;
      --  Hour tens in BCD format
      HT             : TR_HT_Field := 16#0#;
      --  AM/PM notation
      PM             : Boolean := False;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TR_Register use record
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

   subtype DR_DU_Field is HAL.UInt4;
   subtype DR_DT_Field is HAL.UInt2;
   subtype DR_MU_Field is HAL.UInt4;
   subtype DR_WDU_Field is HAL.UInt3;
   subtype DR_YU_Field is HAL.UInt4;
   subtype DR_YT_Field is HAL.UInt4;

   --  DR
   type DR_Register is record
      --  Date units in BCD format
      DU             : DR_DU_Field := 16#1#;
      --  Date tens in BCD format
      DT             : DR_DT_Field := 16#0#;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Month units in BCD format
      MU             : DR_MU_Field := 16#1#;
      --  Month tens in BCD format
      MT             : Boolean := False;
      --  Week day units
      WDU            : DR_WDU_Field := 16#1#;
      --  Year units in BCD format
      YU             : DR_YU_Field := 16#0#;
      --  Year tens in BCD format
      YT             : DR_YT_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR_Register use record
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

   subtype ICSR_BIN_Field is HAL.UInt2;
   subtype ICSR_BCDU_Field is HAL.UInt3;

   --  ICSR
   type ICSR_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#3#;
      --  Read-only. Wakeup timer write flag
      WUTWF          : Boolean := True;
      --  Read-only. Shift operation pending
      SHPF           : Boolean := False;
      --  Read-only. Initialization status flag
      INITS          : Boolean := False;
      --  Registers synchronization flag
      RSF            : Boolean := False;
      --  Read-only. Initialization flag
      INITF          : Boolean := False;
      --  Initialization mode
      INIT           : Boolean := False;
      --  Binary mode
      BIN            : ICSR_BIN_Field := 16#0#;
      --  BCD update
      BCDU           : ICSR_BCDU_Field := 16#0#;
      --  unspecified
      Reserved_13_15 : HAL.UInt3 := 16#0#;
      --  Read-only. Recalibration pending Flag
      RECALPF        : Boolean := False;
      --  unspecified
      Reserved_17_31 : HAL.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ICSR_Register use record
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

   subtype PRER_PREDIV_S_Field is HAL.UInt15;
   subtype PRER_PREDIV_A_Field is HAL.UInt7;

   --  PRER
   type PRER_Register is record
      --  Synchronous prescaler factor
      PREDIV_S       : PRER_PREDIV_S_Field := 16#FF#;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Asynchronous prescaler factor
      PREDIV_A       : PRER_PREDIV_A_Field := 16#7F#;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PRER_Register use record
      PREDIV_S       at 0 range 0 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      PREDIV_A       at 0 range 16 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   subtype WUTR_WUT_Field is HAL.UInt16;
   subtype WUTR_WUTOCLR_Field is HAL.UInt16;

   --  WUTR
   type WUTR_Register is record
      --  Wakeup auto-reload value bits
      WUT     : WUTR_WUT_Field := 16#FFFF#;
      --  Wakeup auto-reload output clear value
      WUTOCLR : WUTR_WUTOCLR_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for WUTR_Register use record
      WUT     at 0 range 0 .. 15;
      WUTOCLR at 0 range 16 .. 31;
   end record;

   subtype CR_WUCKSEL_Field is HAL.UInt3;
   subtype CR_OSEL_Field is HAL.UInt2;

   --  CR
   type CR_Register is record
      --  Wakeup clock selection
      WUCKSEL        : CR_WUCKSEL_Field := 16#0#;
      --  Timestamp event active edge
      TSEDGE         : Boolean := False;
      --  RTC_REFIN reference clock detection enable (50 or 60 Hz)
      REFCKON        : Boolean := False;
      --  Bypass the shadow registers
      BYPSHAD        : Boolean := False;
      --  Hour format
      FMT            : Boolean := False;
      --  SSR underflow interrupt enable
      SSRUIE         : Boolean := False;
      --  Alarm A enable
      ALRAE          : Boolean := False;
      --  Alarm B enable
      ALRBE          : Boolean := False;
      --  Wakeup timer enable
      WUTE           : Boolean := False;
      --  timestamp enable
      TSE            : Boolean := False;
      --  Alarm A interrupt enable
      ALRAIE         : Boolean := False;
      --  Alarm B interrupt enable
      ALRBIE         : Boolean := False;
      --  Wakeup timer interrupt enable
      WUTIE          : Boolean := False;
      --  Timestamp interrupt enable
      TSIE           : Boolean := False;
      --  Write-only. Add 1 hour (summer time change)
      ADD1H          : Boolean := False;
      --  Write-only. Subtract 1 hour (winter time change)
      SUB1H          : Boolean := False;
      --  Backup
      BKP            : Boolean := False;
      --  Calibration output selection
      COSEL          : Boolean := False;
      --  Output polarity
      POL            : Boolean := False;
      --  Output selection
      OSEL           : CR_OSEL_Field := 16#0#;
      --  Calibration output enable
      COE            : Boolean := False;
      --  timestamp on internal event enable
      ITSE           : Boolean := False;
      --  Activate timestamp on tamper detection event
      TAMPTS         : Boolean := False;
      --  Tamper detection output enable on TAMPALRM
      TAMPOE         : Boolean := False;
      --  unspecified
      Reserved_27_28 : HAL.UInt2 := 16#0#;
      --  TAMPALRM pull-up enable
      TAMPALRM_PU    : Boolean := False;
      --  TAMPALRM output type
      TAMPALRM_TYPE  : Boolean := False;
      --  RTC_OUT2 output enable
      OUT2EN         : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      WUCKSEL        at 0 range 0 .. 2;
      TSEDGE         at 0 range 3 .. 3;
      REFCKON        at 0 range 4 .. 4;
      BYPSHAD        at 0 range 5 .. 5;
      FMT            at 0 range 6 .. 6;
      SSRUIE         at 0 range 7 .. 7;
      ALRAE          at 0 range 8 .. 8;
      ALRBE          at 0 range 9 .. 9;
      WUTE           at 0 range 10 .. 10;
      TSE            at 0 range 11 .. 11;
      ALRAIE         at 0 range 12 .. 12;
      ALRBIE         at 0 range 13 .. 13;
      WUTIE          at 0 range 14 .. 14;
      TSIE           at 0 range 15 .. 15;
      ADD1H          at 0 range 16 .. 16;
      SUB1H          at 0 range 17 .. 17;
      BKP            at 0 range 18 .. 18;
      COSEL          at 0 range 19 .. 19;
      POL            at 0 range 20 .. 20;
      OSEL           at 0 range 21 .. 22;
      COE            at 0 range 23 .. 23;
      ITSE           at 0 range 24 .. 24;
      TAMPTS         at 0 range 25 .. 25;
      TAMPOE         at 0 range 26 .. 26;
      Reserved_27_28 at 0 range 27 .. 28;
      TAMPALRM_PU    at 0 range 29 .. 29;
      TAMPALRM_TYPE  at 0 range 30 .. 30;
      OUT2EN         at 0 range 31 .. 31;
   end record;

   subtype WPR_KEY_Field is HAL.UInt8;

   --  write protection register
   type WPR_Register is record
      --  Write-only. Write protection key
      KEY           : WPR_KEY_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for WPR_Register use record
      KEY           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype CALR_CALM_Field is HAL.UInt9;

   --  CALR
   type CALR_Register is record
      --  Calibration minus
      CALM           : CALR_CALM_Field := 16#0#;
      --  unspecified
      Reserved_9_11  : HAL.UInt3 := 16#0#;
      --  Calibration low-power mode
      LPCAL          : Boolean := False;
      --  CALW16
      CALW16         : Boolean := False;
      --  Use a 16-second calibration cycle period
      CALW8          : Boolean := False;
      --  Use an 8-second calibration cycle period
      CALP           : Boolean := False;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CALR_Register use record
      CALM           at 0 range 0 .. 8;
      Reserved_9_11  at 0 range 9 .. 11;
      LPCAL          at 0 range 12 .. 12;
      CALW16         at 0 range 13 .. 13;
      CALW8          at 0 range 14 .. 14;
      CALP           at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype SHIFTR_SUBFS_Field is HAL.UInt15;

   --  SHIFTR
   type SHIFTR_Register is record
      --  Write-only. Subtract a fraction of a second
      SUBFS          : SHIFTR_SUBFS_Field := 16#0#;
      --  unspecified
      Reserved_15_30 : HAL.UInt16 := 16#0#;
      --  Write-only. Add one second
      ADD1S          : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SHIFTR_Register use record
      SUBFS          at 0 range 0 .. 14;
      Reserved_15_30 at 0 range 15 .. 30;
      ADD1S          at 0 range 31 .. 31;
   end record;

   subtype TSTR_SU_Field is HAL.UInt4;
   subtype TSTR_ST_Field is HAL.UInt3;
   subtype TSTR_MNU_Field is HAL.UInt4;
   subtype TSTR_MNT_Field is HAL.UInt3;
   subtype TSTR_HU_Field is HAL.UInt4;
   subtype TSTR_HT_Field is HAL.UInt2;

   --  TSTR
   type TSTR_Register is record
      --  Read-only. Second units in BCD format.
      SU             : TSTR_SU_Field;
      --  Read-only. Second tens in BCD format.
      ST             : TSTR_ST_Field;
      --  unspecified
      Reserved_7_7   : HAL.Bit;
      --  Read-only. Minute units in BCD format.
      MNU            : TSTR_MNU_Field;
      --  Read-only. Minute tens in BCD format.
      MNT            : TSTR_MNT_Field;
      --  unspecified
      Reserved_15_15 : HAL.Bit;
      --  Read-only. Hour units in BCD format.
      HU             : TSTR_HU_Field;
      --  Read-only. Hour tens in BCD format.
      HT             : TSTR_HT_Field;
      --  Read-only. AM/PM notation
      PM             : Boolean;
      --  unspecified
      Reserved_23_31 : HAL.UInt9;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TSTR_Register use record
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

   subtype TSDR_DU_Field is HAL.UInt4;
   subtype TSDR_DT_Field is HAL.UInt2;
   subtype TSDR_MU_Field is HAL.UInt4;
   subtype TSDR_WDU_Field is HAL.UInt3;

   --  TSDR
   type TSDR_Register is record
      --  Read-only. Date units in BCD format
      DU             : TSDR_DU_Field;
      --  Read-only. Date tens in BCD format
      DT             : TSDR_DT_Field;
      --  unspecified
      Reserved_6_7   : HAL.UInt2;
      --  Read-only. Month units in BCD format
      MU             : TSDR_MU_Field;
      --  Read-only. Month tens in BCD format
      MT             : Boolean;
      --  Read-only. Week day units
      WDU            : TSDR_WDU_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TSDR_Register use record
      DU             at 0 range 0 .. 3;
      DT             at 0 range 4 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      MU             at 0 range 8 .. 11;
      MT             at 0 range 12 .. 12;
      WDU            at 0 range 13 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype ALRMAR_SU_Field is HAL.UInt4;
   subtype ALRMAR_ST_Field is HAL.UInt3;
   subtype ALRMAR_MNU_Field is HAL.UInt4;
   subtype ALRMAR_MNT_Field is HAL.UInt3;
   subtype ALRMAR_HU_Field is HAL.UInt4;
   subtype ALRMAR_HT_Field is HAL.UInt2;
   subtype ALRMAR_DU_Field is HAL.UInt4;
   subtype ALRMAR_DT_Field is HAL.UInt2;

   --  ALRMAR
   type ALRMAR_Register is record
      --  Second units in BCD format.
      SU    : ALRMAR_SU_Field := 16#0#;
      --  Second tens in BCD format.
      ST    : ALRMAR_ST_Field := 16#0#;
      --  Alarm A seconds mask
      MSK1  : Boolean := False;
      --  Minute units in BCD format
      MNU   : ALRMAR_MNU_Field := 16#0#;
      --  Minute tens in BCD format
      MNT   : ALRMAR_MNT_Field := 16#0#;
      --  Alarm A minutes mask
      MSK2  : Boolean := False;
      --  Hour units in BCD format
      HU    : ALRMAR_HU_Field := 16#0#;
      --  Hour tens in BCD format
      HT    : ALRMAR_HT_Field := 16#0#;
      --  AM/PM notation
      PM    : Boolean := False;
      --  Alarm A hours mask
      MSK3  : Boolean := False;
      --  Date units or day in BCD format
      DU    : ALRMAR_DU_Field := 16#0#;
      --  Date tens in BCD format
      DT    : ALRMAR_DT_Field := 16#0#;
      --  Week day selection
      WDSEL : Boolean := False;
      --  Alarm A date mask
      MSK4  : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ALRMAR_Register use record
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

   subtype ALRMASSR_SS_Field is HAL.UInt15;
   subtype ALRMASSR_MASKSS_Field is HAL.UInt6;

   --  ALRMASSR
   type ALRMASSR_Register is record
      --  Sub seconds value
      SS             : ALRMASSR_SS_Field := 16#0#;
      --  unspecified
      Reserved_15_23 : HAL.UInt9 := 16#0#;
      --  Mask the most-significant bits starting at this bit
      MASKSS         : ALRMASSR_MASKSS_Field := 16#0#;
      --  unspecified
      Reserved_30_30 : HAL.Bit := 16#0#;
      --  Clear synchronous counter on alarm (Binary mode only)
      SSCLR          : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ALRMASSR_Register use record
      SS             at 0 range 0 .. 14;
      Reserved_15_23 at 0 range 15 .. 23;
      MASKSS         at 0 range 24 .. 29;
      Reserved_30_30 at 0 range 30 .. 30;
      SSCLR          at 0 range 31 .. 31;
   end record;

   subtype ALRMBR_SU_Field is HAL.UInt4;
   subtype ALRMBR_ST_Field is HAL.UInt3;
   subtype ALRMBR_MNU_Field is HAL.UInt4;
   subtype ALRMBR_MNT_Field is HAL.UInt3;
   subtype ALRMBR_HU_Field is HAL.UInt4;
   subtype ALRMBR_HT_Field is HAL.UInt2;
   subtype ALRMBR_DU_Field is HAL.UInt4;
   subtype ALRMBR_DT_Field is HAL.UInt2;

   --  ALRMBR
   type ALRMBR_Register is record
      --  Second units in BCD format
      SU    : ALRMBR_SU_Field := 16#0#;
      --  Second tens in BCD format
      ST    : ALRMBR_ST_Field := 16#0#;
      --  Alarm B seconds mask
      MSK1  : Boolean := False;
      --  Minute units in BCD format
      MNU   : ALRMBR_MNU_Field := 16#0#;
      --  Minute tens in BCD format
      MNT   : ALRMBR_MNT_Field := 16#0#;
      --  Alarm B minutes mask
      MSK2  : Boolean := False;
      --  Hour units in BCD format
      HU    : ALRMBR_HU_Field := 16#0#;
      --  Hour tens in BCD format
      HT    : ALRMBR_HT_Field := 16#0#;
      --  AM/PM notation
      PM    : Boolean := False;
      --  Alarm B hours mask
      MSK3  : Boolean := False;
      --  Date units or day in BCD format
      DU    : ALRMBR_DU_Field := 16#0#;
      --  Date tens in BCD format
      DT    : ALRMBR_DT_Field := 16#0#;
      --  Week day selection
      WDSEL : Boolean := False;
      --  Alarm B date mask
      MSK4  : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ALRMBR_Register use record
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

   subtype ALRMBSSR_SS_Field is HAL.UInt15;
   subtype ALRMBSSR_MASKSS_Field is HAL.UInt6;

   --  ALRMBSSR
   type ALRMBSSR_Register is record
      --  Sub seconds value
      SS             : ALRMBSSR_SS_Field := 16#0#;
      --  unspecified
      Reserved_15_23 : HAL.UInt9 := 16#0#;
      --  Mask the most-significant bits starting at this bit
      MASKSS         : ALRMBSSR_MASKSS_Field := 16#0#;
      --  unspecified
      Reserved_30_30 : HAL.Bit := 16#0#;
      --  Clear synchronous counter on alarm (Binary mode only)
      SSCLR          : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ALRMBSSR_Register use record
      SS             at 0 range 0 .. 14;
      Reserved_15_23 at 0 range 15 .. 23;
      MASKSS         at 0 range 24 .. 29;
      Reserved_30_30 at 0 range 30 .. 30;
      SSCLR          at 0 range 31 .. 31;
   end record;

   --  SR
   type SR_Register is record
      --  Read-only. Alarm A flag
      ALRAF         : Boolean;
      --  Read-only. Alarm B flag
      ALRBF         : Boolean;
      --  Read-only. Wakeup timer flag
      WUTF          : Boolean;
      --  Read-only. Timestamp flag
      TSF           : Boolean;
      --  Read-only. Timestamp overflow flag
      TSOVF         : Boolean;
      --  Read-only. Internal timestamp flag
      ITSF          : Boolean;
      --  Read-only. SSR underflow flag
      SSRUF         : Boolean;
      --  unspecified
      Reserved_7_31 : HAL.UInt25;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register use record
      ALRAF         at 0 range 0 .. 0;
      ALRBF         at 0 range 1 .. 1;
      WUTF          at 0 range 2 .. 2;
      TSF           at 0 range 3 .. 3;
      TSOVF         at 0 range 4 .. 4;
      ITSF          at 0 range 5 .. 5;
      SSRUF         at 0 range 6 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   --  MISR
   type MISR_Register is record
      --  Read-only. Alarm A masked flag
      ALRAMF        : Boolean;
      --  Read-only. Alarm B masked flag
      ALRBMF        : Boolean;
      --  Read-only. Wakeup timer masked flag
      WUTMF         : Boolean;
      --  Read-only. Timestamp masked flag
      TSMF          : Boolean;
      --  Read-only. Timestamp overflow masked flag
      TSOVMF        : Boolean;
      --  Read-only. Internal timestamp masked flag
      ITSMF         : Boolean;
      --  Read-only. SSR underflow masked flag
      SSRUMF        : Boolean;
      --  unspecified
      Reserved_7_31 : HAL.UInt25;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MISR_Register use record
      ALRAMF        at 0 range 0 .. 0;
      ALRBMF        at 0 range 1 .. 1;
      WUTMF         at 0 range 2 .. 2;
      TSMF          at 0 range 3 .. 3;
      TSOVMF        at 0 range 4 .. 4;
      ITSMF         at 0 range 5 .. 5;
      SSRUMF        at 0 range 6 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   --  SCR
   type SCR_Register is record
      --  Write-only. Clear alarm A flag
      CALRAF        : Boolean := False;
      --  Write-only. Clear alarm B flag
      CALRBF        : Boolean := False;
      --  Write-only. Clear wakeup timer flag
      CWUTF         : Boolean := False;
      --  Write-only. Clear timestamp flag
      CTSF          : Boolean := False;
      --  Write-only. Clear timestamp overflow flag
      CTSOVF        : Boolean := False;
      --  Write-only. Clear internal timestamp flag
      CITSF         : Boolean := False;
      --  Write-only. Clear SSR underflow flag
      CSSRUF        : Boolean := False;
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SCR_Register use record
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

   --  Real-time clock
   type RTC_Peripheral is record
      --  TR
      TR       : aliased TR_Register;
      --  DR
      DR       : aliased DR_Register;
      --  SSR
      SSR      : aliased HAL.UInt32;
      --  ICSR
      ICSR     : aliased ICSR_Register;
      --  PRER
      PRER     : aliased PRER_Register;
      --  WUTR
      WUTR     : aliased WUTR_Register;
      --  CR
      CR       : aliased CR_Register;
      --  write protection register
      WPR      : aliased WPR_Register;
      --  CALR
      CALR     : aliased CALR_Register;
      --  SHIFTR
      SHIFTR   : aliased SHIFTR_Register;
      --  TSTR
      TSTR     : aliased TSTR_Register;
      --  TSDR
      TSDR     : aliased TSDR_Register;
      --  TSSSR
      TSSSR    : aliased HAL.UInt32;
      --  ALRMAR
      ALRMAR   : aliased ALRMAR_Register;
      --  ALRMASSR
      ALRMASSR : aliased ALRMASSR_Register;
      --  ALRMBR
      ALRMBR   : aliased ALRMBR_Register;
      --  ALRMBSSR
      ALRMBSSR : aliased ALRMBSSR_Register;
      --  SR
      SR       : aliased SR_Register;
      --  MISR
      MISR     : aliased MISR_Register;
      --  SCR
      SCR      : aliased SCR_Register;
      --  RTC alarm A binary mode register
      ALRABINR : aliased HAL.UInt32;
      --  RTC alarm B binary mode register
      ALRBBINR : aliased HAL.UInt32;
   end record
     with Volatile;

   for RTC_Peripheral use record
      TR       at 16#0# range 0 .. 31;
      DR       at 16#4# range 0 .. 31;
      SSR      at 16#8# range 0 .. 31;
      ICSR     at 16#C# range 0 .. 31;
      PRER     at 16#10# range 0 .. 31;
      WUTR     at 16#14# range 0 .. 31;
      CR       at 16#18# range 0 .. 31;
      WPR      at 16#24# range 0 .. 31;
      CALR     at 16#28# range 0 .. 31;
      SHIFTR   at 16#2C# range 0 .. 31;
      TSTR     at 16#30# range 0 .. 31;
      TSDR     at 16#34# range 0 .. 31;
      TSSSR    at 16#38# range 0 .. 31;
      ALRMAR   at 16#40# range 0 .. 31;
      ALRMASSR at 16#44# range 0 .. 31;
      ALRMBR   at 16#48# range 0 .. 31;
      ALRMBSSR at 16#4C# range 0 .. 31;
      SR       at 16#50# range 0 .. 31;
      MISR     at 16#54# range 0 .. 31;
      SCR      at 16#5C# range 0 .. 31;
      ALRABINR at 16#70# range 0 .. 31;
      ALRBBINR at 16#74# range 0 .. 31;
   end record;

   --  Real-time clock
   RTC_Periph : aliased RTC_Peripheral
     with Import, Address => RTC_Base;

end STM32_SVD.RTC;
