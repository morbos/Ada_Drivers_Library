--  This spec has been automatically generated from STM32U5xx.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.DBGMCU is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype IDCODE_DEV_ID_Field is HAL.UInt12;
   subtype IDCODE_REV_ID_Field is HAL.UInt16;

   --  DBGMCU_IDCODE
   type IDCODE_Register is record
      --  Read-only. Device dentification
      DEV_ID         : IDCODE_DEV_ID_Field;
      --  unspecified
      Reserved_12_15 : HAL.UInt4;
      --  Read-only. Revision
      REV_ID         : IDCODE_REV_ID_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IDCODE_Register use record
      DEV_ID         at 0 range 0 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      REV_ID         at 0 range 16 .. 31;
   end record;

   subtype CR_TRACE_MODE_Field is HAL.UInt2;

   --  Debug MCU configuration register
   type CR_Register is record
      --  unspecified
      Reserved_0_0  : HAL.Bit := 16#0#;
      --  Debug Stop mode
      DBG_STOP      : Boolean := False;
      --  Debug Standby mode
      DBG_STANDBY   : Boolean := False;
      --  unspecified
      Reserved_3_3  : HAL.Bit := 16#0#;
      --  Trace pin assignment control
      TRACE_IOEN    : Boolean := False;
      --  trace port and clock enable
      TRACE_EN      : Boolean := False;
      --  Trace pin assignment control
      TRACE_MODE    : CR_TRACE_MODE_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      Reserved_0_0  at 0 range 0 .. 0;
      DBG_STOP      at 0 range 1 .. 1;
      DBG_STANDBY   at 0 range 2 .. 2;
      Reserved_3_3  at 0 range 3 .. 3;
      TRACE_IOEN    at 0 range 4 .. 4;
      TRACE_EN      at 0 range 5 .. 5;
      TRACE_MODE    at 0 range 6 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  Debug MCU APB1L peripheral freeze register
   type APB1LFZR_Register is record
      --  TIM2 stop in debug
      DBG_TIM2_STOP  : Boolean := False;
      --  TIM3 stop in debug
      DBG_TIM3_STOP  : Boolean := False;
      --  TIM4 stop in debug
      DBG_TIM4_STOP  : Boolean := False;
      --  TIM5 stop in debug
      DBG_TIM5_STOP  : Boolean := False;
      --  TIM6 stop in debug
      DBG_TIM6_STOP  : Boolean := False;
      --  TIM7 stop in debug
      DBG_TIM7_STOP  : Boolean := False;
      --  unspecified
      Reserved_6_10  : HAL.UInt5 := 16#0#;
      --  Window watchdog counter stop in debug
      DBG_WWDG_STOP  : Boolean := False;
      --  Independent watchdog counter stop in debug
      DBG_IWDG_STOP  : Boolean := False;
      --  unspecified
      Reserved_13_20 : HAL.UInt8 := 16#0#;
      --  I2C1 SMBUS timeout stop in debug
      DBG_I2C1_STOP  : Boolean := False;
      --  I2C2 SMBUS timeout stop in debug
      DBG_I2C2_STOP  : Boolean := False;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for APB1LFZR_Register use record
      DBG_TIM2_STOP  at 0 range 0 .. 0;
      DBG_TIM3_STOP  at 0 range 1 .. 1;
      DBG_TIM4_STOP  at 0 range 2 .. 2;
      DBG_TIM5_STOP  at 0 range 3 .. 3;
      DBG_TIM6_STOP  at 0 range 4 .. 4;
      DBG_TIM7_STOP  at 0 range 5 .. 5;
      Reserved_6_10  at 0 range 6 .. 10;
      DBG_WWDG_STOP  at 0 range 11 .. 11;
      DBG_IWDG_STOP  at 0 range 12 .. 12;
      Reserved_13_20 at 0 range 13 .. 20;
      DBG_I2C1_STOP  at 0 range 21 .. 21;
      DBG_I2C2_STOP  at 0 range 22 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  Debug MCU APB1H peripheral freeze register
   type APB1HFZR_Register is record
      --  unspecified
      Reserved_0_0    : HAL.Bit := 16#0#;
      --  I2C4 stop in debug
      DBG_I2C4_STOP   : Boolean := False;
      --  unspecified
      Reserved_2_4    : HAL.UInt3 := 16#0#;
      --  LPTIM2 stop in debug
      DBG_LPTIM2_STOP : Boolean := False;
      --  unspecified
      Reserved_6_31   : HAL.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for APB1HFZR_Register use record
      Reserved_0_0    at 0 range 0 .. 0;
      DBG_I2C4_STOP   at 0 range 1 .. 1;
      Reserved_2_4    at 0 range 2 .. 4;
      DBG_LPTIM2_STOP at 0 range 5 .. 5;
      Reserved_6_31   at 0 range 6 .. 31;
   end record;

   --  Debug MCU APB2 peripheral freeze register
   type APB2FZR_Register is record
      --  unspecified
      Reserved_0_10  : HAL.UInt11 := 16#0#;
      --  TIM1 counter stopped when core is halted
      DBG_TIM1_STOP  : Boolean := False;
      --  unspecified
      Reserved_12_12 : HAL.Bit := 16#0#;
      --  TIM8 stop in debug
      DBG_TIM8_STOP  : Boolean := False;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  TIM15 counter stopped when core is halted
      DBG_TIM15_STOP : Boolean := False;
      --  TIM16 counter stopped when core is halted
      DBG_TIM16_STOP : Boolean := False;
      --  DBG_TIM17_STOP
      DBG_TIM17_STOP : Boolean := False;
      --  unspecified
      Reserved_19_31 : HAL.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for APB2FZR_Register use record
      Reserved_0_10  at 0 range 0 .. 10;
      DBG_TIM1_STOP  at 0 range 11 .. 11;
      Reserved_12_12 at 0 range 12 .. 12;
      DBG_TIM8_STOP  at 0 range 13 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      DBG_TIM15_STOP at 0 range 16 .. 16;
      DBG_TIM16_STOP at 0 range 17 .. 17;
      DBG_TIM17_STOP at 0 range 18 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   --  Debug MCU APB3 peripheral freeze register
   type APB3FZR_Register is record
      --  unspecified
      Reserved_0_9    : HAL.UInt10 := 16#0#;
      --  I2C3 stop in debug
      DBG_I2C3_STOP   : Boolean := False;
      --  unspecified
      Reserved_11_16  : HAL.UInt6 := 16#0#;
      --  LPTIM1 stop in debug
      DBG_LPTIM1_STOP : Boolean := False;
      --  LPTIM3 stop in debug
      DBG_LPTIM3_STOP : Boolean := False;
      --  LPTIM4 stop in debug
      DBG_LPTIM4_STOP : Boolean := False;
      --  unspecified
      Reserved_20_29  : HAL.UInt10 := 16#0#;
      --  RTC stop in debug
      DBG_RTC_STOP    : Boolean := False;
      --  unspecified
      Reserved_31_31  : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for APB3FZR_Register use record
      Reserved_0_9    at 0 range 0 .. 9;
      DBG_I2C3_STOP   at 0 range 10 .. 10;
      Reserved_11_16  at 0 range 11 .. 16;
      DBG_LPTIM1_STOP at 0 range 17 .. 17;
      DBG_LPTIM3_STOP at 0 range 18 .. 18;
      DBG_LPTIM4_STOP at 0 range 19 .. 19;
      Reserved_20_29  at 0 range 20 .. 29;
      DBG_RTC_STOP    at 0 range 30 .. 30;
      Reserved_31_31  at 0 range 31 .. 31;
   end record;

   --  Debug MCU AHB1 peripheral freeze register
   type AHB1FZR_Register is record
      --  GPDMA channel 0 stop in debug
      DBG_GPDMA0_STOP  : Boolean := False;
      --  GPDMA channel 1 stop in debug
      DBG_GPDMA1_STOP  : Boolean := False;
      --  GPDMA channel 2 stop in debug
      DBG_GPDMA2_STOP  : Boolean := False;
      --  GPDMA channel 3 stop in debug
      DBG_GPDMA3_STOP  : Boolean := False;
      --  GPDMA channel 4 stop in debug
      DBG_GPDMA4_STOP  : Boolean := False;
      --  GPDMA channel 5 stop in debug
      DBG_GPDMA5_STOP  : Boolean := False;
      --  GPDMA channel 6 stop in debug
      DBG_GPDMA6_STOP  : Boolean := False;
      --  GPDMA channel 7 stop in debug
      DBG_GPDMA7_STOP  : Boolean := False;
      --  GPDMA channel 8 stop in debug
      DBG_GPDMA8_STOP  : Boolean := False;
      --  GPDMA channel 9 stop in debug
      DBG_GPDMA9_STOP  : Boolean := False;
      --  GPDMA channel 10 stop in debug
      DBG_GPDMA10_STOP : Boolean := False;
      --  GPDMA channel 11 stop in debug
      DBG_GPDMA11_STOP : Boolean := False;
      --  GPDMA channel 12 stop in debug
      DBG_GPDMA12_STOP : Boolean := False;
      --  GPDMA channel 13 stop in debug
      DBG_GPDMA13_STOP : Boolean := False;
      --  GPDMA channel 14 stop in debug
      DBG_GPDMA14_STOP : Boolean := False;
      --  GPDMA channel 15 stop in debug
      DBG_GPDMA15_STOP : Boolean := False;
      --  unspecified
      Reserved_16_31   : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AHB1FZR_Register use record
      DBG_GPDMA0_STOP  at 0 range 0 .. 0;
      DBG_GPDMA1_STOP  at 0 range 1 .. 1;
      DBG_GPDMA2_STOP  at 0 range 2 .. 2;
      DBG_GPDMA3_STOP  at 0 range 3 .. 3;
      DBG_GPDMA4_STOP  at 0 range 4 .. 4;
      DBG_GPDMA5_STOP  at 0 range 5 .. 5;
      DBG_GPDMA6_STOP  at 0 range 6 .. 6;
      DBG_GPDMA7_STOP  at 0 range 7 .. 7;
      DBG_GPDMA8_STOP  at 0 range 8 .. 8;
      DBG_GPDMA9_STOP  at 0 range 9 .. 9;
      DBG_GPDMA10_STOP at 0 range 10 .. 10;
      DBG_GPDMA11_STOP at 0 range 11 .. 11;
      DBG_GPDMA12_STOP at 0 range 12 .. 12;
      DBG_GPDMA13_STOP at 0 range 13 .. 13;
      DBG_GPDMA14_STOP at 0 range 14 .. 14;
      DBG_GPDMA15_STOP at 0 range 15 .. 15;
      Reserved_16_31   at 0 range 16 .. 31;
   end record;

   --  Debug MCU AHB3 peripheral freeze register
   type AHB3FZR_Register is record
      --  LPDMA channel 0 stop in debug
      DBG_LPDMA0_STOP : Boolean := False;
      --  LPDMA channel 1 stop in debug
      DBG_LPDMA1_STOP : Boolean := False;
      --  LPDMA channel 2 stop in debug
      DBG_LPDMA2_STOP : Boolean := False;
      --  LPDMA channel 3 stop in debug
      DBG_LPDMA3_STOP : Boolean := False;
      --  unspecified
      Reserved_4_31   : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AHB3FZR_Register use record
      DBG_LPDMA0_STOP at 0 range 0 .. 0;
      DBG_LPDMA1_STOP at 0 range 1 .. 1;
      DBG_LPDMA2_STOP at 0 range 2 .. 2;
      DBG_LPDMA3_STOP at 0 range 3 .. 3;
      Reserved_4_31   at 0 range 4 .. 31;
   end record;

   subtype DBGMCU_SR_AP_PRESENT_Field is HAL.UInt8;
   subtype DBGMCU_SR_AP_LOCKED_Field is HAL.UInt8;

   --  DBGMCU status register
   type DBGMCU_SR_Register is record
      --  Read-only. Bit n identifies whether access port AP n is present in
      --  device 	Bit n = 0: APn absent 	Bit n = 1: APn present
      AP_PRESENT     : DBGMCU_SR_AP_PRESENT_Field;
      --  Read-only. DECLARATION TO BE CONFIRMED by PRODUCT OWNER! Bit n
      --  identifies whether access port AP n is open (can be accessed via the
      --  debug port) or locked (debug access to the AP is blocked) 	Bit
      --  n = 0: APn locked 	Bit n = 1: APn enabled
      AP_LOCKED      : DBGMCU_SR_AP_LOCKED_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DBGMCU_SR_Register use record
      AP_PRESENT     at 0 range 0 .. 7;
      AP_LOCKED      at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype PIDR4_JEP106CON_Field is HAL.UInt4;
   subtype PIDR4_KCOUNT_4_Field is HAL.UInt4;

   --  Debug MCU CoreSight peripheral identity register 4
   type PIDR4_Register is record
      --  Read-only. JEP106 continuation code
      JEP106CON     : PIDR4_JEP106CON_Field;
      --  Read-only. register file size
      KCOUNT_4      : PIDR4_KCOUNT_4_Field;
      --  unspecified
      Reserved_8_31 : HAL.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIDR4_Register use record
      JEP106CON     at 0 range 0 .. 3;
      KCOUNT_4      at 0 range 4 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype PIDR0_PARTNUM_Field is HAL.UInt8;

   --  Debug MCU CoreSight peripheral identity register 0
   type PIDR0_Register is record
      --  Read-only. part number bits [7:0]
      PARTNUM       : PIDR0_PARTNUM_Field;
      --  unspecified
      Reserved_8_31 : HAL.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIDR0_Register use record
      PARTNUM       at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype PIDR1_PARTNUM_Field is HAL.UInt4;
   subtype PIDR1_JEP106ID_Field is HAL.UInt4;

   --  Debug MCU CoreSight peripheral identity register 1
   type PIDR1_Register is record
      --  Read-only. part number bits [11:8]
      PARTNUM       : PIDR1_PARTNUM_Field;
      --  Read-only. JEP106 identity code bits [3:0]
      JEP106ID      : PIDR1_JEP106ID_Field;
      --  unspecified
      Reserved_8_31 : HAL.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIDR1_Register use record
      PARTNUM       at 0 range 0 .. 3;
      JEP106ID      at 0 range 4 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype PIDR2_JEP106ID_Field is HAL.UInt3;
   subtype PIDR2_REVISION_Field is HAL.UInt4;

   --  Debug MCU CoreSight peripheral identity register 2
   type PIDR2_Register is record
      --  Read-only. JEP106 identity code bits [6:4]
      JEP106ID      : PIDR2_JEP106ID_Field;
      --  Read-only. JEDEC assigned value
      JEDEC         : Boolean;
      --  Read-only. component revision number
      REVISION      : PIDR2_REVISION_Field;
      --  unspecified
      Reserved_8_31 : HAL.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIDR2_Register use record
      JEP106ID      at 0 range 0 .. 2;
      JEDEC         at 0 range 3 .. 3;
      REVISION      at 0 range 4 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype PIDR3_CMOD_Field is HAL.UInt4;
   subtype PIDR3_REVAND_Field is HAL.UInt4;

   --  Debug MCU CoreSight peripheral identity register 3
   type PIDR3_Register is record
      --  Read-only. customer modified
      CMOD          : PIDR3_CMOD_Field;
      --  Read-only. metal fix version
      REVAND        : PIDR3_REVAND_Field;
      --  unspecified
      Reserved_8_31 : HAL.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIDR3_Register use record
      CMOD          at 0 range 0 .. 3;
      REVAND        at 0 range 4 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype CIDR_PREAMBLE_Field is HAL.UInt8;

   --  Debug MCU CoreSight component identity register 0
   type CIDR_Register is record
      --  Read-only. component identification bits [7:0]
      PREAMBLE      : CIDR_PREAMBLE_Field;
      --  unspecified
      Reserved_8_31 : HAL.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CIDR_Register use record
      PREAMBLE      at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype CIDR1_PREAMBLE_Field is HAL.UInt4;
   subtype CIDR1_CLASS_Field is HAL.UInt4;

   --  Debug MCU CoreSight component identity register 1
   type CIDR1_Register is record
      --  Read-only. component identification bits [11:8]
      PREAMBLE      : CIDR1_PREAMBLE_Field;
      --  Read-only. component identification bits [15:12] - component class
      CLASS         : CIDR1_CLASS_Field;
      --  unspecified
      Reserved_8_31 : HAL.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CIDR1_Register use record
      PREAMBLE      at 0 range 0 .. 3;
      CLASS         at 0 range 4 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  MCU debug component
   type DBGMCU_Peripheral is record
      --  DBGMCU_IDCODE
      IDCODE                 : aliased IDCODE_Register;
      --  Debug MCU configuration register
      CR                     : aliased CR_Register;
      --  Debug MCU APB1L peripheral freeze register
      APB1LFZR               : aliased APB1LFZR_Register;
      --  Debug MCU APB1H peripheral freeze register
      APB1HFZR               : aliased APB1HFZR_Register;
      --  Debug MCU APB2 peripheral freeze register
      APB2FZR                : aliased APB2FZR_Register;
      --  Debug MCU APB3 peripheral freeze register
      APB3FZR                : aliased APB3FZR_Register;
      --  Debug MCU AHB1 peripheral freeze register
      AHB1FZR                : aliased AHB1FZR_Register;
      --  Debug MCU AHB3 peripheral freeze register
      AHB3FZR                : aliased AHB3FZR_Register;
      --  DBGMCU status register
      DBGMCU_SR              : aliased DBGMCU_SR_Register;
      --  DBGMCU debug host authentication register
      DBGMCU_DBG_AUTH_HOST   : aliased HAL.UInt32;
      --  DBGMCU debug device authentication register
      DBGMCU_DBG_AUTH_DEVICE : aliased HAL.UInt32;
      --  Debug MCU CoreSight peripheral identity register 4
      PIDR4                  : aliased PIDR4_Register;
      --  Debug MCU CoreSight peripheral identity register 0
      PIDR0                  : aliased PIDR0_Register;
      --  Debug MCU CoreSight peripheral identity register 1
      PIDR1                  : aliased PIDR1_Register;
      --  Debug MCU CoreSight peripheral identity register 2
      PIDR2                  : aliased PIDR2_Register;
      --  Debug MCU CoreSight peripheral identity register 3
      PIDR3                  : aliased PIDR3_Register;
      --  Debug MCU CoreSight component identity register 0
      CIDR0                  : aliased CIDR_Register;
      --  Debug MCU CoreSight component identity register 1
      CIDR1                  : aliased CIDR1_Register;
      --  Debug MCU CoreSight component identity register 2
      CIDR2                  : aliased CIDR_Register;
      --  Debug MCU CoreSight component identity register 3
      CIDR3                  : aliased CIDR_Register;
   end record
     with Volatile;

   for DBGMCU_Peripheral use record
      IDCODE                 at 16#0# range 0 .. 31;
      CR                     at 16#4# range 0 .. 31;
      APB1LFZR               at 16#8# range 0 .. 31;
      APB1HFZR               at 16#C# range 0 .. 31;
      APB2FZR                at 16#10# range 0 .. 31;
      APB3FZR                at 16#14# range 0 .. 31;
      AHB1FZR                at 16#20# range 0 .. 31;
      AHB3FZR                at 16#28# range 0 .. 31;
      DBGMCU_SR              at 16#FC# range 0 .. 31;
      DBGMCU_DBG_AUTH_HOST   at 16#100# range 0 .. 31;
      DBGMCU_DBG_AUTH_DEVICE at 16#104# range 0 .. 31;
      PIDR4                  at 16#FD0# range 0 .. 31;
      PIDR0                  at 16#FE0# range 0 .. 31;
      PIDR1                  at 16#FE4# range 0 .. 31;
      PIDR2                  at 16#FE8# range 0 .. 31;
      PIDR3                  at 16#FEC# range 0 .. 31;
      CIDR0                  at 16#FF0# range 0 .. 31;
      CIDR1                  at 16#FF4# range 0 .. 31;
      CIDR2                  at 16#FF8# range 0 .. 31;
      CIDR3                  at 16#FFC# range 0 .. 31;
   end record;

   --  MCU debug component
   DBGMCU_Periph : aliased DBGMCU_Peripheral
     with Import, Address => DBGMCU_Base;

end STM32_SVD.DBGMCU;
