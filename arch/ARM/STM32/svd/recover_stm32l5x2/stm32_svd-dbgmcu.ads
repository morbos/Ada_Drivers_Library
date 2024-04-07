--  This spec has been automatically generated from xyzzy

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
      --  Read-only. Device identifier
      DEV_ID         : IDCODE_DEV_ID_Field;
      --  unspecified
      Reserved_12_15 : HAL.UInt4;
      --  Read-only. Revision identifie
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

   --  Debug MCU APB1 freeze register1
   type APB1LFZR_Register is record
      --  TIM2 counter stopped when core is halted
      DBG_TIM2_STOP   : Boolean := False;
      --  TIM3 stop in debug
      DBG_TIM3_STOP   : Boolean := False;
      --  TIM4 stop in debug
      DBG_TIM4_STOP   : Boolean := False;
      --  TIM5 stop in debug
      DBG_TIM5_STOP   : Boolean := False;
      --  TIM6 counter stopped when core is halted
      DBG_TIM6_STOP   : Boolean := False;
      --  TIM7 counter stopped when core is halted
      DBG_TIM7_STOP   : Boolean := False;
      --  unspecified
      Reserved_6_9    : HAL.UInt4 := 16#0#;
      --  RTC counter stopped when core is halted
      DBG_RTC_STOP    : Boolean := False;
      --  Window watchdog counter stopped when core is halted
      DBG_WWDG_STOP   : Boolean := False;
      --  Independent watchdog counter stopped when core is halted
      DBG_IWDG_STOP   : Boolean := False;
      --  unspecified
      Reserved_13_20  : HAL.UInt8 := 16#0#;
      --  I2C1 SMBUS timeout counter stopped when core is halted
      DBG_I2C1_STOP   : Boolean := False;
      --  I2C2 SMBUS timeout counter stopped when core is halted
      DBG_I2C2_STOP   : Boolean := False;
      --  I2C3 SMBUS timeout counter stopped when core is halted
      DBG_I2C3_STOP   : Boolean := False;
      --  unspecified
      Reserved_24_30  : HAL.UInt7 := 16#0#;
      --  LPTIM1 counter stopped when core is halted
      DBG_LPTIM1_STOP : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for APB1LFZR_Register use record
      DBG_TIM2_STOP   at 0 range 0 .. 0;
      DBG_TIM3_STOP   at 0 range 1 .. 1;
      DBG_TIM4_STOP   at 0 range 2 .. 2;
      DBG_TIM5_STOP   at 0 range 3 .. 3;
      DBG_TIM6_STOP   at 0 range 4 .. 4;
      DBG_TIM7_STOP   at 0 range 5 .. 5;
      Reserved_6_9    at 0 range 6 .. 9;
      DBG_RTC_STOP    at 0 range 10 .. 10;
      DBG_WWDG_STOP   at 0 range 11 .. 11;
      DBG_IWDG_STOP   at 0 range 12 .. 12;
      Reserved_13_20  at 0 range 13 .. 20;
      DBG_I2C1_STOP   at 0 range 21 .. 21;
      DBG_I2C2_STOP   at 0 range 22 .. 22;
      DBG_I2C3_STOP   at 0 range 23 .. 23;
      Reserved_24_30  at 0 range 24 .. 30;
      DBG_LPTIM1_STOP at 0 range 31 .. 31;
   end record;

   --  Debug MCU APB1 freeze register 2
   type APB1HFZR_Register is record
      --  unspecified
      Reserved_0_0    : HAL.Bit := 16#0#;
      --  I2C4 stop in debug
      DBG_I2C4_STOP   : Boolean := False;
      --  unspecified
      Reserved_2_4    : HAL.UInt3 := 16#0#;
      --  LPTIM2 counter stopped when core is halted
      DBG_LPTIM2_STOP : Boolean := False;
      --  LPTIM3 stop in debug
      DBG_LPTIM3_STOP : Boolean := False;
      --  unspecified
      Reserved_7_31   : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for APB1HFZR_Register use record
      Reserved_0_0    at 0 range 0 .. 0;
      DBG_I2C4_STOP   at 0 range 1 .. 1;
      Reserved_2_4    at 0 range 2 .. 4;
      DBG_LPTIM2_STOP at 0 range 5 .. 5;
      DBG_LPTIM3_STOP at 0 range 6 .. 6;
      Reserved_7_31   at 0 range 7 .. 31;
   end record;

   --  Debug MCU APB2 freeze register
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

   -----------------
   -- Peripherals --
   -----------------

   --  MCU debug component
   type DBGMCU_Peripheral is record
      --  DBGMCU_IDCODE
      IDCODE   : aliased IDCODE_Register;
      --  Debug MCU configuration register
      CR       : aliased CR_Register;
      --  Debug MCU APB1 freeze register1
      APB1LFZR : aliased APB1LFZR_Register;
      --  Debug MCU APB1 freeze register 2
      APB1HFZR : aliased APB1HFZR_Register;
      --  Debug MCU APB2 freeze register
      APB2FZR  : aliased APB2FZR_Register;
   end record
     with Volatile;

   for DBGMCU_Peripheral use record
      IDCODE   at 16#0# range 0 .. 31;
      CR       at 16#4# range 0 .. 31;
      APB1LFZR at 16#8# range 0 .. 31;
      APB1HFZR at 16#C# range 0 .. 31;
      APB2FZR  at 16#10# range 0 .. 31;
   end record;

   --  MCU debug component
   DBGMCU_Periph : aliased DBGMCU_Peripheral
     with Import, Address => DBGMCU_Base;

end STM32_SVD.DBGMCU;
