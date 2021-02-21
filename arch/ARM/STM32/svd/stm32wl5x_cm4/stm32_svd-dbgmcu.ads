--  This spec has been automatically generated from STM32WL5x_CM4.svd

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

   subtype IDCODER_DEV_ID_Field is HAL.UInt12;
   subtype IDCODER_REV_ID_Field is HAL.UInt16;

   --  DBGMCU Identity Code Register
   type IDCODER_Register is record
      --  Read-only. Device ID
      DEV_ID         : IDCODER_DEV_ID_Field;
      --  unspecified
      Reserved_12_15 : HAL.UInt4;
      --  Read-only. Revision
      REV_ID         : IDCODER_REV_ID_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IDCODER_Register use record
      DEV_ID         at 0 range 0 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      REV_ID         at 0 range 16 .. 31;
   end record;

   --  DBGMCU Configuration Register
   type CR_Register is record
      --  Allow debug in SLEEP mode
      DBG_SLEEP     : Boolean := False;
      --  Allow debug in STOP mode
      DBG_STOP      : Boolean := False;
      --  Allow debug in STANDBY mode
      DBG_STANDBY   : Boolean := False;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      DBG_SLEEP     at 0 range 0 .. 0;
      DBG_STOP      at 0 range 1 .. 1;
      DBG_STANDBY   at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  DBGMCU CPU1 APB1 Peripheral Freeze Register 1
   type APB1FZR1_Register is record
      --  TIM2 stop in CPU1 debug
      DBG_TIM2_STOP   : Boolean := False;
      --  unspecified
      Reserved_1_9    : HAL.UInt9 := 16#0#;
      --  RTC stop in CPU1 debug
      DBG_RTC_STOP    : Boolean := False;
      --  WWDG stop in CPU1 debug
      DBG_WWDG_STOP   : Boolean := False;
      --  IWDG stop in CPU1 debug
      DBG_IWDG_STOP   : Boolean := False;
      --  unspecified
      Reserved_13_20  : HAL.UInt8 := 16#0#;
      --  I2C1 SMBUS timeout stop in CPU1 debug
      DBG_I2C1_STOP   : Boolean := False;
      --  I2C2 SMBUS timeout stop in CPU1 debug
      DBG_I2C2_STOP   : Boolean := False;
      --  I2C3 SMBUS timeout stop in CPU1 debug
      DBG_I2C3_STOP   : Boolean := False;
      --  unspecified
      Reserved_24_30  : HAL.UInt7 := 16#0#;
      --  LPTIM1 stop in CPU1 debug
      DBG_LPTIM1_STOP : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for APB1FZR1_Register use record
      DBG_TIM2_STOP   at 0 range 0 .. 0;
      Reserved_1_9    at 0 range 1 .. 9;
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

   --  DBGMCU CPU2 APB1 Peripheral Freeze Register 1 [dual core device
   type C2APB1FZR1_Register is record
      --  DBG_TIM2_STOP
      DBG_TIM2_STOP   : Boolean := False;
      --  unspecified
      Reserved_1_9    : HAL.UInt9 := 16#0#;
      --  DBG_RTC_STOP
      DBG_RTC_STOP    : Boolean := False;
      --  unspecified
      Reserved_11_11  : HAL.Bit := 16#0#;
      --  DBG_IWDG_STOP
      DBG_IWDG_STOP   : Boolean := False;
      --  unspecified
      Reserved_13_20  : HAL.UInt8 := 16#0#;
      --  DBG_I2C1_STOP
      DBG_I2C1_STOP   : Boolean := False;
      --  DBG_I2C2_STOP
      DBG_I2C2_STOP   : Boolean := False;
      --  DBG_I2C3_STOP
      DBG_I2C3_STOP   : Boolean := False;
      --  unspecified
      Reserved_24_30  : HAL.UInt7 := 16#0#;
      --  DBG_LPTIM1_STOP
      DBG_LPTIM1_STOP : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C2APB1FZR1_Register use record
      DBG_TIM2_STOP   at 0 range 0 .. 0;
      Reserved_1_9    at 0 range 1 .. 9;
      DBG_RTC_STOP    at 0 range 10 .. 10;
      Reserved_11_11  at 0 range 11 .. 11;
      DBG_IWDG_STOP   at 0 range 12 .. 12;
      Reserved_13_20  at 0 range 13 .. 20;
      DBG_I2C1_STOP   at 0 range 21 .. 21;
      DBG_I2C2_STOP   at 0 range 22 .. 22;
      DBG_I2C3_STOP   at 0 range 23 .. 23;
      Reserved_24_30  at 0 range 24 .. 30;
      DBG_LPTIM1_STOP at 0 range 31 .. 31;
   end record;

   --  DBGMCU CPU1 APB1 Peripheral Freeze Register 2
   type APB1FZR2_Register is record
      --  unspecified
      Reserved_0_4    : HAL.UInt5 := 16#0#;
      --  DBG_LPTIM2_STOP
      DBG_LPTIM2_STOP : Boolean := False;
      --  DBG_LPTIM3_STOP
      DBG_LPTIM3_STOP : Boolean := False;
      --  unspecified
      Reserved_7_31   : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for APB1FZR2_Register use record
      Reserved_0_4    at 0 range 0 .. 4;
      DBG_LPTIM2_STOP at 0 range 5 .. 5;
      DBG_LPTIM3_STOP at 0 range 6 .. 6;
      Reserved_7_31   at 0 range 7 .. 31;
   end record;

   --  DBGMCU CPU2 APB1 Peripheral Freeze Register 2 [dual core device
   type C2APB1FZR2_Register is record
      --  unspecified
      Reserved_0_4    : HAL.UInt5 := 16#0#;
      --  DBG_LPTIM2_STOP
      DBG_LPTIM2_STOP : Boolean := False;
      --  DBG_LPTIM3_STOP
      DBG_LPTIM3_STOP : Boolean := False;
      --  unspecified
      Reserved_7_31   : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C2APB1FZR2_Register use record
      Reserved_0_4    at 0 range 0 .. 4;
      DBG_LPTIM2_STOP at 0 range 5 .. 5;
      DBG_LPTIM3_STOP at 0 range 6 .. 6;
      Reserved_7_31   at 0 range 7 .. 31;
   end record;

   --  DBGMCU CPU1 APB2 Peripheral Freeze Register
   type APB2FZR_Register is record
      --  unspecified
      Reserved_0_10  : HAL.UInt11 := 16#0#;
      --  DBG_TIM1_STOP
      DBG_TIM1_STOP  : Boolean := False;
      --  unspecified
      Reserved_12_16 : HAL.UInt5 := 16#0#;
      --  DBG_TIM16_STOP
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
      Reserved_12_16 at 0 range 12 .. 16;
      DBG_TIM16_STOP at 0 range 17 .. 17;
      DBG_TIM17_STOP at 0 range 18 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   --  DBGMCU CPU2 APB2 Peripheral Freeze Register [dual core device
   type C2APB2FZR_Register is record
      --  unspecified
      Reserved_0_10  : HAL.UInt11 := 16#0#;
      --  DBG_TIM1_STOP
      DBG_TIM1_STOP  : Boolean := False;
      --  unspecified
      Reserved_12_16 : HAL.UInt5 := 16#0#;
      --  DBG_TIM16_STOP
      DBG_TIM16_STOP : Boolean := False;
      --  DBG_TIM17_STOP
      DBG_TIM17_STOP : Boolean := False;
      --  unspecified
      Reserved_19_31 : HAL.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C2APB2FZR_Register use record
      Reserved_0_10  at 0 range 0 .. 10;
      DBG_TIM1_STOP  at 0 range 11 .. 11;
      Reserved_12_16 at 0 range 12 .. 16;
      DBG_TIM16_STOP at 0 range 17 .. 17;
      DBG_TIM17_STOP at 0 range 18 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Microcontroller Debug Unit
   type DBGMCU_Peripheral is record
      --  DBGMCU Identity Code Register
      IDCODER    : aliased IDCODER_Register;
      --  DBGMCU Configuration Register
      CR         : aliased CR_Register;
      --  DBGMCU CPU1 APB1 Peripheral Freeze Register 1
      APB1FZR1   : aliased APB1FZR1_Register;
      --  DBGMCU CPU2 APB1 Peripheral Freeze Register 1 [dual core device
      C2APB1FZR1 : aliased C2APB1FZR1_Register;
      --  DBGMCU CPU1 APB1 Peripheral Freeze Register 2
      APB1FZR2   : aliased APB1FZR2_Register;
      --  DBGMCU CPU2 APB1 Peripheral Freeze Register 2 [dual core device
      C2APB1FZR2 : aliased C2APB1FZR2_Register;
      --  DBGMCU CPU1 APB2 Peripheral Freeze Register
      APB2FZR    : aliased APB2FZR_Register;
      --  DBGMCU CPU2 APB2 Peripheral Freeze Register [dual core device
      C2APB2FZR  : aliased C2APB2FZR_Register;
   end record
     with Volatile;

   for DBGMCU_Peripheral use record
      IDCODER    at 16#0# range 0 .. 31;
      CR         at 16#4# range 0 .. 31;
      APB1FZR1   at 16#3C# range 0 .. 31;
      C2APB1FZR1 at 16#40# range 0 .. 31;
      APB1FZR2   at 16#44# range 0 .. 31;
      C2APB1FZR2 at 16#48# range 0 .. 31;
      APB2FZR    at 16#4C# range 0 .. 31;
      C2APB2FZR  at 16#50# range 0 .. 31;
   end record;

   --  Microcontroller Debug Unit
   DBGMCU_Periph : aliased DBGMCU_Peripheral
     with Import, Address => DBGMCU_Base;

end STM32_SVD.DBGMCU;
