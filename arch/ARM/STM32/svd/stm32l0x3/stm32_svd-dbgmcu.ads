--  This spec has been automatically generated from STM32L0x3.svd

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

   --  MCU Device ID Code Register
   type IDCODE_Register is record
      --  Read-only. Device Identifier
      DEV_ID         : IDCODE_DEV_ID_Field;
      --  unspecified
      Reserved_12_15 : HAL.UInt4;
      --  Read-only. Revision Identifier
      REV_ID         : IDCODE_REV_ID_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IDCODE_Register use record
      DEV_ID         at 0 range 0 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      REV_ID         at 0 range 16 .. 31;
   end record;

   --  Debug MCU Configuration Register
   type CR_Register is record
      --  Debug Sleep Mode
      DBG_SLEEP     : Boolean := False;
      --  Debug Stop Mode
      DBG_STOP      : Boolean := False;
      --  Debug Standby Mode
      DBG_STANDBY   : Boolean := False;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      DBG_SLEEP     at 0 range 0 .. 0;
      DBG_STOP      at 0 range 1 .. 1;
      DBG_STANDBY   at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  APB Low Freeze Register
   type APB1_FZ_Register is record
      --  Debug Timer 2 stopped when Core is halted
      DBG_TIMER2_STOP  : Boolean := False;
      --  unspecified
      Reserved_1_3     : HAL.UInt3 := 16#0#;
      --  Debug Timer 6 stopped when Core is halted
      DBG_TIMER6_STOP  : Boolean := False;
      --  unspecified
      Reserved_5_9     : HAL.UInt5 := 16#0#;
      --  Debug RTC stopped when Core is halted
      DBG_RTC_STOP     : Boolean := False;
      --  Debug Window Wachdog stopped when Core is halted
      DBG_WWDG_STOP    : Boolean := False;
      --  Debug Independent Wachdog stopped when Core is halted
      DBG_IWDG_STOP    : Boolean := False;
      --  unspecified
      Reserved_13_20   : HAL.UInt8 := 16#0#;
      --  I2C1 SMBUS timeout mode stopped when core is halted
      DBG_I2C1_STOP    : Boolean := False;
      --  I2C2 SMBUS timeout mode stopped when core is halted
      DBG_I2C2_STOP    : Boolean := False;
      --  unspecified
      Reserved_23_30   : HAL.UInt8 := 16#0#;
      --  LPTIM1 counter stopped when core is halted
      DBG_LPTIMER_STOP : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for APB1_FZ_Register use record
      DBG_TIMER2_STOP  at 0 range 0 .. 0;
      Reserved_1_3     at 0 range 1 .. 3;
      DBG_TIMER6_STOP  at 0 range 4 .. 4;
      Reserved_5_9     at 0 range 5 .. 9;
      DBG_RTC_STOP     at 0 range 10 .. 10;
      DBG_WWDG_STOP    at 0 range 11 .. 11;
      DBG_IWDG_STOP    at 0 range 12 .. 12;
      Reserved_13_20   at 0 range 13 .. 20;
      DBG_I2C1_STOP    at 0 range 21 .. 21;
      DBG_I2C2_STOP    at 0 range 22 .. 22;
      Reserved_23_30   at 0 range 23 .. 30;
      DBG_LPTIMER_STOP at 0 range 31 .. 31;
   end record;

   --  APB High Freeze Register
   type APB2_FZ_Register is record
      --  unspecified
      Reserved_0_1     : HAL.UInt2 := 16#0#;
      --  Debug Timer 21 stopped when Core is halted
      DBG_TIMER21_STOP : Boolean := False;
      --  unspecified
      Reserved_3_5     : HAL.UInt3 := 16#0#;
      --  Debug Timer 22 stopped when Core is halted
      DBG_TIMER22_STO  : Boolean := False;
      --  unspecified
      Reserved_7_31    : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for APB2_FZ_Register use record
      Reserved_0_1     at 0 range 0 .. 1;
      DBG_TIMER21_STOP at 0 range 2 .. 2;
      Reserved_3_5     at 0 range 3 .. 5;
      DBG_TIMER22_STO  at 0 range 6 .. 6;
      Reserved_7_31    at 0 range 7 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Debug support
   type DBGMCU_Peripheral is record
      --  MCU Device ID Code Register
      IDCODE  : aliased IDCODE_Register;
      --  Debug MCU Configuration Register
      CR      : aliased CR_Register;
      --  APB Low Freeze Register
      APB1_FZ : aliased APB1_FZ_Register;
      --  APB High Freeze Register
      APB2_FZ : aliased APB2_FZ_Register;
   end record
     with Volatile;

   for DBGMCU_Peripheral use record
      IDCODE  at 16#0# range 0 .. 31;
      CR      at 16#4# range 0 .. 31;
      APB1_FZ at 16#8# range 0 .. 31;
      APB2_FZ at 16#C# range 0 .. 31;
   end record;

   --  Debug support
   DBGMCU_Periph : aliased DBGMCU_Peripheral
     with Import, Address => System'To_Address (16#40015800#);

end STM32_SVD.DBGMCU;
