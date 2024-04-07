--  This spec has been automatically generated from STM32U073.svd

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

   subtype DBGMCU_IDCODE_DEV_ID_Field is HAL.UInt12;
   subtype DBGMCU_IDCODE_REV_ID_Field is HAL.UInt16;

   --  DBGMCU device ID code register
   type DBGMCU_IDCODE_Register is record
      --  Read-only. Device identifier This field indicates the device ID.
      DEV_ID         : DBGMCU_IDCODE_DEV_ID_Field;
      --  unspecified
      Reserved_12_15 : HAL.UInt4;
      --  Read-only. Revision identifier This field indicates the revision of
      --  the device.
      REV_ID         : DBGMCU_IDCODE_REV_ID_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DBGMCU_IDCODE_Register use record
      DEV_ID         at 0 range 0 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      REV_ID         at 0 range 16 .. 31;
   end record;

   --  DBGMCU configuration register
   type DBGMCU_CR_Register is record
      --  unspecified
      Reserved_0_0  : HAL.Bit := 16#0#;
      --  Debug Stop mode Debug options in Stop mode.
      DBG_STOP      : Boolean := False;
      --  Debug Standby and Shutdown modes Debug options in Standby or Shutdown
      --  mode.
      DBG_STANDBY   : Boolean := False;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DBGMCU_CR_Register use record
      Reserved_0_0  at 0 range 0 .. 0;
      DBG_STOP      at 0 range 1 .. 1;
      DBG_STANDBY   at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  DBGMCU APB1 freeze register
   type DBGMCU_APB1FZR_Register is record
      --  TIM2 stop in debug
      DBG_TIM2_STOP   : Boolean := False;
      --  TIM3 stop in debug
      DBG_TIM3_STOP   : Boolean := False;
      --  TIM4 stop in debug
      DBG_TIM4_STOP   : Boolean := False;
      --  unspecified
      Reserved_3_3    : HAL.Bit := 16#0#;
      --  TIM6 stop in debug
      DBG_TIM6_STOP   : Boolean := False;
      --  TIM7 stop in debug
      DBG_TIM7_STOP   : Boolean := False;
      --  unspecified
      Reserved_6_9    : HAL.UInt4 := 16#0#;
      --  RTC stop in debug
      DBG_RTC_STOP    : Boolean := False;
      --  WWDG stop in debug
      DBG_WWDG_STOP   : Boolean := False;
      --  IWDG stop in debug
      DBG_IWDG_STOP   : Boolean := False;
      --  unspecified
      Reserved_13_20  : HAL.UInt8 := 16#0#;
      --  I2C3 SMBUS timeout stop in debug
      DBG_I2C3_STOP   : Boolean := False;
      --  I2C1 SMBUS timeout stop in debug
      DBG_I2C1_STOP   : Boolean := False;
      --  unspecified
      Reserved_23_29  : HAL.UInt7 := 16#0#;
      --  LPTIM2 stop in debug
      DBG_LPTIM2_STOP : Boolean := False;
      --  LPTIM1 stop in debug
      DBG_LPTIM1_STOP : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DBGMCU_APB1FZR_Register use record
      DBG_TIM2_STOP   at 0 range 0 .. 0;
      DBG_TIM3_STOP   at 0 range 1 .. 1;
      DBG_TIM4_STOP   at 0 range 2 .. 2;
      Reserved_3_3    at 0 range 3 .. 3;
      DBG_TIM6_STOP   at 0 range 4 .. 4;
      DBG_TIM7_STOP   at 0 range 5 .. 5;
      Reserved_6_9    at 0 range 6 .. 9;
      DBG_RTC_STOP    at 0 range 10 .. 10;
      DBG_WWDG_STOP   at 0 range 11 .. 11;
      DBG_IWDG_STOP   at 0 range 12 .. 12;
      Reserved_13_20  at 0 range 13 .. 20;
      DBG_I2C3_STOP   at 0 range 21 .. 21;
      DBG_I2C1_STOP   at 0 range 22 .. 22;
      Reserved_23_29  at 0 range 23 .. 29;
      DBG_LPTIM2_STOP at 0 range 30 .. 30;
      DBG_LPTIM1_STOP at 0 range 31 .. 31;
   end record;

   --  DBG APB2 freeze register
   type DBGMCU_APB2FZR_Register is record
      --  unspecified
      Reserved_0_10   : HAL.UInt11 := 16#0#;
      --  TIM1 stop in debug
      DBG_TIM1_STOP   : Boolean := False;
      --  unspecified
      Reserved_12_14  : HAL.UInt3 := 16#0#;
      --  TIM14 stop in debug
      DBG_TIM14_STOP  : Boolean := False;
      --  TIM15 stop in debug
      DBG_TIM15_STOP  : Boolean := False;
      --  TIM16 stop in debug
      DBG_TIM16_STOP  : Boolean := False;
      --  LPTIM3 stop in debug
      DBG_LPTIM3_STOP : Boolean := False;
      --  unspecified
      Reserved_19_31  : HAL.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DBGMCU_APB2FZR_Register use record
      Reserved_0_10   at 0 range 0 .. 10;
      DBG_TIM1_STOP   at 0 range 11 .. 11;
      Reserved_12_14  at 0 range 12 .. 14;
      DBG_TIM14_STOP  at 0 range 15 .. 15;
      DBG_TIM15_STOP  at 0 range 16 .. 16;
      DBG_TIM16_STOP  at 0 range 17 .. 17;
      DBG_LPTIM3_STOP at 0 range 18 .. 18;
      Reserved_19_31  at 0 range 19 .. 31;
   end record;

   --  DBGMCU status register
   type DBGMCU_SR_Register is record
      --  Read-only. Identifies whether access port AP1 is present in device
      AP1_PRESENT    : Boolean;
      --  Read-only. Identifies whether access port AP0 is present in device
      AP0_PRESENT    : Boolean;
      --  unspecified
      Reserved_2_15  : HAL.UInt14;
      --  Read-only. Identifies whether access port AP0 is open (can be
      --  accessed via the debug port) or locked (debug access to the AP is
      --  blocked)
      AP1_ENABLED    : Boolean;
      --  Read-only. Identifies whether access port AP0 is open (can be
      --  accessed via the debug port) or locked (debug access to the AP is
      --  blocked)
      AP0_ENABLED    : Boolean;
      --  unspecified
      Reserved_18_31 : HAL.UInt14;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DBGMCU_SR_Register use record
      AP1_PRESENT    at 0 range 0 .. 0;
      AP0_PRESENT    at 0 range 1 .. 1;
      Reserved_2_15  at 0 range 2 .. 15;
      AP1_ENABLED    at 0 range 16 .. 16;
      AP0_ENABLED    at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   subtype DBGMCU_PIDR4_JEP106CON_Field is HAL.UInt4;
   subtype DBGMCU_PIDR4_SIZE_Field is HAL.UInt4;

   --  DBGMCU CoreSight peripheral identity register 4
   type DBGMCU_PIDR4_Register is record
      --  Read-only. JEP106 continuation code
      JEP106CON     : DBGMCU_PIDR4_JEP106CON_Field;
      --  Read-only. register file size
      SIZE          : DBGMCU_PIDR4_SIZE_Field;
      --  unspecified
      Reserved_8_31 : HAL.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DBGMCU_PIDR4_Register use record
      JEP106CON     at 0 range 0 .. 3;
      SIZE          at 0 range 4 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype DBGMCU_PIDR0_PARTNUM_Field is HAL.UInt8;

   --  DBGMCU CoreSight peripheral identity register 0
   type DBGMCU_PIDR0_Register is record
      --  Read-only. part number bits [7:0]
      PARTNUM       : DBGMCU_PIDR0_PARTNUM_Field;
      --  unspecified
      Reserved_8_31 : HAL.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DBGMCU_PIDR0_Register use record
      PARTNUM       at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype DBGMCU_PIDR1_PARTNUM_Field is HAL.UInt4;
   subtype DBGMCU_PIDR1_JEP106ID_Field is HAL.UInt4;

   --  DBGMCU CoreSight peripheral identity register 1
   type DBGMCU_PIDR1_Register is record
      --  Read-only. part number bits [11:8]
      PARTNUM       : DBGMCU_PIDR1_PARTNUM_Field;
      --  Read-only. JEP106 identity code bits [3:0]
      JEP106ID      : DBGMCU_PIDR1_JEP106ID_Field;
      --  unspecified
      Reserved_8_31 : HAL.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DBGMCU_PIDR1_Register use record
      PARTNUM       at 0 range 0 .. 3;
      JEP106ID      at 0 range 4 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype DBGMCU_PIDR2_JEP106ID_Field is HAL.UInt3;
   subtype DBGMCU_PIDR2_REVISION_Field is HAL.UInt4;

   --  DBGMCU CoreSight peripheral identity register 2
   type DBGMCU_PIDR2_Register is record
      --  Read-only. JEP106 identity code bits [6:4]
      JEP106ID      : DBGMCU_PIDR2_JEP106ID_Field;
      --  Read-only. JEDEC assigned value
      JEDEC         : Boolean;
      --  Read-only. component revision number
      REVISION      : DBGMCU_PIDR2_REVISION_Field;
      --  unspecified
      Reserved_8_31 : HAL.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DBGMCU_PIDR2_Register use record
      JEP106ID      at 0 range 0 .. 2;
      JEDEC         at 0 range 3 .. 3;
      REVISION      at 0 range 4 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype DBGMCU_PIDR3_CMOD_Field is HAL.UInt4;
   subtype DBGMCU_PIDR3_REVAND_Field is HAL.UInt4;

   --  DBGMCU CoreSight peripheral identity register 3
   type DBGMCU_PIDR3_Register is record
      --  Read-only. customer modified
      CMOD          : DBGMCU_PIDR3_CMOD_Field;
      --  Read-only. metal fix version
      REVAND        : DBGMCU_PIDR3_REVAND_Field;
      --  unspecified
      Reserved_8_31 : HAL.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DBGMCU_PIDR3_Register use record
      CMOD          at 0 range 0 .. 3;
      REVAND        at 0 range 4 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype DBGMCU_CIDR_PREAMBLE_Field is HAL.UInt8;

   --  DBGMCU CoreSight component identity register 0
   type DBGMCU_CIDR_Register is record
      --  Read-only. component identification bits [7:0]
      PREAMBLE      : DBGMCU_CIDR_PREAMBLE_Field;
      --  unspecified
      Reserved_8_31 : HAL.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DBGMCU_CIDR_Register use record
      PREAMBLE      at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype DBGMCU_CIDR1_PREAMBLE_Field is HAL.UInt4;
   subtype DBGMCU_CIDR1_CLASS_Field is HAL.UInt4;

   --  DBGMCU CoreSight component identity register 1
   type DBGMCU_CIDR1_Register is record
      --  Read-only. component identification bits [11:8]
      PREAMBLE      : DBGMCU_CIDR1_PREAMBLE_Field;
      --  Read-only. component identification bits [15:12] - component class
      CLASS         : DBGMCU_CIDR1_CLASS_Field;
      --  unspecified
      Reserved_8_31 : HAL.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DBGMCU_CIDR1_Register use record
      PREAMBLE      at 0 range 0 .. 3;
      CLASS         at 0 range 4 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  DBGMCU register block
   type DBGMCU_Peripheral is record
      --  DBGMCU device ID code register
      DBGMCU_IDCODE          : aliased DBGMCU_IDCODE_Register;
      --  DBGMCU configuration register
      DBGMCU_CR              : aliased DBGMCU_CR_Register;
      --  DBGMCU APB1 freeze register
      DBGMCU_APB1FZR         : aliased DBGMCU_APB1FZR_Register;
      --  DBG APB2 freeze register
      DBGMCU_APB2FZR         : aliased DBGMCU_APB2FZR_Register;
      --  DBGMCU status register
      DBGMCU_SR              : aliased DBGMCU_SR_Register;
      --  DBGMCU debug authentication mailbox host register
      DBGMCU_DBG_AUTH_HOST   : aliased HAL.UInt32;
      --  DBGMCU debug authentication mailbox device register
      DBGMCU_DBG_AUTH_DEVICE : aliased HAL.UInt32;
      --  DBGMCU CoreSight peripheral identity register 4
      DBGMCU_PIDR4           : aliased DBGMCU_PIDR4_Register;
      --  DBGMCU CoreSight peripheral identity register 0
      DBGMCU_PIDR0           : aliased DBGMCU_PIDR0_Register;
      --  DBGMCU CoreSight peripheral identity register 1
      DBGMCU_PIDR1           : aliased DBGMCU_PIDR1_Register;
      --  DBGMCU CoreSight peripheral identity register 2
      DBGMCU_PIDR2           : aliased DBGMCU_PIDR2_Register;
      --  DBGMCU CoreSight peripheral identity register 3
      DBGMCU_PIDR3           : aliased DBGMCU_PIDR3_Register;
      --  DBGMCU CoreSight component identity register 0
      DBGMCU_CIDR0           : aliased DBGMCU_CIDR_Register;
      --  DBGMCU CoreSight component identity register 1
      DBGMCU_CIDR1           : aliased DBGMCU_CIDR1_Register;
      --  DBGMCU CoreSight component identity register 2
      DBGMCU_CIDR2           : aliased DBGMCU_CIDR_Register;
      --  DBGMCU CoreSight component identity register 3
      DBGMCU_CIDR3           : aliased DBGMCU_CIDR_Register;
   end record
     with Volatile;

   for DBGMCU_Peripheral use record
      DBGMCU_IDCODE          at 16#0# range 0 .. 31;
      DBGMCU_CR              at 16#4# range 0 .. 31;
      DBGMCU_APB1FZR         at 16#8# range 0 .. 31;
      DBGMCU_APB2FZR         at 16#C# range 0 .. 31;
      DBGMCU_SR              at 16#FC# range 0 .. 31;
      DBGMCU_DBG_AUTH_HOST   at 16#100# range 0 .. 31;
      DBGMCU_DBG_AUTH_DEVICE at 16#104# range 0 .. 31;
      DBGMCU_PIDR4           at 16#FD0# range 0 .. 31;
      DBGMCU_PIDR0           at 16#FE0# range 0 .. 31;
      DBGMCU_PIDR1           at 16#FE4# range 0 .. 31;
      DBGMCU_PIDR2           at 16#FE8# range 0 .. 31;
      DBGMCU_PIDR3           at 16#FEC# range 0 .. 31;
      DBGMCU_CIDR0           at 16#FF0# range 0 .. 31;
      DBGMCU_CIDR1           at 16#FF4# range 0 .. 31;
      DBGMCU_CIDR2           at 16#FF8# range 0 .. 31;
      DBGMCU_CIDR3           at 16#FFC# range 0 .. 31;
   end record;

   --  DBGMCU register block
   DBGMCU_Periph : aliased DBGMCU_Peripheral
     with Import, Address => DBGMCU_Base;

end STM32_SVD.DBGMCU;
