--  This spec has been automatically generated from STM32U585.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.IWDG is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype KR_KEY_Field is HAL.UInt16;

   --  Key register
   type KR_Register is record
      --  Write-only. Key value (write only, read 0x0000)
      KEY            : KR_KEY_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for KR_Register use record
      KEY            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype PR_PR_Field is HAL.UInt4;

   --  Prescaler register
   type PR_Register is record
      --  Prescaler divider
      PR            : PR_PR_Field := 16#0#;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PR_Register use record
      PR            at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   subtype RLR_RL_Field is HAL.UInt12;

   --  Reload register
   type RLR_Register is record
      --  Watchdog counter reload value
      RL             : RLR_RL_Field := 16#FFF#;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RLR_Register use record
      RL             at 0 range 0 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   --  Status register
   type SR_Register is record
      --  Read-only. Watchdog prescaler value update
      PVU            : Boolean;
      --  Read-only. Watchdog counter reload value update
      RVU            : Boolean;
      --  Read-only. Watchdog counter window value update
      WVU            : Boolean;
      --  Read-only. Watchdog interrupt comparator value update
      EWU            : Boolean;
      --  unspecified
      Reserved_4_13  : HAL.UInt10;
      --  Read-only. Watchdog Early interrupt flag
      EWIF           : Boolean;
      --  unspecified
      Reserved_15_31 : HAL.UInt17;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register use record
      PVU            at 0 range 0 .. 0;
      RVU            at 0 range 1 .. 1;
      WVU            at 0 range 2 .. 2;
      EWU            at 0 range 3 .. 3;
      Reserved_4_13  at 0 range 4 .. 13;
      EWIF           at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype WINR_WIN_Field is HAL.UInt12;

   --  Window register
   type WINR_Register is record
      --  Watchdog counter window value
      WIN            : WINR_WIN_Field := 16#FFF#;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for WINR_Register use record
      WIN            at 0 range 0 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype EWCR_EWIT_Field is HAL.UInt12;

   --  IWDG early wakeup interrupt register
   type EWCR_Register is record
      --  Watchdog counter window value
      EWIT           : EWCR_EWIT_Field := 16#0#;
      --  unspecified
      Reserved_12_13 : HAL.UInt2 := 16#0#;
      --  Watchdog early interrupt acknowledge
      EWIC           : Boolean := False;
      --  Watchdog early interrupt enable
      EWIE           : Boolean := False;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EWCR_Register use record
      EWIT           at 0 range 0 .. 11;
      Reserved_12_13 at 0 range 12 .. 13;
      EWIC           at 0 range 14 .. 14;
      EWIE           at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Independent watchdog
   type IWDG_Peripheral is record
      --  Key register
      KR   : aliased KR_Register;
      --  Prescaler register
      PR   : aliased PR_Register;
      --  Reload register
      RLR  : aliased RLR_Register;
      --  Status register
      SR   : aliased SR_Register;
      --  Window register
      WINR : aliased WINR_Register;
      --  IWDG early wakeup interrupt register
      EWCR : aliased EWCR_Register;
   end record
     with Volatile;

   for IWDG_Peripheral use record
      KR   at 16#0# range 0 .. 31;
      PR   at 16#4# range 0 .. 31;
      RLR  at 16#8# range 0 .. 31;
      SR   at 16#C# range 0 .. 31;
      WINR at 16#10# range 0 .. 31;
      EWCR at 16#14# range 0 .. 31;
   end record;

   --  Independent watchdog
   IWDG_Periph : aliased IWDG_Peripheral
     with Import, Address => IWDG_Base;

   --  Independent watchdog
   SEC_IWDG_Periph : aliased IWDG_Peripheral
     with Import, Address => SEC_IWDG_Base;

end STM32_SVD.IWDG;
