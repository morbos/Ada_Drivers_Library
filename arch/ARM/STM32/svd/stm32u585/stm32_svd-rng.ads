--  This spec has been automatically generated from STM32U585.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.RNG is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype CR_RNG_CONFIG3_Field is HAL.UInt4;
   subtype CR_RNG_CONFIG2_Field is HAL.UInt3;
   subtype CR_CLKDIV_Field is HAL.UInt4;
   subtype CR_RNG_CONFIG1_Field is HAL.UInt6;

   --  control register
   type CR_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  True random number generator enable
      RNGEN          : Boolean := False;
      --  Interrupt Enable
      IE             : Boolean := False;
      --  unspecified
      Reserved_4_4   : HAL.Bit := 16#0#;
      --  Clock error detection
      CED            : Boolean := False;
      --  unspecified
      Reserved_6_6   : HAL.Bit := 16#0#;
      --  Auto reset disable
      ARDIS          : Boolean := False;
      --  RNG configuration 3
      RNG_CONFIG3    : CR_RNG_CONFIG3_Field := 16#0#;
      --  Non NIST compliant
      NISTC          : Boolean := False;
      --  RNG configuration 2
      RNG_CONFIG2    : CR_RNG_CONFIG2_Field := 16#0#;
      --  Clock divider factor
      CLKDIV         : CR_CLKDIV_Field := 16#0#;
      --  RNG configuration 1
      RNG_CONFIG1    : CR_RNG_CONFIG1_Field := 16#0#;
      --  unspecified
      Reserved_26_29 : HAL.UInt4 := 16#0#;
      --  Conditioning soft reset
      CONDRST        : Boolean := False;
      --  RNG Config Lock
      CONFIGLOCK     : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      RNGEN          at 0 range 2 .. 2;
      IE             at 0 range 3 .. 3;
      Reserved_4_4   at 0 range 4 .. 4;
      CED            at 0 range 5 .. 5;
      Reserved_6_6   at 0 range 6 .. 6;
      ARDIS          at 0 range 7 .. 7;
      RNG_CONFIG3    at 0 range 8 .. 11;
      NISTC          at 0 range 12 .. 12;
      RNG_CONFIG2    at 0 range 13 .. 15;
      CLKDIV         at 0 range 16 .. 19;
      RNG_CONFIG1    at 0 range 20 .. 25;
      Reserved_26_29 at 0 range 26 .. 29;
      CONDRST        at 0 range 30 .. 30;
      CONFIGLOCK     at 0 range 31 .. 31;
   end record;

   --  status register
   type SR_Register is record
      --  Read-only. Data ready
      DRDY          : Boolean := False;
      --  Read-only. Clock error current status
      CECS          : Boolean := False;
      --  Read-only. Seed error current status
      SECS          : Boolean := False;
      --  unspecified
      Reserved_3_4  : HAL.UInt2 := 16#0#;
      --  Clock error interrupt status
      CEIS          : Boolean := False;
      --  Seed error interrupt status
      SEIS          : Boolean := False;
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register use record
      DRDY          at 0 range 0 .. 0;
      CECS          at 0 range 1 .. 1;
      SECS          at 0 range 2 .. 2;
      Reserved_3_4  at 0 range 3 .. 4;
      CEIS          at 0 range 5 .. 5;
      SEIS          at 0 range 6 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Random number generator
   type RNG_Peripheral is record
      --  control register
      CR   : aliased CR_Register;
      --  status register
      SR   : aliased SR_Register;
      --  data register
      DR   : aliased HAL.UInt32;
      --  health test control register
      HTCR : aliased HAL.UInt32;
   end record
     with Volatile;

   for RNG_Peripheral use record
      CR   at 16#0# range 0 .. 31;
      SR   at 16#4# range 0 .. 31;
      DR   at 16#8# range 0 .. 31;
      HTCR at 16#10# range 0 .. 31;
   end record;

   --  Random number generator
   RNG_Periph : aliased RNG_Peripheral
     with Import, Address => RNG_Base;

   --  Random number generator
   SEC_RNG_Periph : aliased RNG_Peripheral
     with Import, Address => SEC_RNG_Base;

end STM32_SVD.RNG;
