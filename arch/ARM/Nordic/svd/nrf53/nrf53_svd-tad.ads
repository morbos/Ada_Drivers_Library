--  This spec has been automatically generated from nrf53.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NRF53_SVD.TAD is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Start all trace debug clocks
   type CLOCKSTART_Register is record
      --  Start trace debug clocks
      START         : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CLOCKSTART_Register use record
      START         at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Stop all trace debug clocks
   type CLOCKSTOP_Register is record
      --  Stop trace debug clocks
      STOP          : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CLOCKSTOP_Register use record
      STOP          at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Enable debug domain and aquire selected GPIOs
   type ENABLE_Register is record
      --  Enable debug domain and aquire selected GPIOs
      ENABLE        : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ENABLE_Register use record
      ENABLE        at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   subtype TRACECLK_PIN_Field is HAL.UInt5;

   --  Pin configuration for traceclk
   type TRACECLK_Register is record
      --  Pin number
      PIN           : TRACECLK_PIN_Field := 16#0#;
      --  unspecified
      Reserved_5_30 : HAL.UInt26 := 16#0#;
      --  connection
      CONNECT       : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TRACECLK_Register use record
      PIN           at 0 range 0 .. 4;
      Reserved_5_30 at 0 range 5 .. 30;
      CONNECT       at 0 range 31 .. 31;
   end record;

   subtype TRACEDATA_PIN_Field is HAL.UInt5;

   --  Pin configuration for tracedata0
   type TRACEDATA_Register is record
      --  Pin number
      PIN           : TRACEDATA_PIN_Field := 16#0#;
      --  unspecified
      Reserved_5_30 : HAL.UInt26 := 16#0#;
      --  connection
      CONNECT       : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TRACEDATA_Register use record
      PIN           at 0 range 0 .. 4;
      Reserved_5_30 at 0 range 5 .. 30;
      CONNECT       at 0 range 31 .. 31;
   end record;

   subtype TRACESPEED_TRACEPORTSPEED_Field is HAL.UInt2;

   --  Clocking options for the trace port debug interface
   type TRACESPEED_Register is record
      --  speed of traceport
      TRACEPORTSPEED : TRACESPEED_TRACEPORTSPEED_Field := 16#0#;
      --  unspecified
      Reserved_2_31  : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TRACESPEED_Register use record
      TRACEPORTSPEED at 0 range 0 .. 1;
      Reserved_2_31  at 0 range 2 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Trace and debug
   type TAD_Peripheral is record
      --  Start all trace debug clocks
      CLOCKSTART : aliased CLOCKSTART_Register;
      --  Stop all trace debug clocks
      CLOCKSTOP  : aliased CLOCKSTOP_Register;
      --  Enable debug domain and aquire selected GPIOs
      ENABLE     : aliased ENABLE_Register;
      --  Pin configuration for traceclk
      TRACECLK   : aliased TRACECLK_Register;
      --  Pin configuration for tracedata0
      TRACEDATA0 : aliased TRACEDATA_Register;
      --  Pin configuration for tracedata1
      TRACEDATA1 : aliased TRACEDATA_Register;
      --  Pin configuration for tracedata2
      TRACEDATA2 : aliased TRACEDATA_Register;
      --  Pin configuration for tracedata3
      TRACEDATA3 : aliased TRACEDATA_Register;
      --  Clocking options for the trace port debug interface
      TRACESPEED : aliased TRACESPEED_Register;
   end record
     with Volatile;

   for TAD_Peripheral use record
      CLOCKSTART at 16#4# range 0 .. 31;
      CLOCKSTOP  at 16#8# range 0 .. 31;
      ENABLE     at 16#500# range 0 .. 31;
      TRACECLK   at 16#504# range 0 .. 31;
      TRACEDATA0 at 16#508# range 0 .. 31;
      TRACEDATA1 at 16#50C# range 0 .. 31;
      TRACEDATA2 at 16#510# range 0 .. 31;
      TRACEDATA3 at 16#514# range 0 .. 31;
      TRACESPEED at 16#518# range 0 .. 31;
   end record;

   --  Trace and debug
   TAD_Periph : aliased TAD_Peripheral
     with Import, Address => System'To_Address (16#E0080000#);

end NRF53_SVD.TAD;
