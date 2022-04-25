--  This spec has been automatically generated from STM32U5xx.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.RAMCFG is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype RAM1CR_WSC_Field is HAL.UInt3;

   --  RAMCFG SRAM x control register
   type RAM1CR_Register is record
      --  ECCE
      ECCE           : Boolean := False;
      --  unspecified
      Reserved_1_3   : HAL.UInt3 := 16#0#;
      --  ALE
      ALE            : Boolean := False;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  SRAMER
      SRAMER         : Boolean := False;
      --  unspecified
      Reserved_9_15  : HAL.UInt7 := 16#0#;
      --  WSC
      WSC            : RAM1CR_WSC_Field := 16#0#;
      --  unspecified
      Reserved_19_31 : HAL.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RAM1CR_Register use record
      ECCE           at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      ALE            at 0 range 4 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      SRAMER         at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      WSC            at 0 range 16 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   --  RAMCFG RAMx interrupt status register
   type RAM1ISR_Register is record
      --  Read-only. SEDC
      SEDC          : Boolean;
      --  Read-only. DED
      DED           : Boolean;
      --  unspecified
      Reserved_2_7  : HAL.UInt6;
      --  Read-only. SRAMBUSY
      SRAMBUSY      : Boolean;
      --  unspecified
      Reserved_9_31 : HAL.UInt23;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RAM1ISR_Register use record
      SEDC          at 0 range 0 .. 0;
      DED           at 0 range 1 .. 1;
      Reserved_2_7  at 0 range 2 .. 7;
      SRAMBUSY      at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   subtype RAM1ERKEYR_ERASEKEY_Field is HAL.UInt8;

   --  RAMCFG SRAM x erase key register
   type RAM1ERKEYR_Register is record
      --  Write-only. ERASEKEY
      ERASEKEY      : RAM1ERKEYR_ERASEKEY_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RAM1ERKEYR_Register use record
      ERASEKEY      at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype RAM2CR_WSC_Field is HAL.UInt3;

   --  RAMCFG SRAM x control register
   type RAM2CR_Register is record
      --  ECCE
      ECCE           : Boolean := False;
      --  unspecified
      Reserved_1_3   : HAL.UInt3 := 16#0#;
      --  ALE
      ALE            : Boolean := False;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  SRAMER
      SRAMER         : Boolean := False;
      --  unspecified
      Reserved_9_15  : HAL.UInt7 := 16#0#;
      --  WSC
      WSC            : RAM2CR_WSC_Field := 16#0#;
      --  unspecified
      Reserved_19_31 : HAL.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RAM2CR_Register use record
      ECCE           at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      ALE            at 0 range 4 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      SRAMER         at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      WSC            at 0 range 16 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   --  RAMCFG SRAM x interrupt enable register
   type RAM2IER_Register is record
      --  SEIE
      SEIE          : Boolean := False;
      --  DEIE
      DEIE          : Boolean := False;
      --  unspecified
      Reserved_2_2  : HAL.Bit := 16#0#;
      --  ECCNMI
      ECCNMI        : Boolean := False;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RAM2IER_Register use record
      SEIE          at 0 range 0 .. 0;
      DEIE          at 0 range 1 .. 1;
      Reserved_2_2  at 0 range 2 .. 2;
      ECCNMI        at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  RAMCFG RAMx interrupt status register
   type RAM2ISR_Register is record
      --  Read-only. SEDC
      SEDC          : Boolean;
      --  Read-only. DED
      DED           : Boolean;
      --  unspecified
      Reserved_2_7  : HAL.UInt6;
      --  Read-only. SRAMBUSY
      SRAMBUSY      : Boolean;
      --  unspecified
      Reserved_9_31 : HAL.UInt23;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RAM2ISR_Register use record
      SEDC          at 0 range 0 .. 0;
      DED           at 0 range 1 .. 1;
      Reserved_2_7  at 0 range 2 .. 7;
      SRAMBUSY      at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   --  RAMCFG RAM x interrupt clear register x
   type RAM2ICR_Register is record
      --  CSEDC
      CSEDC         : Boolean := False;
      --  CDED
      CDED          : Boolean := False;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RAM2ICR_Register use record
      CSEDC         at 0 range 0 .. 0;
      CDED          at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  RAMCFG SRAM2 write protection register 1
   type RAM2WPR1_Register is record
      --  P0WP
      P0WP  : Boolean := False;
      --  P1WP
      P1WP  : Boolean := False;
      --  P2WP
      P2WP  : Boolean := False;
      --  P3WP
      P3WP  : Boolean := False;
      --  P4WP
      P4WP  : Boolean := False;
      --  P5WP
      P5WP  : Boolean := False;
      --  P6WP
      P6WP  : Boolean := False;
      --  P7WP
      P7WP  : Boolean := False;
      --  P8WP
      P8WP  : Boolean := False;
      --  P9WP
      P9WP  : Boolean := False;
      --  P10WP
      P10WP : Boolean := False;
      --  P11WP
      P11WP : Boolean := False;
      --  P12WP
      P12WP : Boolean := False;
      --  P13WP
      P13WP : Boolean := False;
      --  P14WP
      P14WP : Boolean := False;
      --  P15WP
      P15WP : Boolean := False;
      --  P16WP
      P16WP : Boolean := False;
      --  P17WP
      P17WP : Boolean := False;
      --  P18WP
      P18WP : Boolean := False;
      --  P19WP
      P19WP : Boolean := False;
      --  P20WP
      P20WP : Boolean := False;
      --  P21WP
      P21WP : Boolean := False;
      --  P22WP
      P22WP : Boolean := False;
      --  P23WP
      P23WP : Boolean := False;
      --  P24WP
      P24WP : Boolean := False;
      --  P25WP
      P25WP : Boolean := False;
      --  P26WP
      P26WP : Boolean := False;
      --  P27WP
      P27WP : Boolean := False;
      --  P28WP
      P28WP : Boolean := False;
      --  P29WP
      P29WP : Boolean := False;
      --  P30WP
      P30WP : Boolean := False;
      --  P31WP
      P31WP : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RAM2WPR1_Register use record
      P0WP  at 0 range 0 .. 0;
      P1WP  at 0 range 1 .. 1;
      P2WP  at 0 range 2 .. 2;
      P3WP  at 0 range 3 .. 3;
      P4WP  at 0 range 4 .. 4;
      P5WP  at 0 range 5 .. 5;
      P6WP  at 0 range 6 .. 6;
      P7WP  at 0 range 7 .. 7;
      P8WP  at 0 range 8 .. 8;
      P9WP  at 0 range 9 .. 9;
      P10WP at 0 range 10 .. 10;
      P11WP at 0 range 11 .. 11;
      P12WP at 0 range 12 .. 12;
      P13WP at 0 range 13 .. 13;
      P14WP at 0 range 14 .. 14;
      P15WP at 0 range 15 .. 15;
      P16WP at 0 range 16 .. 16;
      P17WP at 0 range 17 .. 17;
      P18WP at 0 range 18 .. 18;
      P19WP at 0 range 19 .. 19;
      P20WP at 0 range 20 .. 20;
      P21WP at 0 range 21 .. 21;
      P22WP at 0 range 22 .. 22;
      P23WP at 0 range 23 .. 23;
      P24WP at 0 range 24 .. 24;
      P25WP at 0 range 25 .. 25;
      P26WP at 0 range 26 .. 26;
      P27WP at 0 range 27 .. 27;
      P28WP at 0 range 28 .. 28;
      P29WP at 0 range 29 .. 29;
      P30WP at 0 range 30 .. 30;
      P31WP at 0 range 31 .. 31;
   end record;

   --  RAMCFG SRAM2 write protection register 2
   type RAM2WPR2_Register is record
      --  P32WP
      P32WP : Boolean := False;
      --  P33WP
      P33WP : Boolean := False;
      --  P34WP
      P34WP : Boolean := False;
      --  P35WP
      P35WP : Boolean := False;
      --  P36WP
      P36WP : Boolean := False;
      --  P37WP
      P37WP : Boolean := False;
      --  P38WP
      P38WP : Boolean := False;
      --  P39WP
      P39WP : Boolean := False;
      --  P40WP
      P40WP : Boolean := False;
      --  P41WP
      P41WP : Boolean := False;
      --  P42WP
      P42WP : Boolean := False;
      --  P43WP
      P43WP : Boolean := False;
      --  P44WP
      P44WP : Boolean := False;
      --  P45WP
      P45WP : Boolean := False;
      --  P46WP
      P46WP : Boolean := False;
      --  P47WP
      P47WP : Boolean := False;
      --  P48WP
      P48WP : Boolean := False;
      --  P49WP
      P49WP : Boolean := False;
      --  P50WP
      P50WP : Boolean := False;
      --  P51WP
      P51WP : Boolean := False;
      --  P52WP
      P52WP : Boolean := False;
      --  P53WP
      P53WP : Boolean := False;
      --  P54WP
      P54WP : Boolean := False;
      --  P55WP
      P55WP : Boolean := False;
      --  P56WP
      P56WP : Boolean := False;
      --  P57WP
      P57WP : Boolean := False;
      --  P58WP
      P58WP : Boolean := False;
      --  P59WP
      P59WP : Boolean := False;
      --  P60WP
      P60WP : Boolean := False;
      --  P61WP
      P61WP : Boolean := False;
      --  P62WP
      P62WP : Boolean := False;
      --  P63WP
      P63WP : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RAM2WPR2_Register use record
      P32WP at 0 range 0 .. 0;
      P33WP at 0 range 1 .. 1;
      P34WP at 0 range 2 .. 2;
      P35WP at 0 range 3 .. 3;
      P36WP at 0 range 4 .. 4;
      P37WP at 0 range 5 .. 5;
      P38WP at 0 range 6 .. 6;
      P39WP at 0 range 7 .. 7;
      P40WP at 0 range 8 .. 8;
      P41WP at 0 range 9 .. 9;
      P42WP at 0 range 10 .. 10;
      P43WP at 0 range 11 .. 11;
      P44WP at 0 range 12 .. 12;
      P45WP at 0 range 13 .. 13;
      P46WP at 0 range 14 .. 14;
      P47WP at 0 range 15 .. 15;
      P48WP at 0 range 16 .. 16;
      P49WP at 0 range 17 .. 17;
      P50WP at 0 range 18 .. 18;
      P51WP at 0 range 19 .. 19;
      P52WP at 0 range 20 .. 20;
      P53WP at 0 range 21 .. 21;
      P54WP at 0 range 22 .. 22;
      P55WP at 0 range 23 .. 23;
      P56WP at 0 range 24 .. 24;
      P57WP at 0 range 25 .. 25;
      P58WP at 0 range 26 .. 26;
      P59WP at 0 range 27 .. 27;
      P60WP at 0 range 28 .. 28;
      P61WP at 0 range 29 .. 29;
      P62WP at 0 range 30 .. 30;
      P63WP at 0 range 31 .. 31;
   end record;

   subtype RAM2ECCKEYR_ECCKEY_Field is HAL.UInt8;

   --  RAMCFG SRAM x ECC key register
   type RAM2ECCKEYR_Register is record
      --  Write-only. ECCKEY
      ECCKEY        : RAM2ECCKEYR_ECCKEY_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RAM2ECCKEYR_Register use record
      ECCKEY        at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype RAM2ERKEYR_ERASEKEY_Field is HAL.UInt8;

   --  RAMCFG SRAM x erase key register
   type RAM2ERKEYR_Register is record
      --  Write-only. ERASEKEY
      ERASEKEY      : RAM2ERKEYR_ERASEKEY_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RAM2ERKEYR_Register use record
      ERASEKEY      at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype RAM3CR_WSC_Field is HAL.UInt3;

   --  RAMCFG SRAM x control register
   type RAM3CR_Register is record
      --  ECCE
      ECCE           : Boolean := False;
      --  unspecified
      Reserved_1_3   : HAL.UInt3 := 16#0#;
      --  ALE
      ALE            : Boolean := False;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  SRAMER
      SRAMER         : Boolean := False;
      --  unspecified
      Reserved_9_15  : HAL.UInt7 := 16#0#;
      --  WSC
      WSC            : RAM3CR_WSC_Field := 16#0#;
      --  unspecified
      Reserved_19_31 : HAL.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RAM3CR_Register use record
      ECCE           at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      ALE            at 0 range 4 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      SRAMER         at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      WSC            at 0 range 16 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   --  RAMCFG SRAM x interrupt enable register
   type RAM3IER_Register is record
      --  SEIE
      SEIE          : Boolean := False;
      --  DEIE
      DEIE          : Boolean := False;
      --  unspecified
      Reserved_2_2  : HAL.Bit := 16#0#;
      --  ECCNMI
      ECCNMI        : Boolean := False;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RAM3IER_Register use record
      SEIE          at 0 range 0 .. 0;
      DEIE          at 0 range 1 .. 1;
      Reserved_2_2  at 0 range 2 .. 2;
      ECCNMI        at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  RAMCFG RAMx interrupt status register
   type RAM3ISR_Register is record
      --  Read-only. SEDC
      SEDC          : Boolean;
      --  Read-only. DED
      DED           : Boolean;
      --  unspecified
      Reserved_2_7  : HAL.UInt6;
      --  Read-only. SRAMBUSY
      SRAMBUSY      : Boolean;
      --  unspecified
      Reserved_9_31 : HAL.UInt23;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RAM3ISR_Register use record
      SEDC          at 0 range 0 .. 0;
      DED           at 0 range 1 .. 1;
      Reserved_2_7  at 0 range 2 .. 7;
      SRAMBUSY      at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   --  RAMCFG RAM x interrupt clear register x
   type RAM3ICR_Register is record
      --  CSEDC
      CSEDC         : Boolean := False;
      --  CDED
      CDED          : Boolean := False;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RAM3ICR_Register use record
      CSEDC         at 0 range 0 .. 0;
      CDED          at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   subtype RAM3ECCKEYR_ECCKEY_Field is HAL.UInt8;

   --  RAMCFG SRAM x ECC key register
   type RAM3ECCKEYR_Register is record
      --  Write-only. ECCKEY
      ECCKEY        : RAM3ECCKEYR_ECCKEY_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RAM3ECCKEYR_Register use record
      ECCKEY        at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype RAM3ERKEYR_ERASEKEY_Field is HAL.UInt8;

   --  RAMCFG SRAM x erase key register
   type RAM3ERKEYR_Register is record
      --  Write-only. ERASEKEY
      ERASEKEY      : RAM3ERKEYR_ERASEKEY_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RAM3ERKEYR_Register use record
      ERASEKEY      at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype RAM4CR_WSC_Field is HAL.UInt3;

   --  RAMCFG SRAM x control register
   type RAM4CR_Register is record
      --  ECCE
      ECCE           : Boolean := False;
      --  unspecified
      Reserved_1_3   : HAL.UInt3 := 16#0#;
      --  ALE
      ALE            : Boolean := False;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  SRAMER
      SRAMER         : Boolean := False;
      --  unspecified
      Reserved_9_15  : HAL.UInt7 := 16#0#;
      --  WSC
      WSC            : RAM4CR_WSC_Field := 16#0#;
      --  unspecified
      Reserved_19_31 : HAL.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RAM4CR_Register use record
      ECCE           at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      ALE            at 0 range 4 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      SRAMER         at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      WSC            at 0 range 16 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   --  RAMCFG RAMx interrupt status register
   type RAM4ISR_Register is record
      --  Read-only. SEDC
      SEDC          : Boolean;
      --  Read-only. DED
      DED           : Boolean;
      --  unspecified
      Reserved_2_7  : HAL.UInt6;
      --  Read-only. SRAMBUSY
      SRAMBUSY      : Boolean;
      --  unspecified
      Reserved_9_31 : HAL.UInt23;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RAM4ISR_Register use record
      SEDC          at 0 range 0 .. 0;
      DED           at 0 range 1 .. 1;
      Reserved_2_7  at 0 range 2 .. 7;
      SRAMBUSY      at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   subtype RAM4ERKEYR_ERASEKEY_Field is HAL.UInt8;

   --  RAMCFG SRAM x erase key register
   type RAM4ERKEYR_Register is record
      --  Write-only. ERASEKEY
      ERASEKEY      : RAM4ERKEYR_ERASEKEY_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RAM4ERKEYR_Register use record
      ERASEKEY      at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype RAM5CR_WSC_Field is HAL.UInt3;

   --  RAMCFG SRAM x control register
   type RAM5CR_Register is record
      --  ECCE
      ECCE           : Boolean := False;
      --  unspecified
      Reserved_1_3   : HAL.UInt3 := 16#0#;
      --  ALE
      ALE            : Boolean := False;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  SRAMER
      SRAMER         : Boolean := False;
      --  unspecified
      Reserved_9_15  : HAL.UInt7 := 16#0#;
      --  WSC
      WSC            : RAM5CR_WSC_Field := 16#0#;
      --  unspecified
      Reserved_19_31 : HAL.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RAM5CR_Register use record
      ECCE           at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      ALE            at 0 range 4 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      SRAMER         at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      WSC            at 0 range 16 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   --  RAMCFG SRAM x interrupt enable register
   type RAM5IER_Register is record
      --  SEIE
      SEIE          : Boolean := False;
      --  DEIE
      DEIE          : Boolean := False;
      --  unspecified
      Reserved_2_2  : HAL.Bit := 16#0#;
      --  ECCNMI
      ECCNMI        : Boolean := False;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RAM5IER_Register use record
      SEIE          at 0 range 0 .. 0;
      DEIE          at 0 range 1 .. 1;
      Reserved_2_2  at 0 range 2 .. 2;
      ECCNMI        at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  RAMCFG RAMx interrupt status register
   type RAM5ISR_Register is record
      --  Read-only. SEDC
      SEDC          : Boolean;
      --  Read-only. DED
      DED           : Boolean;
      --  unspecified
      Reserved_2_7  : HAL.UInt6;
      --  Read-only. SRAMBUSY
      SRAMBUSY      : Boolean;
      --  unspecified
      Reserved_9_31 : HAL.UInt23;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RAM5ISR_Register use record
      SEDC          at 0 range 0 .. 0;
      DED           at 0 range 1 .. 1;
      Reserved_2_7  at 0 range 2 .. 7;
      SRAMBUSY      at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   --  RAMCFG RAM x interrupt clear register x
   type RAM5ICR_Register is record
      --  CSEDC
      CSEDC         : Boolean := False;
      --  CDED
      CDED          : Boolean := False;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RAM5ICR_Register use record
      CSEDC         at 0 range 0 .. 0;
      CDED          at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  RAMCFG
   type RAMCFG_Peripheral is record
      --  RAMCFG SRAM x control register
      RAM1CR      : aliased RAM1CR_Register;
      --  RAMCFG RAMx interrupt status register
      RAM1ISR     : aliased RAM1ISR_Register;
      --  RAMCFG SRAM x erase key register
      RAM1ERKEYR  : aliased RAM1ERKEYR_Register;
      --  RAMCFG SRAM x control register
      RAM2CR      : aliased RAM2CR_Register;
      --  RAMCFG SRAM x interrupt enable register
      RAM2IER     : aliased RAM2IER_Register;
      --  RAMCFG RAMx interrupt status register
      RAM2ISR     : aliased RAM2ISR_Register;
      --  RAMCFG RAM x ECC single error address register
      RAM2SEAR    : aliased HAL.UInt32;
      --  RAMCFG RAM x ECC double error address register
      RAM2DEAR    : aliased HAL.UInt32;
      --  RAMCFG RAM x interrupt clear register x
      RAM2ICR     : aliased RAM2ICR_Register;
      --  RAMCFG SRAM2 write protection register 1
      RAM2WPR1    : aliased RAM2WPR1_Register;
      --  RAMCFG SRAM2 write protection register 2
      RAM2WPR2    : aliased RAM2WPR2_Register;
      --  RAMCFG SRAM x ECC key register
      RAM2ECCKEYR : aliased RAM2ECCKEYR_Register;
      --  RAMCFG SRAM x erase key register
      RAM2ERKEYR  : aliased RAM2ERKEYR_Register;
      --  RAMCFG SRAM x control register
      RAM3CR      : aliased RAM3CR_Register;
      --  RAMCFG SRAM x interrupt enable register
      RAM3IER     : aliased RAM3IER_Register;
      --  RAMCFG RAMx interrupt status register
      RAM3ISR     : aliased RAM3ISR_Register;
      --  RAMCFG RAM x ECC single error address register
      RAM3SEAR    : aliased HAL.UInt32;
      --  RAMCFG RAM x ECC double error address register
      RAM3DEAR    : aliased HAL.UInt32;
      --  RAMCFG RAM x interrupt clear register x
      RAM3ICR     : aliased RAM3ICR_Register;
      --  RAMCFG SRAM x ECC key register
      RAM3ECCKEYR : aliased RAM3ECCKEYR_Register;
      --  RAMCFG SRAM x erase key register
      RAM3ERKEYR  : aliased RAM3ERKEYR_Register;
      --  RAMCFG SRAM x control register
      RAM4CR      : aliased RAM4CR_Register;
      --  RAMCFG RAMx interrupt status register
      RAM4ISR     : aliased RAM4ISR_Register;
      --  RAMCFG SRAM x erase key register
      RAM4ERKEYR  : aliased RAM4ERKEYR_Register;
      --  RAMCFG SRAM x control register
      RAM5CR      : aliased RAM5CR_Register;
      --  RAMCFG SRAM x interrupt enable register
      RAM5IER     : aliased RAM5IER_Register;
      --  RAMCFG RAMx interrupt status register
      RAM5ISR     : aliased RAM5ISR_Register;
      --  RAMCFG RAM x ECC single error address register
      RAM5SEAR    : aliased HAL.UInt32;
      --  RAMCFG RAM x ECC double error address register
      RAM5DEAR    : aliased HAL.UInt32;
      --  RAMCFG RAM x interrupt clear register x
      RAM5ICR     : aliased RAM5ICR_Register;
   end record
     with Volatile;

   for RAMCFG_Peripheral use record
      RAM1CR      at 16#0# range 0 .. 31;
      RAM1ISR     at 16#8# range 0 .. 31;
      RAM1ERKEYR  at 16#28# range 0 .. 31;
      RAM2CR      at 16#40# range 0 .. 31;
      RAM2IER     at 16#44# range 0 .. 31;
      RAM2ISR     at 16#48# range 0 .. 31;
      RAM2SEAR    at 16#4C# range 0 .. 31;
      RAM2DEAR    at 16#50# range 0 .. 31;
      RAM2ICR     at 16#54# range 0 .. 31;
      RAM2WPR1    at 16#58# range 0 .. 31;
      RAM2WPR2    at 16#5C# range 0 .. 31;
      RAM2ECCKEYR at 16#64# range 0 .. 31;
      RAM2ERKEYR  at 16#68# range 0 .. 31;
      RAM3CR      at 16#80# range 0 .. 31;
      RAM3IER     at 16#84# range 0 .. 31;
      RAM3ISR     at 16#88# range 0 .. 31;
      RAM3SEAR    at 16#8C# range 0 .. 31;
      RAM3DEAR    at 16#90# range 0 .. 31;
      RAM3ICR     at 16#94# range 0 .. 31;
      RAM3ECCKEYR at 16#A4# range 0 .. 31;
      RAM3ERKEYR  at 16#A8# range 0 .. 31;
      RAM4CR      at 16#C0# range 0 .. 31;
      RAM4ISR     at 16#C8# range 0 .. 31;
      RAM4ERKEYR  at 16#E8# range 0 .. 31;
      RAM5CR      at 16#100# range 0 .. 31;
      RAM5IER     at 16#104# range 0 .. 31;
      RAM5ISR     at 16#108# range 0 .. 31;
      RAM5SEAR    at 16#10C# range 0 .. 31;
      RAM5DEAR    at 16#110# range 0 .. 31;
      RAM5ICR     at 16#114# range 0 .. 31;
   end record;

   --  RAMCFG
   RAMCFG_Periph : aliased RAMCFG_Peripheral
     with Import, Address => RAMCFG_Base;

   --  RAMCFG
   SEC_RAMCFG_Periph : aliased RAMCFG_Peripheral
     with Import, Address => SEC_RAMCFG_Base;

end STM32_SVD.RAMCFG;
