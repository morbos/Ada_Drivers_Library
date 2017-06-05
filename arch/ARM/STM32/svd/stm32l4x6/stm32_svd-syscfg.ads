--  This spec has been automatically generated from STM32L4x6.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.SYSCFG is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype MEMRMP_MEM_MODE_Field is HAL.UInt3;

   --  memory remap register
   type MEMRMP_Register is record
      --  Memory mapping selection
      MEM_MODE      : MEMRMP_MEM_MODE_Field := 16#0#;
      --  QUADSPI memory mapping swap
      QFS           : Boolean := False;
      --  unspecified
      Reserved_4_7  : HAL.UInt4 := 16#0#;
      --  Flash Bank mode selection
      FB_MODE       : Boolean := False;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MEMRMP_Register use record
      MEM_MODE      at 0 range 0 .. 2;
      QFS           at 0 range 3 .. 3;
      Reserved_4_7  at 0 range 4 .. 7;
      FB_MODE       at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   subtype CFGR1_FPU_IE_Field is HAL.UInt6;

   --  configuration register 1
   type CFGR1_Register is record
      --  Firewall disable
      FWDIS          : Boolean := True;
      --  unspecified
      Reserved_1_7   : HAL.UInt7 := 16#0#;
      --  I/O analog switch voltage booster enable
      BOOSTEN        : Boolean := False;
      --  unspecified
      Reserved_9_15  : HAL.UInt7 := 16#0#;
      --  Fast-mode Plus (Fm+) driving capability activation on PB6
      I2C_PB6_FMP    : Boolean := False;
      --  Fast-mode Plus (Fm+) driving capability activation on PB7
      I2C_PB7_FMP    : Boolean := False;
      --  Fast-mode Plus (Fm+) driving capability activation on PB8
      I2C_PB8_FMP    : Boolean := False;
      --  Fast-mode Plus (Fm+) driving capability activation on PB9
      I2C_PB9_FMP    : Boolean := False;
      --  I2C1 Fast-mode Plus driving capability activation
      I2C1_FMP       : Boolean := False;
      --  I2C2 Fast-mode Plus driving capability activation
      I2C2_FMP       : Boolean := False;
      --  I2C3 Fast-mode Plus driving capability activation
      I2C3_FMP       : Boolean := False;
      --  unspecified
      Reserved_23_25 : HAL.UInt3 := 16#0#;
      --  Floating Point Unit interrupts enable bits
      FPU_IE         : CFGR1_FPU_IE_Field := 16#1F#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CFGR1_Register use record
      FWDIS          at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      BOOSTEN        at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      I2C_PB6_FMP    at 0 range 16 .. 16;
      I2C_PB7_FMP    at 0 range 17 .. 17;
      I2C_PB8_FMP    at 0 range 18 .. 18;
      I2C_PB9_FMP    at 0 range 19 .. 19;
      I2C1_FMP       at 0 range 20 .. 20;
      I2C2_FMP       at 0 range 21 .. 21;
      I2C3_FMP       at 0 range 22 .. 22;
      Reserved_23_25 at 0 range 23 .. 25;
      FPU_IE         at 0 range 26 .. 31;
   end record;

   subtype EXTICR1_EXTI0_Field is HAL.UInt3;
   subtype EXTICR1_EXTI1_Field is HAL.UInt3;
   subtype EXTICR1_EXTI2_Field is HAL.UInt3;
   subtype EXTICR1_EXTI3_Field is HAL.UInt3;

   --  external interrupt configuration register 1
   type EXTICR1_Register is record
      --  EXTI 0 configuration bits
      EXTI0          : EXTICR1_EXTI0_Field := 16#0#;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  EXTI 1 configuration bits
      EXTI1          : EXTICR1_EXTI1_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  EXTI 2 configuration bits
      EXTI2          : EXTICR1_EXTI2_Field := 16#0#;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  EXTI 3 configuration bits
      EXTI3          : EXTICR1_EXTI3_Field := 16#0#;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EXTICR1_Register use record
      EXTI0          at 0 range 0 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      EXTI1          at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      EXTI2          at 0 range 8 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      EXTI3          at 0 range 12 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype EXTICR2_EXTI4_Field is HAL.UInt3;
   subtype EXTICR2_EXTI5_Field is HAL.UInt3;
   subtype EXTICR2_EXTI6_Field is HAL.UInt3;
   subtype EXTICR2_EXTI7_Field is HAL.UInt3;

   --  external interrupt configuration register 2
   type EXTICR2_Register is record
      --  EXTI 4 configuration bits
      EXTI4          : EXTICR2_EXTI4_Field := 16#0#;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  EXTI 5 configuration bits
      EXTI5          : EXTICR2_EXTI5_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  EXTI 6 configuration bits
      EXTI6          : EXTICR2_EXTI6_Field := 16#0#;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  EXTI 7 configuration bits
      EXTI7          : EXTICR2_EXTI7_Field := 16#0#;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EXTICR2_Register use record
      EXTI4          at 0 range 0 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      EXTI5          at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      EXTI6          at 0 range 8 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      EXTI7          at 0 range 12 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype EXTICR3_EXTI8_Field is HAL.UInt3;
   subtype EXTICR3_EXTI9_Field is HAL.UInt3;
   subtype EXTICR3_EXTI10_Field is HAL.UInt3;
   subtype EXTICR3_EXTI11_Field is HAL.UInt3;

   --  external interrupt configuration register 3
   type EXTICR3_Register is record
      --  EXTI 8 configuration bits
      EXTI8          : EXTICR3_EXTI8_Field := 16#0#;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  EXTI 9 configuration bits
      EXTI9          : EXTICR3_EXTI9_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  EXTI 10 configuration bits
      EXTI10         : EXTICR3_EXTI10_Field := 16#0#;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  EXTI 11 configuration bits
      EXTI11         : EXTICR3_EXTI11_Field := 16#0#;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EXTICR3_Register use record
      EXTI8          at 0 range 0 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      EXTI9          at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      EXTI10         at 0 range 8 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      EXTI11         at 0 range 12 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype EXTICR4_EXTI12_Field is HAL.UInt3;
   subtype EXTICR4_EXTI13_Field is HAL.UInt3;
   subtype EXTICR4_EXTI14_Field is HAL.UInt3;
   subtype EXTICR4_EXTI15_Field is HAL.UInt3;

   --  external interrupt configuration register 4
   type EXTICR4_Register is record
      --  EXTI12 configuration bits
      EXTI12         : EXTICR4_EXTI12_Field := 16#0#;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  EXTI13 configuration bits
      EXTI13         : EXTICR4_EXTI13_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  EXTI14 configuration bits
      EXTI14         : EXTICR4_EXTI14_Field := 16#0#;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  EXTI15 configuration bits
      EXTI15         : EXTICR4_EXTI15_Field := 16#0#;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EXTICR4_Register use record
      EXTI12         at 0 range 0 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      EXTI13         at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      EXTI14         at 0 range 8 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      EXTI15         at 0 range 12 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   --  SCSR
   type SCSR_Register is record
      --  SRAM2 Erase
      SRAM2ER       : Boolean := False;
      --  Read-only. SRAM2 busy by erase operation
      SRAM2BSY      : Boolean := False;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SCSR_Register use record
      SRAM2ER       at 0 range 0 .. 0;
      SRAM2BSY      at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  CFGR2
   type CFGR2_Register is record
      --  Write-only. Cortex-M4 LOCKUP (Hardfault) output enable bit
      CLL           : Boolean := False;
      --  Write-only. SRAM2 parity lock bit
      SPL           : Boolean := False;
      --  Write-only. PVD lock enable bit
      PVDL          : Boolean := False;
      --  Write-only. ECC Lock
      ECCL          : Boolean := False;
      --  unspecified
      Reserved_4_7  : HAL.UInt4 := 16#0#;
      --  SRAM2 parity error flag
      SPF           : Boolean := False;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CFGR2_Register use record
      CLL           at 0 range 0 .. 0;
      SPL           at 0 range 1 .. 1;
      PVDL          at 0 range 2 .. 2;
      ECCL          at 0 range 3 .. 3;
      Reserved_4_7  at 0 range 4 .. 7;
      SPF           at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   --  SWPR
   type SWPR_Register is record
      --  Write-only. P0WP
      P0WP  : Boolean := False;
      --  Write-only. P1WP
      P1WP  : Boolean := False;
      --  Write-only. P2WP
      P2WP  : Boolean := False;
      --  Write-only. P3WP
      P3WP  : Boolean := False;
      --  Write-only. P4WP
      P4WP  : Boolean := False;
      --  Write-only. P5WP
      P5WP  : Boolean := False;
      --  Write-only. P6WP
      P6WP  : Boolean := False;
      --  Write-only. P7WP
      P7WP  : Boolean := False;
      --  Write-only. P8WP
      P8WP  : Boolean := False;
      --  Write-only. P9WP
      P9WP  : Boolean := False;
      --  Write-only. P10WP
      P10WP : Boolean := False;
      --  Write-only. P11WP
      P11WP : Boolean := False;
      --  Write-only. P12WP
      P12WP : Boolean := False;
      --  Write-only. P13WP
      P13WP : Boolean := False;
      --  Write-only. P14WP
      P14WP : Boolean := False;
      --  Write-only. P15WP
      P15WP : Boolean := False;
      --  Write-only. P16WP
      P16WP : Boolean := False;
      --  Write-only. P17WP
      P17WP : Boolean := False;
      --  Write-only. P18WP
      P18WP : Boolean := False;
      --  Write-only. P19WP
      P19WP : Boolean := False;
      --  Write-only. P20WP
      P20WP : Boolean := False;
      --  Write-only. P21WP
      P21WP : Boolean := False;
      --  Write-only. P22WP
      P22WP : Boolean := False;
      --  Write-only. P23WP
      P23WP : Boolean := False;
      --  Write-only. P24WP
      P24WP : Boolean := False;
      --  Write-only. P25WP
      P25WP : Boolean := False;
      --  Write-only. P26WP
      P26WP : Boolean := False;
      --  Write-only. P27WP
      P27WP : Boolean := False;
      --  Write-only. P28WP
      P28WP : Boolean := False;
      --  Write-only. P29WP
      P29WP : Boolean := False;
      --  Write-only. P30WP
      P30WP : Boolean := False;
      --  Write-only. SRAM2 page 31 write protection
      P31WP : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SWPR_Register use record
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

   subtype SKR_KEY_Field is HAL.UInt8;

   --  SKR
   type SKR_Register is record
      --  Write-only. SRAM2 write protection key for software erase
      KEY           : SKR_KEY_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SKR_Register use record
      KEY           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  System configuration controller
   type SYSCFG_Peripheral is record
      --  memory remap register
      MEMRMP  : aliased MEMRMP_Register;
      --  configuration register 1
      CFGR1   : aliased CFGR1_Register;
      --  external interrupt configuration register 1
      EXTICR1 : aliased EXTICR1_Register;
      --  external interrupt configuration register 2
      EXTICR2 : aliased EXTICR2_Register;
      --  external interrupt configuration register 3
      EXTICR3 : aliased EXTICR3_Register;
      --  external interrupt configuration register 4
      EXTICR4 : aliased EXTICR4_Register;
      --  SCSR
      SCSR    : aliased SCSR_Register;
      --  CFGR2
      CFGR2   : aliased CFGR2_Register;
      --  SWPR
      SWPR    : aliased SWPR_Register;
      --  SKR
      SKR     : aliased SKR_Register;
   end record
     with Volatile;

   for SYSCFG_Peripheral use record
      MEMRMP  at 16#0# range 0 .. 31;
      CFGR1   at 16#4# range 0 .. 31;
      EXTICR1 at 16#8# range 0 .. 31;
      EXTICR2 at 16#C# range 0 .. 31;
      EXTICR3 at 16#10# range 0 .. 31;
      EXTICR4 at 16#14# range 0 .. 31;
      SCSR    at 16#18# range 0 .. 31;
      CFGR2   at 16#1C# range 0 .. 31;
      SWPR    at 16#20# range 0 .. 31;
      SKR     at 16#24# range 0 .. 31;
   end record;

   --  System configuration controller
   SYSCFG_Periph : aliased SYSCFG_Peripheral
     with Import, Address => System'To_Address (16#40010000#);

end STM32_SVD.SYSCFG;
