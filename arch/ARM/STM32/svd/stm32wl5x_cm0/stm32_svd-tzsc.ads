--  This spec has been automatically generated from STM32WL5x_CM0P.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.TZSC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  TZIC interrupt enable register 1
   type IER1_Register is record
      --  TZICIE
      TZICIE         : Boolean := True;
      --  TZSCIE
      TZSCIE         : Boolean := True;
      --  AESIE
      AESIE          : Boolean := True;
      --  RNGIE
      RNGIE          : Boolean := True;
      --  SUBGHZSPIIE
      SUBGHZSPIIE    : Boolean := True;
      --  PWRIE
      PWRIE          : Boolean := True;
      --  FLASHIFIE
      FLASHIFIE      : Boolean := True;
      --  DMA1IE
      DMA1IE         : Boolean := True;
      --  DMA2IE
      DMA2IE         : Boolean := True;
      --  DMAMUX1IE
      DMAMUX1IE      : Boolean := True;
      --  FLASHIE
      FLASHIE        : Boolean := True;
      --  SRAM1IE
      SRAM1IE        : Boolean := True;
      --  SRAM2IE
      SRAM2IE        : Boolean := True;
      --  PKAIE
      PKAIE          : Boolean := True;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#3FFFF#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IER1_Register use record
      TZICIE         at 0 range 0 .. 0;
      TZSCIE         at 0 range 1 .. 1;
      AESIE          at 0 range 2 .. 2;
      RNGIE          at 0 range 3 .. 3;
      SUBGHZSPIIE    at 0 range 4 .. 4;
      PWRIE          at 0 range 5 .. 5;
      FLASHIFIE      at 0 range 6 .. 6;
      DMA1IE         at 0 range 7 .. 7;
      DMA2IE         at 0 range 8 .. 8;
      DMAMUX1IE      at 0 range 9 .. 9;
      FLASHIE        at 0 range 10 .. 10;
      SRAM1IE        at 0 range 11 .. 11;
      SRAM2IE        at 0 range 12 .. 12;
      PKAIE          at 0 range 13 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   --  TZIC status register 1
   type MISR1_Register is record
      --  Read-only. TZICMF
      TZICMF         : Boolean;
      --  Read-only. TZSCMF
      TZSCMF         : Boolean;
      --  Read-only. AESMF
      AESMF          : Boolean;
      --  Read-only. RNGMF
      RNGMF          : Boolean;
      --  Read-only. SUBGHZSPIMF
      SUBGHZSPIMF    : Boolean;
      --  Read-only. PWRMF
      PWRMF          : Boolean;
      --  Read-only. FLASHIFMF
      FLASHIFMF      : Boolean;
      --  Read-only. DMA1MF
      DMA1MF         : Boolean;
      --  Read-only. DMA2MF
      DMA2MF         : Boolean;
      --  Read-only. DMAMUX1MF
      DMAMUX1MF      : Boolean;
      --  Read-only. FLASHMF
      FLASHMF        : Boolean;
      --  Read-only. SRAM1MF
      SRAM1MF        : Boolean;
      --  Read-only. SRAM2MF
      SRAM2MF        : Boolean;
      --  Read-only. PKAMF
      PKAMF          : Boolean;
      --  unspecified
      Reserved_14_31 : HAL.UInt18;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MISR1_Register use record
      TZICMF         at 0 range 0 .. 0;
      TZSCMF         at 0 range 1 .. 1;
      AESMF          at 0 range 2 .. 2;
      RNGMF          at 0 range 3 .. 3;
      SUBGHZSPIMF    at 0 range 4 .. 4;
      PWRMF          at 0 range 5 .. 5;
      FLASHIFMF      at 0 range 6 .. 6;
      DMA1MF         at 0 range 7 .. 7;
      DMA2MF         at 0 range 8 .. 8;
      DMAMUX1MF      at 0 range 9 .. 9;
      FLASHMF        at 0 range 10 .. 10;
      SRAM1MF        at 0 range 11 .. 11;
      SRAM2MF        at 0 range 12 .. 12;
      PKAMF          at 0 range 13 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   --  TZIC interrupt status clear register 1
   type ICR1_Register is record
      --  TZICCF
      TZICCF         : Boolean := False;
      --  TZSCCF
      TZSCCF         : Boolean := False;
      --  AESCF
      AESCF          : Boolean := False;
      --  RNGCF
      RNGCF          : Boolean := False;
      --  SUBGHZSPICF
      SUBGHZSPICF    : Boolean := False;
      --  PWRCF
      PWRCF          : Boolean := False;
      --  FLASHIFCF
      FLASHIFCF      : Boolean := False;
      --  DMA1CF
      DMA1CF         : Boolean := False;
      --  DMA2CF
      DMA2CF         : Boolean := False;
      --  DMAMUX1CF
      DMAMUX1CF      : Boolean := False;
      --  FLASHCF
      FLASHCF        : Boolean := False;
      --  SRAM1CF
      SRAM1CF        : Boolean := False;
      --  SRAM2CF
      SRAM2CF        : Boolean := False;
      --  PKACF
      PKACF          : Boolean := False;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ICR1_Register use record
      TZICCF         at 0 range 0 .. 0;
      TZSCCF         at 0 range 1 .. 1;
      AESCF          at 0 range 2 .. 2;
      RNGCF          at 0 range 3 .. 3;
      SUBGHZSPICF    at 0 range 4 .. 4;
      PWRCF          at 0 range 5 .. 5;
      FLASHIFCF      at 0 range 6 .. 6;
      DMA1CF         at 0 range 7 .. 7;
      DMA2CF         at 0 range 8 .. 8;
      DMAMUX1CF      at 0 range 9 .. 9;
      FLASHCF        at 0 range 10 .. 10;
      SRAM1CF        at 0 range 11 .. 11;
      SRAM2CF        at 0 range 12 .. 12;
      PKACF          at 0 range 13 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   --  TZSC control register
   type TZSC_CR_Register is record
      --  LCK
      LCK           : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_CR_Register use record
      LCK           at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  TZSC security configuration register
   type TZSC_SECCFGR1_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  AESSEC
      AESSEC         : Boolean := False;
      --  RNGSEC
      RNGSEC         : Boolean := False;
      --  unspecified
      Reserved_4_12  : HAL.UInt9 := 16#0#;
      --  PKASEC
      PKASEC         : Boolean := False;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_SECCFGR1_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      AESSEC         at 0 range 2 .. 2;
      RNGSEC         at 0 range 3 .. 3;
      Reserved_4_12  at 0 range 4 .. 12;
      PKASEC         at 0 range 13 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   --  TZSC privilege configuration register 1
   type TZSC_PRIVCFGR1_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  AESPRIV
      AESPRIV        : Boolean := False;
      --  RNGPRIV
      RNGPRIV        : Boolean := False;
      --  SUBGHZSPIPRIV
      SUBGHZSPIPRIV  : Boolean := False;
      --  unspecified
      Reserved_5_12  : HAL.UInt8 := 16#0#;
      --  PKAPRIV
      PKAPRIV        : Boolean := False;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_PRIVCFGR1_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      AESPRIV        at 0 range 2 .. 2;
      RNGPRIV        at 0 range 3 .. 3;
      SUBGHZSPIPRIV  at 0 range 4 .. 4;
      Reserved_5_12  at 0 range 5 .. 12;
      PKAPRIV        at 0 range 13 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   subtype TZSC_MPCWM1_UPWMR_LGTH_Field is HAL.UInt12;

   --  Unprivileged Water Mark 1 register
   type TZSC_MPCWM1_UPWMR_Register is record
      --  unspecified
      Reserved_0_15  : HAL.UInt16 := 16#0#;
      --  LGTH
      LGTH           : TZSC_MPCWM1_UPWMR_LGTH_Field := 16#FFF#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_MPCWM1_UPWMR_Register use record
      Reserved_0_15  at 0 range 0 .. 15;
      LGTH           at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype TZSC_MPCWM1_UPWWMR_LGTH_Field is HAL.UInt12;

   --  Unprivileged Writable Water Mark 1 register
   type TZSC_MPCWM1_UPWWMR_Register is record
      --  unspecified
      Reserved_0_15  : HAL.UInt16 := 16#0#;
      --  Define the length of Flash Unprivileged Writable area, in 2
      LGTH           : TZSC_MPCWM1_UPWWMR_LGTH_Field := 16#FFF#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_MPCWM1_UPWWMR_Register use record
      Reserved_0_15  at 0 range 0 .. 15;
      LGTH           at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype TZSC_MPCWM2_UPWMR_LGTH_Field is HAL.UInt12;

   --  Unprivileged Water Mark 2 register
   type TZSC_MPCWM2_UPWMR_Register is record
      --  unspecified
      Reserved_0_15  : HAL.UInt16 := 16#0#;
      --  LGTH
      LGTH           : TZSC_MPCWM2_UPWMR_LGTH_Field := 16#FFF#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_MPCWM2_UPWMR_Register use record
      Reserved_0_15  at 0 range 0 .. 15;
      LGTH           at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype TZSC_MPCWM3_UPWMR_LGTH_Field is HAL.UInt12;

   --  Unprivileged Water Mark 3 register
   type TZSC_MPCWM3_UPWMR_Register is record
      --  unspecified
      Reserved_0_15  : HAL.UInt16 := 16#0#;
      --  LGTH
      LGTH           : TZSC_MPCWM3_UPWMR_LGTH_Field := 16#FFF#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_MPCWM3_UPWMR_Register use record
      Reserved_0_15  at 0 range 0 .. 15;
      LGTH           at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  TrustZone Interrupt Control
   type TZIC_Peripheral is record
      --  TZIC interrupt enable register 1
      IER1  : aliased IER1_Register;
      --  TZIC status register 1
      MISR1 : aliased MISR1_Register;
      --  TZIC interrupt status clear register 1
      ICR1  : aliased ICR1_Register;
   end record
     with Volatile;

   for TZIC_Peripheral use record
      IER1  at 16#0# range 0 .. 31;
      MISR1 at 16#10# range 0 .. 31;
      ICR1  at 16#20# range 0 .. 31;
   end record;

   --  TrustZone Interrupt Control
   TZIC_Periph : aliased TZIC_Peripheral
     with Import, Address => TZIC_Base;

   --  Global TrustZone Controller
   type TZSC_Peripheral is record
      --  TZSC control register
      TZSC_CR            : aliased TZSC_CR_Register;
      --  TZSC security configuration register
      TZSC_SECCFGR1      : aliased TZSC_SECCFGR1_Register;
      --  TZSC privilege configuration register 1
      TZSC_PRIVCFGR1     : aliased TZSC_PRIVCFGR1_Register;
      --  Unprivileged Water Mark 1 register
      TZSC_MPCWM1_UPWMR  : aliased TZSC_MPCWM1_UPWMR_Register;
      --  Unprivileged Writable Water Mark 1 register
      TZSC_MPCWM1_UPWWMR : aliased TZSC_MPCWM1_UPWWMR_Register;
      --  Unprivileged Water Mark 2 register
      TZSC_MPCWM2_UPWMR  : aliased TZSC_MPCWM2_UPWMR_Register;
      --  Unprivileged Water Mark 3 register
      TZSC_MPCWM3_UPWMR  : aliased TZSC_MPCWM3_UPWMR_Register;
   end record
     with Volatile;

   for TZSC_Peripheral use record
      TZSC_CR            at 16#0# range 0 .. 31;
      TZSC_SECCFGR1      at 16#10# range 0 .. 31;
      TZSC_PRIVCFGR1     at 16#20# range 0 .. 31;
      TZSC_MPCWM1_UPWMR  at 16#130# range 0 .. 31;
      TZSC_MPCWM1_UPWWMR at 16#134# range 0 .. 31;
      TZSC_MPCWM2_UPWMR  at 16#138# range 0 .. 31;
      TZSC_MPCWM3_UPWMR  at 16#140# range 0 .. 31;
   end record;

   --  Global TrustZone Controller
   TZSC_Periph : aliased TZSC_Peripheral
     with Import, Address => TZSC_Base;

end STM32_SVD.TZSC;
