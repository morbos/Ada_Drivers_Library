--  This spec has been automatically generated from STM32U5xx.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.DLYB is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  control register
   type DLYB_CR_Register is record
      --  Operational amplifier Enable
      DEN           : Boolean := False;
      --  OPALPM
      SEN           : Boolean := False;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DLYB_CR_Register use record
      DEN           at 0 range 0 .. 0;
      SEN           at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   subtype DLYB_CFGR_SEL_Field is HAL.UInt4;
   subtype DLYB_CFGR_UNIT_Field is HAL.UInt7;
   subtype DLYB_CFGR_LNG_Field is HAL.UInt12;

   --  configuration register
   type DLYB_CFGR_Register is record
      --  SEL
      SEL            : DLYB_CFGR_SEL_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  UNIT
      UNIT           : DLYB_CFGR_UNIT_Field := 16#0#;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Read-only. LNG
      LNG            : DLYB_CFGR_LNG_Field := 16#0#;
      --  unspecified
      Reserved_28_30 : HAL.UInt3 := 16#0#;
      --  Read-only. LNGF
      LNGF           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DLYB_CFGR_Register use record
      SEL            at 0 range 0 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      UNIT           at 0 range 8 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      LNG            at 0 range 16 .. 27;
      Reserved_28_30 at 0 range 28 .. 30;
      LNGF           at 0 range 31 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  The delay block (DLYB) is used to generate an output clock that is
   --  dephased from the input clock
   type DLYB_Peripheral is record
      --  control register
      DLYB_CR   : aliased DLYB_CR_Register;
      --  configuration register
      DLYB_CFGR : aliased DLYB_CFGR_Register;
   end record
     with Volatile;

   for DLYB_Peripheral use record
      DLYB_CR   at 16#0# range 0 .. 31;
      DLYB_CFGR at 16#4# range 0 .. 31;
   end record;

   --  The delay block (DLYB) is used to generate an output clock that is
   --  dephased from the input clock
   DLYBOS1_Periph : aliased DLYB_Peripheral
     with Import, Address => DLYBOS1_Base;

   --  The delay block (DLYB) is used to generate an output clock that is
   --  dephased from the input clock
   DLYBOS2_Periph : aliased DLYB_Peripheral
     with Import, Address => DLYBOS2_Base;

   --  The delay block (DLYB) is used to generate an output clock that is
   --  dephased from the input clock
   DLYBSD1_Periph : aliased DLYB_Peripheral
     with Import, Address => DLYBSD1_Base;

   --  The delay block (DLYB) is used to generate an output clock that is
   --  dephased from the input clock
   DLYBSD2_Periph : aliased DLYB_Peripheral
     with Import, Address => DLYBSD2_Base;

   --  The delay block (DLYB) is used to generate an output clock that is
   --  dephased from the input clock
   SEC_DLYBOS1_Periph : aliased DLYB_Peripheral
     with Import, Address => SEC_DLYBOS1_Base;

   --  The delay block (DLYB) is used to generate an output clock that is
   --  dephased from the input clock
   SEC_DLYBOS2_Periph : aliased DLYB_Peripheral
     with Import, Address => SEC_DLYBOS2_Base;

   --  The delay block (DLYB) is used to generate an output clock that is
   --  dephased from the input clock
   SEC_DLYBSD1_Periph : aliased DLYB_Peripheral
     with Import, Address => SEC_DLYBSD1_Base;

   --  The delay block (DLYB) is used to generate an output clock that is
   --  dephased from the input clock
   SEC_DLYBSD2_Periph : aliased DLYB_Peripheral
     with Import, Address => SEC_DLYBSD2_Base;

end STM32_SVD.DLYB;
