--  This spec has been automatically generated from xyzzy

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.PKA is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype PKA_CR_MODE_Field is HAL.UInt6;

   --  PKA control register
   type PKA_CR_Register is record
      --  PKA Enable
      EN             : Boolean := False;
      --  Start the operation
      START          : Boolean := False;
      --  unspecified
      Reserved_2_7   : HAL.UInt6 := 16#0#;
      --  PKA operation code
      MODE           : PKA_CR_MODE_Field := 16#0#;
      --  unspecified
      Reserved_14_16 : HAL.UInt3 := 16#0#;
      --  End of operation interrupt enable
      PROCENDIE      : Boolean := False;
      --  unspecified
      Reserved_18_18 : HAL.Bit := 16#0#;
      --  RAM error interrupt enable
      RAMERRIE       : Boolean := False;
      --  Address error interrupt enable
      ADDRERRIE      : Boolean := False;
      --  unspecified
      Reserved_21_31 : HAL.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PKA_CR_Register use record
      EN             at 0 range 0 .. 0;
      START          at 0 range 1 .. 1;
      Reserved_2_7   at 0 range 2 .. 7;
      MODE           at 0 range 8 .. 13;
      Reserved_14_16 at 0 range 14 .. 16;
      PROCENDIE      at 0 range 17 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      RAMERRIE       at 0 range 19 .. 19;
      ADDRERRIE      at 0 range 20 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   --  PKA status register
   type PKA_SR_Register is record
      --  unspecified
      Reserved_0_15  : HAL.UInt16;
      --  Read-only. PKA operation in progress
      BUSY           : Boolean;
      --  Read-only. PKA end of operation flag
      PROCENDF       : Boolean;
      --  unspecified
      Reserved_18_18 : HAL.Bit;
      --  Read-only. PKA ram error flag
      RAMERRF        : Boolean;
      --  Read-only. address er flag
      ADDRERRF       : Boolean;
      --  unspecified
      Reserved_21_31 : HAL.UInt11;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PKA_SR_Register use record
      Reserved_0_15  at 0 range 0 .. 15;
      BUSY           at 0 range 16 .. 16;
      PROCENDF       at 0 range 17 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      RAMERRF        at 0 range 19 .. 19;
      ADDRERRF       at 0 range 20 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   --  PKA clear flag register
   type PKA_CLRFR_Register is record
      --  unspecified
      Reserved_0_16  : HAL.UInt17 := 16#0#;
      --  Write-only. clear PKA end of operation flag
      PROCENDFC      : Boolean := False;
      --  unspecified
      Reserved_18_18 : HAL.Bit := 16#0#;
      --  Write-only. CLEAR PKA RAM ERROR FLAG
      RAMERRFC       : Boolean := False;
      --  Write-only. clear address error flag
      ADDRERRFC      : Boolean := False;
      --  unspecified
      Reserved_21_31 : HAL.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PKA_CLRFR_Register use record
      Reserved_0_16  at 0 range 0 .. 16;
      PROCENDFC      at 0 range 17 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      RAMERRFC       at 0 range 19 .. 19;
      ADDRERRFC      at 0 range 20 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  PKA
   type PKA_Peripheral is record
      --  PKA control register
      PKA_CR    : aliased PKA_CR_Register;
      --  PKA status register
      PKA_SR    : aliased PKA_SR_Register;
      --  PKA clear flag register
      PKA_CLRFR : aliased PKA_CLRFR_Register;
   end record
     with Volatile;

   for PKA_Peripheral use record
      PKA_CR    at 16#0# range 0 .. 31;
      PKA_SR    at 16#4# range 0 .. 31;
      PKA_CLRFR at 16#8# range 0 .. 31;
   end record;

   --  PKA
   PKA_Periph : aliased PKA_Peripheral
     with Import, Address => PKA_Base;

   --  PKA
   SEC_PKA_Periph : aliased PKA_Peripheral
     with Import, Address => SEC_PKA_Base;

end STM32_SVD.PKA;
