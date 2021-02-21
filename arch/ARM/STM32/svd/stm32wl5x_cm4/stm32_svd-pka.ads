--  This spec has been automatically generated from STM32WL5x_CM4.svd

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

   subtype CR_MODE_Field is HAL.UInt6;

   --  control register
   type CR_Register is record
      --  PKA enable.
      EN             : Boolean := False;
      --  start the operation
      START          : Boolean := False;
      --  unspecified
      Reserved_2_7   : HAL.UInt6 := 16#0#;
      --  PKA operation code
      MODE           : CR_MODE_Field := 16#0#;
      --  unspecified
      Reserved_14_16 : HAL.UInt3 := 16#0#;
      --  PROCENDIE
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

   for CR_Register use record
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

   --  status register
   type SR_Register is record
      --  unspecified
      Reserved_0_15  : HAL.UInt16;
      --  Read-only. PKA operation is in progressThis bit is set to 1 whenever
      --  START bit in the PKA_CR is set. It is automatically cleared when the
      --  computation is complete, meaning that PKA RAM can be safely accessed
      --  and a new operation can be started.
      BUSY           : Boolean;
      --  Read-only. PKA End of Operation flag
      PROCENDF       : Boolean;
      --  unspecified
      Reserved_18_18 : HAL.Bit;
      --  Read-only. PKA RAM error flag
      RAMERRF        : Boolean;
      --  Read-only. Address error flag
      ADDRERRF       : Boolean;
      --  unspecified
      Reserved_21_31 : HAL.UInt11;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register use record
      Reserved_0_15  at 0 range 0 .. 15;
      BUSY           at 0 range 16 .. 16;
      PROCENDF       at 0 range 17 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      RAMERRF        at 0 range 19 .. 19;
      ADDRERRF       at 0 range 20 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   --  clear flag register
   type CLRFR_Register is record
      --  unspecified
      Reserved_0_16  : HAL.UInt17 := 16#0#;
      --  Write-only. Clear PKA End of Operation flag
      PROCENDFC      : Boolean := False;
      --  unspecified
      Reserved_18_18 : HAL.Bit := 16#0#;
      --  Write-only. Clear PKA RAM error flag
      RAMERRFC       : Boolean := False;
      --  Write-only. Clear Address error flag
      ADDRERRFC      : Boolean := False;
      --  unspecified
      Reserved_21_31 : HAL.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CLRFR_Register use record
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

   --  Public key accelerator
   type PKA_Peripheral is record
      --  control register
      CR    : aliased CR_Register;
      --  status register
      SR    : aliased SR_Register;
      --  clear flag register
      CLRFR : aliased CLRFR_Register;
   end record
     with Volatile;

   for PKA_Peripheral use record
      CR    at 16#0# range 0 .. 31;
      SR    at 16#4# range 0 .. 31;
      CLRFR at 16#8# range 0 .. 31;
   end record;

   --  Public key accelerator
   PKA_Periph : aliased PKA_Peripheral
     with Import, Address => PKA_Base;

end STM32_SVD.PKA;
