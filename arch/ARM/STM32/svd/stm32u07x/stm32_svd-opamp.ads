--  This spec has been automatically generated from STM32U073.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.OPAMP is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype OPAMP_CSR_OPAMODE_Field is HAL.UInt2;
   subtype OPAMP_CSR_PGA_GAIN_Field is HAL.UInt2;
   subtype OPAMP_CSR_VM_SEL_Field is HAL.UInt2;

   --  OPAMP control/status register
   type OPAMP_CSR_Register is record
      --  Operational amplifier Enable
      OPAEN          : Boolean := False;
      --  Operational amplifier Low Power Mode The operational amplifier must
      --  be disable to change this configuration.
      OPALPM         : Boolean := False;
      --  Operational amplifier PGA mode
      OPAMODE        : OPAMP_CSR_OPAMODE_Field := 16#0#;
      --  Operational amplifier Programmable amplifier gain value
      PGA_GAIN       : OPAMP_CSR_PGA_GAIN_Field := 16#0#;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Inverting input selection These bits are used only when OPAMODE = 00,
      --  01 or 10. 1x:	Inverting input not externally connected. These
      --  configurations are valid only when OPAMODE = 10 (PGA mode)
      VM_SEL         : OPAMP_CSR_VM_SEL_Field := 16#0#;
      --  Non inverted input selection
      VP_SEL         : Boolean := False;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  Calibration mode enabled
      CALON          : Boolean := False;
      --  Calibration selection
      CALSEL         : Boolean := False;
      --  allows to switch from factory AOP offset trimmed values to AOP offset
      --  user trimmed values This bit is active for both mode normal and
      --  low-power.
      USERTRIM       : Boolean := False;
      --  Read-only. Operational amplifier calibration output During
      --  calibration mode offset is trimmed when this signal toggle.
      CALOUT         : Boolean := False;
      --  unspecified
      Reserved_16_30 : HAL.UInt15 := 16#0#;
      --  Operational amplifier power supply range for stability All AOP must
      --  be in power down to allow AOP-RANGE bit write. It applies to all AOP
      --  embedded in the product.
      OPA_RANGE      : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPAMP_CSR_Register use record
      OPAEN          at 0 range 0 .. 0;
      OPALPM         at 0 range 1 .. 1;
      OPAMODE        at 0 range 2 .. 3;
      PGA_GAIN       at 0 range 4 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      VM_SEL         at 0 range 8 .. 9;
      VP_SEL         at 0 range 10 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      CALON          at 0 range 12 .. 12;
      CALSEL         at 0 range 13 .. 13;
      USERTRIM       at 0 range 14 .. 14;
      CALOUT         at 0 range 15 .. 15;
      Reserved_16_30 at 0 range 16 .. 30;
      OPA_RANGE      at 0 range 31 .. 31;
   end record;

   subtype OPAMP_OTR_TRIMOFFSETN_Field is HAL.UInt5;
   subtype OPAMP_OTR_TRIMOFFSETP_Field is HAL.UInt5;

   --  OPAMP offset trimming register in normal mode
   type OPAMP_OTR_Register is record
      --  Trim for NMOS differential pairs
      TRIMOFFSETN    : OPAMP_OTR_TRIMOFFSETN_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  Trim for PMOS differential pairs
      TRIMOFFSETP    : OPAMP_OTR_TRIMOFFSETP_Field := 16#0#;
      --  unspecified
      Reserved_13_31 : HAL.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPAMP_OTR_Register use record
      TRIMOFFSETN    at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      TRIMOFFSETP    at 0 range 8 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   subtype OPAMP_LPOTR_TRIMLPOFFSETN_Field is HAL.UInt5;
   subtype OPAMP_LPOTR_TRIMLPOFFSETP_Field is HAL.UInt5;

   --  OPAMP offset trimming register in low-power mode
   type OPAMP_LPOTR_Register is record
      --  Low-power mode trim for NMOS differential pairs
      TRIMLPOFFSETN  : OPAMP_LPOTR_TRIMLPOFFSETN_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  Low-power mode trim for PMOS differential pairs
      TRIMLPOFFSETP  : OPAMP_LPOTR_TRIMLPOFFSETP_Field := 16#0#;
      --  unspecified
      Reserved_13_31 : HAL.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPAMP_LPOTR_Register use record
      TRIMLPOFFSETN  at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      TRIMLPOFFSETP  at 0 range 8 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  OPAMP address block description
   type OPAMP_Peripheral is record
      --  OPAMP control/status register
      OPAMP_CSR   : aliased OPAMP_CSR_Register;
      --  OPAMP offset trimming register in normal mode
      OPAMP_OTR   : aliased OPAMP_OTR_Register;
      --  OPAMP offset trimming register in low-power mode
      OPAMP_LPOTR : aliased OPAMP_LPOTR_Register;
   end record
     with Volatile;

   for OPAMP_Peripheral use record
      OPAMP_CSR   at 16#0# range 0 .. 31;
      OPAMP_OTR   at 16#4# range 0 .. 31;
      OPAMP_LPOTR at 16#8# range 0 .. 31;
   end record;

   --  OPAMP address block description
   OPAMP_Periph : aliased OPAMP_Peripheral
     with Import, Address => OPAMP_Base;

end STM32_SVD.OPAMP;
