--  This spec has been automatically generated from STM32L4x5.svd

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

   subtype OPAMP1_CSR_OPAMODE_Field is HAL.UInt2;
   subtype OPAMP1_CSR_PGA_GAIN_Field is HAL.UInt2;
   subtype OPAMP1_CSR_VM_SEL_Field is HAL.UInt2;

   --  OPAMP1 control/status register
   type OPAMP1_CSR_Register is record
      --  Operational amplifier Enable
      OPAEN          : Boolean := False;
      --  Operational amplifier Low Power Mode
      OPALPM         : Boolean := False;
      --  Operational amplifier PGA mode
      OPAMODE        : OPAMP1_CSR_OPAMODE_Field := 16#0#;
      --  Operational amplifier Programmable amplifier gain value
      PGA_GAIN       : OPAMP1_CSR_PGA_GAIN_Field := 16#0#;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Inverting input selection
      VM_SEL         : OPAMP1_CSR_VM_SEL_Field := 16#0#;
      --  Non inverted input selection
      VP_SEL         : Boolean := False;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  Calibration mode enabled
      CALON          : Boolean := False;
      --  Calibration selection
      CALSEL         : Boolean := False;
      --  allows to switch from AOP offset trimmed values to AOP offset
      USERTRIM       : Boolean := False;
      --  Operational amplifier calibration output
      CALOUT         : Boolean := False;
      --  unspecified
      Reserved_16_30 : HAL.UInt15 := 16#0#;
      --  Operational amplifier power supply range for stability
      OPA_RANGE      : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPAMP1_CSR_Register use record
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

   subtype OPAMP1_OTR_TRIMOFFSETN_Field is HAL.UInt5;
   subtype OPAMP1_OTR_TRIMOFFSETP_Field is HAL.UInt5;

   --  OPAMP1 offset trimming register in normal mode
   type OPAMP1_OTR_Register is record
      --  Trim for NMOS differential pairs
      TRIMOFFSETN    : OPAMP1_OTR_TRIMOFFSETN_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  Trim for PMOS differential pairs
      TRIMOFFSETP    : OPAMP1_OTR_TRIMOFFSETP_Field := 16#0#;
      --  unspecified
      Reserved_13_31 : HAL.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPAMP1_OTR_Register use record
      TRIMOFFSETN    at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      TRIMOFFSETP    at 0 range 8 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   subtype OPAMP1_LPOTR_TRIMLPOFFSETN_Field is HAL.UInt5;
   subtype OPAMP1_LPOTR_TRIMLPOFFSETP_Field is HAL.UInt5;

   --  OPAMP1 offset trimming register in low-power mode
   type OPAMP1_LPOTR_Register is record
      --  Trim for NMOS differential pairs
      TRIMLPOFFSETN  : OPAMP1_LPOTR_TRIMLPOFFSETN_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  Trim for PMOS differential pairs
      TRIMLPOFFSETP  : OPAMP1_LPOTR_TRIMLPOFFSETP_Field := 16#0#;
      --  unspecified
      Reserved_13_31 : HAL.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPAMP1_LPOTR_Register use record
      TRIMLPOFFSETN  at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      TRIMLPOFFSETP  at 0 range 8 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   subtype OPAMP2_CSR_OPAMODE_Field is HAL.UInt2;
   subtype OPAMP2_CSR_PGA_GAIN_Field is HAL.UInt2;
   subtype OPAMP2_CSR_VM_SEL_Field is HAL.UInt2;

   --  OPAMP2 control/status register
   type OPAMP2_CSR_Register is record
      --  Operational amplifier Enable
      OPAEN          : Boolean := False;
      --  Operational amplifier Low Power Mode
      OPALPM         : Boolean := False;
      --  Operational amplifier PGA mode
      OPAMODE        : OPAMP2_CSR_OPAMODE_Field := 16#0#;
      --  Operational amplifier Programmable amplifier gain value
      PGA_GAIN       : OPAMP2_CSR_PGA_GAIN_Field := 16#0#;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Inverting input selection
      VM_SEL         : OPAMP2_CSR_VM_SEL_Field := 16#0#;
      --  Non inverted input selection
      VP_SEL         : Boolean := False;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  Calibration mode enabled
      CALON          : Boolean := False;
      --  Calibration selection
      CALSEL         : Boolean := False;
      --  allows to switch from AOP offset trimmed values to AOP offset
      USERTRIM       : Boolean := False;
      --  Operational amplifier calibration output
      CALOUT         : Boolean := False;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPAMP2_CSR_Register use record
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
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype OPAMP2_OTR_TRIMOFFSETN_Field is HAL.UInt5;
   subtype OPAMP2_OTR_TRIMOFFSETP_Field is HAL.UInt5;

   --  OPAMP2 offset trimming register in normal mode
   type OPAMP2_OTR_Register is record
      --  Trim for NMOS differential pairs
      TRIMOFFSETN    : OPAMP2_OTR_TRIMOFFSETN_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  Trim for PMOS differential pairs
      TRIMOFFSETP    : OPAMP2_OTR_TRIMOFFSETP_Field := 16#0#;
      --  unspecified
      Reserved_13_31 : HAL.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPAMP2_OTR_Register use record
      TRIMOFFSETN    at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      TRIMOFFSETP    at 0 range 8 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   subtype OPAMP2_LPOTR_TRIMLPOFFSETN_Field is HAL.UInt5;
   subtype OPAMP2_LPOTR_TRIMLPOFFSETP_Field is HAL.UInt5;

   --  OPAMP2 offset trimming register in low-power mode
   type OPAMP2_LPOTR_Register is record
      --  Trim for NMOS differential pairs
      TRIMLPOFFSETN  : OPAMP2_LPOTR_TRIMLPOFFSETN_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  Trim for PMOS differential pairs
      TRIMLPOFFSETP  : OPAMP2_LPOTR_TRIMLPOFFSETP_Field := 16#0#;
      --  unspecified
      Reserved_13_31 : HAL.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPAMP2_LPOTR_Register use record
      TRIMLPOFFSETN  at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      TRIMLPOFFSETP  at 0 range 8 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Operational amplifiers
   type OPAMP_Peripheral is record
      --  OPAMP1 control/status register
      OPAMP1_CSR   : aliased OPAMP1_CSR_Register;
      --  OPAMP1 offset trimming register in normal mode
      OPAMP1_OTR   : aliased OPAMP1_OTR_Register;
      --  OPAMP1 offset trimming register in low-power mode
      OPAMP1_LPOTR : aliased OPAMP1_LPOTR_Register;
      --  OPAMP2 control/status register
      OPAMP2_CSR   : aliased OPAMP2_CSR_Register;
      --  OPAMP2 offset trimming register in normal mode
      OPAMP2_OTR   : aliased OPAMP2_OTR_Register;
      --  OPAMP2 offset trimming register in low-power mode
      OPAMP2_LPOTR : aliased OPAMP2_LPOTR_Register;
   end record
     with Volatile;

   for OPAMP_Peripheral use record
      OPAMP1_CSR   at 16#0# range 0 .. 31;
      OPAMP1_OTR   at 16#4# range 0 .. 31;
      OPAMP1_LPOTR at 16#8# range 0 .. 31;
      OPAMP2_CSR   at 16#10# range 0 .. 31;
      OPAMP2_OTR   at 16#14# range 0 .. 31;
      OPAMP2_LPOTR at 16#18# range 0 .. 31;
   end record;

   --  Operational amplifiers
   OPAMP_Periph : aliased OPAMP_Peripheral
     with Import, Address => System'To_Address (16#40007800#);

end STM32_SVD.OPAMP;
