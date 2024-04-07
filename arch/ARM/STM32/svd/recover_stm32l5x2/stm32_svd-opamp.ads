--  This spec has been automatically generated from xyzzy

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

   -------------------------------
   -- OPAMP cluster's Registers --
   -------------------------------

   subtype OPAMPx_CSR_OPAMP_OPAMODE_Field is HAL.UInt2;
   subtype OPAMPx_CSR_OPAMP_PGA_GAIN_Field is HAL.UInt2;
   subtype OPAMPx_CSR_OPAMP_VM_SEL_Field is HAL.UInt2;

   --  OPAMP1 control/status register
   type OPAMPx_CSR_OPAMP_Register is record
      --  Operational amplifier Enable
      OPAEN          : Boolean := False;
      --  Operational amplifier Low Power Mode
      OPALPM         : Boolean := False;
      --  Operational amplifier PGA mode
      OPAMODE        : OPAMPx_CSR_OPAMP_OPAMODE_Field := 16#0#;
      --  Operational amplifier Programmable amplifier gain value
      PGA_GAIN       : OPAMPx_CSR_OPAMP_PGA_GAIN_Field := 16#0#;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  inverting input selection
      VM_SEL         : OPAMPx_CSR_OPAMP_VM_SEL_Field := 16#0#;
      --  non inverted input selection
      VP_SEL         : Boolean := False;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  calibration mode enable
      CALON          : Boolean := False;
      --  calibration selection
      CALSEL         : Boolean := False;
      --  User trimming enable
      USERTRIM       : Boolean := False;
      --  Operational amplifier calibration output
      CALOUT         : Boolean := False;
      --  unspecified
      Reserved_16_30 : HAL.UInt15 := 16#0#;
      --  Operational amplifier power supply range for stability
      OPA_RANGE      : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPAMPx_CSR_OPAMP_Register use record
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

   subtype OPAMPx_OTR_OPAMP_TRIMOFFSETN_Field is HAL.UInt5;
   subtype OPAMPx_OTR_OPAMP_TRIMOFFSETP_Field is HAL.UInt5;

   --  OPAMP1 offset trimming register in normal mode
   type OPAMPx_OTR_OPAMP_Register is record
      --  Trim for NMOS differential pairs
      TRIMOFFSETN    : OPAMPx_OTR_OPAMP_TRIMOFFSETN_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  Trim for PMOS differential pairs
      TRIMOFFSETP    : OPAMPx_OTR_OPAMP_TRIMOFFSETP_Field := 16#0#;
      --  unspecified
      Reserved_13_31 : HAL.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPAMPx_OTR_OPAMP_Register use record
      TRIMOFFSETN    at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      TRIMOFFSETP    at 0 range 8 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   subtype OPAMPx_LPOTR_OPAMP_TRIMLPOFFSETN_Field is HAL.UInt5;
   subtype OPAMPx_LPOTR_OPAMP_TRIMLPOFFSETP_Field is HAL.UInt5;

   --  OPAMP1 offset trimming register in low-powe mode
   type OPAMPx_LPOTR_OPAMP_Register is record
      --  Trim for NMOS differential pairs
      TRIMLPOFFSETN  : OPAMPx_LPOTR_OPAMP_TRIMLPOFFSETN_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  Trim for PMOS differential pairs
      TRIMLPOFFSETP  : OPAMPx_LPOTR_OPAMP_TRIMLPOFFSETP_Field := 16#0#;
      --  unspecified
      Reserved_13_31 : HAL.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPAMPx_LPOTR_OPAMP_Register use record
      TRIMLPOFFSETN  at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      TRIMLPOFFSETP  at 0 range 8 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   type OPAMP_Cluster is record
      --  OPAMP1 control/status register
      OPAMPx_CSR   : aliased OPAMPx_CSR_OPAMP_Register;
      --  OPAMP1 offset trimming register in normal mode
      OPAMPx_OTR   : aliased OPAMPx_OTR_OPAMP_Register;
      --  OPAMP1 offset trimming register in low-powe mode
      OPAMPx_LPOTR : aliased OPAMPx_LPOTR_OPAMP_Register;
   end record
     with Size => 128;

   for OPAMP_Cluster use record
      OPAMPx_CSR   at 16#0# range 0 .. 31;
      OPAMPx_OTR   at 16#4# range 0 .. 31;
      OPAMPx_LPOTR at 16#8# range 0 .. 31;
   end record;

   type OPAMP_Clusters is array (1 .. 2) of OPAMP_Cluster;

   -------------------------------
   -- OPAMP cluster's Registers --
   -------------------------------

   -----------------
   -- Peripherals --
   -----------------

   --  Operational amplifiers
   type OPAMP_Peripheral is record
      OPAMP : aliased OPAMP_Clusters;
   end record
     with Volatile;

   for OPAMP_Peripheral use record
      OPAMP at 0 range 0 .. 255;
   end record;

   --  Operational amplifiers
   OPAMP_Periph : aliased OPAMP_Peripheral
     with Import, Address => OPAMP_Base;

   --  Operational amplifiers
   SEC_OPAMP_Periph : aliased OPAMP_Peripheral
     with Import, Address => SEC_OPAMP_Base;

end STM32_SVD.OPAMP;
