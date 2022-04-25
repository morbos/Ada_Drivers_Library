--  This spec has been automatically generated from xyzzy

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.MPU is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype MPU_TYPE_DREGION_Field is HAL.UInt8;

   --  MPU Type Register
   type MPU_TYPE_Register is record
      --  Separate instr/data
      SEP            : Boolean := False;
      --  unspecified
      Reserved_1_7   : HAL.UInt7 := 16#0#;
      --  Data Regions
      DREGION        : MPU_TYPE_DREGION_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPU_TYPE_Register use record
      SEP            at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      DREGION        at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   type MPU_CTRL_Register is record
      ENABLE        : Boolean := False;
      HFNMIENA      : Boolean := False;
      PRIVDEFENA    : Boolean := False;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPU_CTRL_Register use record
      ENABLE        at 0 range 0 .. 0;
      HFNMIENA      at 0 range 1 .. 1;
      PRIVDEFENA    at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype MPU_RNR_REGION_Field is HAL.UInt8;

   --  Region select
   type MPU_RNR_Register is record
      REGION        : MPU_RNR_REGION_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPU_RNR_Register use record
      REGION        at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype MPU_RBAR_AP_Field is HAL.UInt2;
   subtype MPU_RBAR_SH_Field is HAL.UInt2;
   subtype MPU_RBAR_BADDR_Field is HAL.UInt27;

   --  MPU Region Base Address Register
   type MPU_RBAR_Register is record
      XN    : Boolean := False;
      AP    : MPU_RBAR_AP_Field := 16#0#;
      SH    : MPU_RBAR_SH_Field := 16#0#;
      --  Holds bits [31:5] of the base address for the selected MPU region
      BADDR : MPU_RBAR_BADDR_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPU_RBAR_Register use record
      XN    at 0 range 0 .. 0;
      AP    at 0 range 1 .. 2;
      SH    at 0 range 3 .. 4;
      BADDR at 0 range 5 .. 31;
   end record;

   subtype MPU_RLAR_AttrIndx_Field is HAL.UInt3;
   subtype MPU_RLAR_LIMIT_Field is HAL.UInt27;

   type MPU_RLAR_Register is record
      EN       : Boolean := False;
      AttrIndx : MPU_RLAR_AttrIndx_Field := 16#0#;
      PXN      : Boolean := False;
      --  Holds bits [31:5] of the limit for the selected MPU region
      LIMIT    : MPU_RLAR_LIMIT_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPU_RLAR_Register use record
      EN       at 0 range 0 .. 0;
      AttrIndx at 0 range 1 .. 3;
      PXN      at 0 range 4 .. 4;
      LIMIT    at 0 range 5 .. 31;
   end record;

   -------------------------------
   -- ALIAS cluster's Registers --
   -------------------------------

   subtype MPU_RBAR_ALIASx_ALIAS_AP_Field is HAL.UInt2;
   subtype MPU_RBAR_ALIASx_ALIAS_SH_Field is HAL.UInt2;
   subtype MPU_RBAR_ALIASx_ALIAS_BADDR_Field is HAL.UInt27;

   type MPU_RBAR_ALIASx_ALIAS_Register is record
      XN    : Boolean := False;
      AP    : MPU_RBAR_ALIASx_ALIAS_AP_Field := 16#0#;
      SH    : MPU_RBAR_ALIASx_ALIAS_SH_Field := 16#0#;
      --  Holds bits [31:5] of the base address for the selected MPU region
      BADDR : MPU_RBAR_ALIASx_ALIAS_BADDR_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPU_RBAR_ALIASx_ALIAS_Register use record
      XN    at 0 range 0 .. 0;
      AP    at 0 range 1 .. 2;
      SH    at 0 range 3 .. 4;
      BADDR at 0 range 5 .. 31;
   end record;

   subtype MPU_RLAR_ALIASx_ALIAS_AttrIndx_Field is HAL.UInt3;
   subtype MPU_RLAR_ALIASx_ALIAS_LIMIT_Field is HAL.UInt27;

   type MPU_RLAR_ALIASx_ALIAS_Register is record
      EN       : Boolean := False;
      AttrIndx : MPU_RLAR_ALIASx_ALIAS_AttrIndx_Field := 16#0#;
      PXN      : Boolean := False;
      --  Holds bits [31:5] of the limit for the selected MPU region
      LIMIT    : MPU_RLAR_ALIASx_ALIAS_LIMIT_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPU_RLAR_ALIASx_ALIAS_Register use record
      EN       at 0 range 0 .. 0;
      AttrIndx at 0 range 1 .. 3;
      PXN      at 0 range 4 .. 4;
      LIMIT    at 0 range 5 .. 31;
   end record;

   type ALIAS_Cluster is record
      MPU_RBAR_ALIASx : aliased MPU_RBAR_ALIASx_ALIAS_Register;
      MPU_RLAR_ALIASx : aliased MPU_RLAR_ALIASx_ALIAS_Register;
   end record
     with Size => 64;

   for ALIAS_Cluster use record
      MPU_RBAR_ALIASx at 16#0# range 0 .. 31;
      MPU_RLAR_ALIASx at 16#4# range 0 .. 31;
   end record;

   type ALIAS_Clusters is array (1 .. 3) of ALIAS_Cluster;

   --  MPU_MAIR_Attr array element
   subtype MPU_MAIR_Attr_Element is HAL.UInt8;

   --  MPU_MAIR_Attr array
   type MPU_MAIR_Attr_Field_Array is array (0 .. 3) of MPU_MAIR_Attr_Element
     with Component_Size => 8, Size => 32;

   type MPU_MAIR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  Attr as a value
            Val : HAL.UInt32;
         when True =>
            --  Attr as an array
            Arr : MPU_MAIR_Attr_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPU_MAIR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   type MPU_MAIR_Registers is array (0 .. 1) of MPU_MAIR_Register;

   -----------------
   -- Peripherals --
   -----------------

   --  Memory Protection Unit
   type MPU_Peripheral is record
      --  MPU Type Register
      MPU_TYPE : aliased MPU_TYPE_Register;
      MPU_CTRL : aliased MPU_CTRL_Register;
      --  Region select
      MPU_RNR  : aliased MPU_RNR_Register;
      --  MPU Region Base Address Register
      MPU_RBAR : aliased MPU_RBAR_Register;
      MPU_RLAR : aliased MPU_RLAR_Register;
      ALIAS    : aliased ALIAS_Clusters;
      MPU_MAIR : aliased MPU_MAIR_Registers;
   end record
     with Volatile;

   for MPU_Peripheral use record
      MPU_TYPE at 16#0# range 0 .. 31;
      MPU_CTRL at 16#4# range 0 .. 31;
      MPU_RNR  at 16#8# range 0 .. 31;
      MPU_RBAR at 16#C# range 0 .. 31;
      MPU_RLAR at 16#10# range 0 .. 31;
      ALIAS    at 16#14# range 0 .. 191;
      MPU_MAIR at 16#30# range 0 .. 63;
   end record;

   --  Memory Protection Unit
   MPU_Periph : aliased MPU_Peripheral
     with Import, Address => MPU_Base;

end STM32_SVD.MPU;
