--  This spec has been automatically generated from STM32L5x2.svd

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
      SEP            : Boolean;
      --  unspecified
      Reserved_1_7   : HAL.UInt7;
      --  Data Regions
      DREGION        : MPU_TYPE_DREGION_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPU_TYPE_Register use record
      SEP            at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      DREGION        at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   type MPU_CTRL_Register is record
      ENABLE        : Boolean;
      HFNMIENA      : Boolean;
      PRIVDEFENA    : Boolean;
      --  unspecified
      Reserved_3_31 : HAL.UInt29;
   end record
     with Volatile_Full_Access, Size => 32,
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
      REGION        : MPU_RNR_REGION_Field;
      --  unspecified
      Reserved_8_31 : HAL.UInt24;
   end record
     with Volatile_Full_Access, Size => 32,
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
      XN    : Boolean;
      AP    : MPU_RBAR_AP_Field;
      SH    : MPU_RBAR_SH_Field;
      --  Holds bits [31:5] of the base address for the selected MPU region
      BADDR : MPU_RBAR_BADDR_Field;
   end record
     with Volatile_Full_Access, Size => 32,
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
      EN       : Boolean;
      AttrIndx : MPU_RLAR_AttrIndx_Field;
      PXN      : Boolean;
      --  Holds bits [31:5] of the limit for the selected MPU region
      LIMIT    : MPU_RLAR_LIMIT_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPU_RLAR_Register use record
      EN       at 0 range 0 .. 0;
      AttrIndx at 0 range 1 .. 3;
      PXN      at 0 range 4 .. 4;
      LIMIT    at 0 range 5 .. 31;
   end record;

   subtype MPU_RBAR_ALIAS_AP_Field is HAL.UInt2;
   subtype MPU_RBAR_ALIAS_SH_Field is HAL.UInt2;
   subtype MPU_RBAR_ALIAS_BADDR_Field is HAL.UInt27;

   type MPU_RBAR_ALIAS_Register is record
      XN    : Boolean;
      AP    : MPU_RBAR_ALIAS_AP_Field;
      SH    : MPU_RBAR_ALIAS_SH_Field;
      --  Holds bits [31:5] of the base address for the selected MPU region
      BADDR : MPU_RBAR_ALIAS_BADDR_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPU_RBAR_ALIAS_Register use record
      XN    at 0 range 0 .. 0;
      AP    at 0 range 1 .. 2;
      SH    at 0 range 3 .. 4;
      BADDR at 0 range 5 .. 31;
   end record;

   subtype MPU_RLAR_ALIAS_AttrIndx_Field is HAL.UInt3;
   subtype MPU_RLAR_ALIAS_LIMIT_Field is HAL.UInt27;

   type MPU_RLAR_ALIAS_Register is record
      EN       : Boolean;
      AttrIndx : MPU_RLAR_ALIAS_AttrIndx_Field;
      PXN      : Boolean;
      --  Holds bits [31:5] of the limit for the selected MPU region
      LIMIT    : MPU_RLAR_ALIAS_LIMIT_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPU_RLAR_ALIAS_Register use record
      EN       at 0 range 0 .. 0;
      AttrIndx at 0 range 1 .. 3;
      PXN      at 0 range 4 .. 4;
      LIMIT    at 0 range 5 .. 31;
   end record;

   --  MPU_MAIR0_Attr array element
   subtype MPU_MAIR0_Attr_Element is HAL.UInt8;

   --  MPU_MAIR0_Attr array
   type MPU_MAIR0_Attr_Field_Array is array (0 .. 3)
     of MPU_MAIR0_Attr_Element
     with Component_Size => 8, Size => 32;

   type MPU_MAIR0_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  Attr as a value
            Val : HAL.UInt32;
         when True =>
            --  Attr as an array
            Arr : MPU_MAIR0_Attr_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPU_MAIR0_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPU_MAIR1_Attr array element
   subtype MPU_MAIR1_Attr_Element is HAL.UInt8;

   --  MPU_MAIR1_Attr array
   type MPU_MAIR1_Attr_Field_Array is array (4 .. 7)
     of MPU_MAIR1_Attr_Element
     with Component_Size => 8, Size => 32;

   type MPU_MAIR1_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  Attr as a value
            Val : HAL.UInt32;
         when True =>
            --  Attr as an array
            Arr : MPU_MAIR1_Attr_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPU_MAIR1_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Memory Protection Unit
   type MPU_Peripheral is record
      --  MPU Type Register
      MPU_TYPE        : aliased MPU_TYPE_Register;
      MPU_CTRL        : aliased MPU_CTRL_Register;
      --  Region select
      MPU_RNR         : aliased MPU_RNR_Register;
      --  MPU Region Base Address Register
      MPU_RBAR        : aliased MPU_RBAR_Register;
      MPU_RLAR        : aliased MPU_RLAR_Register;
      MPU_RBAR_ALIAS1 : aliased MPU_RBAR_ALIAS_Register;
      MPU_RLAR_ALIAS1 : aliased MPU_RLAR_ALIAS_Register;
      MPU_RBAR_ALIAS2 : aliased MPU_RBAR_ALIAS_Register;
      MPU_RLAR_ALIAS2 : aliased MPU_RLAR_ALIAS_Register;
      MPU_RBAR_ALIAS3 : aliased MPU_RBAR_ALIAS_Register;
      MPU_RLAR_ALIAS3 : aliased MPU_RLAR_ALIAS_Register;
      MPU_MAIR0       : aliased MPU_MAIR0_Register;
      MPU_MAIR1       : aliased MPU_MAIR1_Register;
   end record
     with Volatile;

   for MPU_Peripheral use record
      MPU_TYPE        at 16#0# range 0 .. 31;
      MPU_CTRL        at 16#4# range 0 .. 31;
      MPU_RNR         at 16#8# range 0 .. 31;
      MPU_RBAR        at 16#C# range 0 .. 31;
      MPU_RLAR        at 16#10# range 0 .. 31;
      MPU_RBAR_ALIAS1 at 16#14# range 0 .. 31;
      MPU_RLAR_ALIAS1 at 16#18# range 0 .. 31;
      MPU_RBAR_ALIAS2 at 16#1C# range 0 .. 31;
      MPU_RLAR_ALIAS2 at 16#20# range 0 .. 31;
      MPU_RBAR_ALIAS3 at 16#24# range 0 .. 31;
      MPU_RLAR_ALIAS3 at 16#28# range 0 .. 31;
      MPU_MAIR0       at 16#30# range 0 .. 31;
      MPU_MAIR1       at 16#34# range 0 .. 31;
   end record;

   --  Memory Protection Unit
   MPU_Periph : aliased MPU_Peripheral
     with Import, Address => System'To_Address (16#E000ED90#);

end STM32_SVD.MPU;
