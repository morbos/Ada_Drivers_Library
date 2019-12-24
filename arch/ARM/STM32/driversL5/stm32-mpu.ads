with System;

with STM32_SVD.MPU; use STM32_SVD.MPU;

package STM32.MPU is

   type MPU_Regions is range 0 .. 7;

   type Attr_Type is record
      Outer : UInt4;
      Inner : UInt4;
   end record
     with Pack, Size => 8;

   procedure Add_Region (Region_Num : MPU_Regions;
                         Addr       : UInt32;
                         Size       : UInt32;
                         AttIdx     : UInt3);

   procedure Add_Attrib (AttrIdx : UInt3; Attrib : Attr_Type);

   procedure Enable_MPU;

end STM32.MPU;
