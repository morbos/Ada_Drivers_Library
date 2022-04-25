with System;

package body STM32.MPU is

   type RLAR_Overlay
     (As_32Bit : Boolean := False)
   is record
      case As_32Bit is
         when False =>
            Val : HAL.UInt32;
         when True =>
            Reg : MPU_RLAR_Register;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   procedure Add_Region (Region_Num : MPU_Regions;
                         Addr       : UInt32;
                         Size       : UInt32;
                         AttIdx     : UInt3)
   is
      A : UInt32;
      X : MPU_RBAR_Register;
      for X'Address use A'Address;
      L : RLAR_Overlay;
   begin
      A := Addr;
      MPU_Periph.MPU_RNR.REGION := UInt8 (Region_Num);
      MPU_Periph.MPU_RBAR.BADDR := X.BADDR;
      L.Val := UInt32 (Addr);
      L.Val := L.Val + Size;
      L.Reg.AttrIndx := AttIdx;
      L.Reg.EN := True;
      MPU_Periph.MPU_RLAR := L.Reg;
   end Add_Region;

   procedure Add_Attrib (AttrIdx : UInt3; Attrib : Attr_Type)
   is
      X : UInt8;
      LAttr : Attr_Type := Attrib;
      for X'Address use LAttr'Address;
   begin
      if AttrIdx < 4 then
         MPU_Periph.MPU_MAIR (0).Arr (Integer (AttrIdx)) := X;
      else
         MPU_Periph.MPU_MAIR (1).Arr (Integer (AttrIdx)) := X;
      end if;
   end Add_Attrib;

   procedure Enable_MPU
   is
   begin
      MPU_Periph.MPU_CTRL.ENABLE := True;
   end Enable_MPU;

end STM32.MPU;
