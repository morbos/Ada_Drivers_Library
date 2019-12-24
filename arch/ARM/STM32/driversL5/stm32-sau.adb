package body STM32.SAU is

   type RLAR_Overlay
     (As_32Bit : Boolean := False)
   is record
      case As_32Bit is
         when False =>
            Val : HAL.UInt32;
         when True =>
            Reg : SAU_RLAR_Register;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   procedure Add_Region (Region_Num : SAU_Regions;
                         Addr       : UInt32;
                         Size       : UInt32;
                         NSC        : Boolean)
   is
      A : UInt32;
      X : SAU_RBAR_Register;
      for X'Address use A'Address;
      L : RLAR_Overlay;
   begin
      A := Addr;
      SAU_Periph.SAU_RNR.REGION := UInt8 (Region_Num);
      SAU_Periph.SAU_RBAR.BADDR := X.BADDR;
      L.Val := UInt32 (Addr);
      L.Val := L.Val + Size;
      L.Reg.NSC := NSC;
      L.Reg.ENABLE := True;
      SAU_Periph.SAU_RLAR := L.Reg;
   end Add_Region;

   procedure Enable_SAU
   is
   begin
      SAU_Periph.SAU_CTRL.ENABLE := True;
   end Enable_SAU;

   procedure All_NS
   is
   begin
      SAU_Periph.SAU_CTRL.ENABLE := False;
      SAU_Periph.SAU_CTRL.ALLNS := True;
   end All_NS;

end STM32.SAU;
