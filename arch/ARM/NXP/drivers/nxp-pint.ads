private with NXP_SVD.INPUTMUX;
with HAL.GPIO;

package NXP.Pint is

   type Pint_PinInt is
     (PinInt_0, PinInt_1, PinInt_2,  PinInt_3,  PinInt_4,  PinInt_5,  PinInt_6,  PinInt_7)
     with Size => 3;

   type Mode_Selection is (Pint_Edge, Pint_Level);

   type Edge_Selection is (Pint_Rising, Pint_Falling);

   type Level_Selection is (Pint_Low, Pint_High);

   type Pint_Configuration is record
      Slot  : Pint_PinInt;
      Mode  : Mode_Selection;
      Edge  : Edge_Selection;
      Level : Level_Selection;
   end record;

   procedure Enable_Pint;

   procedure Disable_Pint;

   function Pint_Interrupt_Pending (IntNum : Pint_PinInt) return Boolean;

   procedure Pint_Clear_Interrupt (IntNum : Pint_PinInt);

end NXP.Pint;
