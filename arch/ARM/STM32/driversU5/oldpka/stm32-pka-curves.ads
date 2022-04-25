package STM32.PKA.Curves is

   type Init_Mode is (Signing, Validation, Arithmetic, Field_Arithmetic);

   function Common_Init (Mode : Init_Mode) return CurveData;

end STM32.PKA.Curves;
