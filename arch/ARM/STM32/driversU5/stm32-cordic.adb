with STM32_SVD.RCC; use STM32_SVD.RCC;

with Ada.Text_IO;   use Ada.Text_IO;
with Semihosting;

package body STM32.CORDIC is

   procedure Enable
   is
   begin
      RCC_Periph.RCC_AHB1ENR.CORDICEN := True;
   end Enable;

   procedure Disable
   is
   begin
      RCC_Periph.RCC_AHB1ENR.CORDICEN := False;
   end Disable;

   function Float_To_Q15 (From : Float) return Q15
   is
      X   : Q15;
      Tmp : Float;
   begin
      Tmp := From;
      Tmp := Tmp * 32768.0;
      if Tmp > 32767.0 then
         X.Val := 2 ** 15 - 1;
      elsif Tmp < -32768.0 then
         X.Val := -2 ** 15;
      else
         X.Val := SInt16 (Tmp);
      end if;
      return X;
   end Float_To_Q15;

   function Q15_To_Float (From : Q15) return Float
   is
      X    : Q15;
      Tmp  : Float;
   begin
      X := From;
      Tmp := Float (X.Val) / 32768.0;
      if From.Num.Sign then
         Tmp := -Tmp;
      end if;
      return Tmp;
   end Q15_To_Float;

   function Float_To_Q31 (From : Float) return Q31
   is
      X   : Q31;
      Tmp : Float;
   begin
      Tmp := From;
      Tmp := Tmp * 2147483648.0;
      if Tmp > 2147483648.0 then
         X.Val := 2 ** 31 - 1;
      elsif Tmp < -2147483648.0 then
         X.Val := -2 ** 31;
      else
         X.Val := SInt32 (Tmp);
      end if;
      return X;
   end Float_To_Q31;

   function Q31_To_Float (From : Q31) return Float
   is
      X    : Q31;
      Tmp  : Float;
   begin
      X := From;
      Tmp := Float (X.Val) / 2147483648.0;
      return Tmp;
   end Q31_To_Float;

   --  The cordic circuit is +/- n * Pi.
   --  First convert degrees to rads
   --  then compute the fractional value n
   function Degrees_To_Cordic (Degrees : Float) return Q31
   is
      Tmp    : Float;
      Result : Q31;
   begin
      Tmp := Float'Remainder (Degrees, 180.0);
      Tmp := Tmp / 180.0;
      Result := Float_To_Q31 (Tmp);
      return Result;
   end Degrees_To_Cordic;

   function Sin (Angle : Q15) return Q15
   is
      Modulus  : Q15;
      Value    : UInt32;
      Result   : Q15;
   begin
      Modulus := Float_To_Q15 (0.5);
      CORDIC_Periph.CSR.FUNC      := UInt4 (Sine'Enum_Rep);
      CORDIC_Periph.CSR.PRECISION := 5;
      CORDIC_Periph.CSR.SCALE     := 0;
      CORDIC_Periph.CSR.NARGS     := False; --  2 but packed into one 32bit arg
      CORDIC_Periph.CSR.ARGSIZE   := True;
      CORDIC_Periph.CSR.RESSIZE   := True;
      Value := Shift_Left (UInt32 (Modulus.Val), 16) or UInt32 (Angle.Val);
      CORDIC_Periph.WDATA := Value;
      loop
         exit when CORDIC_Periph.CSR.RRDY;
      end loop;
      Result.Val := SInt16 (CORDIC_Periph.RDATA and 16#ffff#);
      return Result;
   end Sin;

   function Sin (Angle : Q31) return Q31
   is
      Value    : SInt32;
      Raw      : UInt32;
      for Raw'Address use Value'Address;
      Result   : Q31;
   begin
      CORDIC_Periph.CSR.FUNC      := UInt4 (Sine'Enum_Rep);
      CORDIC_Periph.CSR.PRECISION := 6;
      CORDIC_Periph.CSR.SCALE     := 0;
      CORDIC_Periph.CSR.NARGS     := False;
      CORDIC_Periph.CSR.ARGSIZE   := False;
      CORDIC_Periph.CSR.RESSIZE   := False;
      Value := Angle.Val;
      CORDIC_Periph.WDATA := Raw;
      loop
         exit when CORDIC_Periph.CSR.RRDY;
      end loop;
      Raw := CORDIC_Periph.RDATA;
      Result.Val := Value;
      return Result;
   end Sin;

   function Cos (Angle : Q15) return Q15
   is
      Modulus  : Q15;
      Value    : UInt32;
      Result   : Q15;
   begin
      Modulus := Float_To_Q15 (0.5);
      CORDIC_Periph.CSR.FUNC      := UInt4 (Cosine'Enum_Rep);
      CORDIC_Periph.CSR.PRECISION := 5;
      CORDIC_Periph.CSR.SCALE     := 0;
      CORDIC_Periph.CSR.NARGS     := False; --  2 but packed into one 32bit arg
      CORDIC_Periph.CSR.ARGSIZE   := True;
      CORDIC_Periph.CSR.RESSIZE   := True;
      Value := Shift_Left (UInt32 (Modulus.Val), 16) or UInt32 (Angle.Val);
      CORDIC_Periph.WDATA := Value;
      loop
         exit when CORDIC_Periph.CSR.RRDY;
      end loop;
      Result.Val := SInt16 (CORDIC_Periph.RDATA and 16#ffff#);
      return Result;
   end Cos;

   function Cos (Angle : Q31) return Q31
   is
      Value    : SInt32;
      Raw      : UInt32;
      for Raw'Address use Value'Address;
      Result   : Q31;
   begin
      CORDIC_Periph.CSR.FUNC      := UInt4 (Cosine'Enum_Rep);
      CORDIC_Periph.CSR.PRECISION := 6;
      CORDIC_Periph.CSR.SCALE     := 0;
      CORDIC_Periph.CSR.NARGS     := False;
      CORDIC_Periph.CSR.ARGSIZE   := False;
      CORDIC_Periph.CSR.RESSIZE   := False;
      Value := Angle.Val;
      CORDIC_Periph.WDATA := Raw;
      loop
         exit when CORDIC_Periph.CSR.RRDY;
      end loop;
      Raw := CORDIC_Periph.RDATA;
      Result.Val := Value;
      return Result;
   end Cos;

   function Modulus (X, Y : Q31) return Q31
   is
      Value    : SInt32;
      Raw      : UInt32;
      for Raw'Address use Value'Address;
      Result   : Q31;
   begin
      CORDIC_Periph.CSR.FUNC      := UInt4 (Modulus'Enum_Rep);
      CORDIC_Periph.CSR.PRECISION := 6;
      CORDIC_Periph.CSR.SCALE     := 0;
      CORDIC_Periph.CSR.NARGS     := True;
      CORDIC_Periph.CSR.ARGSIZE   := False;
      CORDIC_Periph.CSR.RESSIZE   := False;
      Value := X.Val;
      CORDIC_Periph.WDATA := Raw;
      Value := Y.Val;
      CORDIC_Periph.WDATA := Raw;
      loop
         exit when CORDIC_Periph.CSR.RRDY;
      end loop;
      Raw := CORDIC_Periph.RDATA;
      Result.Val := Value;
      return Result;
   end Modulus;

   function Phase (X, Y : Q31) return Q31
   is
      Value    : SInt32;
      Raw      : UInt32;
      for Raw'Address use Value'Address;
      Result   : Q31;
   begin
      CORDIC_Periph.CSR.FUNC      := UInt4 (Phase'Enum_Rep);
      CORDIC_Periph.CSR.PRECISION := 6;
      CORDIC_Periph.CSR.SCALE     := 0;
      CORDIC_Periph.CSR.NARGS     := True;
      CORDIC_Periph.CSR.ARGSIZE   := False;
      CORDIC_Periph.CSR.RESSIZE   := False;
      Value := X.Val;
      CORDIC_Periph.WDATA := Raw;
      Value := Y.Val;
      CORDIC_Periph.WDATA := Raw;
      loop
         exit when CORDIC_Periph.CSR.RRDY;
      end loop;
      Raw := CORDIC_Periph.RDATA;
      Result.Val := Value;
      return Result;
   end Phase;

   --  The result needs to be scaled by
   --  2^scale * Pi
   --  ... should we do that here..?
   function Atan (X : Q31) return Q31
   is
      Value    : SInt32;
      Raw      : UInt32;
      for Raw'Address use Value'Address;
      Result   : Q31;
   begin
      CORDIC_Periph.CSR.FUNC      := UInt4 (Arctangent'Enum_Rep);
      CORDIC_Periph.CSR.PRECISION := 6;
      CORDIC_Periph.CSR.SCALE     := 0;
      CORDIC_Periph.CSR.NARGS     := False;
      CORDIC_Periph.CSR.ARGSIZE   := False;
      CORDIC_Periph.CSR.RESSIZE   := False;
      Value := X.Val;
      CORDIC_Periph.WDATA := Raw;
      loop
         exit when CORDIC_Periph.CSR.RRDY;
      end loop;
      Raw := CORDIC_Periph.RDATA;
      Result.Val := Value;
      return Result;
   end Atan;

   function Cosh (X : Q31) return Q31
   is
      Value    : SInt32;
      Raw      : UInt32;
      for Raw'Address use Value'Address;
      Result   : Q31;
   begin
      CORDIC_Periph.CSR.FUNC      := UInt4 (Hyperbolic_Cosine'Enum_Rep);
      CORDIC_Periph.CSR.PRECISION := 6;
      CORDIC_Periph.CSR.SCALE     := 0;
      CORDIC_Periph.CSR.NARGS     := False;
      CORDIC_Periph.CSR.ARGSIZE   := False;
      CORDIC_Periph.CSR.RESSIZE   := False;
      Value := X.Val;
      CORDIC_Periph.WDATA := Raw;
      loop
         exit when CORDIC_Periph.CSR.RRDY;
      end loop;
      Raw := CORDIC_Periph.RDATA;
      Result.Val := Value;
      return Result;
   end Cosh;

   function Sinh (X : Q31) return Q31
   is
      Value    : SInt32;
      Raw      : UInt32;
      for Raw'Address use Value'Address;
      Result   : Q31;
   begin
      CORDIC_Periph.CSR.FUNC      := UInt4 (Hyperbolic_Sine'Enum_Rep);
      CORDIC_Periph.CSR.PRECISION := 6;
      CORDIC_Periph.CSR.SCALE     := 0;
      CORDIC_Periph.CSR.NARGS     := False;
      CORDIC_Periph.CSR.ARGSIZE   := False;
      CORDIC_Periph.CSR.RESSIZE   := False;
      Value := X.Val;
      CORDIC_Periph.WDATA := Raw;
      loop
         exit when CORDIC_Periph.CSR.RRDY;
      end loop;
      Raw := CORDIC_Periph.RDATA;
      Result.Val := Value;
      return Result;
   end Sinh;

   function Atanh (X : Q31) return Q31
   is
      Value    : SInt32;
      Raw      : UInt32;
      for Raw'Address use Value'Address;
      Result   : Q31;
   begin
      CORDIC_Periph.CSR.FUNC      := UInt4 (Arctanh'Enum_Rep);
      CORDIC_Periph.CSR.PRECISION := 6;
      CORDIC_Periph.CSR.SCALE     := 1;
      CORDIC_Periph.CSR.NARGS     := False;
      CORDIC_Periph.CSR.ARGSIZE   := False;
      CORDIC_Periph.CSR.RESSIZE   := False;
      Value := X.Val;
      CORDIC_Periph.WDATA := Raw;
      loop
         exit when CORDIC_Periph.CSR.RRDY;
      end loop;
      Raw := CORDIC_Periph.RDATA;
      Result.Val := Value;
      return Result;
   end Atanh;

   function Ln (X : Q31) return Q31
   is
      Value    : SInt32;
      Raw      : UInt32;
      for Raw'Address use Value'Address;
      Result   : Q31;
   begin
      CORDIC_Periph.CSR.FUNC      := UInt4 (Natural_Logarithm'Enum_Rep);
      CORDIC_Periph.CSR.PRECISION := 6;
      CORDIC_Periph.CSR.SCALE     := 1;
      CORDIC_Periph.CSR.NARGS     := False;
      CORDIC_Periph.CSR.ARGSIZE   := False;
      CORDIC_Periph.CSR.RESSIZE   := False;
      Value := X.Val;
      CORDIC_Periph.WDATA := Raw;
      loop
         exit when CORDIC_Periph.CSR.RRDY;
      end loop;
      Raw := CORDIC_Periph.RDATA;
      Result.Val := Value;
      return Result;
   end Ln;

   function Sqrt (X : Q31) return Q31
   is
      Value    : SInt32;
      Raw      : UInt32;
      for Raw'Address use Value'Address;
      Result   : Q31;
   begin
      CORDIC_Periph.CSR.FUNC      := UInt4 (Square_Root'Enum_Rep);
      CORDIC_Periph.CSR.PRECISION := 6;
      CORDIC_Periph.CSR.SCALE     := 0;
      CORDIC_Periph.CSR.NARGS     := False;
      CORDIC_Periph.CSR.ARGSIZE   := False;
      CORDIC_Periph.CSR.RESSIZE   := False;
      Value := X.Val;
      CORDIC_Periph.WDATA := Raw;
      loop
         exit when CORDIC_Periph.CSR.RRDY;
      end loop;
      Raw := CORDIC_Periph.RDATA;
      Result.Val := Value;
      return Result;
   end Sqrt;

end STM32.CORDIC;
