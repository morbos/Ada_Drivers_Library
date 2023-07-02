with System;

with STM32_SVD.CORDIC; use STM32_SVD.CORDIC;

package STM32.CORDIC is

   type SInt32 is range -2 ** 31 .. 2 ** 31 - 1;
   for SInt32'Size use 32;

   type SInt16 is range -2 ** 15 .. 2 ** 15 - 1;
   for SInt16'Size use 16;

   type Q15_Elem is record
      Fraction : UInt15;
      Sign     : Boolean;
   end record
     with Volatile_Full_Access, Object_Size => 16,
     Bit_Order => System.Low_Order_First;

   for Q15_Elem use record
      Fraction       at 0 range  0 .. 14;
      Sign           at 0 range 15 .. 15;
   end record;

   type Q15
     (As_Word : Boolean := False)
   is record
      case As_Word is
         when False =>
            --  MODE as a value
            Val : SInt16;
         when True =>
            Num : Q15_Elem;
      end case;
   end record
     with Unchecked_Union, Size => 16, Volatile_Full_Access, Object_Size => 16;

   type Q31_Elem is record
      Fraction : UInt31;
      Sign     : Boolean;
   end record
     with Volatile_Full_Access, Object_Size => 32,
     Bit_Order => System.Low_Order_First;

   for Q31_Elem use record
      Fraction       at 0 range  0 .. 30;
      Sign           at 0 range 31 .. 31;
   end record;

   type Q31
     (As_DWord : Boolean := False)
   is record
      case As_DWord is
         when False =>
            --  MODE as a value
            Val : SInt32;
         when True =>
            Num : Q31_Elem;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32;

   type Cordic_Selection is
     (Cosine,
      Sine,
      Phase,
      Modulus,
      Arctangent,
      Hyperbolic_Cosine,
      Hyperbolic_Sine,
      Arctanh,
      Natural_Logarithm,
      Square_Root);

   for Cordic_Selection use
     (Cosine              => 0,
      Sine                => 1,
      Phase               => 2,
      Modulus             => 3,
      Arctangent          => 4,
      Hyperbolic_Cosine   => 5,
      Hyperbolic_Sine     => 6,
      Arctanh             => 7,
      Natural_Logarithm   => 8,
      Square_Root         => 9);

   procedure Enable;

   procedure Disable;

   function Float_To_Q15 (From : Float) return Q15;

   function Q15_To_Float (From : Q15) return Float;

   function Float_To_Q31 (From : Float) return Q31;

   function Q31_To_Float (From : Q31) return Float;

   function Degrees_To_Cordic (Degrees : Float) return Q31;

   --  These trig f's take a +/- angle.
   --  ex: 45degrees
   --  is 45.0 / 180.0 => Angle (in Qx repr)
   --  input then is +/- pi normalized to +/- one
   function Sin (Angle : Q15) return Q15;

   function Sin (Angle : Q31) return Q31;

   function Cos (Angle : Q15) return Q15;

   function Cos (Angle : Q31) return Q31;

   function Modulus (X, Y : Q31) return Q31;

   --  This is atan2(y, x) (can prepare modulus also)
   function Phase (X, Y : Q31) return Q31;

   function Atan (X : Q31) return Q31;

   function Cosh (X : Q31) return Q31;

   function Sinh (X : Q31) return Q31;

   function Atanh (X : Q31) return Q31;

   function Ln (X : Q31) return Q31;

   function Sqrt (X : Q31) return Q31;

end STM32.CORDIC;
