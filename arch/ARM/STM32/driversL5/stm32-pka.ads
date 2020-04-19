with System;

with STM32.Device;   use STM32.Device;
with STM32_SVD;      use STM32_SVD;
with STM32_SVD.PKA;  use STM32_SVD.PKA;

generic
   Curve_Name  : String;
   Num_Bits    : UInt32;
   Hash_Size   : UInt32 := 256;
package STM32.PKA is
   N_By_8  : constant UInt32 := (Num_Bits / 8) + (if (Num_Bits mod 8) > 0 then 1 else 0);
   N_By_32 : constant UInt32 := (Num_Bits / 32) + (if (Num_Bits mod 32) > 0 then 1 else 0);
   Rem_By_32 : constant UInt32 := (Num_Bits mod 32);
   Hash_N_By_8 : UInt32 := Hash_Size / 8;

   subtype ECDSA_Array is UInt8_Array (0 .. Integer (N_By_8 - 1));
   subtype ECDSA_Key is ECDSA_Array;
   subtype ECDSA_Hash is UInt8_Array (0 .. Integer (Hash_N_By_8 - 1));
   subtype ECDSA_Rand is ECDSA_Array;

   type ECDSA_Signature is record
      R : ECDSA_Array;
      S : ECDSA_Array;
   end record;

   type ECDSA_PublicKey is record
      X : ECDSA_Array;
      Y : ECDSA_Array;
   end record;

   subtype ECDSA_String is String (1 .. Integer ((2 * N_By_8)));
   subtype ECDSA_KeyStr is ECDSA_String;
   subtype ECDSA_HashStr is String (1 .. Integer ((2 * Hash_N_By_8)));
   subtype ECDSA_RandStr is ECDSA_String;

   type ECDSA_SignatureStr is record
      R : ECDSA_String;
      S : ECDSA_String;
   end record;

   type ECDSA_PublicKeyStr is record
      X : ECDSA_String;
      Y : ECDSA_String;
   end record;

   subtype Digest_Buffer is UInt8_Array (0 .. Integer (N_By_8 - 1));
   subtype Digest_BufferStr is String (1 .. Integer ((2 * N_By_8)));

   PKA : aliased PKA_Peripheral with Import, Volatile, Address => S_NS_Periph (PKA_Base);

   --  PKA RAM
   --  The PKA RAM is mapped at the offset address of 0x0400 compared to the PKA base
   --                vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
   --  address. Only 32-bit word single accesses are supported, through PKA.AHB interface.
   --                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   --  RAM size is 3576 bytes (max word offset: 0x11F4)
   type ECDSA_Sign_Ram is record
      Order_Num_Bits : UInt32;
      Mod_Num_Bits   : UInt32;
      A_Coeff_Sign   : UInt32;
      A_Coeff        : UInt32_Array (0 .. 20);
      Mod_GF         : UInt32_Array (0 .. 41);
      K              : UInt32_Array (0 .. 20);
      X              : UInt32_Array (0 .. 20);
      Y              : UInt32_Array (0 .. 20);
      R              : UInt32_Array (0 .. 20);
      S              : UInt32_Array (0 .. 20);
      B_Coeff        : UInt32_Array (0 .. 20);
      Hash           : UInt32_Array (0 .. 20);
      PrivateKey     : UInt32_Array (0 .. 21);
      Order_N        : UInt32_Array (0 .. 20);
      Error          : UInt32;
      Final_Pt_X     : UInt32_Array (0 .. 20);
      Final_Pt_Y     : UInt32_Array (0 .. 20);
   end record;

   for ECDSA_Sign_Ram use record
      Order_Num_Bits at 16#0400# range 0 .. 31;
      Mod_Num_Bits   at 16#0404# range 0 .. 31;
      A_Coeff_Sign   at 16#0408# range 0 .. 31;
      A_Coeff        at 16#040c# range 0 .. 671;
      Mod_GF         at 16#0460# range 0 .. 1343;
      K              at 16#0508# range 0 .. 671;
      X              at 16#055c# range 0 .. 671;
      Y              at 16#05b0# range 0 .. 671;
      R              at 16#0700# range 0 .. 671;
      S              at 16#0754# range 0 .. 671;
      B_Coeff        at 16#07fc# range 0 .. 671;
      Hash           at 16#0de8# range 0 .. 671;
      PrivateKey     at 16#0e3c# range 0 .. 703;
      Order_N        at 16#0e94# range 0 .. 671;
      Error          at 16#0ee8# range 0 .. 31;
      Final_Pt_X     at 16#103c# range 0 .. 671;
      Final_Pt_Y     at 16#1090# range 0 .. 671;
   end record;

   type ECDSA_Verify_Ram is record
      Order_Num_Bits : UInt32;
      A_Coeff_Sign   : UInt32;
      A_Coeff        : UInt32_Array (0 .. 20);
      Mod_Num_Bits   : UInt32;
      Mod_GF         : UInt32_Array (0 .. 61);
      Result         : UInt32;
      IP_X           : UInt32_Array (0 .. 20);
      IP_Y           : UInt32_Array (0 .. 20);
      S              : UInt32_Array (0 .. 20);
      Order_N        : UInt32_Array (0 .. 20);
      PK_X           : UInt32_Array (0 .. 20);
      PK_Y           : UInt32_Array (0 .. 20);
      Hash           : UInt32_Array (0 .. 20);
      R              : UInt32_Array (0 .. 20);
   end record;

   for ECDSA_Verify_Ram use record
      Order_Num_Bits at 16#0404# range 0 .. 31;
      A_Coeff_Sign   at 16#045c# range 0 .. 31;
      A_Coeff        at 16#0460# range 0 .. 671;
      Mod_Num_Bits   at 16#04b4# range 0 .. 31;
      Mod_GF         at 16#04b8# range 0 .. 1983;
      Result         at 16#05b0# range 0 .. 31;
      IP_X           at 16#05e8# range 0 .. 671;
      IP_Y           at 16#063c# range 0 .. 671;
      S              at 16#0a44# range 0 .. 671;
      Order_N        at 16#0d5c# range 0 .. 671;
      PK_X           at 16#0f40# range 0 .. 671;
      PK_Y           at 16#0f94# range 0 .. 671;
      Hash           at 16#0fe8# range 0 .. 671;
      R              at 16#1098# range 0 .. 671;
   end record;

   type ECDSA_Ram is record
      Op_Len         : UInt32;
      N_Bits         : UInt32;
      M_Param        : UInt32_Array (0 .. 20);
      A              : UInt32_Array (0 .. 20);
      T1             : UInt32_Array (0 .. 20);
      T2             : UInt32_Array (0 .. 20);
      T3             : UInt32_Array (0 .. 20);
      B              : UInt32_Array (0 .. 20);
      Result         : UInt32_Array (0 .. 20);
      Mod_GF         : UInt32_Array (0 .. 20);
   end record;

   for ECDSA_Ram use record
      Op_Len         at 16#0400# range 0 .. 31;
      N_Bits         at 16#0404# range 0 .. 31;
      M_Param        at 16#0594# range 0 .. 671;
      A              at 16#08b4# range 0 .. 671;
      T1             at 16#0908# range 0 .. 671;
      T2             at 16#095c# range 0 .. 671;
      T3             at 16#09b0# range 0 .. 671;
      B              at 16#0a44# range 0 .. 671;
      Result         at 16#0bd0# range 0 .. 671;
      Mod_GF         at 16#0d5c# range 0 .. 671;
   end record;

   type ECDSA_Point_Ram is record
      Scalar_Len     : UInt32;
      N_Bits         : UInt32;
      A_Sign         : UInt32;
      A_Coeff        : UInt32_Array (0 .. 20);
      Curve_Mod      : UInt32_Array (0 .. 20);
      Scalar         : UInt32_Array (0 .. 20);
      X              : UInt32_Array (0 .. 20);
      Y              : UInt32_Array (0 .. 20);
   end record;

   for ECDSA_Point_Ram use record
      Scalar_Len     at 16#0400# range 0 .. 31;
      N_Bits         at 16#0404# range 0 .. 31;
      A_Sign         at 16#0408# range 0 .. 31;
      A_Coeff        at 16#040c# range 0 .. 671;
      Curve_Mod      at 16#0460# range 0 .. 671;
      Scalar         at 16#0508# range 0 .. 671;
      X              at 16#055c# range 0 .. 671;
      Y              at 16#05b0# range 0 .. 671;
   end record;

   type All_PKA_Ram is record
      RAM            : UInt32_Array (0 .. 893);
   end record;

   for All_PKA_Ram use record
      RAM            at 16#400# range 0 .. 28607;
   end record;

   type PKA_Parameters is
     (
      Montgomery_Parameter_Computation_With_Modular_Exponentiation,
      Montgomery_Parameter_Computation_Only,
      Modular_Exponentiation_Only,
      RSA_CRT_Exponentiation,
      Modular_Inversion,
      Arithmetic_Addition,
      Arithmetic_Subtraction,
      Arithmetic_Multiplication,
      Arithmetic_Comparison,
      Modular_Reduction,
      Modular_Addition,
      Modular_Subtraction,
      Montgomery_Multiplication,
      FP_Scalar_Multiplication,
      FP_Scalar_Multiplication_Fast,
      ECDSA_Sign,
      ECDSA_Verification,
      Point_On_Elliptic_Curve_Fp_Check)
     with Size => 6;

   for PKA_Parameters use
     (
      Montgomery_Parameter_Computation_With_Modular_Exponentiation     => 2#000000#,
      Montgomery_Parameter_Computation_Only                            => 2#000001#,
      Modular_Exponentiation_Only                                      => 2#000010#,
      RSA_CRT_Exponentiation                                           => 2#000111#,
      Modular_Inversion                                                => 2#001000#,
      Arithmetic_Addition                                              => 2#001001#,
      Arithmetic_Subtraction                                           => 2#001010#,
      Arithmetic_Multiplication                                        => 2#001011#,
      Arithmetic_Comparison                                            => 2#001100#,
      Modular_Reduction                                                => 2#001101#,
      Modular_Addition                                                 => 2#001110#,
      Modular_Subtraction                                              => 2#001111#,
      Montgomery_Multiplication                                        => 2#010000#,
      FP_Scalar_Multiplication                                         => 2#100000#,
      FP_Scalar_Multiplication_Fast                                    => 2#100010#,
      ECDSA_Sign                                                       => 2#100100#,
      ECDSA_Verification                                               => 2#100110#,
      Point_On_Elliptic_Curve_Fp_Check                                 => 2#101000#);

   type CurveData is record
      P : String (1 .. Integer (N_By_8 * 2));
      X : String (1 .. Integer (N_By_8 * 2));
      Y : String (1 .. Integer (N_By_8 * 2));
      A : String (1 .. Integer (N_By_8 * 2));
      N : String (1 .. Integer (N_By_8 * 2));
   end record;

   type Init_Mode is (Signing, Validation, Arithmetic, Field_Arithmetic);
   procedure Copy_S_To_U32 (S : String; To : out UInt32_Array);
   procedure Copy_U8_To_U32 (From : UInt8_Array; To : out UInt32_Array);
   procedure Copy_U32_To_U8 (From : UInt32_Array; To : out UInt8_Array);
   procedure Copy_U8_To_S (From : UInt8; To : out ECDSA_String; Offset : UInt32);
   procedure Copy_U32_To_S (From : UInt32_Array; To : out ECDSA_String);
   procedure Enable_Pka;
   procedure Disable_Pka;
   procedure StartPKA (Mode : PKA_Parameters);
   function Check_Errors return Boolean;
   procedure Clear_Flags;
   procedure Clear_Ram;
   function Common_Init (Mode : Init_Mode) return CurveData;
   function Normalize (Digest : ECDSA_Hash) return Digest_Buffer;
   function Normalize (Digest : ECDSA_HashStr) return Digest_BufferStr;
   function ECDSA_Sign (Private_Key : ECDSA_Key;
                        H           : ECDSA_Hash;
                        K           : ECDSA_Rand;
                        Signature   : out ECDSA_Signature) return Boolean;

   function ECDSA_Sign (Private_Key : ECDSA_KeyStr;
                        H           : ECDSA_HashStr;
                        K           : ECDSA_RandStr;
                        Signature   : out ECDSA_SignatureStr) return Boolean;

   function ECDSA_Valid (Public_Key  : ECDSA_PublicKey;
                         Digest      : ECDSA_Hash;
                         Signature   : ECDSA_Signature) return Boolean;

   function ECDSA_Valid (Public_Key  : ECDSA_PublicKeyStr;
                         Digest      : ECDSA_HashStr;
                         Signature   : ECDSA_SignatureStr) return Boolean;

   procedure ECDSA_Math (Work : String;
                         A   :  ECDSA_String;
                         B   :  ECDSA_String := (1 => '0', others => '0');
                         C   :  ECDSA_String := (1 => '0', others => '0');
                         O1  : out ECDSA_String;
                         O2  : out ECDSA_String);

   procedure ECDSA_Point_Mult (X      : ECDSA_String;
                               Y      : ECDSA_String;
                               Scalar : ECDSA_String;
                               X_Res  : out ECDSA_String;
                               Y_Res  : out ECDSA_String);

   procedure ECDSA_Point_Mult (Scalar : ECDSA_String;
                               X_Res  : out ECDSA_String;
                               Y_Res  : out ECDSA_String);

   function Make_Random_Group_String (NClamp : Boolean := False) return ECDSA_String;

   function Make_Public_Key_String (PrivateKey : ECDSA_String) return ECDSA_PublicKeyStr;

end STM32.PKA;
