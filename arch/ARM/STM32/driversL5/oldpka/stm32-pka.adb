with System;

package body STM32.PKA is

   function ECDSA_Sign (Private_Key : ECDSA_Key;
                        H           : ECDSA_Hash;
                        K           : ECDSA_Rand;
                        Signature   : out ECDSA_Signature) return Boolean
   is
      ECDSA : aliased ECDSA_Sign_Ram with Import, Address => S_NS_Periph (PKA_Base);
      Result : Boolean := True;
      Err : UInt32;
      Gap : UInt32 := (Num_Bits - Hash_Size) / 8;
      Curve  : CurveData;
   begin
      Disable_Pka;
      Clear_Ram;
      Curve := Common_Init (Signing);
      Copy_U8_To_U32 (K, ECDSA.K);
      Copy_U8_To_U32 (UInt8_Array (Normalize (H)), ECDSA.Hash);
      Copy_U8_To_U32 (UInt8_Array (Private_Key), ECDSA.PrivateKey);

      ECDSA.Order_Num_Bits := Num_Bits;
      ECDSA.Mod_Num_Bits   := Num_Bits;

      Clear_Flags;

      StartPKA (ECDSA_Sign);

      --  sequential here vvv
      if Check_Errors then
         Result := False;
      end if;
      Clear_Flags;

      Copy_U32_To_U8 (ECDSA.R, Signature.R);
      Copy_U32_To_U8 (ECDSA.S, Signature.S);

      --  vvvvv if you don't disable here...
      --  The the Error read below can get bad data
      Disable_Pka;
      --   or vvvvv he will flip a flag indicating ram error(!)
      Err := ECDSA.Error;
      if Err /= 0 then
         Result := False;
      end if;
      return Result;
   end ECDSA_Sign;

   function ECDSA_Sign (Private_Key : ECDSA_KeyStr;
                        H           : ECDSA_HashStr;
                        K           : ECDSA_RandStr;
                        Signature   : out ECDSA_SignatureStr) return Boolean
   is
      ECDSA : aliased ECDSA_Sign_Ram with Import, Address => S_NS_Periph (PKA_Base);
      Result : Boolean := True;
      Err : UInt32;
      Curve  : CurveData;
   begin
      Disable_Pka;
      Clear_Ram;
      Curve := Common_Init (Signing);
      Copy_S_To_U32 (K, ECDSA.K);
      Copy_S_To_U32 (Normalize (H), ECDSA.Hash);
      Copy_S_To_U32 (Private_Key, ECDSA.PrivateKey);

      ECDSA.Order_Num_Bits := Num_Bits;
      ECDSA.Mod_Num_Bits   := Num_Bits;

      Clear_Flags;

      StartPKA (ECDSA_Sign);

      --  sequential here vvv
      if Check_Errors then
         Result := False;
      end if;
      Clear_Flags;

      Copy_U32_To_S (ECDSA.R, Signature.R);
      Copy_U32_To_S (ECDSA.S, Signature.S);

      --  vvvvv if you don't disable here...
      --  The the Error read below can get bad data
      Disable_Pka;
      --   or vvvvv he will flip a flag indicating ram error(!)
      Err := ECDSA.Error;
      if Err /= 0 then
         Result := False;
      end if;
      return Result;
   end ECDSA_Sign;

   function ECDSA_Valid (Public_Key  : ECDSA_PublicKey;
                         Digest      : ECDSA_Hash;
                         Signature   : ECDSA_Signature) return Boolean
   is
      ECDSA : aliased ECDSA_Verify_Ram with Import, Address => S_NS_Periph (PKA_Base);
      Result : Boolean := True;
      Curve  : CurveData;
   begin
      Disable_Pka;
      Clear_Ram;
      Curve := Common_Init (Validation);
      Copy_U8_To_U32 (UInt8_Array (Public_Key.X), ECDSA.PK_X);
      Copy_U8_To_U32 (UInt8_Array (Public_Key.Y), ECDSA.PK_Y);
      Copy_U8_To_U32 (Signature.R, ECDSA.R);
      Copy_U8_To_U32 (Signature.S, ECDSA.S);
      Copy_U8_To_U32 (UInt8_Array (Normalize (Digest)), ECDSA.Hash);

      ECDSA.Order_Num_Bits := Num_Bits;
      ECDSA.Mod_Num_Bits   := Num_Bits;

      Clear_Flags;

      StartPKA (ECDSA_Verification);

      --  sequential here vvv
      if Check_Errors then
         Result := False;
      end if;
      Clear_Flags;
      --  vvvvv if you don't disable here...
      --  The the Result read below can get bad data
      Disable_Pka;
      return Result and (ECDSA.Result = 0);
   end ECDSA_Valid;


   function ECDSA_Valid (Public_Key  : ECDSA_PublicKeyStr;
                         Digest      : ECDSA_HashStr;
                         Signature   : ECDSA_SignatureStr) return Boolean
   is
      ECDSA : aliased ECDSA_Verify_Ram with Import, Address => S_NS_Periph (PKA_Base);
      Result : Boolean := True;
      Curve  : CurveData;
   begin
      Disable_Pka;
      Clear_Ram;
      Curve := Common_Init (Validation);

      Copy_S_To_U32 (Public_Key.X, ECDSA.PK_X);
      Copy_S_To_U32 (Public_Key.Y, ECDSA.PK_Y);
      Copy_S_To_U32 (Signature.R, ECDSA.R);
      Copy_S_To_U32 (Signature.S, ECDSA.S);
      Copy_S_To_U32 (Normalize (Digest), ECDSA.Hash);

      ECDSA.Order_Num_Bits := Num_Bits;
      ECDSA.Mod_Num_Bits   := Num_Bits;

      Clear_Flags;

      StartPKA (ECDSA_Verification);

      --  sequential here vvv
      if Check_Errors then
         Result := False;
      end if;
      Clear_Flags;
      --  vvvvv if you don't disable here...
      --  The the Result read below can get bad data
      Disable_Pka;
      return Result and (ECDSA.Result = 0);
   end ECDSA_Valid;

   procedure ECDSA_Math (Work : String;
                         A   :  ECDSA_String;
                         B   :  ECDSA_String := (1 => '0', others => '0');
                         C   :  ECDSA_String := (1 => '0', others => '0');
                         O1  : out ECDSA_String;
                         O2  : out ECDSA_String)
   is
      ECDSA : aliased ECDSA_Ram with Import, Address => S_NS_Periph (PKA_Base);
      Curve  : CurveData;
   begin
      Clear_Ram;
      Curve := Common_Init (Field_Arithmetic);
      ECDSA.N_Bits := Num_Bits;
      for I in Work'Range loop
         Disable_Pka;
         Clear_Flags;
         case Work (I) is
            when 'A' =>
               Copy_S_To_U32 (A, ECDSA.A);
            when 'B' =>
               Copy_S_To_U32 (B, ECDSA.B);
            when 'C' =>
               Copy_S_To_U32 (C, ECDSA.A);
            when 'b' =>
               Copy_S_To_U32 (B, ECDSA.A);
            when 'c' =>
               Copy_S_To_U32 (C, ECDSA.B);
            when 'O' => --  output to A
               ECDSA.A := ECDSA.Result;
            when 'o' => --  output to B
               ECDSA.B := ECDSA.Result;
            when '+' =>
               StartPKA (Arithmetic_Addition);
            when '-' =>
               StartPKA (Arithmetic_Subtraction);
            when '*' =>
               StartPKA (Arithmetic_Multiplication);
            when '|' =>
               ECDSA.Op_Len := Num_Bits * 2;
               StartPKA (Modular_Reduction);
            when '%' =>
               ECDSA.Op_Len := Num_Bits;
               StartPKA (Modular_Reduction);
            when '&' =>
               Copy_S_To_U32 (Curve.N, ECDSA.Mod_GF);
               StartPKA (Modular_Subtraction);
            when 'm' =>
               StartPKA (Montgomery_Multiplication);
            when 'N' =>
               Copy_S_To_U32 (Curve.N, ECDSA.A);
            when 'n' =>
               Copy_S_To_U32 (Curve.N, ECDSA.B);
            when 'P' =>
               Copy_S_To_U32 (Curve.P, ECDSA.A);
            when 'p' =>
               Copy_S_To_U32 (Curve.P, ECDSA.B);
            when 'R' =>
               ECDSA.A := ECDSA.M_Param;
            when 'r' =>
               ECDSA.B := ECDSA.M_Param;
            when '!' =>
               StartPKA (Modular_Inversion);
            when '=' =>
               Copy_U32_To_S (ECDSA.T1, O1);
            when '^' =>
               Copy_U32_To_S (ECDSA.T2, O2);
            when '1' =>
               ECDSA.T1 := ECDSA.Result;
            when '2' =>
               ECDSA.T2 := ECDSA.Result;
            when '3' =>
               ECDSA.T3 := ECDSA.Result;
            when '4' =>
               ECDSA.A := ECDSA.T1;
            when '5' =>
               ECDSA.A := ECDSA.T2;
            when '6' =>
               ECDSA.B := ECDSA.T3;
            when '7' =>
               ECDSA.B := ECDSA.T1;
            when '8' =>
               ECDSA.B := ECDSA.T2;
            when '9' =>
               ECDSA.B := ECDSA.T3;
            when others =>
               loop
                  null;
               end loop;
         end case;
      end loop;
   end ECDSA_Math;

   procedure ECDSA_Point_Mult (X      : ECDSA_String;
                               Y      : ECDSA_String;
                               Scalar : ECDSA_String;
                               X_Res  : out ECDSA_String;
                               Y_Res  : out ECDSA_String)
   is
      ECDSA : aliased ECDSA_Point_Ram with Import, Address => S_NS_Periph (PKA_Base);
      Curve  : CurveData;
   begin
      Enable_Pka;
      Curve := Common_Init (Field_Arithmetic);
      Clear_Ram;
      ECDSA.N_Bits := Num_Bits;
      Disable_Pka;
      ECDSA.Scalar_Len   := Num_Bits;
      ECDSA.A_Sign       := 0;
      Copy_S_To_U32 (Curve.A, ECDSA.A_Coeff);
      Copy_S_To_U32 (Curve.P, ECDSA.Curve_Mod);
      Copy_S_To_U32 (Scalar, ECDSA.Scalar);
      Copy_S_To_U32 (X, ECDSA.X);
      Copy_S_To_U32 (Y, ECDSA.Y);
      StartPKA (FP_Scalar_Multiplication);
      Copy_U32_To_S (ECDSA.X, X_Res);
      Copy_U32_To_S (ECDSA.Y, Y_Res);
   end ECDSA_Point_Mult;

   procedure ECDSA_Point_Mult (Scalar : ECDSA_String;
                               X_Res  : out ECDSA_String;
                               Y_Res  : out ECDSA_String)
   is
      ECDSA : aliased ECDSA_Point_Ram with Import, Address => S_NS_Periph (PKA_Base);
      Curve  : CurveData;
   begin
      Enable_Pka;
      Curve := Common_Init (Field_Arithmetic);
      Clear_Ram;
      ECDSA.N_Bits := Num_Bits;
      Disable_Pka;
      ECDSA.Scalar_Len   := Num_Bits;
      ECDSA.A_Sign       := 0;
      Copy_S_To_U32 (Curve.A, ECDSA.A_Coeff);
      Copy_S_To_U32 (Curve.P, ECDSA.Curve_Mod);
      Copy_S_To_U32 (Scalar, ECDSA.Scalar);
      Copy_S_To_U32 (Curve.X, ECDSA.X);
      Copy_S_To_U32 (Curve.Y, ECDSA.Y);
      StartPKA (FP_Scalar_Multiplication);
      Copy_U32_To_S (ECDSA.X, X_Res);
      Copy_U32_To_S (ECDSA.Y, Y_Res);
   end ECDSA_Point_Mult;

   function Make_Random_Group_String (NClamp : Boolean := False) return ECDSA_String
   is
      Result : ECDSA_String;
      TmpStr : ECDSA_String;
      One    : ECDSA_String;
      Temp   : UInt32_Array (0 .. Integer (N_By_32 - 1));
      Curve  : CurveData;
   begin
      Enable_Rng;
      Enable_Pka;
      Curve := Common_Init (Field_Arithmetic);
      for I in Temp'Range loop
         Temp (I) := Random;
      end loop;
      --  fix last dword
      if Rem_By_32 /= 0 then
         declare
            Mask : UInt32 := Shift_Right (UInt32'Last, Integer (32 - Rem_By_32));
            Idx  : Integer := Integer (N_By_32 - 1);
         begin
            Temp (Idx) := Temp (Idx) and Mask;
         end;
      end if;
      Copy_U32_To_S (Temp, TmpStr);

      if NClamp then
         One := (1 => '0', others => '0');
         One (One'Length) := '1';
         --  compute 1 + ((TmpStr-1) % n)
         ECDSA_Math (Work => "NB-Ao%OB+1=", A => TmpStr, B => One, O1 => Result, O2 => Result);
      else
         Result := TmpStr;
      end if;

      return Result;
   end Make_Random_Group_String;

   function Make_Public_Key_String (PrivateKey : ECDSA_String) return ECDSA_PublicKeyStr
   is
      Result : ECDSA_PublicKeyStr;
   begin
      ECDSA_Point_Mult (PrivateKey, Result.X, Result.Y);
      return Result;
   end Make_Public_Key_String;

end STM32.PKA;
