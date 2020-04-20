with System;
with System.Machine_Code; use System.Machine_Code;

with STM32.RNG;      use STM32.RNG;
with STM32_SVD.RCC;  use STM32_SVD.RCC;

package body STM32.PKA is

   Curve : CurveData;

   procedure Copy_S_To_U32 (S : String; To : out UInt32_Array)
   is
      X : UInt32;
      Y : UInt8_Array (0 .. 3);
      for X'Address use Y'Address;
      type ModBy4 is mod 4;
      Idx : ModBy4 := 0;
      Limit : Integer := (S'Length / 2);
      T : UInt8_Array (0 .. Limit - 1);
      Tidx : Integer := 0;
      Off : Integer := 0;
      type ModBy2 is mod 2;
      Idx2 : ModBy2 := 0;
      Z : UInt8;
   begin
      Z := 0;
      for I in S'Range loop
         Z := (Z * 16);
         if (Character'Pos (S (I)) >= Character'Pos ('a')) then
            Z := Z + (10 + (Character'Pos (S (I)) - Character'Pos ('a')));
         elsif (Character'Pos (S (I)) >= Character'Pos ('A')) then
            Z := Z + (10 + (Character'Pos (S (I)) - Character'Pos ('A')));
         else
            Z := Z + (Character'Pos (S (I)) - Character'Pos ('0'));
         end if;
         Idx2 := Idx2 + 1;
         if 0 = Idx2 then
            T (Tidx) := Z;
            Tidx := Tidx + 1;
            Z := 0;
         end if;
      end loop;

      --  ((Integer (N_By_8) - I) + Extra
      for I in reverse T'Range loop
         Y (Integer (Idx)) := T (I);
         Idx := Idx + 1;
         if Idx = 0 then
            To (Off) := X;
            X := 0;
            Off := Off + 1;
         end if;
      end loop;
      if Idx /= 0 then  -- Remainder
         To (Off) := X;
      end if;
   end Copy_S_To_U32;
   procedure Copy_U8_To_U32 (From : UInt8_Array; To : out UInt32_Array)
   is
      N : Integer;
      X : UInt32;
      Y : UInt8_Array (0 .. 3);
      for X'Address use Y'Address;
      type ModBy4 is mod 4;
      Idx : ModBy4 := 0;
   begin
      N := From'Size / 8;
      for I in From'Range loop
         Y (Integer (3 - Idx)) := From (I);
         Idx := Idx + 1;
         if Idx = 0 then
            To ((N - I) / 4) := X;
         end if;
      end loop;
   end Copy_U8_To_U32;

   procedure Copy_U32_To_U8 (From : UInt32_Array; To : out UInt8_Array)
   is
      N : Integer;
      X : UInt32;
      Y : UInt8_Array (0 .. 3);
      for X'Address use Y'Address;
      type ModBy4 is mod 4;
      Idx : ModBy4 := 0;
   begin
      N := (To'Size / 8) - 1;
      X := From (0);
      for I in To'Range loop
         To (N - I) := Y (Integer (Idx));
         Idx := Idx + 1;
         if Idx = 0 then
            X := From ((I + 1) / 4);
         end if;
      end loop;
   end Copy_U32_To_U8;

   procedure Copy_U8_To_S (From : UInt8; To : out ECDSA_String; Offset : UInt32)
   is
      Ch   : Character;
      Nib  : UInt4;
   begin
      for I in 1 .. 2 loop
         if I = 1 then
            Nib := UInt4 (Shift_Right (From, 4));
         else
            Nib := UInt4 (From and 16#0f#);
         end if;
         if Nib >= 10 then
            Ch := Character'Val (Integer ((Nib - 10)) + Character'Pos ('A'));
         else
            Ch := Character'Val (Integer (Nib) + Character'Pos ('0'));
         end if;
         To (I + Integer ((Offset * 2))) := Ch;
      end loop;
   end Copy_U8_To_S;

   procedure Copy_U32_To_S (From : UInt32_Array; To : out ECDSA_String)
   is
      N : UInt32;
      X : UInt32;
      Y : UInt8_Array (0 .. 3);
      for X'Address use Y'Address;
      Idx : UInt32 := 0;
      To_Copy : UInt32 := N_By_8;
   begin
      --      X := From (0);
      N := Num_Bits / 32;
      if (Num_Bits mod 32) > 0 then
         N := N + 1;
      end if;
      for I in 0 .. N - 1 loop
         X := From (Integer (I));
         for J in 0 .. 3 loop
            To_Copy := To_Copy - 1;
            Copy_U8_To_S (Y (J), To, To_Copy);
            if To_Copy = 0 then
               return;
            end if;
         end loop;
         Idx := Idx + 4;
      end loop;
   end Copy_U32_To_S;

   procedure Enable_Pka
   is
      RCC : aliased RCC_Peripheral
        with Import, Address => S_NS_Periph (RCC_Base);
   begin
      RCC.AHB2ENR.PKAEN := True;
      loop
         PKA.CR.EN := True;
         exit when PKA.CR.EN;
      end loop;
   end Enable_Pka;

   procedure Disable_Pka
   is
      RCC : aliased RCC_Peripheral
        with Import, Address => S_NS_Periph (RCC_Base);
   begin
      loop
         PKA.CR.EN := False;
         exit when not PKA.CR.EN;
      end loop;
   end Disable_Pka;

   function Check_Errors return Boolean
   is
   begin
      return PKA.SR.RAMERRF or PKA.SR.ADDRERRF;
   end Check_Errors;

   procedure Clear_Flags
   is
   begin
      PKA.CLRFR.PROCENDFC := True;
      PKA.CLRFR.RAMERRFC  := True;
      PKA.CLRFR.ADDRERRFC := True;
   end Clear_Flags;

   procedure Clear_Ram
   is
      PKA_RAM : aliased All_PKA_Ram with Import, Volatile, Address => S_NS_Periph (PKA_Base);
   begin
      PKA_RAM.RAM := (0 => 0, others => 0);
   end Clear_Ram;

   function Common_Init (Mode : Init_Mode) return CurveData
   is
      Neg   : Boolean := True; --  Coeff A is usually -ve (we only false the exceptions)
      Curve : CurveData;
   begin
      if Curve_Name = "secp521r1" then
         Curve.P := "01FF" &
              "FFFFFFFF" & "FFFFFFFF" & "FFFFFFFF" & "FFFFFFFF" &
              "FFFFFFFF" & "FFFFFFFF" & "FFFFFFFF" & "FFFFFFFF" &
              "FFFFFFFF" & "FFFFFFFF" & "FFFFFFFF" & "FFFFFFFF" &
              "FFFFFFFF" & "FFFFFFFF" & "FFFFFFFF" & "FFFFFFFF";
         Curve.A := "01FF" &
              "FFFFFFFF" & "FFFFFFFF" & "FFFFFFFF" & "FFFFFFFF" &
              "FFFFFFFF" & "FFFFFFFF" & "FFFFFFFF" & "FFFFFFFF" &
              "FFFFFFFF" & "FFFFFFFF" & "FFFFFFFF" & "FFFFFFFF" &
              "FFFFFFFF" & "FFFFFFFF" & "FFFFFFFF" & "FFFFFFFC";
         Curve.B := "0051" &
              "953EB961" & "8E1C9A1F" & "929A21A0" & "B68540EE" &
              "A2DA725B" & "99B315F3" & "B8B48991" & "8EF109E1" &
              "56193951" & "EC7E937B" & "1652C0BD" & "3BB1BF07" &
              "3573DF88" & "3D2C34F1" & "EF451FD4" & "6B503F00";
         Curve.X := "00C6" &
              "858E06B7" & "0404E9CD" & "9E3ECB66" & "2395B442" &
              "9C648139" & "053FB521" & "F828AF60" & "6B4D3DBA" &
              "A14B5E77" & "EFE75928" & "FE1DC127" & "A2FFA8DE" &
              "3348B3C1" & "856A429B" & "F97E7E31" & "C2E5BD66";
         Curve.Y := "0118" &
              "39296A78" & "9A3BC004" & "5C8A5FB4" & "2C7D1BD9" &
              "98F54449" & "579B4468" & "17AFBD17" & "273E662C" &
              "97EE7299" & "5EF42640" & "C550B901" & "3FAD0761" &
              "353C7086" & "A272C240" & "88BE9476" & "9FD16650";
         Curve.N := "01FF" &
              "FFFFFFFF" & "FFFFFFFF" & "FFFFFFFF" & "FFFFFFFF" &
              "FFFFFFFF" & "FFFFFFFF" & "FFFFFFFF" & "FFFFFFFA" &
              "51868783" & "BF2F966B" & "7FCC0148" & "F709A5D0" &
              "3BB5C9B8" & "899C47AE" & "BB6FB71E" & "91386409";
      elsif Curve_Name = "secp384r1" then
         Curve.P :=
              "FFFFFFFF" & "FFFFFFFF" & "FFFFFFFF" & "FFFFFFFF" &
              "FFFFFFFF" & "FFFFFFFF" & "FFFFFFFF" & "FFFFFFFE" &
              "FFFFFFFF" & "00000000" & "00000000" & "FFFFFFFF";
         Curve.X :=
              "AA87CA22" & "BE8B0537" & "8EB1C71E" & "F320AD74" &
              "6E1D3B62" & "8BA79B98" & "59F741E0" & "82542A38" &
              "5502F25D" & "BF55296C" & "3A545E38" & "72760AB7";
         Curve.Y :=
              "3617DE4A" & "96262C6F" & "5D9E98BF" & "9292DC29" &
              "F8F41DBD" & "289A147C" & "E9DA3113" & "B5F0B8C0" &
              "0A60B1CE" & "1D7E819D" & "7A431D7C" & "90EA0E5F";
         Curve.A :=
              "FFFFFFFF" & "FFFFFFFF" & "FFFFFFFF" & "FFFFFFFF" &
              "FFFFFFFF" & "FFFFFFFF" & "FFFFFFFF" & "FFFFFFFE" &
              "FFFFFFFF" & "00000000" & "00000000" & "FFFFFFFC";
         Curve.B :=
              "B3312FA7" & "E23EE7E4" & "988E056B" & "E3F82D19" &
              "181D9C6E" & "FE814112" & "0314088F" & "5013875A" &
              "C656398D" & "8A2ED19D" & "2A85C8ED" & "D3EC2AEF";
         Curve.N := "FFFFFFFF" & "FFFFFFFF" & "FFFFFFFF" & "FFFFFFFF" &
              "FFFFFFFF" & "FFFFFFFF" & "C7634D81" & "F4372DDF" &
              "581A0DB2" & "48B0A77A" & "ECEC196A" & "CCC52973";
      elsif Curve_Name = "secp256r1" then
         Curve.P :=
           "FFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF";
         Curve.X :=
           "6B17D1F2E12C4247F8BCE6E563A440F277037D812DEB33A0F4A13945D898C296";
         Curve.Y :=
           "4FE342E2FE1A7F9B8EE7EB4A7C0F9E162BCE33576B315ECECBB6406837BF51F5";
         Curve.A :=
           "FFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFC";
         Curve.B :=
           "5AC635D8AA3A93E7B3EBBD55769886BC651D06B0CC53B0F63BCE3C3E27D2604B";
         Curve.N :=
           "FFFFFFFF00000000FFFFFFFFFFFFFFFFBCE6FAADA7179E84F3B9CAC2FC632551";
      elsif Curve_Name = "secp256k1" then
         Curve.P :=
           "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F";
         Curve.X :=
           "79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798";
         Curve.Y :=
           "483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8";
         Curve.A :=
           "0000000000000000000000000000000000000000000000000000000000000000";
         Curve.B :=
           "0000000000000000000000000000000000000000000000000000000000000007";
         Curve.N :=
           "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141";
         Neg := False;
      elsif Curve_Name = "secp128r1" then
         Curve.P := "FFFFFFFDFFFFFFFFFFFFFFFFFFFFFFFF";
         Curve.X := "161FF7528B899B2D0C28607CA52C5B86";
         Curve.Y := "CF5AC8395BAFEB13C02DA292DDED7A83";
         Curve.A := "FFFFFFFDFFFFFFFFFFFFFFFFFFFFFFFC";
         Curve.B := "E87579C11079F43DD824993C2CEE5ED3";
         Curve.N := "FFFFFFFE0000000075A30D1B9038A115";
      elsif Curve_Name = "secp112r1" then
         Curve.P := "DB7C2ABF62E35E668076BEAD208B";
         Curve.X := "09487239995A5EE76B55F9C2F098";
         Curve.Y := "A89CE5AF8724C0A23E0E0FF77500";
         Curve.A := "DB7C2ABF62E35E668076BEAD2088";
         Curve.B := "659EF8BA043916EEDE8911702B22";
         Curve.N := "DB7C2ABF62E35E7628DFAC6561C5";
      elsif Curve_Name = "secp112r2" then
         Curve.P := "DB7C2ABF62E35E668076BEAD208B";
         Curve.X := "4BA30AB5E892B4E1649DD0928643";
         Curve.Y := "ADCD46F5882E3747DEF36E956E97";
         Curve.A := "6127C24C05F38A0AAAF65C0EF02C";
         Curve.B := "51DEF1815DB5ED74FCC34C85D709";
         Curve.N := "36DF0AAFD8B8D7597CA10520D04B";
         Neg := False;
      elsif Curve_Name = "nistp521" then
         Curve.P := "01ff" &
              "ffffffff" & "ffffffff" & "ffffffff" & "ffffffff" &
              "ffffffff" & "ffffffff" & "ffffffff" & "ffffffff" &
              "ffffffff" & "ffffffff" & "ffffffff" & "ffffffff" &
              "ffffffff" & "ffffffff" & "ffffffff" & "ffffffff";
         Curve.X := "00c6" &
              "858e06b7" & "0404e9cd" & "9e3ecb66" & "2395b442" &
              "9c648139" & "053fb521" & "f828af60" & "6b4d3dba" &
              "a14b5e77" & "efe75928" & "fe1dc127" & "a2ffa8de" &
              "3348b3c1" & "856a429b" & "f97e7e31" & "c2e5bd66";
         Curve.Y := "0118" &
              "39296a78" & "9a3bc004" & "5c8a5fb4" & "2c7d1bd9" &
              "98f54449" & "579b4468" & "17afbd17" & "273e662c" &
              "97ee7299" & "5ef42640" & "c550b901" & "3fad0761" &
              "353c7086" & "a272c240" & "88be9476" & "9fd16650";
         Curve.A := "01ff" &
              "ffffffff" & "ffffffff" & "ffffffff" & "ffffffff" &
              "ffffffff" & "ffffffff" & "ffffffff" & "ffffffff" &
              "ffffffff" & "ffffffff" & "ffffffff" & "ffffffff" &
              "ffffffff" & "ffffffff" & "ffffffff" & "fffffffc";
         Curve.B := "0051" &
              "953eb961" & "8e1c9a1f" & "929a21a0" & "b68540ee" &
              "a2da725b" & "99b315f3" & "b8b48991" & "8ef109e1" &
              "56193951" & "ec7e937b" & "1652c0bd" & "3bb1bf07" &
              "3573df88" & "3d2c34f1" & "ef451fd4" & "6b503f00";
         Curve.N := "01ff" &
              "ffffffff" & "ffffffff" & "ffffffff" & "ffffffff" &
              "ffffffff" & "ffffffff" & "ffffffff" & "fffffffa" &
              "51868783" & "bf2f966b" & "7fcc0148" & "f709a5d0" &
              "3bb5c9b8" & "899c47ae" & "bb6fb71e" & "91386409";
      elsif Curve_Name = "nistp384" then
         Curve.P :=
              "ffffffff" & "ffffffff" & "ffffffff" & "ffffffff" &
              "ffffffff" & "ffffffff" & "ffffffff" & "fffffffe" &
              "ffffffff" & "00000000" & "00000000" & "ffffffff";
         Curve.X :=
              "aa87ca22" & "be8b0537" & "8eb1c71e" & "f320ad74" &
              "6e1d3b62" & "8ba79b98" & "59f741e0" & "82542a38" &
              "5502f25d" & "bf55296c" & "3a545e38" & "72760ab7";
         Curve.Y :=
              "3617de4a" & "96262c6f" & "5d9e98bf" & "9292dc29" &
              "f8f41dbd" & "289a147c" & "e9da3113" & "b5f0b8c0" &
              "0a60b1ce" & "1d7e819d" & "7a431d7c" & "90ea0e5f";
         Curve.A :=
              "ffffffff" & "ffffffff" & "ffffffff" & "ffffffff" &
              "ffffffff" & "ffffffff" & "ffffffff" & "fffffffe" &
              "ffffffff" & "00000000" & "00000000" & "fffffffc";
         Curve.B :=
              "b3312fa7" & "e23ee7e4" & "988e056b" & "e3f82d19" &
              "181d9c6e" & "fe814112" & "0314088f" & "5013875a" &
              "c656398d" & "8a2ed19d" & "2a85c8ed" & "d3ec2aef";
         Curve.N :=
              "ffffffff" & "ffffffff" & "ffffffff" & "ffffffff" &
              "ffffffff" & "ffffffff" & "c7634d81" & "f4372ddf" &
              "581a0db2" & "48b0a77a" & "ecec196a" & "ccc52973";
      elsif Curve_Name = "nistp256" then
         Curve.P := "ffffffff00000001000000000000000000000000ffffffffffffffffffffffff";
         Curve.X := "6b17d1f2e12c4247f8bce6e563a440f277037d812deb33a0f4a13945d898c296";
         Curve.Y := "4fe342e2fe1a7f9b8ee7eb4a7c0f9e162bce33576b315ececbb6406837bf51f5";
         Curve.A := "ffffffff00000001000000000000000000000000fffffffffffffffffffffffc";
         Curve.B := "5ac635d8aa3a93e7b3ebbd55769886bc651d06b0cc53b0f63bce3c3e27d2604b";
         Curve.N := "ffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551";
      elsif Curve_Name = "nistp224" then
         Curve.P := "ffffffffffffffffffffffffffffffff000000000000000000000001";
         Curve.X := "b70e0cbd6bb4bf7f321390b94a03c1d356c21122343280d6115c1d21";
         Curve.Y := "bd376388b5f723fb4c22dfe6cd4375a05a07476444d5819985007e34";
         Curve.A := "fffffffffffffffffffffffffffffffefffffffffffffffffffffffe";
         Curve.B := "b4050a850c04b3abf54132565044b0b7d7bfd8ba270b39432355ffb4";
         Curve.N := "ffffffffffffffffffffffffffff16a2e0b8f03e13dd29455c5c2a3d";
      elsif Curve_Name = "nistp192" then
         Curve.P := "fffffffffffffffffffffffffffffffeffffffffffffffff";
         Curve.X := "188da80eb03090f67cbf20eb43a18800f4ff0afd82ff1012";
         Curve.Y := "07192b95ffc8da78631011ed6b24cdd573f977a11e794811";
         Curve.A := "fffffffffffffffffffffffffffffffefffffffffffffffc";
         Curve.B := "64210519e59c80e70fa7e9ab72243049feb8deecc146b9b1";
         Curve.N := "ffffffffffffffffffffffff99def836146bc9b1b4d22831";
      elsif Curve_Name = "brainpool256r1" then
         Curve.P := "A9FB57DBA1EEA9BC3E660A909D838D726E3BF623D52620282013481D1F6E5377";
         Curve.X := "8BD2AEB9CB7E57CB2C4B482FFC81B7AFB9DE27E1E3BD23C23A4453BD9ACE3262";
         Curve.Y := "547EF835C3DAC4FD97F8461A14611DC9C27745132DED8E545C1D54C72F046997";
         Curve.A := "7D5A0975FC2C3057EEF67530417AFFE7FB8055C126DC5C6CE94A4B44F330B5D9";
         Curve.B := "26DC5C6CE94A4B44F330B5D9BBD77CBF958416295CF7E1CE6BCCDC18FF8C07B6";
         Curve.N := "A9FB57DBA1EEA9BC3E660A909D838D718C397AA3B561A6F7901E0E82974856A7";
         Neg := False;
      elsif Curve_Name = "brainpool256t1" then
         Curve.P := "A9FB57DBA1EEA9BC3E660A909D838D726E3BF623D52620282013481D1F6E5377";
         Curve.X := "A3E8EB3CC1CFE7B7732213B23A656149AFA142C47AAFBC2B79A191562E1305F4";
         Curve.Y := "2D996C823439C56D7F7B22E14644417E69BCB6DE39D027001DABE8F35B25C9BE";
         Curve.A := "A9FB57DBA1EEA9BC3E660A909D838D726E3BF623D52620282013481D1F6E5374";
         Curve.B := "662C61C430D84EA4FE66A7733D0B76B7BF93EBC4AF2F49256AE58101FEE92B04";
         Curve.N := "A9FB57DBA1EEA9BC3E660A909D838D718C397AA3B561A6F7901E0E82974856A7";
      elsif Curve_Name = "brainpool384r1" then
         Curve.P := "8CB91E82A3386D280F5D6F7E50E641DF152F7109ED5456B412B1DA197FB71123ACD3A729901D1A71874700133107EC53";
         Curve.X := "1D1C64F068CF45FFA2A63A81B7C13F6B8847A3E77EF14FE3DB7FCAFE0CBD10E8E826E03436D646AAEF87B2E247D4AF1E";
         Curve.Y := "8ABE1D7520F9C2A45CB1EB8E95CFD55262B70B29FEEC5864E19C054FF99129280E4646217791811142820341263C5315";
         Curve.A := "7BC382C63D8C150C3C72080ACE05AFA0C2BEA28E4FB22787139165EFBA91F90F8AA5814A503AD4EB04A8C7DD22CE2826";
         Curve.B := "04A8C7DD22CE28268B39B55416F0447C2FB77DE107DCD2A62E880EA53EEB62D57CB4390295DBC9943AB78696FA504C11";
         Curve.N := "8CB91E82A3386D280F5D6F7E50E641DF152F7109ED5456B31F166E6CAC0425A7CF3AB6AF6B7FC3103B883202E9046565";
         Neg := False;
      elsif Curve_Name = "brainpool384t1" then
         Curve.P := "8CB91E82A3386D280F5D6F7E50E641DF152F7109ED5456B412B1DA197FB71123ACD3A729901D1A71874700133107EC53";
         Curve.X := "18DE98B02DB9A306F2AFCD7235F72A819B80AB12EBD653172476FECD462AABFFC4FF191B946A5F54D8D0AA2F418808CC";
         Curve.Y := "25AB056962D30651A114AFD2755AD336747F93475B7A1FCA3B88F2B6A208CCFE469408584DC2B2912675BF5B9E582928";
         Curve.A := "8CB91E82A3386D280F5D6F7E50E641DF152F7109ED5456B412B1DA197FB71123ACD3A729901D1A71874700133107EC50";
         Curve.B := "7F519EADA7BDA81BD826DBA647910F8C4B9346ED8CCDC64E4B1ABD11756DCE1D2074AA263B88805CED70355A33B471EE";
         Curve.N := "8CB91E82A3386D280F5D6F7E50E641DF152F7109ED5456B31F166E6CAC0425A7CF3AB6AF6B7FC3103B883202E9046565";
      else
         raise Program_Error with "Curve not supported";
      end if;
      if Mode = Signing then
         declare
            ECDSA : aliased ECDSA_Sign_Ram with Import, Address => S_NS_Periph (PKA_Base);
         begin
            Copy_S_To_U32 (Curve.P, ECDSA.Mod_GF);
            Copy_S_To_U32 (Curve.X, ECDSA.X);
            Copy_S_To_U32 (Curve.Y, ECDSA.Y);
            Copy_S_To_U32 (Curve.A, ECDSA.A_Coeff);
            Copy_S_To_U32 (Curve.N, ECDSA.Order_N);
         end;
      elsif Mode = Validation then
         declare
            ECDSA : aliased ECDSA_Verify_Ram with Import, Address => S_NS_Periph (PKA_Base);
         begin
            Copy_S_To_U32 (Curve.P, ECDSA.Mod_GF);
            Copy_S_To_U32 (Curve.X, ECDSA.IP_X);
            Copy_S_To_U32 (Curve.Y, ECDSA.IP_Y);
            Copy_S_To_U32 (Curve.N, ECDSA.Order_N);
            if Neg then
               ECDSA.A_Coeff_Sign   := 1;
               ECDSA.A_Coeff (0)    := 3;
            else
               Copy_S_To_U32 (Curve.A, ECDSA.A_Coeff);
            end if;
         end;
      elsif Mode = Point_Check then
         declare
            ECDSA : aliased ECDSA_Point_Check_Ram with Import, Address => S_NS_Periph (PKA_Base);
         begin
            Copy_S_To_U32 (Curve.P, ECDSA.Curve_Mod);
            Copy_S_To_U32 (Curve.B, ECDSA.B_Coeff);
            if Neg then
               ECDSA.A_Sign         := 1;
               ECDSA.A_Coeff (0)    := 3;
            else
               Copy_S_To_U32 (Curve.A, ECDSA.A_Coeff);
            end if;
         end;
      else
         declare
            ECDSA : aliased ECDSA_Ram with Import, Address => S_NS_Periph (PKA_Base);
         begin
            Copy_S_To_U32 (Curve.P, ECDSA.Mod_GF);
         end;
      end if;
      return Curve;
   end Common_Init;
   function Normalize (Digest : ECDSA_Hash) return Digest_Buffer
   is
      Gap : UInt32 := (Num_Bits - Hash_Size) / 8;
      Norm_Digest : Digest_Buffer;
   begin
      if Num_Bits > Hash_Size then
         Norm_Digest := (0 => 0, others => 0);
         for I in Digest'Range loop
            Norm_Digest (I + Integer (Gap)) := Digest (I);
         end loop;
      else
         for I in Norm_Digest'Range loop
            Norm_Digest (I) := Digest (I);
         end loop;
      end if;
      return Norm_Digest;
   end Normalize;
   function Normalize (Digest : ECDSA_HashStr) return Digest_BufferStr
   is
      Gap : UInt32 := (Num_Bits - Hash_Size) / 8;
      Norm_Digest : Digest_BufferStr;
      Extra : Natural := 0;
   begin
      if (Num_Bits mod 8) /= 0 then
         Extra := 2;
      end if;
      if Num_Bits > Hash_Size then
         Norm_Digest := (1 => '0', others => '0');
         for I in Digest'Range loop
            Norm_Digest (I + Integer (Gap * 2) + Extra) := Digest (I);
         end loop;
      else
         for I in Norm_Digest'Range loop
            Norm_Digest (I + Extra) := Digest (I);
         end loop;
      end if;
      return Norm_Digest;
   end Normalize;

   procedure StartPKA (Mode : PKA_Parameters)
   is
   begin
      Enable_Pka;
      PKA.CR.MODE := Mode'Enum_Rep;
      PKA.CR.START := True;
      --  Need to add timeout here vvv
      loop
         exit when PKA.SR.PROCENDF;
      end loop;
   end StartPKA;

   function ECDSA_Sign (Private_Key : ECDSA_Key;
                        H           : ECDSA_Hash;
                        K           : ECDSA_Rand;
                        Signature   : out ECDSA_Signature) return Boolean
   is
      ECDSA : aliased ECDSA_Sign_Ram with Import, Address => S_NS_Periph (PKA_Base);
      Result : Boolean := True;
      Err : UInt32;
      Gap : UInt32 := (Num_Bits - Hash_Size) / 8;
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
               ECDSA.A := ECDSA.T3;
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

   function ECDSA_Point_On_Curve (Point : ECDSA_PointStr) return Boolean
   is
      ECDSA : aliased ECDSA_Point_Check_Ram with Import, Address => S_NS_Periph (PKA_Base);
   begin
      Enable_Pka;
      Clear_Ram;
      Curve := Common_Init (Point_Check);
      ECDSA.N_Bits := Num_Bits;
      Copy_S_To_U32 (Point.X, ECDSA.X);
      Copy_S_To_U32 (Point.Y, ECDSA.Y);
      StartPKA (Point_On_Elliptic_Curve_Fp_Check);
      Disable_Pka;
      return (ECDSA.Result = 0);
   end ECDSA_Point_On_Curve;

   function Make_Random_Group_String (NClamp : Boolean := False) return ECDSA_String
   is
      Result : ECDSA_String;
      TmpStr : ECDSA_String;
      One    : ECDSA_String;
      Temp   : UInt32_Array (0 .. Integer (N_By_32 - 1));
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
         --  n : group order (sometimes a cursive l in papers)
         --  compute 1 + (TmpStr % (n - 1)) using the PKA
         --  'N' send group order to A operand ('n' would be to B)
         --  'B' -> B arg -> B operand (B arg is 0x1 in this code here)
         --  '-' do A-B on the 2 operands (or output := group order - 1)
         --  'A' A arg -> A operand
         --  'o' output of last calc -> B operand ('O' goes to A operand btw)
         --  '%' A modulo B
         --  'O' output to A operand
         --  'B' -> B arg -> B operand
         --  '+' A+B
         --  '1' move output to Temp1
         --  '=' Temp1 to O1 (output1)
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
