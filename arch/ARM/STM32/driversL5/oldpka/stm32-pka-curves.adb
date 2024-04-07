
package body STM32.PKA.Curves is

   function Common_Init (Mode : Init_Mode) return CurveData
   is
      Neg : Boolean := True;
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
         Curve.Y := "3617DE4A" & "96262C6F" & "5D9E98BF" & "9292DC29" &
              "F8F41DBD" & "289A147C" & "E9DA3113" & "B5F0B8C0" &
              "0A60B1CE" & "1D7E819D" & "7A431D7C" & "90EA0E5F";
         Curve.A := "FFFFFFFF" & "FFFFFFFF" & "FFFFFFFF" & "FFFFFFFF" &
              "FFFFFFFF" & "FFFFFFFF" & "FFFFFFFF" & "FFFFFFFE" &
              "FFFFFFFF" & "00000000" & "00000000" & "FFFFFFFC";
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
         Curve.N :=
           "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141";
         Neg := False;
      elsif Curve_Name = "secp128r1" then
         Curve.P := "FFFFFFFDFFFFFFFFFFFFFFFFFFFFFFFF";
         Curve.X := "161FF7528B899B2D0C28607CA52C5B86";
         Curve.Y := "CF5AC8395BAFEB13C02DA292DDED7A83";
         Curve.A := "FFFFFFFDFFFFFFFFFFFFFFFFFFFFFFFC";
         Curve.N := "FFFFFFFE0000000075A30D1B9038A115";
      elsif Curve_Name = "secp112r1" then
         Curve.P := "DB7C2ABF62E35E668076BEAD208B";
         Curve.X := "09487239995A5EE76B55F9C2F098";
         Curve.Y := "A89CE5AF8724C0A23E0E0FF77500";
         Curve.A := "DB7C2ABF62E35E668076BEAD2088";
         Curve.N := "DB7C2ABF62E35E7628DFAC6561C5";
      elsif Curve_Name = "secp112r2" then
         Curve.P := "DB7C2ABF62E35E668076BEAD208B";
         Curve.X := "4BA30AB5E892B4E1649DD0928643";
         Curve.Y := "ADCD46F5882E3747DEF36E956E97";
         Curve.A := "6127C24C05F38A0AAAF65C0EF02C";
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
         Curve.N :=
              "ffffffff" & "ffffffff" & "ffffffff" & "ffffffff" &
              "ffffffff" & "ffffffff" & "c7634d81" & "f4372ddf" &
              "581a0db2" & "48b0a77a" & "ecec196a" & "ccc52973";
      elsif Curve_Name = "nistp256" then
         Curve.P := "ffffffff00000001000000000000000000000000ffffffffffffffffffffffff";
         Curve.X := "6b17d1f2e12c4247f8bce6e563a440f277037d812deb33a0f4a13945d898c296";
         Curve.Y := "4fe342e2fe1a7f9b8ee7eb4a7c0f9e162bce33576b315ececbb6406837bf51f5";
         Curve.A := "ffffffff00000001000000000000000000000000fffffffffffffffffffffffc";
         Curve.N := "ffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551";
      elsif Curve_Name = "nistp224" then
         Curve.P := "ffffffffffffffffffffffffffffffff000000000000000000000001";
         Curve.X := "b70e0cbd6bb4bf7f321390b94a03c1d356c21122343280d6115c1d21";
         Curve.Y := "bd376388b5f723fb4c22dfe6cd4375a05a07476444d5819985007e34";
         Curve.A := "fffffffffffffffffffffffffffffffefffffffffffffffffffffffe";
         Curve.N := "ffffffffffffffffffffffffffff16a2e0b8f03e13dd29455c5c2a3d";
      elsif Curve_Name = "nistp192" then
         Curve.P := "fffffffffffffffffffffffffffffffeffffffffffffffff";
         Curve.X := "188da80eb03090f67cbf20eb43a18800f4ff0afd82ff1012";
         Curve.Y := "07192b95ffc8da78631011ed6b24cdd573f977a11e794811";
         Curve.A := "fffffffffffffffffffffffffffffffefffffffffffffffc";
         Curve.N := "ffffffffffffffffffffffff99def836146bc9b1b4d22831";
      elsif Curve_Name = "brainpool256r1" then
         Curve.P := "A9FB57DBA1EEA9BC3E660A909D838D726E3BF623D52620282013481D1F6E5377";
         Curve.X := "8BD2AEB9CB7E57CB2C4B482FFC81B7AFB9DE27E1E3BD23C23A4453BD9ACE3262";
         Curve.Y := "547EF835C3DAC4FD97F8461A14611DC9C27745132DED8E545C1D54C72F046997";
         Curve.A := "7D5A0975FC2C3057EEF67530417AFFE7FB8055C126DC5C6CE94A4B44F330B5D9";
         Curve.N := "A9FB57DBA1EEA9BC3E660A909D838D718C397AA3B561A6F7901E0E82974856A7";
         Neg := False;
      elsif Curve_Name = "brainpool256t1" then
         Curve.P := "A9FB57DBA1EEA9BC3E660A909D838D726E3BF623D52620282013481D1F6E5377";
         Curve.X := "A3E8EB3CC1CFE7B7732213B23A656149AFA142C47AAFBC2B79A191562E1305F4";
         Curve.Y := "2D996C823439C56D7F7B22E14644417E69BCB6DE39D027001DABE8F35B25C9BE";
         Curve.A := "A9FB57DBA1EEA9BC3E660A909D838D726E3BF623D52620282013481D1F6E5374";
         Curve.N := "A9FB57DBA1EEA9BC3E660A909D838D718C397AA3B561A6F7901E0E82974856A7";
      elsif Curve_Name = "brainpool384r1" then
         Curve.P := "8CB91E82A3386D280F5D6F7E50E641DF152F7109ED5456B412B1DA197FB71123ACD3A729901D1A71874700133107EC53";
         Curve.X := "1D1C64F068CF45FFA2A63A81B7C13F6B8847A3E77EF14FE3DB7FCAFE0CBD10E8E826E03436D646AAEF87B2E247D4AF1E";
         Curve.Y := "8ABE1D7520F9C2A45CB1EB8E95CFD55262B70B29FEEC5864E19C054FF99129280E4646217791811142820341263C5315";
         Curve.A := "7BC382C63D8C150C3C72080ACE05AFA0C2BEA28E4FB22787139165EFBA91F90F8AA5814A503AD4EB04A8C7DD22CE2826";
         Curve.N := "8CB91E82A3386D280F5D6F7E50E641DF152F7109ED5456B31F166E6CAC0425A7CF3AB6AF6B7FC3103B883202E9046565";
         Neg := False;
      elsif Curve_Name = "brainpool384t1" then
         Curve.P := "8CB91E82A3386D280F5D6F7E50E641DF152F7109ED5456B412B1DA197FB71123ACD3A729901D1A71874700133107EC53";
         Curve.X := "18DE98B02DB9A306F2AFCD7235F72A819B80AB12EBD653172476FECD462AABFFC4FF191B946A5F54D8D0AA2F418808CC";
         Curve.Y := "25AB056962D30651A114AFD2755AD336747F93475B7A1FCA3B88F2B6A208CCFE469408584DC2B2912675BF5B9E582928";
         Curve.A := "8CB91E82A3386D280F5D6F7E50E641DF152F7109ED5456B412B1DA197FB71123ACD3A729901D1A71874700133107EC50";
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
      else
         declare
            ECDSA : aliased ECDSA_Ram with Import, Address => S_NS_Periph (PKA_Base);
         begin
            Copy_S_To_U32 (Curve.P, ECDSA.Mod_GF);
         end;
      end if;
      return Curve;
   end Common_Init;
end STM32.PKA.Curves;
