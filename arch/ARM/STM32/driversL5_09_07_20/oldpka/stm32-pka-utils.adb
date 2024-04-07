with STM32.RNG;      use STM32.RNG;
with STM32_SVD.RCC;  use STM32_SVD.RCC;
package body STM32.PKA.Utils is

   --
   --  All of the PKA RAM is 32bit access only. That is why there are
   --  so many 32bit copy funcs.
   --  It would be much easier were the ram byte addressable.

   --  String args are of this form ex:
   --  "13612b47a99844aa4c1492624afb9f2c1f36b098cd581ad100bfc19f447db024"
   --  Copy_S_To_U32
   --   Move the hexstring to the 32bit writeable To
   --   This function could be 1 loop but I had an 8bit to 32bit copy so
   --   step 1 is
   procedure Copy_S_To_U32 (S : String; To : out UInt32_Array)
   is
      --  Overlay 4 bytes on a 32bit word
      X     : UInt32;
      Y     : UInt8_Array (0 .. 3);
      for X'Address use Y'Address;
      type ModBy4 is mod 4;
      Idx : ModBy4 := 0;
      Limit : Integer := (S'Length / 2);
      T     : UInt8_Array (0 .. Limit - 1);
      Tidx  : Integer := 0;
      Off   : Integer := 0;
      type ModBy2 is mod 2;
      Idx2  : ModBy2 := 0;
      Z     : UInt8;
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


end STM32.PKA.Utils;
