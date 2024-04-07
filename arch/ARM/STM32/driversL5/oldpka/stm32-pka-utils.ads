package STM32.PKA.Utils is

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
   function Normalize (Digest : ECDSA_Hash) return Digest_Buffer;
   function Normalize (Digest : ECDSA_HashStr) return Digest_BufferStr;

end STM32.PKA.Utils;
