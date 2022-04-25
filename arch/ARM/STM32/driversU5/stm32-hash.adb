with System;

with STM32_SVD.RCC;  use STM32_SVD.RCC;

package body STM32.HASH is

   procedure Enable_Hash
   is
      RCC : aliased RCC_Peripheral
        with Import, Address => S_NS_Periph (RCC_Base);
   begin
      RCC.RCC_AHB2ENR1.HASHEN := True;
   end Enable_Hash;

   procedure Init_Hash (Algorithm : Algorithm_Selection;
                        Mode      : Mode_Selection;
                        Width     : Width_Selection)
   is
      X : UInt2 := UInt2 (Algorithm'Enum_Rep);
   begin
      --  *sigh*, ALGO is a split field... we need to move the bits around
      HASH.CR.ALGO         := X;
      HASH.CR.MODE         := (if Mode = HMAC_Mode then True else False);
      HASH.CR.DATATYPE     := Width'Enum_Rep;
      --  Clear the hash
      HASH.CR.INIT       := True;

   end Init_Hash;

   procedure Feed_Hash (Input : UInt8_Array)
   is
      --  vvvv used to pack 8 to 32.
      X : UInt32;
      Y : UInt8_Array (0 .. 3);
      for X'Address use Y'Address;
      type Counter is mod 4;
      Count : Counter;
   begin
      Count := 0;
      for Ch of Input loop
         Y (Integer (Count)) := Ch;
         Count := Count + 1;
         if Count = 0 then
            --  Wrapped, push in what we have
            HASH.DIN := X;
         end if;
      end loop;
      --  Handle the remainder
      if Count > 0 then
         HASH.DIN := X;
         HASH.STR.NBLW := UInt5 (Count) * 8;
      end if;
   end Feed_Hash;

   procedure Feed_Hash (Input : UInt16_Array)
   is
      --  vvvv used to pack 16 to 32.
      X : UInt32;
      Y : UInt16_Array (0 .. 1);
      for X'Address use Y'Address;
      type Counter is mod 2;
      Count : Counter;
   begin
      Count := 0;
      for Ch of Input loop
         Y (Integer (Count)) := Ch;
         Count := Count + 1;
         if Count = 0 then
            --  Wrapped, push in what we have
            HASH.DIN := X;
         end if;
      end loop;
      --  Handle the remainder
      if Count > 0 then
         HASH.DIN := X;
         HASH.STR.NBLW := UInt5 (Count) * 16;
      end if;
   end Feed_Hash;

   procedure Feed_Hash (Input : UInt32_Array)
   is
   begin
      for Val of Input loop
         HASH.DIN := Val;
      end loop;
   end Feed_Hash;

   --  Start the digest computation and wait
   --  for it to finish.
   procedure Finish_Hash
   is
   begin
      HASH.STR.DCAL := True;
      loop
         exit when not HASH.SR.BUSY;
      end loop;
   end Finish_Hash;

end STM32.HASH;
