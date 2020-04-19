with System;

with STM32.Device;   use STM32.Device;
with STM32_SVD;      use STM32_SVD;
with STM32_SVD.HASH; use STM32_SVD.HASH;

package STM32.HASH is

   HASH : aliased HASH_Peripheral with Import, Volatile, Address => S_NS_Periph (HASH_Base);

   type Algorithm_Selection is
     (SHA_1,
      MD5,
      SHA_224,
      SHA_256)
     with Size => 2;

   type Mode_Selection is
     (Hash_Mode,
      HMAC_Mode);

   type Width_Selection is
     (Width_32bit,
      Width_16bit,
      Width_8bit,
      Width_1bit);

--  This selection is only taken into account when the INIT bit is set. Changing this
--  bitfield during a computation has no effect.
--  Bit 6 MODE: Mode selection
--  This bit selects the HASH or HMAC mode for the selected algorithm:
--  0: Hash mode selected
--  1: HMAC mode selected. LKEY must be set if the key being used is longer than
--  64 bytes.
--  This selection is only taken into account when the INIT bit is set. Changing this
--  bit during a computation has no effect.
--  Bits 5:4 DATATYPE[1:0]: Data type selection
--  Defines the format of the data entered into the HASH_DIN register:
--  00: 32-bit data. The data written into HASH_DIN are directly used by the HASH
--  processing, without reordering.
--  01: 16-bit data, or half-word. The data written into HASH_DIN are considered as
--  two half-words, and are swapped before being used by the HASH processing.
--  10: 8-bit data, or bytes. The data written into HASH_DIN are considered as four
--  bytes, and are swapped before being used by the HASH processing.
--  11: bit data, or bit-string. The data written into HASH_DIN are considered as 32
--  bits (1st bit of the string at position 0), and are swapped before being used by the
--  HASH processing (1st bit of the string at position 31).

   procedure Enable_Hash;

   procedure Init_Hash (Algorithm : Algorithm_Selection;
                        Mode      : Mode_Selection;
                        Width     : Width_Selection);

   procedure Feed_Hash (Input : UInt8_Array);

   procedure Feed_Hash (Input : UInt16_Array);

   procedure Feed_Hash (Input : UInt32_Array);

   procedure Finish_Hash;

end STM32.HASH;
