--  Copyright 2016-2019 NXP
--  All rights reserved.SPDX-License-Identifier: BSD-3-Clause


--  This spec has been automatically generated from LPC55S6x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NXP_SVD.HASHCRYPT is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  The operational mode to use, or 0 if none. Note that the CONFIG register
   --  will indicate if specific modes beyond SHA1 and SHA2-256 are available.
   type CTRL_Mode_Field is
     (
      --  Disabled
      Disabled,
      --  SHA1 is enabled
      Sha1,
      --  SHA2-256 is enabled
      Sha2_256,
      --  AES if available (see also CRYPTCFG register for more controls)
      Aes,
      --  ICB-AES if available (see also CRYPTCFG register for more controls)
      Icb_Aes)
     with Size => 3;
   for CTRL_Mode_Field use
     (Disabled => 0,
      Sha1 => 1,
      Sha2_256 => 2,
      Aes => 4,
      Icb_Aes => 5);

   --  Written with 1 when starting a new Hash/Crypto. It self clears. Note
   --  that the WAITING Status bit will clear for a cycle during the
   --  initialization from New=1.
   type CTRL_New_Hash_Field is
     (
      --  Reset value for the field
      Ctrl_New_Hash_Field_Reset,
      --  Starts a new Hash/Crypto and initializes the Digest/Result.
      Start)
     with Size => 1;
   for CTRL_New_Hash_Field use
     (Ctrl_New_Hash_Field_Reset => 0,
      Start => 1);

   --  Written with 1 to use DMA to fill INDATA. If Hash, will request from DMA
   --  for 16 words and then will process the Hash. If Cryptographic, it will
   --  load as many words as needed, including key if not already loaded. It
   --  will then request again. Normal model is that the DMA interrupts the
   --  processor when its length expires. Note that if the processor will write
   --  the key and optionally IV, it should not enable this until it has done
   --  so. Otherwise, the DMA will be expected to load those for the 1st block
   --  (when needed).
   type CTRL_DMA_I_Field is
     (
      --  DMA is not used. Processor writes the necessary words when WAITING is
      --  set (interrupts), unless AHB Master is used.
      Not_Used,
      --  DMA will push in the data.
      Push)
     with Size => 1;
   for CTRL_DMA_I_Field use
     (Not_Used => 0,
      Push => 1);

   --  Written to 1 to use DMA to drain the digest/output. If both DMA_I and
   --  DMA_O are set, the DMA has to know to switch direction and the
   --  locations. This can be used for crypto uses.
   type CTRL_DMA_O_Field is
     (
      --  DMA is not used. Processor reads the digest/output in response to
      --  DIGEST interrupt.
      Notused)
     with Size => 1;
   for CTRL_DMA_O_Field use
     (Notused => 0);

   --  Control register to enable and operate Hash and Crypto
   type CTRL_Register is record
      --  The operational mode to use, or 0 if none. Note that the CONFIG
      --  register will indicate if specific modes beyond SHA1 and SHA2-256 are
      --  available.
      Mode           : CTRL_Mode_Field := NXP_SVD.HASHCRYPT.Disabled;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Write-only. Written with 1 when starting a new Hash/Crypto. It self
      --  clears. Note that the WAITING Status bit will clear for a cycle
      --  during the initialization from New=1.
      New_Hash       : CTRL_New_Hash_Field := Ctrl_New_Hash_Field_Reset;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  Written with 1 to use DMA to fill INDATA. If Hash, will request from
      --  DMA for 16 words and then will process the Hash. If Cryptographic, it
      --  will load as many words as needed, including key if not already
      --  loaded. It will then request again. Normal model is that the DMA
      --  interrupts the processor when its length expires. Note that if the
      --  processor will write the key and optionally IV, it should not enable
      --  this until it has done so. Otherwise, the DMA will be expected to
      --  load those for the 1st block (when needed).
      DMA_I          : CTRL_DMA_I_Field := NXP_SVD.HASHCRYPT.Not_Used;
      --  Written to 1 to use DMA to drain the digest/output. If both DMA_I and
      --  DMA_O are set, the DMA has to know to switch direction and the
      --  locations. This can be used for crypto uses.
      DMA_O          : CTRL_DMA_O_Field := NXP_SVD.HASHCRYPT.Notused;
      --  unspecified
      Reserved_10_11 : HAL.UInt2 := 16#0#;
      --  If 1, will swap bytes in the word for SHA hashing. The default is
      --  byte order (so LSB is 1st byte) but this allows swapping to MSB is
      --  1st such as is shown in SHS spec. For cryptographic swapping, see the
      --  CRYPTCFG register.
      HASHSWPB       : Boolean := False;
      --  unspecified
      Reserved_13_31 : HAL.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CTRL_Register use record
      Mode           at 0 range 0 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      New_Hash       at 0 range 4 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      DMA_I          at 0 range 8 .. 8;
      DMA_O          at 0 range 9 .. 9;
      Reserved_10_11 at 0 range 10 .. 11;
      HASHSWPB       at 0 range 12 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   --  If 1, the block is waiting for more data to process.
   type STATUS_WAITING_Field is
     (
      --  Not waiting for data - may be disabled or may be busy. Note that for
      --  cryptographic uses, this is not set if IsLast is set nor will it set
      --  until at least 1 word is read of the output.
      Not_Waiting,
      --  Waiting for data to be written in (16 words)
      Waiting)
     with Size => 1;
   for STATUS_WAITING_Field use
     (Not_Waiting => 0,
      Waiting => 1);

   --  For Hash, if 1 then a DIGEST is ready and waiting and there is no active
   --  next block already started. For Cryptographic uses, this will be set for
   --  each block processed, indicating OUTDATA (and OUTDATA2 if larger output)
   --  contains the next value to read out. This is cleared when any data is
   --  written, when New is written, for Cryptographic uses when the last word
   --  is read out, or when the block is disabled.
   type STATUS_DIGEST_Field is
     (
      --  No Digest is ready
      Not_Ready,
      --  Digest is ready. Application may read it or may write more data
      Ready)
     with Size => 1;
   for STATUS_DIGEST_Field use
     (Not_Ready => 0,
      Ready => 1);

   --  If 1, an error occurred. For normal uses, this is due to an attempted
   --  overrun: INDATA was written when it was not appropriate. For Master
   --  cases, this is an AHB bus error; the COUNT field will indicate which
   --  block it was on.
   type STATUS_ERROR_Field is
     (
      --  No error.
      No_Error,
      --  An error occurred since last cleared (written 1 to clear).
      Error)
     with Size => 1;
   for STATUS_ERROR_Field use
     (No_Error => 0,
      Error => 1);

   --  Indicates the block wants the key to be written in (set along with
   --  WAITING)
   type STATUS_NEEDKEY_Field is
     (
      --  No Key is needed and writes will not be treated as Key
      Not_Need,
      --  Key is needed and INDATA/ALIAS will be accepted as Key. Will also set
      --  WAITING.
      Need)
     with Size => 1;
   for STATUS_NEEDKEY_Field use
     (Not_Need => 0,
      Need => 1);

   --  Indicates the block wants an IV/NONE to be written in (set along with
   --  WAITING)
   type STATUS_NEEDIV_Field is
     (
      --  No IV/Nonce is needed, either because written already or because not
      --  needed.
      Not_Need,
      --  IV/Nonce is needed and INDATA/ALIAS will be accepted as IV/Nonce.
      --  Will also set WAITING.
      Need)
     with Size => 1;
   for STATUS_NEEDIV_Field use
     (Not_Need => 0,
      Need => 1);

   subtype STATUS_ICBIDX_Field is HAL.UInt6;

   --  Indicates status of Hash peripheral.
   type STATUS_Register is record
      --  Read-only. If 1, the block is waiting for more data to process.
      WAITING        : STATUS_WAITING_Field := NXP_SVD.HASHCRYPT.Not_Waiting;
      --  Read-only. For Hash, if 1 then a DIGEST is ready and waiting and
      --  there is no active next block already started. For Cryptographic
      --  uses, this will be set for each block processed, indicating OUTDATA
      --  (and OUTDATA2 if larger output) contains the next value to read out.
      --  This is cleared when any data is written, when New is written, for
      --  Cryptographic uses when the last word is read out, or when the block
      --  is disabled.
      DIGEST         : STATUS_DIGEST_Field := NXP_SVD.HASHCRYPT.Not_Ready;
      --  Write data bit of one shall clear (set to zero) the corresponding bit
      --  in the field. If 1, an error occurred. For normal uses, this is due
      --  to an attempted overrun: INDATA was written when it was not
      --  appropriate. For Master cases, this is an AHB bus error; the COUNT
      --  field will indicate which block it was on.
      ERROR          : STATUS_ERROR_Field := NXP_SVD.HASHCRYPT.No_Error;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Read-only. Indicates the block wants the key to be written in (set
      --  along with WAITING)
      NEEDKEY        : STATUS_NEEDKEY_Field := NXP_SVD.HASHCRYPT.Not_Need;
      --  Read-only. Indicates the block wants an IV/NONE to be written in (set
      --  along with WAITING)
      NEEDIV         : STATUS_NEEDIV_Field := NXP_SVD.HASHCRYPT.Not_Need;
      --  unspecified
      Reserved_6_15  : HAL.UInt10 := 16#0#;
      --  Read-only. If ICB-AES is selected, then reads as the ICB index count
      --  based on ICBSTRM (from CRYPTCFG). That is, if 3 bits of ICBSTRM, then
      --  this will count from 0 to 7 and then back to 0. On 0, it has to
      --  compute the full ICB, quicker when not 0.
      ICBIDX         : STATUS_ICBIDX_Field := 16#0#;
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for STATUS_Register use record
      WAITING        at 0 range 0 .. 0;
      DIGEST         at 0 range 1 .. 1;
      ERROR          at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      NEEDKEY        at 0 range 4 .. 4;
      NEEDIV         at 0 range 5 .. 5;
      Reserved_6_15  at 0 range 6 .. 15;
      ICBIDX         at 0 range 16 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  Indicates if should interrupt when waiting for data input.
   type INTENSET_WAITING_Field is
     (
      --  Will not interrupt when waiting.
      No_Interrupt,
      --  Will interrupt when waiting
      Interrupt)
     with Size => 1;
   for INTENSET_WAITING_Field use
     (No_Interrupt => 0,
      Interrupt => 1);

   --  Indicates if should interrupt when Digest (or Outdata) is ready
   --  (completed a hash/crypto or completed a full sequence).
   type INTENSET_DIGEST_Field is
     (
      --  Will not interrupt when Digest is ready
      No_Interrupt,
      --  Will interrupt when Digest is ready. Interrupt cleared by writing
      --  more data, starting a new Hash, or disabling (done).
      Interrupt)
     with Size => 1;
   for INTENSET_DIGEST_Field use
     (No_Interrupt => 0,
      Interrupt => 1);

   --  Indicates if should interrupt on an ERROR (as defined in Status)
   type INTENSET_ERROR_Field is
     (
      --  Will not interrupt on Error.
      Not_Interrupt,
      --  Will interrupt on Error (until cleared).
      Interrupt)
     with Size => 1;
   for INTENSET_ERROR_Field use
     (Not_Interrupt => 0,
      Interrupt => 1);

   --  Write 1 to enable interrupts; reads back with which are set.
   type INTENSET_Register is record
      --  Indicates if should interrupt when waiting for data input.
      WAITING       : INTENSET_WAITING_Field :=
                       NXP_SVD.HASHCRYPT.No_Interrupt;
      --  Indicates if should interrupt when Digest (or Outdata) is ready
      --  (completed a hash/crypto or completed a full sequence).
      DIGEST        : INTENSET_DIGEST_Field := NXP_SVD.HASHCRYPT.No_Interrupt;
      --  Indicates if should interrupt on an ERROR (as defined in Status)
      ERROR         : INTENSET_ERROR_Field := NXP_SVD.HASHCRYPT.Not_Interrupt;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENSET_Register use record
      WAITING       at 0 range 0 .. 0;
      DIGEST        at 0 range 1 .. 1;
      ERROR         at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  Write 1 to clear interrupts.
   type INTENCLR_Register is record
      --  Write data bit of one shall clear (set to zero) the corresponding bit
      --  in the field. Write 1 to clear mask.
      WAITING       : Boolean := False;
      --  Write data bit of one shall clear (set to zero) the corresponding bit
      --  in the field. Write 1 to clear mask.
      DIGEST        : Boolean := False;
      --  Write data bit of one shall clear (set to zero) the corresponding bit
      --  in the field. Write 1 to clear mask.
      ERROR         : Boolean := False;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENCLR_Register use record
      WAITING       at 0 range 0 .. 0;
      DIGEST        at 0 range 1 .. 1;
      ERROR         at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  Enables mastering.
   type MEMCTRL_MASTER_Field is
     (
      --  Mastering is not used and the normal DMA or Interrupt based model is
      --  used with INDATA.
      Not_Used,
      --  Mastering is enabled and DMA and INDATA should not be used.
      Enabled)
     with Size => 1;
   for MEMCTRL_MASTER_Field use
     (Not_Used => 0,
      Enabled => 1);

   subtype MEMCTRL_COUNT_Field is HAL.UInt11;

   --  Setup Master to access memory (if available)
   type MEMCTRL_Register is record
      --  Enables mastering.
      MASTER         : MEMCTRL_MASTER_Field := NXP_SVD.HASHCRYPT.Not_Used;
      --  unspecified
      Reserved_1_15  : HAL.UInt15 := 16#0#;
      --  Number of 512-bit (128-bit if AES, except 1st block which may include
      --  key and IV) blocks to copy starting at MEMADDR. This register will
      --  decrement after each block is copied, ending in 0. For Hash, the
      --  DIGEST interrupt will occur when it reaches 0. Fro AES, the
      --  DIGEST/OUTDATA interrupt will occur on ever block. If a bus error
      --  occurs, it will stop with this field set to the block that failed.
      --  0:Done - nothing to process. 1 to 2K: Number of 512-bit (or 128bit)
      --  blocks to hash.
      COUNT          : MEMCTRL_COUNT_Field := 16#0#;
      --  unspecified
      Reserved_27_31 : HAL.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MEMCTRL_Register use record
      MASTER         at 0 range 0 .. 0;
      Reserved_1_15  at 0 range 1 .. 15;
      COUNT          at 0 range 16 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   --  no description available

   --  no description available
   type ALIAS_Registers is array (0 .. 6) of HAL.UInt32
     with Volatile;

   --  no description available

   --  no description available
   type DIGEST0_Registers is array (0 .. 7) of HAL.UInt32
     with Volatile;

   --  AES Cipher mode to use if plain AES
   type CRYPTCFG_AESMODE_Field is
     (
      --  ECB - used as is
      Ecb,
      --  CBC mode (see details on IV/nonce)
      Cbc,
      --  CTR mode (see details on IV/nonce). See also AESCTRPOS.
      Ctr)
     with Size => 2;
   for CRYPTCFG_AESMODE_Field use
     (Ecb => 0,
      Cbc => 1,
      Ctr => 2);

   --  AES ECB direction. Only encryption used if CTR mode or manual modes such
   --  as CFB
   type CRYPTCFG_AESDECRYPT_Field is
     (
      --  Encrypt
      Encrypt,
      --  Decrypt
      Decrypt)
     with Size => 1;
   for CRYPTCFG_AESDECRYPT_Field use
     (Encrypt => 0,
      Decrypt => 1);

   --  Selects the Hidden Secret key vs. User key, if provided. If security
   --  levels are used, only the highest level is permitted to select this.
   type CRYPTCFG_AESSECRET_Field is
     (
      --  User key provided in normal way
      Normal_Way,
      --  Secret key provided in hidden way by HW
      Hidden_Way)
     with Size => 1;
   for CRYPTCFG_AESSECRET_Field use
     (Normal_Way => 0,
      Hidden_Way => 1);

   --  Sets the AES key size
   type CRYPTCFG_AESKEYSZ_Field is
     (
      --  128 bit key
      Bits_128,
      --  192 bit key
      Bits_192,
      --  256 bit key
      Bits_256)
     with Size => 2;
   for CRYPTCFG_AESKEYSZ_Field use
     (Bits_128 => 0,
      Bits_192 => 1,
      Bits_256 => 2);

   subtype CRYPTCFG_AESCTRPOS_Field is HAL.UInt3;

   --  This sets the ICB size between 32 and 128 bits, using the following
   --  rules. Note that the counter is assumed to occupy the low order bits of
   --  the IV.
   type CRYPTCFG_ICBSZ_Field is
     (
      --  32 bits of the IV/ctr are used (from 127:96)
      Bits_32,
      --  64 bits of the IV/ctr are used (from 127:64)
      Bits_64,
      --  96 bits of the IV/ctr are used (from 127:32)
      Bits_96,
      --  All 128 bits of the IV/ctr are used
      Bit_128)
     with Size => 2;
   for CRYPTCFG_ICBSZ_Field use
     (Bits_32 => 0,
      Bits_64 => 1,
      Bits_96 => 2,
      Bit_128 => 3);

   --  The size of the ICB-AES stream that can be pushed before needing to
   --  compute a new IV/ctr (counter start). This optimizes the performance of
   --  the stream of blocks after the 1st.
   type CRYPTCFG_ICBSTRM_Field is
     (
      --  8 blocks
      Blocks_8,
      --  16 blocks
      Blocks_16,
      --  32 blocks
      Blocks_32,
      --  64 blocks
      Blocks_64)
     with Size => 2;
   for CRYPTCFG_ICBSTRM_Field use
     (Blocks_8 => 0,
      Blocks_16 => 1,
      Blocks_32 => 2,
      Blocks_64 => 3);

   --  Crypto settings for AES and Salsa and ChaCha
   type CRYPTCFG_Register is record
      --  If 1, OUTDATA0 will be read Most significant word 1st for AES. Else
      --  it will be read in normal little endian - Least significant word 1st.
      --  Note: only if allowed by configuration.
      MSW1ST_OUT     : Boolean := False;
      --  If 1, will Swap the key input (bytes in each word).
      SWAPKEY        : Boolean := False;
      --  If 1, will SWAP the data and IV inputs (bytes in each word).
      SWAPDAT        : Boolean := False;
      --  If 1, load of key, IV, and data is MSW 1st for AES. Else, the words
      --  are little endian. Note: only if allowed by configuration.
      MSW1ST         : Boolean := False;
      --  AES Cipher mode to use if plain AES
      AESMODE        : CRYPTCFG_AESMODE_Field := NXP_SVD.HASHCRYPT.Ecb;
      --  AES ECB direction. Only encryption used if CTR mode or manual modes
      --  such as CFB
      AESDECRYPT     : CRYPTCFG_AESDECRYPT_Field := NXP_SVD.HASHCRYPT.Encrypt;
      --  Selects the Hidden Secret key vs. User key, if provided. If security
      --  levels are used, only the highest level is permitted to select this.
      AESSECRET      : CRYPTCFG_AESSECRET_Field :=
                        NXP_SVD.HASHCRYPT.Normal_Way;
      --  Sets the AES key size
      AESKEYSZ       : CRYPTCFG_AESKEYSZ_Field := NXP_SVD.HASHCRYPT.Bits_128;
      --  Halfword position of 16b counter in IV if AESMODE is CTR (position is
      --  fixed for Salsa and ChaCha). Only supports 16b counter, so
      --  application must control any additional bytes if using more. The
      --  16-bit counter is read from the IV and incremented by 1 each time.
      --  Any other use CTR should use ECB directly and do its own XOR and so
      --  on.
      AESCTRPOS      : CRYPTCFG_AESCTRPOS_Field := 16#0#;
      --  unspecified
      Reserved_13_15 : HAL.UInt3 := 16#0#;
      --  Is 1 if last stream block. If not 1, then the engine will compute the
      --  next "hash".
      STREAMLAST     : Boolean := False;
      --  unspecified
      Reserved_17_19 : HAL.UInt3 := 16#0#;
      --  This sets the ICB size between 32 and 128 bits, using the following
      --  rules. Note that the counter is assumed to occupy the low order bits
      --  of the IV.
      ICBSZ          : CRYPTCFG_ICBSZ_Field := NXP_SVD.HASHCRYPT.Bits_32;
      --  The size of the ICB-AES stream that can be pushed before needing to
      --  compute a new IV/ctr (counter start). This optimizes the performance
      --  of the stream of blocks after the 1st.
      ICBSTRM        : CRYPTCFG_ICBSTRM_Field := NXP_SVD.HASHCRYPT.Blocks_8;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CRYPTCFG_Register use record
      MSW1ST_OUT     at 0 range 0 .. 0;
      SWAPKEY        at 0 range 1 .. 1;
      SWAPDAT        at 0 range 2 .. 2;
      MSW1ST         at 0 range 3 .. 3;
      AESMODE        at 0 range 4 .. 5;
      AESDECRYPT     at 0 range 6 .. 6;
      AESSECRET      at 0 range 7 .. 7;
      AESKEYSZ       at 0 range 8 .. 9;
      AESCTRPOS      at 0 range 10 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      STREAMLAST     at 0 range 16 .. 16;
      Reserved_17_19 at 0 range 17 .. 19;
      ICBSZ          at 0 range 20 .. 21;
      ICBSTRM        at 0 range 22 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  Returns the configuration of this block in this chip - indicates what
   --  services are available.
   type CONFIG_Register is record
      --  Read-only. 1 if 2 x 512 bit buffers, 0 if only 1 x 512 bit
      DUAL           : Boolean;
      --  Read-only. 1 if DMA is connected
      DMA            : Boolean;
      --  unspecified
      Reserved_2_2   : HAL.Bit;
      --  Read-only. 1 if AHB Master is enabled
      AHB            : Boolean;
      --  unspecified
      Reserved_4_5   : HAL.UInt2;
      --  Read-only. 1 if AES 128 included
      AES            : Boolean;
      --  Read-only. 1 if AES 192 and 256 also included
      AESKEY         : Boolean;
      --  Read-only. 1 if AES Secret key available
      SECRET         : Boolean;
      --  unspecified
      Reserved_9_10  : HAL.UInt2;
      --  Read-only. 1 if ICB over AES included
      ICB            : Boolean;
      --  unspecified
      Reserved_12_31 : HAL.UInt20;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CONFIG_Register use record
      DUAL           at 0 range 0 .. 0;
      DMA            at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      AHB            at 0 range 3 .. 3;
      Reserved_4_5   at 0 range 4 .. 5;
      AES            at 0 range 6 .. 6;
      AESKEY         at 0 range 7 .. 7;
      SECRET         at 0 range 8 .. 8;
      Reserved_9_10  at 0 range 9 .. 10;
      ICB            at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   --  Write 1 to secure-lock this block (if running in a security state).
   --  Write 0 to unlock. If locked already, may only write if at same or
   --  higher security level as lock. Reads as: 0 if unlocked, else 1, 2, 3 to
   --  indicate security level it is locked at. NOTE: this and ID are the only
   --  readable registers if locked and current state is lower than lock level.
   type LOCK_SECLOCK_Field is
     (
      --  Unlocks, so block is open to all. But, AHB Master will only issue
      --  non-secure requests.
      Unlock,
      --  Locks to the current security level. AHB Master will issue requests
      --  at this level.
      Lock)
     with Size => 2;
   for LOCK_SECLOCK_Field use
     (Unlock => 0,
      Lock => 1);

   subtype LOCK_PATTERN_Field is HAL.UInt12;

   --  Lock register allows locking to the current security level or unlocking
   --  by the lock holding level.
   type LOCK_Register is record
      --  Write 1 to secure-lock this block (if running in a security state).
      --  Write 0 to unlock. If locked already, may only write if at same or
      --  higher security level as lock. Reads as: 0 if unlocked, else 1, 2, 3
      --  to indicate security level it is locked at. NOTE: this and ID are the
      --  only readable registers if locked and current state is lower than
      --  lock level.
      SECLOCK        : LOCK_SECLOCK_Field := NXP_SVD.HASHCRYPT.Unlock;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  Must write 0xA75 to change lock state. A75:Pattern needed to change
      --  bits 1:0
      PATTERN        : LOCK_PATTERN_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for LOCK_Register use record
      SECLOCK        at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      PATTERN        at 0 range 4 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  no description available

   --  no description available
   type MASK_Registers is array (0 .. 3) of HAL.UInt32
     with Volatile;

   -----------------
   -- Peripherals --
   -----------------

   --  Hash-Crypt peripheral
   type HASHCRYPT_Peripheral is record
      --  Control register to enable and operate Hash and Crypto
      CTRL     : aliased CTRL_Register;
      --  Indicates status of Hash peripheral.
      STATUS   : aliased STATUS_Register;
      --  Write 1 to enable interrupts; reads back with which are set.
      INTENSET : aliased INTENSET_Register;
      --  Write 1 to clear interrupts.
      INTENCLR : aliased INTENCLR_Register;
      --  Setup Master to access memory (if available)
      MEMCTRL  : aliased MEMCTRL_Register;
      --  Address to start memory access from (if available).
      MEMADDR  : aliased HAL.UInt32;
      --  Input of 16 words at a time to load up buffer.
      INDATA   : aliased HAL.UInt32;
      --  no description available
      ALIAS    : aliased ALIAS_Registers;
      --  no description available
      DIGEST0  : aliased DIGEST0_Registers;
      --  Crypto settings for AES and Salsa and ChaCha
      CRYPTCFG : aliased CRYPTCFG_Register;
      --  Returns the configuration of this block in this chip - indicates what
      --  services are available.
      CONFIG   : aliased CONFIG_Register;
      --  Lock register allows locking to the current security level or
      --  unlocking by the lock holding level.
      LOCK     : aliased LOCK_Register;
      --  no description available
      MASK     : aliased MASK_Registers;
   end record
     with Volatile;

   for HASHCRYPT_Peripheral use record
      CTRL     at 16#0# range 0 .. 31;
      STATUS   at 16#4# range 0 .. 31;
      INTENSET at 16#8# range 0 .. 31;
      INTENCLR at 16#C# range 0 .. 31;
      MEMCTRL  at 16#10# range 0 .. 31;
      MEMADDR  at 16#14# range 0 .. 31;
      INDATA   at 16#20# range 0 .. 31;
      ALIAS    at 16#24# range 0 .. 223;
      DIGEST0  at 16#40# range 0 .. 255;
      CRYPTCFG at 16#80# range 0 .. 31;
      CONFIG   at 16#84# range 0 .. 31;
      LOCK     at 16#8C# range 0 .. 31;
      MASK     at 16#90# range 0 .. 127;
   end record;

   --  Hash-Crypt peripheral
   HASHCRYPT_Periph : aliased HASHCRYPT_Peripheral
     with Import, Address => System'To_Address (16#400A4000#);

end NXP_SVD.HASHCRYPT;
