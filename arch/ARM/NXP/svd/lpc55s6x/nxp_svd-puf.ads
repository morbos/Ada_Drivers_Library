--  Copyright 2016-2019 NXP
--  All rights reserved.SPDX-License-Identifier: BSD-3-Clause


--  This spec has been automatically generated from LPC55S6x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NXP_SVD.PUF is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  PUF Control register
   type CTRL_Register is record
      --  Begin Zeroize operation for PUF and go to Error state
      zeroize       : Boolean := False;
      --  Begin Enroll operation
      enroll        : Boolean := False;
      --  Begin Start operation
      start         : Boolean := False;
      --  Begin Set Intrinsic Key operation
      GENERATEKEY   : Boolean := False;
      --  Begin Set User Key operation
      SETKEY        : Boolean := False;
      --  unspecified
      Reserved_5_5  : HAL.Bit := 16#0#;
      --  Begin Get Key operation
      GETKEY        : Boolean := False;
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CTRL_Register use record
      zeroize       at 0 range 0 .. 0;
      enroll        at 0 range 1 .. 1;
      start         at 0 range 2 .. 2;
      GENERATEKEY   at 0 range 3 .. 3;
      SETKEY        at 0 range 4 .. 4;
      Reserved_5_5  at 0 range 5 .. 5;
      GETKEY        at 0 range 6 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   subtype KEYINDEX_KEYIDX_Field is HAL.UInt4;

   --  PUF Key Index register
   type KEYINDEX_Register is record
      --  Key index for Set Key operations
      KEYIDX        : KEYINDEX_KEYIDX_Field := 16#0#;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for KEYINDEX_Register use record
      KEYIDX        at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   subtype KEYSIZE_KEYSIZE_Field is HAL.UInt6;

   --  PUF Key Size register
   type KEYSIZE_Register is record
      --  Key size for Set Key operations
      KEYSIZE       : KEYSIZE_KEYSIZE_Field := 16#0#;
      --  unspecified
      Reserved_6_31 : HAL.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for KEYSIZE_Register use record
      KEYSIZE       at 0 range 0 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   --  PUF Status register
   type STAT_Register is record
      --  Read-only. Indicates that operation is in progress
      busy          : Boolean;
      --  Read-only. Last operation was successful
      SUCCESS       : Boolean;
      --  Read-only. PUF is in the Error state and no operations can be
      --  performed
      error         : Boolean;
      --  unspecified
      Reserved_3_3  : HAL.Bit;
      --  Read-only. Request for next part of key
      KEYINREQ      : Boolean;
      --  Read-only. Next part of key is available
      KEYOUTAVAIL   : Boolean;
      --  Read-only. Request for next part of AC/KC
      CODEINREQ     : Boolean;
      --  Read-only. Next part of AC/KC is available
      CODEOUTAVAIL  : Boolean;
      --  unspecified
      Reserved_8_31 : HAL.UInt24;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for STAT_Register use record
      busy          at 0 range 0 .. 0;
      SUCCESS       at 0 range 1 .. 1;
      error         at 0 range 2 .. 2;
      Reserved_3_3  at 0 range 3 .. 3;
      KEYINREQ      at 0 range 4 .. 4;
      KEYOUTAVAIL   at 0 range 5 .. 5;
      CODEINREQ     at 0 range 6 .. 6;
      CODEOUTAVAIL  at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  PUF Allow register
   type ALLOW_Register is record
      --  Read-only. Enroll operation is allowed
      ALLOWENROLL   : Boolean;
      --  Read-only. Start operation is allowed
      ALLOWSTART    : Boolean;
      --  Read-only. Set Key operations are allowed
      ALLOWSETKEY   : Boolean;
      --  Read-only. Get Key operation is allowed
      ALLOWGETKEY   : Boolean;
      --  unspecified
      Reserved_4_31 : HAL.UInt28;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ALLOW_Register use record
      ALLOWENROLL   at 0 range 0 .. 0;
      ALLOWSTART    at 0 range 1 .. 1;
      ALLOWSETKEY   at 0 range 2 .. 2;
      ALLOWGETKEY   at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   subtype KEYOUTINDEX_KEYOUTIDX_Field is HAL.UInt4;

   --  PUF Key Output Index register
   type KEYOUTINDEX_Register is record
      --  Read-only. Key index for the key that is currently output via the Key
      --  Output register
      KEYOUTIDX     : KEYOUTINDEX_KEYOUTIDX_Field;
      --  unspecified
      Reserved_4_31 : HAL.UInt28;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for KEYOUTINDEX_Register use record
      KEYOUTIDX     at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  PUF Interface Status and clear register
   type IFSTAT_Register is record
      --  Indicates that an APB error has occurred,Writing logic1 clears the
      --  if_error bit
      ERROR         : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IFSTAT_Register use record
      ERROR         at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  PUF Interrupt Enable
   type INTEN_Register is record
      --  Enable corresponding interrupt. Note that bit numbers match those
      --  assigned in QK_SR (Quiddikey Status Register)
      READYEN        : Boolean := False;
      --  Enable corresponding interrupt. Note that bit numbers match those
      --  assigned in QK_SR (Quiddikey Status Register)
      SUCCESEN       : Boolean := False;
      --  Enable corresponding interrupt. Note that bit numbers match those
      --  assigned in QK_SR (Quiddikey Status Register)
      ERROREN        : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Enable corresponding interrupt. Note that bit numbers match those
      --  assigned in QK_SR (Quiddikey Status Register)
      KEYINREQEN     : Boolean := False;
      --  Enable corresponding interrupt. Note that bit numbers match those
      --  assigned in QK_SR (Quiddikey Status Register)
      KEYOUTAVAILEN  : Boolean := False;
      --  Enable corresponding interrupt. Note that bit numbers match those
      --  assigned in QK_SR (Quiddikey Status Register)
      CODEINREQEN    : Boolean := False;
      --  Enable corresponding interrupt. Note that bit numbers match those
      --  assigned in QK_SR (Quiddikey Status Register)
      CODEOUTAVAILEN : Boolean := False;
      --  unspecified
      Reserved_8_31  : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTEN_Register use record
      READYEN        at 0 range 0 .. 0;
      SUCCESEN       at 0 range 1 .. 1;
      ERROREN        at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      KEYINREQEN     at 0 range 4 .. 4;
      KEYOUTAVAILEN  at 0 range 5 .. 5;
      CODEINREQEN    at 0 range 6 .. 6;
      CODEOUTAVAILEN at 0 range 7 .. 7;
      Reserved_8_31  at 0 range 8 .. 31;
   end record;

   --  PUF interrupt status
   type INTSTAT_Register is record
      --  Triggers on falling edge of busy, write 1 to clear
      READY         : Boolean := False;
      --  Level sensitive interrupt, cleared when interrupt source clears
      SUCCESS       : Boolean := False;
      --  Level sensitive interrupt, cleared when interrupt source clears
      ERROR         : Boolean := False;
      --  unspecified
      Reserved_3_3  : HAL.Bit := 16#0#;
      --  Level sensitive interrupt, cleared when interrupt source clears
      KEYINREQ      : Boolean := False;
      --  Level sensitive interrupt, cleared when interrupt source clears
      KEYOUTAVAIL   : Boolean := False;
      --  Level sensitive interrupt, cleared when interrupt source clears
      CODEINREQ     : Boolean := False;
      --  Level sensitive interrupt, cleared when interrupt source clears
      CODEOUTAVAIL  : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTSTAT_Register use record
      READY         at 0 range 0 .. 0;
      SUCCESS       at 0 range 1 .. 1;
      ERROR         at 0 range 2 .. 2;
      Reserved_3_3  at 0 range 3 .. 3;
      KEYINREQ      at 0 range 4 .. 4;
      KEYOUTAVAIL   at 0 range 5 .. 5;
      CODEINREQ     at 0 range 6 .. 6;
      CODEOUTAVAIL  at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  PUF RAM Power Control
   type PWRCTRL_Register is record
      --  Power on the PUF RAM.
      RAMON         : Boolean := False;
      --  PUF RAM status.
      RAMSTAT       : Boolean := False;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#3E#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWRCTRL_Register use record
      RAMON         at 0 range 0 .. 0;
      RAMSTAT       at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  PUF config register for block bits
   type CFG_Register is record
      --  Block enroll operation. Write 1 to set, cleared on reset.
      BLOCKENROLL_SETKEY : Boolean := False;
      --  Block set key operation. Write 1 to set, cleared on reset.
      BLOCKKEYOUTPUT     : Boolean := False;
      --  unspecified
      Reserved_2_31      : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CFG_Register use record
      BLOCKENROLL_SETKEY at 0 range 0 .. 0;
      BLOCKKEYOUTPUT     at 0 range 1 .. 1;
      Reserved_2_31      at 0 range 2 .. 31;
   end record;

   --  KEYLOCK_KEY array element
   subtype KEYLOCK_KEY_Element is HAL.UInt2;

   --  KEYLOCK_KEY array
   type KEYLOCK_KEY_Field_Array is array (0 .. 3) of KEYLOCK_KEY_Element
     with Component_Size => 2, Size => 8;

   --  Type definition for KEYLOCK_KEY
   type KEYLOCK_KEY_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  KEY as a value
            Val : HAL.UInt8;
         when True =>
            --  KEY as an array
            Arr : KEYLOCK_KEY_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 8;

   for KEYLOCK_KEY_Field use record
      Val at 0 range 0 .. 7;
      Arr at 0 range 0 .. 7;
   end record;

   --  Only reset in case of full IC reset
   type KEYLOCK_Register is record
      --  "10:Write access to KEY0MASK, KEYENABLE.KEY0 and KEYRESET.KEY0 is
      --  allowed. 00, 01, 11:Write access to KEY0MASK, KEYENABLE.KEY0 and
      --  KEYRESET.KEY0 is NOT allowed. Important Note : Once this field is
      --  written with a value different from '10', its value can no longer be
      --  modified until un Power On Reset occurs."
      KEY           : KEYLOCK_KEY_Field := (As_Array => False, Val => 16#2#);
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for KEYLOCK_Register use record
      KEY           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  KEYENABLE_KEY array element
   subtype KEYENABLE_KEY_Element is HAL.UInt2;

   --  KEYENABLE_KEY array
   type KEYENABLE_KEY_Field_Array is array (0 .. 3) of KEYENABLE_KEY_Element
     with Component_Size => 2, Size => 8;

   --  Type definition for KEYENABLE_KEY
   type KEYENABLE_KEY_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  KEY as a value
            Val : HAL.UInt8;
         when True =>
            --  KEY as an array
            Arr : KEYENABLE_KEY_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 8;

   for KEYENABLE_KEY_Field use record
      Val at 0 range 0 .. 7;
      Arr at 0 range 0 .. 7;
   end record;

   --  no description available
   type KEYENABLE_Register is record
      --  "10: Data coming out from PUF Index 0 interface are shifted in KEY0
      --  register. 00, 01, 11 : Data coming out from PUF Index 0 interface are
      --  NOT shifted in KEY0 register."
      KEY           : KEYENABLE_KEY_Field :=
                       (As_Array => False, Val => 16#1#);
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for KEYENABLE_Register use record
      KEY           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  KEYRESET_KEY array element
   subtype KEYRESET_KEY_Element is HAL.UInt2;

   --  KEYRESET_KEY array
   type KEYRESET_KEY_Field_Array is array (0 .. 3) of KEYRESET_KEY_Element
     with Component_Size => 2, Size => 8;

   --  Type definition for KEYRESET_KEY
   type KEYRESET_KEY_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  KEY as a value
            Val : HAL.UInt8;
         when True =>
            --  KEY as an array
            Arr : KEYRESET_KEY_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 8;

   for KEYRESET_KEY_Field use record
      Val at 0 range 0 .. 7;
      Arr at 0 range 0 .. 7;
   end record;

   --  Reinitialize Keys shift registers counters
   type KEYRESET_Register is record
      --  Write-only. 10: Reset KEY0 shift register. Self clearing. Must be
      --  done before loading any new key.
      KEY           : KEYRESET_KEY_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for KEYRESET_Register use record
      KEY           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  IDXBLK_L_IDX array element
   subtype IDXBLK_L_IDX_Element is HAL.UInt2;

   --  IDXBLK_L_IDX array
   type IDXBLK_L_IDX_Field_Array is array (1 .. 7) of IDXBLK_L_IDX_Element
     with Component_Size => 2, Size => 14;

   --  Type definition for IDXBLK_L_IDX
   type IDXBLK_L_IDX_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IDX as a value
            Val : HAL.UInt14;
         when True =>
            --  IDX as an array
            Arr : IDXBLK_L_IDX_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 14;

   for IDXBLK_L_IDX_Field use record
      Val at 0 range 0 .. 13;
      Arr at 0 range 0 .. 13;
   end record;

   subtype IDXBLK_L_LOCK_IDX_Field is HAL.UInt2;

   --  no description available
   type IDXBLK_L_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#2#;
      --  Use to block PUF index 1
      IDX            : IDXBLK_L_IDX_Field :=
                        (As_Array => False, Val => 16#2#);
      --  unspecified
      Reserved_16_29 : HAL.UInt14 := 16#0#;
      --  Write-only. Lock 0 to 7 PUF key indexes
      LOCK_IDX       : IDXBLK_L_LOCK_IDX_Field := 16#2#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IDXBLK_L_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      IDX            at 0 range 2 .. 15;
      Reserved_16_29 at 0 range 16 .. 29;
      LOCK_IDX       at 0 range 30 .. 31;
   end record;

   --  IDXBLK_H_DP_IDX array element
   subtype IDXBLK_H_DP_IDX_Element is HAL.UInt2;

   --  IDXBLK_H_DP_IDX array
   type IDXBLK_H_DP_IDX_Field_Array is array (8 .. 15)
     of IDXBLK_H_DP_IDX_Element
     with Component_Size => 2, Size => 16;

   --  Type definition for IDXBLK_H_DP_IDX
   type IDXBLK_H_DP_IDX_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IDX as a value
            Val : HAL.UInt16;
         when True =>
            --  IDX as an array
            Arr : IDXBLK_H_DP_IDX_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for IDXBLK_H_DP_IDX_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  no description available
   type IDXBLK_H_DP_Register is record
      --  Use to block PUF index 8
      IDX            : IDXBLK_H_DP_IDX_Field :=
                        (As_Array => False, Val => 16#2#);
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IDXBLK_H_DP_Register use record
      IDX            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  Only reset in case of full IC reset

   --  Only reset in case of full IC reset
   type KEYMASK_Registers is array (0 .. 3) of HAL.UInt32
     with Volatile;

   --  IDXBLK_H_IDX array element
   subtype IDXBLK_H_IDX_Element is HAL.UInt2;

   --  IDXBLK_H_IDX array
   type IDXBLK_H_IDX_Field_Array is array (8 .. 15) of IDXBLK_H_IDX_Element
     with Component_Size => 2, Size => 16;

   --  Type definition for IDXBLK_H_IDX
   type IDXBLK_H_IDX_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IDX as a value
            Val : HAL.UInt16;
         when True =>
            --  IDX as an array
            Arr : IDXBLK_H_IDX_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for IDXBLK_H_IDX_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   subtype IDXBLK_H_LOCK_IDX_Field is HAL.UInt2;

   --  no description available
   type IDXBLK_H_Register is record
      --  Use to block PUF index 8
      IDX            : IDXBLK_H_IDX_Field :=
                        (As_Array => False, Val => 16#2#);
      --  unspecified
      Reserved_16_29 : HAL.UInt14 := 16#0#;
      --  Write-only. Lock 8 to 15 PUF key indexes
      LOCK_IDX       : IDXBLK_H_LOCK_IDX_Field := 16#2#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IDXBLK_H_Register use record
      IDX            at 0 range 0 .. 15;
      Reserved_16_29 at 0 range 16 .. 29;
      LOCK_IDX       at 0 range 30 .. 31;
   end record;

   --  IDXBLK_L_DP_IDX array element
   subtype IDXBLK_L_DP_IDX_Element is HAL.UInt2;

   --  IDXBLK_L_DP_IDX array
   type IDXBLK_L_DP_IDX_Field_Array is array (1 .. 7)
     of IDXBLK_L_DP_IDX_Element
     with Component_Size => 2, Size => 14;

   --  Type definition for IDXBLK_L_DP_IDX
   type IDXBLK_L_DP_IDX_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IDX as a value
            Val : HAL.UInt14;
         when True =>
            --  IDX as an array
            Arr : IDXBLK_L_DP_IDX_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 14;

   for IDXBLK_L_DP_IDX_Field use record
      Val at 0 range 0 .. 13;
      Arr at 0 range 0 .. 13;
   end record;

   --  no description available
   type IDXBLK_L_DP_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#2#;
      --  Use to block PUF index 1
      IDX            : IDXBLK_L_DP_IDX_Field :=
                        (As_Array => False, Val => 16#2#);
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IDXBLK_L_DP_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      IDX            at 0 range 2 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  SHIFT_STATUS_KEY array element
   subtype SHIFT_STATUS_KEY_Element is HAL.UInt4;

   --  SHIFT_STATUS_KEY array
   type SHIFT_STATUS_KEY_Field_Array is array (0 .. 3)
     of SHIFT_STATUS_KEY_Element
     with Component_Size => 4, Size => 16;

   --  Type definition for SHIFT_STATUS_KEY
   type SHIFT_STATUS_KEY_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  KEY as a value
            Val : HAL.UInt16;
         when True =>
            --  KEY as an array
            Arr : SHIFT_STATUS_KEY_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for SHIFT_STATUS_KEY_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  no description available
   type SHIFT_STATUS_Register is record
      --  Read-only. Index counter from key 0 shift register
      KEY            : SHIFT_STATUS_KEY_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SHIFT_STATUS_Register use record
      KEY            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  PUFCTRL
   type PUF_Peripheral is record
      --  PUF Control register
      CTRL         : aliased CTRL_Register;
      --  PUF Key Index register
      KEYINDEX     : aliased KEYINDEX_Register;
      --  PUF Key Size register
      KEYSIZE      : aliased KEYSIZE_Register;
      --  PUF Status register
      STAT         : aliased STAT_Register;
      --  PUF Allow register
      ALLOW        : aliased ALLOW_Register;
      --  PUF Key Input register
      KEYINPUT     : aliased HAL.UInt32;
      --  PUF Code Input register
      CODEINPUT    : aliased HAL.UInt32;
      --  PUF Code Output register
      CODEOUTPUT   : aliased HAL.UInt32;
      --  PUF Key Output Index register
      KEYOUTINDEX  : aliased KEYOUTINDEX_Register;
      --  PUF Key Output register
      KEYOUTPUT    : aliased HAL.UInt32;
      --  PUF Interface Status and clear register
      IFSTAT       : aliased IFSTAT_Register;
      --  PUF version register.
      VERSION      : aliased HAL.UInt32;
      --  PUF Interrupt Enable
      INTEN        : aliased INTEN_Register;
      --  PUF interrupt status
      INTSTAT      : aliased INTSTAT_Register;
      --  PUF RAM Power Control
      PWRCTRL      : aliased PWRCTRL_Register;
      --  PUF config register for block bits
      CFG          : aliased CFG_Register;
      --  Only reset in case of full IC reset
      KEYLOCK      : aliased KEYLOCK_Register;
      --  no description available
      KEYENABLE    : aliased KEYENABLE_Register;
      --  Reinitialize Keys shift registers counters
      KEYRESET     : aliased KEYRESET_Register;
      --  no description available
      IDXBLK_L     : aliased IDXBLK_L_Register;
      --  no description available
      IDXBLK_H_DP  : aliased IDXBLK_H_DP_Register;
      --  Only reset in case of full IC reset
      KEYMASK      : aliased KEYMASK_Registers;
      --  no description available
      IDXBLK_H     : aliased IDXBLK_H_Register;
      --  no description available
      IDXBLK_L_DP  : aliased IDXBLK_L_DP_Register;
      --  no description available
      SHIFT_STATUS : aliased SHIFT_STATUS_Register;
   end record
     with Volatile;

   for PUF_Peripheral use record
      CTRL         at 16#0# range 0 .. 31;
      KEYINDEX     at 16#4# range 0 .. 31;
      KEYSIZE      at 16#8# range 0 .. 31;
      STAT         at 16#20# range 0 .. 31;
      ALLOW        at 16#28# range 0 .. 31;
      KEYINPUT     at 16#40# range 0 .. 31;
      CODEINPUT    at 16#44# range 0 .. 31;
      CODEOUTPUT   at 16#48# range 0 .. 31;
      KEYOUTINDEX  at 16#60# range 0 .. 31;
      KEYOUTPUT    at 16#64# range 0 .. 31;
      IFSTAT       at 16#DC# range 0 .. 31;
      VERSION      at 16#FC# range 0 .. 31;
      INTEN        at 16#100# range 0 .. 31;
      INTSTAT      at 16#104# range 0 .. 31;
      PWRCTRL      at 16#108# range 0 .. 31;
      CFG          at 16#10C# range 0 .. 31;
      KEYLOCK      at 16#200# range 0 .. 31;
      KEYENABLE    at 16#204# range 0 .. 31;
      KEYRESET     at 16#208# range 0 .. 31;
      IDXBLK_L     at 16#20C# range 0 .. 31;
      IDXBLK_H_DP  at 16#210# range 0 .. 31;
      KEYMASK      at 16#214# range 0 .. 127;
      IDXBLK_H     at 16#254# range 0 .. 31;
      IDXBLK_L_DP  at 16#258# range 0 .. 31;
      SHIFT_STATUS at 16#25C# range 0 .. 31;
   end record;

   --  PUFCTRL
   PUF_Periph : aliased PUF_Peripheral
     with Import, Address => System'To_Address (16#4003B000#);

end NXP_SVD.PUF;
