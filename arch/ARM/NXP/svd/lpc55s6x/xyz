--  Copyright 2016-2019 NXP
--  All rights reserved.SPDX-License-Identifier: BSD-3-Clause


--  This spec has been automatically generated from LPC55S6x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NXP_SVD.DBGMAILBOX is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  CRC mode register
   type CSW_Register is record
      --  Debugger will set this bit to 1 to request a resynchronrisation
      RESYNCH_REQ    : Boolean := False;
      --  Request is pending from debugger (i.e unread value in REQUEST)
      REQ_PENDING    : Boolean := False;
      --  Debugger overrun error (previous REQUEST overwritten before being
      --  picked up by ROM)
      DBG_OR_ERR     : Boolean := False;
      --  AHB overrun Error (Return value overwritten by ROM)
      AHB_OR_ERR     : Boolean := False;
      --  Soft Reset for DM (write-only from AHB, not readable and
      --  selfclearing). A write to this bit will cause a soft reset for DM.
      SOFT_RESET     : Boolean := False;
      --  Write-only. Write only bit. Once written will cause the chip to reset
      --  (note that the DM is not reset by this reset as it is only resettable
      --  by a SOFT reset or a POR/BOD event)
      CHIP_RESET_REQ : Boolean := False;
      --  unspecified
      Reserved_6_31  : HAL.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CSW_Register use record
      RESYNCH_REQ    at 0 range 0 .. 0;
      REQ_PENDING    at 0 range 1 .. 1;
      DBG_OR_ERR     at 0 range 2 .. 2;
      AHB_OR_ERR     at 0 range 3 .. 3;
      SOFT_RESET     at 0 range 4 .. 4;
      CHIP_RESET_REQ at 0 range 5 .. 5;
      Reserved_6_31  at 0 range 6 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  MCU Debugger Mailbox
   type DBGMAILBOX_Peripheral is record
      --  CRC mode register
      CSW      : aliased CSW_Register;
      --  CRC seed register
      REQUEST  : aliased HAL.UInt32;
      --  Return value from ROM.
      RETURN_k : aliased HAL.UInt32;
      --  Identification register
      ID       : aliased HAL.UInt32;
   end record
     with Volatile;

   for DBGMAILBOX_Peripheral use record
      CSW      at 16#0# range 0 .. 31;
      REQUEST  at 16#4# range 0 .. 31;
      RETURN_k at 16#8# range 0 .. 31;
      ID       at 16#FC# range 0 .. 31;
   end record;

   --  MCU Debugger Mailbox
   DBGMAILBOX_Periph : aliased DBGMAILBOX_Peripheral
     with Import, Address => System'To_Address (16#4009C000#);

end NXP_SVD.DBGMAILBOX;
