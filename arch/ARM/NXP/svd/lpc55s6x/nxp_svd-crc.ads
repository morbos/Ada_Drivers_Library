--  Copyright 2016-2019 NXP
--  All rights reserved.SPDX-License-Identifier: BSD-3-Clause


--  This spec has been automatically generated from LPC55S6x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NXP_SVD.CRC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype MODE_CRC_POLY_Field is HAL.UInt2;

   --  CRC mode register
   type MODE_Register is record
      --  CRC polynomial: 1X = CRC-32 polynomial 01 = CRC-16 polynomial 00 =
      --  CRC-CCITT polynomial
      CRC_POLY      : MODE_CRC_POLY_Field := 16#0#;
      --  Data bit order: 1 = Bit order reverse for CRC_WR_DATA (per byte) 0 =
      --  No bit order reverse for CRC_WR_DATA (per byte)
      BIT_RVS_WR    : Boolean := False;
      --  Data complement: 1 = 1's complement for CRC_WR_DATA 0 = No 1's
      --  complement for CRC_WR_DATA
      CMPL_WR       : Boolean := False;
      --  CRC sum bit order: 1 = Bit order reverse for CRC_SUM 0 = No bit order
      --  reverse for CRC_SUM
      BIT_RVS_SUM   : Boolean := False;
      --  CRC sum complement: 1 = 1's complement for CRC_SUM 0 = No 1's
      --  complement for CRC_SUM
      CMPL_SUM      : Boolean := False;
      --  unspecified
      Reserved_6_31 : HAL.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MODE_Register use record
      CRC_POLY      at 0 range 0 .. 1;
      BIT_RVS_WR    at 0 range 2 .. 2;
      CMPL_WR       at 0 range 3 .. 3;
      BIT_RVS_SUM   at 0 range 4 .. 4;
      CMPL_SUM      at 0 range 5 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   type CRC_ENGINE_Disc is
     (
      Mode_1,
      Mode_2);

   --  CRC engine
   type CRC_ENGINE_Peripheral
     (Discriminent : CRC_ENGINE_Disc := Mode_1)
   is record
      --  CRC mode register
      MODE    : aliased MODE_Register;
      --  CRC seed register
      SEED    : aliased HAL.UInt32;
      case Discriminent is
         when Mode_1 =>
            --  CRC checksum register
            SUM : aliased HAL.UInt32;
         when Mode_2 =>
            --  CRC data register
            WR_DATA : aliased HAL.UInt32;
      end case;
   end record
     with Unchecked_Union, Volatile;

   for CRC_ENGINE_Peripheral use record
      MODE    at 16#0# range 0 .. 31;
      SEED    at 16#4# range 0 .. 31;
      SUM     at 16#8# range 0 .. 31;
      WR_DATA at 16#8# range 0 .. 31;
   end record;

   --  CRC engine
   CRC_ENGINE_Periph : aliased CRC_ENGINE_Peripheral
     with Import, Address => System'To_Address (16#40095000#);

end NXP_SVD.CRC;
