--  Copyright 2016-2019 NXP
--  All rights reserved.SPDX-License-Identifier: BSD-3-Clause


--  This spec has been automatically generated from LPC55S6x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NXP_SVD.RNG is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype COUNTER_VAL_CLK_RATIO_Field is HAL.UInt8;
   subtype COUNTER_VAL_REFRESH_CNT_Field is HAL.UInt5;

   --  no description available
   type COUNTER_VAL_Register is record
      --  Read-only. Gives the ratio between the internal clocks frequencies
      --  and the register clock frequency for evaluation and certification
      --  purposes.
      CLK_RATIO      : COUNTER_VAL_CLK_RATIO_Field;
      --  Read-only. Incremented (till max possible value) each time COUNTER
      --  was updated since last reading to any *_NUMBER.
      REFRESH_CNT    : COUNTER_VAL_REFRESH_CNT_Field;
      --  unspecified
      Reserved_13_31 : HAL.UInt19;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for COUNTER_VAL_Register use record
      CLK_RATIO      at 0 range 0 .. 7;
      REFRESH_CNT    at 0 range 8 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   subtype COUNTER_CFG_MODE_Field is HAL.UInt2;
   subtype COUNTER_CFG_CLOCK_SEL_Field is HAL.UInt3;
   subtype COUNTER_CFG_SHIFT4X_Field is HAL.UInt3;

   --  no description available
   type COUNTER_CFG_Register is record
      --  00: disabled 01: update once.
      MODE          : COUNTER_CFG_MODE_Field := 16#0#;
      --  Selects the internal clock on which to compute statistics.
      CLOCK_SEL     : COUNTER_CFG_CLOCK_SEL_Field := 16#0#;
      --  To be used to add precision to clock_ratio and determine 'entropy
      --  refill'.
      SHIFT4X       : COUNTER_CFG_SHIFT4X_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for COUNTER_CFG_Register use record
      MODE          at 0 range 0 .. 1;
      CLOCK_SEL     at 0 range 2 .. 4;
      SHIFT4X       at 0 range 5 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype ONLINE_TEST_CFG_DATA_SEL_Field is HAL.UInt2;

   --  no description available
   type ONLINE_TEST_CFG_Register is record
      --  0: disabled 1: activated Update rythm for VAL depends on COUNTER_CFG
      --  if data_sel is set to COUNTER.
      ACTIVATE      : Boolean := False;
      --  Selects source on which to apply online test: 00: LSB of COUNTER: raw
      --  data from one or all sources of entropy 01: MSB of COUNTER: raw data
      --  from one or all sources of entropy 10: RANDOM_NUMBER 11:
      --  ENCRYPTED_NUMBER 'activate' should be set to 'disabled' before
      --  changing this field.
      DATA_SEL      : ONLINE_TEST_CFG_DATA_SEL_Field := 16#0#;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ONLINE_TEST_CFG_Register use record
      ACTIVATE      at 0 range 0 .. 0;
      DATA_SEL      at 0 range 1 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype ONLINE_TEST_VAL_LIVE_CHI_SQUARED_Field is HAL.UInt4;
   subtype ONLINE_TEST_VAL_MIN_CHI_SQUARED_Field is HAL.UInt4;
   subtype ONLINE_TEST_VAL_MAX_CHI_SQUARED_Field is HAL.UInt4;

   --  no description available
   type ONLINE_TEST_VAL_Register is record
      --  Read-only. This value is updated as described in field 'activate'.
      LIVE_CHI_SQUARED : ONLINE_TEST_VAL_LIVE_CHI_SQUARED_Field;
      --  Read-only. This field is reset when 'activate'==0.
      MIN_CHI_SQUARED  : ONLINE_TEST_VAL_MIN_CHI_SQUARED_Field;
      --  Read-only. This field is reset when 'activate'==0.
      MAX_CHI_SQUARED  : ONLINE_TEST_VAL_MAX_CHI_SQUARED_Field;
      --  unspecified
      Reserved_12_31   : HAL.UInt20;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ONLINE_TEST_VAL_Register use record
      LIVE_CHI_SQUARED at 0 range 0 .. 3;
      MIN_CHI_SQUARED  at 0 range 4 .. 7;
      MAX_CHI_SQUARED  at 0 range 8 .. 11;
      Reserved_12_31   at 0 range 12 .. 31;
   end record;

   subtype MODULEID_APERTURE_Field is HAL.UInt8;
   subtype MODULEID_MIN_REV_Field is HAL.UInt4;
   subtype MODULEID_MAJ_REV_Field is HAL.UInt4;
   subtype MODULEID_ID_Field is HAL.UInt16;

   --  IP identifier
   type MODULEID_Register is record
      --  Read-only. Aperture i.
      APERTURE : MODULEID_APERTURE_Field;
      --  Read-only. Minor revision i.
      MIN_REV  : MODULEID_MIN_REV_Field;
      --  Read-only. Major revision i.
      MAJ_REV  : MODULEID_MAJ_REV_Field;
      --  Read-only. Identifier.
      ID       : MODULEID_ID_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MODULEID_Register use record
      APERTURE at 0 range 0 .. 7;
      MIN_REV  at 0 range 8 .. 11;
      MAJ_REV  at 0 range 12 .. 15;
      ID       at 0 range 16 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  RNG
   type RNG_Peripheral is record
      --  This register contains a random 32 bit number which is computed on
      --  demand, at each time it is read
      RANDOM_NUMBER   : aliased HAL.UInt32;
      --  no description available
      COUNTER_VAL     : aliased COUNTER_VAL_Register;
      --  no description available
      COUNTER_CFG     : aliased COUNTER_CFG_Register;
      --  no description available
      ONLINE_TEST_CFG : aliased ONLINE_TEST_CFG_Register;
      --  no description available
      ONLINE_TEST_VAL : aliased ONLINE_TEST_VAL_Register;
      --  IP identifier
      MODULEID        : aliased MODULEID_Register;
   end record
     with Volatile;

   for RNG_Peripheral use record
      RANDOM_NUMBER   at 16#0# range 0 .. 31;
      COUNTER_VAL     at 16#8# range 0 .. 31;
      COUNTER_CFG     at 16#C# range 0 .. 31;
      ONLINE_TEST_CFG at 16#10# range 0 .. 31;
      ONLINE_TEST_VAL at 16#14# range 0 .. 31;
      MODULEID        at 16#FFC# range 0 .. 31;
   end record;

   --  RNG
   RNG_Periph : aliased RNG_Peripheral
     with Import, Address => System'To_Address (16#4003A000#);

end NXP_SVD.RNG;
