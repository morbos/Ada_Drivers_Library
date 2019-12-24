--  This spec has been automatically generated from nrf53.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NRF53_SVD.GPIO_P0 is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  If pin satisfied sense criteria
   type DETECTMODE_Register is record
      --  LDETECT if 1
      DETECTMODE    : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DETECTMODE_Register use record
      DETECTMODE    at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  If pin satisfied sense criteria
   type DETECTMODE_SEC_Register is record
      --  LDETECT if 1
      DETECTMODE    : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DETECTMODE_SEC_Register use record
      DETECTMODE    at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   subtype PIN_CFG_0_PULL_Field is HAL.UInt2;
   subtype PIN_CFG_0_DRIVE_Field is HAL.UInt4;
   subtype PIN_CFG_0_SENSE_Field is HAL.UInt2;
   subtype PIN_CFG_0_MCUSEL_Field is HAL.UInt3;

   --  Pin configuration
   type PIN_CFG_0_Register is record
      --  In/out configuration
      STANCE         : Boolean := False;
      --  Input configuration
      INPUT          : Boolean := False;
      --  Pull configuration
      PULL           : PIN_CFG_0_PULL_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Drive configuration
      DRIVE          : PIN_CFG_0_DRIVE_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Pin sensing mech
      SENSE          : PIN_CFG_0_SENSE_Field := 16#0#;
      --  unspecified
      Reserved_18_27 : HAL.UInt10 := 16#0#;
      --  Which MCU subsystem controls the pin
      MCUSEL         : PIN_CFG_0_MCUSEL_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIN_CFG_0_Register use record
      STANCE         at 0 range 0 .. 0;
      INPUT          at 0 range 1 .. 1;
      PULL           at 0 range 2 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      DRIVE          at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      SENSE          at 0 range 16 .. 17;
      Reserved_18_27 at 0 range 18 .. 27;
      MCUSEL         at 0 range 28 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype PIN_CFG_1_PULL_Field is HAL.UInt2;
   subtype PIN_CFG_1_DRIVE_Field is HAL.UInt4;
   subtype PIN_CFG_1_SENSE_Field is HAL.UInt2;
   subtype PIN_CFG_1_MCUSEL_Field is HAL.UInt3;

   --  Pin configuration
   type PIN_CFG_1_Register is record
      --  In/out configuration
      STANCE         : Boolean := False;
      --  Input configuration
      INPUT          : Boolean := False;
      --  Pull configuration
      PULL           : PIN_CFG_1_PULL_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Drive configuration
      DRIVE          : PIN_CFG_1_DRIVE_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Pin sensing mech
      SENSE          : PIN_CFG_1_SENSE_Field := 16#0#;
      --  unspecified
      Reserved_18_27 : HAL.UInt10 := 16#0#;
      --  Which MCU subsystem controls the pin
      MCUSEL         : PIN_CFG_1_MCUSEL_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIN_CFG_1_Register use record
      STANCE         at 0 range 0 .. 0;
      INPUT          at 0 range 1 .. 1;
      PULL           at 0 range 2 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      DRIVE          at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      SENSE          at 0 range 16 .. 17;
      Reserved_18_27 at 0 range 18 .. 27;
      MCUSEL         at 0 range 28 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype PIN_CFG_2_PULL_Field is HAL.UInt2;
   subtype PIN_CFG_2_DRIVE_Field is HAL.UInt4;
   subtype PIN_CFG_2_SENSE_Field is HAL.UInt2;
   subtype PIN_CFG_2_MCUSEL_Field is HAL.UInt3;

   --  Pin configuration
   type PIN_CFG_2_Register is record
      --  In/out configuration
      STANCE         : Boolean := False;
      --  Input configuration
      INPUT          : Boolean := False;
      --  Pull configuration
      PULL           : PIN_CFG_2_PULL_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Drive configuration
      DRIVE          : PIN_CFG_2_DRIVE_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Pin sensing mech
      SENSE          : PIN_CFG_2_SENSE_Field := 16#0#;
      --  unspecified
      Reserved_18_27 : HAL.UInt10 := 16#0#;
      --  Which MCU subsystem controls the pin
      MCUSEL         : PIN_CFG_2_MCUSEL_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIN_CFG_2_Register use record
      STANCE         at 0 range 0 .. 0;
      INPUT          at 0 range 1 .. 1;
      PULL           at 0 range 2 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      DRIVE          at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      SENSE          at 0 range 16 .. 17;
      Reserved_18_27 at 0 range 18 .. 27;
      MCUSEL         at 0 range 28 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype PIN_CFG_3_PULL_Field is HAL.UInt2;
   subtype PIN_CFG_3_DRIVE_Field is HAL.UInt4;
   subtype PIN_CFG_3_SENSE_Field is HAL.UInt2;
   subtype PIN_CFG_3_MCUSEL_Field is HAL.UInt3;

   --  Pin configuration
   type PIN_CFG_3_Register is record
      --  In/out configuration
      STANCE         : Boolean := False;
      --  Input configuration
      INPUT          : Boolean := False;
      --  Pull configuration
      PULL           : PIN_CFG_3_PULL_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Drive configuration
      DRIVE          : PIN_CFG_3_DRIVE_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Pin sensing mech
      SENSE          : PIN_CFG_3_SENSE_Field := 16#0#;
      --  unspecified
      Reserved_18_27 : HAL.UInt10 := 16#0#;
      --  Which MCU subsystem controls the pin
      MCUSEL         : PIN_CFG_3_MCUSEL_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIN_CFG_3_Register use record
      STANCE         at 0 range 0 .. 0;
      INPUT          at 0 range 1 .. 1;
      PULL           at 0 range 2 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      DRIVE          at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      SENSE          at 0 range 16 .. 17;
      Reserved_18_27 at 0 range 18 .. 27;
      MCUSEL         at 0 range 28 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype PIN_CFG_4_PULL_Field is HAL.UInt2;
   subtype PIN_CFG_4_DRIVE_Field is HAL.UInt4;
   subtype PIN_CFG_4_SENSE_Field is HAL.UInt2;
   subtype PIN_CFG_4_MCUSEL_Field is HAL.UInt3;

   --  Pin configuration
   type PIN_CFG_4_Register is record
      --  In/out configuration
      STANCE         : Boolean := False;
      --  Input configuration
      INPUT          : Boolean := False;
      --  Pull configuration
      PULL           : PIN_CFG_4_PULL_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Drive configuration
      DRIVE          : PIN_CFG_4_DRIVE_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Pin sensing mech
      SENSE          : PIN_CFG_4_SENSE_Field := 16#0#;
      --  unspecified
      Reserved_18_27 : HAL.UInt10 := 16#0#;
      --  Which MCU subsystem controls the pin
      MCUSEL         : PIN_CFG_4_MCUSEL_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIN_CFG_4_Register use record
      STANCE         at 0 range 0 .. 0;
      INPUT          at 0 range 1 .. 1;
      PULL           at 0 range 2 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      DRIVE          at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      SENSE          at 0 range 16 .. 17;
      Reserved_18_27 at 0 range 18 .. 27;
      MCUSEL         at 0 range 28 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype PIN_CFG_5_PULL_Field is HAL.UInt2;
   subtype PIN_CFG_5_DRIVE_Field is HAL.UInt4;
   subtype PIN_CFG_5_SENSE_Field is HAL.UInt2;
   subtype PIN_CFG_5_MCUSEL_Field is HAL.UInt3;

   --  Pin configuration
   type PIN_CFG_5_Register is record
      --  In/out configuration
      STANCE         : Boolean := False;
      --  Input configuration
      INPUT          : Boolean := False;
      --  Pull configuration
      PULL           : PIN_CFG_5_PULL_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Drive configuration
      DRIVE          : PIN_CFG_5_DRIVE_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Pin sensing mech
      SENSE          : PIN_CFG_5_SENSE_Field := 16#0#;
      --  unspecified
      Reserved_18_27 : HAL.UInt10 := 16#0#;
      --  Which MCU subsystem controls the pin
      MCUSEL         : PIN_CFG_5_MCUSEL_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIN_CFG_5_Register use record
      STANCE         at 0 range 0 .. 0;
      INPUT          at 0 range 1 .. 1;
      PULL           at 0 range 2 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      DRIVE          at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      SENSE          at 0 range 16 .. 17;
      Reserved_18_27 at 0 range 18 .. 27;
      MCUSEL         at 0 range 28 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype PIN_CFG_6_PULL_Field is HAL.UInt2;
   subtype PIN_CFG_6_DRIVE_Field is HAL.UInt4;
   subtype PIN_CFG_6_SENSE_Field is HAL.UInt2;
   subtype PIN_CFG_6_MCUSEL_Field is HAL.UInt3;

   --  Pin configuration
   type PIN_CFG_6_Register is record
      --  In/out configuration
      STANCE         : Boolean := False;
      --  Input configuration
      INPUT          : Boolean := False;
      --  Pull configuration
      PULL           : PIN_CFG_6_PULL_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Drive configuration
      DRIVE          : PIN_CFG_6_DRIVE_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Pin sensing mech
      SENSE          : PIN_CFG_6_SENSE_Field := 16#0#;
      --  unspecified
      Reserved_18_27 : HAL.UInt10 := 16#0#;
      --  Which MCU subsystem controls the pin
      MCUSEL         : PIN_CFG_6_MCUSEL_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIN_CFG_6_Register use record
      STANCE         at 0 range 0 .. 0;
      INPUT          at 0 range 1 .. 1;
      PULL           at 0 range 2 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      DRIVE          at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      SENSE          at 0 range 16 .. 17;
      Reserved_18_27 at 0 range 18 .. 27;
      MCUSEL         at 0 range 28 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype PIN_CFG_7_PULL_Field is HAL.UInt2;
   subtype PIN_CFG_7_DRIVE_Field is HAL.UInt4;
   subtype PIN_CFG_7_SENSE_Field is HAL.UInt2;
   subtype PIN_CFG_7_MCUSEL_Field is HAL.UInt3;

   --  Pin configuration
   type PIN_CFG_7_Register is record
      --  In/out configuration
      STANCE         : Boolean := False;
      --  Input configuration
      INPUT          : Boolean := False;
      --  Pull configuration
      PULL           : PIN_CFG_7_PULL_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Drive configuration
      DRIVE          : PIN_CFG_7_DRIVE_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Pin sensing mech
      SENSE          : PIN_CFG_7_SENSE_Field := 16#0#;
      --  unspecified
      Reserved_18_27 : HAL.UInt10 := 16#0#;
      --  Which MCU subsystem controls the pin
      MCUSEL         : PIN_CFG_7_MCUSEL_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIN_CFG_7_Register use record
      STANCE         at 0 range 0 .. 0;
      INPUT          at 0 range 1 .. 1;
      PULL           at 0 range 2 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      DRIVE          at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      SENSE          at 0 range 16 .. 17;
      Reserved_18_27 at 0 range 18 .. 27;
      MCUSEL         at 0 range 28 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype PIN_CFG_8_PULL_Field is HAL.UInt2;
   subtype PIN_CFG_8_DRIVE_Field is HAL.UInt4;
   subtype PIN_CFG_8_SENSE_Field is HAL.UInt2;
   subtype PIN_CFG_8_MCUSEL_Field is HAL.UInt3;

   --  Pin configuration
   type PIN_CFG_8_Register is record
      --  In/out configuration
      STANCE         : Boolean := False;
      --  Input configuration
      INPUT          : Boolean := False;
      --  Pull configuration
      PULL           : PIN_CFG_8_PULL_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Drive configuration
      DRIVE          : PIN_CFG_8_DRIVE_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Pin sensing mech
      SENSE          : PIN_CFG_8_SENSE_Field := 16#0#;
      --  unspecified
      Reserved_18_27 : HAL.UInt10 := 16#0#;
      --  Which MCU subsystem controls the pin
      MCUSEL         : PIN_CFG_8_MCUSEL_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIN_CFG_8_Register use record
      STANCE         at 0 range 0 .. 0;
      INPUT          at 0 range 1 .. 1;
      PULL           at 0 range 2 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      DRIVE          at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      SENSE          at 0 range 16 .. 17;
      Reserved_18_27 at 0 range 18 .. 27;
      MCUSEL         at 0 range 28 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype PIN_CFG_9_PULL_Field is HAL.UInt2;
   subtype PIN_CFG_9_DRIVE_Field is HAL.UInt4;
   subtype PIN_CFG_9_SENSE_Field is HAL.UInt2;
   subtype PIN_CFG_9_MCUSEL_Field is HAL.UInt3;

   --  Pin configuration
   type PIN_CFG_9_Register is record
      --  In/out configuration
      STANCE         : Boolean := False;
      --  Input configuration
      INPUT          : Boolean := False;
      --  Pull configuration
      PULL           : PIN_CFG_9_PULL_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Drive configuration
      DRIVE          : PIN_CFG_9_DRIVE_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Pin sensing mech
      SENSE          : PIN_CFG_9_SENSE_Field := 16#0#;
      --  unspecified
      Reserved_18_27 : HAL.UInt10 := 16#0#;
      --  Which MCU subsystem controls the pin
      MCUSEL         : PIN_CFG_9_MCUSEL_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIN_CFG_9_Register use record
      STANCE         at 0 range 0 .. 0;
      INPUT          at 0 range 1 .. 1;
      PULL           at 0 range 2 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      DRIVE          at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      SENSE          at 0 range 16 .. 17;
      Reserved_18_27 at 0 range 18 .. 27;
      MCUSEL         at 0 range 28 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype PIN_CFG_10_PULL_Field is HAL.UInt2;
   subtype PIN_CFG_10_DRIVE_Field is HAL.UInt4;
   subtype PIN_CFG_10_SENSE_Field is HAL.UInt2;
   subtype PIN_CFG_10_MCUSEL_Field is HAL.UInt3;

   --  Pin configuration
   type PIN_CFG_10_Register is record
      --  In/out configuration
      STANCE         : Boolean := False;
      --  Input configuration
      INPUT          : Boolean := False;
      --  Pull configuration
      PULL           : PIN_CFG_10_PULL_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Drive configuration
      DRIVE          : PIN_CFG_10_DRIVE_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Pin sensing mech
      SENSE          : PIN_CFG_10_SENSE_Field := 16#0#;
      --  unspecified
      Reserved_18_27 : HAL.UInt10 := 16#0#;
      --  Which MCU subsystem controls the pin
      MCUSEL         : PIN_CFG_10_MCUSEL_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIN_CFG_10_Register use record
      STANCE         at 0 range 0 .. 0;
      INPUT          at 0 range 1 .. 1;
      PULL           at 0 range 2 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      DRIVE          at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      SENSE          at 0 range 16 .. 17;
      Reserved_18_27 at 0 range 18 .. 27;
      MCUSEL         at 0 range 28 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype PIN_CFG_11_PULL_Field is HAL.UInt2;
   subtype PIN_CFG_11_DRIVE_Field is HAL.UInt4;
   subtype PIN_CFG_11_SENSE_Field is HAL.UInt2;
   subtype PIN_CFG_11_MCUSEL_Field is HAL.UInt3;

   --  Pin configuration
   type PIN_CFG_11_Register is record
      --  In/out configuration
      STANCE         : Boolean := False;
      --  Input configuration
      INPUT          : Boolean := False;
      --  Pull configuration
      PULL           : PIN_CFG_11_PULL_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Drive configuration
      DRIVE          : PIN_CFG_11_DRIVE_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Pin sensing mech
      SENSE          : PIN_CFG_11_SENSE_Field := 16#0#;
      --  unspecified
      Reserved_18_27 : HAL.UInt10 := 16#0#;
      --  Which MCU subsystem controls the pin
      MCUSEL         : PIN_CFG_11_MCUSEL_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIN_CFG_11_Register use record
      STANCE         at 0 range 0 .. 0;
      INPUT          at 0 range 1 .. 1;
      PULL           at 0 range 2 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      DRIVE          at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      SENSE          at 0 range 16 .. 17;
      Reserved_18_27 at 0 range 18 .. 27;
      MCUSEL         at 0 range 28 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype PIN_CFG_12_PULL_Field is HAL.UInt2;
   subtype PIN_CFG_12_DRIVE_Field is HAL.UInt4;
   subtype PIN_CFG_12_SENSE_Field is HAL.UInt2;
   subtype PIN_CFG_12_MCUSEL_Field is HAL.UInt3;

   --  Pin configuration
   type PIN_CFG_12_Register is record
      --  In/out configuration
      STANCE         : Boolean := False;
      --  Input configuration
      INPUT          : Boolean := False;
      --  Pull configuration
      PULL           : PIN_CFG_12_PULL_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Drive configuration
      DRIVE          : PIN_CFG_12_DRIVE_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Pin sensing mech
      SENSE          : PIN_CFG_12_SENSE_Field := 16#0#;
      --  unspecified
      Reserved_18_27 : HAL.UInt10 := 16#0#;
      --  Which MCU subsystem controls the pin
      MCUSEL         : PIN_CFG_12_MCUSEL_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIN_CFG_12_Register use record
      STANCE         at 0 range 0 .. 0;
      INPUT          at 0 range 1 .. 1;
      PULL           at 0 range 2 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      DRIVE          at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      SENSE          at 0 range 16 .. 17;
      Reserved_18_27 at 0 range 18 .. 27;
      MCUSEL         at 0 range 28 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype PIN_CFG_13_PULL_Field is HAL.UInt2;
   subtype PIN_CFG_13_DRIVE_Field is HAL.UInt4;
   subtype PIN_CFG_13_SENSE_Field is HAL.UInt2;
   subtype PIN_CFG_13_MCUSEL_Field is HAL.UInt3;

   --  Pin configuration
   type PIN_CFG_13_Register is record
      --  In/out configuration
      STANCE         : Boolean := False;
      --  Input configuration
      INPUT          : Boolean := False;
      --  Pull configuration
      PULL           : PIN_CFG_13_PULL_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Drive configuration
      DRIVE          : PIN_CFG_13_DRIVE_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Pin sensing mech
      SENSE          : PIN_CFG_13_SENSE_Field := 16#0#;
      --  unspecified
      Reserved_18_27 : HAL.UInt10 := 16#0#;
      --  Which MCU subsystem controls the pin
      MCUSEL         : PIN_CFG_13_MCUSEL_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIN_CFG_13_Register use record
      STANCE         at 0 range 0 .. 0;
      INPUT          at 0 range 1 .. 1;
      PULL           at 0 range 2 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      DRIVE          at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      SENSE          at 0 range 16 .. 17;
      Reserved_18_27 at 0 range 18 .. 27;
      MCUSEL         at 0 range 28 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype PIN_CFG_14_PULL_Field is HAL.UInt2;
   subtype PIN_CFG_14_DRIVE_Field is HAL.UInt4;
   subtype PIN_CFG_14_SENSE_Field is HAL.UInt2;
   subtype PIN_CFG_14_MCUSEL_Field is HAL.UInt3;

   --  Pin configuration
   type PIN_CFG_14_Register is record
      --  In/out configuration
      STANCE         : Boolean := False;
      --  Input configuration
      INPUT          : Boolean := False;
      --  Pull configuration
      PULL           : PIN_CFG_14_PULL_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Drive configuration
      DRIVE          : PIN_CFG_14_DRIVE_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Pin sensing mech
      SENSE          : PIN_CFG_14_SENSE_Field := 16#0#;
      --  unspecified
      Reserved_18_27 : HAL.UInt10 := 16#0#;
      --  Which MCU subsystem controls the pin
      MCUSEL         : PIN_CFG_14_MCUSEL_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIN_CFG_14_Register use record
      STANCE         at 0 range 0 .. 0;
      INPUT          at 0 range 1 .. 1;
      PULL           at 0 range 2 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      DRIVE          at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      SENSE          at 0 range 16 .. 17;
      Reserved_18_27 at 0 range 18 .. 27;
      MCUSEL         at 0 range 28 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype PIN_CFG_15_PULL_Field is HAL.UInt2;
   subtype PIN_CFG_15_DRIVE_Field is HAL.UInt4;
   subtype PIN_CFG_15_SENSE_Field is HAL.UInt2;
   subtype PIN_CFG_15_MCUSEL_Field is HAL.UInt3;

   --  Pin configuration
   type PIN_CFG_15_Register is record
      --  In/out configuration
      STANCE         : Boolean := False;
      --  Input configuration
      INPUT          : Boolean := False;
      --  Pull configuration
      PULL           : PIN_CFG_15_PULL_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Drive configuration
      DRIVE          : PIN_CFG_15_DRIVE_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Pin sensing mech
      SENSE          : PIN_CFG_15_SENSE_Field := 16#0#;
      --  unspecified
      Reserved_18_27 : HAL.UInt10 := 16#0#;
      --  Which MCU subsystem controls the pin
      MCUSEL         : PIN_CFG_15_MCUSEL_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIN_CFG_15_Register use record
      STANCE         at 0 range 0 .. 0;
      INPUT          at 0 range 1 .. 1;
      PULL           at 0 range 2 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      DRIVE          at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      SENSE          at 0 range 16 .. 17;
      Reserved_18_27 at 0 range 18 .. 27;
      MCUSEL         at 0 range 28 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype PIN_CFG_16_PULL_Field is HAL.UInt2;
   subtype PIN_CFG_16_DRIVE_Field is HAL.UInt4;
   subtype PIN_CFG_16_SENSE_Field is HAL.UInt2;
   subtype PIN_CFG_16_MCUSEL_Field is HAL.UInt3;

   --  Pin configuration
   type PIN_CFG_16_Register is record
      --  In/out configuration
      STANCE         : Boolean := False;
      --  Input configuration
      INPUT          : Boolean := False;
      --  Pull configuration
      PULL           : PIN_CFG_16_PULL_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Drive configuration
      DRIVE          : PIN_CFG_16_DRIVE_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Pin sensing mech
      SENSE          : PIN_CFG_16_SENSE_Field := 16#0#;
      --  unspecified
      Reserved_18_27 : HAL.UInt10 := 16#0#;
      --  Which MCU subsystem controls the pin
      MCUSEL         : PIN_CFG_16_MCUSEL_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIN_CFG_16_Register use record
      STANCE         at 0 range 0 .. 0;
      INPUT          at 0 range 1 .. 1;
      PULL           at 0 range 2 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      DRIVE          at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      SENSE          at 0 range 16 .. 17;
      Reserved_18_27 at 0 range 18 .. 27;
      MCUSEL         at 0 range 28 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype PIN_CFG_17_PULL_Field is HAL.UInt2;
   subtype PIN_CFG_17_DRIVE_Field is HAL.UInt4;
   subtype PIN_CFG_17_SENSE_Field is HAL.UInt2;
   subtype PIN_CFG_17_MCUSEL_Field is HAL.UInt3;

   --  Pin configuration
   type PIN_CFG_17_Register is record
      --  In/out configuration
      STANCE         : Boolean := False;
      --  Input configuration
      INPUT          : Boolean := False;
      --  Pull configuration
      PULL           : PIN_CFG_17_PULL_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Drive configuration
      DRIVE          : PIN_CFG_17_DRIVE_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Pin sensing mech
      SENSE          : PIN_CFG_17_SENSE_Field := 16#0#;
      --  unspecified
      Reserved_18_27 : HAL.UInt10 := 16#0#;
      --  Which MCU subsystem controls the pin
      MCUSEL         : PIN_CFG_17_MCUSEL_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIN_CFG_17_Register use record
      STANCE         at 0 range 0 .. 0;
      INPUT          at 0 range 1 .. 1;
      PULL           at 0 range 2 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      DRIVE          at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      SENSE          at 0 range 16 .. 17;
      Reserved_18_27 at 0 range 18 .. 27;
      MCUSEL         at 0 range 28 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype PIN_CFG_18_PULL_Field is HAL.UInt2;
   subtype PIN_CFG_18_DRIVE_Field is HAL.UInt4;
   subtype PIN_CFG_18_SENSE_Field is HAL.UInt2;
   subtype PIN_CFG_18_MCUSEL_Field is HAL.UInt3;

   --  Pin configuration
   type PIN_CFG_18_Register is record
      --  In/out configuration
      STANCE         : Boolean := False;
      --  Input configuration
      INPUT          : Boolean := False;
      --  Pull configuration
      PULL           : PIN_CFG_18_PULL_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Drive configuration
      DRIVE          : PIN_CFG_18_DRIVE_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Pin sensing mech
      SENSE          : PIN_CFG_18_SENSE_Field := 16#0#;
      --  unspecified
      Reserved_18_27 : HAL.UInt10 := 16#0#;
      --  Which MCU subsystem controls the pin
      MCUSEL         : PIN_CFG_18_MCUSEL_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIN_CFG_18_Register use record
      STANCE         at 0 range 0 .. 0;
      INPUT          at 0 range 1 .. 1;
      PULL           at 0 range 2 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      DRIVE          at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      SENSE          at 0 range 16 .. 17;
      Reserved_18_27 at 0 range 18 .. 27;
      MCUSEL         at 0 range 28 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype PIN_CFG_19_PULL_Field is HAL.UInt2;
   subtype PIN_CFG_19_DRIVE_Field is HAL.UInt4;
   subtype PIN_CFG_19_SENSE_Field is HAL.UInt2;
   subtype PIN_CFG_19_MCUSEL_Field is HAL.UInt3;

   --  Pin configuration
   type PIN_CFG_19_Register is record
      --  In/out configuration
      STANCE         : Boolean := False;
      --  Input configuration
      INPUT          : Boolean := False;
      --  Pull configuration
      PULL           : PIN_CFG_19_PULL_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Drive configuration
      DRIVE          : PIN_CFG_19_DRIVE_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Pin sensing mech
      SENSE          : PIN_CFG_19_SENSE_Field := 16#0#;
      --  unspecified
      Reserved_18_27 : HAL.UInt10 := 16#0#;
      --  Which MCU subsystem controls the pin
      MCUSEL         : PIN_CFG_19_MCUSEL_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIN_CFG_19_Register use record
      STANCE         at 0 range 0 .. 0;
      INPUT          at 0 range 1 .. 1;
      PULL           at 0 range 2 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      DRIVE          at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      SENSE          at 0 range 16 .. 17;
      Reserved_18_27 at 0 range 18 .. 27;
      MCUSEL         at 0 range 28 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype PIN_CFG_20_PULL_Field is HAL.UInt2;
   subtype PIN_CFG_20_DRIVE_Field is HAL.UInt4;
   subtype PIN_CFG_20_SENSE_Field is HAL.UInt2;
   subtype PIN_CFG_20_MCUSEL_Field is HAL.UInt3;

   --  Pin configuration
   type PIN_CFG_20_Register is record
      --  In/out configuration
      STANCE         : Boolean := False;
      --  Input configuration
      INPUT          : Boolean := False;
      --  Pull configuration
      PULL           : PIN_CFG_20_PULL_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Drive configuration
      DRIVE          : PIN_CFG_20_DRIVE_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Pin sensing mech
      SENSE          : PIN_CFG_20_SENSE_Field := 16#0#;
      --  unspecified
      Reserved_18_27 : HAL.UInt10 := 16#0#;
      --  Which MCU subsystem controls the pin
      MCUSEL         : PIN_CFG_20_MCUSEL_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIN_CFG_20_Register use record
      STANCE         at 0 range 0 .. 0;
      INPUT          at 0 range 1 .. 1;
      PULL           at 0 range 2 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      DRIVE          at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      SENSE          at 0 range 16 .. 17;
      Reserved_18_27 at 0 range 18 .. 27;
      MCUSEL         at 0 range 28 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype PIN_CFG_21_PULL_Field is HAL.UInt2;
   subtype PIN_CFG_21_DRIVE_Field is HAL.UInt4;
   subtype PIN_CFG_21_SENSE_Field is HAL.UInt2;
   subtype PIN_CFG_21_MCUSEL_Field is HAL.UInt3;

   --  Pin configuration
   type PIN_CFG_21_Register is record
      --  In/out configuration
      STANCE         : Boolean := False;
      --  Input configuration
      INPUT          : Boolean := False;
      --  Pull configuration
      PULL           : PIN_CFG_21_PULL_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Drive configuration
      DRIVE          : PIN_CFG_21_DRIVE_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Pin sensing mech
      SENSE          : PIN_CFG_21_SENSE_Field := 16#0#;
      --  unspecified
      Reserved_18_27 : HAL.UInt10 := 16#0#;
      --  Which MCU subsystem controls the pin
      MCUSEL         : PIN_CFG_21_MCUSEL_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIN_CFG_21_Register use record
      STANCE         at 0 range 0 .. 0;
      INPUT          at 0 range 1 .. 1;
      PULL           at 0 range 2 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      DRIVE          at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      SENSE          at 0 range 16 .. 17;
      Reserved_18_27 at 0 range 18 .. 27;
      MCUSEL         at 0 range 28 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype PIN_CFG_22_PULL_Field is HAL.UInt2;
   subtype PIN_CFG_22_DRIVE_Field is HAL.UInt4;
   subtype PIN_CFG_22_SENSE_Field is HAL.UInt2;
   subtype PIN_CFG_22_MCUSEL_Field is HAL.UInt3;

   --  Pin configuration
   type PIN_CFG_22_Register is record
      --  In/out configuration
      STANCE         : Boolean := False;
      --  Input configuration
      INPUT          : Boolean := False;
      --  Pull configuration
      PULL           : PIN_CFG_22_PULL_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Drive configuration
      DRIVE          : PIN_CFG_22_DRIVE_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Pin sensing mech
      SENSE          : PIN_CFG_22_SENSE_Field := 16#0#;
      --  unspecified
      Reserved_18_27 : HAL.UInt10 := 16#0#;
      --  Which MCU subsystem controls the pin
      MCUSEL         : PIN_CFG_22_MCUSEL_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIN_CFG_22_Register use record
      STANCE         at 0 range 0 .. 0;
      INPUT          at 0 range 1 .. 1;
      PULL           at 0 range 2 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      DRIVE          at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      SENSE          at 0 range 16 .. 17;
      Reserved_18_27 at 0 range 18 .. 27;
      MCUSEL         at 0 range 28 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype PIN_CFG_23_PULL_Field is HAL.UInt2;
   subtype PIN_CFG_23_DRIVE_Field is HAL.UInt4;
   subtype PIN_CFG_23_SENSE_Field is HAL.UInt2;
   subtype PIN_CFG_23_MCUSEL_Field is HAL.UInt3;

   --  Pin configuration
   type PIN_CFG_23_Register is record
      --  In/out configuration
      STANCE         : Boolean := False;
      --  Input configuration
      INPUT          : Boolean := False;
      --  Pull configuration
      PULL           : PIN_CFG_23_PULL_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Drive configuration
      DRIVE          : PIN_CFG_23_DRIVE_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Pin sensing mech
      SENSE          : PIN_CFG_23_SENSE_Field := 16#0#;
      --  unspecified
      Reserved_18_27 : HAL.UInt10 := 16#0#;
      --  Which MCU subsystem controls the pin
      MCUSEL         : PIN_CFG_23_MCUSEL_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIN_CFG_23_Register use record
      STANCE         at 0 range 0 .. 0;
      INPUT          at 0 range 1 .. 1;
      PULL           at 0 range 2 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      DRIVE          at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      SENSE          at 0 range 16 .. 17;
      Reserved_18_27 at 0 range 18 .. 27;
      MCUSEL         at 0 range 28 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype PIN_CFG_24_PULL_Field is HAL.UInt2;
   subtype PIN_CFG_24_DRIVE_Field is HAL.UInt4;
   subtype PIN_CFG_24_SENSE_Field is HAL.UInt2;
   subtype PIN_CFG_24_MCUSEL_Field is HAL.UInt3;

   --  Pin configuration
   type PIN_CFG_24_Register is record
      --  In/out configuration
      STANCE         : Boolean := False;
      --  Input configuration
      INPUT          : Boolean := False;
      --  Pull configuration
      PULL           : PIN_CFG_24_PULL_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Drive configuration
      DRIVE          : PIN_CFG_24_DRIVE_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Pin sensing mech
      SENSE          : PIN_CFG_24_SENSE_Field := 16#0#;
      --  unspecified
      Reserved_18_27 : HAL.UInt10 := 16#0#;
      --  Which MCU subsystem controls the pin
      MCUSEL         : PIN_CFG_24_MCUSEL_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIN_CFG_24_Register use record
      STANCE         at 0 range 0 .. 0;
      INPUT          at 0 range 1 .. 1;
      PULL           at 0 range 2 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      DRIVE          at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      SENSE          at 0 range 16 .. 17;
      Reserved_18_27 at 0 range 18 .. 27;
      MCUSEL         at 0 range 28 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype PIN_CFG_25_PULL_Field is HAL.UInt2;
   subtype PIN_CFG_25_DRIVE_Field is HAL.UInt4;
   subtype PIN_CFG_25_SENSE_Field is HAL.UInt2;
   subtype PIN_CFG_25_MCUSEL_Field is HAL.UInt3;

   --  Pin configuration
   type PIN_CFG_25_Register is record
      --  In/out configuration
      STANCE         : Boolean := False;
      --  Input configuration
      INPUT          : Boolean := False;
      --  Pull configuration
      PULL           : PIN_CFG_25_PULL_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Drive configuration
      DRIVE          : PIN_CFG_25_DRIVE_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Pin sensing mech
      SENSE          : PIN_CFG_25_SENSE_Field := 16#0#;
      --  unspecified
      Reserved_18_27 : HAL.UInt10 := 16#0#;
      --  Which MCU subsystem controls the pin
      MCUSEL         : PIN_CFG_25_MCUSEL_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIN_CFG_25_Register use record
      STANCE         at 0 range 0 .. 0;
      INPUT          at 0 range 1 .. 1;
      PULL           at 0 range 2 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      DRIVE          at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      SENSE          at 0 range 16 .. 17;
      Reserved_18_27 at 0 range 18 .. 27;
      MCUSEL         at 0 range 28 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype PIN_CFG_26_PULL_Field is HAL.UInt2;
   subtype PIN_CFG_26_DRIVE_Field is HAL.UInt4;
   subtype PIN_CFG_26_SENSE_Field is HAL.UInt2;
   subtype PIN_CFG_26_MCUSEL_Field is HAL.UInt3;

   --  Pin configuration
   type PIN_CFG_26_Register is record
      --  In/out configuration
      STANCE         : Boolean := False;
      --  Input configuration
      INPUT          : Boolean := False;
      --  Pull configuration
      PULL           : PIN_CFG_26_PULL_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Drive configuration
      DRIVE          : PIN_CFG_26_DRIVE_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Pin sensing mech
      SENSE          : PIN_CFG_26_SENSE_Field := 16#0#;
      --  unspecified
      Reserved_18_27 : HAL.UInt10 := 16#0#;
      --  Which MCU subsystem controls the pin
      MCUSEL         : PIN_CFG_26_MCUSEL_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIN_CFG_26_Register use record
      STANCE         at 0 range 0 .. 0;
      INPUT          at 0 range 1 .. 1;
      PULL           at 0 range 2 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      DRIVE          at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      SENSE          at 0 range 16 .. 17;
      Reserved_18_27 at 0 range 18 .. 27;
      MCUSEL         at 0 range 28 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype PIN_CFG_27_PULL_Field is HAL.UInt2;
   subtype PIN_CFG_27_DRIVE_Field is HAL.UInt4;
   subtype PIN_CFG_27_SENSE_Field is HAL.UInt2;
   subtype PIN_CFG_27_MCUSEL_Field is HAL.UInt3;

   --  Pin configuration
   type PIN_CFG_27_Register is record
      --  In/out configuration
      STANCE         : Boolean := False;
      --  Input configuration
      INPUT          : Boolean := False;
      --  Pull configuration
      PULL           : PIN_CFG_27_PULL_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Drive configuration
      DRIVE          : PIN_CFG_27_DRIVE_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Pin sensing mech
      SENSE          : PIN_CFG_27_SENSE_Field := 16#0#;
      --  unspecified
      Reserved_18_27 : HAL.UInt10 := 16#0#;
      --  Which MCU subsystem controls the pin
      MCUSEL         : PIN_CFG_27_MCUSEL_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIN_CFG_27_Register use record
      STANCE         at 0 range 0 .. 0;
      INPUT          at 0 range 1 .. 1;
      PULL           at 0 range 2 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      DRIVE          at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      SENSE          at 0 range 16 .. 17;
      Reserved_18_27 at 0 range 18 .. 27;
      MCUSEL         at 0 range 28 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype PIN_CFG_28_PULL_Field is HAL.UInt2;
   subtype PIN_CFG_28_DRIVE_Field is HAL.UInt4;
   subtype PIN_CFG_28_SENSE_Field is HAL.UInt2;
   subtype PIN_CFG_28_MCUSEL_Field is HAL.UInt3;

   --  Pin configuration
   type PIN_CFG_28_Register is record
      --  In/out configuration
      STANCE         : Boolean := False;
      --  Input configuration
      INPUT          : Boolean := False;
      --  Pull configuration
      PULL           : PIN_CFG_28_PULL_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Drive configuration
      DRIVE          : PIN_CFG_28_DRIVE_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Pin sensing mech
      SENSE          : PIN_CFG_28_SENSE_Field := 16#0#;
      --  unspecified
      Reserved_18_27 : HAL.UInt10 := 16#0#;
      --  Which MCU subsystem controls the pin
      MCUSEL         : PIN_CFG_28_MCUSEL_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIN_CFG_28_Register use record
      STANCE         at 0 range 0 .. 0;
      INPUT          at 0 range 1 .. 1;
      PULL           at 0 range 2 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      DRIVE          at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      SENSE          at 0 range 16 .. 17;
      Reserved_18_27 at 0 range 18 .. 27;
      MCUSEL         at 0 range 28 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype PIN_CFG_29_PULL_Field is HAL.UInt2;
   subtype PIN_CFG_29_DRIVE_Field is HAL.UInt4;
   subtype PIN_CFG_29_SENSE_Field is HAL.UInt2;
   subtype PIN_CFG_29_MCUSEL_Field is HAL.UInt3;

   --  Pin configuration
   type PIN_CFG_29_Register is record
      --  In/out configuration
      STANCE         : Boolean := False;
      --  Input configuration
      INPUT          : Boolean := False;
      --  Pull configuration
      PULL           : PIN_CFG_29_PULL_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Drive configuration
      DRIVE          : PIN_CFG_29_DRIVE_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Pin sensing mech
      SENSE          : PIN_CFG_29_SENSE_Field := 16#0#;
      --  unspecified
      Reserved_18_27 : HAL.UInt10 := 16#0#;
      --  Which MCU subsystem controls the pin
      MCUSEL         : PIN_CFG_29_MCUSEL_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIN_CFG_29_Register use record
      STANCE         at 0 range 0 .. 0;
      INPUT          at 0 range 1 .. 1;
      PULL           at 0 range 2 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      DRIVE          at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      SENSE          at 0 range 16 .. 17;
      Reserved_18_27 at 0 range 18 .. 27;
      MCUSEL         at 0 range 28 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype PIN_CFG_30_PULL_Field is HAL.UInt2;
   subtype PIN_CFG_30_DRIVE_Field is HAL.UInt4;
   subtype PIN_CFG_30_SENSE_Field is HAL.UInt2;
   subtype PIN_CFG_30_MCUSEL_Field is HAL.UInt3;

   --  Pin configuration
   type PIN_CFG_30_Register is record
      --  In/out configuration
      STANCE         : Boolean := False;
      --  Input configuration
      INPUT          : Boolean := False;
      --  Pull configuration
      PULL           : PIN_CFG_30_PULL_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Drive configuration
      DRIVE          : PIN_CFG_30_DRIVE_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Pin sensing mech
      SENSE          : PIN_CFG_30_SENSE_Field := 16#0#;
      --  unspecified
      Reserved_18_27 : HAL.UInt10 := 16#0#;
      --  Which MCU subsystem controls the pin
      MCUSEL         : PIN_CFG_30_MCUSEL_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIN_CFG_30_Register use record
      STANCE         at 0 range 0 .. 0;
      INPUT          at 0 range 1 .. 1;
      PULL           at 0 range 2 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      DRIVE          at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      SENSE          at 0 range 16 .. 17;
      Reserved_18_27 at 0 range 18 .. 27;
      MCUSEL         at 0 range 28 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype PIN_CFG_31_PULL_Field is HAL.UInt2;
   subtype PIN_CFG_31_DRIVE_Field is HAL.UInt4;
   subtype PIN_CFG_31_SENSE_Field is HAL.UInt2;
   subtype PIN_CFG_31_MCUSEL_Field is HAL.UInt3;

   --  Pin configuration
   type PIN_CFG_31_Register is record
      --  In/out configuration
      STANCE         : Boolean := False;
      --  Input configuration
      INPUT          : Boolean := False;
      --  Pull configuration
      PULL           : PIN_CFG_31_PULL_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Drive configuration
      DRIVE          : PIN_CFG_31_DRIVE_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Pin sensing mech
      SENSE          : PIN_CFG_31_SENSE_Field := 16#0#;
      --  unspecified
      Reserved_18_27 : HAL.UInt10 := 16#0#;
      --  Which MCU subsystem controls the pin
      MCUSEL         : PIN_CFG_31_MCUSEL_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIN_CFG_31_Register use record
      STANCE         at 0 range 0 .. 0;
      INPUT          at 0 range 1 .. 1;
      PULL           at 0 range 2 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      DRIVE          at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      SENSE          at 0 range 16 .. 17;
      Reserved_18_27 at 0 range 18 .. 27;
      MCUSEL         at 0 range 28 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  General purpose input and output
   type GPIO_P0_Peripheral is record
      --  Write GPIO port
      OUT_k          : aliased HAL.UInt32;
      --  Set individual bits in GPIO port
      OUTSET         : aliased HAL.UInt32;
      --  Clear individual bits in GPIO port
      OUTCLR         : aliased HAL.UInt32;
      --  Read GPIO port
      IN_k           : aliased HAL.UInt32;
      --  Direction of GPIO port
      DIR            : aliased HAL.UInt32;
      --  direction set
      DIRSET         : aliased HAL.UInt32;
      --  direction clr
      DIRCLR         : aliased HAL.UInt32;
      --  If pin satisfied sense criteria
      LATCH          : aliased HAL.UInt32;
      --  If pin satisfied sense criteria
      DETECTMODE     : aliased DETECTMODE_Register;
      --  If pin satisfied sense criteria
      DETECTMODE_SEC : aliased DETECTMODE_SEC_Register;
      --  Pin configuration
      PIN_CFG_0      : aliased PIN_CFG_0_Register;
      --  Pin configuration
      PIN_CFG_1      : aliased PIN_CFG_1_Register;
      --  Pin configuration
      PIN_CFG_2      : aliased PIN_CFG_2_Register;
      --  Pin configuration
      PIN_CFG_3      : aliased PIN_CFG_3_Register;
      --  Pin configuration
      PIN_CFG_4      : aliased PIN_CFG_4_Register;
      --  Pin configuration
      PIN_CFG_5      : aliased PIN_CFG_5_Register;
      --  Pin configuration
      PIN_CFG_6      : aliased PIN_CFG_6_Register;
      --  Pin configuration
      PIN_CFG_7      : aliased PIN_CFG_7_Register;
      --  Pin configuration
      PIN_CFG_8      : aliased PIN_CFG_8_Register;
      --  Pin configuration
      PIN_CFG_9      : aliased PIN_CFG_9_Register;
      --  Pin configuration
      PIN_CFG_10     : aliased PIN_CFG_10_Register;
      --  Pin configuration
      PIN_CFG_11     : aliased PIN_CFG_11_Register;
      --  Pin configuration
      PIN_CFG_12     : aliased PIN_CFG_12_Register;
      --  Pin configuration
      PIN_CFG_13     : aliased PIN_CFG_13_Register;
      --  Pin configuration
      PIN_CFG_14     : aliased PIN_CFG_14_Register;
      --  Pin configuration
      PIN_CFG_15     : aliased PIN_CFG_15_Register;
      --  Pin configuration
      PIN_CFG_16     : aliased PIN_CFG_16_Register;
      --  Pin configuration
      PIN_CFG_17     : aliased PIN_CFG_17_Register;
      --  Pin configuration
      PIN_CFG_18     : aliased PIN_CFG_18_Register;
      --  Pin configuration
      PIN_CFG_19     : aliased PIN_CFG_19_Register;
      --  Pin configuration
      PIN_CFG_20     : aliased PIN_CFG_20_Register;
      --  Pin configuration
      PIN_CFG_21     : aliased PIN_CFG_21_Register;
      --  Pin configuration
      PIN_CFG_22     : aliased PIN_CFG_22_Register;
      --  Pin configuration
      PIN_CFG_23     : aliased PIN_CFG_23_Register;
      --  Pin configuration
      PIN_CFG_24     : aliased PIN_CFG_24_Register;
      --  Pin configuration
      PIN_CFG_25     : aliased PIN_CFG_25_Register;
      --  Pin configuration
      PIN_CFG_26     : aliased PIN_CFG_26_Register;
      --  Pin configuration
      PIN_CFG_27     : aliased PIN_CFG_27_Register;
      --  Pin configuration
      PIN_CFG_28     : aliased PIN_CFG_28_Register;
      --  Pin configuration
      PIN_CFG_29     : aliased PIN_CFG_29_Register;
      --  Pin configuration
      PIN_CFG_30     : aliased PIN_CFG_30_Register;
      --  Pin configuration
      PIN_CFG_31     : aliased PIN_CFG_31_Register;
   end record
     with Volatile;

   for GPIO_P0_Peripheral use record
      OUT_k          at 16#4# range 0 .. 31;
      OUTSET         at 16#8# range 0 .. 31;
      OUTCLR         at 16#C# range 0 .. 31;
      IN_k           at 16#10# range 0 .. 31;
      DIR            at 16#14# range 0 .. 31;
      DIRSET         at 16#18# range 0 .. 31;
      DIRCLR         at 16#1C# range 0 .. 31;
      LATCH          at 16#20# range 0 .. 31;
      DETECTMODE     at 16#24# range 0 .. 31;
      DETECTMODE_SEC at 16#28# range 0 .. 31;
      PIN_CFG_0      at 16#200# range 0 .. 31;
      PIN_CFG_1      at 16#204# range 0 .. 31;
      PIN_CFG_2      at 16#208# range 0 .. 31;
      PIN_CFG_3      at 16#20C# range 0 .. 31;
      PIN_CFG_4      at 16#210# range 0 .. 31;
      PIN_CFG_5      at 16#214# range 0 .. 31;
      PIN_CFG_6      at 16#218# range 0 .. 31;
      PIN_CFG_7      at 16#21C# range 0 .. 31;
      PIN_CFG_8      at 16#220# range 0 .. 31;
      PIN_CFG_9      at 16#224# range 0 .. 31;
      PIN_CFG_10     at 16#228# range 0 .. 31;
      PIN_CFG_11     at 16#22C# range 0 .. 31;
      PIN_CFG_12     at 16#230# range 0 .. 31;
      PIN_CFG_13     at 16#234# range 0 .. 31;
      PIN_CFG_14     at 16#238# range 0 .. 31;
      PIN_CFG_15     at 16#23C# range 0 .. 31;
      PIN_CFG_16     at 16#240# range 0 .. 31;
      PIN_CFG_17     at 16#244# range 0 .. 31;
      PIN_CFG_18     at 16#248# range 0 .. 31;
      PIN_CFG_19     at 16#24C# range 0 .. 31;
      PIN_CFG_20     at 16#250# range 0 .. 31;
      PIN_CFG_21     at 16#254# range 0 .. 31;
      PIN_CFG_22     at 16#258# range 0 .. 31;
      PIN_CFG_23     at 16#25C# range 0 .. 31;
      PIN_CFG_24     at 16#260# range 0 .. 31;
      PIN_CFG_25     at 16#264# range 0 .. 31;
      PIN_CFG_26     at 16#268# range 0 .. 31;
      PIN_CFG_27     at 16#26C# range 0 .. 31;
      PIN_CFG_28     at 16#270# range 0 .. 31;
      PIN_CFG_29     at 16#274# range 0 .. 31;
      PIN_CFG_30     at 16#278# range 0 .. 31;
      PIN_CFG_31     at 16#27C# range 0 .. 31;
   end record;

   --  General purpose input and output
   GPIO_P0_Periph : aliased GPIO_P0_Peripheral
     with Import, Address => System'To_Address (16#50842500#);

   --  General purpose input and output
   GPIO_P1_Periph : aliased GPIO_P0_Peripheral
     with Import, Address => System'To_Address (16#50842800#);

end NRF53_SVD.GPIO_P0;
