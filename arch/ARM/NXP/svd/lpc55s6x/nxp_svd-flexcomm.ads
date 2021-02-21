--  Copyright 2016-2019 NXP
--  All rights reserved.SPDX-License-Identifier: BSD-3-Clause


--  This spec has been automatically generated from LPC55S6x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NXP_SVD.FLEXCOMM is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Peripheral Select. This field is writable by software.
   type PSELID_PERSEL_Field is
     (
      --  No peripheral selected.
      No_Periph_Selected,
      --  USART function selected.
      Usart,
      --  SPI function selected.
      Spi,
      --  I2C function selected.
      I2C,
      --  I2S transmit function selected.
      I2S_Transmit,
      --  I2S receive function selected.
      I2S_Receive)
     with Size => 3;
   for PSELID_PERSEL_Field use
     (No_Periph_Selected => 0,
      Usart => 1,
      Spi => 2,
      I2C => 3,
      I2S_Transmit => 4,
      I2S_Receive => 5);

   --  Lock the peripheral select. This field is writable by software.
   type PSELID_LOCK_Field is
     (
      --  Peripheral select can be changed by software.
      Unlocked,
      --  Peripheral select is locked and cannot be changed until this Flexcomm
      --  or the entire device is reset.
      Locked)
     with Size => 1;
   for PSELID_LOCK_Field use
     (Unlocked => 0,
      Locked => 1);

   --  USART present indicator. This field is Read-only.
   type PSELID_USARTPRESENT_Field is
     (
      --  This Flexcomm does not include the USART function.
      Not_Present,
      --  This Flexcomm includes the USART function.
      Present)
     with Size => 1;
   for PSELID_USARTPRESENT_Field use
     (Not_Present => 0,
      Present => 1);

   --  SPI present indicator. This field is Read-only.
   type PSELID_SPIPRESENT_Field is
     (
      --  This Flexcomm does not include the SPI function.
      Not_Present,
      --  This Flexcomm includes the SPI function.
      Present)
     with Size => 1;
   for PSELID_SPIPRESENT_Field use
     (Not_Present => 0,
      Present => 1);

   --  I2C present indicator. This field is Read-only.
   type PSELID_I2CPRESENT_Field is
     (
      --  This Flexcomm does not include the I2C function.
      Not_Present,
      --  This Flexcomm includes the I2C function.
      Present)
     with Size => 1;
   for PSELID_I2CPRESENT_Field use
     (Not_Present => 0,
      Present => 1);

   --  I 2S present indicator. This field is Read-only.
   type PSELID_I2SPRESENT_Field is
     (
      --  This Flexcomm does not include the I2S function.
      Not_Present,
      --  This Flexcomm includes the I2S function.
      Present)
     with Size => 1;
   for PSELID_I2SPRESENT_Field use
     (Not_Present => 0,
      Present => 1);

   subtype PSELID_ID_Field is HAL.UInt20;

   --  Peripheral Select and Flexcomm ID register.
   type PSELID_Register is record
      --  Peripheral Select. This field is writable by software.
      PERSEL        : PSELID_PERSEL_Field :=
                       NXP_SVD.FLEXCOMM.No_Periph_Selected;
      --  Lock the peripheral select. This field is writable by software.
      LOCK          : PSELID_LOCK_Field := NXP_SVD.FLEXCOMM.Unlocked;
      --  Read-only. USART present indicator. This field is Read-only.
      USARTPRESENT  : PSELID_USARTPRESENT_Field :=
                       NXP_SVD.FLEXCOMM.Not_Present;
      --  Read-only. SPI present indicator. This field is Read-only.
      SPIPRESENT    : PSELID_SPIPRESENT_Field := NXP_SVD.FLEXCOMM.Not_Present;
      --  Read-only. I2C present indicator. This field is Read-only.
      I2CPRESENT    : PSELID_I2CPRESENT_Field := NXP_SVD.FLEXCOMM.Not_Present;
      --  Read-only. I 2S present indicator. This field is Read-only.
      I2SPRESENT    : PSELID_I2SPRESENT_Field := NXP_SVD.FLEXCOMM.Not_Present;
      --  unspecified
      Reserved_8_11 : HAL.UInt4 := 16#0#;
      --  Read-only. Flexcomm ID.
      ID            : PSELID_ID_Field := 16#101#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PSELID_Register use record
      PERSEL        at 0 range 0 .. 2;
      LOCK          at 0 range 3 .. 3;
      USARTPRESENT  at 0 range 4 .. 4;
      SPIPRESENT    at 0 range 5 .. 5;
      I2CPRESENT    at 0 range 6 .. 6;
      I2SPRESENT    at 0 range 7 .. 7;
      Reserved_8_11 at 0 range 8 .. 11;
      ID            at 0 range 12 .. 31;
   end record;

   subtype PID_APERTURE_Field is HAL.UInt8;
   subtype PID_MINOR_REV_Field is HAL.UInt4;
   subtype PID_MAJOR_REV_Field is HAL.UInt4;
   subtype PID_ID_Field is HAL.UInt16;

   --  Peripheral identification register.
   type PID_Register is record
      --  Read-only. size aperture for the register port on the bus (APB or
      --  AHB).
      APERTURE  : PID_APERTURE_Field;
      --  Read-only. Minor revision of module implementation.
      MINOR_REV : PID_MINOR_REV_Field;
      --  Read-only. Major revision of module implementation.
      MAJOR_REV : PID_MAJOR_REV_Field;
      --  Read-only. Module identifier for the selected function.
      ID        : PID_ID_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PID_Register use record
      APERTURE  at 0 range 0 .. 7;
      MINOR_REV at 0 range 8 .. 11;
      MAJOR_REV at 0 range 12 .. 15;
      ID        at 0 range 16 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Flexcomm serial communication
   type FLEXCOMM_Peripheral is record
      --  Peripheral Select and Flexcomm ID register.
      PSELID : aliased PSELID_Register;
      --  Peripheral identification register.
      PID    : aliased PID_Register;
   end record
     with Volatile;

   for FLEXCOMM_Peripheral use record
      PSELID at 16#FF8# range 0 .. 31;
      PID    at 16#FFC# range 0 .. 31;
   end record;

   --  Flexcomm serial communication
   FLEXCOMM0_Periph : aliased FLEXCOMM_Peripheral
     with Import, Address => System'To_Address (16#40086000#);

   --  Flexcomm serial communication
   FLEXCOMM1_Periph : aliased FLEXCOMM_Peripheral
     with Import, Address => System'To_Address (16#40087000#);

   --  Flexcomm serial communication
   FLEXCOMM2_Periph : aliased FLEXCOMM_Peripheral
     with Import, Address => System'To_Address (16#40088000#);

   --  Flexcomm serial communication
   FLEXCOMM3_Periph : aliased FLEXCOMM_Peripheral
     with Import, Address => System'To_Address (16#40089000#);

   --  Flexcomm serial communication
   FLEXCOMM4_Periph : aliased FLEXCOMM_Peripheral
     with Import, Address => System'To_Address (16#4008A000#);

   --  Flexcomm serial communication
   FLEXCOMM5_Periph : aliased FLEXCOMM_Peripheral
     with Import, Address => System'To_Address (16#40096000#);

   --  Flexcomm serial communication
   FLEXCOMM6_Periph : aliased FLEXCOMM_Peripheral
     with Import, Address => System'To_Address (16#40097000#);

   --  Flexcomm serial communication
   FLEXCOMM7_Periph : aliased FLEXCOMM_Peripheral
     with Import, Address => System'To_Address (16#40098000#);

   --  Flexcomm serial communication
   FLEXCOMM8_Periph : aliased FLEXCOMM_Peripheral
     with Import, Address => System'To_Address (16#4009F000#);

end NXP_SVD.FLEXCOMM;
