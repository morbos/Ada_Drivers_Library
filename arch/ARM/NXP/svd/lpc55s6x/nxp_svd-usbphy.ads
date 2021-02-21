--  Copyright 2016-2019 NXP
--  All rights reserved.SPDX-License-Identifier: BSD-3-Clause


--  This spec has been automatically generated from LPC55S6x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NXP_SVD.USBPHY is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Note that this bit will be auto cleared if there is USB wakeup event
   --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
   type PWD_TXPWDFS_Field is
     (
      --  Normal operation.
      VALUE0,
      --  Power-down the USB full-speed drivers. This turns off the current
      --  starvation sources and puts the
      VALUE1)
     with Size => 1;
   for PWD_TXPWDFS_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  Note that this bit will be auto cleared if there is USB wakeup event
   --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
   type PWD_TXPWDIBIAS_Field is
     (
      --  Normal operation.
      VALUE0,
      --  Power-down the USB PHY current bias block for the transmitter. This
      --  bit should be set only when the
      VALUE1)
     with Size => 1;
   for PWD_TXPWDIBIAS_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  Note that this bit will be auto cleared if there is USB wakeup event
   --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
   type PWD_TXPWDV2I_Field is
     (
      --  Normal operation.
      VALUE0,
      --  Power-down the USB PHY transmit V-to-I converter and the current
      --  mirror
      VALUE1)
     with Size => 1;
   for PWD_TXPWDV2I_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  Note that this bit will be auto cleared if there is USB wakeup event
   --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
   type PWD_RXPWDENV_Field is
     (
      --  Normal operation.
      VALUE0,
      --  Power-down the USB high-speed receiver envelope detector (squelch
      --  signal)
      VALUE1)
     with Size => 1;
   for PWD_RXPWDENV_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  Note that this bit will be auto cleared if there is USB wakeup event
   --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
   type PWD_RXPWD1PT1_Field is
     (
      --  Normal operation.
      VALUE0,
      --  Power-down the USB full-speed differential receiver.
      VALUE1)
     with Size => 1;
   for PWD_RXPWD1PT1_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  Note that this bit will be auto cleared if there is USB wakeup event
   --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
   type PWD_RXPWDDIFF_Field is
     (
      --  Normal operation.
      VALUE0,
      --  Power-down the USB high-speed differential receive
      VALUE1)
     with Size => 1;
   for PWD_RXPWDDIFF_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  This bit will be auto cleared if there is USB wakeup event while
   --  ENAUTOCLR_PHY_PWD bit of CTRL is enabled
   type PWD_RXPWDRX_Field is
     (
      --  Normal operation.
      VALUE0,
      --  Power-down the entire USB PHY receiver block except for the
      --  full-speed differential receiver
      VALUE1)
     with Size => 1;
   for PWD_RXPWDRX_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  USB PHY Power-Down Register
   type PWD_Register is record
      --  unspecified
      Reserved_0_9   : HAL.UInt10 := 16#0#;
      --  Note that this bit will be auto cleared if there is USB wakeup event
      --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
      TXPWDFS        : PWD_TXPWDFS_Field := NXP_SVD.USBPHY.VALUE1;
      --  Note that this bit will be auto cleared if there is USB wakeup event
      --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
      TXPWDIBIAS     : PWD_TXPWDIBIAS_Field := NXP_SVD.USBPHY.VALUE1;
      --  Note that this bit will be auto cleared if there is USB wakeup event
      --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
      TXPWDV2I       : PWD_TXPWDV2I_Field := NXP_SVD.USBPHY.VALUE1;
      --  unspecified
      Reserved_13_16 : HAL.UInt4 := 16#0#;
      --  Note that this bit will be auto cleared if there is USB wakeup event
      --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
      RXPWDENV       : PWD_RXPWDENV_Field := NXP_SVD.USBPHY.VALUE1;
      --  Note that this bit will be auto cleared if there is USB wakeup event
      --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
      RXPWD1PT1      : PWD_RXPWD1PT1_Field := NXP_SVD.USBPHY.VALUE1;
      --  Note that this bit will be auto cleared if there is USB wakeup event
      --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
      RXPWDDIFF      : PWD_RXPWDDIFF_Field := NXP_SVD.USBPHY.VALUE1;
      --  This bit will be auto cleared if there is USB wakeup event while
      --  ENAUTOCLR_PHY_PWD bit of CTRL is enabled
      RXPWDRX        : PWD_RXPWDRX_Field := NXP_SVD.USBPHY.VALUE1;
      --  unspecified
      Reserved_21_31 : HAL.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWD_Register use record
      Reserved_0_9   at 0 range 0 .. 9;
      TXPWDFS        at 0 range 10 .. 10;
      TXPWDIBIAS     at 0 range 11 .. 11;
      TXPWDV2I       at 0 range 12 .. 12;
      Reserved_13_16 at 0 range 13 .. 16;
      RXPWDENV       at 0 range 17 .. 17;
      RXPWD1PT1      at 0 range 18 .. 18;
      RXPWDDIFF      at 0 range 19 .. 19;
      RXPWDRX        at 0 range 20 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   --  Note that this bit will be auto cleared if there is USB wakeup event
   --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
   type PWD_SET_TXPWDFS_Field is
     (
      --  Normal operation.
      VALUE0,
      --  Power-down the USB full-speed drivers. This turns off the current
      --  starvation sources and puts the
      VALUE1)
     with Size => 1;
   for PWD_SET_TXPWDFS_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  Note that this bit will be auto cleared if there is USB wakeup event
   --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
   type PWD_SET_TXPWDIBIAS_Field is
     (
      --  Normal operation.
      VALUE0,
      --  Power-down the USB PHY current bias block for the transmitter. This
      --  bit should be set only when the
      VALUE1)
     with Size => 1;
   for PWD_SET_TXPWDIBIAS_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  Note that this bit will be auto cleared if there is USB wakeup event
   --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
   type PWD_SET_TXPWDV2I_Field is
     (
      --  Normal operation.
      VALUE0,
      --  Power-down the USB PHY transmit V-to-I converter and the current
      --  mirror
      VALUE1)
     with Size => 1;
   for PWD_SET_TXPWDV2I_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  Note that this bit will be auto cleared if there is USB wakeup event
   --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
   type PWD_SET_RXPWDENV_Field is
     (
      --  Normal operation.
      VALUE0,
      --  Power-down the USB high-speed receiver envelope detector (squelch
      --  signal)
      VALUE1)
     with Size => 1;
   for PWD_SET_RXPWDENV_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  Note that this bit will be auto cleared if there is USB wakeup event
   --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
   type PWD_SET_RXPWD1PT1_Field is
     (
      --  Normal operation.
      VALUE0,
      --  Power-down the USB full-speed differential receiver.
      VALUE1)
     with Size => 1;
   for PWD_SET_RXPWD1PT1_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  Note that this bit will be auto cleared if there is USB wakeup event
   --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
   type PWD_SET_RXPWDDIFF_Field is
     (
      --  Normal operation.
      VALUE0,
      --  Power-down the USB high-speed differential receive
      VALUE1)
     with Size => 1;
   for PWD_SET_RXPWDDIFF_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  This bit will be auto cleared if there is USB wakeup event while
   --  ENAUTOCLR_PHY_PWD bit of CTRL is enabled
   type PWD_SET_RXPWDRX_Field is
     (
      --  Normal operation.
      VALUE0,
      --  Power-down the entire USB PHY receiver block except for the
      --  full-speed differential receiver
      VALUE1)
     with Size => 1;
   for PWD_SET_RXPWDRX_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  USB PHY Power-Down Register
   type PWD_SET_Register is record
      --  unspecified
      Reserved_0_9   : HAL.UInt10 := 16#0#;
      --  Note that this bit will be auto cleared if there is USB wakeup event
      --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
      TXPWDFS        : PWD_SET_TXPWDFS_Field := NXP_SVD.USBPHY.VALUE1;
      --  Note that this bit will be auto cleared if there is USB wakeup event
      --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
      TXPWDIBIAS     : PWD_SET_TXPWDIBIAS_Field := NXP_SVD.USBPHY.VALUE1;
      --  Note that this bit will be auto cleared if there is USB wakeup event
      --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
      TXPWDV2I       : PWD_SET_TXPWDV2I_Field := NXP_SVD.USBPHY.VALUE1;
      --  unspecified
      Reserved_13_16 : HAL.UInt4 := 16#0#;
      --  Note that this bit will be auto cleared if there is USB wakeup event
      --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
      RXPWDENV       : PWD_SET_RXPWDENV_Field := NXP_SVD.USBPHY.VALUE1;
      --  Note that this bit will be auto cleared if there is USB wakeup event
      --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
      RXPWD1PT1      : PWD_SET_RXPWD1PT1_Field := NXP_SVD.USBPHY.VALUE1;
      --  Note that this bit will be auto cleared if there is USB wakeup event
      --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
      RXPWDDIFF      : PWD_SET_RXPWDDIFF_Field := NXP_SVD.USBPHY.VALUE1;
      --  This bit will be auto cleared if there is USB wakeup event while
      --  ENAUTOCLR_PHY_PWD bit of CTRL is enabled
      RXPWDRX        : PWD_SET_RXPWDRX_Field := NXP_SVD.USBPHY.VALUE1;
      --  unspecified
      Reserved_21_31 : HAL.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWD_SET_Register use record
      Reserved_0_9   at 0 range 0 .. 9;
      TXPWDFS        at 0 range 10 .. 10;
      TXPWDIBIAS     at 0 range 11 .. 11;
      TXPWDV2I       at 0 range 12 .. 12;
      Reserved_13_16 at 0 range 13 .. 16;
      RXPWDENV       at 0 range 17 .. 17;
      RXPWD1PT1      at 0 range 18 .. 18;
      RXPWDDIFF      at 0 range 19 .. 19;
      RXPWDRX        at 0 range 20 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   --  Note that this bit will be auto cleared if there is USB wakeup event
   --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
   type PWD_CLR_TXPWDFS_Field is
     (
      --  Normal operation.
      VALUE0,
      --  Power-down the USB full-speed drivers. This turns off the current
      --  starvation sources and puts the
      VALUE1)
     with Size => 1;
   for PWD_CLR_TXPWDFS_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  Note that this bit will be auto cleared if there is USB wakeup event
   --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
   type PWD_CLR_TXPWDIBIAS_Field is
     (
      --  Normal operation.
      VALUE0,
      --  Power-down the USB PHY current bias block for the transmitter. This
      --  bit should be set only when the
      VALUE1)
     with Size => 1;
   for PWD_CLR_TXPWDIBIAS_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  Note that this bit will be auto cleared if there is USB wakeup event
   --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
   type PWD_CLR_TXPWDV2I_Field is
     (
      --  Normal operation.
      VALUE0,
      --  Power-down the USB PHY transmit V-to-I converter and the current
      --  mirror
      VALUE1)
     with Size => 1;
   for PWD_CLR_TXPWDV2I_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  Note that this bit will be auto cleared if there is USB wakeup event
   --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
   type PWD_CLR_RXPWDENV_Field is
     (
      --  Normal operation.
      VALUE0,
      --  Power-down the USB high-speed receiver envelope detector (squelch
      --  signal)
      VALUE1)
     with Size => 1;
   for PWD_CLR_RXPWDENV_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  Note that this bit will be auto cleared if there is USB wakeup event
   --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
   type PWD_CLR_RXPWD1PT1_Field is
     (
      --  Normal operation.
      VALUE0,
      --  Power-down the USB full-speed differential receiver.
      VALUE1)
     with Size => 1;
   for PWD_CLR_RXPWD1PT1_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  Note that this bit will be auto cleared if there is USB wakeup event
   --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
   type PWD_CLR_RXPWDDIFF_Field is
     (
      --  Normal operation.
      VALUE0,
      --  Power-down the USB high-speed differential receive
      VALUE1)
     with Size => 1;
   for PWD_CLR_RXPWDDIFF_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  This bit will be auto cleared if there is USB wakeup event while
   --  ENAUTOCLR_PHY_PWD bit of CTRL is enabled
   type PWD_CLR_RXPWDRX_Field is
     (
      --  Normal operation.
      VALUE0,
      --  Power-down the entire USB PHY receiver block except for the
      --  full-speed differential receiver
      VALUE1)
     with Size => 1;
   for PWD_CLR_RXPWDRX_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  USB PHY Power-Down Register
   type PWD_CLR_Register is record
      --  unspecified
      Reserved_0_9   : HAL.UInt10 := 16#0#;
      --  Note that this bit will be auto cleared if there is USB wakeup event
      --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
      TXPWDFS        : PWD_CLR_TXPWDFS_Field := NXP_SVD.USBPHY.VALUE1;
      --  Note that this bit will be auto cleared if there is USB wakeup event
      --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
      TXPWDIBIAS     : PWD_CLR_TXPWDIBIAS_Field := NXP_SVD.USBPHY.VALUE1;
      --  Note that this bit will be auto cleared if there is USB wakeup event
      --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
      TXPWDV2I       : PWD_CLR_TXPWDV2I_Field := NXP_SVD.USBPHY.VALUE1;
      --  unspecified
      Reserved_13_16 : HAL.UInt4 := 16#0#;
      --  Note that this bit will be auto cleared if there is USB wakeup event
      --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
      RXPWDENV       : PWD_CLR_RXPWDENV_Field := NXP_SVD.USBPHY.VALUE1;
      --  Note that this bit will be auto cleared if there is USB wakeup event
      --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
      RXPWD1PT1      : PWD_CLR_RXPWD1PT1_Field := NXP_SVD.USBPHY.VALUE1;
      --  Note that this bit will be auto cleared if there is USB wakeup event
      --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
      RXPWDDIFF      : PWD_CLR_RXPWDDIFF_Field := NXP_SVD.USBPHY.VALUE1;
      --  This bit will be auto cleared if there is USB wakeup event while
      --  ENAUTOCLR_PHY_PWD bit of CTRL is enabled
      RXPWDRX        : PWD_CLR_RXPWDRX_Field := NXP_SVD.USBPHY.VALUE1;
      --  unspecified
      Reserved_21_31 : HAL.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWD_CLR_Register use record
      Reserved_0_9   at 0 range 0 .. 9;
      TXPWDFS        at 0 range 10 .. 10;
      TXPWDIBIAS     at 0 range 11 .. 11;
      TXPWDV2I       at 0 range 12 .. 12;
      Reserved_13_16 at 0 range 13 .. 16;
      RXPWDENV       at 0 range 17 .. 17;
      RXPWD1PT1      at 0 range 18 .. 18;
      RXPWDDIFF      at 0 range 19 .. 19;
      RXPWDRX        at 0 range 20 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   --  Note that this bit will be auto cleared if there is USB wakeup event
   --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
   type PWD_TOG_TXPWDFS_Field is
     (
      --  Normal operation.
      VALUE0,
      --  Power-down the USB full-speed drivers. This turns off the current
      --  starvation sources and puts the
      VALUE1)
     with Size => 1;
   for PWD_TOG_TXPWDFS_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  Note that this bit will be auto cleared if there is USB wakeup event
   --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
   type PWD_TOG_TXPWDIBIAS_Field is
     (
      --  Normal operation.
      VALUE0,
      --  Power-down the USB PHY current bias block for the transmitter. This
      --  bit should be set only when the
      VALUE1)
     with Size => 1;
   for PWD_TOG_TXPWDIBIAS_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  Note that this bit will be auto cleared if there is USB wakeup event
   --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
   type PWD_TOG_TXPWDV2I_Field is
     (
      --  Normal operation.
      VALUE0,
      --  Power-down the USB PHY transmit V-to-I converter and the current
      --  mirror
      VALUE1)
     with Size => 1;
   for PWD_TOG_TXPWDV2I_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  Note that this bit will be auto cleared if there is USB wakeup event
   --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
   type PWD_TOG_RXPWDENV_Field is
     (
      --  Normal operation.
      VALUE0,
      --  Power-down the USB high-speed receiver envelope detector (squelch
      --  signal)
      VALUE1)
     with Size => 1;
   for PWD_TOG_RXPWDENV_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  Note that this bit will be auto cleared if there is USB wakeup event
   --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
   type PWD_TOG_RXPWD1PT1_Field is
     (
      --  Normal operation.
      VALUE0,
      --  Power-down the USB full-speed differential receiver.
      VALUE1)
     with Size => 1;
   for PWD_TOG_RXPWD1PT1_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  Note that this bit will be auto cleared if there is USB wakeup event
   --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
   type PWD_TOG_RXPWDDIFF_Field is
     (
      --  Normal operation.
      VALUE0,
      --  Power-down the USB high-speed differential receive
      VALUE1)
     with Size => 1;
   for PWD_TOG_RXPWDDIFF_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  This bit will be auto cleared if there is USB wakeup event while
   --  ENAUTOCLR_PHY_PWD bit of CTRL is enabled
   type PWD_TOG_RXPWDRX_Field is
     (
      --  Normal operation.
      VALUE0,
      --  Power-down the entire USB PHY receiver block except for the
      --  full-speed differential receiver
      VALUE1)
     with Size => 1;
   for PWD_TOG_RXPWDRX_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  USB PHY Power-Down Register
   type PWD_TOG_Register is record
      --  unspecified
      Reserved_0_9   : HAL.UInt10 := 16#0#;
      --  Note that this bit will be auto cleared if there is USB wakeup event
      --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
      TXPWDFS        : PWD_TOG_TXPWDFS_Field := NXP_SVD.USBPHY.VALUE1;
      --  Note that this bit will be auto cleared if there is USB wakeup event
      --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
      TXPWDIBIAS     : PWD_TOG_TXPWDIBIAS_Field := NXP_SVD.USBPHY.VALUE1;
      --  Note that this bit will be auto cleared if there is USB wakeup event
      --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
      TXPWDV2I       : PWD_TOG_TXPWDV2I_Field := NXP_SVD.USBPHY.VALUE1;
      --  unspecified
      Reserved_13_16 : HAL.UInt4 := 16#0#;
      --  Note that this bit will be auto cleared if there is USB wakeup event
      --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
      RXPWDENV       : PWD_TOG_RXPWDENV_Field := NXP_SVD.USBPHY.VALUE1;
      --  Note that this bit will be auto cleared if there is USB wakeup event
      --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
      RXPWD1PT1      : PWD_TOG_RXPWD1PT1_Field := NXP_SVD.USBPHY.VALUE1;
      --  Note that this bit will be auto cleared if there is USB wakeup event
      --  while ENAUTOCLR_PHY_PWD bit of CTRL is enabled
      RXPWDDIFF      : PWD_TOG_RXPWDDIFF_Field := NXP_SVD.USBPHY.VALUE1;
      --  This bit will be auto cleared if there is USB wakeup event while
      --  ENAUTOCLR_PHY_PWD bit of CTRL is enabled
      RXPWDRX        : PWD_TOG_RXPWDRX_Field := NXP_SVD.USBPHY.VALUE1;
      --  unspecified
      Reserved_21_31 : HAL.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWD_TOG_Register use record
      Reserved_0_9   at 0 range 0 .. 9;
      TXPWDFS        at 0 range 10 .. 10;
      TXPWDIBIAS     at 0 range 11 .. 11;
      TXPWDV2I       at 0 range 12 .. 12;
      Reserved_13_16 at 0 range 13 .. 16;
      RXPWDENV       at 0 range 17 .. 17;
      RXPWD1PT1      at 0 range 18 .. 18;
      RXPWDDIFF      at 0 range 19 .. 19;
      RXPWDRX        at 0 range 20 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   --  Decode to trim the nominal 17
   type TX_D_CAL_Field is
     (
      --  Maximum current, approximately 19% above nominal.
      VALUE0,
      --  Reset value for the field
      Tx_D_Cal_Field_Reset,
      --  Nominal
      VALUE7,
      --  Minimum current, approximately 19% below nominal.
      VALUE15)
     with Size => 4;
   for TX_D_CAL_Field use
     (VALUE0 => 0,
      Tx_D_Cal_Field_Reset => 2,
      VALUE7 => 7,
      VALUE15 => 15);

   subtype TX_TXCAL45DM_Field is HAL.UInt4;
   subtype TX_TXCAL45DP_Field is HAL.UInt4;

   --  USB PHY Transmitter Control Register
   type TX_Register is record
      --  Decode to trim the nominal 17
      D_CAL          : TX_D_CAL_Field := Tx_D_Cal_Field_Reset;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Decode to trim the nominal 45ohm series termination resistance to the
      --  USB_DM output pin
      TXCAL45DM      : TX_TXCAL45DM_Field := 16#4#;
      --  unspecified
      Reserved_12_12 : HAL.Bit := 16#0#;
      --  Enable resistance calibration on DN.
      TXENCAL45DN    : Boolean := False;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  Decode to trim the nominal 45ohm series termination resistance to the
      --  USB_DP output pin
      TXCAL45DP      : TX_TXCAL45DP_Field := 16#0#;
      --  unspecified
      Reserved_20_20 : HAL.Bit := 16#0#;
      --  Enable resistance calibration on DP.
      TXENCAL45DP    : Boolean := False;
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#28#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TX_Register use record
      D_CAL          at 0 range 0 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      TXCAL45DM      at 0 range 8 .. 11;
      Reserved_12_12 at 0 range 12 .. 12;
      TXENCAL45DN    at 0 range 13 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      TXCAL45DP      at 0 range 16 .. 19;
      Reserved_20_20 at 0 range 20 .. 20;
      TXENCAL45DP    at 0 range 21 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  Decode to trim the nominal 17
   type TX_SET_D_CAL_Field is
     (
      --  Maximum current, approximately 19% above nominal.
      VALUE0,
      --  Reset value for the field
      Tx_Set_D_Cal_Field_Reset,
      --  Nominal
      VALUE7,
      --  Minimum current, approximately 19% below nominal.
      VALUE15)
     with Size => 4;
   for TX_SET_D_CAL_Field use
     (VALUE0 => 0,
      Tx_Set_D_Cal_Field_Reset => 2,
      VALUE7 => 7,
      VALUE15 => 15);

   subtype TX_SET_TXCAL45DM_Field is HAL.UInt4;
   subtype TX_SET_TXCAL45DP_Field is HAL.UInt4;

   --  USB PHY Transmitter Control Register
   type TX_SET_Register is record
      --  Decode to trim the nominal 17
      D_CAL          : TX_SET_D_CAL_Field := Tx_Set_D_Cal_Field_Reset;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Decode to trim the nominal 45ohm series termination resistance to the
      --  USB_DM output pin
      TXCAL45DM      : TX_SET_TXCAL45DM_Field := 16#4#;
      --  unspecified
      Reserved_12_12 : HAL.Bit := 16#0#;
      --  Enable resistance calibration on DN.
      TXENCAL45DN    : Boolean := False;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  Decode to trim the nominal 45ohm series termination resistance to the
      --  USB_DP output pin
      TXCAL45DP      : TX_SET_TXCAL45DP_Field := 16#0#;
      --  unspecified
      Reserved_20_20 : HAL.Bit := 16#0#;
      --  Enable resistance calibration on DP.
      TXENCAL45DP    : Boolean := False;
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#28#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TX_SET_Register use record
      D_CAL          at 0 range 0 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      TXCAL45DM      at 0 range 8 .. 11;
      Reserved_12_12 at 0 range 12 .. 12;
      TXENCAL45DN    at 0 range 13 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      TXCAL45DP      at 0 range 16 .. 19;
      Reserved_20_20 at 0 range 20 .. 20;
      TXENCAL45DP    at 0 range 21 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  Decode to trim the nominal 17
   type TX_CLR_D_CAL_Field is
     (
      --  Maximum current, approximately 19% above nominal.
      VALUE0,
      --  Reset value for the field
      Tx_Clr_D_Cal_Field_Reset,
      --  Nominal
      VALUE7,
      --  Minimum current, approximately 19% below nominal.
      VALUE15)
     with Size => 4;
   for TX_CLR_D_CAL_Field use
     (VALUE0 => 0,
      Tx_Clr_D_Cal_Field_Reset => 2,
      VALUE7 => 7,
      VALUE15 => 15);

   subtype TX_CLR_TXCAL45DM_Field is HAL.UInt4;
   subtype TX_CLR_TXCAL45DP_Field is HAL.UInt4;

   --  USB PHY Transmitter Control Register
   type TX_CLR_Register is record
      --  Decode to trim the nominal 17
      D_CAL          : TX_CLR_D_CAL_Field := Tx_Clr_D_Cal_Field_Reset;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Decode to trim the nominal 45ohm series termination resistance to the
      --  USB_DM output pin
      TXCAL45DM      : TX_CLR_TXCAL45DM_Field := 16#4#;
      --  unspecified
      Reserved_12_12 : HAL.Bit := 16#0#;
      --  Enable resistance calibration on DN.
      TXENCAL45DN    : Boolean := False;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  Decode to trim the nominal 45ohm series termination resistance to the
      --  USB_DP output pin
      TXCAL45DP      : TX_CLR_TXCAL45DP_Field := 16#0#;
      --  unspecified
      Reserved_20_20 : HAL.Bit := 16#0#;
      --  Enable resistance calibration on DP.
      TXENCAL45DP    : Boolean := False;
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#28#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TX_CLR_Register use record
      D_CAL          at 0 range 0 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      TXCAL45DM      at 0 range 8 .. 11;
      Reserved_12_12 at 0 range 12 .. 12;
      TXENCAL45DN    at 0 range 13 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      TXCAL45DP      at 0 range 16 .. 19;
      Reserved_20_20 at 0 range 20 .. 20;
      TXENCAL45DP    at 0 range 21 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  Decode to trim the nominal 17
   type TX_TOG_D_CAL_Field is
     (
      --  Maximum current, approximately 19% above nominal.
      VALUE0,
      --  Reset value for the field
      Tx_Tog_D_Cal_Field_Reset,
      --  Nominal
      VALUE7,
      --  Minimum current, approximately 19% below nominal.
      VALUE15)
     with Size => 4;
   for TX_TOG_D_CAL_Field use
     (VALUE0 => 0,
      Tx_Tog_D_Cal_Field_Reset => 2,
      VALUE7 => 7,
      VALUE15 => 15);

   subtype TX_TOG_TXCAL45DM_Field is HAL.UInt4;
   subtype TX_TOG_TXCAL45DP_Field is HAL.UInt4;

   --  USB PHY Transmitter Control Register
   type TX_TOG_Register is record
      --  Decode to trim the nominal 17
      D_CAL          : TX_TOG_D_CAL_Field := Tx_Tog_D_Cal_Field_Reset;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Decode to trim the nominal 45ohm series termination resistance to the
      --  USB_DM output pin
      TXCAL45DM      : TX_TOG_TXCAL45DM_Field := 16#4#;
      --  unspecified
      Reserved_12_12 : HAL.Bit := 16#0#;
      --  Enable resistance calibration on DN.
      TXENCAL45DN    : Boolean := False;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  Decode to trim the nominal 45ohm series termination resistance to the
      --  USB_DP output pin
      TXCAL45DP      : TX_TOG_TXCAL45DP_Field := 16#0#;
      --  unspecified
      Reserved_20_20 : HAL.Bit := 16#0#;
      --  Enable resistance calibration on DP.
      TXENCAL45DP    : Boolean := False;
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#28#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TX_TOG_Register use record
      D_CAL          at 0 range 0 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      TXCAL45DM      at 0 range 8 .. 11;
      Reserved_12_12 at 0 range 12 .. 12;
      TXENCAL45DN    at 0 range 13 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      TXCAL45DP      at 0 range 16 .. 19;
      Reserved_20_20 at 0 range 20 .. 20;
      TXENCAL45DP    at 0 range 21 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  The ENVADJ field adjusts the trip point for the envelope detector
   type RX_ENVADJ_Field is
     (
      --  Trip-Level Voltage is 0.1000 V
      VALUE0,
      --  Trip-Level Voltage is 0.1125 V
      VALUE1,
      --  Trip-Level Voltage is 0.1250 V
      VALUE2,
      --  Trip-Level Voltage is 0.0875 V
      VALUE3)
     with Size => 3;
   for RX_ENVADJ_Field use
     (VALUE0 => 0,
      VALUE1 => 1,
      VALUE2 => 2,
      VALUE3 => 3);

   --  The DISCONADJ field adjusts the trip point for the disconnect detector.
   type RX_DISCONADJ_Field is
     (
      --  Trip-Level Voltage is 0.56875 V
      VALUE0,
      --  Trip-Level Voltage is 0.55000 V
      VALUE1,
      --  Trip-Level Voltage is 0.58125 V
      VALUE2,
      --  Trip-Level Voltage is 0.60000 V
      VALUE3)
     with Size => 3;
   for RX_DISCONADJ_Field use
     (VALUE0 => 0,
      VALUE1 => 1,
      VALUE2 => 2,
      VALUE3 => 3);

   --  This test mode is intended for lab use only, replace FS differential
   --  receiver with DP single ended receiver
   type RX_RXDBYPASS_Field is
     (
      --  Normal operation.
      VALUE0,
      --  Use the output of the USB_DP single-ended receiver in place of the
      --  full-speed differential receiver
      VALUE1)
     with Size => 1;
   for RX_RXDBYPASS_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  USB PHY Receiver Control Register
   type RX_Register is record
      --  The ENVADJ field adjusts the trip point for the envelope detector
      ENVADJ         : RX_ENVADJ_Field := NXP_SVD.USBPHY.VALUE0;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  The DISCONADJ field adjusts the trip point for the disconnect
      --  detector.
      DISCONADJ      : RX_DISCONADJ_Field := NXP_SVD.USBPHY.VALUE0;
      --  unspecified
      Reserved_7_21  : HAL.UInt15 := 16#0#;
      --  This test mode is intended for lab use only, replace FS differential
      --  receiver with DP single ended receiver
      RXDBYPASS      : RX_RXDBYPASS_Field := NXP_SVD.USBPHY.VALUE0;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RX_Register use record
      ENVADJ         at 0 range 0 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      DISCONADJ      at 0 range 4 .. 6;
      Reserved_7_21  at 0 range 7 .. 21;
      RXDBYPASS      at 0 range 22 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  The ENVADJ field adjusts the trip point for the envelope detector
   type RX_SET_ENVADJ_Field is
     (
      --  Trip-Level Voltage is 0.1000 V
      VALUE0,
      --  Trip-Level Voltage is 0.1125 V
      VALUE1,
      --  Trip-Level Voltage is 0.1250 V
      VALUE2,
      --  Trip-Level Voltage is 0.0875 V
      VALUE3)
     with Size => 3;
   for RX_SET_ENVADJ_Field use
     (VALUE0 => 0,
      VALUE1 => 1,
      VALUE2 => 2,
      VALUE3 => 3);

   --  The DISCONADJ field adjusts the trip point for the disconnect detector.
   type RX_SET_DISCONADJ_Field is
     (
      --  Trip-Level Voltage is 0.56875 V
      VALUE0,
      --  Trip-Level Voltage is 0.55000 V
      VALUE1,
      --  Trip-Level Voltage is 0.58125 V
      VALUE2,
      --  Trip-Level Voltage is 0.60000 V
      VALUE3)
     with Size => 3;
   for RX_SET_DISCONADJ_Field use
     (VALUE0 => 0,
      VALUE1 => 1,
      VALUE2 => 2,
      VALUE3 => 3);

   --  This test mode is intended for lab use only, replace FS differential
   --  receiver with DP single ended receiver
   type RX_SET_RXDBYPASS_Field is
     (
      --  Normal operation.
      VALUE0,
      --  Use the output of the USB_DP single-ended receiver in place of the
      --  full-speed differential receiver
      VALUE1)
     with Size => 1;
   for RX_SET_RXDBYPASS_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  USB PHY Receiver Control Register
   type RX_SET_Register is record
      --  The ENVADJ field adjusts the trip point for the envelope detector
      ENVADJ         : RX_SET_ENVADJ_Field := NXP_SVD.USBPHY.VALUE0;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  The DISCONADJ field adjusts the trip point for the disconnect
      --  detector.
      DISCONADJ      : RX_SET_DISCONADJ_Field := NXP_SVD.USBPHY.VALUE0;
      --  unspecified
      Reserved_7_21  : HAL.UInt15 := 16#0#;
      --  This test mode is intended for lab use only, replace FS differential
      --  receiver with DP single ended receiver
      RXDBYPASS      : RX_SET_RXDBYPASS_Field := NXP_SVD.USBPHY.VALUE0;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RX_SET_Register use record
      ENVADJ         at 0 range 0 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      DISCONADJ      at 0 range 4 .. 6;
      Reserved_7_21  at 0 range 7 .. 21;
      RXDBYPASS      at 0 range 22 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  The ENVADJ field adjusts the trip point for the envelope detector
   type RX_CLR_ENVADJ_Field is
     (
      --  Trip-Level Voltage is 0.1000 V
      VALUE0,
      --  Trip-Level Voltage is 0.1125 V
      VALUE1,
      --  Trip-Level Voltage is 0.1250 V
      VALUE2,
      --  Trip-Level Voltage is 0.0875 V
      VALUE3)
     with Size => 3;
   for RX_CLR_ENVADJ_Field use
     (VALUE0 => 0,
      VALUE1 => 1,
      VALUE2 => 2,
      VALUE3 => 3);

   --  The DISCONADJ field adjusts the trip point for the disconnect detector.
   type RX_CLR_DISCONADJ_Field is
     (
      --  Trip-Level Voltage is 0.56875 V
      VALUE0,
      --  Trip-Level Voltage is 0.55000 V
      VALUE1,
      --  Trip-Level Voltage is 0.58125 V
      VALUE2,
      --  Trip-Level Voltage is 0.60000 V
      VALUE3)
     with Size => 3;
   for RX_CLR_DISCONADJ_Field use
     (VALUE0 => 0,
      VALUE1 => 1,
      VALUE2 => 2,
      VALUE3 => 3);

   --  This test mode is intended for lab use only, replace FS differential
   --  receiver with DP single ended receiver
   type RX_CLR_RXDBYPASS_Field is
     (
      --  Normal operation.
      VALUE0,
      --  Use the output of the USB_DP single-ended receiver in place of the
      --  full-speed differential receiver
      VALUE1)
     with Size => 1;
   for RX_CLR_RXDBYPASS_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  USB PHY Receiver Control Register
   type RX_CLR_Register is record
      --  The ENVADJ field adjusts the trip point for the envelope detector
      ENVADJ         : RX_CLR_ENVADJ_Field := NXP_SVD.USBPHY.VALUE0;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  The DISCONADJ field adjusts the trip point for the disconnect
      --  detector.
      DISCONADJ      : RX_CLR_DISCONADJ_Field := NXP_SVD.USBPHY.VALUE0;
      --  unspecified
      Reserved_7_21  : HAL.UInt15 := 16#0#;
      --  This test mode is intended for lab use only, replace FS differential
      --  receiver with DP single ended receiver
      RXDBYPASS      : RX_CLR_RXDBYPASS_Field := NXP_SVD.USBPHY.VALUE0;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RX_CLR_Register use record
      ENVADJ         at 0 range 0 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      DISCONADJ      at 0 range 4 .. 6;
      Reserved_7_21  at 0 range 7 .. 21;
      RXDBYPASS      at 0 range 22 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  The ENVADJ field adjusts the trip point for the envelope detector
   type RX_TOG_ENVADJ_Field is
     (
      --  Trip-Level Voltage is 0.1000 V
      VALUE0,
      --  Trip-Level Voltage is 0.1125 V
      VALUE1,
      --  Trip-Level Voltage is 0.1250 V
      VALUE2,
      --  Trip-Level Voltage is 0.0875 V
      VALUE3)
     with Size => 3;
   for RX_TOG_ENVADJ_Field use
     (VALUE0 => 0,
      VALUE1 => 1,
      VALUE2 => 2,
      VALUE3 => 3);

   --  The DISCONADJ field adjusts the trip point for the disconnect detector.
   type RX_TOG_DISCONADJ_Field is
     (
      --  Trip-Level Voltage is 0.56875 V
      VALUE0,
      --  Trip-Level Voltage is 0.55000 V
      VALUE1,
      --  Trip-Level Voltage is 0.58125 V
      VALUE2,
      --  Trip-Level Voltage is 0.60000 V
      VALUE3)
     with Size => 3;
   for RX_TOG_DISCONADJ_Field use
     (VALUE0 => 0,
      VALUE1 => 1,
      VALUE2 => 2,
      VALUE3 => 3);

   --  This test mode is intended for lab use only, replace FS differential
   --  receiver with DP single ended receiver
   type RX_TOG_RXDBYPASS_Field is
     (
      --  Normal operation.
      VALUE0,
      --  Use the output of the USB_DP single-ended receiver in place of the
      --  full-speed differential receiver
      VALUE1)
     with Size => 1;
   for RX_TOG_RXDBYPASS_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  USB PHY Receiver Control Register
   type RX_TOG_Register is record
      --  The ENVADJ field adjusts the trip point for the envelope detector
      ENVADJ         : RX_TOG_ENVADJ_Field := NXP_SVD.USBPHY.VALUE0;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  The DISCONADJ field adjusts the trip point for the disconnect
      --  detector.
      DISCONADJ      : RX_TOG_DISCONADJ_Field := NXP_SVD.USBPHY.VALUE0;
      --  unspecified
      Reserved_7_21  : HAL.UInt15 := 16#0#;
      --  This test mode is intended for lab use only, replace FS differential
      --  receiver with DP single ended receiver
      RXDBYPASS      : RX_TOG_RXDBYPASS_Field := NXP_SVD.USBPHY.VALUE0;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RX_TOG_Register use record
      ENVADJ         at 0 range 0 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      DISCONADJ      at 0 range 4 .. 6;
      Reserved_7_21  at 0 range 7 .. 21;
      RXDBYPASS      at 0 range 22 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  Enables non-standard resistive plugged-in detection This bit field
   --  controls connection of nominal 200kohm resistors to both the USB_DP and
   --  USB_DM pins as one method of detecting when a USB cable is attached in
   --  device mode
   type CTRL_ENDEVPLUGINDET_Field is
     (
      --  Disables 200kohm pullup resistors on USB_DP and USB_DM pins (Default)
      VALUE0,
      --  Enables 200kohm pullup resistors on USB_DP and USB_DM pins
      VALUE1)
     with Size => 1;
   for CTRL_ENDEVPLUGINDET_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  CTRL_ENUTMILEVEL array
   type CTRL_ENUTMILEVEL_Field_Array is array (2 .. 3) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CTRL_ENUTMILEVEL
   type CTRL_ENUTMILEVEL_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  ENUTMILEVEL as a value
            Val : HAL.UInt2;
         when True =>
            --  ENUTMILEVEL as an array
            Arr : CTRL_ENUTMILEVEL_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CTRL_ENUTMILEVEL_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  USB PHY General Control Register
   type CTRL_Register is record
      --  unspecified
      Reserved_0_0         : HAL.Bit := 16#0#;
      --  For host mode, enables high-speed disconnect detector
      ENHOSTDISCONDETECT   : Boolean := False;
      --  Enable IRQ for Host disconnect: Enables interrupt for detection of
      --  disconnection to Device when in high-speed host mode
      ENIRQHOSTDISCON      : Boolean := False;
      --  Indicates that the device has disconnected in High-Speed mode
      HOSTDISCONDETECT_IRQ : Boolean := False;
      --  Enables non-standard resistive plugged-in detection This bit field
      --  controls connection of nominal 200kohm resistors to both the USB_DP
      --  and USB_DM pins as one method of detecting when a USB cable is
      --  attached in device mode
      ENDEVPLUGINDET       : CTRL_ENDEVPLUGINDET_Field :=
                              NXP_SVD.USBPHY.VALUE0;
      --  Device plugin polarity: For device mode, if this bit is cleared to 0,
      --  then it trips the interrupt if the device is plugged in
      DEVPLUGIN_POLARITY   : Boolean := False;
      --  unspecified
      Reserved_6_7         : HAL.UInt2 := 16#0#;
      --  Resume IRQ: Set to 1 will make RESUME_IRQ bit a sticky bit until
      --  software clear it
      RESUMEIRQSTICKY      : Boolean := False;
      --  Enable IRQ Resume detect: Enables interrupt for detection of a non-J
      --  state on the USB line
      ENIRQRESUMEDETECT    : Boolean := False;
      --  Resume IRQ: Indicates that the host is sending a wake-up after
      --  suspend
      RESUME_IRQ           : Boolean := False;
      --  unspecified
      Reserved_11_11       : HAL.Bit := 16#0#;
      --  Indicates that the device is connected
      DEVPLUGIN_IRQ        : Boolean := False;
      --  unspecified
      Reserved_13_13       : HAL.Bit := 16#0#;
      --  Enables UTMI+ Level 2 operation for the USB HS PHY
      ENUTMILEVEL          : CTRL_ENUTMILEVEL_Field :=
                              (As_Array => False, Val => 16#0#);
      --  Enable wake-up IRQ: Enables interrupt for the wake-up events.
      ENIRQWAKEUP          : Boolean := False;
      --  Wake-up IRQ: Indicates that there is a wak-eup event
      WAKEUP_IRQ           : Boolean := False;
      --  Enable the auto resume feature, when set, HW will use 32KHz clock to
      --  send Resume to respond to the device remote wakeup(for host mode
      --  only)
      AUTORESUME_EN        : Boolean := False;
      --  Enables the feature to auto-clear the CLKGATE bit if there is wakeup
      --  event while USB is suspended
      ENAUTOCLR_CLKGATE    : Boolean := False;
      --  Enables the feature to auto-clear the PWD register bits in PWD if
      --  there is wakeup event while USB is suspended
      ENAUTOCLR_PHY_PWD    : Boolean := False;
      --  Enable DP DM change wake-up: Not for customer use
      ENDPDMCHG_WKUP       : Boolean := False;
      --  unspecified
      Reserved_22_22       : HAL.Bit := 16#0#;
      --  Enable VBUS change wake-up: Enables the feature to wake-up USB if
      --  VBUS is toggled when USB is suspended
      ENVBUSCHG_WKUP       : Boolean := False;
      --  unspecified
      Reserved_24_24       : HAL.Bit := 16#0#;
      --  Enable auto-clear USB Clock gate: Enables the feature to auto-clear
      --  the USB0_CLKGATE/USB1_CLKGATE register bit in HW_DIGCTL_CTRL if there
      --  is wake-up event on USB0/USB1 while USB0/USB1 is suspended
      ENAUTOCLR_USBCLKGATE : Boolean := False;
      --  Enable auto-set of USB clocks: Enables the feature to auto-clear the
      --  EN_USB_CLKS register bits in HW_CLKCTRL_PLL1CTRL0/HW_CLKCTRL_P
      --  LL1CTRL1 if there is wake-up event on USB0/USB1 while USB0/USB1 is
      --  suspended
      ENAUTOSET_USBCLKS    : Boolean := False;
      --  unspecified
      Reserved_27_27       : HAL.Bit := 16#0#;
      --  Forces the next FS packet that is transmitted to have a EOP with
      --  low-speed timing
      HOST_FORCE_LS_SE0    : Boolean := False;
      --  Used by the PHY to indicate a powered-down state
      UTMI_SUSPENDM        : Boolean := False;
      --  Gate UTMI Clocks
      CLKGATE              : Boolean := True;
      --  Writing a 1 to this bit will soft-reset the PWD, TX, RX, and CTRL
      --  registers
      SFTRST               : Boolean := True;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CTRL_Register use record
      Reserved_0_0         at 0 range 0 .. 0;
      ENHOSTDISCONDETECT   at 0 range 1 .. 1;
      ENIRQHOSTDISCON      at 0 range 2 .. 2;
      HOSTDISCONDETECT_IRQ at 0 range 3 .. 3;
      ENDEVPLUGINDET       at 0 range 4 .. 4;
      DEVPLUGIN_POLARITY   at 0 range 5 .. 5;
      Reserved_6_7         at 0 range 6 .. 7;
      RESUMEIRQSTICKY      at 0 range 8 .. 8;
      ENIRQRESUMEDETECT    at 0 range 9 .. 9;
      RESUME_IRQ           at 0 range 10 .. 10;
      Reserved_11_11       at 0 range 11 .. 11;
      DEVPLUGIN_IRQ        at 0 range 12 .. 12;
      Reserved_13_13       at 0 range 13 .. 13;
      ENUTMILEVEL          at 0 range 14 .. 15;
      ENIRQWAKEUP          at 0 range 16 .. 16;
      WAKEUP_IRQ           at 0 range 17 .. 17;
      AUTORESUME_EN        at 0 range 18 .. 18;
      ENAUTOCLR_CLKGATE    at 0 range 19 .. 19;
      ENAUTOCLR_PHY_PWD    at 0 range 20 .. 20;
      ENDPDMCHG_WKUP       at 0 range 21 .. 21;
      Reserved_22_22       at 0 range 22 .. 22;
      ENVBUSCHG_WKUP       at 0 range 23 .. 23;
      Reserved_24_24       at 0 range 24 .. 24;
      ENAUTOCLR_USBCLKGATE at 0 range 25 .. 25;
      ENAUTOSET_USBCLKS    at 0 range 26 .. 26;
      Reserved_27_27       at 0 range 27 .. 27;
      HOST_FORCE_LS_SE0    at 0 range 28 .. 28;
      UTMI_SUSPENDM        at 0 range 29 .. 29;
      CLKGATE              at 0 range 30 .. 30;
      SFTRST               at 0 range 31 .. 31;
   end record;

   --  Enables non-standard resistive plugged-in detection This bit field
   --  controls connection of nominal 200kohm resistors to both the USB_DP and
   --  USB_DM pins as one method of detecting when a USB cable is attached in
   --  device mode
   type CTRL_SET_ENDEVPLUGINDET_Field is
     (
      --  Disables 200kohm pullup resistors on USB_DP and USB_DM pins (Default)
      VALUE0,
      --  Enables 200kohm pullup resistors on USB_DP and USB_DM pins
      VALUE1)
     with Size => 1;
   for CTRL_SET_ENDEVPLUGINDET_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  CTRL_SET_ENUTMILEVEL array
   type CTRL_SET_ENUTMILEVEL_Field_Array is array (2 .. 3) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CTRL_SET_ENUTMILEVEL
   type CTRL_SET_ENUTMILEVEL_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  ENUTMILEVEL as a value
            Val : HAL.UInt2;
         when True =>
            --  ENUTMILEVEL as an array
            Arr : CTRL_SET_ENUTMILEVEL_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CTRL_SET_ENUTMILEVEL_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  USB PHY General Control Register
   type CTRL_SET_Register is record
      --  unspecified
      Reserved_0_0         : HAL.Bit := 16#0#;
      --  For host mode, enables high-speed disconnect detector
      ENHOSTDISCONDETECT   : Boolean := False;
      --  Enable IRQ for Host disconnect: Enables interrupt for detection of
      --  disconnection to Device when in high-speed host mode
      ENIRQHOSTDISCON      : Boolean := False;
      --  Indicates that the device has disconnected in High-Speed mode
      HOSTDISCONDETECT_IRQ : Boolean := False;
      --  Enables non-standard resistive plugged-in detection This bit field
      --  controls connection of nominal 200kohm resistors to both the USB_DP
      --  and USB_DM pins as one method of detecting when a USB cable is
      --  attached in device mode
      ENDEVPLUGINDET       : CTRL_SET_ENDEVPLUGINDET_Field :=
                              NXP_SVD.USBPHY.VALUE0;
      --  Device plugin polarity: For device mode, if this bit is cleared to 0,
      --  then it trips the interrupt if the device is plugged in
      DEVPLUGIN_POLARITY   : Boolean := False;
      --  unspecified
      Reserved_6_7         : HAL.UInt2 := 16#0#;
      --  Resume IRQ: Set to 1 will make RESUME_IRQ bit a sticky bit until
      --  software clear it
      RESUMEIRQSTICKY      : Boolean := False;
      --  Enable IRQ Resume detect: Enables interrupt for detection of a non-J
      --  state on the USB line
      ENIRQRESUMEDETECT    : Boolean := False;
      --  Resume IRQ: Indicates that the host is sending a wake-up after
      --  suspend
      RESUME_IRQ           : Boolean := False;
      --  unspecified
      Reserved_11_11       : HAL.Bit := 16#0#;
      --  Indicates that the device is connected
      DEVPLUGIN_IRQ        : Boolean := False;
      --  unspecified
      Reserved_13_13       : HAL.Bit := 16#0#;
      --  Enables UTMI+ Level 2 operation for the USB HS PHY
      ENUTMILEVEL          : CTRL_SET_ENUTMILEVEL_Field :=
                              (As_Array => False, Val => 16#0#);
      --  Enable wake-up IRQ: Enables interrupt for the wake-up events.
      ENIRQWAKEUP          : Boolean := False;
      --  Wake-up IRQ: Indicates that there is a wak-eup event
      WAKEUP_IRQ           : Boolean := False;
      --  Enable the auto resume feature, when set, HW will use 32KHz clock to
      --  send Resume to respond to the device remote wakeup(for host mode
      --  only)
      AUTORESUME_EN        : Boolean := False;
      --  Enables the feature to auto-clear the CLKGATE bit if there is wakeup
      --  event while USB is suspended
      ENAUTOCLR_CLKGATE    : Boolean := False;
      --  Enables the feature to auto-clear the PWD register bits in PWD if
      --  there is wakeup event while USB is suspended
      ENAUTOCLR_PHY_PWD    : Boolean := False;
      --  Enable DP DM change wake-up: Not for customer use
      ENDPDMCHG_WKUP       : Boolean := False;
      --  unspecified
      Reserved_22_22       : HAL.Bit := 16#0#;
      --  Enable VBUS change wake-up: Enables the feature to wake-up USB if
      --  VBUS is toggled when USB is suspended
      ENVBUSCHG_WKUP       : Boolean := False;
      --  unspecified
      Reserved_24_24       : HAL.Bit := 16#0#;
      --  Enable auto-clear USB Clock gate: Enables the feature to auto-clear
      --  the USB0_CLKGATE/USB1_CLKGATE register bit in HW_DIGCTL_CTRL if there
      --  is wake-up event on USB0/USB1 while USB0/USB1 is suspended
      ENAUTOCLR_USBCLKGATE : Boolean := False;
      --  Enable auto-set of USB clocks: Enables the feature to auto-clear the
      --  EN_USB_CLKS register bits in HW_CLKCTRL_PLL1CTRL0/HW_CLKCTRL_P
      --  LL1CTRL1 if there is wake-up event on USB0/USB1 while USB0/USB1 is
      --  suspended
      ENAUTOSET_USBCLKS    : Boolean := False;
      --  unspecified
      Reserved_27_27       : HAL.Bit := 16#0#;
      --  Forces the next FS packet that is transmitted to have a EOP with
      --  low-speed timing
      HOST_FORCE_LS_SE0    : Boolean := False;
      --  Read-only. Used by the PHY to indicate a powered-down state
      UTMI_SUSPENDM        : Boolean := False;
      --  Gate UTMI Clocks
      CLKGATE              : Boolean := True;
      --  Writing a 1 to this bit will soft-reset the PWD, TX, RX, and CTRL
      --  registers
      SFTRST               : Boolean := True;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CTRL_SET_Register use record
      Reserved_0_0         at 0 range 0 .. 0;
      ENHOSTDISCONDETECT   at 0 range 1 .. 1;
      ENIRQHOSTDISCON      at 0 range 2 .. 2;
      HOSTDISCONDETECT_IRQ at 0 range 3 .. 3;
      ENDEVPLUGINDET       at 0 range 4 .. 4;
      DEVPLUGIN_POLARITY   at 0 range 5 .. 5;
      Reserved_6_7         at 0 range 6 .. 7;
      RESUMEIRQSTICKY      at 0 range 8 .. 8;
      ENIRQRESUMEDETECT    at 0 range 9 .. 9;
      RESUME_IRQ           at 0 range 10 .. 10;
      Reserved_11_11       at 0 range 11 .. 11;
      DEVPLUGIN_IRQ        at 0 range 12 .. 12;
      Reserved_13_13       at 0 range 13 .. 13;
      ENUTMILEVEL          at 0 range 14 .. 15;
      ENIRQWAKEUP          at 0 range 16 .. 16;
      WAKEUP_IRQ           at 0 range 17 .. 17;
      AUTORESUME_EN        at 0 range 18 .. 18;
      ENAUTOCLR_CLKGATE    at 0 range 19 .. 19;
      ENAUTOCLR_PHY_PWD    at 0 range 20 .. 20;
      ENDPDMCHG_WKUP       at 0 range 21 .. 21;
      Reserved_22_22       at 0 range 22 .. 22;
      ENVBUSCHG_WKUP       at 0 range 23 .. 23;
      Reserved_24_24       at 0 range 24 .. 24;
      ENAUTOCLR_USBCLKGATE at 0 range 25 .. 25;
      ENAUTOSET_USBCLKS    at 0 range 26 .. 26;
      Reserved_27_27       at 0 range 27 .. 27;
      HOST_FORCE_LS_SE0    at 0 range 28 .. 28;
      UTMI_SUSPENDM        at 0 range 29 .. 29;
      CLKGATE              at 0 range 30 .. 30;
      SFTRST               at 0 range 31 .. 31;
   end record;

   --  Enables non-standard resistive plugged-in detection This bit field
   --  controls connection of nominal 200kohm resistors to both the USB_DP and
   --  USB_DM pins as one method of detecting when a USB cable is attached in
   --  device mode
   type CTRL_CLR_ENDEVPLUGINDET_Field is
     (
      --  Disables 200kohm pullup resistors on USB_DP and USB_DM pins (Default)
      VALUE0,
      --  Enables 200kohm pullup resistors on USB_DP and USB_DM pins
      VALUE1)
     with Size => 1;
   for CTRL_CLR_ENDEVPLUGINDET_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  CTRL_CLR_ENUTMILEVEL array
   type CTRL_CLR_ENUTMILEVEL_Field_Array is array (2 .. 3) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CTRL_CLR_ENUTMILEVEL
   type CTRL_CLR_ENUTMILEVEL_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  ENUTMILEVEL as a value
            Val : HAL.UInt2;
         when True =>
            --  ENUTMILEVEL as an array
            Arr : CTRL_CLR_ENUTMILEVEL_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CTRL_CLR_ENUTMILEVEL_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  USB PHY General Control Register
   type CTRL_CLR_Register is record
      --  unspecified
      Reserved_0_0         : HAL.Bit := 16#0#;
      --  For host mode, enables high-speed disconnect detector
      ENHOSTDISCONDETECT   : Boolean := False;
      --  Enable IRQ for Host disconnect: Enables interrupt for detection of
      --  disconnection to Device when in high-speed host mode
      ENIRQHOSTDISCON      : Boolean := False;
      --  Indicates that the device has disconnected in High-Speed mode
      HOSTDISCONDETECT_IRQ : Boolean := False;
      --  Enables non-standard resistive plugged-in detection This bit field
      --  controls connection of nominal 200kohm resistors to both the USB_DP
      --  and USB_DM pins as one method of detecting when a USB cable is
      --  attached in device mode
      ENDEVPLUGINDET       : CTRL_CLR_ENDEVPLUGINDET_Field :=
                              NXP_SVD.USBPHY.VALUE0;
      --  Device plugin polarity: For device mode, if this bit is cleared to 0,
      --  then it trips the interrupt if the device is plugged in
      DEVPLUGIN_POLARITY   : Boolean := False;
      --  unspecified
      Reserved_6_7         : HAL.UInt2 := 16#0#;
      --  Resume IRQ: Set to 1 will make RESUME_IRQ bit a sticky bit until
      --  software clear it
      RESUMEIRQSTICKY      : Boolean := False;
      --  Enable IRQ Resume detect: Enables interrupt for detection of a non-J
      --  state on the USB line
      ENIRQRESUMEDETECT    : Boolean := False;
      --  Resume IRQ: Indicates that the host is sending a wake-up after
      --  suspend
      RESUME_IRQ           : Boolean := False;
      --  unspecified
      Reserved_11_11       : HAL.Bit := 16#0#;
      --  Indicates that the device is connected
      DEVPLUGIN_IRQ        : Boolean := False;
      --  unspecified
      Reserved_13_13       : HAL.Bit := 16#0#;
      --  Enables UTMI+ Level 2 operation for the USB HS PHY
      ENUTMILEVEL          : CTRL_CLR_ENUTMILEVEL_Field :=
                              (As_Array => False, Val => 16#0#);
      --  Enable wake-up IRQ: Enables interrupt for the wake-up events.
      ENIRQWAKEUP          : Boolean := False;
      --  Wake-up IRQ: Indicates that there is a wak-eup event
      WAKEUP_IRQ           : Boolean := False;
      --  Enable the auto resume feature, when set, HW will use 32KHz clock to
      --  send Resume to respond to the device remote wakeup(for host mode
      --  only)
      AUTORESUME_EN        : Boolean := False;
      --  Enables the feature to auto-clear the CLKGATE bit if there is wakeup
      --  event while USB is suspended
      ENAUTOCLR_CLKGATE    : Boolean := False;
      --  Enables the feature to auto-clear the PWD register bits in PWD if
      --  there is wakeup event while USB is suspended
      ENAUTOCLR_PHY_PWD    : Boolean := False;
      --  Enable DP DM change wake-up: Not for customer use
      ENDPDMCHG_WKUP       : Boolean := False;
      --  unspecified
      Reserved_22_22       : HAL.Bit := 16#0#;
      --  Enable VBUS change wake-up: Enables the feature to wake-up USB if
      --  VBUS is toggled when USB is suspended
      ENVBUSCHG_WKUP       : Boolean := False;
      --  unspecified
      Reserved_24_24       : HAL.Bit := 16#0#;
      --  Enable auto-clear USB Clock gate: Enables the feature to auto-clear
      --  the USB0_CLKGATE/USB1_CLKGATE register bit in HW_DIGCTL_CTRL if there
      --  is wake-up event on USB0/USB1 while USB0/USB1 is suspended
      ENAUTOCLR_USBCLKGATE : Boolean := False;
      --  Enable auto-set of USB clocks: Enables the feature to auto-clear the
      --  EN_USB_CLKS register bits in HW_CLKCTRL_PLL1CTRL0/HW_CLKCTRL_P
      --  LL1CTRL1 if there is wake-up event on USB0/USB1 while USB0/USB1 is
      --  suspended
      ENAUTOSET_USBCLKS    : Boolean := False;
      --  unspecified
      Reserved_27_27       : HAL.Bit := 16#0#;
      --  Forces the next FS packet that is transmitted to have a EOP with
      --  low-speed timing
      HOST_FORCE_LS_SE0    : Boolean := False;
      --  Used by the PHY to indicate a powered-down state
      UTMI_SUSPENDM        : Boolean := False;
      --  Gate UTMI Clocks
      CLKGATE              : Boolean := True;
      --  Writing a 1 to this bit will soft-reset the PWD, TX, RX, and CTRL
      --  registers
      SFTRST               : Boolean := True;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CTRL_CLR_Register use record
      Reserved_0_0         at 0 range 0 .. 0;
      ENHOSTDISCONDETECT   at 0 range 1 .. 1;
      ENIRQHOSTDISCON      at 0 range 2 .. 2;
      HOSTDISCONDETECT_IRQ at 0 range 3 .. 3;
      ENDEVPLUGINDET       at 0 range 4 .. 4;
      DEVPLUGIN_POLARITY   at 0 range 5 .. 5;
      Reserved_6_7         at 0 range 6 .. 7;
      RESUMEIRQSTICKY      at 0 range 8 .. 8;
      ENIRQRESUMEDETECT    at 0 range 9 .. 9;
      RESUME_IRQ           at 0 range 10 .. 10;
      Reserved_11_11       at 0 range 11 .. 11;
      DEVPLUGIN_IRQ        at 0 range 12 .. 12;
      Reserved_13_13       at 0 range 13 .. 13;
      ENUTMILEVEL          at 0 range 14 .. 15;
      ENIRQWAKEUP          at 0 range 16 .. 16;
      WAKEUP_IRQ           at 0 range 17 .. 17;
      AUTORESUME_EN        at 0 range 18 .. 18;
      ENAUTOCLR_CLKGATE    at 0 range 19 .. 19;
      ENAUTOCLR_PHY_PWD    at 0 range 20 .. 20;
      ENDPDMCHG_WKUP       at 0 range 21 .. 21;
      Reserved_22_22       at 0 range 22 .. 22;
      ENVBUSCHG_WKUP       at 0 range 23 .. 23;
      Reserved_24_24       at 0 range 24 .. 24;
      ENAUTOCLR_USBCLKGATE at 0 range 25 .. 25;
      ENAUTOSET_USBCLKS    at 0 range 26 .. 26;
      Reserved_27_27       at 0 range 27 .. 27;
      HOST_FORCE_LS_SE0    at 0 range 28 .. 28;
      UTMI_SUSPENDM        at 0 range 29 .. 29;
      CLKGATE              at 0 range 30 .. 30;
      SFTRST               at 0 range 31 .. 31;
   end record;

   --  Enables non-standard resistive plugged-in detection This bit field
   --  controls connection of nominal 200kohm resistors to both the USB_DP and
   --  USB_DM pins as one method of detecting when a USB cable is attached in
   --  device mode
   type CTRL_TOG_ENDEVPLUGINDET_Field is
     (
      --  Disables 200kohm pullup resistors on USB_DP and USB_DM pins (Default)
      VALUE0,
      --  Enables 200kohm pullup resistors on USB_DP and USB_DM pins
      VALUE1)
     with Size => 1;
   for CTRL_TOG_ENDEVPLUGINDET_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  CTRL_TOG_ENUTMILEVEL array
   type CTRL_TOG_ENUTMILEVEL_Field_Array is array (2 .. 3) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CTRL_TOG_ENUTMILEVEL
   type CTRL_TOG_ENUTMILEVEL_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  ENUTMILEVEL as a value
            Val : HAL.UInt2;
         when True =>
            --  ENUTMILEVEL as an array
            Arr : CTRL_TOG_ENUTMILEVEL_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CTRL_TOG_ENUTMILEVEL_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  USB PHY General Control Register
   type CTRL_TOG_Register is record
      --  unspecified
      Reserved_0_0         : HAL.Bit := 16#0#;
      --  For host mode, enables high-speed disconnect detector
      ENHOSTDISCONDETECT   : Boolean := False;
      --  Enable IRQ for Host disconnect: Enables interrupt for detection of
      --  disconnection to Device when in high-speed host mode
      ENIRQHOSTDISCON      : Boolean := False;
      --  Indicates that the device has disconnected in High-Speed mode
      HOSTDISCONDETECT_IRQ : Boolean := False;
      --  Enables non-standard resistive plugged-in detection This bit field
      --  controls connection of nominal 200kohm resistors to both the USB_DP
      --  and USB_DM pins as one method of detecting when a USB cable is
      --  attached in device mode
      ENDEVPLUGINDET       : CTRL_TOG_ENDEVPLUGINDET_Field :=
                              NXP_SVD.USBPHY.VALUE0;
      --  Device plugin polarity: For device mode, if this bit is cleared to 0,
      --  then it trips the interrupt if the device is plugged in
      DEVPLUGIN_POLARITY   : Boolean := False;
      --  unspecified
      Reserved_6_7         : HAL.UInt2 := 16#0#;
      --  Resume IRQ: Set to 1 will make RESUME_IRQ bit a sticky bit until
      --  software clear it
      RESUMEIRQSTICKY      : Boolean := False;
      --  Enable IRQ Resume detect: Enables interrupt for detection of a non-J
      --  state on the USB line
      ENIRQRESUMEDETECT    : Boolean := False;
      --  Resume IRQ: Indicates that the host is sending a wake-up after
      --  suspend
      RESUME_IRQ           : Boolean := False;
      --  unspecified
      Reserved_11_11       : HAL.Bit := 16#0#;
      --  Indicates that the device is connected
      DEVPLUGIN_IRQ        : Boolean := False;
      --  unspecified
      Reserved_13_13       : HAL.Bit := 16#0#;
      --  Enables UTMI+ Level 2 operation for the USB HS PHY
      ENUTMILEVEL          : CTRL_TOG_ENUTMILEVEL_Field :=
                              (As_Array => False, Val => 16#0#);
      --  Enable wake-up IRQ: Enables interrupt for the wake-up events.
      ENIRQWAKEUP          : Boolean := False;
      --  Wake-up IRQ: Indicates that there is a wak-eup event
      WAKEUP_IRQ           : Boolean := False;
      --  Enable the auto resume feature, when set, HW will use 32KHz clock to
      --  send Resume to respond to the device remote wakeup(for host mode
      --  only)
      AUTORESUME_EN        : Boolean := False;
      --  Enables the feature to auto-clear the CLKGATE bit if there is wakeup
      --  event while USB is suspended
      ENAUTOCLR_CLKGATE    : Boolean := False;
      --  Enables the feature to auto-clear the PWD register bits in PWD if
      --  there is wakeup event while USB is suspended
      ENAUTOCLR_PHY_PWD    : Boolean := False;
      --  Enable DP DM change wake-up: Not for customer use
      ENDPDMCHG_WKUP       : Boolean := False;
      --  unspecified
      Reserved_22_22       : HAL.Bit := 16#0#;
      --  Enable VBUS change wake-up: Enables the feature to wake-up USB if
      --  VBUS is toggled when USB is suspended
      ENVBUSCHG_WKUP       : Boolean := False;
      --  unspecified
      Reserved_24_24       : HAL.Bit := 16#0#;
      --  Enable auto-clear USB Clock gate: Enables the feature to auto-clear
      --  the USB0_CLKGATE/USB1_CLKGATE register bit in HW_DIGCTL_CTRL if there
      --  is wake-up event on USB0/USB1 while USB0/USB1 is suspended
      ENAUTOCLR_USBCLKGATE : Boolean := False;
      --  Enable auto-set of USB clocks: Enables the feature to auto-clear the
      --  EN_USB_CLKS register bits in HW_CLKCTRL_PLL1CTRL0/HW_CLKCTRL_P
      --  LL1CTRL1 if there is wake-up event on USB0/USB1 while USB0/USB1 is
      --  suspended
      ENAUTOSET_USBCLKS    : Boolean := False;
      --  unspecified
      Reserved_27_27       : HAL.Bit := 16#0#;
      --  Forces the next FS packet that is transmitted to have a EOP with
      --  low-speed timing
      HOST_FORCE_LS_SE0    : Boolean := False;
      --  Used by the PHY to indicate a powered-down state
      UTMI_SUSPENDM        : Boolean := False;
      --  Gate UTMI Clocks
      CLKGATE              : Boolean := True;
      --  Writing a 1 to this bit will soft-reset the PWD, TX, RX, and CTRL
      --  registers
      SFTRST               : Boolean := True;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CTRL_TOG_Register use record
      Reserved_0_0         at 0 range 0 .. 0;
      ENHOSTDISCONDETECT   at 0 range 1 .. 1;
      ENIRQHOSTDISCON      at 0 range 2 .. 2;
      HOSTDISCONDETECT_IRQ at 0 range 3 .. 3;
      ENDEVPLUGINDET       at 0 range 4 .. 4;
      DEVPLUGIN_POLARITY   at 0 range 5 .. 5;
      Reserved_6_7         at 0 range 6 .. 7;
      RESUMEIRQSTICKY      at 0 range 8 .. 8;
      ENIRQRESUMEDETECT    at 0 range 9 .. 9;
      RESUME_IRQ           at 0 range 10 .. 10;
      Reserved_11_11       at 0 range 11 .. 11;
      DEVPLUGIN_IRQ        at 0 range 12 .. 12;
      Reserved_13_13       at 0 range 13 .. 13;
      ENUTMILEVEL          at 0 range 14 .. 15;
      ENIRQWAKEUP          at 0 range 16 .. 16;
      WAKEUP_IRQ           at 0 range 17 .. 17;
      AUTORESUME_EN        at 0 range 18 .. 18;
      ENAUTOCLR_CLKGATE    at 0 range 19 .. 19;
      ENAUTOCLR_PHY_PWD    at 0 range 20 .. 20;
      ENDPDMCHG_WKUP       at 0 range 21 .. 21;
      Reserved_22_22       at 0 range 22 .. 22;
      ENVBUSCHG_WKUP       at 0 range 23 .. 23;
      Reserved_24_24       at 0 range 24 .. 24;
      ENAUTOCLR_USBCLKGATE at 0 range 25 .. 25;
      ENAUTOSET_USBCLKS    at 0 range 26 .. 26;
      Reserved_27_27       at 0 range 27 .. 27;
      HOST_FORCE_LS_SE0    at 0 range 28 .. 28;
      UTMI_SUSPENDM        at 0 range 29 .. 29;
      CLKGATE              at 0 range 30 .. 30;
      SFTRST               at 0 range 31 .. 31;
   end record;

   --  Indicates at the local host (downstream) port that the remote device has
   --  disconnected while in High-Speed mode
   type STATUS_HOSTDISCONDETECT_STATUS_Field is
     (
      --  USB cable disconnect has not been detected at the local host
      VALUE0,
      --  USB cable disconnect has been detected at the local host
      VALUE1)
     with Size => 1;
   for STATUS_HOSTDISCONDETECT_STATUS_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  Status indicator for non-standard resistive plugged-in detection
   --  Indicates that the device has been connected on the USB_DP and USB_DM
   --  lines using the nonstandard resistive plugged-in detection method
   --  controlled by CTRL[4]
   type STATUS_DEVPLUGIN_STATUS_Field is
     (
      --  No attachment to a USB host is detected
      VALUE0,
      --  Cable attachment to a USB host is detected
      VALUE1)
     with Size => 1;
   for STATUS_DEVPLUGIN_STATUS_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  USB PHY Status Register
   type STATUS_Register is record
      --  Read-only. Indicates the USB 3v power rails are in range.
      OK_STATUS_3V            : Boolean;
      --  unspecified
      Reserved_1_2            : HAL.UInt2;
      --  Read-only. Indicates at the local host (downstream) port that the
      --  remote device has disconnected while in High-Speed mode
      HOSTDISCONDETECT_STATUS : STATUS_HOSTDISCONDETECT_STATUS_Field;
      --  unspecified
      Reserved_4_5            : HAL.UInt2;
      --  Read-only. Status indicator for non-standard resistive plugged-in
      --  detection Indicates that the device has been connected on the USB_DP
      --  and USB_DM lines using the nonstandard resistive plugged-in detection
      --  method controlled by CTRL[4]
      DEVPLUGIN_STATUS        : STATUS_DEVPLUGIN_STATUS_Field;
      --  unspecified
      Reserved_7_9            : HAL.UInt3;
      --  Read-only. Indicates that the host is sending a wake-up after Suspend
      --  and has triggered an interrupt.
      RESUME_STATUS           : Boolean;
      --  unspecified
      Reserved_11_31          : HAL.UInt21;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for STATUS_Register use record
      OK_STATUS_3V            at 0 range 0 .. 0;
      Reserved_1_2            at 0 range 1 .. 2;
      HOSTDISCONDETECT_STATUS at 0 range 3 .. 3;
      Reserved_4_5            at 0 range 4 .. 5;
      DEVPLUGIN_STATUS        at 0 range 6 .. 6;
      Reserved_7_9            at 0 range 7 .. 9;
      RESUME_STATUS           at 0 range 10 .. 10;
      Reserved_11_31          at 0 range 11 .. 31;
   end record;

   --  Reference bias power down select.
   type PLL_SIC_REFBIAS_PWD_SEL_Field is
     (
      --  Selects PLL_POWER to control the reference bias
      VALUE0,
      --  Selects REFBIAS_PWD to control the reference bias
      VALUE1)
     with Size => 1;
   for PLL_SIC_REFBIAS_PWD_SEL_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  This field controls the USB PLL feedback loop divider
   type PLL_SIC_PLL_DIV_SEL_Field is
     (
      --  Divide by 13
      VALUE0,
      --  Divide by 15
      VALUE1,
      --  Divide by 16
      VALUE2,
      --  Divide by 20
      VALUE3,
      --  Divide by 22
      VALUE4,
      --  Divide by 25
      VALUE5,
      --  Divide by 30
      VALUE6,
      --  Divide by 240
      VALUE7)
     with Size => 3;
   for PLL_SIC_PLL_DIV_SEL_Field use
     (VALUE0 => 0,
      VALUE1 => 1,
      VALUE2 => 2,
      VALUE3 => 3,
      VALUE4 => 4,
      VALUE5 => 5,
      VALUE6 => 6,
      VALUE7 => 7);

   --  USB PLL lock status indicator
   type PLL_SIC_PLL_LOCK_Field is
     (
      --  PLL is not currently locked
      VALUE0,
      --  PLL is currently locked
      VALUE1)
     with Size => 1;
   for PLL_SIC_PLL_LOCK_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  USB PHY PLL Control/Status Register
   type PLL_SIC_Register is record
      --  unspecified
      Reserved_0_5    : HAL.UInt6 := 16#0#;
      --  Enables the USB clock from PLL to USB PHY
      PLL_EN_USB_CLKS : Boolean := False;
      --  unspecified
      Reserved_7_11   : HAL.UInt5 := 16#0#;
      --  Power up the USB PLL
      PLL_POWER       : Boolean := False;
      --  Enables the clock output from the USB PLL
      PLL_ENABLE      : Boolean := True;
      --  unspecified
      Reserved_14_18  : HAL.UInt5 := 16#4#;
      --  Reference bias power down select.
      REFBIAS_PWD_SEL : PLL_SIC_REFBIAS_PWD_SEL_Field :=
                         NXP_SVD.USBPHY.VALUE0;
      --  Power down the reference bias This bit is only used when
      --  REFBIAS_PWD_SEL is set to 1.
      REFBIAS_PWD     : Boolean := True;
      --  This field controls the USB PLL regulator, set to enable the
      --  regulator
      PLL_REG_ENABLE  : Boolean := False;
      --  This field controls the USB PLL feedback loop divider
      PLL_DIV_SEL     : PLL_SIC_PLL_DIV_SEL_Field := NXP_SVD.USBPHY.VALUE3;
      --  unspecified
      Reserved_25_29  : HAL.UInt5 := 16#0#;
      --  This is selection between /1 or /2 to expand the range of ref input
      --  clock.
      PLL_PREDIV      : Boolean := False;
      --  Read-only. USB PLL lock status indicator
      PLL_LOCK        : PLL_SIC_PLL_LOCK_Field := NXP_SVD.USBPHY.VALUE0;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PLL_SIC_Register use record
      Reserved_0_5    at 0 range 0 .. 5;
      PLL_EN_USB_CLKS at 0 range 6 .. 6;
      Reserved_7_11   at 0 range 7 .. 11;
      PLL_POWER       at 0 range 12 .. 12;
      PLL_ENABLE      at 0 range 13 .. 13;
      Reserved_14_18  at 0 range 14 .. 18;
      REFBIAS_PWD_SEL at 0 range 19 .. 19;
      REFBIAS_PWD     at 0 range 20 .. 20;
      PLL_REG_ENABLE  at 0 range 21 .. 21;
      PLL_DIV_SEL     at 0 range 22 .. 24;
      Reserved_25_29  at 0 range 25 .. 29;
      PLL_PREDIV      at 0 range 30 .. 30;
      PLL_LOCK        at 0 range 31 .. 31;
   end record;

   --  Reference bias power down select.
   type PLL_SIC_SET_REFBIAS_PWD_SEL_Field is
     (
      --  Selects PLL_POWER to control the reference bias
      VALUE0,
      --  Selects REFBIAS_PWD to control the reference bias
      VALUE1)
     with Size => 1;
   for PLL_SIC_SET_REFBIAS_PWD_SEL_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  This field controls the USB PLL feedback loop divider
   type PLL_SIC_SET_PLL_DIV_SEL_Field is
     (
      --  Divide by 13
      VALUE0,
      --  Divide by 15
      VALUE1,
      --  Divide by 16
      VALUE2,
      --  Divide by 20
      VALUE3,
      --  Divide by 22
      VALUE4,
      --  Divide by 25
      VALUE5,
      --  Divide by 30
      VALUE6,
      --  Divide by 240
      VALUE7)
     with Size => 3;
   for PLL_SIC_SET_PLL_DIV_SEL_Field use
     (VALUE0 => 0,
      VALUE1 => 1,
      VALUE2 => 2,
      VALUE3 => 3,
      VALUE4 => 4,
      VALUE5 => 5,
      VALUE6 => 6,
      VALUE7 => 7);

   --  USB PLL lock status indicator
   type PLL_SIC_SET_PLL_LOCK_Field is
     (
      --  PLL is not currently locked
      VALUE0,
      --  PLL is currently locked
      VALUE1)
     with Size => 1;
   for PLL_SIC_SET_PLL_LOCK_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  USB PHY PLL Control/Status Register
   type PLL_SIC_SET_Register is record
      --  unspecified
      Reserved_0_5    : HAL.UInt6 := 16#0#;
      --  Enables the USB clock from PLL to USB PHY
      PLL_EN_USB_CLKS : Boolean := False;
      --  unspecified
      Reserved_7_11   : HAL.UInt5 := 16#0#;
      --  Power up the USB PLL
      PLL_POWER       : Boolean := False;
      --  Enables the clock output from the USB PLL
      PLL_ENABLE      : Boolean := True;
      --  unspecified
      Reserved_14_18  : HAL.UInt5 := 16#4#;
      --  Reference bias power down select.
      REFBIAS_PWD_SEL : PLL_SIC_SET_REFBIAS_PWD_SEL_Field :=
                         NXP_SVD.USBPHY.VALUE0;
      --  Power down the reference bias This bit is only used when
      --  REFBIAS_PWD_SEL is set to 1.
      REFBIAS_PWD     : Boolean := True;
      --  This field controls the USB PLL regulator, set to enable the
      --  regulator
      PLL_REG_ENABLE  : Boolean := False;
      --  This field controls the USB PLL feedback loop divider
      PLL_DIV_SEL     : PLL_SIC_SET_PLL_DIV_SEL_Field :=
                         NXP_SVD.USBPHY.VALUE3;
      --  unspecified
      Reserved_25_29  : HAL.UInt5 := 16#0#;
      --  This is selection between /1 or /2 to expand the range of ref input
      --  clock.
      PLL_PREDIV      : Boolean := False;
      --  Read-only. USB PLL lock status indicator
      PLL_LOCK        : PLL_SIC_SET_PLL_LOCK_Field := NXP_SVD.USBPHY.VALUE0;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PLL_SIC_SET_Register use record
      Reserved_0_5    at 0 range 0 .. 5;
      PLL_EN_USB_CLKS at 0 range 6 .. 6;
      Reserved_7_11   at 0 range 7 .. 11;
      PLL_POWER       at 0 range 12 .. 12;
      PLL_ENABLE      at 0 range 13 .. 13;
      Reserved_14_18  at 0 range 14 .. 18;
      REFBIAS_PWD_SEL at 0 range 19 .. 19;
      REFBIAS_PWD     at 0 range 20 .. 20;
      PLL_REG_ENABLE  at 0 range 21 .. 21;
      PLL_DIV_SEL     at 0 range 22 .. 24;
      Reserved_25_29  at 0 range 25 .. 29;
      PLL_PREDIV      at 0 range 30 .. 30;
      PLL_LOCK        at 0 range 31 .. 31;
   end record;

   --  Reference bias power down select.
   type PLL_SIC_CLR_REFBIAS_PWD_SEL_Field is
     (
      --  Selects PLL_POWER to control the reference bias
      VALUE0,
      --  Selects REFBIAS_PWD to control the reference bias
      VALUE1)
     with Size => 1;
   for PLL_SIC_CLR_REFBIAS_PWD_SEL_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  This field controls the USB PLL feedback loop divider
   type PLL_SIC_CLR_PLL_DIV_SEL_Field is
     (
      --  Divide by 13
      VALUE0,
      --  Divide by 15
      VALUE1,
      --  Divide by 16
      VALUE2,
      --  Divide by 20
      VALUE3,
      --  Divide by 22
      VALUE4,
      --  Divide by 25
      VALUE5,
      --  Divide by 30
      VALUE6,
      --  Divide by 240
      VALUE7)
     with Size => 3;
   for PLL_SIC_CLR_PLL_DIV_SEL_Field use
     (VALUE0 => 0,
      VALUE1 => 1,
      VALUE2 => 2,
      VALUE3 => 3,
      VALUE4 => 4,
      VALUE5 => 5,
      VALUE6 => 6,
      VALUE7 => 7);

   --  USB PLL lock status indicator
   type PLL_SIC_CLR_PLL_LOCK_Field is
     (
      --  PLL is not currently locked
      VALUE0,
      --  PLL is currently locked
      VALUE1)
     with Size => 1;
   for PLL_SIC_CLR_PLL_LOCK_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  USB PHY PLL Control/Status Register
   type PLL_SIC_CLR_Register is record
      --  unspecified
      Reserved_0_5    : HAL.UInt6 := 16#0#;
      --  Enables the USB clock from PLL to USB PHY
      PLL_EN_USB_CLKS : Boolean := False;
      --  unspecified
      Reserved_7_11   : HAL.UInt5 := 16#0#;
      --  Power up the USB PLL
      PLL_POWER       : Boolean := False;
      --  Enables the clock output from the USB PLL
      PLL_ENABLE      : Boolean := True;
      --  unspecified
      Reserved_14_18  : HAL.UInt5 := 16#4#;
      --  Reference bias power down select.
      REFBIAS_PWD_SEL : PLL_SIC_CLR_REFBIAS_PWD_SEL_Field :=
                         NXP_SVD.USBPHY.VALUE0;
      --  Power down the reference bias This bit is only used when
      --  REFBIAS_PWD_SEL is set to 1.
      REFBIAS_PWD     : Boolean := True;
      --  This field controls the USB PLL regulator, set to enable the
      --  regulator
      PLL_REG_ENABLE  : Boolean := False;
      --  This field controls the USB PLL feedback loop divider
      PLL_DIV_SEL     : PLL_SIC_CLR_PLL_DIV_SEL_Field :=
                         NXP_SVD.USBPHY.VALUE3;
      --  unspecified
      Reserved_25_29  : HAL.UInt5 := 16#0#;
      --  This is selection between /1 or /2 to expand the range of ref input
      --  clock.
      PLL_PREDIV      : Boolean := False;
      --  Read-only. USB PLL lock status indicator
      PLL_LOCK        : PLL_SIC_CLR_PLL_LOCK_Field := NXP_SVD.USBPHY.VALUE0;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PLL_SIC_CLR_Register use record
      Reserved_0_5    at 0 range 0 .. 5;
      PLL_EN_USB_CLKS at 0 range 6 .. 6;
      Reserved_7_11   at 0 range 7 .. 11;
      PLL_POWER       at 0 range 12 .. 12;
      PLL_ENABLE      at 0 range 13 .. 13;
      Reserved_14_18  at 0 range 14 .. 18;
      REFBIAS_PWD_SEL at 0 range 19 .. 19;
      REFBIAS_PWD     at 0 range 20 .. 20;
      PLL_REG_ENABLE  at 0 range 21 .. 21;
      PLL_DIV_SEL     at 0 range 22 .. 24;
      Reserved_25_29  at 0 range 25 .. 29;
      PLL_PREDIV      at 0 range 30 .. 30;
      PLL_LOCK        at 0 range 31 .. 31;
   end record;

   --  Reference bias power down select.
   type PLL_SIC_TOG_REFBIAS_PWD_SEL_Field is
     (
      --  Selects PLL_POWER to control the reference bias
      VALUE0,
      --  Selects REFBIAS_PWD to control the reference bias
      VALUE1)
     with Size => 1;
   for PLL_SIC_TOG_REFBIAS_PWD_SEL_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  This field controls the USB PLL feedback loop divider
   type PLL_SIC_TOG_PLL_DIV_SEL_Field is
     (
      --  Divide by 13
      VALUE0,
      --  Divide by 15
      VALUE1,
      --  Divide by 16
      VALUE2,
      --  Divide by 20
      VALUE3,
      --  Divide by 22
      VALUE4,
      --  Divide by 25
      VALUE5,
      --  Divide by 30
      VALUE6,
      --  Divide by 240
      VALUE7)
     with Size => 3;
   for PLL_SIC_TOG_PLL_DIV_SEL_Field use
     (VALUE0 => 0,
      VALUE1 => 1,
      VALUE2 => 2,
      VALUE3 => 3,
      VALUE4 => 4,
      VALUE5 => 5,
      VALUE6 => 6,
      VALUE7 => 7);

   --  USB PLL lock status indicator
   type PLL_SIC_TOG_PLL_LOCK_Field is
     (
      --  PLL is not currently locked
      VALUE0,
      --  PLL is currently locked
      VALUE1)
     with Size => 1;
   for PLL_SIC_TOG_PLL_LOCK_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  USB PHY PLL Control/Status Register
   type PLL_SIC_TOG_Register is record
      --  unspecified
      Reserved_0_5    : HAL.UInt6 := 16#0#;
      --  Enables the USB clock from PLL to USB PHY
      PLL_EN_USB_CLKS : Boolean := False;
      --  unspecified
      Reserved_7_11   : HAL.UInt5 := 16#0#;
      --  Power up the USB PLL
      PLL_POWER       : Boolean := False;
      --  Enables the clock output from the USB PLL
      PLL_ENABLE      : Boolean := True;
      --  unspecified
      Reserved_14_18  : HAL.UInt5 := 16#4#;
      --  Reference bias power down select.
      REFBIAS_PWD_SEL : PLL_SIC_TOG_REFBIAS_PWD_SEL_Field :=
                         NXP_SVD.USBPHY.VALUE0;
      --  Power down the reference bias This bit is only used when
      --  REFBIAS_PWD_SEL is set to 1.
      REFBIAS_PWD     : Boolean := True;
      --  This field controls the USB PLL regulator, set to enable the
      --  regulator
      PLL_REG_ENABLE  : Boolean := False;
      --  This field controls the USB PLL feedback loop divider
      PLL_DIV_SEL     : PLL_SIC_TOG_PLL_DIV_SEL_Field :=
                         NXP_SVD.USBPHY.VALUE3;
      --  unspecified
      Reserved_25_29  : HAL.UInt5 := 16#0#;
      --  This is selection between /1 or /2 to expand the range of ref input
      --  clock.
      PLL_PREDIV      : Boolean := False;
      --  Read-only. USB PLL lock status indicator
      PLL_LOCK        : PLL_SIC_TOG_PLL_LOCK_Field := NXP_SVD.USBPHY.VALUE0;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PLL_SIC_TOG_Register use record
      Reserved_0_5    at 0 range 0 .. 5;
      PLL_EN_USB_CLKS at 0 range 6 .. 6;
      Reserved_7_11   at 0 range 7 .. 11;
      PLL_POWER       at 0 range 12 .. 12;
      PLL_ENABLE      at 0 range 13 .. 13;
      Reserved_14_18  at 0 range 14 .. 18;
      REFBIAS_PWD_SEL at 0 range 19 .. 19;
      REFBIAS_PWD     at 0 range 20 .. 20;
      PLL_REG_ENABLE  at 0 range 21 .. 21;
      PLL_DIV_SEL     at 0 range 22 .. 24;
      Reserved_25_29  at 0 range 25 .. 29;
      PLL_PREDIV      at 0 range 30 .. 30;
      PLL_LOCK        at 0 range 31 .. 31;
   end record;

   --  Sets the threshold for the VBUSVALID comparator
   type USB1_VBUS_DETECT_VBUSVALID_THRESH_Field is
     (
      --  4.0V
      VALUE0,
      --  4.1V
      VALUE1,
      --  4.2V
      VALUE2,
      --  4.3V
      VALUE3,
      --  4.4V(Default)
      VALUE4,
      --  4.5V
      VALUE5,
      --  4.6V
      VALUE6,
      --  4.7V
      VALUE7)
     with Size => 3;
   for USB1_VBUS_DETECT_VBUSVALID_THRESH_Field use
     (VALUE0 => 0,
      VALUE1 => 1,
      VALUE2 => 2,
      VALUE3 => 3,
      VALUE4 => 4,
      VALUE5 => 5,
      VALUE6 => 6,
      VALUE7 => 7);

   --  VBUS detect signal override enable
   type USB1_VBUS_DETECT_VBUS_OVERRIDE_EN_Field is
     (
      --  Use the results of the internal VBUS_VALID and Session Valid
      --  comparators for VBUS_VALID, AVALID, BVALID, and SESSEND (Default)
      VALUE0,
      --  Use the override values for VBUS_VALID, AVALID, BVALID, and SESSEND
      VALUE1)
     with Size => 1;
   for USB1_VBUS_DETECT_VBUS_OVERRIDE_EN_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  Selects the source of the VBUS_VALID signal reported to the USB
   --  controller This is one of the bit fields that selects the source of the
   --  VBUS_VALID signal reported to the USB controller
   type USB1_VBUS_DETECT_VBUSVALID_SEL_Field is
     (
      --  Use the VBUS_VALID comparator results for signal reported to the USB
      --  controller (Default)
      VALUE0,
      --  Use the VBUS_VALID_3V detector results for signal reported to the USB
      --  controller
      VALUE1)
     with Size => 1;
   for USB1_VBUS_DETECT_VBUSVALID_SEL_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  Selects the source of the VBUS_VALID signal reported to the USB
   --  controller This is one of the bit fields that selects the source of the
   --  VBUS_VALID signal reported to the USB controller
   type USB1_VBUS_DETECT_VBUS_SOURCE_SEL_Field is
     (
      --  Use the VBUS_VALID comparator results for signal reported to the USB
      --  controller (Default)
      VALUE0,
      --  Use the Session Valid comparator results for signal reported to the
      --  USB controller
      VALUE1,
      --  Use the Session Valid comparator results for signal reported to the
      --  USB controller
      VALUE2)
     with Size => 2;
   for USB1_VBUS_DETECT_VBUS_SOURCE_SEL_Field use
     (VALUE0 => 0,
      VALUE1 => 1,
      VALUE2 => 2);

   --  Enable ID override using the pinmuxed value:
   type USB1_VBUS_DETECT_EXT_ID_OVERRIDE_EN_Field is
     (
      --  Select the Muxed value chosen using ID_OVERRIDE_EN.
      VALUE0,
      --  Select the external ID value.
      VALUE1)
     with Size => 1;
   for USB1_VBUS_DETECT_EXT_ID_OVERRIDE_EN_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  Enable VBUS override using the pinmuxed value.
   type USB1_VBUS_DETECT_EXT_VBUS_OVERRIDE_EN_Field is
     (
      --  Select the Muxed value chosen using VBUS_OVERRIDE_EN.
      VALUE0,
      --  Select the external VBUS VALID value.
      VALUE1)
     with Size => 1;
   for USB1_VBUS_DETECT_EXT_VBUS_OVERRIDE_EN_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  Selects the comparator used for VBUS_VALID This bit field controls the
   --  comparator used to report the VBUS_VALID results in USB1_VBUS_DETECT[3]
   --  between the VBUS_VALID comparator and the Session Valid comparator
   type USB1_VBUS_DETECT_VBUSVALID_TO_SESSVALID_Field is
     (
      --  Use the VBUS_VALID comparator for VBUS_VALID results
      VALUE0,
      --  Use the Session End comparator for VBUS_VALID results. The Session
      --  End threshold is >0.8V and <4.0V.
      VALUE1)
     with Size => 1;
   for USB1_VBUS_DETECT_VBUSVALID_TO_SESSVALID_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  Enables the VBUS_VALID comparator: Powers up the comparator used for the
   --  VBUS_VALID detector
   type USB1_VBUS_DETECT_PWRUP_CMPS_Field is
     (
      --  Powers down the VBUS_VALID comparator
      VALUE0,
      --  Enables the VBUS_VALID comparator (default)
      VALUE1)
     with Size => 3;
   for USB1_VBUS_DETECT_PWRUP_CMPS_Field use
     (VALUE0 => 0,
      VALUE1 => 7);

   --  Controls VBUS discharge resistor This bit field controls a nominal
   --  22kohm resistor between the USB1_VBUS pin and ground
   type USB1_VBUS_DETECT_DISCHARGE_VBUS_Field is
     (
      --  VBUS discharge resistor is disabled (Default)
      VALUE0,
      --  VBUS discharge resistor is enabled
      VALUE1)
     with Size => 1;
   for USB1_VBUS_DETECT_DISCHARGE_VBUS_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  USB PHY VBUS Detect Control Register
   type USB1_VBUS_DETECT_Register is record
      --  Sets the threshold for the VBUSVALID comparator
      VBUSVALID_THRESH       : USB1_VBUS_DETECT_VBUSVALID_THRESH_Field :=
                                NXP_SVD.USBPHY.VALUE4;
      --  VBUS detect signal override enable
      VBUS_OVERRIDE_EN       : USB1_VBUS_DETECT_VBUS_OVERRIDE_EN_Field :=
                                NXP_SVD.USBPHY.VALUE0;
      --  Override value for SESSEND The bit field provides the value for
      --  USB1_VBUS_DET_STAT[0] if USB_VBUS_DETECT[3] is set to value 1'b1
      SESSEND_OVERRIDE       : Boolean := False;
      --  Override value for B-Device Session Valid The bit field provides the
      --  value for USB1_VBUS_DET_STAT[1] if USB_VBUS_DETECT[3] is set to value
      --  1'b1
      BVALID_OVERRIDE        : Boolean := False;
      --  Override value for A-Device Session Valid The bit field provides the
      --  value for USB1_VBUS_DET_STAT[2] if USB_VBUS_DETECT[3] is set to value
      --  1'b1
      AVALID_OVERRIDE        : Boolean := False;
      --  Override value for VBUS_VALID signal sent to USB controller The bit
      --  field provides the value for VBUS_VALID reported to the USB
      --  controller if the value of USB1_VBUS_DETECT[3] is set to 1'b1
      VBUSVALID_OVERRIDE     : Boolean := False;
      --  Selects the source of the VBUS_VALID signal reported to the USB
      --  controller This is one of the bit fields that selects the source of
      --  the VBUS_VALID signal reported to the USB controller
      VBUSVALID_SEL          : USB1_VBUS_DETECT_VBUSVALID_SEL_Field :=
                                NXP_SVD.USBPHY.VALUE0;
      --  Selects the source of the VBUS_VALID signal reported to the USB
      --  controller This is one of the bit fields that selects the source of
      --  the VBUS_VALID signal reported to the USB controller
      VBUS_SOURCE_SEL        : USB1_VBUS_DETECT_VBUS_SOURCE_SEL_Field :=
                                NXP_SVD.USBPHY.VALUE0;
      --  Enable ID override using the register field. This bit is only used if
      --  EXT_ID_OVERRIDE_EN = 1'b0.
      ID_OVERRIDE_EN         : Boolean := False;
      --  ID override value.
      ID_OVERRIDE            : Boolean := False;
      --  Enable ID override using the pinmuxed value:
      EXT_ID_OVERRIDE_EN     : USB1_VBUS_DETECT_EXT_ID_OVERRIDE_EN_Field :=
                                NXP_SVD.USBPHY.VALUE0;
      --  Enable VBUS override using the pinmuxed value.
      EXT_VBUS_OVERRIDE_EN   : USB1_VBUS_DETECT_EXT_VBUS_OVERRIDE_EN_Field :=
                                NXP_SVD.USBPHY.VALUE0;
      --  unspecified
      Reserved_15_17         : HAL.UInt3 := 16#0#;
      --  Selects the comparator used for VBUS_VALID This bit field controls
      --  the comparator used to report the VBUS_VALID results in
      --  USB1_VBUS_DETECT[3] between the VBUS_VALID comparator and the Session
      --  Valid comparator
      VBUSVALID_TO_SESSVALID : USB1_VBUS_DETECT_VBUSVALID_TO_SESSVALID_Field :=
                                NXP_SVD.USBPHY.VALUE0;
      --  no description available
      VBUSVALID_5VDETECT     : Boolean := False;
      --  Enables the VBUS_VALID comparator: Powers up the comparator used for
      --  the VBUS_VALID detector
      PWRUP_CMPS             : USB1_VBUS_DETECT_PWRUP_CMPS_Field :=
                                NXP_SVD.USBPHY.VALUE1;
      --  unspecified
      Reserved_23_25         : HAL.UInt3 := 16#0#;
      --  Controls VBUS discharge resistor This bit field controls a nominal
      --  22kohm resistor between the USB1_VBUS pin and ground
      DISCHARGE_VBUS         : USB1_VBUS_DETECT_DISCHARGE_VBUS_Field :=
                                NXP_SVD.USBPHY.VALUE0;
      --  unspecified
      Reserved_27_31         : HAL.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for USB1_VBUS_DETECT_Register use record
      VBUSVALID_THRESH       at 0 range 0 .. 2;
      VBUS_OVERRIDE_EN       at 0 range 3 .. 3;
      SESSEND_OVERRIDE       at 0 range 4 .. 4;
      BVALID_OVERRIDE        at 0 range 5 .. 5;
      AVALID_OVERRIDE        at 0 range 6 .. 6;
      VBUSVALID_OVERRIDE     at 0 range 7 .. 7;
      VBUSVALID_SEL          at 0 range 8 .. 8;
      VBUS_SOURCE_SEL        at 0 range 9 .. 10;
      ID_OVERRIDE_EN         at 0 range 11 .. 11;
      ID_OVERRIDE            at 0 range 12 .. 12;
      EXT_ID_OVERRIDE_EN     at 0 range 13 .. 13;
      EXT_VBUS_OVERRIDE_EN   at 0 range 14 .. 14;
      Reserved_15_17         at 0 range 15 .. 17;
      VBUSVALID_TO_SESSVALID at 0 range 18 .. 18;
      VBUSVALID_5VDETECT     at 0 range 19 .. 19;
      PWRUP_CMPS             at 0 range 20 .. 22;
      Reserved_23_25         at 0 range 23 .. 25;
      DISCHARGE_VBUS         at 0 range 26 .. 26;
      Reserved_27_31         at 0 range 27 .. 31;
   end record;

   --  Sets the threshold for the VBUSVALID comparator
   type USB1_VBUS_DETECT_SET_VBUSVALID_THRESH_Field is
     (
      --  4.0V
      VALUE0,
      --  4.1V
      VALUE1,
      --  4.2V
      VALUE2,
      --  4.3V
      VALUE3,
      --  4.4V(Default)
      VALUE4,
      --  4.5V
      VALUE5,
      --  4.6V
      VALUE6,
      --  4.7V
      VALUE7)
     with Size => 3;
   for USB1_VBUS_DETECT_SET_VBUSVALID_THRESH_Field use
     (VALUE0 => 0,
      VALUE1 => 1,
      VALUE2 => 2,
      VALUE3 => 3,
      VALUE4 => 4,
      VALUE5 => 5,
      VALUE6 => 6,
      VALUE7 => 7);

   --  VBUS detect signal override enable
   type USB1_VBUS_DETECT_SET_VBUS_OVERRIDE_EN_Field is
     (
      --  Use the results of the internal VBUS_VALID and Session Valid
      --  comparators for VBUS_VALID, AVALID, BVALID, and SESSEND (Default)
      VALUE0,
      --  Use the override values for VBUS_VALID, AVALID, BVALID, and SESSEND
      VALUE1)
     with Size => 1;
   for USB1_VBUS_DETECT_SET_VBUS_OVERRIDE_EN_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  Selects the source of the VBUS_VALID signal reported to the USB
   --  controller This is one of the bit fields that selects the source of the
   --  VBUS_VALID signal reported to the USB controller
   type USB1_VBUS_DETECT_SET_VBUSVALID_SEL_Field is
     (
      --  Use the VBUS_VALID comparator results for signal reported to the USB
      --  controller (Default)
      VALUE0,
      --  Use the VBUS_VALID_3V detector results for signal reported to the USB
      --  controller
      VALUE1)
     with Size => 1;
   for USB1_VBUS_DETECT_SET_VBUSVALID_SEL_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  Selects the source of the VBUS_VALID signal reported to the USB
   --  controller This is one of the bit fields that selects the source of the
   --  VBUS_VALID signal reported to the USB controller
   type USB1_VBUS_DETECT_SET_VBUS_SOURCE_SEL_Field is
     (
      --  Use the VBUS_VALID comparator results for signal reported to the USB
      --  controller (Default)
      VALUE0,
      --  Use the Session Valid comparator results for signal reported to the
      --  USB controller
      VALUE1,
      --  Use the Session Valid comparator results for signal reported to the
      --  USB controller
      VALUE2)
     with Size => 2;
   for USB1_VBUS_DETECT_SET_VBUS_SOURCE_SEL_Field use
     (VALUE0 => 0,
      VALUE1 => 1,
      VALUE2 => 2);

   --  Enable ID override using the pinmuxed value:
   type USB1_VBUS_DETECT_SET_EXT_ID_OVERRIDE_EN_Field is
     (
      --  Select the Muxed value chosen using ID_OVERRIDE_EN.
      VALUE0,
      --  Select the external ID value.
      VALUE1)
     with Size => 1;
   for USB1_VBUS_DETECT_SET_EXT_ID_OVERRIDE_EN_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  Enable VBUS override using the pinmuxed value.
   type USB1_VBUS_DETECT_SET_EXT_VBUS_OVERRIDE_EN_Field is
     (
      --  Select the Muxed value chosen using VBUS_OVERRIDE_EN.
      VALUE0,
      --  Select the external VBUS VALID value.
      VALUE1)
     with Size => 1;
   for USB1_VBUS_DETECT_SET_EXT_VBUS_OVERRIDE_EN_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  Selects the comparator used for VBUS_VALID This bit field controls the
   --  comparator used to report the VBUS_VALID results in USB1_VBUS_DETECT[3]
   --  between the VBUS_VALID comparator and the Session Valid comparator
   type USB1_VBUS_DETECT_SET_VBUSVALID_TO_SESSVALID_Field is
     (
      --  Use the VBUS_VALID comparator for VBUS_VALID results
      VALUE0,
      --  Use the Session End comparator for VBUS_VALID results. The Session
      --  End threshold is >0.8V and <4.0V.
      VALUE1)
     with Size => 1;
   for USB1_VBUS_DETECT_SET_VBUSVALID_TO_SESSVALID_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  Enables the VBUS_VALID comparator: Powers up the comparator used for the
   --  VBUS_VALID detector
   type USB1_VBUS_DETECT_SET_PWRUP_CMPS_Field is
     (
      --  Powers down the VBUS_VALID comparator
      VALUE0,
      --  Enables the VBUS_VALID comparator (default)
      VALUE1)
     with Size => 3;
   for USB1_VBUS_DETECT_SET_PWRUP_CMPS_Field use
     (VALUE0 => 0,
      VALUE1 => 7);

   --  Controls VBUS discharge resistor This bit field controls a nominal
   --  22kohm resistor between the USB1_VBUS pin and ground
   type USB1_VBUS_DETECT_SET_DISCHARGE_VBUS_Field is
     (
      --  VBUS discharge resistor is disabled (Default)
      VALUE0,
      --  VBUS discharge resistor is enabled
      VALUE1)
     with Size => 1;
   for USB1_VBUS_DETECT_SET_DISCHARGE_VBUS_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  USB PHY VBUS Detect Control Register
   type USB1_VBUS_DETECT_SET_Register is record
      --  Sets the threshold for the VBUSVALID comparator
      VBUSVALID_THRESH       : USB1_VBUS_DETECT_SET_VBUSVALID_THRESH_Field :=
                                NXP_SVD.USBPHY.VALUE4;
      --  VBUS detect signal override enable
      VBUS_OVERRIDE_EN       : USB1_VBUS_DETECT_SET_VBUS_OVERRIDE_EN_Field :=
                                NXP_SVD.USBPHY.VALUE0;
      --  Override value for SESSEND The bit field provides the value for
      --  USB1_VBUS_DET_STAT[0] if USB_VBUS_DETECT[3] is set to value 1'b1
      SESSEND_OVERRIDE       : Boolean := False;
      --  Override value for B-Device Session Valid The bit field provides the
      --  value for USB1_VBUS_DET_STAT[1] if USB_VBUS_DETECT[3] is set to value
      --  1'b1
      BVALID_OVERRIDE        : Boolean := False;
      --  Override value for A-Device Session Valid The bit field provides the
      --  value for USB1_VBUS_DET_STAT[2] if USB_VBUS_DETECT[3] is set to value
      --  1'b1
      AVALID_OVERRIDE        : Boolean := False;
      --  Override value for VBUS_VALID signal sent to USB controller The bit
      --  field provides the value for VBUS_VALID reported to the USB
      --  controller if the value of USB1_VBUS_DETECT[3] is set to 1'b1
      VBUSVALID_OVERRIDE     : Boolean := False;
      --  Selects the source of the VBUS_VALID signal reported to the USB
      --  controller This is one of the bit fields that selects the source of
      --  the VBUS_VALID signal reported to the USB controller
      VBUSVALID_SEL          : USB1_VBUS_DETECT_SET_VBUSVALID_SEL_Field :=
                                NXP_SVD.USBPHY.VALUE0;
      --  Selects the source of the VBUS_VALID signal reported to the USB
      --  controller This is one of the bit fields that selects the source of
      --  the VBUS_VALID signal reported to the USB controller
      VBUS_SOURCE_SEL        : USB1_VBUS_DETECT_SET_VBUS_SOURCE_SEL_Field :=
                                NXP_SVD.USBPHY.VALUE0;
      --  Enable ID override using the register field. This bit is only used if
      --  EXT_ID_OVERRIDE_EN = 1'b0.
      ID_OVERRIDE_EN         : Boolean := False;
      --  ID override value.
      ID_OVERRIDE            : Boolean := False;
      --  Enable ID override using the pinmuxed value:
      EXT_ID_OVERRIDE_EN     : USB1_VBUS_DETECT_SET_EXT_ID_OVERRIDE_EN_Field :=
                                NXP_SVD.USBPHY.VALUE0;
      --  Enable VBUS override using the pinmuxed value.
      EXT_VBUS_OVERRIDE_EN   : USB1_VBUS_DETECT_SET_EXT_VBUS_OVERRIDE_EN_Field :=
                                NXP_SVD.USBPHY.VALUE0;
      --  unspecified
      Reserved_15_17         : HAL.UInt3 := 16#0#;
      --  Selects the comparator used for VBUS_VALID This bit field controls
      --  the comparator used to report the VBUS_VALID results in
      --  USB1_VBUS_DETECT[3] between the VBUS_VALID comparator and the Session
      --  Valid comparator
      VBUSVALID_TO_SESSVALID : USB1_VBUS_DETECT_SET_VBUSVALID_TO_SESSVALID_Field :=
                                NXP_SVD.USBPHY.VALUE0;
      --  no description available
      VBUSVALID_5VDETECT     : Boolean := False;
      --  Enables the VBUS_VALID comparator: Powers up the comparator used for
      --  the VBUS_VALID detector
      PWRUP_CMPS             : USB1_VBUS_DETECT_SET_PWRUP_CMPS_Field :=
                                NXP_SVD.USBPHY.VALUE1;
      --  unspecified
      Reserved_23_25         : HAL.UInt3 := 16#0#;
      --  Controls VBUS discharge resistor This bit field controls a nominal
      --  22kohm resistor between the USB1_VBUS pin and ground
      DISCHARGE_VBUS         : USB1_VBUS_DETECT_SET_DISCHARGE_VBUS_Field :=
                                NXP_SVD.USBPHY.VALUE0;
      --  unspecified
      Reserved_27_31         : HAL.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for USB1_VBUS_DETECT_SET_Register use record
      VBUSVALID_THRESH       at 0 range 0 .. 2;
      VBUS_OVERRIDE_EN       at 0 range 3 .. 3;
      SESSEND_OVERRIDE       at 0 range 4 .. 4;
      BVALID_OVERRIDE        at 0 range 5 .. 5;
      AVALID_OVERRIDE        at 0 range 6 .. 6;
      VBUSVALID_OVERRIDE     at 0 range 7 .. 7;
      VBUSVALID_SEL          at 0 range 8 .. 8;
      VBUS_SOURCE_SEL        at 0 range 9 .. 10;
      ID_OVERRIDE_EN         at 0 range 11 .. 11;
      ID_OVERRIDE            at 0 range 12 .. 12;
      EXT_ID_OVERRIDE_EN     at 0 range 13 .. 13;
      EXT_VBUS_OVERRIDE_EN   at 0 range 14 .. 14;
      Reserved_15_17         at 0 range 15 .. 17;
      VBUSVALID_TO_SESSVALID at 0 range 18 .. 18;
      VBUSVALID_5VDETECT     at 0 range 19 .. 19;
      PWRUP_CMPS             at 0 range 20 .. 22;
      Reserved_23_25         at 0 range 23 .. 25;
      DISCHARGE_VBUS         at 0 range 26 .. 26;
      Reserved_27_31         at 0 range 27 .. 31;
   end record;

   --  Sets the threshold for the VBUSVALID comparator
   type USB1_VBUS_DETECT_CLR_VBUSVALID_THRESH_Field is
     (
      --  4.0V
      VALUE0,
      --  4.1V
      VALUE1,
      --  4.2V
      VALUE2,
      --  4.3V
      VALUE3,
      --  4.4V(Default)
      VALUE4,
      --  4.5V
      VALUE5,
      --  4.6V
      VALUE6,
      --  4.7V
      VALUE7)
     with Size => 3;
   for USB1_VBUS_DETECT_CLR_VBUSVALID_THRESH_Field use
     (VALUE0 => 0,
      VALUE1 => 1,
      VALUE2 => 2,
      VALUE3 => 3,
      VALUE4 => 4,
      VALUE5 => 5,
      VALUE6 => 6,
      VALUE7 => 7);

   --  VBUS detect signal override enable
   type USB1_VBUS_DETECT_CLR_VBUS_OVERRIDE_EN_Field is
     (
      --  Use the results of the internal VBUS_VALID and Session Valid
      --  comparators for VBUS_VALID, AVALID, BVALID, and SESSEND (Default)
      VALUE0,
      --  Use the override values for VBUS_VALID, AVALID, BVALID, and SESSEND
      VALUE1)
     with Size => 1;
   for USB1_VBUS_DETECT_CLR_VBUS_OVERRIDE_EN_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  Selects the source of the VBUS_VALID signal reported to the USB
   --  controller This is one of the bit fields that selects the source of the
   --  VBUS_VALID signal reported to the USB controller
   type USB1_VBUS_DETECT_CLR_VBUSVALID_SEL_Field is
     (
      --  Use the VBUS_VALID comparator results for signal reported to the USB
      --  controller (Default)
      VALUE0,
      --  Use the VBUS_VALID_3V detector results for signal reported to the USB
      --  controller
      VALUE1)
     with Size => 1;
   for USB1_VBUS_DETECT_CLR_VBUSVALID_SEL_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  Selects the source of the VBUS_VALID signal reported to the USB
   --  controller This is one of the bit fields that selects the source of the
   --  VBUS_VALID signal reported to the USB controller
   type USB1_VBUS_DETECT_CLR_VBUS_SOURCE_SEL_Field is
     (
      --  Use the VBUS_VALID comparator results for signal reported to the USB
      --  controller (Default)
      VALUE0,
      --  Use the Session Valid comparator results for signal reported to the
      --  USB controller
      VALUE1,
      --  Use the Session Valid comparator results for signal reported to the
      --  USB controller
      VALUE2)
     with Size => 2;
   for USB1_VBUS_DETECT_CLR_VBUS_SOURCE_SEL_Field use
     (VALUE0 => 0,
      VALUE1 => 1,
      VALUE2 => 2);

   --  Enable ID override using the pinmuxed value:
   type USB1_VBUS_DETECT_CLR_EXT_ID_OVERRIDE_EN_Field is
     (
      --  Select the Muxed value chosen using ID_OVERRIDE_EN.
      VALUE0,
      --  Select the external ID value.
      VALUE1)
     with Size => 1;
   for USB1_VBUS_DETECT_CLR_EXT_ID_OVERRIDE_EN_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  Enable VBUS override using the pin muxed value.
   type USB1_VBUS_DETECT_CLR_EXT_VBUS_OVERRIDE_EN_Field is
     (
      --  Select the muxed value chosen using VBUS_OVERRIDE_EN.
      VALUE0,
      --  Select the external VBUS VALID value.
      VALUE1)
     with Size => 1;
   for USB1_VBUS_DETECT_CLR_EXT_VBUS_OVERRIDE_EN_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  Selects the comparator used for VBUS_VALID This bit field controls the
   --  comparator used to report the VBUS_VALID results in USB1_VBUS_DETECT[3]
   --  between the VBUS_VALID comparator and the Session Valid comparator
   type USB1_VBUS_DETECT_CLR_VBUSVALID_TO_SESSVALID_Field is
     (
      --  Use the VBUS_VALID comparator for VBUS_VALID results
      VALUE0,
      --  Use the Session End comparator for VBUS_VALID results. The Session
      --  End threshold is >0.8V and <4.0V.
      VALUE1)
     with Size => 1;
   for USB1_VBUS_DETECT_CLR_VBUSVALID_TO_SESSVALID_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  Enables the VBUS_VALID comparator: Powers up the comparator used for the
   --  VBUS_VALID detector
   type USB1_VBUS_DETECT_CLR_PWRUP_CMPS_Field is
     (
      --  Powers down the VBUS_VALID comparator
      VALUE0,
      --  Enables the VBUS_VALID comparator (default)
      VALUE1)
     with Size => 3;
   for USB1_VBUS_DETECT_CLR_PWRUP_CMPS_Field use
     (VALUE0 => 0,
      VALUE1 => 7);

   --  Controls VBUS discharge resistor This bit field controls a nominal
   --  22kohm resistor between the USB1_VBUS pin and ground
   type USB1_VBUS_DETECT_CLR_DISCHARGE_VBUS_Field is
     (
      --  VBUS discharge resistor is disabled (Default)
      VALUE0,
      --  VBUS discharge resistor is enabled
      VALUE1)
     with Size => 1;
   for USB1_VBUS_DETECT_CLR_DISCHARGE_VBUS_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  USB PHY VBUS Detect Control Register
   type USB1_VBUS_DETECT_CLR_Register is record
      --  Sets the threshold for the VBUSVALID comparator
      VBUSVALID_THRESH       : USB1_VBUS_DETECT_CLR_VBUSVALID_THRESH_Field :=
                                NXP_SVD.USBPHY.VALUE4;
      --  VBUS detect signal override enable
      VBUS_OVERRIDE_EN       : USB1_VBUS_DETECT_CLR_VBUS_OVERRIDE_EN_Field :=
                                NXP_SVD.USBPHY.VALUE0;
      --  Override value for SESSEND The bit field provides the value for
      --  USB1_VBUS_DET_STAT[0] if USB_VBUS_DETECT[3] is set to value 1'b1
      SESSEND_OVERRIDE       : Boolean := False;
      --  Override value for B-Device Session Valid The bit field provides the
      --  value for USB1_VBUS_DET_STAT[1] if USB_VBUS_DETECT[3] is set to value
      --  1'b1
      BVALID_OVERRIDE        : Boolean := False;
      --  Override value for A-Device Session Valid The bit field provides the
      --  value for USB1_VBUS_DET_STAT[2] if USB_VBUS_DETECT[3] is set to value
      --  1'b1
      AVALID_OVERRIDE        : Boolean := False;
      --  Override value for VBUS_VALID signal sent to USB controller The bit
      --  field provides the value for VBUS_VALID reported to the USB
      --  controller if the value of USB1_VBUS_DETECT[3] is set to 1'b1
      VBUSVALID_OVERRIDE     : Boolean := False;
      --  Selects the source of the VBUS_VALID signal reported to the USB
      --  controller This is one of the bit fields that selects the source of
      --  the VBUS_VALID signal reported to the USB controller
      VBUSVALID_SEL          : USB1_VBUS_DETECT_CLR_VBUSVALID_SEL_Field :=
                                NXP_SVD.USBPHY.VALUE0;
      --  Selects the source of the VBUS_VALID signal reported to the USB
      --  controller This is one of the bit fields that selects the source of
      --  the VBUS_VALID signal reported to the USB controller
      VBUS_SOURCE_SEL        : USB1_VBUS_DETECT_CLR_VBUS_SOURCE_SEL_Field :=
                                NXP_SVD.USBPHY.VALUE0;
      --  Enable ID override using the register field. This bit is only used if
      --  EXT_ID_OVERRIDE_EN = 1'b0.
      ID_OVERRIDE_EN         : Boolean := False;
      --  ID override value.
      ID_OVERRIDE            : Boolean := False;
      --  Enable ID override using the pinmuxed value:
      EXT_ID_OVERRIDE_EN     : USB1_VBUS_DETECT_CLR_EXT_ID_OVERRIDE_EN_Field :=
                                NXP_SVD.USBPHY.VALUE0;
      --  Enable VBUS override using the pin muxed value.
      EXT_VBUS_OVERRIDE_EN   : USB1_VBUS_DETECT_CLR_EXT_VBUS_OVERRIDE_EN_Field :=
                                NXP_SVD.USBPHY.VALUE0;
      --  unspecified
      Reserved_15_17         : HAL.UInt3 := 16#0#;
      --  Selects the comparator used for VBUS_VALID This bit field controls
      --  the comparator used to report the VBUS_VALID results in
      --  USB1_VBUS_DETECT[3] between the VBUS_VALID comparator and the Session
      --  Valid comparator
      VBUSVALID_TO_SESSVALID : USB1_VBUS_DETECT_CLR_VBUSVALID_TO_SESSVALID_Field :=
                                NXP_SVD.USBPHY.VALUE0;
      --  no description available
      VBUSVALID_5VDETECT     : Boolean := False;
      --  Enables the VBUS_VALID comparator: Powers up the comparator used for
      --  the VBUS_VALID detector
      PWRUP_CMPS             : USB1_VBUS_DETECT_CLR_PWRUP_CMPS_Field :=
                                NXP_SVD.USBPHY.VALUE1;
      --  unspecified
      Reserved_23_25         : HAL.UInt3 := 16#0#;
      --  Controls VBUS discharge resistor This bit field controls a nominal
      --  22kohm resistor between the USB1_VBUS pin and ground
      DISCHARGE_VBUS         : USB1_VBUS_DETECT_CLR_DISCHARGE_VBUS_Field :=
                                NXP_SVD.USBPHY.VALUE0;
      --  unspecified
      Reserved_27_31         : HAL.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for USB1_VBUS_DETECT_CLR_Register use record
      VBUSVALID_THRESH       at 0 range 0 .. 2;
      VBUS_OVERRIDE_EN       at 0 range 3 .. 3;
      SESSEND_OVERRIDE       at 0 range 4 .. 4;
      BVALID_OVERRIDE        at 0 range 5 .. 5;
      AVALID_OVERRIDE        at 0 range 6 .. 6;
      VBUSVALID_OVERRIDE     at 0 range 7 .. 7;
      VBUSVALID_SEL          at 0 range 8 .. 8;
      VBUS_SOURCE_SEL        at 0 range 9 .. 10;
      ID_OVERRIDE_EN         at 0 range 11 .. 11;
      ID_OVERRIDE            at 0 range 12 .. 12;
      EXT_ID_OVERRIDE_EN     at 0 range 13 .. 13;
      EXT_VBUS_OVERRIDE_EN   at 0 range 14 .. 14;
      Reserved_15_17         at 0 range 15 .. 17;
      VBUSVALID_TO_SESSVALID at 0 range 18 .. 18;
      VBUSVALID_5VDETECT     at 0 range 19 .. 19;
      PWRUP_CMPS             at 0 range 20 .. 22;
      Reserved_23_25         at 0 range 23 .. 25;
      DISCHARGE_VBUS         at 0 range 26 .. 26;
      Reserved_27_31         at 0 range 27 .. 31;
   end record;

   --  Sets the threshold for the VBUSVALID comparator
   type USB1_VBUS_DETECT_TOG_VBUSVALID_THRESH_Field is
     (
      --  4.0V
      VALUE0,
      --  4.1V
      VALUE1,
      --  4.2V
      VALUE2,
      --  4.3V
      VALUE3,
      --  4.4V(Default)
      VALUE4,
      --  4.5V
      VALUE5,
      --  4.6V
      VALUE6,
      --  4.7V
      VALUE7)
     with Size => 3;
   for USB1_VBUS_DETECT_TOG_VBUSVALID_THRESH_Field use
     (VALUE0 => 0,
      VALUE1 => 1,
      VALUE2 => 2,
      VALUE3 => 3,
      VALUE4 => 4,
      VALUE5 => 5,
      VALUE6 => 6,
      VALUE7 => 7);

   --  VBUS detect signal override enable
   type USB1_VBUS_DETECT_TOG_VBUS_OVERRIDE_EN_Field is
     (
      --  Use the results of the internal VBUS_VALID and Session Valid
      --  comparators for VBUS_VALID, AVALID, BVALID, and SESSEND (Default)
      VALUE0,
      --  Use the override values for VBUS_VALID, AVALID, BVALID, and SESSEND
      VALUE1)
     with Size => 1;
   for USB1_VBUS_DETECT_TOG_VBUS_OVERRIDE_EN_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  Selects the source of the VBUS_VALID signal reported to the USB
   --  controller This is one of the bit fields that selects the source of the
   --  VBUS_VALID signal reported to the USB controller
   type USB1_VBUS_DETECT_TOG_VBUSVALID_SEL_Field is
     (
      --  Use the VBUS_VALID comparator results for signal reported to the USB
      --  controller (Default)
      VALUE0,
      --  Use the VBUS_VALID_3V detector results for signal reported to the USB
      --  controller
      VALUE1)
     with Size => 1;
   for USB1_VBUS_DETECT_TOG_VBUSVALID_SEL_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  Selects the source of the VBUS_VALID signal reported to the USB
   --  controller This is one of the bit fields that selects the source of the
   --  VBUS_VALID signal reported to the USB controller
   type USB1_VBUS_DETECT_TOG_VBUS_SOURCE_SEL_Field is
     (
      --  Use the VBUS_VALID comparator results for signal reported to the USB
      --  controller (Default)
      VALUE0,
      --  Use the Session Valid comparator results for signal reported to the
      --  USB controller
      VALUE1,
      --  Use the Session Valid comparator results for signal reported to the
      --  USB controller
      VALUE2)
     with Size => 2;
   for USB1_VBUS_DETECT_TOG_VBUS_SOURCE_SEL_Field use
     (VALUE0 => 0,
      VALUE1 => 1,
      VALUE2 => 2);

   --  Enable ID override using the pin muxed value.
   type USB1_VBUS_DETECT_TOG_EXT_ID_OVERRIDE_EN_Field is
     (
      --  Select the muxed value chosen using ID_OVERRIDE_EN.
      VALUE0,
      --  Select the external ID value.
      VALUE1)
     with Size => 1;
   for USB1_VBUS_DETECT_TOG_EXT_ID_OVERRIDE_EN_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  Enable VBUS override using the pin muxed value.
   type USB1_VBUS_DETECT_TOG_EXT_VBUS_OVERRIDE_EN_Field is
     (
      --  Select the Muxed value chosen using VBUS_OVERRIDE_EN.
      VALUE0,
      --  Select the external VBUS VALID value.
      VALUE1)
     with Size => 1;
   for USB1_VBUS_DETECT_TOG_EXT_VBUS_OVERRIDE_EN_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  Selects the comparator used for VBUS_VALID This bit field controls the
   --  comparator used to report the VBUS_VALID results in USB1_VBUS_DETECT[3]
   --  between the VBUS_VALID comparator and the Session Valid comparator
   type USB1_VBUS_DETECT_TOG_VBUSVALID_TO_SESSVALID_Field is
     (
      --  Use the VBUS_VALID comparator for VBUS_VALID results
      VALUE0,
      --  Use the Session End comparator for VBUS_VALID results. The Session
      --  End threshold is >0.8V and <4.0V.
      VALUE1)
     with Size => 1;
   for USB1_VBUS_DETECT_TOG_VBUSVALID_TO_SESSVALID_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  Enables the VBUS_VALID comparator Powers up the comparator used for the
   --  VBUS_VALID detector
   type USB1_VBUS_DETECT_TOG_PWRUP_CMPS_Field is
     (
      --  Powers down the VBUS_VALID comparator
      VALUE0,
      --  Enables the VBUS_VALID comparator (default)
      VALUE1)
     with Size => 3;
   for USB1_VBUS_DETECT_TOG_PWRUP_CMPS_Field use
     (VALUE0 => 0,
      VALUE1 => 7);

   --  Controls VBUS discharge resistor This bit field controls a nominal
   --  22kohm resistor between the USB1_VBUS pin and ground
   type USB1_VBUS_DETECT_TOG_DISCHARGE_VBUS_Field is
     (
      --  VBUS discharge resistor is disabled (Default)
      VALUE0,
      --  VBUS discharge resistor is enabled
      VALUE1)
     with Size => 1;
   for USB1_VBUS_DETECT_TOG_DISCHARGE_VBUS_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  USB PHY VBUS Detect Control Register
   type USB1_VBUS_DETECT_TOG_Register is record
      --  Sets the threshold for the VBUSVALID comparator
      VBUSVALID_THRESH       : USB1_VBUS_DETECT_TOG_VBUSVALID_THRESH_Field :=
                                NXP_SVD.USBPHY.VALUE4;
      --  VBUS detect signal override enable
      VBUS_OVERRIDE_EN       : USB1_VBUS_DETECT_TOG_VBUS_OVERRIDE_EN_Field :=
                                NXP_SVD.USBPHY.VALUE0;
      --  Override value for SESSEND The bit field provides the value for
      --  USB1_VBUS_DET_STAT[0] if USB_VBUS_DETECT[3] is set to value 1'b1
      SESSEND_OVERRIDE       : Boolean := False;
      --  Override value for B-Device Session Valid The bit field provides the
      --  value for USB1_VBUS_DET_STAT[1] if USB_VBUS_DETECT[3] is set to value
      --  1'b1
      BVALID_OVERRIDE        : Boolean := False;
      --  Override value for A-Device Session Valid The bit field provides the
      --  value for USB1_VBUS_DET_STAT[2] if USB_VBUS_DETECT[3] is set to value
      --  1'b1
      AVALID_OVERRIDE        : Boolean := False;
      --  Override value for VBUS_VALID signal sent to USB controller The bit
      --  field provides the value for VBUS_VALID reported to the USB
      --  controller if the value of USB1_VBUS_DETECT[3] is set to 1'b1
      VBUSVALID_OVERRIDE     : Boolean := False;
      --  Selects the source of the VBUS_VALID signal reported to the USB
      --  controller This is one of the bit fields that selects the source of
      --  the VBUS_VALID signal reported to the USB controller
      VBUSVALID_SEL          : USB1_VBUS_DETECT_TOG_VBUSVALID_SEL_Field :=
                                NXP_SVD.USBPHY.VALUE0;
      --  Selects the source of the VBUS_VALID signal reported to the USB
      --  controller This is one of the bit fields that selects the source of
      --  the VBUS_VALID signal reported to the USB controller
      VBUS_SOURCE_SEL        : USB1_VBUS_DETECT_TOG_VBUS_SOURCE_SEL_Field :=
                                NXP_SVD.USBPHY.VALUE0;
      --  Enable ID override using the register field. This bit is only used if
      --  EXT_ID_OVERRIDE_EN = 1'b0.
      ID_OVERRIDE_EN         : Boolean := False;
      --  ID override value.
      ID_OVERRIDE            : Boolean := False;
      --  Enable ID override using the pin muxed value.
      EXT_ID_OVERRIDE_EN     : USB1_VBUS_DETECT_TOG_EXT_ID_OVERRIDE_EN_Field :=
                                NXP_SVD.USBPHY.VALUE0;
      --  Enable VBUS override using the pin muxed value.
      EXT_VBUS_OVERRIDE_EN   : USB1_VBUS_DETECT_TOG_EXT_VBUS_OVERRIDE_EN_Field :=
                                NXP_SVD.USBPHY.VALUE0;
      --  unspecified
      Reserved_15_17         : HAL.UInt3 := 16#0#;
      --  Selects the comparator used for VBUS_VALID This bit field controls
      --  the comparator used to report the VBUS_VALID results in
      --  USB1_VBUS_DETECT[3] between the VBUS_VALID comparator and the Session
      --  Valid comparator
      VBUSVALID_TO_SESSVALID : USB1_VBUS_DETECT_TOG_VBUSVALID_TO_SESSVALID_Field :=
                                NXP_SVD.USBPHY.VALUE0;
      --  no description available
      VBUSVALID_5VDETECT     : Boolean := False;
      --  Enables the VBUS_VALID comparator Powers up the comparator used for
      --  the VBUS_VALID detector
      PWRUP_CMPS             : USB1_VBUS_DETECT_TOG_PWRUP_CMPS_Field :=
                                NXP_SVD.USBPHY.VALUE1;
      --  unspecified
      Reserved_23_25         : HAL.UInt3 := 16#0#;
      --  Controls VBUS discharge resistor This bit field controls a nominal
      --  22kohm resistor between the USB1_VBUS pin and ground
      DISCHARGE_VBUS         : USB1_VBUS_DETECT_TOG_DISCHARGE_VBUS_Field :=
                                NXP_SVD.USBPHY.VALUE0;
      --  unspecified
      Reserved_27_31         : HAL.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for USB1_VBUS_DETECT_TOG_Register use record
      VBUSVALID_THRESH       at 0 range 0 .. 2;
      VBUS_OVERRIDE_EN       at 0 range 3 .. 3;
      SESSEND_OVERRIDE       at 0 range 4 .. 4;
      BVALID_OVERRIDE        at 0 range 5 .. 5;
      AVALID_OVERRIDE        at 0 range 6 .. 6;
      VBUSVALID_OVERRIDE     at 0 range 7 .. 7;
      VBUSVALID_SEL          at 0 range 8 .. 8;
      VBUS_SOURCE_SEL        at 0 range 9 .. 10;
      ID_OVERRIDE_EN         at 0 range 11 .. 11;
      ID_OVERRIDE            at 0 range 12 .. 12;
      EXT_ID_OVERRIDE_EN     at 0 range 13 .. 13;
      EXT_VBUS_OVERRIDE_EN   at 0 range 14 .. 14;
      Reserved_15_17         at 0 range 15 .. 17;
      VBUSVALID_TO_SESSVALID at 0 range 18 .. 18;
      VBUSVALID_5VDETECT     at 0 range 19 .. 19;
      PWRUP_CMPS             at 0 range 20 .. 22;
      Reserved_23_25         at 0 range 23 .. 25;
      DISCHARGE_VBUS         at 0 range 26 .. 26;
      Reserved_27_31         at 0 range 27 .. 31;
   end record;

   subtype ANACTRL_PFD_CLK_SEL_Field is HAL.UInt2;

   --  Setting this field to 1'b1 will enable the 15kohm pulldown resistors on
   --  both USB_DP and USB_DM pins
   type ANACTRL_DEV_PULLDOWN_Field is
     (
      --  The 15kohm nominal pulldowns on the USB_DP and USB_DM pinsare
      --  disabled in device mode.
      VALUE0,
      --  The 15kohm nominal pulldowns on the USB_DP and USB_DM pinsare enabled
      --  in device mode.
      VALUE1)
     with Size => 1;
   for ANACTRL_DEV_PULLDOWN_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  USB PHY Analog Control Register
   type ANACTRL_Register is record
      --  unspecified
      Reserved_0_0   : HAL.Bit := 16#0#;
      --  Vow voltage detector enable bit.
      LVI_EN         : Boolean := True;
      --  For normal USB operation, this bit field must remain at value 2'b00.
      PFD_CLK_SEL    : ANACTRL_PFD_CLK_SEL_Field := 16#0#;
      --  unspecified
      Reserved_4_9   : HAL.UInt6 := 16#0#;
      --  Setting this field to 1'b1 will enable the 15kohm pulldown resistors
      --  on both USB_DP and USB_DM pins
      DEV_PULLDOWN   : ANACTRL_DEV_PULLDOWN_Field := NXP_SVD.USBPHY.VALUE1;
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#14000#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ANACTRL_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      LVI_EN         at 0 range 1 .. 1;
      PFD_CLK_SEL    at 0 range 2 .. 3;
      Reserved_4_9   at 0 range 4 .. 9;
      DEV_PULLDOWN   at 0 range 10 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   subtype ANACTRL_SET_PFD_CLK_SEL_Field is HAL.UInt2;

   --  Setting this field to 1'b1 will enable the 15kohm pulldown resistors on
   --  both USB_DP and USB_DM pins
   type ANACTRL_SET_DEV_PULLDOWN_Field is
     (
      --  The 15kohm nominal pulldowns on the USB_DP and USB_DM pinsare
      --  disabled in device mode.
      VALUE0,
      --  The 15kohm nominal pulldowns on the USB_DP and USB_DM pinsare enabled
      --  in device mode.
      VALUE1)
     with Size => 1;
   for ANACTRL_SET_DEV_PULLDOWN_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  USB PHY Analog Control Register
   type ANACTRL_SET_Register is record
      --  unspecified
      Reserved_0_0   : HAL.Bit := 16#0#;
      --  Vow voltage detector enable bit.
      LVI_EN         : Boolean := True;
      --  For normal USB operation, this bit field must remain at value 2'b00.
      PFD_CLK_SEL    : ANACTRL_SET_PFD_CLK_SEL_Field := 16#0#;
      --  unspecified
      Reserved_4_9   : HAL.UInt6 := 16#0#;
      --  Setting this field to 1'b1 will enable the 15kohm pulldown resistors
      --  on both USB_DP and USB_DM pins
      DEV_PULLDOWN   : ANACTRL_SET_DEV_PULLDOWN_Field :=
                        NXP_SVD.USBPHY.VALUE1;
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#14000#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ANACTRL_SET_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      LVI_EN         at 0 range 1 .. 1;
      PFD_CLK_SEL    at 0 range 2 .. 3;
      Reserved_4_9   at 0 range 4 .. 9;
      DEV_PULLDOWN   at 0 range 10 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   subtype ANACTRL_CLR_PFD_CLK_SEL_Field is HAL.UInt2;

   --  Setting this field to 1'b1 will enable the 15kohm pulldown resistors on
   --  both USB_DP and USB_DM pins
   type ANACTRL_CLR_DEV_PULLDOWN_Field is
     (
      --  The 15kohm nominal pulldowns on the USB_DP and USB_DM pinsare
      --  disabled in device mode.
      VALUE0,
      --  The 15kohm nominal pulldowns on the USB_DP and USB_DM pinsare enabled
      --  in device mode.
      VALUE1)
     with Size => 1;
   for ANACTRL_CLR_DEV_PULLDOWN_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  USB PHY Analog Control Register
   type ANACTRL_CLR_Register is record
      --  unspecified
      Reserved_0_0   : HAL.Bit := 16#0#;
      --  Vow voltage detector enable bit.
      LVI_EN         : Boolean := True;
      --  For normal USB operation, this bit field must remain at value 2'b00.
      PFD_CLK_SEL    : ANACTRL_CLR_PFD_CLK_SEL_Field := 16#0#;
      --  unspecified
      Reserved_4_9   : HAL.UInt6 := 16#0#;
      --  Setting this field to 1'b1 will enable the 15kohm pulldown resistors
      --  on both USB_DP and USB_DM pins
      DEV_PULLDOWN   : ANACTRL_CLR_DEV_PULLDOWN_Field :=
                        NXP_SVD.USBPHY.VALUE1;
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#14000#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ANACTRL_CLR_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      LVI_EN         at 0 range 1 .. 1;
      PFD_CLK_SEL    at 0 range 2 .. 3;
      Reserved_4_9   at 0 range 4 .. 9;
      DEV_PULLDOWN   at 0 range 10 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   subtype ANACTRL_TOG_PFD_CLK_SEL_Field is HAL.UInt2;

   --  Setting this field to 1'b1 will enable the 15kohm pulldown resistors on
   --  both USB_DP and USB_DM pins
   type ANACTRL_TOG_DEV_PULLDOWN_Field is
     (
      --  The 15kohm nominal pulldowns on the USB_DP and USB_DM pinsare
      --  disabled in device mode.
      VALUE0,
      --  The 15kohm nominal pulldowns on the USB_DP and USB_DM pinsare enabled
      --  in device mode.
      VALUE1)
     with Size => 1;
   for ANACTRL_TOG_DEV_PULLDOWN_Field use
     (VALUE0 => 0,
      VALUE1 => 1);

   --  USB PHY Analog Control Register
   type ANACTRL_TOG_Register is record
      --  unspecified
      Reserved_0_0   : HAL.Bit := 16#0#;
      --  Vow voltage detector enable bit.
      LVI_EN         : Boolean := True;
      --  For normal USB operation, this bit field must remain at value 2'b00.
      PFD_CLK_SEL    : ANACTRL_TOG_PFD_CLK_SEL_Field := 16#0#;
      --  unspecified
      Reserved_4_9   : HAL.UInt6 := 16#0#;
      --  Setting this field to 1'b1 will enable the 15kohm pulldown resistors
      --  on both USB_DP and USB_DM pins
      DEV_PULLDOWN   : ANACTRL_TOG_DEV_PULLDOWN_Field :=
                        NXP_SVD.USBPHY.VALUE1;
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#14000#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ANACTRL_TOG_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      LVI_EN         at 0 range 1 .. 1;
      PFD_CLK_SEL    at 0 range 2 .. 3;
      Reserved_4_9   at 0 range 4 .. 9;
      DEV_PULLDOWN   at 0 range 10 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Universal System Bus Physical Layer
   type USBPHY_Peripheral is record
      --  USB PHY Power-Down Register
      PWD                  : aliased PWD_Register;
      --  USB PHY Power-Down Register
      PWD_SET              : aliased PWD_SET_Register;
      --  USB PHY Power-Down Register
      PWD_CLR              : aliased PWD_CLR_Register;
      --  USB PHY Power-Down Register
      PWD_TOG              : aliased PWD_TOG_Register;
      --  USB PHY Transmitter Control Register
      TX                   : aliased TX_Register;
      --  USB PHY Transmitter Control Register
      TX_SET               : aliased TX_SET_Register;
      --  USB PHY Transmitter Control Register
      TX_CLR               : aliased TX_CLR_Register;
      --  USB PHY Transmitter Control Register
      TX_TOG               : aliased TX_TOG_Register;
      --  USB PHY Receiver Control Register
      RX                   : aliased RX_Register;
      --  USB PHY Receiver Control Register
      RX_SET               : aliased RX_SET_Register;
      --  USB PHY Receiver Control Register
      RX_CLR               : aliased RX_CLR_Register;
      --  USB PHY Receiver Control Register
      RX_TOG               : aliased RX_TOG_Register;
      --  USB PHY General Control Register
      CTRL                 : aliased CTRL_Register;
      --  USB PHY General Control Register
      CTRL_SET             : aliased CTRL_SET_Register;
      --  USB PHY General Control Register
      CTRL_CLR             : aliased CTRL_CLR_Register;
      --  USB PHY General Control Register
      CTRL_TOG             : aliased CTRL_TOG_Register;
      --  USB PHY Status Register
      STATUS               : aliased STATUS_Register;
      --  USB PHY PLL Control/Status Register
      PLL_SIC              : aliased PLL_SIC_Register;
      --  USB PHY PLL Control/Status Register
      PLL_SIC_SET          : aliased PLL_SIC_SET_Register;
      --  USB PHY PLL Control/Status Register
      PLL_SIC_CLR          : aliased PLL_SIC_CLR_Register;
      --  USB PHY PLL Control/Status Register
      PLL_SIC_TOG          : aliased PLL_SIC_TOG_Register;
      --  USB PHY VBUS Detect Control Register
      USB1_VBUS_DETECT     : aliased USB1_VBUS_DETECT_Register;
      --  USB PHY VBUS Detect Control Register
      USB1_VBUS_DETECT_SET : aliased USB1_VBUS_DETECT_SET_Register;
      --  USB PHY VBUS Detect Control Register
      USB1_VBUS_DETECT_CLR : aliased USB1_VBUS_DETECT_CLR_Register;
      --  USB PHY VBUS Detect Control Register
      USB1_VBUS_DETECT_TOG : aliased USB1_VBUS_DETECT_TOG_Register;
      --  USB PHY Analog Control Register
      ANACTRL              : aliased ANACTRL_Register;
      --  USB PHY Analog Control Register
      ANACTRL_SET          : aliased ANACTRL_SET_Register;
      --  USB PHY Analog Control Register
      ANACTRL_CLR          : aliased ANACTRL_CLR_Register;
      --  USB PHY Analog Control Register
      ANACTRL_TOG          : aliased ANACTRL_TOG_Register;
   end record
     with Volatile;

   for USBPHY_Peripheral use record
      PWD                  at 16#0# range 0 .. 31;
      PWD_SET              at 16#4# range 0 .. 31;
      PWD_CLR              at 16#8# range 0 .. 31;
      PWD_TOG              at 16#C# range 0 .. 31;
      TX                   at 16#10# range 0 .. 31;
      TX_SET               at 16#14# range 0 .. 31;
      TX_CLR               at 16#18# range 0 .. 31;
      TX_TOG               at 16#1C# range 0 .. 31;
      RX                   at 16#20# range 0 .. 31;
      RX_SET               at 16#24# range 0 .. 31;
      RX_CLR               at 16#28# range 0 .. 31;
      RX_TOG               at 16#2C# range 0 .. 31;
      CTRL                 at 16#30# range 0 .. 31;
      CTRL_SET             at 16#34# range 0 .. 31;
      CTRL_CLR             at 16#38# range 0 .. 31;
      CTRL_TOG             at 16#3C# range 0 .. 31;
      STATUS               at 16#40# range 0 .. 31;
      PLL_SIC              at 16#A0# range 0 .. 31;
      PLL_SIC_SET          at 16#A4# range 0 .. 31;
      PLL_SIC_CLR          at 16#A8# range 0 .. 31;
      PLL_SIC_TOG          at 16#AC# range 0 .. 31;
      USB1_VBUS_DETECT     at 16#C0# range 0 .. 31;
      USB1_VBUS_DETECT_SET at 16#C4# range 0 .. 31;
      USB1_VBUS_DETECT_CLR at 16#C8# range 0 .. 31;
      USB1_VBUS_DETECT_TOG at 16#CC# range 0 .. 31;
      ANACTRL              at 16#100# range 0 .. 31;
      ANACTRL_SET          at 16#104# range 0 .. 31;
      ANACTRL_CLR          at 16#108# range 0 .. 31;
      ANACTRL_TOG          at 16#10C# range 0 .. 31;
   end record;

   --  Universal System Bus Physical Layer
   USBPHY_Periph : aliased USBPHY_Peripheral
     with Import, Address => System'To_Address (16#40038000#);

end NXP_SVD.USBPHY;
