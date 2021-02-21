--  Copyright 2016-2019 NXP
--  All rights reserved.SPDX-License-Identifier: BSD-3-Clause


--  This spec has been automatically generated from LPC55S6x_mod2.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NXP_SVD.GPIO is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   ---------------------------
   -- B cluster's Registers --
   ---------------------------

   --  Byte pin registers for all port GPIO pins
   type B_B_Register is record
      --  Read: state of the pin PIOm_n, regardless of direction, masking, or
      --  alternate function, except that pins configured as analog I/O always
      --  read as 0. One register for each port pin. Supported pins depends on
      --  the specific device and package. Write: loads the pin's output bit.
      --  One register for each port pin. Supported pins depends on the
      --  specific device and package.
      PBYTE        : Boolean := False;
      --  unspecified
      Reserved_1_7 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 8,
          Bit_Order => System.Low_Order_First;

   for B_B_Register use record
      PBYTE        at 0 range 0 .. 0;
      Reserved_1_7 at 0 range 1 .. 7;
   end record;

   --  Byte pin registers for all port GPIO pins
   type B_B_Registers is array (0 .. 31) of B_B_Register;

   --  no description available
   type B_Cluster is record
      --  Byte pin registers for all port GPIO pins
      B : aliased B_B_Registers;
   end record
     with Size => 256;

   for B_Cluster use record
      B at 0 range 0 .. 255;
   end record;

   --  no description available
   type B_Clusters is array (0 .. 1) of B_Cluster;

   ---------------------------
   -- W cluster's Registers --
   ---------------------------

   --  Word pin registers for all port GPIO pins

   --  Word pin registers for all port GPIO pins
   type W_W_Registers is array (0 .. 31) of HAL.UInt32;

   --  no description available
   type W_Cluster is record
      --  Word pin registers for all port GPIO pins
      W : aliased W_W_Registers;
   end record
     with Size => 1024;

   for W_Cluster use record
      W at 0 range 0 .. 1023;
   end record;

   --  no description available
   type W_Clusters is array (0 .. 1) of W_Cluster;

   --  Direction registers for all port GPIO pins

   --  Direction registers for all port GPIO pins
   type DIR_Registers is array (0 .. 1) of HAL.UInt32;

   --  Mask register for all port GPIO pins

   --  Mask register for all port GPIO pins
   type MASK_Registers is array (0 .. 1) of HAL.UInt32;

   --  Port pin register for all port GPIO pins

   --  Port pin register for all port GPIO pins
   type PIN_Registers is array (0 .. 1) of HAL.UInt32;

   --  Masked port register for all port GPIO pins

   --  Masked port register for all port GPIO pins
   type MPIN_Registers is array (0 .. 1) of HAL.UInt32;

   --  Write: Set register for port. Read: output bits for port

   --  Write: Set register for port. Read: output bits for port
   type SET_Registers is array (0 .. 1) of HAL.UInt32;

   --  Clear port for all port GPIO pins

   --  Clear port for all port GPIO pins
   type CLR_Registers is array (0 .. 1) of HAL.UInt32;

   --  Toggle port for all port GPIO pins

   --  Toggle port for all port GPIO pins
   type NOT_Registers is array (0 .. 1) of HAL.UInt32;

   --  Set pin direction bits for port

   --  Set pin direction bits for port
   type DIRSET_Registers is array (0 .. 1) of HAL.UInt32;

   --  Clear pin direction bits for port

   --  Clear pin direction bits for port
   type DIRCLR_Registers is array (0 .. 1) of HAL.UInt32;

   --  Toggle pin direction bits for port

   --  Toggle pin direction bits for port
   type DIRNOT_Registers is array (0 .. 1) of HAL.UInt32;

   ---------------------------
   -- B cluster's Registers --
   ---------------------------

   ---------------------------
   -- W cluster's Registers --
   ---------------------------

   --  Word pin registers for all port GPIO pins

   -----------------
   -- Peripherals --
   -----------------

   --  General Purpose I/O (GPIO)
   type GPIO_Peripheral is record
      --  no description available
      B      : aliased B_Clusters;
      --  no description available
      W      : aliased W_Clusters;
      --  Direction registers for all port GPIO pins
      DIR    : aliased DIR_Registers;
      --  Mask register for all port GPIO pins
      MASK   : aliased MASK_Registers;
      --  Port pin register for all port GPIO pins
      PIN    : aliased PIN_Registers;
      --  Masked port register for all port GPIO pins
      MPIN   : aliased MPIN_Registers;
      --  Write: Set register for port. Read: output bits for port
      SET    : aliased SET_Registers;
      --  Clear port for all port GPIO pins
      CLR    : aliased CLR_Registers;
      --  Toggle port for all port GPIO pins
      NOT_k  : aliased NOT_Registers;
      --  Set pin direction bits for port
      DIRSET : aliased DIRSET_Registers;
      --  Clear pin direction bits for port
      DIRCLR : aliased DIRCLR_Registers;
      --  Toggle pin direction bits for port
      DIRNOT : aliased DIRNOT_Registers;
   end record
     with Volatile;

   for GPIO_Peripheral use record
      B      at 16#0# range 0 .. 511;
      W      at 16#1000# range 0 .. 2047;
      DIR    at 16#2000# range 0 .. 63;
      MASK   at 16#2080# range 0 .. 63;
      PIN    at 16#2100# range 0 .. 63;
      MPIN   at 16#2180# range 0 .. 63;
      SET    at 16#2200# range 0 .. 63;
      CLR    at 16#2280# range 0 .. 63;
      NOT_k  at 16#2300# range 0 .. 63;
      DIRSET at 16#2380# range 0 .. 63;
      DIRCLR at 16#2400# range 0 .. 63;
      DIRNOT at 16#2480# range 0 .. 63;
   end record;

   --  General Purpose I/O (GPIO)
   GPIO_Periph : aliased GPIO_Peripheral
     with Import, Address => GPIO_Base;

   --  General Purpose I/O (GPIO)
   SECGPIO_Periph : aliased GPIO_Peripheral
     with Import, Address => SECGPIO_Base;

end NXP_SVD.GPIO;
