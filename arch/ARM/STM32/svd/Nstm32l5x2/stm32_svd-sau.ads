--  This spec has been automatically generated from STM32L562.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.SAU is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  SAU Control Register
   type SAU_CTRL_Register is record
      --  Enables the SAU
      ENABLE        : Boolean := False;
      --  All Non-secure. When SAU_CTRL.ENABLE is 0 this bit controls if the
      --  memory is marked as Non-secure or Secure
      ALLNS         : Boolean := False;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SAU_CTRL_Register use record
      ENABLE        at 0 range 0 .. 0;
      ALLNS         at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   subtype SAU_TYPE_SREGION_Field is HAL.UInt8;

   --  Indicates the number of regions implemented by the Security Attribution
   --  Unit.
   type SAU_TYPE_Register is record
      --  The number of implemented SAU regions
      SREGION       : SAU_TYPE_SREGION_Field := 16#8#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SAU_TYPE_Register use record
      SREGION       at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype SAU_RNR_REGION_Field is HAL.UInt8;

   --  Selects the region currently accessed by SAU_RBAR and SAU_RLAR
   type SAU_RNR_Register is record
      --  Indicates the SAU region accessed by SAU_RBAR and SAU_RLAR
      REGION        : SAU_RNR_REGION_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SAU_RNR_Register use record
      REGION        at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype SAU_RBAR_BADDR_Field is HAL.UInt27;

   --  SAU Region Base Address Register
   type SAU_RBAR_Register is record
      --  unspecified
      Reserved_0_4 : HAL.UInt5 := 16#0#;
      --  Holds bits [31:5] of the base address for the selected SAU region
      BADDR        : SAU_RBAR_BADDR_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SAU_RBAR_Register use record
      Reserved_0_4 at 0 range 0 .. 4;
      BADDR        at 0 range 5 .. 31;
   end record;

   subtype SAU_RLAR_LADDR_Field is HAL.UInt27;

   --  SAU Region Limit Address Register
   type SAU_RLAR_Register is record
      --  SAU region enable
      ENABLE       : Boolean := False;
      --  Controls whether Non-secure state is permitted to execute an SG
      --  instruction from this region
      NSC          : Boolean := False;
      --  unspecified
      Reserved_2_4 : HAL.UInt3 := 16#0#;
      --  Holds bits [31:5] of the limit address for the selected SAU region
      LADDR        : SAU_RLAR_LADDR_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SAU_RLAR_Register use record
      ENABLE       at 0 range 0 .. 0;
      NSC          at 0 range 1 .. 1;
      Reserved_2_4 at 0 range 2 .. 4;
      LADDR        at 0 range 5 .. 31;
   end record;

   --  Provides information about any security related faults
   type SFSR_Register is record
      --  This bit is set if a function call from the Non-secure state or
      --  exception targets a non-SG instruction in the Secure state. This bit
      --  is also set if the target address is an SG instruction, but there is
      --  no matching SAU/IDAU region with the NSC flag set.
      INVEP         : Boolean := False;
      --  This bit is set if the integrity signature in an exception stack
      --  frame is found to be invalid during the unstacking operation
      INVIS         : Boolean := False;
      --  This can be caused by EXC_RETURN.DCRS being set to 0 when returning
      --  from an exception in the Non-secure state, or by EXC_RETURN.ES being
      --  set to 1 when returning from an exception in the Non-secure state
      INVER         : Boolean := False;
      --  Sticky flag indicating that an attempt was made to access parts of
      --  the address space that are marked as Secure with NS-Req for the
      --  transaction set to Non-secure
      AUVIOL        : Boolean := False;
      --  Sticky flag indicating that an exception was raised due to a branch
      --  that was not flagged as being domain crossing causing a transition
      --  from Secure to Non-secure memory
      INVTRAN       : Boolean := False;
      --  Sticky flag indicating that an SAU or IDAU violation occurred during
      --  the lazy preservation of Floating-point state
      LSPERR        : Boolean := False;
      --  This bit is set when the SFAR register contains a valid value
      SFARVALID     : Boolean := False;
      --  Sticky flag indicating that an error occurred during lazy state
      --  activation or deactivation
      LSERR         : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SFSR_Register use record
      INVEP         at 0 range 0 .. 0;
      INVIS         at 0 range 1 .. 1;
      INVER         at 0 range 2 .. 2;
      AUVIOL        at 0 range 3 .. 3;
      INVTRAN       at 0 range 4 .. 4;
      LSPERR        at 0 range 5 .. 5;
      SFARVALID     at 0 range 6 .. 6;
      LSERR         at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Security Attribution Unit
   type SAU_Peripheral is record
      --  SAU Control Register
      SAU_CTRL : aliased SAU_CTRL_Register;
      --  Indicates the number of regions implemented by the Security
      --  Attribution Unit.
      SAU_TYPE : aliased SAU_TYPE_Register;
      --  Selects the region currently accessed by SAU_RBAR and SAU_RLAR
      SAU_RNR  : aliased SAU_RNR_Register;
      --  SAU Region Base Address Register
      SAU_RBAR : aliased SAU_RBAR_Register;
      --  SAU Region Limit Address Register
      SAU_RLAR : aliased SAU_RLAR_Register;
      --  Provides information about any security related faults
      SFSR     : aliased SFSR_Register;
      --  Shows the address of the memory location that caused a Security
      --  violation
      SFAR     : aliased HAL.UInt32;
   end record
     with Volatile;

   for SAU_Peripheral use record
      SAU_CTRL at 16#0# range 0 .. 31;
      SAU_TYPE at 16#4# range 0 .. 31;
      SAU_RNR  at 16#8# range 0 .. 31;
      SAU_RBAR at 16#C# range 0 .. 31;
      SAU_RLAR at 16#10# range 0 .. 31;
      SFSR     at 16#14# range 0 .. 31;
      SFAR     at 16#18# range 0 .. 31;
   end record;

   --  Security Attribution Unit
   SAU_Periph : aliased SAU_Peripheral
     with Import, Address => System'To_Address (16#E000EDD0#);

end STM32_SVD.SAU;
