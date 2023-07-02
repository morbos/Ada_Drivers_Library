--  This spec has been automatically generated from STM32U585.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.PSSI is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype CR_EDM_Field is HAL.UInt2;
   subtype CR_DERDYCFG_Field is HAL.UInt3;

   --  PSSI control register
   type CR_Register is record
      --  unspecified
      Reserved_0_4   : HAL.UInt5 := 16#0#;
      --  Parallel data clock polarity 		This bit configures the capture edge
      --  of the parallel clock or the edge used for driving outputs, depending
      --  on OUTEN.
      CKPOL          : Boolean := False;
      --  Data enable (PSSI_DE) polarity 		This bit indicates the level on the
      --  PSSI_DE pin when the data are not valid on the parallel interface.
      DEPOL          : Boolean := False;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Ready (PSSI_RDY) polarity 		This bit indicates the level on the
      --  PSSI_RDY pin when the data are not valid on the parallel interface.
      RDYPOL         : Boolean := False;
      --  unspecified
      Reserved_9_9   : HAL.Bit := 16#0#;
      --  Extended data mode
      EDM            : CR_EDM_Field := 16#0#;
      --  unspecified
      Reserved_12_13 : HAL.UInt2 := 16#0#;
      --  PSSI enable 		The contents of the FIFO are flushed when ENABLE is
      --  cleared to 0. 		Note: When ENABLE=1, the content of PSSI_CR must not
      --  be changed, except for the ENABLE bit itself. All configuration bits
      --  can change as soon as ENABLE changes from 0 to 1. 		The DMA
      --  controller and all PSSI configuration registers must be programmed
      --  correctly before setting the ENABLE bit to 1. 		The ENABLE bit and
      --  the DCMI ENABLE bit (bit 15 of DCMI_CR) must not be set to 1 at the
      --  same time.
      ENABLE         : Boolean := False;
      --  unspecified
      Reserved_15_17 : HAL.UInt3 := 16#0#;
      --  Data enable and ready configuration 		When the PSSI_RDY function is
      --  mapped to the PSSI_DE pin (settings 101 or 111), it is still the
      --  RDYPOL bit which determines its polarity. Similarly, when the PSSI_DE
      --  function is mapped to the PSSI_RDY pin (settings 110 or 111), it is
      --  still the DEPOL bit which determines its polarity.
      DERDYCFG       : CR_DERDYCFG_Field := 16#0#;
      --  unspecified
      Reserved_21_29 : HAL.UInt9 := 16#0#;
      --  DMA enable bit
      DMAEN          : Boolean := True;
      --  Data direction selection bit
      OUTEN          : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      Reserved_0_4   at 0 range 0 .. 4;
      CKPOL          at 0 range 5 .. 5;
      DEPOL          at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      RDYPOL         at 0 range 8 .. 8;
      Reserved_9_9   at 0 range 9 .. 9;
      EDM            at 0 range 10 .. 11;
      Reserved_12_13 at 0 range 12 .. 13;
      ENABLE         at 0 range 14 .. 14;
      Reserved_15_17 at 0 range 15 .. 17;
      DERDYCFG       at 0 range 18 .. 20;
      Reserved_21_29 at 0 range 21 .. 29;
      DMAEN          at 0 range 30 .. 30;
      OUTEN          at 0 range 31 .. 31;
   end record;

   --  PSSI status register
   type SR_Register is record
      --  unspecified
      Reserved_0_1  : HAL.UInt2;
      --  Read-only. RTT4B
      RTT4B         : Boolean;
      --  Read-only. RTT1B
      RTT1B         : Boolean;
      --  unspecified
      Reserved_4_31 : HAL.UInt28;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register use record
      Reserved_0_1  at 0 range 0 .. 1;
      RTT4B         at 0 range 2 .. 2;
      RTT1B         at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  PSSI raw interrupt status register
   type RIS_Register is record
      --  unspecified
      Reserved_0_0  : HAL.Bit;
      --  Read-only. OVR_RIS
      OVR_RIS       : Boolean;
      --  unspecified
      Reserved_2_31 : HAL.UInt30;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RIS_Register use record
      Reserved_0_0  at 0 range 0 .. 0;
      OVR_RIS       at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  PSSI interrupt enable register
   type IER_Register is record
      --  unspecified
      Reserved_0_0  : HAL.Bit := 16#0#;
      --  OVR_IE
      OVR_IE        : Boolean := False;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IER_Register use record
      Reserved_0_0  at 0 range 0 .. 0;
      OVR_IE        at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  PSSI masked interrupt status register
   type MIS_Register is record
      --  unspecified
      Reserved_0_0  : HAL.Bit;
      --  Read-only. OVR_MIS
      OVR_MIS       : Boolean;
      --  unspecified
      Reserved_2_31 : HAL.UInt30;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MIS_Register use record
      Reserved_0_0  at 0 range 0 .. 0;
      OVR_MIS       at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  PSSI interrupt clear register
   type ICR_Register is record
      --  unspecified
      Reserved_0_0  : HAL.Bit := 16#0#;
      --  Write-only. OVR_ISC
      OVR_ISC       : Boolean := False;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ICR_Register use record
      Reserved_0_0  at 0 range 0 .. 0;
      OVR_ISC       at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  DR_BYTE array element
   subtype DR_BYTE_Element is HAL.UInt8;

   --  DR_BYTE array
   type DR_BYTE_Field_Array is array (0 .. 3) of DR_BYTE_Element
     with Component_Size => 8, Size => 32;

   --  PSSI data register
   type DR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  BYTE as a value
            Val : HAL.UInt32;
         when True =>
            --  BYTE as an array
            Arr : DR_BYTE_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  PSSI
   type PSSI_Peripheral is record
      --  PSSI control register
      CR  : aliased CR_Register;
      --  PSSI status register
      SR  : aliased SR_Register;
      --  PSSI raw interrupt status register
      RIS : aliased RIS_Register;
      --  PSSI interrupt enable register
      IER : aliased IER_Register;
      --  PSSI masked interrupt status register
      MIS : aliased MIS_Register;
      --  PSSI interrupt clear register
      ICR : aliased ICR_Register;
      --  PSSI data register
      DR  : aliased DR_Register;
   end record
     with Volatile;

   for PSSI_Peripheral use record
      CR  at 16#0# range 0 .. 31;
      SR  at 16#4# range 0 .. 31;
      RIS at 16#8# range 0 .. 31;
      IER at 16#C# range 0 .. 31;
      MIS at 16#10# range 0 .. 31;
      ICR at 16#14# range 0 .. 31;
      DR  at 16#28# range 0 .. 31;
   end record;

   --  PSSI
   PSSI_Periph : aliased PSSI_Peripheral
     with Import, Address => PSSI_Base;

   --  PSSI
   SEC_PSSI_Periph : aliased PSSI_Peripheral
     with Import, Address => SEC_PSSI_Base;

end STM32_SVD.PSSI;
