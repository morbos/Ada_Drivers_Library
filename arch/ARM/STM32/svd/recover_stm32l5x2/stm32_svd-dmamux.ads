--  This spec has been automatically generated from xyzzy

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.DMAMUX is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype CCR_DMAREQ_ID_Field is HAL.UInt7;
   subtype CCR_SPOL_Field is HAL.UInt2;
   subtype CCR_NBREQ_Field is HAL.UInt5;
   subtype CCR_SYNC_ID_Field is HAL.UInt5;

   type CCR_Register is record
      --  DMA Request ID
      DMAREQ_ID      : CCR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Synchronization Overrun Interrupt Enable
      SOIE           : Boolean := False;
      --  Event Generation Enable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : Boolean := False;
      --  Sync polarity
      SPOL           : CCR_SPOL_Field := 16#0#;
      --  Nb request
      NBREQ          : CCR_NBREQ_Field := 16#0#;
      --  SYNC_ID
      SYNC_ID        : CCR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   type CCR_Registers is array (0 .. 15) of CCR_Register;

   --  CSR_SOF array
   type CSR_SOF_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for CSR_SOF
   type CSR_SOF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SOF as a value
            Val : HAL.UInt16;
         when True =>
            --  SOF as an array
            Arr : CSR_SOF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for CSR_SOF_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  DMA Multiplexer Channel Status register
   type CSR_Register is record
      --  Synchronization Overrun Flag 0
      SOF            : CSR_SOF_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CSR_Register use record
      SOF            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  CCFR_CSOF array
   type CCFR_CSOF_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for CCFR_CSOF
   type CCFR_CSOF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CSOF as a value
            Val : HAL.UInt16;
         when True =>
            --  CSOF as an array
            Arr : CCFR_CSOF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for CCFR_CSOF_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  DMA Channel Clear Flag Register
   type CCFR_Register is record
      --  Synchronization Clear Overrun Flag 0
      CSOF           : CCFR_CSOF_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCFR_Register use record
      CSOF           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype RGCR_SIG_ID_Field is HAL.UInt5;
   subtype RGCR_GPOL_Field is HAL.UInt2;
   subtype RGCR_GNBREQ_Field is HAL.UInt5;

   type RGCR_Register is record
      --  Signal ID
      SIG_ID         : RGCR_SIG_ID_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  Overrun Interrupt Enable
      OIE            : Boolean := False;
      --  unspecified
      Reserved_9_15  : HAL.UInt7 := 16#0#;
      --  Generation Enable
      GE             : Boolean := False;
      --  Generation Polarity
      GPOL           : RGCR_GPOL_Field := 16#0#;
      --  Number of Request
      GNBREQ         : RGCR_GNBREQ_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RGCR_Register use record
      SIG_ID         at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      OIE            at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      GE             at 0 range 16 .. 16;
      GPOL           at 0 range 17 .. 18;
      GNBREQ         at 0 range 19 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   type RGCR_Registers is array (0 .. 3) of RGCR_Register;

   --  RGSR_OF array
   type RGSR_OF_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for RGSR_OF
   type RGSR_OF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  OF as a value
            Val : HAL.UInt4;
         when True =>
            --  OF as an array
            Arr : RGSR_OF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for RGSR_OF_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  DMA Request Generator Status Register
   type RGSR_Register is record
      --  Read-only. Generator Overrun Flag 0
      OF_k          : RGSR_OF_Field;
      --  unspecified
      Reserved_4_31 : HAL.UInt28;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RGSR_Register use record
      OF_k          at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  RGCFR_CSOF array
   type RGCFR_CSOF_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for RGCFR_CSOF
   type RGCFR_CSOF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CSOF as a value
            Val : HAL.UInt4;
         when True =>
            --  CSOF as an array
            Arr : RGCFR_CSOF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for RGCFR_CSOF_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  DMA Request Generator Clear Flag Register
   type RGCFR_Register is record
      --  Generator Clear Overrun Flag 0
      CSOF          : RGCFR_CSOF_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RGCFR_Register use record
      CSOF          at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Direct memory access Multiplexer
   type DMAMUX_Peripheral is record
      CCR   : aliased CCR_Registers;
      --  DMA Multiplexer Channel Status register
      CSR   : aliased CSR_Register;
      --  DMA Channel Clear Flag Register
      CCFR  : aliased CCFR_Register;
      RGCR  : aliased RGCR_Registers;
      --  DMA Request Generator Status Register
      RGSR  : aliased RGSR_Register;
      --  DMA Request Generator Clear Flag Register
      RGCFR : aliased RGCFR_Register;
   end record
     with Volatile;

   for DMAMUX_Peripheral use record
      CCR   at 16#0# range 0 .. 511;
      CSR   at 16#80# range 0 .. 31;
      CCFR  at 16#84# range 0 .. 31;
      RGCR  at 16#100# range 0 .. 127;
      RGSR  at 16#140# range 0 .. 31;
      RGCFR at 16#144# range 0 .. 31;
   end record;

   --  Direct memory access Multiplexer
   DMAMUX1_Periph : aliased DMAMUX_Peripheral
     with Import, Address => DMAMUX1_Base;

   --  Direct memory access Multiplexer
   SEC_DMAMUX1_Periph : aliased DMAMUX_Peripheral
     with Import, Address => SEC_DMAMUX1_Base;

end STM32_SVD.DMAMUX;
