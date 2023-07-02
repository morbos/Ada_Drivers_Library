--  This spec has been automatically generated from STM32U585.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.CORDIC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype CSR_FUNC_Field is HAL.UInt4;
   subtype CSR_PRECISION_Field is HAL.UInt4;
   subtype CSR_SCALE_Field is HAL.UInt3;

   --  CORDIC Control Status register
   type CSR_Register is record
      --  Function
      FUNC           : CSR_FUNC_Field := 16#0#;
      --  Precision required (number of iterations)
      PRECISION      : CSR_PRECISION_Field := 16#5#;
      --  Scaling factor
      SCALE          : CSR_SCALE_Field := 16#0#;
      --  unspecified
      Reserved_11_15 : HAL.UInt5 := 16#0#;
      --  Enable interrupt
      IEN            : Boolean := False;
      --  Enable DMA read channel
      DMAREN         : Boolean := False;
      --  Enable DMA write channel
      DMAWEN         : Boolean := False;
      --  Number of results in the CORDIC_RDATA register
      NRES           : Boolean := False;
      --  Number of arguments expected by the CORDIC_WDATA register
      NARGS          : Boolean := False;
      --  Width of output data
      RESSIZE        : Boolean := False;
      --  Width of input data
      ARGSIZE        : Boolean := False;
      --  unspecified
      Reserved_23_30 : HAL.UInt8 := 16#0#;
      --  Read-only. Result ready flag
      RRDY           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CSR_Register use record
      FUNC           at 0 range 0 .. 3;
      PRECISION      at 0 range 4 .. 7;
      SCALE          at 0 range 8 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
      IEN            at 0 range 16 .. 16;
      DMAREN         at 0 range 17 .. 17;
      DMAWEN         at 0 range 18 .. 18;
      NRES           at 0 range 19 .. 19;
      NARGS          at 0 range 20 .. 20;
      RESSIZE        at 0 range 21 .. 21;
      ARGSIZE        at 0 range 22 .. 22;
      Reserved_23_30 at 0 range 23 .. 30;
      RRDY           at 0 range 31 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  CORDIC Co-processor
   type CORDIC_Peripheral is record
      --  CORDIC Control Status register
      CSR   : aliased CSR_Register;
      --  FMAC Write Data register
      WDATA : aliased HAL.UInt32;
      --  FMAC Read Data register
      RDATA : aliased HAL.UInt32;
   end record
     with Volatile;

   for CORDIC_Peripheral use record
      CSR   at 16#0# range 0 .. 31;
      WDATA at 16#4# range 0 .. 31;
      RDATA at 16#8# range 0 .. 31;
   end record;

   --  CORDIC Co-processor
   CORDIC_Periph : aliased CORDIC_Peripheral
     with Import, Address => CORDIC_Base;

   --  CORDIC Co-processor
   SEC_CORDIC_Periph : aliased CORDIC_Peripheral
     with Import, Address => SEC_CORDIC_Base;

end STM32_SVD.CORDIC;
