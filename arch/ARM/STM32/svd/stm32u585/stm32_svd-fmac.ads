--  This spec has been automatically generated from STM32U585.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.FMAC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype X1BUFCFG_X1_BASE_Field is HAL.UInt8;
   subtype X1BUFCFG_X1_BUF_SIZE_Field is HAL.UInt8;
   subtype X1BUFCFG_FULL_WM_Field is HAL.UInt2;

   --  FMAC X1 Buffer Configuration register
   type X1BUFCFG_Register is record
      --  Base address of X1 buffer
      X1_BASE        : X1BUFCFG_X1_BASE_Field := 16#0#;
      --  Allocated size of X1 buffer in 16-bit words
      X1_BUF_SIZE    : X1BUFCFG_X1_BUF_SIZE_Field := 16#0#;
      --  unspecified
      Reserved_16_23 : HAL.UInt8 := 16#0#;
      --  Watermark for buffer full flag
      FULL_WM        : X1BUFCFG_FULL_WM_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for X1BUFCFG_Register use record
      X1_BASE        at 0 range 0 .. 7;
      X1_BUF_SIZE    at 0 range 8 .. 15;
      Reserved_16_23 at 0 range 16 .. 23;
      FULL_WM        at 0 range 24 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype X2BUFCFG_X2_BASE_Field is HAL.UInt8;
   subtype X2BUFCFG_X2_BUF_SIZE_Field is HAL.UInt8;

   --  FMAC X2 Buffer Configuration register
   type X2BUFCFG_Register is record
      --  Base address of X2 buffer
      X2_BASE        : X2BUFCFG_X2_BASE_Field := 16#0#;
      --  Size of X2 buffer in 16-bit words
      X2_BUF_SIZE    : X2BUFCFG_X2_BUF_SIZE_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for X2BUFCFG_Register use record
      X2_BASE        at 0 range 0 .. 7;
      X2_BUF_SIZE    at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype YBUFCFG_Y_BASE_Field is HAL.UInt8;
   subtype YBUFCFG_Y_BUF_SIZE_Field is HAL.UInt8;
   subtype YBUFCFG_EMPTY_WM_Field is HAL.UInt2;

   --  FMAC Y Buffer Configuration register
   type YBUFCFG_Register is record
      --  Base address of Y buffer
      Y_BASE         : YBUFCFG_Y_BASE_Field := 16#0#;
      --  Size of Y buffer in 16-bit words
      Y_BUF_SIZE     : YBUFCFG_Y_BUF_SIZE_Field := 16#0#;
      --  unspecified
      Reserved_16_23 : HAL.UInt8 := 16#0#;
      --  Watermark for buffer empty flag
      EMPTY_WM       : YBUFCFG_EMPTY_WM_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for YBUFCFG_Register use record
      Y_BASE         at 0 range 0 .. 7;
      Y_BUF_SIZE     at 0 range 8 .. 15;
      Reserved_16_23 at 0 range 16 .. 23;
      EMPTY_WM       at 0 range 24 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype PARAM_P_Field is HAL.UInt8;
   subtype PARAM_Q_Field is HAL.UInt8;
   subtype PARAM_R_Field is HAL.UInt8;
   subtype PARAM_FUNC_Field is HAL.UInt7;

   --  FMAC Parameter register
   type PARAM_Register is record
      --  Input parameter P
      P     : PARAM_P_Field := 16#0#;
      --  Input parameter Q
      Q     : PARAM_Q_Field := 16#0#;
      --  Input parameter R
      R     : PARAM_R_Field := 16#0#;
      --  Function
      FUNC  : PARAM_FUNC_Field := 16#0#;
      --  Enable execution
      START : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PARAM_Register use record
      P     at 0 range 0 .. 7;
      Q     at 0 range 8 .. 15;
      R     at 0 range 16 .. 23;
      FUNC  at 0 range 24 .. 30;
      START at 0 range 31 .. 31;
   end record;

   --  FMAC Control register
   type CR_Register is record
      --  Enable read interrupt
      RIEN           : Boolean := False;
      --  Enable write interrupt
      WIEN           : Boolean := False;
      --  Enable overflow error interrupts
      OVFLIEN        : Boolean := False;
      --  Enable underflow error interrupts
      UNFLIEN        : Boolean := False;
      --  Enable saturation error interrupts
      SATIEN         : Boolean := False;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  Enable DMA read channel requests
      DMAREN         : Boolean := False;
      --  Enable DMA write channel requests
      DMAWEN         : Boolean := False;
      --  unspecified
      Reserved_10_14 : HAL.UInt5 := 16#0#;
      --  Enable clipping
      CLIPEN         : Boolean := False;
      --  Reset FMAC unit
      RESET          : Boolean := False;
      --  unspecified
      Reserved_17_31 : HAL.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      RIEN           at 0 range 0 .. 0;
      WIEN           at 0 range 1 .. 1;
      OVFLIEN        at 0 range 2 .. 2;
      UNFLIEN        at 0 range 3 .. 3;
      SATIEN         at 0 range 4 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      DMAREN         at 0 range 8 .. 8;
      DMAWEN         at 0 range 9 .. 9;
      Reserved_10_14 at 0 range 10 .. 14;
      CLIPEN         at 0 range 15 .. 15;
      RESET          at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   --  FMAC Status register
   type SR_Register is record
      --  Read-only. Y buffer empty flag
      YEMPTY         : Boolean;
      --  Read-only. X1 buffer full flag
      X1FULL         : Boolean;
      --  unspecified
      Reserved_2_7   : HAL.UInt6;
      --  Read-only. Overflow error flag
      OVFL           : Boolean;
      --  Read-only. Underflow error flag
      UNFL           : Boolean;
      --  Read-only. Saturation error flag
      SAT            : Boolean;
      --  unspecified
      Reserved_11_31 : HAL.UInt21;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register use record
      YEMPTY         at 0 range 0 .. 0;
      X1FULL         at 0 range 1 .. 1;
      Reserved_2_7   at 0 range 2 .. 7;
      OVFL           at 0 range 8 .. 8;
      UNFL           at 0 range 9 .. 9;
      SAT            at 0 range 10 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   subtype WDATA_WDATA_Field is HAL.UInt16;

   --  FMAC Write Data register
   type WDATA_Register is record
      --  Write-only. Write data
      WDATA          : WDATA_WDATA_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for WDATA_Register use record
      WDATA          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype RDATA_RDATA_Field is HAL.UInt16;

   --  FMAC Read Data register
   type RDATA_Register is record
      --  Read-only. Read data
      RDATA          : RDATA_RDATA_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RDATA_Register use record
      RDATA          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Filter Math Accelerator
   type FMAC_Peripheral is record
      --  FMAC X1 Buffer Configuration register
      X1BUFCFG : aliased X1BUFCFG_Register;
      --  FMAC X2 Buffer Configuration register
      X2BUFCFG : aliased X2BUFCFG_Register;
      --  FMAC Y Buffer Configuration register
      YBUFCFG  : aliased YBUFCFG_Register;
      --  FMAC Parameter register
      PARAM    : aliased PARAM_Register;
      --  FMAC Control register
      CR       : aliased CR_Register;
      --  FMAC Status register
      SR       : aliased SR_Register;
      --  FMAC Write Data register
      WDATA    : aliased WDATA_Register;
      --  FMAC Read Data register
      RDATA    : aliased RDATA_Register;
   end record
     with Volatile;

   for FMAC_Peripheral use record
      X1BUFCFG at 16#0# range 0 .. 31;
      X2BUFCFG at 16#4# range 0 .. 31;
      YBUFCFG  at 16#8# range 0 .. 31;
      PARAM    at 16#C# range 0 .. 31;
      CR       at 16#10# range 0 .. 31;
      SR       at 16#14# range 0 .. 31;
      WDATA    at 16#18# range 0 .. 31;
      RDATA    at 16#1C# range 0 .. 31;
   end record;

   --  Filter Math Accelerator
   FMAC_Periph : aliased FMAC_Peripheral
     with Import, Address => FMAC_Base;

   --  Filter Math Accelerator
   SEC_FMAC_Periph : aliased FMAC_Peripheral
     with Import, Address => SEC_FMAC_Base;

end STM32_SVD.FMAC;
