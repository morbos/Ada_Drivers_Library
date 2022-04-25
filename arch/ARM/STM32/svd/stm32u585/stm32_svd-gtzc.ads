--  This spec has been automatically generated from STM32U5xx.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.GTZC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  MPCBB control register
   type MPCBB1_CR_Register is record
      --  lock the control register of the MPCBB until next reset
      GLOCK         : Boolean := False;
      --  unspecified
      Reserved_1_29 : HAL.UInt29 := 16#0#;
      --  SRAMx clocks security state
      INVSECSTATE   : Boolean := False;
      --  secure read/write illegal access disable
      SRWILADIS     : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_CR_Register use record
      GLOCK         at 0 range 0 .. 0;
      Reserved_1_29 at 0 range 1 .. 29;
      INVSECSTATE   at 0 range 30 .. 30;
      SRWILADIS     at 0 range 31 .. 31;
   end record;

   --  MPCBB1_CFGLOCKR1_SPLCK array
   type MPCBB1_CFGLOCKR1_SPLCK_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  GTZC1 SRAMz MPCBB configuration lock register
   type MPCBB1_CFGLOCKR1_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SPLCK as a value
            Val : HAL.UInt32;
         when True =>
            --  SPLCK as an array
            Arr : MPCBB1_CFGLOCKR1_SPLCK_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_CFGLOCKR1_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_SECCFGR_SEC array
   type MPCBB1_SECCFGR_SEC_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx security configuration for super-block x register
   type MPCBB1_SECCFGR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SEC as a value
            Val : HAL.UInt32;
         when True =>
            --  SEC as an array
            Arr : MPCBB1_SECCFGR_SEC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_SECCFGR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_PRIVCFGR_PRIV array
   type MPCBB1_PRIVCFGR_PRIV_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBB privileged configuration for super-block x register
   type MPCBB1_PRIVCFGR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PRIV as a value
            Val : HAL.UInt32;
         when True =>
            --  PRIV as an array
            Arr : MPCBB1_PRIVCFGR_PRIV_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_PRIVCFGR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB control register
   type MPCBB2_CR_Register is record
      --  lock the control register of the MPCBB until next reset
      GLOCK         : Boolean := False;
      --  unspecified
      Reserved_1_29 : HAL.UInt29 := 16#0#;
      --  SRAMx clocks security state
      INVSECSTATE   : Boolean := False;
      --  secure read/write illegal access disable
      SRWILADIS     : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_CR_Register use record
      GLOCK         at 0 range 0 .. 0;
      Reserved_1_29 at 0 range 1 .. 29;
      INVSECSTATE   at 0 range 30 .. 30;
      SRWILADIS     at 0 range 31 .. 31;
   end record;

   --  MPCBB2_CFGLOCKR1_SPLCK array
   type MPCBB2_CFGLOCKR1_SPLCK_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  GTZC1 SRAMz MPCBB configuration lock register
   type MPCBB2_CFGLOCKR1_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SPLCK as a value
            Val : HAL.UInt32;
         when True =>
            --  SPLCK as an array
            Arr : MPCBB2_CFGLOCKR1_SPLCK_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_CFGLOCKR1_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_SECCFGR_SEC array
   type MPCBB2_SECCFGR_SEC_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx security configuration for super-block x register
   type MPCBB2_SECCFGR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SEC as a value
            Val : HAL.UInt32;
         when True =>
            --  SEC as an array
            Arr : MPCBB2_SECCFGR_SEC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_SECCFGR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_PRIVCFGR_PRIV array
   type MPCBB2_PRIVCFGR_PRIV_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBB privileged configuration for super-block x register
   type MPCBB2_PRIVCFGR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PRIV as a value
            Val : HAL.UInt32;
         when True =>
            --  PRIV as an array
            Arr : MPCBB2_PRIVCFGR_PRIV_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_PRIVCFGR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB control register
   type MPCBB3_CR_Register is record
      --  lock the control register of the MPCBB until next reset
      GLOCK         : Boolean := False;
      --  unspecified
      Reserved_1_29 : HAL.UInt29 := 16#0#;
      --  SRAMx clocks security state
      INVSECSTATE   : Boolean := False;
      --  secure read/write illegal access disable
      SRWILADIS     : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPCBB3_CR_Register use record
      GLOCK         at 0 range 0 .. 0;
      Reserved_1_29 at 0 range 1 .. 29;
      INVSECSTATE   at 0 range 30 .. 30;
      SRWILADIS     at 0 range 31 .. 31;
   end record;

   --  MPCBB3_CFGLOCKR1_SPLCK array
   type MPCBB3_CFGLOCKR1_SPLCK_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  GTZC1 SRAMz MPCBB configuration lock register
   type MPCBB3_CFGLOCKR1_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SPLCK as a value
            Val : HAL.UInt32;
         when True =>
            --  SPLCK as an array
            Arr : MPCBB3_CFGLOCKR1_SPLCK_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPCBB3_CFGLOCKR1_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB3_SECCFGR_SEC array
   type MPCBB3_SECCFGR_SEC_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx security configuration for super-block x register
   type MPCBB3_SECCFGR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SEC as a value
            Val : HAL.UInt32;
         when True =>
            --  SEC as an array
            Arr : MPCBB3_SECCFGR_SEC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPCBB3_SECCFGR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB3_PRIVCFGR_PRIV array
   type MPCBB3_PRIVCFGR_PRIV_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBB privileged configuration for super-block x register
   type MPCBB3_PRIVCFGR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PRIV as a value
            Val : HAL.UInt32;
         when True =>
            --  PRIV as an array
            Arr : MPCBB3_PRIVCFGR_PRIV_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPCBB3_PRIVCFGR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  TZIC interrupt enable register 1
   type IER1_Register is record
      --  TIM2IE
      TIM2IE         : Boolean := False;
      --  TIM3IE
      TIM3IE         : Boolean := False;
      --  TIM4IE
      TIM4IE         : Boolean := False;
      --  TIM5IE
      TIM5IE         : Boolean := False;
      --  TIM6IE
      TIM6IE         : Boolean := False;
      --  TIM7IE
      TIM7IE         : Boolean := False;
      --  WWDGIE
      WWDGIE         : Boolean := False;
      --  IWDGIE
      IWDGIE         : Boolean := False;
      --  SPI2IE
      SPI2IE         : Boolean := False;
      --  illegal access interrupt enable for USART2
      USART2IE       : Boolean := False;
      --  illegal access interrupt enable for USART3
      USART3IE       : Boolean := False;
      --  illegal access interrupt enable for UART4
      USART4IE       : Boolean := False;
      --  illegal access interrupt enable for UART5
      UART5IE        : Boolean := False;
      --  illegal access interrupt enable for I2C1
      I2C1IE         : Boolean := False;
      --  illegal access interrupt enable for I2C2
      I2C2IE         : Boolean := False;
      --  illegal access interrupt enable for CRS
      CRSIE          : Boolean := False;
      --  illegal access interrupt enable for I2C4
      I2C4IE         : Boolean := False;
      --  illegal access interrupt enable for LPTIM2
      LPTIM2IE       : Boolean := False;
      --  illegal access interrupt enable for FDCAN1
      FDCAN1IE       : Boolean := False;
      --  illegal access interrupt enable for UCPD1
      UCPD1IE        : Boolean := False;
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IER1_Register use record
      TIM2IE         at 0 range 0 .. 0;
      TIM3IE         at 0 range 1 .. 1;
      TIM4IE         at 0 range 2 .. 2;
      TIM5IE         at 0 range 3 .. 3;
      TIM6IE         at 0 range 4 .. 4;
      TIM7IE         at 0 range 5 .. 5;
      WWDGIE         at 0 range 6 .. 6;
      IWDGIE         at 0 range 7 .. 7;
      SPI2IE         at 0 range 8 .. 8;
      USART2IE       at 0 range 9 .. 9;
      USART3IE       at 0 range 10 .. 10;
      USART4IE       at 0 range 11 .. 11;
      UART5IE        at 0 range 12 .. 12;
      I2C1IE         at 0 range 13 .. 13;
      I2C2IE         at 0 range 14 .. 14;
      CRSIE          at 0 range 15 .. 15;
      I2C4IE         at 0 range 16 .. 16;
      LPTIM2IE       at 0 range 17 .. 17;
      FDCAN1IE       at 0 range 18 .. 18;
      UCPD1IE        at 0 range 19 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  TZIC interrupt enable register 2
   type IER2_Register is record
      --  illegal access interrupt enable for TIM1
      TIM1IE        : Boolean := False;
      --  illegal access interrupt enable for SPI1
      SPI1IE        : Boolean := False;
      --  illegal access interrupt enable for TIM8
      TIM8IE        : Boolean := False;
      --  illegal access interrupt enable for USART1
      USART1IE      : Boolean := False;
      --  illegal access interrupt enable for TIM5
      TIM15IE       : Boolean := False;
      --  illegal access interrupt enable for TIM6
      TIM16IE       : Boolean := False;
      --  illegal access interrupt enable for TIM7
      TIM17IE       : Boolean := False;
      --  illegal access interrupt enable for SAI1
      SAI1IE        : Boolean := False;
      --  illegal access interrupt enable for SAI2
      SAI2IE        : Boolean := False;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IER2_Register use record
      TIM1IE        at 0 range 0 .. 0;
      SPI1IE        at 0 range 1 .. 1;
      TIM8IE        at 0 range 2 .. 2;
      USART1IE      at 0 range 3 .. 3;
      TIM15IE       at 0 range 4 .. 4;
      TIM16IE       at 0 range 5 .. 5;
      TIM17IE       at 0 range 6 .. 6;
      SAI1IE        at 0 range 7 .. 7;
      SAI2IE        at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   --  TZIC interrupt enable register 3
   type IER3_Register is record
      --  illegal access interrupt enable for MDF1
      MDF1IE         : Boolean := False;
      --  illegal access interrupt enable for CORDIC
      CORDICIE       : Boolean := False;
      --  illegal access interrupt enable for FMAC
      FMACIE         : Boolean := False;
      --  illegal access interrupt enable for CRC
      CRCIE          : Boolean := False;
      --  illegal access interrupt enable for TSC
      TSCIE          : Boolean := False;
      --  illegal access interrupt enable for register of DMA2D
      DMA2DIE        : Boolean := False;
      --  illegal access interrupt enable for ICACHE registers
      ICACHEIE       : Boolean := False;
      --  illegal access interrupt enable for DCACHE registers
      DCACHEIE       : Boolean := False;
      --  illegal access interrupt enable for ADC1
      ADC1IE         : Boolean := False;
      --  illegal access interrupt enable for DCMI
      DCMIIE         : Boolean := False;
      --  illegal access interrupt enable for OTG_FS
      OTGFSIE        : Boolean := False;
      --  illegal access interrupt enable for AES
      AESIE          : Boolean := False;
      --  illegal access interrupt enable for HASH
      HASHIE         : Boolean := False;
      --  illegal access interrupt enable for RNG
      RNGIE          : Boolean := False;
      --  illegal access interrupt enable for PKA
      PKAIE          : Boolean := False;
      --  illegal access interrupt enable for SAES
      SAESIE         : Boolean := False;
      --  illegal access interrupt enable for OCTOSPIM
      OCTOSPIMIE     : Boolean := False;
      --  illegal access interrupt enable for SDMMC2
      SDMMC1IE       : Boolean := False;
      --  illegal access interrupt enable for SDMMC1
      SDMMC2IE       : Boolean := False;
      --  illegal access interrupt enable for FSMC registers
      FSMCIE         : Boolean := False;
      --  illegal access interrupt enable for OCTOSPI1 registers
      OCTOSPI1IE     : Boolean := False;
      --  illegal access interrupt enable for OCTOSPI2 registers
      OCTOSPI2IE     : Boolean := False;
      --  illegal access interrupt enable for RAMCFG
      RAMCFGIE       : Boolean := False;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IER3_Register use record
      MDF1IE         at 0 range 0 .. 0;
      CORDICIE       at 0 range 1 .. 1;
      FMACIE         at 0 range 2 .. 2;
      CRCIE          at 0 range 3 .. 3;
      TSCIE          at 0 range 4 .. 4;
      DMA2DIE        at 0 range 5 .. 5;
      ICACHEIE       at 0 range 6 .. 6;
      DCACHEIE       at 0 range 7 .. 7;
      ADC1IE         at 0 range 8 .. 8;
      DCMIIE         at 0 range 9 .. 9;
      OTGFSIE        at 0 range 10 .. 10;
      AESIE          at 0 range 11 .. 11;
      HASHIE         at 0 range 12 .. 12;
      RNGIE          at 0 range 13 .. 13;
      PKAIE          at 0 range 14 .. 14;
      SAESIE         at 0 range 15 .. 15;
      OCTOSPIMIE     at 0 range 16 .. 16;
      SDMMC1IE       at 0 range 17 .. 17;
      SDMMC2IE       at 0 range 18 .. 18;
      FSMCIE         at 0 range 19 .. 19;
      OCTOSPI1IE     at 0 range 20 .. 20;
      OCTOSPI2IE     at 0 range 21 .. 21;
      RAMCFGIE       at 0 range 22 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  TZIC interrupt enable register 4
   type IER4_Register is record
      --  illegal access interrupt enable for GPDMA1
      GPDMA1IE       : Boolean := False;
      --  illegal access interrupt enable for FLASH memory
      FLASHIE        : Boolean := False;
      --  illegal access interrupt enable for FLASH registers
      FLASH_REGIE    : Boolean := False;
      --  illegal access interrupt enable for OTFDEC1
      OTFDEC1IE      : Boolean := False;
      --  illegal access interrupt enable for OTFDEC2
      OTFDEC2IE      : Boolean := False;
      --  unspecified
      Reserved_5_13  : HAL.UInt9 := 16#0#;
      --  illegal access interrupt enable for GTZC1 TZSC registers
      TZSC1IE        : Boolean := False;
      --  illegal access interrupt enable for GTZC1 TZIC registers
      TZIC1IE        : Boolean := False;
      --  illegal access interrupt enable for MPCWM1 (OCTOSPI1) memory bank
      OCTOSPI1_MEMIE : Boolean := False;
      --  illegal access interrupt enable for MPCWM2 (FSMC NAND) and MPCWM3
      FSMC_MEMIE     : Boolean := False;
      --  illegal access interrupt enable for MPCWM3 (BKPSRAM) memory bank
      BKPSRAMIE      : Boolean := False;
      --  illegal access interrupt enable for OCTOSPI2 memory bank
      OCTOSPI2_MEMIE : Boolean := False;
      --  unspecified
      Reserved_20_23 : HAL.UInt4 := 16#0#;
      --  illegal access interrupt enable for SRAM1
      SRAM1IE        : Boolean := False;
      --  illegal access interrupt enable for MPCBB1 registers
      MPCBB1_REGIE   : Boolean := False;
      --  illegal access interrupt enable for SRAM2
      SRAM2IE        : Boolean := False;
      --  illegal access interrupt enable for MPCBB2 registers
      MPCBB2_REGIE   : Boolean := False;
      --  illegal access interrupt enable for SRAM3
      SRAM3IE        : Boolean := False;
      --  illegal access interrupt enable for MPCBB3 registers
      MPCBB3_REGIE   : Boolean := False;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IER4_Register use record
      GPDMA1IE       at 0 range 0 .. 0;
      FLASHIE        at 0 range 1 .. 1;
      FLASH_REGIE    at 0 range 2 .. 2;
      OTFDEC1IE      at 0 range 3 .. 3;
      OTFDEC2IE      at 0 range 4 .. 4;
      Reserved_5_13  at 0 range 5 .. 13;
      TZSC1IE        at 0 range 14 .. 14;
      TZIC1IE        at 0 range 15 .. 15;
      OCTOSPI1_MEMIE at 0 range 16 .. 16;
      FSMC_MEMIE     at 0 range 17 .. 17;
      BKPSRAMIE      at 0 range 18 .. 18;
      OCTOSPI2_MEMIE at 0 range 19 .. 19;
      Reserved_20_23 at 0 range 20 .. 23;
      SRAM1IE        at 0 range 24 .. 24;
      MPCBB1_REGIE   at 0 range 25 .. 25;
      SRAM2IE        at 0 range 26 .. 26;
      MPCBB2_REGIE   at 0 range 27 .. 27;
      SRAM3IE        at 0 range 28 .. 28;
      MPCBB3_REGIE   at 0 range 29 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   --  TZIC status register 1
   type SR1_Register is record
      --  Read-only. illegal access flag for TIM2
      TIM2F          : Boolean;
      --  Read-only. illegal access flag for TIM3
      TIM3F          : Boolean;
      --  Read-only. illegal access flag for TIM4
      TIM4F          : Boolean;
      --  Read-only. illegal access flag for TIM5
      TIM5F          : Boolean;
      --  Read-only. illegal access flag for TIM6
      TIM6F          : Boolean;
      --  Read-only. illegal access flag for TIM7
      TIM7F          : Boolean;
      --  Read-only. illegal access flag for WWDG
      WWDGF          : Boolean;
      --  Read-only. illegal access flag for IWDG
      IWDGF          : Boolean;
      --  Read-only. illegal access flag for SPI2
      SPI2F          : Boolean;
      --  Read-only. illegal access flag for USART2
      USART2F        : Boolean;
      --  Read-only. illegal access flag for USART3
      USART3F        : Boolean;
      --  Read-only. illegal access flag for UART4
      UART4F         : Boolean;
      --  Read-only. illegal access flag for UART5
      UART5F         : Boolean;
      --  Read-only. illegal access flag for I2C1
      I2C1F          : Boolean;
      --  Read-only. illegal access flag for I2C2
      I2C2F          : Boolean;
      --  Read-only. illegal access flag for CRS
      CRSF           : Boolean;
      --  Read-only. illegal access flag for I2C4
      I2C4F          : Boolean;
      --  Read-only. illegal access flag for LPTIM2
      LPTIM2F        : Boolean;
      --  Read-only. illegal access flag for FDCAN1
      FDCAN1F        : Boolean;
      --  Read-only. illegal access flag for UCPD1
      UCPD1F         : Boolean;
      --  unspecified
      Reserved_20_31 : HAL.UInt12;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR1_Register use record
      TIM2F          at 0 range 0 .. 0;
      TIM3F          at 0 range 1 .. 1;
      TIM4F          at 0 range 2 .. 2;
      TIM5F          at 0 range 3 .. 3;
      TIM6F          at 0 range 4 .. 4;
      TIM7F          at 0 range 5 .. 5;
      WWDGF          at 0 range 6 .. 6;
      IWDGF          at 0 range 7 .. 7;
      SPI2F          at 0 range 8 .. 8;
      USART2F        at 0 range 9 .. 9;
      USART3F        at 0 range 10 .. 10;
      UART4F         at 0 range 11 .. 11;
      UART5F         at 0 range 12 .. 12;
      I2C1F          at 0 range 13 .. 13;
      I2C2F          at 0 range 14 .. 14;
      CRSF           at 0 range 15 .. 15;
      I2C4F          at 0 range 16 .. 16;
      LPTIM2F        at 0 range 17 .. 17;
      FDCAN1F        at 0 range 18 .. 18;
      UCPD1F         at 0 range 19 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  TZIC status register 2
   type SR2_Register is record
      --  Read-only. illegal access flag for TIM1
      TIM1F         : Boolean;
      --  Read-only. illegal access flag for SPI1
      SPI1F         : Boolean;
      --  Read-only. illegal access flag for TIM8
      TIM8F         : Boolean;
      --  Read-only. illegal access flag for USART1
      USART1F       : Boolean;
      --  Read-only. illegal access flag for TIM5
      TIM15F        : Boolean;
      --  Read-only. illegal access flag for TIM6
      TIM16F        : Boolean;
      --  Read-only. illegal access flag for TIM7
      TIM17F        : Boolean;
      --  Read-only. illegal access flag for SAI1
      SAI1F         : Boolean;
      --  Read-only. illegal access flag for SAI2
      SAI2F         : Boolean;
      --  unspecified
      Reserved_9_31 : HAL.UInt23;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR2_Register use record
      TIM1F         at 0 range 0 .. 0;
      SPI1F         at 0 range 1 .. 1;
      TIM8F         at 0 range 2 .. 2;
      USART1F       at 0 range 3 .. 3;
      TIM15F        at 0 range 4 .. 4;
      TIM16F        at 0 range 5 .. 5;
      TIM17F        at 0 range 6 .. 6;
      SAI1F         at 0 range 7 .. 7;
      SAI2F         at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   --  TZIC status register 3
   type SR3_Register is record
      --  Read-only. illegal access flag for MDF1
      MDF1F          : Boolean;
      --  Read-only. illegal access flag for CORDIC
      CORDICF        : Boolean;
      --  Read-only. illegal access flag for FMAC
      FMACF          : Boolean;
      --  Read-only. illegal access flag for CRC
      CRCF           : Boolean;
      --  Read-only. illegal access flag for TSC
      TSCF           : Boolean;
      --  Read-only. illegal access flag for register of DMA2D
      DMA2DF         : Boolean;
      --  Read-only. illegal access flag for ICACHE registers
      ICACHEF        : Boolean;
      --  Read-only. illegal access flag for DCACHE registers
      DCACHEF        : Boolean;
      --  Read-only. illegal access flag for ADC1
      ADC1F          : Boolean;
      --  Read-only. illegal access flag for DCMI
      DCMIF          : Boolean;
      --  Read-only. illegal access flag for OTG_FS
      OTGFSF         : Boolean;
      --  Read-only. illegal access flag for AES
      AESF           : Boolean;
      --  Read-only. illegal access flag for HASH
      HASHF          : Boolean;
      --  Read-only. illegal access flag for RNG
      RNGF           : Boolean;
      --  Read-only. illegal access flag for PKA
      PKAF           : Boolean;
      --  Read-only. illegal access flag for SAES
      SAESF          : Boolean;
      --  Read-only. illegal access flag for OCTOSPIM
      OCTOSPIMF      : Boolean;
      --  Read-only. illegal access flag for SDMMC2
      SDMMC1F        : Boolean;
      --  Read-only. illegal access flag for SDMMC1
      SDMMC2F        : Boolean;
      --  Read-only. illegal access flag for FSMC registers
      FSMCF          : Boolean;
      --  Read-only. illegal access flag for OCTOSPI1 registers
      OCTOSPI1F      : Boolean;
      --  Read-only. illegal access flag for OCTOSPI2 registers
      OCTOSPI2F      : Boolean;
      --  Read-only. illegal access flag for RAMCFG
      RAMCFGF        : Boolean;
      --  unspecified
      Reserved_23_31 : HAL.UInt9;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR3_Register use record
      MDF1F          at 0 range 0 .. 0;
      CORDICF        at 0 range 1 .. 1;
      FMACF          at 0 range 2 .. 2;
      CRCF           at 0 range 3 .. 3;
      TSCF           at 0 range 4 .. 4;
      DMA2DF         at 0 range 5 .. 5;
      ICACHEF        at 0 range 6 .. 6;
      DCACHEF        at 0 range 7 .. 7;
      ADC1F          at 0 range 8 .. 8;
      DCMIF          at 0 range 9 .. 9;
      OTGFSF         at 0 range 10 .. 10;
      AESF           at 0 range 11 .. 11;
      HASHF          at 0 range 12 .. 12;
      RNGF           at 0 range 13 .. 13;
      PKAF           at 0 range 14 .. 14;
      SAESF          at 0 range 15 .. 15;
      OCTOSPIMF      at 0 range 16 .. 16;
      SDMMC1F        at 0 range 17 .. 17;
      SDMMC2F        at 0 range 18 .. 18;
      FSMCF          at 0 range 19 .. 19;
      OCTOSPI1F      at 0 range 20 .. 20;
      OCTOSPI2F      at 0 range 21 .. 21;
      RAMCFGF        at 0 range 22 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  TZIC status register 4
   type SR4_Register is record
      --  Read-only. illegal access flag for GPDMA1
      GPDMA1F        : Boolean;
      --  Read-only. illegal access flag for FLASH memory
      FLASHF         : Boolean;
      --  Read-only. illegal access flag for FLASH registers
      FLASH_REGF     : Boolean;
      --  Read-only. illegal access flag for OTFDEC1
      OTFDEC1F       : Boolean;
      --  Read-only. illegal access flag for OTFDEC2
      OTFDEC2F       : Boolean;
      --  unspecified
      Reserved_5_13  : HAL.UInt9;
      --  Read-only. illegal access flag for GTZC1 TZSC registers
      TZSC1F         : Boolean;
      --  Read-only. illegal access flag for GTZC1 TZIC registers
      TZIC1F         : Boolean;
      --  Read-only. illegal access flag for MPCWM1 (OCTOSPI1) memory bank
      OCTOSPI1_MEMF  : Boolean;
      --  Read-only. illegal access flag for MPCWM2 (FSMC NAND) and MPCWM3
      --  (FSMC NOR)
      FSMC_MEMF      : Boolean;
      --  Read-only. illegal access flag for MPCWM3 (BKPSRAM) memory bank
      BKPSRAMF       : Boolean;
      --  Read-only. illegal access flag for OCTOSPI2 memory bank
      OCTOSPI2_MEMF  : Boolean;
      --  unspecified
      Reserved_20_23 : HAL.UInt4;
      --  Read-only. illegal access flag for SRAM1
      SRAM1F         : Boolean;
      --  Read-only. illegal access flag for MPCBB1 registers
      MPCBB1_REGF    : Boolean;
      --  Read-only. illegal access flag for SRAM2
      SRAM2F         : Boolean;
      --  Read-only. illegal access flag for MPCBB2 registers
      MPCBB2_REGF    : Boolean;
      --  Read-only. illegal access flag for SRAM3
      SRAM3F         : Boolean;
      --  Read-only. illegal access flag for MPCBB3 registers
      MPCBB3_REGF    : Boolean;
      --  unspecified
      Reserved_30_31 : HAL.UInt2;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR4_Register use record
      GPDMA1F        at 0 range 0 .. 0;
      FLASHF         at 0 range 1 .. 1;
      FLASH_REGF     at 0 range 2 .. 2;
      OTFDEC1F       at 0 range 3 .. 3;
      OTFDEC2F       at 0 range 4 .. 4;
      Reserved_5_13  at 0 range 5 .. 13;
      TZSC1F         at 0 range 14 .. 14;
      TZIC1F         at 0 range 15 .. 15;
      OCTOSPI1_MEMF  at 0 range 16 .. 16;
      FSMC_MEMF      at 0 range 17 .. 17;
      BKPSRAMF       at 0 range 18 .. 18;
      OCTOSPI2_MEMF  at 0 range 19 .. 19;
      Reserved_20_23 at 0 range 20 .. 23;
      SRAM1F         at 0 range 24 .. 24;
      MPCBB1_REGF    at 0 range 25 .. 25;
      SRAM2F         at 0 range 26 .. 26;
      MPCBB2_REGF    at 0 range 27 .. 27;
      SRAM3F         at 0 range 28 .. 28;
      MPCBB3_REGF    at 0 range 29 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   --  TZIC flag clear register 1
   type FCR1_Register is record
      --  Write-only. clear the illegal access flag for TIM2
      CTIM2F         : Boolean := False;
      --  Write-only. clear the illegal access flag for TIM3
      CTIM3F         : Boolean := False;
      --  Write-only. clear the illegal access flag for TIM4
      CTIM4F         : Boolean := False;
      --  Write-only. clear the illegal access flag for TIM5
      CTIM5F         : Boolean := False;
      --  Write-only. clear the illegal access flag for TIM6
      CTIM6F         : Boolean := False;
      --  Write-only. clear the illegal access flag for TIM7
      CTIM7F         : Boolean := False;
      --  Write-only. clear the illegal access flag for WWDG
      CWWDGF         : Boolean := False;
      --  Write-only. clear the illegal access flag for IWDG
      CIWDGF         : Boolean := False;
      --  Write-only. clear the illegal access flag for SPI2
      CSPI2F         : Boolean := False;
      --  Write-only. clear the illegal access flag for USART2
      CUSART2F       : Boolean := False;
      --  Write-only. clear the illegal access flag for USART3
      CUSART3F       : Boolean := False;
      --  Write-only. clear the illegal access flag for UART4
      CUART4F        : Boolean := False;
      --  Write-only. clear the illegal access flag for UART5
      CUART5F        : Boolean := False;
      --  Write-only. clear the illegal access flag for I2C1
      CI2C1F         : Boolean := False;
      --  Write-only. clear the illegal access flag for I2C2
      CI2C2F         : Boolean := False;
      --  Write-only. clear the illegal access flag for CRS
      CCRSF          : Boolean := False;
      --  Write-only. clear the illegal access flag for I2C4
      CI2C4F         : Boolean := False;
      --  Write-only. clear the illegal access flag for LPTIM2
      CLPTIM2F       : Boolean := False;
      --  Write-only. clear the illegal access flag for FDCAN1
      CFDCAN1F       : Boolean := False;
      --  Write-only. clear the illegal access flag for UCPD1
      CUCPD1F        : Boolean := False;
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FCR1_Register use record
      CTIM2F         at 0 range 0 .. 0;
      CTIM3F         at 0 range 1 .. 1;
      CTIM4F         at 0 range 2 .. 2;
      CTIM5F         at 0 range 3 .. 3;
      CTIM6F         at 0 range 4 .. 4;
      CTIM7F         at 0 range 5 .. 5;
      CWWDGF         at 0 range 6 .. 6;
      CIWDGF         at 0 range 7 .. 7;
      CSPI2F         at 0 range 8 .. 8;
      CUSART2F       at 0 range 9 .. 9;
      CUSART3F       at 0 range 10 .. 10;
      CUART4F        at 0 range 11 .. 11;
      CUART5F        at 0 range 12 .. 12;
      CI2C1F         at 0 range 13 .. 13;
      CI2C2F         at 0 range 14 .. 14;
      CCRSF          at 0 range 15 .. 15;
      CI2C4F         at 0 range 16 .. 16;
      CLPTIM2F       at 0 range 17 .. 17;
      CFDCAN1F       at 0 range 18 .. 18;
      CUCPD1F        at 0 range 19 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  TZIC flag clear register 2
   type FCR2_Register is record
      --  Write-only. clear the illegal access flag for TIM1
      CTIM1F        : Boolean := False;
      --  Write-only. clear the illegal access flag for SPI1
      CSPI1F        : Boolean := False;
      --  Write-only. clear the illegal access flag for TIM8
      CTIM8F        : Boolean := False;
      --  Write-only. clear the illegal access flag for USART1
      CUSART1F      : Boolean := False;
      --  Write-only. clear the illegal access flag for TIM5
      CTIM15F       : Boolean := False;
      --  Write-only. clear the illegal access flag for TIM6
      CTIM16F       : Boolean := False;
      --  Write-only. clear the illegal access flag for TIM7
      CTIM17F       : Boolean := False;
      --  Write-only. clear the illegal access flag for SAI1
      CSAI1F        : Boolean := False;
      --  Write-only. clear the illegal access flag for SAI2
      CSAI2F        : Boolean := False;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FCR2_Register use record
      CTIM1F        at 0 range 0 .. 0;
      CSPI1F        at 0 range 1 .. 1;
      CTIM8F        at 0 range 2 .. 2;
      CUSART1F      at 0 range 3 .. 3;
      CTIM15F       at 0 range 4 .. 4;
      CTIM16F       at 0 range 5 .. 5;
      CTIM17F       at 0 range 6 .. 6;
      CSAI1F        at 0 range 7 .. 7;
      CSAI2F        at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   --  TZIC flag clear register 3
   type FCR3_Register is record
      --  Write-only. clear the illegal access flag for MDF1
      CMDF1F         : Boolean := False;
      --  Write-only. clear the illegal access flag for CORDIC
      CCORDICF       : Boolean := False;
      --  Write-only. clear the illegal access flag for FMAC
      CFMACF         : Boolean := False;
      --  Write-only. clear the illegal access flag for CRC
      CCRCF          : Boolean := False;
      --  Write-only. clear the illegal access flag for TSC
      CTSCF          : Boolean := False;
      --  Write-only. clear the illegal access flag for register of DMA2D
      CDMA2DF        : Boolean := False;
      --  Write-only. clear the illegal access flag for ICACHE registers
      CICACHEF       : Boolean := False;
      --  Write-only. clear the illegal access flag for DCACHE registers
      CDCACHEF       : Boolean := False;
      --  Write-only. clear the illegal access flag for ADC1
      CADC1F         : Boolean := False;
      --  Write-only. clear the illegal access flag for DCMI
      CDCMIF         : Boolean := False;
      --  Write-only. clear the illegal access flag for OTG_FS
      COTGFSF        : Boolean := False;
      --  Write-only. clear the illegal access flag for AES
      CAESF          : Boolean := False;
      --  Write-only. clear the illegal access flag for HASH
      CHASHF         : Boolean := False;
      --  Write-only. clear the illegal access flag for RNG
      CRNGF          : Boolean := False;
      --  Write-only. clear the illegal access flag for PKA
      CPKAF          : Boolean := False;
      --  Write-only. clear the illegal access flag for SAES
      CSAESF         : Boolean := False;
      --  Write-only. clear the illegal access flag for OCTOSPIM
      COCTOSPIMF     : Boolean := False;
      --  Write-only. clear the illegal access flag for SDMMC2
      CSDMMC1F       : Boolean := False;
      --  Write-only. clear the illegal access flag for SDMMC1
      CSDMMC2F       : Boolean := False;
      --  Write-only. clear the illegal access flag for FSMC registers
      CFSMCF         : Boolean := False;
      --  Write-only. clear the illegal access flag for OCTOSPI1 registers
      COCTOSPI1F     : Boolean := False;
      --  Write-only. clear the illegal access flag for OCTOSPI2 registers
      COCTOSPI2F     : Boolean := False;
      --  Write-only. clear the illegal access flag for RAMCFG
      CRAMCFGF       : Boolean := False;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FCR3_Register use record
      CMDF1F         at 0 range 0 .. 0;
      CCORDICF       at 0 range 1 .. 1;
      CFMACF         at 0 range 2 .. 2;
      CCRCF          at 0 range 3 .. 3;
      CTSCF          at 0 range 4 .. 4;
      CDMA2DF        at 0 range 5 .. 5;
      CICACHEF       at 0 range 6 .. 6;
      CDCACHEF       at 0 range 7 .. 7;
      CADC1F         at 0 range 8 .. 8;
      CDCMIF         at 0 range 9 .. 9;
      COTGFSF        at 0 range 10 .. 10;
      CAESF          at 0 range 11 .. 11;
      CHASHF         at 0 range 12 .. 12;
      CRNGF          at 0 range 13 .. 13;
      CPKAF          at 0 range 14 .. 14;
      CSAESF         at 0 range 15 .. 15;
      COCTOSPIMF     at 0 range 16 .. 16;
      CSDMMC1F       at 0 range 17 .. 17;
      CSDMMC2F       at 0 range 18 .. 18;
      CFSMCF         at 0 range 19 .. 19;
      COCTOSPI1F     at 0 range 20 .. 20;
      COCTOSPI2F     at 0 range 21 .. 21;
      CRAMCFGF       at 0 range 22 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  TZIC flag clear register 3
   type FCR4_Register is record
      --  Write-only. clear the illegal access flag for GPDMA1
      CGPDMA1F       : Boolean := False;
      --  Write-only. clear the illegal access flag for FLASH memory
      CFLASHF        : Boolean := False;
      --  Write-only. clear the illegal access flag for FLASH registers
      CFLASH_REGF    : Boolean := False;
      --  Write-only. clear the illegal access flag for OTFDEC1
      COTFDEC1F      : Boolean := False;
      --  Write-only. clear the illegal access flag for OTFDEC2
      COTFDEC2F      : Boolean := False;
      --  unspecified
      Reserved_5_13  : HAL.UInt9 := 16#0#;
      --  Write-only. clear the illegal access flag for GTZC1 TZSC registers
      CTZSC1F        : Boolean := False;
      --  Write-only. clear the illegal access flag for GTZC1 TZIC registers
      CTZIC1F        : Boolean := False;
      --  Write-only. clear the illegal access flag for MPCWM1 (OCTOSPI1)
      --  memory bank
      COCTOSPI1_MEMF : Boolean := False;
      --  Write-only. clear the illegal access flag for MPCWM2 (FSMC NAND) and
      --  MPCWM3
      CFSMC_MEMF     : Boolean := False;
      --  Write-only. clear the illegal access flag for MPCWM3 (BKPSRAM) memory
      --  bank
      CBKPSRAMF      : Boolean := False;
      --  Write-only. clear the illegal access flag for OCTOSPI2 memory bank
      COCTOSPI2_MEMF : Boolean := False;
      --  unspecified
      Reserved_20_23 : HAL.UInt4 := 16#0#;
      --  Write-only. clear the illegal access flag for SRAM1
      CSRAM1F        : Boolean := False;
      --  Write-only. clear the illegal access flag for MPCBB1 registers
      CMPCBB1_REGF   : Boolean := False;
      --  Write-only. clear the illegal access flag for SRAM2
      CSRAM2F        : Boolean := False;
      --  Write-only. clear the illegal access flag for MPCBB2 registers
      CMPCBB2_REGF   : Boolean := False;
      --  Write-only. clear the illegal access flag for SRAM3
      CSRAM3F        : Boolean := False;
      --  Write-only. clear the illegal access flag for MPCBB3 registers
      CMPCBB3_REGF   : Boolean := False;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FCR4_Register use record
      CGPDMA1F       at 0 range 0 .. 0;
      CFLASHF        at 0 range 1 .. 1;
      CFLASH_REGF    at 0 range 2 .. 2;
      COTFDEC1F      at 0 range 3 .. 3;
      COTFDEC2F      at 0 range 4 .. 4;
      Reserved_5_13  at 0 range 5 .. 13;
      CTZSC1F        at 0 range 14 .. 14;
      CTZIC1F        at 0 range 15 .. 15;
      COCTOSPI1_MEMF at 0 range 16 .. 16;
      CFSMC_MEMF     at 0 range 17 .. 17;
      CBKPSRAMF      at 0 range 18 .. 18;
      COCTOSPI2_MEMF at 0 range 19 .. 19;
      Reserved_20_23 at 0 range 20 .. 23;
      CSRAM1F        at 0 range 24 .. 24;
      CMPCBB1_REGF   at 0 range 25 .. 25;
      CSRAM2F        at 0 range 26 .. 26;
      CMPCBB2_REGF   at 0 range 27 .. 27;
      CSRAM3F        at 0 range 28 .. 28;
      CMPCBB3_REGF   at 0 range 29 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   --  TZSC control register
   type TZSC_CR_Register is record
      --  lock the configuration of GTZC1_TZSC_SECCFGRx and
      --  GTZC1_TZSC_PRIVCFGRx registers until next reset
      LCK           : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_CR_Register use record
      LCK           at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  TZSC secure configuration register 1
   type TZSC_SECCFGR1_Register is record
      --  secure access mode for TIM2
      TIM2SEC        : Boolean := False;
      --  secure access mode for TIM3
      TIM3SEC        : Boolean := False;
      --  secure access mode for TIM4
      TIM4SEC        : Boolean := False;
      --  secure access mode for TIM5
      TIM5SEC        : Boolean := False;
      --  secure access mode for TIM6
      TIM6SEC        : Boolean := False;
      --  secure access mode for TIM7
      TIM7SEC        : Boolean := False;
      --  secure access mode for WWDG
      WWDGSEC        : Boolean := False;
      --  secure access mode for IWDG
      IWDGSEC        : Boolean := False;
      --  secure access mode for SPI2
      SPI2SEC        : Boolean := False;
      --  secure access mode for USART2
      USART2SEC      : Boolean := False;
      --  secure access mode for USART3
      USART3SEC      : Boolean := False;
      --  secure access mode for UART4
      UART4SEC       : Boolean := False;
      --  secure access mode for UART5
      UART5SEC       : Boolean := False;
      --  secure access mode for I2C1
      I2C1SEC        : Boolean := False;
      --  secure access mode for I2C2
      I2C2SEC        : Boolean := False;
      --  secure access mode for CRS
      CRSSEC         : Boolean := False;
      --  secure access mode for I2C4
      I2C4SEC        : Boolean := False;
      --  secure access mode for LPTIM2
      LPTIM2SEC      : Boolean := False;
      --  secure access mode for FDCAN1
      FDCAN1SEC      : Boolean := False;
      --  secure access mode for UCPD1
      UCPD1SEC       : Boolean := False;
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_SECCFGR1_Register use record
      TIM2SEC        at 0 range 0 .. 0;
      TIM3SEC        at 0 range 1 .. 1;
      TIM4SEC        at 0 range 2 .. 2;
      TIM5SEC        at 0 range 3 .. 3;
      TIM6SEC        at 0 range 4 .. 4;
      TIM7SEC        at 0 range 5 .. 5;
      WWDGSEC        at 0 range 6 .. 6;
      IWDGSEC        at 0 range 7 .. 7;
      SPI2SEC        at 0 range 8 .. 8;
      USART2SEC      at 0 range 9 .. 9;
      USART3SEC      at 0 range 10 .. 10;
      UART4SEC       at 0 range 11 .. 11;
      UART5SEC       at 0 range 12 .. 12;
      I2C1SEC        at 0 range 13 .. 13;
      I2C2SEC        at 0 range 14 .. 14;
      CRSSEC         at 0 range 15 .. 15;
      I2C4SEC        at 0 range 16 .. 16;
      LPTIM2SEC      at 0 range 17 .. 17;
      FDCAN1SEC      at 0 range 18 .. 18;
      UCPD1SEC       at 0 range 19 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  TZSC secure configuration register 2
   type TZSC_SECCFGR2_Register is record
      --  secure access mode for TIM1
      TIM1SEC       : Boolean := False;
      --  secure access mode for SPI1
      SPI1SEC       : Boolean := False;
      --  secure access mode for TIM8
      TIM8SEC       : Boolean := False;
      --  secure access mode for USART1
      USART1SEC     : Boolean := False;
      --  secure access mode for TIM5
      TIM15SEC      : Boolean := False;
      --  secure access mode for TIM6
      TIM16SEC      : Boolean := False;
      --  secure access mode for TIM7
      TIM17SEC      : Boolean := False;
      --  secure access mode for SAI1
      SAI1SEC       : Boolean := False;
      --  secure access mode for SAI2
      SAI2SEC       : Boolean := False;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_SECCFGR2_Register use record
      TIM1SEC       at 0 range 0 .. 0;
      SPI1SEC       at 0 range 1 .. 1;
      TIM8SEC       at 0 range 2 .. 2;
      USART1SEC     at 0 range 3 .. 3;
      TIM15SEC      at 0 range 4 .. 4;
      TIM16SEC      at 0 range 5 .. 5;
      TIM17SEC      at 0 range 6 .. 6;
      SAI1SEC       at 0 range 7 .. 7;
      SAI2SEC       at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   --  TZSC secure configuration register 3
   type TZSC_SECCFGR3_Register is record
      --  secure access mode for MDF1
      MDF1SEC         : Boolean := False;
      --  secure access mode for CORDIC
      CORDICSEC       : Boolean := False;
      --  secure access mode for FMAC
      FMACSEC         : Boolean := False;
      --  secure access mode for CRC
      CRCSEC          : Boolean := False;
      --  secure access mode for TSC
      TSCSEC          : Boolean := False;
      --  secure access mode for register of DMA2D
      DMA2DSEC        : Boolean := False;
      --  secure access mode for ICACHE registers
      ICACHE_REGSEC   : Boolean := False;
      --  secure access mode for DCACHE registers
      DCACHE_REGSEC   : Boolean := False;
      --  secure access mode for ADC1
      ADC1SEC         : Boolean := False;
      --  secure access mode for DCMI
      DCMISEC         : Boolean := False;
      --  secure access mode for OTG_FS
      OTGFSSEC        : Boolean := False;
      --  secure access mode for AES
      AESSEC          : Boolean := False;
      --  secure access mode for HASH
      HASHSEC         : Boolean := False;
      --  secure access mode for RNG
      RNGSEC          : Boolean := False;
      --  secure access mode for PKA
      PKASEC          : Boolean := False;
      --  secure access mode for SAES
      SAESSEC         : Boolean := False;
      --  secure access mode for OCTOSPIM
      OCTOSPIMSEC     : Boolean := False;
      --  secure access mode for SDMMC2
      SDMMC1SEC       : Boolean := False;
      --  secure access mode for SDMMC1
      SDMMC2SEC       : Boolean := False;
      --  secure access mode for FSMC registers
      FSMC_REGSEC     : Boolean := False;
      --  secure access mode for OCTOSPI1 registers
      OCTOSPI1_REGSEC : Boolean := False;
      --  secure access mode for OCTOSPI2 registers
      OCTOSPI2_REGSEC : Boolean := False;
      --  secure access mode for RAMCFG
      RAMCFGSEC       : Boolean := False;
      --  unspecified
      Reserved_23_31  : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_SECCFGR3_Register use record
      MDF1SEC         at 0 range 0 .. 0;
      CORDICSEC       at 0 range 1 .. 1;
      FMACSEC         at 0 range 2 .. 2;
      CRCSEC          at 0 range 3 .. 3;
      TSCSEC          at 0 range 4 .. 4;
      DMA2DSEC        at 0 range 5 .. 5;
      ICACHE_REGSEC   at 0 range 6 .. 6;
      DCACHE_REGSEC   at 0 range 7 .. 7;
      ADC1SEC         at 0 range 8 .. 8;
      DCMISEC         at 0 range 9 .. 9;
      OTGFSSEC        at 0 range 10 .. 10;
      AESSEC          at 0 range 11 .. 11;
      HASHSEC         at 0 range 12 .. 12;
      RNGSEC          at 0 range 13 .. 13;
      PKASEC          at 0 range 14 .. 14;
      SAESSEC         at 0 range 15 .. 15;
      OCTOSPIMSEC     at 0 range 16 .. 16;
      SDMMC1SEC       at 0 range 17 .. 17;
      SDMMC2SEC       at 0 range 18 .. 18;
      FSMC_REGSEC     at 0 range 19 .. 19;
      OCTOSPI1_REGSEC at 0 range 20 .. 20;
      OCTOSPI2_REGSEC at 0 range 21 .. 21;
      RAMCFGSEC       at 0 range 22 .. 22;
      Reserved_23_31  at 0 range 23 .. 31;
   end record;

   --  TZSC privilege configuration register 1
   type TZSC_PRIVCFGR1_Register is record
      --  privileged access mode for TIM2
      TIM2PRIV       : Boolean := False;
      --  privileged access mode for TIM3
      TIM3PRIV       : Boolean := False;
      --  privileged access mode for TIM4
      TIM4PRIV       : Boolean := False;
      --  privileged access mode for TIM5
      TIM5PRIV       : Boolean := False;
      --  privileged access mode for TIM6
      TIM6PRIV       : Boolean := False;
      --  privileged access mode for TIM7
      TIM7PRIV       : Boolean := False;
      --  privileged access mode for WWDG
      WWDGPRIV       : Boolean := False;
      --  privileged access mode for IWDG
      IWDGPRIV       : Boolean := False;
      --  privileged access mode for SPI2
      SPI2PRIV       : Boolean := False;
      --  privileged access mode for USART2
      USART2PRIV     : Boolean := False;
      --  privileged access mode for USART3
      USART3PRIV     : Boolean := False;
      --  privileged access mode for UART4
      UART4PRIV      : Boolean := False;
      --  privileged access mode for UART5
      UART5PRIV      : Boolean := False;
      --  privileged access mode for I2C1
      I2C1PRIV       : Boolean := False;
      --  privileged access mode for I2C2
      I2C2PRIV       : Boolean := False;
      --  privileged access mode for CRS
      CRSPRIV        : Boolean := False;
      --  privileged access mode for I2C4
      I2C4PRIV       : Boolean := False;
      --  privileged access mode for LPTIM2
      LPTIM2PRIV     : Boolean := False;
      --  privileged access mode for FDCAN1
      FDCAN1PRIV     : Boolean := False;
      --  privileged access mode for UCPD1
      UCPD1PRIV      : Boolean := False;
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_PRIVCFGR1_Register use record
      TIM2PRIV       at 0 range 0 .. 0;
      TIM3PRIV       at 0 range 1 .. 1;
      TIM4PRIV       at 0 range 2 .. 2;
      TIM5PRIV       at 0 range 3 .. 3;
      TIM6PRIV       at 0 range 4 .. 4;
      TIM7PRIV       at 0 range 5 .. 5;
      WWDGPRIV       at 0 range 6 .. 6;
      IWDGPRIV       at 0 range 7 .. 7;
      SPI2PRIV       at 0 range 8 .. 8;
      USART2PRIV     at 0 range 9 .. 9;
      USART3PRIV     at 0 range 10 .. 10;
      UART4PRIV      at 0 range 11 .. 11;
      UART5PRIV      at 0 range 12 .. 12;
      I2C1PRIV       at 0 range 13 .. 13;
      I2C2PRIV       at 0 range 14 .. 14;
      CRSPRIV        at 0 range 15 .. 15;
      I2C4PRIV       at 0 range 16 .. 16;
      LPTIM2PRIV     at 0 range 17 .. 17;
      FDCAN1PRIV     at 0 range 18 .. 18;
      UCPD1PRIV      at 0 range 19 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  TZSC privilege configuration register 2
   type TZSC_PRIVCFGR2_Register is record
      --  privileged access mode for TIM1
      TIM1PRIV      : Boolean := False;
      --  privileged access mode for SPI1PRIV
      SPI1PRIV      : Boolean := False;
      --  privileged access mode for TIM8
      TIM8PRIV      : Boolean := False;
      --  privileged access mode for USART1
      USART1PRIV    : Boolean := False;
      --  privileged access mode for TIM15
      TIM15PRIV     : Boolean := False;
      --  privileged access mode for TIM16
      TIM16PRIV     : Boolean := False;
      --  privileged access mode for TIM17
      TIM17PRIV     : Boolean := False;
      --  privileged access mode for SAI1
      SAI1PRIV      : Boolean := False;
      --  privileged access mode for SAI2
      SAI2PRIV      : Boolean := False;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_PRIVCFGR2_Register use record
      TIM1PRIV      at 0 range 0 .. 0;
      SPI1PRIV      at 0 range 1 .. 1;
      TIM8PRIV      at 0 range 2 .. 2;
      USART1PRIV    at 0 range 3 .. 3;
      TIM15PRIV     at 0 range 4 .. 4;
      TIM16PRIV     at 0 range 5 .. 5;
      TIM17PRIV     at 0 range 6 .. 6;
      SAI1PRIV      at 0 range 7 .. 7;
      SAI2PRIV      at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   --  TZSC privilege configuration register 3
   type TZSC_PRIVCFGR3_Register is record
      --  privileged access mode for MDF1
      MDF1PRIV         : Boolean := False;
      --  privileged access mode for CORDIC
      CORDICPRIV       : Boolean := False;
      --  privileged access mode for FMAC
      FMACPRIV         : Boolean := False;
      --  privileged access mode for CRC
      CRCPRIV          : Boolean := False;
      --  privileged access mode for TSC
      TSCPRIV          : Boolean := False;
      --  privileged access mode for register of DMA2D
      DMA2DPRIV        : Boolean := False;
      --  privileged access mode for ICACHE registers
      ICACHE_REGPRIV   : Boolean := False;
      --  privileged access mode for DCACHE registers
      DCACHE_REGPRIV   : Boolean := False;
      --  privileged access mode for ADC1
      ADC1PRIV         : Boolean := False;
      --  privileged access mode for DCMI
      DCMIPRIV         : Boolean := False;
      --  privileged access mode for OTG_FS
      OTGFSPRIV        : Boolean := False;
      --  privileged access mode for AES
      AESPRIV          : Boolean := False;
      --  privileged access mode for HASH
      HASHPRIV         : Boolean := False;
      --  privileged access mode for RNG
      RNGPRIV          : Boolean := False;
      --  privileged access mode for PKA
      PKAPRIV          : Boolean := False;
      --  privileged access mode for SAES
      SAESPRIV         : Boolean := False;
      --  privileged access mode for OCTOSPIM
      OCTOSPIMPRIV     : Boolean := False;
      --  privileged access mode for SDMMC2
      SDMMC1PRIV       : Boolean := False;
      --  privileged access mode for SDMMC1
      SDMMC2PRIV       : Boolean := False;
      --  privileged access mode for FSMC registers
      FSMC_REGPRIV     : Boolean := False;
      --  privileged access mode for OCTOSPI1
      OCTOSPI1_REGPRIV : Boolean := False;
      --  privileged access mode for OCTOSPI2
      OCTOSPI2_REGPRIV : Boolean := False;
      --  privileged access mode for RAMCFG
      RAMCFGPRIV       : Boolean := False;
      --  unspecified
      Reserved_23_31   : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_PRIVCFGR3_Register use record
      MDF1PRIV         at 0 range 0 .. 0;
      CORDICPRIV       at 0 range 1 .. 1;
      FMACPRIV         at 0 range 2 .. 2;
      CRCPRIV          at 0 range 3 .. 3;
      TSCPRIV          at 0 range 4 .. 4;
      DMA2DPRIV        at 0 range 5 .. 5;
      ICACHE_REGPRIV   at 0 range 6 .. 6;
      DCACHE_REGPRIV   at 0 range 7 .. 7;
      ADC1PRIV         at 0 range 8 .. 8;
      DCMIPRIV         at 0 range 9 .. 9;
      OTGFSPRIV        at 0 range 10 .. 10;
      AESPRIV          at 0 range 11 .. 11;
      HASHPRIV         at 0 range 12 .. 12;
      RNGPRIV          at 0 range 13 .. 13;
      PKAPRIV          at 0 range 14 .. 14;
      SAESPRIV         at 0 range 15 .. 15;
      OCTOSPIMPRIV     at 0 range 16 .. 16;
      SDMMC1PRIV       at 0 range 17 .. 17;
      SDMMC2PRIV       at 0 range 18 .. 18;
      FSMC_REGPRIV     at 0 range 19 .. 19;
      OCTOSPI1_REGPRIV at 0 range 20 .. 20;
      OCTOSPI2_REGPRIV at 0 range 21 .. 21;
      RAMCFGPRIV       at 0 range 22 .. 22;
      Reserved_23_31   at 0 range 23 .. 31;
   end record;

   --  TZSC memory 1 sub-region A watermark configuration register
   type TZSC_MPCWM1ACFGR_Register is record
      --  Sub-region enable
      SREN           : Boolean := False;
      --  Sub-region lock
      SRLOCK         : Boolean := False;
      --  unspecified
      Reserved_2_7   : HAL.UInt6 := 16#0#;
      --  Secure sub-region
      SEC            : Boolean := False;
      --  Privileged sub-region
      PRIV           : Boolean := False;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_MPCWM1ACFGR_Register use record
      SREN           at 0 range 0 .. 0;
      SRLOCK         at 0 range 1 .. 1;
      Reserved_2_7   at 0 range 2 .. 7;
      SEC            at 0 range 8 .. 8;
      PRIV           at 0 range 9 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype TZSC_MPCWM1AR_SUBA_START_Field is HAL.UInt11;
   subtype TZSC_MPCWM1AR_SUBA_LENGTH_Field is HAL.UInt12;

   --  TZSC memory 1 sub-region A watermark register
   type TZSC_MPCWM1AR_Register is record
      --  Start of sub-region A
      SUBA_START     : TZSC_MPCWM1AR_SUBA_START_Field := 16#0#;
      --  unspecified
      Reserved_11_15 : HAL.UInt5 := 16#0#;
      --  Length of sub-region A
      SUBA_LENGTH    : TZSC_MPCWM1AR_SUBA_LENGTH_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_MPCWM1AR_Register use record
      SUBA_START     at 0 range 0 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
      SUBA_LENGTH    at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  TZSC memory 1 sub-region B watermark configuration register
   type TZSC_MPCWM1BCFGR_Register is record
      --  Sub-region enable
      SREN           : Boolean := False;
      --  Sub-region lock
      SRLOCK         : Boolean := False;
      --  unspecified
      Reserved_2_7   : HAL.UInt6 := 16#0#;
      --  Secure sub-region
      SEC            : Boolean := False;
      --  Privileged sub-region
      PRIV           : Boolean := False;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_MPCWM1BCFGR_Register use record
      SREN           at 0 range 0 .. 0;
      SRLOCK         at 0 range 1 .. 1;
      Reserved_2_7   at 0 range 2 .. 7;
      SEC            at 0 range 8 .. 8;
      PRIV           at 0 range 9 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype TZSC_MPCWM1BR_SUBB_START_Field is HAL.UInt11;
   subtype TZSC_MPCWM1BR_SUBB_LENGTH_Field is HAL.UInt12;

   --  TZSC memory 1 sub-region B watermark register
   type TZSC_MPCWM1BR_Register is record
      --  Start of sub-region A
      SUBB_START     : TZSC_MPCWM1BR_SUBB_START_Field := 16#0#;
      --  unspecified
      Reserved_11_15 : HAL.UInt5 := 16#0#;
      --  Length of sub-region A
      SUBB_LENGTH    : TZSC_MPCWM1BR_SUBB_LENGTH_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_MPCWM1BR_Register use record
      SUBB_START     at 0 range 0 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
      SUBB_LENGTH    at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  TZSC memory 2 sub-region A watermark configuration register
   type TZSC_MPCWM2ACFGR_Register is record
      --  Sub-region enable
      SREN           : Boolean := False;
      --  Sub-region lock
      SRLOCK         : Boolean := False;
      --  unspecified
      Reserved_2_7   : HAL.UInt6 := 16#0#;
      --  Secure sub-region
      SEC            : Boolean := False;
      --  Privileged sub-region
      PRIV           : Boolean := False;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_MPCWM2ACFGR_Register use record
      SREN           at 0 range 0 .. 0;
      SRLOCK         at 0 range 1 .. 1;
      Reserved_2_7   at 0 range 2 .. 7;
      SEC            at 0 range 8 .. 8;
      PRIV           at 0 range 9 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype TZSC_MPCWM2AR_SUBA_START_Field is HAL.UInt11;
   subtype TZSC_MPCWM2AR_SUBA_LENGTH_Field is HAL.UInt12;

   --  TZSC memory 2 sub-region A watermark register
   type TZSC_MPCWM2AR_Register is record
      --  Start of sub-region A
      SUBA_START     : TZSC_MPCWM2AR_SUBA_START_Field := 16#0#;
      --  unspecified
      Reserved_11_15 : HAL.UInt5 := 16#0#;
      --  Length of sub-region A
      SUBA_LENGTH    : TZSC_MPCWM2AR_SUBA_LENGTH_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_MPCWM2AR_Register use record
      SUBA_START     at 0 range 0 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
      SUBA_LENGTH    at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  TZSC memory 2 sub-region B watermark configuration register
   type TZSC_MPCWM2BCFGR_Register is record
      --  Sub-region enable
      SREN           : Boolean := False;
      --  Sub-region lock
      SRLOCK         : Boolean := False;
      --  unspecified
      Reserved_2_7   : HAL.UInt6 := 16#0#;
      --  Secure sub-region
      SEC            : Boolean := False;
      --  Privileged sub-region
      PRIV           : Boolean := False;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_MPCWM2BCFGR_Register use record
      SREN           at 0 range 0 .. 0;
      SRLOCK         at 0 range 1 .. 1;
      Reserved_2_7   at 0 range 2 .. 7;
      SEC            at 0 range 8 .. 8;
      PRIV           at 0 range 9 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype TZSC_MPCWM2BR_SUBB_START_Field is HAL.UInt11;
   subtype TZSC_MPCWM2BR_SUBB_LENGTH_Field is HAL.UInt12;

   --  TZSC memory 2 sub-region B watermark register
   type TZSC_MPCWM2BR_Register is record
      --  Start of sub-region A
      SUBB_START     : TZSC_MPCWM2BR_SUBB_START_Field := 16#0#;
      --  unspecified
      Reserved_11_15 : HAL.UInt5 := 16#0#;
      --  Length of sub-region A
      SUBB_LENGTH    : TZSC_MPCWM2BR_SUBB_LENGTH_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_MPCWM2BR_Register use record
      SUBB_START     at 0 range 0 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
      SUBB_LENGTH    at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  TZSC memory 3 sub-region A watermark configuration register
   type TZSC_MPCWM3ACFGR_Register is record
      --  Sub-region enable
      SREN           : Boolean := False;
      --  Sub-region lock
      SRLOCK         : Boolean := False;
      --  unspecified
      Reserved_2_7   : HAL.UInt6 := 16#0#;
      --  Secure sub-region
      SEC            : Boolean := False;
      --  Privileged sub-region
      PRIV           : Boolean := False;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_MPCWM3ACFGR_Register use record
      SREN           at 0 range 0 .. 0;
      SRLOCK         at 0 range 1 .. 1;
      Reserved_2_7   at 0 range 2 .. 7;
      SEC            at 0 range 8 .. 8;
      PRIV           at 0 range 9 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype TZSC_MPCWM3AR_SUBA_START_Field is HAL.UInt11;
   subtype TZSC_MPCWM3AR_SUBA_LENGTH_Field is HAL.UInt12;

   --  TZSC memory 3 sub-region A watermark register
   type TZSC_MPCWM3AR_Register is record
      --  Start of sub-region A
      SUBA_START     : TZSC_MPCWM3AR_SUBA_START_Field := 16#0#;
      --  unspecified
      Reserved_11_15 : HAL.UInt5 := 16#0#;
      --  Length of sub-region A
      SUBA_LENGTH    : TZSC_MPCWM3AR_SUBA_LENGTH_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_MPCWM3AR_Register use record
      SUBA_START     at 0 range 0 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
      SUBA_LENGTH    at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  TZSC memory 4 sub-region A watermark configuration register
   type TZSC_MPCWM4ACFGR_Register is record
      --  Sub-region enable
      SREN           : Boolean := False;
      --  Sub-region lock
      SRLOCK         : Boolean := False;
      --  unspecified
      Reserved_2_7   : HAL.UInt6 := 16#0#;
      --  Secure sub-region
      SEC            : Boolean := False;
      --  Privileged sub-region
      PRIV           : Boolean := False;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_MPCWM4ACFGR_Register use record
      SREN           at 0 range 0 .. 0;
      SRLOCK         at 0 range 1 .. 1;
      Reserved_2_7   at 0 range 2 .. 7;
      SEC            at 0 range 8 .. 8;
      PRIV           at 0 range 9 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype TZSC_MPCWM4AR_SUBA_START_Field is HAL.UInt11;
   subtype TZSC_MPCWM4AR_SUBA_LENGTH_Field is HAL.UInt12;

   --  TZSC memory 4 sub-region A watermark register
   type TZSC_MPCWM4AR_Register is record
      --  Start of sub-region A
      SUBA_START     : TZSC_MPCWM4AR_SUBA_START_Field := 16#0#;
      --  unspecified
      Reserved_11_15 : HAL.UInt5 := 16#0#;
      --  Length of sub-region A
      SUBA_LENGTH    : TZSC_MPCWM4AR_SUBA_LENGTH_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_MPCWM4AR_Register use record
      SUBA_START     at 0 range 0 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
      SUBA_LENGTH    at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  TZSC memory 5 sub-region A watermark configuration register
   type TZSC_MPCWM5ACFGR_Register is record
      --  Sub-region enable
      SREN           : Boolean := False;
      --  Sub-region lock
      SRLOCK         : Boolean := False;
      --  unspecified
      Reserved_2_7   : HAL.UInt6 := 16#0#;
      --  Secure sub-region
      SEC            : Boolean := False;
      --  Privileged sub-region
      PRIV           : Boolean := False;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_MPCWM5ACFGR_Register use record
      SREN           at 0 range 0 .. 0;
      SRLOCK         at 0 range 1 .. 1;
      Reserved_2_7   at 0 range 2 .. 7;
      SEC            at 0 range 8 .. 8;
      PRIV           at 0 range 9 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype TZSC_MPCWM5AR_SUBA_START_Field is HAL.UInt11;
   subtype TZSC_MPCWM5AR_SUBA_LENGTH_Field is HAL.UInt12;

   --  TZSC memory 5 sub-region A watermark register
   type TZSC_MPCWM5AR_Register is record
      --  Start of sub-region A
      SUBA_START     : TZSC_MPCWM5AR_SUBA_START_Field := 16#0#;
      --  unspecified
      Reserved_11_15 : HAL.UInt5 := 16#0#;
      --  Length of sub-region A
      SUBA_LENGTH    : TZSC_MPCWM5AR_SUBA_LENGTH_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_MPCWM5AR_Register use record
      SUBA_START     at 0 range 0 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
      SUBA_LENGTH    at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  TZSC memory 5 sub-region B watermark configuration register
   type TZSC_MPCWM5BCFGR_Register is record
      --  Sub-region enable
      SREN           : Boolean := False;
      --  Sub-region lock
      SRLOCK         : Boolean := False;
      --  unspecified
      Reserved_2_7   : HAL.UInt6 := 16#0#;
      --  Secure sub-region
      SEC            : Boolean := False;
      --  Privileged sub-region
      PRIV           : Boolean := False;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_MPCWM5BCFGR_Register use record
      SREN           at 0 range 0 .. 0;
      SRLOCK         at 0 range 1 .. 1;
      Reserved_2_7   at 0 range 2 .. 7;
      SEC            at 0 range 8 .. 8;
      PRIV           at 0 range 9 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype TZSC_MPCWM5BR_SUBB_START_Field is HAL.UInt11;
   subtype TZSC_MPCWM5BR_SUBB_LENGTH_Field is HAL.UInt12;

   --  TZSC memory 5 sub-region B watermark register
   type TZSC_MPCWM5BR_Register is record
      --  Start of sub-region A
      SUBB_START     : TZSC_MPCWM5BR_SUBB_START_Field := 16#0#;
      --  unspecified
      Reserved_11_15 : HAL.UInt5 := 16#0#;
      --  Length of sub-region A
      SUBB_LENGTH    : TZSC_MPCWM5BR_SUBB_LENGTH_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_MPCWM5BR_Register use record
      SUBB_START     at 0 range 0 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
      SUBB_LENGTH    at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  MPCBB control register
   type MPCBB4_CR_Register is record
      --  lock the control register of the MPCBB until next reset
      GLOCK         : Boolean := False;
      --  unspecified
      Reserved_1_29 : HAL.UInt29 := 16#0#;
      --  SRAMx clocks security state
      INVSECSTATE   : Boolean := False;
      --  secure read/write illegal access disable
      SRWILADIS     : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPCBB4_CR_Register use record
      GLOCK         at 0 range 0 .. 0;
      Reserved_1_29 at 0 range 1 .. 29;
      INVSECSTATE   at 0 range 30 .. 30;
      SRWILADIS     at 0 range 31 .. 31;
   end record;

   --  GTZC2 SRAM4 MPCBB configuration lock register
   type MPCBB4_CFGLOCK_Register is record
      --  Security/privilege configuration lock for super-block 0
      SPLCK0        : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPCBB4_CFGLOCK_Register use record
      SPLCK0        at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  MPCBB4_SECCFGR0_SEC array
   type MPCBB4_SECCFGR0_SEC_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBB security configuration for super-block 0 register
   type MPCBB4_SECCFGR0_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SEC as a value
            Val : HAL.UInt32;
         when True =>
            --  SEC as an array
            Arr : MPCBB4_SECCFGR0_SEC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPCBB4_SECCFGR0_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB4_PRIVCFGR0_PRIV array
   type MPCBB4_PRIVCFGR0_PRIV_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBB privileged configuration for super-block 0 register
   type MPCBB4_PRIVCFGR0_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PRIV as a value
            Val : HAL.UInt32;
         when True =>
            --  PRIV as an array
            Arr : MPCBB4_PRIVCFGR0_PRIV_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPCBB4_PRIVCFGR0_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  TZIC interrupt enable register 1
   type IER1_Register_1 is record
      --  illegal access interrupt enable for SPI3
      SPI3IE         : Boolean := False;
      --  illegal access interrupt enable for LPUART1
      LPUART1IE      : Boolean := False;
      --  illegal access interrupt enable for I2C3
      I2C3IE         : Boolean := False;
      --  illegal access interrupt enable for LPTIM1
      LPTIM1IE       : Boolean := False;
      --  illegal access interrupt enable for LPTIM3
      LPTIM3IE       : Boolean := False;
      --  illegal access interrupt enable for LPTIM4
      LPTIM4IE       : Boolean := False;
      --  illegal access interrupt enable for OPAMP
      OPAMPIE        : Boolean := False;
      --  illegal access interrupt enable for COMP
      COMPIE         : Boolean := False;
      --  illegal access interrupt enable for ADC4
      ADC4IE         : Boolean := False;
      --  illegal access interrupt enable for VREFBUF
      VREFBUFIE      : Boolean := False;
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  illegal access interrupt enable for DAC1
      DAC1IE         : Boolean := False;
      --  illegal access interrupt enable for ADF1
      ADF1IE         : Boolean := False;
      --  unspecified
      Reserved_13_31 : HAL.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IER1_Register_1 use record
      SPI3IE         at 0 range 0 .. 0;
      LPUART1IE      at 0 range 1 .. 1;
      I2C3IE         at 0 range 2 .. 2;
      LPTIM1IE       at 0 range 3 .. 3;
      LPTIM3IE       at 0 range 4 .. 4;
      LPTIM4IE       at 0 range 5 .. 5;
      OPAMPIE        at 0 range 6 .. 6;
      COMPIE         at 0 range 7 .. 7;
      ADC4IE         at 0 range 8 .. 8;
      VREFBUFIE      at 0 range 9 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      DAC1IE         at 0 range 11 .. 11;
      ADF1IE         at 0 range 12 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   --  TZIC interrupt enable register 2
   type IER2_Register_1 is record
      --  illegal access interrupt enable for SYSCFG
      SYSCFGIE       : Boolean := False;
      --  illegal access interrupt enable for RTC
      RTCIE          : Boolean := False;
      --  illegal access interrupt enable for TAMP
      TAMPIE         : Boolean := False;
      --  illegal access interrupt enable for PWR
      PWRIE          : Boolean := False;
      --  illegal access interrupt enable for RCC
      RCCIE          : Boolean := False;
      --  illegal access interrupt enable for LPDMA
      LPDMA1IE       : Boolean := False;
      --  illegal access interrupt enable for EXTI
      EXTIIE         : Boolean := False;
      --  unspecified
      Reserved_7_13  : HAL.UInt7 := 16#0#;
      --  illegal access interrupt enable for GTZC2 TZSC registers
      TZSC2IE        : Boolean := False;
      --  illegal access interrupt enable for GTZC2 TZIC registers
      TZIC2IE        : Boolean := False;
      --  unspecified
      Reserved_16_23 : HAL.UInt8 := 16#0#;
      --  illegal access interrupt enable for SRAM4
      SRAM4IE        : Boolean := False;
      --  illegal access interrupt enable for MPCBB4 registers
      MPCBB4_REGIE   : Boolean := False;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IER2_Register_1 use record
      SYSCFGIE       at 0 range 0 .. 0;
      RTCIE          at 0 range 1 .. 1;
      TAMPIE         at 0 range 2 .. 2;
      PWRIE          at 0 range 3 .. 3;
      RCCIE          at 0 range 4 .. 4;
      LPDMA1IE       at 0 range 5 .. 5;
      EXTIIE         at 0 range 6 .. 6;
      Reserved_7_13  at 0 range 7 .. 13;
      TZSC2IE        at 0 range 14 .. 14;
      TZIC2IE        at 0 range 15 .. 15;
      Reserved_16_23 at 0 range 16 .. 23;
      SRAM4IE        at 0 range 24 .. 24;
      MPCBB4_REGIE   at 0 range 25 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   --  TZIC status register 1
   type SR1_Register_1 is record
      --  Read-only. illegal access flag for SPI3
      SPI3F          : Boolean;
      --  Read-only. illegal access flag for LPUART1
      LPUART1F       : Boolean;
      --  Read-only. illegal access flag for I2C3
      I2C3F          : Boolean;
      --  Read-only. illegal access flag for LPTIM1
      LPTIM1F        : Boolean;
      --  Read-only. illegal access flag for LPTIM3
      LPTIM3F        : Boolean;
      --  Read-only. illegal access flag for LPTIM4
      LPTIM4F        : Boolean;
      --  Read-only. illegal access flag for OPAMP
      OPAMPF         : Boolean;
      --  Read-only. illegal access flag for COMP
      COMPF          : Boolean;
      --  Read-only. illegal access flag for ADC4
      ADC4F          : Boolean;
      --  Read-only. illegal access flag for VREFBUF
      VREFBUFF       : Boolean;
      --  unspecified
      Reserved_10_10 : HAL.Bit;
      --  Read-only. illegal access flag for DAC1
      DAC1F          : Boolean;
      --  Read-only. illegal access flag for ADF1
      ADF1F          : Boolean;
      --  unspecified
      Reserved_13_31 : HAL.UInt19;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR1_Register_1 use record
      SPI3F          at 0 range 0 .. 0;
      LPUART1F       at 0 range 1 .. 1;
      I2C3F          at 0 range 2 .. 2;
      LPTIM1F        at 0 range 3 .. 3;
      LPTIM3F        at 0 range 4 .. 4;
      LPTIM4F        at 0 range 5 .. 5;
      OPAMPF         at 0 range 6 .. 6;
      COMPF          at 0 range 7 .. 7;
      ADC4F          at 0 range 8 .. 8;
      VREFBUFF       at 0 range 9 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      DAC1F          at 0 range 11 .. 11;
      ADF1F          at 0 range 12 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   --  TZIC status register 2
   type SR2_Register_1 is record
      --  Read-only. illegal access flag for SYSCFG
      SYSCFGF        : Boolean;
      --  Read-only. illegal access flag for RTC
      RTCF           : Boolean;
      --  Read-only. illegal access flag for TAMP
      TAMPF          : Boolean;
      --  Read-only. illegal access flag for PWRUSART1F
      PWRF           : Boolean;
      --  Read-only. illegal access flag for RCC
      RCCF           : Boolean;
      --  Read-only. illegal access flag for LPDMA
      LPDMA1F        : Boolean;
      --  Read-only. illegal access flag for EXTI
      EXTIF          : Boolean;
      --  unspecified
      Reserved_7_13  : HAL.UInt7;
      --  Read-only. illegal access flag for GTZC2 TZSC registers
      TZSC2F         : Boolean;
      --  Read-only. illegal access flag for GTZC2 TZIC registers
      TZIC2F         : Boolean;
      --  unspecified
      Reserved_16_23 : HAL.UInt8;
      --  Read-only. illegal access flag for SRAM4
      SRAM4F         : Boolean;
      --  Read-only. illegal access flag for MPCBB4 registers
      MPCBB4_REGF    : Boolean;
      --  unspecified
      Reserved_26_31 : HAL.UInt6;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR2_Register_1 use record
      SYSCFGF        at 0 range 0 .. 0;
      RTCF           at 0 range 1 .. 1;
      TAMPF          at 0 range 2 .. 2;
      PWRF           at 0 range 3 .. 3;
      RCCF           at 0 range 4 .. 4;
      LPDMA1F        at 0 range 5 .. 5;
      EXTIF          at 0 range 6 .. 6;
      Reserved_7_13  at 0 range 7 .. 13;
      TZSC2F         at 0 range 14 .. 14;
      TZIC2F         at 0 range 15 .. 15;
      Reserved_16_23 at 0 range 16 .. 23;
      SRAM4F         at 0 range 24 .. 24;
      MPCBB4_REGF    at 0 range 25 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   --  TZIC flag clear register 1
   type FCR1_Register_1 is record
      --  Write-only. clear the illegal access flag for SPI3
      CSPI3F         : Boolean := False;
      --  Write-only. clear the illegal access flag for LPUART1
      CLPUART1F      : Boolean := False;
      --  Write-only. clear the illegal access flag for I2C3
      CI2C3F         : Boolean := False;
      --  Write-only. clear the illegal access flag for LPTIM1
      CLPTIM1F       : Boolean := False;
      --  Write-only. clear the illegal access flag for LPTIM3
      CLPTIM3F       : Boolean := False;
      --  Write-only. clear the illegal access flag for LPTIM4
      CLPTIM4F       : Boolean := False;
      --  Write-only. clear the illegal access flag for OPAMP
      COPAMPF        : Boolean := False;
      --  Write-only. clear the illegal access flag for COMP
      CCOMPF         : Boolean := False;
      --  Write-only. clear the illegal access flag for ADC4
      CADC4F         : Boolean := False;
      --  Write-only. clear the illegal access flag for VREFBUF
      CVREFBUFF      : Boolean := False;
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  Write-only. clear the illegal access flag for DAC1
      CDAC1F         : Boolean := False;
      --  Write-only. clear the illegal access flag for ADF1
      CADF1F         : Boolean := False;
      --  unspecified
      Reserved_13_31 : HAL.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FCR1_Register_1 use record
      CSPI3F         at 0 range 0 .. 0;
      CLPUART1F      at 0 range 1 .. 1;
      CI2C3F         at 0 range 2 .. 2;
      CLPTIM1F       at 0 range 3 .. 3;
      CLPTIM3F       at 0 range 4 .. 4;
      CLPTIM4F       at 0 range 5 .. 5;
      COPAMPF        at 0 range 6 .. 6;
      CCOMPF         at 0 range 7 .. 7;
      CADC4F         at 0 range 8 .. 8;
      CVREFBUFF      at 0 range 9 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      CDAC1F         at 0 range 11 .. 11;
      CADF1F         at 0 range 12 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   --  TZIC flag clear register 2
   type FCR2_Register_1 is record
      --  Write-only. clear the illegal access flag for SYSCFG
      CSYSCFGF       : Boolean := False;
      --  Write-only. clear the illegal access flag for RTC
      CRTCF          : Boolean := False;
      --  Write-only. clear the illegal access flag for TAMP
      CTAMPF         : Boolean := False;
      --  Write-only. clear the illegal access flag for PWR
      CPWRF          : Boolean := False;
      --  Write-only. clear the illegal access flag for RCC
      CRCCF          : Boolean := False;
      --  Write-only. clear the illegal access flag for LPDMA
      CLPDMA1F       : Boolean := False;
      --  Write-only. clear the illegal access flag for EXTI
      CEXTIF         : Boolean := False;
      --  unspecified
      Reserved_7_13  : HAL.UInt7 := 16#0#;
      --  Write-only. clear the illegal access flag for GTZC2 TZSC registers
      CTZSC2F        : Boolean := False;
      --  Write-only. clear the illegal access flag for GTZC2 TZIC registers
      CTZIC2F        : Boolean := False;
      --  unspecified
      Reserved_16_23 : HAL.UInt8 := 16#0#;
      --  Write-only. clear the illegal access flag for SRAM4
      CSRAM4F        : Boolean := False;
      --  Write-only. clear the illegal access flag for MPCBB4 registers
      CMPCBB4_REGF   : Boolean := False;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FCR2_Register_1 use record
      CSYSCFGF       at 0 range 0 .. 0;
      CRTCF          at 0 range 1 .. 1;
      CTAMPF         at 0 range 2 .. 2;
      CPWRF          at 0 range 3 .. 3;
      CRCCF          at 0 range 4 .. 4;
      CLPDMA1F       at 0 range 5 .. 5;
      CEXTIF         at 0 range 6 .. 6;
      Reserved_7_13  at 0 range 7 .. 13;
      CTZSC2F        at 0 range 14 .. 14;
      CTZIC2F        at 0 range 15 .. 15;
      Reserved_16_23 at 0 range 16 .. 23;
      CSRAM4F        at 0 range 24 .. 24;
      CMPCBB4_REGF   at 0 range 25 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   --  TZSC secure configuration register 1
   type TZSC_SECCFGR1_Register_1 is record
      --  secure access mode for SPI3
      SPI3SEC        : Boolean := False;
      --  secure access mode for LPUART1
      LPUART1SEC     : Boolean := False;
      --  secure access mode for I2C3
      I2C3SEC        : Boolean := False;
      --  secure access mode for LPTIM1
      LPTIM1SEC      : Boolean := False;
      --  secure access mode for LPTIM3
      LPTIM3SEC      : Boolean := False;
      --  secure access mode for LPTIM4
      LPTIM4SEC      : Boolean := False;
      --  secure access mode for OPAMP
      OPAMPSEC       : Boolean := False;
      --  secure access mode for COMP
      COMPSEC        : Boolean := False;
      --  secure access mode for ADC4
      ADC4SEC        : Boolean := False;
      --  secure access mode for VREFBUF
      VREFBUFSEC     : Boolean := False;
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  secure access mode for DAC1
      DAC1SEC        : Boolean := False;
      --  secure access mode for ADF1
      ADF1SEC        : Boolean := False;
      --  unspecified
      Reserved_13_31 : HAL.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_SECCFGR1_Register_1 use record
      SPI3SEC        at 0 range 0 .. 0;
      LPUART1SEC     at 0 range 1 .. 1;
      I2C3SEC        at 0 range 2 .. 2;
      LPTIM1SEC      at 0 range 3 .. 3;
      LPTIM3SEC      at 0 range 4 .. 4;
      LPTIM4SEC      at 0 range 5 .. 5;
      OPAMPSEC       at 0 range 6 .. 6;
      COMPSEC        at 0 range 7 .. 7;
      ADC4SEC        at 0 range 8 .. 8;
      VREFBUFSEC     at 0 range 9 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      DAC1SEC        at 0 range 11 .. 11;
      ADF1SEC        at 0 range 12 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   --  TZSC privilege configuration register 1
   type TZSC_PRIVCFGR1_Register_1 is record
      --  privileged access mode for SPI3
      SPI3PRIV       : Boolean := False;
      --  privileged access mode for LPUART1
      LPUART1PRIV    : Boolean := False;
      --  privileged access mode for I2C3
      I2C3PRIV       : Boolean := False;
      --  privileged access mode for LPTIM1
      LPTIM1PRIV     : Boolean := False;
      --  privileged access mode for LPTIM3
      LPTIM3PRIV     : Boolean := False;
      --  privileged access mode for LPTIM4
      LPTIM4PRIV     : Boolean := False;
      --  privileged access mode for OPAMP
      OPAMPPRIV      : Boolean := False;
      --  privileged access mode for COMP
      COMPPRIV       : Boolean := False;
      --  privileged access mode for ADC4
      ADC4PRIV       : Boolean := False;
      --  privileged access mode for VREFBUF
      VREFBUFPRIV    : Boolean := False;
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  privileged access mode for DAC1
      DAC1PRIV       : Boolean := False;
      --  privileged access mode for ADF1
      ADF1PRIV       : Boolean := False;
      --  unspecified
      Reserved_13_31 : HAL.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_PRIVCFGR1_Register_1 use record
      SPI3PRIV       at 0 range 0 .. 0;
      LPUART1PRIV    at 0 range 1 .. 1;
      I2C3PRIV       at 0 range 2 .. 2;
      LPTIM1PRIV     at 0 range 3 .. 3;
      LPTIM3PRIV     at 0 range 4 .. 4;
      LPTIM4PRIV     at 0 range 5 .. 5;
      OPAMPPRIV      at 0 range 6 .. 6;
      COMPPRIV       at 0 range 7 .. 7;
      ADC4PRIV       at 0 range 8 .. 8;
      VREFBUFPRIV    at 0 range 9 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      DAC1PRIV       at 0 range 11 .. 11;
      ADF1PRIV       at 0 range 12 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  GTZC1_MPCBB1
   type GTZC1_MPCBB1_Peripheral is record
      --  MPCBB control register
      MPCBB1_CR         : aliased MPCBB1_CR_Register;
      --  GTZC1 SRAMz MPCBB configuration lock register
      MPCBB1_CFGLOCKR1  : aliased MPCBB1_CFGLOCKR1_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB1_SECCFGR0   : aliased MPCBB1_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB1_SECCFGR1   : aliased MPCBB1_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB1_SECCFGR2   : aliased MPCBB1_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB1_SECCFGR3   : aliased MPCBB1_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB1_SECCFGR4   : aliased MPCBB1_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB1_SECCFGR5   : aliased MPCBB1_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB1_SECCFGR6   : aliased MPCBB1_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB1_SECCFGR7   : aliased MPCBB1_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB1_SECCFGR8   : aliased MPCBB1_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB1_SECCFGR9   : aliased MPCBB1_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB1_SECCFGR10  : aliased MPCBB1_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB1_SECCFGR11  : aliased MPCBB1_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB1_SECCFGR12  : aliased MPCBB1_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB1_SECCFGR13  : aliased MPCBB1_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB1_SECCFGR14  : aliased MPCBB1_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB1_SECCFGR15  : aliased MPCBB1_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB1_SECCFGR16  : aliased MPCBB1_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB1_SECCFGR17  : aliased MPCBB1_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB1_SECCFGR18  : aliased MPCBB1_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB1_SECCFGR19  : aliased MPCBB1_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB1_SECCFGR20  : aliased MPCBB1_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB1_SECCFGR21  : aliased MPCBB1_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB1_SECCFGR22  : aliased MPCBB1_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB1_SECCFGR23  : aliased MPCBB1_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB1_SECCFGR24  : aliased MPCBB1_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB1_SECCFGR25  : aliased MPCBB1_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB1_SECCFGR26  : aliased MPCBB1_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB1_SECCFGR27  : aliased MPCBB1_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB1_SECCFGR28  : aliased MPCBB1_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB1_SECCFGR29  : aliased MPCBB1_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB1_SECCFGR30  : aliased MPCBB1_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB1_SECCFGR31  : aliased MPCBB1_SECCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB1_PRIVCFGR0  : aliased MPCBB1_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB1_PRIVCFGR1  : aliased MPCBB1_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB1_PRIVCFGR2  : aliased MPCBB1_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB1_PRIVCFGR3  : aliased MPCBB1_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB1_PRIVCFGR4  : aliased MPCBB1_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB1_PRIVCFGR5  : aliased MPCBB1_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB1_PRIVCFGR6  : aliased MPCBB1_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB1_PRIVCFGR7  : aliased MPCBB1_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB1_PRIVCFGR8  : aliased MPCBB1_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB1_PRIVCFGR9  : aliased MPCBB1_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB1_PRIVCFGR10 : aliased MPCBB1_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB1_PRIVCFGR11 : aliased MPCBB1_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB1_PRIVCFGR12 : aliased MPCBB1_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB1_PRIVCFGR13 : aliased MPCBB1_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB1_PRIVCFGR14 : aliased MPCBB1_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB1_PRIVCFGR15 : aliased MPCBB1_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB1_PRIVCFGR16 : aliased MPCBB1_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB1_PRIVCFGR17 : aliased MPCBB1_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB1_PRIVCFGR18 : aliased MPCBB1_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB1_PRIVCFGR19 : aliased MPCBB1_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB1_PRIVCFGR20 : aliased MPCBB1_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB1_PRIVCFGR21 : aliased MPCBB1_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB1_PRIVCFGR22 : aliased MPCBB1_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB1_PRIVCFGR23 : aliased MPCBB1_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB1_PRIVCFGR24 : aliased MPCBB1_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB1_PRIVCFGR25 : aliased MPCBB1_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB1_PRIVCFGR26 : aliased MPCBB1_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB1_PRIVCFGR27 : aliased MPCBB1_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB1_PRIVCFGR28 : aliased MPCBB1_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB1_PRIVCFGR29 : aliased MPCBB1_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB1_PRIVCFGR30 : aliased MPCBB1_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB1_PRIVCFGR31 : aliased MPCBB1_PRIVCFGR_Register;
   end record
     with Volatile;

   for GTZC1_MPCBB1_Peripheral use record
      MPCBB1_CR         at 16#0# range 0 .. 31;
      MPCBB1_CFGLOCKR1  at 16#10# range 0 .. 31;
      MPCBB1_SECCFGR0   at 16#100# range 0 .. 31;
      MPCBB1_SECCFGR1   at 16#104# range 0 .. 31;
      MPCBB1_SECCFGR2   at 16#108# range 0 .. 31;
      MPCBB1_SECCFGR3   at 16#10C# range 0 .. 31;
      MPCBB1_SECCFGR4   at 16#110# range 0 .. 31;
      MPCBB1_SECCFGR5   at 16#114# range 0 .. 31;
      MPCBB1_SECCFGR6   at 16#118# range 0 .. 31;
      MPCBB1_SECCFGR7   at 16#11C# range 0 .. 31;
      MPCBB1_SECCFGR8   at 16#120# range 0 .. 31;
      MPCBB1_SECCFGR9   at 16#124# range 0 .. 31;
      MPCBB1_SECCFGR10  at 16#128# range 0 .. 31;
      MPCBB1_SECCFGR11  at 16#12C# range 0 .. 31;
      MPCBB1_SECCFGR12  at 16#130# range 0 .. 31;
      MPCBB1_SECCFGR13  at 16#134# range 0 .. 31;
      MPCBB1_SECCFGR14  at 16#138# range 0 .. 31;
      MPCBB1_SECCFGR15  at 16#13C# range 0 .. 31;
      MPCBB1_SECCFGR16  at 16#140# range 0 .. 31;
      MPCBB1_SECCFGR17  at 16#144# range 0 .. 31;
      MPCBB1_SECCFGR18  at 16#148# range 0 .. 31;
      MPCBB1_SECCFGR19  at 16#14C# range 0 .. 31;
      MPCBB1_SECCFGR20  at 16#150# range 0 .. 31;
      MPCBB1_SECCFGR21  at 16#154# range 0 .. 31;
      MPCBB1_SECCFGR22  at 16#158# range 0 .. 31;
      MPCBB1_SECCFGR23  at 16#15C# range 0 .. 31;
      MPCBB1_SECCFGR24  at 16#160# range 0 .. 31;
      MPCBB1_SECCFGR25  at 16#164# range 0 .. 31;
      MPCBB1_SECCFGR26  at 16#168# range 0 .. 31;
      MPCBB1_SECCFGR27  at 16#16C# range 0 .. 31;
      MPCBB1_SECCFGR28  at 16#170# range 0 .. 31;
      MPCBB1_SECCFGR29  at 16#174# range 0 .. 31;
      MPCBB1_SECCFGR30  at 16#178# range 0 .. 31;
      MPCBB1_SECCFGR31  at 16#17C# range 0 .. 31;
      MPCBB1_PRIVCFGR0  at 16#200# range 0 .. 31;
      MPCBB1_PRIVCFGR1  at 16#204# range 0 .. 31;
      MPCBB1_PRIVCFGR2  at 16#208# range 0 .. 31;
      MPCBB1_PRIVCFGR3  at 16#20C# range 0 .. 31;
      MPCBB1_PRIVCFGR4  at 16#210# range 0 .. 31;
      MPCBB1_PRIVCFGR5  at 16#214# range 0 .. 31;
      MPCBB1_PRIVCFGR6  at 16#218# range 0 .. 31;
      MPCBB1_PRIVCFGR7  at 16#21C# range 0 .. 31;
      MPCBB1_PRIVCFGR8  at 16#220# range 0 .. 31;
      MPCBB1_PRIVCFGR9  at 16#224# range 0 .. 31;
      MPCBB1_PRIVCFGR10 at 16#228# range 0 .. 31;
      MPCBB1_PRIVCFGR11 at 16#22C# range 0 .. 31;
      MPCBB1_PRIVCFGR12 at 16#230# range 0 .. 31;
      MPCBB1_PRIVCFGR13 at 16#234# range 0 .. 31;
      MPCBB1_PRIVCFGR14 at 16#238# range 0 .. 31;
      MPCBB1_PRIVCFGR15 at 16#23C# range 0 .. 31;
      MPCBB1_PRIVCFGR16 at 16#240# range 0 .. 31;
      MPCBB1_PRIVCFGR17 at 16#244# range 0 .. 31;
      MPCBB1_PRIVCFGR18 at 16#248# range 0 .. 31;
      MPCBB1_PRIVCFGR19 at 16#24C# range 0 .. 31;
      MPCBB1_PRIVCFGR20 at 16#250# range 0 .. 31;
      MPCBB1_PRIVCFGR21 at 16#254# range 0 .. 31;
      MPCBB1_PRIVCFGR22 at 16#258# range 0 .. 31;
      MPCBB1_PRIVCFGR23 at 16#25C# range 0 .. 31;
      MPCBB1_PRIVCFGR24 at 16#260# range 0 .. 31;
      MPCBB1_PRIVCFGR25 at 16#264# range 0 .. 31;
      MPCBB1_PRIVCFGR26 at 16#268# range 0 .. 31;
      MPCBB1_PRIVCFGR27 at 16#26C# range 0 .. 31;
      MPCBB1_PRIVCFGR28 at 16#270# range 0 .. 31;
      MPCBB1_PRIVCFGR29 at 16#274# range 0 .. 31;
      MPCBB1_PRIVCFGR30 at 16#278# range 0 .. 31;
      MPCBB1_PRIVCFGR31 at 16#27C# range 0 .. 31;
   end record;

   --  GTZC1_MPCBB1
   GTZC1_MPCBB1_Periph : aliased GTZC1_MPCBB1_Peripheral
     with Import, Address => GTZC1_MPCBB1_Base;

   --  GTZC1_MPCBB1
   SEC_GTZC1_MPCBB1_Periph : aliased GTZC1_MPCBB1_Peripheral
     with Import, Address => SEC_GTZC1_MPCBB1_Base;

   --  GTZC1_MPCBB2
   type GTZC1_MPCBB2_Peripheral is record
      --  MPCBB control register
      MPCBB2_CR         : aliased MPCBB2_CR_Register;
      --  GTZC1 SRAMz MPCBB configuration lock register
      MPCBB2_CFGLOCKR1  : aliased MPCBB2_CFGLOCKR1_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB2_SECCFGR0   : aliased MPCBB2_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB2_SECCFGR1   : aliased MPCBB2_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB2_SECCFGR2   : aliased MPCBB2_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB2_SECCFGR3   : aliased MPCBB2_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB2_SECCFGR4   : aliased MPCBB2_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB2_SECCFGR5   : aliased MPCBB2_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB2_SECCFGR6   : aliased MPCBB2_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB2_SECCFGR7   : aliased MPCBB2_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB2_SECCFGR8   : aliased MPCBB2_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB2_SECCFGR9   : aliased MPCBB2_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB2_SECCFGR10  : aliased MPCBB2_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB2_SECCFGR11  : aliased MPCBB2_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB2_SECCFGR12  : aliased MPCBB2_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB2_SECCFGR13  : aliased MPCBB2_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB2_SECCFGR14  : aliased MPCBB2_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB2_SECCFGR15  : aliased MPCBB2_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB2_SECCFGR16  : aliased MPCBB2_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB2_SECCFGR17  : aliased MPCBB2_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB2_SECCFGR18  : aliased MPCBB2_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB2_SECCFGR19  : aliased MPCBB2_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB2_SECCFGR20  : aliased MPCBB2_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB2_SECCFGR21  : aliased MPCBB2_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB2_SECCFGR22  : aliased MPCBB2_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB2_SECCFGR23  : aliased MPCBB2_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB2_SECCFGR24  : aliased MPCBB2_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB2_SECCFGR25  : aliased MPCBB2_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB2_SECCFGR26  : aliased MPCBB2_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB2_SECCFGR27  : aliased MPCBB2_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB2_SECCFGR28  : aliased MPCBB2_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB2_SECCFGR29  : aliased MPCBB2_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB2_SECCFGR30  : aliased MPCBB2_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB2_SECCFGR31  : aliased MPCBB2_SECCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB2_PRIVCFGR0  : aliased MPCBB2_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB2_PRIVCFGR1  : aliased MPCBB2_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB2_PRIVCFGR2  : aliased MPCBB2_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB2_PRIVCFGR3  : aliased MPCBB2_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB2_PRIVCFGR4  : aliased MPCBB2_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB2_PRIVCFGR5  : aliased MPCBB2_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB2_PRIVCFGR6  : aliased MPCBB2_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB2_PRIVCFGR7  : aliased MPCBB2_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB2_PRIVCFGR8  : aliased MPCBB2_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB2_PRIVCFGR9  : aliased MPCBB2_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB2_PRIVCFGR10 : aliased MPCBB2_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB2_PRIVCFGR11 : aliased MPCBB2_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB2_PRIVCFGR12 : aliased MPCBB2_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB2_PRIVCFGR13 : aliased MPCBB2_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB2_PRIVCFGR14 : aliased MPCBB2_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB2_PRIVCFGR15 : aliased MPCBB2_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB2_PRIVCFGR16 : aliased MPCBB2_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB2_PRIVCFGR17 : aliased MPCBB2_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB2_PRIVCFGR18 : aliased MPCBB2_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB2_PRIVCFGR19 : aliased MPCBB2_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB2_PRIVCFGR20 : aliased MPCBB2_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB2_PRIVCFGR21 : aliased MPCBB2_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB2_PRIVCFGR22 : aliased MPCBB2_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB2_PRIVCFGR23 : aliased MPCBB2_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB2_PRIVCFGR24 : aliased MPCBB2_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB2_PRIVCFGR25 : aliased MPCBB2_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB2_PRIVCFGR26 : aliased MPCBB2_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB2_PRIVCFGR27 : aliased MPCBB2_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB2_PRIVCFGR28 : aliased MPCBB2_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB2_PRIVCFGR29 : aliased MPCBB2_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB2_PRIVCFGR30 : aliased MPCBB2_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB2_PRIVCFGR31 : aliased MPCBB2_PRIVCFGR_Register;
   end record
     with Volatile;

   for GTZC1_MPCBB2_Peripheral use record
      MPCBB2_CR         at 16#0# range 0 .. 31;
      MPCBB2_CFGLOCKR1  at 16#10# range 0 .. 31;
      MPCBB2_SECCFGR0   at 16#100# range 0 .. 31;
      MPCBB2_SECCFGR1   at 16#104# range 0 .. 31;
      MPCBB2_SECCFGR2   at 16#108# range 0 .. 31;
      MPCBB2_SECCFGR3   at 16#10C# range 0 .. 31;
      MPCBB2_SECCFGR4   at 16#110# range 0 .. 31;
      MPCBB2_SECCFGR5   at 16#114# range 0 .. 31;
      MPCBB2_SECCFGR6   at 16#118# range 0 .. 31;
      MPCBB2_SECCFGR7   at 16#11C# range 0 .. 31;
      MPCBB2_SECCFGR8   at 16#120# range 0 .. 31;
      MPCBB2_SECCFGR9   at 16#124# range 0 .. 31;
      MPCBB2_SECCFGR10  at 16#128# range 0 .. 31;
      MPCBB2_SECCFGR11  at 16#12C# range 0 .. 31;
      MPCBB2_SECCFGR12  at 16#130# range 0 .. 31;
      MPCBB2_SECCFGR13  at 16#134# range 0 .. 31;
      MPCBB2_SECCFGR14  at 16#138# range 0 .. 31;
      MPCBB2_SECCFGR15  at 16#13C# range 0 .. 31;
      MPCBB2_SECCFGR16  at 16#140# range 0 .. 31;
      MPCBB2_SECCFGR17  at 16#144# range 0 .. 31;
      MPCBB2_SECCFGR18  at 16#148# range 0 .. 31;
      MPCBB2_SECCFGR19  at 16#14C# range 0 .. 31;
      MPCBB2_SECCFGR20  at 16#150# range 0 .. 31;
      MPCBB2_SECCFGR21  at 16#154# range 0 .. 31;
      MPCBB2_SECCFGR22  at 16#158# range 0 .. 31;
      MPCBB2_SECCFGR23  at 16#15C# range 0 .. 31;
      MPCBB2_SECCFGR24  at 16#160# range 0 .. 31;
      MPCBB2_SECCFGR25  at 16#164# range 0 .. 31;
      MPCBB2_SECCFGR26  at 16#168# range 0 .. 31;
      MPCBB2_SECCFGR27  at 16#16C# range 0 .. 31;
      MPCBB2_SECCFGR28  at 16#170# range 0 .. 31;
      MPCBB2_SECCFGR29  at 16#174# range 0 .. 31;
      MPCBB2_SECCFGR30  at 16#178# range 0 .. 31;
      MPCBB2_SECCFGR31  at 16#17C# range 0 .. 31;
      MPCBB2_PRIVCFGR0  at 16#200# range 0 .. 31;
      MPCBB2_PRIVCFGR1  at 16#204# range 0 .. 31;
      MPCBB2_PRIVCFGR2  at 16#208# range 0 .. 31;
      MPCBB2_PRIVCFGR3  at 16#20C# range 0 .. 31;
      MPCBB2_PRIVCFGR4  at 16#210# range 0 .. 31;
      MPCBB2_PRIVCFGR5  at 16#214# range 0 .. 31;
      MPCBB2_PRIVCFGR6  at 16#218# range 0 .. 31;
      MPCBB2_PRIVCFGR7  at 16#21C# range 0 .. 31;
      MPCBB2_PRIVCFGR8  at 16#220# range 0 .. 31;
      MPCBB2_PRIVCFGR9  at 16#224# range 0 .. 31;
      MPCBB2_PRIVCFGR10 at 16#228# range 0 .. 31;
      MPCBB2_PRIVCFGR11 at 16#22C# range 0 .. 31;
      MPCBB2_PRIVCFGR12 at 16#230# range 0 .. 31;
      MPCBB2_PRIVCFGR13 at 16#234# range 0 .. 31;
      MPCBB2_PRIVCFGR14 at 16#238# range 0 .. 31;
      MPCBB2_PRIVCFGR15 at 16#23C# range 0 .. 31;
      MPCBB2_PRIVCFGR16 at 16#240# range 0 .. 31;
      MPCBB2_PRIVCFGR17 at 16#244# range 0 .. 31;
      MPCBB2_PRIVCFGR18 at 16#248# range 0 .. 31;
      MPCBB2_PRIVCFGR19 at 16#24C# range 0 .. 31;
      MPCBB2_PRIVCFGR20 at 16#250# range 0 .. 31;
      MPCBB2_PRIVCFGR21 at 16#254# range 0 .. 31;
      MPCBB2_PRIVCFGR22 at 16#258# range 0 .. 31;
      MPCBB2_PRIVCFGR23 at 16#25C# range 0 .. 31;
      MPCBB2_PRIVCFGR24 at 16#260# range 0 .. 31;
      MPCBB2_PRIVCFGR25 at 16#264# range 0 .. 31;
      MPCBB2_PRIVCFGR26 at 16#268# range 0 .. 31;
      MPCBB2_PRIVCFGR27 at 16#26C# range 0 .. 31;
      MPCBB2_PRIVCFGR28 at 16#270# range 0 .. 31;
      MPCBB2_PRIVCFGR29 at 16#274# range 0 .. 31;
      MPCBB2_PRIVCFGR30 at 16#278# range 0 .. 31;
      MPCBB2_PRIVCFGR31 at 16#27C# range 0 .. 31;
   end record;

   --  GTZC1_MPCBB2
   GTZC1_MPCBB2_Periph : aliased GTZC1_MPCBB2_Peripheral
     with Import, Address => GTZC1_MPCBB2_Base;

   --  GTZC1_MPCBB2
   SEC_GTZC1_MPCBB2_Periph : aliased GTZC1_MPCBB2_Peripheral
     with Import, Address => SEC_GTZC1_MPCBB2_Base;

   --  GTZC1_MPCBB3
   type GTZC1_MPCBB3_Peripheral is record
      --  MPCBB control register
      MPCBB3_CR         : aliased MPCBB3_CR_Register;
      --  GTZC1 SRAMz MPCBB configuration lock register
      MPCBB3_CFGLOCKR1  : aliased MPCBB3_CFGLOCKR1_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB3_SECCFGR0   : aliased MPCBB3_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB3_SECCFGR1   : aliased MPCBB3_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB3_SECCFGR2   : aliased MPCBB3_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB3_SECCFGR3   : aliased MPCBB3_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB3_SECCFGR4   : aliased MPCBB3_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB3_SECCFGR5   : aliased MPCBB3_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB3_SECCFGR6   : aliased MPCBB3_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB3_SECCFGR7   : aliased MPCBB3_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB3_SECCFGR8   : aliased MPCBB3_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB3_SECCFGR9   : aliased MPCBB3_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB3_SECCFGR10  : aliased MPCBB3_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB3_SECCFGR11  : aliased MPCBB3_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB3_SECCFGR12  : aliased MPCBB3_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB3_SECCFGR13  : aliased MPCBB3_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB3_SECCFGR14  : aliased MPCBB3_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB3_SECCFGR15  : aliased MPCBB3_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB3_SECCFGR16  : aliased MPCBB3_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB3_SECCFGR17  : aliased MPCBB3_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB3_SECCFGR18  : aliased MPCBB3_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB3_SECCFGR19  : aliased MPCBB3_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB3_SECCFGR20  : aliased MPCBB3_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB3_SECCFGR21  : aliased MPCBB3_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB3_SECCFGR22  : aliased MPCBB3_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB3_SECCFGR23  : aliased MPCBB3_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB3_SECCFGR24  : aliased MPCBB3_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB3_SECCFGR25  : aliased MPCBB3_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB3_SECCFGR26  : aliased MPCBB3_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB3_SECCFGR27  : aliased MPCBB3_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB3_SECCFGR28  : aliased MPCBB3_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB3_SECCFGR29  : aliased MPCBB3_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB3_SECCFGR30  : aliased MPCBB3_SECCFGR_Register;
      --  MPCBBx security configuration for super-block x register
      MPCBB3_SECCFGR31  : aliased MPCBB3_SECCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB3_PRIVCFGR0  : aliased MPCBB3_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB3_PRIVCFGR1  : aliased MPCBB3_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB3_PRIVCFGR2  : aliased MPCBB3_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB3_PRIVCFGR3  : aliased MPCBB3_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB3_PRIVCFGR4  : aliased MPCBB3_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB3_PRIVCFGR5  : aliased MPCBB3_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB3_PRIVCFGR6  : aliased MPCBB3_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB3_PRIVCFGR7  : aliased MPCBB3_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB3_PRIVCFGR8  : aliased MPCBB3_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB3_PRIVCFGR9  : aliased MPCBB3_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB3_PRIVCFGR10 : aliased MPCBB3_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB3_PRIVCFGR11 : aliased MPCBB3_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB3_PRIVCFGR12 : aliased MPCBB3_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB3_PRIVCFGR13 : aliased MPCBB3_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB3_PRIVCFGR14 : aliased MPCBB3_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB3_PRIVCFGR15 : aliased MPCBB3_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB3_PRIVCFGR16 : aliased MPCBB3_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB3_PRIVCFGR17 : aliased MPCBB3_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB3_PRIVCFGR18 : aliased MPCBB3_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB3_PRIVCFGR19 : aliased MPCBB3_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB3_PRIVCFGR20 : aliased MPCBB3_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB3_PRIVCFGR21 : aliased MPCBB3_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB3_PRIVCFGR22 : aliased MPCBB3_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB3_PRIVCFGR23 : aliased MPCBB3_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB3_PRIVCFGR24 : aliased MPCBB3_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB3_PRIVCFGR25 : aliased MPCBB3_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB3_PRIVCFGR26 : aliased MPCBB3_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB3_PRIVCFGR27 : aliased MPCBB3_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB3_PRIVCFGR28 : aliased MPCBB3_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB3_PRIVCFGR29 : aliased MPCBB3_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB3_PRIVCFGR30 : aliased MPCBB3_PRIVCFGR_Register;
      --  MPCBB privileged configuration for super-block x register
      MPCBB3_PRIVCFGR31 : aliased MPCBB3_PRIVCFGR_Register;
   end record
     with Volatile;

   for GTZC1_MPCBB3_Peripheral use record
      MPCBB3_CR         at 16#0# range 0 .. 31;
      MPCBB3_CFGLOCKR1  at 16#10# range 0 .. 31;
      MPCBB3_SECCFGR0   at 16#100# range 0 .. 31;
      MPCBB3_SECCFGR1   at 16#104# range 0 .. 31;
      MPCBB3_SECCFGR2   at 16#108# range 0 .. 31;
      MPCBB3_SECCFGR3   at 16#10C# range 0 .. 31;
      MPCBB3_SECCFGR4   at 16#110# range 0 .. 31;
      MPCBB3_SECCFGR5   at 16#114# range 0 .. 31;
      MPCBB3_SECCFGR6   at 16#118# range 0 .. 31;
      MPCBB3_SECCFGR7   at 16#11C# range 0 .. 31;
      MPCBB3_SECCFGR8   at 16#120# range 0 .. 31;
      MPCBB3_SECCFGR9   at 16#124# range 0 .. 31;
      MPCBB3_SECCFGR10  at 16#128# range 0 .. 31;
      MPCBB3_SECCFGR11  at 16#12C# range 0 .. 31;
      MPCBB3_SECCFGR12  at 16#130# range 0 .. 31;
      MPCBB3_SECCFGR13  at 16#134# range 0 .. 31;
      MPCBB3_SECCFGR14  at 16#138# range 0 .. 31;
      MPCBB3_SECCFGR15  at 16#13C# range 0 .. 31;
      MPCBB3_SECCFGR16  at 16#140# range 0 .. 31;
      MPCBB3_SECCFGR17  at 16#144# range 0 .. 31;
      MPCBB3_SECCFGR18  at 16#148# range 0 .. 31;
      MPCBB3_SECCFGR19  at 16#14C# range 0 .. 31;
      MPCBB3_SECCFGR20  at 16#150# range 0 .. 31;
      MPCBB3_SECCFGR21  at 16#154# range 0 .. 31;
      MPCBB3_SECCFGR22  at 16#158# range 0 .. 31;
      MPCBB3_SECCFGR23  at 16#15C# range 0 .. 31;
      MPCBB3_SECCFGR24  at 16#160# range 0 .. 31;
      MPCBB3_SECCFGR25  at 16#164# range 0 .. 31;
      MPCBB3_SECCFGR26  at 16#168# range 0 .. 31;
      MPCBB3_SECCFGR27  at 16#16C# range 0 .. 31;
      MPCBB3_SECCFGR28  at 16#170# range 0 .. 31;
      MPCBB3_SECCFGR29  at 16#174# range 0 .. 31;
      MPCBB3_SECCFGR30  at 16#178# range 0 .. 31;
      MPCBB3_SECCFGR31  at 16#17C# range 0 .. 31;
      MPCBB3_PRIVCFGR0  at 16#200# range 0 .. 31;
      MPCBB3_PRIVCFGR1  at 16#204# range 0 .. 31;
      MPCBB3_PRIVCFGR2  at 16#208# range 0 .. 31;
      MPCBB3_PRIVCFGR3  at 16#20C# range 0 .. 31;
      MPCBB3_PRIVCFGR4  at 16#210# range 0 .. 31;
      MPCBB3_PRIVCFGR5  at 16#214# range 0 .. 31;
      MPCBB3_PRIVCFGR6  at 16#218# range 0 .. 31;
      MPCBB3_PRIVCFGR7  at 16#21C# range 0 .. 31;
      MPCBB3_PRIVCFGR8  at 16#220# range 0 .. 31;
      MPCBB3_PRIVCFGR9  at 16#224# range 0 .. 31;
      MPCBB3_PRIVCFGR10 at 16#228# range 0 .. 31;
      MPCBB3_PRIVCFGR11 at 16#22C# range 0 .. 31;
      MPCBB3_PRIVCFGR12 at 16#230# range 0 .. 31;
      MPCBB3_PRIVCFGR13 at 16#234# range 0 .. 31;
      MPCBB3_PRIVCFGR14 at 16#238# range 0 .. 31;
      MPCBB3_PRIVCFGR15 at 16#23C# range 0 .. 31;
      MPCBB3_PRIVCFGR16 at 16#240# range 0 .. 31;
      MPCBB3_PRIVCFGR17 at 16#244# range 0 .. 31;
      MPCBB3_PRIVCFGR18 at 16#248# range 0 .. 31;
      MPCBB3_PRIVCFGR19 at 16#24C# range 0 .. 31;
      MPCBB3_PRIVCFGR20 at 16#250# range 0 .. 31;
      MPCBB3_PRIVCFGR21 at 16#254# range 0 .. 31;
      MPCBB3_PRIVCFGR22 at 16#258# range 0 .. 31;
      MPCBB3_PRIVCFGR23 at 16#25C# range 0 .. 31;
      MPCBB3_PRIVCFGR24 at 16#260# range 0 .. 31;
      MPCBB3_PRIVCFGR25 at 16#264# range 0 .. 31;
      MPCBB3_PRIVCFGR26 at 16#268# range 0 .. 31;
      MPCBB3_PRIVCFGR27 at 16#26C# range 0 .. 31;
      MPCBB3_PRIVCFGR28 at 16#270# range 0 .. 31;
      MPCBB3_PRIVCFGR29 at 16#274# range 0 .. 31;
      MPCBB3_PRIVCFGR30 at 16#278# range 0 .. 31;
      MPCBB3_PRIVCFGR31 at 16#27C# range 0 .. 31;
   end record;

   --  GTZC1_MPCBB3
   GTZC1_MPCBB3_Periph : aliased GTZC1_MPCBB3_Peripheral
     with Import, Address => GTZC1_MPCBB3_Base;

   --  GTZC1_MPCBB3
   SEC_GTZC1_MPCBB3_Periph : aliased GTZC1_MPCBB3_Peripheral
     with Import, Address => SEC_GTZC1_MPCBB3_Base;

   --  GTZC1_TZIC
   type GTZC1_TZIC_Peripheral is record
      --  TZIC interrupt enable register 1
      IER1 : aliased IER1_Register;
      --  TZIC interrupt enable register 2
      IER2 : aliased IER2_Register;
      --  TZIC interrupt enable register 3
      IER3 : aliased IER3_Register;
      --  TZIC interrupt enable register 4
      IER4 : aliased IER4_Register;
      --  TZIC status register 1
      SR1  : aliased SR1_Register;
      --  TZIC status register 2
      SR2  : aliased SR2_Register;
      --  TZIC status register 3
      SR3  : aliased SR3_Register;
      --  TZIC status register 4
      SR4  : aliased SR4_Register;
      --  TZIC flag clear register 1
      FCR1 : aliased FCR1_Register;
      --  TZIC flag clear register 2
      FCR2 : aliased FCR2_Register;
      --  TZIC flag clear register 3
      FCR3 : aliased FCR3_Register;
      --  TZIC flag clear register 3
      FCR4 : aliased FCR4_Register;
   end record
     with Volatile;

   for GTZC1_TZIC_Peripheral use record
      IER1 at 16#0# range 0 .. 31;
      IER2 at 16#4# range 0 .. 31;
      IER3 at 16#8# range 0 .. 31;
      IER4 at 16#C# range 0 .. 31;
      SR1  at 16#10# range 0 .. 31;
      SR2  at 16#14# range 0 .. 31;
      SR3  at 16#18# range 0 .. 31;
      SR4  at 16#1C# range 0 .. 31;
      FCR1 at 16#20# range 0 .. 31;
      FCR2 at 16#24# range 0 .. 31;
      FCR3 at 16#28# range 0 .. 31;
      FCR4 at 16#2C# range 0 .. 31;
   end record;

   --  GTZC1_TZIC
   GTZC1_TZIC_Periph : aliased GTZC1_TZIC_Peripheral
     with Import, Address => GTZC1_TZIC_Base;

   --  GTZC1_TZIC
   SEC_GTZC1_TZIC_Periph : aliased GTZC1_TZIC_Peripheral
     with Import, Address => SEC_GTZC1_TZIC_Base;

   --  GTZC1_TZSC
   type GTZC1_TZSC_Peripheral is record
      --  TZSC control register
      TZSC_CR          : aliased TZSC_CR_Register;
      --  TZSC secure configuration register 1
      TZSC_SECCFGR1    : aliased TZSC_SECCFGR1_Register;
      --  TZSC secure configuration register 2
      TZSC_SECCFGR2    : aliased TZSC_SECCFGR2_Register;
      --  TZSC secure configuration register 3
      TZSC_SECCFGR3    : aliased TZSC_SECCFGR3_Register;
      --  TZSC privilege configuration register 1
      TZSC_PRIVCFGR1   : aliased TZSC_PRIVCFGR1_Register;
      --  TZSC privilege configuration register 2
      TZSC_PRIVCFGR2   : aliased TZSC_PRIVCFGR2_Register;
      --  TZSC privilege configuration register 3
      TZSC_PRIVCFGR3   : aliased TZSC_PRIVCFGR3_Register;
      --  TZSC memory 1 sub-region A watermark configuration register
      TZSC_MPCWM1ACFGR : aliased TZSC_MPCWM1ACFGR_Register;
      --  TZSC memory 1 sub-region A watermark register
      TZSC_MPCWM1AR    : aliased TZSC_MPCWM1AR_Register;
      --  TZSC memory 1 sub-region B watermark configuration register
      TZSC_MPCWM1BCFGR : aliased TZSC_MPCWM1BCFGR_Register;
      --  TZSC memory 1 sub-region B watermark register
      TZSC_MPCWM1BR    : aliased TZSC_MPCWM1BR_Register;
      --  TZSC memory 2 sub-region A watermark configuration register
      TZSC_MPCWM2ACFGR : aliased TZSC_MPCWM2ACFGR_Register;
      --  TZSC memory 2 sub-region A watermark register
      TZSC_MPCWM2AR    : aliased TZSC_MPCWM2AR_Register;
      --  TZSC memory 2 sub-region B watermark configuration register
      TZSC_MPCWM2BCFGR : aliased TZSC_MPCWM2BCFGR_Register;
      --  TZSC memory 2 sub-region B watermark register
      TZSC_MPCWM2BR    : aliased TZSC_MPCWM2BR_Register;
      --  TZSC memory 3 sub-region A watermark configuration register
      TZSC_MPCWM3ACFGR : aliased TZSC_MPCWM3ACFGR_Register;
      --  TZSC memory 3 sub-region A watermark register
      TZSC_MPCWM3AR    : aliased TZSC_MPCWM3AR_Register;
      --  TZSC memory 4 sub-region A watermark configuration register
      TZSC_MPCWM4ACFGR : aliased TZSC_MPCWM4ACFGR_Register;
      --  TZSC memory 4 sub-region A watermark register
      TZSC_MPCWM4AR    : aliased TZSC_MPCWM4AR_Register;
      --  TZSC memory 5 sub-region A watermark configuration register
      TZSC_MPCWM5ACFGR : aliased TZSC_MPCWM5ACFGR_Register;
      --  TZSC memory 5 sub-region A watermark register
      TZSC_MPCWM5AR    : aliased TZSC_MPCWM5AR_Register;
      --  TZSC memory 5 sub-region B watermark configuration register
      TZSC_MPCWM5BCFGR : aliased TZSC_MPCWM5BCFGR_Register;
      --  TZSC memory 5 sub-region B watermark register
      TZSC_MPCWM5BR    : aliased TZSC_MPCWM5BR_Register;
   end record
     with Volatile;

   for GTZC1_TZSC_Peripheral use record
      TZSC_CR          at 16#0# range 0 .. 31;
      TZSC_SECCFGR1    at 16#10# range 0 .. 31;
      TZSC_SECCFGR2    at 16#14# range 0 .. 31;
      TZSC_SECCFGR3    at 16#18# range 0 .. 31;
      TZSC_PRIVCFGR1   at 16#20# range 0 .. 31;
      TZSC_PRIVCFGR2   at 16#24# range 0 .. 31;
      TZSC_PRIVCFGR3   at 16#28# range 0 .. 31;
      TZSC_MPCWM1ACFGR at 16#40# range 0 .. 31;
      TZSC_MPCWM1AR    at 16#44# range 0 .. 31;
      TZSC_MPCWM1BCFGR at 16#48# range 0 .. 31;
      TZSC_MPCWM1BR    at 16#4C# range 0 .. 31;
      TZSC_MPCWM2ACFGR at 16#50# range 0 .. 31;
      TZSC_MPCWM2AR    at 16#54# range 0 .. 31;
      TZSC_MPCWM2BCFGR at 16#58# range 0 .. 31;
      TZSC_MPCWM2BR    at 16#5C# range 0 .. 31;
      TZSC_MPCWM3ACFGR at 16#60# range 0 .. 31;
      TZSC_MPCWM3AR    at 16#64# range 0 .. 31;
      TZSC_MPCWM4ACFGR at 16#70# range 0 .. 31;
      TZSC_MPCWM4AR    at 16#74# range 0 .. 31;
      TZSC_MPCWM5ACFGR at 16#80# range 0 .. 31;
      TZSC_MPCWM5AR    at 16#84# range 0 .. 31;
      TZSC_MPCWM5BCFGR at 16#88# range 0 .. 31;
      TZSC_MPCWM5BR    at 16#8C# range 0 .. 31;
   end record;

   --  GTZC1_TZSC
   GTZC1_TZSC_Periph : aliased GTZC1_TZSC_Peripheral
     with Import, Address => GTZC1_TZSC_Base;

   --  GTZC1_TZSC
   SEC_GTZC1_TZSC_Periph : aliased GTZC1_TZSC_Peripheral
     with Import, Address => SEC_GTZC1_TZSC_Base;

   --  GTZC2_MPCBB4
   type GTZC2_MPCBB4_Peripheral is record
      --  MPCBB control register
      MPCBB4_CR        : aliased MPCBB4_CR_Register;
      --  GTZC2 SRAM4 MPCBB configuration lock register
      MPCBB4_CFGLOCK   : aliased MPCBB4_CFGLOCK_Register;
      --  MPCBB security configuration for super-block 0 register
      MPCBB4_SECCFGR0  : aliased MPCBB4_SECCFGR0_Register;
      --  MPCBB privileged configuration for super-block 0 register
      MPCBB4_PRIVCFGR0 : aliased MPCBB4_PRIVCFGR0_Register;
   end record
     with Volatile;

   for GTZC2_MPCBB4_Peripheral use record
      MPCBB4_CR        at 16#0# range 0 .. 31;
      MPCBB4_CFGLOCK   at 16#10# range 0 .. 31;
      MPCBB4_SECCFGR0  at 16#100# range 0 .. 31;
      MPCBB4_PRIVCFGR0 at 16#200# range 0 .. 31;
   end record;

   --  GTZC2_MPCBB4
   GTZC2_MPCBB4_Periph : aliased GTZC2_MPCBB4_Peripheral
     with Import, Address => GTZC2_MPCBB4_Base;

   --  GTZC2_MPCBB4
   SEC_GTZC2_MPCBB4_Periph : aliased GTZC2_MPCBB4_Peripheral
     with Import, Address => SEC_GTZC2_MPCBB4_Base;

   --  GTZC2_TZIC
   type GTZC2_TZIC_Peripheral is record
      --  TZIC interrupt enable register 1
      IER1 : aliased IER1_Register_1;
      --  TZIC interrupt enable register 2
      IER2 : aliased IER2_Register_1;
      --  TZIC status register 1
      SR1  : aliased SR1_Register_1;
      --  TZIC status register 2
      SR2  : aliased SR2_Register_1;
      --  TZIC flag clear register 1
      FCR1 : aliased FCR1_Register_1;
      --  TZIC flag clear register 2
      FCR2 : aliased FCR2_Register_1;
   end record
     with Volatile;

   for GTZC2_TZIC_Peripheral use record
      IER1 at 16#0# range 0 .. 31;
      IER2 at 16#4# range 0 .. 31;
      SR1  at 16#10# range 0 .. 31;
      SR2  at 16#14# range 0 .. 31;
      FCR1 at 16#20# range 0 .. 31;
      FCR2 at 16#24# range 0 .. 31;
   end record;

   --  GTZC2_TZIC
   GTZC2_TZIC_Periph : aliased GTZC2_TZIC_Peripheral
     with Import, Address => GTZC2_TZIC_Base;

   --  GTZC2_TZIC
   SEC_GTZC2_TZIC_Periph : aliased GTZC2_TZIC_Peripheral
     with Import, Address => SEC_GTZC2_TZIC_Base;

   --  GTZC2_TZSC
   type GTZC2_TZSC_Peripheral is record
      --  TZSC control register
      TZSC_CR        : aliased TZSC_CR_Register;
      --  TZSC secure configuration register 1
      TZSC_SECCFGR1  : aliased TZSC_SECCFGR1_Register_1;
      --  TZSC privilege configuration register 1
      TZSC_PRIVCFGR1 : aliased TZSC_PRIVCFGR1_Register_1;
   end record
     with Volatile;

   for GTZC2_TZSC_Peripheral use record
      TZSC_CR        at 16#0# range 0 .. 31;
      TZSC_SECCFGR1  at 16#10# range 0 .. 31;
      TZSC_PRIVCFGR1 at 16#20# range 0 .. 31;
   end record;

   --  GTZC2_TZSC
   GTZC2_TZSC_Periph : aliased GTZC2_TZSC_Peripheral
     with Import, Address => GTZC2_TZSC_Base;

   --  GTZC2_TZSC
   SEC_GTZC2_TZSC_Periph : aliased GTZC2_TZSC_Peripheral
     with Import, Address => SEC_GTZC2_TZSC_Base;

end STM32_SVD.GTZC;
