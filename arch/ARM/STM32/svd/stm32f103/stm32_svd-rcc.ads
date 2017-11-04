--  This spec has been automatically generated from STM32F103.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.RCC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype CR_HSITRIM_Field is HAL.UInt5;
   subtype CR_HSICAL_Field is HAL.UInt8;

   --  Clock control register
   type CR_Register is record
      --  Internal High Speed clock enable
      HSION          : Boolean := True;
      --  Read-only. Internal High Speed clock ready flag
      HSIRDY         : Boolean := True;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  Internal High Speed clock trimming
      HSITRIM        : CR_HSITRIM_Field := 16#10#;
      --  Read-only. Internal High Speed clock Calibration
      HSICAL         : CR_HSICAL_Field := 16#0#;
      --  External High Speed clock enable
      HSEON          : Boolean := False;
      --  Read-only. External High Speed clock ready flag
      HSERDY         : Boolean := False;
      --  External High Speed clock Bypass
      HSEBYP         : Boolean := False;
      --  Clock Security System enable
      CSSON          : Boolean := False;
      --  unspecified
      Reserved_20_23 : HAL.UInt4 := 16#0#;
      --  PLL enable
      PLLON          : Boolean := False;
      --  Read-only. PLL clock ready flag
      PLLRDY         : Boolean := False;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      HSION          at 0 range 0 .. 0;
      HSIRDY         at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      HSITRIM        at 0 range 3 .. 7;
      HSICAL         at 0 range 8 .. 15;
      HSEON          at 0 range 16 .. 16;
      HSERDY         at 0 range 17 .. 17;
      HSEBYP         at 0 range 18 .. 18;
      CSSON          at 0 range 19 .. 19;
      Reserved_20_23 at 0 range 20 .. 23;
      PLLON          at 0 range 24 .. 24;
      PLLRDY         at 0 range 25 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype CFGR_SW_Field is HAL.UInt2;
   subtype CFGR_SWS_Field is HAL.UInt2;
   subtype CFGR_HPRE_Field is HAL.UInt4;
   --  CFGR_PPRE array element
   subtype CFGR_PPRE_Element is HAL.UInt3;

   --  CFGR_PPRE array
   type CFGR_PPRE_Field_Array is array (1 .. 2) of CFGR_PPRE_Element
     with Component_Size => 3, Size => 6;

   --  Type definition for CFGR_PPRE
   type CFGR_PPRE_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PPRE as a value
            Val : HAL.UInt6;
         when True =>
            --  PPRE as an array
            Arr : CFGR_PPRE_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 6;

   for CFGR_PPRE_Field use record
      Val at 0 range 0 .. 5;
      Arr at 0 range 0 .. 5;
   end record;

   subtype CFGR_ADCPRE_Field is HAL.UInt2;
   subtype CFGR_PLLMUL_Field is HAL.UInt4;
   subtype CFGR_MCO_Field is HAL.UInt3;

   --  Clock configuration register (RCC_CFGR)
   type CFGR_Register is record
      --  System clock Switch
      SW             : CFGR_SW_Field := 16#0#;
      --  Read-only. System Clock Switch Status
      SWS            : CFGR_SWS_Field := 16#0#;
      --  AHB prescaler
      HPRE           : CFGR_HPRE_Field := 16#0#;
      --  APB Low speed prescaler (APB1)
      PPRE           : CFGR_PPRE_Field := (As_Array => False, Val => 16#0#);
      --  ADC prescaler
      ADCPRE         : CFGR_ADCPRE_Field := 16#0#;
      --  PLL entry clock source
      PLLSRC         : Boolean := False;
      --  HSE divider for PLL entry
      PLLXTPRE       : Boolean := False;
      --  PLL Multiplication Factor
      PLLMUL         : CFGR_PLLMUL_Field := 16#0#;
      --  USB OTG FS prescaler
      OTGFSPRE       : Boolean := False;
      --  unspecified
      Reserved_23_23 : HAL.Bit := 16#0#;
      --  Microcontroller clock output
      MCO            : CFGR_MCO_Field := 16#0#;
      --  unspecified
      Reserved_27_31 : HAL.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CFGR_Register use record
      SW             at 0 range 0 .. 1;
      SWS            at 0 range 2 .. 3;
      HPRE           at 0 range 4 .. 7;
      PPRE           at 0 range 8 .. 13;
      ADCPRE         at 0 range 14 .. 15;
      PLLSRC         at 0 range 16 .. 16;
      PLLXTPRE       at 0 range 17 .. 17;
      PLLMUL         at 0 range 18 .. 21;
      OTGFSPRE       at 0 range 22 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      MCO            at 0 range 24 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   --  Clock interrupt register (RCC_CIR)
   type CIR_Register is record
      --  Read-only. LSI Ready Interrupt flag
      LSIRDYF        : Boolean := False;
      --  Read-only. LSE Ready Interrupt flag
      LSERDYF        : Boolean := False;
      --  Read-only. HSI Ready Interrupt flag
      HSIRDYF        : Boolean := False;
      --  Read-only. HSE Ready Interrupt flag
      HSERDYF        : Boolean := False;
      --  Read-only. PLL Ready Interrupt flag
      PLLRDYF        : Boolean := False;
      --  unspecified
      Reserved_5_6   : HAL.UInt2 := 16#0#;
      --  Read-only. Clock Security System Interrupt flag
      CSSF           : Boolean := False;
      --  LSI Ready Interrupt Enable
      LSIRDYIE       : Boolean := False;
      --  LSE Ready Interrupt Enable
      LSERDYIE       : Boolean := False;
      --  HSI Ready Interrupt Enable
      HSIRDYIE       : Boolean := False;
      --  HSE Ready Interrupt Enable
      HSERDYIE       : Boolean := False;
      --  PLL Ready Interrupt Enable
      PLLRDYIE       : Boolean := False;
      --  unspecified
      Reserved_13_15 : HAL.UInt3 := 16#0#;
      --  Write-only. LSI Ready Interrupt Clear
      LSIRDYC        : Boolean := False;
      --  Write-only. LSE Ready Interrupt Clear
      LSERDYC        : Boolean := False;
      --  Write-only. HSI Ready Interrupt Clear
      HSIRDYC        : Boolean := False;
      --  Write-only. HSE Ready Interrupt Clear
      HSERDYC        : Boolean := False;
      --  Write-only. PLL Ready Interrupt Clear
      PLLRDYC        : Boolean := False;
      --  unspecified
      Reserved_21_22 : HAL.UInt2 := 16#0#;
      --  Write-only. Clock security system interrupt clear
      CSSC           : Boolean := False;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CIR_Register use record
      LSIRDYF        at 0 range 0 .. 0;
      LSERDYF        at 0 range 1 .. 1;
      HSIRDYF        at 0 range 2 .. 2;
      HSERDYF        at 0 range 3 .. 3;
      PLLRDYF        at 0 range 4 .. 4;
      Reserved_5_6   at 0 range 5 .. 6;
      CSSF           at 0 range 7 .. 7;
      LSIRDYIE       at 0 range 8 .. 8;
      LSERDYIE       at 0 range 9 .. 9;
      HSIRDYIE       at 0 range 10 .. 10;
      HSERDYIE       at 0 range 11 .. 11;
      PLLRDYIE       at 0 range 12 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      LSIRDYC        at 0 range 16 .. 16;
      LSERDYC        at 0 range 17 .. 17;
      HSIRDYC        at 0 range 18 .. 18;
      HSERDYC        at 0 range 19 .. 19;
      PLLRDYC        at 0 range 20 .. 20;
      Reserved_21_22 at 0 range 21 .. 22;
      CSSC           at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  APB2 peripheral reset register (RCC_APB2RSTR)
   type APB2RSTR_Register is record
      --  Alternate function I/O reset
      AFIORST        : Boolean := False;
      --  unspecified
      Reserved_1_1   : HAL.Bit := 16#0#;
      --  IO port A reset
      IOPARST        : Boolean := False;
      --  IO port B reset
      IOPBRST        : Boolean := False;
      --  IO port C reset
      IOPCRST        : Boolean := False;
      --  IO port D reset
      IOPDRST        : Boolean := False;
      --  IO port E reset
      IOPERST        : Boolean := False;
      --  IO port F reset
      IOPFRST        : Boolean := False;
      --  IO port G reset
      IOPGRST        : Boolean := False;
      --  ADC 1 interface reset
      ADC1RST        : Boolean := False;
      --  ADC 2 interface reset
      ADC2RST        : Boolean := False;
      --  TIM1 timer reset
      TIM1RST        : Boolean := False;
      --  SPI 1 reset
      SPI1RST        : Boolean := False;
      --  TIM8 timer reset
      TIM8RST        : Boolean := False;
      --  USART1 reset
      USART1RST      : Boolean := False;
      --  ADC 3 interface reset
      ADC3RST        : Boolean := False;
      --  unspecified
      Reserved_16_18 : HAL.UInt3 := 16#0#;
      --  TIM9 timer reset
      TIM9RST        : Boolean := False;
      --  TIM10 timer reset
      TIM10RST       : Boolean := False;
      --  TIM11 timer reset
      TIM11RST       : Boolean := False;
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for APB2RSTR_Register use record
      AFIORST        at 0 range 0 .. 0;
      Reserved_1_1   at 0 range 1 .. 1;
      IOPARST        at 0 range 2 .. 2;
      IOPBRST        at 0 range 3 .. 3;
      IOPCRST        at 0 range 4 .. 4;
      IOPDRST        at 0 range 5 .. 5;
      IOPERST        at 0 range 6 .. 6;
      IOPFRST        at 0 range 7 .. 7;
      IOPGRST        at 0 range 8 .. 8;
      ADC1RST        at 0 range 9 .. 9;
      ADC2RST        at 0 range 10 .. 10;
      TIM1RST        at 0 range 11 .. 11;
      SPI1RST        at 0 range 12 .. 12;
      TIM8RST        at 0 range 13 .. 13;
      USART1RST      at 0 range 14 .. 14;
      ADC3RST        at 0 range 15 .. 15;
      Reserved_16_18 at 0 range 16 .. 18;
      TIM9RST        at 0 range 19 .. 19;
      TIM10RST       at 0 range 20 .. 20;
      TIM11RST       at 0 range 21 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  APB1 peripheral reset register (RCC_APB1RSTR)
   type APB1RSTR_Register is record
      --  Timer 2 reset
      TIM2RST        : Boolean := False;
      --  Timer 3 reset
      TIM3RST        : Boolean := False;
      --  Timer 4 reset
      TIM4RST        : Boolean := False;
      --  Timer 5 reset
      TIM5RST        : Boolean := False;
      --  Timer 6 reset
      TIM6RST        : Boolean := False;
      --  Timer 7 reset
      TIM7RST        : Boolean := False;
      --  Timer 12 reset
      TIM12RST       : Boolean := False;
      --  Timer 13 reset
      TIM13RST       : Boolean := False;
      --  Timer 14 reset
      TIM14RST       : Boolean := False;
      --  unspecified
      Reserved_9_10  : HAL.UInt2 := 16#0#;
      --  Window watchdog reset
      WWDGRST        : Boolean := False;
      --  unspecified
      Reserved_12_13 : HAL.UInt2 := 16#0#;
      --  SPI2 reset
      SPI2RST        : Boolean := False;
      --  SPI3 reset
      SPI3RST        : Boolean := False;
      --  unspecified
      Reserved_16_16 : HAL.Bit := 16#0#;
      --  USART 2 reset
      USART2RST      : Boolean := False;
      --  USART 3 reset
      USART3RST      : Boolean := False;
      --  UART 4 reset
      UART4RST       : Boolean := False;
      --  UART 5 reset
      UART5RST       : Boolean := False;
      --  I2C1 reset
      I2C1RST        : Boolean := False;
      --  I2C2 reset
      I2C2RST        : Boolean := False;
      --  USB reset
      USBRST         : Boolean := False;
      --  unspecified
      Reserved_24_24 : HAL.Bit := 16#0#;
      --  CAN reset
      CANRST         : Boolean := False;
      --  unspecified
      Reserved_26_26 : HAL.Bit := 16#0#;
      --  Backup interface reset
      BKPRST         : Boolean := False;
      --  Power interface reset
      PWRRST         : Boolean := False;
      --  DAC interface reset
      DACRST         : Boolean := False;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for APB1RSTR_Register use record
      TIM2RST        at 0 range 0 .. 0;
      TIM3RST        at 0 range 1 .. 1;
      TIM4RST        at 0 range 2 .. 2;
      TIM5RST        at 0 range 3 .. 3;
      TIM6RST        at 0 range 4 .. 4;
      TIM7RST        at 0 range 5 .. 5;
      TIM12RST       at 0 range 6 .. 6;
      TIM13RST       at 0 range 7 .. 7;
      TIM14RST       at 0 range 8 .. 8;
      Reserved_9_10  at 0 range 9 .. 10;
      WWDGRST        at 0 range 11 .. 11;
      Reserved_12_13 at 0 range 12 .. 13;
      SPI2RST        at 0 range 14 .. 14;
      SPI3RST        at 0 range 15 .. 15;
      Reserved_16_16 at 0 range 16 .. 16;
      USART2RST      at 0 range 17 .. 17;
      USART3RST      at 0 range 18 .. 18;
      UART4RST       at 0 range 19 .. 19;
      UART5RST       at 0 range 20 .. 20;
      I2C1RST        at 0 range 21 .. 21;
      I2C2RST        at 0 range 22 .. 22;
      USBRST         at 0 range 23 .. 23;
      Reserved_24_24 at 0 range 24 .. 24;
      CANRST         at 0 range 25 .. 25;
      Reserved_26_26 at 0 range 26 .. 26;
      BKPRST         at 0 range 27 .. 27;
      PWRRST         at 0 range 28 .. 28;
      DACRST         at 0 range 29 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   --  AHB Peripheral Clock enable register (RCC_AHBENR)
   type AHBENR_Register is record
      --  DMA1 clock enable
      DMA1EN         : Boolean := False;
      --  DMA2 clock enable
      DMA2EN         : Boolean := False;
      --  SRAM interface clock enable
      SRAMEN         : Boolean := True;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  FLITF clock enable
      FLITFEN        : Boolean := True;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  CRC clock enable
      CRCEN          : Boolean := False;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  FSMC clock enable
      FSMCEN         : Boolean := False;
      --  unspecified
      Reserved_9_9   : HAL.Bit := 16#0#;
      --  SDIO clock enable
      SDIOEN         : Boolean := False;
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for AHBENR_Register use record
      DMA1EN         at 0 range 0 .. 0;
      DMA2EN         at 0 range 1 .. 1;
      SRAMEN         at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      FLITFEN        at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      CRCEN          at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      FSMCEN         at 0 range 8 .. 8;
      Reserved_9_9   at 0 range 9 .. 9;
      SDIOEN         at 0 range 10 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   --  APB2 peripheral clock enable register (RCC_APB2ENR)
   type APB2ENR_Register is record
      --  Alternate function I/O clock enable
      AFIOEN         : Boolean := False;
      --  unspecified
      Reserved_1_1   : HAL.Bit := 16#0#;
      --  I/O port A clock enable
      IOPAEN         : Boolean := False;
      --  I/O port B clock enable
      IOPBEN         : Boolean := False;
      --  I/O port C clock enable
      IOPCEN         : Boolean := False;
      --  I/O port D clock enable
      IOPDEN         : Boolean := False;
      --  I/O port E clock enable
      IOPEEN         : Boolean := False;
      --  I/O port F clock enable
      IOPFEN         : Boolean := False;
      --  I/O port G clock enable
      IOPGEN         : Boolean := False;
      --  ADC 1 interface clock enable
      ADC1EN         : Boolean := False;
      --  ADC 2 interface clock enable
      ADC2EN         : Boolean := False;
      --  TIM1 Timer clock enable
      TIM1EN         : Boolean := False;
      --  SPI 1 clock enable
      SPI1EN         : Boolean := False;
      --  TIM8 Timer clock enable
      TIM8EN         : Boolean := False;
      --  USART1 clock enable
      USART1EN       : Boolean := False;
      --  ADC3 interface clock enable
      ADC3EN         : Boolean := False;
      --  unspecified
      Reserved_16_18 : HAL.UInt3 := 16#0#;
      --  TIM9 Timer clock enable
      TIM9EN         : Boolean := False;
      --  TIM10 Timer clock enable
      TIM10EN        : Boolean := False;
      --  TIM11 Timer clock enable
      TIM11EN        : Boolean := False;
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for APB2ENR_Register use record
      AFIOEN         at 0 range 0 .. 0;
      Reserved_1_1   at 0 range 1 .. 1;
      IOPAEN         at 0 range 2 .. 2;
      IOPBEN         at 0 range 3 .. 3;
      IOPCEN         at 0 range 4 .. 4;
      IOPDEN         at 0 range 5 .. 5;
      IOPEEN         at 0 range 6 .. 6;
      IOPFEN         at 0 range 7 .. 7;
      IOPGEN         at 0 range 8 .. 8;
      ADC1EN         at 0 range 9 .. 9;
      ADC2EN         at 0 range 10 .. 10;
      TIM1EN         at 0 range 11 .. 11;
      SPI1EN         at 0 range 12 .. 12;
      TIM8EN         at 0 range 13 .. 13;
      USART1EN       at 0 range 14 .. 14;
      ADC3EN         at 0 range 15 .. 15;
      Reserved_16_18 at 0 range 16 .. 18;
      TIM9EN         at 0 range 19 .. 19;
      TIM10EN        at 0 range 20 .. 20;
      TIM11EN        at 0 range 21 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  APB1 peripheral clock enable register (RCC_APB1ENR)
   type APB1ENR_Register is record
      --  Timer 2 clock enable
      TIM2EN         : Boolean := False;
      --  Timer 3 clock enable
      TIM3EN         : Boolean := False;
      --  Timer 4 clock enable
      TIM4EN         : Boolean := False;
      --  Timer 5 clock enable
      TIM5EN         : Boolean := False;
      --  Timer 6 clock enable
      TIM6EN         : Boolean := False;
      --  Timer 7 clock enable
      TIM7EN         : Boolean := False;
      --  Timer 12 clock enable
      TIM12EN        : Boolean := False;
      --  Timer 13 clock enable
      TIM13EN        : Boolean := False;
      --  Timer 14 clock enable
      TIM14EN        : Boolean := False;
      --  unspecified
      Reserved_9_10  : HAL.UInt2 := 16#0#;
      --  Window watchdog clock enable
      WWDGEN         : Boolean := False;
      --  unspecified
      Reserved_12_13 : HAL.UInt2 := 16#0#;
      --  SPI 2 clock enable
      SPI2EN         : Boolean := False;
      --  SPI 3 clock enable
      SPI3EN         : Boolean := False;
      --  unspecified
      Reserved_16_16 : HAL.Bit := 16#0#;
      --  USART 2 clock enable
      USART2EN       : Boolean := False;
      --  USART 3 clock enable
      USART3EN       : Boolean := False;
      --  UART 4 clock enable
      UART4EN        : Boolean := False;
      --  UART 5 clock enable
      UART5EN        : Boolean := False;
      --  I2C 1 clock enable
      I2C1EN         : Boolean := False;
      --  I2C 2 clock enable
      I2C2EN         : Boolean := False;
      --  USB clock enable
      USBEN          : Boolean := False;
      --  unspecified
      Reserved_24_24 : HAL.Bit := 16#0#;
      --  CAN clock enable
      CANEN          : Boolean := False;
      --  unspecified
      Reserved_26_26 : HAL.Bit := 16#0#;
      --  Backup interface clock enable
      BKPEN          : Boolean := False;
      --  Power interface clock enable
      PWREN          : Boolean := False;
      --  DAC interface clock enable
      DACEN          : Boolean := False;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for APB1ENR_Register use record
      TIM2EN         at 0 range 0 .. 0;
      TIM3EN         at 0 range 1 .. 1;
      TIM4EN         at 0 range 2 .. 2;
      TIM5EN         at 0 range 3 .. 3;
      TIM6EN         at 0 range 4 .. 4;
      TIM7EN         at 0 range 5 .. 5;
      TIM12EN        at 0 range 6 .. 6;
      TIM13EN        at 0 range 7 .. 7;
      TIM14EN        at 0 range 8 .. 8;
      Reserved_9_10  at 0 range 9 .. 10;
      WWDGEN         at 0 range 11 .. 11;
      Reserved_12_13 at 0 range 12 .. 13;
      SPI2EN         at 0 range 14 .. 14;
      SPI3EN         at 0 range 15 .. 15;
      Reserved_16_16 at 0 range 16 .. 16;
      USART2EN       at 0 range 17 .. 17;
      USART3EN       at 0 range 18 .. 18;
      UART4EN        at 0 range 19 .. 19;
      UART5EN        at 0 range 20 .. 20;
      I2C1EN         at 0 range 21 .. 21;
      I2C2EN         at 0 range 22 .. 22;
      USBEN          at 0 range 23 .. 23;
      Reserved_24_24 at 0 range 24 .. 24;
      CANEN          at 0 range 25 .. 25;
      Reserved_26_26 at 0 range 26 .. 26;
      BKPEN          at 0 range 27 .. 27;
      PWREN          at 0 range 28 .. 28;
      DACEN          at 0 range 29 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype BDCR_RTCSEL_Field is HAL.UInt2;

   --  Backup domain control register (RCC_BDCR)
   type BDCR_Register is record
      --  External Low Speed oscillator enable
      LSEON          : Boolean := False;
      --  Read-only. External Low Speed oscillator ready
      LSERDY         : Boolean := False;
      --  External Low Speed oscillator bypass
      LSEBYP         : Boolean := False;
      --  unspecified
      Reserved_3_7   : HAL.UInt5 := 16#0#;
      --  RTC clock source selection
      RTCSEL         : BDCR_RTCSEL_Field := 16#0#;
      --  unspecified
      Reserved_10_14 : HAL.UInt5 := 16#0#;
      --  RTC clock enable
      RTCEN          : Boolean := False;
      --  Backup domain software reset
      BDRST          : Boolean := False;
      --  unspecified
      Reserved_17_31 : HAL.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for BDCR_Register use record
      LSEON          at 0 range 0 .. 0;
      LSERDY         at 0 range 1 .. 1;
      LSEBYP         at 0 range 2 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      RTCSEL         at 0 range 8 .. 9;
      Reserved_10_14 at 0 range 10 .. 14;
      RTCEN          at 0 range 15 .. 15;
      BDRST          at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   --  Control/status register (RCC_CSR)
   type CSR_Register is record
      --  Internal low speed oscillator enable
      LSION          : Boolean := False;
      --  Read-only. Internal low speed oscillator ready
      LSIRDY         : Boolean := False;
      --  unspecified
      Reserved_2_23  : HAL.UInt22 := 16#0#;
      --  Remove reset flag
      RMVF           : Boolean := False;
      --  unspecified
      Reserved_25_25 : HAL.Bit := 16#0#;
      --  PIN reset flag
      PINRSTF        : Boolean := True;
      --  POR/PDR reset flag
      PORRSTF        : Boolean := True;
      --  Software reset flag
      SFTRSTF        : Boolean := False;
      --  Independent watchdog reset flag
      IWDGRSTF       : Boolean := False;
      --  Window watchdog reset flag
      WWDGRSTF       : Boolean := False;
      --  Low-power reset flag
      LPWRRSTF       : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CSR_Register use record
      LSION          at 0 range 0 .. 0;
      LSIRDY         at 0 range 1 .. 1;
      Reserved_2_23  at 0 range 2 .. 23;
      RMVF           at 0 range 24 .. 24;
      Reserved_25_25 at 0 range 25 .. 25;
      PINRSTF        at 0 range 26 .. 26;
      PORRSTF        at 0 range 27 .. 27;
      SFTRSTF        at 0 range 28 .. 28;
      IWDGRSTF       at 0 range 29 .. 29;
      WWDGRSTF       at 0 range 30 .. 30;
      LPWRRSTF       at 0 range 31 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Reset and clock control
   type RCC_Peripheral is record
      --  Clock control register
      CR       : aliased CR_Register;
      --  Clock configuration register (RCC_CFGR)
      CFGR     : aliased CFGR_Register;
      --  Clock interrupt register (RCC_CIR)
      CIR      : aliased CIR_Register;
      --  APB2 peripheral reset register (RCC_APB2RSTR)
      APB2RSTR : aliased APB2RSTR_Register;
      --  APB1 peripheral reset register (RCC_APB1RSTR)
      APB1RSTR : aliased APB1RSTR_Register;
      --  AHB Peripheral Clock enable register (RCC_AHBENR)
      AHBENR   : aliased AHBENR_Register;
      --  APB2 peripheral clock enable register (RCC_APB2ENR)
      APB2ENR  : aliased APB2ENR_Register;
      --  APB1 peripheral clock enable register (RCC_APB1ENR)
      APB1ENR  : aliased APB1ENR_Register;
      --  Backup domain control register (RCC_BDCR)
      BDCR     : aliased BDCR_Register;
      --  Control/status register (RCC_CSR)
      CSR      : aliased CSR_Register;
   end record
     with Volatile;

   for RCC_Peripheral use record
      CR       at 16#0# range 0 .. 31;
      CFGR     at 16#4# range 0 .. 31;
      CIR      at 16#8# range 0 .. 31;
      APB2RSTR at 16#C# range 0 .. 31;
      APB1RSTR at 16#10# range 0 .. 31;
      AHBENR   at 16#14# range 0 .. 31;
      APB2ENR  at 16#18# range 0 .. 31;
      APB1ENR  at 16#1C# range 0 .. 31;
      BDCR     at 16#20# range 0 .. 31;
      CSR      at 16#24# range 0 .. 31;
   end record;

   --  Reset and clock control
   RCC_Periph : aliased RCC_Peripheral
     with Import, Address => System'To_Address (16#40021000#);

end STM32_SVD.RCC;
