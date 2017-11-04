------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2017, AdaCore                     --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of STMicroelectronics nor the names of its       --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
--                                                                          --
--  This file is based on:                                                  --
--                                                                          --
--   @file    stm32l43[5|7]xx.h                                             --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   CMSIS STM32F407xx Device Peripheral Access Layer Header File. --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

--  This file provides declarations for devices on the STM32F40xxx MCUs
--  manufactured by ST Microelectronics.  For example, an STM32F405.

with STM32_SVD;     use STM32_SVD;

with STM32.GPIO;    use STM32.GPIO;
with STM32.I2C;     use STM32.I2C;
with STM32.SPI;     use STM32.SPI;
with STM32.Timers;  use STM32.Timers;
with STM32.DAC;     use STM32.DAC;
with STM32.ADC;     use STM32.ADC;
with STM32.DMA;     use STM32.DMA;

package STM32.Device is
   pragma Elaborate_Body;

   Unknown_Device : exception;
   --  Raised by the routines below for a device passed as an actual parameter
   --  when that device is not present on the given hardware instance.

   procedure Enable_Clock (This : aliased in out GPIO_Port);
   procedure Enable_Clock (Point : GPIO_Point);
   procedure Enable_Clock (Points : GPIO_Points);
   procedure Enable_Clock (This : in out Timer);

   Timer_1  : aliased Timer with Import, Volatile, Address => TIM1_Base;
   Timer_2  : aliased Timer with Import, Volatile, Address => TIM2_Base;
   Timer_3  : aliased Timer with Import, Volatile, Address => TIM3_Base;
   Timer_6  : aliased Timer with Import, Volatile, Address => TIM6_Base;
   Timer_7  : aliased Timer with Import, Volatile, Address => TIM7_Base;
   Timer_15 : aliased Timer with Import, Volatile, Address => TIM15_Base;
   Timer_16 : aliased Timer with Import, Volatile, Address => TIM16_Base;

   procedure Reset (This : aliased in out GPIO_Port)
     with Inline;
   procedure Reset (Point : GPIO_Point)
     with Inline;
   procedure Reset (Points : GPIO_Points)
     with Inline;
   procedure Reset (This : in out Timer);

   function GPIO_Port_Representation (Port : GPIO_Port) return UInt4
     with Inline;

   GPIO_A : aliased GPIO_Port with Import, Volatile, Address => GPIOA_Base;
   GPIO_B : aliased GPIO_Port with Import, Volatile, Address => GPIOB_Base;
   GPIO_C : aliased GPIO_Port with Import, Volatile, Address => GPIOC_Base;
   GPIO_D : aliased GPIO_Port with Import, Volatile, Address => GPIOD_Base;
   GPIO_E : aliased GPIO_Port with Import, Volatile, Address => GPIOE_Base;

   PA0  : aliased GPIO_Point := (GPIO_A'Access, Pin_0);
   PA1  : aliased GPIO_Point := (GPIO_A'Access, Pin_1);
   PA2  : aliased GPIO_Point := (GPIO_A'Access, Pin_2);
   PA3  : aliased GPIO_Point := (GPIO_A'Access, Pin_3);
   PA4  : aliased GPIO_Point := (GPIO_A'Access, Pin_4);
   PA5  : aliased GPIO_Point := (GPIO_A'Access, Pin_5);
   PA6  : aliased GPIO_Point := (GPIO_A'Access, Pin_6);
   PA7  : aliased GPIO_Point := (GPIO_A'Access, Pin_7);
   PA8  : aliased GPIO_Point := (GPIO_A'Access, Pin_8);
   PA9  : aliased GPIO_Point := (GPIO_A'Access, Pin_9);
   PA10 : aliased GPIO_Point := (GPIO_A'Access, Pin_10);
   PA11 : aliased GPIO_Point := (GPIO_A'Access, Pin_11);
   PA12 : aliased GPIO_Point := (GPIO_A'Access, Pin_12);
   PA13 : aliased GPIO_Point := (GPIO_A'Access, Pin_13);
   PA14 : aliased GPIO_Point := (GPIO_A'Access, Pin_14);
   PA15 : aliased GPIO_Point := (GPIO_A'Access, Pin_15);
   PB0  : aliased GPIO_Point := (GPIO_B'Access, Pin_0);
   PB1  : aliased GPIO_Point := (GPIO_B'Access, Pin_1);
   PB2  : aliased GPIO_Point := (GPIO_B'Access, Pin_2);
   PB3  : aliased GPIO_Point := (GPIO_B'Access, Pin_3);
   PB4  : aliased GPIO_Point := (GPIO_B'Access, Pin_4);
   PB5  : aliased GPIO_Point := (GPIO_B'Access, Pin_5);
   PB6  : aliased GPIO_Point := (GPIO_B'Access, Pin_6);
   PB7  : aliased GPIO_Point := (GPIO_B'Access, Pin_7);
   PB8  : aliased GPIO_Point := (GPIO_B'Access, Pin_8);
   PB9  : aliased GPIO_Point := (GPIO_B'Access, Pin_9);
   PB10 : aliased GPIO_Point := (GPIO_B'Access, Pin_10);
   PB11 : aliased GPIO_Point := (GPIO_B'Access, Pin_11);
   PB12 : aliased GPIO_Point := (GPIO_B'Access, Pin_12);
   PB13 : aliased GPIO_Point := (GPIO_B'Access, Pin_13);
   PB14 : aliased GPIO_Point := (GPIO_B'Access, Pin_14);
   PB15 : aliased GPIO_Point := (GPIO_B'Access, Pin_15);
   PC0  : aliased GPIO_Point := (GPIO_C'Access, Pin_0);
   PC1  : aliased GPIO_Point := (GPIO_C'Access, Pin_1);
   PC2  : aliased GPIO_Point := (GPIO_C'Access, Pin_2);
   PC3  : aliased GPIO_Point := (GPIO_C'Access, Pin_3);
   PC4  : aliased GPIO_Point := (GPIO_C'Access, Pin_4);
   PC5  : aliased GPIO_Point := (GPIO_C'Access, Pin_5);
   PC6  : aliased GPIO_Point := (GPIO_C'Access, Pin_6);
   PC7  : aliased GPIO_Point := (GPIO_C'Access, Pin_7);
   PC8  : aliased GPIO_Point := (GPIO_C'Access, Pin_8);
   PC9  : aliased GPIO_Point := (GPIO_C'Access, Pin_9);
   PC10 : aliased GPIO_Point := (GPIO_C'Access, Pin_10);
   PC11 : aliased GPIO_Point := (GPIO_C'Access, Pin_11);
   PC12 : aliased GPIO_Point := (GPIO_C'Access, Pin_12);
   PC13 : aliased GPIO_Point := (GPIO_C'Access, Pin_13);
   PC14 : aliased GPIO_Point := (GPIO_C'Access, Pin_14);
   PC15 : aliased GPIO_Point := (GPIO_C'Access, Pin_15);
   PD0  : aliased GPIO_Point := (GPIO_D'Access, Pin_0);
   PD1  : aliased GPIO_Point := (GPIO_D'Access, Pin_1);
   PD2  : aliased GPIO_Point := (GPIO_D'Access, Pin_2);
   PD3  : aliased GPIO_Point := (GPIO_D'Access, Pin_3);
   PD4  : aliased GPIO_Point := (GPIO_D'Access, Pin_4);
   PD5  : aliased GPIO_Point := (GPIO_D'Access, Pin_5);
   PD6  : aliased GPIO_Point := (GPIO_D'Access, Pin_6);
   PD7  : aliased GPIO_Point := (GPIO_D'Access, Pin_7);
   PD8  : aliased GPIO_Point := (GPIO_D'Access, Pin_8);
   PD9  : aliased GPIO_Point := (GPIO_D'Access, Pin_9);
   PD10 : aliased GPIO_Point := (GPIO_D'Access, Pin_10);
   PD11 : aliased GPIO_Point := (GPIO_D'Access, Pin_11);
   PD12 : aliased GPIO_Point := (GPIO_D'Access, Pin_12);
   PD13 : aliased GPIO_Point := (GPIO_D'Access, Pin_13);
   PD14 : aliased GPIO_Point := (GPIO_D'Access, Pin_14);
   PD15 : aliased GPIO_Point := (GPIO_D'Access, Pin_15);
   PE0  : aliased GPIO_Point := (GPIO_E'Access, Pin_0);
   PE1  : aliased GPIO_Point := (GPIO_E'Access, Pin_1);
   PE2  : aliased GPIO_Point := (GPIO_E'Access, Pin_2);
   PE3  : aliased GPIO_Point := (GPIO_E'Access, Pin_3);
   PE4  : aliased GPIO_Point := (GPIO_E'Access, Pin_4);
   PE5  : aliased GPIO_Point := (GPIO_E'Access, Pin_5);
   PE6  : aliased GPIO_Point := (GPIO_E'Access, Pin_6);
   PE7  : aliased GPIO_Point := (GPIO_E'Access, Pin_7);
   PE8  : aliased GPIO_Point := (GPIO_E'Access, Pin_8);
   PE9  : aliased GPIO_Point := (GPIO_E'Access, Pin_9);
   PE10 : aliased GPIO_Point := (GPIO_E'Access, Pin_10);
   PE11 : aliased GPIO_Point := (GPIO_E'Access, Pin_11);
   PE12 : aliased GPIO_Point := (GPIO_E'Access, Pin_12);
   PE13 : aliased GPIO_Point := (GPIO_E'Access, Pin_13);
   PE14 : aliased GPIO_Point := (GPIO_E'Access, Pin_14);
   PE15 : aliased GPIO_Point := (GPIO_E'Access, Pin_15);

   GPIO_AF_RTC_50Hz_0  : constant GPIO_Alternate_Function;
   GPIO_AF_MCO_0       : constant GPIO_Alternate_Function;
   GPIO_AF_TAMPER_0    : constant GPIO_Alternate_Function;
   GPIO_AF_SWJ_0       : constant GPIO_Alternate_Function;
   GPIO_AF_TRACE_0     : constant GPIO_Alternate_Function;
   GPIO_AF_TIM1_1      : constant GPIO_Alternate_Function;
   GPIO_AF_TIM2_1      : constant GPIO_Alternate_Function;
   GPIO_AF_TIM3_2      : constant GPIO_Alternate_Function;
   GPIO_AF_TIM4_2      : constant GPIO_Alternate_Function;
   GPIO_AF_TIM5_2      : constant GPIO_Alternate_Function;
   GPIO_AF_TIM8_3      : constant GPIO_Alternate_Function;
   GPIO_AF_TIM9_3      : constant GPIO_Alternate_Function;
   GPIO_AF_TIM10_3     : constant GPIO_Alternate_Function;
   GPIO_AF_TIM11_3     : constant GPIO_Alternate_Function;
   GPIO_AF_I2C1_4      : constant GPIO_Alternate_Function;
   GPIO_AF_I2C2_4      : constant GPIO_Alternate_Function;
   GPIO_AF_I2C3_4      : constant GPIO_Alternate_Function;
   GPIO_AF_SPI1_5      : constant GPIO_Alternate_Function;
   GPIO_AF_SPI2_5      : constant GPIO_Alternate_Function;
   GPIO_AF_I2S2_5      : constant GPIO_Alternate_Function;
   GPIO_AF_I2S2ext_5   : constant GPIO_Alternate_Function;
   GPIO_AF_SPI3_6      : constant GPIO_Alternate_Function;
   GPIO_AF_I2S3_6      : constant GPIO_Alternate_Function;
   GPIO_AF_I2Sext_6    : constant GPIO_Alternate_Function;
   GPIO_AF_I2S3ext_7   : constant GPIO_Alternate_Function;
   GPIO_AF_USART1_7    : constant GPIO_Alternate_Function;
   GPIO_AF_USART2_7    : constant GPIO_Alternate_Function;
   GPIO_AF_USART3_7    : constant GPIO_Alternate_Function;
   GPIO_AF_UART4_8     : constant GPIO_Alternate_Function;
   GPIO_AF_UART5_8     : constant GPIO_Alternate_Function;
   GPIO_AF_USART6_8    : constant GPIO_Alternate_Function;
   GPIO_AF_CAN1_9      : constant GPIO_Alternate_Function;
   GPIO_AF_CAN2_9      : constant GPIO_Alternate_Function;
   GPIO_AF_TIM12_9     : constant GPIO_Alternate_Function;
   GPIO_AF_TIM13_9     : constant GPIO_Alternate_Function;
   GPIO_AF_TIM14_9     : constant GPIO_Alternate_Function;
   GPIO_AF_OTG_FS_10   : constant GPIO_Alternate_Function;
   GPIO_AF_OTG_HS_10   : constant GPIO_Alternate_Function;
   GPIO_AF_ETH_11      : constant GPIO_Alternate_Function;
   GPIO_AF_FMC_12      : constant GPIO_Alternate_Function;
   GPIO_AF_OTG_FS_12   : constant GPIO_Alternate_Function;
   GPIO_AF_DCMI_13     : constant GPIO_Alternate_Function;
   GPIO_AF_EVENTOUT_15 : constant GPIO_Alternate_Function;

   -----------------------------
   -- Reset and Clock Control --
   -----------------------------

   type RCC_System_Clocks is record
      SYSCLK  : UInt32;
      HCLK    : UInt32;
      PCLK1   : UInt32;
      PCLK2   : UInt32;
      TIMCLK1 : UInt32;
      TIMCLK2 : UInt32;
      I2SCLK  : UInt32;
   end record;

   function System_Clock_Frequencies return RCC_System_Clocks;

   Internal_I2C_Port_1 : aliased Internal_I2C_Port
     with Import, Volatile, Address => I2C1_Base;
   Internal_I2C_Port_2 : aliased Internal_I2C_Port
     with Import, Volatile, Address => I2C2_Base;
   Internal_I2C_Port_3 : aliased Internal_I2C_Port
     with Import, Volatile, Address => I2C3_Base;
   Internal_I2C_Port_4 : aliased Internal_I2C_Port
     with Import, Volatile, Address => I2C4_Base;

   I2C_1 : aliased I2C_Port (Internal_I2C_Port_1'Access);
   I2C_2 : aliased I2C_Port (Internal_I2C_Port_2'Access);
   I2C_3 : aliased I2C_Port (Internal_I2C_Port_3'Access);
   I2C_4 : aliased I2C_Port (Internal_I2C_Port_4'Access);

   type I2C_Port_Id is (I2C_Id_1, I2C_Id_2, I2C_Id_3, I2C_Id_4);

   function As_Port_Id (Port : I2C_Port) return I2C_Port_Id with Inline;
   procedure Enable_Clock (This : I2C_Port);
   procedure Enable_Clock (This : I2C_Port_Id);
   procedure Reset (This : I2C_Port);
   procedure Reset (This : I2C_Port_Id);
   Internal_SPI_1 : aliased Internal_SPI_Port
     with Import, Volatile, Address => SPI1_Base;
   Internal_SPI_2 : aliased Internal_SPI_Port
     with Import, Volatile, Address => SPI2_Base;
   Internal_SPI_3 : aliased Internal_SPI_Port
     with Import, Volatile, Address => SPI3_Base;

   SPI_1 : aliased SPI_Port (Internal_SPI_1'Access);
   SPI_2 : aliased SPI_Port (Internal_SPI_2'Access);
   SPI_3 : aliased SPI_Port (Internal_SPI_3'Access);

   procedure Enable_Clock (This : SPI_Port);
   procedure Reset (This : in out SPI_Port);

   ADC : aliased Analog_To_Digital_Converter with Import, Volatile, Address => ADC_Base;

   procedure Enable_Clock (This : aliased in out Analog_To_Digital_Converter);
   procedure Reset_All_ADC_Units;

   DAC_1 : aliased Digital_To_Analog_Converter with Import, Volatile, Address => DAC1_Base;

   DAC_Channel_1_IO : GPIO_Point renames PA4;
   DAC_Channel_2_IO : GPIO_Point renames PA5;

   procedure Enable_Clock (This : aliased in out Digital_To_Analog_Converter);

   procedure Reset (This : aliased in out Digital_To_Analog_Converter);

   ---------
   -- DMA --
   ---------

   DMA_1 : aliased DMA_Controller with Import, Volatile, Address => DMA1_Base;
   DMA_2 : aliased DMA_Controller with Import, Volatile, Address => DMA2_Base;

   procedure Enable_Clock (This : aliased in out DMA_Controller);
   procedure Reset (This : aliased in out DMA_Controller);

private

   GPIO_AF_RTC_50Hz_0  : constant GPIO_Alternate_Function := 0;
   GPIO_AF_MCO_0       : constant GPIO_Alternate_Function := 0;
   GPIO_AF_TAMPER_0    : constant GPIO_Alternate_Function := 0;
   GPIO_AF_SWJ_0       : constant GPIO_Alternate_Function := 0;
   GPIO_AF_TRACE_0     : constant GPIO_Alternate_Function := 0;
   GPIO_AF_TIM1_1      : constant GPIO_Alternate_Function := 1;
   GPIO_AF_TIM2_1      : constant GPIO_Alternate_Function := 1;
   GPIO_AF_TIM3_2      : constant GPIO_Alternate_Function := 2;
   GPIO_AF_TIM4_2      : constant GPIO_Alternate_Function := 2;
   GPIO_AF_TIM5_2      : constant GPIO_Alternate_Function := 2;
   GPIO_AF_TIM8_3      : constant GPIO_Alternate_Function := 3;
   GPIO_AF_TIM9_3      : constant GPIO_Alternate_Function := 3;
   GPIO_AF_TIM10_3     : constant GPIO_Alternate_Function := 3;
   GPIO_AF_TIM11_3     : constant GPIO_Alternate_Function := 3;
   GPIO_AF_I2C1_4      : constant GPIO_Alternate_Function := 4;
   GPIO_AF_I2C2_4      : constant GPIO_Alternate_Function := 4;
   GPIO_AF_I2C3_4      : constant GPIO_Alternate_Function := 4;
   GPIO_AF_SPI1_5      : constant GPIO_Alternate_Function := 5;
   GPIO_AF_SPI2_5      : constant GPIO_Alternate_Function := 5;
   GPIO_AF_I2S2_5      : constant GPIO_Alternate_Function := 5;
   GPIO_AF_I2S2ext_5   : constant GPIO_Alternate_Function := 5;
   GPIO_AF_SPI3_6      : constant GPIO_Alternate_Function := 6;
   GPIO_AF_I2S3_6      : constant GPIO_Alternate_Function := 6;
   GPIO_AF_I2Sext_6    : constant GPIO_Alternate_Function := 6;
   GPIO_AF_I2S3ext_7   : constant GPIO_Alternate_Function := 7;
   GPIO_AF_USART1_7    : constant GPIO_Alternate_Function := 7;
   GPIO_AF_USART2_7    : constant GPIO_Alternate_Function := 7;
   GPIO_AF_USART3_7    : constant GPIO_Alternate_Function := 7;
   GPIO_AF_UART4_8     : constant GPIO_Alternate_Function := 8;
   GPIO_AF_UART5_8     : constant GPIO_Alternate_Function := 8;
   GPIO_AF_USART6_8    : constant GPIO_Alternate_Function := 8;
   GPIO_AF_CAN1_9      : constant GPIO_Alternate_Function := 9;
   GPIO_AF_CAN2_9      : constant GPIO_Alternate_Function := 9;
   GPIO_AF_TIM12_9     : constant GPIO_Alternate_Function := 9;
   GPIO_AF_TIM13_9     : constant GPIO_Alternate_Function := 9;
   GPIO_AF_TIM14_9     : constant GPIO_Alternate_Function := 9;
   GPIO_AF_OTG_FS_10   : constant GPIO_Alternate_Function := 10;
   GPIO_AF_OTG_HS_10   : constant GPIO_Alternate_Function := 10;
   GPIO_AF_ETH_11      : constant GPIO_Alternate_Function := 11;
   GPIO_AF_FMC_12      : constant GPIO_Alternate_Function := 12;
   GPIO_AF_OTG_FS_12   : constant GPIO_Alternate_Function := 12;
   GPIO_AF_DCMI_13     : constant GPIO_Alternate_Function := 13;
   GPIO_AF_EVENTOUT_15 : constant GPIO_Alternate_Function := 15;


end STM32.Device;
