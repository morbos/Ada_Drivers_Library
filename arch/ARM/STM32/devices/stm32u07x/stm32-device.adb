------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
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
--     3. Neither the name of the copyright holder nor the names of its     --
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
------------------------------------------------------------------------------

with System; use System;
pragma Warnings (Off, "* is an internal GNAT unit");
with System.BB.Parameters;
pragma Warnings (On, "* is an internal GNAT unit");

with STM32_SVD.RCC; use STM32_SVD.RCC;

package body STM32.Device is

   HSE_VALUE : constant := System.BB.Parameters.HSE_Clock;
   --  External oscillator in Hz

   HSI_VALUE : constant := 16_000_000;
   --  Internal oscillator in Hz

   HPRE_Presc_Table : constant array (UInt4) of UInt32 :=
     (1, 1, 1, 1, 1, 1, 1, 1, 2, 4, 8, 16, 64, 128, 256, 512);

   PPRE_Presc_Table : constant array (UInt3) of UInt32 :=
     (1, 1, 1, 1, 2, 4, 8, 16);

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : aliased in out GPIO_Port) is
   begin
      if This'Address = GPIOA_Base then
         RCC_Periph.RCC_IOPENR.GPIOAEN := True;
      elsif This'Address = GPIOB_Base then
         RCC_Periph.RCC_IOPENR.GPIOBEN := True;
      elsif This'Address = GPIOC_Base then
         RCC_Periph.RCC_IOPENR.GPIOCEN := True;
      elsif This'Address = GPIOD_Base then
         RCC_Periph.RCC_IOPENR.GPIODEN := True;
      elsif This'Address = GPIOE_Base then
         RCC_Periph.RCC_IOPENR.GPIOEEN := True;
      else
         raise Unknown_Device;
      end if;
   end Enable_Clock;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (Point : GPIO_Point)
   is
   begin
      Enable_Clock (Point.Periph.all);
   end Enable_Clock;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (Points : GPIO_Points)
   is
   begin
      for Point of Points loop
         Enable_Clock (Point.Periph.all);
      end loop;
   end Enable_Clock;


   ------------------
   -- Disable_Clock --
   ------------------

   procedure Disable_Clock (This : aliased in out GPIO_Port) is
   begin
      if This'Address = GPIOA_Base then
         RCC_Periph.RCC_IOPENR.GPIOAEN := False;
      elsif This'Address = GPIOB_Base then
         RCC_Periph.RCC_IOPENR.GPIOBEN := False;
      elsif This'Address = GPIOC_Base then
         RCC_Periph.RCC_IOPENR.GPIOCEN := False;
      elsif This'Address = GPIOD_Base then
         RCC_Periph.RCC_IOPENR.GPIODEN := False;
      elsif This'Address = GPIOE_Base then
         RCC_Periph.RCC_IOPENR.GPIOEEN := False;
      else
         raise Unknown_Device;
      end if;
   end Disable_Clock;

   ------------------
   -- Disable_Clock --
   ------------------

   procedure Disable_Clock (Point : GPIO_Point)
   is
   begin
      Disable_Clock (Point.Periph.all);
   end Disable_Clock;

   ------------------
   -- Disable_Clock --
   ------------------

   procedure Disable_Clock (Points : GPIO_Points)
   is
   begin
      for Point of Points loop
         Disable_Clock (Point.Periph.all);
      end loop;
   end Disable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : aliased in out GPIO_Port) is
   begin
      if This'Address = GPIOA_Base then
         RCC_Periph.RCC_IOPRSTR.GPIOARST := True;
         RCC_Periph.RCC_IOPRSTR.GPIOARST := False;
      elsif This'Address = GPIOB_Base then
         RCC_Periph.RCC_IOPRSTR.GPIOBRST := True;
         RCC_Periph.RCC_IOPRSTR.GPIOBRST := False;
      elsif This'Address = GPIOC_Base then
         RCC_Periph.RCC_IOPRSTR.GPIOCRST := True;
         RCC_Periph.RCC_IOPRSTR.GPIOCRST := False;
      elsif This'Address = GPIOD_Base then
         RCC_Periph.RCC_IOPRSTR.GPIODRST := True;
         RCC_Periph.RCC_IOPRSTR.GPIODRST := False;
      elsif This'Address = GPIOE_Base then
         RCC_Periph.RCC_IOPRSTR.GPIOERST := True;
         RCC_Periph.RCC_IOPRSTR.GPIOERST := False;
      else
         raise Unknown_Device;
      end if;
   end Reset;

   -----------
   -- Reset --
   -----------

   procedure Reset (Point : GPIO_Point) is
   begin
      Reset (Point.Periph.all);
   end Reset;

   -----------
   -- Reset --
   -----------

   procedure Reset (Points : GPIO_Points)
   is
      Do_Reset : Boolean;
   begin
      for J in Points'Range loop
         Do_Reset := True;
         for K in Points'First .. J - 1 loop
            if Points (K).Periph = Points (J).Periph then
               Do_Reset := False;

               exit;
            end if;
         end loop;

         if Do_Reset then
            Reset (Points (J).Periph.all);
         end if;
      end loop;
   end Reset;
   ------------------------------
   -- GPIO_Port_Representation --
   ------------------------------

   function GPIO_Port_Representation (Port : GPIO_Port) return UInt4 is
   begin
      --  TODO: rather ugly to have this board-specific range here
      if Port'Address = GPIOA_Base then
         return 0;
      elsif Port'Address = GPIOB_Base then
         return 1;
      elsif Port'Address = GPIOC_Base then
         return 2;
      elsif Port'Address = GPIOD_Base then
         return 3;
      elsif Port'Address = GPIOE_Base then
         return 4;
      else
         raise Program_Error;
      end if;
   end GPIO_Port_Representation;

   ------------------
   -- Enable_Clock --
   ------------------

--   procedure Enable_Clock (This : aliased in out DMA_Controller) is
--   begin
--      if This'Address = STM32_SVD.DMA1_Base then
--         RCC_Periph.AHB1ENR.DMA1EN := True;
--      elsif This'Address = STM32_SVD.DMA2_Base then
--         RCC_Periph.AHB1ENR.DMA2EN := True;
--      else
--         raise Unknown_Device;
--      end if;
--   end Enable_Clock;

   -----------
   -- Reset --
   -----------

--   procedure Reset (This : aliased in out DMA_Controller) is
--   begin
--      if This'Address = STM32_SVD.DMA1_Base then
--         RCC_Periph.AHB1RSTR.DMA1RST := True;
--         RCC_Periph.AHB1RSTR.DMA1RST := False;
--      elsif This'Address = STM32_SVD.DMA2_Base then
--         RCC_Periph.AHB1RSTR.DMA2RST := True;
--         RCC_Periph.AHB1RSTR.DMA2RST := False;
--      else
--         raise Unknown_Device;
--      end if;
--   end Reset;

   ----------------
   -- As_Port_Id --
   ----------------

   function As_Port_Id (Port : I2C_Port) return I2C_Port_Id is
   begin
      if Port.Periph.all'Address = I2C1_Base then
         return I2C_Id_1;
      elsif Port.Periph.all'Address = I2C2_Base then
         return I2C_Id_2;
      elsif Port.Periph.all'Address = I2C3_Base then
         return I2C_Id_3;
      else
         raise Unknown_Device;
      end if;
   end As_Port_Id;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : I2C_Port) is
   begin
      Enable_Clock (As_Port_Id (This));
   end Enable_Clock;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : I2C_Port_Id) is
   begin
      case This is
         when I2C_Id_1 =>
            RCC_Periph.RCC_APBENR1.I2C1EN := True;
         when I2C_Id_2 =>
            RCC_Periph.RCC_APBENR1.I2C2EN := True;
         when I2C_Id_3 =>
            RCC_Periph.RCC_APBENR1.I2C3EN := True;
      end case;
   end Enable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : I2C_Port) is
   begin
      Reset (As_Port_Id (This));
   end Reset;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : I2C_Port_Id) is
   begin
      case This is
         when I2C_Id_1 =>
            RCC_Periph.RCC_APBRSTR1.I2C1RST := True;
            RCC_Periph.RCC_APBRSTR1.I2C1RST := False;
         when I2C_Id_2 =>
            RCC_Periph.RCC_APBRSTR1.I2C2RST := True;
            RCC_Periph.RCC_APBRSTR1.I2C2RST := False;
         when I2C_Id_3 =>
            RCC_Periph.RCC_APBRSTR1.I2C3RST := True;
            RCC_Periph.RCC_APBRSTR1.I2C3RST := False;
      end case;
   end Reset;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : SPI_Port) is
   begin
      if This.Periph.all'Address = SPI1_Base then
         RCC_Periph.RCC_APBENR2.SPI1EN := True;
      elsif This.Periph.all'Address = SPI2_Base then
         RCC_Periph.RCC_APBENR1.SPI2EN := True;
      else
         raise Unknown_Device;
      end if;
   end Enable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : in out SPI_Port) is
   begin
      if This.Periph.all'Address = SPI1_Base then
         RCC_Periph.RCC_APBRSTR2.SPI1RST := True;
         RCC_Periph.RCC_APBRSTR2.SPI1RST := False;
      elsif This.Periph.all'Address = SPI2_Base then
         RCC_Periph.RCC_APBRSTR1.SPI2RST := True;
         RCC_Periph.RCC_APBRSTR1.SPI2RST := False;
      elsif This.Periph.all'Address = SPI3_Base then
         RCC_Periph.RCC_APBRSTR1.SPI3RST := True;
         RCC_Periph.RCC_APBRSTR1.SPI3RST := False;
      else
         raise Unknown_Device;
      end if;
   end Reset;
   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : in out Timer) is
   begin
      if This'Address = TIM1_Base then
         RCC_Periph.RCC_APBENR2.TIM1EN := True;
      elsif This'Address = TIM6_Base then
         RCC_Periph.RCC_APBENR1.TIM6EN := True;
      elsif This'Address = TIM7_Base then
         RCC_Periph.RCC_APBENR1.TIM7EN := True;
      else
         raise Unknown_Device;
      end if;
   end Enable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : in out Timer) is
   begin
      if This'Address = TIM1_Base then
         RCC_Periph.RCC_APBRSTR2.TIM1RST := True;
         RCC_Periph.RCC_APBRSTR2.TIM1RST := False;
      elsif This'Address = TIM6_Base then
         RCC_Periph.RCC_APBRSTR1.TIM6RST := True;
         RCC_Periph.RCC_APBRSTR1.TIM6RST := False;
      elsif This'Address = TIM7_Base then
         RCC_Periph.RCC_APBRSTR1.TIM7RST := True;
         RCC_Periph.RCC_APBRSTR1.TIM7RST := False;
      else
         raise Unknown_Device;
      end if;
   end Reset;

   ------------------
   -- Enable_Clock --
   ------------------

--   procedure Enable_Clock (This : aliased in out Analog_To_Digital_Converter)
--   is
--   begin
--      if This'Address = ADC1_Base then
--         RCC_Periph.AHB2ENR.ADCEN := True;
--         RCC_Periph.CCIPR.ADCSEL := 3;  --  clock sel. Default is... none!
--      else
--         raise Unknown_Device;
--      end if;
--   end Enable_Clock;

   -------------------------
   -- Reset_All_ADC_Units --
   -------------------------

--   procedure Reset_All_ADC_Units is
--   begin
--      RCC_Periph.AHB2RSTR.ADCRST := True;
--      RCC_Periph.AHB2RSTR.ADCRST := False;
--   end Reset_All_ADC_Units;


   -----------
   -- Reset --
   -----------

   procedure Reset (This : aliased in out Digital_To_Analog_Converter) is
      pragma Unreferenced (This);
   begin
      RCC_Periph.RCC_APBRSTR1.DAC1RST := True;
      RCC_Periph.RCC_APBRSTR1.DAC1RST := False;
   end Reset;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : aliased in out Digital_To_Analog_Converter)
   is
      pragma Unreferenced (This);
   begin
      RCC_Periph.RCC_APBENR1.DAC1EN := True;
   end Enable_Clock;
   ------------------------------
   -- System_Clock_Frequencies --
   ------------------------------

   function System_Clock_Frequencies return RCC_System_Clocks
   is
      Source       : constant UInt3 := RCC_Periph.RCC_CFGR.SWS;
      Result       : RCC_System_Clocks;
   begin
      Result.I2SCLK := 0;
      return Result;
   end System_Clock_Frequencies;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : in out CRC_32) is
      pragma Unreferenced (This);
   begin
      RCC_Periph.RCC_AHBENR.CRCEN := True;
   end Enable_Clock;

   -------------------
   -- Disable_Clock --
   -------------------

   procedure Disable_Clock (This : in out CRC_32) is
      pragma Unreferenced (This);
   begin
      RCC_Periph.RCC_AHBENR.CRCEN := False;
   end Disable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : in out CRC_32) is
      pragma Unreferenced (This);
   begin
--      RCC_Periph.AHBRSTR.CRCRST := True;
--      RCC_Periph.AHBRSTR.CRCRST := False;
      null;
   end Reset;

end STM32.Device;
