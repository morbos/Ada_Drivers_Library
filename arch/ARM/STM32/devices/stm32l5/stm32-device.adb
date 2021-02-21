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

with HAL;    use HAL;
with System; use System;
pragma Warnings (Off, "* is an internal GNAT unit");
with System.BB.Parameters;
pragma Warnings (On, "* is an internal GNAT unit");

with STM32_SVD.RCC; use STM32_SVD.RCC;

package body STM32.Device is
   ------------------
   -- Enable_Clock --
   ------------------

   Secure_Code : UInt32;
   pragma Import (C, Secure_Code, "secure_code");

   RCC : aliased RCC_Peripheral
     with Import, Address => S_NS_Periph (RCC_Base);

   procedure Enable_Clock (This : aliased in out Digital_To_Analog_Converter)
   is
   begin
      RCC_Periph.APB1ENR1.DAC1EN := True;
   end Enable_Clock;

   procedure Reset (This : aliased in out Digital_To_Analog_Converter)
   is
   begin
      RCC_Periph.APB1RSTR1.DAC1RST := True;
      RCC_Periph.APB1RSTR1.DAC1RST := False;
   end Reset;

   procedure Enable_Clock (This : aliased in out GPIO_Port) is
   begin
      if This'Address = S_NS_Periph (GPIOA_Base) then
         RCC.AHB2ENR.GPIOAEN := True;
      elsif This'Address = S_NS_Periph (GPIOB_Base) then
         RCC.AHB2ENR.GPIOBEN := True;
      elsif This'Address = S_NS_Periph (GPIOC_Base) then
         RCC.AHB2ENR.GPIOCEN := True;
      elsif This'Address = S_NS_Periph (GPIOD_Base) then
         RCC.AHB2ENR.GPIODEN := True;
      elsif This'Address = S_NS_Periph (GPIOE_Base) then
         RCC.AHB2ENR.GPIOEEN := True;
      elsif This'Address = S_NS_Periph (GPIOF_Base) then
         RCC.AHB2ENR.GPIOFEN := True;
      elsif This'Address = S_NS_Periph (GPIOG_Base) then
         RCC.AHB2ENR.GPIOGEN := True;
      elsif This'Address = S_NS_Periph (GPIOH_Base) then
         RCC.AHB2ENR.GPIOHEN := True;
      else
         raise Unknown_Device;
      end if;
   end Enable_Clock;

   procedure Disable_Clock (This : aliased in out GPIO_Port) is
   begin
      if This'Address = S_NS_Periph (GPIOA_Base) then
         RCC.AHB2ENR.GPIOAEN := False;
      elsif This'Address = S_NS_Periph (GPIOB_Base) then
         RCC.AHB2ENR.GPIOBEN := False;
      elsif This'Address = S_NS_Periph (GPIOC_Base) then
         RCC.AHB2ENR.GPIOCEN := False;
      elsif This'Address = S_NS_Periph (GPIOD_Base) then
         RCC.AHB2ENR.GPIODEN := False;
      elsif This'Address = S_NS_Periph (GPIOE_Base) then
         RCC.AHB2ENR.GPIOEEN := False;
      elsif This'Address = S_NS_Periph (GPIOF_Base) then
         RCC.AHB2ENR.GPIOFEN := False;
      elsif This'Address = S_NS_Periph (GPIOG_Base) then
         RCC.AHB2ENR.GPIOGEN := False;
      elsif This'Address = S_NS_Periph (GPIOH_Base) then
         RCC.AHB2ENR.GPIOHEN := False;
      else
         raise Unknown_Device;
      end if;
   end Disable_Clock;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (Point : GPIO_Point)
   is
   begin
      Enable_Clock (Point.Periph.all);
   end Enable_Clock;

   procedure Disable_Clock (Point : GPIO_Point)
   is
   begin
      Disable_Clock (Point.Periph.all);
   end Disable_Clock;


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
      if This'Address = S_NS_Periph (GPIOA_Base) then
         RCC.AHB2RSTR.GPIOARST := True;
         RCC.AHB2RSTR.GPIOARST := False;
      elsif This'Address = S_NS_Periph (GPIOB_Base) then
         RCC.AHB2RSTR.GPIOBRST := True;
         RCC.AHB2RSTR.GPIOBRST := False;
      elsif This'Address = S_NS_Periph (GPIOC_Base) then
         RCC.AHB2RSTR.GPIOCRST := True;
         RCC.AHB2RSTR.GPIOCRST := False;
      elsif This'Address = S_NS_Periph (GPIOD_Base) then
         RCC.AHB2RSTR.GPIODRST := True;
         RCC.AHB2RSTR.GPIODRST := False;
      elsif This'Address = S_NS_Periph (GPIOE_Base) then
         RCC.AHB2RSTR.GPIOERST := True;
         RCC.AHB2RSTR.GPIOERST := False;
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
      if Port'Address = S_NS_Periph (GPIOA_Base) then
         return 0;
      elsif Port'Address = S_NS_Periph (GPIOB_Base) then
         return 1;
      elsif Port'Address = S_NS_Periph (GPIOC_Base) then
         return 2;
      elsif Port'Address = S_NS_Periph (GPIOD_Base) then
         return 3;
      elsif Port'Address = S_NS_Periph (GPIOE_Base) then
         return 4;
      else
         raise Program_Error;
      end if;
   end GPIO_Port_Representation;

   ----------------
   -- As_Port_Id --
   ----------------

   function As_Port_Id (Port : I2C_Port) return I2C_Port_Id is
   begin
      if Port.Periph.all'Address = S_NS_Periph (I2C1_Base) then
         return I2C_Id_1;
      elsif Port.Periph.all'Address = S_NS_Periph (I2C2_Base) then
         return I2C_Id_2;
      elsif Port.Periph.all'Address = S_NS_Periph (I2C3_Base) then
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
            RCC_Periph.APB1ENR1.I2C1EN := True;
         when I2C_Id_2 =>
            RCC_Periph.APB1ENR1.I2C2EN := True;
         when I2C_Id_3 =>
            RCC_Periph.APB1ENR1.I2C3EN := True;
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
            RCC_Periph.APB1RSTR1.I2C1RST := True;
            RCC_Periph.APB1RSTR1.I2C1RST := False;
         when I2C_Id_2 =>
            RCC_Periph.APB1RSTR1.I2C2RST := True;
            RCC_Periph.APB1RSTR1.I2C2RST := False;
         when I2C_Id_3 =>
            RCC_Periph.APB1RSTR1.I2C3RST := True;
            RCC_Periph.APB1RSTR1.I2C3RST := False;
      end case;
   end Reset;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : SPI_Port) is
   begin
      if This.Periph.all'Address = S_NS_Periph (SPI1_Base) then
         RCC.APB2ENR.SPI1EN := True;
      elsif This.Periph.all'Address = S_NS_Periph (SPI2_Base) then
         RCC.APB1ENR1.SPI2EN := True;
      elsif This.Periph.all'Address = S_NS_Periph (SPI3_Base) then
         RCC.APB1ENR1.SPI3EN := True;
      else
         raise Unknown_Device;
      end if;
   end Enable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : in out SPI_Port) is
   begin
      if This.Periph.all'Address = S_NS_Periph (SPI1_Base) then
         RCC.APB2RSTR.SPI1RST := True;
         RCC.APB2RSTR.SPI1RST := False;
      elsif This.Periph.all'Address = S_NS_Periph (SPI2_Base) then
         RCC.APB1RSTR1.SPI2RST := True;
         RCC.APB1RSTR1.SPI2RST := False;
      elsif This.Periph.all'Address = S_NS_Periph (SPI3_Base) then
         RCC.APB1RSTR1.SPI3RST := True;
         RCC.APB1RSTR1.SPI3RST := False;
      else
         raise Unknown_Device;
      end if;
   end Reset;

   function S_NS_Periph (Addr : System.Address) return System.Address
   is
      X : UInt32;
      LAddr : System.Address;
      for X'Address use LAddr'Address;
   begin
      LAddr := Addr;
      if Secure_Code > 0 then
         X := X + 16#1000_0000#;
      end if;
      return LAddr;
   end S_NS_Periph;

end STM32.Device;
