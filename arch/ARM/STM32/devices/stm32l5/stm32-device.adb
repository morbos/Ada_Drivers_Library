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
