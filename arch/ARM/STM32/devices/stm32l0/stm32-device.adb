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
--   @file    stm32f40[5|7]xx.h                                             --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   CMSIS STM32F407xx Device Peripheral Access Layer Header File. --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

with System; use System;
with STM32_SVD.RCC; use STM32_SVD.RCC;

package body STM32.Device is

   procedure Enable_Clock (This : aliased in out GPIO_Port) is
   begin
      if This'Address = GPIOA_Base then
         RCC_Periph.IOPENR.IOPAEN := True;
      elsif This'Address = GPIOB_Base then
         RCC_Periph.IOPENR.IOPBEN := True;
      elsif This'Address = GPIOC_Base then
         RCC_Periph.IOPENR.IOPCEN := True;
      elsif This'Address = GPIOD_Base then
         RCC_Periph.IOPENR.IOPDEN := True;
      elsif This'Address = GPIOE_Base then
         RCC_Periph.IOPENR.IOPEEN := True;
      elsif This'Address = GPIOH_Base then
         RCC_Periph.IOPENR.IOPHEN := True;
      else
         raise Unknown_Device;
      end if;
   end Enable_Clock;

   procedure Enable_Clock (Point : GPIO_Point) is
   begin
      Enable_Clock (Point.Periph.all);
   end Enable_Clock;

   procedure Enable_Clock (Points : GPIO_Points) is
   begin
      for Point of Points loop
         Enable_Clock (Point.Periph.all);
      end loop;
   end Enable_Clock;

   procedure Disable_Clock (This : aliased in out GPIO_Port) is
   begin
      if This'Address = GPIOA_Base then
         RCC_Periph.IOPENR.IOPAEN := False;
      elsif This'Address = GPIOB_Base then
         RCC_Periph.IOPENR.IOPBEN := False;
      elsif This'Address = GPIOC_Base then
         RCC_Periph.IOPENR.IOPCEN := False;
      elsif This'Address = GPIOD_Base then
         RCC_Periph.IOPENR.IOPDEN := False;
      elsif This'Address = GPIOE_Base then
         RCC_Periph.IOPENR.IOPEEN := False;
      elsif This'Address = GPIOH_Base then
         RCC_Periph.IOPENR.IOPHEN := False;
      else
         raise Unknown_Device;
      end if;
   end Disable_Clock;

   procedure Disable_Clock (Point : GPIO_Point) is
   begin
      Disable_Clock (Point.Periph.all);
   end Disable_Clock;

   procedure Disable_Clock (Points : GPIO_Points) is
   begin
      for Point of Points loop
         Disable_Clock (Point.Periph.all);
      end loop;
   end Disable_Clock;

   procedure Reset (This : aliased in out GPIO_Port) is
   begin
      if This'Address = GPIOA_Base then
         RCC_Periph.IOPRSTR.IOPARST := True;
         RCC_Periph.IOPRSTR.IOPARST := False;
      elsif This'Address = GPIOB_Base then
         RCC_Periph.IOPRSTR.IOPBRST := True;
         RCC_Periph.IOPRSTR.IOPBRST := False;
      elsif This'Address = GPIOC_Base then
         RCC_Periph.IOPRSTR.IOPCRST := True;
         RCC_Periph.IOPRSTR.IOPCRST := False;
      elsif This'Address = GPIOD_Base then
         RCC_Periph.IOPRSTR.IOPDRST := True;
         RCC_Periph.IOPRSTR.IOPDRST := False;
      elsif This'Address = GPIOE_Base then
         RCC_Periph.IOPRSTR.IOPERST := True;
         RCC_Periph.IOPRSTR.IOPERST := False;
      elsif This'Address = GPIOH_Base then
         RCC_Periph.IOPRSTR.IOPHRST := True;
         RCC_Periph.IOPRSTR.IOPHRST := False;
      else
         raise Unknown_Device;
      end if;
   end Reset;

   procedure Reset (Point : GPIO_Point) is
   begin
      Reset (Point.Periph.all);
   end Reset;

   procedure Reset (Points : GPIO_Points) is
   begin
      null;
   end Reset;

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
      elsif Port'Address = GPIOH_Base then
         return 5;
      else
         raise Program_Error;
      end if;
   end GPIO_Port_Representation;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : SPI_Port) is
   begin
      if This.Periph.all'Address = SPI1_Base then
         RCC_Periph.APB2ENR.SPI1EN := True;
      elsif This.Periph.all'Address = SPI2_Base then
         RCC_Periph.APB1ENR.SPI2EN := True;
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
         RCC_Periph.APB2RSTR.SPI1RST := True;
         RCC_Periph.APB2RSTR.SPI1RST := False;
      elsif This.Periph.all'Address = SPI2_Base then
         RCC_Periph.APB1RSTR.SPI2RST := True;
         RCC_Periph.APB1RSTR.SPI2RST := False;
      else
         raise Unknown_Device;
      end if;
   end Reset;

end STM32.Device;
