------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2015, AdaCore                           --
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
--   @file    stm32f4xx_hal_spi.c                                           --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   SPI HAL module driver.                                        --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------
with STM32.Device;    use STM32.Device;

package body STM32.SPI.DMA is

   procedure Configure_DMA
     (Controller : access DMA_Controller;
      Channel    : DMA_Channel_Selector;
      Data_Width : DMA_Data_Transfer_Widths);

   procedure Configure_DMA
     (Controller : access DMA_Controller;
      Channel    : DMA_Channel_Selector;
      Data_Width : DMA_Data_Transfer_Widths)
   is
      Config : DMA_Channel_Configuration;
   begin
      --  See app note AN4187 Table 3 for this configuration (other than the
      --  channel number). It works, although it looks counterintuitive.

      Config.Channel                      := Channel_1;  -- arbitrary
      Config.Direction                    := Memory_To_Peripheral;
      Config.Memory_Data_Format           := Data_Width;
      Config.Peripheral_Data_Format       := Bytes;
      Config.Increment_Peripheral_Address := False;
      Config.Increment_Memory_Address     := True;
      Config.Operation_Mode               := Normal_Mode;
      Config.Priority                     := Priority_Very_High;
      Config.FIFO_Enabled                 := False;
      Config.Memory_Burst_Size            := Memory_Burst_Single;
      Config.Peripheral_Burst_Size        := Peripheral_Burst_Single;
      Config.CS                           := 1;
      Configure (Controller.all, Channel, Config);
   end Configure_DMA;

   procedure Transmit_DMA (This       : in out SPI_Port;
                           Controller : access DMA_Controller;
                           Channel    : DMA_Channel_Selector;
                           Outgoing   : HAL.SPI.SPI_Data_8b)
   is
      Data_Width  : DMA_Data_Transfer_Widths := Bytes;
   begin
      Configure_DMA (Controller, Channel, Data_Width);
      --  We configure the unit each time to ensure the data width is right.

      Clear_All_Status (Controller.all, Channel);
      --  Ensure previous calls or other use hasn't set any status flags.

      Start_Transfer_with_Interrupts
        (Controller.all,
         Channel,
         Source             => Outgoing'Address,
         Destination        => This.Periph.DR'Address,
         Data_Count         => Outgoing'Length,
         Enabled_Interrupts => (Transfer_Complete_Interrupt => True,
                                others                      => False));

   end Transmit_DMA;

end STM32.SPI.DMA;
