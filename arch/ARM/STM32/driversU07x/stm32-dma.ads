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
--   @file    stm32f4xx_hal_dma.h                                           --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   Header file of DMA HAL module.                                --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

--  This file provides definitions for the DMA controllers on the STM32F4 (ARM
--  Cortex M4F) microcontrollers from ST Microelectronics.

--  See Application Note AN4031: "Using the STM32F2 and STM32F4 DMA controller"
--  and Reference Manual RM0090: "STM32F405xx/07xx, STM32F415xx/17xx,
--  STM32F42xxx and STM32F43xxx advanced ARM-based 32-bit MCUs" In the
--  application note, see especially section four, titled "Tips and
--  warnings while programming the DMA controller"

--  The basic call sequence, given a Controller and a Stream, is as follows:

--  1) Configure

--     Configures the Controller and Stream per application requirements. This
--     is the primary setup call, specifying the static characteristics of all
--     the transfers to be performed on the stream, such as the direction, the
--     channel, and so forth. The Controller is disabled after the call.

--  2) Configure_Data_Flow

--     Sets the dynamic parameters of a given transfer, i.e., the source and
--     destination addresses and the number of data items to transfer.

--  3) Enable

--     Enables transfers on the Controller and Stream. Transfers will begin
--     immediately unless programmed otherwise.

--  You can enable some or all DMA interrupts prior to the call to Enable, if
--  required by your usage.

--  Ensure all the status flags are cleared prior to the call to Enable, since
--  a transfer will then begin. This can be accomplished by relying on the fact
--  that the board has just powered-up, by a call to Reset, or by a call to
--  Clear_All_Status.

--  Note that there are convenience routines that do steps two and three:
--     Start_Transfer
--     Start_Transfer_with_Interrupts

pragma Restrictions (No_Elaboration_Code);

with System;         use System;
with Ada.Real_Time;  use Ada.Real_Time;

private with STM32_SVD.DMA;

package STM32.DMA with SPARK_Mode => Off is

end STM32.DMA;
