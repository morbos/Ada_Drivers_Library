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

package body STM32.DMA with SPARK_Mode => Off is

   procedure Enable
     (This    : DMA_Controller;
      Channel : DMA_Channel_Selector)
   is
   begin
      null;
   end Enable;

   --  Before enabling a stream to start a new transfer, the event status flags
   --  corresponding to the stream must be cleared. Note that the unit may not
   --  be enabled by the time the call returns.

   procedure Disable
     (This    : in out DMA_Controller;
      Channel : DMA_Channel_Selector) is
   begin
      case Channel is
         when Channel_1 =>
            This.CCR1.EN := False;
         when Channel_2 =>
            This.CCR2.EN := False;
         when Channel_3 =>
            This.CCR3.EN := False;
         when Channel_4 =>
            This.CCR4.EN := False;
         when Channel_5 =>
            This.CCR5.EN := False;
         when Channel_6 =>
            This.CCR6.EN := False;
         when Channel_7 =>
            This.CCR7.EN := False;
      end case;
   end Disable;

   function Enabled
     (This    : DMA_Controller;
      Channel : DMA_Channel_Selector)
     return Boolean is
   begin
      return False;
   end Enabled;

   procedure Reset
     (This    : in out DMA_Controller;
      Channel : DMA_Channel_Selector) is
   begin
      null;
   end Reset;

   procedure Configure_Data_Flow
     (This        : DMA_Controller;
      Channel     : DMA_Channel_Selector;
      Source      : Address;
      Destination : Address;
      Data_Count  : UInt16) is
   begin
      null;
   end Configure_Data_Flow;

   procedure Start_Transfer
     (This        : DMA_Controller;
      Channel     : DMA_Channel_Selector;
      Source      : Address;
      Destination : Address;
      Data_Count  : UInt16) is
   begin
      null;
   end Start_Transfer;

   procedure Start_Transfer_with_Interrupts
     (This               : DMA_Controller;
      Channel            : DMA_Channel_Selector;
      Source             : Address;
      Destination        : Address;
      Data_Count         : UInt16;
      Enabled_Interrupts : Interrupt_Selections := (others => True)) is
   begin
      null;
   end Start_Transfer_with_Interrupts;

   procedure Abort_Transfer
     (This    : DMA_Controller;
      Channel : DMA_Channel_Selector;
      Result  : out DMA_Error_Code) is
   begin
   --  Disables the specified stream and then waits until the request is
   --  effective. If a stream is disabled while a data transfer is ongoing, the
   --  current datum will be transferred and the stream will be disabled only
   --  after the transfer of this single datum completes.
      null;
   end Abort_Transfer;

   procedure Poll_For_Completion
     (This           : in out DMA_Controller;
      Channel        : DMA_Channel_Selector;
      Expected_Level : DMA_Transfer_Level;
      Timeout        : Time_Span;
      Result         : out DMA_Error_Code) is
   begin
      null;
   end Poll_For_Completion;

   procedure Set_NDT
     (This       : DMA_Controller;
      Channel    : DMA_Channel_Selector;
      Data_Count : UInt16) is
   begin
      null;
   end Set_NDT;

   --  Sets the number of data items to be transferred on the stream.
   --  The Data_Count parameter specifies the number of data items to be
   --  transferred (from 0 to 65535) on the next transfer. The value is
   --  as described for procedure Configure_Data_Flow.

   function Items_Transferred
     (This    : DMA_Controller;
      Channel : DMA_Channel_Selector)
     return UInt16 is
      --  returns the number of items transferred
   begin
      return 0;
   end Items_Transferred;

   function Current_NDT
     (This    : DMA_Controller;
      Channel : DMA_Channel_Selector)
      return UInt16 is
   begin
      return 0;
   end Current_NDT;

   function Circular_Mode
     (This    : DMA_Controller;
      Channel : DMA_Channel_Selector)
     return Boolean is
   begin
      return False;
   end Circular_Mode;

   procedure Enable_Interrupt
     (This    : DMA_Controller;
      Channel : DMA_Channel_Selector;
      Source  : DMA_Interrupt) is
   --  The postcondition should not be relied upon completely because it is
   --  possible, under just the wrong conditions, for the interrupt to be
   --  disabled immediately, prior to return from this routine
   begin
      null;
   end Enable_Interrupt;

   procedure Disable_Interrupt
     (This    : DMA_Controller;
      Channel : DMA_Channel_Selector;
      Source  : DMA_Interrupt) is
   begin
      null;
   end Disable_Interrupt;

   function Interrupt_Enabled
     (This    : DMA_Controller;
      Channel : DMA_Channel_Selector;
      Source  : DMA_Interrupt)
     return Boolean is
   begin
      return False;
   end Interrupt_Enabled;

   procedure Clear_Status
     (This    : in out DMA_Controller;
      Channel : DMA_Channel_Selector;
      Flag    : DMA_Status_Flag) is
   begin
      null;
   end Clear_Status;

   procedure Clear_All_Status
     (This    : in out DMA_Controller;
      Channel : DMA_Channel_Selector) is
   begin
      null;
   end Clear_All_Status;

   function Status
     (This    : DMA_Controller;
      Channel : DMA_Channel_Selector;
      Flag    : DMA_Status_Flag)
     return Boolean is
   begin
      return False;
   end Status;

   function Selected_Channel
     (This    : DMA_Controller;
      Channel : DMA_Channel_Selector)
     return DMA_Channel_Selector is
   begin
      return Channel;  --   !!!
   end Selected_Channel;

   function Transfer_Direction
     (This    : DMA_Controller;
      Channel : DMA_Channel_Selector)
      return DMA_Data_Transfer_Direction is
   begin
      return Memory_To_Memory;   --   !!!
   end Transfer_Direction;

   function Peripheral_Data_Width
     (This    : DMA_Controller;
      Channel : DMA_Channel_Selector)
      return DMA_Data_Transfer_Widths is
   begin
      return Words;  --   !!!
   end Peripheral_Data_Width;

   function Memory_Data_Width
     (This    : DMA_Controller;
      Channel : DMA_Channel_Selector)
      return DMA_Data_Transfer_Widths is
   begin
      return Words;  --   !!!
   end Memory_Data_Width;

   function Operating_Mode
     (This    : DMA_Controller;
      Channel : DMA_Channel_Selector)
     return DMA_Mode is
   begin
      return Normal_Mode;  --  !!!
   end Operating_Mode;

   function Priority
     (This    : DMA_Controller;
      Channel : DMA_Channel_Selector)
     return DMA_Priority_Level is
   begin
      return Priority_Low;  --  !!!
   end Priority;

   function Current_Memory_Buffer
     (This    : DMA_Controller;
      Channel : DMA_Channel_Selector)
     return Memory_Buffer_Target is
   begin
      return Memory_Buffer_0;   --  !!!
   end Current_Memory_Buffer;

   procedure Select_Current_Memory_Buffer
     (This    : DMA_Controller;
      Channel : DMA_Channel_Selector;
      Buffer  : Memory_Buffer_Target) is
   begin
      null;
   end Select_Current_Memory_Buffer;

   procedure Set_Memory_Buffer
     (This    : DMA_Controller;
      Channel : DMA_Channel_Selector;
      Buffer  : Memory_Buffer_Target;
      To      : System.Address) is
   begin
      null;
   end Set_Memory_Buffer;

   procedure Configure_Double_Buffered_Mode
     (This              : DMA_Controller;
      Channel           : DMA_Channel_Selector;
      Buffer_0_Value    : Address;
      Buffer_1_Value    : Address;
      First_Buffer_Used : Memory_Buffer_Target) is
   begin
      null;
   end Configure_Double_Buffered_Mode;

   procedure Enable_Double_Buffered_Mode
     (This    : DMA_Controller;
      Channel : DMA_Channel_Selector) is
   begin
      null;
   end Enable_Double_Buffered_Mode;

   procedure Disable_Double_Buffered_Mode
     (This    : DMA_Controller;
      Channel : DMA_Channel_Selector) is
   begin
      null;
   end Disable_Double_Buffered_Mode;

   function Double_Buffered
     (This    : DMA_Controller;
      Channel : DMA_Channel_Selector)
     return Boolean is
   begin
      return False;
   end Double_Buffered;

   procedure Configure
     (This    : in out DMA_Controller;
      Channel : DMA_Channel_Selector;
      Config  : DMA_Channel_Configuration)
   is
      Req : UInt4;
   begin
      Disable (This, Channel);
      --  Setup the request
      --  Config.Request
      Req := 0;
      case Channel is
         when Channel_1 =>
            This.CSELR.C1S := Req;
         when Channel_2 =>
            This.CSELR.C2S := Req;
         when Channel_3 =>
            This.CSELR.C3S := Req;
         when Channel_4 =>
            This.CSELR.C4S := Req;
         when Channel_5 =>
            This.CSELR.C5S := Req;
         when Channel_6 =>
            This.CSELR.C6S := Req;
         when Channel_7 =>
            This.CSELR.C7S := Req;
      end case;

   end Configure;

   function Aligned
     (This : Address;
      Width : DMA_Data_Transfer_Widths) return Boolean is
   begin
      return False;
   end Aligned;

end STM32.DMA;
