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
--   @file    stm32f407xx.h   et al.                                        --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   CMSIS STM32F407xx Device Peripheral Access Layer Header File. --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

--  This file provides register definitions for the STM32F4 (ARM Cortex M4F)
--  microcontrollers from ST Microelectronics.

with STM32_SVD.EXTI; use STM32_SVD.EXTI;

package body STM32.EXTI is

   -------------------------------
   -- Enable_External_Interrupt --
   -------------------------------

   procedure Enable_External_Interrupt
     (Line    : External_Line_Number;
      Trigger : Interrupt_Triggers)
   is
      Index : constant Natural := External_Line_Number'Pos (Line);
   begin
      EXTI_Periph.IMR.MR := EXTI_Periph.IMR.MR or (2 ** Index);
      EXTI_Periph.RTSR.TR := EXTI_Periph.RTSR.TR or
        (if Trigger in Interrupt_Rising_Edge  | Interrupt_Rising_Falling_Edge then
           2 ** Index else 0);
      EXTI_Periph.FTSR.TR := EXTI_Periph.FTSR.TR or
        (if Trigger in Interrupt_Falling_Edge | Interrupt_Rising_Falling_Edge then
           2 ** Index else 0);
   end Enable_External_Interrupt;

   --------------------------------
   -- Disable_External_Interrupt --
   --------------------------------

   procedure Disable_External_Interrupt (Line : External_Line_Number) is
      Index : constant Natural := External_Line_Number'Pos (Line);
   begin
      EXTI_Periph.IMR.MR := EXTI_Periph.IMR.MR and not (2 ** Index);
      EXTI_Periph.RTSR.TR := EXTI_Periph.RTSR.TR and not (2 ** Index);
      EXTI_Periph.FTSR.TR := EXTI_Periph.FTSR.TR and not (2 ** Index);
   end Disable_External_Interrupt;

   ---------------------------
   -- Enable_External_Event --
   ---------------------------

   procedure Enable_External_Event
     (Line    : External_Line_Number;
      Trigger : Event_Triggers)
   is
      Index : constant Natural := External_Line_Number'Pos (Line);
   begin
      EXTI_Periph.EMR.MR  := EXTI_Periph.EMR.MR or (2 ** Index);
      EXTI_Periph.RTSR.TR := EXTI_Periph.RTSR.TR or
        (if Trigger in Interrupt_Rising_Edge  | Interrupt_Rising_Falling_Edge then
           2 ** Index else 0);
      EXTI_Periph.FTSR.TR := EXTI_Periph.FTSR.TR or
        (if Trigger in Interrupt_Falling_Edge | Interrupt_Rising_Falling_Edge then
           2 ** Index else 0);
   end Enable_External_Event;

   ----------------------------
   -- Disable_External_Event --
   ----------------------------

   procedure Disable_External_Event (Line : External_Line_Number) is
      Index : constant Natural := External_Line_Number'Pos (Line);
   begin
      EXTI_Periph.EMR.MR := EXTI_Periph.EMR.MR and not (2 ** Index);
      EXTI_Periph.RTSR.TR := EXTI_Periph.RTSR.TR and not (2 ** Index);
      EXTI_Periph.FTSR.TR := EXTI_Periph.FTSR.TR and not (2 ** Index);
   end Disable_External_Event;

   ------------------
   -- Generate_SWI --
   ------------------

   procedure Generate_SWI (Line : External_Line_Number) is
      Index : constant Natural := External_Line_Number'Pos (Line);
   begin
      EXTI_Periph.SWIER.SWIER := EXTI_Periph.SWIER.SWIER or (2 ** Index);
   end Generate_SWI;

   --------------------------------
   -- External_Interrupt_Pending --
   --------------------------------

   function External_Interrupt_Pending (Line : External_Line_Number) return Boolean is
      Index : constant Natural := External_Line_Number'Pos (Line);
   begin
      return (EXTI_Periph.PR.PR and (2 ** Index)) = (2 ** Index);
   end External_Interrupt_Pending;

   ------------------------------
   -- Clear_External_Interrupt --
   ------------------------------

   procedure Clear_External_Interrupt (Line : External_Line_Number) is
      Index : constant Natural := External_Line_Number'Pos (Line);
   begin
      --  yes, one to clear
      EXTI_Periph.PR.PR := EXTI_Periph.PR.PR and (2 ** Index);
   end Clear_External_Interrupt;

end STM32.EXTI;
