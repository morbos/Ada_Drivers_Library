------------------------------------------------------------------------------
--                                                                          --
--                 Copyright (C) 2015-2017, AdaCore                         --
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
--   @file    stm32f4xx_hal_gpio.c                                          --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   GPIO HAL module driver.                                       --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

with System;          use System;
with STM32_SVD.GPIO;  use STM32_SVD.GPIO;
with STM32_SVD.AFIO;  use STM32_SVD.AFIO;

with STM32.RCC;
with STM32.SYSCFG;
with System.Machine_Code;
with Ada.Unchecked_Conversion;

package body STM32.GPIO is

   function To_GPIO_Mode is new Ada.Unchecked_Conversion (UInt4, GPIO_Mode);
   function To_UInt4 is new Ada.Unchecked_Conversion (Cnf_Mode, UInt4);
   function To_UInt4 is new Ada.Unchecked_Conversion (GPIO_Mode, UInt4);
   function To_UInt4 is new Ada.Unchecked_Conversion (GPIO_Cnf_Input, UInt4);
   function To_UInt4 is new Ada.Unchecked_Conversion (GPIO_Cnf_Output, UInt4);

   function To_UInt3 is new Ada.Unchecked_Conversion (AFIO_JTAG, UInt3);

   procedure Lock_The_Pin (This : in out GPIO_Port;  Pin : GPIO_Pin);
   --  This is the routine that actually locks the pin for the port. It is an
   --  internal routine and has no preconditions. We use it to avoid redundant
   --  calls to the precondition that checks that the pin is not already
   --  locked. The redundancy would otherwise occur because the routine that
   --  locks an array of pins is implemented by calling the routine that locks
   --  a single pin: both those Lock routines have a precondition that checks
   --  that the pin(s) is not already being locked.

   subtype GPIO_Pin_Index is Natural range 0 .. GPIO_Pin'Pos (GPIO_Pin'Last);

   -------------
   -- Any_Set --
   -------------

   function Any_Set (Pins : GPIO_Points) return Boolean is
   begin
      for Pin of Pins loop
         if Pin.Set then
            return True;
         end if;
      end loop;

      return False;
   end Any_Set;

   ----------
   -- Mode --
   ----------

   overriding
   function Mode (This : GPIO_Point) return HAL.GPIO.GPIO_Mode is
      Index     : constant GPIO_Pin_Index := GPIO_Pin'Pos (This.Pin);
      Tmp       : GPIO_Mode;
      Cnf_Mask  : constant UInt4 := 2#1100#;
      Mode_Mask : constant UInt4 := 2#0011#;
      CM        : Cnf_Mode;
   begin
      if Index < 8 then
         CM := This.Periph.CRL.Arr (Index);
      else
         CM := This.Periph.CRH.Arr (Index - 8);
      end if;
      Tmp := To_GPIO_Mode (To_UInt4 (CM) and Mode_Mask);
      if Tmp = Mode_Input then
         return HAL.GPIO.Input;
      else
         return HAL.GPIO.Output;
      end if;
   end Mode;

   --------------
   -- Set_Mode --
   --------------

   overriding
   function Set_Mode
     (This : in out GPIO_Point;
      Mode : HAL.GPIO.GPIO_Config_Mode)
      return Boolean
   is
      Index      : constant GPIO_Pin_Index := GPIO_Pin'Pos (This.Pin);
      Mode_Tmp   : GPIO_Mode;
      Input      : GPIO_Cnf_Input;
      Output     : GPIO_Cnf_Output;
      Compose    : UInt4;
   begin
      case Mode is
         when HAL.GPIO.Output =>
            Output := Cnf_Push_Pull;
            Mode_Tmp := Mode_Output_2MHz;
            Compose := To_UInt4 (Output) or To_UInt4 (Mode_Tmp);
         when HAL.GPIO.Input =>
            Input := Cnf_Input;
            Mode_Tmp := Mode_Input;
            Compose := To_UInt4 (Input) or To_UInt4 (Mode_Tmp);
      end case;

      if Index < 8 then
         This.Periph.CRL.Arr (Index) := Compose;
      else
         This.Periph.CRH.Arr (Index - 8) := Compose;
      end if;
      return True;
   end Set_Mode;

   -------------------
   -- Pull_Resistor --
   -------------------

   overriding
   function Pull_Resistor
     (This : GPIO_Point)
      return HAL.GPIO.GPIO_Pull_Resistor
   is
      Index : constant GPIO_Pin_Index := GPIO_Pin'Pos (This.Pin);
   begin
--      if  This.Periph.PUPDR.Arr (Index) = 0 then
--         return HAL.GPIO.Floating;
--      elsif This.Periph.PUPDR.Arr (Index) = 1 then
--         return HAL.GPIO.Pull_Up;
--      else
--         return HAL.GPIO.Pull_Down;
--      end if;
      return HAL.GPIO.Floating;
   end Pull_Resistor;

   -----------------------
   -- Set_Pull_Resistor --
   -----------------------

   overriding
   function Set_Pull_Resistor
     (This : in out GPIO_Point;
      Pull : HAL.GPIO.GPIO_Pull_Resistor)
      return Boolean
   is
      Index : constant GPIO_Pin_Index := GPIO_Pin'Pos (This.Pin);
   begin
--      case Pull is
--         when HAL.GPIO.Floating =>
--            This.Periph.PUPDR.Arr (Index) := 0;
--         when HAL.GPIO.Pull_Up =>
--            This.Periph.PUPDR.Arr (Index) := 1;
--         when HAL.GPIO.Pull_Down =>
--            This.Periph.PUPDR.Arr (Index) := 2;
--      end case;
      return True;
   end Set_Pull_Resistor;

   ---------
   -- Set --
   ---------

   overriding
   function Set (This : GPIO_Point) return Boolean is
      Pin_Mask : constant UInt16 := GPIO_Pin'Enum_Rep (This.Pin);
   begin
      return (This.Periph.IDR.IDR.Val and Pin_Mask) = Pin_Mask;
   end Set;

   -------------
   -- All_Set --
   -------------

   function All_Set (Pins : GPIO_Points) return Boolean is
   begin
      for Pin of Pins loop
         if not Pin.Set then
            return False;
         end if;
      end loop;

      return True;
   end All_Set;

   ---------
   -- Set --
   ---------

   overriding
   procedure Set (This : in out GPIO_Point) is
   begin
      This.Periph.BSRR.BS.Val := GPIO_Pin'Enum_Rep (This.Pin);
      --  The bit-set and bit-reset registers ignore writes of zeros so we
      --  don't need to preserve the existing bit values in those registers.
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set (Pins : in out GPIO_Points) is
   begin
      for Pin of Pins loop
         Pin.Set;
      end loop;
   end Set;

   -----------
   -- Clear --
   -----------

   overriding
   procedure Clear (This : in out GPIO_Point) is
   begin
      This.Periph.BSRR.BR.Val := GPIO_Pin'Enum_Rep (This.Pin);
      --  The bit-set and bit-reset registers ignore writes of zeros so we
      --  don't need to preserve the existing bit values in those registers.
   end Clear;

   -----------
   -- Clear --
   -----------

   procedure Clear (Pins : in out GPIO_Points) is
   begin
      for Pin of Pins loop
         Pin.Clear;
      end loop;
   end Clear;

   ------------
   -- Toggle --
   ------------

   overriding
   procedure Toggle (This : in out GPIO_Point) is
   begin
      This.Periph.ODR.ODR.Val := This.Periph.ODR.ODR.Val xor GPIO_Pin'Enum_Rep (This.Pin);
   end Toggle;

   ------------
   -- Toggle --
   ------------

   procedure Toggle (Points : in out GPIO_Points) is
   begin
      for Point of Points loop
         Point.Toggle;
      end loop;
   end Toggle;

   ------------
   -- Locked --
   ------------

   function Locked (This : GPIO_Point) return Boolean is
      Mask : constant UInt16 := GPIO_Pin'Enum_Rep (This.Pin);
   begin
      --      return (This.Periph.LCKR.LCK.Val and Mask) = Mask;
      return True;
   end Locked;

   ------------------
   -- Lock_The_Pin --
   ------------------

   procedure Lock_The_Pin (This : in out GPIO_Port;  Pin : GPIO_Pin) is
      Temp : UInt32;
      pragma Volatile (Temp);

      use System.Machine_Code;
      use ASCII;
   begin
      --  As per the Reference Manual (RM0090; Doc ID 018909 Rev 6) pg 282,
      --  a specific sequence is required to set the Lock bit. Throughout the
      --  sequence the same value for the lower 15 bits of the word must be
      --  used (ie the pin number). The lock bit is referred to as LCKK in
      --  the doc.

--        Temp := LCCK or Pin'Enum_Rep;
--
--        --  set the lock bit
--        Port.LCKR := Temp;
--
--        --  clear the lock bit
--        Port.LCKR := Pin'Enum_Rep;
--
--        --  set the lock bit again
--        Port.LCKR := Temp;
--
--        --  read the lock bit
--        Temp := Port.LCKR;
--
--        --  read the lock bit again
--        Temp := Port.LCKR;

      --  We use the following assembly language sequence because the above
      --  high-level version in Ada works only if the optimizer is enabled.
      --  This is not an issue specific to Ada. If you need a specific sequence
      --  of instructions you should really specify those instructions.
      --  We don't want the functionality to depend on the switches, and we
      --  don't want to preclude debugging, hence the following:

      Asm ("orr  r3, %2, #65536"  & LF & HT &
           "str  r3, %0"          & LF & HT &
           "ldr  r3, %0"          & LF & HT &  -- temp <- pin or LCCK
           "str  r3, [%1, #28]"   & LF & HT &  -- temp -> lckr
           "str  %2, [%1, #28]"   & LF & HT &  -- pin -> lckr
           "ldr  r3, %0"          & LF & HT &
           "str  r3, [%1, #28]"   & LF & HT &  -- temp -> lckr
           "ldr  r3, [%1, #28]"   & LF & HT &
           "str  r3, %0"          & LF & HT &  -- temp <- lckr
           "ldr  r3, [%1, #28]"   & LF & HT &
           "str  r3, %0"          & LF & HT,   -- temp <- lckr
           Inputs => (Address'Asm_Input ("r", This'Address), -- %1
                     (GPIO_Pin'Asm_Input ("r", Pin))),            -- %2
           Outputs => (UInt32'Asm_Output ("=m", Temp)),  -- %0
           Volatile => True,
           Clobber  => ("r2, r3"));
   end Lock_The_Pin;

   ----------
   -- Lock --
   ----------

   procedure Lock (This : GPIO_Point) is
   begin
      Lock_The_Pin (This.Periph.all, This.Pin);
   end Lock;

   ----------
   -- Lock --
   ----------

   procedure Lock (Points : GPIO_Points) is
   begin
      for Point of Points loop
         if not Locked (Point) then
            Point.Lock;
         end if;
      end loop;
   end Lock;

   ------------------
   -- Configure_IO --
   ------------------

   procedure Configure_IO
     (This   : GPIO_Point;
      Config : GPIO_Port_Configuration)
   is
      Index : constant GPIO_Pin_Index := GPIO_Pin'Pos (This.Pin);
      Mode_Tmp   : GPIO_Mode;
      Input      : GPIO_Cnf_Input;
      Output     : GPIO_Cnf_Output;
      Compose    : UInt4;
   begin
      if Config.Mode /= Mode_In then
         case Config.Output_Type is
            when Push_Pull =>
               Output := Cnf_Push_Pull;
            when Open_Drain =>
               Output := Cnf_Open_Drain;
            when others =>
               null;
         end case;
         case Config.Speed is
            when Speed_2MHz =>
               Mode_Tmp := Mode_Output_2MHz;
            when Speed_10MHz =>
               Mode_Tmp := Mode_Output_10MHz;
            when Speed_50MHz =>
               Mode_Tmp := Mode_Output_50MHz;
            when others =>
               null;
         end case;
      end if;

      case Config.Resistors is
         when Floating =>
            Input := Cnf_Floating;
         when Pull_Up =>
            Input := Cnf_Input;
         when others =>
            Input := Cnf_Input;
      end case;

      case Config.Mode is
         when Mode_Out =>
            Compose := To_UInt4 (Output) or To_UInt4 (Mode_Tmp);
         when Mode_In =>
            Mode_Tmp := Mode_Input;
            Compose := To_UInt4 (Input) or To_UInt4 (Mode_Tmp);
         when Mode_AF =>
            if Config.Output_Type = Open_Drain then
               Output := Cnf_AF_Open_Drain;
            else
               Output := Cnf_AF_Push_Pull;
            end if;
            Compose := To_UInt4 (Output) or To_UInt4 (Mode_Tmp);
         when others =>
            null;
      end case;
      if Index < 8 then
         This.Periph.CRL.Arr (Index) := Compose;
      else
         This.Periph.CRH.Arr (Index - 8) := Compose;
      end if;
   end Configure_IO;

   ------------------
   -- Configure_IO --
   ------------------

   procedure Configure_IO
     (Points : GPIO_Points;
      Config : GPIO_Port_Configuration)
   is
   begin
      for Point of Points loop
         Point.Configure_IO (Config);
      end loop;
   end Configure_IO;

   ----------------------------------
   -- Configure_Alternate_Function --
   ----------------------------------

   procedure Configure_Alternate_Function
     (This : GPIO_Point;
      AF   : GPIO_Alternate_Function)
   is
      Index : constant GPIO_Pin_Index := GPIO_Pin'Pos (This.Pin);
   begin
--      if Index < 8 then
--         This.Periph.AFRL.Arr (Index) := UInt4 (AF);
--      else
--         This.Periph.AFRH.Arr (Index) := UInt4 (AF);
--      end if;
      null;
   end Configure_Alternate_Function;

   ----------------------------------
   -- Configure_Alternate_Function --
   ----------------------------------

   procedure Configure_Alternate_Function
     (Points : GPIO_Points;
      AF     : GPIO_Alternate_Function)
   is
   begin
      for Point of Points loop
         Point.Configure_Alternate_Function (AF);
      end loop;
   end Configure_Alternate_Function;

   procedure Configure_AF_SWD is
   begin
      AFIO_Periph.MAPR.SWJ_CFG := To_UInt3 (JTAG_DIS_SWD_ENA);  --  SWD ena
   end Configure_AF_SWD;

   ---------------------------
   -- Interrupt_Line_Number --
   ---------------------------

   function Interrupt_Line_Number
     (This : GPIO_Point) return EXTI.External_Line_Number
   is
   begin
      return EXTI.External_Line_Number'Val (GPIO_Pin'Pos (This.Pin));
   end Interrupt_Line_Number;

   -----------------------
   -- Configure_Trigger --
   -----------------------

   procedure Configure_Trigger
     (This    : GPIO_Point;
      Trigger : EXTI.External_Triggers)
   is
      use STM32.EXTI;
      Line : constant External_Line_Number := External_Line_Number'Val (GPIO_Pin'Pos (This.Pin));
      use STM32.SYSCFG, STM32.RCC;
   begin
--      SYSCFG_Clock_Enable;

--      Connect_External_Interrupt (This);
---      if Trigger in Interrupt_Triggers then
--         Enable_External_Interrupt (Line, Trigger);
--      else
--         Enable_External_Event (Line, Trigger);
--      end if;
      null;
   end Configure_Trigger;

   -----------------------
   -- Configure_Trigger --
   -----------------------

   procedure Configure_Trigger
     (Points  : GPIO_Points;
      Trigger : EXTI.External_Triggers)
   is
   begin
      for Point of Points loop
         Point.Configure_Trigger (Trigger);
      end loop;
   end Configure_Trigger;

end STM32.GPIO;
