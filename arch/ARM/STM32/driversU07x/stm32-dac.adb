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
--   @file    stm32f4xx_hal_dac.c and stm32f4xx_hal_dac_ex.c                --
--   @author  MCD Application Team                                          --
--   @version V1.3.1                                                        --
--   @date    25-March-2015                                                 --
--   @brief   Header file of DAC HAL module.                                --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;

with STM32_SVD.DAC; use STM32_SVD.DAC;

package body STM32.DAC is

   ------------
   -- Enable --
   ------------

   procedure Enable
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
   is
   begin
      case Channel is
         when Channel_1 =>
            This.DAC_MCR.MODE1 := 0; -- conn 2 pin, buff enabled
            This.DAC_CR.EN1 := True;
      end case;
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
   is
   begin
      case Channel is
         when Channel_1 =>
            This.DAC_CR.EN1 := False;
      end case;
   end Disable;

   -------------
   -- Enabled --
   -------------

   function Enabled
     (This    : Digital_To_Analog_Converter;
      Channel : DAC_Channel)
      return Boolean is
   begin
      case Channel is
         when Channel_1 =>
            return This.DAC_CR.EN1;
      end case;
   end Enabled;

   ----------------
   -- Set_Output --
   ----------------

   procedure Set_Output
     (This       : in out Digital_To_Analog_Converter;
      Channel    : DAC_Channel;
      Value      : UInt32;
      Resolution : DAC_Resolution;
      Alignment  : Data_Alignment)
   is
   begin
      case Channel is

         when Channel_1 =>
            case Resolution is
               when DAC_Resolution_12_Bits =>
                  case Alignment is
                     when Left_Aligned =>
                        This.DAC_DHR12L1.DACC1DHR :=
                          UInt12 (Value and Max_12bit_Resolution);
                     when Right_Aligned =>
                        This.DAC_DHR12R1.DACC1DHR :=
                          UInt12 (Value and Max_12bit_Resolution);
                  end case;
               when DAC_Resolution_8_Bits =>
                  This.DAC_DHR8R1.DACC1DHR := UInt8 (Value and Max_8bit_Resolution);
            end case;

      end case;
   end Set_Output;

   ------------------------------------
   -- Trigger_Conversion_By_Software --
   ------------------------------------

   procedure Trigger_Conversion_By_Software
     (This    : in out Digital_To_Analog_Converter)
   is
   begin
      This.DAC_SWTRGR.SWTRIG1 := True; -- cleared by hardware
   end Trigger_Conversion_By_Software;

   ----------------------------
   -- Converted_Output_Value --
   ----------------------------

   function Converted_Output_Value
     (This    : Digital_To_Analog_Converter;
      Channel : DAC_Channel)
      return UInt32
   is
   begin
      case Channel is
         when Channel_1 =>
            return UInt32 (This.DAC_DOR1.DACC1DOR);
      end case;
   end Converted_Output_Value;

   ------------------------------
   -- Set_Dual_Output_Voltages --
   ------------------------------

   procedure Set_Dual_Output_Voltages
     (This            : in out Digital_To_Analog_Converter;
      Channel_1_Value : UInt32;
      Resolution      : DAC_Resolution;
      Alignment       : Data_Alignment)
   is
   begin
      case Resolution is
         when DAC_Resolution_12_Bits =>
            case Alignment is
               when Left_Aligned =>
                  This.DAC_DHR12L1.DACC1DHR :=
                    UInt12 (Channel_1_Value and Max_12bit_Resolution);
               when Right_Aligned =>
                  This.DAC_DHR12R1.DACC1DHR :=
                    UInt12 (Channel_1_Value and Max_12bit_Resolution);
            end case;
         when DAC_Resolution_8_Bits =>
            This.DAC_DHR8R1.DACC1DHR :=
              UInt8 (Channel_1_Value and Max_8bit_Resolution);
      end case;
   end Set_Dual_Output_Voltages;

   --------------------
   -- Select_Trigger --
   --------------------

   procedure Select_Trigger
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel;
      Trigger : External_Event_Trigger_Selection)
   is
   begin
      case Channel is
         when Channel_1 =>
            This.DAC_CR.TSEL1 :=
              External_Event_Trigger_Selection'Enum_Rep (Trigger);
      end case;
   end Select_Trigger;

   -----------------------
   -- Trigger_Selection --
   -----------------------

   function Trigger_Selection
     (This    : Digital_To_Analog_Converter;
      Channel : DAC_Channel)
      return External_Event_Trigger_Selection
   is
   begin
      case Channel is
         when Channel_1 =>
            return External_Event_Trigger_Selection'Val (This.DAC_CR.TSEL1);
      end case;
   end Trigger_Selection;

   --------------------
   -- Enable_Trigger --
   --------------------

   procedure Enable_Trigger
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
   is
   begin
      case Channel is
         when Channel_1 =>
            This.DAC_CR.TEN1 := True;
      end case;
   end Enable_Trigger;

   ---------------------
   -- Disable_Trigger --
   ---------------------

   procedure Disable_Trigger
     (This : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
   is
   begin
      case Channel is
         when Channel_1 =>
            This.DAC_CR.TEN1 := False;
      end case;
   end Disable_Trigger;

   ---------------------
   -- Trigger_Enabled --
   ---------------------

   function Trigger_Enabled
     (This    : Digital_To_Analog_Converter;
      Channel : DAC_Channel)
      return Boolean
   is
   begin
      case Channel is
         when Channel_1 =>
            return This.DAC_CR.TEN1;
      end case;
   end Trigger_Enabled;

   ----------------
   -- Enable_DMA --
   ----------------

   procedure Enable_DMA
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
   is
   begin
      case Channel is
         when Channel_1 =>
            This.DAC_CR.DMAEN1 := True;
      end case;
   end Enable_DMA;

   -----------------
   -- Disable_DMA --
   -----------------

   procedure Disable_DMA
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
   is
   begin
      case Channel is
         when Channel_1 =>
            This.DAC_CR.DMAEN1 := False;
      end case;
   end Disable_DMA;

   -----------------
   -- DMA_Enabled --
   -----------------

   function DMA_Enabled
     (This    : Digital_To_Analog_Converter;
      Channel : DAC_Channel)
      return Boolean
   is
   begin
      case Channel is
         when Channel_1 =>
            return This.DAC_CR.DMAEN1;
      end case;
   end DMA_Enabled;

   ------------
   -- Status --
   ------------

   function Status
     (This : Digital_To_Analog_Converter;
      Flag : DAC_Status_Flag)
      return Boolean
   is
   begin
      case Flag is
         when DMA_Underrun_Channel_1 =>
            return This.DAC_SR.DMAUDR1;
      end case;
   end Status;

   ------------------
   -- Clear_Status --
   ------------------

   procedure Clear_Status
     (This : in out Digital_To_Analog_Converter;
      Flag : DAC_Status_Flag)
   is
   begin
      case Flag is
         when DMA_Underrun_Channel_1 =>
            This.DAC_SR.DMAUDR1 := True; -- set to 1 to clear
      end case;
   end Clear_Status;

   -----------------------
   -- Enable_Interrupts --
   -----------------------

   procedure Enable_Interrupts
     (This : in out Digital_To_Analog_Converter;
      Source : DAC_Interrupts)
   is
   begin
      case Source is
         when DMA_Underrun_Channel_1 =>
            This.DAC_CR.DMAUDRIE1 := True;
      end case;
   end Enable_Interrupts;

   ------------------------
   -- Disable_Interrupts --
   ------------------------

   procedure Disable_Interrupts
     (This   : in out Digital_To_Analog_Converter;
      Source : DAC_Interrupts)
   is
   begin
      case Source is
         when DMA_Underrun_Channel_1 =>
            This.DAC_CR.DMAUDRIE1 := False;
      end case;
   end Disable_Interrupts;

   -----------------------
   -- Interrupt_Enabled --
   -----------------------

   function Interrupt_Enabled
     (This   : Digital_To_Analog_Converter;
      Source : DAC_Interrupts)
      return Boolean
   is
   begin
      case Source is
         when DMA_Underrun_Channel_1 =>
            return This.DAC_CR.DMAUDRIE1;
      end case;
   end Interrupt_Enabled;

   ----------------------
   -- Interrupt_Source --
   ----------------------

   function Interrupt_Source
     (This : Digital_To_Analog_Converter)
      return DAC_Interrupts
   is
   begin
      if This.DAC_CR.DMAUDRIE1 then
         return DMA_Underrun_Channel_1;
      end if;
   end Interrupt_Source;

   -----------------------------
   -- Clear_Interrupt_Pending --
   -----------------------------

   procedure Clear_Interrupt_Pending
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
   is
   begin
      case Channel is
         when Channel_1 =>
            This.DAC_SR.DMAUDR1 := False;
      end case;
   end Clear_Interrupt_Pending;

   ----------------------------
   -- Select_Wave_Generation --
   ----------------------------

   procedure Select_Wave_Generation
     (This      : in out Digital_To_Analog_Converter;
      Channel   : DAC_Channel;
      Selection : Wave_Generation)
   is

      function As_UInt4 is new Ada.Unchecked_Conversion
        (Source => Noise_Wave_Mask_Selection, Target => UInt4);

      function As_UInt4 is new Ada.Unchecked_Conversion
        (Source => Triangle_Wave_Amplitude_Selection, Target => UInt4);

   begin
      case Channel is
         when Channel_1 =>
            This.DAC_CR.WAVE1 :=
              Wave_Generation_Selection'Enum_Rep (Selection.Kind);
      end case;

      case Selection.Kind is

         when No_Wave_Generation =>
            null;

         when Noise_Wave =>
            case Channel is
               when Channel_1 =>
                  This.DAC_CR.MAMP1 := As_UInt4 (Selection.Mask);
            end case;

         when Triangle_Wave =>
            case Channel is
               when Channel_1 =>
                  This.DAC_CR.MAMP1 := As_UInt4 (Selection.Amplitude);
            end case;

      end case;
   end Select_Wave_Generation;

   ------------------------------
   -- Selected_Wave_Generation --
   ------------------------------

   function Selected_Wave_Generation
     (This    : Digital_To_Analog_Converter;
      Channel : DAC_Channel)
      return Wave_Generation
   is
      Kind : Wave_Generation_Selection;

      function As_Mask is new Ada.Unchecked_Conversion
        (Target => Noise_Wave_Mask_Selection, Source => UInt4);

      function As_Amplitude is new Ada.Unchecked_Conversion
        (Target => Triangle_Wave_Amplitude_Selection, Source => UInt4);

   begin
      case Channel is
         when Channel_1 =>
            Kind := Wave_Generation_Selection'Val (This.DAC_CR.WAVE1);
      end case;
      declare
         Result : Wave_Generation (Kind);
      begin
         case Kind is
            when No_Wave_Generation =>
               null;

            when Noise_Wave =>
               case Channel is
                  when Channel_1 =>
                     Result.Mask := As_Mask (This.DAC_CR.MAMP1);
               end case;

            when Triangle_Wave =>
               case Channel is
                  when Channel_1 =>
                     Result.Amplitude := As_Amplitude (This.DAC_CR.MAMP1);
               end case;
         end case;

         return Result;
      end;
   end Selected_Wave_Generation;

   ------------------
   -- Data_Address --
   ------------------

   function Data_Address
     (This       : Digital_To_Analog_Converter;
      Channel    : DAC_Channel;
      Resolution : DAC_Resolution;
      Alignment  : Data_Alignment)
      return Address
   is
      Result : Address;
   begin
      case Channel is

         when Channel_1 =>
            case Resolution is
               when DAC_Resolution_12_Bits =>
                  case Alignment is
                     when Left_Aligned =>
                        Result := This.DAC_DHR12L1'Address;
                     when Right_Aligned =>
                        Result := This.DAC_DHR12R1'Address;
                  end case;
               when DAC_Resolution_8_Bits =>
                  Result := This.DAC_DHR8R1'Address;
            end case;

      end case;

      return Result;
   end Data_Address;

end STM32.DAC;
