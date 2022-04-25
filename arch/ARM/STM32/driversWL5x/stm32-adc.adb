------------------------------------------------------------------------------
--                                                                          --
--                  Copyright (C) 2015-2017, AdaCore                        --
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
--   @file    stm32f4xx_hal_adc.c                                           --
--   @author  MCD Application Team                                          --
--   @version V1.3.1                                                        --
--   @date    25-March-2015                                                 --
--   @brief   Header file of ADC HAL module.                                --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

with STM32_SVD.ADC; use STM32_SVD.ADC;

package body STM32.ADC is

   procedure Set_Sequence_Position
     (This    : in out Analog_To_Digital_Converter;
      Channel : Analog_Input_Channel;
      Rank    : Regular_Channel_Rank)
     with Inline;

   function Init (This : in out Analog_To_Digital_Converter) return Boolean is
      Cal : Boolean;
   begin
      This.CR.ADVREGEN := True;
      --  Wait 10usec per STs code
      delay until Clock + ADC_VREG_Stabilization;

      Cal := Calibration (This);

      if not Cal or not This.CR.ADVREGEN then
         return False;
      else
         return True;
      end if;

   end Init;

   function Calibration (This : in out Analog_To_Digital_Converter) return Boolean is
--      Start : Time_Span;
   begin
      --  ADC should be disabled here.
      This.CR.ADCAL := True;

      delay until Clock + ADC_Calibration;

      return This.CR.ADCAL = False;

   end Calibration;
   ------------
   -- Enable --
   ------------

   procedure Enable (This : in out Analog_To_Digital_Converter) is
   begin
      if not This.CR.ADEN then
         This.CR.ADEN := True;
         delay until Clock + ADC_Stabilization;
      end if;
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable (This : in out Analog_To_Digital_Converter) is
   begin
      This.CR.ADEN := False;
   end Disable;

   -------------
   -- Enabled --
   -------------

   function Enabled (This : Analog_To_Digital_Converter) return Boolean is
     (This.CR.ADEN);

   ----------------------
   -- Conversion_Value --
   ----------------------

   function Conversion_Value
     (This : Analog_To_Digital_Converter)
      return UInt16
   is
   begin
      return This.DR.DATA;
   end Conversion_Value;

   ---------------------------
   -- Data_Register_Address --
   ---------------------------

   function Data_Register_Address
     (This : Analog_To_Digital_Converter)
      return System.Address
   is
      (This.DR'Address);


   --------------------------------
   -- Multimode_Conversion_Value --
   --------------------------------

   function Multimode_Conversion_Value return UInt32 is
      --(C_ADC_Periph.CDR.Val);
      (0);

   --------------------
   -- Configure_Unit --
   --------------------

   procedure Configure_Unit
     (This       : in out Analog_To_Digital_Converter;
      Resolution : ADC_Resolution;
      Alignment  : Data_Alignment)
   is
   begin
      This.CFGR1.RES := ADC_Resolution'Enum_Rep (Resolution);
      This.CFGR1.ALIGN := Alignment = Left_Aligned;
      This.CR.ADVREGEN := True;
   end Configure_Unit;

   ------------------------
   -- Current_Resolution --
   ------------------------

   function Current_Resolution
     (This : Analog_To_Digital_Converter)
      return ADC_Resolution
   is (ADC_Resolution'Val (This.CFGR1.RES));

   -----------------------
   -- Current_Alignment --
   -----------------------

   function Current_Alignment
     (This : Analog_To_Digital_Converter)
      return Data_Alignment
   is ((if This.CFGR1.ALIGN then Left_Aligned else Right_Aligned));

   -----------------------------------
   -- Configure_Regular_Conversions --
   -----------------------------------

   procedure Configure_Regular_Conversions
     (This        : in out Analog_To_Digital_Converter;
      Continuous  : Boolean;
      Trigger     : Regular_Channel_Conversion_Trigger;
      Enable_EOC  : Boolean;
      Conversions : Regular_Channel_Conversions)
   is
   begin
      This.IER.EOCIE := Enable_EOC;
      This.CFGR1.CONT := Continuous;
--
--      This.CR1.SCAN := Conversions'Length > 1;
--

--      if Trigger.Enabler /= Trigger_Disabled then
--         This.CR2.EXTSEL := External_Events_Regular_Group'Enum_Rep (Trigger.Event);
--         This.CR2.EXTEN := External_Trigger'Enum_Rep (Trigger.Enabler);
--      else
         This.CFGR1.EXTSEL := 0;
         This.CFGR1.EXTEN := 0;
--      end if;
--
      for Rank in Conversions'Range loop
         declare
            Conversion : Regular_Channel_Conversion renames Conversions (Rank);
         begin
            Configure_Regular_Channel
              (This, Conversion.Channel, Rank, Conversion.Sample_Time);
         end;
      end loop;

--      This.SQR1.L := UInt4 (Conversions'Length - 1);  -- biased rep

   end Configure_Regular_Conversions;

   ----------------------------
   -- Enable_VBat_Connection --
   ----------------------------

   procedure Enable_VBat_Connection is
   begin
      ADC_Periph.CCR.VBATEN := True;
   end Enable_VBat_Connection;

   ------------------
   -- VBat_Enabled --
   ------------------

   function VBat_Enabled return Boolean is
      (ADC_Periph.CCR.VBATEN);

   ----------------------------------------------
   -- Enable_VRef_Connection --
   ----------------------------------------------

   procedure Enable_VRef_Connection is
   begin
      ADC_Periph.CCR.VREFEN := True;
      null;
   end Enable_VRef_Connection;

   --------------------------------------
   -- VRef_Connected --
   --------------------------------------

   function VRef_Enabled return Boolean is
      (ADC_Periph.CCR.VREFEN);

   ----------------------------------------------
   -- Enable_TemperatureSensor_Connection --
   ----------------------------------------------

   procedure Enable_TemperatureSensor_Connection is
   begin
      ADC_Periph.CCR.TSEN := True;
   end Enable_TemperatureSensor_Connection;

   --------------------------------------
   -- VRef_Connected --
   --------------------------------------

   function TemperatureSensor_Enabled return Boolean is
      (ADC_Periph.CCR.TSEN);

   -----------------------
   -- Scan_Mode_Enabled --
   -----------------------

   function Scan_Mode_Enabled (This : Analog_To_Digital_Converter)
                               return Boolean
   is (False);

   ---------------------------
   -- EOC_Selection_Enabled --
   ---------------------------

   function EOC_Selection_Enabled (This : Analog_To_Digital_Converter)
                                   return Boolean
     is (This.IER.EOCIE);

   -------------------------------
   -- Configure_Regular_Channel --
   -------------------------------

   procedure Configure_Regular_Channel
     (This        : in out Analog_To_Digital_Converter;
      Channel     : Analog_Input_Channel;
      Rank        : Regular_Channel_Rank;
      Sample_Time : Sample_Choice)
   is
   begin
      Set_Sampling_Time (This, Channel, Sample_Time);
      Set_Sequence_Position (This, Channel, Rank);
   end Configure_Regular_Channel;


   ----------------------
   -- Start_Conversion --
   ----------------------

   procedure Start_Conversion (This : in out Analog_To_Digital_Converter) is
   begin
      if External_Trigger'Val (This.CFGR1.EXTEN) /= Trigger_Disabled then
         return;
      end if;

--  L4 vvv out till we find out about multi-adc
--      if Multi_ADC_Mode_Selections'Val (C_ADC_Periph.CCR.MULT) = Independent
--        or else This'Address = STM32_SVD.ADC1_Base
--      then
         This.CR.ADSTART := True;
--      end if;
   end Start_Conversion;

   ------------------------
   -- Conversion_Started --
   ------------------------

   function Conversion_Started (This : Analog_To_Digital_Converter)
      return Boolean
   is
      (This.CR.ADSTART);

   -----------------------------
   -- Watchdog_Enable_Channel --
   -----------------------------

   procedure Watchdog_Enable_Channel
     (This    : in out Analog_To_Digital_Converter;
      Mode    : Single_Channel_Watchdog;
      Channel : Analog_Input_Channel;
      Low     : Watchdog_Threshold;
      High    : Watchdog_Threshold)
   is
   begin
      null;
--      This.TR1.HT1 := High;
--      This.TR1.LT1 := Low;
--
--      --  Set then channel
--      This.CFGR1.AWDCH1CH := Channel;
--      --  Enable single channel mode
--      This.CFGR1.AWD1SGL := True;
--      This.CFGR1.AWD1EN := True;
   end Watchdog_Enable_Channel;

   ----------------------
   -- Watchdog_Disable --
   ----------------------

   procedure Watchdog_Disable (This : in out Analog_To_Digital_Converter) is
   begin
      This.CFGR1.AWD1EN := False;
      --  clearing the single-channel bit (AWGSDL) is not required to disable,
      --  per the RM table 66, section 13.3.7, pg 391, but seems cleanest
      This.CFGR1.AWD1SGL := False;
   end Watchdog_Disable;

   ----------------------
   -- Watchdog_Enabled --
   ----------------------

   function Watchdog_Enabled (This : Analog_To_Digital_Converter)
     return Boolean
   is
      (This.CFGR1.AWD1EN);

   -------------------------------
   -- Enable_Discontinuous_Mode --
   -------------------------------

   procedure Enable_Discontinuous_Mode
     (This    : in out Analog_To_Digital_Converter)
   is
   begin
      This.CFGR1.DISCEN := True;
--      This.CFGR1.DISCNUM := UInt3 (Count - 1);  -- biased
   end Enable_Discontinuous_Mode;

   ----------------------------------------
   -- Disable_Discontinuous_Mode_Regular --
   ---------------------------------------

   procedure Disable_Discontinuous_Mode_Regular
     (This : in out Analog_To_Digital_Converter)
   is
   begin
      This.CFGR1.DISCEN := False;
   end Disable_Discontinuous_Mode_Regular;

   ----------------------------------------
   -- Discontinuous_Mode_Regular_Enabled --
   ----------------------------------------

   function Discontinuous_Mode_Regular_Enabled
     (This : Analog_To_Digital_Converter)
      return Boolean
   is (This.CFGR1.DISCEN);

   ----------------
   -- Enable_DMA --
   ----------------

   procedure Enable_DMA (This : in out Analog_To_Digital_Converter) is
   begin
      This.CFGR1.DMAEN := True;
   end Enable_DMA;

   -----------------
   -- Disable_DMA --
   -----------------

   procedure Disable_DMA (This : in out Analog_To_Digital_Converter) is
   begin
      This.CFGR1.DMAEN := False;
   end Disable_DMA;

   -----------------
   -- DMA_Enabled --
   -----------------

   function DMA_Enabled (This : Analog_To_Digital_Converter) return Boolean is
     (This.CFGR1.DMAEN);

   ------------------------------------
   -- Enable_DMA_After_Last_Transfer --
   ------------------------------------

   procedure Enable_DMA_After_Last_Transfer
     (This : in out Analog_To_Digital_Converter)
   is
   begin
      --      This.CR2.DDS := True;  -- doesn't exist on L4?
      null;
   end Enable_DMA_After_Last_Transfer;

   -------------------------------------
   -- Disable_DMA_After_Last_Transfer --
   -------------------------------------

   procedure Disable_DMA_After_Last_Transfer
     (This : in out Analog_To_Digital_Converter)
   is
   begin
      --      This.CR2.DDS := False;
      null;
   end Disable_DMA_After_Last_Transfer;

   -------------------------------------
   -- DMA_Enabled_After_Last_Transfer --
   -------------------------------------

   function DMA_Enabled_After_Last_Transfer
     (This : Analog_To_Digital_Converter)
     return Boolean
     --   is (This.CR2.DDS);
   is (False);

   ---------------------
   -- Poll_For_Status --
   ---------------------

   procedure Poll_For_Status
     (This    : in out Analog_To_Digital_Converter;
      Flag    : ADC_Status_Flag;
      Success : out Boolean;
      Timeout : Time_Span := Time_Span_Last)
   is
      Deadline : constant Time := Clock + Timeout;
   begin
      Success := False;
      while Clock < Deadline loop
         if Status (This, Flag) then
            Success := True;
            exit;
         end if;
      end loop;
   end Poll_For_Status;

   ------------
   -- Status --
   ------------

   function Status
     (This : Analog_To_Digital_Converter;
      Flag : ADC_Status_Flag)
      return Boolean
   is
   begin
      case Flag is
         when Overrun =>
            return This.ISR.OVR;
         when Regular_Channel_Conversion_Started =>
            return This.CR.ADSTART;
         when Regular_Channel_Conversion_Complete =>
            return This.ISR.EOC;
         when Analog_Watchdog_Event_Occurred =>
            return This.ISR.AWD.Arr (1);
      end case;
   end Status;

   ------------------
   -- Clear_Status --
   ------------------

   procedure Clear_Status
     (This : in out Analog_To_Digital_Converter;
      Flag : ADC_Status_Flag)
   is
   begin
      case Flag is
         when Overrun =>
            This.ISR.OVR := False;
         when Regular_Channel_Conversion_Started =>
            This.CR.ADSTART := False;
         when Regular_Channel_Conversion_Complete =>
            This.ISR.EOC := False;
         when Analog_Watchdog_Event_Occurred =>
            This.ISR.AWD.Arr (1) := False;
      end case;
   end Clear_Status;

   -----------------------
   -- Enable_Interrupts --
   -----------------------

   procedure Enable_Interrupts
     (This   : in out Analog_To_Digital_Converter;
      Source : ADC_Interrupts)
   is
   begin
      case Source is
         when Overrun =>
            This.IER.OVRIE := True;
         when Regular_Channel_Conversion_Complete =>
            This.IER.EOCIE := True;
         when Analog_Watchdog_Event =>
            This.IER.AWD1IE := True;
      end case;
   end Enable_Interrupts;

   -----------------------
   -- Interrupt_Enabled --
   -----------------------

   function Interrupt_Enabled
     (This   : Analog_To_Digital_Converter;
      Source : ADC_Interrupts)
     return Boolean
   is
   begin
      case Source is
         when Overrun =>
            return This.IER.OVRIE;
         when Regular_Channel_Conversion_Complete =>
            return This.IER.EOCIE;
         when Analog_Watchdog_Event =>
            return This.IER.AWD1IE;
      end case;
   end Interrupt_Enabled;

   ------------------------
   -- Disable_Interrupts --
   ------------------------

   procedure Disable_Interrupts
     (This   : in out Analog_To_Digital_Converter;
      Source : ADC_Interrupts)
   is
   begin
      case Source is
         when Overrun =>
            This.IER.OVRIE := False;
         when Regular_Channel_Conversion_Complete =>
            This.IER.EOCIE := False;
         when Analog_Watchdog_Event =>
            This.IER.AWD1IE := False;
      end case;
   end Disable_Interrupts;

   -----------------------------
   -- Clear_Interrupt_Pending --
   -----------------------------

   procedure Clear_Interrupt_Pending
     (This   : in out Analog_To_Digital_Converter;
      Source : ADC_Interrupts)
   is
   begin
      case Source is
         when Overrun =>
            This.ISR.OVR := False;
         when Regular_Channel_Conversion_Complete =>
            This.ISR.EOC := False;
         when Analog_Watchdog_Event =>
            This.ISR.AWD.Arr (1) := False;
      end case;
   end Clear_Interrupt_Pending;

   ---------------------------
   -- Set_Sequence_Position --
   ---------------------------

   procedure Set_Sequence_Position
     (This    : in out Analog_To_Digital_Converter;
      Channel : Analog_Input_Channel;
      Rank    : Regular_Channel_Rank)
   is
   begin
      This.CHSELR1.Arr (Integer (Rank)) := UInt4 (Channel);
   end Set_Sequence_Position;


   procedure Set_Sampling_Choice
     (This        : in out Analog_To_Digital_Converter;
      Index       : Sample_Choice;
      Sample_Time : Channel_Sampling_Times)
   is
   begin
      if Index = Sample1 then
         This.SMPR.SMP1 := Channel_Sampling_Times'Enum_Rep (Sample_Time);
      else
         This.SMPR.SMP2 := Channel_Sampling_Times'Enum_Rep (Sample_Time);
      end if;
   end Set_Sampling_Choice;

   -----------------------
   -- Set_Sampling_Time --
   -----------------------

   procedure Set_Sampling_Time
     (This        : in out Analog_To_Digital_Converter;
      Channel     : Analog_Input_Channel;
      Sample_Time : Sample_Choice)
   is
   begin
      if Sample_Time = Sample2 then
         This.SMPR.SMPSEL := This.SMPR.SMPSEL or 2 ** (Natural (Channel));
      else
         This.SMPR.SMPSEL := This.SMPR.SMPSEL and not (2 ** (Natural (Channel)));
      end if;
   end Set_Sampling_Time;

end STM32.ADC;
