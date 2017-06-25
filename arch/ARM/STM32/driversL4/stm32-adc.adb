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

   procedure Set_Sampling_Time
     (This        : in out Analog_To_Digital_Converter;
      Channel     : Analog_Input_Channel;
      Sample_Time : Channel_Sampling_Times)
     with Inline;

   procedure Set_Injected_Channel_Sequence_Position
     (This    : in out Analog_To_Digital_Converter;
      Channel : Analog_Input_Channel;
      Rank    : Injected_Channel_Rank)
     with Inline;

   procedure Set_Injected_Channel_Offset
     (This   : in out Analog_To_Digital_Converter;
      Rank   : Injected_Channel_Rank;
      Offset : Injected_Data_Offset)
     with Inline;
   
   function Init (This : in out Analog_To_Digital_Converter) return Boolean is
      Cal : Boolean;
   begin
      --  Exit from deep powerdown if so set
      if This.CR.DEEPPWD then
	 This.CR.DEEPPWD := False; --  Cal is now a must btw
	 
      end if;
  
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
      Start : Time_Span;
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
      return This.DR.regularDATA;
   end Conversion_Value;

   ---------------------------
   -- Data_Register_Address --
   ---------------------------

   function Data_Register_Address
     (This : Analog_To_Digital_Converter)
      return System.Address
   is
      (This.DR'Address);

   -------------------------------
   -- Injected_Conversion_Value --
   -------------------------------

   function Injected_Conversion_Value
     (This : Analog_To_Digital_Converter;
      Rank : Injected_Channel_Rank)
      return UInt16
   is
   begin
      case Rank is
         when 1 =>
            return This.JDR1.JDATA1;
         when 2 =>
            return This.JDR2.JDATA2;
         when 3 =>
            return This.JDR3.JDATA3;
         when 4 =>
            return This.JDR4.JDATA4;
      end case;
   end Injected_Conversion_Value;

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
      This.CFGR.RES := ADC_Resolution'Enum_Rep (Resolution);
      This.CFGR.ALIGN := Alignment = Left_Aligned;
   end Configure_Unit;

   ------------------------
   -- Current_Resolution --
   ------------------------

   function Current_Resolution
     (This : Analog_To_Digital_Converter)
      return ADC_Resolution
   is (ADC_Resolution'Val (This.CFGR.RES));

   -----------------------
   -- Current_Alignment --
   -----------------------

   function Current_Alignment
     (This : Analog_To_Digital_Converter)
      return Data_Alignment
   is ((if This.CFGR.ALIGN then Left_Aligned else Right_Aligned));

   ---------------------------------
   -- Configure_Common_Properties --
   ---------------------------------

   procedure Configure_Common_Properties
     (Mode           : Multi_ADC_Mode_Selections;
      Prescalar      : ADC_Prescalars;
      DMA_Mode       : Multi_ADC_DMA_Modes;
      Sampling_Delay : Sampling_Delay_Selections)
   is
   begin
--      C_ADC_Periph.CCR.MULT    := Multi_ADC_Mode_Selections'Enum_Rep (Mode);
--      C_ADC_Periph.CCR.DELAY_k :=
--        Sampling_Delay_Selections'Enum_Rep (Sampling_Delay);
--      C_ADC_Periph.CCR.DMA     := Multi_ADC_DMA_Modes'Enum_Rep (DMA_Mode);
--      C_ADC_Periph.CCR.ADCPRE  := ADC_Prescalars'Enum_Rep (Prescalar);
      null;
   end Configure_Common_Properties;

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
      This.CFGR.CONT := Continuous;
--
--      This.CR1.SCAN := Conversions'Length > 1;
--
      
--      if Trigger.Enabler /= Trigger_Disabled then
--         This.CR2.EXTSEL := External_Events_Regular_Group'Enum_Rep (Trigger.Event);
--         This.CR2.EXTEN := External_Trigger'Enum_Rep (Trigger.Enabler);
--      else
         This.CFGR.EXTSEL := 0;
         This.CFGR.EXTEN := 0;
--      end if;
--
      for Rank in Conversions'Range loop
         declare
            Conversion : Regular_Channel_Conversion renames Conversions (Rank);
         begin
            Configure_Regular_Channel
              (This, Conversion.Channel, Rank, Conversion.Sample_Time);
--
--            --  We check the VBat first because that channel is also used for
--            --  the temperature sensor channel on some MCUs, in which case the
--            --  VBat conversion is the only one done. This order reflects that
--            --  hardware behavior.
--            if VBat_Conversion (This, Conversion.Channel) then
--               Enable_VBat_Connection;
--            elsif VRef_TemperatureSensor_Conversion (This, Conversion.Channel)
--            then
--               Enable_VRef_TemperatureSensor_Connection;
--            end if;
         end;
      end loop;

      This.SQR1.L3 := UInt4 (Conversions'Length - 1);  -- biased rep

   end Configure_Regular_Conversions;

   ------------------------------------
   -- Configure_Injected_Conversions --
   ------------------------------------

   procedure Configure_Injected_Conversions
     (This          : in out Analog_To_Digital_Converter;
      AutoInjection : Boolean;
      Trigger       : Injected_Channel_Conversion_Trigger;
      Enable_EOC    : Boolean;
      Conversions   : Injected_Channel_Conversions)
   is
   begin
      This.IER.EOCIE := Enable_EOC;

      --  Injected channels cannot be converted continuously. The only
      --  exception is when an injected channel is configured to be converted
      --  automatically after regular channels in continuous mode. See note in
      --  RM 13.3.5, pg 390, and "Auto-injection" section on pg 392.
      This.CFGR.JAUTO := AutoInjection;

      if Trigger.Enabler /= Trigger_Disabled then
         This.JSQR.JEXTEN := External_Trigger'Enum_Rep (Trigger.Enabler);
         This.JSQR.JEXTSEL := External_Events_Injected_Group'Enum_Rep (Trigger.Event);
      else
         This.JSQR.JEXTEN := 0;
         This.JSQR.JEXTSEL := 0;
      end if;

      for Rank in Conversions'Range loop
         declare
            Conversion : Injected_Channel_Conversion renames
              Conversions (Rank);
         begin
            Configure_Injected_Channel
              (This,
               Conversion.Channel,
               Rank,
               Conversion.Sample_Time,
               Conversion.Offset);

            --  We check the VBat first because that channel is also used for
            --  the temperature sensor channel on some MCUs, in which case the
            --  VBat conversion is the only one done. This order reflects that
            --  hardware behavior.
            if VBat_Conversion (This, Conversion.Channel) then
               Enable_VBat_Connection;
            elsif VRef_TemperatureSensor_Conversion (This, Conversion.Channel)
            then
               Enable_VRef_TemperatureSensor_Connection;
            end if;
         end;
      end loop;

      This.JSQR.JL := UInt2 (Conversions'Length - 1);  -- biased rep
   end Configure_Injected_Conversions;

   ----------------------------
   -- Enable_VBat_Connection --
   ----------------------------

   procedure Enable_VBat_Connection is
   begin
      --      C_ADC_Periph.CCR.VBATE := True;
      null;
   end Enable_VBat_Connection;

   ------------------
   -- VBat_Enabled --
   ------------------

   function VBat_Enabled return Boolean is
      --      (C_ADC_Periph.CCR.VBATE);
      (False);

   ----------------------------------------------
   -- Enable_VRef_TemperatureSensor_Connection --
   ----------------------------------------------

   procedure Enable_VRef_TemperatureSensor_Connection is
   begin
--      C_ADC_Periph.CCR.TSVREFE := True;
--      delay until Clock + Temperature_Sensor_Stabilization;
      null;
   end Enable_VRef_TemperatureSensor_Connection;

   --------------------------------------
   -- VRef_TemperatureSensor_Connected --
   --------------------------------------

   function VRef_TemperatureSensor_Enabled return Boolean is
      --      (C_ADC_Periph.CCR.TSVREFE);
      (False);

   ----------------------------------
   -- Regular_Conversions_Expected --
   ----------------------------------

   function Regular_Conversions_Expected (This : Analog_To_Digital_Converter)
     return Natural is
     (Natural (This.SQR1.L3) + 1);

   -----------------------------------
   -- Injected_Conversions_Expected --
   -----------------------------------

   function Injected_Conversions_Expected (This : Analog_To_Digital_Converter)
     return Natural is
     (Natural (This.JSQR.JL) + 1);

   -----------------------
   -- Scan_Mode_Enabled --
   -----------------------

   function Scan_Mode_Enabled (This : Analog_To_Digital_Converter)
                               return Boolean
     --     is (This.CFGR.SCAN);
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
      Sample_Time : Channel_Sampling_Times)
   is
   begin
      Set_Sampling_Time (This, Channel, Sample_Time);
      Set_Sequence_Position (This, Channel, Rank);
   end Configure_Regular_Channel;

   --------------------------------
   -- Configure_Injected_Channel --
   --------------------------------

   procedure Configure_Injected_Channel
     (This        : in out Analog_To_Digital_Converter;
      Channel     : Analog_Input_Channel;
      Rank        : Injected_Channel_Rank;
      Sample_Time : Channel_Sampling_Times;
      Offset      : Injected_Data_Offset)
   is
   begin
      Set_Sampling_Time (This, Channel, Sample_Time);
      Set_Injected_Channel_Sequence_Position (This, Channel, Rank);
      Set_Injected_Channel_Offset (This, Rank, Offset);
   end Configure_Injected_Channel;

   ----------------------
   -- Start_Conversion --
   ----------------------

   procedure Start_Conversion (This : in out Analog_To_Digital_Converter) is
   begin
      if External_Trigger'Val (This.CFGR.EXTEN) /= Trigger_Disabled then
         return;
      end if;
      
      -- L4 vvv out till we find out about multi-adc
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

   -------------------------------
   -- Start_Injected_Conversion --
   -------------------------------

   procedure Start_Injected_Conversion
     (This : in out Analog_To_Digital_Converter)
   is
   begin
      This.CR.JADSTART := True;
   end Start_Injected_Conversion;

   ---------------------------------
   -- Injected_Conversion_Started --
   ---------------------------------

   function Injected_Conversion_Started (This : Analog_To_Digital_Converter)
     return Boolean
   is
     (This.CR.JADSTART);

   ------------------------------
   -- Watchdog_Enable_Channels --
   ------------------------------

   procedure Watchdog_Enable_Channels
     (This : in out Analog_To_Digital_Converter;
      Mode : Multiple_Channels_Watchdog;
      Low  : Watchdog_Threshold;
      High : Watchdog_Threshold)
   is
   begin
      This.TR1.HT1 := High;
      This.TR1.LT1 := Low;
      --  see RM 13.3.7, pg 391, table 66
      case Mode is
         when Watchdog_All_Regular_Channels =>
            This.CFGR.AWD1EN := True;
         when Watchdog_All_Injected_Channels =>
            This.CFGR.JAWD1EN := True;
         when Watchdog_All_Both_Kinds =>
            This.CFGR.AWD1EN := True;
            This.CFGR.JAWD1EN := True;
      end case;
   end Watchdog_Enable_Channels;

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
      This.TR1.HT1 := High;
      This.TR1.LT1 := Low;

      --  Set then channel
      This.CFGR.AWDCH1CH := Channel;
      --  Enable single channel mode
      This.CFGR.AWD1SGL := True;

      case Mode is
         when Watchdog_Single_Regular_Channel =>
            This.CFGR.AWD1EN := True;
         when Watchdog_Single_Injected_Channel =>
            This.CFGR.JAWD1EN := True;
         when Watchdog_Single_Both_Kinds =>
            This.CFGR.AWD1EN := True;
            This.CFGR.JAWD1EN := True;
      end case;
   end Watchdog_Enable_Channel;

   ----------------------
   -- Watchdog_Disable --
   ----------------------

   procedure Watchdog_Disable (This : in out Analog_To_Digital_Converter) is
   begin
      This.CFGR.AWD1EN := False;
      This.CFGR.JAWD1EN := False;

      --  clearing the single-channel bit (AWGSDL) is not required to disable,
      --  per the RM table 66, section 13.3.7, pg 391, but seems cleanest
      This.CFGR.AWD1SGL := False;
   end Watchdog_Disable;

   ----------------------
   -- Watchdog_Enabled --
   ----------------------

   function Watchdog_Enabled (This : Analog_To_Digital_Converter)
     return Boolean
   is
      (This.CFGR.AWD1EN or This.CFGR.JAWD1EN);
   --  per the RM table 66, section 13.3.7, pg 391

   -------------------------------
   -- Enable_Discontinuous_Mode --
   -------------------------------

   procedure Enable_Discontinuous_Mode
     (This    : in out Analog_To_Digital_Converter;
      Regular : Boolean;  -- if False, enabling for Injected channels
      Count   : Discontinuous_Mode_Channel_Count)
   is
   begin
      if Regular then
         This.CFGR.JDISCEN := False;
         This.CFGR.DISCEN := True;
      else -- Injected
         This.CFGR.DISCEN := False;
         This.CFGR.JDISCEN := True;
      end if;
      This.CFGR.DISCNUM := UInt3 (Count - 1);  -- biased
   end Enable_Discontinuous_Mode;

   ----------------------------------------
   -- Disable_Discontinuous_Mode_Regular --
   ---------------------------------------

   procedure Disable_Discontinuous_Mode_Regular
     (This : in out Analog_To_Digital_Converter)
   is
   begin
      This.CFGR.DISCEN := False;
   end Disable_Discontinuous_Mode_Regular;

   -----------------------------------------
   -- Disable_Discontinuous_Mode_Injected --
   -----------------------------------------

   procedure Disable_Discontinuous_Mode_Injected
     (This : in out Analog_To_Digital_Converter)
   is
   begin
      This.CFGR.JDISCEN := False;
   end Disable_Discontinuous_Mode_Injected;

   ----------------------------------------
   -- Discontinuous_Mode_Regular_Enabled --
   ----------------------------------------

   function Discontinuous_Mode_Regular_Enabled
     (This : Analog_To_Digital_Converter)
      return Boolean
   is (This.CFGR.DISCEN);

   -----------------------------------------
   -- Discontinuous_Mode_Injected_Enabled --
   -----------------------------------------

   function Discontinuous_Mode_Injected_Enabled
     (This : Analog_To_Digital_Converter)
      return Boolean
   is (This.CFGR.JDISCEN);

   ---------------------------
   -- AutoInjection_Enabled --
   ---------------------------

   function AutoInjection_Enabled
     (This : Analog_To_Digital_Converter)
      return Boolean
   is (This.CFGR.JAUTO);

   ----------------
   -- Enable_DMA --
   ----------------

   procedure Enable_DMA (This : in out Analog_To_Digital_Converter) is
   begin
      This.CFGR.DMAEN := True;
   end Enable_DMA;

   -----------------
   -- Disable_DMA --
   -----------------

   procedure Disable_DMA (This : in out Analog_To_Digital_Converter) is
   begin
      This.CFGR.DMAEN := False;
   end Disable_DMA;

   -----------------
   -- DMA_Enabled --
   -----------------

   function DMA_Enabled (This : Analog_To_Digital_Converter) return Boolean is
     (This.CFGR.DMAEN);

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

   ------------------------------------------
   -- Multi_Enable_DMA_After_Last_Transfer --
   ------------------------------------------

   procedure Multi_Enable_DMA_After_Last_Transfer is
   begin
      --      C_ADC_Periph.CCR.DMA := 1;
      null;
   end Multi_Enable_DMA_After_Last_Transfer;

   -------------------------------------------
   -- Multi_Disable_DMA_After_Last_Transfer --
   -------------------------------------------

   procedure Multi_Disable_DMA_After_Last_Transfer is
   begin
      --      C_ADC_Periph.CCR.DMA := 0;
      null;
   end Multi_Disable_DMA_After_Last_Transfer;

   -------------------------------------------
   -- Multi_DMA_Enabled_After_Last_Transfer --
   -------------------------------------------

   function Multi_DMA_Enabled_After_Last_Transfer return Boolean is
      --     (C_ADC_Periph.CCR.DMA = 1);
      (False);

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
         when Injected_Channel_Conversion_Started =>
            return This.CR.JADSTART;
         when Injected_Channel_Conversion_Complete =>
            return This.ISR.JEOC;
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
         when Injected_Channel_Conversion_Started =>
            This.CR.JADSTART := False;
         when Injected_Channel_Conversion_Complete =>
            This.ISR.JEOC := False;
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
         when Injected_Channel_Conversion_Complete =>
            This.IER.JEOCIE := True;
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
         when Injected_Channel_Conversion_Complete =>
            return This.IER.JEOCIE;
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
         when Injected_Channel_Conversion_Complete =>
            This.IER.JEOCIE := False;
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
         when Injected_Channel_Conversion_Complete =>
            This.ISR.JEOC := False;
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
      case Rank is
         when 1 =>
	    This.SQR1.SQ1 := Channel;
         when 2 =>
	    This.SQR1.SQ2 := Channel;
         when 3 =>
	    This.SQR1.SQ3 := Channel;
         when 4 =>
	    This.SQR1.SQ4 := Channel;
	    
         when 5 =>
	    This.SQR2.SQ5 := Channel;
         when 6 =>
	    This.SQR2.SQ6 := Channel;
         when 7 =>
	    This.SQR2.SQ7 := Channel;
         when 8 =>
	    This.SQR2.SQ8 := Channel;
         when 9 =>
	    This.SQR2.SQ9 := Channel;
	    
         when 10 =>
	    This.SQR3.SQ10 := Channel;
         when 11 =>
	    This.SQR3.SQ11 := Channel;
         when 12 =>
	    This.SQR3.SQ12 := Channel;
         when 13 =>
	    This.SQR3.SQ13 := Channel;
         when 14 =>
	    This.SQR3.SQ14 := Channel;
	    
	 when 15 =>
	    This.SQR4.SQ15 := Channel;
         when 16 =>
	    This.SQR4.SQ16 := Channel;
	    
	 when others =>
	    null;
      end case;
   end Set_Sequence_Position;

   --------------------------------------------
   -- Set_Injected_Channel_Sequence_Position --
   --------------------------------------------

   procedure Set_Injected_Channel_Sequence_Position
     (This    : in out Analog_To_Digital_Converter;
      Channel : Analog_Input_Channel;
      Rank    : Injected_Channel_Rank)
   is
   begin
      --  alas.. F4 is an array... not here since they alloc 6bits for a 5bit fld.
      case Integer (Rank) is
	 when 1 =>
	    This.JSQR.JSQ1 := Channel;
	 when 2 =>
	    This.JSQR.JSQ2 := Channel;
	 when 3 =>
	    This.JSQR.JSQ3 := Channel;
	 when 4 =>
	    This.JSQR.JSQ4 := Channel;
	 when others =>
	    null;
      end case;
   end Set_Injected_Channel_Sequence_Position;

   -----------------------
   -- Set_Sampling_Time --
   -----------------------

   procedure Set_Sampling_Time
     (This        : in out Analog_To_Digital_Converter;
      Channel     : Analog_Input_Channel;
      Sample_Time : Channel_Sampling_Times)
   is
   begin
      if Channel > 9 then
         This.SMPR2.SMP.Arr (Natural (Channel)) :=
           Channel_Sampling_Times'Enum_Rep (Sample_Time);
      else
         This.SMPR1.SMP.Arr (Natural (Channel)) :=
           Channel_Sampling_Times'Enum_Rep (Sample_Time);
      end if;
   end Set_Sampling_Time;

   ---------------------------------
   -- Set_Injected_Channel_Offset --
   ---------------------------------

   procedure Set_Injected_Channel_Offset
     (This   : in out Analog_To_Digital_Converter;
      Rank   : Injected_Channel_Rank;
      Offset : Injected_Data_Offset)
   is
   begin
      case Rank is
         when 1 => This.OFR1.OFFSET1 := Offset;
         when 2 => This.OFR2.OFFSET2 := Offset;
         when 3 => This.OFR3.OFFSET3 := Offset;
         when 4 => This.OFR4.OFFSET4 := Offset;
      end case;
   end Set_Injected_Channel_Offset;

end STM32.ADC;
