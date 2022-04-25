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
--   @file    stm32f4xx_hal_adc.h                                           --
--   @author  MCD Application Team                                          --
--   @version V1.3.1                                                        --
--   @date    25-March-2015                                                 --
--   @brief   Header file of ADC HAL module.                                --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

--  This file provides interfaces for the analog-to-digital converters on the
--  STM32F4 (ARM Cortex M4F) microcontrollers from ST Microelectronics.

--  Channels are mapped to GPIO_Point values as follows.  See
--  the STM32F40x datasheet, Table 7. "STM32F40x pin and ball definitions"
--
--  Channel    ADC    ADC    ADC
--    #         1      2      3
--
--    0        PA0    PA0    PA0
--    1        PA1    PA1    PA1
--    2        PA2    PA2    PA2
--    3        PA3    PA3    PA3
--    4        PA4    PA4    PF6
--    5        PA5    PA5    PF7
--    6        PA6    PA6    PF8
--    7        PA7    PA7    PF9
--    8        PB0    PB0    PF10
--    9        PB1    PB1    PF3
--   10        PC0    PC0    PC0
--   11        PC1    PC1    PC1
--   12        PC2    PC2    PC2
--   13        PC3    PC3    PC3
--   14        PC4    PC4    PF4
--   15        PC5    PC5    PF5

with System;        use System;
with Ada.Real_Time; use Ada.Real_Time;

private with STM32_SVD.ADC;

package STM32.ADC is
   pragma Elaborate_Body;

   type Analog_To_Digital_Converter is limited private;

   subtype Analog_Input_Channel is UInt5 range 0 .. 14;

   type ADC_Point is record
      ADC     : access Analog_To_Digital_Converter;
      Channel : Analog_Input_Channel;
   end record;

   --  TODO: ??? The below predicate does not compile with GNAT GPL 2015.
   --  with Static_Predicate => TemperatureSensor_Channel in 16 | VBat_Channel;
   --  See RM pg 389 section 13.3.3. On some MCUs the temperature channel is
   --  the same as the VBat channel, on others it is channel 16. Note only
   --  available with ADC_1

   ADC_Supply_Voltage : constant := 3000;  -- millivolts
   --  This is the ideal value, likely not the actual

   procedure Enable (This : in out Analog_To_Digital_Converter) with
     Post => Enabled (This);

   procedure Disable (This : in out Analog_To_Digital_Converter) with
     Post => not Enabled (This);

   function Enabled (This : Analog_To_Digital_Converter) return Boolean;

   type ADC_Resolution is
     (ADC_Resolution_12_Bits,  -- 15 ADC Clock cycles
      ADC_Resolution_10_Bits,  -- 13 ADC Clock cycles
      ADC_Resolution_8_Bits,   -- 11 ADC Clock cycles
      ADC_Resolution_6_Bits);  --  9 ADC Clock cycles


   type Sample_Choice is
     (Sample1, Sample2); --  Each channel can only have one of 2 sample times.

   type Data_Alignment is (Right_Aligned, Left_Aligned);

   procedure Configure_Unit
     (This       : in out Analog_To_Digital_Converter;
      Resolution : ADC_Resolution;
      Alignment  : Data_Alignment)
    with
      Post => Current_Resolution (This) = Resolution and
              Current_Alignment (This) = Alignment;

   function Current_Resolution (This : Analog_To_Digital_Converter)
      return ADC_Resolution;

   function Current_Alignment (This : Analog_To_Digital_Converter)
      return Data_Alignment;

   type Channel_Sampling_Times is
     (Sample_1_5_Cycles,
      Sample_3_5_Cycles,
      Sample_7_5_Cycles,
      Sample_12_5_Cycles,
      Sample_19_5_Cycles,
      Sample_39_5_Cycles,
      Sample_79_5_Cycles,
      Sample_160_5_Cycles)
     with Size => 3;

   type External_Trigger is
     (Trigger_Disabled,
      Trigger_Rising_Edge,
      Trigger_Falling_Edge,
      Trigger_Both_Edges);

   type Regular_Channel_Rank is new Natural range 1 .. 8;

   type External_Events_Regular_Group is
     (Timer1_CC1_Event,
      Timer1_CC2_Event,
      Timer1_CC3_Event,
      Timer2_CC2_Event,
      Timer2_CC3_Event,
      Timer2_CC4_Event,
      Timer2_TRGO_Event,
      Timer3_CC1_Event,
      Timer3_TRGO_Event,
      Timer4_CC4_Event,
      Timer5_CC1_Event,
      Timer5_CC2_Event,
      Timer5_CC3_Event,
      Timer8_CC1_Event,
      Timer8_TRGO_Event,
      EXTI_Line11);

   type Regular_Channel_Conversion_Trigger (Enabler : External_Trigger) is
      record
         case Enabler is
            when Trigger_Disabled =>
               null;
            when others =>
               Event : External_Events_Regular_Group;
         end case;
      end record;

   Software_Triggered : constant Regular_Channel_Conversion_Trigger
     := (Enabler => Trigger_Disabled);

   type Regular_Channel_Conversion is record
      Channel     : Analog_Input_Channel;
      Sample_Time : Sample_Choice;
   end record;

   type Regular_Channel_Conversions is
     array (Regular_Channel_Rank range <>) of Regular_Channel_Conversion;

   procedure Set_Sampling_Choice
     (This        : in out Analog_To_Digital_Converter;
      Index       : Sample_Choice;
      Sample_Time : Channel_Sampling_Times);

   procedure Set_Sampling_Time
     (This        : in out Analog_To_Digital_Converter;
      Channel     : Analog_Input_Channel;
      Sample_Time : Sample_Choice);

   procedure Configure_Regular_Conversions
     (This        : in out Analog_To_Digital_Converter;
      Continuous  : Boolean;
      Trigger     : Regular_Channel_Conversion_Trigger;
      Enable_EOC  : Boolean;
      Conversions : Regular_Channel_Conversions)
     with
       Pre => Conversions'Length > 0;

   function Scan_Mode_Enabled (This : Analog_To_Digital_Converter)
     return Boolean;
   --  Returns whether only one channel is converted, or if multiple channels
   --  are converted (i.e., scanned). Note that this is independent of whether
   --  the conversions are continuous.

   function EOC_Selection_Enabled (This : Analog_To_Digital_Converter)
     return Boolean;
   --  Returns whether the End of Conversion Selection (EOCS) bit is enabled.
   --  See the EOCS bit definition, RM pg 417.
   --
   --  When EOCS is not enabled, the EOC bit in the Status Register is set at
   --  the end of each *sequence* of regular conversions. Overrun detection is
   --  enabled only if DMA is enabled.
   --
   --  When EOCS is enabled, the EOC bit in the SR is set at the end of each
   --  *individual* regular conversion, and overrun detection is enabled.

   type External_Events_Injected_Group is
     (Timer1_CC4_Event,
      Timer1_TRGO_Event,
      Timer2_CC1_Event,
      Timer2_TRGO_Event,
      Timer3_CC2_Event,
      Timer3_CC4_Event,
      Timer4_CC1_Event,
      Timer4_CC2_Event,
      Timer4_CC3_Event,
      Timer4_TRGO_Event,
      Timer5_CC4_Event,
      Timer5_TRGO_Event,
      Timer8_CC2_Event,
      Timer8_CC3_Event,
      Timer8_CC4_Event,
      EXTI_Line15);

   function VBat_Enabled return Boolean;
   --  Returns whether the hardware has the VBat internal connection enabled

   function VRef_Enabled return Boolean;
   --  Returns whether the hardware has the VRef internal connection enabled

   function TemperatureSensor_Enabled return Boolean;
   --  Returns whether the hardware has the temperature sensor connection enabled

   procedure Start_Conversion (This : in out Analog_To_Digital_Converter) with
     Pre => Enabled (This);
   --  Starts the conversion(s) for the regular channels

   function Conversion_Started (This : Analog_To_Digital_Converter)
     return Boolean;
   --  Returns whether the regular channels' conversions have started. Note
   --  that the ADC hardware clears the corresponding bit immediately, as
   --  part of starting.

   function Conversion_Value (This : Analog_To_Digital_Converter)
      return UInt16 with Inline;
   --  Returns the latest regular conversion result for the specified ADC unit

   function Data_Register_Address (This : Analog_To_Digital_Converter)
     return System.Address
     with Inline;
   --  Returns the address of the ADC Data Register. This is exported
   --  STRICTLY for the sake of clients using DMA. All other
   --  clients of this package should use the Conversion_Value functions!
   --  Seriously, don't use this function otherwise.

   type Discontinuous_Mode_Channel_Count is range 1 .. 8;
   --  Note this uses a biased representation implicitly because the underlying
   --  representational bit values are 0 ... 7

   procedure Enable_Discontinuous_Mode
     (This    : in out Analog_To_Digital_Converter)
     with
       Post => Discontinuous_Mode_Regular_Enabled (This);

   --  Enables discontinuous mode and sets the count. If Regular is True,
   --  enables the mode only for regular channels. If Regular is False, enables
   --  the mode only for Injected channels. The note in RM 13.3.10, pg 393,
   --  says we cannot enable the mode for both regular and injected channels
   --  at the same time, so this flag ensures we follow that rule.

   procedure Disable_Discontinuous_Mode_Regular
     (This : in out Analog_To_Digital_Converter)
      with Post => not Discontinuous_Mode_Regular_Enabled (This);

   function Discontinuous_Mode_Regular_Enabled
     (This : Analog_To_Digital_Converter)
     return Boolean;

   --  DMA Management  --------------------------------------------------------

   procedure Enable_DMA (This : in out Analog_To_Digital_Converter) with
     Post => DMA_Enabled (This);

   procedure Disable_DMA (This : in out Analog_To_Digital_Converter) with
     Post => not DMA_Enabled (This);

   function DMA_Enabled (This : Analog_To_Digital_Converter) return Boolean;

   procedure Enable_DMA_After_Last_Transfer
     (This : in out Analog_To_Digital_Converter) with
     Post => DMA_Enabled_After_Last_Transfer (This);

   procedure Disable_DMA_After_Last_Transfer
     (This : in out Analog_To_Digital_Converter) with
     Post => not DMA_Enabled_After_Last_Transfer (This);

   function DMA_Enabled_After_Last_Transfer
     (This : Analog_To_Digital_Converter)
      return Boolean;

   --  Analog Watchdog  -------------------------------------------------------

   subtype Watchdog_Threshold is UInt12;

   type Analog_Watchdog_Modes is
     (Watchdog_All_Regular_Channels,
      Watchdog_All_Both_Kinds,
      Watchdog_Single_Regular_Channel,
      Watchdog_Single_Both_Kinds);

   subtype Single_Channel_Watchdog is Analog_Watchdog_Modes
     range Watchdog_Single_Regular_Channel .. Watchdog_Single_Both_Kinds;

   procedure Watchdog_Enable_Channel
     (This    : in out Analog_To_Digital_Converter;
      Mode    : Single_Channel_Watchdog;
      Channel : Analog_Input_Channel;
      Low     : Watchdog_Threshold;
      High    : Watchdog_Threshold)
     with
       Pre  => not Watchdog_Enabled (This),
       Post => Watchdog_Enabled (This);
   --  Enables the watchdog on this single channel, and no others. The kind of
   --  channel depends on Mode. A call to this routine is considered a complete
   --  configuration of the watchdog so do not call the other enabler routine
   --  (for all channels) while this configuration is active. You must
   --  first disable the watchdog if you want to enable the watchdog for
   --  all channels.

   procedure Watchdog_Disable (This : in out Analog_To_Digital_Converter)
     with Post => not Watchdog_Enabled (This);
   --  Whether watching a single channel or all of them, the watchdog is now
   --  disabled

   function Watchdog_Enabled (This : Analog_To_Digital_Converter)
      return Boolean;

   --  Status Management  -----------------------------------------------------

   type ADC_Status_Flag is
     (Overrun,
      Regular_Channel_Conversion_Started,
      Regular_Channel_Conversion_Complete,
      Analog_Watchdog_Event_Occurred);

   function Status
     (This : Analog_To_Digital_Converter;
      Flag : ADC_Status_Flag)
      return Boolean
     with Inline;
   --  Returns whether Flag is indicated, ie set in the Status Register

   procedure Clear_Status
     (This : in out Analog_To_Digital_Converter;
      Flag : ADC_Status_Flag)
     with
       Inline,
       Post => not Status (This, Flag);

   procedure Poll_For_Status
     (This    : in out Analog_To_Digital_Converter;
      Flag    : ADC_Status_Flag;
      Success : out Boolean;
      Timeout : Time_Span := Time_Span_Last);
   --  Continuously polls for the specified status flag to be set, up to the
   --  deadline computed by the value of Clock + Timeout. Sets the Success
   --  argument accordingly. The default Time_Span_Last value is the largest
   --  possible value, thereby setting a very long, but not infinite, timeout.

   --  Interrupt Management  --------------------------------------------------

   type ADC_Interrupts is
     (Overrun,
      Regular_Channel_Conversion_Complete,
      Analog_Watchdog_Event);

   procedure Enable_Interrupts
     (This   : in out Analog_To_Digital_Converter;
      Source : ADC_Interrupts)
     with
       Inline,
       Post => Interrupt_Enabled (This, Source);

   procedure Disable_Interrupts
     (This   : in out Analog_To_Digital_Converter;
      Source : ADC_Interrupts)
     with
       Inline,
       Post => not Interrupt_Enabled (This, Source);

   function Interrupt_Enabled
     (This   : Analog_To_Digital_Converter;
      Source : ADC_Interrupts)
      return Boolean
     with Inline;

   procedure Clear_Interrupt_Pending
     (This   : in out Analog_To_Digital_Converter;
      Source : ADC_Interrupts)
     with Inline;

   --  Common Properties ------------------------------------------------------

   type ADC_Prescalars is
     (PCLK2_Div_2,
      PCLK2_Div_4,
      PCLK2_Div_6,
      PCLK2_Div_8);

   type Sampling_Delay_Selections is
     (Sampling_Delay_5_Cycles,
      Sampling_Delay_6_Cycles,
      Sampling_Delay_7_Cycles,
      Sampling_Delay_8_Cycles,
      Sampling_Delay_9_Cycles,
      Sampling_Delay_10_Cycles,
      Sampling_Delay_11_Cycles,
      Sampling_Delay_12_Cycles,
      Sampling_Delay_13_Cycles,
      Sampling_Delay_14_Cycles,
      Sampling_Delay_15_Cycles,
      Sampling_Delay_16_Cycles,
      Sampling_Delay_17_Cycles,
      Sampling_Delay_18_Cycles,
      Sampling_Delay_19_Cycles,
      Sampling_Delay_20_Cycles);

   function Calibration (This : in out Analog_To_Digital_Converter) return Boolean;

   function Init (This : in out Analog_To_Digital_Converter) return Boolean;

private

   ADC_Stabilization                : constant Time_Span := Microseconds (3);
   ADC_Calibration                  : constant Time_Span := Microseconds (800); --  0.8ms per datasheet
   ADC_VREG_Stabilization           : constant Time_Span := Microseconds (10);
   Temperature_Sensor_Stabilization : constant Time_Span := Microseconds (10);
   --  The RM, section 13.3.6, says stabilization times are required. These
   --  values are specified in the datasheets, eg section 5.3.20, pg 129,
   --  and section 5.3.21, pg 134, of the STM32F405/7xx, DocID022152 Rev 4.

   procedure Configure_Regular_Channel
     (This        : in out Analog_To_Digital_Converter;
      Channel     : Analog_Input_Channel;
      Rank        : Regular_Channel_Rank;
      Sample_Time : Sample_Choice);

   procedure Enable_VBat_Connection with
     Post => VBat_Enabled;

   procedure Enable_VRef_Connection with
     Post => VRef_Enabled;

   procedure Enable_TemperatureSensor_Connection with
     Post => TemperatureSensor_Enabled;

   type Analog_To_Digital_Converter is new STM32_SVD.ADC.ADC_Peripheral;

end STM32.ADC;
