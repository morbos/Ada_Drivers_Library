with HAL;             use HAL;
with HAL.SPI;         use HAL.SPI;
with STM32.Device;    use STM32.Device;
with STM32.SPI;       use STM32.SPI;
with STM32.SubGhzPhy; use STM32.SubGhzPhy;

with System;

package STM32.SubGhzRF is

   type Set_Standby_Selection is
     (RC_13_MHz,
      HSE32);

   type Set_RegulatorMode_Selection is
     (LDO,
      SMPS);

   type Set_TcxoMode_Trim_Voltage is
     (V_1_6,
      V_1_7,
      V_1_8,
      V_2_2,
      V_2_4,
      V_2_7,
      V_3_0,
      V_3_3)
     with Size => 3;

   for Set_TcxoMode_Trim_Voltage use
     (V_1_6 => 0,
      V_1_7 => 1,
      V_1_8 => 2,
      V_2_2 => 3,
      V_2_4 => 4,
      V_2_7 => 5,
      V_3_0 => 6,
      V_3_3 => 7);

   subtype Opcode_Type is UInt8;
   subtype Subghz_Status is UInt8;

   type Set_TcxoMode_Message is record
      Opcode   : Opcode_Type;
      Reserved : UInt5;
      Trim     : Set_TcxoMode_Trim_Voltage;
      Timeout  : UInt24;
   end record
     with Size => 8 + 32,
     Scalar_Storage_Order => System.High_Order_First,
     Bit_Order => System.High_Order_First;

   for Set_TcxoMode_Message use record
      Opcode      at 0 range 0 .. 7;
      Reserved    at 1 range 0 .. 4;
      Trim        at 1 range 5 .. 7;
      Timeout     at 2 range 0 .. 23;
   end record;

   type Write_Register_Message is record
      Opcode   : Opcode_Type;
      Address  : UInt16;
      Value    : UInt8;
   end record
     with Size => 8 + 16 + 8,
     Scalar_Storage_Order => System.High_Order_First,
     Bit_Order => System.High_Order_First;

   for Write_Register_Message use record
      Opcode   at 0 range 0 .. 7;
      Address  at 1 range 0 .. 15;
      Value    at 3 range 0 .. 7;
   end record;

   type Flag is
     (Disabled,
      Enabled)
     with Size => 1;

   for Flag use
     (Disabled => 0,
      Enabled  => 1);

   type Calibrate_Message is record
      Opcode           : Opcode_Type;
      Reserved         : Boolean;
      ImageCalibration : Flag;
      RF_ADC_BulkP_Cal : Flag;
      RF_ADC_BulkN_Cal : Flag;
      RF_ADC_Pulse_Cal : Flag;
      RF_PLL_Cal       : Flag;
      Sub_GHz_RC13Mhz  : Flag;
      Sub_GHz_RC64Khz  : Flag;
   end record
     with Size => 8 + 8;

   for Calibrate_Message use record
      Opcode            at 0 range 0 .. 7;
      Reserved          at 1 range 7 .. 7;
      ImageCalibration  at 1 range 6 .. 6;
      RF_ADC_BulkP_Cal  at 1 range 5 .. 5;
      RF_ADC_BulkN_Cal  at 1 range 4 .. 4;
      RF_ADC_Pulse_Cal  at 1 range 3 .. 3;
      RF_PLL_Cal        at 1 range 2 .. 2;
      Sub_GHz_RC13Mhz   at 1 range 1 .. 1;
      Sub_GHz_RC64Khz   at 1 range 0 .. 0;
   end record;

   type Set_PacketType_Selection is
     (FSK_Packet,
      LoRa_Packet,
      BPSK_Packet,
      MSK_Packet)
     with Size => 2;

   for Set_PacketType_Selection use
     (FSK_Packet   => 0,
      LoRa_Packet  => 1,
      BPSK_Packet  => 2,
      MSK_Packet   => 3);

   type Set_PacketType_Message is record
      Opcode        : Opcode_Type;
      Reserved      : UInt6;
      PacketType    : Set_PacketType_Selection;
   end record
     with Size => 8 + 8;

   for Set_PacketType_Message use record
      Opcode            at 0 range 0 .. 7;
      Reserved          at 1 range 2 .. 7;
      PacketType        at 1 range 0 .. 1;
   end record;

   type Read_Register_Message is record
      Opcode   : Opcode_Type;
      Address  : UInt16;
   end record
     with Size => 8 + 16,
     Scalar_Storage_Order => System.High_Order_First,
     Bit_Order => System.High_Order_First;

   for Read_Register_Message use record
      Opcode   at 0 range 0 .. 7;
      Address  at 1 range 0 .. 15;
   end record;

   type Set_BufferBaseAddress_Message is record
      Opcode         : Opcode_Type;
      Tx_BaseAddress : UInt8;
      Rx_BaseAddress : UInt8;
   end record
     with Size => 8 + 8 + 8;

   for Set_BufferBaseAddress_Message use record
      Opcode          at 0 range 0 .. 7;
      Tx_BaseAddress  at 1 range 0 .. 7;
      Rx_BaseAddress  at 2 range 0 .. 7;
   end record;

   type PaDutyCycle_Selection is
     (Pa_DC_1,
      Pa_DC_2,
      Pa_DC_3,
      Pa_DC_4,
      Pa_DC_6)
     with Size => 3;

   for PaDutyCycle_Selection use
     (Pa_DC_1 => 1,
      Pa_DC_2 => 2,
      Pa_DC_3 => 3,
      Pa_DC_4 => 4,
      Pa_DC_6 => 6);

   type HpMax_Selection is
     (HpMax_0,
      HpMax_2,
      HpMax_3,
      HpMax_5,
      HpMax_7)
     with Size => 3;

   for HpMax_Selection use
     (HpMax_0 => 0,
      HpMax_2 => 2,
      HpMax_3 => 3,
      HpMax_5 => 5,
      HpMax_7 => 7);

   type PaSel_Choice is
     (HP_PA,
      LP_PA)
     with Size => 1;

   for PaSel_Choice use
     (HP_PA => 0,
      LP_PA => 1);

   type Set_PaConfig_Message is record
      Opcode         : Opcode_Type;
      Res1           : UInt5;
      PaDutyCycle    : PaDutyCycle_Selection;
      Res2           : UInt5;
      HpMax          : HpMax_Selection;
      Res3           : UInt7;
      PaSel          : PaSel_Choice;
      Extra          : UInt8;
   end record
     with Size => 8 + (4 * 8);
--     Scalar_Storage_Order => System.High_Order_First,
--     Bit_Order => System.High_Order_First;

   for Set_PaConfig_Message use record
      Opcode          at 0 range 0 .. 7;
      Res1            at 1 range 3 .. 7;
      PaDutyCycle     at 1 range 0 .. 2;
      Res2            at 2 range 3 .. 7;
      HpMax           at 2 range 0 .. 2;
      Res3            at 3 range 1 .. 7;
      PaSel           at 3 range 0 .. 0;
      Extra           at 4 range 0 .. 7;
   end record;

   type LP_Power_Selection is range -17 .. 14
     with Size => 8;

   type HP_Power_Selection is range -9 .. 14
     with Size => 8;

--   type Power_Selection
--     (Choice : PaSel_Choice := LP_PA)
--      is record
--      case Choice is
--         when LP_PA =>
--            LPSel : LP_Power_Selection;
--         when HP_PA =>
--            HPSel : HP_Power_Selection;
--      end case;
--   end record
--     with Unchecked_Union, Size => 8;

   type RampTime_Selection is
     (Microsecs_10,
      Microsecs_20,
      Microsecs_40,
      Microsecs_80,
      Microsecs_200,
      Microsecs_800,
      Microsecs_1700,
      Microsecs_3400)
     with Size => 8;

   for RampTime_Selection use
     (Microsecs_10    => 0,
      Microsecs_20    => 1,
      Microsecs_40    => 2,
      Microsecs_80    => 3,
      Microsecs_200   => 4,
      Microsecs_800   => 5,
      Microsecs_1700  => 6,
      Microsecs_3400  => 7);

   type Set_TxParams_Message is record
      Opcode         : Opcode_Type;
      --      Power          : Power_Selection;
      Power          : UInt8;
      RampTime       : RampTime_Selection;
   end record
     with Size => 8 + 8 + 8;

   for Set_TxParams_Message use record
      Opcode          at 0 range 0 .. 7;
      Power           at 1 range 0 .. 7;
      RampTime        at 2 range 0 .. 7;
   end record;

   type Cfg_DioIrq_Message is record
      Opcode         : Opcode_Type;
      Irq_Mask       : UInt16;
      Irq_Mask1      : UInt16;
      Irq_Mask2      : UInt16;
      Irq_Mask3      : UInt16;
   end record
     with Size => 8 + (4 * 16);

   for Cfg_DioIrq_Message use record
      Opcode          at 0 range 0 .. 7;
      Irq_Mask        at 1 range 0 .. 15;
      Irq_Mask1       at 3 range 0 .. 15;
      Irq_Mask2       at 5 range 0 .. 15;
      Irq_Mask3       at 7 range 0 .. 15;
   end record;

   type SleepCfg_Start_Selection is
     (Cold_Startup,
      Warm_Startup)
     with Size => 1;

   type SleepCfg_Wakeup_Stance is
     (Disabled,
      Enabled)
     with Size => 1;

   type Set_Sleep_Message is record
      Opcode         : Opcode_Type;
      Res1           : UInt5;
      StartSel       : SleepCfg_Start_Selection;
      Res2           : Boolean;
      SleepCfg       : SleepCfg_Wakeup_Stance;
   end record
     with Size => 8 + 8;
--     Scalar_Storage_Order => System.High_Order_First,
--     Bit_Order => System.High_Order_First;

   for Set_Sleep_Message use record
      Opcode          at 0 range 0 .. 7;
      Res1            at 1 range 3 .. 7;
      StartSel        at 1 range 2 .. 2;
      Res2            at 1 range 1 .. 1;
      SleepCfg        at 1 range 0 .. 0;
   end record;

   type SpreadFactor_Selection is
     (Factor_5,
      Factor_6,
      Factor_7,
      Factor_8,
      Factor_9,
      Factor_10,
      Factor_11,
      Factor_12)
     with Size => 4;

   for SpreadFactor_Selection use
     (Factor_5  => 16#5#,
      Factor_6  => 16#6#,
      Factor_7  => 16#7#,
      Factor_8  => 16#8#,
      Factor_9  => 16#9#,
      Factor_10 => 16#a#,
      Factor_11 => 16#b#,
      Factor_12 => 16#c#);

   type BandWidth_Selection is
     (BandWidth_7Khz,
      BandWidth_15Khz,
      BandWidth_31Khz,
      BandWidth_62Khz,
      BandWidth_125Khz,
      BandWidth_250Khz,
      BandWidth_500Khz,
      BandWidth_10Khz,
      BandWidth_20Khz,
      BandWidth_41Khz
     )
     with Size => 8;

   for BandWidth_Selection use
     (
      BandWidth_7Khz   => 16#0#,
      BandWidth_15Khz  => 16#1#,
      BandWidth_31Khz  => 16#2#,
      BandWidth_62Khz  => 16#3#,
      BandWidth_125Khz => 16#4#,
      BandWidth_250Khz => 16#5#,
      BandWidth_500Khz => 16#6#,
      BandWidth_10Khz  => 16#8#,
      BandWidth_20Khz  => 16#9#,
      BandWidth_41Khz  => 16#a#
     );

   type CodingRate_Selection is
     (CR_NoFEC_4_4,
      CR_FEC_4_5,
      CR_FEC_4_6,
      CR_FEC_4_7,
      CR_FEC_4_8)
     with Size => 3;

   for CodingRate_Selection use
     (CR_NoFEC_4_4 => 0,
      CR_FEC_4_5   => 1,
      CR_FEC_4_6   => 2,
      CR_FEC_4_7   => 3,
      CR_FEC_4_8   => 4);

   type Low_Data_Rate_Opt_Selection is
     (Disabled,
      Enabled)
     with Size => 1;

   type LoRa_Mod_Params_Message is record
      Opcode            : Opcode_Type;
      Res1              : UInt4;
      SpreadFactor      : SpreadFactor_Selection;
      BandWidth         : BandWidth_Selection;
      Res2              : UInt5;
      CodingRate        : CodingRate_Selection;
      Res3              : UInt7;
      Low_Data_Rate_Opt : Low_Data_Rate_Opt_Selection;
   end record
     with Size => 8 + (4 * 8);
--     Scalar_Storage_Order => System.High_Order_First,
--     Bit_Order => System.High_Order_First;

   for LoRa_Mod_Params_Message use record
      Opcode              at 0 range 0 .. 7;
      Res1                at 1 range 4 .. 7;
      SpreadFactor        at 1 range 0 .. 3;
      BandWidth           at 2 range 0 .. 7;
      Res2                at 3 range 3 .. 7;
      CodingRate          at 3 range 0 .. 2;
      Res3                at 4 range 1 .. 7;
      Low_Data_Rate_Opt   at 4 range 0 .. 0;
   end record;

   type HeaderType_Selection is
     (Explicit,
      Implicit)
     with Size => 1;

   type InvertIQ_Selection is
     (Standard,
      Inverted)
     with Size => 1;

   type LoRa_Set_PacketParams_Message is record
      Opcode              : Opcode_Type;
      PreambleLength      : UInt16;
      Res1                : UInt7;
      HeaderType          : HeaderType_Selection;
      PayloadLength       : UInt8;
      Res2                : UInt7;
      Crc                 : Flag;
      Res3                : UInt7;
      InvertIQ            : InvertIQ_Selection;
   end record
     with Size => 8 + (6 * 8);
--     Scalar_Storage_Order => System.High_Order_First,
--     Bit_Order => System.High_Order_First;

   for LoRa_Set_PacketParams_Message use record
      Opcode              at 0 range 0 .. 7;
      PreambleLength      at 1 range 0 .. 15;
      Res1                at 3 range 1 .. 7;
      HeaderType          at 3 range 0 .. 0;
      PayloadLength       at 4 range 0 .. 7;
      Res2                at 5 range 1 .. 7;
      Crc                 at 5 range 0 .. 0;
      Res3                at 6 range 1 .. 7;
      InvertIQ            at 6 range 0 .. 0;
   end record;

   type Set_StopRxTimerOnPreamble_Selection is
     (Stop_On_Sync,
      Stop_On_Preamble);


   --  All F calc's are based on a 32Mhz Xtal. I have not seen an SX1276 w/o
   --  a 32Mhz Xtal.
   Xtal : constant Float := 32.0E6;

   Step : constant Float := Xtal / Float (2 ** 25);

   type Frequency is delta Step range 137.0E6 .. 1020.0E6
     with Size => 32;

   for Frequency'Small use Step;

   type Set_RfFrequency_Message is record
      Opcode              : Opcode_Type;
      F                   : Frequency;
   end record
     with Size => 8 + 32;

   for Set_RfFrequency_Message use record
      Opcode            at 0 range 0 .. 7;
      F                 at 1 range 0 .. 31;
   end record;

   procedure NSS_Assert;

   procedure NSS_Deassert;

   procedure SubGhzRF_Init;

   procedure Set_Standby (Choice : Set_Standby_Selection);

   procedure Set_RegulatorMode (Choice : Set_RegulatorMode_Selection);

   procedure Set_TcxoMode (Trim : Set_TcxoMode_Message);

   procedure Read_Register (Reg       : Read_Register_Message;
                            RFStatus  : out Subghz_Status;
                            Value     : out UInt8);

   procedure Write_Register (Reg : Write_Register_Message);

   procedure Calibrate (CalRec : Calibrate_Message);

   procedure Set_PacketType (PacketType : Set_PacketType_Message);

   procedure Set_BufferBaseAddress (Bases : Set_BufferBaseAddress_Message);

   procedure Set_PaConfig (PaCfg : Set_PaConfig_Message);

   procedure Set_TxParams (TxParams : Set_TxParams_Message);

   procedure Cfg_DioIrq (Cfg : Cfg_DioIrq_Message);

   procedure Set_Sleep (Cfg : Set_Sleep_Message);

   procedure LoRa_Set_ModulationParams (Params : LoRa_Mod_Params_Message);

   procedure LoRa_Set_PacketParams (Params : LoRa_Set_PacketParams_Message);

   procedure Set_StopRxTimerOnPreamble (Choice : Set_StopRxTimerOnPreamble_Selection);

   procedure Set_LoRaSymbTimeout (NumberOfSymbols : UInt8);

   procedure Set_RfFrequency (Freq : Set_RfFrequency_Message);

   procedure Set_Rx (Timeout : UInt32);

   procedure Set_Tx (Timeout : UInt32);

   procedure Swap16 (X : in out UInt16);

   procedure Swap24 (X : in out UInt24);

   procedure Swap32 (X : in out UInt32);

   procedure WaitOnBusy;

   procedure CheckDeviceReady;

   function Get_IrqStatus (RFStatus  : out Subghz_Status) return UInt16;

   procedure Clr_IrqStatus (IrqStatus : UInt16);

   procedure Get_RxBufferStatus (RFStatus             : out Subghz_Status;
                                 RxPayloadLength      : out UInt8;
                                 RxStartBufferPointer : out UInt8);

   procedure Read_Buffer (Offset   : UInt8;
                          RFStatus : out Subghz_Status;
                          Buffer   : out SPI_Data_8b);

   procedure LoRa_Get_Stats (RFStatus : out Subghz_Status;
                             NbPktReceived : out UInt16;
                             NbPktCrcError : out UInt16;
                             NbPktLengthError : out UInt16);

   procedure Reset_Stats;

   procedure Toggle_TxRx;

   Opcode_CalibrateImage            : constant Opcode_Type := 16#98#;
   Opcode_Calibrate                 : constant Opcode_Type := 16#89#;
   Opcode_Cfg_DioIrq                : constant Opcode_Type := 16#08#;
   Opcode_Clr_Error                 : constant Opcode_Type := 16#07#;
   Opcode_Clr_IrqStatus             : constant Opcode_Type := 16#02#;
   Opcode_Get_Error                 : constant Opcode_Type := 16#17#;
   Opcode_Get_IrqStatus             : constant Opcode_Type := 16#12#;
   Opcode_Get_PacketStatus          : constant Opcode_Type := 16#14#;
   Opcode_Get_PacketType            : constant Opcode_Type := 16#11#;
   Opcode_Get_RssiInst              : constant Opcode_Type := 16#15#;
   Opcode_Get_RxBufferStatus        : constant Opcode_Type := 16#13#;
   Opcode_Get_Stats                 : constant Opcode_Type := 16#10#;
   Opcode_Get_Status                : constant Opcode_Type := 16#C0#;
   Opcode_Read_Buffer               : constant Opcode_Type := 16#1E#;
   Opcode_Read_Register             : constant Opcode_Type := 16#1d#;
   Opcode_Reset_Stats               : constant Opcode_Type := 16#00#;
   Opcode_Set_BufferBaseAddress     : constant Opcode_Type := 16#8f#;
   Opcode_Set_Cad                   : constant Opcode_Type := 16#c5#;
   Opcode_Set_CadParams             : constant Opcode_Type := 16#88#;
   Opcode_Set_Fs                    : constant Opcode_Type := 16#c1#;
   Opcode_Set_LoRaSymbTimeout       : constant Opcode_Type := 16#a0#;
   Opcode_Set_ModulationParams      : constant Opcode_Type := 16#8B#;
   Opcode_Set_PaConfig              : constant Opcode_Type := 16#95#;
   Opcode_Set_PacketParams          : constant Opcode_Type := 16#8c#;
   Opcode_Set_PacketType            : constant Opcode_Type := 16#8a#;
   Opcode_Set_RegulatorMode         : constant Opcode_Type := 16#96#;
   Opcode_Set_RfFrequency           : constant Opcode_Type := 16#86#;
   Opcode_Set_Rx                    : constant Opcode_Type := 16#82#;
   Opcode_Set_RxDutyCycle           : constant Opcode_Type := 16#94#;
   Opcode_Set_TxRxFallbackMode      : constant Opcode_Type := 16#93#;
   Opcode_Set_Sleep                 : constant Opcode_Type := 16#84#;
   Opcode_Set_Standby               : constant Opcode_Type := 16#80#;
   Opcode_Set_StopRxTimerOnPreamble : constant Opcode_Type := 16#9f#;
   Opcode_Set_TcxoMode              : constant Opcode_Type := 16#97#;
   Opcode_Set_Tx                    : constant Opcode_Type := 16#83#;
   Opcode_Set_TxContinuousWave      : constant Opcode_Type := 16#d1#;
   Opcode_Set_TxContinuousPreamble  : constant Opcode_Type := 16#d2#;
   Opcode_Set_TxParams              : constant Opcode_Type := 16#8e#;
   Opcode_Write_Buffer              : constant Opcode_Type := 16#0e#;
   Opcode_Write_Register            : constant Opcode_Type := 16#0d#;

   procedure Xyzzy;
   pragma Import (C, Xyzzy, "xyzzy");

end STM32.SubGhzRF;
