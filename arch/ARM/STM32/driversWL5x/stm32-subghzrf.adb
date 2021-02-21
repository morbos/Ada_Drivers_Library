with STM32_SVD.PWR; use STM32_SVD.PWR;
with Ada.Real_Time; use Ada.Real_Time;
package body STM32.SubGhzRF is
--   procedure SubGhzRF_RegRead (Addr : UInt16; Value : out UInt8)
--   is
--   begin
--      null;
--   end SubGhzRF_RegRead;

   Pa_Power_Choice : PaSel_Choice := LP_PA;
   GX  : UInt8;

   procedure NSS_Assert
   is
   begin
      Flush_Fifo (SubGhzPhyPort.all);
      PWR_Periph.SUBGHZSPICR.NSS := False;
   end NSS_Assert;

   procedure NSS_Deassert
   is
   begin
      PWR_Periph.SUBGHZSPICR.NSS := True;
   end NSS_Deassert;

   procedure WaitOnBusy
   is
      Mask : Boolean;
   begin
      loop
         Mask := PWR_Periph.SR2.RFBUSYMS;
         --  Need a timeout here
         exit when not (Mask and PWR_Periph.SR2.RFBUSYS);
      end loop;
   end WaitOnBusy;

   procedure CheckDeviceReady
   is
   begin
      --  if in deep sleep... wait (add later)
      NSS_Assert;
      null;
      NSS_Deassert;
      WaitOnBusy;
   end CheckDeviceReady;

   procedure Set_Standby (Choice : Set_Standby_Selection)
   is
      Msg    : SPI_Data_8b (1 .. 2);
      Status : SPI_Status;
   begin
      Msg (1) := Opcode_Set_Standby;
      Msg (2) := Choice'Enum_Rep;
      CheckDeviceReady;
      NSS_Assert;
      Transmit (SubGhzPhyPort.all, Msg, Status);
      NSS_Deassert;
   end Set_Standby;

   procedure Set_RegulatorMode (Choice : Set_RegulatorMode_Selection)
   is
      Msg    : SPI_Data_8b (1 .. 2);
      Status : SPI_Status;
   begin
      Msg (1) := Opcode_Set_RegulatorMode;
      Msg (2) := Choice'Enum_Rep;
      CheckDeviceReady;
      NSS_Assert;
      Transmit (SubGhzPhyPort.all, Msg, Status);
      NSS_Deassert;
   end Set_RegulatorMode;

   procedure Set_TcxoMode (Trim : Set_TcxoMode_Message)
   is
      Msg    : SPI_Data_8b (1 .. Trim'Size / 8);
      LTrim  : Set_TcxoMode_Message := Trim;
      for Msg'Address use LTrim'Address;
      Status : SPI_Status;
   begin
      LTrim.Opcode := Opcode_Set_TcxoMode;
      CheckDeviceReady;
      NSS_Assert;
      Transmit (SubGhzPhyPort.all, Msg, Status);
      NSS_Deassert;
   end Set_TcxoMode;

   procedure Write_Register (Reg : Write_Register_Message)
   is
      Msg    : SPI_Data_8b (1 .. Reg'Size / 8);
      LReg   : Write_Register_Message := Reg;
      for Msg'Address use LReg'Address;
      Status : SPI_Status;
   begin
      LReg.Opcode := Opcode_Write_Register;
      CheckDeviceReady;
      NSS_Assert;
      Transmit (SubGhzPhyPort.all, Msg, Status);
      NSS_Deassert;
      WaitOnBusy;
   end Write_Register;

   procedure Read_Register (Reg       : Read_Register_Message;
                            RFStatus  : out Subghz_Status;
                            Value     : out UInt8)
   is
      Msg    : SPI_Data_8b (1 .. Reg'Size / 8);
      Reply  : SPI_Data_8b (1 .. 2);
      LReg   : Read_Register_Message := Reg;
      for Msg'Address use LReg'Address;
      Status : SPI_Status;
   begin
      LReg.Opcode := Opcode_Read_Register;
      CheckDeviceReady;
      NSS_Assert;
      Transmit (SubGhzPhyPort.all, Msg, Status);
      Flush_Fifo (SubGhzPhyPort.all);
      Receive (SubGhzPhyPort.all, Reply, Status);
      NSS_Deassert;
      WaitOnBusy;
      RFStatus := Reply (1);
      Value := Reply (2);
   end Read_Register;

   procedure Calibrate (CalRec : Calibrate_Message)
   is
      Msg      : SPI_Data_8b (1 .. CalRec'Size / 8);
      LCalRec  : Calibrate_Message := CalRec;
      for Msg'Address use LCalRec'Address;
      Status : SPI_Status;
   begin
      LCalRec.Opcode := Opcode_Calibrate;
      CheckDeviceReady;
      NSS_Assert;
      Transmit (SubGhzPhyPort.all, Msg, Status);
      NSS_Deassert;
   end Calibrate;

   procedure Set_PacketType (PacketType : Set_PacketType_Message)
   is
      Msg      : SPI_Data_8b (1 .. PacketType'Size / 8);
      LPacketType  : Set_PacketType_Message := PacketType;
      for Msg'Address use LPacketType'Address;
      Status : SPI_Status;
   begin
      LPacketType.Opcode := Opcode_Set_PacketType;
      CheckDeviceReady;
      NSS_Assert;
      Transmit (SubGhzPhyPort.all, Msg, Status);
      NSS_Deassert;
   end Set_PacketType;

   procedure Set_BufferBaseAddress (Bases : Set_BufferBaseAddress_Message)
   is
      Msg      : SPI_Data_8b (1 .. Bases'Size / 8);
      LBases   : Set_BufferBaseAddress_Message := Bases;
      for Msg'Address use LBases'Address;
      Status : SPI_Status;
   begin
      LBases.Opcode := Opcode_Set_BufferBaseAddress;
      CheckDeviceReady;
      NSS_Assert;
      Transmit (SubGhzPhyPort.all, Msg, Status);
      NSS_Deassert;
   end Set_BufferBaseAddress;

   procedure Set_PaConfig (PaCfg : Set_PaConfig_Message)
   is
      Msg      : SPI_Data_8b (1 .. PaCfg'Size / 8);
      LPaCfg   : Set_PaConfig_Message := PaCfg;
      for Msg'Address use LPaCfg'Address;
      Status : SPI_Status;
   begin
      LPaCfg.Opcode := Opcode_Set_PaConfig;
      LPaCfg.Extra  := 1;
      Pa_Power_Choice := PaCfg.PaSel; --  Make a note of this. Its used in later setup
      CheckDeviceReady;
      NSS_Assert;
      Transmit (SubGhzPhyPort.all, Msg, Status);
      NSS_Deassert;
   end Set_PaConfig;

   procedure Set_TxParams (TxParams : Set_TxParams_Message)
   is
      Msg        : SPI_Data_8b (1 .. TxParams'Size / 8);
      LTxParams  : Set_TxParams_Message := TxParams;
      for Msg'Address use LTxParams'Address;
      Status     : SPI_Status;
   begin
      LTxParams.Opcode := Opcode_Set_TxParams;
      if (Pa_Power_Choice = LP_PA) and (TxParams.Power in -17 .. 14) then
         raise Program_Error with "LP power out of range";
      elsif Pa_Power_Choice = HP_PA and TxParams.Power in -9 .. 14 then
         raise Program_Error with "HP power out of range";
      end if;
      CheckDeviceReady;
      NSS_Assert;
      Transmit (SubGhzPhyPort.all, Msg, Status);
      NSS_Deassert;
   end Set_TxParams;

   procedure Cfg_DioIrq (Cfg : Cfg_DioIrq_Message)
   is
      Msg        : SPI_Data_8b (1 .. Cfg'Size / 8);
      LCfg       : Cfg_DioIrq_Message := Cfg;
      for Msg'Address use LCfg'Address;
      Status     : SPI_Status;
   begin
      LCfg.Opcode := Opcode_Cfg_DioIrq;
      CheckDeviceReady;
      NSS_Assert;
      Transmit (SubGhzPhyPort.all, Msg, Status);
      NSS_Deassert;
   end Cfg_DioIrq;

   procedure Set_Sleep (Cfg : Set_Sleep_Message)
   is
      Msg        : SPI_Data_8b (1 .. Cfg'Size / 8);
      LCfg       : Set_Sleep_Message := Cfg;
      for Msg'Address use LCfg'Address;
      Status     : SPI_Status;
   begin
      LCfg.Opcode := Opcode_Set_Sleep;
      CheckDeviceReady;
      NSS_Assert;
      Transmit (SubGhzPhyPort.all, Msg, Status);
      NSS_Deassert;
   end Set_Sleep;

   procedure LoRa_Set_ModulationParams (Params : LoRa_Mod_Params_Message)
   is
      Msg        : SPI_Data_8b (1 .. Params'Size / 8);
      LParams    : LoRa_Mod_Params_Message := Params;
      for Msg'Address use LParams'Address;
      Status     : SPI_Status;
   begin
      LParams.Opcode := Opcode_Set_ModulationParams;
      CheckDeviceReady;
      NSS_Assert;
      Transmit (SubGhzPhyPort.all, Msg, Status);
      NSS_Deassert;
   end LoRa_Set_ModulationParams;

   procedure Swap16 (X : in out UInt16)
   is
   begin
      X := Shift_Right (X, 8) or Shift_Left (X, 8);
   end Swap16;

   procedure Swap24 (X : in out UInt24)
   is
      LX : UInt24 := X;
      B : UInt8_Array (1 .. 4);
      for LX'Address use B'Address;
      for LX'Alignment use 1;
   begin
      B := (B (4), B (3), B (2), B (1));
      X := LX;
   end Swap24;

   procedure Swap32 (X : in out UInt32)
   is
      LX : UInt32 := X;
      B : UInt8_Array (1 .. 4);
      for LX'Address use B'Address;
      for LX'Alignment use 1;
   begin
      B := (B (4), B (3), B (2), B (1));
      X := LX;
   end Swap32;

   procedure LoRa_Set_PacketParams (Params : LoRa_Set_PacketParams_Message)
   is
      Msg        : SPI_Data_8b (1 .. Params'Size / 8);
      LParams    : LoRa_Set_PacketParams_Message := Params;
      for Msg'Address use LParams'Address;
      Status     : SPI_Status;
   begin
      Swap16 (LParams.PreambleLength);
      LParams.Opcode := Opcode_Set_PacketParams;
      CheckDeviceReady;
      NSS_Assert;
      Transmit (SubGhzPhyPort.all, Msg, Status);
      NSS_Deassert;
   end LoRa_Set_PacketParams;

   procedure Set_StopRxTimerOnPreamble (Choice : Set_StopRxTimerOnPreamble_Selection)
   is
      Msg    : SPI_Data_8b (1 .. 2);
      Status : SPI_Status;
   begin
      Msg (1) := Opcode_Set_StopRxTimerOnPreamble;
      Msg (2) := Choice'Enum_Rep;
      CheckDeviceReady;
      NSS_Assert;
      Transmit (SubGhzPhyPort.all, Msg, Status);
      NSS_Deassert;
   end Set_StopRxTimerOnPreamble;

   procedure Set_LoRaSymbTimeout (NumberOfSymbols : UInt8)
   is
      Msg    : SPI_Data_8b (1 .. 2);
      Status : SPI_Status;
   begin
      Msg (1) := Opcode_Set_LoRaSymbTimeout;
      Msg (2) := NumberOfSymbols;
      CheckDeviceReady;
      NSS_Assert;
      Transmit (SubGhzPhyPort.all, Msg, Status);
      NSS_Deassert;
   end Set_LoRaSymbTimeout;

   procedure Set_RfFrequency (Freq : Set_RfFrequency_Message)
   is
      Msg    : SPI_Data_8b (1 .. Freq'Size / 8);
      Tmp    : SPI_Data_8b (1 .. 4);
      Status : SPI_Status;
      LF     : aliased Frequency := Freq.F;
      for LF'Address use Tmp (1)'Address;
      for LF'Alignment use 1;
   begin
      Msg (1) := Opcode_Set_RfFrequency;
      --  Trouble swapping this one... (tried in the record type)
      Msg := (Opcode_Set_RfFrequency, Tmp (4), Tmp (3), Tmp (2), Tmp (1));
      CheckDeviceReady;
      NSS_Assert;
      Transmit (SubGhzPhyPort.all, Msg, Status);
      NSS_Deassert;
   end Set_RfFrequency;

   procedure Set_Rx (Timeout : UInt32)
   is
      Msg      : SPI_Data_8b (1 .. 5);
      Tmp      : SPI_Data_8b (1 .. 4);
      Status   : SPI_Status;
      LTimeout : aliased UInt32 := Timeout;
      for LTimeout'Address use Tmp (1)'Address;
      for LTimeout'Alignment use 1;
   begin
      Msg (1) := Opcode_Set_Rx;
      --  Trouble swapping this one... (tried in the record type)
      Msg := (Opcode_Set_Rx, Tmp (4), Tmp (3), Tmp (2), Tmp (1));
      CheckDeviceReady;
      NSS_Assert;
      Transmit (SubGhzPhyPort.all, Msg, Status);
      NSS_Deassert;
   end Set_Rx;

   procedure Set_Tx (Timeout : UInt32)
   is
      Msg      : SPI_Data_8b (1 .. 5);
      Tmp      : SPI_Data_8b (1 .. 4);
      Status   : SPI_Status;
      LTimeout : aliased UInt32 := Timeout;
      for LTimeout'Address use Tmp (1)'Address;
      for LTimeout'Alignment use 1;
   begin
      Msg (1) := Opcode_Set_Tx;
      --  Trouble swapping this one... (tried in the record type)
      Msg := (Opcode_Set_Tx, Tmp (4), Tmp (3), Tmp (2), Tmp (1));
      CheckDeviceReady;
      NSS_Assert;
      Transmit (SubGhzPhyPort.all, Msg, Status);
      NSS_Deassert;
   end Set_Tx;

   function Get_IrqStatus (RFStatus  : out Subghz_Status) return UInt16
   is
      Msg    : SPI_Data_8b (1 .. 1);
      Reply  : SPI_Data_8b (1 .. 3);
      Status : SPI_Status;
   begin
      Msg (1) := Opcode_Get_IrqStatus;
      NSS_Assert;
      Transmit (SubGhzPhyPort.all, Msg, Status);
      Receive (SubGhzPhyPort.all, Reply, Status);
      NSS_Deassert;
      RFStatus := Reply (1);
      return Shift_Left (UInt16 (Reply (2)), 8) or UInt16 (Reply (3));
   end Get_IrqStatus;

   procedure Clr_IrqStatus (IrqStatus : UInt16)
   is
      Status : SPI_Status;
      Msg    : SPI_Data_8b (1 .. 3);
      LStat  : UInt16 := IrqStatus;
      for LStat'Address use Msg (2)'Address;
      for LStat'Alignment use 1;
      Tmp    : UInt8;
   begin
      Msg (1) := Opcode_Clr_IrqStatus;
      Tmp     := Msg (2);
      Msg (2) := Msg (3);
      Msg (3) := Tmp;
      NSS_Assert;
      Transmit (SubGhzPhyPort.all, Msg, Status);
      NSS_Deassert;
   end Clr_IrqStatus;

   procedure Get_RxBufferStatus (RFStatus             : out Subghz_Status;
                                 RxPayloadLength      : out UInt8;
                                 RxStartBufferPointer : out UInt8)
   is
      Msg    : SPI_Data_8b (1 .. 1);
      Reply  : SPI_Data_8b (1 .. 3);
      Status : SPI_Status;
   begin
      Msg (1) := Opcode_Get_RxBufferStatus;
      NSS_Assert;
      Transmit (SubGhzPhyPort.all, Msg, Status);
      Receive (SubGhzPhyPort.all, Reply, Status);
      NSS_Deassert;
      RFStatus             := Reply (1);
      RxPayloadLength      := Reply (2);
      RxStartBufferPointer := Reply (3);
   end Get_RxBufferStatus;

   procedure Read_Buffer (Offset   : UInt8;
                          RFStatus : out Subghz_Status;
                          Buffer   : out SPI_Data_8b)
   is
      Msg    : SPI_Data_8b (1 .. 2);
      Reply  : SPI_Data_8b (1 .. 1);
      Status : SPI_Status;
   begin
      Msg (1) := Opcode_Read_Buffer;
      Msg (2) := Offset;
      NSS_Assert;
      Transmit (SubGhzPhyPort.all, Msg, Status);
      Receive (SubGhzPhyPort.all, Reply, Status);
      Receive (SubGhzPhyPort.all, Buffer, Status);
      NSS_Deassert;
      RFStatus := Reply (1);
   end Read_Buffer;

   procedure LoRa_Get_Stats (RFStatus         : out Subghz_Status;
                             NbPktReceived    : out UInt16;
                             NbPktCrcError    : out UInt16;
                             NbPktLengthError : out UInt16)
   is
      Msg    : SPI_Data_8b (1 .. 1);
      Reply  : SPI_Data_8b (1 .. 1 + (3 * 2));
      Status : SPI_Status;
   begin
      Msg (1) := Opcode_Get_Stats;
      NSS_Assert;
      Transmit (SubGhzPhyPort.all, Msg, Status);
      Receive (SubGhzPhyPort.all, Reply, Status);
      NSS_Deassert;
      RFStatus         := Reply (1);
      NbPktReceived    := Shift_Left (UInt16 (Reply (2)), 8) or UInt16 (Reply (3));
      NbPktCrcError    := Shift_Left (UInt16 (Reply (4)), 8) or UInt16 (Reply (5));
      NbPktLengthError := Shift_Left (UInt16 (Reply (6)), 8) or UInt16 (Reply (7));
   end LoRa_Get_Stats;

   procedure Reset_Stats
   is
      Msg    : SPI_Data_8b (1 .. 7);
      Status : SPI_Status;
   begin
      Msg := (Opcode_Reset_Stats, others => 0);
      NSS_Assert;
      Transmit (SubGhzPhyPort.all, Msg, Status);
      NSS_Deassert;
   end Reset_Stats;

   procedure Toggle_TxRx
   is
   begin
--      Set_RfFrequency ((F => 915.0E6, others => <>));
      Set_Tx (0);
--      Set_RfFrequency ((F => 915.0E6, others => <>));
      Set_Rx (0);
   end Toggle_TxRx;

   procedure SubGhzRF_Init
   is
      RFStat : Subghz_Status;
      X      : UInt8;
      Irq    : UInt16;
   begin
      Set_Standby (RC_13_MHz);
      Set_TcxoMode ((Trim    => V_1_7,
                     Timeout => 640, --  10ms (X * 15.625us)
                     others  => <>));
      Write_Register ((Address => 16#0911#,
                       Value   => 0,
                       others  => <>));
      Calibrate ((ImageCalibration => Enabled,
                  RF_ADC_BulkP_Cal => Enabled,
                  RF_ADC_BulkN_Cal => Enabled,
                  RF_ADC_Pulse_Cal => Enabled,
                  RF_PLL_Cal       => Enabled,
                  Sub_GHz_RC13Mhz  => Enabled,
                  Sub_GHz_RC64Khz  => Enabled,
                  others  => <>));
      Set_PacketType ((PacketType => LoRa_Packet, others => <>));
      Write_Register ((Address => 16#0740#, Value => 20, others => <>));
      Write_Register ((Address => 16#0741#, Value => 36, others => <>));
      Set_RegulatorMode (LDO);
      Set_BufferBaseAddress ((Tx_BaseAddress => 0, Rx_BaseAddress => 0, others => <>));
--      Read_Register ((Address => 16#06b8#, others => <>), RFStatus => RFStat, Value => GX);
      Set_PaConfig ((
                     PaDutyCycle => Pa_DC_4,
                     HpMax       => HpMax_0,
                     PaSel       => LP_PA,
                     others => <>));
      Write_Register ((Address => 16#08e7#, Value => 24, others => <>));
      Set_TxParams ((Power => 0,
                     RampTime => Microsecs_200,
                     others => <>));
      Cfg_DioIrq ((Irq_Mask => 16#ffff#,
                   Irq_Mask1 => 16#ffff#,
                   Irq_Mask2 => 0,
                   Irq_Mask3 => 0,
                   others => <>));
      Set_Sleep ((StartSel => Warm_Startup,
                  SleepCfg => Disabled,
                  others => <>));

--      loop
--         Irq := Get_IrqStatus (RFStat);
--         exit when Irq /= 0;
--      end loop;
--      Clr_IrqStatus (Irq);

      delay until Clock + Milliseconds (100);

      Set_Standby (RC_13_MHz);
      Set_PacketType ((PacketType => LoRa_Packet, others => <>));
      LoRa_Set_ModulationParams ((SpreadFactor      => Factor_7,
                                  BandWidth         => BandWidth_125Khz,
                                  CodingRate        => CR_FEC_4_5,
                                  Low_Data_Rate_Opt => Disabled,
                                  others => <>
                                 ));
      LoRa_Set_PacketParams ((
                         PreambleLength  => 8,
                         HeaderType      => Explicit,
                         PayloadLength   => 16#ff#,
                         Crc             => Enabled,
                         InvertIQ        => Standard,
                         others => <>
                        ));
--      Read_Register ((Address => 16#06b8#, others => <>), RFStatus => RFStat, Value => GX);
      Read_Register ((Address => 16#0889#, others => <>), RFStatus => RFStat, Value => X); --  [37]
      Write_Register ((Address => 16#0889#, Value => X, others => <>)); --  Which bit?
      Set_PaConfig ((
                     PaDutyCycle => Pa_DC_4,
                     HpMax       => HpMax_0,
                     PaSel       => LP_PA,
                     others => <>));
      Write_Register ((Address => 16#08e7#, Value => 24, others => <>)); --  SX1261: 0x18 (60 mA)
      Set_TxParams ((Power => 14,
                     RampTime => Microsecs_40,
                     others => <>));
      Set_StopRxTimerOnPreamble (Stop_On_Sync);
      Set_Standby (RC_13_MHz);
      Set_PacketType ((PacketType => LoRa_Packet, others => <>));
      LoRa_Set_ModulationParams ((SpreadFactor      => Factor_7,
                                  BandWidth         => BandWidth_125Khz,
                                  CodingRate        => CR_FEC_4_5,
                                  Low_Data_Rate_Opt => Disabled,
                                  others => <>
                                 ));
      LoRa_Set_PacketParams ((
                         PreambleLength  => 8,
                         HeaderType      => Explicit,
                         PayloadLength   => 16#ff#,
                         Crc             => Enabled,
                         InvertIQ        => Standard,
                         others => <>
                        ));

      Set_LoRaSymbTimeout (NumberOfSymbols => 0);
      Read_Register ((Address => 16#0736#, others => <>), RFStatus => RFStat, Value => X); --  [13]
      Write_Register ((Address => 16#0736#, Value => X, others => <>)); -- what bit
      LoRa_Set_PacketParams ((
                         PreambleLength  => 8,
                         HeaderType      => Explicit,
                         PayloadLength   => 16#40#,
                         Crc             => Enabled,
                         InvertIQ        => Standard,
                         others => <>
                        ));
--      CalibrateImage; Not needed for 915
      Set_RfFrequency ((F => 915.0E6, others => <>));
      Cfg_DioIrq ((Irq_Mask => 16#ffff#,
                   Irq_Mask1 => 16#ffff#,
                   Irq_Mask2 => 0,
                   Irq_Mask3 => 0,
                   others => <>));
      Reset_Stats;
      Set_Rx (Timeout => 0);
--      Set_Sleep;
--      Set_RfFrequency;
--      Cfg_DioIrq;
   end SubGhzRF_Init;

end STM32.SubGhzRF;
