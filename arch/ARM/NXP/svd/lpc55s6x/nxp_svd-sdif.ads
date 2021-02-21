--  Copyright 2016-2019 NXP
--  All rights reserved.SPDX-License-Identifier: BSD-3-Clause


--  This spec has been automatically generated from LPC55S6x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NXP_SVD.SDIF is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  CTRL_CARD_VOLTAGE_A array
   type CTRL_CARD_VOLTAGE_A_Field_Array is array (0 .. 2) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for CTRL_CARD_VOLTAGE_A
   type CTRL_CARD_VOLTAGE_A_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CARD_VOLTAGE_A as a value
            Val : HAL.UInt3;
         when True =>
            --  CARD_VOLTAGE_A as an array
            Arr : CTRL_CARD_VOLTAGE_A_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for CTRL_CARD_VOLTAGE_A_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  Control register
   type CTRL_Register is record
      --  Controller reset.
      CONTROLLER_RESET              : Boolean := False;
      --  Fifo reset.
      FIFO_RESET                    : Boolean := False;
      --  DMA reset.
      DMA_RESET                     : Boolean := False;
      --  unspecified
      Reserved_3_3                  : HAL.Bit := 16#0#;
      --  Global interrupt enable/disable bit.
      INT_ENABLE                    : Boolean := False;
      --  unspecified
      Reserved_5_5                  : HAL.Bit := 16#0#;
      --  Read/wait.
      READ_WAIT                     : Boolean := False;
      --  Send irq response.
      SEND_IRQ_RESPONSE             : Boolean := False;
      --  Abort read data.
      ABORT_READ_DATA               : Boolean := False;
      --  Send ccsd.
      SEND_CCSD                     : Boolean := False;
      --  Send auto stop ccsd.
      SEND_AUTO_STOP_CCSD           : Boolean := False;
      --  CEATA device interrupt status.
      CEATA_DEVICE_INTERRUPT_STATUS : Boolean := False;
      --  unspecified
      Reserved_12_15                : HAL.UInt4 := 16#0#;
      --  Controls the state of the SD_VOLT0 pin.
      CARD_VOLTAGE_A                : CTRL_CARD_VOLTAGE_A_Field :=
                                       (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_19_24                : HAL.UInt6 := 16#0#;
      --  SD/MMC DMA use.
      USE_INTERNAL_DMAC             : Boolean := False;
      --  unspecified
      Reserved_26_31                : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CTRL_Register use record
      CONTROLLER_RESET              at 0 range 0 .. 0;
      FIFO_RESET                    at 0 range 1 .. 1;
      DMA_RESET                     at 0 range 2 .. 2;
      Reserved_3_3                  at 0 range 3 .. 3;
      INT_ENABLE                    at 0 range 4 .. 4;
      Reserved_5_5                  at 0 range 5 .. 5;
      READ_WAIT                     at 0 range 6 .. 6;
      SEND_IRQ_RESPONSE             at 0 range 7 .. 7;
      ABORT_READ_DATA               at 0 range 8 .. 8;
      SEND_CCSD                     at 0 range 9 .. 9;
      SEND_AUTO_STOP_CCSD           at 0 range 10 .. 10;
      CEATA_DEVICE_INTERRUPT_STATUS at 0 range 11 .. 11;
      Reserved_12_15                at 0 range 12 .. 15;
      CARD_VOLTAGE_A                at 0 range 16 .. 18;
      Reserved_19_24                at 0 range 19 .. 24;
      USE_INTERNAL_DMAC             at 0 range 25 .. 25;
      Reserved_26_31                at 0 range 26 .. 31;
   end record;

   --  PWREN_POWER_ENABLE array
   type PWREN_POWER_ENABLE_Field_Array is array (0 .. 1) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for PWREN_POWER_ENABLE
   type PWREN_POWER_ENABLE_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  POWER_ENABLE as a value
            Val : HAL.UInt2;
         when True =>
            --  POWER_ENABLE as an array
            Arr : PWREN_POWER_ENABLE_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for PWREN_POWER_ENABLE_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Power Enable register
   type PWREN_Register is record
      --  Power on/off switch for card 0; once power is turned on, software
      --  should wait for regulator/switch ramp-up time before trying to
      --  initialize card 0.
      POWER_ENABLE  : PWREN_POWER_ENABLE_Field :=
                       (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWREN_Register use record
      POWER_ENABLE  at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   subtype CLKDIV_CLK_DIVIDER0_Field is HAL.UInt8;

   --  Clock Divider register
   type CLKDIV_Register is record
      --  Clock divider-0 value.
      CLK_DIVIDER0  : CLKDIV_CLK_DIVIDER0_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CLKDIV_Register use record
      CLK_DIVIDER0  at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  Clock Enable register
   type CLKENA_Register is record
      --  Clock-enable control for SD card 0 clock.
      CCLK0_ENABLE    : Boolean := False;
      --  Clock-enable control for SD card 1 clock.
      CCLK1_ENABLE    : Boolean := False;
      --  unspecified
      Reserved_2_15   : HAL.UInt14 := 16#0#;
      --  Low-power control for SD card 0 clock.
      CCLK0_LOW_POWER : Boolean := False;
      --  Low-power control for SD card 1 clock.
      CCLK1_LOW_POWER : Boolean := False;
      --  unspecified
      Reserved_18_31  : HAL.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CLKENA_Register use record
      CCLK0_ENABLE    at 0 range 0 .. 0;
      CCLK1_ENABLE    at 0 range 1 .. 1;
      Reserved_2_15   at 0 range 2 .. 15;
      CCLK0_LOW_POWER at 0 range 16 .. 16;
      CCLK1_LOW_POWER at 0 range 17 .. 17;
      Reserved_18_31  at 0 range 18 .. 31;
   end record;

   subtype TMOUT_RESPONSE_TIMEOUT_Field is HAL.UInt8;
   subtype TMOUT_DATA_TIMEOUT_Field is HAL.UInt24;

   --  Time-out register
   type TMOUT_Register is record
      --  Response time-out value.
      RESPONSE_TIMEOUT : TMOUT_RESPONSE_TIMEOUT_Field := 16#40#;
      --  Value for card Data Read time-out; same value also used for Data
      --  Starvation by Host time-out.
      DATA_TIMEOUT     : TMOUT_DATA_TIMEOUT_Field := 16#FFFFFF#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TMOUT_Register use record
      RESPONSE_TIMEOUT at 0 range 0 .. 7;
      DATA_TIMEOUT     at 0 range 8 .. 31;
   end record;

   --  Card Type register
   type CTYPE_Register is record
      --  Indicates if card 0 is 1-bit or 4-bit: 0 - 1-bit mode 1 - 4-bit mode
      --  1 and 4-bit modes only work when 8-bit mode in CARD0_WIDTH1 is not
      --  enabled (bit 16 in this register is set to 0).
      CARD0_WIDTH0   : Boolean := False;
      --  Indicates if card 1 is 1-bit or 4-bit: 0 - 1-bit mode 1 - 4-bit mode
      --  1 and 4-bit modes only work when 8-bit mode in CARD1_WIDTH1 is not
      --  enabled (bit 16 in this register is set to 0).
      CARD1_WIDTH0   : Boolean := False;
      --  unspecified
      Reserved_2_15  : HAL.UInt14 := 16#0#;
      --  Indicates if card 0 is 8-bit: 0 - Non 8-bit mode 1 - 8-bit mode.
      CARD0_WIDTH1   : Boolean := False;
      --  Indicates if card 1 is 8-bit: 0 - Non 8-bit mode 1 - 8-bit mode.
      CARD1_WIDTH1   : Boolean := False;
      --  unspecified
      Reserved_18_31 : HAL.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CTYPE_Register use record
      CARD0_WIDTH0   at 0 range 0 .. 0;
      CARD1_WIDTH0   at 0 range 1 .. 1;
      Reserved_2_15  at 0 range 2 .. 15;
      CARD0_WIDTH1   at 0 range 16 .. 16;
      CARD1_WIDTH1   at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   subtype BLKSIZ_BLOCK_SIZE_Field is HAL.UInt16;

   --  Block Size register
   type BLKSIZ_Register is record
      --  Block size.
      BLOCK_SIZE     : BLKSIZ_BLOCK_SIZE_Field := 16#200#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for BLKSIZ_Register use record
      BLOCK_SIZE     at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  Interrupt Mask register
   type INTMASK_Register is record
      --  Card detect.
      CDET           : Boolean := False;
      --  Response error.
      RE             : Boolean := False;
      --  Command done.
      CDONE          : Boolean := False;
      --  Data transfer over.
      DTO            : Boolean := False;
      --  Transmit FIFO data request.
      TXDR           : Boolean := False;
      --  Receive FIFO data request.
      RXDR           : Boolean := False;
      --  Response CRC error.
      RCRC           : Boolean := False;
      --  Data CRC error.
      DCRC           : Boolean := False;
      --  Response time-out.
      RTO            : Boolean := False;
      --  Data read time-out.
      DRTO           : Boolean := False;
      --  Data starvation-by-host time-out (HTO).
      HTO            : Boolean := False;
      --  FIFO underrun/overrun error.
      FRUN           : Boolean := False;
      --  Hardware locked write error.
      HLE            : Boolean := False;
      --  Start-bit error.
      SBE            : Boolean := False;
      --  Auto command done.
      ACD            : Boolean := False;
      --  End-bit error (read)/Write no CRC.
      EBE            : Boolean := False;
      --  Mask SDIO interrupt.
      SDIO_INT_MASK  : Boolean := False;
      --  unspecified
      Reserved_17_31 : HAL.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTMASK_Register use record
      CDET           at 0 range 0 .. 0;
      RE             at 0 range 1 .. 1;
      CDONE          at 0 range 2 .. 2;
      DTO            at 0 range 3 .. 3;
      TXDR           at 0 range 4 .. 4;
      RXDR           at 0 range 5 .. 5;
      RCRC           at 0 range 6 .. 6;
      DCRC           at 0 range 7 .. 7;
      RTO            at 0 range 8 .. 8;
      DRTO           at 0 range 9 .. 9;
      HTO            at 0 range 10 .. 10;
      FRUN           at 0 range 11 .. 11;
      HLE            at 0 range 12 .. 12;
      SBE            at 0 range 13 .. 13;
      ACD            at 0 range 14 .. 14;
      EBE            at 0 range 15 .. 15;
      SDIO_INT_MASK  at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype CMD_CMD_INDEX_Field is HAL.UInt6;

   --  Specifies the card number of SDCARD for which the current Command is
   --  being executed
   type CMD_CARD_NUMBER_Field is
     (
      --  Command will be execute on SDCARD 0
      Card0,
      --  Command will be execute on SDCARD 1
      Card1)
     with Size => 5;
   for CMD_CARD_NUMBER_Field use
     (Card0 => 0,
      Card1 => 1);

   --  Command register
   type CMD_Register is record
      --  Command index.
      CMD_INDEX                   : CMD_CMD_INDEX_Field := 16#0#;
      --  Response expect.
      RESPONSE_EXPECT             : Boolean := False;
      --  Response length.
      RESPONSE_LENGTH             : Boolean := False;
      --  Check response CRC.
      CHECK_RESPONSE_CRC          : Boolean := False;
      --  Data expected.
      DATA_EXPECTED               : Boolean := False;
      --  read/write.
      READ_WRITE                  : Boolean := False;
      --  Transfer mode.
      TRANSFER_MODE               : Boolean := False;
      --  Send auto stop.
      SEND_AUTO_STOP              : Boolean := False;
      --  Wait prvdata complete.
      WAIT_PRVDATA_COMPLETE       : Boolean := False;
      --  Stop abort command.
      STOP_ABORT_CMD              : Boolean := False;
      --  Send initialization.
      SEND_INITIALIZATION         : Boolean := False;
      --  Specifies the card number of SDCARD for which the current Command is
      --  being executed
      CARD_NUMBER                 : CMD_CARD_NUMBER_Field :=
                                     NXP_SVD.SDIF.Card0;
      --  Update clock registers only.
      UPDATE_CLOCK_REGISTERS_ONLY : Boolean := False;
      --  Read ceata device.
      READ_CEATA_DEVICE           : Boolean := False;
      --  CCS expected.
      CCS_EXPECTED                : Boolean := False;
      --  Enable Boot - this bit should be set only for mandatory boot mode.
      ENABLE_BOOT                 : Boolean := False;
      --  Expect Boot Acknowledge.
      EXPECT_BOOT_ACK             : Boolean := False;
      --  Disable Boot.
      DISABLE_BOOT                : Boolean := False;
      --  Boot Mode.
      BOOT_MODE                   : Boolean := False;
      --  Voltage switch bit.
      VOLT_SWITCH                 : Boolean := False;
      --  Use Hold Register.
      USE_HOLD_REG                : Boolean := False;
      --  unspecified
      Reserved_30_30              : HAL.Bit := 16#0#;
      --  Start command.
      START_CMD                   : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CMD_Register use record
      CMD_INDEX                   at 0 range 0 .. 5;
      RESPONSE_EXPECT             at 0 range 6 .. 6;
      RESPONSE_LENGTH             at 0 range 7 .. 7;
      CHECK_RESPONSE_CRC          at 0 range 8 .. 8;
      DATA_EXPECTED               at 0 range 9 .. 9;
      READ_WRITE                  at 0 range 10 .. 10;
      TRANSFER_MODE               at 0 range 11 .. 11;
      SEND_AUTO_STOP              at 0 range 12 .. 12;
      WAIT_PRVDATA_COMPLETE       at 0 range 13 .. 13;
      STOP_ABORT_CMD              at 0 range 14 .. 14;
      SEND_INITIALIZATION         at 0 range 15 .. 15;
      CARD_NUMBER                 at 0 range 16 .. 20;
      UPDATE_CLOCK_REGISTERS_ONLY at 0 range 21 .. 21;
      READ_CEATA_DEVICE           at 0 range 22 .. 22;
      CCS_EXPECTED                at 0 range 23 .. 23;
      ENABLE_BOOT                 at 0 range 24 .. 24;
      EXPECT_BOOT_ACK             at 0 range 25 .. 25;
      DISABLE_BOOT                at 0 range 26 .. 26;
      BOOT_MODE                   at 0 range 27 .. 27;
      VOLT_SWITCH                 at 0 range 28 .. 28;
      USE_HOLD_REG                at 0 range 29 .. 29;
      Reserved_30_30              at 0 range 30 .. 30;
      START_CMD                   at 0 range 31 .. 31;
   end record;

   --  Response register

   --  Response register
   type RESP_Registers is array (0 .. 3) of HAL.UInt32
     with Volatile;

   --  Masked Interrupt Status register
   type MINTSTS_Register is record
      --  Card detect.
      CDET           : Boolean := False;
      --  Response error.
      RE             : Boolean := False;
      --  Command done.
      CDONE          : Boolean := False;
      --  Data transfer over.
      DTO            : Boolean := False;
      --  Transmit FIFO data request.
      TXDR           : Boolean := False;
      --  Receive FIFO data request.
      RXDR           : Boolean := False;
      --  Response CRC error.
      RCRC           : Boolean := False;
      --  Data CRC error.
      DCRC           : Boolean := False;
      --  Response time-out.
      RTO            : Boolean := False;
      --  Data read time-out.
      DRTO           : Boolean := False;
      --  Data starvation-by-host time-out (HTO).
      HTO            : Boolean := False;
      --  FIFO underrun/overrun error.
      FRUN           : Boolean := False;
      --  Hardware locked write error.
      HLE            : Boolean := False;
      --  Start-bit error.
      SBE            : Boolean := False;
      --  Auto command done.
      ACD            : Boolean := False;
      --  End-bit error (read)/write no CRC.
      EBE            : Boolean := False;
      --  Interrupt from SDIO card.
      SDIO_INTERRUPT : Boolean := False;
      --  unspecified
      Reserved_17_31 : HAL.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MINTSTS_Register use record
      CDET           at 0 range 0 .. 0;
      RE             at 0 range 1 .. 1;
      CDONE          at 0 range 2 .. 2;
      DTO            at 0 range 3 .. 3;
      TXDR           at 0 range 4 .. 4;
      RXDR           at 0 range 5 .. 5;
      RCRC           at 0 range 6 .. 6;
      DCRC           at 0 range 7 .. 7;
      RTO            at 0 range 8 .. 8;
      DRTO           at 0 range 9 .. 9;
      HTO            at 0 range 10 .. 10;
      FRUN           at 0 range 11 .. 11;
      HLE            at 0 range 12 .. 12;
      SBE            at 0 range 13 .. 13;
      ACD            at 0 range 14 .. 14;
      EBE            at 0 range 15 .. 15;
      SDIO_INTERRUPT at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   --  Raw Interrupt Status register
   type RINTSTS_Register is record
      --  Card detect.
      CDET           : Boolean := False;
      --  Response error.
      RE             : Boolean := False;
      --  Command done.
      CDONE          : Boolean := False;
      --  Data transfer over.
      DTO            : Boolean := False;
      --  Transmit FIFO data request.
      TXDR           : Boolean := False;
      --  Receive FIFO data request.
      RXDR           : Boolean := False;
      --  Response CRC error.
      RCRC           : Boolean := False;
      --  Data CRC error.
      DCRC           : Boolean := False;
      --  Response time-out (RTO)/Boot Ack Received (BAR).
      RTO_BAR        : Boolean := False;
      --  Data read time-out (DRTO)/Boot Data Start (BDS).
      DRTO_BDS       : Boolean := False;
      --  Data starvation-by-host time-out (HTO).
      HTO            : Boolean := False;
      --  FIFO underrun/overrun error.
      FRUN           : Boolean := False;
      --  Hardware locked write error.
      HLE            : Boolean := False;
      --  Start-bit error.
      SBE            : Boolean := False;
      --  Auto command done.
      ACD            : Boolean := False;
      --  End-bit error (read)/write no CRC.
      EBE            : Boolean := False;
      --  Interrupt from SDIO card.
      SDIO_INTERRUPT : Boolean := False;
      --  unspecified
      Reserved_17_31 : HAL.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RINTSTS_Register use record
      CDET           at 0 range 0 .. 0;
      RE             at 0 range 1 .. 1;
      CDONE          at 0 range 2 .. 2;
      DTO            at 0 range 3 .. 3;
      TXDR           at 0 range 4 .. 4;
      RXDR           at 0 range 5 .. 5;
      RCRC           at 0 range 6 .. 6;
      DCRC           at 0 range 7 .. 7;
      RTO_BAR        at 0 range 8 .. 8;
      DRTO_BDS       at 0 range 9 .. 9;
      HTO            at 0 range 10 .. 10;
      FRUN           at 0 range 11 .. 11;
      HLE            at 0 range 12 .. 12;
      SBE            at 0 range 13 .. 13;
      ACD            at 0 range 14 .. 14;
      EBE            at 0 range 15 .. 15;
      SDIO_INTERRUPT at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype STATUS_CMDFSMSTATES_Field is HAL.UInt4;
   subtype STATUS_RESPONSE_INDEX_Field is HAL.UInt6;
   subtype STATUS_FIFO_COUNT_Field is HAL.UInt13;

   --  Status register
   type STATUS_Register is record
      --  FIFO reached Receive watermark level; not qualified with data
      --  transfer.
      FIFO_RX_WATERMARK  : Boolean := False;
      --  FIFO reached Transmit watermark level; not qualified with data
      --  transfer.
      FIFO_TX_WATERMARK  : Boolean := True;
      --  FIFO is empty status.
      FIFO_EMPTY         : Boolean := True;
      --  FIFO is full status.
      FIFO_FULL          : Boolean := False;
      --  Command FSM states: 0 - Idle 1 - Send init sequence 2 - Tx cmd start
      --  bit 3 - Tx cmd tx bit 4 - Tx cmd index + arg 5 - Tx cmd crc7 6 - Tx
      --  cmd end bit 7 - Rx resp start bit 8 - Rx resp IRQ response 9 - Rx
      --  resp tx bit 10 - Rx resp cmd idx 11 - Rx resp data 12 - Rx resp crc7
      --  13 - Rx resp end bit 14 - Cmd path wait NCC 15 - Wait;
      --  CMD-to-response turnaround NOTE: The command FSM state is represented
      --  using 19 bits.
      CMDFSMSTATES       : STATUS_CMDFSMSTATES_Field := 16#0#;
      --  Raw selected card_data[3]; checks whether card is present 0 - card
      --  not present 1 - card present.
      DATA_3_STATUS      : Boolean := False;
      --  Inverted version of raw selected card_data[0] 0 - card data not busy
      --  1 - card data busy.
      DATA_BUSY          : Boolean := False;
      --  Data transmit or receive state-machine is busy.
      DATA_STATE_MC_BUSY : Boolean := True;
      --  Index of previous response, including any auto-stop sent by core.
      RESPONSE_INDEX     : STATUS_RESPONSE_INDEX_Field := 16#0#;
      --  FIFO count - Number of filled locations in FIFO.
      FIFO_COUNT         : STATUS_FIFO_COUNT_Field := 16#0#;
      --  DMA acknowledge signal state.
      DMA_ACK            : Boolean := False;
      --  DMA request signal state.
      DMA_REQ            : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for STATUS_Register use record
      FIFO_RX_WATERMARK  at 0 range 0 .. 0;
      FIFO_TX_WATERMARK  at 0 range 1 .. 1;
      FIFO_EMPTY         at 0 range 2 .. 2;
      FIFO_FULL          at 0 range 3 .. 3;
      CMDFSMSTATES       at 0 range 4 .. 7;
      DATA_3_STATUS      at 0 range 8 .. 8;
      DATA_BUSY          at 0 range 9 .. 9;
      DATA_STATE_MC_BUSY at 0 range 10 .. 10;
      RESPONSE_INDEX     at 0 range 11 .. 16;
      FIFO_COUNT         at 0 range 17 .. 29;
      DMA_ACK            at 0 range 30 .. 30;
      DMA_REQ            at 0 range 31 .. 31;
   end record;

   subtype FIFOTH_TX_WMARK_Field is HAL.UInt12;
   subtype FIFOTH_RX_WMARK_Field is HAL.UInt12;
   subtype FIFOTH_DMA_MTS_Field is HAL.UInt3;

   --  FIFO Threshold Watermark register
   type FIFOTH_Register is record
      --  FIFO threshold watermark level when transmitting data to card.
      TX_WMARK       : FIFOTH_TX_WMARK_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  FIFO threshold watermark level when receiving data to card.
      RX_WMARK       : FIFOTH_RX_WMARK_Field := 16#1F#;
      --  Burst size of multiple transaction; should be programmed same as
      --  DW-DMA controller multiple-transaction-size SRC/DEST_MSIZE.
      DMA_MTS        : FIFOTH_DMA_MTS_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FIFOTH_Register use record
      TX_WMARK       at 0 range 0 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      RX_WMARK       at 0 range 16 .. 27;
      DMA_MTS        at 0 range 28 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   --  Card Detect register
   type CDETECT_Register is record
      --  Card 0 detect
      CARD0_DETECT  : Boolean := False;
      --  Card 1 detect
      CARD1_DETECT  : Boolean := False;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CDETECT_Register use record
      CARD0_DETECT  at 0 range 0 .. 0;
      CARD1_DETECT  at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  Write Protect register
   type WRTPRT_Register is record
      --  Write protect.
      WRITE_PROTECT : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for WRTPRT_Register use record
      WRITE_PROTECT at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   subtype DEBNCE_DEBOUNCE_COUNT_Field is HAL.UInt24;

   --  Debounce Count register
   type DEBNCE_Register is record
      --  Number of host clocks (SD_CLK) used by debounce filter logic for card
      --  detect; typical debounce time is 5-25 ms.
      DEBOUNCE_COUNT : DEBNCE_DEBOUNCE_COUNT_Field := 16#FFFFFF#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DEBNCE_Register use record
      DEBOUNCE_COUNT at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  Hardware Reset
   type RST_N_Register is record
      --  Hardware reset.
      CARD_RESET    : Boolean := True;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RST_N_Register use record
      CARD_RESET    at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   subtype BMOD_DSL_Field is HAL.UInt5;
   subtype BMOD_PBL_Field is HAL.UInt3;

   --  Bus Mode register
   type BMOD_Register is record
      --  Software Reset.
      SWR            : Boolean := False;
      --  Fixed Burst.
      FB             : Boolean := False;
      --  Descriptor Skip Length.
      DSL            : BMOD_DSL_Field := 16#0#;
      --  SD/MMC DMA Enable.
      DE             : Boolean := False;
      --  Programmable Burst Length.
      PBL            : BMOD_PBL_Field := 16#0#;
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for BMOD_Register use record
      SWR            at 0 range 0 .. 0;
      FB             at 0 range 1 .. 1;
      DSL            at 0 range 2 .. 6;
      DE             at 0 range 7 .. 7;
      PBL            at 0 range 8 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   subtype IDSTS_EB_Field is HAL.UInt3;
   subtype IDSTS_FSM_Field is HAL.UInt4;

   --  Internal DMAC Status register
   type IDSTS_Register is record
      --  Transmit Interrupt.
      TI             : Boolean := False;
      --  Receive Interrupt.
      RI             : Boolean := False;
      --  Fatal Bus Error Interrupt.
      FBE            : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Descriptor Unavailable Interrupt.
      DU             : Boolean := False;
      --  Card Error Summary.
      CES            : Boolean := False;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Normal Interrupt Summary.
      NIS            : Boolean := False;
      --  Abnormal Interrupt Summary.
      AIS            : Boolean := False;
      --  Error Bits.
      EB             : IDSTS_EB_Field := 16#0#;
      --  DMAC state machine present state.
      FSM            : IDSTS_FSM_Field := 16#0#;
      --  unspecified
      Reserved_17_31 : HAL.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IDSTS_Register use record
      TI             at 0 range 0 .. 0;
      RI             at 0 range 1 .. 1;
      FBE            at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      DU             at 0 range 4 .. 4;
      CES            at 0 range 5 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      NIS            at 0 range 8 .. 8;
      AIS            at 0 range 9 .. 9;
      EB             at 0 range 10 .. 12;
      FSM            at 0 range 13 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   --  Internal DMAC Interrupt Enable register
   type IDINTEN_Register is record
      --  Transmit Interrupt Enable.
      TI             : Boolean := False;
      --  Receive Interrupt Enable.
      RI             : Boolean := False;
      --  Fatal Bus Error Enable.
      FBE            : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Descriptor Unavailable Interrupt.
      DU             : Boolean := False;
      --  Card Error summary Interrupt Enable.
      CES            : Boolean := False;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Normal Interrupt Summary Enable.
      NIS            : Boolean := False;
      --  Abnormal Interrupt Summary Enable.
      AIS            : Boolean := False;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IDINTEN_Register use record
      TI             at 0 range 0 .. 0;
      RI             at 0 range 1 .. 1;
      FBE            at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      DU             at 0 range 4 .. 4;
      CES            at 0 range 5 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      NIS            at 0 range 8 .. 8;
      AIS            at 0 range 9 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype CARDTHRCTL_CARDTHRESHOLD_Field is HAL.UInt8;

   --  Card Threshold Control
   type CARDTHRCTL_Register is record
      --  Card Read Threshold Enable.
      CARDRDTHREN    : Boolean := False;
      --  Busy Clear Interrupt Enable.
      BSYCLRINTEN    : Boolean := False;
      --  unspecified
      Reserved_2_15  : HAL.UInt14 := 16#0#;
      --  Card Threshold size.
      CARDTHRESHOLD  : CARDTHRCTL_CARDTHRESHOLD_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CARDTHRCTL_Register use record
      CARDRDTHREN    at 0 range 0 .. 0;
      BSYCLRINTEN    at 0 range 1 .. 1;
      Reserved_2_15  at 0 range 2 .. 15;
      CARDTHRESHOLD  at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  Power control
   type BACKENDPWR_Register is record
      --  Back-end Power control for card application.
      BACKENDPWR    : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for BACKENDPWR_Register use record
      BACKENDPWR    at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  SDIF FIFO

   --  SDIF FIFO
   type FIFO_Registers is array (0 .. 63) of HAL.UInt32
     with Volatile;

   -----------------
   -- Peripherals --
   -----------------

   --  SDMMC
   type SDIF_Peripheral is record
      --  Control register
      CTRL       : aliased CTRL_Register;
      --  Power Enable register
      PWREN      : aliased PWREN_Register;
      --  Clock Divider register
      CLKDIV     : aliased CLKDIV_Register;
      --  Clock Enable register
      CLKENA     : aliased CLKENA_Register;
      --  Time-out register
      TMOUT      : aliased TMOUT_Register;
      --  Card Type register
      CTYPE      : aliased CTYPE_Register;
      --  Block Size register
      BLKSIZ     : aliased BLKSIZ_Register;
      --  Byte Count register
      BYTCNT     : aliased HAL.UInt32;
      --  Interrupt Mask register
      INTMASK    : aliased INTMASK_Register;
      --  Command Argument register
      CMDARG     : aliased HAL.UInt32;
      --  Command register
      CMD        : aliased CMD_Register;
      --  Response register
      RESP       : aliased RESP_Registers;
      --  Masked Interrupt Status register
      MINTSTS    : aliased MINTSTS_Register;
      --  Raw Interrupt Status register
      RINTSTS    : aliased RINTSTS_Register;
      --  Status register
      STATUS     : aliased STATUS_Register;
      --  FIFO Threshold Watermark register
      FIFOTH     : aliased FIFOTH_Register;
      --  Card Detect register
      CDETECT    : aliased CDETECT_Register;
      --  Write Protect register
      WRTPRT     : aliased WRTPRT_Register;
      --  Transferred CIU Card Byte Count register
      TCBCNT     : aliased HAL.UInt32;
      --  Transferred Host to BIU-FIFO Byte Count register
      TBBCNT     : aliased HAL.UInt32;
      --  Debounce Count register
      DEBNCE     : aliased DEBNCE_Register;
      --  Hardware Reset
      RST_N      : aliased RST_N_Register;
      --  Bus Mode register
      BMOD       : aliased BMOD_Register;
      --  Poll Demand register
      PLDMND     : aliased HAL.UInt32;
      --  Descriptor List Base Address register
      DBADDR     : aliased HAL.UInt32;
      --  Internal DMAC Status register
      IDSTS      : aliased IDSTS_Register;
      --  Internal DMAC Interrupt Enable register
      IDINTEN    : aliased IDINTEN_Register;
      --  Current Host Descriptor Address register
      DSCADDR    : aliased HAL.UInt32;
      --  Current Buffer Descriptor Address register
      BUFADDR    : aliased HAL.UInt32;
      --  Card Threshold Control
      CARDTHRCTL : aliased CARDTHRCTL_Register;
      --  Power control
      BACKENDPWR : aliased BACKENDPWR_Register;
      --  SDIF FIFO
      FIFO       : aliased FIFO_Registers;
   end record
     with Volatile;

   for SDIF_Peripheral use record
      CTRL       at 16#0# range 0 .. 31;
      PWREN      at 16#4# range 0 .. 31;
      CLKDIV     at 16#8# range 0 .. 31;
      CLKENA     at 16#10# range 0 .. 31;
      TMOUT      at 16#14# range 0 .. 31;
      CTYPE      at 16#18# range 0 .. 31;
      BLKSIZ     at 16#1C# range 0 .. 31;
      BYTCNT     at 16#20# range 0 .. 31;
      INTMASK    at 16#24# range 0 .. 31;
      CMDARG     at 16#28# range 0 .. 31;
      CMD        at 16#2C# range 0 .. 31;
      RESP       at 16#30# range 0 .. 127;
      MINTSTS    at 16#40# range 0 .. 31;
      RINTSTS    at 16#44# range 0 .. 31;
      STATUS     at 16#48# range 0 .. 31;
      FIFOTH     at 16#4C# range 0 .. 31;
      CDETECT    at 16#50# range 0 .. 31;
      WRTPRT     at 16#54# range 0 .. 31;
      TCBCNT     at 16#5C# range 0 .. 31;
      TBBCNT     at 16#60# range 0 .. 31;
      DEBNCE     at 16#64# range 0 .. 31;
      RST_N      at 16#78# range 0 .. 31;
      BMOD       at 16#80# range 0 .. 31;
      PLDMND     at 16#84# range 0 .. 31;
      DBADDR     at 16#88# range 0 .. 31;
      IDSTS      at 16#8C# range 0 .. 31;
      IDINTEN    at 16#90# range 0 .. 31;
      DSCADDR    at 16#94# range 0 .. 31;
      BUFADDR    at 16#98# range 0 .. 31;
      CARDTHRCTL at 16#100# range 0 .. 31;
      BACKENDPWR at 16#104# range 0 .. 31;
      FIFO       at 16#200# range 0 .. 2047;
   end record;

   --  SDMMC
   SDIF_Periph : aliased SDIF_Peripheral
     with Import, Address => System'To_Address (16#4009B000#);

end NXP_SVD.SDIF;
