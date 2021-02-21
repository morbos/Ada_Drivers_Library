--  Copyright 2016-2019 NXP
--  All rights reserved.SPDX-License-Identifier: BSD-3-Clause


--  This spec has been automatically generated from LPC55S6x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NXP_SVD.SPI is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  SPI enable.
   type CFG_ENABLE_Field is
     (
      --  Disabled. The SPI is disabled and the internal state machine and
      --  counters are reset.
      Disabled,
      --  Enabled. The SPI is enabled for operation.
      Enabled)
     with Size => 1;
   for CFG_ENABLE_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Master mode select.
   type CFG_MASTER_Field is
     (
      --  Slave mode. The SPI will operate in slave mode. SCK, MOSI, and the
      --  SSEL signals are inputs, MISO is an output.
      Slave_Mode,
      --  Master mode. The SPI will operate in master mode. SCK, MOSI, and the
      --  SSEL signals are outputs, MISO is an input.
      Master_Mode)
     with Size => 1;
   for CFG_MASTER_Field use
     (Slave_Mode => 0,
      Master_Mode => 1);

   --  LSB First mode enable.
   type CFG_LSBF_Field is
     (
      --  Standard. Data is transmitted and received in standard MSB first
      --  order.
      Standard,
      --  Reverse. Data is transmitted and received in reverse order (LSB
      --  first).
      Reverse_k)
     with Size => 1;
   for CFG_LSBF_Field use
     (Standard => 0,
      Reverse_k => 1);

   --  Clock Phase select.
   type CFG_CPHA_Field is
     (
      --  Change. The SPI captures serial data on the first clock transition of
      --  the transfer (when the clock changes away from the rest state). Data
      --  is changed on the following edge.
      Change,
      --  Capture. The SPI changes serial data on the first clock transition of
      --  the transfer (when the clock changes away from the rest state). Data
      --  is captured on the following edge.
      Capture)
     with Size => 1;
   for CFG_CPHA_Field use
     (Change => 0,
      Capture => 1);

   --  Clock Polarity select.
   type CFG_CPOL_Field is
     (
      --  Low. The rest state of the clock (between transfers) is low.
      Low,
      --  High. The rest state of the clock (between transfers) is high.
      High)
     with Size => 1;
   for CFG_CPOL_Field use
     (Low => 0,
      High => 1);

   --  Loopback mode enable. Loopback mode applies only to Master mode, and
   --  connects transmit and receive data connected together to allow simple
   --  software testing.
   type CFG_LOOP_Field is
     (
      --  Disabled.
      Disabled,
      --  Enabled.
      Enabled)
     with Size => 1;
   for CFG_LOOP_Field use
     (Disabled => 0,
      Enabled => 1);

   --  SSEL0 Polarity select.
   type CFG_SPOL0_Field is
     (
      --  Low. The SSEL0 pin is active low.
      Low,
      --  High. The SSEL0 pin is active high.
      High)
     with Size => 1;
   for CFG_SPOL0_Field use
     (Low => 0,
      High => 1);

   --  SSEL1 Polarity select.
   type CFG_SPOL1_Field is
     (
      --  Low. The SSEL1 pin is active low.
      Low,
      --  High. The SSEL1 pin is active high.
      High)
     with Size => 1;
   for CFG_SPOL1_Field use
     (Low => 0,
      High => 1);

   --  SSEL2 Polarity select.
   type CFG_SPOL2_Field is
     (
      --  Low. The SSEL2 pin is active low.
      Low,
      --  High. The SSEL2 pin is active high.
      High)
     with Size => 1;
   for CFG_SPOL2_Field use
     (Low => 0,
      High => 1);

   --  SSEL3 Polarity select.
   type CFG_SPOL3_Field is
     (
      --  Low. The SSEL3 pin is active low.
      Low,
      --  High. The SSEL3 pin is active high.
      High)
     with Size => 1;
   for CFG_SPOL3_Field use
     (Low => 0,
      High => 1);

   --  SPI Configuration register
   type CFG_Register is record
      --  SPI enable.
      ENABLE         : CFG_ENABLE_Field := NXP_SVD.SPI.Disabled;
      --  unspecified
      Reserved_1_1   : HAL.Bit := 16#0#;
      --  Master mode select.
      MASTER         : CFG_MASTER_Field := NXP_SVD.SPI.Slave_Mode;
      --  LSB First mode enable.
      LSBF           : CFG_LSBF_Field := NXP_SVD.SPI.Standard;
      --  Clock Phase select.
      CPHA           : CFG_CPHA_Field := NXP_SVD.SPI.Change;
      --  Clock Polarity select.
      CPOL           : CFG_CPOL_Field := NXP_SVD.SPI.Low;
      --  unspecified
      Reserved_6_6   : HAL.Bit := 16#0#;
      --  Loopback mode enable. Loopback mode applies only to Master mode, and
      --  connects transmit and receive data connected together to allow simple
      --  software testing.
      LOOP_k         : CFG_LOOP_Field := NXP_SVD.SPI.Disabled;
      --  SSEL0 Polarity select.
      SPOL0          : CFG_SPOL0_Field := NXP_SVD.SPI.Low;
      --  SSEL1 Polarity select.
      SPOL1          : CFG_SPOL1_Field := NXP_SVD.SPI.Low;
      --  SSEL2 Polarity select.
      SPOL2          : CFG_SPOL2_Field := NXP_SVD.SPI.Low;
      --  SSEL3 Polarity select.
      SPOL3          : CFG_SPOL3_Field := NXP_SVD.SPI.Low;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CFG_Register use record
      ENABLE         at 0 range 0 .. 0;
      Reserved_1_1   at 0 range 1 .. 1;
      MASTER         at 0 range 2 .. 2;
      LSBF           at 0 range 3 .. 3;
      CPHA           at 0 range 4 .. 4;
      CPOL           at 0 range 5 .. 5;
      Reserved_6_6   at 0 range 6 .. 6;
      LOOP_k         at 0 range 7 .. 7;
      SPOL0          at 0 range 8 .. 8;
      SPOL1          at 0 range 9 .. 9;
      SPOL2          at 0 range 10 .. 10;
      SPOL3          at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype DLY_PRE_DELAY_Field is HAL.UInt4;
   subtype DLY_POST_DELAY_Field is HAL.UInt4;
   subtype DLY_FRAME_DELAY_Field is HAL.UInt4;
   subtype DLY_TRANSFER_DELAY_Field is HAL.UInt4;

   --  SPI Delay register
   type DLY_Register is record
      --  Controls the amount of time between SSEL assertion and the beginning
      --  of a data transfer. There is always one SPI clock time between SSEL
      --  assertion and the first clock edge. This is not considered part of
      --  the pre-delay. 0x0 = No additional time is inserted. 0x1 = 1 SPI
      --  clock time is inserted. 0x2 = 2 SPI clock times are inserted. 0xF =
      --  15 SPI clock times are inserted.
      PRE_DELAY      : DLY_PRE_DELAY_Field := 16#0#;
      --  Controls the amount of time between the end of a data transfer and
      --  SSEL deassertion. 0x0 = No additional time is inserted. 0x1 = 1 SPI
      --  clock time is inserted. 0x2 = 2 SPI clock times are inserted. 0xF =
      --  15 SPI clock times are inserted.
      POST_DELAY     : DLY_POST_DELAY_Field := 16#0#;
      --  If the EOF flag is set, controls the minimum amount of time between
      --  the current frame and the next frame (or SSEL deassertion if EOT).
      --  0x0 = No additional time is inserted. 0x1 = 1 SPI clock time is
      --  inserted. 0x2 = 2 SPI clock times are inserted. 0xF = 15 SPI clock
      --  times are inserted.
      FRAME_DELAY    : DLY_FRAME_DELAY_Field := 16#0#;
      --  Controls the minimum amount of time that the SSEL is deasserted
      --  between transfers. 0x0 = The minimum time that SSEL is deasserted is
      --  1 SPI clock time. (Zero added time.) 0x1 = The minimum time that SSEL
      --  is deasserted is 2 SPI clock times. 0x2 = The minimum time that SSEL
      --  is deasserted is 3 SPI clock times. 0xF = The minimum time that SSEL
      --  is deasserted is 16 SPI clock times.
      TRANSFER_DELAY : DLY_TRANSFER_DELAY_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DLY_Register use record
      PRE_DELAY      at 0 range 0 .. 3;
      POST_DELAY     at 0 range 4 .. 7;
      FRAME_DELAY    at 0 range 8 .. 11;
      TRANSFER_DELAY at 0 range 12 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  SPI Status. Some status flags can be cleared by writing a 1 to that bit
   --  position.
   type STAT_Register is record
      --  unspecified
      Reserved_0_3  : HAL.UInt4 := 16#0#;
      --  Write-only. Slave Select Assert. This flag is set whenever any slave
      --  select transitions from deasserted to asserted, in both master and
      --  slave modes. This allows determining when the SPI transmit/receive
      --  functions become busy, and allows waking up the device from reduced
      --  power modes when a slave mode access begins. This flag is cleared by
      --  software.
      SSA           : Boolean := False;
      --  Write-only. Slave Select Deassert. This flag is set whenever any
      --  asserted slave selects transition to deasserted, in both master and
      --  slave modes. This allows determining when the SPI transmit/receive
      --  functions become idle. This flag is cleared by software.
      SSD           : Boolean := False;
      --  Read-only. Stalled status flag. This indicates whether the SPI is
      --  currently in a stall condition.
      STALLED       : Boolean := False;
      --  End Transfer control bit. Software can set this bit to force an end
      --  to the current transfer when the transmitter finishes any activity
      --  already in progress, as if the EOT flag had been set prior to the
      --  last transmission. This capability is included to support cases where
      --  it is not known when transmit data is written that it will be the end
      --  of a transfer. The bit is cleared when the transmitter becomes idle
      --  as the transfer comes to an end. Forcing an end of transfer in this
      --  manner causes any specified FRAME_DELAY and TRANSFER_DELAY to be
      --  inserted.
      ENDTRANSFER   : Boolean := False;
      --  Read-only. Master idle status flag. This bit is 1 whenever the SPI
      --  master function is fully idle. This means that the transmit holding
      --  register is empty and the transmitter is not in the process of
      --  sending data.
      MSTIDLE       : Boolean := True;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for STAT_Register use record
      Reserved_0_3  at 0 range 0 .. 3;
      SSA           at 0 range 4 .. 4;
      SSD           at 0 range 5 .. 5;
      STALLED       at 0 range 6 .. 6;
      ENDTRANSFER   at 0 range 7 .. 7;
      MSTIDLE       at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   --  Slave select assert interrupt enable. Determines whether an interrupt
   --  occurs when the Slave Select is asserted.
   type INTENSET_SSAEN_Field is
     (
      --  Disabled. No interrupt will be generated when any Slave Select
      --  transitions from deasserted to asserted.
      Disabled,
      --  Enabled. An interrupt will be generated when any Slave Select
      --  transitions from deasserted to asserted.
      Enabled)
     with Size => 1;
   for INTENSET_SSAEN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Slave select deassert interrupt enable. Determines whether an interrupt
   --  occurs when the Slave Select is deasserted.
   type INTENSET_SSDEN_Field is
     (
      --  Disabled. No interrupt will be generated when all asserted Slave
      --  Selects transition to deasserted.
      Disabled,
      --  Enabled. An interrupt will be generated when all asserted Slave
      --  Selects transition to deasserted.
      Enabled)
     with Size => 1;
   for INTENSET_SSDEN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Master idle interrupt enable.
   type INTENSET_MSTIDLEEN_Field is
     (
      --  No interrupt will be generated when the SPI master function is idle.
      Disabled,
      --  An interrupt will be generated when the SPI master function is fully
      --  idle.
      Enabled)
     with Size => 1;
   for INTENSET_MSTIDLEEN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  SPI Interrupt Enable read and Set. A complete value may be read from
   --  this register. Writing a 1 to any implemented bit position causes that
   --  bit to be set.
   type INTENSET_Register is record
      --  unspecified
      Reserved_0_3  : HAL.UInt4 := 16#0#;
      --  Slave select assert interrupt enable. Determines whether an interrupt
      --  occurs when the Slave Select is asserted.
      SSAEN         : INTENSET_SSAEN_Field := NXP_SVD.SPI.Disabled;
      --  Slave select deassert interrupt enable. Determines whether an
      --  interrupt occurs when the Slave Select is deasserted.
      SSDEN         : INTENSET_SSDEN_Field := NXP_SVD.SPI.Disabled;
      --  unspecified
      Reserved_6_7  : HAL.UInt2 := 16#0#;
      --  Master idle interrupt enable.
      MSTIDLEEN     : INTENSET_MSTIDLEEN_Field := NXP_SVD.SPI.Disabled;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENSET_Register use record
      Reserved_0_3  at 0 range 0 .. 3;
      SSAEN         at 0 range 4 .. 4;
      SSDEN         at 0 range 5 .. 5;
      Reserved_6_7  at 0 range 6 .. 7;
      MSTIDLEEN     at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   --  SPI Interrupt Enable Clear. Writing a 1 to any implemented bit position
   --  causes the corresponding bit in INTENSET to be cleared.
   type INTENCLR_Register is record
      --  unspecified
      Reserved_0_3  : HAL.UInt4 := 16#0#;
      --  Write-only. Writing 1 clears the corresponding bit in the INTENSET
      --  register.
      SSAEN         : Boolean := False;
      --  Write-only. Writing 1 clears the corresponding bit in the INTENSET
      --  register.
      SSDEN         : Boolean := False;
      --  unspecified
      Reserved_6_7  : HAL.UInt2 := 16#0#;
      --  Write-only. Writing 1 clears the corresponding bit in the INTENSET
      --  register.
      MSTIDLE       : Boolean := False;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENCLR_Register use record
      Reserved_0_3  at 0 range 0 .. 3;
      SSAEN         at 0 range 4 .. 4;
      SSDEN         at 0 range 5 .. 5;
      Reserved_6_7  at 0 range 6 .. 7;
      MSTIDLE       at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   subtype DIV_DIVVAL_Field is HAL.UInt16;

   --  SPI clock Divider
   type DIV_Register is record
      --  Rate divider value. Specifies how the Flexcomm clock (FCLK) is
      --  divided to produce the SPI clock rate in master mode. DIVVAL is -1
      --  encoded such that the value 0 results in FCLK/1, the value 1 results
      --  in FCLK/2, up to the maximum possible divide value of 0xFFFF, which
      --  results in FCLK/65536.
      DIVVAL         : DIV_DIVVAL_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DIV_Register use record
      DIVVAL         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  SPI Interrupt Status
   type INTSTAT_Register is record
      --  unspecified
      Reserved_0_3  : HAL.UInt4;
      --  Read-only. Slave Select Assert.
      SSA           : Boolean;
      --  Read-only. Slave Select Deassert.
      SSD           : Boolean;
      --  unspecified
      Reserved_6_7  : HAL.UInt2;
      --  Read-only. Master Idle status flag.
      MSTIDLE       : Boolean;
      --  unspecified
      Reserved_9_31 : HAL.UInt23;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTSTAT_Register use record
      Reserved_0_3  at 0 range 0 .. 3;
      SSA           at 0 range 4 .. 4;
      SSD           at 0 range 5 .. 5;
      Reserved_6_7  at 0 range 6 .. 7;
      MSTIDLE       at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   --  Enable the transmit FIFO.
   type FIFOCFG_ENABLETX_Field is
     (
      --  The transmit FIFO is not enabled.
      Disabled,
      --  The transmit FIFO is enabled.
      Enabled)
     with Size => 1;
   for FIFOCFG_ENABLETX_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable the receive FIFO.
   type FIFOCFG_ENABLERX_Field is
     (
      --  The receive FIFO is not enabled.
      Disabled,
      --  The receive FIFO is enabled.
      Enabled)
     with Size => 1;
   for FIFOCFG_ENABLERX_Field use
     (Disabled => 0,
      Enabled => 1);

   subtype FIFOCFG_SIZE_Field is HAL.UInt2;

   --  DMA configuration for transmit.
   type FIFOCFG_DMATX_Field is
     (
      --  DMA is not used for the transmit function.
      Disabled,
      --  Trigger DMA for the transmit function if the FIFO is not full.
      --  Generally, data interrupts would be disabled if DMA is enabled.
      Enabled)
     with Size => 1;
   for FIFOCFG_DMATX_Field use
     (Disabled => 0,
      Enabled => 1);

   --  DMA configuration for receive.
   type FIFOCFG_DMARX_Field is
     (
      --  DMA is not used for the receive function.
      Disabled,
      --  Trigger DMA for the receive function if the FIFO is not empty.
      --  Generally, data interrupts would be disabled if DMA is enabled.
      Enabled)
     with Size => 1;
   for FIFOCFG_DMARX_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Wake-up for transmit FIFO level. This allows the device to be woken from
   --  reduced power modes (up to power-down, as long as the peripheral
   --  function works in that power mode) without enabling the TXLVL interrupt.
   --  Only DMA wakes up, processes data, and goes back to sleep. The CPU will
   --  remain stopped until woken by another cause, such as DMA completion. See
   --  Hardware Wake-up control register.
   type FIFOCFG_WAKETX_Field is
     (
      --  Only enabled interrupts will wake up the device form reduced power
      --  modes.
      Disabled,
      --  A device wake-up for DMA will occur if the transmit FIFO level
      --  reaches the value specified by TXLVL in FIFOTRIG, even when the TXLVL
      --  interrupt is not enabled.
      Enabled)
     with Size => 1;
   for FIFOCFG_WAKETX_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Wake-up for receive FIFO level. This allows the device to be woken from
   --  reduced power modes (up to power-down, as long as the peripheral
   --  function works in that power mode) without enabling the TXLVL interrupt.
   --  Only DMA wakes up, processes data, and goes back to sleep. The CPU will
   --  remain stopped until woken by another cause, such as DMA completion. See
   --  Hardware Wake-up control register.
   type FIFOCFG_WAKERX_Field is
     (
      --  Only enabled interrupts will wake up the device form reduced power
      --  modes.
      Disabled,
      --  A device wake-up for DMA will occur if the receive FIFO level reaches
      --  the value specified by RXLVL in FIFOTRIG, even when the RXLVL
      --  interrupt is not enabled.
      Enabled)
     with Size => 1;
   for FIFOCFG_WAKERX_Field use
     (Disabled => 0,
      Enabled => 1);

   --  FIFO configuration and enable register.
   type FIFOCFG_Register is record
      --  Enable the transmit FIFO.
      ENABLETX       : FIFOCFG_ENABLETX_Field := NXP_SVD.SPI.Disabled;
      --  Enable the receive FIFO.
      ENABLERX       : FIFOCFG_ENABLERX_Field := NXP_SVD.SPI.Disabled;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  Read-only. FIFO size configuration. This is a read-only field. 0x0 =
      --  FIFO is configured as 16 entries of 8 bits. 0x1, 0x2, 0x3 = not
      --  applicable to USART.
      SIZE           : FIFOCFG_SIZE_Field := 16#0#;
      --  unspecified
      Reserved_6_11  : HAL.UInt6 := 16#0#;
      --  DMA configuration for transmit.
      DMATX          : FIFOCFG_DMATX_Field := NXP_SVD.SPI.Disabled;
      --  DMA configuration for receive.
      DMARX          : FIFOCFG_DMARX_Field := NXP_SVD.SPI.Disabled;
      --  Wake-up for transmit FIFO level. This allows the device to be woken
      --  from reduced power modes (up to power-down, as long as the peripheral
      --  function works in that power mode) without enabling the TXLVL
      --  interrupt. Only DMA wakes up, processes data, and goes back to sleep.
      --  The CPU will remain stopped until woken by another cause, such as DMA
      --  completion. See Hardware Wake-up control register.
      WAKETX         : FIFOCFG_WAKETX_Field := NXP_SVD.SPI.Disabled;
      --  Wake-up for receive FIFO level. This allows the device to be woken
      --  from reduced power modes (up to power-down, as long as the peripheral
      --  function works in that power mode) without enabling the TXLVL
      --  interrupt. Only DMA wakes up, processes data, and goes back to sleep.
      --  The CPU will remain stopped until woken by another cause, such as DMA
      --  completion. See Hardware Wake-up control register.
      WAKERX         : FIFOCFG_WAKERX_Field := NXP_SVD.SPI.Disabled;
      --  Empty command for the transmit FIFO. When a 1 is written to this bit,
      --  the TX FIFO is emptied.
      EMPTYTX        : Boolean := False;
      --  Empty command for the receive FIFO. When a 1 is written to this bit,
      --  the RX FIFO is emptied.
      EMPTYRX        : Boolean := False;
      --  unspecified
      Reserved_18_31 : HAL.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FIFOCFG_Register use record
      ENABLETX       at 0 range 0 .. 0;
      ENABLERX       at 0 range 1 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      SIZE           at 0 range 4 .. 5;
      Reserved_6_11  at 0 range 6 .. 11;
      DMATX          at 0 range 12 .. 12;
      DMARX          at 0 range 13 .. 13;
      WAKETX         at 0 range 14 .. 14;
      WAKERX         at 0 range 15 .. 15;
      EMPTYTX        at 0 range 16 .. 16;
      EMPTYRX        at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   subtype FIFOSTAT_TXLVL_Field is HAL.UInt5;
   subtype FIFOSTAT_RXLVL_Field is HAL.UInt5;

   --  FIFO status register.
   type FIFOSTAT_Register is record
      --  TX FIFO error. Will be set if a transmit FIFO error occurs. This
      --  could be an overflow caused by pushing data into a full FIFO, or by
      --  an underflow if the FIFO is empty when data is needed. Cleared by
      --  writing a 1 to this bit.
      TXERR          : Boolean := False;
      --  RX FIFO error. Will be set if a receive FIFO overflow occurs, caused
      --  by software or DMA not emptying the FIFO fast enough. Cleared by
      --  writing a 1 to this bit.
      RXERR          : Boolean := False;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  Read-only. Peripheral interrupt. When 1, this indicates that the
      --  peripheral function has asserted an interrupt. The details can be
      --  found by reading the peripheral's STAT register.
      PERINT         : Boolean := False;
      --  Read-only. Transmit FIFO empty. When 1, the transmit FIFO is empty.
      --  The peripheral may still be processing the last piece of data.
      TXEMPTY        : Boolean := True;
      --  Read-only. Transmit FIFO not full. When 1, the transmit FIFO is not
      --  full, so more data can be written. When 0, the transmit FIFO is full
      --  and another write would cause it to overflow.
      TXNOTFULL      : Boolean := True;
      --  Read-only. Receive FIFO not empty. When 1, the receive FIFO is not
      --  empty, so data can be read. When 0, the receive FIFO is empty.
      RXNOTEMPTY     : Boolean := False;
      --  Read-only. Receive FIFO full. When 1, the receive FIFO is full. Data
      --  needs to be read out to prevent the peripheral from causing an
      --  overflow.
      RXFULL         : Boolean := False;
      --  Read-only. Transmit FIFO current level. A 0 means the TX FIFO is
      --  currently empty, and the TXEMPTY and TXNOTFULL flags will be 1. Other
      --  values tell how much data is actually in the TX FIFO at the point
      --  where the read occurs. If the TX FIFO is full, the TXEMPTY and
      --  TXNOTFULL flags will be 0.
      TXLVL          : FIFOSTAT_TXLVL_Field := 16#0#;
      --  unspecified
      Reserved_13_15 : HAL.UInt3 := 16#0#;
      --  Read-only. Receive FIFO current level. A 0 means the RX FIFO is
      --  currently empty, and the RXFULL and RXNOTEMPTY flags will be 0. Other
      --  values tell how much data is actually in the RX FIFO at the point
      --  where the read occurs. If the RX FIFO is full, the RXFULL and
      --  RXNOTEMPTY flags will be 1.
      RXLVL          : FIFOSTAT_RXLVL_Field := 16#0#;
      --  unspecified
      Reserved_21_31 : HAL.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FIFOSTAT_Register use record
      TXERR          at 0 range 0 .. 0;
      RXERR          at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      PERINT         at 0 range 3 .. 3;
      TXEMPTY        at 0 range 4 .. 4;
      TXNOTFULL      at 0 range 5 .. 5;
      RXNOTEMPTY     at 0 range 6 .. 6;
      RXFULL         at 0 range 7 .. 7;
      TXLVL          at 0 range 8 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      RXLVL          at 0 range 16 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   --  Transmit FIFO level trigger enable. This trigger will become an
   --  interrupt if enabled in FIFOINTENSET, or a DMA trigger if DMATX in
   --  FIFOCFG is set.
   type FIFOTRIG_TXLVLENA_Field is
     (
      --  Transmit FIFO level does not generate a FIFO level trigger.
      Disabled,
      --  An trigger will be generated if the transmit FIFO level reaches the
      --  value specified by the TXLVL field in this register.
      Enabled)
     with Size => 1;
   for FIFOTRIG_TXLVLENA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Receive FIFO level trigger enable. This trigger will become an interrupt
   --  if enabled in FIFOINTENSET, or a DMA trigger if DMARX in FIFOCFG is set.
   type FIFOTRIG_RXLVLENA_Field is
     (
      --  Receive FIFO level does not generate a FIFO level trigger.
      Disabled,
      --  An trigger will be generated if the receive FIFO level reaches the
      --  value specified by the RXLVL field in this register.
      Enabled)
     with Size => 1;
   for FIFOTRIG_RXLVLENA_Field use
     (Disabled => 0,
      Enabled => 1);

   subtype FIFOTRIG_TXLVL_Field is HAL.UInt4;
   subtype FIFOTRIG_RXLVL_Field is HAL.UInt4;

   --  FIFO trigger settings for interrupt and DMA request.
   type FIFOTRIG_Register is record
      --  Transmit FIFO level trigger enable. This trigger will become an
      --  interrupt if enabled in FIFOINTENSET, or a DMA trigger if DMATX in
      --  FIFOCFG is set.
      TXLVLENA       : FIFOTRIG_TXLVLENA_Field := NXP_SVD.SPI.Disabled;
      --  Receive FIFO level trigger enable. This trigger will become an
      --  interrupt if enabled in FIFOINTENSET, or a DMA trigger if DMARX in
      --  FIFOCFG is set.
      RXLVLENA       : FIFOTRIG_RXLVLENA_Field := NXP_SVD.SPI.Disabled;
      --  unspecified
      Reserved_2_7   : HAL.UInt6 := 16#0#;
      --  Transmit FIFO level trigger point. This field is used only when
      --  TXLVLENA = 1. If enabled to do so, the FIFO level can wake up the
      --  device just enough to perform DMA, then return to the reduced power
      --  mode. See Hardware Wake-up control register. 0 = trigger when the TX
      --  FIFO becomes empty. 1 = trigger when the TX FIFO level decreases to
      --  one entry. 15 = trigger when the TX FIFO level decreases to 15
      --  entries (is no longer full).
      TXLVL          : FIFOTRIG_TXLVL_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Receive FIFO level trigger point. The RX FIFO level is checked when a
      --  new piece of data is received. This field is used only when RXLVLENA
      --  = 1. If enabled to do so, the FIFO level can wake up the device just
      --  enough to perform DMA, then return to the reduced power mode. See
      --  Hardware Wake-up control register. 0 = trigger when the RX FIFO has
      --  received one entry (is no longer empty). 1 = trigger when the RX FIFO
      --  has received two entries. 15 = trigger when the RX FIFO has received
      --  16 entries (has become full).
      RXLVL          : FIFOTRIG_RXLVL_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FIFOTRIG_Register use record
      TXLVLENA       at 0 range 0 .. 0;
      RXLVLENA       at 0 range 1 .. 1;
      Reserved_2_7   at 0 range 2 .. 7;
      TXLVL          at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      RXLVL          at 0 range 16 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  Determines whether an interrupt occurs when a transmit error occurs,
   --  based on the TXERR flag in the FIFOSTAT register.
   type FIFOINTENSET_TXERR_Field is
     (
      --  No interrupt will be generated for a transmit error.
      Disabled,
      --  An interrupt will be generated when a transmit error occurs.
      Enabled)
     with Size => 1;
   for FIFOINTENSET_TXERR_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Determines whether an interrupt occurs when a receive error occurs,
   --  based on the RXERR flag in the FIFOSTAT register.
   type FIFOINTENSET_RXERR_Field is
     (
      --  No interrupt will be generated for a receive error.
      Disabled,
      --  An interrupt will be generated when a receive error occurs.
      Enabled)
     with Size => 1;
   for FIFOINTENSET_RXERR_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Determines whether an interrupt occurs when a the transmit FIFO reaches
   --  the level specified by the TXLVL field in the FIFOTRIG register.
   type FIFOINTENSET_TXLVL_Field is
     (
      --  No interrupt will be generated based on the TX FIFO level.
      Disabled,
      --  If TXLVLENA in the FIFOTRIG register = 1, an interrupt will be
      --  generated when the TX FIFO level decreases to the level specified by
      --  TXLVL in the FIFOTRIG register.
      Enabled)
     with Size => 1;
   for FIFOINTENSET_TXLVL_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Determines whether an interrupt occurs when a the receive FIFO reaches
   --  the level specified by the TXLVL field in the FIFOTRIG register.
   type FIFOINTENSET_RXLVL_Field is
     (
      --  No interrupt will be generated based on the RX FIFO level.
      Disabled,
      --  If RXLVLENA in the FIFOTRIG register = 1, an interrupt will be
      --  generated when the when the RX FIFO level increases to the level
      --  specified by RXLVL in the FIFOTRIG register.
      Enabled)
     with Size => 1;
   for FIFOINTENSET_RXLVL_Field use
     (Disabled => 0,
      Enabled => 1);

   --  FIFO interrupt enable set (enable) and read register.
   type FIFOINTENSET_Register is record
      --  Determines whether an interrupt occurs when a transmit error occurs,
      --  based on the TXERR flag in the FIFOSTAT register.
      TXERR         : FIFOINTENSET_TXERR_Field := NXP_SVD.SPI.Disabled;
      --  Determines whether an interrupt occurs when a receive error occurs,
      --  based on the RXERR flag in the FIFOSTAT register.
      RXERR         : FIFOINTENSET_RXERR_Field := NXP_SVD.SPI.Disabled;
      --  Determines whether an interrupt occurs when a the transmit FIFO
      --  reaches the level specified by the TXLVL field in the FIFOTRIG
      --  register.
      TXLVL         : FIFOINTENSET_TXLVL_Field := NXP_SVD.SPI.Disabled;
      --  Determines whether an interrupt occurs when a the receive FIFO
      --  reaches the level specified by the TXLVL field in the FIFOTRIG
      --  register.
      RXLVL         : FIFOINTENSET_RXLVL_Field := NXP_SVD.SPI.Disabled;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FIFOINTENSET_Register use record
      TXERR         at 0 range 0 .. 0;
      RXERR         at 0 range 1 .. 1;
      TXLVL         at 0 range 2 .. 2;
      RXLVL         at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  FIFO interrupt enable clear (disable) and read register.
   type FIFOINTENCLR_Register is record
      --  Writing one clears the corresponding bits in the FIFOINTENSET
      --  register.
      TXERR         : Boolean := False;
      --  Writing one clears the corresponding bits in the FIFOINTENSET
      --  register.
      RXERR         : Boolean := False;
      --  Writing one clears the corresponding bits in the FIFOINTENSET
      --  register.
      TXLVL         : Boolean := False;
      --  Writing one clears the corresponding bits in the FIFOINTENSET
      --  register.
      RXLVL         : Boolean := False;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FIFOINTENCLR_Register use record
      TXERR         at 0 range 0 .. 0;
      RXERR         at 0 range 1 .. 1;
      TXLVL         at 0 range 2 .. 2;
      RXLVL         at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  FIFO interrupt status register.
   type FIFOINTSTAT_Register is record
      --  Read-only. TX FIFO error.
      TXERR         : Boolean;
      --  Read-only. RX FIFO error.
      RXERR         : Boolean;
      --  Read-only. Transmit FIFO level interrupt.
      TXLVL         : Boolean;
      --  Read-only. Receive FIFO level interrupt.
      RXLVL         : Boolean;
      --  Read-only. Peripheral interrupt.
      PERINT        : Boolean;
      --  unspecified
      Reserved_5_31 : HAL.UInt27;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FIFOINTSTAT_Register use record
      TXERR         at 0 range 0 .. 0;
      RXERR         at 0 range 1 .. 1;
      TXLVL         at 0 range 2 .. 2;
      RXLVL         at 0 range 3 .. 3;
      PERINT        at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype FIFOWR_TXDATA_Field is HAL.UInt16;

   --  Transmit slave select. This field asserts SSEL0 in master mode. The
   --  output on the pin is active LOW by default.
   type FIFOWR_TXSSEL0_N_Field is
     (
      --  SSEL0 asserted.
      Asserted,
      --  SSEL0 not asserted.
      Not_Asserted)
     with Size => 1;
   for FIFOWR_TXSSEL0_N_Field use
     (Asserted => 0,
      Not_Asserted => 1);

   --  Transmit slave select. This field asserts SSEL1 in master mode. The
   --  output on the pin is active LOW by default.
   type FIFOWR_TXSSEL1_N_Field is
     (
      --  SSEL1 asserted.
      Asserted,
      --  SSEL1 not asserted.
      Not_Asserted)
     with Size => 1;
   for FIFOWR_TXSSEL1_N_Field use
     (Asserted => 0,
      Not_Asserted => 1);

   --  Transmit slave select. This field asserts SSEL2 in master mode. The
   --  output on the pin is active LOW by default.
   type FIFOWR_TXSSEL2_N_Field is
     (
      --  SSEL2 asserted.
      Asserted,
      --  SSEL2 not asserted.
      Not_Asserted)
     with Size => 1;
   for FIFOWR_TXSSEL2_N_Field use
     (Asserted => 0,
      Not_Asserted => 1);

   --  Transmit slave select. This field asserts SSEL3 in master mode. The
   --  output on the pin is active LOW by default.
   type FIFOWR_TXSSEL3_N_Field is
     (
      --  SSEL3 asserted.
      Asserted,
      --  SSEL3 not asserted.
      Not_Asserted)
     with Size => 1;
   for FIFOWR_TXSSEL3_N_Field use
     (Asserted => 0,
      Not_Asserted => 1);

   --  End of transfer. The asserted SSEL will be deasserted at the end of a
   --  transfer and remain so far at least the time specified by the
   --  Transfer_delay value in the DLY register.
   type FIFOWR_EOT_Field is
     (
      --  SSEL not deasserted. This piece of data is not treated as the end of
      --  a transfer. SSEL will not be deasserted at the end of this data.
      Not_Deasserted,
      --  SSEL deasserted. This piece of data is treated as the end of a
      --  transfer. SSEL will be deasserted at the end of this piece of data.
      Deasserted)
     with Size => 1;
   for FIFOWR_EOT_Field use
     (Not_Deasserted => 0,
      Deasserted => 1);

   --  End of frame. Between frames, a delay may be inserted, as defined by the
   --  Frame_delay value in the DLY register. The end of a frame may not be
   --  particularly meaningful if the Frame_delay value = 0. This control can
   --  be used as part of the support for frame lengths greater than 16 bits.
   type FIFOWR_EOF_Field is
     (
      --  Data not EOF. This piece of data transmitted is not treated as the
      --  end of a frame.
      Not_Eof,
      --  Data EOF. This piece of data is treated as the end of a frame,
      --  causing the Frame_delay time to be inserted before subsequent data is
      --  transmitted.
      Eof)
     with Size => 1;
   for FIFOWR_EOF_Field use
     (Not_Eof => 0,
      Eof => 1);

   --  Receive Ignore. This allows data to be transmitted using the SPI without
   --  the need to read unneeded data from the receiver. Setting this bit
   --  simplifies the transmit process and can be used with the DMA.
   type FIFOWR_RXIGNORE_Field is
     (
      --  Read received data. Received data must be read in order to allow
      --  transmission to progress. SPI transmit will halt when the receive
      --  data FIFO is full. In slave mode, an overrun error will occur if
      --  received data is not read before new data is received.
      Read,
      --  Ignore received data. Received data is ignored, allowing transmission
      --  without reading unneeded received data. No receiver flags are
      --  generated.
      Ignore)
     with Size => 1;
   for FIFOWR_RXIGNORE_Field use
     (Read => 0,
      Ignore => 1);

   subtype FIFOWR_LEN_Field is HAL.UInt4;

   --  FIFO write data.
   type FIFOWR_Register is record
      --  Write-only. Transmit data to the FIFO.
      TXDATA         : FIFOWR_TXDATA_Field := 16#0#;
      --  Write-only. Transmit slave select. This field asserts SSEL0 in master
      --  mode. The output on the pin is active LOW by default.
      TXSSEL0_N      : FIFOWR_TXSSEL0_N_Field := NXP_SVD.SPI.Asserted;
      --  Write-only. Transmit slave select. This field asserts SSEL1 in master
      --  mode. The output on the pin is active LOW by default.
      TXSSEL1_N      : FIFOWR_TXSSEL1_N_Field := NXP_SVD.SPI.Asserted;
      --  Write-only. Transmit slave select. This field asserts SSEL2 in master
      --  mode. The output on the pin is active LOW by default.
      TXSSEL2_N      : FIFOWR_TXSSEL2_N_Field := NXP_SVD.SPI.Asserted;
      --  Write-only. Transmit slave select. This field asserts SSEL3 in master
      --  mode. The output on the pin is active LOW by default.
      TXSSEL3_N      : FIFOWR_TXSSEL3_N_Field := NXP_SVD.SPI.Asserted;
      --  Write-only. End of transfer. The asserted SSEL will be deasserted at
      --  the end of a transfer and remain so far at least the time specified
      --  by the Transfer_delay value in the DLY register.
      EOT            : FIFOWR_EOT_Field := NXP_SVD.SPI.Not_Deasserted;
      --  Write-only. End of frame. Between frames, a delay may be inserted, as
      --  defined by the Frame_delay value in the DLY register. The end of a
      --  frame may not be particularly meaningful if the Frame_delay value =
      --  0. This control can be used as part of the support for frame lengths
      --  greater than 16 bits.
      EOF            : FIFOWR_EOF_Field := NXP_SVD.SPI.Not_Eof;
      --  Write-only. Receive Ignore. This allows data to be transmitted using
      --  the SPI without the need to read unneeded data from the receiver.
      --  Setting this bit simplifies the transmit process and can be used with
      --  the DMA.
      RXIGNORE       : FIFOWR_RXIGNORE_Field := NXP_SVD.SPI.Read;
      --  unspecified
      Reserved_23_23 : HAL.Bit := 16#0#;
      --  Write-only. Data Length. Specifies the data length from 4 to 16 bits.
      --  Note that transfer lengths greater than 16 bits are supported by
      --  implementing multiple sequential transmits. 0x0-2 = Reserved. 0x3 =
      --  Data transfer is 4 bits in length. 0x4 = Data transfer is 5 bits in
      --  length. 0xF = Data transfer is 16 bits in length.
      LEN            : FIFOWR_LEN_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FIFOWR_Register use record
      TXDATA         at 0 range 0 .. 15;
      TXSSEL0_N      at 0 range 16 .. 16;
      TXSSEL1_N      at 0 range 17 .. 17;
      TXSSEL2_N      at 0 range 18 .. 18;
      TXSSEL3_N      at 0 range 19 .. 19;
      EOT            at 0 range 20 .. 20;
      EOF            at 0 range 21 .. 21;
      RXIGNORE       at 0 range 22 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      LEN            at 0 range 24 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype FIFORD_RXDATA_Field is HAL.UInt16;

   --  FIFO read data.
   type FIFORD_Register is record
      --  Read-only. Received data from the FIFO.
      RXDATA         : FIFORD_RXDATA_Field;
      --  Read-only. Slave Select for receive. This field allows the state of
      --  the SSEL0 pin to be saved along with received data. The value will
      --  reflect the SSEL0 pin for both master and slave operation. A zero
      --  indicates that a slave select is active. The actual polarity of each
      --  slave select pin is configured by the related SPOL bit in CFG.
      RXSSEL0_N      : Boolean;
      --  Read-only. Slave Select for receive. This field allows the state of
      --  the SSEL1 pin to be saved along with received data. The value will
      --  reflect the SSEL1 pin for both master and slave operation. A zero
      --  indicates that a slave select is active. The actual polarity of each
      --  slave select pin is configured by the related SPOL bit in CFG.
      RXSSEL1_N      : Boolean;
      --  Read-only. Slave Select for receive. This field allows the state of
      --  the SSEL2 pin to be saved along with received data. The value will
      --  reflect the SSEL2 pin for both master and slave operation. A zero
      --  indicates that a slave select is active. The actual polarity of each
      --  slave select pin is configured by the related SPOL bit in CFG.
      RXSSEL2_N      : Boolean;
      --  Read-only. Slave Select for receive. This field allows the state of
      --  the SSEL3 pin to be saved along with received data. The value will
      --  reflect the SSEL3 pin for both master and slave operation. A zero
      --  indicates that a slave select is active. The actual polarity of each
      --  slave select pin is configured by the related SPOL bit in CFG.
      RXSSEL3_N      : Boolean;
      --  Read-only. Start of Transfer flag. This flag will be 1 if this is the
      --  first data after the SSELs went from deasserted to asserted (i.e.,
      --  any previous transfer has ended). This information can be used to
      --  identify the first piece of data in cases where the transfer length
      --  is greater than 16 bits.
      SOT            : Boolean;
      --  unspecified
      Reserved_21_31 : HAL.UInt11;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FIFORD_Register use record
      RXDATA         at 0 range 0 .. 15;
      RXSSEL0_N      at 0 range 16 .. 16;
      RXSSEL1_N      at 0 range 17 .. 17;
      RXSSEL2_N      at 0 range 18 .. 18;
      RXSSEL3_N      at 0 range 19 .. 19;
      SOT            at 0 range 20 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   subtype FIFORDNOPOP_RXDATA_Field is HAL.UInt16;

   --  FIFO data read with no FIFO pop.
   type FIFORDNOPOP_Register is record
      --  Read-only. Received data from the FIFO.
      RXDATA         : FIFORDNOPOP_RXDATA_Field;
      --  Read-only. Slave Select for receive.
      RXSSEL0_N      : Boolean;
      --  Read-only. Slave Select for receive.
      RXSSEL1_N      : Boolean;
      --  Read-only. Slave Select for receive.
      RXSSEL2_N      : Boolean;
      --  Read-only. Slave Select for receive.
      RXSSEL3_N      : Boolean;
      --  Read-only. Start of transfer flag.
      SOT            : Boolean;
      --  unspecified
      Reserved_21_31 : HAL.UInt11;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FIFORDNOPOP_Register use record
      RXDATA         at 0 range 0 .. 15;
      RXSSEL0_N      at 0 range 16 .. 16;
      RXSSEL1_N      at 0 range 17 .. 17;
      RXSSEL2_N      at 0 range 18 .. 18;
      RXSSEL3_N      at 0 range 19 .. 19;
      SOT            at 0 range 20 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   subtype ID_APERTURE_Field is HAL.UInt8;
   subtype ID_MINOR_REV_Field is HAL.UInt4;
   subtype ID_MAJOR_REV_Field is HAL.UInt4;
   subtype ID_ID_Field is HAL.UInt16;

   --  Peripheral identification register.
   type ID_Register is record
      --  Read-only. Aperture: encoded as (aperture size/4K) -1, so 0x00 means
      --  a 4K aperture.
      APERTURE  : ID_APERTURE_Field;
      --  Read-only. Minor revision of module implementation.
      MINOR_REV : ID_MINOR_REV_Field;
      --  Read-only. Major revision of module implementation.
      MAJOR_REV : ID_MAJOR_REV_Field;
      --  Read-only. Module identifier for the selected function.
      ID        : ID_ID_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ID_Register use record
      APERTURE  at 0 range 0 .. 7;
      MINOR_REV at 0 range 8 .. 11;
      MAJOR_REV at 0 range 12 .. 15;
      ID        at 0 range 16 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Serial Peripheral Interfaces (SPI)
   type SPI_Peripheral is record
      --  SPI Configuration register
      CFG          : aliased CFG_Register;
      --  SPI Delay register
      DLY          : aliased DLY_Register;
      --  SPI Status. Some status flags can be cleared by writing a 1 to that
      --  bit position.
      STAT         : aliased STAT_Register;
      --  SPI Interrupt Enable read and Set. A complete value may be read from
      --  this register. Writing a 1 to any implemented bit position causes
      --  that bit to be set.
      INTENSET     : aliased INTENSET_Register;
      --  SPI Interrupt Enable Clear. Writing a 1 to any implemented bit
      --  position causes the corresponding bit in INTENSET to be cleared.
      INTENCLR     : aliased INTENCLR_Register;
      --  SPI clock Divider
      DIV          : aliased DIV_Register;
      --  SPI Interrupt Status
      INTSTAT      : aliased INTSTAT_Register;
      --  FIFO configuration and enable register.
      FIFOCFG      : aliased FIFOCFG_Register;
      --  FIFO status register.
      FIFOSTAT     : aliased FIFOSTAT_Register;
      --  FIFO trigger settings for interrupt and DMA request.
      FIFOTRIG     : aliased FIFOTRIG_Register;
      --  FIFO interrupt enable set (enable) and read register.
      FIFOINTENSET : aliased FIFOINTENSET_Register;
      --  FIFO interrupt enable clear (disable) and read register.
      FIFOINTENCLR : aliased FIFOINTENCLR_Register;
      --  FIFO interrupt status register.
      FIFOINTSTAT  : aliased FIFOINTSTAT_Register;
      --  FIFO write data.
      FIFOWR       : aliased FIFOWR_Register;
      --  FIFO read data.
      FIFORD       : aliased FIFORD_Register;
      --  FIFO data read with no FIFO pop.
      FIFORDNOPOP  : aliased FIFORDNOPOP_Register;
      --  Peripheral identification register.
      ID           : aliased ID_Register;
   end record
     with Volatile;

   for SPI_Peripheral use record
      CFG          at 16#400# range 0 .. 31;
      DLY          at 16#404# range 0 .. 31;
      STAT         at 16#408# range 0 .. 31;
      INTENSET     at 16#40C# range 0 .. 31;
      INTENCLR     at 16#410# range 0 .. 31;
      DIV          at 16#424# range 0 .. 31;
      INTSTAT      at 16#428# range 0 .. 31;
      FIFOCFG      at 16#E00# range 0 .. 31;
      FIFOSTAT     at 16#E04# range 0 .. 31;
      FIFOTRIG     at 16#E08# range 0 .. 31;
      FIFOINTENSET at 16#E10# range 0 .. 31;
      FIFOINTENCLR at 16#E14# range 0 .. 31;
      FIFOINTSTAT  at 16#E18# range 0 .. 31;
      FIFOWR       at 16#E20# range 0 .. 31;
      FIFORD       at 16#E30# range 0 .. 31;
      FIFORDNOPOP  at 16#E40# range 0 .. 31;
      ID           at 16#FFC# range 0 .. 31;
   end record;

   --  Serial Peripheral Interfaces (SPI)
   SPI0_Periph : aliased SPI_Peripheral
     with Import, Address => System'To_Address (16#40086000#);

   --  Serial Peripheral Interfaces (SPI)
   SPI1_Periph : aliased SPI_Peripheral
     with Import, Address => System'To_Address (16#40087000#);

   --  Serial Peripheral Interfaces (SPI)
   SPI2_Periph : aliased SPI_Peripheral
     with Import, Address => System'To_Address (16#40088000#);

   --  Serial Peripheral Interfaces (SPI)
   SPI3_Periph : aliased SPI_Peripheral
     with Import, Address => System'To_Address (16#40089000#);

   --  Serial Peripheral Interfaces (SPI)
   SPI4_Periph : aliased SPI_Peripheral
     with Import, Address => System'To_Address (16#4008A000#);

   --  Serial Peripheral Interfaces (SPI)
   SPI5_Periph : aliased SPI_Peripheral
     with Import, Address => System'To_Address (16#40096000#);

   --  Serial Peripheral Interfaces (SPI)
   SPI6_Periph : aliased SPI_Peripheral
     with Import, Address => System'To_Address (16#40097000#);

   --  Serial Peripheral Interfaces (SPI)
   SPI7_Periph : aliased SPI_Peripheral
     with Import, Address => System'To_Address (16#40098000#);

   --  Serial Peripheral Interfaces (SPI)
   SPI8_Periph : aliased SPI_Peripheral
     with Import, Address => System'To_Address (16#4009F000#);

end NXP_SVD.SPI;
