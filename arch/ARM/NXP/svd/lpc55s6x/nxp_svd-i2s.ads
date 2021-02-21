--  Copyright 2016-2019 NXP
--  All rights reserved.SPDX-License-Identifier: BSD-3-Clause


--  This spec has been automatically generated from LPC55S6x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NXP_SVD.I2S is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Main enable for I 2S function in this Flexcomm
   type CFG1_MAINENABLE_Field is
     (
      --  All I 2S channel pairs in this Flexcomm are disabled and the internal
      --  state machines, counters, and flags are reset. No other channel pairs
      --  can be enabled.
      Disabled,
      --  This I 2S channel pair is enabled. Other channel pairs in this
      --  Flexcomm may be enabled in their individual PAIRENABLE bits.
      Enabled)
     with Size => 1;
   for CFG1_MAINENABLE_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Data flow Pause. Allows pausing data flow between the I2S
   --  serializer/deserializer and the FIFO. This could be done in order to
   --  change streams, or while restarting after a data underflow or overflow.
   --  When paused, FIFO operations can be done without corrupting data that is
   --  in the process of being sent or received. Once a data pause has been
   --  requested, the interface may need to complete sending data that was in
   --  progress before interrupting the flow of data. Software must check that
   --  the pause is actually in effect before taking action. This is done by
   --  monitoring the DATAPAUSED flag in the STAT register. When DATAPAUSE is
   --  cleared, data transfer will resume at the beginning of the next frame.
   type CFG1_DATAPAUSE_Field is
     (
      --  Normal operation, or resuming normal operation at the next frame if
      --  the I2S has already been paused.
      Normal,
      --  A pause in the data flow is being requested. It is in effect when
      --  DATAPAUSED in STAT = 1.
      Pause)
     with Size => 1;
   for CFG1_DATAPAUSE_Field use
     (Normal => 0,
      Pause => 1);

   --  Provides the number of I2S channel pairs in this Flexcomm This is a
   --  read-only field whose value may be different in other Flexcomms. 00 =
   --  there is 1 I2S channel pair in this Flexcomm. 01 = there are 2 I2S
   --  channel pairs in this Flexcomm. 10 = there are 3 I2S channel pairs in
   --  this Flexcomm. 11 = there are 4 I2S channel pairs in this Flexcomm.
   type CFG1_PAIRCOUNT_Field is
     (
      --  1 I2S channel pairs in this flexcomm
      Pairs_1,
      --  2 I2S channel pairs in this flexcomm
      Pairs_2,
      --  3 I2S channel pairs in this flexcomm
      Pairs_3,
      --  4 I2S channel pairs in this flexcomm
      Pairs_4)
     with Size => 2;
   for CFG1_PAIRCOUNT_Field use
     (Pairs_1 => 0,
      Pairs_2 => 1,
      Pairs_3 => 2,
      Pairs_4 => 3);

   --  Master / slave configuration selection, determining how SCK and WS are
   --  used by all channel pairs in this Flexcomm.
   type CFG1_MSTSLVCFG_Field is
     (
      --  Normal slave mode, the default mode. SCK and WS are received from a
      --  master and used to transmit or receive data.
      Normal_Slave_Mode,
      --  WS synchronized master. WS is received from another master and used
      --  to synchronize the generation of SCK, when divided from the Flexcomm
      --  function clock.
      Ws_Sync_Master,
      --  Master using an existing SCK. SCK is received and used directly to
      --  generate WS, as well as transmitting or receiving data.
      Master_Using_Sck,
      --  Normal master mode. SCK and WS are generated so they can be sent to
      --  one or more slave devices.
      Normal_Master)
     with Size => 2;
   for CFG1_MSTSLVCFG_Field use
     (Normal_Slave_Mode => 0,
      Ws_Sync_Master => 1,
      Master_Using_Sck => 2,
      Normal_Master => 3);

   --  Selects the basic I2S operating mode. Other configurations modify this
   --  to obtain all supported cases. See Formats and modes for examples.
   type CFG1_MODE_Field is
     (
      --  I2S mode a.k.a. 'classic' mode. WS has a 50% duty cycle, with (for
      --  each enabled channel pair) one piece of left channel data occurring
      --  during the first phase, and one pieces of right channel data
      --  occurring during the second phase. In this mode, the data region
      --  begins one clock after the leading WS edge for the frame. For a 50%
      --  WS duty cycle, FRAMELEN must define an even number of I2S clocks for
      --  the frame. If FRAMELEN defines an odd number of clocks per frame, the
      --  extra clock will occur on the right.
      Classic_Mode,
      --  DSP mode where WS has a 50% duty cycle. See remark for mode 0.
      Dsp_Mode_Ws_50_Dutycycle,
      --  DSP mode where WS has a one clock long pulse at the beginning of each
      --  data frame.
      Dsp_Mode_Ws_1_Clock,
      --  DSP mode where WS has a one data slot long pulse at the beginning of
      --  each data frame.
      Dsp_Mode_Ws_1_Data)
     with Size => 2;
   for CFG1_MODE_Field use
     (Classic_Mode => 0,
      Dsp_Mode_Ws_50_Dutycycle => 1,
      Dsp_Mode_Ws_1_Clock => 2,
      Dsp_Mode_Ws_1_Data => 3);

   --  Right channel data is in the Low portion of FIFO data. Essentially, this
   --  swaps left and right channel data as it is transferred to or from the
   --  FIFO. This bit is not used if the data width is greater than 24 bits or
   --  if PDMDATA = 1. Note that if the ONECHANNEL field (bit 10 of this
   --  register) = 1, the one channel to be used is the nominally the left
   --  channel. POSITION can still place that data in the frame where right
   --  channel data is normally located. if all enabled channel pairs have
   --  ONECHANNEL = 1, then RIGHTLOW = 1 is not allowed.
   type CFG1_RIGHTLOW_Field is
     (
      --  The right channel is taken from the high part of the FIFO data. For
      --  example, when data is 16 bits, FIFO bits 31:16 are used for the right
      --  channel.
      Right_High,
      --  The right channel is taken from the low part of the FIFO data. For
      --  example, when data is 16 bits, FIFO bits 15:0 are used for the right
      --  channel.
      Right_Low)
     with Size => 1;
   for CFG1_RIGHTLOW_Field use
     (Right_High => 0,
      Right_Low => 1);

   --  Left Justify data.
   type CFG1_LEFTJUST_Field is
     (
      --  Data is transferred between the FIFO and the I2S
      --  serializer/deserializer right justified, i.e. starting from bit 0 and
      --  continuing to the position defined by DATALEN. This would correspond
      --  to right justified data in the stream on the data bus.
      Right_Justified,
      --  Data is transferred between the FIFO and the I2S
      --  serializer/deserializer left justified, i.e. starting from the MSB of
      --  the FIFO entry and continuing for the number of bits defined by
      --  DATALEN. This would correspond to left justified data in the stream
      --  on the data bus.
      Left_Justified)
     with Size => 1;
   for CFG1_LEFTJUST_Field use
     (Right_Justified => 0,
      Left_Justified => 1);

   --  Single channel mode. Applies to both transmit and receive. This
   --  configuration bit applies only to the first I2S channel pair. Other
   --  channel pairs may select this mode independently in their separate CFG1
   --  registers.
   type CFG1_ONECHANNEL_Field is
     (
      --  I2S data for this channel pair is treated as left and right channels.
      Dual_Channel,
      --  I2S data for this channel pair is treated as a single channel,
      --  functionally the left channel for this pair. In mode 0 only, the
      --  right side of the frame begins at POSITION = 0x100. This is because
      --  mode 0 makes a clear distinction between the left and right sides of
      --  the frame. When ONECHANNEL = 1, the single channel of data may be
      --  placed on the right by setting POSITION to 0x100 + the data position
      --  within the right side (e.g. 0x108 would place data starting at the
      --  8th clock after the middle of the frame). In other modes, data for
      --  the single channel of data is placed at the clock defined by
      --  POSITION.
      Single_Channel)
     with Size => 1;
   for CFG1_ONECHANNEL_Field use
     (Dual_Channel => 0,
      Single_Channel => 1);

   --  SCK polarity.
   type CFG1_SCK_POL_Field is
     (
      --  Data is launched on SCK falling edges and sampled on SCK rising edges
      --  (standard for I2S).
      Falling_Edge,
      --  Data is launched on SCK rising edges and sampled on SCK falling
      --  edges.
      Rising_Edge)
     with Size => 1;
   for CFG1_SCK_POL_Field use
     (Falling_Edge => 0,
      Rising_Edge => 1);

   --  WS polarity.
   type CFG1_WS_POL_Field is
     (
      --  Data frames begin at a falling edge of WS (standard for classic I2S).
      Not_Inverted,
      --  WS is inverted, resulting in a data frame beginning at a rising edge
      --  of WS (standard for most 'non-classic' variations of I2S).
      Inverted)
     with Size => 1;
   for CFG1_WS_POL_Field use
     (Not_Inverted => 0,
      Inverted => 1);

   subtype CFG1_DATALEN_Field is HAL.UInt5;

   --  Configuration register 1 for the primary channel pair.
   type CFG1_Register is record
      --  Main enable for I 2S function in this Flexcomm
      MAINENABLE     : CFG1_MAINENABLE_Field := NXP_SVD.I2S.Disabled;
      --  Data flow Pause. Allows pausing data flow between the I2S
      --  serializer/deserializer and the FIFO. This could be done in order to
      --  change streams, or while restarting after a data underflow or
      --  overflow. When paused, FIFO operations can be done without corrupting
      --  data that is in the process of being sent or received. Once a data
      --  pause has been requested, the interface may need to complete sending
      --  data that was in progress before interrupting the flow of data.
      --  Software must check that the pause is actually in effect before
      --  taking action. This is done by monitoring the DATAPAUSED flag in the
      --  STAT register. When DATAPAUSE is cleared, data transfer will resume
      --  at the beginning of the next frame.
      DATAPAUSE      : CFG1_DATAPAUSE_Field := NXP_SVD.I2S.Normal;
      --  Provides the number of I2S channel pairs in this Flexcomm This is a
      --  read-only field whose value may be different in other Flexcomms. 00 =
      --  there is 1 I2S channel pair in this Flexcomm. 01 = there are 2 I2S
      --  channel pairs in this Flexcomm. 10 = there are 3 I2S channel pairs in
      --  this Flexcomm. 11 = there are 4 I2S channel pairs in this Flexcomm.
      PAIRCOUNT      : CFG1_PAIRCOUNT_Field := NXP_SVD.I2S.Pairs_1;
      --  Master / slave configuration selection, determining how SCK and WS
      --  are used by all channel pairs in this Flexcomm.
      MSTSLVCFG      : CFG1_MSTSLVCFG_Field := NXP_SVD.I2S.Normal_Slave_Mode;
      --  Selects the basic I2S operating mode. Other configurations modify
      --  this to obtain all supported cases. See Formats and modes for
      --  examples.
      MODE           : CFG1_MODE_Field := NXP_SVD.I2S.Classic_Mode;
      --  Right channel data is in the Low portion of FIFO data. Essentially,
      --  this swaps left and right channel data as it is transferred to or
      --  from the FIFO. This bit is not used if the data width is greater than
      --  24 bits or if PDMDATA = 1. Note that if the ONECHANNEL field (bit 10
      --  of this register) = 1, the one channel to be used is the nominally
      --  the left channel. POSITION can still place that data in the frame
      --  where right channel data is normally located. if all enabled channel
      --  pairs have ONECHANNEL = 1, then RIGHTLOW = 1 is not allowed.
      RIGHTLOW       : CFG1_RIGHTLOW_Field := NXP_SVD.I2S.Right_High;
      --  Left Justify data.
      LEFTJUST       : CFG1_LEFTJUST_Field := NXP_SVD.I2S.Right_Justified;
      --  Single channel mode. Applies to both transmit and receive. This
      --  configuration bit applies only to the first I2S channel pair. Other
      --  channel pairs may select this mode independently in their separate
      --  CFG1 registers.
      ONECHANNEL     : CFG1_ONECHANNEL_Field := NXP_SVD.I2S.Dual_Channel;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  SCK polarity.
      SCK_POL        : CFG1_SCK_POL_Field := NXP_SVD.I2S.Falling_Edge;
      --  WS polarity.
      WS_POL         : CFG1_WS_POL_Field := NXP_SVD.I2S.Not_Inverted;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  Data Length, minus 1 encoded, defines the number of data bits to be
      --  transmitted or received for all I2S channel pairs in this Flexcomm.
      --  Note that data is only driven to or received from SDA for the number
      --  of bits defined by DATALEN. DATALEN is also used in these ways by the
      --  I2S: Determines the size of data transfers between the FIFO and the
      --  I2S serializer/deserializer. See FIFO buffer configurations and usage
      --  In mode 1, 2, and 3, determines the location of right data following
      --  left data in the frame. In mode 3 (where WS has a one data slot long
      --  pulse at the beginning of each data frame) determines the duration of
      --  the WS pulse. Values: 0x00 to 0x02 = not supported 0x03 = data is 4
      --  bits in length 0x04 = data is 5 bits in length 0x1F = data is 32 bits
      --  in length
      DATALEN        : CFG1_DATALEN_Field := 16#0#;
      --  unspecified
      Reserved_21_31 : HAL.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CFG1_Register use record
      MAINENABLE     at 0 range 0 .. 0;
      DATAPAUSE      at 0 range 1 .. 1;
      PAIRCOUNT      at 0 range 2 .. 3;
      MSTSLVCFG      at 0 range 4 .. 5;
      MODE           at 0 range 6 .. 7;
      RIGHTLOW       at 0 range 8 .. 8;
      LEFTJUST       at 0 range 9 .. 9;
      ONECHANNEL     at 0 range 10 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      SCK_POL        at 0 range 12 .. 12;
      WS_POL         at 0 range 13 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      DATALEN        at 0 range 16 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   subtype CFG2_FRAMELEN_Field is HAL.UInt9;
   subtype CFG2_POSITION_Field is HAL.UInt9;

   --  Configuration register 2 for the primary channel pair.
   type CFG2_Register is record
      --  Frame Length, minus 1 encoded, defines the number of clocks and data
      --  bits in the frames that this channel pair participates in. See Frame
      --  format. 0x000 to 0x002 = not supported 0x003 = frame is 4 bits in
      --  total length 0x004 = frame is 5 bits in total length 0x1FF = frame is
      --  512 bits in total length if FRAMELEN is an defines an odd length
      --  frame (e.g. 33 clocks) in mode 0 or 1, the extra clock appears in the
      --  right half. When MODE = 3, FRAMELEN must be larger than DATALEN in
      --  order for the WS pulse to be generated correctly.
      FRAMELEN       : CFG2_FRAMELEN_Field := 16#0#;
      --  unspecified
      Reserved_9_15  : HAL.UInt7 := 16#0#;
      --  Data Position. Defines the location within the frame of the data for
      --  this channel pair. POSITION + DATALEN must be less than FRAMELEN. See
      --  Frame format. When MODE = 0, POSITION defines the location of data in
      --  both the left phase and right phase, starting one clock after the WS
      --  edge. In other modes, POSITION defines the location of data within
      --  the entire frame. ONECHANNEL = 1 while MODE = 0 is a special case,
      --  see the description of ONECHANNEL. The combination of DATALEN and the
      --  POSITION fields of all channel pairs must be made such that the
      --  channels do not overlap within the frame. 0x000 = data begins at bit
      --  position 0 (the first bit position) within the frame or WS phase.
      --  0x001 = data begins at bit position 1 within the frame or WS phase.
      --  0x002 = data begins at bit position 2 within the frame or WS phase.
      POSITION       : CFG2_POSITION_Field := 16#0#;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CFG2_Register use record
      FRAMELEN       at 0 range 0 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      POSITION       at 0 range 16 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  Busy status for the primary channel pair. Other BUSY flags may be found
   --  in the STAT register for each channel pair.
   type STAT_BUSY_Field is
     (
      --  The transmitter/receiver for channel pair is currently idle.
      Idle,
      --  The transmitter/receiver for channel pair is currently processing
      --  data.
      Busy)
     with Size => 1;
   for STAT_BUSY_Field use
     (Idle => 0,
      Busy => 1);

   --  Slave Frame Error flag. This applies when at least one channel pair is
   --  operating as a slave. An error indicates that the incoming WS signal did
   --  not transition as expected due to a mismatch between FRAMELEN and the
   --  actual incoming I2S stream.
   type STAT_SLVFRMERR_Field is
     (
      --  No error has been recorded.
      No_Error,
      --  An error has been recorded for some channel pair that is operating in
      --  slave mode. ERROR is cleared by writing a 1 to this bit position.
      Error)
     with Size => 1;
   for STAT_SLVFRMERR_Field use
     (No_Error => 0,
      Error => 1);

   --  Left/Right indication. This flag is considered to be a debugging aid and
   --  is not expected to be used by an I2S driver. Valid when one channel pair
   --  is busy. Indicates left or right data being processed for the currently
   --  busy channel pair.
   type STAT_LR_Field is
     (
      --  Left channel.
      Left_Channel,
      --  Right channel.
      Right_Channel)
     with Size => 1;
   for STAT_LR_Field use
     (Left_Channel => 0,
      Right_Channel => 1);

   --  Data Paused status flag. Applies to all I2S channels
   type STAT_DATAPAUSED_Field is
     (
      --  Data is not currently paused. A data pause may have been requested
      --  but is not yet in force, waiting for an allowed pause point. Refer to
      --  the description of the DATAPAUSE control bit in the CFG1 register.
      Not_Paused,
      --  A data pause has been requested and is now in force.
      Paused)
     with Size => 1;
   for STAT_DATAPAUSED_Field use
     (Not_Paused => 0,
      Paused => 1);

   --  Status register for the primary channel pair.
   type STAT_Register is record
      --  Read-only. Busy status for the primary channel pair. Other BUSY flags
      --  may be found in the STAT register for each channel pair.
      BUSY          : STAT_BUSY_Field := NXP_SVD.I2S.Idle;
      --  Write-only. Slave Frame Error flag. This applies when at least one
      --  channel pair is operating as a slave. An error indicates that the
      --  incoming WS signal did not transition as expected due to a mismatch
      --  between FRAMELEN and the actual incoming I2S stream.
      SLVFRMERR     : STAT_SLVFRMERR_Field := NXP_SVD.I2S.No_Error;
      --  Read-only. Left/Right indication. This flag is considered to be a
      --  debugging aid and is not expected to be used by an I2S driver. Valid
      --  when one channel pair is busy. Indicates left or right data being
      --  processed for the currently busy channel pair.
      LR            : STAT_LR_Field := NXP_SVD.I2S.Left_Channel;
      --  Read-only. Data Paused status flag. Applies to all I2S channels
      DATAPAUSED    : STAT_DATAPAUSED_Field := NXP_SVD.I2S.Not_Paused;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for STAT_Register use record
      BUSY          at 0 range 0 .. 0;
      SLVFRMERR     at 0 range 1 .. 1;
      LR            at 0 range 2 .. 2;
      DATAPAUSED    at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   subtype DIV_DIV_Field is HAL.UInt12;

   --  Clock divider, used by all channel pairs.
   type DIV_Register is record
      --  This field controls how this I2S block uses the Flexcomm function
      --  clock. 0x000 = The Flexcomm function clock is used directly. 0x001 =
      --  The Flexcomm function clock is divided by 2. 0x002 = The Flexcomm
      --  function clock is divided by 3. 0xFFF = The Flexcomm function clock
      --  is divided by 4,096.
      DIV            : DIV_DIV_Field := 16#0#;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DIV_Register use record
      DIV            at 0 range 0 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
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

   --  Transmit I2S empty 0. Determines the value sent by the I2S in transmit
   --  mode if the TX FIFO becomes empty. This value is sent repeatedly until
   --  the I2S is paused, the error is cleared, new data is provided, and the
   --  I2S is un-paused.
   type FIFOCFG_TXI2SE0_Field is
     (
      --  If the TX FIFO becomes empty, the last value is sent. This setting
      --  may be used when the data length is 24 bits or less, or when MONO = 1
      --  for this channel pair.
      Last_Value,
      --  If the TX FIFO becomes empty, 0 is sent. Use if the data length is
      --  greater than 24 bits or if zero fill is preferred.
      Zero)
     with Size => 1;
   for FIFOCFG_TXI2SE0_Field use
     (Last_Value => 0,
      Zero => 1);

   --  Packing format for 48-bit data. This relates to how data is entered into
   --  or taken from the FIFO by software or DMA.
   type FIFOCFG_PACK48_Field is
     (
      --  48-bit I2S FIFO entries are handled as all 24-bit values.
      Bit_24,
      --  48-bit I2S FIFO entries are handled as alternating 32-bit and 16-bit
      --  values.
      Bit_32_16)
     with Size => 1;
   for FIFOCFG_PACK48_Field use
     (Bit_24 => 0,
      Bit_32_16 => 1);

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
      ENABLETX       : FIFOCFG_ENABLETX_Field := NXP_SVD.I2S.Disabled;
      --  Enable the receive FIFO.
      ENABLERX       : FIFOCFG_ENABLERX_Field := NXP_SVD.I2S.Disabled;
      --  Transmit I2S empty 0. Determines the value sent by the I2S in
      --  transmit mode if the TX FIFO becomes empty. This value is sent
      --  repeatedly until the I2S is paused, the error is cleared, new data is
      --  provided, and the I2S is un-paused.
      TXI2SE0        : FIFOCFG_TXI2SE0_Field := NXP_SVD.I2S.Last_Value;
      --  Packing format for 48-bit data. This relates to how data is entered
      --  into or taken from the FIFO by software or DMA.
      PACK48         : FIFOCFG_PACK48_Field := NXP_SVD.I2S.Bit_24;
      --  Read-only. FIFO size configuration. This is a read-only field. 0x0 =
      --  FIFO is configured as 16 entries of 8 bits. 0x1, 0x2, 0x3 = not
      --  applicable to USART.
      SIZE           : FIFOCFG_SIZE_Field := 16#0#;
      --  unspecified
      Reserved_6_11  : HAL.UInt6 := 16#0#;
      --  DMA configuration for transmit.
      DMATX          : FIFOCFG_DMATX_Field := NXP_SVD.I2S.Disabled;
      --  DMA configuration for receive.
      DMARX          : FIFOCFG_DMARX_Field := NXP_SVD.I2S.Disabled;
      --  Wake-up for transmit FIFO level. This allows the device to be woken
      --  from reduced power modes (up to power-down, as long as the peripheral
      --  function works in that power mode) without enabling the TXLVL
      --  interrupt. Only DMA wakes up, processes data, and goes back to sleep.
      --  The CPU will remain stopped until woken by another cause, such as DMA
      --  completion. See Hardware Wake-up control register.
      WAKETX         : FIFOCFG_WAKETX_Field := NXP_SVD.I2S.Disabled;
      --  Wake-up for receive FIFO level. This allows the device to be woken
      --  from reduced power modes (up to power-down, as long as the peripheral
      --  function works in that power mode) without enabling the TXLVL
      --  interrupt. Only DMA wakes up, processes data, and goes back to sleep.
      --  The CPU will remain stopped until woken by another cause, such as DMA
      --  completion. See Hardware Wake-up control register.
      WAKERX         : FIFOCFG_WAKERX_Field := NXP_SVD.I2S.Disabled;
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
      TXI2SE0        at 0 range 2 .. 2;
      PACK48         at 0 range 3 .. 3;
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
      TXLVLENA       : FIFOTRIG_TXLVLENA_Field := NXP_SVD.I2S.Disabled;
      --  Receive FIFO level trigger enable. This trigger will become an
      --  interrupt if enabled in FIFOINTENSET, or a DMA trigger if DMARX in
      --  FIFOCFG is set.
      RXLVLENA       : FIFOTRIG_RXLVLENA_Field := NXP_SVD.I2S.Disabled;
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
      TXERR         : FIFOINTENSET_TXERR_Field := NXP_SVD.I2S.Disabled;
      --  Determines whether an interrupt occurs when a receive error occurs,
      --  based on the RXERR flag in the FIFOSTAT register.
      RXERR         : FIFOINTENSET_RXERR_Field := NXP_SVD.I2S.Disabled;
      --  Determines whether an interrupt occurs when a the transmit FIFO
      --  reaches the level specified by the TXLVL field in the FIFOTRIG
      --  register.
      TXLVL         : FIFOINTENSET_TXLVL_Field := NXP_SVD.I2S.Disabled;
      --  Determines whether an interrupt occurs when a the receive FIFO
      --  reaches the level specified by the TXLVL field in the FIFOTRIG
      --  register.
      RXLVL         : FIFOINTENSET_RXLVL_Field := NXP_SVD.I2S.Disabled;
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

   subtype FIFOWR48H_TXDATA_Field is HAL.UInt24;

   --  FIFO write data for upper data bits. May only be used if the I2S is
   --  configured for 2x 24-bit data and not using DMA.
   type FIFOWR48H_Register is record
      --  Write-only. Transmit data to the FIFO. Whether this register is used
      --  and the number of bits used depends on configuration details.
      TXDATA         : FIFOWR48H_TXDATA_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FIFOWR48H_Register use record
      TXDATA         at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype FIFORD48H_RXDATA_Field is HAL.UInt24;

   --  FIFO read data for upper data bits. May only be used if the I2S is
   --  configured for 2x 24-bit data and not using DMA.
   type FIFORD48H_Register is record
      --  Read-only. Received data from the FIFO. Whether this register is used
      --  and the number of bits used depends on configuration details.
      RXDATA         : FIFORD48H_RXDATA_Field;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FIFORD48H_Register use record
      RXDATA         at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype FIFORD48HNOPOP_RXDATA_Field is HAL.UInt24;

   --  FIFO data read for upper data bits with no FIFO pop. May only be used if
   --  the I2S is configured for 2x 24-bit data and not using DMA.
   type FIFORD48HNOPOP_Register is record
      --  Read-only. Received data from the FIFO. Whether this register is used
      --  and the number of bits used depends on configuration details.
      RXDATA         : FIFORD48HNOPOP_RXDATA_Field;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FIFORD48HNOPOP_Register use record
      RXDATA         at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype ID_APERTURE_Field is HAL.UInt8;
   subtype ID_MINOR_REV_Field is HAL.UInt4;
   subtype ID_MAJOR_REV_Field is HAL.UInt4;
   subtype ID_ID_Field is HAL.UInt16;

   --  I2S Module identification
   type ID_Register is record
      --  Read-only. Aperture: encoded as (aperture size/4K) -1, so 0x00 means
      --  a 4K aperture.
      APERTURE  : ID_APERTURE_Field;
      --  Read-only. Minor revision of module implementation, starting at 0.
      MINOR_REV : ID_MINOR_REV_Field;
      --  Read-only. Major revision of module implementation, starting at 0.
      MAJOR_REV : ID_MAJOR_REV_Field;
      --  Read-only. Unique module identifier for this IP block.
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

   --  I2S interface
   type I2S_Peripheral is record
      --  Configuration register 1 for the primary channel pair.
      CFG1           : aliased CFG1_Register;
      --  Configuration register 2 for the primary channel pair.
      CFG2           : aliased CFG2_Register;
      --  Status register for the primary channel pair.
      STAT           : aliased STAT_Register;
      --  Clock divider, used by all channel pairs.
      DIV            : aliased DIV_Register;
      --  FIFO configuration and enable register.
      FIFOCFG        : aliased FIFOCFG_Register;
      --  FIFO status register.
      FIFOSTAT       : aliased FIFOSTAT_Register;
      --  FIFO trigger settings for interrupt and DMA request.
      FIFOTRIG       : aliased FIFOTRIG_Register;
      --  FIFO interrupt enable set (enable) and read register.
      FIFOINTENSET   : aliased FIFOINTENSET_Register;
      --  FIFO interrupt enable clear (disable) and read register.
      FIFOINTENCLR   : aliased FIFOINTENCLR_Register;
      --  FIFO interrupt status register.
      FIFOINTSTAT    : aliased FIFOINTSTAT_Register;
      --  FIFO write data.
      FIFOWR         : aliased HAL.UInt32;
      --  FIFO write data for upper data bits. May only be used if the I2S is
      --  configured for 2x 24-bit data and not using DMA.
      FIFOWR48H      : aliased FIFOWR48H_Register;
      --  FIFO read data.
      FIFORD         : aliased HAL.UInt32;
      --  FIFO read data for upper data bits. May only be used if the I2S is
      --  configured for 2x 24-bit data and not using DMA.
      FIFORD48H      : aliased FIFORD48H_Register;
      --  FIFO data read with no FIFO pop.
      FIFORDNOPOP    : aliased HAL.UInt32;
      --  FIFO data read for upper data bits with no FIFO pop. May only be used
      --  if the I2S is configured for 2x 24-bit data and not using DMA.
      FIFORD48HNOPOP : aliased FIFORD48HNOPOP_Register;
      --  I2S Module identification
      ID             : aliased ID_Register;
   end record
     with Volatile;

   for I2S_Peripheral use record
      CFG1           at 16#C00# range 0 .. 31;
      CFG2           at 16#C04# range 0 .. 31;
      STAT           at 16#C08# range 0 .. 31;
      DIV            at 16#C1C# range 0 .. 31;
      FIFOCFG        at 16#E00# range 0 .. 31;
      FIFOSTAT       at 16#E04# range 0 .. 31;
      FIFOTRIG       at 16#E08# range 0 .. 31;
      FIFOINTENSET   at 16#E10# range 0 .. 31;
      FIFOINTENCLR   at 16#E14# range 0 .. 31;
      FIFOINTSTAT    at 16#E18# range 0 .. 31;
      FIFOWR         at 16#E20# range 0 .. 31;
      FIFOWR48H      at 16#E24# range 0 .. 31;
      FIFORD         at 16#E30# range 0 .. 31;
      FIFORD48H      at 16#E34# range 0 .. 31;
      FIFORDNOPOP    at 16#E40# range 0 .. 31;
      FIFORD48HNOPOP at 16#E44# range 0 .. 31;
      ID             at 16#FFC# range 0 .. 31;
   end record;

   --  I2S interface
   I2S0_Periph : aliased I2S_Peripheral
     with Import, Address => System'To_Address (16#40086000#);

   --  I2S interface
   I2S1_Periph : aliased I2S_Peripheral
     with Import, Address => System'To_Address (16#40087000#);

   --  I2S interface
   I2S2_Periph : aliased I2S_Peripheral
     with Import, Address => System'To_Address (16#40088000#);

   --  I2S interface
   I2S3_Periph : aliased I2S_Peripheral
     with Import, Address => System'To_Address (16#40089000#);

   --  I2S interface
   I2S4_Periph : aliased I2S_Peripheral
     with Import, Address => System'To_Address (16#4008A000#);

   --  I2S interface
   I2S5_Periph : aliased I2S_Peripheral
     with Import, Address => System'To_Address (16#40096000#);

   --  I2S interface
   I2S6_Periph : aliased I2S_Peripheral
     with Import, Address => System'To_Address (16#40097000#);

   --  I2S interface
   I2S7_Periph : aliased I2S_Peripheral
     with Import, Address => System'To_Address (16#40098000#);

end NXP_SVD.I2S;
