--  Copyright 2016-2019 NXP
--  All rights reserved.SPDX-License-Identifier: BSD-3-Clause


--  This spec has been automatically generated from LPC55S6x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NXP_SVD.USART is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  USART Enable.
   type CFG_ENABLE_Field is
     (
      --  Disabled. The USART is disabled and the internal state machine and
      --  counters are reset. While Enable = 0, all USART interrupts and DMA
      --  transfers are disabled. When Enable is set again, CFG and most other
      --  control bits remain unchanged. When re-enabled, the USART will
      --  immediately be ready to transmit because the transmitter has been
      --  reset and is therefore available.
      Disabled,
      --  Enabled. The USART is enabled for operation.
      Enabled)
     with Size => 1;
   for CFG_ENABLE_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Selects the data size for the USART.
   type CFG_DATALEN_Field is
     (
      --  7 bit Data length.
      Bit_7,
      --  8 bit Data length.
      Bit_8,
      --  9 bit data length. The 9th bit is commonly used for addressing in
      --  multidrop mode. See the ADDRDET bit in the CTL register.
      Bit_9)
     with Size => 2;
   for CFG_DATALEN_Field use
     (Bit_7 => 0,
      Bit_8 => 1,
      Bit_9 => 2);

   --  Selects what type of parity is used by the USART.
   type CFG_PARITYSEL_Field is
     (
      --  No parity.
      No_Parity,
      --  Even parity. Adds a bit to each character such that the number of 1s
      --  in a transmitted character is even, and the number of 1s in a
      --  received character is expected to be even.
      Even_Parity,
      --  Odd parity. Adds a bit to each character such that the number of 1s
      --  in a transmitted character is odd, and the number of 1s in a received
      --  character is expected to be odd.
      Odd_Parity)
     with Size => 2;
   for CFG_PARITYSEL_Field use
     (No_Parity => 0,
      Even_Parity => 2,
      Odd_Parity => 3);

   --  Number of stop bits appended to transmitted data. Only a single stop bit
   --  is required for received data.
   type CFG_STOPLEN_Field is
     (
      --  1 stop bit.
      Bit_1,
      --  2 stop bits. This setting should only be used for asynchronous
      --  communication.
      Bits_2)
     with Size => 1;
   for CFG_STOPLEN_Field use
     (Bit_1 => 0,
      Bits_2 => 1);

   --  Selects standard or 32 kHz clocking mode.
   type CFG_MODE32K_Field is
     (
      --  Disabled. USART uses standard clocking.
      Disabled,
      --  Enabled. USART uses the 32 kHz clock from the RTC oscillator as the
      --  clock source to the BRG, and uses a special bit clocking scheme.
      Enabled)
     with Size => 1;
   for CFG_MODE32K_Field use
     (Disabled => 0,
      Enabled => 1);

   --  LIN break mode enable.
   type CFG_LINMODE_Field is
     (
      --  Disabled. Break detect and generate is configured for normal
      --  operation.
      Disabled,
      --  Enabled. Break detect and generate is configured for LIN bus
      --  operation.
      Enabled)
     with Size => 1;
   for CFG_LINMODE_Field use
     (Disabled => 0,
      Enabled => 1);

   --  CTS Enable. Determines whether CTS is used for flow control. CTS can be
   --  from the input pin, or from the USART's own RTS if loopback mode is
   --  enabled.
   type CFG_CTSEN_Field is
     (
      --  No flow control. The transmitter does not receive any automatic flow
      --  control signal.
      Disabled,
      --  Flow control enabled. The transmitter uses the CTS input (or RTS
      --  output in loopback mode) for flow control purposes.
      Enabled)
     with Size => 1;
   for CFG_CTSEN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Selects synchronous or asynchronous operation.
   type CFG_SYNCEN_Field is
     (
      --  Asynchronous mode.
      Asynchronous_Mode,
      --  Synchronous mode.
      Synchronous_Mode)
     with Size => 1;
   for CFG_SYNCEN_Field use
     (Asynchronous_Mode => 0,
      Synchronous_Mode => 1);

   --  Selects the clock polarity and sampling edge of received data in
   --  synchronous mode.
   type CFG_CLKPOL_Field is
     (
      --  Falling edge. Un_RXD is sampled on the falling edge of SCLK.
      Falling_Edge,
      --  Rising edge. Un_RXD is sampled on the rising edge of SCLK.
      Rising_Edge)
     with Size => 1;
   for CFG_CLKPOL_Field use
     (Falling_Edge => 0,
      Rising_Edge => 1);

   --  Synchronous mode Master select.
   type CFG_SYNCMST_Field is
     (
      --  Slave. When synchronous mode is enabled, the USART is a slave.
      Slave,
      --  Master. When synchronous mode is enabled, the USART is a master.
      Master)
     with Size => 1;
   for CFG_SYNCMST_Field use
     (Slave => 0,
      Master => 1);

   --  Selects data loopback mode.
   type CFG_LOOP_Field is
     (
      --  Normal operation.
      Normal,
      --  Loopback mode. This provides a mechanism to perform diagnostic
      --  loopback testing for USART data. Serial data from the transmitter
      --  (Un_TXD) is connected internally to serial input of the receive
      --  (Un_RXD). Un_TXD and Un_RTS activity will also appear on external
      --  pins if these functions are configured to appear on device pins. The
      --  receiver RTS signal is also looped back to CTS and performs flow
      --  control if enabled by CTSEN.
      Loopback)
     with Size => 1;
   for CFG_LOOP_Field use
     (Normal => 0,
      Loopback => 1);

   --  Output Enable Turnaround time enable for RS-485 operation.
   type CFG_OETA_Field is
     (
      --  Disabled. If selected by OESEL, the Output Enable signal deasserted
      --  at the end of the last stop bit of a transmission.
      Disabled,
      --  Enabled. If selected by OESEL, the Output Enable signal remains
      --  asserted for one character time after the end of the last stop bit of
      --  a transmission. OE will also remain asserted if another transmit
      --  begins before it is deasserted.
      Enabled)
     with Size => 1;
   for CFG_OETA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Automatic Address matching enable.
   type CFG_AUTOADDR_Field is
     (
      --  Disabled. When addressing is enabled by ADDRDET, address matching is
      --  done by software. This provides the possibility of versatile
      --  addressing (e.g. respond to more than one address).
      Disabled,
      --  Enabled. When addressing is enabled by ADDRDET, address matching is
      --  done by hardware, using the value in the ADDR register as the address
      --  to match.
      Enabled)
     with Size => 1;
   for CFG_AUTOADDR_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Output Enable Select.
   type CFG_OESEL_Field is
     (
      --  Standard. The RTS signal is used as the standard flow control
      --  function.
      Standard,
      --  RS-485. The RTS signal configured to provide an output enable signal
      --  to control an RS-485 transceiver.
      Rs_485)
     with Size => 1;
   for CFG_OESEL_Field use
     (Standard => 0,
      Rs_485 => 1);

   --  Output Enable Polarity.
   type CFG_OEPOL_Field is
     (
      --  Low. If selected by OESEL, the output enable is active low.
      Low,
      --  High. If selected by OESEL, the output enable is active high.
      High)
     with Size => 1;
   for CFG_OEPOL_Field use
     (Low => 0,
      High => 1);

   --  Receive data polarity.
   type CFG_RXPOL_Field is
     (
      --  Standard. The RX signal is used as it arrives from the pin. This
      --  means that the RX rest value is 1, start bit is 0, data is not
      --  inverted, and the stop bit is 1.
      Standard,
      --  Inverted. The RX signal is inverted before being used by the USART.
      --  This means that the RX rest value is 0, start bit is 1, data is
      --  inverted, and the stop bit is 0.
      Inverted)
     with Size => 1;
   for CFG_RXPOL_Field use
     (Standard => 0,
      Inverted => 1);

   --  Transmit data polarity.
   type CFG_TXPOL_Field is
     (
      --  Standard. The TX signal is sent out without change. This means that
      --  the TX rest value is 1, start bit is 0, data is not inverted, and the
      --  stop bit is 1.
      Standard,
      --  Inverted. The TX signal is inverted by the USART before being sent
      --  out. This means that the TX rest value is 0, start bit is 1, data is
      --  inverted, and the stop bit is 0.
      Inverted)
     with Size => 1;
   for CFG_TXPOL_Field use
     (Standard => 0,
      Inverted => 1);

   --  USART Configuration register. Basic USART configuration settings that
   --  typically are not changed during operation.
   type CFG_Register is record
      --  USART Enable.
      ENABLE         : CFG_ENABLE_Field := NXP_SVD.USART.Disabled;
      --  unspecified
      Reserved_1_1   : HAL.Bit := 16#0#;
      --  Selects the data size for the USART.
      DATALEN        : CFG_DATALEN_Field := NXP_SVD.USART.Bit_7;
      --  Selects what type of parity is used by the USART.
      PARITYSEL      : CFG_PARITYSEL_Field := NXP_SVD.USART.No_Parity;
      --  Number of stop bits appended to transmitted data. Only a single stop
      --  bit is required for received data.
      STOPLEN        : CFG_STOPLEN_Field := NXP_SVD.USART.Bit_1;
      --  Selects standard or 32 kHz clocking mode.
      MODE32K        : CFG_MODE32K_Field := NXP_SVD.USART.Disabled;
      --  LIN break mode enable.
      LINMODE        : CFG_LINMODE_Field := NXP_SVD.USART.Disabled;
      --  CTS Enable. Determines whether CTS is used for flow control. CTS can
      --  be from the input pin, or from the USART's own RTS if loopback mode
      --  is enabled.
      CTSEN          : CFG_CTSEN_Field := NXP_SVD.USART.Disabled;
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  Selects synchronous or asynchronous operation.
      SYNCEN         : CFG_SYNCEN_Field := NXP_SVD.USART.Asynchronous_Mode;
      --  Selects the clock polarity and sampling edge of received data in
      --  synchronous mode.
      CLKPOL         : CFG_CLKPOL_Field := NXP_SVD.USART.Falling_Edge;
      --  unspecified
      Reserved_13_13 : HAL.Bit := 16#0#;
      --  Synchronous mode Master select.
      SYNCMST        : CFG_SYNCMST_Field := NXP_SVD.USART.Slave;
      --  Selects data loopback mode.
      LOOP_k         : CFG_LOOP_Field := NXP_SVD.USART.Normal;
      --  unspecified
      Reserved_16_17 : HAL.UInt2 := 16#0#;
      --  Output Enable Turnaround time enable for RS-485 operation.
      OETA           : CFG_OETA_Field := NXP_SVD.USART.Disabled;
      --  Automatic Address matching enable.
      AUTOADDR       : CFG_AUTOADDR_Field := NXP_SVD.USART.Disabled;
      --  Output Enable Select.
      OESEL          : CFG_OESEL_Field := NXP_SVD.USART.Standard;
      --  Output Enable Polarity.
      OEPOL          : CFG_OEPOL_Field := NXP_SVD.USART.Low;
      --  Receive data polarity.
      RXPOL          : CFG_RXPOL_Field := NXP_SVD.USART.Standard;
      --  Transmit data polarity.
      TXPOL          : CFG_TXPOL_Field := NXP_SVD.USART.Standard;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CFG_Register use record
      ENABLE         at 0 range 0 .. 0;
      Reserved_1_1   at 0 range 1 .. 1;
      DATALEN        at 0 range 2 .. 3;
      PARITYSEL      at 0 range 4 .. 5;
      STOPLEN        at 0 range 6 .. 6;
      MODE32K        at 0 range 7 .. 7;
      LINMODE        at 0 range 8 .. 8;
      CTSEN          at 0 range 9 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      SYNCEN         at 0 range 11 .. 11;
      CLKPOL         at 0 range 12 .. 12;
      Reserved_13_13 at 0 range 13 .. 13;
      SYNCMST        at 0 range 14 .. 14;
      LOOP_k         at 0 range 15 .. 15;
      Reserved_16_17 at 0 range 16 .. 17;
      OETA           at 0 range 18 .. 18;
      AUTOADDR       at 0 range 19 .. 19;
      OESEL          at 0 range 20 .. 20;
      OEPOL          at 0 range 21 .. 21;
      RXPOL          at 0 range 22 .. 22;
      TXPOL          at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  Break Enable.
   type CTL_TXBRKEN_Field is
     (
      --  Normal operation.
      Normal,
      --  Continuous break. Continuous break is sent immediately when this bit
      --  is set, and remains until this bit is cleared. A break may be sent
      --  without danger of corrupting any currently transmitting character if
      --  the transmitter is first disabled (TXDIS in CTL is set) and then
      --  waiting for the transmitter to be disabled (TXDISINT in STAT = 1)
      --  before writing 1 to TXBRKEN.
      Continous)
     with Size => 1;
   for CTL_TXBRKEN_Field use
     (Normal => 0,
      Continous => 1);

   --  Enable address detect mode.
   type CTL_ADDRDET_Field is
     (
      --  Disabled. The USART presents all incoming data.
      Disabled,
      --  Enabled. The USART receiver ignores incoming data that does not have
      --  the most significant bit of the data (typically the 9th bit) = 1.
      --  When the data MSB bit = 1, the receiver treats the incoming data
      --  normally, generating a received data interrupt. Software can then
      --  check the data to see if this is an address that should be handled.
      --  If it is, the ADDRDET bit is cleared by software and further incoming
      --  data is handled normally.
      Enabled)
     with Size => 1;
   for CTL_ADDRDET_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Transmit Disable.
   type CTL_TXDIS_Field is
     (
      --  Not disabled. USART transmitter is not disabled.
      Enabled,
      --  Disabled. USART transmitter is disabled after any character currently
      --  being transmitted is complete. This feature can be used to facilitate
      --  software flow control.
      Disabled)
     with Size => 1;
   for CTL_TXDIS_Field use
     (Enabled => 0,
      Disabled => 1);

   --  Continuous Clock generation. By default, SCLK is only output while data
   --  is being transmitted in synchronous mode.
   type CTL_CC_Field is
     (
      --  Clock on character. In synchronous mode, SCLK cycles only when
      --  characters are being sent on Un_TXD or to complete a character that
      --  is being received.
      Clock_On_Character,
      --  Continuous clock. SCLK runs continuously in synchronous mode,
      --  allowing characters to be received on Un_RxD independently from
      --  transmission on Un_TXD).
      Continous_Clock)
     with Size => 1;
   for CTL_CC_Field use
     (Clock_On_Character => 0,
      Continous_Clock => 1);

   --  Clear Continuous Clock.
   type CTL_CLRCCONRX_Field is
     (
      --  No effect. No effect on the CC bit.
      No_Effect,
      --  Auto-clear. The CC bit is automatically cleared when a complete
      --  character has been received. This bit is cleared at the same time.
      Auto_Clear)
     with Size => 1;
   for CTL_CLRCCONRX_Field use
     (No_Effect => 0,
      Auto_Clear => 1);

   --  Autobaud enable.
   type CTL_AUTOBAUD_Field is
     (
      --  Disabled. USART is in normal operating mode.
      Disabled,
      --  Enabled. USART is in autobaud mode. This bit should only be set when
      --  the USART receiver is idle. The first start bit of RX is measured and
      --  used the update the BRG register to match the received data rate.
      --  AUTOBAUD is cleared once this process is complete, or if there is an
      --  AERR.
      Enabled)
     with Size => 1;
   for CTL_AUTOBAUD_Field use
     (Disabled => 0,
      Enabled => 1);

   --  USART Control register. USART control settings that are more likely to
   --  change during operation.
   type CTL_Register is record
      --  unspecified
      Reserved_0_0   : HAL.Bit := 16#0#;
      --  Break Enable.
      TXBRKEN        : CTL_TXBRKEN_Field := NXP_SVD.USART.Normal;
      --  Enable address detect mode.
      ADDRDET        : CTL_ADDRDET_Field := NXP_SVD.USART.Disabled;
      --  unspecified
      Reserved_3_5   : HAL.UInt3 := 16#0#;
      --  Transmit Disable.
      TXDIS          : CTL_TXDIS_Field := NXP_SVD.USART.Enabled;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Continuous Clock generation. By default, SCLK is only output while
      --  data is being transmitted in synchronous mode.
      CC             : CTL_CC_Field := NXP_SVD.USART.Clock_On_Character;
      --  Clear Continuous Clock.
      CLRCCONRX      : CTL_CLRCCONRX_Field := NXP_SVD.USART.No_Effect;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Autobaud enable.
      AUTOBAUD       : CTL_AUTOBAUD_Field := NXP_SVD.USART.Disabled;
      --  unspecified
      Reserved_17_31 : HAL.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CTL_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      TXBRKEN        at 0 range 1 .. 1;
      ADDRDET        at 0 range 2 .. 2;
      Reserved_3_5   at 0 range 3 .. 5;
      TXDIS          at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      CC             at 0 range 8 .. 8;
      CLRCCONRX      at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      AUTOBAUD       at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   --  USART Status register. The complete status value can be read here.
   --  Writing ones clears some bits in the register. Some bits can be cleared
   --  by writing a 1 to them.
   type STAT_Register is record
      --  unspecified
      Reserved_0_0   : HAL.Bit := 16#0#;
      --  Read-only. Receiver Idle. When 0, indicates that the receiver is
      --  currently in the process of receiving data. When 1, indicates that
      --  the receiver is not currently in the process of receiving data.
      RXIDLE         : Boolean := True;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  Read-only. Transmitter Idle. When 0, indicates that the transmitter
      --  is currently in the process of sending data.When 1, indicate that the
      --  transmitter is not currently in the process of sending data.
      TXIDLE         : Boolean := True;
      --  Read-only. This bit reflects the current state of the CTS signal,
      --  regardless of the setting of the CTSEN bit in the CFG register. This
      --  will be the value of the CTS input pin unless loopback mode is
      --  enabled.
      CTS            : Boolean := False;
      --  Write-only. This bit is set when a change in the state is detected
      --  for the CTS flag above. This bit is cleared by software.
      DELTACTS       : Boolean := False;
      --  Read-only. Transmitter Disabled Status flag. When 1, this bit
      --  indicates that the USART transmitter is fully idle after being
      --  disabled via the TXDIS bit in the CFG register (TXDIS = 1).
      TXDISSTAT      : Boolean := False;
      --  unspecified
      Reserved_7_9   : HAL.UInt3 := 16#0#;
      --  Read-only. Received Break. This bit reflects the current state of the
      --  receiver break detection logic. It is set when the Un_RXD pin remains
      --  low for 16 bit times. Note that FRAMERRINT will also be set when this
      --  condition occurs because the stop bit(s) for the character would be
      --  missing. RXBRK is cleared when the Un_RXD pin goes high.
      RXBRK          : Boolean := False;
      --  Write-only. This bit is set when a change in the state of receiver
      --  break detection occurs. Cleared by software.
      DELTARXBRK     : Boolean := False;
      --  Write-only. This bit is set when a start is detected on the receiver
      --  input. Its purpose is primarily to allow wake-up from Deep-sleep or
      --  Power-down mode immediately when a start is detected. Cleared by
      --  software.
      START          : Boolean := False;
      --  Write-only. Framing Error interrupt flag. This flag is set when a
      --  character is received with a missing stop bit at the expected
      --  location. This could be an indication of a baud rate or configuration
      --  mismatch with the transmitting source.
      FRAMERRINT     : Boolean := False;
      --  Write-only. Parity Error interrupt flag. This flag is set when a
      --  parity error is detected in a received character.
      PARITYERRINT   : Boolean := False;
      --  Write-only. Received Noise interrupt flag. Three samples of received
      --  data are taken in order to determine the value of each received data
      --  bit, except in synchronous mode. This acts as a noise filter if one
      --  sample disagrees. This flag is set when a received data bit contains
      --  one disagreeing sample. This could indicate line noise, a baud rate
      --  or character format mismatch, or loss of synchronization during data
      --  reception.
      RXNOISEINT     : Boolean := False;
      --  Write-only. Auto baud Error. An auto baud error can occur if the BRG
      --  counts to its limit before the end of the start bit that is being
      --  measured, essentially an auto baud time-out.
      ABERR          : Boolean := False;
      --  unspecified
      Reserved_17_31 : HAL.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for STAT_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      RXIDLE         at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      TXIDLE         at 0 range 3 .. 3;
      CTS            at 0 range 4 .. 4;
      DELTACTS       at 0 range 5 .. 5;
      TXDISSTAT      at 0 range 6 .. 6;
      Reserved_7_9   at 0 range 7 .. 9;
      RXBRK          at 0 range 10 .. 10;
      DELTARXBRK     at 0 range 11 .. 11;
      START          at 0 range 12 .. 12;
      FRAMERRINT     at 0 range 13 .. 13;
      PARITYERRINT   at 0 range 14 .. 14;
      RXNOISEINT     at 0 range 15 .. 15;
      ABERR          at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   --  Interrupt Enable read and Set register for USART (not FIFO) status.
   --  Contains individual interrupt enable bits for each potential USART
   --  interrupt. A complete value may be read from this register. Writing a 1
   --  to any implemented bit position causes that bit to be set.
   type INTENSET_Register is record
      --  unspecified
      Reserved_0_2   : HAL.UInt3 := 16#0#;
      --  When 1, enables an interrupt when the transmitter becomes idle
      --  (TXIDLE = 1).
      TXIDLEEN       : Boolean := False;
      --  unspecified
      Reserved_4_4   : HAL.Bit := 16#0#;
      --  When 1, enables an interrupt when there is a change in the state of
      --  the CTS input.
      DELTACTSEN     : Boolean := False;
      --  When 1, enables an interrupt when the transmitter is fully disabled
      --  as indicated by the TXDISINT flag in STAT. See description of the
      --  TXDISINT bit for details.
      TXDISEN        : Boolean := False;
      --  unspecified
      Reserved_7_10  : HAL.UInt4 := 16#0#;
      --  When 1, enables an interrupt when a change of state has occurred in
      --  the detection of a received break condition (break condition asserted
      --  or deasserted).
      DELTARXBRKEN   : Boolean := False;
      --  When 1, enables an interrupt when a received start bit has been
      --  detected.
      STARTEN        : Boolean := False;
      --  When 1, enables an interrupt when a framing error has been detected.
      FRAMERREN      : Boolean := False;
      --  When 1, enables an interrupt when a parity error has been detected.
      PARITYERREN    : Boolean := False;
      --  When 1, enables an interrupt when noise is detected. See description
      --  of the RXNOISEINT bit in Table 354.
      RXNOISEEN      : Boolean := False;
      --  When 1, enables an interrupt when an auto baud error occurs.
      ABERREN        : Boolean := False;
      --  unspecified
      Reserved_17_31 : HAL.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENSET_Register use record
      Reserved_0_2   at 0 range 0 .. 2;
      TXIDLEEN       at 0 range 3 .. 3;
      Reserved_4_4   at 0 range 4 .. 4;
      DELTACTSEN     at 0 range 5 .. 5;
      TXDISEN        at 0 range 6 .. 6;
      Reserved_7_10  at 0 range 7 .. 10;
      DELTARXBRKEN   at 0 range 11 .. 11;
      STARTEN        at 0 range 12 .. 12;
      FRAMERREN      at 0 range 13 .. 13;
      PARITYERREN    at 0 range 14 .. 14;
      RXNOISEEN      at 0 range 15 .. 15;
      ABERREN        at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   --  Interrupt Enable Clear register. Allows clearing any combination of bits
   --  in the INTENSET register. Writing a 1 to any implemented bit position
   --  causes the corresponding bit to be cleared.
   type INTENCLR_Register is record
      --  unspecified
      Reserved_0_2   : HAL.UInt3 := 16#0#;
      --  Write-only. Writing 1 clears the corresponding bit in the INTENSET
      --  register.
      TXIDLECLR      : Boolean := False;
      --  unspecified
      Reserved_4_4   : HAL.Bit := 16#0#;
      --  Write-only. Writing 1 clears the corresponding bit in the INTENSET
      --  register.
      DELTACTSCLR    : Boolean := False;
      --  Write-only. Writing 1 clears the corresponding bit in the INTENSET
      --  register.
      TXDISCLR       : Boolean := False;
      --  unspecified
      Reserved_7_10  : HAL.UInt4 := 16#0#;
      --  Write-only. Writing 1 clears the corresponding bit in the INTENSET
      --  register.
      DELTARXBRKCLR  : Boolean := False;
      --  Write-only. Writing 1 clears the corresponding bit in the INTENSET
      --  register.
      STARTCLR       : Boolean := False;
      --  Write-only. Writing 1 clears the corresponding bit in the INTENSET
      --  register.
      FRAMERRCLR     : Boolean := False;
      --  Write-only. Writing 1 clears the corresponding bit in the INTENSET
      --  register.
      PARITYERRCLR   : Boolean := False;
      --  Write-only. Writing 1 clears the corresponding bit in the INTENSET
      --  register.
      RXNOISECLR     : Boolean := False;
      --  Write-only. Writing 1 clears the corresponding bit in the INTENSET
      --  register.
      ABERRCLR       : Boolean := False;
      --  unspecified
      Reserved_17_31 : HAL.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENCLR_Register use record
      Reserved_0_2   at 0 range 0 .. 2;
      TXIDLECLR      at 0 range 3 .. 3;
      Reserved_4_4   at 0 range 4 .. 4;
      DELTACTSCLR    at 0 range 5 .. 5;
      TXDISCLR       at 0 range 6 .. 6;
      Reserved_7_10  at 0 range 7 .. 10;
      DELTARXBRKCLR  at 0 range 11 .. 11;
      STARTCLR       at 0 range 12 .. 12;
      FRAMERRCLR     at 0 range 13 .. 13;
      PARITYERRCLR   at 0 range 14 .. 14;
      RXNOISECLR     at 0 range 15 .. 15;
      ABERRCLR       at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype BRG_BRGVAL_Field is HAL.UInt16;

   --  Baud Rate Generator register. 16-bit integer baud rate divisor value.
   type BRG_Register is record
      --  This value is used to divide the USART input clock to determine the
      --  baud rate, based on the input clock from the FRG. 0 = FCLK is used
      --  directly by the USART function. 1 = FCLK is divided by 2 before use
      --  by the USART function. 2 = FCLK is divided by 3 before use by the
      --  USART function. 0xFFFF = FCLK is divided by 65,536 before use by the
      --  USART function.
      BRGVAL         : BRG_BRGVAL_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for BRG_Register use record
      BRGVAL         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  Interrupt status register. Reflects interrupts that are currently
   --  enabled.
   type INTSTAT_Register is record
      --  unspecified
      Reserved_0_2   : HAL.UInt3;
      --  Read-only. Transmitter Idle status.
      TXIDLE         : Boolean;
      --  unspecified
      Reserved_4_4   : HAL.Bit;
      --  Read-only. This bit is set when a change in the state of the CTS
      --  input is detected.
      DELTACTS       : Boolean;
      --  Read-only. Transmitter Disabled Interrupt flag.
      TXDISINT       : Boolean;
      --  unspecified
      Reserved_7_10  : HAL.UInt4;
      --  Read-only. This bit is set when a change in the state of receiver
      --  break detection occurs.
      DELTARXBRK     : Boolean;
      --  Read-only. This bit is set when a start is detected on the receiver
      --  input.
      START          : Boolean;
      --  Read-only. Framing Error interrupt flag.
      FRAMERRINT     : Boolean;
      --  Read-only. Parity Error interrupt flag.
      PARITYERRINT   : Boolean;
      --  Read-only. Received Noise interrupt flag.
      RXNOISEINT     : Boolean;
      --  Read-only. Auto baud Error Interrupt flag.
      ABERRINT       : Boolean;
      --  unspecified
      Reserved_17_31 : HAL.UInt15;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTSTAT_Register use record
      Reserved_0_2   at 0 range 0 .. 2;
      TXIDLE         at 0 range 3 .. 3;
      Reserved_4_4   at 0 range 4 .. 4;
      DELTACTS       at 0 range 5 .. 5;
      TXDISINT       at 0 range 6 .. 6;
      Reserved_7_10  at 0 range 7 .. 10;
      DELTARXBRK     at 0 range 11 .. 11;
      START          at 0 range 12 .. 12;
      FRAMERRINT     at 0 range 13 .. 13;
      PARITYERRINT   at 0 range 14 .. 14;
      RXNOISEINT     at 0 range 15 .. 15;
      ABERRINT       at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype OSR_OSRVAL_Field is HAL.UInt4;

   --  Oversample selection register for asynchronous communication.
   type OSR_Register is record
      --  Oversample Selection Value. 0 to 3 = not supported 0x4 = 5 function
      --  clocks are used to transmit and receive each data bit. 0x5 = 6
      --  function clocks are used to transmit and receive each data bit. 0xF=
      --  16 function clocks are used to transmit and receive each data bit.
      OSRVAL        : OSR_OSRVAL_Field := 16#F#;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OSR_Register use record
      OSRVAL        at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   subtype ADDR_ADDRESS_Field is HAL.UInt8;

   --  Address register for automatic address matching.
   type ADDR_Register is record
      --  8-bit address used with automatic address matching. Used when address
      --  detection is enabled (ADDRDET in CTL = 1) and automatic address
      --  matching is enabled (AUTOADDR in CFG = 1).
      ADDRESS       : ADDR_ADDRESS_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADDR_Register use record
      ADDRESS       at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
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
      ENABLETX       : FIFOCFG_ENABLETX_Field := NXP_SVD.USART.Disabled;
      --  Enable the receive FIFO.
      ENABLERX       : FIFOCFG_ENABLERX_Field := NXP_SVD.USART.Disabled;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  Read-only. FIFO size configuration. This is a read-only field. 0x0 =
      --  FIFO is configured as 16 entries of 8 bits. 0x1, 0x2, 0x3 = not
      --  applicable to USART.
      SIZE           : FIFOCFG_SIZE_Field := 16#0#;
      --  unspecified
      Reserved_6_11  : HAL.UInt6 := 16#0#;
      --  DMA configuration for transmit.
      DMATX          : FIFOCFG_DMATX_Field := NXP_SVD.USART.Disabled;
      --  DMA configuration for receive.
      DMARX          : FIFOCFG_DMARX_Field := NXP_SVD.USART.Disabled;
      --  Wake-up for transmit FIFO level. This allows the device to be woken
      --  from reduced power modes (up to power-down, as long as the peripheral
      --  function works in that power mode) without enabling the TXLVL
      --  interrupt. Only DMA wakes up, processes data, and goes back to sleep.
      --  The CPU will remain stopped until woken by another cause, such as DMA
      --  completion. See Hardware Wake-up control register.
      WAKETX         : FIFOCFG_WAKETX_Field := NXP_SVD.USART.Disabled;
      --  Wake-up for receive FIFO level. This allows the device to be woken
      --  from reduced power modes (up to power-down, as long as the peripheral
      --  function works in that power mode) without enabling the TXLVL
      --  interrupt. Only DMA wakes up, processes data, and goes back to sleep.
      --  The CPU will remain stopped until woken by another cause, such as DMA
      --  completion. See Hardware Wake-up control register.
      WAKERX         : FIFOCFG_WAKERX_Field := NXP_SVD.USART.Disabled;
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
      TXLVLENA       : FIFOTRIG_TXLVLENA_Field := NXP_SVD.USART.Disabled;
      --  Receive FIFO level trigger enable. This trigger will become an
      --  interrupt if enabled in FIFOINTENSET, or a DMA trigger if DMARX in
      --  FIFOCFG is set.
      RXLVLENA       : FIFOTRIG_RXLVLENA_Field := NXP_SVD.USART.Disabled;
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
      TXERR         : FIFOINTENSET_TXERR_Field := NXP_SVD.USART.Disabled;
      --  Determines whether an interrupt occurs when a receive error occurs,
      --  based on the RXERR flag in the FIFOSTAT register.
      RXERR         : FIFOINTENSET_RXERR_Field := NXP_SVD.USART.Disabled;
      --  Determines whether an interrupt occurs when a the transmit FIFO
      --  reaches the level specified by the TXLVL field in the FIFOTRIG
      --  register.
      TXLVL         : FIFOINTENSET_TXLVL_Field := NXP_SVD.USART.Disabled;
      --  Determines whether an interrupt occurs when a the receive FIFO
      --  reaches the level specified by the TXLVL field in the FIFOTRIG
      --  register.
      RXLVL         : FIFOINTENSET_RXLVL_Field := NXP_SVD.USART.Disabled;
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

   subtype FIFOWR_TXDATA_Field is HAL.UInt9;

   --  FIFO write data.
   type FIFOWR_Register is record
      --  Write-only. Transmit data to the FIFO.
      TXDATA        : FIFOWR_TXDATA_Field := 16#0#;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FIFOWR_Register use record
      TXDATA        at 0 range 0 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   subtype FIFORD_RXDATA_Field is HAL.UInt9;

   --  FIFO read data.
   type FIFORD_Register is record
      --  Read-only. Received data from the FIFO. The number of bits used
      --  depends on the DATALEN and PARITYSEL settings.
      RXDATA         : FIFORD_RXDATA_Field;
      --  unspecified
      Reserved_9_12  : HAL.UInt4;
      --  Read-only. Framing Error status flag. This bit reflects the status
      --  for the data it is read along with from the FIFO, and indicates that
      --  the character was received with a missing stop bit at the expected
      --  location. This could be an indication of a baud rate or configuration
      --  mismatch with the transmitting source.
      FRAMERR        : Boolean;
      --  Read-only. Parity Error status flag. This bit reflects the status for
      --  the data it is read along with from the FIFO. This bit will be set
      --  when a parity error is detected in a received character.
      PARITYERR      : Boolean;
      --  Read-only. Received Noise flag. See description of the RxNoiseInt bit
      --  in Table 354.
      RXNOISE        : Boolean;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FIFORD_Register use record
      RXDATA         at 0 range 0 .. 8;
      Reserved_9_12  at 0 range 9 .. 12;
      FRAMERR        at 0 range 13 .. 13;
      PARITYERR      at 0 range 14 .. 14;
      RXNOISE        at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype FIFORDNOPOP_RXDATA_Field is HAL.UInt9;

   --  FIFO data read with no FIFO pop.
   type FIFORDNOPOP_Register is record
      --  Read-only. Received data from the FIFO. The number of bits used
      --  depends on the DATALEN and PARITYSEL settings.
      RXDATA         : FIFORDNOPOP_RXDATA_Field;
      --  unspecified
      Reserved_9_12  : HAL.UInt4;
      --  Read-only. Framing Error status flag. This bit reflects the status
      --  for the data it is read along with from the FIFO, and indicates that
      --  the character was received with a missing stop bit at the expected
      --  location. This could be an indication of a baud rate or configuration
      --  mismatch with the transmitting source.
      FRAMERR        : Boolean;
      --  Read-only. Parity Error status flag. This bit reflects the status for
      --  the data it is read along with from the FIFO. This bit will be set
      --  when a parity error is detected in a received character.
      PARITYERR      : Boolean;
      --  Read-only. Received Noise flag. See description of the RxNoiseInt bit
      --  in Table 354.
      RXNOISE        : Boolean;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FIFORDNOPOP_Register use record
      RXDATA         at 0 range 0 .. 8;
      Reserved_9_12  at 0 range 9 .. 12;
      FRAMERR        at 0 range 13 .. 13;
      PARITYERR      at 0 range 14 .. 14;
      RXNOISE        at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
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

   --  USARTs
   type USART_Peripheral is record
      --  USART Configuration register. Basic USART configuration settings that
      --  typically are not changed during operation.
      CFG          : aliased CFG_Register;
      --  USART Control register. USART control settings that are more likely
      --  to change during operation.
      CTL          : aliased CTL_Register;
      --  USART Status register. The complete status value can be read here.
      --  Writing ones clears some bits in the register. Some bits can be
      --  cleared by writing a 1 to them.
      STAT         : aliased STAT_Register;
      --  Interrupt Enable read and Set register for USART (not FIFO) status.
      --  Contains individual interrupt enable bits for each potential USART
      --  interrupt. A complete value may be read from this register. Writing a
      --  1 to any implemented bit position causes that bit to be set.
      INTENSET     : aliased INTENSET_Register;
      --  Interrupt Enable Clear register. Allows clearing any combination of
      --  bits in the INTENSET register. Writing a 1 to any implemented bit
      --  position causes the corresponding bit to be cleared.
      INTENCLR     : aliased INTENCLR_Register;
      --  Baud Rate Generator register. 16-bit integer baud rate divisor value.
      BRG          : aliased BRG_Register;
      --  Interrupt status register. Reflects interrupts that are currently
      --  enabled.
      INTSTAT      : aliased INTSTAT_Register;
      --  Oversample selection register for asynchronous communication.
      OSR          : aliased OSR_Register;
      --  Address register for automatic address matching.
      ADDR         : aliased ADDR_Register;
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

   for USART_Peripheral use record
      CFG          at 16#0# range 0 .. 31;
      CTL          at 16#4# range 0 .. 31;
      STAT         at 16#8# range 0 .. 31;
      INTENSET     at 16#C# range 0 .. 31;
      INTENCLR     at 16#10# range 0 .. 31;
      BRG          at 16#20# range 0 .. 31;
      INTSTAT      at 16#24# range 0 .. 31;
      OSR          at 16#28# range 0 .. 31;
      ADDR         at 16#2C# range 0 .. 31;
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

   --  USARTs
   USART0_Periph : aliased USART_Peripheral
     with Import, Address => System'To_Address (16#40086000#);

   --  USARTs
   USART1_Periph : aliased USART_Peripheral
     with Import, Address => System'To_Address (16#40087000#);

   --  USARTs
   USART2_Periph : aliased USART_Peripheral
     with Import, Address => System'To_Address (16#40088000#);

   --  USARTs
   USART3_Periph : aliased USART_Peripheral
     with Import, Address => System'To_Address (16#40089000#);

   --  USARTs
   USART4_Periph : aliased USART_Peripheral
     with Import, Address => System'To_Address (16#4008A000#);

   --  USARTs
   USART5_Periph : aliased USART_Peripheral
     with Import, Address => System'To_Address (16#40096000#);

   --  USARTs
   USART6_Periph : aliased USART_Peripheral
     with Import, Address => System'To_Address (16#40097000#);

   --  USARTs
   USART7_Periph : aliased USART_Peripheral
     with Import, Address => System'To_Address (16#40098000#);

end NXP_SVD.USART;
