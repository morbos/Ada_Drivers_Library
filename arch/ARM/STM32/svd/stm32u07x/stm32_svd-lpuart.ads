--  This spec has been automatically generated from STM32U073.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.LPUART is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype LPUART_CR1_DEDT_Field is HAL.UInt5;
   subtype LPUART_CR1_DEAT_Field is HAL.UInt5;

   --  LPUART control register 1
   type LPUART_CR1_Register is record
      --  LPUART enable When this bit is cleared, the LPUART prescalers and
      --  outputs are stopped immediately, and current operations are
      --  discarded. The configuration of the LPUART is kept, but all the
      --  status flags, in the LPUART_ISR are reset. This bit is set and
      --  cleared by software. Note: To enter low-power mode without generating
      --  errors on the line, the TE bit must be reset before and the software
      --  must wait for the TC bit in the LPUART_ISR to be set before resetting
      --  the UE bit. Note: The DMA requests are also reset when UE = 0 so the
      --  DMA channel must be disabled before resetting the UE bit.
      UE             : Boolean := False;
      --  LPUART enable in low-power mode When this bit is cleared, the LPUART
      --  cannot wake up the MCU from low-power mode. When this bit is set, the
      --  LPUART can wake up the MCU from low-power mode. This bit is set and
      --  cleared by software. Note: It is recommended to set the UESM bit just
      --  before entering low-power mode, and clear it when exiting low-power
      --  mode.
      UESM           : Boolean := False;
      --  Receiver enable This bit enables the receiver. It is set and cleared
      --  by software.
      RE             : Boolean := False;
      --  Transmitter enable This bit enables the transmitter. It is set and
      --  cleared by software. Note: During transmission, a low pulse on the TE
      --  bit (0 followed by 1) sends a preamble (idle line) after the current
      --  word, except in Smartcard mode. In order to generate an idle
      --  character, the TE must not be immediately written to 1. To ensure the
      --  required duration, the software can poll the TEACK bit in the
      --  LPUART_ISR register. Note: In Smartcard mode, when TE is set, there
      --  is a 1 bit-time delay before the transmission starts.
      TE             : Boolean := False;
      --  IDLE interrupt enable This bit is set and cleared by software.
      IDLEIE         : Boolean := False;
      --  RXFIFO not empty interrupt enable This bit is set and cleared by
      --  software.
      RXFNEIE        : Boolean := False;
      --  Transmission complete interrupt enable This bit is set and cleared by
      --  software.
      TCIE           : Boolean := False;
      --  TXFIFO not full interrupt enable This bit is set and cleared by
      --  software.
      TXFNFIE        : Boolean := False;
      --  PE interrupt enable This bit is set and cleared by software.
      PEIE           : Boolean := False;
      --  Parity selection This bit selects the odd or even parity when the
      --  parity generation/detection is enabled (PCE bit set). It is set and
      --  cleared by software. The parity is selected after the current byte.
      --  This bitfield can only be written when the LPUART is disabled (UE=0).
      PS             : Boolean := False;
      --  Parity control enable This bit selects the hardware parity control
      --  (generation and detection). When the parity control is enabled, the
      --  computed parity is inserted at the MSB position (9th bit if M=1; 8th
      --  bit if M=0) and parity is checked on the received data. This bit is
      --  set and cleared by software. Once it is set, PCE is active after the
      --  current byte (in reception and in transmission). This bitfield can
      --  only be written when the LPUART is disabled (UE=0).
      PCE            : Boolean := False;
      --  Receiver wake-up method This bit determines the LPUART wake-up method
      --  from Mute mode. It is set or cleared by software. This bitfield can
      --  only be written when the LPUART is disabled (UE=0).
      WAKE           : Boolean := False;
      --  Word length This bit is used in conjunction with bit 28 (M1) to
      --  determine the word length. It is set or cleared by software (refer to
      --  bit 28 (M1) description). This bit can only be written when the
      --  LPUART is disabled (UE=0).
      M0             : Boolean := False;
      --  Mute mode enable This bit activates the Mute mode function of the
      --  LPUART. When set, the LPUART can switch between the active and Mute
      --  modes, as defined by the WAKE bit. It is set and cleared by software.
      MME            : Boolean := False;
      --  Character match interrupt enable This bit is set and cleared by
      --  software.
      CMIE           : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Driver Enable deassertion time This 5-bit value defines the time
      --  between the end of the last stop bit, in a transmitted message, and
      --  the de-activation of the DE (Driver Enable) signal.It is expressed in
      --  lpuart_ker_ck clock cycles. For more details, refer Section132.4.14:
      --  RS232 Hardware flow control and RS485 Driver Enable. If the
      --  LPUART_TDR register is written during the DEDT time, the new data is
      --  transmitted only when the DEDT and DEAT times have both elapsed. This
      --  bitfield can only be written when the LPUART is disabled (UE=0).
      DEDT           : LPUART_CR1_DEDT_Field := 16#0#;
      --  Driver Enable assertion time This 5-bit value defines the time
      --  between the activation of the DE (Driver Enable) signal and the
      --  beginning of the start bit. It is expressed in lpuart_ker_ck clock
      --  cycles. For more details, refer Section131.5.21: RS232 Hardware flow
      --  control and RS485 Driver Enable. This bitfield can only be written
      --  when the LPUART is disabled (UE=0).
      DEAT           : LPUART_CR1_DEAT_Field := 16#0#;
      --  unspecified
      Reserved_26_27 : HAL.UInt2 := 16#0#;
      --  Word length This bit must be used in conjunction with bit 12 (M0) to
      --  determine the word length. It is set or cleared by software. M[1:0] =
      --  00: 1 Start bit, 8 Data bits, n Stop bit M[1:0] = 01: 1 Start bit, 9
      --  Data bits, n Stop bit M[1:0] = 10: 1 Start bit, 7 Data bits, n Stop
      --  bit This bit can only be written when the LPUART is disabled (UE=0).
      --  Note: In 7-bit data length mode, the Smartcard mode, LIN master mode
      --  and auto baud rate (0x7F and 0x55 frames detection) are not
      --  supported.
      M1             : Boolean := False;
      --  FIFO mode enable This bit is set and cleared by software.
      FIFOEN         : Boolean := False;
      --  TXFIFO empty interrupt enable This bit is set and cleared by
      --  software.
      TXFEIE         : Boolean := False;
      --  RXFIFO Full interrupt enable This bit is set and cleared by software.
      RXFFIE         : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPUART_CR1_Register use record
      UE             at 0 range 0 .. 0;
      UESM           at 0 range 1 .. 1;
      RE             at 0 range 2 .. 2;
      TE             at 0 range 3 .. 3;
      IDLEIE         at 0 range 4 .. 4;
      RXFNEIE        at 0 range 5 .. 5;
      TCIE           at 0 range 6 .. 6;
      TXFNFIE        at 0 range 7 .. 7;
      PEIE           at 0 range 8 .. 8;
      PS             at 0 range 9 .. 9;
      PCE            at 0 range 10 .. 10;
      WAKE           at 0 range 11 .. 11;
      M0             at 0 range 12 .. 12;
      MME            at 0 range 13 .. 13;
      CMIE           at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      DEDT           at 0 range 16 .. 20;
      DEAT           at 0 range 21 .. 25;
      Reserved_26_27 at 0 range 26 .. 27;
      M1             at 0 range 28 .. 28;
      FIFOEN         at 0 range 29 .. 29;
      TXFEIE         at 0 range 30 .. 30;
      RXFFIE         at 0 range 31 .. 31;
   end record;

   subtype LPUART_CR1_ALTERNATE_DEDT_Field is HAL.UInt5;
   subtype LPUART_CR1_ALTERNATE_DEAT_Field is HAL.UInt5;

   --  LPUART control register 1
   type LPUART_CR1_ALTERNATE_Register is record
      --  LPUART enable When this bit is cleared, the LPUART prescalers and
      --  outputs are stopped immediately, and current operations are
      --  discarded. The configuration of the LPUART is kept, but all the
      --  status flags, in the LPUART_ISR are reset. This bit is set and
      --  cleared by software. Note: To enter low-power mode without generating
      --  errors on the line, the TE bit must be reset before and the software
      --  must wait for the TC bit in the LPUART_ISR to be set before resetting
      --  the UE bit. Note: The DMA requests are also reset when UE = 0 so the
      --  DMA channel must be disabled before resetting the UE bit.
      UE             : Boolean := False;
      --  LPUART enable in low-power mode When this bit is cleared, the LPUART
      --  cannot wake up the MCU from low-power mode. When this bit is set, the
      --  LPUART can wake up the MCU from low-power mode. This bit is set and
      --  cleared by software. Note: It is recommended to set the UESM bit just
      --  before entering low-power mode, and clear it when exiting low-power
      --  mode.
      UESM           : Boolean := False;
      --  Receiver enable This bit enables the receiver. It is set and cleared
      --  by software.
      RE             : Boolean := False;
      --  Transmitter enable This bit enables the transmitter. It is set and
      --  cleared by software. Note: During transmission, a low pulse on the TE
      --  bit (0 followed by 1) sends a preamble (idle line) after the current
      --  word, except in Smartcard mode. In order to generate an idle
      --  character, the TE must not be immediately written to 1. To ensure the
      --  required duration, the software can poll the TEACK bit in the
      --  LPUART_ISR register. Note: In Smartcard mode, when TE is set, there
      --  is a 1 bit-time delay before the transmission starts.
      TE             : Boolean := False;
      --  IDLE interrupt enable This bit is set and cleared by software.
      IDLEIE         : Boolean := False;
      --  Receive data register not empty This bit is set and cleared by
      --  software.
      RXNEIE         : Boolean := False;
      --  Transmission complete interrupt enable This bit is set and cleared by
      --  software.
      TCIE           : Boolean := False;
      --  Transmit data register empty This bit is set and cleared by software.
      TXEIE          : Boolean := False;
      --  PE interrupt enable This bit is set and cleared by software.
      PEIE           : Boolean := False;
      --  Parity selection This bit selects the odd or even parity when the
      --  parity generation/detection is enabled (PCE bit set). It is set and
      --  cleared by software. The parity is selected after the current byte.
      --  This bitfield can only be written when the LPUART is disabled (UE=0).
      PS             : Boolean := False;
      --  Parity control enable This bit selects the hardware parity control
      --  (generation and detection). When the parity control is enabled, the
      --  computed parity is inserted at the MSB position (9th bit if M=1; 8th
      --  bit if M=0) and parity is checked on the received data. This bit is
      --  set and cleared by software. Once it is set, PCE is active after the
      --  current byte (in reception and in transmission). This bitfield can
      --  only be written when the LPUART is disabled (UE=0).
      PCE            : Boolean := False;
      --  Receiver wake-up method This bit determines the LPUART wake-up method
      --  from Mute mode. It is set or cleared by software. This bitfield can
      --  only be written when the LPUART is disabled (UE=0).
      WAKE           : Boolean := False;
      --  Word length This bit is used in conjunction with bit 28 (M1) to
      --  determine the word length. It is set or cleared by software (refer to
      --  bit 28 (M1) description). This bit can only be written when the
      --  LPUART is disabled (UE=0).
      M0             : Boolean := False;
      --  Mute mode enable This bit activates the Mute mode function of the
      --  LPUART. When set, the LPUART can switch between the active and Mute
      --  modes, as defined by the WAKE bit. It is set and cleared by software.
      MME            : Boolean := False;
      --  Character match interrupt enable This bit is set and cleared by
      --  software.
      CMIE           : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Driver Enable deassertion time This 5-bit value defines the time
      --  between the end of the last stop bit, in a transmitted message, and
      --  the de-activation of the DE (Driver Enable) signal.It is expressed in
      --  lpuart_ker_ck clock cycles. For more details, refer Section132.4.14:
      --  RS232 Hardware flow control and RS485 Driver Enable. If the
      --  LPUART_TDR register is written during the DEDT time, the new data is
      --  transmitted only when the DEDT and DEAT times have both elapsed. This
      --  bitfield can only be written when the LPUART is disabled (UE=0).
      DEDT           : LPUART_CR1_ALTERNATE_DEDT_Field := 16#0#;
      --  Driver Enable assertion time This 5-bit value defines the time
      --  between the activation of the DE (Driver Enable) signal and the
      --  beginning of the start bit. It is expressed in lpuart_ker_ck clock
      --  cycles. For more details, refer Section131.5.21: RS232 Hardware flow
      --  control and RS485 Driver Enable. This bitfield can only be written
      --  when the LPUART is disabled (UE=0).
      DEAT           : LPUART_CR1_ALTERNATE_DEAT_Field := 16#0#;
      --  unspecified
      Reserved_26_27 : HAL.UInt2 := 16#0#;
      --  Word length This bit must be used in conjunction with bit 12 (M0) to
      --  determine the word length. It is set or cleared by software. M[1:0] =
      --  00: 1 Start bit, 8 Data bits, n Stop bit M[1:0] = 01: 1 Start bit, 9
      --  Data bits, n Stop bit M[1:0] = 10: 1 Start bit, 7 Data bits, n Stop
      --  bit This bit can only be written when the LPUART is disabled (UE=0).
      --  Note: In 7-bit data length mode, the Smartcard mode, LIN master mode
      --  and auto baud rate (0x7F and 0x55 frames detection) are not
      --  supported.
      M1             : Boolean := False;
      --  FIFO mode enable This bit is set and cleared by software.
      FIFOEN         : Boolean := False;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPUART_CR1_ALTERNATE_Register use record
      UE             at 0 range 0 .. 0;
      UESM           at 0 range 1 .. 1;
      RE             at 0 range 2 .. 2;
      TE             at 0 range 3 .. 3;
      IDLEIE         at 0 range 4 .. 4;
      RXNEIE         at 0 range 5 .. 5;
      TCIE           at 0 range 6 .. 6;
      TXEIE          at 0 range 7 .. 7;
      PEIE           at 0 range 8 .. 8;
      PS             at 0 range 9 .. 9;
      PCE            at 0 range 10 .. 10;
      WAKE           at 0 range 11 .. 11;
      M0             at 0 range 12 .. 12;
      MME            at 0 range 13 .. 13;
      CMIE           at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      DEDT           at 0 range 16 .. 20;
      DEAT           at 0 range 21 .. 25;
      Reserved_26_27 at 0 range 26 .. 27;
      M1             at 0 range 28 .. 28;
      FIFOEN         at 0 range 29 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype LPUART_CR2_STOP_Field is HAL.UInt2;
   subtype LPUART_CR2_ADD_Field is HAL.UInt8;

   --  LPUART control register 2
   type LPUART_CR2_Register is record
      --  unspecified
      Reserved_0_3   : HAL.UInt4 := 16#0#;
      --  7-bit Address Detection/4-bit Address Detection This bit is for
      --  selection between 4-bit address detection or 7-bit address detection.
      --  This bit can only be written when the LPUART is disabled (UE=0) Note:
      --  In 7-bit and 9-bit data modes, the address detection is done on 6-bit
      --  and 8-bit address (ADD[5:0] and ADD[7:0]) respectively.
      ADDM7          : Boolean := False;
      --  unspecified
      Reserved_5_11  : HAL.UInt7 := 16#0#;
      --  STOP bits These bits are used for programming the stop bits. This
      --  bitfield can only be written when the LPUART is disabled (UE=0).
      STOP           : LPUART_CR2_STOP_Field := 16#0#;
      --  unspecified
      Reserved_14_14 : HAL.Bit := 16#0#;
      --  Swap TX/RX pins This bit is set and cleared by software. This
      --  bitfield can only be written when the LPUART is disabled (UE=0).
      SWAP           : Boolean := False;
      --  RX pin active level inversion This bit is set and cleared by
      --  software. This enables the use of an external inverter on the RX
      --  line. This bitfield can only be written when the LPUART is disabled
      --  (UE=0).
      RXINV          : Boolean := False;
      --  TX pin active level inversion This bit is set and cleared by
      --  software. This enables the use of an external inverter on the TX
      --  line. This bitfield can only be written when the LPUART is disabled
      --  (UE=0).
      TXINV          : Boolean := False;
      --  Binary data inversion This bit is set and cleared by software. This
      --  bitfield can only be written when the LPUART is disabled (UE=0).
      DATAINV        : Boolean := False;
      --  Most significant bit first This bit is set and cleared by software.
      --  This bitfield can only be written when the LPUART is disabled (UE=0).
      MSBFIRST       : Boolean := False;
      --  unspecified
      Reserved_20_23 : HAL.UInt4 := 16#0#;
      --  Address of the LPUART node These bits give the address of the LPUART
      --  node in Mute mode or a character code to be recognized in low-power
      --  or Run mode: In Mute mode: they are used in multiprocessor
      --  communication to wake up from Mute mode with 4-bit/7-bit address mark
      --  detection. The MSB of the character sent by the transmitter should be
      --  equal to 1. In 4-bit address mark detection, only ADD[3:0] bits are
      --  used. In low-power mode: they are used for wake up from low-power
      --  mode on character match. When WUS[1:0] is programmed to 0b00 (WUF
      --  active on address match), the wake-up from low-power mode is
      --  performed when the received character corresponds to the character
      --  programmed through ADD[6:0] or ADD[3:0] bitfield (depending on ADDM7
      --  bit), and WUF interrupt is enabled by setting WUFIE bit. The MSB of
      --  the character sent by transmitter should be equal to 1. In Run mode
      --  with Mute mode inactive (for example, end-of-block detection in
      --  ModBus protocol): the whole received character (8 bits) is compared
      --  to ADD[7:0] value and CMF flag is set on match. An interrupt is
      --  generated if the CMIE bit is set. These bits can only be written when
      --  the reception is disabled (RE1=10) or when the USART is disabled
      --  (UE1=10).
      ADD            : LPUART_CR2_ADD_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPUART_CR2_Register use record
      Reserved_0_3   at 0 range 0 .. 3;
      ADDM7          at 0 range 4 .. 4;
      Reserved_5_11  at 0 range 5 .. 11;
      STOP           at 0 range 12 .. 13;
      Reserved_14_14 at 0 range 14 .. 14;
      SWAP           at 0 range 15 .. 15;
      RXINV          at 0 range 16 .. 16;
      TXINV          at 0 range 17 .. 17;
      DATAINV        at 0 range 18 .. 18;
      MSBFIRST       at 0 range 19 .. 19;
      Reserved_20_23 at 0 range 20 .. 23;
      ADD            at 0 range 24 .. 31;
   end record;

   --  LPUART_CR3_WUS array
   type LPUART_CR3_WUS_Field_Array is array (0 .. 1) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for LPUART_CR3_WUS
   type LPUART_CR3_WUS_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  WUS as a value
            Val : HAL.UInt2;
         when True =>
            --  WUS as an array
            Arr : LPUART_CR3_WUS_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for LPUART_CR3_WUS_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   subtype LPUART_CR3_RXFTCFG_Field is HAL.UInt3;
   subtype LPUART_CR3_TXFTCFG_Field is HAL.UInt3;

   --  LPUART control register 3
   type LPUART_CR3_Register is record
      --  Error interrupt enable Error Interrupt Enable Bit is required to
      --  enable interrupt generation in case of a framing error, overrun error
      --  or noise flag (FE=1 or ORE=1 or NE=1 in the LPUART_ISR register).
      EIE            : Boolean := False;
      --  unspecified
      Reserved_1_2   : HAL.UInt2 := 16#0#;
      --  Half-duplex selection Selection of Single-wire Half-duplex mode This
      --  bit can only be written when the LPUART is disabled (UE=0).
      HDSEL          : Boolean := False;
      --  unspecified
      Reserved_4_5   : HAL.UInt2 := 16#0#;
      --  DMA enable receiver This bit is set/reset by software
      DMAR           : Boolean := False;
      --  DMA enable transmitter This bit is set/reset by software
      DMAT           : Boolean := False;
      --  RTS enable This bit can only be written when the LPUART is disabled
      --  (UE=0).
      RTSE           : Boolean := False;
      --  CTS enable This bit can only be written when the LPUART is disabled
      --  (UE=0)
      CTSE           : Boolean := False;
      --  CTS interrupt enable
      CTSIE          : Boolean := False;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  Overrun Disable This bit is used to disable the receive overrun
      --  detection. the ORE flag is not set and the new received data
      --  overwrites the previous content of the LPUART_RDR register. This bit
      --  can only be written when the LPUART is disabled (UE=0). Note: This
      --  control bit enables checking the communication flow w/o reading the
      --  data.
      OVRDIS         : Boolean := False;
      --  DMA Disable on Reception Error This bit can only be written when the
      --  LPUART is disabled (UE=0). Note: The reception errors are: parity
      --  error, framing error or noise error.
      DDRE           : Boolean := False;
      --  Driver enable mode This bit enables the user to activate the external
      --  transceiver control, through the DE signal. This bit can only be
      --  written when the LPUART is disabled (UE=0).
      DEM            : Boolean := False;
      --  Driver enable polarity selection This bit can only be written when
      --  the LPUART is disabled (UE=0).
      DEP            : Boolean := False;
      --  unspecified
      Reserved_16_19 : HAL.UInt4 := 16#0#;
      --  Wake-up from low-power mode interrupt flag selection This bitfield
      --  specifies the event which activates the WUF (Wake-up from low-power
      --  mode flag). This bitfield can only be written when the LPUART is
      --  disabled (UE=0). Note: If the USART does not support the wake-up from
      --  Stop feature, this bit is reserved and must be kept at reset value.
      --  Refer to Section132.3: LPUART implementation on page1914.
      WUS            : LPUART_CR3_WUS_Field :=
                        (As_Array => False, Val => 16#0#);
      --  Wake-up from low-power mode interrupt enable This bit is set and
      --  cleared by software. Note: WUFIE must be set before entering in
      --  low-power mode. Note: If the USART does not support the wake-up from
      --  Stop feature, this bit is reserved and must be kept at reset value.
      --  Refer to Section132.3: LPUART implementation on page1914.
      WUFIE          : Boolean := False;
      --  TXFIFO threshold interrupt enable This bit is set and cleared by
      --  software.
      TXFTIE         : Boolean := False;
      --  unspecified
      Reserved_24_24 : HAL.Bit := 16#0#;
      --  Receive FIFO threshold configuration Remaining combinations:
      --  Reserved.
      RXFTCFG        : LPUART_CR3_RXFTCFG_Field := 16#0#;
      --  RXFIFO threshold interrupt enable This bit is set and cleared by
      --  software.
      RXFTIE         : Boolean := False;
      --  TXFIFO threshold configuration Remaining combinations: Reserved.
      TXFTCFG        : LPUART_CR3_TXFTCFG_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPUART_CR3_Register use record
      EIE            at 0 range 0 .. 0;
      Reserved_1_2   at 0 range 1 .. 2;
      HDSEL          at 0 range 3 .. 3;
      Reserved_4_5   at 0 range 4 .. 5;
      DMAR           at 0 range 6 .. 6;
      DMAT           at 0 range 7 .. 7;
      RTSE           at 0 range 8 .. 8;
      CTSE           at 0 range 9 .. 9;
      CTSIE          at 0 range 10 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      OVRDIS         at 0 range 12 .. 12;
      DDRE           at 0 range 13 .. 13;
      DEM            at 0 range 14 .. 14;
      DEP            at 0 range 15 .. 15;
      Reserved_16_19 at 0 range 16 .. 19;
      WUS            at 0 range 20 .. 21;
      WUFIE          at 0 range 22 .. 22;
      TXFTIE         at 0 range 23 .. 23;
      Reserved_24_24 at 0 range 24 .. 24;
      RXFTCFG        at 0 range 25 .. 27;
      RXFTIE         at 0 range 28 .. 28;
      TXFTCFG        at 0 range 29 .. 31;
   end record;

   subtype LPUART_BRR_BRR_Field is HAL.UInt20;

   --  LPUART baud rate register
   type LPUART_BRR_Register is record
      --  LPUART baud rate division (LPUARTDIV)
      BRR            : LPUART_BRR_BRR_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPUART_BRR_Register use record
      BRR            at 0 range 0 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  LPUART request register
   type LPUART_RQR_Register is record
      --  unspecified
      Reserved_0_0  : HAL.Bit := 16#0#;
      --  Write-only. Send break request Writing 1 to this bit sets the SBKF
      --  flag and request to send a BREAK on the line, as soon as the transmit
      --  machine is available. Note: If the application needs to send the
      --  break character following all previously inserted data, including the
      --  ones not yet transmitted, the software must wait for the TXE flag
      --  assertion before setting the SBKRQ bit.
      SBKRQ         : Boolean := False;
      --  Write-only. Mute mode request Writing 1 to this bit puts the LPUART
      --  in Mute mode and resets the RWU flag.
      MMRQ          : Boolean := False;
      --  Write-only. Receive data flush request Writing 1 to this bit clears
      --  the RXNE flag. This enables discarding the received data without
      --  reading it, and avoid an overrun condition.
      RXFRQ         : Boolean := False;
      --  Write-only. Transmit data flush request This bit is used when FIFO
      --  mode is enabled. TXFRQ bit is set to flush the whole FIFO. This sets
      --  the flag TXFE (TXFIFO empty, bit 23 in the LPUART_ISR register).
      --  Note: In FIFO mode, the TXFNF flag is reset during the flush request
      --  until TxFIFO is empty in order to ensure that no data are written in
      --  the data register.
      TXFRQ         : Boolean := False;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPUART_RQR_Register use record
      Reserved_0_0  at 0 range 0 .. 0;
      SBKRQ         at 0 range 1 .. 1;
      MMRQ          at 0 range 2 .. 2;
      RXFRQ         at 0 range 3 .. 3;
      TXFRQ         at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   --  LPUART interrupt and status register
   type LPUART_ISR_Register is record
      --  Read-only. Parity error This bit is set by hardware when a parity
      --  error occurs in Reception mode. It is cleared by software, writing 1
      --  to the PECF in the LPUART_ICR register. An interrupt is generated if
      --  PEIE = 1 in the LPUART_CR1 register. Note: This error is associated
      --  with the character in the LPUART_RDR.
      PE             : Boolean;
      --  Read-only. Framing error This bit is set by hardware when a
      --  de-synchronization, excessive noise or a break character is detected.
      --  It is cleared by software, writing 1 to the FECF bit in the
      --  LPUART_ICR register. When transmitting data in Smartcard mode, this
      --  bit is set when the maximum number of transmit attempts is reached
      --  without success (the card NACKs the data frame). An interrupt is
      --  generated if EIE1=11 in the LPUART_CR3 register. Note: This error is
      --  associated with the character in the LPUART_RDR.
      FE             : Boolean;
      --  Read-only. Start bit noise detection flag This bit is set by hardware
      --  when noise is detected on the start bit of a received frame. It is
      --  cleared by software, writing 1 to the NFCF bit in the LPUART_ICR
      --  register. Note: This bit does not generate an interrupt as it appears
      --  at the same time as the RXFNE bit which itself generates an
      --  interrupt. An interrupt is generated when the NE flag is set during
      --  multi buffer communication if the EIE bit is set. Note: This error is
      --  associated with the character in the LPUART_RDR.
      NE             : Boolean;
      --  Read-only. Overrun error This bit is set by hardware when the data
      --  currently being received in the shift register is ready to be
      --  transferred into the LPUART_RDR register while RXFF = 1. It is
      --  cleared by a software, writing 1 to the ORECF, in the LPUART_ICR
      --  register. An interrupt is generated if RXFNEIE=1 in the LPUART_CR1
      --  register, or EIE = 1 in the LPUART_CR3 register. Note: When this bit
      --  is set, the LPUART_RDR register content is not lost but the shift
      --  register is overwritten. An interrupt is generated if the ORE flag is
      --  set during multi buffer communication if the EIE bit is set. Note:
      --  This bit is permanently forced to 0 (no overrun detection) when the
      --  bit OVRDIS is set in the LPUART_CR3 register.
      ORE            : Boolean;
      --  Read-only. Idle line detected This bit is set by hardware when an
      --  Idle line is detected. An interrupt is generated if IDLEIE=1 in the
      --  LPUART_CR1 register. It is cleared by software, writing 1 to the
      --  IDLECF in the LPUART_ICR register. Note: The IDLE bit is not set
      --  again until the RXFNE bit has been set (i.e. a new idle line occurs).
      --  Note: If Mute mode is enabled (MME=1), IDLE is set if the LPUART is
      --  not mute (RWU=0), whatever the Mute mode selected by the WAKE bit. If
      --  RWU=1, IDLE is not set.
      IDLE           : Boolean;
      --  Read-only. RXFIFO not empty RXFNE bit is set by hardware when the
      --  RXFIFO is not empty, and so data can be read from the LPUART_RDR
      --  register. Every read of the LPUART_RDR frees a location in the
      --  RXFIFO. It is cleared when the RXFIFO is empty. The RXFNE flag can
      --  also be cleared by writing 1 to the RXFRQ in the LPUART_RQR register.
      --  An interrupt is generated if RXFNEIE=1 in the LPUART_CR1 register.
      RXFNE          : Boolean;
      --  Read-only. Transmission complete This bit indicates that the last
      --  data written in the LPUART_TDR has been transmitted out of the shift
      --  register. The TC flag behaves as follows: When TDN = 0, the TC flag
      --  is set when the transmission of a frame containing data is complete
      --  and when TXFE is set. When TDN is equal to the number of data in the
      --  TXFIFO, the TC flag is set when TXFIFO is empty and TDN is reached.
      --  When TDN is greater than the number of data in the TXFIFO, TC remains
      --  cleared until the TXFIFO is filled again to reach the programmed
      --  number of data to be transferred. When TDN is less than the number of
      --  data in the TXFIFO, TC is set when TDN is reached even if the TXFIFO
      --  is not empty. An interrupt is generated if TCIE=1 in the LPUART_CR1
      --  register. TC bit is cleared by software by writing 1 to the TCCF in
      --  the LPUART_ICR register or by writing to the LPUART_TDR register.
      TC             : Boolean;
      --  Read-only. TXFIFO not full TXFNF is set by hardware when TXFIFO is
      --  not full, and so data can be written in the LPUART_TDR. Every write
      --  in the LPUART_TDR places the data in the TXFIFO. This flag remains
      --  set until the TXFIFO is full. When the TXFIFO is full, this flag is
      --  cleared indicating that data can not be written into the LPUART_TDR.
      --  The TXFNF is kept reset during the flush request until TXFIFO is
      --  empty. After sending the flush request (by setting TXFRQ bit), the
      --  flag TXFNF must be checked prior to writing in TXFIFO (TXFNF and TXFE
      --  are set at the same time). An interrupt is generated if the TXFNFIE
      --  bit =1 in the LPUART_CR1 register. Note: This bit is used during
      --  single buffer transmission.
      TXFNF          : Boolean;
      --  unspecified
      Reserved_8_8   : HAL.Bit;
      --  Read-only. CTS interrupt flag This bit is set by hardware when the
      --  CTS input toggles, if the CTSE bit is set. It is cleared by software,
      --  by writing 1 to the CTSCF bit in the LPUART_ICR register. An
      --  interrupt is generated if CTSIE=1 in the LPUART_CR3 register. Note:
      --  If the hardware flow control feature is not supported, this bit is
      --  reserved and kept at reset value.
      CTSIF          : Boolean;
      --  Read-only. CTS flag This bit is set/reset by hardware. It is an
      --  inverted copy of the status of the CTS input pin. Note: If the
      --  hardware flow control feature is not supported, this bit is reserved
      --  and kept at reset value.
      CTS            : Boolean;
      --  unspecified
      Reserved_11_15 : HAL.UInt5;
      --  Read-only. Busy flag This bit is set and reset by hardware. It is
      --  active when a communication is ongoing on the RX line (successful
      --  start bit detected). It is reset at the end of the reception
      --  (successful or not).
      BUSY           : Boolean;
      --  Read-only. Character match flag This bit is set by hardware, when a
      --  the character defined by ADD[7:0] is received. It is cleared by
      --  software, writing 1 to the CMCF in the LPUART_ICR register. An
      --  interrupt is generated if CMIE=1in the LPUART_CR1 register.
      CMF            : Boolean;
      --  Read-only. Send break flag This bit indicates that a send break
      --  character was requested. It is set by software, by writing 1 to the
      --  SBKRQ bit in the LPUART_CR3 register. It is automatically reset by
      --  hardware during the stop bit of break transmission.
      SBKF           : Boolean;
      --  Read-only. Receiver wake-up from Mute mode This bit indicates if the
      --  LPUART is in Mute mode. It is cleared/set by hardware when a
      --  wake-up/mute sequence is recognized. The Mute mode control sequence
      --  (address or IDLE) is selected by the WAKE bit in the LPUART_CR1
      --  register. When wake-up on IDLE mode is selected, this bit can only be
      --  set by software, writing 1 to the MMRQ bit in the LPUART_RQR
      --  register. Note: If the LPUART does not support the wake-up from Stop
      --  feature, this bit is reserved and kept at reset value.
      RWU            : Boolean;
      --  Read-only. Wake-up from low-power mode flag This bit is set by
      --  hardware, when a wake-up event is detected. The event is defined by
      --  the WUS bitfield. It is cleared by software, writing a 1 to the WUCF
      --  in the LPUART_ICR register. An interrupt is generated if WUFIE=1 in
      --  the LPUART_CR3 register. Note: When UESM is cleared, WUF flag is also
      --  cleared. Note: If the USART does not support the wake-up from Stop
      --  feature, this bit is reserved and kept at reset value. Refer to
      --  Section132.3: LPUART implementation on page1914.
      WUF            : Boolean;
      --  Read-only. Transmit enable acknowledge flag This bit is set/reset by
      --  hardware, when the Transmit Enable value is taken into account by the
      --  LPUART. It can be used when an idle frame request is generated by
      --  writing TE=0, followed by TE=1 in the LPUART_CR1 register, in order
      --  to respect the TE=0 minimum period.
      TEACK          : Boolean;
      --  Read-only. Receive enable acknowledge flag This bit is set/reset by
      --  hardware, when the Receive Enable value is taken into account by the
      --  LPUART. It can be used to verify that the LPUART is ready for
      --  reception before entering low-power mode. Note: If the LPUART does
      --  not support the wake-up from Stop feature, this bit is reserved and
      --  kept at reset value.
      REACK          : Boolean;
      --  Read-only. TXFIFO Empty This bit is set by hardware when TXFIFO is
      --  Empty. When the TXFIFO contains at least one data, this flag is
      --  cleared. The TXFE flag can also be set by writing 1 to the bit TXFRQ
      --  (bit 4) in the LPUART_RQR register. An interrupt is generated if the
      --  TXFEIE bit =1 (bit 30) in the LPUART_CR1 register.
      TXFE           : Boolean;
      --  Read-only. RXFIFO Full This bit is set by hardware when the number of
      --  received data corresponds to RXFIFO1size1+11 (RXFIFO full + 1 data in
      --  the LPUART_RDR register. An interrupt is generated if the RXFFIE bit
      --  =1 in the LPUART_CR1 register.
      RXFF           : Boolean;
      --  unspecified
      Reserved_25_25 : HAL.Bit;
      --  Read-only. RXFIFO threshold flag This bit is set by hardware when the
      --  RXFIFO reaches the threshold programmed in RXFTCFG in LPUART_CR3
      --  register i.e. the Receive FIFO contains RXFTCFG data. An interrupt is
      --  generated if the RXFTIE bit =1 (bit 27) in the LPUART_CR3 register.
      RXFT           : Boolean;
      --  Read-only. TXFIFO threshold flag This bit is set by hardware when the
      --  TXFIFO reaches the threshold programmed in TXFTCFG in LPUART_CR3
      --  register i.e. the TXFIFO contains TXFTCFG empty locations. An
      --  interrupt is generated if the TXFTIE bit =1 (bit 31) in the
      --  LPUART_CR3 register.
      TXFT           : Boolean;
      --  unspecified
      Reserved_28_31 : HAL.UInt4;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPUART_ISR_Register use record
      PE             at 0 range 0 .. 0;
      FE             at 0 range 1 .. 1;
      NE             at 0 range 2 .. 2;
      ORE            at 0 range 3 .. 3;
      IDLE           at 0 range 4 .. 4;
      RXFNE          at 0 range 5 .. 5;
      TC             at 0 range 6 .. 6;
      TXFNF          at 0 range 7 .. 7;
      Reserved_8_8   at 0 range 8 .. 8;
      CTSIF          at 0 range 9 .. 9;
      CTS            at 0 range 10 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
      BUSY           at 0 range 16 .. 16;
      CMF            at 0 range 17 .. 17;
      SBKF           at 0 range 18 .. 18;
      RWU            at 0 range 19 .. 19;
      WUF            at 0 range 20 .. 20;
      TEACK          at 0 range 21 .. 21;
      REACK          at 0 range 22 .. 22;
      TXFE           at 0 range 23 .. 23;
      RXFF           at 0 range 24 .. 24;
      Reserved_25_25 at 0 range 25 .. 25;
      RXFT           at 0 range 26 .. 26;
      TXFT           at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  LPUART interrupt and status register
   type LPUART_ISR_ALTERNATE_Register is record
      --  Read-only. Parity error This bit is set by hardware when a parity
      --  error occurs in Reception mode. It is cleared by software, writing 1
      --  to the PECF in the LPUART_ICR register. An interrupt is generated if
      --  PEIE = 1 in the LPUART_CR1 register. Note: In FIFO mode, this error
      --  is associated with the character in the LPUART_RDR.
      PE             : Boolean;
      --  Read-only. Framing error This bit is set by hardware when a
      --  de-synchronization, excessive noise or a break character is detected.
      --  It is cleared by software, writing 1 to the FECF bit in the
      --  LPUART_ICR register. When transmitting data in Smartcard mode, this
      --  bit is set when the maximum number of transmit attempts is reached
      --  without success (the card NACKs the data frame). An interrupt is
      --  generated if EIE1=11 in the LPUART_CR3 register. Note: In FIFO mode,
      --  this error is associated with the character in the LPUART_RDR.
      FE             : Boolean;
      --  Read-only. Start bit noise detection flag This bit is set by hardware
      --  when noise is detected on the start bit of a received frame. It is
      --  cleared by software, writing 1 to the NFCF bit in the LPUART_ICR
      --  register. Note: This bit does not generate an interrupt as it appears
      --  at the same time as the RXNE/RXFNE bit which itself generates an
      --  interrupt. An interrupt is generated when the NE flag is set during
      --  multi buffer communication if the EIE bit is set. Note: In FIFO mode,
      --  this error is associated with the character in the LPUART_RDR.
      NE             : Boolean;
      --  Read-only. Overrun error This bit is set by hardware when the data
      --  currently being received in the shift register is ready to be
      --  transferred into the LPUART_RDR register while RXNE=1 (RXFF = 1 in
      --  case FIFO mode is enabled). It is cleared by a software, writing 1 to
      --  the ORECF, in the LPUART_ICR register. An interrupt is generated if
      --  RXNEIE=1 in the LPUART_CR1 register, or EIE = 1 in the LPUART_CR3
      --  register. Note: When this bit is set, the LPUART_RDR register content
      --  is not lost but the shift register is overwritten. An interrupt is
      --  generated if the ORE flag is set during multi buffer communication if
      --  the EIE bit is set. Note: This bit is permanently forced to 0 (no
      --  overrun detection) when the bit OVRDIS is set in the LPUART_CR3
      --  register.
      ORE            : Boolean;
      --  Read-only. Idle line detected This bit is set by hardware when an
      --  Idle Line is detected. An interrupt is generated if IDLEIE=1 in the
      --  LPUART_CR1 register. It is cleared by software, writing 1 to the
      --  IDLECF in the LPUART_ICR register. Note: The IDLE bit is not set
      --  again until the RXNE bit has been set (i.e. a new idle line occurs).
      --  Note: If Mute mode is enabled (MME=1), IDLE is set if the LPUART is
      --  not mute (RWU=0), whatever the Mute mode selected by the WAKE bit. If
      --  RWU=1, IDLE is not set.
      IDLE           : Boolean;
      --  Read-only. Read data register not empty RXNE bit is set by hardware
      --  when the content of the LPUART_RDR shift register has been
      --  transferred to the LPUART_RDR register. It is cleared by a read to
      --  the LPUART_RDR register. The RXNE flag can also be cleared by writing
      --  1 to the RXFRQ in the LPUART_RQR register. The RXFNE flag can also be
      --  cleared by writing 1 to the RXFRQ in the LPUART_RQR register. An
      --  interrupt is generated if RXNEIE=1 in the LPUART_CR1 register.
      RXNE           : Boolean;
      --  Read-only. Transmission complete This bit indicates that the last
      --  data written in the USART_TDR has been transmitted out of the shift
      --  register. The TC flag is set when the transmission of a frame
      --  containing data is complete and when TXE is set. An interrupt is
      --  generated if TCIE=1 in the LPUART_CR1 register. TC bit is cleared by
      --  software by writing 1 to the TCCF in the USART_ICR register or by
      --  writing to the USART_TDR register.
      TC             : Boolean;
      --  Read-only. Transmit data register empty TXE is set by hardware when
      --  the content of the LPUART_TDR register has been transferred into the
      --  shift register. It is cleared by a write to the LPUART_TDR register.
      --  An interrupt is generated if the TXEIE bit =1 in the LPUART_CR1
      --  register. Note: This bit is used during single buffer transmission.
      TXE            : Boolean;
      --  unspecified
      Reserved_8_8   : HAL.Bit;
      --  Read-only. CTS interrupt flag This bit is set by hardware when the
      --  CTS input toggles, if the CTSE bit is set. It is cleared by software,
      --  by writing 1 to the CTSCF bit in the LPUART_ICR register. An
      --  interrupt is generated if CTSIE=1 in the LPUART_CR3 register. Note:
      --  If the hardware flow control feature is not supported, this bit is
      --  reserved and kept at reset value.
      CTSIF          : Boolean;
      --  Read-only. CTS flag This bit is set/reset by hardware. It is an
      --  inverted copy of the status of the CTS input pin. Note: If the
      --  hardware flow control feature is not supported, this bit is reserved
      --  and kept at reset value.
      CTS            : Boolean;
      --  unspecified
      Reserved_11_15 : HAL.UInt5;
      --  Read-only. Busy flag This bit is set and reset by hardware. It is
      --  active when a communication is ongoing on the RX line (successful
      --  start bit detected). It is reset at the end of the reception
      --  (successful or not).
      BUSY           : Boolean;
      --  Read-only. Character match flag This bit is set by hardware, when a
      --  the character defined by ADD[7:0] is received. It is cleared by
      --  software, writing 1 to the CMCF in the LPUART_ICR register. An
      --  interrupt is generated if CMIE=1in the LPUART_CR1 register.
      CMF            : Boolean;
      --  Read-only. Send break flag This bit indicates that a send break
      --  character was requested. It is set by software, by writing 1 to the
      --  SBKRQ bit in the LPUART_CR3 register. It is automatically reset by
      --  hardware during the stop bit of break transmission.
      SBKF           : Boolean;
      --  Read-only. Receiver wake-up from Mute mode This bit indicates if the
      --  LPUART is in Mute mode. It is cleared/set by hardware when a
      --  wake-up/mute sequence is recognized. The Mute mode control sequence
      --  (address or IDLE) is selected by the WAKE bit in the LPUART_CR1
      --  register. When wake-up on IDLE mode is selected, this bit can only be
      --  set by software, writing 1 to the MMRQ bit in the LPUART_RQR
      --  register. Note: If the LPUART does not support the wake-up from Stop
      --  feature, this bit is reserved and kept at reset value.
      RWU            : Boolean;
      --  Read-only. Wake-up from low-power mode flag This bit is set by
      --  hardware, when a wake-up event is detected. The event is defined by
      --  the WUS bitfield. It is cleared by software, writing a 1 to the WUCF
      --  in the LPUART_ICR register. An interrupt is generated if WUFIE=1 in
      --  the LPUART_CR3 register. Note: When UESM is cleared, WUF flag is also
      --  cleared. Note: If the USART does not support the wake-up from Stop
      --  feature, this bit is reserved and kept at reset value. Refer to
      --  Section132.3: LPUART implementation on page1914.
      WUF            : Boolean;
      --  Read-only. Transmit enable acknowledge flag This bit is set/reset by
      --  hardware, when the Transmit Enable value is taken into account by the
      --  LPUART. It can be used when an idle frame request is generated by
      --  writing TE=0, followed by TE=1 in the LPUART_CR1 register, in order
      --  to respect the TE=0 minimum period.
      TEACK          : Boolean;
      --  Read-only. Receive enable acknowledge flag This bit is set/reset by
      --  hardware, when the Receive Enable value is taken into account by the
      --  LPUART. It can be used to verify that the LPUART is ready for
      --  reception before entering low-power mode. Note: If the LPUART does
      --  not support the wake-up from Stop feature, this bit is reserved and
      --  kept at reset value.
      REACK          : Boolean;
      --  unspecified
      Reserved_23_31 : HAL.UInt9;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPUART_ISR_ALTERNATE_Register use record
      PE             at 0 range 0 .. 0;
      FE             at 0 range 1 .. 1;
      NE             at 0 range 2 .. 2;
      ORE            at 0 range 3 .. 3;
      IDLE           at 0 range 4 .. 4;
      RXNE           at 0 range 5 .. 5;
      TC             at 0 range 6 .. 6;
      TXE            at 0 range 7 .. 7;
      Reserved_8_8   at 0 range 8 .. 8;
      CTSIF          at 0 range 9 .. 9;
      CTS            at 0 range 10 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
      BUSY           at 0 range 16 .. 16;
      CMF            at 0 range 17 .. 17;
      SBKF           at 0 range 18 .. 18;
      RWU            at 0 range 19 .. 19;
      WUF            at 0 range 20 .. 20;
      TEACK          at 0 range 21 .. 21;
      REACK          at 0 range 22 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  LPUART interrupt flag clear register
   type LPUART_ICR_Register is record
      --  Write-only. Parity error clear flag Writing 1 to this bit clears the
      --  PE flag in the LPUART_ISR register.
      PECF           : Boolean := False;
      --  Write-only. Framing error clear flag Writing 1 to this bit clears the
      --  FE flag in the LPUART_ISR register.
      FECF           : Boolean := False;
      --  Write-only. Noise detected clear flag Writing 1 to this bit clears
      --  the NE flag in the LPUART_ISR register.
      NECF           : Boolean := False;
      --  Write-only. Overrun error clear flag Writing 1 to this bit clears the
      --  ORE flag in the LPUART_ISR register.
      ORECF          : Boolean := False;
      --  Write-only. Idle line detected clear flag Writing 1 to this bit
      --  clears the IDLE flag in the LPUART_ISR register.
      IDLECF         : Boolean := False;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  Write-only. Transmission complete clear flag Writing 1 to this bit
      --  clears the TC flag in the LPUART_ISR register.
      TCCF           : Boolean := False;
      --  unspecified
      Reserved_7_8   : HAL.UInt2 := 16#0#;
      --  Write-only. CTS clear flag Writing 1 to this bit clears the CTSIF
      --  flag in the LPUART_ISR register.
      CTSCF          : Boolean := False;
      --  unspecified
      Reserved_10_16 : HAL.UInt7 := 16#0#;
      --  Write-only. Character match clear flag Writing 1 to this bit clears
      --  the CMF flag in the LPUART_ISR register.
      CMCF           : Boolean := False;
      --  unspecified
      Reserved_18_19 : HAL.UInt2 := 16#0#;
      --  Write-only. Wake-up from low-power mode clear flag Writing 1 to this
      --  bit clears the WUF flag in the USART_ISR register. Note: If the USART
      --  does not support the wake-up from Stop feature, this bit is reserved
      --  and must be kept at reset value. Refer to Section132.3: LPUART
      --  implementation on page1914.
      WUCF           : Boolean := False;
      --  unspecified
      Reserved_21_31 : HAL.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPUART_ICR_Register use record
      PECF           at 0 range 0 .. 0;
      FECF           at 0 range 1 .. 1;
      NECF           at 0 range 2 .. 2;
      ORECF          at 0 range 3 .. 3;
      IDLECF         at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      TCCF           at 0 range 6 .. 6;
      Reserved_7_8   at 0 range 7 .. 8;
      CTSCF          at 0 range 9 .. 9;
      Reserved_10_16 at 0 range 10 .. 16;
      CMCF           at 0 range 17 .. 17;
      Reserved_18_19 at 0 range 18 .. 19;
      WUCF           at 0 range 20 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   subtype LPUART_RDR_RDR_Field is HAL.UInt9;

   --  LPUART receive data register
   type LPUART_RDR_Register is record
      --  Read-only. Receive data value Contains the received data character.
      --  The RDR register provides the parallel interface between the input
      --  shift register and the internal bus (see Figure1254). When receiving
      --  with the parity enabled, the value read in the MSB bit is the
      --  received parity bit.
      RDR           : LPUART_RDR_RDR_Field;
      --  unspecified
      Reserved_9_31 : HAL.UInt23;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPUART_RDR_Register use record
      RDR           at 0 range 0 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   subtype LPUART_TDR_TDR_Field is HAL.UInt9;

   --  LPUART transmit data register
   type LPUART_TDR_Register is record
      --  Transmit data value Contains the data character to be transmitted.
      --  The TDR register provides the parallel interface between the internal
      --  bus and the output shift register (see Figure1254). When transmitting
      --  with the parity enabled (PCE bit set to 1 in the LPUART_CR1
      --  register), the value written in the MSB (bit 7 or bit 8 depending on
      --  the data length) has no effect because it is replaced by the parity.
      --  Note: This register must be written only when TXE/TXFNF=1.
      TDR           : LPUART_TDR_TDR_Field := 16#0#;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPUART_TDR_Register use record
      TDR           at 0 range 0 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   subtype LPUART_PRESC_PRESCALER_Field is HAL.UInt4;

   --  LPUART prescaler register
   type LPUART_PRESC_Register is record
      --  Clock prescaler The LPUART input clock can be divided by a prescaler:
      --  Remaining combinations: Reserved. Note: When PRESCALER is programmed
      --  with a value different of the allowed ones, programmed prescaler
      --  value is equal to 1011 i.e. input clock divided by 256.
      PRESCALER     : LPUART_PRESC_PRESCALER_Field := 16#0#;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPUART_PRESC_Register use record
      PRESCALER     at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   type LPUART1_Disc is
     (Default,
      Alternate);

   --  LPUART address block description
   type LPUART_Peripheral
     (Discriminent : LPUART1_Disc := Default)
   is record
      --  LPUART control register 2
      LPUART_CR2           : aliased LPUART_CR2_Register;
      --  LPUART control register 3
      LPUART_CR3           : aliased LPUART_CR3_Register;
      --  LPUART baud rate register
      LPUART_BRR           : aliased LPUART_BRR_Register;
      --  LPUART request register
      LPUART_RQR           : aliased LPUART_RQR_Register;
      --  LPUART interrupt flag clear register
      LPUART_ICR           : aliased LPUART_ICR_Register;
      --  LPUART receive data register
      LPUART_RDR           : aliased LPUART_RDR_Register;
      --  LPUART transmit data register
      LPUART_TDR           : aliased LPUART_TDR_Register;
      --  LPUART prescaler register
      LPUART_PRESC         : aliased LPUART_PRESC_Register;
      case Discriminent is
         when Default =>
            --  LPUART control register 1
            LPUART_CR1 : aliased LPUART_CR1_Register;
            --  LPUART interrupt and status register
            LPUART_ISR : aliased LPUART_ISR_Register;
         when Alternate =>
            --  LPUART control register 1
            LPUART_CR1_ALTERNATE : aliased LPUART_CR1_ALTERNATE_Register;
            --  LPUART interrupt and status register
            LPUART_ISR_ALTERNATE : aliased LPUART_ISR_ALTERNATE_Register;
      end case;
   end record
     with Unchecked_Union, Volatile;

   for LPUART_Peripheral use record
      LPUART_CR2           at 16#4# range 0 .. 31;
      LPUART_CR3           at 16#8# range 0 .. 31;
      LPUART_BRR           at 16#C# range 0 .. 31;
      LPUART_RQR           at 16#18# range 0 .. 31;
      LPUART_ICR           at 16#20# range 0 .. 31;
      LPUART_RDR           at 16#24# range 0 .. 31;
      LPUART_TDR           at 16#28# range 0 .. 31;
      LPUART_PRESC         at 16#2C# range 0 .. 31;
      LPUART_CR1           at 16#0# range 0 .. 31;
      LPUART_ISR           at 16#1C# range 0 .. 31;
      LPUART_CR1_ALTERNATE at 16#0# range 0 .. 31;
      LPUART_ISR_ALTERNATE at 16#1C# range 0 .. 31;
   end record;

   --  LPUART address block description
   LPUART1_Periph : aliased LPUART_Peripheral
     with Import, Address => LPUART1_Base;

   --  LPUART address block description
   LPUART2_Periph : aliased LPUART_Peripheral
     with Import, Address => LPUART2_Base;

   --  LPUART address block description
   LPUART3_Periph : aliased LPUART_Peripheral
     with Import, Address => LPUART3_Base;

end STM32_SVD.LPUART;
