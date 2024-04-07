--  This spec has been automatically generated from STM32U073.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.SPI is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype SPI_CR1_BR_Field is HAL.UInt3;

   --  SPI control register 1
   type SPI_CR1_Register is record
      --  Clock phase Note: This bit should not be changed when communication
      --  is ongoing. Note: This bit is not used in SPI TI mode except the case
      --  when CRC is applied at TI mode.
      CPHA     : Boolean := False;
      --  Clock polarity Note: This bit should not be changed when
      --  communication is ongoing. Note: This bit is not used in SPI TI mode
      --  except the case when CRC is applied at TI mode.
      CPOL     : Boolean := False;
      --  Master selection Note: This bit should not be changed when
      --  communication is ongoing.
      MSTR     : Boolean := False;
      --  Baud rate control Note: These bits should not be changed when
      --  communication is ongoing.
      BR       : SPI_CR1_BR_Field := 16#0#;
      --  SPI enable Note: When disabling the SPI, follow the procedure
      --  described in Procedure for disabling the SPI on page1954.
      SPE      : Boolean := False;
      --  Frame format Note: 1. This bit should not be changed when
      --  communication is ongoing. Note: 2. This bit is not used in SPI TI
      --  mode.
      LSBFIRST : Boolean := False;
      --  Internal slave select This bit has an effect only when the SSM bit is
      --  set. The value of this bit is forced onto the NSS pin and the I/O
      --  value of the NSS pin is ignored. Note: This bit is not used in SPI TI
      --  mode.
      SSI      : Boolean := False;
      --  Software slave management When the SSM bit is set, the NSS pin input
      --  is replaced with the value from the SSI bit. Note: This bit is not
      --  used in SPI TI mode.
      SSM      : Boolean := False;
      --  Receive only mode enabled. This bit enables simplex communication
      --  using a single unidirectional line to receive data exclusively. Keep
      --  BIDIMODE bit clear when receive only mode is active.This bit is also
      --  useful in a multislave system in which this particular slave is not
      --  accessed, the output from the accessed slave is not corrupted.
      RXONLY   : Boolean := False;
      --  CRC length This bit is set and cleared by software to select the CRC
      --  length. Note: This bit should be written only when SPI is disabled
      --  (SPE = 0) for correct operation.
      CRCL     : Boolean := False;
      --  Transmit CRC next Note: This bit has to be written as soon as the
      --  last data is written in the SPI_DR register.
      CRCNEXT  : Boolean := False;
      --  Hardware CRC calculation enable Note: This bit should be written only
      --  when SPI is disabled (SPE = 0) for correct operation.
      CRCEN    : Boolean := False;
      --  Output enable in bidirectional mode This bit combined with the
      --  BIDIMODE bit selects the direction of transfer in bidirectional mode.
      --  Note: In master mode, the MOSI pin is used and in slave mode, the
      --  MISO pin is used.
      BIDIOE   : Boolean := False;
      --  Bidirectional data mode enable. This bit enables half-duplex
      --  communication using common single bidirectional data line. Keep
      --  RXONLY bit clear when bidirectional mode is active.
      BIDIMODE : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 16,
          Bit_Order => System.Low_Order_First;

   for SPI_CR1_Register use record
      CPHA     at 0 range 0 .. 0;
      CPOL     at 0 range 1 .. 1;
      MSTR     at 0 range 2 .. 2;
      BR       at 0 range 3 .. 5;
      SPE      at 0 range 6 .. 6;
      LSBFIRST at 0 range 7 .. 7;
      SSI      at 0 range 8 .. 8;
      SSM      at 0 range 9 .. 9;
      RXONLY   at 0 range 10 .. 10;
      CRCL     at 0 range 11 .. 11;
      CRCNEXT  at 0 range 12 .. 12;
      CRCEN    at 0 range 13 .. 13;
      BIDIOE   at 0 range 14 .. 14;
      BIDIMODE at 0 range 15 .. 15;
   end record;

   subtype SPI_CR2_DS_Field is HAL.UInt4;

   --  SPI control register 2
   type SPI_CR2_Register is record
      --  Rx buffer DMA enable When this bit is set, a DMA request is generated
      --  whenever the RXNE flag is set.
      RXDMAEN        : Boolean := False;
      --  Tx buffer DMA enable When this bit is set, a DMA request is generated
      --  whenever the TXE flag is set.
      TXDMAEN        : Boolean := False;
      --  SS output enable Note: This bit is not used in SPI TI mode.
      SSOE           : Boolean := False;
      --  NSS pulse management This bit is used in master mode only. it allows
      --  the SPI to generate an NSS pulse between two consecutive data when
      --  doing continuous transfers. In the case of a single data transfer, it
      --  forces the NSS pin high level after the transfer. It has no meaning
      --  if CPHA = 1, or FRF = 1. Note: 1. This bit must be written only when
      --  the SPI is disabled (SPE=0). Note: 2. This bit is not used in SPI TI
      --  mode.
      NSSP           : Boolean := False;
      --  Frame format 1 SPI TI mode Note: This bit must be written only when
      --  the SPI is disabled (SPE=0).
      FRF            : Boolean := False;
      --  Error interrupt enable This bit controls the generation of an
      --  interrupt when an error condition occurs (CRCERR, OVR, MODF in SPI
      --  mode, FRE at TI mode).
      ERRIE          : Boolean := False;
      --  RX buffer not empty interrupt enable
      RXNEIE         : Boolean := False;
      --  Tx buffer empty interrupt enable
      TXEIE          : Boolean := False;
      --  Data size These bits configure the data length for SPI transfers. If
      --  software attempts to write one of the Not used values, they are
      --  forced to the value 0111 (8-bit)
      DS             : SPI_CR2_DS_Field := 16#7#;
      --  FIFO reception threshold This bit is used to set the threshold of the
      --  RXFIFO that triggers an RXNE event
      FRXTH          : Boolean := False;
      --  Last DMA transfer for reception This bit is used in data packing
      --  mode, to define if the total number of data to receive by DMA is odd
      --  or even. It has significance only if the RXDMAEN bit in the SPI_CR2
      --  register is set and if packing mode is used (data length =< 8-bit and
      --  write access to SPI_DR is 16-bit wide). It has to be written when the
      --  SPI is disabled (SPE = 0 in the SPI_CR1 register). Note: Refer to
      --  Procedure for disabling the SPI on page1954 if the CRCEN bit is set.
      LDMA_RX        : Boolean := False;
      --  Last DMA transfer for transmission This bit is used in data packing
      --  mode, to define if the total number of data to transmit by DMA is odd
      --  or even. It has significance only if the TXDMAEN bit in the SPI_CR2
      --  register is set and if packing mode is used (data length =< 8-bit and
      --  write access to SPI_DR is 16-bit wide). It has to be written when the
      --  SPI is disabled (SPE = 0 in the SPI_CR1 register). Note: Refer to
      --  Procedure for disabling the SPI on page1954 if the CRCEN bit is set.
      LDMA_TX        : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 16,
          Bit_Order => System.Low_Order_First;

   for SPI_CR2_Register use record
      RXDMAEN        at 0 range 0 .. 0;
      TXDMAEN        at 0 range 1 .. 1;
      SSOE           at 0 range 2 .. 2;
      NSSP           at 0 range 3 .. 3;
      FRF            at 0 range 4 .. 4;
      ERRIE          at 0 range 5 .. 5;
      RXNEIE         at 0 range 6 .. 6;
      TXEIE          at 0 range 7 .. 7;
      DS             at 0 range 8 .. 11;
      FRXTH          at 0 range 12 .. 12;
      LDMA_RX        at 0 range 13 .. 13;
      LDMA_TX        at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
   end record;

   subtype SPI_SR_FRLVL_Field is HAL.UInt2;
   subtype SPI_SR_FTLVL_Field is HAL.UInt2;

   --  SPI status register
   type SPI_SR_Register is record
      --  Read-only. Receive buffer not empty
      RXNE           : Boolean := False;
      --  Read-only. Transmit buffer empty
      TXE            : Boolean := True;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  CRC error flag Note: This flag is set by hardware and cleared by
      --  software writing 0.
      CRCERR         : Boolean := False;
      --  Read-only. Mode fault This flag is set by hardware and reset by a
      --  software sequence. Refer to Section1: Mode fault (MODF) on page1964
      --  for the software sequence.
      MODF           : Boolean := False;
      --  Read-only. Overrun flag This flag is set by hardware and reset by a
      --  software sequence.
      OVR            : Boolean := False;
      --  Read-only. Busy flag This flag is set and cleared by hardware. Note:
      --  The BSY flag must be used with caution: refer to Section133.4.10: SPI
      --  status flags and Procedure for disabling the SPI on page1954.
      BSY            : Boolean := False;
      --  Read-only. Frame format error This flag is used for SPI in TI slave
      --  mode. Refer to Section133.4.11: SPI error flags. This flag is set by
      --  hardware and reset when SPI_SR is read by software.
      FRE            : Boolean := False;
      --  Read-only. FIFO reception level These bits are set and cleared by
      --  hardware. Note: These bits are not used in SPI receive-only mode
      --  while CRC calculation is enabled.
      FRLVL          : SPI_SR_FRLVL_Field := 16#0#;
      --  Read-only. FIFO transmission level These bits are set and cleared by
      --  hardware.
      FTLVL          : SPI_SR_FTLVL_Field := 16#0#;
      --  unspecified
      Reserved_13_15 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 16,
          Bit_Order => System.Low_Order_First;

   for SPI_SR_Register use record
      RXNE           at 0 range 0 .. 0;
      TXE            at 0 range 1 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      CRCERR         at 0 range 4 .. 4;
      MODF           at 0 range 5 .. 5;
      OVR            at 0 range 6 .. 6;
      BSY            at 0 range 7 .. 7;
      FRE            at 0 range 8 .. 8;
      FRLVL          at 0 range 9 .. 10;
      FTLVL          at 0 range 11 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  SPI address block description
   type SPI_Peripheral is record
      --  SPI control register 1
      SPI_CR1    : aliased SPI_CR1_Register;
      --  SPI control register 2
      SPI_CR2    : aliased SPI_CR2_Register;
      --  SPI status register
      SPI_SR     : aliased SPI_SR_Register;
      --  SPI data register
      SPI_DR     : aliased HAL.UInt16;
      --  SPI CRC polynomial register
      SPI_CRCPR  : aliased HAL.UInt16;
      --  SPI Rx CRC register
      SPI_RXCRCR : aliased HAL.UInt16;
      --  SPI Tx CRC register
      SPI_TXCRCR : aliased HAL.UInt16;
   end record
     with Volatile;

   for SPI_Peripheral use record
      SPI_CR1    at 16#0# range 0 .. 15;
      SPI_CR2    at 16#4# range 0 .. 15;
      SPI_SR     at 16#8# range 0 .. 15;
      SPI_DR     at 16#C# range 0 .. 15;
      SPI_CRCPR  at 16#10# range 0 .. 15;
      SPI_RXCRCR at 16#14# range 0 .. 15;
      SPI_TXCRCR at 16#18# range 0 .. 15;
   end record;

   --  SPI address block description
   SPI1_Periph : aliased SPI_Peripheral
     with Import, Address => SPI1_Base;

   --  SPI address block description
   SPI2_Periph : aliased SPI_Peripheral
     with Import, Address => SPI2_Base;

   --  SPI address block description
   SPI3_Periph : aliased SPI_Peripheral
     with Import, Address => SPI3_Base;

end STM32_SVD.SPI;
