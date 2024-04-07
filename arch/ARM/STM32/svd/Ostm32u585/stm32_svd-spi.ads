--  This spec has been automatically generated from STM32U5xx.svd

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

   type SPI_CR1_Register is record
      --  serial peripheral enable This bit is set by and cleared by software.
      --  When SPE=1, SPI data transfer is enabled, SPI_CFG1 and SPI_CFG2
      --  configuration registers, CRCPOLY, UDRDR, part of SPI_AUTOCR register
      --  and IOLOCK bit in the SPI_CR1 register are write protected. They can
      --  be changed only when SPE=0. When SPE=0 any SPI operation is stopped
      --  and disabled, all the pending requests of the events with enabled
      --  interrupt are blocked except the MODF interrupt request (but their
      --  pending still propagates the request of the spi_plck clock), the SS
      --  output is deactivated at master, the RDY signal keeps not ready
      --  status at slave, the internal state machine is reseted, all the FIFOs
      --  content is flushed, CRC calculation initialized, receive data
      --  register is read zero. SPE is cleared and cannot be set when MODF
      --  error flag is active.
      SPE            : Boolean := False;
      --  unspecified
      Reserved_1_7   : HAL.UInt7 := 16#0#;
      --  master automatic suspension in Receive mode This bit is set and
      --  cleared by software to control continuous SPI transfer in master
      --  receiver mode and automatic management in order to avoid overrun
      --  condition. When SPI communication is suspended by hardware
      --  automatically, it could happen that few bits of next frame are
      --  already clocked out due to internal synchronization delay. This is
      --  why, the automatic suspension is not quite reliable when size of data
      --  drops below 8 bits. In this case, a safe suspension can be achieved
      --  by combination with delay inserted between data frames applied when
      --  MIDI parameter keeps a non zero value; sum of data size and the
      --  interleaved SPI cycles should always produce interval at length of 8
      --  SPI clock periods at minimum. After software clearing of the SUSP
      --  bit, the communication resumes and continues by subsequent bits
      --  transaction without any next constraint. Prior the SUSP bit is
      --  cleared, the user must release the RxFIFO space as much as possible
      --  by reading out all the data packets available at RxFIFO based on the
      --  RXP flag indication to prevent any subsequent suspension.
      MASRX          : Boolean := False;
      --  master transfer start This bit can be set by software if SPI is
      --  enabled only to start an SPI communication. it is cleared by hardware
      --  when end of transfer (EOT) flag is set or when a transaction suspend
      --  request is accepted. In SPI mode, the bit is taken into account at
      --  master mode only. If transmission is enabled, communication starts or
      --  continues only if any data is available in the transmission FIFO.
      CSTART         : Boolean := False;
      --  Write-only. master SUSPend request This bit reads as zero. In Master
      --  mode, when this bit is set by software, the CSTART bit is reset at
      --  the end of the current frame and SPI communication is suspended. The
      --  user has to check SUSP flag to check end of the frame transaction.
      --  The Master mode communication must be suspended (using this bit or
      --  keeping TXDR empty) before disabling the SPI or going to Low-power
      --  mode. After software suspension, SUSP flag has to be cleared and SPI
      --  disabled and re-enabled before the next transaction starts.
      CSUSP          : Boolean := False;
      --  Rx/Tx direction at Half-duplex mode In Half-Duplex configuration the
      --  HDDIR bit establishes the Rx/Tx direction of the data transfer. This
      --  bit is ignored in Full-Duplex or any Simplex configuration.
      HDDIR          : Boolean := False;
      --  internal SS signal input level This bit has an effect only when the
      --  SSM bit is set. The value of this bit is forced onto the peripheral
      --  SS input internally and the I/O value of the SS pin is ignored.
      SSI            : Boolean := False;
      --  32-bit CRC polynomial configuration
      CRC33_17       : Boolean := False;
      --  CRC calculation initialization pattern control for receiver
      RCRCINI        : Boolean := False;
      --  CRC calculation initialization pattern control for transmitter
      TCRCINI        : Boolean := False;
      --  locking the AF configuration of associated IOs This bit is set by
      --  software and cleared by hardware whenever the SPE bit is changed from
      --  1 to 0. When this bit is set, SPI_CFG2 register content cannot be
      --  modified. This bit can be set when SPI is disabled only else it is
      --  write protected. It is cleared and cannot be set when MODF bit is
      --  set.
      IOLOCK         : Boolean := False;
      --  unspecified
      Reserved_17_31 : HAL.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SPI_CR1_Register use record
      SPE            at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      MASRX          at 0 range 8 .. 8;
      CSTART         at 0 range 9 .. 9;
      CSUSP          at 0 range 10 .. 10;
      HDDIR          at 0 range 11 .. 11;
      SSI            at 0 range 12 .. 12;
      CRC33_17       at 0 range 13 .. 13;
      RCRCINI        at 0 range 14 .. 14;
      TCRCINI        at 0 range 15 .. 15;
      IOLOCK         at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype SPI_CR2_TSIZE_Field is HAL.UInt16;

   type SPI_CR2_Register is record
      --  number of data at current transfer When these bits are changed by
      --  software, the SPI has to be disabled. Endless transaction is
      --  initialized when CSTART is set while zero value is stored at TSIZE.
      --  TSIZE cannot be set to 0xFFFF respective 0x3FFF value when CRC is
      --  enabled. Note: TSIZE[15:10] bits are reserved at limited feature set
      --  instances and must be kept at reset value.
      TSIZE          : SPI_CR2_TSIZE_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SPI_CR2_Register use record
      TSIZE          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype SPI_CFG1_DSIZE_Field is HAL.UInt5;
   subtype SPI_CFG1_FTHLV_Field is HAL.UInt4;
   subtype SPI_CFG1_CRCSIZE_Field is HAL.UInt5;
   subtype SPI_CFG1_MBR_Field is HAL.UInt3;

   --  SPI configuration register 1
   type SPI_CFG1_Register is record
      --  number of bits in at single SPI data frame ..... Note: Maximum data
      --  size can be limited up to 16-bits at some instances. At instances
      --  with limited set of features, DSIZE2:0] bits are reserved and must be
      --  kept at reset state. DSIZE[4:3] bits then control next settings of
      --  data size: 00xxx: 8-bits 01xxx: 16-bits 10xxx: 24-bits 11xxx:
      --  32-bits.
      DSIZE          : SPI_CFG1_DSIZE_Field := 16#7#;
      --  FIFO threshold level Defines number of data frames at single data
      --  packet. Size of the packet should not exceed 1/2 of FIFO space. SPI
      --  interface is more efficient if configured packet sizes are aligned
      --  with data register access parallelism: If SPI data register is
      --  accessed as a 16-bit register and DSIZE ¤ 8 bit, better to select
      --  FTHLV = 2, 4, 6. If SPI data register is accessed as a 32-bit
      --  register and DSIZE> 8 bit, better to select FTHLV = 2, 4, 6, while
      --  if DSIZE ¤ 8bit, better to select FTHLV = 4, 8, 12. Note:
      --  FTHLV[3:2] bits are reserved at instances with limited set of
      --  features
      FTHLV          : SPI_CFG1_FTHLV_Field := 16#0#;
      --  behavior of slave transmitter at underrun condition For more details
      --  see underrun condition.
      UDRCFG         : Boolean := False;
      --  unspecified
      Reserved_10_13 : HAL.UInt4 := 16#0#;
      --  Rx DMA stream enable
      RXDMAEN        : Boolean := False;
      --  Tx DMA stream enable
      TXDMAEN        : Boolean := False;
      --  length of CRC frame to be transacted and compared Most significant
      --  bits are taken into account from polynomial calculation when CRC
      --  result is transacted or compared. The length of the polynomial is not
      --  affected by this setting. ..... The value must be set equal or
      --  multiply of data size (DSIZE[4:0]). Its maximum size corresponds to
      --  DSIZE maximum at the instance. Note: The most significant bit at
      --  CRCSIZE bit field is reserved at the peripheral instances where data
      --  size is limited to 16-bit.
      CRCSIZE        : SPI_CFG1_CRCSIZE_Field := 16#7#;
      --  unspecified
      Reserved_21_21 : HAL.Bit := 16#0#;
      --  hardware CRC computation enable
      CRCEN          : Boolean := False;
      --  unspecified
      Reserved_23_27 : HAL.UInt5 := 16#0#;
      --  master baud rate prescaler setting Note: MBR setting is considered at
      --  slave working at TI mode, too (see mode).
      MBR            : SPI_CFG1_MBR_Field := 16#0#;
      --  bypass of the prescaler at master baud rate clock generator
      BPASS          : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SPI_CFG1_Register use record
      DSIZE          at 0 range 0 .. 4;
      FTHLV          at 0 range 5 .. 8;
      UDRCFG         at 0 range 9 .. 9;
      Reserved_10_13 at 0 range 10 .. 13;
      RXDMAEN        at 0 range 14 .. 14;
      TXDMAEN        at 0 range 15 .. 15;
      CRCSIZE        at 0 range 16 .. 20;
      Reserved_21_21 at 0 range 21 .. 21;
      CRCEN          at 0 range 22 .. 22;
      Reserved_23_27 at 0 range 23 .. 27;
      MBR            at 0 range 28 .. 30;
      BPASS          at 0 range 31 .. 31;
   end record;

   subtype SPI_CFG2_MSSI_Field is HAL.UInt4;
   subtype SPI_CFG2_MIDI_Field is HAL.UInt4;
   subtype SPI_CFG2_COMM_Field is HAL.UInt2;
   subtype SPI_CFG2_SP_Field is HAL.UInt3;

   --  SPI configuration register 2
   type SPI_CFG2_Register is record
      --  Master SS Idleness Specifies an extra delay, expressed in number of
      --  SPI clock cycle periods, inserted additionally between active edge of
      --  SS opening a session and the beginning of the first data frame of the
      --  session in Master mode when SSOE is enabled. ... Note: This feature
      --  is not supported in TI mode. To include the delay, the SPI must be
      --  disabled and re-enabled between sessions.
      MSSI           : SPI_CFG2_MSSI_Field := 16#0#;
      --  master Inter-Data Idleness Specifies minimum time delay (expressed in
      --  SPI clock cycles periods) inserted between two consecutive data
      --  frames in Master mode. ... Note: This feature is not supported in TI
      --  mode.
      MIDI           : SPI_CFG2_MIDI_Field := 16#0#;
      --  unspecified
      Reserved_8_12  : HAL.UInt5 := 16#0#;
      --  RDY signal input/output management Note: When DSIZE at the SPI_CFG1
      --  register is configured shorter than 8-bit, the RDIOM bit has to be
      --  kept at zero.
      RDIOM          : Boolean := False;
      --  RDY signal input/output polarity
      RDIOP          : Boolean := False;
      --  swap functionality of MISO and MOSI pins When this bit is set, the
      --  function of MISO and MOSI pins alternate functions are inverted.
      --  Original MISO pin becomes MOSI and original MOSI pin becomes MISO.
      IOSWP          : Boolean := False;
      --  unspecified
      Reserved_16_16 : HAL.Bit := 16#0#;
      --  SPI Communication Mode
      COMM           : SPI_CFG2_COMM_Field := 16#0#;
      --  serial protocol others: reserved, must not be used
      SP             : SPI_CFG2_SP_Field := 16#0#;
      --  SPI Master
      MASTER         : Boolean := False;
      --  data frame format
      LSBFRST        : Boolean := False;
      --  clock phase
      CPHA           : Boolean := False;
      --  clock polarity
      CPOL           : Boolean := False;
      --  software management of SS signal input When master uses hardware SS
      --  output (SSM=0 and SSOE=1) the SS signal input is forced to not active
      --  state internally to prevent master mode fault error.
      SSM            : Boolean := False;
      --  unspecified
      Reserved_27_27 : HAL.Bit := 16#0#;
      --  SS input/output polarity
      SSIOP          : Boolean := False;
      --  SS output enable This bit is taken into account in Master mode only
      SSOE           : Boolean := False;
      --  SS output management in Master mode This bit is taken into account in
      --  Master mode when SSOE is enabled. It allows the SS output to be
      --  configured between two consecutive data transfers.
      SSOM           : Boolean := False;
      --  alternate function GPIOs control This bit is taken into account when
      --  SPE=0 only When SPI has to be disabled temporary for a specific
      --  configuration reason (e.g. CRC reset, CPHA or HDDIR change) setting
      --  this bit prevents any glitches on the associated outputs configured
      --  at alternate function mode by keeping them forced at state
      --  corresponding the current SPI configuration.
      AFCNTR         : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SPI_CFG2_Register use record
      MSSI           at 0 range 0 .. 3;
      MIDI           at 0 range 4 .. 7;
      Reserved_8_12  at 0 range 8 .. 12;
      RDIOM          at 0 range 13 .. 13;
      RDIOP          at 0 range 14 .. 14;
      IOSWP          at 0 range 15 .. 15;
      Reserved_16_16 at 0 range 16 .. 16;
      COMM           at 0 range 17 .. 18;
      SP             at 0 range 19 .. 21;
      MASTER         at 0 range 22 .. 22;
      LSBFRST        at 0 range 23 .. 23;
      CPHA           at 0 range 24 .. 24;
      CPOL           at 0 range 25 .. 25;
      SSM            at 0 range 26 .. 26;
      Reserved_27_27 at 0 range 27 .. 27;
      SSIOP          at 0 range 28 .. 28;
      SSOE           at 0 range 29 .. 29;
      SSOM           at 0 range 30 .. 30;
      AFCNTR         at 0 range 31 .. 31;
   end record;

   type SPI_IER_Register is record
      --  RXP interrupt enable
      RXPIE          : Boolean := False;
      --  TXP interrupt enable TXPIE is set by software and cleared by TXTF
      --  flag set event.
      TXPIE          : Boolean := False;
      --  DXP interrupt enabled DXPIE is set by software and cleared by TXTF
      --  flag set event.
      DXPIE          : Boolean := False;
      --  EOT, SUSP and TXC interrupt enable
      EOTIE          : Boolean := False;
      --  TXTFIE interrupt enable
      TXTFIE         : Boolean := False;
      --  UDR interrupt enable
      UDRIE          : Boolean := False;
      --  OVR interrupt enable
      OVRIE          : Boolean := False;
      --  CRC error interrupt enable
      CRCEIE         : Boolean := False;
      --  TIFRE interrupt enable
      TIFREIE        : Boolean := False;
      --  mode Fault interrupt enable
      MODFIE         : Boolean := False;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SPI_IER_Register use record
      RXPIE          at 0 range 0 .. 0;
      TXPIE          at 0 range 1 .. 1;
      DXPIE          at 0 range 2 .. 2;
      EOTIE          at 0 range 3 .. 3;
      TXTFIE         at 0 range 4 .. 4;
      UDRIE          at 0 range 5 .. 5;
      OVRIE          at 0 range 6 .. 6;
      CRCEIE         at 0 range 7 .. 7;
      TIFREIE        at 0 range 8 .. 8;
      MODFIE         at 0 range 9 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype SPI_SR_RXPLVL_Field is HAL.UInt2;
   subtype SPI_SR_CTSIZE_Field is HAL.UInt16;

   type SPI_SR_Register is record
      --  Read-only. Rx-Packet available RXP flag is changed by hardware. It
      --  monitors number of overall data currently available at RxFIFO if SPI
      --  is enabled. It has to be checked once a data packet is completely
      --  read out from RxFIFO.
      RXP            : Boolean;
      --  Read-only. Tx-Packet space available TXP flag is changed by hardware.
      --  It monitors overall space currently available at TxFIFO no matter if
      --  SPI is enabled or not. It has to be checked once a complete data
      --  packet is stored at TxFIFO.
      TXP            : Boolean;
      --  Read-only. duplex packet DXP flag is set whenever both TXP and RXP
      --  flags are set regardless SPI mode.
      DXP            : Boolean;
      --  Read-only. end of transfer EOT is set by hardware as soon as a full
      --  transfer is complete, that is when TSIZE number of data have been
      --  transmitted and/or received on the SPI. EOT is cleared by software
      --  write 1 to EOTC bit at SPI_IFCR. EOT flag triggers an interrupt if
      --  EOTIE bit is set. If DXP flag is used until TXTF flag is set and
      --  DXPIE is cleared, EOT can be used to download the last packets
      --  contained into RxFIFO in one-shot. In master, EOT event terminates
      --  the data transaction and handles SS output optionally. When CRC is
      --  applied, the EOT event is extended over the CRC frame transaction. To
      --  restart the internal state machine properly, SPI is strongly
      --  suggested to be disabled and re-enabled before next transaction
      --  starts despite its setting is not changed.
      EOT            : Boolean;
      --  Read-only. transmission transfer filled TXTF is set by hardware as
      --  soon as all of the data packets in a transfer have been submitted for
      --  transmission by application software or DMA, that is when TSIZE
      --  number of data have been pushed into the TxFIFO. This bit is cleared
      --  by software write 1 to TXTFC bit at SPI_IFCR TXTF flag triggers an
      --  interrupt if TXTFIE bit is set. TXTF setting clears the TXPIE and
      --  DXPIE masks so to off-load application software from calculating when
      --  to disable TXP and DXP interrupts.
      TXTF           : Boolean;
      --  Read-only. underrun at slave transmission mode This bit is cleared by
      --  writing 1 to UDRC bit at SPI_IFCR Note: UDR flag applies to Slave
      --  mode only
      UDR            : Boolean;
      --  Read-only. overrun This bit is cleared by writing 1 to OVRC bit at
      --  SPI_IFCR
      OVR            : Boolean;
      --  Read-only. CRC error This bit is cleared by writing 1 to CRCEC bit at
      --  SPI_IFCR
      CRCE           : Boolean;
      --  Read-only. TI frame format error This bit is cleared by writing 1 to
      --  TIFREC bit at SPI_IFCR
      TIFRE          : Boolean;
      --  Read-only. mode fault This bit is cleared by writing 1 to MODFC bit
      --  at SPI_IFCR
      MODF           : Boolean;
      --  unspecified
      Reserved_10_10 : HAL.Bit;
      --  Read-only. suspension status In Master mode, SUSP is set by hardware
      --  either as soon as the current frame is completed after CSUSP request
      --  is done or at master automatic suspend receive mode (MASRX bit is set
      --  at SPI_CR1 register) on RxFIFO full condition. SUSP generates an
      --  interrupt when EOTIE is set. This bit has to be cleared prior SPI is
      --  disabled by writing 1 to SUSPC bit at SPI_IFCR.
      SUSP           : Boolean;
      --  Read-only. TxFIFO transmission complete The flag behavior depends on
      --  TSIZE setting. When TSIZE=0 the TXC is changed by hardware
      --  exclusively and it raises each time the TxFIFO becomes empty and
      --  there is no activity on the bus. If TSIZE <>0 there is no specific
      --  reason to monitor TXC as it just copies the EOT flag value including
      --  its software clearing. The TXC generates an interrupt when EOTIE is
      --  set.
      TXC            : Boolean;
      --  Read-only. RxFIFO packing level When RXWNE=0 and data size is set up
      --  to 16-bit, the value gives number of remaining data frames persisting
      --  at RxFIFO. Note: (*): Optional value when data size is set up to
      --  8-bit only. When data size is greater than 16-bit, these bits are
      --  always read as 00. In that consequence, the single data frame
      --  received at the FIFO cannot be detected neither by RWNE nor by RXPLVL
      --  bits if data size is set from 17 to 24 bits. The user then must apply
      --  other methods like TSIZE>0 or FTHLV=0.
      RXPLVL         : SPI_SR_RXPLVL_Field;
      --  Read-only. RxFIFO word not empty Note: This bit value does not depend
      --  on DSIZE setting and keeps together with RXPLVL[1:0] information
      --  about RxFIFO occupancy by residual data.
      RXWNE          : Boolean;
      --  Read-only. number of data frames remaining in current TSIZE session
      --  The value is not quite reliable when traffic is ongoing on bus or
      --  during autonomous operation at low-power mode. Note: CTSIZE[15:0]
      --  bits are not available at instances with limited set of features
      CTSIZE         : SPI_SR_CTSIZE_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SPI_SR_Register use record
      RXP            at 0 range 0 .. 0;
      TXP            at 0 range 1 .. 1;
      DXP            at 0 range 2 .. 2;
      EOT            at 0 range 3 .. 3;
      TXTF           at 0 range 4 .. 4;
      UDR            at 0 range 5 .. 5;
      OVR            at 0 range 6 .. 6;
      CRCE           at 0 range 7 .. 7;
      TIFRE          at 0 range 8 .. 8;
      MODF           at 0 range 9 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      SUSP           at 0 range 11 .. 11;
      TXC            at 0 range 12 .. 12;
      RXPLVL         at 0 range 13 .. 14;
      RXWNE          at 0 range 15 .. 15;
      CTSIZE         at 0 range 16 .. 31;
   end record;

   type SPI_IFCR_Register is record
      --  unspecified
      Reserved_0_2   : HAL.UInt3 := 16#0#;
      --  Write-only. end of transfer flag clear Writing a 1 into this bit
      --  clears EOT flag in the SPI_SR register
      EOTC           : Boolean := False;
      --  Write-only. transmission transfer filled flag clear Writing a 1 into
      --  this bit clears TXTF flag in the SPI_SR register
      TXTFC          : Boolean := False;
      --  Write-only. underrun flag clear Writing a 1 into this bit clears UDR
      --  flag in the SPI_SR register
      UDRC           : Boolean := False;
      --  Write-only. overrun flag clear Writing a 1 into this bit clears OVR
      --  flag in the SPI_SR register
      OVRC           : Boolean := False;
      --  Write-only. CRC error flag clear Writing a 1 into this bit clears
      --  CRCE flag in the SPI_SR register
      CRCEC          : Boolean := False;
      --  Write-only. TI frame format error flag clear Writing a 1 into this
      --  bit clears TIFRE flag in the SPI_SR register
      TIFREC         : Boolean := False;
      --  Write-only. mode fault flag clear Writing a 1 into this bit clears
      --  MODF flag in the SPI_SR register
      MODFC          : Boolean := False;
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  Write-only. SUSPend flag clear Writing a 1 into this bit clears SUSP
      --  flag in the SPI_SR register
      SUSPC          : Boolean := False;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SPI_IFCR_Register use record
      Reserved_0_2   at 0 range 0 .. 2;
      EOTC           at 0 range 3 .. 3;
      TXTFC          at 0 range 4 .. 4;
      UDRC           at 0 range 5 .. 5;
      OVRC           at 0 range 6 .. 6;
      CRCEC          at 0 range 7 .. 7;
      TIFREC         at 0 range 8 .. 8;
      MODFC          at 0 range 9 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      SUSPC          at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype SPI_AUTOCR_TRIGSEL_Field is HAL.UInt4;

   type SPI_AUTOCR_Register is record
      --  unspecified
      Reserved_0_15  : HAL.UInt16 := 16#0#;
      --  trigger selection (refer ). ... Note: these bits can be written only
      --  when SPE = 0.
      TRIGSEL        : SPI_AUTOCR_TRIGSEL_Field := 16#0#;
      --  trigger polarity Note: This bit can be written only when SPE = 0.
      TRIGPOL        : Boolean := False;
      --  trigger of CSTART control enable Note: if user can't prevent trigger
      --  event during write, the TRIGEN has to be changed when SPI is disabled
      TRIGEN         : Boolean := False;
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SPI_AUTOCR_Register use record
      Reserved_0_15  at 0 range 0 .. 15;
      TRIGSEL        at 0 range 16 .. 19;
      TRIGPOL        at 0 range 20 .. 20;
      TRIGEN         at 0 range 21 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Serial peripheral interface
   type SPI_Peripheral is record
      SPI_CR1     : aliased SPI_CR1_Register;
      SPI_CR2     : aliased SPI_CR2_Register;
      --  SPI configuration register 1
      SPI_CFG1    : aliased SPI_CFG1_Register;
      --  SPI configuration register 2
      SPI_CFG2    : aliased SPI_CFG2_Register;
      SPI_IER     : aliased SPI_IER_Register;
      SPI_SR      : aliased SPI_SR_Register;
      SPI_IFCR    : aliased SPI_IFCR_Register;
      SPI_AUTOCR  : aliased SPI_AUTOCR_Register;
      SPI_TXDR    : aliased HAL.UInt32;
      SPI_RXDR    : aliased HAL.UInt32;
      --  SPI polynomial register
      SPI_CRCPOLY : aliased HAL.UInt32;
      SPI_TXCRC   : aliased HAL.UInt32;
      SPI_RXCRC   : aliased HAL.UInt32;
      --  SPI underrun data register
      SPI_UDRDR   : aliased HAL.UInt32;
   end record
     with Volatile;

   for SPI_Peripheral use record
      SPI_CR1     at 16#0# range 0 .. 31;
      SPI_CR2     at 16#4# range 0 .. 31;
      SPI_CFG1    at 16#8# range 0 .. 31;
      SPI_CFG2    at 16#C# range 0 .. 31;
      SPI_IER     at 16#10# range 0 .. 31;
      SPI_SR      at 16#14# range 0 .. 31;
      SPI_IFCR    at 16#18# range 0 .. 31;
      SPI_AUTOCR  at 16#1C# range 0 .. 31;
      SPI_TXDR    at 16#20# range 0 .. 31;
      SPI_RXDR    at 16#30# range 0 .. 31;
      SPI_CRCPOLY at 16#40# range 0 .. 31;
      SPI_TXCRC   at 16#44# range 0 .. 31;
      SPI_RXCRC   at 16#48# range 0 .. 31;
      SPI_UDRDR   at 16#4C# range 0 .. 31;
   end record;

   --  Serial peripheral interface
   SEC_SPI1_Periph : aliased SPI_Peripheral
     with Import, Address => SEC_SPI1_Base;

   --  Serial peripheral interface
   SEC_SPI2_Periph : aliased SPI_Peripheral
     with Import, Address => SEC_SPI2_Base;

   --  Serial peripheral interface
   SEC_SPI3_Periph : aliased SPI_Peripheral
     with Import, Address => SEC_SPI3_Base;

   --  Serial peripheral interface
   SPI1_Periph : aliased SPI_Peripheral
     with Import, Address => SPI1_Base;

   --  Serial peripheral interface
   SPI2_Periph : aliased SPI_Peripheral
     with Import, Address => SPI2_Base;

   --  Serial peripheral interface
   SPI3_Periph : aliased SPI_Peripheral
     with Import, Address => SPI3_Base;

end STM32_SVD.SPI;
