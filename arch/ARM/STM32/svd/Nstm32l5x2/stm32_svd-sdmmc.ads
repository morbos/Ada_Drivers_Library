--  This spec has been automatically generated from STM32L562.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.SDMMC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype SDMMC_POWER_PWRCTRL_Field is HAL.UInt2;

   --  SDMMC power control register
   type SDMMC_POWER_Register is record
      --  SDMMC state control bits. These bits can only be written when the
      --  SDMMC is not in the power-on state (PWRCTRL?11). These bits are used
      --  to define the functional state of the SDMMC signals: Any further
      --  write will be ignored, PWRCTRL value will keep 11.
      PWRCTRL       : SDMMC_POWER_PWRCTRL_Field := 16#0#;
      --  Voltage switch sequence start. This bit is used to start the timing
      --  critical section of the voltage switch sequence:
      VSWITCH       : Boolean := False;
      --  Voltage switch procedure enable. This bit can only be written by
      --  firmware when CPSM is disabled (CPSMEN = 0). This bit is used to stop
      --  the SDMMC_CK after the voltage switch command response:
      VSWITCHEN     : Boolean := False;
      --  Data and command direction signals polarity selection. This bit can
      --  only be written when the SDMMC is in the power-off state (PWRCTRL =
      --  00).
      DIRPOL        : Boolean := False;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SDMMC_POWER_Register use record
      PWRCTRL       at 0 range 0 .. 1;
      VSWITCH       at 0 range 2 .. 2;
      VSWITCHEN     at 0 range 3 .. 3;
      DIRPOL        at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype SDMMC_CLKCR_CLKDIV_Field is HAL.UInt10;
   subtype SDMMC_CLKCR_WIDBUS_Field is HAL.UInt2;
   subtype SDMMC_CLKCR_SELCLKRX_Field is HAL.UInt2;

   --  The SDMMC_CLKCR register controls the SDMMC_CK output clock, the
   --  SDMMC_RX_CLK receive clock, and the bus width.
   type SDMMC_CLKCR_Register is record
      --  Clock divide factor This bit can only be written when the CPSM and
      --  DPSM are not active (CPSMACT = 0 and DPSMACT = 0). This field defines
      --  the divide factor between the input clock (SDMMCCLK) and the output
      --  clock (SDMMC_CK): SDMMC_CK frequency = SDMMCCLK / [2 * CLKDIV]. 0xx:
      --  etc.. xxx: etc..
      CLKDIV         : SDMMC_CLKCR_CLKDIV_Field := 16#0#;
      --  unspecified
      Reserved_10_11 : HAL.UInt2 := 16#0#;
      --  Power saving configuration bit This bit can only be written when the
      --  CPSM and DPSM are not active (CPSMACT = 0 and DPSMACT = 0) For power
      --  saving, the SDMMC_CK clock output can be disabled when the bus is
      --  idle by setting PWRSAV:
      PWRSAV         : Boolean := False;
      --  unspecified
      Reserved_13_13 : HAL.Bit := 16#0#;
      --  Wide bus mode enable bit This bit can only be written when the CPSM
      --  and DPSM are not active (CPSMACT = 0 and DPSMACT = 0)
      WIDBUS         : SDMMC_CLKCR_WIDBUS_Field := 16#0#;
      --  SDMMC_CK dephasing selection bit for data and Command. This bit can
      --  only be written when the CPSM and DPSM are not active (CPSMACT = 0
      --  and DPSMACT = 0). When clock division = 1 (CLKDIV = 0), this bit has
      --  no effect. Data and Command change on SDMMC_CK falling edge. When
      --  clock division &gt;1 (CLKDIV &gt; 0) &amp; DDR = 0: - SDMMC_CK edge
      --  occurs on SDMMCCLK rising edge. When clock division >1 (CLKDIV > 0) &
      --  DDR = 1: - Data changed on the SDMMCCLK falling edge succeeding a
      --  SDMMC_CK edge. - SDMMC_CK edge occurs on SDMMCCLK rising edge. - Data
      --  changed on the SDMMC_CK falling edge succeeding a SDMMC_CK edge. -
      --  SDMMC_CK edge occurs on SDMMCCLK rising edge.
      NEGEDGE        : Boolean := False;
      --  Hardware flow control enable This bit can only be written when the
      --  CPSM and DPSM are not active (CPSMACT = 0 and DPSMACT = 0) When
      --  Hardware flow control is enabled, the meaning of the TXFIFOE and
      --  RXFIFOF flags change, please see SDMMC status register definition in
      --  Section56.8.11.
      HWFC_EN        : Boolean := False;
      --  Data rate signaling selection This bit can only be written when the
      --  CPSM and DPSM are not active (CPSMACT = 0 and DPSMACT = 0) DDR rate
      --  shall only be selected with 4-bit or 8-bit wide bus mode. (WIDBUS
      --  &gt; 00). DDR = 1 has no effect when WIDBUS = 00 (1-bit wide bus).
      --  DDR rate shall only be selected with clock division &gt;1. (CLKDIV
      --  &gt; 0)
      DDR            : Boolean := False;
      --  Bus speed mode selection between DS, HS, SDR12, SDR25 and SDR50,
      --  DDR50, SDR104. This bit can only be written when the CPSM and DPSM
      --  are not active (CPSMACT = 0 and DPSMACT = 0)
      BUSSPEED       : Boolean := False;
      --  Receive clock selection. These bits can only be written when the CPSM
      --  and DPSM are not active (CPSMACT = 0 and DPSMACT = 0)
      SELCLKRX       : SDMMC_CLKCR_SELCLKRX_Field := 16#0#;
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SDMMC_CLKCR_Register use record
      CLKDIV         at 0 range 0 .. 9;
      Reserved_10_11 at 0 range 10 .. 11;
      PWRSAV         at 0 range 12 .. 12;
      Reserved_13_13 at 0 range 13 .. 13;
      WIDBUS         at 0 range 14 .. 15;
      NEGEDGE        at 0 range 16 .. 16;
      HWFC_EN        at 0 range 17 .. 17;
      DDR            at 0 range 18 .. 18;
      BUSSPEED       at 0 range 19 .. 19;
      SELCLKRX       at 0 range 20 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   subtype SDMMC_CMDR_CMDINDEX_Field is HAL.UInt6;
   subtype SDMMC_CMDR_WAITRESP_Field is HAL.UInt2;

   --  The SDMMC_CMDR register contains the command index and command type
   --  bits. The command index is sent to a card as part of a command message.
   --  The command type bits control the command path state machine (CPSM).
   type SDMMC_CMDR_Register is record
      --  Command index. This bit can only be written by firmware when CPSM is
      --  disabled (CPSMEN = 0). The command index is sent to the card as part
      --  of a command message.
      CMDINDEX       : SDMMC_CMDR_CMDINDEX_Field := 16#0#;
      --  The CPSM treats the command as a data transfer command, stops the
      --  interrupt period, and signals DataEnable to the DPSM This bit can
      --  only be written by firmware when CPSM is disabled (CPSMEN = 0). If
      --  this bit is set, the CPSM issues an end of interrupt period and
      --  issues DataEnable signal to the DPSM when the command is sent.
      CMDTRANS       : Boolean := False;
      --  The CPSM treats the command as a Stop Transmission command and
      --  signals Abort to the DPSM. This bit can only be written by firmware
      --  when CPSM is disabled (CPSMEN = 0). If this bit is set, the CPSM
      --  issues the Abort signal to the DPSM when the command is sent.
      CMDSTOP        : Boolean := False;
      --  Wait for response bits. This bit can only be written by firmware when
      --  CPSM is disabled (CPSMEN = 0). They are used to configure whether the
      --  CPSM is to wait for a response, and if yes, which kind of response.
      WAITRESP       : SDMMC_CMDR_WAITRESP_Field := 16#0#;
      --  CPSM waits for interrupt request. If this bit is set, the CPSM
      --  disables command timeout and waits for an card interrupt request
      --  (Response). If this bit is cleared in the CPSM Wait state, will cause
      --  the abort of the interrupt mode.
      WAITINT        : Boolean := False;
      --  CPSM Waits for end of data transfer (CmdPend internal signal) from
      --  DPSM. This bit when set, the CPSM waits for the end of data transfer
      --  trigger before it starts sending a command. WAITPEND is only taken
      --  into account when DTMODE = MMC stream data transfer, WIDBUS = 1-bit
      --  wide bus mode, DPSMACT = 1 and DTDIR = from host to card.
      WAITPEND       : Boolean := False;
      --  Command path state machine (CPSM) Enable bit This bit is written 1 by
      --  firmware, and cleared by hardware when the CPSM enters the Idle
      --  state. If this bit is set, the CPSM is enabled. When DTEN = 1, no
      --  command will be transfered nor boot procedure will be started. CPSMEN
      --  is cleared to 0.
      CPSMEN         : Boolean := False;
      --  Hold new data block transmission and reception in the DPSM. If this
      --  bit is set, the DPSM will not move from the Wait_S state to the Send
      --  state or from the Wait_R state to the Receive state.
      DTHOLD         : Boolean := False;
      --  Select the boot mode procedure to be used. This bit can only be
      --  written by firmware when CPSM is disabled (CPSMEN = 0)
      BOOTMODE       : Boolean := False;
      --  Enable boot mode procedure.
      BOOTEN         : Boolean := False;
      --  The CPSM treats the command as a Suspend or Resume command and
      --  signals interrupt period start/end. This bit can only be written by
      --  firmware when CPSM is disabled (CPSMEN = 0). CMDSUSPEND = 1 and
      --  CMDTRANS = 0 Suspend command, start interrupt period when response
      --  bit BS=0. CMDSUSPEND = 1 and CMDTRANS = 1 Resume command with data,
      --  end interrupt period when response bit DF=1.
      CMDSUSPEND     : Boolean := False;
      --  unspecified
      Reserved_17_31 : HAL.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SDMMC_CMDR_Register use record
      CMDINDEX       at 0 range 0 .. 5;
      CMDTRANS       at 0 range 6 .. 6;
      CMDSTOP        at 0 range 7 .. 7;
      WAITRESP       at 0 range 8 .. 9;
      WAITINT        at 0 range 10 .. 10;
      WAITPEND       at 0 range 11 .. 11;
      CPSMEN         at 0 range 12 .. 12;
      DTHOLD         at 0 range 13 .. 13;
      BOOTMODE       at 0 range 14 .. 14;
      BOOTEN         at 0 range 15 .. 15;
      CMDSUSPEND     at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype SDMMC_RESPCMDR_RESPCMD_Field is HAL.UInt6;

   --  SDMMC command response register
   type SDMMC_RESPCMDR_Register is record
      --  Read-only. Response command index
      RESPCMD       : SDMMC_RESPCMDR_RESPCMD_Field;
      --  unspecified
      Reserved_6_31 : HAL.UInt26;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SDMMC_RESPCMDR_Register use record
      RESPCMD       at 0 range 0 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   subtype SDMMC_DLENR_DATALENGTH_Field is HAL.UInt25;

   --  The SDMMC_DLENR register contains the number of data bytes to be
   --  transferred. The value is loaded into the data counter when data
   --  transfer starts.
   type SDMMC_DLENR_Register is record
      --  Data length value This register can only be written by firmware when
      --  DPSM is inactive (DPSMACT = 0). Number of data bytes to be
      --  transferred. When DDR = 1 DATALENGTH is truncated to a multiple of 2.
      --  (The last odd byte is not transfered) When DATALENGTH = 0 no data
      --  will be transfered, when requested by a CPSMEN and CMDTRANS = 1 also
      --  no command will be transfered. DTEN and CPSMEN are cleared to 0.
      DATALENGTH     : SDMMC_DLENR_DATALENGTH_Field := 16#0#;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SDMMC_DLENR_Register use record
      DATALENGTH     at 0 range 0 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype SDMMC_DCTRL_DTMODE_Field is HAL.UInt2;
   subtype SDMMC_DCTRL_DBLOCKSIZE_Field is HAL.UInt4;

   --  The SDMMC_DCTRL register control the data path state machine (DPSM).
   type SDMMC_DCTRL_Register is record
      --  Data transfer enable bit This bit can only be written by firmware
      --  when DPSM is inactive (DPSMACT = 0). This bit is cleared by Hardware
      --  when data transfer completes. This bit shall only be used to transfer
      --  data when no associated data transfer command is used, i.e. shall not
      --  be used with SD or eMMC cards.
      DTEN           : Boolean := False;
      --  Data transfer direction selection This bit can only be written by
      --  firmware when DPSM is inactive (DPSMACT = 0).
      DTDIR          : Boolean := False;
      --  Data transfer mode selection. This bit can only be written by
      --  firmware when DPSM is inactive (DPSMACT = 0).
      DTMODE         : SDMMC_DCTRL_DTMODE_Field := 16#0#;
      --  Data block size This bit can only be written by firmware when DPSM is
      --  inactive (DPSMACT = 0). Define the data block length when the block
      --  data transfer mode is selected: When DATALENGTH is not a multiple of
      --  DBLOCKSIZE, the transfered data is truncated at a multiple of
      --  DBLOCKSIZE. (Any remain data will not be transfered.) When DDR = 1,
      --  DBLOCKSIZE = 0000 shall not be used. (No data will be transfered)
      DBLOCKSIZE     : SDMMC_DCTRL_DBLOCKSIZE_Field := 16#0#;
      --  Read wait start. If this bit is set, read wait operation starts.
      RWSTART        : Boolean := False;
      --  Read wait stop This bit is written by firmware and auto cleared by
      --  hardware when the DPSM moves from the READ_WAIT state to the WAIT_R
      --  or IDLE state.
      RWSTOP         : Boolean := False;
      --  Read wait mode. This bit can only be written by firmware when DPSM is
      --  inactive (DPSMACT = 0).
      RWMOD          : Boolean := False;
      --  SD I/O interrupt enable functions This bit can only be written by
      --  firmware when DPSM is inactive (DPSMACT = 0). If this bit is set, the
      --  DPSM enables the SD I/O card specific interrupt operation.
      SDIOEN         : Boolean := False;
      --  Enable the reception of the boot acknowledgment. This bit can only be
      --  written by firmware when DPSM is inactive (DPSMACT = 0).
      BOOTACKEN      : Boolean := False;
      --  FIFO reset, will flush any remaining data. This bit can only be
      --  written by firmware when IDMAEN= 0 and DPSM is active (DPSMACT = 1).
      --  This bit will only take effect when a transfer error or transfer hold
      --  occurs.
      FIFORST        : Boolean := False;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SDMMC_DCTRL_Register use record
      DTEN           at 0 range 0 .. 0;
      DTDIR          at 0 range 1 .. 1;
      DTMODE         at 0 range 2 .. 3;
      DBLOCKSIZE     at 0 range 4 .. 7;
      RWSTART        at 0 range 8 .. 8;
      RWSTOP         at 0 range 9 .. 9;
      RWMOD          at 0 range 10 .. 10;
      SDIOEN         at 0 range 11 .. 11;
      BOOTACKEN      at 0 range 12 .. 12;
      FIFORST        at 0 range 13 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   subtype SDMMC_DCNTR_DATACOUNT_Field is HAL.UInt25;

   --  The SDMMC_DCNTR register loads the value from the data length register
   --  (see SDMMC_DLENR) when the DPSM moves from the Idle state to the Wait_R
   --  or Wait_S state. As data is transferred, the counter decrements the
   --  value until it reaches 0. The DPSM then moves to the Idle state and when
   --  there has been no error, the data status end flag (DATAEND) is set.
   type SDMMC_DCNTR_Register is record
      --  Read-only. Data count value When read, the number of remaining data
      --  bytes to be transferred is returned. Write has no effect.
      DATACOUNT      : SDMMC_DCNTR_DATACOUNT_Field;
      --  unspecified
      Reserved_25_31 : HAL.UInt7;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SDMMC_DCNTR_Register use record
      DATACOUNT      at 0 range 0 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  The SDMMC_STAR register is a read-only register. It contains two types
   --  of flag:Static flags (bits [29,21,11:0]): these bits remain asserted
   --  until they are cleared by writing to the SDMMC interrupt Clear register
   --  (see SDMMC_ICR)Dynamic flags (bits [20:12]): these bits change state
   --  depending on the state of the underlying logic (for example, FIFO full
   --  and empty flags are asserted and de-asserted as data while written to
   --  the FIFO)
   type SDMMC_STAR_Register is record
      --  Read-only. Command response received (CRC check failed). Interrupt
      --  flag is cleared by writing corresponding interrupt clear bit in
      --  SDMMC_ICR.
      CCRCFAIL       : Boolean;
      --  Read-only. Data block sent/received (CRC check failed). Interrupt
      --  flag is cleared by writing corresponding interrupt clear bit in
      --  SDMMC_ICR.
      DCRCFAIL       : Boolean;
      --  Read-only. Command response timeout. Interrupt flag is cleared by
      --  writing corresponding interrupt clear bit in SDMMC_ICR. The Command
      --  Timeout period has a fixed value of 64 SDMMC_CK clock periods.
      CTIMEOUT       : Boolean;
      --  Read-only. Data timeout. Interrupt flag is cleared by writing
      --  corresponding interrupt clear bit in SDMMC_ICR.
      DTIMEOUT       : Boolean;
      --  Read-only. Transmit FIFO underrun error or IDMA read transfer error.
      --  Interrupt flag is cleared by writing corresponding interrupt clear
      --  bit in SDMMC_ICR.
      TXUNDERR       : Boolean;
      --  Read-only. Received FIFO overrun error or IDMA write transfer error.
      --  Interrupt flag is cleared by writing corresponding interrupt clear
      --  bit in SDMMC_ICR.
      RXOVERR        : Boolean;
      --  Read-only. Command response received (CRC check passed, or no CRC).
      --  Interrupt flag is cleared by writing corresponding interrupt clear
      --  bit in SDMMC_ICR.
      CMDREND        : Boolean;
      --  Read-only. Command sent (no response required). Interrupt flag is
      --  cleared by writing corresponding interrupt clear bit in SDMMC_ICR.
      CMDSENT        : Boolean;
      --  Read-only. Data transfer ended correctly. (data counter, DATACOUNT is
      --  zero and no errors occur). Interrupt flag is cleared by writing
      --  corresponding interrupt clear bit in SDMMC_ICR.
      DATAEND        : Boolean;
      --  Read-only. Data transfer Hold. Interrupt flag is cleared by writing
      --  corresponding interrupt clear bit in SDMMC_ICR.
      DHOLD          : Boolean;
      --  Read-only. Data block sent/received. (CRC check passed) and DPSM
      --  moves to the READWAIT state. Interrupt flag is cleared by writing
      --  corresponding interrupt clear bit in SDMMC_ICR.
      DBCKEND        : Boolean;
      --  Read-only. Data transfer aborted by CMD12. Interrupt flag is cleared
      --  by writing corresponding interrupt clear bit in SDMMC_ICR.
      DABORT         : Boolean;
      --  Read-only. Data path state machine active, i.e. not in Idle state.
      --  This is a hardware status flag only, does not generate an interrupt.
      DPSMACT        : Boolean;
      --  Read-only. Command path state machine active, i.e. not in Idle state.
      --  This is a hardware status flag only, does not generate an interrupt.
      CPSMACT        : Boolean;
      --  Read-only. Transmit FIFO half empty At least half the number of words
      --  can be written into the FIFO. This bit is cleared when the FIFO
      --  becomes half+1 full.
      TXFIFOHE       : Boolean;
      --  Read-only. Receive FIFO half full There are at least half the number
      --  of words in the FIFO. This bit is cleared when the FIFO becomes
      --  half+1 empty.
      RXFIFOHF       : Boolean;
      --  Read-only. Transmit FIFO full This is a hardware status flag only,
      --  does not generate an interrupt. This bit is cleared when one FIFO
      --  location becomes empty.
      TXFIFOF        : Boolean;
      --  Read-only. Receive FIFO full This bit is cleared when one FIFO
      --  location becomes empty.
      RXFIFOF        : Boolean;
      --  Read-only. Transmit FIFO empty This bit is cleared when one FIFO
      --  location becomes full.
      TXFIFOE        : Boolean;
      --  Read-only. Receive FIFO empty This is a hardware status flag only,
      --  does not generate an interrupt. This bit is cleared when one FIFO
      --  location becomes full.
      RXFIFOE        : Boolean;
      --  Read-only. Inverted value of SDMMC_D0 line (Busy), sampled at the end
      --  of a CMD response and a second time 2 SDMMC_CK cycles after the CMD
      --  response. This bit is reset to not busy when the SDMMCD0 line changes
      --  from busy to not busy. This bit does not signal busy due to data
      --  transfer. This is a hardware status flag only, it does not generate
      --  an interrupt.
      BUSYD0         : Boolean;
      --  Read-only. end of SDMMC_D0 Busy following a CMD response detected.
      --  This indicates only end of busy following a CMD response. This bit
      --  does not signal busy due to data transfer. Interrupt flag is cleared
      --  by writing corresponding interrupt clear bit in SDMMC_ICR.
      BUSYD0END      : Boolean;
      --  Read-only. SDIO interrupt received. Interrupt flag is cleared by
      --  writing corresponding interrupt clear bit in SDMMC_ICR.
      SDIOIT         : Boolean;
      --  Read-only. Boot acknowledgment received (boot acknowledgment check
      --  fail). Interrupt flag is cleared by writing corresponding interrupt
      --  clear bit in SDMMC_ICR.
      ACKFAIL        : Boolean;
      --  Read-only. Boot acknowledgment timeout. Interrupt flag is cleared by
      --  writing corresponding interrupt clear bit in SDMMC_ICR.
      ACKTIMEOUT     : Boolean;
      --  Read-only. Voltage switch critical timing section completion.
      --  Interrupt flag is cleared by writing corresponding interrupt clear
      --  bit in SDMMC_ICR.
      VSWEND         : Boolean;
      --  Read-only. SDMMC_CK stopped in Voltage switch procedure. Interrupt
      --  flag is cleared by writing corresponding interrupt clear bit in
      --  SDMMC_ICR.
      CKSTOP         : Boolean;
      --  Read-only. IDMA transfer error. Interrupt flag is cleared by writing
      --  corresponding interrupt clear bit in SDMMC_ICR.
      IDMATE         : Boolean;
      --  Read-only. IDMA buffer transfer complete. interrupt flag is cleared
      --  by writing corresponding interrupt clear bit in SDMMC_ICR.
      IDMABTC        : Boolean;
      --  unspecified
      Reserved_29_31 : HAL.UInt3;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SDMMC_STAR_Register use record
      CCRCFAIL       at 0 range 0 .. 0;
      DCRCFAIL       at 0 range 1 .. 1;
      CTIMEOUT       at 0 range 2 .. 2;
      DTIMEOUT       at 0 range 3 .. 3;
      TXUNDERR       at 0 range 4 .. 4;
      RXOVERR        at 0 range 5 .. 5;
      CMDREND        at 0 range 6 .. 6;
      CMDSENT        at 0 range 7 .. 7;
      DATAEND        at 0 range 8 .. 8;
      DHOLD          at 0 range 9 .. 9;
      DBCKEND        at 0 range 10 .. 10;
      DABORT         at 0 range 11 .. 11;
      DPSMACT        at 0 range 12 .. 12;
      CPSMACT        at 0 range 13 .. 13;
      TXFIFOHE       at 0 range 14 .. 14;
      RXFIFOHF       at 0 range 15 .. 15;
      TXFIFOF        at 0 range 16 .. 16;
      RXFIFOF        at 0 range 17 .. 17;
      TXFIFOE        at 0 range 18 .. 18;
      RXFIFOE        at 0 range 19 .. 19;
      BUSYD0         at 0 range 20 .. 20;
      BUSYD0END      at 0 range 21 .. 21;
      SDIOIT         at 0 range 22 .. 22;
      ACKFAIL        at 0 range 23 .. 23;
      ACKTIMEOUT     at 0 range 24 .. 24;
      VSWEND         at 0 range 25 .. 25;
      CKSTOP         at 0 range 26 .. 26;
      IDMATE         at 0 range 27 .. 27;
      IDMABTC        at 0 range 28 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   --  The SDMMC_ICR register is a write-only register. Writing a bit with 1
   --  clears the corresponding bit in the SDMMC_STAR status register.
   type SDMMC_ICR_Register is record
      --  CCRCFAIL flag clear bit Set by software to clear the CCRCFAIL flag.
      CCRCFAILC      : Boolean := False;
      --  DCRCFAIL flag clear bit Set by software to clear the DCRCFAIL flag.
      DCRCFAILC      : Boolean := False;
      --  CTIMEOUT flag clear bit Set by software to clear the CTIMEOUT flag.
      CTIMEOUTC      : Boolean := False;
      --  DTIMEOUT flag clear bit Set by software to clear the DTIMEOUT flag.
      DTIMEOUTC      : Boolean := False;
      --  TXUNDERR flag clear bit Set by software to clear TXUNDERR flag.
      TXUNDERRC      : Boolean := False;
      --  RXOVERR flag clear bit Set by software to clear the RXOVERR flag.
      RXOVERRC       : Boolean := False;
      --  CMDREND flag clear bit Set by software to clear the CMDREND flag.
      CMDRENDC       : Boolean := False;
      --  CMDSENT flag clear bit Set by software to clear the CMDSENT flag.
      CMDSENTC       : Boolean := False;
      --  DATAEND flag clear bit Set by software to clear the DATAEND flag.
      DATAENDC       : Boolean := False;
      --  DHOLD flag clear bit Set by software to clear the DHOLD flag.
      DHOLDC         : Boolean := False;
      --  DBCKEND flag clear bit Set by software to clear the DBCKEND flag.
      DBCKENDC       : Boolean := False;
      --  DABORT flag clear bit Set by software to clear the DABORT flag.
      DABORTC        : Boolean := False;
      --  unspecified
      Reserved_12_20 : HAL.UInt9 := 16#0#;
      --  BUSYD0END flag clear bit Set by software to clear the BUSYD0END flag.
      BUSYD0ENDC     : Boolean := False;
      --  SDIOIT flag clear bit Set by software to clear the SDIOIT flag.
      SDIOITC        : Boolean := False;
      --  ACKFAIL flag clear bit Set by software to clear the ACKFAIL flag.
      ACKFAILC       : Boolean := False;
      --  ACKTIMEOUT flag clear bit Set by software to clear the ACKTIMEOUT
      --  flag.
      ACKTIMEOUTC    : Boolean := False;
      --  VSWEND flag clear bit Set by software to clear the VSWEND flag.
      VSWENDC        : Boolean := False;
      --  CKSTOP flag clear bit Set by software to clear the CKSTOP flag.
      CKSTOPC        : Boolean := False;
      --  IDMA transfer error clear bit Set by software to clear the IDMATE
      --  flag.
      IDMATEC        : Boolean := False;
      --  IDMA buffer transfer complete clear bit Set by software to clear the
      --  IDMABTC flag.
      IDMABTCC       : Boolean := False;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SDMMC_ICR_Register use record
      CCRCFAILC      at 0 range 0 .. 0;
      DCRCFAILC      at 0 range 1 .. 1;
      CTIMEOUTC      at 0 range 2 .. 2;
      DTIMEOUTC      at 0 range 3 .. 3;
      TXUNDERRC      at 0 range 4 .. 4;
      RXOVERRC       at 0 range 5 .. 5;
      CMDRENDC       at 0 range 6 .. 6;
      CMDSENTC       at 0 range 7 .. 7;
      DATAENDC       at 0 range 8 .. 8;
      DHOLDC         at 0 range 9 .. 9;
      DBCKENDC       at 0 range 10 .. 10;
      DABORTC        at 0 range 11 .. 11;
      Reserved_12_20 at 0 range 12 .. 20;
      BUSYD0ENDC     at 0 range 21 .. 21;
      SDIOITC        at 0 range 22 .. 22;
      ACKFAILC       at 0 range 23 .. 23;
      ACKTIMEOUTC    at 0 range 24 .. 24;
      VSWENDC        at 0 range 25 .. 25;
      CKSTOPC        at 0 range 26 .. 26;
      IDMATEC        at 0 range 27 .. 27;
      IDMABTCC       at 0 range 28 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   --  The interrupt mask register determines which status flags generate an
   --  interrupt request by setting the corresponding bit to 1.
   type SDMMC_MASKR_Register is record
      --  Command CRC fail interrupt enable Set and cleared by software to
      --  enable/disable interrupt caused by command CRC failure.
      CCRCFAILIE     : Boolean := False;
      --  Data CRC fail interrupt enable Set and cleared by software to
      --  enable/disable interrupt caused by data CRC failure.
      DCRCFAILIE     : Boolean := False;
      --  Command timeout interrupt enable Set and cleared by software to
      --  enable/disable interrupt caused by command timeout.
      CTIMEOUTIE     : Boolean := False;
      --  Data timeout interrupt enable Set and cleared by software to
      --  enable/disable interrupt caused by data timeout.
      DTIMEOUTIE     : Boolean := False;
      --  Tx FIFO underrun error interrupt enable Set and cleared by software
      --  to enable/disable interrupt caused by Tx FIFO underrun error.
      TXUNDERRIE     : Boolean := False;
      --  Rx FIFO overrun error interrupt enable Set and cleared by software to
      --  enable/disable interrupt caused by Rx FIFO overrun error.
      RXOVERRIE      : Boolean := False;
      --  Command response received interrupt enable Set and cleared by
      --  software to enable/disable interrupt caused by receiving command
      --  response.
      CMDRENDIE      : Boolean := False;
      --  Command sent interrupt enable Set and cleared by software to
      --  enable/disable interrupt caused by sending command.
      CMDSENTIE      : Boolean := False;
      --  Data end interrupt enable Set and cleared by software to
      --  enable/disable interrupt caused by data end.
      DATAENDIE      : Boolean := False;
      --  Data hold interrupt enable Set and cleared by software to
      --  enable/disable the interrupt generated when sending new data is hold
      --  in the DPSM Wait_S state.
      DHOLDIE        : Boolean := False;
      --  Data block end interrupt enable Set and cleared by software to
      --  enable/disable interrupt caused by data block end.
      DBCKENDIE      : Boolean := False;
      --  Data transfer aborted interrupt enable Set and cleared by software to
      --  enable/disable interrupt caused by a data transfer being aborted.
      DABORTIE       : Boolean := False;
      --  unspecified
      Reserved_12_13 : HAL.UInt2 := 16#0#;
      --  Tx FIFO half empty interrupt enable Set and cleared by software to
      --  enable/disable interrupt caused by Tx FIFO half empty.
      TXFIFOHEIE     : Boolean := False;
      --  Rx FIFO half full interrupt enable Set and cleared by software to
      --  enable/disable interrupt caused by Rx FIFO half full.
      RXFIFOHFIE     : Boolean := False;
      --  unspecified
      Reserved_16_16 : HAL.Bit := 16#0#;
      --  Rx FIFO full interrupt enable Set and cleared by software to
      --  enable/disable interrupt caused by Rx FIFO full.
      RXFIFOFIE      : Boolean := False;
      --  Tx FIFO empty interrupt enable Set and cleared by software to
      --  enable/disable interrupt caused by Tx FIFO empty.
      TXFIFOEIE      : Boolean := False;
      --  unspecified
      Reserved_19_20 : HAL.UInt2 := 16#0#;
      --  BUSYD0END interrupt enable Set and cleared by software to
      --  enable/disable the interrupt generated when SDMMC_D0 signal changes
      --  from busy to NOT busy following a CMD response.
      BUSYD0ENDIE    : Boolean := False;
      --  SDIO mode interrupt received interrupt enable Set and cleared by
      --  software to enable/disable the interrupt generated when receiving the
      --  SDIO mode interrupt.
      SDIOITIE       : Boolean := False;
      --  Acknowledgment Fail interrupt enable Set and cleared by software to
      --  enable/disable interrupt caused by acknowledgment Fail.
      ACKFAILIE      : Boolean := False;
      --  Acknowledgment timeout interrupt enable Set and cleared by software
      --  to enable/disable interrupt caused by acknowledgment timeout.
      ACKTIMEOUTIE   : Boolean := False;
      --  Voltage switch critical timing section completion interrupt enable
      --  Set and cleared by software to enable/disable the interrupt generated
      --  when voltage switch critical timing section completion.
      VSWENDIE       : Boolean := False;
      --  Voltage Switch clock stopped interrupt enable Set and cleared by
      --  software to enable/disable interrupt caused by Voltage Switch clock
      --  stopped.
      CKSTOPIE       : Boolean := False;
      --  unspecified
      Reserved_27_27 : HAL.Bit := 16#0#;
      --  IDMA buffer transfer complete interrupt enable Set and cleared by
      --  software to enable/disable the interrupt generated when the IDMA has
      --  transferred all data belonging to a memory buffer.
      IDMABTCIE      : Boolean := False;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SDMMC_MASKR_Register use record
      CCRCFAILIE     at 0 range 0 .. 0;
      DCRCFAILIE     at 0 range 1 .. 1;
      CTIMEOUTIE     at 0 range 2 .. 2;
      DTIMEOUTIE     at 0 range 3 .. 3;
      TXUNDERRIE     at 0 range 4 .. 4;
      RXOVERRIE      at 0 range 5 .. 5;
      CMDRENDIE      at 0 range 6 .. 6;
      CMDSENTIE      at 0 range 7 .. 7;
      DATAENDIE      at 0 range 8 .. 8;
      DHOLDIE        at 0 range 9 .. 9;
      DBCKENDIE      at 0 range 10 .. 10;
      DABORTIE       at 0 range 11 .. 11;
      Reserved_12_13 at 0 range 12 .. 13;
      TXFIFOHEIE     at 0 range 14 .. 14;
      RXFIFOHFIE     at 0 range 15 .. 15;
      Reserved_16_16 at 0 range 16 .. 16;
      RXFIFOFIE      at 0 range 17 .. 17;
      TXFIFOEIE      at 0 range 18 .. 18;
      Reserved_19_20 at 0 range 19 .. 20;
      BUSYD0ENDIE    at 0 range 21 .. 21;
      SDIOITIE       at 0 range 22 .. 22;
      ACKFAILIE      at 0 range 23 .. 23;
      ACKTIMEOUTIE   at 0 range 24 .. 24;
      VSWENDIE       at 0 range 25 .. 25;
      CKSTOPIE       at 0 range 26 .. 26;
      Reserved_27_27 at 0 range 27 .. 27;
      IDMABTCIE      at 0 range 28 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype SDMMC_ACKTIMER_ACKTIME_Field is HAL.UInt25;

   --  The SDMMC_ACKTIMER register contains the acknowledgment timeout period,
   --  in SDMMC_CK bus clock periods. A counter loads the value from the
   --  SDMMC_ACKTIMER register, and starts decrementing when the data path
   --  state machine (DPSM) enters the Wait_Ack state. If the timer reaches 0
   --  while the DPSM is in this states, the acknowledgment timeout status flag
   --  is set.
   type SDMMC_ACKTIMER_Register is record
      --  Boot acknowledgment timeout period This bit can only be written by
      --  firmware when CPSM is disabled (CPSMEN = 0). Boot acknowledgment
      --  timeout period expressed in card bus clock periods.
      ACKTIME        : SDMMC_ACKTIMER_ACKTIME_Field := 16#0#;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SDMMC_ACKTIMER_Register use record
      ACKTIME        at 0 range 0 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  The receive and transmit FIFOs can be read or written as 32-bit wide
   --  registers. The FIFOs contain 32 entries on 32 sequential addresses. This
   --  allows the CPU to use its load and store multiple operands to read
   --  from/write to the FIFO.
   type SDMMC_IDMACTRLR_Register is record
      --  IDMA enable This bit can only be written by firmware when DPSM is
      --  inactive (DPSMACT = 0).
      IDMAEN        : Boolean := False;
      --  Buffer mode selection. This bit can only be written by firmware when
      --  DPSM is inactive (DPSMACT = 0).
      IDMABMODE     : Boolean := False;
      --  Double buffer mode active buffer indication This bit can only be
      --  written by firmware when DPSM is inactive (DPSMACT = 0). When IDMA is
      --  enabled this bit is toggled by hardware.
      IDMABACT      : Boolean := False;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SDMMC_IDMACTRLR_Register use record
      IDMAEN        at 0 range 0 .. 0;
      IDMABMODE     at 0 range 1 .. 1;
      IDMABACT      at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype SDMMC_IDMABSIZER_IDMABNDT_Field is HAL.UInt8;

   --  The SDMMC_IDMABSIZER register contains the buffers size when in double
   --  buffer configuration.
   type SDMMC_IDMABSIZER_Register is record
      --  unspecified
      Reserved_0_4   : HAL.UInt5 := 16#0#;
      --  Number of transfers per buffer. This 8-bit value shall be multiplied
      --  by 8 to get the size of the buffer in 32-bit words and by 32 to get
      --  the size of the buffer in bytes. Example: IDMABNDT = 0x01: buffer
      --  size = 8 words = 32 bytes. These bits can only be written by firmware
      --  when DPSM is inactive (DPSMACT = 0).
      IDMABNDT       : SDMMC_IDMABSIZER_IDMABNDT_Field := 16#0#;
      --  unspecified
      Reserved_13_31 : HAL.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SDMMC_IDMABSIZER_Register use record
      Reserved_0_4   at 0 range 0 .. 4;
      IDMABNDT       at 0 range 5 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   subtype SDMMC_VER_MINREV_Field is HAL.UInt4;
   subtype SDMMC_VER_MAJREV_Field is HAL.UInt4;

   --  SDMMC IP version register
   type SDMMC_VER_Register is record
      --  Read-only. IP minor revision number.
      MINREV        : SDMMC_VER_MINREV_Field;
      --  Read-only. IP major revision number.
      MAJREV        : SDMMC_VER_MAJREV_Field;
      --  unspecified
      Reserved_8_31 : HAL.UInt24;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SDMMC_VER_Register use record
      MINREV        at 0 range 0 .. 3;
      MAJREV        at 0 range 4 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  SDMMC1
   type SDMMC_Peripheral is record
      --  SDMMC power control register
      SDMMC_POWER      : aliased SDMMC_POWER_Register;
      --  The SDMMC_CLKCR register controls the SDMMC_CK output clock, the
      --  SDMMC_RX_CLK receive clock, and the bus width.
      SDMMC_CLKCR      : aliased SDMMC_CLKCR_Register;
      --  The SDMMC_ARGR register contains a 32-bit command argument, which is
      --  sent to a card as part of a command message.
      SDMMC_ARGR       : aliased HAL.UInt32;
      --  The SDMMC_CMDR register contains the command index and command type
      --  bits. The command index is sent to a card as part of a command
      --  message. The command type bits control the command path state machine
      --  (CPSM).
      SDMMC_CMDR       : aliased SDMMC_CMDR_Register;
      --  SDMMC command response register
      SDMMC_RESPCMDR   : aliased SDMMC_RESPCMDR_Register;
      --  The SDMMC_RESP1/2/3/4R registers contain the status of a card, which
      --  is part of the received response.
      SDMMC_RESP1R     : aliased HAL.UInt32;
      --  The SDMMC_RESP1/2/3/4R registers contain the status of a card, which
      --  is part of the received response.
      SDMMC_RESP2R     : aliased HAL.UInt32;
      --  The SDMMC_RESP1/2/3/4R registers contain the status of a card, which
      --  is part of the received response.
      SDMMC_RESP3R     : aliased HAL.UInt32;
      --  The SDMMC_RESP1/2/3/4R registers contain the status of a card, which
      --  is part of the received response.
      SDMMC_RESP4R     : aliased HAL.UInt32;
      --  The SDMMC_DTIMER register contains the data timeout period, in card
      --  bus clock periods. A counter loads the value from the SDMMC_DTIMER
      --  register, and starts decrementing when the data path state machine
      --  (DPSM) enters the Wait_R or Busy state. If the timer reaches 0 while
      --  the DPSM is in either of these states, the timeout status flag is
      --  set.
      SDMMC_DTIMER     : aliased HAL.UInt32;
      --  The SDMMC_DLENR register contains the number of data bytes to be
      --  transferred. The value is loaded into the data counter when data
      --  transfer starts.
      SDMMC_DLENR      : aliased SDMMC_DLENR_Register;
      --  The SDMMC_DCTRL register control the data path state machine (DPSM).
      SDMMC_DCTRL      : aliased SDMMC_DCTRL_Register;
      --  The SDMMC_DCNTR register loads the value from the data length
      --  register (see SDMMC_DLENR) when the DPSM moves from the Idle state to
      --  the Wait_R or Wait_S state. As data is transferred, the counter
      --  decrements the value until it reaches 0. The DPSM then moves to the
      --  Idle state and when there has been no error, the data status end flag
      --  (DATAEND) is set.
      SDMMC_DCNTR      : aliased SDMMC_DCNTR_Register;
      --  The SDMMC_STAR register is a read-only register. It contains two
      --  types of flag:Static flags (bits [29,21,11:0]): these bits remain
      --  asserted until they are cleared by writing to the SDMMC interrupt
      --  Clear register (see SDMMC_ICR)Dynamic flags (bits [20:12]): these
      --  bits change state depending on the state of the underlying logic (for
      --  example, FIFO full and empty flags are asserted and de-asserted as
      --  data while written to the FIFO)
      SDMMC_STAR       : aliased SDMMC_STAR_Register;
      --  The SDMMC_ICR register is a write-only register. Writing a bit with 1
      --  clears the corresponding bit in the SDMMC_STAR status register.
      SDMMC_ICR        : aliased SDMMC_ICR_Register;
      --  The interrupt mask register determines which status flags generate an
      --  interrupt request by setting the corresponding bit to 1.
      SDMMC_MASKR      : aliased SDMMC_MASKR_Register;
      --  The SDMMC_ACKTIMER register contains the acknowledgment timeout
      --  period, in SDMMC_CK bus clock periods. A counter loads the value from
      --  the SDMMC_ACKTIMER register, and starts decrementing when the data
      --  path state machine (DPSM) enters the Wait_Ack state. If the timer
      --  reaches 0 while the DPSM is in this states, the acknowledgment
      --  timeout status flag is set.
      SDMMC_ACKTIMER   : aliased SDMMC_ACKTIMER_Register;
      --  The receive and transmit FIFOs can be read or written as 32-bit wide
      --  registers. The FIFOs contain 32 entries on 32 sequential addresses.
      --  This allows the CPU to use its load and store multiple operands to
      --  read from/write to the FIFO.
      SDMMC_IDMACTRLR  : aliased SDMMC_IDMACTRLR_Register;
      --  The SDMMC_IDMABSIZER register contains the buffers size when in
      --  double buffer configuration.
      SDMMC_IDMABSIZER : aliased SDMMC_IDMABSIZER_Register;
      --  The SDMMC_IDMABASE0R register contains the memory buffer base address
      --  in single buffer configuration and the buffer 0 base address in
      --  double buffer configuration.
      SDMMC_IDMABASE0R : aliased HAL.UInt32;
      --  The SDMMC_IDMABASE1R register contains the double buffer
      --  configuration second buffer memory base address.
      SDMMC_IDMABASE1R : aliased HAL.UInt32;
      --  The receive and transmit FIFOs can be only read or written as word
      --  (32-bit) wide registers. The FIFOs contain 16 entries on sequential
      --  addresses. This allows the CPU to use its load and store multiple
      --  operands to read from/write to the FIFO.When accessing SDMMC_FIFOR
      --  with half word or byte access an AHB bus fault is generated.
      SDMMC_FIFOR      : aliased HAL.UInt32;
      --  SDMMC IP version register
      SDMMC_VER        : aliased SDMMC_VER_Register;
      --  SDMMC IP identification register
      SDMMC_ID         : aliased HAL.UInt32;
   end record
     with Volatile;

   for SDMMC_Peripheral use record
      SDMMC_POWER      at 16#0# range 0 .. 31;
      SDMMC_CLKCR      at 16#4# range 0 .. 31;
      SDMMC_ARGR       at 16#8# range 0 .. 31;
      SDMMC_CMDR       at 16#C# range 0 .. 31;
      SDMMC_RESPCMDR   at 16#10# range 0 .. 31;
      SDMMC_RESP1R     at 16#14# range 0 .. 31;
      SDMMC_RESP2R     at 16#18# range 0 .. 31;
      SDMMC_RESP3R     at 16#1C# range 0 .. 31;
      SDMMC_RESP4R     at 16#20# range 0 .. 31;
      SDMMC_DTIMER     at 16#24# range 0 .. 31;
      SDMMC_DLENR      at 16#28# range 0 .. 31;
      SDMMC_DCTRL      at 16#2C# range 0 .. 31;
      SDMMC_DCNTR      at 16#30# range 0 .. 31;
      SDMMC_STAR       at 16#34# range 0 .. 31;
      SDMMC_ICR        at 16#38# range 0 .. 31;
      SDMMC_MASKR      at 16#3C# range 0 .. 31;
      SDMMC_ACKTIMER   at 16#40# range 0 .. 31;
      SDMMC_IDMACTRLR  at 16#50# range 0 .. 31;
      SDMMC_IDMABSIZER at 16#54# range 0 .. 31;
      SDMMC_IDMABASE0R at 16#58# range 0 .. 31;
      SDMMC_IDMABASE1R at 16#5C# range 0 .. 31;
      SDMMC_FIFOR      at 16#80# range 0 .. 31;
      SDMMC_VER        at 16#3F4# range 0 .. 31;
      SDMMC_ID         at 16#3F8# range 0 .. 31;
   end record;

   --  SDMMC1
   SDMMC1_Periph : aliased SDMMC_Peripheral
     with Import, Address => System'To_Address (16#420C8000#);

   --  SDMMC1
   SEC_SDMMC1_Periph : aliased SDMMC_Peripheral
     with Import, Address => System'To_Address (16#520C8000#);

end STM32_SVD.SDMMC;
