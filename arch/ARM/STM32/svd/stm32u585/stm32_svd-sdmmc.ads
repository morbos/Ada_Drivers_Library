--  This spec has been automatically generated from STM32U5xx.svd

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

   subtype POWER_PWRCTRL_Field is HAL.UInt2;

   --  power control register
   type POWER_Register is record
      --  SDMMC state control bits
      PWRCTRL       : POWER_PWRCTRL_Field := 16#0#;
      --  Voltage switch sequence start
      VSWITCH       : Boolean := False;
      --  Voltage switch procedure enable
      VSWITCHEN     : Boolean := False;
      --  Data and command direction signals polarity selection
      DIRPOL        : Boolean := False;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for POWER_Register use record
      PWRCTRL       at 0 range 0 .. 1;
      VSWITCH       at 0 range 2 .. 2;
      VSWITCHEN     at 0 range 3 .. 3;
      DIRPOL        at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype CLKCR_CLKDIV_Field is HAL.UInt10;
   subtype CLKCR_WIDBUS_Field is HAL.UInt2;
   subtype CLKCR_SELCLKRX_Field is HAL.UInt2;

   --  clock control register
   type CLKCR_Register is record
      --  Clock divide factor
      CLKDIV         : CLKCR_CLKDIV_Field := 16#0#;
      --  unspecified
      Reserved_10_11 : HAL.UInt2 := 16#0#;
      --  Power saving configuration bit
      PWRSAV         : Boolean := False;
      --  unspecified
      Reserved_13_13 : HAL.Bit := 16#0#;
      --  Wide bus mode enable bit
      WIDBUS         : CLKCR_WIDBUS_Field := 16#0#;
      --  SDIO_CK dephasing selection bit
      NEGEDGE        : Boolean := False;
      --  HW Flow Control enable
      HWFC_EN        : Boolean := False;
      --  Data rate signaling selection
      DDR            : Boolean := False;
      --  Bus speed mode selection between DS, HS, SDR12, SDR25 and
      --  SDR50,DDR50, SDR104
      BUSSPEED       : Boolean := False;
      --  Receive clock selection
      SELCLKRX       : CLKCR_SELCLKRX_Field := 16#0#;
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CLKCR_Register use record
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

   subtype CMDR_CMDINDEX_Field is HAL.UInt6;
   subtype CMDR_WAITRESP_Field is HAL.UInt2;

   --  command register
   type CMDR_Register is record
      --  Command index
      CMDINDEX       : CMDR_CMDINDEX_Field := 16#0#;
      --  The CPSM treats the command as a data transfer command, stops the
      --  interrupt period, and signals DataEnable to the DPSM
      CMDTRANS       : Boolean := False;
      --  The CPSM treats the command as a Stop Transmission command and
      --  signals Abort to the DPSM
      CMDSTOP        : Boolean := False;
      --  Wait for response bits
      WAITRESP       : CMDR_WAITRESP_Field := 16#0#;
      --  CPSM waits for interrupt request
      WAITINT        : Boolean := False;
      --  CPSM Waits for ends of data transfer (CmdPend internal signal) from
      --  DPSM
      WAITPEND       : Boolean := False;
      --  Command path state machine (CPSM) Enable bit
      CPSMEN         : Boolean := False;
      --  Hold new data block transmission and reception in the DPSM
      DTHOLD         : Boolean := False;
      --  Select the boot mode procedure to be used
      BOOTMODE       : Boolean := False;
      --  Enable boot mode procedure
      BOOTEN         : Boolean := False;
      --  The CPSM treats the command as a Suspend or Resume command and
      --  signals interrupt period start/end
      CMDSUSPEND     : Boolean := False;
      --  unspecified
      Reserved_17_31 : HAL.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CMDR_Register use record
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

   subtype RESPCMDR_RESPCMD_Field is HAL.UInt6;

   --  command response register
   type RESPCMDR_Register is record
      --  Read-only. Response command index
      RESPCMD       : RESPCMDR_RESPCMD_Field;
      --  unspecified
      Reserved_6_31 : HAL.UInt26;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RESPCMDR_Register use record
      RESPCMD       at 0 range 0 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   subtype DLENR_DATALENGTH_Field is HAL.UInt25;

   --  data length register
   type DLENR_Register is record
      --  Data length value
      DATALENGTH     : DLENR_DATALENGTH_Field := 16#0#;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DLENR_Register use record
      DATALENGTH     at 0 range 0 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype DCTRL_DTMODE_Field is HAL.UInt2;
   subtype DCTRL_DBLOCKSIZE_Field is HAL.UInt4;

   --  data control register
   type DCTRL_Register is record
      --  DTEN
      DTEN           : Boolean := False;
      --  Data transfer direction selection
      DTDIR          : Boolean := False;
      --  Data transfer mode selection
      DTMODE         : DCTRL_DTMODE_Field := 16#0#;
      --  Data block size
      DBLOCKSIZE     : DCTRL_DBLOCKSIZE_Field := 16#0#;
      --  Read wait start
      RWSTART        : Boolean := False;
      --  Read wait stop
      RWSTOP         : Boolean := False;
      --  Read wait mode
      RWMOD          : Boolean := False;
      --  SD I/O enable functions
      SDIOEN         : Boolean := False;
      --  Enable the reception of the boot acknowledgment
      BOOTACKEN      : Boolean := False;
      --  FIFO reset, will flush any remaining data
      FIFORST        : Boolean := False;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DCTRL_Register use record
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

   subtype DCNTR_DATACOUNT_Field is HAL.UInt25;

   --  data counter register
   type DCNTR_Register is record
      --  Read-only. Data count value
      DATACOUNT      : DCNTR_DATACOUNT_Field;
      --  unspecified
      Reserved_25_31 : HAL.UInt7;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DCNTR_Register use record
      DATACOUNT      at 0 range 0 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  status register
   type STAR_Register is record
      --  Read-only. Command response received (CRC check failed)
      CCRCFAIL       : Boolean;
      --  Read-only. Data block sent/received (CRC check failed)
      DCRCFAIL       : Boolean;
      --  Read-only. Command response timeout
      CTIMEOUT       : Boolean;
      --  Read-only. Data timeout
      DTIMEOUT       : Boolean;
      --  Read-only. Transmit FIFO underrun error (masked by hardware when IDMA
      --  is enabled)
      TXUNDERR       : Boolean;
      --  Read-only. Received FIFO overrun error (masked by hardware when IDMA
      --  is enabled)
      RXOVERR        : Boolean;
      --  Read-only. Command response received (CRC check passed, or no CRC)
      CMDREND        : Boolean;
      --  Read-only. Command sent (no response required)
      CMDSENT        : Boolean;
      --  Read-only. Data transfer ended correctly
      DATAEND        : Boolean;
      --  Read-only. Data transfer Hold
      DHOLD          : Boolean;
      --  Read-only. Data block sent/received
      DBCKEND        : Boolean;
      --  Read-only. Data transfer aborted by CMD12
      DABORT         : Boolean;
      --  Read-only. Data path state machine active, i.e. not in Idle state
      DPSMACT        : Boolean;
      --  Read-only. Command path state machine active, i.e. not in Idle state
      CPSMACT        : Boolean;
      --  Read-only. Transmit FIFO half empty
      TXFIFOHE       : Boolean;
      --  Read-only. Receive FIFO half full
      RXFIFOHF       : Boolean;
      --  Read-only. Transmit FIFO full
      TXFIFOF        : Boolean;
      --  Read-only. Receive FIFO full
      RXFIFOF        : Boolean;
      --  Read-only. Transmit FIFO empty
      TXFIFOE        : Boolean;
      --  Read-only. Receive FIFO empty
      RXFIFOE        : Boolean;
      --  Read-only. Inverted value of SDMMC_D0 line (Busy), sampled at the end
      --  of a CMD response and a second time 2 SDMMC_CK cycles after the CMD
      --  response
      BUSYD0         : Boolean;
      --  Read-only. end of SDMMC_D0 Busy following a CMD response detected
      BUSYD0END      : Boolean;
      --  Read-only. SDIO interrupt received
      SDIOIT         : Boolean;
      --  Read-only. Boot acknowledgment received (boot acknowledgment check
      --  fail)
      ACKFAIL        : Boolean;
      --  Read-only. Boot acknowledgment timeout
      ACKTIMEOUT     : Boolean;
      --  Read-only. Voltage switch critical timing section completion
      VSWEND         : Boolean;
      --  Read-only. SDMMC_CK stopped in Voltage switch procedure
      CKSTOP         : Boolean;
      --  Read-only. IDMA transfer error
      IDMATE         : Boolean;
      --  Read-only. IDMA buffer transfer complete
      IDMABTC        : Boolean;
      --  unspecified
      Reserved_29_31 : HAL.UInt3;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for STAR_Register use record
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

   --  interrupt clear register
   type ICR_Register is record
      --  CCRCFAIL flag clear bit
      CCRCFAILC      : Boolean := False;
      --  DCRCFAIL flag clear bit
      DCRCFAILC      : Boolean := False;
      --  CTIMEOUT flag clear bit
      CTIMEOUTC      : Boolean := False;
      --  DTIMEOUT flag clear bit
      DTIMEOUTC      : Boolean := False;
      --  TXUNDERR flag clear bit
      TXUNDERRC      : Boolean := False;
      --  RXOVERR flag clear bit
      RXOVERRC       : Boolean := False;
      --  CMDREND flag clear bit
      CMDRENDC       : Boolean := False;
      --  CMDSENT flag clear bit
      CMDSENTC       : Boolean := False;
      --  DATAEND flag clear bit
      DATAENDC       : Boolean := False;
      --  DHOLD flag clear bit
      DHOLDC         : Boolean := False;
      --  DBCKEND flag clear bit
      DBCKENDC       : Boolean := False;
      --  DABORT flag clear bit
      DABORTC        : Boolean := False;
      --  unspecified
      Reserved_12_20 : HAL.UInt9 := 16#0#;
      --  BUSYD0END flag clear bit
      BUSYD0ENDC     : Boolean := False;
      --  SDIOIT flag clear bit
      SDIOITC        : Boolean := False;
      --  ACKFAIL flag clear bit
      ACKFAILC       : Boolean := False;
      --  ACKTIMEOUT flag clear bit
      ACKTIMEOUTC    : Boolean := False;
      --  VSWEND flag clear bit
      VSWENDC        : Boolean := False;
      --  CKSTOP flag clear bit
      CKSTOPC        : Boolean := False;
      --  IDMA transfer error clear bit
      IDMATEC        : Boolean := False;
      --  IDMA buffer transfer complete clear bit
      IDMABTCC       : Boolean := False;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ICR_Register use record
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

   --  mask register
   type MASKR_Register is record
      --  Command CRC fail interrupt enable
      CCRCFAILIE     : Boolean := False;
      --  Data CRC fail interrupt enable
      DCRCFAILIE     : Boolean := False;
      --  Command timeout interrupt enable
      CTIMEOUTIE     : Boolean := False;
      --  Data timeout interrupt enable
      DTIMEOUTIE     : Boolean := False;
      --  Tx FIFO underrun error interrupt enable
      TXUNDERRIE     : Boolean := False;
      --  Rx FIFO overrun error interrupt enable
      RXOVERRIE      : Boolean := False;
      --  Command response received interrupt enable
      CMDRENDIE      : Boolean := False;
      --  Command sent interrupt enable
      CMDSENTIE      : Boolean := False;
      --  Data end interrupt enable
      DATAENDIE      : Boolean := False;
      --  Data hold interrupt enable
      DHOLDIE        : Boolean := False;
      --  Data block end interrupt enable
      DBCKENDIE      : Boolean := False;
      --  Data transfer aborted interrupt enable
      DABORTIE       : Boolean := False;
      --  unspecified
      Reserved_12_13 : HAL.UInt2 := 16#0#;
      --  Tx FIFO half empty interrupt enable
      TXFIFOHEIE     : Boolean := False;
      --  Rx FIFO half full interrupt enable
      RXFIFOHFIE     : Boolean := False;
      --  unspecified
      Reserved_16_16 : HAL.Bit := 16#0#;
      --  Rx FIFO full interrupt enable
      RXFIFOFIE      : Boolean := False;
      --  Tx FIFO empty interrupt enable
      TXFIFOEIE      : Boolean := False;
      --  unspecified
      Reserved_19_20 : HAL.UInt2 := 16#0#;
      --  BUSYD0END interrupt enable
      BUSYD0ENDIE    : Boolean := False;
      --  SDIO mode interrupt received interrupt enable
      SDIOITIE       : Boolean := False;
      --  Acknowledgment Fail interrupt enable
      ACKFAILIE      : Boolean := False;
      --  Acknowledgment timeout interrupt enable
      ACKTIMEOUTIE   : Boolean := False;
      --  Voltage switch critical timing section completion interrupt enable
      VSWENDIE       : Boolean := False;
      --  Voltage Switch clock stopped interrupt enable
      CKSTOPIE       : Boolean := False;
      --  unspecified
      Reserved_27_27 : HAL.Bit := 16#0#;
      --  IDMA buffer transfer complete interrupt enable
      IDMABTCIE      : Boolean := False;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MASKR_Register use record
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

   subtype ACKTIMER_ACKTIME_Field is HAL.UInt25;

   --  acknowledgment timer register
   type ACKTIMER_Register is record
      --  Boot acknowledgment timeout period
      ACKTIME        : ACKTIMER_ACKTIME_Field := 16#0#;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ACKTIMER_Register use record
      ACKTIME        at 0 range 0 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  DMA control register
   type SDMMC_IDMACTRLR_Register is record
      --  IDMA enable This bit can only be written by firmware when DPSM is
      --  inactive (DPSMACT = 0).
      IDMAEN        : Boolean := False;
      --  Buffer mode selection. This bit can only be written by firmware when
      --  DPSM is inactive (DPSMACT = 0).
      IDMABMODE     : Boolean := False;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SDMMC_IDMACTRLR_Register use record
      IDMAEN        at 0 range 0 .. 0;
      IDMABMODE     at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   subtype SDMMC_IDMABSIZER_IDMABNDT_Field is HAL.UInt12;

   --  buffer size register
   type SDMMC_IDMABSIZER_Register is record
      --  unspecified
      Reserved_0_4   : HAL.UInt5 := 16#0#;
      --  Number of bytes per buffer
      IDMABNDT       : SDMMC_IDMABSIZER_IDMABNDT_Field := 16#0#;
      --  unspecified
      Reserved_17_31 : HAL.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SDMMC_IDMABSIZER_Register use record
      Reserved_0_4   at 0 range 0 .. 4;
      IDMABNDT       at 0 range 5 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype SDMMC_IDMALAR_IDMALA_Field is HAL.UInt14;

   --  linked list address register
   type SDMMC_IDMALAR_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  Acknowledge linked list buffer ready
      IDMALA         : SDMMC_IDMALAR_IDMALA_Field := 16#0#;
      --  unspecified
      Reserved_16_28 : HAL.UInt13 := 16#0#;
      --  Acknowledge linked list buffer ready
      ABR            : Boolean := False;
      --  Update SDMMC_IDMABSIZE from the next linked list when in linked list
      --  mode (SDMMC_IDMACTRLR.IDMABMODE select linked list mode and ULA = 1)
      ULS            : Boolean := False;
      --  Update SDMMC_IDMALAR from linked list when in linked list mode
      --  (SDMMC_IDMACTRLR.IDMABMODE select linked list mode)
      ULA            : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SDMMC_IDMALAR_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      IDMALA         at 0 range 2 .. 15;
      Reserved_16_28 at 0 range 16 .. 28;
      ABR            at 0 range 29 .. 29;
      ULS            at 0 range 30 .. 30;
      ULA            at 0 range 31 .. 31;
   end record;

   subtype SDMMC_IDMABAR_IDMABA_Field is HAL.UInt30;

   --  linked list memory base register
   type SDMMC_IDMABAR_Register is record
      --  unspecified
      Reserved_0_1 : HAL.UInt2 := 16#0#;
      --  Word aligned Linked list memory base address
      IDMABA       : SDMMC_IDMABAR_IDMABA_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SDMMC_IDMABAR_Register use record
      Reserved_0_1 at 0 range 0 .. 1;
      IDMABA       at 0 range 2 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Secure digital input/output MultiMediaCard interface
   type SDMMC_Peripheral is record
      --  power control register
      POWER            : aliased POWER_Register;
      --  clock control register
      CLKCR            : aliased CLKCR_Register;
      --  argument register
      ARGR             : aliased HAL.UInt32;
      --  command register
      CMDR             : aliased CMDR_Register;
      --  command response register
      RESPCMDR         : aliased RESPCMDR_Register;
      --  response 1 register
      RESP1            : aliased HAL.UInt32;
      --  response 2 register
      RESP2            : aliased HAL.UInt32;
      --  response 3 register
      RESP3            : aliased HAL.UInt32;
      --  response 4 register
      RESP4            : aliased HAL.UInt32;
      --  data timer register
      DTIMER           : aliased HAL.UInt32;
      --  data length register
      DLENR            : aliased DLENR_Register;
      --  data control register
      DCTRL            : aliased DCTRL_Register;
      --  data counter register
      DCNTR            : aliased DCNTR_Register;
      --  status register
      STAR             : aliased STAR_Register;
      --  interrupt clear register
      ICR              : aliased ICR_Register;
      --  mask register
      MASKR            : aliased MASKR_Register;
      --  acknowledgment timer register
      ACKTIMER         : aliased ACKTIMER_Register;
      --  DMA control register
      SDMMC_IDMACTRLR  : aliased SDMMC_IDMACTRLR_Register;
      --  buffer size register
      SDMMC_IDMABSIZER : aliased SDMMC_IDMABSIZER_Register;
      --  buffer base address register
      SDMMC_IDMABASER  : aliased HAL.UInt32;
      --  linked list address register
      SDMMC_IDMALAR    : aliased SDMMC_IDMALAR_Register;
      --  linked list memory base register
      SDMMC_IDMABAR    : aliased SDMMC_IDMABAR_Register;
      --  data FIFO register 0
      FIFOR0           : aliased HAL.UInt32;
      --  data FIFO register 1
      FIFOR1           : aliased HAL.UInt32;
      --  data FIFO register 2
      FIFOR2           : aliased HAL.UInt32;
      --  data FIFO register 3
      FIFOR3           : aliased HAL.UInt32;
      --  data FIFO register 4
      FIFOR4           : aliased HAL.UInt32;
      --  data FIFO register 5
      FIFOR5           : aliased HAL.UInt32;
      --  data FIFO register 6
      FIFOR6           : aliased HAL.UInt32;
      --  data FIFO register 7
      FIFOR7           : aliased HAL.UInt32;
      --  data FIFO register 8
      FIFOR8           : aliased HAL.UInt32;
      --  data FIFO register 9
      FIFOR9           : aliased HAL.UInt32;
      --  data FIFO register 10
      FIFOR10          : aliased HAL.UInt32;
      --  data FIFO register 11
      FIFOR11          : aliased HAL.UInt32;
      --  data FIFO register 12
      FIFOR12          : aliased HAL.UInt32;
      --  data FIFO register 13
      FIFOR13          : aliased HAL.UInt32;
      --  data FIFO register 14
      FIFOR14          : aliased HAL.UInt32;
      --  data FIFO register 15
      FIFOR15          : aliased HAL.UInt32;
   end record
     with Volatile;

   for SDMMC_Peripheral use record
      POWER            at 16#0# range 0 .. 31;
      CLKCR            at 16#4# range 0 .. 31;
      ARGR             at 16#8# range 0 .. 31;
      CMDR             at 16#C# range 0 .. 31;
      RESPCMDR         at 16#10# range 0 .. 31;
      RESP1            at 16#14# range 0 .. 31;
      RESP2            at 16#18# range 0 .. 31;
      RESP3            at 16#1C# range 0 .. 31;
      RESP4            at 16#20# range 0 .. 31;
      DTIMER           at 16#24# range 0 .. 31;
      DLENR            at 16#28# range 0 .. 31;
      DCTRL            at 16#2C# range 0 .. 31;
      DCNTR            at 16#30# range 0 .. 31;
      STAR             at 16#34# range 0 .. 31;
      ICR              at 16#38# range 0 .. 31;
      MASKR            at 16#3C# range 0 .. 31;
      ACKTIMER         at 16#40# range 0 .. 31;
      SDMMC_IDMACTRLR  at 16#50# range 0 .. 31;
      SDMMC_IDMABSIZER at 16#54# range 0 .. 31;
      SDMMC_IDMABASER  at 16#58# range 0 .. 31;
      SDMMC_IDMALAR    at 16#64# range 0 .. 31;
      SDMMC_IDMABAR    at 16#68# range 0 .. 31;
      FIFOR0           at 16#80# range 0 .. 31;
      FIFOR1           at 16#84# range 0 .. 31;
      FIFOR2           at 16#88# range 0 .. 31;
      FIFOR3           at 16#8C# range 0 .. 31;
      FIFOR4           at 16#90# range 0 .. 31;
      FIFOR5           at 16#94# range 0 .. 31;
      FIFOR6           at 16#98# range 0 .. 31;
      FIFOR7           at 16#9C# range 0 .. 31;
      FIFOR8           at 16#A0# range 0 .. 31;
      FIFOR9           at 16#A4# range 0 .. 31;
      FIFOR10          at 16#A8# range 0 .. 31;
      FIFOR11          at 16#AC# range 0 .. 31;
      FIFOR12          at 16#B0# range 0 .. 31;
      FIFOR13          at 16#B4# range 0 .. 31;
      FIFOR14          at 16#B8# range 0 .. 31;
      FIFOR15          at 16#BC# range 0 .. 31;
   end record;

   --  Secure digital input/output MultiMediaCard interface
   SDMMC1_Periph : aliased SDMMC_Peripheral
     with Import, Address => SDMMC1_Base;

   --  Secure digital input/output MultiMediaCard interface
   SDMMC2_Periph : aliased SDMMC_Peripheral
     with Import, Address => SDMMC2_Base;

   --  Secure digital input/output MultiMediaCard interface
   SEC_SDMMC1_Periph : aliased SDMMC_Peripheral
     with Import, Address => SEC_SDMMC1_Base;

   --  Secure digital input/output MultiMediaCard interface
   SEC_SDMMC2_Periph : aliased SDMMC_Peripheral
     with Import, Address => SEC_SDMMC2_Base;

end STM32_SVD.SDMMC;
