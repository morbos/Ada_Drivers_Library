--  This spec has been automatically generated from STM32F103.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.SDIO is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype POWER_PWRCTRL_Field is HAL.UInt2;

   --  Bits 1:0 = PWRCTRL: Power supply control bits
   type POWER_Register is record
      --  PWRCTRL
      PWRCTRL       : POWER_PWRCTRL_Field := 16#0#;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for POWER_Register use record
      PWRCTRL       at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   subtype CLKCR_CLKDIV_Field is HAL.UInt8;
   subtype CLKCR_WIDBUS_Field is HAL.UInt2;

   --  SDI clock control register (SDIO_CLKCR)
   type CLKCR_Register is record
      --  Clock divide factor
      CLKDIV         : CLKCR_CLKDIV_Field := 16#0#;
      --  Clock enable bit
      CLKEN          : Boolean := False;
      --  Power saving configuration bit
      PWRSAV         : Boolean := False;
      --  Clock divider bypass enable bit
      BYPASS         : Boolean := False;
      --  Wide bus mode enable bit
      WIDBUS         : CLKCR_WIDBUS_Field := 16#0#;
      --  SDIO_CK dephasing selection bit
      NEGEDGE        : Boolean := False;
      --  HW Flow Control enable
      HWFC_EN        : Boolean := False;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CLKCR_Register use record
      CLKDIV         at 0 range 0 .. 7;
      CLKEN          at 0 range 8 .. 8;
      PWRSAV         at 0 range 9 .. 9;
      BYPASS         at 0 range 10 .. 10;
      WIDBUS         at 0 range 11 .. 12;
      NEGEDGE        at 0 range 13 .. 13;
      HWFC_EN        at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype CMD_CMDINDEX_Field is HAL.UInt6;
   subtype CMD_WAITRESP_Field is HAL.UInt2;

   --  SDIO command register (SDIO_CMD)
   type CMD_Register is record
      --  CMDINDEX
      CMDINDEX       : CMD_CMDINDEX_Field := 16#0#;
      --  WAITRESP
      WAITRESP       : CMD_WAITRESP_Field := 16#0#;
      --  WAITINT
      WAITINT        : Boolean := False;
      --  WAITPEND
      WAITPEND       : Boolean := False;
      --  CPSMEN
      CPSMEN         : Boolean := False;
      --  SDIOSuspend
      SDIOSuspend    : Boolean := False;
      --  ENCMDcompl
      ENCMDcompl     : Boolean := False;
      --  nIEN
      nIEN           : Boolean := False;
      --  CE_ATACMD
      CE_ATACMD      : Boolean := False;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CMD_Register use record
      CMDINDEX       at 0 range 0 .. 5;
      WAITRESP       at 0 range 6 .. 7;
      WAITINT        at 0 range 8 .. 8;
      WAITPEND       at 0 range 9 .. 9;
      CPSMEN         at 0 range 10 .. 10;
      SDIOSuspend    at 0 range 11 .. 11;
      ENCMDcompl     at 0 range 12 .. 12;
      nIEN           at 0 range 13 .. 13;
      CE_ATACMD      at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype RESPCMD_RESPCMD_Field is HAL.UInt6;

   --  SDIO command register
   type RESPCMD_Register is record
      --  Read-only. RESPCMD
      RESPCMD       : RESPCMD_RESPCMD_Field;
      --  unspecified
      Reserved_6_31 : HAL.UInt26;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RESPCMD_Register use record
      RESPCMD       at 0 range 0 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   subtype DLEN_DATALENGTH_Field is HAL.UInt25;

   --  Bits 24:0 = DATALENGTH: Data length value
   type DLEN_Register is record
      --  Data length value
      DATALENGTH     : DLEN_DATALENGTH_Field := 16#0#;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DLEN_Register use record
      DATALENGTH     at 0 range 0 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype DCTRL_DBLOCKSIZE_Field is HAL.UInt4;

   --  SDIO data control register (SDIO_DCTRL)
   type DCTRL_Register is record
      --  DTEN
      DTEN           : Boolean := False;
      --  DTDIR
      DTDIR          : Boolean := False;
      --  DTMODE
      DTMODE         : Boolean := False;
      --  DMAEN
      DMAEN          : Boolean := False;
      --  DBLOCKSIZE
      DBLOCKSIZE     : DCTRL_DBLOCKSIZE_Field := 16#0#;
      --  PWSTART
      PWSTART        : Boolean := False;
      --  PWSTOP
      PWSTOP         : Boolean := False;
      --  RWMOD
      RWMOD          : Boolean := False;
      --  SDIOEN
      SDIOEN         : Boolean := False;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DCTRL_Register use record
      DTEN           at 0 range 0 .. 0;
      DTDIR          at 0 range 1 .. 1;
      DTMODE         at 0 range 2 .. 2;
      DMAEN          at 0 range 3 .. 3;
      DBLOCKSIZE     at 0 range 4 .. 7;
      PWSTART        at 0 range 8 .. 8;
      PWSTOP         at 0 range 9 .. 9;
      RWMOD          at 0 range 10 .. 10;
      SDIOEN         at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype DCOUNT_DATACOUNT_Field is HAL.UInt25;

   --  Bits 24:0 = DATACOUNT: Data count value
   type DCOUNT_Register is record
      --  Read-only. Data count value
      DATACOUNT      : DCOUNT_DATACOUNT_Field;
      --  unspecified
      Reserved_25_31 : HAL.UInt7;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DCOUNT_Register use record
      DATACOUNT      at 0 range 0 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  SDIO status register (SDIO_STA)
   type STA_Register is record
      --  Read-only. CCRCFAIL
      CCRCFAIL       : Boolean;
      --  Read-only. DCRCFAIL
      DCRCFAIL       : Boolean;
      --  Read-only. CTIMEOUT
      CTIMEOUT       : Boolean;
      --  Read-only. DTIMEOUT
      DTIMEOUT       : Boolean;
      --  Read-only. TXUNDERR
      TXUNDERR       : Boolean;
      --  Read-only. RXOVERR
      RXOVERR        : Boolean;
      --  Read-only. CMDREND
      CMDREND        : Boolean;
      --  Read-only. CMDSENT
      CMDSENT        : Boolean;
      --  Read-only. DATAEND
      DATAEND        : Boolean;
      --  Read-only. STBITERR
      STBITERR       : Boolean;
      --  Read-only. DBCKEND
      DBCKEND        : Boolean;
      --  Read-only. CMDACT
      CMDACT         : Boolean;
      --  Read-only. TXACT
      TXACT          : Boolean;
      --  Read-only. RXACT
      RXACT          : Boolean;
      --  Read-only. TXFIFOHE
      TXFIFOHE       : Boolean;
      --  Read-only. RXFIFOHF
      RXFIFOHF       : Boolean;
      --  Read-only. TXFIFOF
      TXFIFOF        : Boolean;
      --  Read-only. RXFIFOF
      RXFIFOF        : Boolean;
      --  Read-only. TXFIFOE
      TXFIFOE        : Boolean;
      --  Read-only. RXFIFOE
      RXFIFOE        : Boolean;
      --  Read-only. TXDAVL
      TXDAVL         : Boolean;
      --  Read-only. RXDAVL
      RXDAVL         : Boolean;
      --  Read-only. SDIOIT
      SDIOIT         : Boolean;
      --  Read-only. CEATAEND
      CEATAEND       : Boolean;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for STA_Register use record
      CCRCFAIL       at 0 range 0 .. 0;
      DCRCFAIL       at 0 range 1 .. 1;
      CTIMEOUT       at 0 range 2 .. 2;
      DTIMEOUT       at 0 range 3 .. 3;
      TXUNDERR       at 0 range 4 .. 4;
      RXOVERR        at 0 range 5 .. 5;
      CMDREND        at 0 range 6 .. 6;
      CMDSENT        at 0 range 7 .. 7;
      DATAEND        at 0 range 8 .. 8;
      STBITERR       at 0 range 9 .. 9;
      DBCKEND        at 0 range 10 .. 10;
      CMDACT         at 0 range 11 .. 11;
      TXACT          at 0 range 12 .. 12;
      RXACT          at 0 range 13 .. 13;
      TXFIFOHE       at 0 range 14 .. 14;
      RXFIFOHF       at 0 range 15 .. 15;
      TXFIFOF        at 0 range 16 .. 16;
      RXFIFOF        at 0 range 17 .. 17;
      TXFIFOE        at 0 range 18 .. 18;
      RXFIFOE        at 0 range 19 .. 19;
      TXDAVL         at 0 range 20 .. 20;
      RXDAVL         at 0 range 21 .. 21;
      SDIOIT         at 0 range 22 .. 22;
      CEATAEND       at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  SDIO interrupt clear register (SDIO_ICR)
   type ICR_Register is record
      --  CCRCFAILC
      CCRCFAILC      : Boolean := False;
      --  DCRCFAILC
      DCRCFAILC      : Boolean := False;
      --  CTIMEOUTC
      CTIMEOUTC      : Boolean := False;
      --  DTIMEOUTC
      DTIMEOUTC      : Boolean := False;
      --  TXUNDERRC
      TXUNDERRC      : Boolean := False;
      --  RXOVERRC
      RXOVERRC       : Boolean := False;
      --  CMDRENDC
      CMDRENDC       : Boolean := False;
      --  CMDSENTC
      CMDSENTC       : Boolean := False;
      --  DATAENDC
      DATAENDC       : Boolean := False;
      --  STBITERRC
      STBITERRC      : Boolean := False;
      --  DBCKENDC
      DBCKENDC       : Boolean := False;
      --  unspecified
      Reserved_11_21 : HAL.UInt11 := 16#0#;
      --  SDIOITC
      SDIOITC        : Boolean := False;
      --  CEATAENDC
      CEATAENDC      : Boolean := False;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
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
      STBITERRC      at 0 range 9 .. 9;
      DBCKENDC       at 0 range 10 .. 10;
      Reserved_11_21 at 0 range 11 .. 21;
      SDIOITC        at 0 range 22 .. 22;
      CEATAENDC      at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  SDIO mask register (SDIO_MASK)
   type MASK_Register is record
      --  CCRCFAILIE
      CCRCFAILIE     : Boolean := False;
      --  DCRCFAILIE
      DCRCFAILIE     : Boolean := False;
      --  CTIMEOUTIE
      CTIMEOUTIE     : Boolean := False;
      --  DTIMEOUTIE
      DTIMEOUTIE     : Boolean := False;
      --  TXUNDERRIE
      TXUNDERRIE     : Boolean := False;
      --  RXOVERRIE
      RXOVERRIE      : Boolean := False;
      --  CMDRENDIE
      CMDRENDIE      : Boolean := False;
      --  CMDSENTIE
      CMDSENTIE      : Boolean := False;
      --  DATAENDIE
      DATAENDIE      : Boolean := False;
      --  STBITERRIE
      STBITERRIE     : Boolean := False;
      --  DBACKENDIE
      DBACKENDIE     : Boolean := False;
      --  CMDACTIE
      CMDACTIE       : Boolean := False;
      --  TXACTIE
      TXACTIE        : Boolean := False;
      --  RXACTIE
      RXACTIE        : Boolean := False;
      --  TXFIFOHEIE
      TXFIFOHEIE     : Boolean := False;
      --  RXFIFOHFIE
      RXFIFOHFIE     : Boolean := False;
      --  TXFIFOFIE
      TXFIFOFIE      : Boolean := False;
      --  RXFIFOFIE
      RXFIFOFIE      : Boolean := False;
      --  TXFIFOEIE
      TXFIFOEIE      : Boolean := False;
      --  RXFIFOEIE
      RXFIFOEIE      : Boolean := False;
      --  TXDAVLIE
      TXDAVLIE       : Boolean := False;
      --  RXDAVLIE
      RXDAVLIE       : Boolean := False;
      --  SDIOITIE
      SDIOITIE       : Boolean := False;
      --  CEATENDIE
      CEATENDIE      : Boolean := False;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MASK_Register use record
      CCRCFAILIE     at 0 range 0 .. 0;
      DCRCFAILIE     at 0 range 1 .. 1;
      CTIMEOUTIE     at 0 range 2 .. 2;
      DTIMEOUTIE     at 0 range 3 .. 3;
      TXUNDERRIE     at 0 range 4 .. 4;
      RXOVERRIE      at 0 range 5 .. 5;
      CMDRENDIE      at 0 range 6 .. 6;
      CMDSENTIE      at 0 range 7 .. 7;
      DATAENDIE      at 0 range 8 .. 8;
      STBITERRIE     at 0 range 9 .. 9;
      DBACKENDIE     at 0 range 10 .. 10;
      CMDACTIE       at 0 range 11 .. 11;
      TXACTIE        at 0 range 12 .. 12;
      RXACTIE        at 0 range 13 .. 13;
      TXFIFOHEIE     at 0 range 14 .. 14;
      RXFIFOHFIE     at 0 range 15 .. 15;
      TXFIFOFIE      at 0 range 16 .. 16;
      RXFIFOFIE      at 0 range 17 .. 17;
      TXFIFOEIE      at 0 range 18 .. 18;
      RXFIFOEIE      at 0 range 19 .. 19;
      TXDAVLIE       at 0 range 20 .. 20;
      RXDAVLIE       at 0 range 21 .. 21;
      SDIOITIE       at 0 range 22 .. 22;
      CEATENDIE      at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype FIFOCNT_FIF0COUNT_Field is HAL.UInt24;

   --  Bits 23:0 = FIFOCOUNT: Remaining number of words to be written to or
   --  read from the FIFO
   type FIFOCNT_Register is record
      --  Read-only. FIF0COUNT
      FIF0COUNT      : FIFOCNT_FIF0COUNT_Field;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FIFOCNT_Register use record
      FIF0COUNT      at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Secure digital input/output interface
   type SDIO_Peripheral is record
      --  Bits 1:0 = PWRCTRL: Power supply control bits
      POWER   : aliased POWER_Register;
      --  SDI clock control register (SDIO_CLKCR)
      CLKCR   : aliased CLKCR_Register;
      --  Bits 31:0 = : Command argument
      ARG     : aliased HAL.UInt32;
      --  SDIO command register (SDIO_CMD)
      CMD     : aliased CMD_Register;
      --  SDIO command register
      RESPCMD : aliased RESPCMD_Register;
      --  Bits 31:0 = CARDSTATUS1
      RESPI1  : aliased HAL.UInt32;
      --  Bits 31:0 = CARDSTATUS2
      RESP2   : aliased HAL.UInt32;
      --  Bits 31:0 = CARDSTATUS3
      RESP3   : aliased HAL.UInt32;
      --  Bits 31:0 = CARDSTATUS4
      RESP4   : aliased HAL.UInt32;
      --  Bits 31:0 = DATATIME: Data timeout period
      DTIMER  : aliased HAL.UInt32;
      --  Bits 24:0 = DATALENGTH: Data length value
      DLEN    : aliased DLEN_Register;
      --  SDIO data control register (SDIO_DCTRL)
      DCTRL   : aliased DCTRL_Register;
      --  Bits 24:0 = DATACOUNT: Data count value
      DCOUNT  : aliased DCOUNT_Register;
      --  SDIO status register (SDIO_STA)
      STA     : aliased STA_Register;
      --  SDIO interrupt clear register (SDIO_ICR)
      ICR     : aliased ICR_Register;
      --  SDIO mask register (SDIO_MASK)
      MASK    : aliased MASK_Register;
      --  Bits 23:0 = FIFOCOUNT: Remaining number of words to be written to or
      --  read from the FIFO
      FIFOCNT : aliased FIFOCNT_Register;
      --  bits 31:0 = FIFOData: Receive and transmit FIFO data
      FIFO    : aliased HAL.UInt32;
   end record
     with Volatile;

   for SDIO_Peripheral use record
      POWER   at 16#0# range 0 .. 31;
      CLKCR   at 16#4# range 0 .. 31;
      ARG     at 16#8# range 0 .. 31;
      CMD     at 16#C# range 0 .. 31;
      RESPCMD at 16#10# range 0 .. 31;
      RESPI1  at 16#14# range 0 .. 31;
      RESP2   at 16#18# range 0 .. 31;
      RESP3   at 16#1C# range 0 .. 31;
      RESP4   at 16#20# range 0 .. 31;
      DTIMER  at 16#24# range 0 .. 31;
      DLEN    at 16#28# range 0 .. 31;
      DCTRL   at 16#2C# range 0 .. 31;
      DCOUNT  at 16#30# range 0 .. 31;
      STA     at 16#34# range 0 .. 31;
      ICR     at 16#38# range 0 .. 31;
      MASK    at 16#3C# range 0 .. 31;
      FIFOCNT at 16#48# range 0 .. 31;
      FIFO    at 16#80# range 0 .. 31;
   end record;

   --  Secure digital input/output interface
   SDIO_Periph : aliased SDIO_Peripheral
     with Import, Address => System'To_Address (16#40018000#);

end STM32_SVD.SDIO;
