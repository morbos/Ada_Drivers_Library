--  This spec has been automatically generated from STM32U5xx.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.FMC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype BCR_MTYP_Field is HAL.UInt2;
   subtype BCR_MWID_Field is HAL.UInt2;
   subtype BCR_CPSIZE_Field is HAL.UInt3;
   subtype BCR_NBLSET_Field is HAL.UInt2;

   --  SRAM/NOR-Flash chip-select control register for bank 1
   type BCR_Register is record
      --  Memory bank enable bit
      MBKEN          : Boolean := True;
      --  Address/data multiplexing enable bit
      MUXEN          : Boolean := True;
      --  Memory type
      MTYP           : BCR_MTYP_Field := 16#2#;
      --  Memory data bus width
      MWID           : BCR_MWID_Field := 16#1#;
      --  Flash access enable
      FACCEN         : Boolean := True;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#1#;
      --  Burst enable bit
      BURSTEN        : Boolean := False;
      --  Wait signal polarity bit
      WAITPOL        : Boolean := False;
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  Wait timing configuration
      WAITCFG        : Boolean := False;
      --  Write enable bit
      WREN           : Boolean := True;
      --  Wait enable bit
      WAITEN         : Boolean := True;
      --  Extended mode enable
      EXTMOD         : Boolean := False;
      --  Wait signal during asynchronous transfers
      ASYNCWAIT      : Boolean := False;
      --  CRAM Page Size
      CPSIZE         : BCR_CPSIZE_Field := 16#0#;
      --  Write burst enable
      CBURSTRW       : Boolean := False;
      --  Continuous clock enable
      CCLKEN         : Boolean := False;
      --  Write FIFO disable
      WFDIS          : Boolean := False;
      --  Byte lane (NBL) setup
      NBLSET         : BCR_NBLSET_Field := 16#0#;
      --  unspecified
      Reserved_24_30 : HAL.UInt7 := 16#0#;
      --  FMC controller enable
      FMCEN          : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for BCR_Register use record
      MBKEN          at 0 range 0 .. 0;
      MUXEN          at 0 range 1 .. 1;
      MTYP           at 0 range 2 .. 3;
      MWID           at 0 range 4 .. 5;
      FACCEN         at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      BURSTEN        at 0 range 8 .. 8;
      WAITPOL        at 0 range 9 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      WAITCFG        at 0 range 11 .. 11;
      WREN           at 0 range 12 .. 12;
      WAITEN         at 0 range 13 .. 13;
      EXTMOD         at 0 range 14 .. 14;
      ASYNCWAIT      at 0 range 15 .. 15;
      CPSIZE         at 0 range 16 .. 18;
      CBURSTRW       at 0 range 19 .. 19;
      CCLKEN         at 0 range 20 .. 20;
      WFDIS          at 0 range 21 .. 21;
      NBLSET         at 0 range 22 .. 23;
      Reserved_24_30 at 0 range 24 .. 30;
      FMCEN          at 0 range 31 .. 31;
   end record;

   subtype BTR_ADDSET_Field is HAL.UInt4;
   subtype BTR_ADDHLD_Field is HAL.UInt4;
   subtype BTR_DATAST_Field is HAL.UInt8;
   subtype BTR_BUSTURN_Field is HAL.UInt4;
   subtype BTR_CLKDIV_Field is HAL.UInt4;
   subtype BTR_DATLAT_Field is HAL.UInt4;
   subtype BTR_ACCMOD_Field is HAL.UInt2;
   subtype BTR_DATAHLD_Field is HAL.UInt2;

   --  SRAM/NOR-Flash chip-select timing register for bank 1
   type BTR_Register is record
      --  Address setup phase duration
      ADDSET  : BTR_ADDSET_Field := 16#F#;
      --  Address-hold phase duration
      ADDHLD  : BTR_ADDHLD_Field := 16#F#;
      --  Data-phase duration
      DATAST  : BTR_DATAST_Field := 16#FF#;
      --  Bus turnaround phase duration
      BUSTURN : BTR_BUSTURN_Field := 16#F#;
      --  Clock divide ratio (for FMC_CLK signal)
      CLKDIV  : BTR_CLKDIV_Field := 16#F#;
      --  Data latency for synchronous memory
      DATLAT  : BTR_DATLAT_Field := 16#F#;
      --  Access mode
      ACCMOD  : BTR_ACCMOD_Field := 16#0#;
      --  Data hold phase duration
      DATAHLD : BTR_DATAHLD_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for BTR_Register use record
      ADDSET  at 0 range 0 .. 3;
      ADDHLD  at 0 range 4 .. 7;
      DATAST  at 0 range 8 .. 15;
      BUSTURN at 0 range 16 .. 19;
      CLKDIV  at 0 range 20 .. 23;
      DATLAT  at 0 range 24 .. 27;
      ACCMOD  at 0 range 28 .. 29;
      DATAHLD at 0 range 30 .. 31;
   end record;

   subtype PCSCNTR_CSCOUNT_Field is HAL.UInt16;

   --  PSRAM chip select counter register
   type PCSCNTR_Register is record
      --  Chip select counter
      CSCOUNT        : PCSCNTR_CSCOUNT_Field := 16#0#;
      --  Counter Bank 1 enable
      CNTB1EN        : Boolean := False;
      --  Counter Bank 2 enable
      CNTB2EN        : Boolean := False;
      --  Counter Bank 3 enable
      CNTB3EN        : Boolean := False;
      --  Counter Bank 4 enable
      CNTB4EN        : Boolean := False;
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PCSCNTR_Register use record
      CSCOUNT        at 0 range 0 .. 15;
      CNTB1EN        at 0 range 16 .. 16;
      CNTB2EN        at 0 range 17 .. 17;
      CNTB3EN        at 0 range 18 .. 18;
      CNTB4EN        at 0 range 19 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   subtype PCR_PWID_Field is HAL.UInt2;
   subtype PCR_TCLR_Field is HAL.UInt4;
   subtype PCR_TAR_Field is HAL.UInt3;
   subtype PCR_ECCPS_Field is HAL.UInt3;

   --  NAND Flash control registers
   type PCR_Register is record
      --  unspecified
      Reserved_0_0   : HAL.Bit := 16#0#;
      --  Wait feature enable bit
      PWAITEN        : Boolean := False;
      --  NAND Flash memory bank enable bit
      PBKEN          : Boolean := False;
      --  Memory type
      PTYP           : Boolean := True;
      --  Data bus width
      PWID           : PCR_PWID_Field := 16#1#;
      --  ECC computation logic enable bit
      ECCEN          : Boolean := False;
      --  unspecified
      Reserved_7_8   : HAL.UInt2 := 16#0#;
      --  CLE to RE delay
      TCLR           : PCR_TCLR_Field := 16#0#;
      --  ALE to RE delay
      TAR            : PCR_TAR_Field := 16#0#;
      --  unspecified
      Reserved_16_16 : HAL.Bit := 16#0#;
      --  ECC page size
      ECCPS          : PCR_ECCPS_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PCR_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      PWAITEN        at 0 range 1 .. 1;
      PBKEN          at 0 range 2 .. 2;
      PTYP           at 0 range 3 .. 3;
      PWID           at 0 range 4 .. 5;
      ECCEN          at 0 range 6 .. 6;
      Reserved_7_8   at 0 range 7 .. 8;
      TCLR           at 0 range 9 .. 12;
      TAR            at 0 range 13 .. 15;
      Reserved_16_16 at 0 range 16 .. 16;
      ECCPS          at 0 range 17 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  status and interrupt register
   type SR_Register is record
      --  Interrupt rising edge status The flag is set by hardware and reset by
      --  software. Note: If this bit is written by software to 1 it will be
      --  set.
      IRS           : Boolean := False;
      --  Interrupt high-level status The flag is set by hardware and reset by
      --  software.
      ILS           : Boolean := False;
      --  Interrupt falling edge status The flag is set by hardware and reset
      --  by software. Note: If this bit is written by software to 1 it will be
      --  set.
      IFS           : Boolean := False;
      --  Interrupt rising edge detection enable bit
      IREN          : Boolean := False;
      --  Interrupt high-level detection enable bit
      ILEN          : Boolean := False;
      --  Interrupt falling edge detection enable bit
      IFEN          : Boolean := False;
      --  Read-only. FIFO empty. Read-only bit that provides the status of the
      --  FIFO
      FEMPT         : Boolean := True;
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register use record
      IRS           at 0 range 0 .. 0;
      ILS           at 0 range 1 .. 1;
      IFS           at 0 range 2 .. 2;
      IREN          at 0 range 3 .. 3;
      ILEN          at 0 range 4 .. 4;
      IFEN          at 0 range 5 .. 5;
      FEMPT         at 0 range 6 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   subtype PMEM_MEMSET_Field is HAL.UInt8;
   subtype PMEM_MEMWAIT_Field is HAL.UInt8;
   subtype PMEM_MEMHOLD_Field is HAL.UInt8;
   subtype PMEM_MEMHIZ_Field is HAL.UInt8;

   --  Common memory space timing register
   type PMEM_Register is record
      --  Common memory x setup time These bits define the number of KCK_FMC
      --  (+1) clock cycles to set up the address before the command assertion
      --  (NWE, NOE), for NAND Flash read or write access to common memory
      --  space:
      MEMSET  : PMEM_MEMSET_Field := 16#FC#;
      --  Common memory wait time These bits define the minimum number of
      --  KCK_FMC (+1) clock cycles to assert the command (NWE, NOE), for NAND
      --  Flash read or write access to common memory space. The duration of
      --  command assertion is extended if the wait signal (NWAIT) is active
      --  (low) at the end of the programmed value of KCK_FMC:
      MEMWAIT : PMEM_MEMWAIT_Field := 16#FC#;
      --  Common memory hold time These bits define the number of KCK_FMC clock
      --  cycles for write accesses and KCK_FMC+1 clock cycles for read
      --  accesses during which the address is held (and data for write
      --  accesses) after the command is de-asserted (NWE, NOE), for NAND Flash
      --  read or write access to common memory space:
      MEMHOLD : PMEM_MEMHOLD_Field := 16#FC#;
      --  Common memory x data bus Hi-Z time These bits define the number of
      --  KCK_FMC clock cycles during which the data bus is kept Hi-Z after the
      --  start of a NAND Flash write access to common memory space. This is
      --  only valid for write transactions:
      MEMHIZ  : PMEM_MEMHIZ_Field := 16#FC#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMEM_Register use record
      MEMSET  at 0 range 0 .. 7;
      MEMWAIT at 0 range 8 .. 15;
      MEMHOLD at 0 range 16 .. 23;
      MEMHIZ  at 0 range 24 .. 31;
   end record;

   subtype PATT_ATTSET_Field is HAL.UInt8;
   subtype PATT_ATTWAIT_Field is HAL.UInt8;
   subtype PATT_ATTHOLD_Field is HAL.UInt8;
   subtype PATT_ATTHIZ_Field is HAL.UInt8;

   --  The FMC_PATT read/write register contains the timing information for
   --  NAND Flash memory bank. It is used for 8-bit accesses to the attribute
   --  memory space of the NAND Flash for the last address write access if the
   --  timing must differ from that of previous accesses (for Ready/Busy
   --  management, refer to Section20.8.5: NAND Flash prewait feature).
   type PATT_Register is record
      --  Attribute memory setup time These bits define the number of KCK_FMC
      --  (+1) clock cycles to set up address before the command assertion
      --  (NWE, NOE), for NAND Flash read or write access to attribute memory
      --  space:
      ATTSET  : PATT_ATTSET_Field := 16#FC#;
      --  Attribute memory wait time These bits define the minimum number of x
      --  KCK_FMC (+1) clock cycles to assert the command (NWE, NOE), for NAND
      --  Flash read or write access to attribute memory space. The duration
      --  for command assertion is extended if the wait signal (NWAIT) is
      --  active (low) at the end of the programmed value of KCK_FMC:
      ATTWAIT : PATT_ATTWAIT_Field := 16#FC#;
      --  Attribute memory hold time These bits define the number of KCK_FMC
      --  clock cycles during which the address is held (and data for write
      --  access) after the command de-assertion (NWE, NOE), for NAND Flash
      --  read or write access to attribute memory space:
      ATTHOLD : PATT_ATTHOLD_Field := 16#FC#;
      --  Attribute memory data bus Hi-Z time These bits define the number of
      --  KCK_FMC clock cycles during which the data bus is kept in Hi-Z after
      --  the start of a NAND Flash write access to attribute memory space on
      --  socket. Only valid for writ transaction:
      ATTHIZ  : PATT_ATTHIZ_Field := 16#FC#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PATT_Register use record
      ATTSET  at 0 range 0 .. 7;
      ATTWAIT at 0 range 8 .. 15;
      ATTHOLD at 0 range 16 .. 23;
      ATTHIZ  at 0 range 24 .. 31;
   end record;

   subtype BWTR_ADDSET_Field is HAL.UInt4;
   subtype BWTR_ADDHLD_Field is HAL.UInt4;
   subtype BWTR_DATAST_Field is HAL.UInt8;
   subtype BWTR_BUSTURN_Field is HAL.UInt4;
   subtype BWTR_ACCMOD_Field is HAL.UInt2;
   subtype BWTR_DATAHLD_Field is HAL.UInt2;

   --  SRAM/NOR-Flash write timing registers 1
   type BWTR_Register is record
      --  Address setup phase duration
      ADDSET         : BWTR_ADDSET_Field := 16#F#;
      --  Address-hold phase duration
      ADDHLD         : BWTR_ADDHLD_Field := 16#F#;
      --  Data-phase duration
      DATAST         : BWTR_DATAST_Field := 16#FF#;
      --  Bus turnaround phase duration
      BUSTURN        : BWTR_BUSTURN_Field := 16#F#;
      --  unspecified
      Reserved_20_27 : HAL.UInt8 := 16#FF#;
      --  Access mode
      ACCMOD         : BWTR_ACCMOD_Field := 16#0#;
      --  Data hold phase duration
      DATAHLD        : BWTR_DATAHLD_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for BWTR_Register use record
      ADDSET         at 0 range 0 .. 3;
      ADDHLD         at 0 range 4 .. 7;
      DATAST         at 0 range 8 .. 15;
      BUSTURN        at 0 range 16 .. 19;
      Reserved_20_27 at 0 range 20 .. 27;
      ACCMOD         at 0 range 28 .. 29;
      DATAHLD        at 0 range 30 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  FMC
   type FMC_Peripheral is record
      --  SRAM/NOR-Flash chip-select control register for bank 1
      BCR1    : aliased BCR_Register;
      --  SRAM/NOR-Flash chip-select timing register for bank 1
      BTR1    : aliased BTR_Register;
      --  SRAM/NOR-Flash chip-select control register for bank 2
      BCR2    : aliased BCR_Register;
      --  SRAM/NOR-Flash chip-select timing register for bank 2
      BTR2    : aliased BTR_Register;
      --  SRAM/NOR-Flash chip-select control register for bank 3
      BCR3    : aliased BCR_Register;
      --  SRAM/NOR-Flash chip-select timing register for bank 3
      BTR3    : aliased BTR_Register;
      --  SRAM/NOR-Flash chip-select control register for bank 4
      BCR4    : aliased BCR_Register;
      --  SRAM/NOR-Flash chip-select timing register for bank 4
      BTR4    : aliased BTR_Register;
      --  PSRAM chip select counter register
      PCSCNTR : aliased PCSCNTR_Register;
      --  NAND Flash control registers
      PCR     : aliased PCR_Register;
      --  status and interrupt register
      SR      : aliased SR_Register;
      --  Common memory space timing register
      PMEM    : aliased PMEM_Register;
      --  The FMC_PATT read/write register contains the timing information for
      --  NAND Flash memory bank. It is used for 8-bit accesses to the
      --  attribute memory space of the NAND Flash for the last address write
      --  access if the timing must differ from that of previous accesses (for
      --  Ready/Busy management, refer to Section20.8.5: NAND Flash prewait
      --  feature).
      PATT    : aliased PATT_Register;
      --  This register contain the current error correction code value
      --  computed by the ECC computation modules of the FMC NAND controller.
      --  When the CPU reads/writes the data from a NAND Flash memory page at
      --  the correct address (refer to Section20.8.6: Computation of the error
      --  correction code (ECC) in NAND Flash memory), the data read/written
      --  from/to the NAND Flash memory are processed automatically by the ECC
      --  computation module. When X bytes have been read (according to the
      --  ECCPS field in the FMC_PCR registers), the CPU must read the computed
      --  ECC value from the FMC_ECC registers. It then verifies if these
      --  computed parity data are the same as the parity value recorded in the
      --  spare area, to determine whether a page is valid, and, to correct it
      --  otherwise. The FMC_ECCR register should be cleared after being read
      --  by setting the ECCEN bit to 0. To compute a new data block, the ECCEN
      --  bit must be set to 1.
      ECCR    : aliased HAL.UInt32;
      --  SRAM/NOR-Flash write timing registers 1
      BWTR1   : aliased BWTR_Register;
      --  SRAM/NOR-Flash write timing registers 2
      BWTR2   : aliased BWTR_Register;
      --  SRAM/NOR-Flash write timing registers 3
      BWTR3   : aliased BWTR_Register;
      --  SRAM/NOR-Flash write timing registers 4
      BWTR4   : aliased BWTR_Register;
   end record
     with Volatile;

   for FMC_Peripheral use record
      BCR1    at 16#0# range 0 .. 31;
      BTR1    at 16#4# range 0 .. 31;
      BCR2    at 16#8# range 0 .. 31;
      BTR2    at 16#C# range 0 .. 31;
      BCR3    at 16#10# range 0 .. 31;
      BTR3    at 16#14# range 0 .. 31;
      BCR4    at 16#18# range 0 .. 31;
      BTR4    at 16#1C# range 0 .. 31;
      PCSCNTR at 16#20# range 0 .. 31;
      PCR     at 16#80# range 0 .. 31;
      SR      at 16#84# range 0 .. 31;
      PMEM    at 16#88# range 0 .. 31;
      PATT    at 16#8C# range 0 .. 31;
      ECCR    at 16#94# range 0 .. 31;
      BWTR1   at 16#104# range 0 .. 31;
      BWTR2   at 16#10C# range 0 .. 31;
      BWTR3   at 16#114# range 0 .. 31;
      BWTR4   at 16#11C# range 0 .. 31;
   end record;

   --  FMC
   FMC_Periph : aliased FMC_Peripheral
     with Import, Address => FMC_Base;

   --  FMC
   SEC_FMC_Periph : aliased FMC_Peripheral
     with Import, Address => SEC_FMC_Base;

end STM32_SVD.FMC;
