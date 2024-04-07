--  This spec has been automatically generated from xyzzy

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

   -----------------------------
   -- BXR cluster's Registers --
   -----------------------------

   subtype FMC_BCRx_BXR_MTYP_Field is HAL.UInt2;
   subtype FMC_BCRx_BXR_MWID_Field is HAL.UInt2;
   subtype FMC_BCRx_BXR_CPSIZE_Field is HAL.UInt3;
   subtype FMC_BCRx_BXR_NBLSET_Field is HAL.UInt2;

   --  FMC_BCR1
   type FMC_BCRx_BXR_Register is record
      --  Memory bank enable bit This bit enables the memory bank. After reset
      --  Bank1 is enabled, all others are disabled. Accessing a disabled bank
      --  causes an ERROR on AXI bus.
      MBKEN          : Boolean := False;
      --  Address/data multiplexing enable bit When this bit is set, the
      --  address and data values are multiplexed on the data bus, valid only
      --  with NOR and PSRAM memories:
      MUXEN          : Boolean := False;
      --  Memory type These bits define the type of external memory attached to
      --  the corresponding memory bank:
      MTYP           : FMC_BCRx_BXR_MTYP_Field := 16#0#;
      --  Memory data bus width Defines the external memory device width, valid
      --  for all type of memories.
      MWID           : FMC_BCRx_BXR_MWID_Field := 16#0#;
      --  Flash access enable This bit enables NOR Flash memory access
      --  operations.
      FACCEN         : Boolean := False;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Burst enable bit This bit enables/disables synchronous accesses
      --  during read operations. It is valid only for synchronous memories
      --  operating in Burst mode:
      BURSTEN        : Boolean := False;
      --  Wait signal polarity bit This bit defines the polarity of the wait
      --  signal from memory used for either in synchronous or asynchronous
      --  mode:
      WAITPOL        : Boolean := False;
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  Wait timing configuration The NWAIT signal indicates whether the data
      --  from the memory are valid or if a wait state must be inserted when
      --  accessing the memory in synchronous mode. This configuration bit
      --  determines if NWAIT is asserted by the memory one clock cycle before
      --  the wait state or during the wait state:
      WAITCFG        : Boolean := False;
      --  Write enable bit This bit indicates whether write operations are
      --  enabled/disabled in the bank by the FMC:
      WREN           : Boolean := False;
      --  Wait enable bit This bit enables/disables wait-state insertion via
      --  the NWAIT signal when accessing the memory in synchronous mode.
      WAITEN         : Boolean := False;
      --  Extended mode enable. This bit enables the FMC to program the write
      --  timings for asynchronous accesses inside the FMC_BWTR register, thus
      --  resulting in different timings for read and write operations. Note:
      --  When the extended mode is disabled, the FMC can operate in Mode1 or
      --  Mode2 as follows: ** Mode 1 is the default mode when the SRAM/PSRAM
      --  memory type is selected (MTYP =0x0 or 0x01) ** Mode 2 is the default
      --  mode when the NOR memory type is selected (MTYP = 0x10).
      EXTMOD         : Boolean := False;
      --  Wait signal during asynchronous transfers This bit enables/disables
      --  the FMC to use the wait signal even during an asynchronous protocol.
      ASYNCWAIT      : Boolean := False;
      --  CRAM Page Size These are used for Cellular RAM 1.5 which does not
      --  allow burst access to cross the address boundaries between pages.
      --  When these bits are configured, the FMC controller splits
      --  automatically the burst access when the memory page size is reached
      --  (refer to memory datasheet for page size). Other configuration:
      --  reserved.
      CPSIZE         : FMC_BCRx_BXR_CPSIZE_Field := 16#0#;
      --  Write burst enable For PSRAM (CRAM) operating in Burst mode, the bit
      --  enables synchronous accesses during write operations. The enable bit
      --  for synchronous read accesses is the BURSTEN bit in the FMC_BCRx
      --  register.
      CBURSTRW       : Boolean := False;
      --  Continuous Clock Enable This bit enables the FMC_CLK clock output to
      --  external memory devices. Note: The CCLKEN bit of the FMC_BCR2..4
      --  registers is dont care. It is only enabled through the FMC_BCR1
      --  register. Bank 1 must be configured in synchronous mode to generate
      --  the FMC_CLK continuous clock. If CCLKEN bit is set, the FMC_CLK clock
      --  ratio is specified by CLKDIV value in the FMC_BTR1 register. CLKDIV
      --  in FMC_BWTR1 is dont care. If the synchronous mode is used and CCLKEN
      --  bit is set, the synchronous memories connected to other banks than
      --  Bank 1 are clocked by the same clock (the CLKDIV value in the
      --  FMC_BTR2..4 and FMC_BWTR2..4 registers for other banks has no
      --  effect.)
      CCLKEN         : Boolean := False;
      --  Write FIFO Disable This bit disables the Write FIFO used by the FMC
      --  controller. Note: The WFDIS bit of the FMC_BCR2..4 registers is dont
      --  care. It is only enabled through the FMC_BCR1 register.
      WFDIS          : Boolean := False;
      --  NBLSET
      NBLSET         : FMC_BCRx_BXR_NBLSET_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FMC_BCRx_BXR_Register use record
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
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype FMC_BTRx_BXR_ADDSET_Field is HAL.UInt4;
   subtype FMC_BTRx_BXR_ADDHLD_Field is HAL.UInt4;
   subtype FMC_BTRx_BXR_DATAST_Field is HAL.UInt8;
   subtype FMC_BTRx_BXR_BUSTURN_Field is HAL.UInt4;
   subtype FMC_BTRx_BXR_CLKDIV_Field is HAL.UInt4;
   subtype FMC_BTRx_BXR_DATLAT_Field is HAL.UInt4;
   subtype FMC_BTRx_BXR_ACCMOD_Field is HAL.UInt2;
   subtype FMC_BTRx_BXR_DATAHLD_Field is HAL.UInt2;

   --  This register contains the control information of each memory bank, used
   --  for SRAMs, PSRAM and NOR Flash memories.If the EXTMOD bit is set in the
   --  FMC_BCRx register, then this register is partitioned for write and read
   --  access, that is, 2 registers are available: one to configure read
   --  accesses (this register) and one to configure write accesses (FMC_BWTRx
   --  registers).
   type FMC_BTRx_BXR_Register is record
      --  Address setup phase duration These bits are written by software to
      --  define the duration of the address setup phase (refer to Figure81 to
      --  Figure93), used in SRAMs, ROMs and asynchronous NOR Flash: For each
      --  access mode address setup phase duration, please refer to the
      --  respective figure (refer to Figure81 to Figure93). Note: In
      --  synchronous accesses, this value is dont care. In Muxed mode or Mode
      --  D, the minimum value for ADDSET is 1.
      ADDSET  : FMC_BTRx_BXR_ADDSET_Field := 16#0#;
      --  Address-hold phase duration These bits are written by software to
      --  define the duration of the address hold phase (refer to Figure81 to
      --  Figure93), used in mode D or multiplexed accesses: For each access
      --  mode address-hold phase duration, please refer to the respective
      --  figure (Figure81 to Figure93). Note: In synchronous accesses, this
      --  value is not used, the address hold phase is always 1 memory clock
      --  period duration.
      ADDHLD  : FMC_BTRx_BXR_ADDHLD_Field := 16#0#;
      --  Data-phase duration These bits are written by software to define the
      --  duration of the data phase (refer to Figure81 to Figure93), used in
      --  asynchronous accesses: For each memory type and access mode
      --  data-phase duration, please refer to the respective figure (Figure81
      --  to Figure93). Example: Mode1, write access, DATAST=1: Data-phase
      --  duration= DATAST+1 = 2 KCK_FMC clock cycles. Note: In synchronous
      --  accesses, this value is dont care.
      DATAST  : FMC_BTRx_BXR_DATAST_Field := 16#0#;
      --  Bus turnaround phase duration These bits are written by software to
      --  add a delay at the end of a write-to-read or read-to write
      --  transaction. The programmed bus turnaround delay is inserted between
      --  an asynchronous read (in muxed or mode D) or write transaction and
      --  any other asynchronous /synchronous read/write from/to a static bank.
      --  If a read operation is performed, the bank can be the same or a
      --  different one, whereas it must be different in case of write
      --  operation to the bank, except in muxed mode or mode D. In some cases,
      --  whatever the programmed BUSTRUN values, the bus turnaround delay is
      --  fixed as follows: The bus turnaround delay is not inserted between
      --  two consecutive asynchronous write transfers to the same static
      --  memory bank except in muxed mode and mode D. There is a bus
      --  turnaround delay of 1 FMC clock cycle between: Two consecutive
      --  asynchronous read transfers to the same static memory bank except for
      --  modes muxed and D. An asynchronous read to an asynchronous or
      --  synchronous write to any static bank or dynamic bank except in modes
      --  muxed and D mode. There is a bus turnaround delay of 2 FMC clock
      --  cycle between: Two consecutive synchronous write operations (in Burst
      --  or Single mode) to the same bank. A synchronous write (burst or
      --  single) access and an asynchronous write or read transfer to or from
      --  static memory bank (the bank can be the same or a different one in
      --  case of a read operation. Two consecutive synchronous read operations
      --  (in Burst or Single mode) followed by any synchronous/asynchronous
      --  read or write from/to another static memory bank. There is a bus
      --  turnaround delay of 3 FMC clock cycle between: Two consecutive
      --  synchronous write operations (in Burst or Single mode) to different
      --  static banks. A synchronous write access (in Burst or Single mode)
      --  and a synchronous read from the same or a different bank. The bus
      --  turnaround delay allows to match the minimum time between consecutive
      --  transactions (tEHEL from NEx high to NEx low) and the maximum time
      --  required by the memory to free the data bus after a read access
      --  (tEHQZ): (BUSTRUN + 1) KCK_FMC period &#8805; tEHELmin and (BUSTRUN +
      --  2)KCK_FMC period &#8805; tEHQZmax if EXTMOD = 0 (BUSTRUN + 2)KCK_FMC
      --  period &#8805; max (tEHELmin, tEHQZmax) if EXTMOD = 126. ...
      BUSTURN : FMC_BTRx_BXR_BUSTURN_Field := 16#0#;
      --  Clock divide ratio (for FMC_CLK signal) These bits define the period
      --  of FMC_CLK clock output signal, expressed in number of KCK_FMC
      --  cycles: In asynchronous NOR Flash, SRAM or PSRAM accesses, this value
      --  is dont care. Note: Refer to Section20.6.5: Synchronous transactions
      --  for FMC_CLK divider ratio formula)
      CLKDIV  : FMC_BTRx_BXR_CLKDIV_Field := 16#0#;
      --  Data latency for synchronous memory For synchronous access with read
      --  write burst mode enabled these bits define the number of memory clock
      --  cycles
      DATLAT  : FMC_BTRx_BXR_DATLAT_Field := 16#0#;
      --  Access mode These bits specify the asynchronous access modes as shown
      --  in the timing diagrams. They are taken into account only when the
      --  EXTMOD bit in the FMC_BCRx register is 1.
      ACCMOD  : FMC_BTRx_BXR_ACCMOD_Field := 16#0#;
      --  DATAHLD
      DATAHLD : FMC_BTRx_BXR_DATAHLD_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FMC_BTRx_BXR_Register use record
      ADDSET  at 0 range 0 .. 3;
      ADDHLD  at 0 range 4 .. 7;
      DATAST  at 0 range 8 .. 15;
      BUSTURN at 0 range 16 .. 19;
      CLKDIV  at 0 range 20 .. 23;
      DATLAT  at 0 range 24 .. 27;
      ACCMOD  at 0 range 28 .. 29;
      DATAHLD at 0 range 30 .. 31;
   end record;

   type BXR_Cluster is record
      --  FMC_BCR1
      FMC_BCRx : aliased FMC_BCRx_BXR_Register;
      --  This register contains the control information of each memory bank,
      --  used for SRAMs, PSRAM and NOR Flash memories.If the EXTMOD bit is set
      --  in the FMC_BCRx register, then this register is partitioned for write
      --  and read access, that is, 2 registers are available: one to configure
      --  read accesses (this register) and one to configure write accesses
      --  (FMC_BWTRx registers).
      FMC_BTRx : aliased FMC_BTRx_BXR_Register;
   end record
     with Size => 64;

   for BXR_Cluster use record
      FMC_BCRx at 16#0# range 0 .. 31;
      FMC_BTRx at 16#4# range 0 .. 31;
   end record;

   type BXR_Clusters is array (1 .. 4) of BXR_Cluster;

   subtype PCSCNTR_CSCOUNT_Field is HAL.UInt16;

   --  PCSCNTR
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

   subtype FMC_PCR_PWID_Field is HAL.UInt2;
   subtype FMC_PCR_TCLR_Field is HAL.UInt4;
   subtype FMC_PCR_TAR_Field is HAL.UInt4;
   subtype FMC_PCR_ECCPS_Field is HAL.UInt3;

   --  NAND Flash control registers
   type FMC_PCR_Register is record
      --  unspecified
      Reserved_0_0   : HAL.Bit := 16#0#;
      --  Wait feature enable bit. This bit enables the Wait feature for the
      --  NAND Flash memory bank:
      PWAITEN        : Boolean := False;
      --  NAND Flash memory bank enable bit. This bit enables the memory bank.
      --  Accessing a disabled memory bank causes an ERROR on AXI bus
      PBKEN          : Boolean := False;
      --  Memory type
      PTYP           : Boolean := True;
      --  Data bus width. These bits define the external memory device width.
      PWID           : FMC_PCR_PWID_Field := 16#1#;
      --  ECC computation logic enable bit
      ECCEN          : Boolean := False;
      --  unspecified
      Reserved_7_8   : HAL.UInt2 := 16#0#;
      --  CLE to RE delay. These bits set time from CLE low to RE low in number
      --  of KCK_FMC clock cycles. The time is give by the following formula:
      --  t_clr = (TCLR + SET + 2) TKCK_FMC where TKCK_FMC is the KCK_FMC clock
      --  period Note: Set is MEMSET or ATTSET according to the addressed
      --  space.
      TCLR           : FMC_PCR_TCLR_Field := 16#0#;
      --  ALE to RE delay. These bits set time from ALE low to RE low in number
      --  of KCK_FMC clock cycles. Time is: t_ar = (TAR + SET + 2) TKCK_FMC
      --  where TKCK_FMC is the FMC clock period Note: Set is MEMSET or ATTSET
      --  according to the addressed space.
      TAR            : FMC_PCR_TAR_Field := 16#0#;
      --  ECC page size. These bits define the page size for the extended ECC:
      ECCPS          : FMC_PCR_ECCPS_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FMC_PCR_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      PWAITEN        at 0 range 1 .. 1;
      PBKEN          at 0 range 2 .. 2;
      PTYP           at 0 range 3 .. 3;
      PWID           at 0 range 4 .. 5;
      ECCEN          at 0 range 6 .. 6;
      Reserved_7_8   at 0 range 7 .. 8;
      TCLR           at 0 range 9 .. 12;
      TAR            at 0 range 13 .. 16;
      ECCPS          at 0 range 17 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  This register contains information about the FIFO status and interrupt.
   --  The FMC features a FIFO that is used when writing to memories to
   --  transfer up to 16 words of data.This is used to quickly write to the
   --  FIFO and free the AXI bus for transactions to peripherals other than the
   --  FMC, while the FMC is draining its FIFO into the memory. One of these
   --  register bits indicates the status of the FIFO, for ECC purposes.The ECC
   --  is calculated while the data are written to the memory. To read the
   --  correct ECC, the software must consequently wait until the FIFO is
   --  empty.
   type FMC_SR_Register is record
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

   for FMC_SR_Register use record
      IRS           at 0 range 0 .. 0;
      ILS           at 0 range 1 .. 1;
      IFS           at 0 range 2 .. 2;
      IREN          at 0 range 3 .. 3;
      ILEN          at 0 range 4 .. 4;
      IFEN          at 0 range 5 .. 5;
      FEMPT         at 0 range 6 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   subtype FMC_PMEM_MEMSET_Field is HAL.UInt8;
   subtype FMC_PMEM_MEMWAIT_Field is HAL.UInt8;
   subtype FMC_PMEM_MEMHOLD_Field is HAL.UInt8;
   subtype FMC_PMEM_MEMHIZ_Field is HAL.UInt8;

   --  The FMC_PMEM read/write register contains the timing information for
   --  NAND Flash memory bank. This information is used to access either the
   --  common memory space of the NAND Flash for command, address write access
   --  and data read/write access.
   type FMC_PMEM_Register is record
      --  Common memory x setup time These bits define the number of KCK_FMC
      --  (+1) clock cycles to set up the address before the command assertion
      --  (NWE, NOE), for NAND Flash read or write access to common memory
      --  space:
      MEMSET  : FMC_PMEM_MEMSET_Field := 16#FC#;
      --  Common memory wait time These bits define the minimum number of
      --  KCK_FMC (+1) clock cycles to assert the command (NWE, NOE), for NAND
      --  Flash read or write access to common memory space. The duration of
      --  command assertion is extended if the wait signal (NWAIT) is active
      --  (low) at the end of the programmed value of KCK_FMC:
      MEMWAIT : FMC_PMEM_MEMWAIT_Field := 16#FC#;
      --  Common memory hold time These bits define the number of KCK_FMC clock
      --  cycles for write accesses and KCK_FMC+1 clock cycles for read
      --  accesses during which the address is held (and data for write
      --  accesses) after the command is de-asserted (NWE, NOE), for NAND Flash
      --  read or write access to common memory space:
      MEMHOLD : FMC_PMEM_MEMHOLD_Field := 16#FC#;
      --  Common memory x data bus Hi-Z time These bits define the number of
      --  KCK_FMC clock cycles during which the data bus is kept Hi-Z after the
      --  start of a NAND Flash write access to common memory space. This is
      --  only valid for write transactions:
      MEMHIZ  : FMC_PMEM_MEMHIZ_Field := 16#FC#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FMC_PMEM_Register use record
      MEMSET  at 0 range 0 .. 7;
      MEMWAIT at 0 range 8 .. 15;
      MEMHOLD at 0 range 16 .. 23;
      MEMHIZ  at 0 range 24 .. 31;
   end record;

   subtype FMC_PATT_ATTSET_Field is HAL.UInt8;
   subtype FMC_PATT_ATTWAIT_Field is HAL.UInt8;
   subtype FMC_PATT_ATTHOLD_Field is HAL.UInt8;
   subtype FMC_PATT_ATTHIZ_Field is HAL.UInt8;

   --  The FMC_PATT read/write register contains the timing information for
   --  NAND Flash memory bank. It is used for 8-bit accesses to the attribute
   --  memory space of the NAND Flash for the last address write access if the
   --  timing must differ from that of previous accesses (for Ready/Busy
   --  management, refer to Section20.8.5: NAND Flash prewait feature).
   type FMC_PATT_Register is record
      --  Attribute memory setup time These bits define the number of KCK_FMC
      --  (+1) clock cycles to set up address before the command assertion
      --  (NWE, NOE), for NAND Flash read or write access to attribute memory
      --  space:
      ATTSET  : FMC_PATT_ATTSET_Field := 16#FC#;
      --  Attribute memory wait time These bits define the minimum number of x
      --  KCK_FMC (+1) clock cycles to assert the command (NWE, NOE), for NAND
      --  Flash read or write access to attribute memory space. The duration
      --  for command assertion is extended if the wait signal (NWAIT) is
      --  active (low) at the end of the programmed value of KCK_FMC:
      ATTWAIT : FMC_PATT_ATTWAIT_Field := 16#FC#;
      --  Attribute memory hold time These bits define the number of KCK_FMC
      --  clock cycles during which the address is held (and data for write
      --  access) after the command de-assertion (NWE, NOE), for NAND Flash
      --  read or write access to attribute memory space:
      ATTHOLD : FMC_PATT_ATTHOLD_Field := 16#FC#;
      --  Attribute memory data bus Hi-Z time These bits define the number of
      --  KCK_FMC clock cycles during which the data bus is kept in Hi-Z after
      --  the start of a NAND Flash write access to attribute memory space on
      --  socket. Only valid for writ transaction:
      ATTHIZ  : FMC_PATT_ATTHIZ_Field := 16#FC#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FMC_PATT_Register use record
      ATTSET  at 0 range 0 .. 7;
      ATTWAIT at 0 range 8 .. 15;
      ATTHOLD at 0 range 16 .. 23;
      ATTHIZ  at 0 range 24 .. 31;
   end record;

   ------------------------------
   -- BWTR cluster's Registers --
   ------------------------------

   subtype FMC_BWTRx_BWTR_ADDSET_Field is HAL.UInt4;
   subtype FMC_BWTRx_BWTR_ADDHLD_Field is HAL.UInt4;
   subtype FMC_BWTRx_BWTR_DATAST_Field is HAL.UInt8;
   subtype FMC_BWTRx_BWTR_BUSTURN_Field is HAL.UInt4;
   subtype FMC_BWTRx_BWTR_ACCMOD_Field is HAL.UInt2;

   --  This register contains the control information of each memory bank. It
   --  is used for SRAMs, PSRAMs and NOR Flash memories. When the EXTMOD bit is
   --  set in the FMC_BCRx register, then this register is active for write
   --  access.
   type FMC_BWTRx_BWTR_Register is record
      --  Address setup phase duration. These bits are written by software to
      --  define the duration of the address setup phase in KCK_FMC cycles
      --  (refer to Figure81 to Figure93), used in asynchronous accesses: ...
      --  Note: In synchronous accesses, this value is not used, the address
      --  setup phase is always 1 Flash clock period duration. In muxed mode,
      --  the minimum ADDSET value is 1.
      ADDSET         : FMC_BWTRx_BWTR_ADDSET_Field := 16#0#;
      --  Address-hold phase duration. These bits are written by software to
      --  define the duration of the address hold phase (refer to Figure81 to
      --  Figure93), used in asynchronous multiplexed accesses: ... Note: In
      --  synchronous NOR Flash accesses, this value is not used, the address
      --  hold phase is always 1 Flash clock period duration.
      ADDHLD         : FMC_BWTRx_BWTR_ADDHLD_Field := 16#0#;
      --  Data-phase duration. These bits are written by software to define the
      --  duration of the data phase (refer to Figure81 to Figure93), used in
      --  asynchronous SRAM, PSRAM and NOR Flash memory accesses:
      DATAST         : FMC_BWTRx_BWTR_DATAST_Field := 16#0#;
      --  Bus turnaround phase duration These bits are written by software to
      --  add a delay at the end of a write transaction to match the minimum
      --  time between consecutive transactions (tEHEL from ENx high to ENx
      --  low): (BUSTRUN + 1) KCK_FMC period &#8805; tEHELmin. The programmed
      --  bus turnaround delay is inserted between a an asynchronous write
      --  transfer and any other asynchronous /synchronous read or write
      --  transfer to or from a static bank. If a read operation is performed,
      --  the bank can be the same or a different one, whereas it must be
      --  different in case of write operation to the bank, except in muxed
      --  mode or mode D. In some cases, whatever the programmed BUSTRUN
      --  values, the bus turnaround delay is fixed as follows: The bus
      --  turnaround delay is not inserted between two consecutive asynchronous
      --  write transfers to the same static memory bank except for muxed mode
      --  and mode D. There is a bus turnaround delay of 2 FMC clock cycle
      --  between: Two consecutive synchronous write operations (in Burst or
      --  Single mode) to the same bank A synchronous write transfer ((in Burst
      --  or Single mode) and an asynchronous write or read transfer to or from
      --  static memory bank. There is a bus turnaround delay of 3 FMC clock
      --  cycle between: Two consecutive synchronous write operations (in Burst
      --  or Single mode) to different static banks. A synchronous write
      --  transfer (in Burst or Single mode) and a synchronous read from the
      --  same or a different bank. ...
      BUSTURN        : FMC_BWTRx_BWTR_BUSTURN_Field := 16#0#;
      --  unspecified
      Reserved_20_27 : HAL.UInt8 := 16#0#;
      --  Access mode. These bits specify the asynchronous access modes as
      --  shown in the next timing diagrams.These bits are taken into account
      --  only when the EXTMOD bit in the FMC_BCRx register is 1.
      ACCMOD         : FMC_BWTRx_BWTR_ACCMOD_Field := 16#0#;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FMC_BWTRx_BWTR_Register use record
      ADDSET         at 0 range 0 .. 3;
      ADDHLD         at 0 range 4 .. 7;
      DATAST         at 0 range 8 .. 15;
      BUSTURN        at 0 range 16 .. 19;
      Reserved_20_27 at 0 range 20 .. 27;
      ACCMOD         at 0 range 28 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   type BWTR_Cluster is record
      --  This register contains the control information of each memory bank.
      --  It is used for SRAMs, PSRAMs and NOR Flash memories. When the EXTMOD
      --  bit is set in the FMC_BCRx register, then this register is active for
      --  write access.
      FMC_BWTRx : aliased FMC_BWTRx_BWTR_Register;
   end record
     with Size => 64;

   for BWTR_Cluster use record
      FMC_BWTRx at 0 range 0 .. 31;
   end record;

   type BWTR_Clusters is array (1 .. 4) of BWTR_Cluster;

   -----------------------------
   -- BXR cluster's Registers --
   -----------------------------

   ------------------------------
   -- BWTR cluster's Registers --
   ------------------------------

   -----------------
   -- Peripherals --
   -----------------

   --  FMC
   type FMC_Peripheral is record
      BXR      : aliased BXR_Clusters;
      --  PCSCNTR
      PCSCNTR  : aliased PCSCNTR_Register;
      --  NAND Flash control registers
      FMC_PCR  : aliased FMC_PCR_Register;
      --  This register contains information about the FIFO status and
      --  interrupt. The FMC features a FIFO that is used when writing to
      --  memories to transfer up to 16 words of data.This is used to quickly
      --  write to the FIFO and free the AXI bus for transactions to
      --  peripherals other than the FMC, while the FMC is draining its FIFO
      --  into the memory. One of these register bits indicates the status of
      --  the FIFO, for ECC purposes.The ECC is calculated while the data are
      --  written to the memory. To read the correct ECC, the software must
      --  consequently wait until the FIFO is empty.
      FMC_SR   : aliased FMC_SR_Register;
      --  The FMC_PMEM read/write register contains the timing information for
      --  NAND Flash memory bank. This information is used to access either the
      --  common memory space of the NAND Flash for command, address write
      --  access and data read/write access.
      FMC_PMEM : aliased FMC_PMEM_Register;
      --  The FMC_PATT read/write register contains the timing information for
      --  NAND Flash memory bank. It is used for 8-bit accesses to the
      --  attribute memory space of the NAND Flash for the last address write
      --  access if the timing must differ from that of previous accesses (for
      --  Ready/Busy management, refer to Section20.8.5: NAND Flash prewait
      --  feature).
      FMC_PATT : aliased FMC_PATT_Register;
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
      FMC_ECCR : aliased HAL.UInt32;
      BWTR     : aliased BWTR_Clusters;
   end record
     with Volatile;

   for FMC_Peripheral use record
      BXR      at 16#0# range 0 .. 255;
      PCSCNTR  at 16#20# range 0 .. 31;
      FMC_PCR  at 16#80# range 0 .. 31;
      FMC_SR   at 16#84# range 0 .. 31;
      FMC_PMEM at 16#88# range 0 .. 31;
      FMC_PATT at 16#8C# range 0 .. 31;
      FMC_ECCR at 16#94# range 0 .. 31;
      BWTR     at 16#104# range 0 .. 255;
   end record;

   --  FMC
   FMC_Periph : aliased FMC_Peripheral
     with Import, Address => FMC_Base;

   --  FMC
   SEC_FMC_Periph : aliased FMC_Peripheral
     with Import, Address => SEC_FMC_Base;

end STM32_SVD.FMC;
