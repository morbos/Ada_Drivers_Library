--  This spec has been automatically generated from STM32U585.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.OTG_FS is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  The GOTGCTL register controls the behavior and reflects the status of
   --  the OTG function of the core.
   type GOTGCTL_Register is record
      --  Read-only. SRQSCS
      SRQSCS         : Boolean := False;
      --  SRQ
      SRQ            : Boolean := False;
      --  VBVALOEN
      VBVALOEN       : Boolean := False;
      --  VBVALOVAL
      VBVALOVAL      : Boolean := False;
      --  AVALOEN
      AVALOEN        : Boolean := False;
      --  AVALOVAL
      AVALOVAL       : Boolean := False;
      --  BVALOEN
      BVALOEN        : Boolean := False;
      --  BVALOVAL
      BVALOVAL       : Boolean := False;
      --  Read-only. HNGSCS
      HNGSCS         : Boolean := False;
      --  HNPRQ
      HNPRQ          : Boolean := False;
      --  HSHNPEN
      HSHNPEN        : Boolean := False;
      --  DHNPEN
      DHNPEN         : Boolean := False;
      --  EHEN
      EHEN           : Boolean := False;
      --  unspecified
      Reserved_13_15 : HAL.UInt3 := 16#0#;
      --  Read-only. CIDSTS
      CIDSTS         : Boolean := True;
      --  Read-only. DBCT
      DBCT           : Boolean := False;
      --  Read-only. ASVLD
      ASVLD          : Boolean := False;
      --  Read-only. BSVLD
      BSVLD          : Boolean := False;
      --  OTGVER
      OTGVER         : Boolean := False;
      --  Read-only. CURMOD
      CURMOD         : Boolean := False;
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GOTGCTL_Register use record
      SRQSCS         at 0 range 0 .. 0;
      SRQ            at 0 range 1 .. 1;
      VBVALOEN       at 0 range 2 .. 2;
      VBVALOVAL      at 0 range 3 .. 3;
      AVALOEN        at 0 range 4 .. 4;
      AVALOVAL       at 0 range 5 .. 5;
      BVALOEN        at 0 range 6 .. 6;
      BVALOVAL       at 0 range 7 .. 7;
      HNGSCS         at 0 range 8 .. 8;
      HNPRQ          at 0 range 9 .. 9;
      HSHNPEN        at 0 range 10 .. 10;
      DHNPEN         at 0 range 11 .. 11;
      EHEN           at 0 range 12 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      CIDSTS         at 0 range 16 .. 16;
      DBCT           at 0 range 17 .. 17;
      ASVLD          at 0 range 18 .. 18;
      BSVLD          at 0 range 19 .. 19;
      OTGVER         at 0 range 20 .. 20;
      CURMOD         at 0 range 21 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  The application reads this register whenever there is an OTG interrupt
   --  and clears the bits in this register to clear the OTG interrupt.
   type GOTGINT_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  SEDET
      SEDET          : Boolean := False;
      --  unspecified
      Reserved_3_7   : HAL.UInt5 := 16#0#;
      --  SRSSCHG
      SRSSCHG        : Boolean := False;
      --  HNSSCHG
      HNSSCHG        : Boolean := False;
      --  unspecified
      Reserved_10_16 : HAL.UInt7 := 16#0#;
      --  HNGDET
      HNGDET         : Boolean := False;
      --  ADTOCHG
      ADTOCHG        : Boolean := False;
      --  DBCDNE
      DBCDNE         : Boolean := False;
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GOTGINT_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      SEDET          at 0 range 2 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      SRSSCHG        at 0 range 8 .. 8;
      HNSSCHG        at 0 range 9 .. 9;
      Reserved_10_16 at 0 range 10 .. 16;
      HNGDET         at 0 range 17 .. 17;
      ADTOCHG        at 0 range 18 .. 18;
      DBCDNE         at 0 range 19 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  This register can be used to configure the core after power-on or a
   --  change in mode. This register mainly contains AHB system-related
   --  configuration parameters. Do not change this register after the initial
   --  programming. The application must program this register before starting
   --  any transactions on either the AHB or the USB.
   type GAHBCFG_Register is record
      --  GINTMSK
      GINTMSK       : Boolean := False;
      --  unspecified
      Reserved_1_6  : HAL.UInt6 := 16#0#;
      --  TXFELVL
      TXFELVL       : Boolean := False;
      --  PTXFELVL
      PTXFELVL      : Boolean := False;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GAHBCFG_Register use record
      GINTMSK       at 0 range 0 .. 0;
      Reserved_1_6  at 0 range 1 .. 6;
      TXFELVL       at 0 range 7 .. 7;
      PTXFELVL      at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   subtype GUSBCFG_TOCAL_Field is HAL.UInt3;
   subtype GUSBCFG_TRDT_Field is HAL.UInt4;

   --  This register can be used to configure the core after power-on or a
   --  changing to host mode or device mode. It contains USB and USB-PHY
   --  related configuration parameters. The application must program this
   --  register before starting any transactions on either the AHB or the USB.
   --  Do not make changes to this register after the initial programming.
   type GUSBCFG_Register is record
      --  TOCAL
      TOCAL          : GUSBCFG_TOCAL_Field := 16#0#;
      --  unspecified
      Reserved_3_5   : HAL.UInt3 := 16#0#;
      --  Read-only. PHYSEL
      PHYSEL         : Boolean := True;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  SRPCAP
      SRPCAP         : Boolean := False;
      --  HNPCAP
      HNPCAP         : Boolean := False;
      --  TRDT
      TRDT           : GUSBCFG_TRDT_Field := 16#5#;
      --  unspecified
      Reserved_14_28 : HAL.UInt15 := 16#0#;
      --  FHMOD
      FHMOD          : Boolean := False;
      --  FDMOD
      FDMOD          : Boolean := False;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GUSBCFG_Register use record
      TOCAL          at 0 range 0 .. 2;
      Reserved_3_5   at 0 range 3 .. 5;
      PHYSEL         at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SRPCAP         at 0 range 8 .. 8;
      HNPCAP         at 0 range 9 .. 9;
      TRDT           at 0 range 10 .. 13;
      Reserved_14_28 at 0 range 14 .. 28;
      FHMOD          at 0 range 29 .. 29;
      FDMOD          at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype GRSTCTL_TXFNUM_Field is HAL.UInt5;

   --  The application uses this register to reset various hardware features
   --  inside the core.
   type GRSTCTL_Register is record
      --  Read-only. CSRST
      CSRST          : Boolean := False;
      --  PSRST
      PSRST          : Boolean := False;
      --  FSRST
      FSRST          : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  RXFFLSH
      RXFFLSH        : Boolean := False;
      --  TXFFLSH
      TXFFLSH        : Boolean := False;
      --  TXFNUM
      TXFNUM         : GRSTCTL_TXFNUM_Field := 16#0#;
      --  unspecified
      Reserved_11_30 : HAL.UInt20 := 16#0#;
      --  Read-only. AHBIDL
      AHBIDL         : Boolean := True;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GRSTCTL_Register use record
      CSRST          at 0 range 0 .. 0;
      PSRST          at 0 range 1 .. 1;
      FSRST          at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      RXFFLSH        at 0 range 4 .. 4;
      TXFFLSH        at 0 range 5 .. 5;
      TXFNUM         at 0 range 6 .. 10;
      Reserved_11_30 at 0 range 11 .. 30;
      AHBIDL         at 0 range 31 .. 31;
   end record;

   --  This register interrupts the application for system-level events in the
   --  current mode (device mode or host mode). Some of the bits in this
   --  register are valid only in host mode, while others are valid in device
   --  mode only. This register also indicates the current mode. To clear the
   --  interrupt status bits of the rc_w1 type, the application must write 1
   --  into the bit. The FIFO status interrupts are read-only; once software
   --  reads from or writes to the FIFO while servicing these interrupts, FIFO
   --  interrupt conditions are cleared automatically. The application must
   --  clear the GINTSTS register at initialization before unmasking the
   --  interrupt bit to avoid any interrupts generated prior to initialization.
   type GINTSTS_Register is record
      --  Read-only. CMOD
      CMOD           : Boolean := False;
      --  MMIS
      MMIS           : Boolean := False;
      --  Read-only. OTGINT
      OTGINT         : Boolean := False;
      --  SOF
      SOF            : Boolean := False;
      --  Read-only. RXFLVL
      RXFLVL         : Boolean := False;
      --  Read-only. NPTXFE
      NPTXFE         : Boolean := True;
      --  Read-only. GINAKEFF
      GINAKEFF       : Boolean := False;
      --  Read-only. GONAKEFF
      GONAKEFF       : Boolean := False;
      --  unspecified
      Reserved_8_9   : HAL.UInt2 := 16#0#;
      --  ESUSP
      ESUSP          : Boolean := False;
      --  USBSUSP
      USBSUSP        : Boolean := False;
      --  USBRST
      USBRST         : Boolean := False;
      --  ENUMDNE
      ENUMDNE        : Boolean := False;
      --  ISOODRP
      ISOODRP        : Boolean := False;
      --  EOPF
      EOPF           : Boolean := False;
      --  unspecified
      Reserved_16_17 : HAL.UInt2 := 16#0#;
      --  Read-only. IEPINT
      IEPINT         : Boolean := False;
      --  Read-only. OEPINT
      OEPINT         : Boolean := False;
      --  IISOIXFR
      IISOIXFR       : Boolean := False;
      --  IPXFR
      IPXFR          : Boolean := False;
      --  unspecified
      Reserved_22_22 : HAL.Bit := 16#0#;
      --  RSTDET
      RSTDET         : Boolean := False;
      --  Read-only. HPRTINT
      HPRTINT        : Boolean := False;
      --  Read-only. HCINT
      HCINT          : Boolean := False;
      --  Read-only. PTXFE
      PTXFE          : Boolean := True;
      --  LPMINT
      LPMINT         : Boolean := False;
      --  CIDSCHG
      CIDSCHG        : Boolean := False;
      --  DISCINT
      DISCINT        : Boolean := False;
      --  SRQINT
      SRQINT         : Boolean := False;
      --  WKUPINT
      WKUPINT        : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GINTSTS_Register use record
      CMOD           at 0 range 0 .. 0;
      MMIS           at 0 range 1 .. 1;
      OTGINT         at 0 range 2 .. 2;
      SOF            at 0 range 3 .. 3;
      RXFLVL         at 0 range 4 .. 4;
      NPTXFE         at 0 range 5 .. 5;
      GINAKEFF       at 0 range 6 .. 6;
      GONAKEFF       at 0 range 7 .. 7;
      Reserved_8_9   at 0 range 8 .. 9;
      ESUSP          at 0 range 10 .. 10;
      USBSUSP        at 0 range 11 .. 11;
      USBRST         at 0 range 12 .. 12;
      ENUMDNE        at 0 range 13 .. 13;
      ISOODRP        at 0 range 14 .. 14;
      EOPF           at 0 range 15 .. 15;
      Reserved_16_17 at 0 range 16 .. 17;
      IEPINT         at 0 range 18 .. 18;
      OEPINT         at 0 range 19 .. 19;
      IISOIXFR       at 0 range 20 .. 20;
      IPXFR          at 0 range 21 .. 21;
      Reserved_22_22 at 0 range 22 .. 22;
      RSTDET         at 0 range 23 .. 23;
      HPRTINT        at 0 range 24 .. 24;
      HCINT          at 0 range 25 .. 25;
      PTXFE          at 0 range 26 .. 26;
      LPMINT         at 0 range 27 .. 27;
      CIDSCHG        at 0 range 28 .. 28;
      DISCINT        at 0 range 29 .. 29;
      SRQINT         at 0 range 30 .. 30;
      WKUPINT        at 0 range 31 .. 31;
   end record;

   --  This register works with the core interrupt register to interrupt the
   --  application. When an interrupt bit is masked, the interrupt associated
   --  with that bit is not generated. However, the core interrupt (GINTSTS)
   --  register bit corresponding to that interrupt is still set.
   type GINTMSK_Register is record
      --  unspecified
      Reserved_0_0   : HAL.Bit := 16#0#;
      --  MMISM
      MMISM          : Boolean := False;
      --  OTGINT
      OTGINT         : Boolean := False;
      --  SOFM
      SOFM           : Boolean := False;
      --  RXFLVLM
      RXFLVLM        : Boolean := False;
      --  NPTXFEM
      NPTXFEM        : Boolean := False;
      --  GINAKEFFM
      GINAKEFFM      : Boolean := False;
      --  GONAKEFFM
      GONAKEFFM      : Boolean := False;
      --  unspecified
      Reserved_8_9   : HAL.UInt2 := 16#0#;
      --  ESUSPM
      ESUSPM         : Boolean := False;
      --  USBSUSPM
      USBSUSPM       : Boolean := False;
      --  USBRST
      USBRST         : Boolean := False;
      --  ENUMDNEM
      ENUMDNEM       : Boolean := False;
      --  ISOODRPM
      ISOODRPM       : Boolean := False;
      --  EOPFM
      EOPFM          : Boolean := False;
      --  unspecified
      Reserved_16_17 : HAL.UInt2 := 16#0#;
      --  IEPINT
      IEPINT         : Boolean := False;
      --  OEPINT
      OEPINT         : Boolean := False;
      --  IISOIXFRM
      IISOIXFRM      : Boolean := False;
      --  IPXFRM
      IPXFRM         : Boolean := False;
      --  unspecified
      Reserved_22_22 : HAL.Bit := 16#0#;
      --  RSTDETM
      RSTDETM        : Boolean := False;
      --  PRTIM
      PRTIM          : Boolean := False;
      --  HCIM
      HCIM           : Boolean := False;
      --  PTXFEM
      PTXFEM         : Boolean := False;
      --  LPMINTM
      LPMINTM        : Boolean := False;
      --  CIDSCHGM
      CIDSCHGM       : Boolean := False;
      --  DISCINT
      DISCINT        : Boolean := False;
      --  SRQIM
      SRQIM          : Boolean := False;
      --  WUIM
      WUIM           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GINTMSK_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      MMISM          at 0 range 1 .. 1;
      OTGINT         at 0 range 2 .. 2;
      SOFM           at 0 range 3 .. 3;
      RXFLVLM        at 0 range 4 .. 4;
      NPTXFEM        at 0 range 5 .. 5;
      GINAKEFFM      at 0 range 6 .. 6;
      GONAKEFFM      at 0 range 7 .. 7;
      Reserved_8_9   at 0 range 8 .. 9;
      ESUSPM         at 0 range 10 .. 10;
      USBSUSPM       at 0 range 11 .. 11;
      USBRST         at 0 range 12 .. 12;
      ENUMDNEM       at 0 range 13 .. 13;
      ISOODRPM       at 0 range 14 .. 14;
      EOPFM          at 0 range 15 .. 15;
      Reserved_16_17 at 0 range 16 .. 17;
      IEPINT         at 0 range 18 .. 18;
      OEPINT         at 0 range 19 .. 19;
      IISOIXFRM      at 0 range 20 .. 20;
      IPXFRM         at 0 range 21 .. 21;
      Reserved_22_22 at 0 range 22 .. 22;
      RSTDETM        at 0 range 23 .. 23;
      PRTIM          at 0 range 24 .. 24;
      HCIM           at 0 range 25 .. 25;
      PTXFEM         at 0 range 26 .. 26;
      LPMINTM        at 0 range 27 .. 27;
      CIDSCHGM       at 0 range 28 .. 28;
      DISCINT        at 0 range 29 .. 29;
      SRQIM          at 0 range 30 .. 30;
      WUIM           at 0 range 31 .. 31;
   end record;

   subtype GRXSTSR_DEVICE_EPNUM_Field is HAL.UInt4;
   subtype GRXSTSR_DEVICE_BCNT_Field is HAL.UInt11;
   subtype GRXSTSR_DEVICE_DPID_Field is HAL.UInt2;
   subtype GRXSTSR_DEVICE_PKTSTS_Field is HAL.UInt4;
   subtype GRXSTSR_DEVICE_FRMNUM_Field is HAL.UInt4;

   --  This description is for register GRXSTSR in Device mode. A read to the
   --  receive status debug read register returns the contents of the top of
   --  the receive FIFO. The core ignores the receive status read when the
   --  receive FIFO is empty and returns a value of 0x00000000.
   type GRXSTSR_DEVICE_Register is record
      --  Read-only. EPNUM
      EPNUM          : GRXSTSR_DEVICE_EPNUM_Field;
      --  Read-only. BCNT
      BCNT           : GRXSTSR_DEVICE_BCNT_Field;
      --  Read-only. DPID
      DPID           : GRXSTSR_DEVICE_DPID_Field;
      --  Read-only. PKTSTS
      PKTSTS         : GRXSTSR_DEVICE_PKTSTS_Field;
      --  Read-only. FRMNUM
      FRMNUM         : GRXSTSR_DEVICE_FRMNUM_Field;
      --  unspecified
      Reserved_25_26 : HAL.UInt2;
      --  Read-only. STSPHST
      STSPHST        : Boolean;
      --  unspecified
      Reserved_28_31 : HAL.UInt4;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GRXSTSR_DEVICE_Register use record
      EPNUM          at 0 range 0 .. 3;
      BCNT           at 0 range 4 .. 14;
      DPID           at 0 range 15 .. 16;
      PKTSTS         at 0 range 17 .. 20;
      FRMNUM         at 0 range 21 .. 24;
      Reserved_25_26 at 0 range 25 .. 26;
      STSPHST        at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype GRXSTSR_HOST_CHNUM_Field is HAL.UInt4;
   subtype GRXSTSR_HOST_BCNT_Field is HAL.UInt11;
   subtype GRXSTSR_HOST_DPID_Field is HAL.UInt2;
   subtype GRXSTSR_HOST_PKTSTS_Field is HAL.UInt4;

   --  This description is for register GRXSTSR in Host mode
   type GRXSTSR_HOST_Register is record
      --  Read-only. CHNUM
      CHNUM          : GRXSTSR_HOST_CHNUM_Field;
      --  Read-only. BCNT
      BCNT           : GRXSTSR_HOST_BCNT_Field;
      --  Read-only. DPID
      DPID           : GRXSTSR_HOST_DPID_Field;
      --  Read-only. PKTSTS
      PKTSTS         : GRXSTSR_HOST_PKTSTS_Field;
      --  unspecified
      Reserved_21_31 : HAL.UInt11;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GRXSTSR_HOST_Register use record
      CHNUM          at 0 range 0 .. 3;
      BCNT           at 0 range 4 .. 14;
      DPID           at 0 range 15 .. 16;
      PKTSTS         at 0 range 17 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   subtype GRXSTSP_DEVICE_EPNUM_Field is HAL.UInt4;
   subtype GRXSTSP_DEVICE_BCNT_Field is HAL.UInt11;
   subtype GRXSTSP_DEVICE_DPID_Field is HAL.UInt2;
   subtype GRXSTSP_DEVICE_PKTSTS_Field is HAL.UInt4;
   subtype GRXSTSP_DEVICE_FRMNUM_Field is HAL.UInt4;

   --  This description is for register GRXSTSP in Device mode. Similarly to
   --  GRXSTSR (receive status debug read register) where a read returns the
   --  contents of the top of the receive FIFO, a read to GRXSTSP (receive
   --  status read and pop register) additionally pops the top data entry out
   --  of the Rx FIFO. The core ignores the receive status pop/read when the
   --  receive FIFO is empty and returns a value of 0x00000000. The application
   --  must only pop the receive status FIFO when the receive FIFO non-empty
   --  bit of the core interrupt register (RXFLVL bit in GINTSTS) is asserted.
   type GRXSTSP_DEVICE_Register is record
      --  Read-only. EPNUM
      EPNUM          : GRXSTSP_DEVICE_EPNUM_Field;
      --  Read-only. BCNT
      BCNT           : GRXSTSP_DEVICE_BCNT_Field;
      --  Read-only. DPID
      DPID           : GRXSTSP_DEVICE_DPID_Field;
      --  Read-only. PKTSTS
      PKTSTS         : GRXSTSP_DEVICE_PKTSTS_Field;
      --  Read-only. FRMNUM
      FRMNUM         : GRXSTSP_DEVICE_FRMNUM_Field;
      --  unspecified
      Reserved_25_26 : HAL.UInt2;
      --  Read-only. STSPHST
      STSPHST        : Boolean;
      --  unspecified
      Reserved_28_31 : HAL.UInt4;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GRXSTSP_DEVICE_Register use record
      EPNUM          at 0 range 0 .. 3;
      BCNT           at 0 range 4 .. 14;
      DPID           at 0 range 15 .. 16;
      PKTSTS         at 0 range 17 .. 20;
      FRMNUM         at 0 range 21 .. 24;
      Reserved_25_26 at 0 range 25 .. 26;
      STSPHST        at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype GRXSTSP_HOST_CHNUM_Field is HAL.UInt4;
   subtype GRXSTSP_HOST_BCNT_Field is HAL.UInt11;
   subtype GRXSTSP_HOST_DPID_Field is HAL.UInt2;
   subtype GRXSTSP_HOST_PKTSTS_Field is HAL.UInt4;

   --  This description is for register GRXSTSP in HOST mode
   type GRXSTSP_HOST_Register is record
      --  Read-only. CHNUM
      CHNUM          : GRXSTSP_HOST_CHNUM_Field;
      --  Read-only. BCNT
      BCNT           : GRXSTSP_HOST_BCNT_Field;
      --  Read-only. DPID
      DPID           : GRXSTSP_HOST_DPID_Field;
      --  Read-only. PKTSTS
      PKTSTS         : GRXSTSP_HOST_PKTSTS_Field;
      --  unspecified
      Reserved_21_31 : HAL.UInt11;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GRXSTSP_HOST_Register use record
      CHNUM          at 0 range 0 .. 3;
      BCNT           at 0 range 4 .. 14;
      DPID           at 0 range 15 .. 16;
      PKTSTS         at 0 range 17 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   subtype GRXFSIZ_RXFD_Field is HAL.UInt16;

   --  The application can program the RAM size that must be allocated to the
   --  Rx FIFO.
   type GRXFSIZ_Register is record
      --  RXFD
      RXFD           : GRXFSIZ_RXFD_Field := 16#200#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GRXFSIZ_Register use record
      RXFD           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype HNPTXFSIZ_NPTXFSA_Field is HAL.UInt16;
   subtype HNPTXFSIZ_NPTXFD_Field is HAL.UInt16;

   --  Host mode
   type HNPTXFSIZ_Register is record
      --  NPTXFSA
      NPTXFSA : HNPTXFSIZ_NPTXFSA_Field := 16#200#;
      --  NPTXFD
      NPTXFD  : HNPTXFSIZ_NPTXFD_Field := 16#200#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for HNPTXFSIZ_Register use record
      NPTXFSA at 0 range 0 .. 15;
      NPTXFD  at 0 range 16 .. 31;
   end record;

   subtype HNPTXSTS_NPTXFSAV_Field is HAL.UInt16;
   subtype HNPTXSTS_NPTQXSAV_Field is HAL.UInt8;
   subtype HNPTXSTS_NPTXQTOP_Field is HAL.UInt7;

   --  In device mode, this register is not valid. This read-only register
   --  contains the free space information for the non-periodic Tx FIFO and the
   --  non-periodic transmit request queue.
   type HNPTXSTS_Register is record
      --  Read-only. NPTXFSAV
      NPTXFSAV       : HNPTXSTS_NPTXFSAV_Field;
      --  Read-only. NPTQXSAV
      NPTQXSAV       : HNPTXSTS_NPTQXSAV_Field;
      --  Read-only. NPTXQTOP
      NPTXQTOP       : HNPTXSTS_NPTXQTOP_Field;
      --  unspecified
      Reserved_31_31 : HAL.Bit;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for HNPTXSTS_Register use record
      NPTXFSAV       at 0 range 0 .. 15;
      NPTQXSAV       at 0 range 16 .. 23;
      NPTXQTOP       at 0 range 24 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   --  OTG general core configuration register
   type GCCFG_Register is record
      --  Read-only. DCDET
      DCDET          : Boolean := False;
      --  Read-only. PDET
      PDET           : Boolean := False;
      --  Read-only. SDET
      SDET           : Boolean := False;
      --  Read-only. PS2DET
      PS2DET         : Boolean := False;
      --  unspecified
      Reserved_4_15  : HAL.UInt12 := 16#0#;
      --  PWRDWN
      PWRDWN         : Boolean := False;
      --  BCDEN
      BCDEN          : Boolean := False;
      --  DCDEN
      DCDEN          : Boolean := False;
      --  PDEN
      PDEN           : Boolean := False;
      --  SDEN
      SDEN           : Boolean := False;
      --  VBDEN
      VBDEN          : Boolean := False;
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GCCFG_Register use record
      DCDET          at 0 range 0 .. 0;
      PDET           at 0 range 1 .. 1;
      SDET           at 0 range 2 .. 2;
      PS2DET         at 0 range 3 .. 3;
      Reserved_4_15  at 0 range 4 .. 15;
      PWRDWN         at 0 range 16 .. 16;
      BCDEN          at 0 range 17 .. 17;
      DCDEN          at 0 range 18 .. 18;
      PDEN           at 0 range 19 .. 19;
      SDEN           at 0 range 20 .. 20;
      VBDEN          at 0 range 21 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   subtype GLPMCFG_BESL_Field is HAL.UInt4;
   subtype GLPMCFG_BESLTHRS_Field is HAL.UInt4;
   subtype GLPMCFG_LPMRSP_Field is HAL.UInt2;
   subtype GLPMCFG_LPMCHIDX_Field is HAL.UInt4;
   subtype GLPMCFG_LPMRCNT_Field is HAL.UInt3;
   subtype GLPMCFG_LPMRCNTSTS_Field is HAL.UInt3;

   --  OTG core LPM configuration register
   type GLPMCFG_Register is record
      --  LPMEN
      LPMEN          : Boolean := False;
      --  LPMACK
      LPMACK         : Boolean := False;
      --  BESL
      BESL           : GLPMCFG_BESL_Field := 16#0#;
      --  REMWAKE
      REMWAKE        : Boolean := False;
      --  L1SSEN
      L1SSEN         : Boolean := False;
      --  BESLTHRS
      BESLTHRS       : GLPMCFG_BESLTHRS_Field := 16#0#;
      --  L1DSEN
      L1DSEN         : Boolean := False;
      --  Read-only. LPMRSP
      LPMRSP         : GLPMCFG_LPMRSP_Field := 16#0#;
      --  Read-only. SLPSTS
      SLPSTS         : Boolean := False;
      --  Read-only. L1RSMOK
      L1RSMOK        : Boolean := False;
      --  LPMCHIDX
      LPMCHIDX       : GLPMCFG_LPMCHIDX_Field := 16#0#;
      --  LPMRCNT
      LPMRCNT        : GLPMCFG_LPMRCNT_Field := 16#0#;
      --  SNDLPM
      SNDLPM         : Boolean := False;
      --  Read-only. LPMRCNTSTS
      LPMRCNTSTS     : GLPMCFG_LPMRCNTSTS_Field := 16#0#;
      --  ENBESL
      ENBESL         : Boolean := False;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GLPMCFG_Register use record
      LPMEN          at 0 range 0 .. 0;
      LPMACK         at 0 range 1 .. 1;
      BESL           at 0 range 2 .. 5;
      REMWAKE        at 0 range 6 .. 6;
      L1SSEN         at 0 range 7 .. 7;
      BESLTHRS       at 0 range 8 .. 11;
      L1DSEN         at 0 range 12 .. 12;
      LPMRSP         at 0 range 13 .. 14;
      SLPSTS         at 0 range 15 .. 15;
      L1RSMOK        at 0 range 16 .. 16;
      LPMCHIDX       at 0 range 17 .. 20;
      LPMRCNT        at 0 range 21 .. 23;
      SNDLPM         at 0 range 24 .. 24;
      LPMRCNTSTS     at 0 range 25 .. 27;
      ENBESL         at 0 range 28 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype HPTXFSIZ_PTXSA_Field is HAL.UInt16;
   subtype HPTXFSIZ_PTXFSIZ_Field is HAL.UInt16;

   --  OTG host periodic transmit FIFO size register
   type HPTXFSIZ_Register is record
      --  PTXSA
      PTXSA   : HPTXFSIZ_PTXSA_Field := 16#400#;
      --  PTXFSIZ
      PTXFSIZ : HPTXFSIZ_PTXFSIZ_Field := 16#200#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for HPTXFSIZ_Register use record
      PTXSA   at 0 range 0 .. 15;
      PTXFSIZ at 0 range 16 .. 31;
   end record;

   subtype DIEPTXF_INEPTXSA_Field is HAL.UInt16;
   subtype DIEPTXF_INEPTXFD_Field is HAL.UInt16;

   --  OTG device IN endpoint transmit FIFO 1 size register
   type DIEPTXF_Register is record
      --  INEPTXSA
      INEPTXSA : DIEPTXF_INEPTXSA_Field := 16#400#;
      --  INEPTXFD
      INEPTXFD : DIEPTXF_INEPTXFD_Field := 16#200#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DIEPTXF_Register use record
      INEPTXSA at 0 range 0 .. 15;
      INEPTXFD at 0 range 16 .. 31;
   end record;

   subtype HCFG_FSLSPCS_Field is HAL.UInt2;

   --  This register configures the core after power-on. Do not make changes to
   --  this register after initializing the host.
   type HCFG_Register is record
      --  FSLSPCS
      FSLSPCS       : HCFG_FSLSPCS_Field := 16#0#;
      --  Read-only. FSLSS
      FSLSS         : Boolean := False;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for HCFG_Register use record
      FSLSPCS       at 0 range 0 .. 1;
      FSLSS         at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype HFIR_FRIVL_Field is HAL.UInt16;

   --  This register stores the frame interval information for the current
   --  speed to which the OTG controller has enumerated.
   type HFIR_Register is record
      --  FRIVL
      FRIVL          : HFIR_FRIVL_Field := 16#EA60#;
      --  RLDCTRL
      RLDCTRL        : Boolean := False;
      --  unspecified
      Reserved_17_31 : HAL.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for HFIR_Register use record
      FRIVL          at 0 range 0 .. 15;
      RLDCTRL        at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype HFNUM_FRNUM_Field is HAL.UInt16;
   subtype HFNUM_FTREM_Field is HAL.UInt16;

   --  This register indicates the current frame number. It also indicates the
   --  time remaining (in terms of the number of PHY clocks) in the current
   --  frame.
   type HFNUM_Register is record
      --  Read-only. FRNUM
      FRNUM : HFNUM_FRNUM_Field;
      --  Read-only. FTREM
      FTREM : HFNUM_FTREM_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for HFNUM_Register use record
      FRNUM at 0 range 0 .. 15;
      FTREM at 0 range 16 .. 31;
   end record;

   subtype HPTXSTS_PTXFSAVL_Field is HAL.UInt16;
   subtype HPTXSTS_PTXQSAV_Field is HAL.UInt8;
   subtype HPTXSTS_PTXQTOP_Field is HAL.UInt8;

   --  This read-only register contains the free space information for the
   --  periodic Tx FIFO and the periodic transmit request queue.
   type HPTXSTS_Register is record
      --  Read-only. PTXFSAVL
      PTXFSAVL : HPTXSTS_PTXFSAVL_Field;
      --  Read-only. PTXQSAV
      PTXQSAV  : HPTXSTS_PTXQSAV_Field;
      --  Read-only. PTXQTOP
      PTXQTOP  : HPTXSTS_PTXQTOP_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for HPTXSTS_Register use record
      PTXFSAVL at 0 range 0 .. 15;
      PTXQSAV  at 0 range 16 .. 23;
      PTXQTOP  at 0 range 24 .. 31;
   end record;

   subtype HAINT_HAINT_Field is HAL.UInt16;

   --  When a significant event occurs on a channel, the host all channels
   --  interrupt register interrupts the application using the host channels
   --  interrupt bit of the core interrupt register (HCINT bit in GINTSTS).
   --  This is shown in Figure724. There is one interrupt bit per channel, up
   --  to a maximum of 16 bits. Bits in this register are set and cleared when
   --  the application sets and clears bits in the corresponding host channel-x
   --  interrupt register.
   type HAINT_Register is record
      --  Read-only. HAINT
      HAINT          : HAINT_HAINT_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for HAINT_Register use record
      HAINT          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype HAINTMSK_HAINTM_Field is HAL.UInt16;

   --  The host all channel interrupt mask register works with the host all
   --  channel interrupt register to interrupt the application when an event
   --  occurs on a channel. There is one interrupt mask bit per channel, up to
   --  a maximum of 16 bits.
   type HAINTMSK_Register is record
      --  HAINTM
      HAINTM         : HAINTMSK_HAINTM_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for HAINTMSK_Register use record
      HAINTM         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype HPRT_PLSTS_Field is HAL.UInt2;
   subtype HPRT_PTCTL_Field is HAL.UInt4;
   subtype HPRT_PSPD_Field is HAL.UInt2;

   --  This register is available only in host mode. Currently, the OTG host
   --  supports only one port. A single register holds USB port-related
   --  information such as USB reset, enable, suspend, resume, connect status,
   --  and test mode for each port. It is shown in Figure724. The rc_w1 bits in
   --  this register can trigger an interrupt to the application through the
   --  host port interrupt bit of the core interrupt register (HPRTINT bit in
   --  GINTSTS). On a port interrupt, the application must read this register
   --  and clear the bit that caused the interrupt. For the rc_w1 bits, the
   --  application must write a 1 to the bit to clear the interrupt.
   type HPRT_Register is record
      --  Read-only. PCSTS
      PCSTS          : Boolean := False;
      --  PCDET
      PCDET          : Boolean := False;
      --  PENA
      PENA           : Boolean := False;
      --  PENCHNG
      PENCHNG        : Boolean := False;
      --  Read-only. POCA
      POCA           : Boolean := False;
      --  POCCHNG
      POCCHNG        : Boolean := False;
      --  PRES
      PRES           : Boolean := False;
      --  PSUSP
      PSUSP          : Boolean := False;
      --  PRST
      PRST           : Boolean := False;
      --  unspecified
      Reserved_9_9   : HAL.Bit := 16#0#;
      --  Read-only. PLSTS
      PLSTS          : HPRT_PLSTS_Field := 16#0#;
      --  PPWR
      PPWR           : Boolean := False;
      --  PTCTL
      PTCTL          : HPRT_PTCTL_Field := 16#0#;
      --  Read-only. PSPD
      PSPD           : HPRT_PSPD_Field := 16#0#;
      --  unspecified
      Reserved_19_31 : HAL.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for HPRT_Register use record
      PCSTS          at 0 range 0 .. 0;
      PCDET          at 0 range 1 .. 1;
      PENA           at 0 range 2 .. 2;
      PENCHNG        at 0 range 3 .. 3;
      POCA           at 0 range 4 .. 4;
      POCCHNG        at 0 range 5 .. 5;
      PRES           at 0 range 6 .. 6;
      PSUSP          at 0 range 7 .. 7;
      PRST           at 0 range 8 .. 8;
      Reserved_9_9   at 0 range 9 .. 9;
      PLSTS          at 0 range 10 .. 11;
      PPWR           at 0 range 12 .. 12;
      PTCTL          at 0 range 13 .. 16;
      PSPD           at 0 range 17 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   subtype HCCHAR_MPSIZ_Field is HAL.UInt11;
   subtype HCCHAR_EPNUM_Field is HAL.UInt4;
   subtype HCCHAR_EPTYP_Field is HAL.UInt2;
   subtype HCCHAR_MCNT_Field is HAL.UInt2;
   subtype HCCHAR_DAD_Field is HAL.UInt7;

   --  OTG host channel 0 characteristics register
   type HCCHAR_Register is record
      --  MPSIZ
      MPSIZ          : HCCHAR_MPSIZ_Field := 16#0#;
      --  EPNUM
      EPNUM          : HCCHAR_EPNUM_Field := 16#0#;
      --  EPDIR
      EPDIR          : Boolean := False;
      --  unspecified
      Reserved_16_16 : HAL.Bit := 16#0#;
      --  LSDEV
      LSDEV          : Boolean := False;
      --  EPTYP
      EPTYP          : HCCHAR_EPTYP_Field := 16#0#;
      --  MCNT
      MCNT           : HCCHAR_MCNT_Field := 16#0#;
      --  DAD
      DAD            : HCCHAR_DAD_Field := 16#0#;
      --  ODDFRM
      ODDFRM         : Boolean := False;
      --  CHDIS
      CHDIS          : Boolean := False;
      --  CHENA
      CHENA          : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for HCCHAR_Register use record
      MPSIZ          at 0 range 0 .. 10;
      EPNUM          at 0 range 11 .. 14;
      EPDIR          at 0 range 15 .. 15;
      Reserved_16_16 at 0 range 16 .. 16;
      LSDEV          at 0 range 17 .. 17;
      EPTYP          at 0 range 18 .. 19;
      MCNT           at 0 range 20 .. 21;
      DAD            at 0 range 22 .. 28;
      ODDFRM         at 0 range 29 .. 29;
      CHDIS          at 0 range 30 .. 30;
      CHENA          at 0 range 31 .. 31;
   end record;

   --  This register indicates the status of a channel with respect to USB- and
   --  AHB-related events. It is shown in Figure724. The application must read
   --  this register when the host channels interrupt bit in the core interrupt
   --  register (HCINT bit in GINTSTS) is set. Before the application can read
   --  this register, it must first read the host all channels interrupt
   --  (HAINT) register to get the exact channel number for the host channel-x
   --  interrupt register. The application must clear the appropriate bit in
   --  this register to clear the corresponding bits in the HAINT and GINTSTS
   --  registers.
   type HCINT_Register is record
      --  XFRC
      XFRC           : Boolean := False;
      --  CHH
      CHH            : Boolean := False;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  STALL
      STALL          : Boolean := False;
      --  NAK
      NAK            : Boolean := False;
      --  ACK
      ACK            : Boolean := False;
      --  unspecified
      Reserved_6_6   : HAL.Bit := 16#0#;
      --  TXERR
      TXERR          : Boolean := False;
      --  BBERR
      BBERR          : Boolean := False;
      --  FRMOR
      FRMOR          : Boolean := False;
      --  DTERR
      DTERR          : Boolean := False;
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for HCINT_Register use record
      XFRC           at 0 range 0 .. 0;
      CHH            at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      STALL          at 0 range 3 .. 3;
      NAK            at 0 range 4 .. 4;
      ACK            at 0 range 5 .. 5;
      Reserved_6_6   at 0 range 6 .. 6;
      TXERR          at 0 range 7 .. 7;
      BBERR          at 0 range 8 .. 8;
      FRMOR          at 0 range 9 .. 9;
      DTERR          at 0 range 10 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   --  This register reflects the mask for each channel status described in the
   --  previous section.
   type HCINTMSK_Register is record
      --  XFRCM
      XFRCM          : Boolean := False;
      --  CHHM
      CHHM           : Boolean := False;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  STALLM
      STALLM         : Boolean := False;
      --  NAKM
      NAKM           : Boolean := False;
      --  ACKM
      ACKM           : Boolean := False;
      --  unspecified
      Reserved_6_6   : HAL.Bit := 16#0#;
      --  TXERRM
      TXERRM         : Boolean := False;
      --  BBERRM
      BBERRM         : Boolean := False;
      --  FRMORM
      FRMORM         : Boolean := False;
      --  DTERRM
      DTERRM         : Boolean := False;
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for HCINTMSK_Register use record
      XFRCM          at 0 range 0 .. 0;
      CHHM           at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      STALLM         at 0 range 3 .. 3;
      NAKM           at 0 range 4 .. 4;
      ACKM           at 0 range 5 .. 5;
      Reserved_6_6   at 0 range 6 .. 6;
      TXERRM         at 0 range 7 .. 7;
      BBERRM         at 0 range 8 .. 8;
      FRMORM         at 0 range 9 .. 9;
      DTERRM         at 0 range 10 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   subtype HCTSIZ_XFRSIZ_Field is HAL.UInt19;
   subtype HCTSIZ_PKTCNT_Field is HAL.UInt10;
   subtype HCTSIZ_DPID_Field is HAL.UInt2;

   --  OTG host channel 0 transfer size register
   type HCTSIZ_Register is record
      --  XFRSIZ
      XFRSIZ : HCTSIZ_XFRSIZ_Field := 16#0#;
      --  PKTCNT
      PKTCNT : HCTSIZ_PKTCNT_Field := 16#0#;
      --  DPID
      DPID   : HCTSIZ_DPID_Field := 16#0#;
      --  DOPNG
      DOPNG  : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for HCTSIZ_Register use record
      XFRSIZ at 0 range 0 .. 18;
      PKTCNT at 0 range 19 .. 28;
      DPID   at 0 range 29 .. 30;
      DOPNG  at 0 range 31 .. 31;
   end record;

   subtype DCFG_DSPD_Field is HAL.UInt2;
   subtype DCFG_DAD_Field is HAL.UInt7;
   subtype DCFG_PFIVL_Field is HAL.UInt2;

   --  This register configures the core in device mode after power-on or after
   --  certain control commands or enumeration. Do not make changes to this
   --  register after initial programming.
   type DCFG_Register is record
      --  DSPD
      DSPD           : DCFG_DSPD_Field := 16#0#;
      --  NZLSOHSK
      NZLSOHSK       : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  DAD
      DAD            : DCFG_DAD_Field := 16#0#;
      --  PFIVL
      PFIVL          : DCFG_PFIVL_Field := 16#0#;
      --  unspecified
      Reserved_13_14 : HAL.UInt2 := 16#0#;
      --  ERRATIM
      ERRATIM        : Boolean := False;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#220#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DCFG_Register use record
      DSPD           at 0 range 0 .. 1;
      NZLSOHSK       at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      DAD            at 0 range 4 .. 10;
      PFIVL          at 0 range 11 .. 12;
      Reserved_13_14 at 0 range 13 .. 14;
      ERRATIM        at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DCTL_TCTL_Field is HAL.UInt3;

   --  OTG device control register
   type DCTL_Register is record
      --  RWUSIG
      RWUSIG         : Boolean := False;
      --  SDIS
      SDIS           : Boolean := True;
      --  Read-only. GINSTS
      GINSTS         : Boolean := False;
      --  Read-only. GONSTS
      GONSTS         : Boolean := False;
      --  TCTL
      TCTL           : DCTL_TCTL_Field := 16#0#;
      --  Write-only. SGINAK
      SGINAK         : Boolean := False;
      --  Write-only. CGINAK
      CGINAK         : Boolean := False;
      --  Write-only. SGONAK
      SGONAK         : Boolean := False;
      --  Write-only. CGONAK
      CGONAK         : Boolean := False;
      --  POPRGDNE
      POPRGDNE       : Boolean := False;
      --  unspecified
      Reserved_12_17 : HAL.UInt6 := 16#0#;
      --  DSBESLRJCT
      DSBESLRJCT     : Boolean := False;
      --  unspecified
      Reserved_19_31 : HAL.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DCTL_Register use record
      RWUSIG         at 0 range 0 .. 0;
      SDIS           at 0 range 1 .. 1;
      GINSTS         at 0 range 2 .. 2;
      GONSTS         at 0 range 3 .. 3;
      TCTL           at 0 range 4 .. 6;
      SGINAK         at 0 range 7 .. 7;
      CGINAK         at 0 range 8 .. 8;
      SGONAK         at 0 range 9 .. 9;
      CGONAK         at 0 range 10 .. 10;
      POPRGDNE       at 0 range 11 .. 11;
      Reserved_12_17 at 0 range 12 .. 17;
      DSBESLRJCT     at 0 range 18 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   subtype DSTS_ENUMSPD_Field is HAL.UInt2;
   subtype DSTS_FNSOF_Field is HAL.UInt14;
   subtype DSTS_DEVLNSTS_Field is HAL.UInt2;

   --  This register indicates the status of the core with respect to
   --  USB-related events. It must be read on interrupts from the device all
   --  interrupts (DAINT) register.
   type DSTS_Register is record
      --  Read-only. SUSPSTS
      SUSPSTS        : Boolean;
      --  Read-only. ENUMSPD
      ENUMSPD        : DSTS_ENUMSPD_Field;
      --  Read-only. EERR
      EERR           : Boolean;
      --  unspecified
      Reserved_4_7   : HAL.UInt4;
      --  Read-only. FNSOF
      FNSOF          : DSTS_FNSOF_Field;
      --  Read-only. DEVLNSTS
      DEVLNSTS       : DSTS_DEVLNSTS_Field;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSTS_Register use record
      SUSPSTS        at 0 range 0 .. 0;
      ENUMSPD        at 0 range 1 .. 2;
      EERR           at 0 range 3 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      FNSOF          at 0 range 8 .. 21;
      DEVLNSTS       at 0 range 22 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  This register works with each of the DIEPINTx registers for all
   --  endpoints to generate an interrupt per IN endpoint. The IN endpoint
   --  interrupt for a specific status in the DIEPINTx register can be masked
   --  by writing to the corresponding bit in this register. Status bits are
   --  masked by default.
   type DIEPMSK_Register is record
      --  XFRCM
      XFRCM          : Boolean := False;
      --  EPDM
      EPDM           : Boolean := False;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  TOM
      TOM            : Boolean := False;
      --  ITTXFEMSK
      ITTXFEMSK      : Boolean := False;
      --  INEPNMM
      INEPNMM        : Boolean := False;
      --  INEPNEM
      INEPNEM        : Boolean := False;
      --  unspecified
      Reserved_7_12  : HAL.UInt6 := 16#0#;
      --  NAKM
      NAKM           : Boolean := False;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DIEPMSK_Register use record
      XFRCM          at 0 range 0 .. 0;
      EPDM           at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      TOM            at 0 range 3 .. 3;
      ITTXFEMSK      at 0 range 4 .. 4;
      INEPNMM        at 0 range 5 .. 5;
      INEPNEM        at 0 range 6 .. 6;
      Reserved_7_12  at 0 range 7 .. 12;
      NAKM           at 0 range 13 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   --  This register works with each of the DOEPINTx registers for all
   --  endpoints to generate an interrupt per OUT endpoint. The OUT endpoint
   --  interrupt for a specific status in the DOEPINTx register can be masked
   --  by writing into the corresponding bit in this register. Status bits are
   --  masked by default.
   type DOEPMSK_Register is record
      --  XFRCM
      XFRCM          : Boolean := False;
      --  EPDM
      EPDM           : Boolean := False;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  STUPM
      STUPM          : Boolean := False;
      --  OTEPDM
      OTEPDM         : Boolean := False;
      --  STSPHSRXM
      STSPHSRXM      : Boolean := False;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  OUTPKTERRM
      OUTPKTERRM     : Boolean := False;
      --  unspecified
      Reserved_9_11  : HAL.UInt3 := 16#0#;
      --  BERRM
      BERRM          : Boolean := False;
      --  NAKMSK
      NAKMSK         : Boolean := False;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOEPMSK_Register use record
      XFRCM          at 0 range 0 .. 0;
      EPDM           at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      STUPM          at 0 range 3 .. 3;
      OTEPDM         at 0 range 4 .. 4;
      STSPHSRXM      at 0 range 5 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      OUTPKTERRM     at 0 range 8 .. 8;
      Reserved_9_11  at 0 range 9 .. 11;
      BERRM          at 0 range 12 .. 12;
      NAKMSK         at 0 range 13 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   subtype DAINT_IEPINT_Field is HAL.UInt16;
   subtype DAINT_OEPINT_Field is HAL.UInt16;

   --  When a significant event occurs on an endpoint, a DAINT register
   --  interrupts the application using the device OUT endpoints interrupt bit
   --  or device IN endpoints interrupt bit of the GINTSTS register (OEPINT or
   --  IEPINT in GINTSTS, respectively). There is one interrupt bit per
   --  endpoint, up to a maximum of 16 bits for OUT endpoints and 16 bits for
   --  IN endpoints. For a bidirectional endpoint, the corresponding IN and OUT
   --  interrupt bits are used. Bits in this register are set and cleared when
   --  the application sets and clears bits in the corresponding device
   --  endpoint-x interrupt register (DIEPINTx/DOEPINTx).
   type DAINT_Register is record
      --  Read-only. IEPINT
      IEPINT : DAINT_IEPINT_Field;
      --  Read-only. OEPINT
      OEPINT : DAINT_OEPINT_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DAINT_Register use record
      IEPINT at 0 range 0 .. 15;
      OEPINT at 0 range 16 .. 31;
   end record;

   subtype DAINTMSK_IEPM_Field is HAL.UInt16;
   subtype DAINTMSK_OEPM_Field is HAL.UInt16;

   --  The DAINTMSK register works with the device endpoint interrupt register
   --  to interrupt the application when an event occurs on a device endpoint.
   --  However, the DAINT register bit corresponding to that interrupt is still
   --  set.
   type DAINTMSK_Register is record
      --  IEPM
      IEPM : DAINTMSK_IEPM_Field := 16#0#;
      --  OEPM
      OEPM : DAINTMSK_OEPM_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DAINTMSK_Register use record
      IEPM at 0 range 0 .. 15;
      OEPM at 0 range 16 .. 31;
   end record;

   subtype DVBUSDIS_VBUSDT_Field is HAL.UInt16;

   --  This register specifies the VBUS discharge time after VBUS pulsing
   --  during SRP.
   type DVBUSDIS_Register is record
      --  VBUSDT
      VBUSDT         : DVBUSDIS_VBUSDT_Field := 16#17D7#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DVBUSDIS_Register use record
      VBUSDT         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DVBUSPULSE_DVBUSP_Field is HAL.UInt16;

   --  This register specifies the VBUS pulsing time during SRP.
   type DVBUSPULSE_Register is record
      --  DVBUSP
      DVBUSP         : DVBUSPULSE_DVBUSP_Field := 16#5B8#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DVBUSPULSE_Register use record
      DVBUSP         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DIEPEMPMSK_INEPTXFEM_Field is HAL.UInt16;

   --  This register is used to control the IN endpoint FIFO empty interrupt
   --  generation (TXFE_DIEPINTx).
   type DIEPEMPMSK_Register is record
      --  INEPTXFEM
      INEPTXFEM      : DIEPEMPMSK_INEPTXFEM_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DIEPEMPMSK_Register use record
      INEPTXFEM      at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DIEPCTL0_MPSIZ_Field is HAL.UInt2;
   subtype DIEPCTL0_EPTYP_Field is HAL.UInt2;
   subtype DIEPCTL0_TXFNUM_Field is HAL.UInt4;

   --  The application uses this register to control the behavior of each
   --  logical endpoint other than endpoint 0.
   type DIEPCTL0_Register is record
      --  MPSIZ
      MPSIZ          : DIEPCTL0_MPSIZ_Field := 16#0#;
      --  unspecified
      Reserved_2_14  : HAL.UInt13 := 16#0#;
      --  Read-only. USBAEP
      USBAEP         : Boolean := False;
      --  unspecified
      Reserved_16_16 : HAL.Bit := 16#0#;
      --  Read-only. NAKSTS
      NAKSTS         : Boolean := False;
      --  EPTYP
      EPTYP          : DIEPCTL0_EPTYP_Field := 16#0#;
      --  unspecified
      Reserved_20_20 : HAL.Bit := 16#0#;
      --  STALL
      STALL          : Boolean := False;
      --  TXFNUM
      TXFNUM         : DIEPCTL0_TXFNUM_Field := 16#0#;
      --  Write-only. CNAK
      CNAK           : Boolean := False;
      --  Write-only. SNAK
      SNAK           : Boolean := False;
      --  unspecified
      Reserved_28_29 : HAL.UInt2 := 16#0#;
      --  EPDIS
      EPDIS          : Boolean := False;
      --  EPENA
      EPENA          : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DIEPCTL0_Register use record
      MPSIZ          at 0 range 0 .. 1;
      Reserved_2_14  at 0 range 2 .. 14;
      USBAEP         at 0 range 15 .. 15;
      Reserved_16_16 at 0 range 16 .. 16;
      NAKSTS         at 0 range 17 .. 17;
      EPTYP          at 0 range 18 .. 19;
      Reserved_20_20 at 0 range 20 .. 20;
      STALL          at 0 range 21 .. 21;
      TXFNUM         at 0 range 22 .. 25;
      CNAK           at 0 range 26 .. 26;
      SNAK           at 0 range 27 .. 27;
      Reserved_28_29 at 0 range 28 .. 29;
      EPDIS          at 0 range 30 .. 30;
      EPENA          at 0 range 31 .. 31;
   end record;

   --  This register indicates the status of an endpoint with respect to USB-
   --  and AHB-related events. It is shown in Figure724. The application must
   --  read this register when the IN endpoints interrupt bit of the core
   --  interrupt register (IEPINT in GINTSTS) is set. Before the application
   --  can read this register, it must first read the device all endpoints
   --  interrupt (DAINT) register to get the exact endpoint number for the
   --  device endpoint-x interrupt register. The application must clear the
   --  appropriate bit in this register to clear the corresponding bits in the
   --  DAINT and GINTSTS registers.
   type DIEPINT_Register is record
      --  XFRC
      XFRC           : Boolean := False;
      --  EPDISD
      EPDISD         : Boolean := False;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  TOC
      TOC            : Boolean := False;
      --  ITTXFE
      ITTXFE         : Boolean := False;
      --  INEPNM
      INEPNM         : Boolean := False;
      --  Read-only. INEPNE
      INEPNE         : Boolean := False;
      --  Read-only. TXFE
      TXFE           : Boolean := True;
      --  unspecified
      Reserved_8_10  : HAL.UInt3 := 16#0#;
      --  PKTDRPSTS
      PKTDRPSTS      : Boolean := False;
      --  unspecified
      Reserved_12_12 : HAL.Bit := 16#0#;
      --  NAK
      NAK            : Boolean := False;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DIEPINT_Register use record
      XFRC           at 0 range 0 .. 0;
      EPDISD         at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      TOC            at 0 range 3 .. 3;
      ITTXFE         at 0 range 4 .. 4;
      INEPNM         at 0 range 5 .. 5;
      INEPNE         at 0 range 6 .. 6;
      TXFE           at 0 range 7 .. 7;
      Reserved_8_10  at 0 range 8 .. 10;
      PKTDRPSTS      at 0 range 11 .. 11;
      Reserved_12_12 at 0 range 12 .. 12;
      NAK            at 0 range 13 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   subtype DIEPTSIZ0_XFRSIZ_Field is HAL.UInt7;
   subtype DIEPTSIZ0_PKTCNT_Field is HAL.UInt2;

   --  The application must modify this register before enabling endpoint 0.
   type DIEPTSIZ0_Register is record
      --  XFRSIZ
      XFRSIZ         : DIEPTSIZ0_XFRSIZ_Field := 16#0#;
      --  unspecified
      Reserved_7_18  : HAL.UInt12 := 16#0#;
      --  PKTCNT
      PKTCNT         : DIEPTSIZ0_PKTCNT_Field := 16#0#;
      --  unspecified
      Reserved_21_31 : HAL.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DIEPTSIZ0_Register use record
      XFRSIZ         at 0 range 0 .. 6;
      Reserved_7_18  at 0 range 7 .. 18;
      PKTCNT         at 0 range 19 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   subtype DTXFSTS_INEPTFSAV_Field is HAL.UInt16;

   --  This read-only register contains the free space information for the
   --  device IN endpoint Tx FIFO.
   type DTXFSTS_Register is record
      --  Read-only. INEPTFSAV
      INEPTFSAV      : DTXFSTS_INEPTFSAV_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DTXFSTS_Register use record
      INEPTFSAV      at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DIEPCTL_MPSIZ_Field is HAL.UInt11;
   subtype DIEPCTL_EPTYP_Field is HAL.UInt2;
   subtype DIEPCTL_TXFNUM_Field is HAL.UInt4;

   --  The application uses this register to control the behavior of each
   --  logical endpoint other than endpoint 0.
   type DIEPCTL_Register is record
      --  MPSIZ
      MPSIZ          : DIEPCTL_MPSIZ_Field := 16#0#;
      --  unspecified
      Reserved_11_14 : HAL.UInt4 := 16#0#;
      --  USBAEP
      USBAEP         : Boolean := False;
      --  Read-only. EONUM_DPIP
      EONUM_DPIP     : Boolean := False;
      --  Read-only. NAKSTS
      NAKSTS         : Boolean := False;
      --  EPTYP
      EPTYP          : DIEPCTL_EPTYP_Field := 16#0#;
      --  unspecified
      Reserved_20_20 : HAL.Bit := 16#0#;
      --  STALL
      STALL          : Boolean := False;
      --  TXFNUM
      TXFNUM         : DIEPCTL_TXFNUM_Field := 16#0#;
      --  Write-only. CNAK
      CNAK           : Boolean := False;
      --  Write-only. SNAK
      SNAK           : Boolean := False;
      --  Write-only. SD0PID_SEVNFRM
      SD0PID_SEVNFRM : Boolean := False;
      --  Write-only. SODDFRM
      SODDFRM        : Boolean := False;
      --  EPDIS
      EPDIS          : Boolean := False;
      --  EPENA
      EPENA          : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DIEPCTL_Register use record
      MPSIZ          at 0 range 0 .. 10;
      Reserved_11_14 at 0 range 11 .. 14;
      USBAEP         at 0 range 15 .. 15;
      EONUM_DPIP     at 0 range 16 .. 16;
      NAKSTS         at 0 range 17 .. 17;
      EPTYP          at 0 range 18 .. 19;
      Reserved_20_20 at 0 range 20 .. 20;
      STALL          at 0 range 21 .. 21;
      TXFNUM         at 0 range 22 .. 25;
      CNAK           at 0 range 26 .. 26;
      SNAK           at 0 range 27 .. 27;
      SD0PID_SEVNFRM at 0 range 28 .. 28;
      SODDFRM        at 0 range 29 .. 29;
      EPDIS          at 0 range 30 .. 30;
      EPENA          at 0 range 31 .. 31;
   end record;

   subtype DIEPTSIZ_XFRSIZ_Field is HAL.UInt19;
   subtype DIEPTSIZ_PKTCNT_Field is HAL.UInt10;
   subtype DIEPTSIZ_MCNT_Field is HAL.UInt2;

   --  The application must modify this register before enabling the endpoint.
   --  Once the endpoint is enabled using the endpoint enable bit in the
   --  DIEPCTLx registers (EPENA bit in DIEPCTLx), the core modifies this
   --  register. The application can only read this register once the core has
   --  cleared the endpoint enable bit.
   type DIEPTSIZ_Register is record
      --  XFRSIZ
      XFRSIZ         : DIEPTSIZ_XFRSIZ_Field := 16#0#;
      --  PKTCNT
      PKTCNT         : DIEPTSIZ_PKTCNT_Field := 16#0#;
      --  MCNT
      MCNT           : DIEPTSIZ_MCNT_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DIEPTSIZ_Register use record
      XFRSIZ         at 0 range 0 .. 18;
      PKTCNT         at 0 range 19 .. 28;
      MCNT           at 0 range 29 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype DOEPCTL0_MPSIZ_Field is HAL.UInt2;
   subtype DOEPCTL0_EPTYP_Field is HAL.UInt2;

   --  This section describes the DOEPCTL0 register.
   type DOEPCTL0_Register is record
      --  Read-only. MPSIZ
      MPSIZ          : DOEPCTL0_MPSIZ_Field := 16#0#;
      --  unspecified
      Reserved_2_14  : HAL.UInt13 := 16#0#;
      --  Read-only. USBAEP
      USBAEP         : Boolean := True;
      --  unspecified
      Reserved_16_16 : HAL.Bit := 16#0#;
      --  Read-only. NAKSTS
      NAKSTS         : Boolean := False;
      --  Read-only. EPTYP
      EPTYP          : DOEPCTL0_EPTYP_Field := 16#0#;
      --  SNPM
      SNPM           : Boolean := False;
      --  STALL
      STALL          : Boolean := False;
      --  unspecified
      Reserved_22_25 : HAL.UInt4 := 16#0#;
      --  Write-only. CNAK
      CNAK           : Boolean := False;
      --  Write-only. SNAK
      SNAK           : Boolean := False;
      --  unspecified
      Reserved_28_29 : HAL.UInt2 := 16#0#;
      --  Read-only. EPDIS
      EPDIS          : Boolean := False;
      --  Write-only. EPENA
      EPENA          : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOEPCTL0_Register use record
      MPSIZ          at 0 range 0 .. 1;
      Reserved_2_14  at 0 range 2 .. 14;
      USBAEP         at 0 range 15 .. 15;
      Reserved_16_16 at 0 range 16 .. 16;
      NAKSTS         at 0 range 17 .. 17;
      EPTYP          at 0 range 18 .. 19;
      SNPM           at 0 range 20 .. 20;
      STALL          at 0 range 21 .. 21;
      Reserved_22_25 at 0 range 22 .. 25;
      CNAK           at 0 range 26 .. 26;
      SNAK           at 0 range 27 .. 27;
      Reserved_28_29 at 0 range 28 .. 29;
      EPDIS          at 0 range 30 .. 30;
      EPENA          at 0 range 31 .. 31;
   end record;

   --  This register indicates the status of an endpoint with respect to USB-
   --  and AHB-related events. It is shown in Figure724. The application must
   --  read this register when the OUT endpoints interrupt bit of the GINTSTS
   --  register (OEPINT bit in GINTSTS) is set. Before the application can read
   --  this register, it must first read the DAINT register to get the exact
   --  endpoint number for the DOEPINTx register. The application must clear
   --  the appropriate bit in this register to clear the corresponding bits in
   --  the DAINT and GINTSTS registers.
   type DOEPINT_Register is record
      --  XFRC
      XFRC           : Boolean := False;
      --  EPDISD
      EPDISD         : Boolean := False;
      --  AHBERR
      AHBERR         : Boolean := False;
      --  STUP
      STUP           : Boolean := False;
      --  OTEPDIS
      OTEPDIS        : Boolean := False;
      --  STSPHSRX
      STSPHSRX       : Boolean := False;
      --  B2BSTUP
      B2BSTUP        : Boolean := False;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#1#;
      --  OUTPKTERR
      OUTPKTERR      : Boolean := False;
      --  BNA
      BNA            : Boolean := False;
      --  unspecified
      Reserved_10_11 : HAL.UInt2 := 16#0#;
      --  BERR
      BERR           : Boolean := False;
      --  NAK
      NAK            : Boolean := False;
      --  NYET
      NYET           : Boolean := False;
      --  STPKTRX
      STPKTRX        : Boolean := False;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOEPINT_Register use record
      XFRC           at 0 range 0 .. 0;
      EPDISD         at 0 range 1 .. 1;
      AHBERR         at 0 range 2 .. 2;
      STUP           at 0 range 3 .. 3;
      OTEPDIS        at 0 range 4 .. 4;
      STSPHSRX       at 0 range 5 .. 5;
      B2BSTUP        at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      OUTPKTERR      at 0 range 8 .. 8;
      BNA            at 0 range 9 .. 9;
      Reserved_10_11 at 0 range 10 .. 11;
      BERR           at 0 range 12 .. 12;
      NAK            at 0 range 13 .. 13;
      NYET           at 0 range 14 .. 14;
      STPKTRX        at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DOEPTSIZ0_XFRSIZ_Field is HAL.UInt7;
   subtype DOEPTSIZ0_STUPCNT_Field is HAL.UInt2;

   --  The application must modify this register before enabling endpoint 0.
   type DOEPTSIZ0_Register is record
      --  XFRSIZ
      XFRSIZ         : DOEPTSIZ0_XFRSIZ_Field := 16#0#;
      --  unspecified
      Reserved_7_18  : HAL.UInt12 := 16#0#;
      --  PKTCNT
      PKTCNT         : Boolean := False;
      --  unspecified
      Reserved_20_28 : HAL.UInt9 := 16#0#;
      --  STUPCNT
      STUPCNT        : DOEPTSIZ0_STUPCNT_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOEPTSIZ0_Register use record
      XFRSIZ         at 0 range 0 .. 6;
      Reserved_7_18  at 0 range 7 .. 18;
      PKTCNT         at 0 range 19 .. 19;
      Reserved_20_28 at 0 range 20 .. 28;
      STUPCNT        at 0 range 29 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype DOEPCTL_MPSIZ_Field is HAL.UInt11;
   subtype DOEPCTL_EPTYP_Field is HAL.UInt2;

   --  The application uses this register to control the behavior of each
   --  logical endpoint other than endpoint 0.
   type DOEPCTL_Register is record
      --  MPSIZ
      MPSIZ          : DOEPCTL_MPSIZ_Field := 16#0#;
      --  unspecified
      Reserved_11_14 : HAL.UInt4 := 16#0#;
      --  USBAEP
      USBAEP         : Boolean := False;
      --  Read-only. EONUM_DPIP
      EONUM_DPIP     : Boolean := False;
      --  Read-only. NAKSTS
      NAKSTS         : Boolean := False;
      --  EPTYP
      EPTYP          : DOEPCTL_EPTYP_Field := 16#0#;
      --  SNPM
      SNPM           : Boolean := False;
      --  STALL
      STALL          : Boolean := False;
      --  unspecified
      Reserved_22_25 : HAL.UInt4 := 16#0#;
      --  Write-only. CNAK
      CNAK           : Boolean := False;
      --  Write-only. SNAK
      SNAK           : Boolean := False;
      --  Write-only. SD0PID_SEVNFRM
      SD0PID_SEVNFRM : Boolean := False;
      --  Write-only. SD1PID_SODDFRM
      SD1PID_SODDFRM : Boolean := False;
      --  EPDIS
      EPDIS          : Boolean := False;
      --  EPENA
      EPENA          : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOEPCTL_Register use record
      MPSIZ          at 0 range 0 .. 10;
      Reserved_11_14 at 0 range 11 .. 14;
      USBAEP         at 0 range 15 .. 15;
      EONUM_DPIP     at 0 range 16 .. 16;
      NAKSTS         at 0 range 17 .. 17;
      EPTYP          at 0 range 18 .. 19;
      SNPM           at 0 range 20 .. 20;
      STALL          at 0 range 21 .. 21;
      Reserved_22_25 at 0 range 22 .. 25;
      CNAK           at 0 range 26 .. 26;
      SNAK           at 0 range 27 .. 27;
      SD0PID_SEVNFRM at 0 range 28 .. 28;
      SD1PID_SODDFRM at 0 range 29 .. 29;
      EPDIS          at 0 range 30 .. 30;
      EPENA          at 0 range 31 .. 31;
   end record;

   subtype DOEPTSIZ_XFRSIZ_Field is HAL.UInt19;
   subtype DOEPTSIZ_PKTCNT_Field is HAL.UInt10;
   subtype DOEPTSIZ_RXDPID_STUPCNT_Field is HAL.UInt2;

   --  The application must modify this register before enabling the endpoint.
   --  Once the endpoint is enabled using endpoint enable bit of the DOEPCTLx
   --  registers (EPENA bit in DOEPCTLx), the core modifies this register. The
   --  application can only read this register once the core has cleared the
   --  endpoint enable bit.
   type DOEPTSIZ_Register is record
      --  XFRSIZ
      XFRSIZ         : DOEPTSIZ_XFRSIZ_Field := 16#0#;
      --  PKTCNT
      PKTCNT         : DOEPTSIZ_PKTCNT_Field := 16#0#;
      --  RXDPID_STUPCNT
      RXDPID_STUPCNT : DOEPTSIZ_RXDPID_STUPCNT_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOEPTSIZ_Register use record
      XFRSIZ         at 0 range 0 .. 18;
      PKTCNT         at 0 range 19 .. 28;
      RXDPID_STUPCNT at 0 range 29 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   --  This register is available in host and device modes.
   type PCGCCTL_Register is record
      --  STPPCLK
      STPPCLK       : Boolean := False;
      --  GATEHCLK
      GATEHCLK      : Boolean := False;
      --  unspecified
      Reserved_2_3  : HAL.UInt2 := 16#0#;
      --  Read-only. PHYSUSP
      PHYSUSP       : Boolean := False;
      --  ENL1GTG
      ENL1GTG       : Boolean := False;
      --  Read-only. PHYSLEEP
      PHYSLEEP      : Boolean := False;
      --  Read-only. SUSP
      SUSP          : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#200B80#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PCGCCTL_Register use record
      STPPCLK       at 0 range 0 .. 0;
      GATEHCLK      at 0 range 1 .. 1;
      Reserved_2_3  at 0 range 2 .. 3;
      PHYSUSP       at 0 range 4 .. 4;
      ENL1GTG       at 0 range 5 .. 5;
      PHYSLEEP      at 0 range 6 .. 6;
      SUSP          at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   type OTG_FS_Disc is
     (Device,
      Host);

   --  OTG_FS
   type OTG_FS_Peripheral
     (Discriminent : OTG_FS_Disc := Device)
   is record
      --  The GOTGCTL register controls the behavior and reflects the status of
      --  the OTG function of the core.
      GOTGCTL        : aliased GOTGCTL_Register;
      --  The application reads this register whenever there is an OTG
      --  interrupt and clears the bits in this register to clear the OTG
      --  interrupt.
      GOTGINT        : aliased GOTGINT_Register;
      --  This register can be used to configure the core after power-on or a
      --  change in mode. This register mainly contains AHB system-related
      --  configuration parameters. Do not change this register after the
      --  initial programming. The application must program this register
      --  before starting any transactions on either the AHB or the USB.
      GAHBCFG        : aliased GAHBCFG_Register;
      --  This register can be used to configure the core after power-on or a
      --  changing to host mode or device mode. It contains USB and USB-PHY
      --  related configuration parameters. The application must program this
      --  register before starting any transactions on either the AHB or the
      --  USB. Do not make changes to this register after the initial
      --  programming.
      GUSBCFG        : aliased GUSBCFG_Register;
      --  The application uses this register to reset various hardware features
      --  inside the core.
      GRSTCTL        : aliased GRSTCTL_Register;
      --  This register interrupts the application for system-level events in
      --  the current mode (device mode or host mode). Some of the bits in this
      --  register are valid only in host mode, while others are valid in
      --  device mode only. This register also indicates the current mode. To
      --  clear the interrupt status bits of the rc_w1 type, the application
      --  must write 1 into the bit. The FIFO status interrupts are read-only;
      --  once software reads from or writes to the FIFO while servicing these
      --  interrupts, FIFO interrupt conditions are cleared automatically. The
      --  application must clear the GINTSTS register at initialization before
      --  unmasking the interrupt bit to avoid any interrupts generated prior
      --  to initialization.
      GINTSTS        : aliased GINTSTS_Register;
      --  This register works with the core interrupt register to interrupt the
      --  application. When an interrupt bit is masked, the interrupt
      --  associated with that bit is not generated. However, the core
      --  interrupt (GINTSTS) register bit corresponding to that interrupt is
      --  still set.
      GINTMSK        : aliased GINTMSK_Register;
      --  The application can program the RAM size that must be allocated to
      --  the Rx FIFO.
      GRXFSIZ        : aliased GRXFSIZ_Register;
      --  Host mode
      HNPTXFSIZ      : aliased HNPTXFSIZ_Register;
      --  In device mode, this register is not valid. This read-only register
      --  contains the free space information for the non-periodic Tx FIFO and
      --  the non-periodic transmit request queue.
      HNPTXSTS       : aliased HNPTXSTS_Register;
      --  OTG general core configuration register
      GCCFG          : aliased GCCFG_Register;
      --  This is a register containing the Product ID as reset value.
      CID            : aliased HAL.UInt32;
      --  OTG core LPM configuration register
      GLPMCFG        : aliased GLPMCFG_Register;
      --  OTG host periodic transmit FIFO size register
      HPTXFSIZ       : aliased HPTXFSIZ_Register;
      --  OTG device IN endpoint transmit FIFO 1 size register
      DIEPTXF1       : aliased DIEPTXF_Register;
      --  OTG device IN endpoint transmit FIFO 2 size register
      DIEPTXF2       : aliased DIEPTXF_Register;
      --  OTG device IN endpoint transmit FIFO 3 size register
      DIEPTXF3       : aliased DIEPTXF_Register;
      --  OTG device IN endpoint transmit FIFO 4 size register
      DIEPTXF4       : aliased DIEPTXF_Register;
      --  OTG device IN endpoint transmit FIFO 5 size register
      DIEPTXF5       : aliased DIEPTXF_Register;
      --  This register configures the core after power-on. Do not make changes
      --  to this register after initializing the host.
      HCFG           : aliased HCFG_Register;
      --  This register stores the frame interval information for the current
      --  speed to which the OTG controller has enumerated.
      HFIR           : aliased HFIR_Register;
      --  This register indicates the current frame number. It also indicates
      --  the time remaining (in terms of the number of PHY clocks) in the
      --  current frame.
      HFNUM          : aliased HFNUM_Register;
      --  This read-only register contains the free space information for the
      --  periodic Tx FIFO and the periodic transmit request queue.
      HPTXSTS        : aliased HPTXSTS_Register;
      --  When a significant event occurs on a channel, the host all channels
      --  interrupt register interrupts the application using the host channels
      --  interrupt bit of the core interrupt register (HCINT bit in GINTSTS).
      --  This is shown in Figure724. There is one interrupt bit per channel,
      --  up to a maximum of 16 bits. Bits in this register are set and cleared
      --  when the application sets and clears bits in the corresponding host
      --  channel-x interrupt register.
      HAINT          : aliased HAINT_Register;
      --  The host all channel interrupt mask register works with the host all
      --  channel interrupt register to interrupt the application when an event
      --  occurs on a channel. There is one interrupt mask bit per channel, up
      --  to a maximum of 16 bits.
      HAINTMSK       : aliased HAINTMSK_Register;
      --  This register is available only in host mode. Currently, the OTG host
      --  supports only one port. A single register holds USB port-related
      --  information such as USB reset, enable, suspend, resume, connect
      --  status, and test mode for each port. It is shown in Figure724. The
      --  rc_w1 bits in this register can trigger an interrupt to the
      --  application through the host port interrupt bit of the core interrupt
      --  register (HPRTINT bit in GINTSTS). On a port interrupt, the
      --  application must read this register and clear the bit that caused the
      --  interrupt. For the rc_w1 bits, the application must write a 1 to the
      --  bit to clear the interrupt.
      HPRT           : aliased HPRT_Register;
      --  OTG host channel 0 characteristics register
      HCCHAR0        : aliased HCCHAR_Register;
      --  This register indicates the status of a channel with respect to USB-
      --  and AHB-related events. It is shown in Figure724. The application
      --  must read this register when the host channels interrupt bit in the
      --  core interrupt register (HCINT bit in GINTSTS) is set. Before the
      --  application can read this register, it must first read the host all
      --  channels interrupt (HAINT) register to get the exact channel number
      --  for the host channel-x interrupt register. The application must clear
      --  the appropriate bit in this register to clear the corresponding bits
      --  in the HAINT and GINTSTS registers.
      HCINT0         : aliased HCINT_Register;
      --  This register reflects the mask for each channel status described in
      --  the previous section.
      HCINTMSK0      : aliased HCINTMSK_Register;
      --  OTG host channel 0 transfer size register
      HCTSIZ0        : aliased HCTSIZ_Register;
      --  OTG host channel 1 characteristics register
      HCCHAR1        : aliased HCCHAR_Register;
      --  This register indicates the status of a channel with respect to USB-
      --  and AHB-related events. It is shown in Figure724. The application
      --  must read this register when the host channels interrupt bit in the
      --  core interrupt register (HCINT bit in GINTSTS) is set. Before the
      --  application can read this register, it must first read the host all
      --  channels interrupt (HAINT) register to get the exact channel number
      --  for the host channel-x interrupt register. The application must clear
      --  the appropriate bit in this register to clear the corresponding bits
      --  in the HAINT and GINTSTS registers.
      HCINT1         : aliased HCINT_Register;
      --  This register reflects the mask for each channel status described in
      --  the previous section.
      HCINTMSK1      : aliased HCINTMSK_Register;
      --  OTG host channel 1 transfer size register
      HCTSIZ1        : aliased HCTSIZ_Register;
      --  OTG host channel 2 characteristics register
      HCCHAR2        : aliased HCCHAR_Register;
      --  This register indicates the status of a channel with respect to USB-
      --  and AHB-related events. It is shown in Figure724. The application
      --  must read this register when the host channels interrupt bit in the
      --  core interrupt register (HCINT bit in GINTSTS) is set. Before the
      --  application can read this register, it must first read the host all
      --  channels interrupt (HAINT) register to get the exact channel number
      --  for the host channel-x interrupt register. The application must clear
      --  the appropriate bit in this register to clear the corresponding bits
      --  in the HAINT and GINTSTS registers.
      HCINT2         : aliased HCINT_Register;
      --  This register reflects the mask for each channel status described in
      --  the previous section.
      HCINTMSK2      : aliased HCINTMSK_Register;
      --  OTG host channel 2 transfer size register
      HCTSIZ2        : aliased HCTSIZ_Register;
      --  OTG host channel 3 characteristics register
      HCCHAR3        : aliased HCCHAR_Register;
      --  This register indicates the status of a channel with respect to USB-
      --  and AHB-related events. It is shown in Figure724. The application
      --  must read this register when the host channels interrupt bit in the
      --  core interrupt register (HCINT bit in GINTSTS) is set. Before the
      --  application can read this register, it must first read the host all
      --  channels interrupt (HAINT) register to get the exact channel number
      --  for the host channel-x interrupt register. The application must clear
      --  the appropriate bit in this register to clear the corresponding bits
      --  in the HAINT and GINTSTS registers.
      HCINT3         : aliased HCINT_Register;
      --  This register reflects the mask for each channel status described in
      --  the previous section.
      HCINTMSK3      : aliased HCINTMSK_Register;
      --  OTG host channel 3 transfer size register
      HCTSIZ3        : aliased HCTSIZ_Register;
      --  OTG host channel 4 characteristics register
      HCCHAR4        : aliased HCCHAR_Register;
      --  This register indicates the status of a channel with respect to USB-
      --  and AHB-related events. It is shown in Figure724. The application
      --  must read this register when the host channels interrupt bit in the
      --  core interrupt register (HCINT bit in GINTSTS) is set. Before the
      --  application can read this register, it must first read the host all
      --  channels interrupt (HAINT) register to get the exact channel number
      --  for the host channel-x interrupt register. The application must clear
      --  the appropriate bit in this register to clear the corresponding bits
      --  in the HAINT and GINTSTS registers.
      HCINT4         : aliased HCINT_Register;
      --  This register reflects the mask for each channel status described in
      --  the previous section.
      HCINTMSK4      : aliased HCINTMSK_Register;
      --  OTG host channel 4 transfer size register
      HCTSIZ4        : aliased HCTSIZ_Register;
      --  OTG host channel 5 characteristics register
      HCCHAR5        : aliased HCCHAR_Register;
      --  This register indicates the status of a channel with respect to USB-
      --  and AHB-related events. It is shown in Figure724. The application
      --  must read this register when the host channels interrupt bit in the
      --  core interrupt register (HCINT bit in GINTSTS) is set. Before the
      --  application can read this register, it must first read the host all
      --  channels interrupt (HAINT) register to get the exact channel number
      --  for the host channel-x interrupt register. The application must clear
      --  the appropriate bit in this register to clear the corresponding bits
      --  in the HAINT and GINTSTS registers.
      HCINT5         : aliased HCINT_Register;
      --  This register reflects the mask for each channel status described in
      --  the previous section.
      HCINTMSK5      : aliased HCINTMSK_Register;
      --  OTG host channel 5 transfer size register
      HCTSIZ5        : aliased HCTSIZ_Register;
      --  OTG host channel 6 characteristics register
      HCCHAR6        : aliased HCCHAR_Register;
      --  This register indicates the status of a channel with respect to USB-
      --  and AHB-related events. It is shown in Figure724. The application
      --  must read this register when the host channels interrupt bit in the
      --  core interrupt register (HCINT bit in GINTSTS) is set. Before the
      --  application can read this register, it must first read the host all
      --  channels interrupt (HAINT) register to get the exact channel number
      --  for the host channel-x interrupt register. The application must clear
      --  the appropriate bit in this register to clear the corresponding bits
      --  in the HAINT and GINTSTS registers.
      HCINT6         : aliased HCINT_Register;
      --  This register reflects the mask for each channel status described in
      --  the previous section.
      HCINTMSK6      : aliased HCINTMSK_Register;
      --  OTG host channel 6 transfer size register
      HCTSIZ6        : aliased HCTSIZ_Register;
      --  OTG host channel 7 characteristics register
      HCCHAR7        : aliased HCCHAR_Register;
      --  This register indicates the status of a channel with respect to USB-
      --  and AHB-related events. It is shown in Figure724. The application
      --  must read this register when the host channels interrupt bit in the
      --  core interrupt register (HCINT bit in GINTSTS) is set. Before the
      --  application can read this register, it must first read the host all
      --  channels interrupt (HAINT) register to get the exact channel number
      --  for the host channel-x interrupt register. The application must clear
      --  the appropriate bit in this register to clear the corresponding bits
      --  in the HAINT and GINTSTS registers.
      HCINT7         : aliased HCINT_Register;
      --  This register reflects the mask for each channel status described in
      --  the previous section.
      HCINTMSK7      : aliased HCINTMSK_Register;
      --  OTG host channel 7 transfer size register
      HCTSIZ7        : aliased HCTSIZ_Register;
      --  OTG host channel 8 characteristics register
      HCCHAR8        : aliased HCCHAR_Register;
      --  This register indicates the status of a channel with respect to USB-
      --  and AHB-related events. It is shown in Figure724. The application
      --  must read this register when the host channels interrupt bit in the
      --  core interrupt register (HCINT bit in GINTSTS) is set. Before the
      --  application can read this register, it must first read the host all
      --  channels interrupt (HAINT) register to get the exact channel number
      --  for the host channel-x interrupt register. The application must clear
      --  the appropriate bit in this register to clear the corresponding bits
      --  in the HAINT and GINTSTS registers.
      HCINT8         : aliased HCINT_Register;
      --  This register reflects the mask for each channel status described in
      --  the previous section.
      HCINTMSK8      : aliased HCINTMSK_Register;
      --  OTG host channel 8 transfer size register
      HCTSIZ8        : aliased HCTSIZ_Register;
      --  OTG host channel 9 characteristics register
      HCCHAR9        : aliased HCCHAR_Register;
      --  This register indicates the status of a channel with respect to USB-
      --  and AHB-related events. It is shown in Figure724. The application
      --  must read this register when the host channels interrupt bit in the
      --  core interrupt register (HCINT bit in GINTSTS) is set. Before the
      --  application can read this register, it must first read the host all
      --  channels interrupt (HAINT) register to get the exact channel number
      --  for the host channel-x interrupt register. The application must clear
      --  the appropriate bit in this register to clear the corresponding bits
      --  in the HAINT and GINTSTS registers.
      HCINT9         : aliased HCINT_Register;
      --  This register reflects the mask for each channel status described in
      --  the previous section.
      HCINTMSK9      : aliased HCINTMSK_Register;
      --  OTG host channel 9 transfer size register
      HCTSIZ9        : aliased HCTSIZ_Register;
      --  OTG host channel 10 characteristics register
      HCCHAR10       : aliased HCCHAR_Register;
      --  This register indicates the status of a channel with respect to USB-
      --  and AHB-related events. It is shown in Figure724. The application
      --  must read this register when the host channels interrupt bit in the
      --  core interrupt register (HCINT bit in GINTSTS) is set. Before the
      --  application can read this register, it must first read the host all
      --  channels interrupt (HAINT) register to get the exact channel number
      --  for the host channel-x interrupt register. The application must clear
      --  the appropriate bit in this register to clear the corresponding bits
      --  in the HAINT and GINTSTS registers.
      HCINT10        : aliased HCINT_Register;
      --  This register reflects the mask for each channel status described in
      --  the previous section.
      HCINTMSK10     : aliased HCINTMSK_Register;
      --  OTG host channel 10 transfer size register
      HCTSIZ10       : aliased HCTSIZ_Register;
      --  OTG host channel 11 characteristics register
      HCCHAR11       : aliased HCCHAR_Register;
      --  This register indicates the status of a channel with respect to USB-
      --  and AHB-related events. It is shown in Figure724. The application
      --  must read this register when the host channels interrupt bit in the
      --  core interrupt register (HCINT bit in GINTSTS) is set. Before the
      --  application can read this register, it must first read the host all
      --  channels interrupt (HAINT) register to get the exact channel number
      --  for the host channel-x interrupt register. The application must clear
      --  the appropriate bit in this register to clear the corresponding bits
      --  in the HAINT and GINTSTS registers.
      HCINT11        : aliased HCINT_Register;
      --  This register reflects the mask for each channel status described in
      --  the previous section.
      HCINTMSK11     : aliased HCINTMSK_Register;
      --  OTG host channel 11 transfer size register
      HCTSIZ11       : aliased HCTSIZ_Register;
      --  This register configures the core in device mode after power-on or
      --  after certain control commands or enumeration. Do not make changes to
      --  this register after initial programming.
      DCFG           : aliased DCFG_Register;
      --  OTG device control register
      DCTL           : aliased DCTL_Register;
      --  This register indicates the status of the core with respect to
      --  USB-related events. It must be read on interrupts from the device all
      --  interrupts (DAINT) register.
      DSTS           : aliased DSTS_Register;
      --  This register works with each of the DIEPINTx registers for all
      --  endpoints to generate an interrupt per IN endpoint. The IN endpoint
      --  interrupt for a specific status in the DIEPINTx register can be
      --  masked by writing to the corresponding bit in this register. Status
      --  bits are masked by default.
      DIEPMSK        : aliased DIEPMSK_Register;
      --  This register works with each of the DOEPINTx registers for all
      --  endpoints to generate an interrupt per OUT endpoint. The OUT endpoint
      --  interrupt for a specific status in the DOEPINTx register can be
      --  masked by writing into the corresponding bit in this register. Status
      --  bits are masked by default.
      DOEPMSK        : aliased DOEPMSK_Register;
      --  When a significant event occurs on an endpoint, a DAINT register
      --  interrupts the application using the device OUT endpoints interrupt
      --  bit or device IN endpoints interrupt bit of the GINTSTS register
      --  (OEPINT or IEPINT in GINTSTS, respectively). There is one interrupt
      --  bit per endpoint, up to a maximum of 16 bits for OUT endpoints and 16
      --  bits for IN endpoints. For a bidirectional endpoint, the
      --  corresponding IN and OUT interrupt bits are used. Bits in this
      --  register are set and cleared when the application sets and clears
      --  bits in the corresponding device endpoint-x interrupt register
      --  (DIEPINTx/DOEPINTx).
      DAINT          : aliased DAINT_Register;
      --  The DAINTMSK register works with the device endpoint interrupt
      --  register to interrupt the application when an event occurs on a
      --  device endpoint. However, the DAINT register bit corresponding to
      --  that interrupt is still set.
      DAINTMSK       : aliased DAINTMSK_Register;
      --  This register specifies the VBUS discharge time after VBUS pulsing
      --  during SRP.
      DVBUSDIS       : aliased DVBUSDIS_Register;
      --  This register specifies the VBUS pulsing time during SRP.
      DVBUSPULSE     : aliased DVBUSPULSE_Register;
      --  This register is used to control the IN endpoint FIFO empty interrupt
      --  generation (TXFE_DIEPINTx).
      DIEPEMPMSK     : aliased DIEPEMPMSK_Register;
      --  The application uses this register to control the behavior of each
      --  logical endpoint other than endpoint 0.
      DIEPCTL0       : aliased DIEPCTL0_Register;
      --  This register indicates the status of an endpoint with respect to
      --  USB- and AHB-related events. It is shown in Figure724. The
      --  application must read this register when the IN endpoints interrupt
      --  bit of the core interrupt register (IEPINT in GINTSTS) is set. Before
      --  the application can read this register, it must first read the device
      --  all endpoints interrupt (DAINT) register to get the exact endpoint
      --  number for the device endpoint-x interrupt register. The application
      --  must clear the appropriate bit in this register to clear the
      --  corresponding bits in the DAINT and GINTSTS registers.
      DIEPINT0       : aliased DIEPINT_Register;
      --  The application must modify this register before enabling endpoint 0.
      DIEPTSIZ0      : aliased DIEPTSIZ0_Register;
      --  This read-only register contains the free space information for the
      --  device IN endpoint Tx FIFO.
      DTXFSTS0       : aliased DTXFSTS_Register;
      --  The application uses this register to control the behavior of each
      --  logical endpoint other than endpoint 0.
      DIEPCTL1       : aliased DIEPCTL_Register;
      --  This register indicates the status of an endpoint with respect to
      --  USB- and AHB-related events. It is shown in Figure724. The
      --  application must read this register when the IN endpoints interrupt
      --  bit of the core interrupt register (IEPINT in GINTSTS) is set. Before
      --  the application can read this register, it must first read the device
      --  all endpoints interrupt (DAINT) register to get the exact endpoint
      --  number for the device endpoint-x interrupt register. The application
      --  must clear the appropriate bit in this register to clear the
      --  corresponding bits in the DAINT and GINTSTS registers.
      DIEPINT1       : aliased DIEPINT_Register;
      --  The application must modify this register before enabling the
      --  endpoint. Once the endpoint is enabled using the endpoint enable bit
      --  in the DIEPCTLx registers (EPENA bit in DIEPCTLx), the core modifies
      --  this register. The application can only read this register once the
      --  core has cleared the endpoint enable bit.
      DIEPTSIZ1      : aliased DIEPTSIZ_Register;
      --  This read-only register contains the free space information for the
      --  device IN endpoint Tx FIFO.
      DTXFSTS1       : aliased DTXFSTS_Register;
      --  The application uses this register to control the behavior of each
      --  logical endpoint other than endpoint 0.
      DIEPCTL2       : aliased DIEPCTL_Register;
      --  This register indicates the status of an endpoint with respect to
      --  USB- and AHB-related events. It is shown in Figure724. The
      --  application must read this register when the IN endpoints interrupt
      --  bit of the core interrupt register (IEPINT in GINTSTS) is set. Before
      --  the application can read this register, it must first read the device
      --  all endpoints interrupt (DAINT) register to get the exact endpoint
      --  number for the device endpoint-x interrupt register. The application
      --  must clear the appropriate bit in this register to clear the
      --  corresponding bits in the DAINT and GINTSTS registers.
      DIEPINT2       : aliased DIEPINT_Register;
      --  The application must modify this register before enabling the
      --  endpoint. Once the endpoint is enabled using the endpoint enable bit
      --  in the DIEPCTLx registers (EPENA bit in DIEPCTLx), the core modifies
      --  this register. The application can only read this register once the
      --  core has cleared the endpoint enable bit.
      DIEPTSIZ2      : aliased DIEPTSIZ_Register;
      --  This read-only register contains the free space information for the
      --  device IN endpoint Tx FIFO.
      DTXFSTS2       : aliased DTXFSTS_Register;
      --  The application uses this register to control the behavior of each
      --  logical endpoint other than endpoint 0.
      DIEPCTL3       : aliased DIEPCTL_Register;
      --  This register indicates the status of an endpoint with respect to
      --  USB- and AHB-related events. It is shown in Figure724. The
      --  application must read this register when the IN endpoints interrupt
      --  bit of the core interrupt register (IEPINT in GINTSTS) is set. Before
      --  the application can read this register, it must first read the device
      --  all endpoints interrupt (DAINT) register to get the exact endpoint
      --  number for the device endpoint-x interrupt register. The application
      --  must clear the appropriate bit in this register to clear the
      --  corresponding bits in the DAINT and GINTSTS registers.
      DIEPINT3       : aliased DIEPINT_Register;
      --  The application must modify this register before enabling the
      --  endpoint. Once the endpoint is enabled using the endpoint enable bit
      --  in the DIEPCTLx registers (EPENA bit in DIEPCTLx), the core modifies
      --  this register. The application can only read this register once the
      --  core has cleared the endpoint enable bit.
      DIEPTSIZ3      : aliased DIEPTSIZ_Register;
      --  This read-only register contains the free space information for the
      --  device IN endpoint Tx FIFO.
      DTXFSTS3       : aliased DTXFSTS_Register;
      --  The application uses this register to control the behavior of each
      --  logical endpoint other than endpoint 0.
      DIEPCTL4       : aliased DIEPCTL_Register;
      --  This register indicates the status of an endpoint with respect to
      --  USB- and AHB-related events. It is shown in Figure724. The
      --  application must read this register when the IN endpoints interrupt
      --  bit of the core interrupt register (IEPINT in GINTSTS) is set. Before
      --  the application can read this register, it must first read the device
      --  all endpoints interrupt (DAINT) register to get the exact endpoint
      --  number for the device endpoint-x interrupt register. The application
      --  must clear the appropriate bit in this register to clear the
      --  corresponding bits in the DAINT and GINTSTS registers.
      DIEPINT4       : aliased DIEPINT_Register;
      --  The application must modify this register before enabling the
      --  endpoint. Once the endpoint is enabled using the endpoint enable bit
      --  in the DIEPCTLx registers (EPENA bit in DIEPCTLx), the core modifies
      --  this register. The application can only read this register once the
      --  core has cleared the endpoint enable bit.
      DIEPTSIZ4      : aliased DIEPTSIZ_Register;
      --  This read-only register contains the free space information for the
      --  device IN endpoint Tx FIFO.
      DTXFSTS4       : aliased DTXFSTS_Register;
      --  The application uses this register to control the behavior of each
      --  logical endpoint other than endpoint 0.
      DIEPCTL5       : aliased DIEPCTL_Register;
      --  This register indicates the status of an endpoint with respect to
      --  USB- and AHB-related events. It is shown in Figure724. The
      --  application must read this register when the IN endpoints interrupt
      --  bit of the core interrupt register (IEPINT in GINTSTS) is set. Before
      --  the application can read this register, it must first read the device
      --  all endpoints interrupt (DAINT) register to get the exact endpoint
      --  number for the device endpoint-x interrupt register. The application
      --  must clear the appropriate bit in this register to clear the
      --  corresponding bits in the DAINT and GINTSTS registers.
      DIEPINT5       : aliased DIEPINT_Register;
      --  The application must modify this register before enabling the
      --  endpoint. Once the endpoint is enabled using the endpoint enable bit
      --  in the DIEPCTLx registers (EPENA bit in DIEPCTLx), the core modifies
      --  this register. The application can only read this register once the
      --  core has cleared the endpoint enable bit.
      DIEPTSIZ5      : aliased DIEPTSIZ_Register;
      --  This read-only register contains the free space information for the
      --  device IN endpoint Tx FIFO.
      DTXFSTS5       : aliased DTXFSTS_Register;
      --  This section describes the DOEPCTL0 register.
      DOEPCTL0       : aliased DOEPCTL0_Register;
      --  This register indicates the status of an endpoint with respect to
      --  USB- and AHB-related events. It is shown in Figure724. The
      --  application must read this register when the OUT endpoints interrupt
      --  bit of the GINTSTS register (OEPINT bit in GINTSTS) is set. Before
      --  the application can read this register, it must first read the DAINT
      --  register to get the exact endpoint number for the DOEPINTx register.
      --  The application must clear the appropriate bit in this register to
      --  clear the corresponding bits in the DAINT and GINTSTS registers.
      DOEPINT0       : aliased DOEPINT_Register;
      --  The application must modify this register before enabling endpoint 0.
      DOEPTSIZ0      : aliased DOEPTSIZ0_Register;
      --  The application uses this register to control the behavior of each
      --  logical endpoint other than endpoint 0.
      DOEPCTL1       : aliased DOEPCTL_Register;
      --  This register indicates the status of an endpoint with respect to
      --  USB- and AHB-related events. It is shown in Figure724. The
      --  application must read this register when the OUT endpoints interrupt
      --  bit of the GINTSTS register (OEPINT bit in GINTSTS) is set. Before
      --  the application can read this register, it must first read the DAINT
      --  register to get the exact endpoint number for the DOEPINTx register.
      --  The application must clear the appropriate bit in this register to
      --  clear the corresponding bits in the DAINT and GINTSTS registers.
      DOEPINT1       : aliased DOEPINT_Register;
      --  The application must modify this register before enabling the
      --  endpoint. Once the endpoint is enabled using endpoint enable bit of
      --  the DOEPCTLx registers (EPENA bit in DOEPCTLx), the core modifies
      --  this register. The application can only read this register once the
      --  core has cleared the endpoint enable bit.
      DOEPTSIZ1      : aliased DOEPTSIZ_Register;
      --  The application uses this register to control the behavior of each
      --  logical endpoint other than endpoint 0.
      DOEPCTL2       : aliased DOEPCTL_Register;
      --  This register indicates the status of an endpoint with respect to
      --  USB- and AHB-related events. It is shown in Figure724. The
      --  application must read this register when the OUT endpoints interrupt
      --  bit of the GINTSTS register (OEPINT bit in GINTSTS) is set. Before
      --  the application can read this register, it must first read the DAINT
      --  register to get the exact endpoint number for the DOEPINTx register.
      --  The application must clear the appropriate bit in this register to
      --  clear the corresponding bits in the DAINT and GINTSTS registers.
      DOEPINT2       : aliased DOEPINT_Register;
      --  The application must modify this register before enabling the
      --  endpoint. Once the endpoint is enabled using endpoint enable bit of
      --  the DOEPCTLx registers (EPENA bit in DOEPCTLx), the core modifies
      --  this register. The application can only read this register once the
      --  core has cleared the endpoint enable bit.
      DOEPTSIZ2      : aliased DOEPTSIZ_Register;
      --  The application uses this register to control the behavior of each
      --  logical endpoint other than endpoint 0.
      DOEPCTL3       : aliased DOEPCTL_Register;
      --  This register indicates the status of an endpoint with respect to
      --  USB- and AHB-related events. It is shown in Figure724. The
      --  application must read this register when the OUT endpoints interrupt
      --  bit of the GINTSTS register (OEPINT bit in GINTSTS) is set. Before
      --  the application can read this register, it must first read the DAINT
      --  register to get the exact endpoint number for the DOEPINTx register.
      --  The application must clear the appropriate bit in this register to
      --  clear the corresponding bits in the DAINT and GINTSTS registers.
      DOEPINT3       : aliased DOEPINT_Register;
      --  The application must modify this register before enabling the
      --  endpoint. Once the endpoint is enabled using endpoint enable bit of
      --  the DOEPCTLx registers (EPENA bit in DOEPCTLx), the core modifies
      --  this register. The application can only read this register once the
      --  core has cleared the endpoint enable bit.
      DOEPTSIZ3      : aliased DOEPTSIZ_Register;
      --  The application uses this register to control the behavior of each
      --  logical endpoint other than endpoint 0.
      DOEPCTL4       : aliased DOEPCTL_Register;
      --  This register indicates the status of an endpoint with respect to
      --  USB- and AHB-related events. It is shown in Figure724. The
      --  application must read this register when the OUT endpoints interrupt
      --  bit of the GINTSTS register (OEPINT bit in GINTSTS) is set. Before
      --  the application can read this register, it must first read the DAINT
      --  register to get the exact endpoint number for the DOEPINTx register.
      --  The application must clear the appropriate bit in this register to
      --  clear the corresponding bits in the DAINT and GINTSTS registers.
      DOEPINT4       : aliased DOEPINT_Register;
      --  The application must modify this register before enabling the
      --  endpoint. Once the endpoint is enabled using endpoint enable bit of
      --  the DOEPCTLx registers (EPENA bit in DOEPCTLx), the core modifies
      --  this register. The application can only read this register once the
      --  core has cleared the endpoint enable bit.
      DOEPTSIZ4      : aliased DOEPTSIZ_Register;
      --  The application uses this register to control the behavior of each
      --  logical endpoint other than endpoint 0.
      DOEPCTL5       : aliased DOEPCTL_Register;
      --  This register indicates the status of an endpoint with respect to
      --  USB- and AHB-related events. It is shown in Figure724. The
      --  application must read this register when the OUT endpoints interrupt
      --  bit of the GINTSTS register (OEPINT bit in GINTSTS) is set. Before
      --  the application can read this register, it must first read the DAINT
      --  register to get the exact endpoint number for the DOEPINTx register.
      --  The application must clear the appropriate bit in this register to
      --  clear the corresponding bits in the DAINT and GINTSTS registers.
      DOEPINT5       : aliased DOEPINT_Register;
      --  The application must modify this register before enabling the
      --  endpoint. Once the endpoint is enabled using endpoint enable bit of
      --  the DOEPCTLx registers (EPENA bit in DOEPCTLx), the core modifies
      --  this register. The application can only read this register once the
      --  core has cleared the endpoint enable bit.
      DOEPTSIZ5      : aliased DOEPTSIZ_Register;
      --  This register is available in host and device modes.
      PCGCCTL        : aliased PCGCCTL_Register;
      case Discriminent is
         when Device =>
            --  This description is for register GRXSTSR in Device mode. A read
            --  to the receive status debug read register returns the contents
            --  of the top of the receive FIFO. The core ignores the receive
            --  status read when the receive FIFO is empty and returns a value
            --  of 0x00000000.
            GRXSTSR_DEVICE : aliased GRXSTSR_DEVICE_Register;
            --  This description is for register GRXSTSP in Device mode.
            --  Similarly to GRXSTSR (receive status debug read register) where
            --  a read returns the contents of the top of the receive FIFO, a
            --  read to GRXSTSP (receive status read and pop register)
            --  additionally pops the top data entry out of the Rx FIFO. The
            --  core ignores the receive status pop/read when the receive FIFO
            --  is empty and returns a value of 0x00000000. The application
            --  must only pop the receive status FIFO when the receive FIFO
            --  non-empty bit of the core interrupt register (RXFLVL bit in
            --  GINTSTS) is asserted.
            GRXSTSP_DEVICE : aliased GRXSTSP_DEVICE_Register;
         when Host =>
            --  This description is for register GRXSTSR in Host mode
            GRXSTSR_HOST : aliased GRXSTSR_HOST_Register;
            --  This description is for register GRXSTSP in HOST mode
            GRXSTSP_HOST : aliased GRXSTSP_HOST_Register;
      end case;
   end record
     with Unchecked_Union, Volatile;

   for OTG_FS_Peripheral use record
      GOTGCTL        at 16#0# range 0 .. 31;
      GOTGINT        at 16#4# range 0 .. 31;
      GAHBCFG        at 16#8# range 0 .. 31;
      GUSBCFG        at 16#C# range 0 .. 31;
      GRSTCTL        at 16#10# range 0 .. 31;
      GINTSTS        at 16#14# range 0 .. 31;
      GINTMSK        at 16#18# range 0 .. 31;
      GRXFSIZ        at 16#24# range 0 .. 31;
      HNPTXFSIZ      at 16#28# range 0 .. 31;
      HNPTXSTS       at 16#2C# range 0 .. 31;
      GCCFG          at 16#38# range 0 .. 31;
      CID            at 16#3C# range 0 .. 31;
      GLPMCFG        at 16#54# range 0 .. 31;
      HPTXFSIZ       at 16#100# range 0 .. 31;
      DIEPTXF1       at 16#104# range 0 .. 31;
      DIEPTXF2       at 16#108# range 0 .. 31;
      DIEPTXF3       at 16#10C# range 0 .. 31;
      DIEPTXF4       at 16#110# range 0 .. 31;
      DIEPTXF5       at 16#114# range 0 .. 31;
      HCFG           at 16#400# range 0 .. 31;
      HFIR           at 16#404# range 0 .. 31;
      HFNUM          at 16#408# range 0 .. 31;
      HPTXSTS        at 16#410# range 0 .. 31;
      HAINT          at 16#414# range 0 .. 31;
      HAINTMSK       at 16#418# range 0 .. 31;
      HPRT           at 16#440# range 0 .. 31;
      HCCHAR0        at 16#500# range 0 .. 31;
      HCINT0         at 16#508# range 0 .. 31;
      HCINTMSK0      at 16#50C# range 0 .. 31;
      HCTSIZ0        at 16#510# range 0 .. 31;
      HCCHAR1        at 16#520# range 0 .. 31;
      HCINT1         at 16#528# range 0 .. 31;
      HCINTMSK1      at 16#52C# range 0 .. 31;
      HCTSIZ1        at 16#530# range 0 .. 31;
      HCCHAR2        at 16#540# range 0 .. 31;
      HCINT2         at 16#548# range 0 .. 31;
      HCINTMSK2      at 16#54C# range 0 .. 31;
      HCTSIZ2        at 16#550# range 0 .. 31;
      HCCHAR3        at 16#560# range 0 .. 31;
      HCINT3         at 16#568# range 0 .. 31;
      HCINTMSK3      at 16#56C# range 0 .. 31;
      HCTSIZ3        at 16#570# range 0 .. 31;
      HCCHAR4        at 16#580# range 0 .. 31;
      HCINT4         at 16#588# range 0 .. 31;
      HCINTMSK4      at 16#58C# range 0 .. 31;
      HCTSIZ4        at 16#590# range 0 .. 31;
      HCCHAR5        at 16#5A0# range 0 .. 31;
      HCINT5         at 16#5A8# range 0 .. 31;
      HCINTMSK5      at 16#5AC# range 0 .. 31;
      HCTSIZ5        at 16#5B0# range 0 .. 31;
      HCCHAR6        at 16#5C0# range 0 .. 31;
      HCINT6         at 16#5C8# range 0 .. 31;
      HCINTMSK6      at 16#5CC# range 0 .. 31;
      HCTSIZ6        at 16#5D0# range 0 .. 31;
      HCCHAR7        at 16#5E0# range 0 .. 31;
      HCINT7         at 16#5E8# range 0 .. 31;
      HCINTMSK7      at 16#5EC# range 0 .. 31;
      HCTSIZ7        at 16#5F0# range 0 .. 31;
      HCCHAR8        at 16#600# range 0 .. 31;
      HCINT8         at 16#608# range 0 .. 31;
      HCINTMSK8      at 16#60C# range 0 .. 31;
      HCTSIZ8        at 16#610# range 0 .. 31;
      HCCHAR9        at 16#620# range 0 .. 31;
      HCINT9         at 16#628# range 0 .. 31;
      HCINTMSK9      at 16#62C# range 0 .. 31;
      HCTSIZ9        at 16#630# range 0 .. 31;
      HCCHAR10       at 16#640# range 0 .. 31;
      HCINT10        at 16#648# range 0 .. 31;
      HCINTMSK10     at 16#64C# range 0 .. 31;
      HCTSIZ10       at 16#650# range 0 .. 31;
      HCCHAR11       at 16#660# range 0 .. 31;
      HCINT11        at 16#668# range 0 .. 31;
      HCINTMSK11     at 16#66C# range 0 .. 31;
      HCTSIZ11       at 16#670# range 0 .. 31;
      DCFG           at 16#800# range 0 .. 31;
      DCTL           at 16#804# range 0 .. 31;
      DSTS           at 16#808# range 0 .. 31;
      DIEPMSK        at 16#810# range 0 .. 31;
      DOEPMSK        at 16#814# range 0 .. 31;
      DAINT          at 16#818# range 0 .. 31;
      DAINTMSK       at 16#81C# range 0 .. 31;
      DVBUSDIS       at 16#828# range 0 .. 31;
      DVBUSPULSE     at 16#82C# range 0 .. 31;
      DIEPEMPMSK     at 16#834# range 0 .. 31;
      DIEPCTL0       at 16#900# range 0 .. 31;
      DIEPINT0       at 16#908# range 0 .. 31;
      DIEPTSIZ0      at 16#910# range 0 .. 31;
      DTXFSTS0       at 16#918# range 0 .. 31;
      DIEPCTL1       at 16#920# range 0 .. 31;
      DIEPINT1       at 16#928# range 0 .. 31;
      DIEPTSIZ1      at 16#930# range 0 .. 31;
      DTXFSTS1       at 16#938# range 0 .. 31;
      DIEPCTL2       at 16#940# range 0 .. 31;
      DIEPINT2       at 16#948# range 0 .. 31;
      DIEPTSIZ2      at 16#950# range 0 .. 31;
      DTXFSTS2       at 16#958# range 0 .. 31;
      DIEPCTL3       at 16#960# range 0 .. 31;
      DIEPINT3       at 16#968# range 0 .. 31;
      DIEPTSIZ3      at 16#970# range 0 .. 31;
      DTXFSTS3       at 16#978# range 0 .. 31;
      DIEPCTL4       at 16#980# range 0 .. 31;
      DIEPINT4       at 16#988# range 0 .. 31;
      DIEPTSIZ4      at 16#990# range 0 .. 31;
      DTXFSTS4       at 16#998# range 0 .. 31;
      DIEPCTL5       at 16#9A0# range 0 .. 31;
      DIEPINT5       at 16#9A8# range 0 .. 31;
      DIEPTSIZ5      at 16#9B0# range 0 .. 31;
      DTXFSTS5       at 16#9B8# range 0 .. 31;
      DOEPCTL0       at 16#B00# range 0 .. 31;
      DOEPINT0       at 16#B08# range 0 .. 31;
      DOEPTSIZ0      at 16#B10# range 0 .. 31;
      DOEPCTL1       at 16#B20# range 0 .. 31;
      DOEPINT1       at 16#B28# range 0 .. 31;
      DOEPTSIZ1      at 16#B30# range 0 .. 31;
      DOEPCTL2       at 16#B40# range 0 .. 31;
      DOEPINT2       at 16#B48# range 0 .. 31;
      DOEPTSIZ2      at 16#B50# range 0 .. 31;
      DOEPCTL3       at 16#B60# range 0 .. 31;
      DOEPINT3       at 16#B68# range 0 .. 31;
      DOEPTSIZ3      at 16#B70# range 0 .. 31;
      DOEPCTL4       at 16#B80# range 0 .. 31;
      DOEPINT4       at 16#B88# range 0 .. 31;
      DOEPTSIZ4      at 16#B90# range 0 .. 31;
      DOEPCTL5       at 16#BA0# range 0 .. 31;
      DOEPINT5       at 16#BA8# range 0 .. 31;
      DOEPTSIZ5      at 16#BB0# range 0 .. 31;
      PCGCCTL        at 16#E00# range 0 .. 31;
      GRXSTSR_DEVICE at 16#1C# range 0 .. 31;
      GRXSTSP_DEVICE at 16#20# range 0 .. 31;
      GRXSTSR_HOST   at 16#1C# range 0 .. 31;
      GRXSTSP_HOST   at 16#20# range 0 .. 31;
   end record;

   --  OTG_FS
   OTG_FS_Periph : aliased OTG_FS_Peripheral
     with Import, Address => OTG_FS_Base;

   --  OTG_FS
   SEC_OTG_FS_Periph : aliased OTG_FS_Peripheral
     with Import, Address => SEC_OTG_FS_Base;

end STM32_SVD.OTG_FS;
