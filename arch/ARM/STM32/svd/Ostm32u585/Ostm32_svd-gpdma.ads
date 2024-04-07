--  This spec has been automatically generated from STM32U5xx.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.GPDMA is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  GPDMA_SECCFGR_SEC array
   type GPDMA_SECCFGR_SEC_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for GPDMA_SECCFGR_SEC
   type GPDMA_SECCFGR_SEC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SEC as a value
            Val : HAL.UInt16;
         when True =>
            --  SEC as an array
            Arr : GPDMA_SECCFGR_SEC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for GPDMA_SECCFGR_SEC_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  GPDMA secure configuration register
   type GPDMA_SECCFGR_Register is record
      --  SEC0
      SEC            : GPDMA_SECCFGR_SEC_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_SECCFGR_Register use record
      SEC            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  GPDMA_PRIVCFGR_PRIV array
   type GPDMA_PRIVCFGR_PRIV_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for GPDMA_PRIVCFGR_PRIV
   type GPDMA_PRIVCFGR_PRIV_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PRIV as a value
            Val : HAL.UInt16;
         when True =>
            --  PRIV as an array
            Arr : GPDMA_PRIVCFGR_PRIV_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for GPDMA_PRIVCFGR_PRIV_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  GPDMA privileged configuration register
   type GPDMA_PRIVCFGR_Register is record
      --  PRIV0
      PRIV           : GPDMA_PRIVCFGR_PRIV_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_PRIVCFGR_Register use record
      PRIV           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  GPDMA_RCFGLOCKR_LOCK array
   type GPDMA_RCFGLOCKR_LOCK_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for GPDMA_RCFGLOCKR_LOCK
   type GPDMA_RCFGLOCKR_LOCK_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  LOCK as a value
            Val : HAL.UInt16;
         when True =>
            --  LOCK as an array
            Arr : GPDMA_RCFGLOCKR_LOCK_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for GPDMA_RCFGLOCKR_LOCK_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  GPDMA configuration lock register
   type GPDMA_RCFGLOCKR_Register is record
      --  LOCK0
      LOCK           : GPDMA_RCFGLOCKR_LOCK_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_RCFGLOCKR_Register use record
      LOCK           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  GPDMA_MISR_MIS array
   type GPDMA_MISR_MIS_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for GPDMA_MISR_MIS
   type GPDMA_MISR_MIS_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MIS as a value
            Val : HAL.UInt16;
         when True =>
            --  MIS as an array
            Arr : GPDMA_MISR_MIS_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for GPDMA_MISR_MIS_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  GPDMA non-secure masked interrupt status register	
   type GPDMA_MISR_Register is record
      --  Read-only. MIS0
      MIS            : GPDMA_MISR_MIS_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_MISR_Register use record
      MIS            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  GPDMA_SMISR_MIS array
   type GPDMA_SMISR_MIS_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for GPDMA_SMISR_MIS
   type GPDMA_SMISR_MIS_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MIS as a value
            Val : HAL.UInt16;
         when True =>
            --  MIS as an array
            Arr : GPDMA_SMISR_MIS_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for GPDMA_SMISR_MIS_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  GPDMA secure masked interrupt status register
   type GPDMA_SMISR_Register is record
      --  Read-only. MIS0
      MIS            : GPDMA_SMISR_MIS_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_SMISR_Register use record
      MIS            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype GPDMA_C0LBAR_LBA_Field is HAL.UInt16;

   --  GPDMA channel 0 linked-list base address register
   type GPDMA_C0LBAR_Register is record
      --  unspecified
      Reserved_0_15 : HAL.UInt16 := 16#0#;
      --  linked-list base address of GPDMA channel x
      LBA           : GPDMA_C0LBAR_LBA_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C0LBAR_Register use record
      Reserved_0_15 at 0 range 0 .. 15;
      LBA           at 0 range 16 .. 31;
   end record;

   --  GPDMA channel 0 flag clear register
   type GPDMA_C0FCR_Register is record
      --  unspecified
      Reserved_0_7   : HAL.UInt8 := 16#0#;
      --  Write-only. transfer complete flag clear
      TCF            : Boolean := False;
      --  Write-only. half transfer flag clear
      HTF            : Boolean := False;
      --  Write-only. data transfer error flag clear
      DTEF           : Boolean := False;
      --  Write-only. update link transfer error flag clear
      ULEF           : Boolean := False;
      --  Write-only. user setting error flag clear
      USEF           : Boolean := False;
      --  Write-only. completed suspension flag clear
      SUSPF          : Boolean := False;
      --  Write-only. trigger overrun flag clear
      TOF            : Boolean := False;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C0FCR_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      TCF            at 0 range 8 .. 8;
      HTF            at 0 range 9 .. 9;
      DTEF           at 0 range 10 .. 10;
      ULEF           at 0 range 11 .. 11;
      USEF           at 0 range 12 .. 12;
      SUSPF          at 0 range 13 .. 13;
      TOF            at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype GPDMA_C0SR_FIFOL_Field is HAL.UInt8;

   --  GPDMA channel 0 status register
   type GPDMA_C0SR_Register is record
      --  Read-only. idle flag 	This idle flag is de-asserted by hardware when
      --  the channel is enabled (GPDMA_CxCR.EN = 1) with a valid channel
      --  configuration (no USEF to be immediately reported). 	This idle flag
      --  is asserted after hard reset or by hardware when the channel is back
      --  in idle state (in suspended or disabled state).
      IDLEF          : Boolean;
      --  unspecified
      Reserved_1_7   : HAL.UInt7;
      --  Read-only. transfer complete flag 	A transfer complete event is
      --  either a block transfer complete, a 2D/repeated block transfer
      --  complete, a LLI transfer complete including the upload of the next
      --  LLI if any, or the full linked-list completion, depending on the
      --  transfer complete event mode (GPDMA_CxTR2.TCEM[1:0]).
      TCF            : Boolean;
      --  Read-only. half transfer flag 	An half transfer event is either an
      --  half block transfer or an half 2D/repeated block transfer, depending
      --  on the transfer complete event mode (GPDMA_CxTR2.TCEM[1:0]). 	An half
      --  block transfer occurs when half of the bytes of the source block size
      --  (rounded up integer of GPDMA_CxBR1.BNDT[15:0]/2) has been transferred
      --  to the destination. 	An half 2D/repeated block transfer occurs when
      --  half of the repeated blocks (rounded up integer of
      --  (GPDMA_CxBR1.BRC[10:0]+1)/2)) has been transferred to the
      --  destination.
      HTF            : Boolean;
      --  Read-only. data transfer error flag
      DTEF           : Boolean;
      --  Read-only. update link transfer error flag
      ULEF           : Boolean;
      --  Read-only. user setting error flag
      USEF           : Boolean;
      --  Read-only. completed suspension flag
      SUSPF          : Boolean;
      --  Read-only. trigger overrun flag
      TOF            : Boolean;
      --  unspecified
      Reserved_15_15 : HAL.Bit;
      --  Read-only. monitored FIFO level 	Number of available write beats in
      --  the FIFO, in units of the programmed destination data width (see
      --  GPDMA_CxTR1.DDW_LOG2[1:0], in units of bytes, half-words, or words).
      --  	Note: After having suspended an active transfer, the user may need
      --  to read FIFOL[7:0], additionally to GPDMA_CxBR1.BDNT[15:0] and
      --  GPDMA_CxBR1.BRC[10:0], to know how many data have been transferred to
      --  the destination. Before reading, the user may wait for the transfer
      --  to be suspended (GPDMA_CxSR.SUSPF = 1).
      FIFOL          : GPDMA_C0SR_FIFOL_Field;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C0SR_Register use record
      IDLEF          at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      TCF            at 0 range 8 .. 8;
      HTF            at 0 range 9 .. 9;
      DTEF           at 0 range 10 .. 10;
      ULEF           at 0 range 11 .. 11;
      USEF           at 0 range 12 .. 12;
      SUSPF          at 0 range 13 .. 13;
      TOF            at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      FIFOL          at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype GPDMA_C0CR_PRIO_Field is HAL.UInt2;

   --  GPDMA channel 0 control register
   type GPDMA_C0CR_Register is record
      --  enable 	Writing 1 into the field RESET (bit 1) causes the hardware to
      --  de-assert this bit, whatever is written into this bit 0. Else: 	this
      --  bit is de-asserted by hardware when there is a transfer error (master
      --  bus error or user setting error) or when there is a channel transfer
      --  complete (channel ready to be configured, e.g. if LSM=1 at the end of
      --  a single execution of the LLI). 	Else, this bit can be asserted by
      --  software. 	Writing 0 into this EN bit is ignored.
      EN             : Boolean := False;
      --  Write-only. reset 	This bit is write only. Writing 0 has no impact.
      --  Writing 1 implies the reset of the following: the FIFO, the channel
      --  internal state, SUSP and EN bits (whatever is written receptively in
      --  bit 2 and bit 0). 	The reset is effective when the channel is in
      --  steady state, meaning one of the following: 	- active channel in
      --  suspended state (GPDMA_CxSR.SUSPF = 1 and
      --  GPDMA_CxSR.IDLEF = GPDMA_CxCR.EN = 1) 	- channel in disabled
      --  state (GPDMA_CxSR.IDLEF = 1 and GPDMA_CxCR.EN = 0). 	After
      --  writing a RESET, to continue using this channel, the user must
      --  explicitly reconfigure the channel including the hardware-modified
      --  configuration registers (GPDMA_CxBR1, GPDMA_CxSAR and GPDMA_CxDAR)
      --  before enabling again the channel (see the programming sequence in ).
      RESET          : Boolean := False;
      --  suspend 	Writing 1 into the field RESET (bit 1) causes the hardware
      --  to de-assert this bit, whatever is written into this bit 2. Else:
      --  	Software must write 1 in order to suspend an active channel i.e. a
      --  channel with an on-going GPDMA transfer over its master ports. 	The
      --  software must write 0 in order to resume a suspended channel,
      --  following the programming sequence detailed in .
      SUSP           : Boolean := False;
      --  unspecified
      Reserved_3_7   : HAL.UInt5 := 16#0#;
      --  transfer complete interrupt enable
      TCIE           : Boolean := False;
      --  half transfer complete interrupt enable
      HTIE           : Boolean := False;
      --  data transfer error interrupt enable
      DTEIE          : Boolean := False;
      --  update link transfer error interrupt enable
      ULEIE          : Boolean := False;
      --  user setting error interrupt enable
      USEIE          : Boolean := False;
      --  completed suspension interrupt enable
      SUSPIE         : Boolean := False;
      --  trigger overrun interrupt enable
      TOIE           : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Link step mode 	First the (possible 1D/repeated) block transfer is
      --  executed as defined by the current internal register file until
      --  GPDMA_CxBR1.BNDT[15:0] = 0 and GPDMA_CxBR1.BRC[10:0] = 0 if
      --  present. Secondly the next linked-list data structure is
      --  conditionally uploaded from memory as defined by GPDMA_CxLLR. Then
      --  channel execution is completed. 	Note: This bit must be written when
      --  EN=0. This bit is read-only when EN=1.
      LSM            : Boolean := False;
      --  linked-list allocated port 	This bit is used to allocate the master
      --  port for the update of the GPDMA linked-list registers from the
      --  memory. 	Note: This bit must be written when EN=0. This bit is
      --  read-only when EN=1.
      LAP            : Boolean := False;
      --  unspecified
      Reserved_18_21 : HAL.UInt4 := 16#0#;
      --  priority level of the channel x GPDMA transfer versus others 	Note:
      --  This bit must be written when EN = 0. This bit is read-only when
      --  EN = 1.
      PRIO           : GPDMA_C0CR_PRIO_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C0CR_Register use record
      EN             at 0 range 0 .. 0;
      RESET          at 0 range 1 .. 1;
      SUSP           at 0 range 2 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      TCIE           at 0 range 8 .. 8;
      HTIE           at 0 range 9 .. 9;
      DTEIE          at 0 range 10 .. 10;
      ULEIE          at 0 range 11 .. 11;
      USEIE          at 0 range 12 .. 12;
      SUSPIE         at 0 range 13 .. 13;
      TOIE           at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      LSM            at 0 range 16 .. 16;
      LAP            at 0 range 17 .. 17;
      Reserved_18_21 at 0 range 18 .. 21;
      PRIO           at 0 range 22 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype GPDMA_C0TR1_SDW_LOG2_Field is HAL.UInt2;
   subtype GPDMA_C0TR1_SBL_1_Field is HAL.UInt6;
   subtype GPDMA_C0TR1_PAM_Field is HAL.UInt2;
   subtype GPDMA_C0TR1_DDW_LOG2_Field is HAL.UInt2;
   subtype GPDMA_C0TR1_DBL_1_Field is HAL.UInt6;

   --  GPDMA channel 0 transfer register 1
   type GPDMA_C0TR1_Register is record
      --  binary logarithm of the source data width of a burst in bytes 	Note:
      --  Setting a 8-byte data width causes a user setting error to be
      --  reported and no transfer is issued. 	A source block size must be a
      --  multiple of the source data width (GPDMA_CxBR1.BNDT[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and no
      --  transfer is issued. 	A source single transfer must have an aligned
      --  address with its data width (start address GPDMA_CxSAR[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and none
      --  transfer is issued.
      SDW_LOG2       : GPDMA_C0TR1_SDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  source incrementing burst 	The source address, pointed by
      --  GPDMA_CxSAR, is kept constant after a burst beat/single transfer or
      --  is incremented by the offset value corresponding to a contiguous data
      --  after a burst beat/single transfer.
      SINC           : Boolean := False;
      --  source burst length minus 1, between 0 and 63 	The burst length unit
      --  is one data named beat within a burst. If SBL_1[5:0] =0 , the burst
      --  can be named as single. Each data/beat has a width defined by the
      --  destination data width SDW_LOG2[1:0]. 	Note: If a burst transfer
      --  crossed a 1-Kbyte address boundary on a AHB transfer, the GPDMA
      --  modifies and shortens the programmed burst into singles or bursts of
      --  lower length, to be compliant with the AHB protocol. 	If a burst
      --  transfer is of length greater than the FIFO size of the channel x,
      --  the GPDMA modifies and shortens the programmed burst into singles or
      --  bursts of lower length, to be compliant with the FIFO size. Transfer
      --  performance is lower, with GPDMA re-arbitration between effective and
      --  lower bursts/singles, but the data integrity is guaranteed.
      SBL_1          : GPDMA_C0TR1_SBL_1_Field := 16#0#;
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  padding/alignment mode 	If DDW_LOG2[1:0] = SDW_LOG2[1:0]: if the data
      --  width of a burst destination transfer is equal to the data width of a
      --  burst source transfer, these bits are ignored. 	Else: 	- Case 1: If
      --  destination data width > source data width 	1x: successive source
      --  data are FIFO queued and packed at the destination data width, in a
      --  left (LSB) to right (MSB) order (named little endian), before a
      --  destination transfer 	- Case 2: If destination data width < source
      --  data width 	1x: source data is FIFO queued and unpacked at the
      --  destination data width, to be transferred in a left (LSB) to right
      --  (MSB) order (named little endian) to the destination 	Note:
      PAM            : GPDMA_C0TR1_PAM_Field := 16#0#;
      --  source byte exchange within the unaligned half-word of each source
      --  word 	If the source data width is shorter than a word, this bit is
      --  ignored. 	If the source data width is a word:
      SBX            : Boolean := False;
      --  source allocated port 	This bit is used to allocate the master port
      --  for the source transfer 	Note: This bit must be written when
      --  EN = 0. This bit is read-only when EN = 1.
      SAP            : Boolean := False;
      --  security attribute of the GPDMA transfer from the source 	If
      --  GPDMA_SECCFGR.SECx = 1 and the access is secure: 	This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  GPDMA_SECCFGR.SECx =1 . A secure write is ignored when
      --  GPDMA_SECCFGR.SECx = 0. 	When GPDMA_SECCFGR.SECx is de-asserted,
      --  this SSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the GPDMA transfer
      --  from the source is non-secure.
      SSEC           : Boolean := False;
      --  binary logarithm of the destination data width of a burst, in bytes
      --  	Note: Setting a 8-byte data width causes a user setting error to be
      --  reported and none transfer is issued. 	A destination burst transfer
      --  must have an aligned address with its data width (start address
      --  GPDMA_CxDAR[2:0] and address offset GPDMA_CxTR3.DAO[2:0], versus
      --  DDW_LOG2[1:0]). Otherwise a user setting error is reported and no
      --  transfer is issued.
      DDW_LOG2       : GPDMA_C0TR1_DDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_18_18 : HAL.Bit := 16#0#;
      --  destination incrementing burst 	The destination address, pointed by
      --  GPDMA_CxDAR, is kept constant after a burst beat/single transfer, or
      --  is incremented by the offset value corresponding to a contiguous data
      --  after a burst beat/single transfer.
      DINC           : Boolean := False;
      --  destination burst length minus 1, between 0 and 63 	The burst length
      --  unit is one data named beat within a burst. If DBL_1[5:0] =0 , the
      --  burst can be named as single. Each data/beat has a width defined by
      --  the destination data width DDW_LOG2[1:0]. 	Note: If a burst transfer
      --  crossed a 1-Kbyte address boundary on a AHB transfer, the GPDMA
      --  modifies and shortens the programmed burst into singles or bursts of
      --  lower length, to be compliant with the AHB protocol. 	If a burst
      --  transfer is of length greater than the FIFO size of the channel x,
      --  the GPDMA modifies and shortens the programmed burst into singles or
      --  bursts of lower length, to be compliant with the FIFO size. Transfer
      --  performance is lower, with GPDMA re-arbitration between effective and
      --  lower bursts/singles, but the data integrity is guaranteed.
      DBL_1          : GPDMA_C0TR1_DBL_1_Field := 16#0#;
      --  destination byte exchange 	If the destination data size is a byte,
      --  this bit is ignored. 	If the destination data size is not a byte:
      DBX            : Boolean := False;
      --  destination half-word exchange 	If the destination data size is
      --  shorter than a word, this bit is ignored. 	If the destination data
      --  size is a word:
      DHX            : Boolean := False;
      --  unspecified
      Reserved_28_29 : HAL.UInt2 := 16#0#;
      --  destination allocated port 	This bit is used to allocate the master
      --  port for the destination transfer 	Note: This bit must be written
      --  when EN = 0. This bit is read-only when EN = 1.
      DAP            : Boolean := False;
      --  security attribute of the GPDMA transfer to the destination 	If
      --  GPDMA_SECCFGR.SECx = 1 and the access is secure: 	This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  GPDMA_SECCFGR.SECx = 1. A secure write is ignored when
      --  GPDMA_SECCFGR.SECx = 0. 	When GPDMA_SECCFGR.SECx is de-asserted,
      --  this DSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the GPDMA transfer
      --  to the destination is non-secure.
      DSEC           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C0TR1_Register use record
      SDW_LOG2       at 0 range 0 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      SINC           at 0 range 3 .. 3;
      SBL_1          at 0 range 4 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      PAM            at 0 range 11 .. 12;
      SBX            at 0 range 13 .. 13;
      SAP            at 0 range 14 .. 14;
      SSEC           at 0 range 15 .. 15;
      DDW_LOG2       at 0 range 16 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      DINC           at 0 range 19 .. 19;
      DBL_1          at 0 range 20 .. 25;
      DBX            at 0 range 26 .. 26;
      DHX            at 0 range 27 .. 27;
      Reserved_28_29 at 0 range 28 .. 29;
      DAP            at 0 range 30 .. 30;
      DSEC           at 0 range 31 .. 31;
   end record;

   subtype GPDMA_C0TR2_REQSEL_Field is HAL.UInt7;
   subtype GPDMA_C0TR2_TRIGM_Field is HAL.UInt2;
   subtype GPDMA_C0TR2_TRIGSEL_Field is HAL.UInt6;
   subtype GPDMA_C0TR2_TRIGPOL_Field is HAL.UInt2;
   subtype GPDMA_C0TR2_TCEM_Field is HAL.UInt2;

   --  GPDMA channel 0 transfer register 2
   type GPDMA_C0TR2_Register is record
      --  GPDMA hardware request selection 	These bits are ignored if channel x
      --  is activated (GPDMA_CxCR.EN asserted) with SWREQ = 1 (software
      --  request for a memory-to-memory transfer). Else, the selected hardware
      --  request is internally taken into account as per . 	The user must not
      --  assign a same input hardware request (same REQSEL[6:0] value) to
      --  different active GPDMA channels (GPDMA_CxCR.EN = 1 and
      --  GPDMA_CxTR2.SWREQ = 0 for these channels). GPDMA is not intended to
      --  hardware support the case of simultaneous enabled channels
      --  incorrectly configured with a same hardware peripheral request
      --  signal, and there is no user setting error reporting.
      REQSEL         : GPDMA_C0TR2_REQSEL_Field := 16#0#;
      --  unspecified
      Reserved_7_8   : HAL.UInt2 := 16#0#;
      --  software request 	This bit is internally taken into account when
      --  GPDMA_CxCR.EN is asserted.
      SWREQ          : Boolean := False;
      --  destination hardware request 	This bit is ignored if channel x is
      --  activated (GPDMA_CxCR.EN asserted) with SWREQ = 1 (software request
      --  for a memory-to-memory transfer). Else: 	Note:
      DREQ           : Boolean := False;
      --  Block hardware request 	If the channel x is activated (GPDMA_CxCR.EN
      --  asserted) with SWREQ = 1 (software request for a memory-to-memory
      --  transfer), this bit is ignored. Else:
      BREQ           : Boolean := False;
      --  unspecified
      Reserved_12_13 : HAL.UInt2 := 16#0#;
      --  trigger mode 	These bits define the transfer granularity for its
      --  conditioning by the trigger. 	If the channel x is enabled
      --  (GPDMA_CxCR.EN asserted) with TRIGPOL[1:0] = 00 or 11, these
      --  TRIGM[1:0] bits are ignored. 	Else, a GPDMA transfer is conditioned
      --  by at least one trigger hit: 	first burst read of a 2D/repeated block
      --  transfer is conditioned by one hit trigger. 	– If the peripheral is
      --  programmed as a source (DREQ = 0) of the LLI data transfer, each
      --  programmed burst read is conditioned. 	– If the peripheral is
      --  programmed as a destination (DREQ = 1) of the LLI data transfer,
      --  each programmed burst write is conditioned. The first memory burst
      --  read of a (possibly 2D/repeated) block, also named as the first ready
      --  FIFO-based source burst, is gated by the occurrence of both the
      --  hardware request and the first trigger hit. 	The GPDMA monitoring of
      --  a trigger for channel x is started when the channel is enabled/loaded
      --  with a new active trigger configuration: rising or falling edge on a
      --  selected trigger (TRIGPOL[1:0] = 01 or respectively
      --  TRIGPOL[1:0] = 10). 	The monitoring of this trigger is kept active
      --  during the triggered and uncompleted (data or link) transfer; and if
      --  a new trigger is detected then, this hit is internally memorized to
      --  grant the next transfer, as long as the defined rising or falling
      --  edge is not modified, and the TRIGSEL[5:0] is not modified, and the
      --  channel is enabled. 	Transferring a next LLIn+1 that updates the
      --  GPDMA_CxTR2 with a new value for any of TRIGSEL[5:0] or TRIGPOL[1:0],
      --  resets the monitoring, trashing the memorized hit of the formerly
      --  defined LLIn trigger. 	After a first new trigger hitn+1 is memorized,
      --  if another second trigger hitn+2 is detected and if the hitn
      --  triggered transfer is still not completed, hitn+2 is lost and not
      --  memorized.memorized. A trigger overrun flag is reported
      --  (GPDMA_CxSR.TOF =1 ), and an interrupt is generated if enabled
      --  (GPDMA_CxCR.TOIE = 1). The channel is not automatically disabled by
      --  hardware due to a trigger overrun. 	Note: When the source block size
      --  is not a multiple of the source burst size and is a multiple of the
      --  source data width, then the last programmed source burst is not
      --  completed and is internally shorten to match the block size. In this
      --  case, if TRIGM[1:0] = 11 and (SWREQ =1  or (SWREQ = 0 and
      --  DREQ =0 )), the shortened burst transfer (by singles or/and by
      --  bursts of lower length) is conditioned once by the trigger. 	When the
      --  programmed destination burst is internally shortened by singles
      --  or/and by bursts of lower length (versus FIFO size, versus block
      --  size, 1-Kbyte boundary address crossing): if the trigger is
      --  conditioning the programmed destination burst (if TRIGM[1:0] = 11
      --  and SWREQ = 0 and DREQ = 1), this shortened destination burst
      --  transfer is conditioned once by the trigger.
      TRIGM          : GPDMA_C0TR2_TRIGM_Field := 16#0#;
      --  trigger event input selection 	These bits select the trigger event
      --  input of the GPDMA transfer (as per ), with an active trigger event
      --  if TRIGPOL[1:0] ≠ 00.
      TRIGSEL        : GPDMA_C0TR2_TRIGSEL_Field := 16#0#;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  trigger event polarity 	These bits define the polarity of the
      --  selected trigger event input defined by TRIGSEL[5:0].
      TRIGPOL        : GPDMA_C0TR2_TRIGPOL_Field := 16#0#;
      --  unspecified
      Reserved_26_29 : HAL.UInt4 := 16#0#;
      --  transfer complete event mode 	These bits define the transfer
      --  granularity for the transfer complete and half transfer complete
      --  events generation. 	Note: If the initial LLI0 data transfer is
      --  null/void (directly programmed by the internal register file with
      --  GPDMA_CxBR1.BNDT[15:0] = 0), then neither the complete transfer
      --  event nor the half transfer event is generated. 	Note: If the initial
      --  LLI0 data transfer is null/void (directly programmed by the internal
      --  register file with GPDMA_CxBR1.BNDT[15:0] = 0), then neither the
      --  complete transfer event nor the half transfer event is generated.
      --  	Note: If the initial LLI0 data transfer is null/void (i.e. directly
      --  programmed by the internal register file with
      --  GPDMA_CxBR1.BNDT[15:0] =0 ), then the half transfer event is not
      --  generated, and the transfer complete event is generated when is
      --  completed the loading of the LLI1.
      TCEM           : GPDMA_C0TR2_TCEM_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C0TR2_Register use record
      REQSEL         at 0 range 0 .. 6;
      Reserved_7_8   at 0 range 7 .. 8;
      SWREQ          at 0 range 9 .. 9;
      DREQ           at 0 range 10 .. 10;
      BREQ           at 0 range 11 .. 11;
      Reserved_12_13 at 0 range 12 .. 13;
      TRIGM          at 0 range 14 .. 15;
      TRIGSEL        at 0 range 16 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      TRIGPOL        at 0 range 24 .. 25;
      Reserved_26_29 at 0 range 26 .. 29;
      TCEM           at 0 range 30 .. 31;
   end record;

   subtype GPDMA_C0BR1_BNDT_Field is HAL.UInt16;

   --  GPDMA channel 0 block register 1
   type GPDMA_C0BR1_Register is record
      --  block number of data bytes to transfer from the source 	Block size
      --  transferred from the source. When the channel is enabled, this field
      --  becomes read-only and is decremented, indicating the remaining number
      --  of data items in the current source block to be transferred.
      --  BNDT[15:0] is programmed in number of bytes, maximum source block
      --  size is 64 Kbytes -1. 	Once the last data transfer is completed
      --  (BNDT[15:0] = 0): 	- if GPDMA_CxLLR.UB1 = 1, this field is
      --  updated by the LLI in the memory. 	- if GPDMA_CxLLR.UB1 = 0 and if
      --  there is at least one non null Uxx update bit, this field is
      --  internally restored to the programmed value. 	- if all
      --  GPDMA_CxLLR.Uxx = 0 and if GPDMA_CxLLR.LA[15:0] = 0, this field
      --  is internally restored to the programmed value (infinite/continuous
      --  last LLI). 	- if GPDMA_CxLLR = 0, this field is kept as zero
      --  following the last LLI data transfer. 	Note: A non-null source block
      --  size must be a multiple of the source data width (BNDT[2:0] versus
      --  GPDMA_CxTR1.SDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued. 	When configured in packing mode
      --  (GPDMA_CxTR1.PAM[1] = 1 and destination data width different from
      --  source data width), a non-null source block size must be a multiple
      --  of the destination data width (BNDT[2:0] versus
      --  GPDMA_CxTR1.DDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued.
      BNDT           : GPDMA_C0BR1_BNDT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C0BR1_Register use record
      BNDT           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype GPDMA_C0LLR_LA_Field is HAL.UInt14;

   --  GPDMA channel 0 linked-list address register
   type GPDMA_C0LLR_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  pointer (16-bit low-significant address) to the next linked-list data
      --  structure 	If UT1 = UT2 = UB1 = USA = UDA = ULL = 0 and
      --  if LA[15:20] = 0, the current LLI is the last one. The channel
      --  transfer is completed without any update of the linked-list GPDMA
      --  register file. 	Else, this field is the pointer to the memory address
      --  offset from which the next linked-list data structure is
      --  automatically fetched from, once the data transfer is completed, in
      --  order to conditionally update the linked-list GPDMA internal register
      --  file (GPDMA_CxCTR1, GPDMA_CxTR2, GPDMA_CxBR1, GPDMA_CxSAR,
      --  GPDMA_CxDAR and GPDMA_CxLLR). 	Note: The user must program the
      --  pointer to be 32-bit aligned. The two low-significant bits are write
      --  ignored.
      LA             : GPDMA_C0LLR_LA_Field := 16#0#;
      --  Update GPDMA_CxLLR register from memory 	This bit is used to control
      --  the update of GPDMA_CxLLR from the memory during the link transfer.
      ULL            : Boolean := False;
      --  unspecified
      Reserved_17_26 : HAL.UInt10 := 16#0#;
      --  Update GPDMA_CxDAR register from memory 	This bit is used to control
      --  the update of GPDMA_CxDAR from the memory during the link transfer.
      UDA            : Boolean := False;
      --  update GPDMA_CxSAR from memory 	This bit controls the update of
      --  GPDMA_CxSAR from the memory during the link transfer.
      USA            : Boolean := False;
      --  Update GPDMA_CxBR1 from memory 	This bit controls the update of
      --  GPDMA_CxBR1 from the memory during the link transfer. If UB1 = 0
      --  and if GPDMA_CxLLR ≠ 0, the linked-list is not completed.
      --  GPDMA_CxBR1.BNDT[15:0] is then restored to the programmed value after
      --  data transfer is completed and before the link transfer.
      UB1            : Boolean := False;
      --  Update GPDMA_CxTR2 from memory 	This bit controls the update of
      --  GPDMA_CxTR2 from the memory during the link transfer.
      UT2            : Boolean := False;
      --  Update GPDMA_CxTR1 from memory 	This bit controls the update of
      --  GPDMA_CxTR1 from the memory during the link transfer.
      UT1            : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C0LLR_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      LA             at 0 range 2 .. 15;
      ULL            at 0 range 16 .. 16;
      Reserved_17_26 at 0 range 17 .. 26;
      UDA            at 0 range 27 .. 27;
      USA            at 0 range 28 .. 28;
      UB1            at 0 range 29 .. 29;
      UT2            at 0 range 30 .. 30;
      UT1            at 0 range 31 .. 31;
   end record;

   subtype GPDMA_C1LBAR_LBA_Field is HAL.UInt16;

   --  GPDMA channel 1 linked-list base address register
   type GPDMA_C1LBAR_Register is record
      --  unspecified
      Reserved_0_15 : HAL.UInt16 := 16#0#;
      --  linked-list base address of GPDMA channel x
      LBA           : GPDMA_C1LBAR_LBA_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C1LBAR_Register use record
      Reserved_0_15 at 0 range 0 .. 15;
      LBA           at 0 range 16 .. 31;
   end record;

   --  GPDMA channel 1 flag clear register
   type GPDMA_C1FCR_Register is record
      --  unspecified
      Reserved_0_7   : HAL.UInt8 := 16#0#;
      --  Write-only. transfer complete flag clear
      TCF            : Boolean := False;
      --  Write-only. half transfer flag clear
      HTF            : Boolean := False;
      --  Write-only. data transfer error flag clear
      DTEF           : Boolean := False;
      --  Write-only. update link transfer error flag clear
      ULEF           : Boolean := False;
      --  Write-only. user setting error flag clear
      USEF           : Boolean := False;
      --  Write-only. completed suspension flag clear
      SUSPF          : Boolean := False;
      --  Write-only. trigger overrun flag clear
      TOF            : Boolean := False;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C1FCR_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      TCF            at 0 range 8 .. 8;
      HTF            at 0 range 9 .. 9;
      DTEF           at 0 range 10 .. 10;
      ULEF           at 0 range 11 .. 11;
      USEF           at 0 range 12 .. 12;
      SUSPF          at 0 range 13 .. 13;
      TOF            at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype GPDMA_C1SR_FIFOL_Field is HAL.UInt8;

   --  GPDMA channel 1 status register
   type GPDMA_C1SR_Register is record
      --  Read-only. idle flag 	This idle flag is de-asserted by hardware when
      --  the channel is enabled (GPDMA_CxCR.EN = 1) with a valid channel
      --  configuration (no USEF to be immediately reported). 	This idle flag
      --  is asserted after hard reset or by hardware when the channel is back
      --  in idle state (in suspended or disabled state).
      IDLEF          : Boolean;
      --  unspecified
      Reserved_1_7   : HAL.UInt7;
      --  Read-only. transfer complete flag 	A transfer complete event is
      --  either a block transfer complete, a 2D/repeated block transfer
      --  complete, a LLI transfer complete including the upload of the next
      --  LLI if any, or the full linked-list completion, depending on the
      --  transfer complete event mode (GPDMA_CxTR2.TCEM[1:0]).
      TCF            : Boolean;
      --  Read-only. half transfer flag 	An half transfer event is either an
      --  half block transfer or an half 2D/repeated block transfer, depending
      --  on the transfer complete event mode (GPDMA_CxTR2.TCEM[1:0]). 	An half
      --  block transfer occurs when half of the bytes of the source block size
      --  (rounded up integer of GPDMA_CxBR1.BNDT[15:0]/2) has been transferred
      --  to the destination. 	An half 2D/repeated block transfer occurs when
      --  half of the repeated blocks (rounded up integer of
      --  (GPDMA_CxBR1.BRC[10:0]+1)/2)) has been transferred to the
      --  destination.
      HTF            : Boolean;
      --  Read-only. data transfer error flag
      DTEF           : Boolean;
      --  Read-only. update link transfer error flag
      ULEF           : Boolean;
      --  Read-only. user setting error flag
      USEF           : Boolean;
      --  Read-only. completed suspension flag
      SUSPF          : Boolean;
      --  Read-only. trigger overrun flag
      TOF            : Boolean;
      --  unspecified
      Reserved_15_15 : HAL.Bit;
      --  Read-only. monitored FIFO level 	Number of available write beats in
      --  the FIFO, in units of the programmed destination data width (see
      --  GPDMA_CxTR1.DDW_LOG2[1:0], in units of bytes, half-words, or words).
      --  	Note: After having suspended an active transfer, the user may need
      --  to read FIFOL[7:0], additionally to GPDMA_CxBR1.BDNT[15:0] and
      --  GPDMA_CxBR1.BRC[10:0], to know how many data have been transferred to
      --  the destination. Before reading, the user may wait for the transfer
      --  to be suspended (GPDMA_CxSR.SUSPF = 1).
      FIFOL          : GPDMA_C1SR_FIFOL_Field;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C1SR_Register use record
      IDLEF          at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      TCF            at 0 range 8 .. 8;
      HTF            at 0 range 9 .. 9;
      DTEF           at 0 range 10 .. 10;
      ULEF           at 0 range 11 .. 11;
      USEF           at 0 range 12 .. 12;
      SUSPF          at 0 range 13 .. 13;
      TOF            at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      FIFOL          at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype GPDMA_C1CR_PRIO_Field is HAL.UInt2;

   --  GPDMA channel 1 control register
   type GPDMA_C1CR_Register is record
      --  enable 	Writing 1 into the field RESET (bit 1) causes the hardware to
      --  de-assert this bit, whatever is written into this bit 0. Else: 	this
      --  bit is de-asserted by hardware when there is a transfer error (master
      --  bus error or user setting error) or when there is a channel transfer
      --  complete (channel ready to be configured, e.g. if LSM=1 at the end of
      --  a single execution of the LLI). 	Else, this bit can be asserted by
      --  software. 	Writing 0 into this EN bit is ignored.
      EN             : Boolean := False;
      --  Write-only. reset 	This bit is write only. Writing 0 has no impact.
      --  Writing 1 implies the reset of the following: the FIFO, the channel
      --  internal state, SUSP and EN bits (whatever is written receptively in
      --  bit 2 and bit 0). 	The reset is effective when the channel is in
      --  steady state, meaning one of the following: 	- active channel in
      --  suspended state (GPDMA_CxSR.SUSPF = 1 and
      --  GPDMA_CxSR.IDLEF = GPDMA_CxCR.EN = 1) 	- channel in disabled
      --  state (GPDMA_CxSR.IDLEF = 1 and GPDMA_CxCR.EN = 0). 	After
      --  writing a RESET, to continue using this channel, the user must
      --  explicitly reconfigure the channel including the hardware-modified
      --  configuration registers (GPDMA_CxBR1, GPDMA_CxSAR and GPDMA_CxDAR)
      --  before enabling again the channel (see the programming sequence in ).
      RESET          : Boolean := False;
      --  suspend 	Writing 1 into the field RESET (bit 1) causes the hardware
      --  to de-assert this bit, whatever is written into this bit 2. Else:
      --  	Software must write 1 in order to suspend an active channel i.e. a
      --  channel with an on-going GPDMA transfer over its master ports. 	The
      --  software must write 0 in order to resume a suspended channel,
      --  following the programming sequence detailed in .
      SUSP           : Boolean := False;
      --  unspecified
      Reserved_3_7   : HAL.UInt5 := 16#0#;
      --  transfer complete interrupt enable
      TCIE           : Boolean := False;
      --  half transfer complete interrupt enable
      HTIE           : Boolean := False;
      --  data transfer error interrupt enable
      DTEIE          : Boolean := False;
      --  update link transfer error interrupt enable
      ULEIE          : Boolean := False;
      --  user setting error interrupt enable
      USEIE          : Boolean := False;
      --  completed suspension interrupt enable
      SUSPIE         : Boolean := False;
      --  trigger overrun interrupt enable
      TOIE           : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Link step mode 	First the (possible 1D/repeated) block transfer is
      --  executed as defined by the current internal register file until
      --  GPDMA_CxBR1.BNDT[15:0] = 0 and GPDMA_CxBR1.BRC[10:0] = 0 if
      --  present. Secondly the next linked-list data structure is
      --  conditionally uploaded from memory as defined by GPDMA_CxLLR. Then
      --  channel execution is completed. 	Note: This bit must be written when
      --  EN=0. This bit is read-only when EN=1.
      LSM            : Boolean := False;
      --  linked-list allocated port 	This bit is used to allocate the master
      --  port for the update of the GPDMA linked-list registers from the
      --  memory. 	Note: This bit must be written when EN=0. This bit is
      --  read-only when EN=1.
      LAP            : Boolean := False;
      --  unspecified
      Reserved_18_21 : HAL.UInt4 := 16#0#;
      --  priority level of the channel x GPDMA transfer versus others 	Note:
      --  This bit must be written when EN = 0. This bit is read-only when
      --  EN = 1.
      PRIO           : GPDMA_C1CR_PRIO_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C1CR_Register use record
      EN             at 0 range 0 .. 0;
      RESET          at 0 range 1 .. 1;
      SUSP           at 0 range 2 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      TCIE           at 0 range 8 .. 8;
      HTIE           at 0 range 9 .. 9;
      DTEIE          at 0 range 10 .. 10;
      ULEIE          at 0 range 11 .. 11;
      USEIE          at 0 range 12 .. 12;
      SUSPIE         at 0 range 13 .. 13;
      TOIE           at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      LSM            at 0 range 16 .. 16;
      LAP            at 0 range 17 .. 17;
      Reserved_18_21 at 0 range 18 .. 21;
      PRIO           at 0 range 22 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype GPDMA_C1TR1_SDW_LOG2_Field is HAL.UInt2;
   subtype GPDMA_C1TR1_SBL_1_Field is HAL.UInt6;
   subtype GPDMA_C1TR1_PAM_Field is HAL.UInt2;
   subtype GPDMA_C1TR1_DDW_LOG2_Field is HAL.UInt2;
   subtype GPDMA_C1TR1_DBL_1_Field is HAL.UInt6;

   --  GPDMA channel 1 transfer register 1
   type GPDMA_C1TR1_Register is record
      --  binary logarithm of the source data width of a burst in bytes 	Note:
      --  Setting a 8-byte data width causes a user setting error to be
      --  reported and no transfer is issued. 	A source block size must be a
      --  multiple of the source data width (GPDMA_CxBR1.BNDT[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and no
      --  transfer is issued. 	A source single transfer must have an aligned
      --  address with its data width (start address GPDMA_CxSAR[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and none
      --  transfer is issued.
      SDW_LOG2       : GPDMA_C1TR1_SDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  source incrementing burst 	The source address, pointed by
      --  GPDMA_CxSAR, is kept constant after a burst beat/single transfer or
      --  is incremented by the offset value corresponding to a contiguous data
      --  after a burst beat/single transfer.
      SINC           : Boolean := False;
      --  source burst length minus 1, between 0 and 63 	The burst length unit
      --  is one data named beat within a burst. If SBL_1[5:0] =0 , the burst
      --  can be named as single. Each data/beat has a width defined by the
      --  destination data width SDW_LOG2[1:0]. 	Note: If a burst transfer
      --  crossed a 1-Kbyte address boundary on a AHB transfer, the GPDMA
      --  modifies and shortens the programmed burst into singles or bursts of
      --  lower length, to be compliant with the AHB protocol. 	If a burst
      --  transfer is of length greater than the FIFO size of the channel x,
      --  the GPDMA modifies and shortens the programmed burst into singles or
      --  bursts of lower length, to be compliant with the FIFO size. Transfer
      --  performance is lower, with GPDMA re-arbitration between effective and
      --  lower bursts/singles, but the data integrity is guaranteed.
      SBL_1          : GPDMA_C1TR1_SBL_1_Field := 16#0#;
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  padding/alignment mode 	If DDW_LOG2[1:0] = SDW_LOG2[1:0]: if the data
      --  width of a burst destination transfer is equal to the data width of a
      --  burst source transfer, these bits are ignored. 	Else: 	- Case 1: If
      --  destination data width > source data width 	1x: successive source
      --  data are FIFO queued and packed at the destination data width, in a
      --  left (LSB) to right (MSB) order (named little endian), before a
      --  destination transfer 	- Case 2: If destination data width < source
      --  data width 	1x: source data is FIFO queued and unpacked at the
      --  destination data width, to be transferred in a left (LSB) to right
      --  (MSB) order (named little endian) to the destination 	Note:
      PAM            : GPDMA_C1TR1_PAM_Field := 16#0#;
      --  source byte exchange within the unaligned half-word of each source
      --  word 	If the source data width is shorter than a word, this bit is
      --  ignored. 	If the source data width is a word:
      SBX            : Boolean := False;
      --  source allocated port 	This bit is used to allocate the master port
      --  for the source transfer 	Note: This bit must be written when
      --  EN = 0. This bit is read-only when EN = 1.
      SAP            : Boolean := False;
      --  security attribute of the GPDMA transfer from the source 	If
      --  GPDMA_SECCFGR.SECx = 1 and the access is secure: 	This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  GPDMA_SECCFGR.SECx =1 . A secure write is ignored when
      --  GPDMA_SECCFGR.SECx = 0. 	When GPDMA_SECCFGR.SECx is de-asserted,
      --  this SSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the GPDMA transfer
      --  from the source is non-secure.
      SSEC           : Boolean := False;
      --  binary logarithm of the destination data width of a burst, in bytes
      --  	Note: Setting a 8-byte data width causes a user setting error to be
      --  reported and none transfer is issued. 	A destination burst transfer
      --  must have an aligned address with its data width (start address
      --  GPDMA_CxDAR[2:0] and address offset GPDMA_CxTR3.DAO[2:0], versus
      --  DDW_LOG2[1:0]). Otherwise a user setting error is reported and no
      --  transfer is issued.
      DDW_LOG2       : GPDMA_C1TR1_DDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_18_18 : HAL.Bit := 16#0#;
      --  destination incrementing burst 	The destination address, pointed by
      --  GPDMA_CxDAR, is kept constant after a burst beat/single transfer, or
      --  is incremented by the offset value corresponding to a contiguous data
      --  after a burst beat/single transfer.
      DINC           : Boolean := False;
      --  destination burst length minus 1, between 0 and 63 	The burst length
      --  unit is one data named beat within a burst. If DBL_1[5:0] =0 , the
      --  burst can be named as single. Each data/beat has a width defined by
      --  the destination data width DDW_LOG2[1:0]. 	Note: If a burst transfer
      --  crossed a 1-Kbyte address boundary on a AHB transfer, the GPDMA
      --  modifies and shortens the programmed burst into singles or bursts of
      --  lower length, to be compliant with the AHB protocol. 	If a burst
      --  transfer is of length greater than the FIFO size of the channel x,
      --  the GPDMA modifies and shortens the programmed burst into singles or
      --  bursts of lower length, to be compliant with the FIFO size. Transfer
      --  performance is lower, with GPDMA re-arbitration between effective and
      --  lower bursts/singles, but the data integrity is guaranteed.
      DBL_1          : GPDMA_C1TR1_DBL_1_Field := 16#0#;
      --  destination byte exchange 	If the destination data size is a byte,
      --  this bit is ignored. 	If the destination data size is not a byte:
      DBX            : Boolean := False;
      --  destination half-word exchange 	If the destination data size is
      --  shorter than a word, this bit is ignored. 	If the destination data
      --  size is a word:
      DHX            : Boolean := False;
      --  unspecified
      Reserved_28_29 : HAL.UInt2 := 16#0#;
      --  destination allocated port 	This bit is used to allocate the master
      --  port for the destination transfer 	Note: This bit must be written
      --  when EN = 0. This bit is read-only when EN = 1.
      DAP            : Boolean := False;
      --  security attribute of the GPDMA transfer to the destination 	If
      --  GPDMA_SECCFGR.SECx = 1 and the access is secure: 	This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  GPDMA_SECCFGR.SECx = 1. A secure write is ignored when
      --  GPDMA_SECCFGR.SECx = 0. 	When GPDMA_SECCFGR.SECx is de-asserted,
      --  this DSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the GPDMA transfer
      --  to the destination is non-secure.
      DSEC           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C1TR1_Register use record
      SDW_LOG2       at 0 range 0 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      SINC           at 0 range 3 .. 3;
      SBL_1          at 0 range 4 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      PAM            at 0 range 11 .. 12;
      SBX            at 0 range 13 .. 13;
      SAP            at 0 range 14 .. 14;
      SSEC           at 0 range 15 .. 15;
      DDW_LOG2       at 0 range 16 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      DINC           at 0 range 19 .. 19;
      DBL_1          at 0 range 20 .. 25;
      DBX            at 0 range 26 .. 26;
      DHX            at 0 range 27 .. 27;
      Reserved_28_29 at 0 range 28 .. 29;
      DAP            at 0 range 30 .. 30;
      DSEC           at 0 range 31 .. 31;
   end record;

   subtype GPDMA_C1TR2_REQSEL_Field is HAL.UInt7;
   subtype GPDMA_C1TR2_TRIGM_Field is HAL.UInt2;
   subtype GPDMA_C1TR2_TRIGSEL_Field is HAL.UInt6;
   subtype GPDMA_C1TR2_TRIGPOL_Field is HAL.UInt2;
   subtype GPDMA_C1TR2_TCEM_Field is HAL.UInt2;

   --  GPDMA channel 1 transfer register 2
   type GPDMA_C1TR2_Register is record
      --  GPDMA hardware request selection 	These bits are ignored if channel x
      --  is activated (GPDMA_CxCR.EN asserted) with SWREQ = 1 (software
      --  request for a memory-to-memory transfer). Else, the selected hardware
      --  request is internally taken into account as per . 	The user must not
      --  assign a same input hardware request (same REQSEL[6:0] value) to
      --  different active GPDMA channels (GPDMA_CxCR.EN = 1 and
      --  GPDMA_CxTR2.SWREQ = 0 for these channels). GPDMA is not intended to
      --  hardware support the case of simultaneous enabled channels
      --  incorrectly configured with a same hardware peripheral request
      --  signal, and there is no user setting error reporting.
      REQSEL         : GPDMA_C1TR2_REQSEL_Field := 16#0#;
      --  unspecified
      Reserved_7_8   : HAL.UInt2 := 16#0#;
      --  software request 	This bit is internally taken into account when
      --  GPDMA_CxCR.EN is asserted.
      SWREQ          : Boolean := False;
      --  destination hardware request 	This bit is ignored if channel x is
      --  activated (GPDMA_CxCR.EN asserted) with SWREQ = 1 (software request
      --  for a memory-to-memory transfer). Else: 	Note:
      DREQ           : Boolean := False;
      --  Block hardware request 	If the channel x is activated (GPDMA_CxCR.EN
      --  asserted) with SWREQ = 1 (software request for a memory-to-memory
      --  transfer), this bit is ignored. Else:
      BREQ           : Boolean := False;
      --  unspecified
      Reserved_12_13 : HAL.UInt2 := 16#0#;
      --  trigger mode 	These bits define the transfer granularity for its
      --  conditioning by the trigger. 	If the channel x is enabled
      --  (GPDMA_CxCR.EN asserted) with TRIGPOL[1:0] = 00 or 11, these
      --  TRIGM[1:0] bits are ignored. 	Else, a GPDMA transfer is conditioned
      --  by at least one trigger hit: 	first burst read of a 2D/repeated block
      --  transfer is conditioned by one hit trigger. 	– If the peripheral is
      --  programmed as a source (DREQ = 0) of the LLI data transfer, each
      --  programmed burst read is conditioned. 	– If the peripheral is
      --  programmed as a destination (DREQ = 1) of the LLI data transfer,
      --  each programmed burst write is conditioned. The first memory burst
      --  read of a (possibly 2D/repeated) block, also named as the first ready
      --  FIFO-based source burst, is gated by the occurrence of both the
      --  hardware request and the first trigger hit. 	The GPDMA monitoring of
      --  a trigger for channel x is started when the channel is enabled/loaded
      --  with a new active trigger configuration: rising or falling edge on a
      --  selected trigger (TRIGPOL[1:0] = 01 or respectively
      --  TRIGPOL[1:0] = 10). 	The monitoring of this trigger is kept active
      --  during the triggered and uncompleted (data or link) transfer; and if
      --  a new trigger is detected then, this hit is internally memorized to
      --  grant the next transfer, as long as the defined rising or falling
      --  edge is not modified, and the TRIGSEL[5:0] is not modified, and the
      --  channel is enabled. 	Transferring a next LLIn+1 that updates the
      --  GPDMA_CxTR2 with a new value for any of TRIGSEL[5:0] or TRIGPOL[1:0],
      --  resets the monitoring, trashing the memorized hit of the formerly
      --  defined LLIn trigger. 	After a first new trigger hitn+1 is memorized,
      --  if another second trigger hitn+2 is detected and if the hitn
      --  triggered transfer is still not completed, hitn+2 is lost and not
      --  memorized.memorized. A trigger overrun flag is reported
      --  (GPDMA_CxSR.TOF =1 ), and an interrupt is generated if enabled
      --  (GPDMA_CxCR.TOIE = 1). The channel is not automatically disabled by
      --  hardware due to a trigger overrun. 	Note: When the source block size
      --  is not a multiple of the source burst size and is a multiple of the
      --  source data width, then the last programmed source burst is not
      --  completed and is internally shorten to match the block size. In this
      --  case, if TRIGM[1:0] = 11 and (SWREQ =1  or (SWREQ = 0 and
      --  DREQ =0 )), the shortened burst transfer (by singles or/and by
      --  bursts of lower length) is conditioned once by the trigger. 	When the
      --  programmed destination burst is internally shortened by singles
      --  or/and by bursts of lower length (versus FIFO size, versus block
      --  size, 1-Kbyte boundary address crossing): if the trigger is
      --  conditioning the programmed destination burst (if TRIGM[1:0] = 11
      --  and SWREQ = 0 and DREQ = 1), this shortened destination burst
      --  transfer is conditioned once by the trigger.
      TRIGM          : GPDMA_C1TR2_TRIGM_Field := 16#0#;
      --  trigger event input selection 	These bits select the trigger event
      --  input of the GPDMA transfer (as per ), with an active trigger event
      --  if TRIGPOL[1:0] ≠ 00.
      TRIGSEL        : GPDMA_C1TR2_TRIGSEL_Field := 16#0#;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  trigger event polarity 	These bits define the polarity of the
      --  selected trigger event input defined by TRIGSEL[5:0].
      TRIGPOL        : GPDMA_C1TR2_TRIGPOL_Field := 16#0#;
      --  unspecified
      Reserved_26_29 : HAL.UInt4 := 16#0#;
      --  transfer complete event mode 	These bits define the transfer
      --  granularity for the transfer complete and half transfer complete
      --  events generation. 	Note: If the initial LLI0 data transfer is
      --  null/void (directly programmed by the internal register file with
      --  GPDMA_CxBR1.BNDT[15:0] = 0), then neither the complete transfer
      --  event nor the half transfer event is generated. 	Note: If the initial
      --  LLI0 data transfer is null/void (directly programmed by the internal
      --  register file with GPDMA_CxBR1.BNDT[15:0] = 0), then neither the
      --  complete transfer event nor the half transfer event is generated.
      --  	Note: If the initial LLI0 data transfer is null/void (i.e. directly
      --  programmed by the internal register file with
      --  GPDMA_CxBR1.BNDT[15:0] =0 ), then the half transfer event is not
      --  generated, and the transfer complete event is generated when is
      --  completed the loading of the LLI1.
      TCEM           : GPDMA_C1TR2_TCEM_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C1TR2_Register use record
      REQSEL         at 0 range 0 .. 6;
      Reserved_7_8   at 0 range 7 .. 8;
      SWREQ          at 0 range 9 .. 9;
      DREQ           at 0 range 10 .. 10;
      BREQ           at 0 range 11 .. 11;
      Reserved_12_13 at 0 range 12 .. 13;
      TRIGM          at 0 range 14 .. 15;
      TRIGSEL        at 0 range 16 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      TRIGPOL        at 0 range 24 .. 25;
      Reserved_26_29 at 0 range 26 .. 29;
      TCEM           at 0 range 30 .. 31;
   end record;

   subtype GPDMA_C1BR1_BNDT_Field is HAL.UInt16;

   --  GPDMA channel 1 block register 1
   type GPDMA_C1BR1_Register is record
      --  block number of data bytes to transfer from the source 	Block size
      --  transferred from the source. When the channel is enabled, this field
      --  becomes read-only and is decremented, indicating the remaining number
      --  of data items in the current source block to be transferred.
      --  BNDT[15:0] is programmed in number of bytes, maximum source block
      --  size is 64 Kbytes -1. 	Once the last data transfer is completed
      --  (BNDT[15:0] = 0): 	- if GPDMA_CxLLR.UB1 = 1, this field is
      --  updated by the LLI in the memory. 	- if GPDMA_CxLLR.UB1 = 0 and if
      --  there is at least one non null Uxx update bit, this field is
      --  internally restored to the programmed value. 	- if all
      --  GPDMA_CxLLR.Uxx = 0 and if GPDMA_CxLLR.LA[15:0] = 0, this field
      --  is internally restored to the programmed value (infinite/continuous
      --  last LLI). 	- if GPDMA_CxLLR = 0, this field is kept as zero
      --  following the last LLI data transfer. 	Note: A non-null source block
      --  size must be a multiple of the source data width (BNDT[2:0] versus
      --  GPDMA_CxTR1.SDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued. 	When configured in packing mode
      --  (GPDMA_CxTR1.PAM[1] = 1 and destination data width different from
      --  source data width), a non-null source block size must be a multiple
      --  of the destination data width (BNDT[2:0] versus
      --  GPDMA_CxTR1.DDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued.
      BNDT           : GPDMA_C1BR1_BNDT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C1BR1_Register use record
      BNDT           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype GPDMA_C1LLR_LA_Field is HAL.UInt14;

   --  GPDMA channel 1 linked-list address register
   type GPDMA_C1LLR_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  pointer (16-bit low-significant address) to the next linked-list data
      --  structure 	If UT1 = UT2 = UB1 = USA = UDA = ULL = 0 and
      --  if LA[15:20] = 0, the current LLI is the last one. The channel
      --  transfer is completed without any update of the linked-list GPDMA
      --  register file. 	Else, this field is the pointer to the memory address
      --  offset from which the next linked-list data structure is
      --  automatically fetched from, once the data transfer is completed, in
      --  order to conditionally update the linked-list GPDMA internal register
      --  file (GPDMA_CxCTR1, GPDMA_CxTR2, GPDMA_CxBR1, GPDMA_CxSAR,
      --  GPDMA_CxDAR and GPDMA_CxLLR). 	Note: The user must program the
      --  pointer to be 32-bit aligned. The two low-significant bits are write
      --  ignored.
      LA             : GPDMA_C1LLR_LA_Field := 16#0#;
      --  Update GPDMA_CxLLR register from memory 	This bit is used to control
      --  the update of GPDMA_CxLLR from the memory during the link transfer.
      ULL            : Boolean := False;
      --  unspecified
      Reserved_17_26 : HAL.UInt10 := 16#0#;
      --  Update GPDMA_CxDAR register from memory 	This bit is used to control
      --  the update of GPDMA_CxDAR from the memory during the link transfer.
      UDA            : Boolean := False;
      --  update GPDMA_CxSAR from memory 	This bit controls the update of
      --  GPDMA_CxSAR from the memory during the link transfer.
      USA            : Boolean := False;
      --  Update GPDMA_CxBR1 from memory 	This bit controls the update of
      --  GPDMA_CxBR1 from the memory during the link transfer. If UB1 = 0
      --  and if GPDMA_CxLLR ≠ 0, the linked-list is not completed.
      --  GPDMA_CxBR1.BNDT[15:0] is then restored to the programmed value after
      --  data transfer is completed and before the link transfer.
      UB1            : Boolean := False;
      --  Update GPDMA_CxTR2 from memory 	This bit controls the update of
      --  GPDMA_CxTR2 from the memory during the link transfer.
      UT2            : Boolean := False;
      --  Update GPDMA_CxTR1 from memory 	This bit controls the update of
      --  GPDMA_CxTR1 from the memory during the link transfer.
      UT1            : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C1LLR_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      LA             at 0 range 2 .. 15;
      ULL            at 0 range 16 .. 16;
      Reserved_17_26 at 0 range 17 .. 26;
      UDA            at 0 range 27 .. 27;
      USA            at 0 range 28 .. 28;
      UB1            at 0 range 29 .. 29;
      UT2            at 0 range 30 .. 30;
      UT1            at 0 range 31 .. 31;
   end record;

   subtype GPDMA_C2LBAR_LBA_Field is HAL.UInt16;

   --  GPDMA channel 2 linked-list base address register
   type GPDMA_C2LBAR_Register is record
      --  unspecified
      Reserved_0_15 : HAL.UInt16 := 16#0#;
      --  linked-list base address of GPDMA channel x
      LBA           : GPDMA_C2LBAR_LBA_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C2LBAR_Register use record
      Reserved_0_15 at 0 range 0 .. 15;
      LBA           at 0 range 16 .. 31;
   end record;

   --  GPDMA channel 2 flag clear register
   type GPDMA_C2FCR_Register is record
      --  unspecified
      Reserved_0_7   : HAL.UInt8 := 16#0#;
      --  Write-only. transfer complete flag clear
      TCF            : Boolean := False;
      --  Write-only. half transfer flag clear
      HTF            : Boolean := False;
      --  Write-only. data transfer error flag clear
      DTEF           : Boolean := False;
      --  Write-only. update link transfer error flag clear
      ULEF           : Boolean := False;
      --  Write-only. user setting error flag clear
      USEF           : Boolean := False;
      --  Write-only. completed suspension flag clear
      SUSPF          : Boolean := False;
      --  Write-only. trigger overrun flag clear
      TOF            : Boolean := False;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C2FCR_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      TCF            at 0 range 8 .. 8;
      HTF            at 0 range 9 .. 9;
      DTEF           at 0 range 10 .. 10;
      ULEF           at 0 range 11 .. 11;
      USEF           at 0 range 12 .. 12;
      SUSPF          at 0 range 13 .. 13;
      TOF            at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype GPDMA_C2SR_FIFOL_Field is HAL.UInt8;

   --  GPDMA channel 2 status register
   type GPDMA_C2SR_Register is record
      --  Read-only. idle flag 	This idle flag is de-asserted by hardware when
      --  the channel is enabled (GPDMA_CxCR.EN = 1) with a valid channel
      --  configuration (no USEF to be immediately reported). 	This idle flag
      --  is asserted after hard reset or by hardware when the channel is back
      --  in idle state (in suspended or disabled state).
      IDLEF          : Boolean;
      --  unspecified
      Reserved_1_7   : HAL.UInt7;
      --  Read-only. transfer complete flag 	A transfer complete event is
      --  either a block transfer complete, a 2D/repeated block transfer
      --  complete, a LLI transfer complete including the upload of the next
      --  LLI if any, or the full linked-list completion, depending on the
      --  transfer complete event mode (GPDMA_CxTR2.TCEM[1:0]).
      TCF            : Boolean;
      --  Read-only. half transfer flag 	An half transfer event is either an
      --  half block transfer or an half 2D/repeated block transfer, depending
      --  on the transfer complete event mode (GPDMA_CxTR2.TCEM[1:0]). 	An half
      --  block transfer occurs when half of the bytes of the source block size
      --  (rounded up integer of GPDMA_CxBR1.BNDT[15:0]/2) has been transferred
      --  to the destination. 	An half 2D/repeated block transfer occurs when
      --  half of the repeated blocks (rounded up integer of
      --  (GPDMA_CxBR1.BRC[10:0]+1)/2)) has been transferred to the
      --  destination.
      HTF            : Boolean;
      --  Read-only. data transfer error flag
      DTEF           : Boolean;
      --  Read-only. update link transfer error flag
      ULEF           : Boolean;
      --  Read-only. user setting error flag
      USEF           : Boolean;
      --  Read-only. completed suspension flag
      SUSPF          : Boolean;
      --  Read-only. trigger overrun flag
      TOF            : Boolean;
      --  unspecified
      Reserved_15_15 : HAL.Bit;
      --  Read-only. monitored FIFO level 	Number of available write beats in
      --  the FIFO, in units of the programmed destination data width (see
      --  GPDMA_CxTR1.DDW_LOG2[1:0], in units of bytes, half-words, or words).
      --  	Note: After having suspended an active transfer, the user may need
      --  to read FIFOL[7:0], additionally to GPDMA_CxBR1.BDNT[15:0] and
      --  GPDMA_CxBR1.BRC[10:0], to know how many data have been transferred to
      --  the destination. Before reading, the user may wait for the transfer
      --  to be suspended (GPDMA_CxSR.SUSPF = 1).
      FIFOL          : GPDMA_C2SR_FIFOL_Field;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C2SR_Register use record
      IDLEF          at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      TCF            at 0 range 8 .. 8;
      HTF            at 0 range 9 .. 9;
      DTEF           at 0 range 10 .. 10;
      ULEF           at 0 range 11 .. 11;
      USEF           at 0 range 12 .. 12;
      SUSPF          at 0 range 13 .. 13;
      TOF            at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      FIFOL          at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype GPDMA_C2CR_PRIO_Field is HAL.UInt2;

   --  GPDMA channel 2 control register
   type GPDMA_C2CR_Register is record
      --  enable 	Writing 1 into the field RESET (bit 1) causes the hardware to
      --  de-assert this bit, whatever is written into this bit 0. Else: 	this
      --  bit is de-asserted by hardware when there is a transfer error (master
      --  bus error or user setting error) or when there is a channel transfer
      --  complete (channel ready to be configured, e.g. if LSM=1 at the end of
      --  a single execution of the LLI). 	Else, this bit can be asserted by
      --  software. 	Writing 0 into this EN bit is ignored.
      EN             : Boolean := False;
      --  Write-only. reset 	This bit is write only. Writing 0 has no impact.
      --  Writing 1 implies the reset of the following: the FIFO, the channel
      --  internal state, SUSP and EN bits (whatever is written receptively in
      --  bit 2 and bit 0). 	The reset is effective when the channel is in
      --  steady state, meaning one of the following: 	- active channel in
      --  suspended state (GPDMA_CxSR.SUSPF = 1 and
      --  GPDMA_CxSR.IDLEF = GPDMA_CxCR.EN = 1) 	- channel in disabled
      --  state (GPDMA_CxSR.IDLEF = 1 and GPDMA_CxCR.EN = 0). 	After
      --  writing a RESET, to continue using this channel, the user must
      --  explicitly reconfigure the channel including the hardware-modified
      --  configuration registers (GPDMA_CxBR1, GPDMA_CxSAR and GPDMA_CxDAR)
      --  before enabling again the channel (see the programming sequence in ).
      RESET          : Boolean := False;
      --  suspend 	Writing 1 into the field RESET (bit 1) causes the hardware
      --  to de-assert this bit, whatever is written into this bit 2. Else:
      --  	Software must write 1 in order to suspend an active channel i.e. a
      --  channel with an on-going GPDMA transfer over its master ports. 	The
      --  software must write 0 in order to resume a suspended channel,
      --  following the programming sequence detailed in .
      SUSP           : Boolean := False;
      --  unspecified
      Reserved_3_7   : HAL.UInt5 := 16#0#;
      --  transfer complete interrupt enable
      TCIE           : Boolean := False;
      --  half transfer complete interrupt enable
      HTIE           : Boolean := False;
      --  data transfer error interrupt enable
      DTEIE          : Boolean := False;
      --  update link transfer error interrupt enable
      ULEIE          : Boolean := False;
      --  user setting error interrupt enable
      USEIE          : Boolean := False;
      --  completed suspension interrupt enable
      SUSPIE         : Boolean := False;
      --  trigger overrun interrupt enable
      TOIE           : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Link step mode 	First the (possible 1D/repeated) block transfer is
      --  executed as defined by the current internal register file until
      --  GPDMA_CxBR1.BNDT[15:0] = 0 and GPDMA_CxBR1.BRC[10:0] = 0 if
      --  present. Secondly the next linked-list data structure is
      --  conditionally uploaded from memory as defined by GPDMA_CxLLR. Then
      --  channel execution is completed. 	Note: This bit must be written when
      --  EN=0. This bit is read-only when EN=1.
      LSM            : Boolean := False;
      --  linked-list allocated port 	This bit is used to allocate the master
      --  port for the update of the GPDMA linked-list registers from the
      --  memory. 	Note: This bit must be written when EN=0. This bit is
      --  read-only when EN=1.
      LAP            : Boolean := False;
      --  unspecified
      Reserved_18_21 : HAL.UInt4 := 16#0#;
      --  priority level of the channel x GPDMA transfer versus others 	Note:
      --  This bit must be written when EN = 0. This bit is read-only when
      --  EN = 1.
      PRIO           : GPDMA_C2CR_PRIO_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C2CR_Register use record
      EN             at 0 range 0 .. 0;
      RESET          at 0 range 1 .. 1;
      SUSP           at 0 range 2 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      TCIE           at 0 range 8 .. 8;
      HTIE           at 0 range 9 .. 9;
      DTEIE          at 0 range 10 .. 10;
      ULEIE          at 0 range 11 .. 11;
      USEIE          at 0 range 12 .. 12;
      SUSPIE         at 0 range 13 .. 13;
      TOIE           at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      LSM            at 0 range 16 .. 16;
      LAP            at 0 range 17 .. 17;
      Reserved_18_21 at 0 range 18 .. 21;
      PRIO           at 0 range 22 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype GPDMA_C2TR1_SDW_LOG2_Field is HAL.UInt2;
   subtype GPDMA_C2TR1_SBL_1_Field is HAL.UInt6;
   subtype GPDMA_C2TR1_PAM_Field is HAL.UInt2;
   subtype GPDMA_C2TR1_DDW_LOG2_Field is HAL.UInt2;
   subtype GPDMA_C2TR1_DBL_1_Field is HAL.UInt6;

   --  GPDMA channel 2 transfer register 1
   type GPDMA_C2TR1_Register is record
      --  binary logarithm of the source data width of a burst in bytes 	Note:
      --  Setting a 8-byte data width causes a user setting error to be
      --  reported and no transfer is issued. 	A source block size must be a
      --  multiple of the source data width (GPDMA_CxBR1.BNDT[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and no
      --  transfer is issued. 	A source single transfer must have an aligned
      --  address with its data width (start address GPDMA_CxSAR[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and none
      --  transfer is issued.
      SDW_LOG2       : GPDMA_C2TR1_SDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  source incrementing burst 	The source address, pointed by
      --  GPDMA_CxSAR, is kept constant after a burst beat/single transfer or
      --  is incremented by the offset value corresponding to a contiguous data
      --  after a burst beat/single transfer.
      SINC           : Boolean := False;
      --  source burst length minus 1, between 0 and 63 	The burst length unit
      --  is one data named beat within a burst. If SBL_1[5:0] =0 , the burst
      --  can be named as single. Each data/beat has a width defined by the
      --  destination data width SDW_LOG2[1:0]. 	Note: If a burst transfer
      --  crossed a 1-Kbyte address boundary on a AHB transfer, the GPDMA
      --  modifies and shortens the programmed burst into singles or bursts of
      --  lower length, to be compliant with the AHB protocol. 	If a burst
      --  transfer is of length greater than the FIFO size of the channel x,
      --  the GPDMA modifies and shortens the programmed burst into singles or
      --  bursts of lower length, to be compliant with the FIFO size. Transfer
      --  performance is lower, with GPDMA re-arbitration between effective and
      --  lower bursts/singles, but the data integrity is guaranteed.
      SBL_1          : GPDMA_C2TR1_SBL_1_Field := 16#0#;
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  padding/alignment mode 	If DDW_LOG2[1:0] = SDW_LOG2[1:0]: if the data
      --  width of a burst destination transfer is equal to the data width of a
      --  burst source transfer, these bits are ignored. 	Else: 	- Case 1: If
      --  destination data width > source data width 	1x: successive source
      --  data are FIFO queued and packed at the destination data width, in a
      --  left (LSB) to right (MSB) order (named little endian), before a
      --  destination transfer 	- Case 2: If destination data width < source
      --  data width 	1x: source data is FIFO queued and unpacked at the
      --  destination data width, to be transferred in a left (LSB) to right
      --  (MSB) order (named little endian) to the destination 	Note:
      PAM            : GPDMA_C2TR1_PAM_Field := 16#0#;
      --  source byte exchange within the unaligned half-word of each source
      --  word 	If the source data width is shorter than a word, this bit is
      --  ignored. 	If the source data width is a word:
      SBX            : Boolean := False;
      --  source allocated port 	This bit is used to allocate the master port
      --  for the source transfer 	Note: This bit must be written when
      --  EN = 0. This bit is read-only when EN = 1.
      SAP            : Boolean := False;
      --  security attribute of the GPDMA transfer from the source 	If
      --  GPDMA_SECCFGR.SECx = 1 and the access is secure: 	This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  GPDMA_SECCFGR.SECx =1 . A secure write is ignored when
      --  GPDMA_SECCFGR.SECx = 0. 	When GPDMA_SECCFGR.SECx is de-asserted,
      --  this SSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the GPDMA transfer
      --  from the source is non-secure.
      SSEC           : Boolean := False;
      --  binary logarithm of the destination data width of a burst, in bytes
      --  	Note: Setting a 8-byte data width causes a user setting error to be
      --  reported and none transfer is issued. 	A destination burst transfer
      --  must have an aligned address with its data width (start address
      --  GPDMA_CxDAR[2:0] and address offset GPDMA_CxTR3.DAO[2:0], versus
      --  DDW_LOG2[1:0]). Otherwise a user setting error is reported and no
      --  transfer is issued.
      DDW_LOG2       : GPDMA_C2TR1_DDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_18_18 : HAL.Bit := 16#0#;
      --  destination incrementing burst 	The destination address, pointed by
      --  GPDMA_CxDAR, is kept constant after a burst beat/single transfer, or
      --  is incremented by the offset value corresponding to a contiguous data
      --  after a burst beat/single transfer.
      DINC           : Boolean := False;
      --  destination burst length minus 1, between 0 and 63 	The burst length
      --  unit is one data named beat within a burst. If DBL_1[5:0] =0 , the
      --  burst can be named as single. Each data/beat has a width defined by
      --  the destination data width DDW_LOG2[1:0]. 	Note: If a burst transfer
      --  crossed a 1-Kbyte address boundary on a AHB transfer, the GPDMA
      --  modifies and shortens the programmed burst into singles or bursts of
      --  lower length, to be compliant with the AHB protocol. 	If a burst
      --  transfer is of length greater than the FIFO size of the channel x,
      --  the GPDMA modifies and shortens the programmed burst into singles or
      --  bursts of lower length, to be compliant with the FIFO size. Transfer
      --  performance is lower, with GPDMA re-arbitration between effective and
      --  lower bursts/singles, but the data integrity is guaranteed.
      DBL_1          : GPDMA_C2TR1_DBL_1_Field := 16#0#;
      --  destination byte exchange 	If the destination data size is a byte,
      --  this bit is ignored. 	If the destination data size is not a byte:
      DBX            : Boolean := False;
      --  destination half-word exchange 	If the destination data size is
      --  shorter than a word, this bit is ignored. 	If the destination data
      --  size is a word:
      DHX            : Boolean := False;
      --  unspecified
      Reserved_28_29 : HAL.UInt2 := 16#0#;
      --  destination allocated port 	This bit is used to allocate the master
      --  port for the destination transfer 	Note: This bit must be written
      --  when EN = 0. This bit is read-only when EN = 1.
      DAP            : Boolean := False;
      --  security attribute of the GPDMA transfer to the destination 	If
      --  GPDMA_SECCFGR.SECx = 1 and the access is secure: 	This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  GPDMA_SECCFGR.SECx = 1. A secure write is ignored when
      --  GPDMA_SECCFGR.SECx = 0. 	When GPDMA_SECCFGR.SECx is de-asserted,
      --  this DSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the GPDMA transfer
      --  to the destination is non-secure.
      DSEC           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C2TR1_Register use record
      SDW_LOG2       at 0 range 0 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      SINC           at 0 range 3 .. 3;
      SBL_1          at 0 range 4 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      PAM            at 0 range 11 .. 12;
      SBX            at 0 range 13 .. 13;
      SAP            at 0 range 14 .. 14;
      SSEC           at 0 range 15 .. 15;
      DDW_LOG2       at 0 range 16 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      DINC           at 0 range 19 .. 19;
      DBL_1          at 0 range 20 .. 25;
      DBX            at 0 range 26 .. 26;
      DHX            at 0 range 27 .. 27;
      Reserved_28_29 at 0 range 28 .. 29;
      DAP            at 0 range 30 .. 30;
      DSEC           at 0 range 31 .. 31;
   end record;

   subtype GPDMA_C2TR2_REQSEL_Field is HAL.UInt7;
   subtype GPDMA_C2TR2_TRIGM_Field is HAL.UInt2;
   subtype GPDMA_C2TR2_TRIGSEL_Field is HAL.UInt6;
   subtype GPDMA_C2TR2_TRIGPOL_Field is HAL.UInt2;
   subtype GPDMA_C2TR2_TCEM_Field is HAL.UInt2;

   --  GPDMA channel 2 transfer register 2
   type GPDMA_C2TR2_Register is record
      --  GPDMA hardware request selection 	These bits are ignored if channel x
      --  is activated (GPDMA_CxCR.EN asserted) with SWREQ = 1 (software
      --  request for a memory-to-memory transfer). Else, the selected hardware
      --  request is internally taken into account as per . 	The user must not
      --  assign a same input hardware request (same REQSEL[6:0] value) to
      --  different active GPDMA channels (GPDMA_CxCR.EN = 1 and
      --  GPDMA_CxTR2.SWREQ = 0 for these channels). GPDMA is not intended to
      --  hardware support the case of simultaneous enabled channels
      --  incorrectly configured with a same hardware peripheral request
      --  signal, and there is no user setting error reporting.
      REQSEL         : GPDMA_C2TR2_REQSEL_Field := 16#0#;
      --  unspecified
      Reserved_7_8   : HAL.UInt2 := 16#0#;
      --  software request 	This bit is internally taken into account when
      --  GPDMA_CxCR.EN is asserted.
      SWREQ          : Boolean := False;
      --  destination hardware request 	This bit is ignored if channel x is
      --  activated (GPDMA_CxCR.EN asserted) with SWREQ = 1 (software request
      --  for a memory-to-memory transfer). Else: 	Note:
      DREQ           : Boolean := False;
      --  Block hardware request 	If the channel x is activated (GPDMA_CxCR.EN
      --  asserted) with SWREQ = 1 (software request for a memory-to-memory
      --  transfer), this bit is ignored. Else:
      BREQ           : Boolean := False;
      --  unspecified
      Reserved_12_13 : HAL.UInt2 := 16#0#;
      --  trigger mode 	These bits define the transfer granularity for its
      --  conditioning by the trigger. 	If the channel x is enabled
      --  (GPDMA_CxCR.EN asserted) with TRIGPOL[1:0] = 00 or 11, these
      --  TRIGM[1:0] bits are ignored. 	Else, a GPDMA transfer is conditioned
      --  by at least one trigger hit: 	first burst read of a 2D/repeated block
      --  transfer is conditioned by one hit trigger. 	– If the peripheral is
      --  programmed as a source (DREQ = 0) of the LLI data transfer, each
      --  programmed burst read is conditioned. 	– If the peripheral is
      --  programmed as a destination (DREQ = 1) of the LLI data transfer,
      --  each programmed burst write is conditioned. The first memory burst
      --  read of a (possibly 2D/repeated) block, also named as the first ready
      --  FIFO-based source burst, is gated by the occurrence of both the
      --  hardware request and the first trigger hit. 	The GPDMA monitoring of
      --  a trigger for channel x is started when the channel is enabled/loaded
      --  with a new active trigger configuration: rising or falling edge on a
      --  selected trigger (TRIGPOL[1:0] = 01 or respectively
      --  TRIGPOL[1:0] = 10). 	The monitoring of this trigger is kept active
      --  during the triggered and uncompleted (data or link) transfer; and if
      --  a new trigger is detected then, this hit is internally memorized to
      --  grant the next transfer, as long as the defined rising or falling
      --  edge is not modified, and the TRIGSEL[5:0] is not modified, and the
      --  channel is enabled. 	Transferring a next LLIn+1 that updates the
      --  GPDMA_CxTR2 with a new value for any of TRIGSEL[5:0] or TRIGPOL[1:0],
      --  resets the monitoring, trashing the memorized hit of the formerly
      --  defined LLIn trigger. 	After a first new trigger hitn+1 is memorized,
      --  if another second trigger hitn+2 is detected and if the hitn
      --  triggered transfer is still not completed, hitn+2 is lost and not
      --  memorized.memorized. A trigger overrun flag is reported
      --  (GPDMA_CxSR.TOF =1 ), and an interrupt is generated if enabled
      --  (GPDMA_CxCR.TOIE = 1). The channel is not automatically disabled by
      --  hardware due to a trigger overrun. 	Note: When the source block size
      --  is not a multiple of the source burst size and is a multiple of the
      --  source data width, then the last programmed source burst is not
      --  completed and is internally shorten to match the block size. In this
      --  case, if TRIGM[1:0] = 11 and (SWREQ =1  or (SWREQ = 0 and
      --  DREQ =0 )), the shortened burst transfer (by singles or/and by
      --  bursts of lower length) is conditioned once by the trigger. 	When the
      --  programmed destination burst is internally shortened by singles
      --  or/and by bursts of lower length (versus FIFO size, versus block
      --  size, 1-Kbyte boundary address crossing): if the trigger is
      --  conditioning the programmed destination burst (if TRIGM[1:0] = 11
      --  and SWREQ = 0 and DREQ = 1), this shortened destination burst
      --  transfer is conditioned once by the trigger.
      TRIGM          : GPDMA_C2TR2_TRIGM_Field := 16#0#;
      --  trigger event input selection 	These bits select the trigger event
      --  input of the GPDMA transfer (as per ), with an active trigger event
      --  if TRIGPOL[1:0] ≠ 00.
      TRIGSEL        : GPDMA_C2TR2_TRIGSEL_Field := 16#0#;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  trigger event polarity 	These bits define the polarity of the
      --  selected trigger event input defined by TRIGSEL[5:0].
      TRIGPOL        : GPDMA_C2TR2_TRIGPOL_Field := 16#0#;
      --  unspecified
      Reserved_26_29 : HAL.UInt4 := 16#0#;
      --  transfer complete event mode 	These bits define the transfer
      --  granularity for the transfer complete and half transfer complete
      --  events generation. 	Note: If the initial LLI0 data transfer is
      --  null/void (directly programmed by the internal register file with
      --  GPDMA_CxBR1.BNDT[15:0] = 0), then neither the complete transfer
      --  event nor the half transfer event is generated. 	Note: If the initial
      --  LLI0 data transfer is null/void (directly programmed by the internal
      --  register file with GPDMA_CxBR1.BNDT[15:0] = 0), then neither the
      --  complete transfer event nor the half transfer event is generated.
      --  	Note: If the initial LLI0 data transfer is null/void (i.e. directly
      --  programmed by the internal register file with
      --  GPDMA_CxBR1.BNDT[15:0] =0 ), then the half transfer event is not
      --  generated, and the transfer complete event is generated when is
      --  completed the loading of the LLI1.
      TCEM           : GPDMA_C2TR2_TCEM_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C2TR2_Register use record
      REQSEL         at 0 range 0 .. 6;
      Reserved_7_8   at 0 range 7 .. 8;
      SWREQ          at 0 range 9 .. 9;
      DREQ           at 0 range 10 .. 10;
      BREQ           at 0 range 11 .. 11;
      Reserved_12_13 at 0 range 12 .. 13;
      TRIGM          at 0 range 14 .. 15;
      TRIGSEL        at 0 range 16 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      TRIGPOL        at 0 range 24 .. 25;
      Reserved_26_29 at 0 range 26 .. 29;
      TCEM           at 0 range 30 .. 31;
   end record;

   subtype GPDMA_C2BR1_BNDT_Field is HAL.UInt16;

   --  GPDMA channel 2 block register 1
   type GPDMA_C2BR1_Register is record
      --  block number of data bytes to transfer from the source 	Block size
      --  transferred from the source. When the channel is enabled, this field
      --  becomes read-only and is decremented, indicating the remaining number
      --  of data items in the current source block to be transferred.
      --  BNDT[15:0] is programmed in number of bytes, maximum source block
      --  size is 64 Kbytes -1. 	Once the last data transfer is completed
      --  (BNDT[15:0] = 0): 	- if GPDMA_CxLLR.UB1 = 1, this field is
      --  updated by the LLI in the memory. 	- if GPDMA_CxLLR.UB1 = 0 and if
      --  there is at least one non null Uxx update bit, this field is
      --  internally restored to the programmed value. 	- if all
      --  GPDMA_CxLLR.Uxx = 0 and if GPDMA_CxLLR.LA[15:0] = 0, this field
      --  is internally restored to the programmed value (infinite/continuous
      --  last LLI). 	- if GPDMA_CxLLR = 0, this field is kept as zero
      --  following the last LLI data transfer. 	Note: A non-null source block
      --  size must be a multiple of the source data width (BNDT[2:0] versus
      --  GPDMA_CxTR1.SDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued. 	When configured in packing mode
      --  (GPDMA_CxTR1.PAM[1] = 1 and destination data width different from
      --  source data width), a non-null source block size must be a multiple
      --  of the destination data width (BNDT[2:0] versus
      --  GPDMA_CxTR1.DDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued.
      BNDT           : GPDMA_C2BR1_BNDT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C2BR1_Register use record
      BNDT           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype GPDMA_C2LLR_LA_Field is HAL.UInt14;

   --  GPDMA channel 2 linked-list address register
   type GPDMA_C2LLR_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  pointer (16-bit low-significant address) to the next linked-list data
      --  structure 	If UT1 = UT2 = UB1 = USA = UDA = ULL = 0 and
      --  if LA[15:20] = 0, the current LLI is the last one. The channel
      --  transfer is completed without any update of the linked-list GPDMA
      --  register file. 	Else, this field is the pointer to the memory address
      --  offset from which the next linked-list data structure is
      --  automatically fetched from, once the data transfer is completed, in
      --  order to conditionally update the linked-list GPDMA internal register
      --  file (GPDMA_CxCTR1, GPDMA_CxTR2, GPDMA_CxBR1, GPDMA_CxSAR,
      --  GPDMA_CxDAR and GPDMA_CxLLR). 	Note: The user must program the
      --  pointer to be 32-bit aligned. The two low-significant bits are write
      --  ignored.
      LA             : GPDMA_C2LLR_LA_Field := 16#0#;
      --  Update GPDMA_CxLLR register from memory 	This bit is used to control
      --  the update of GPDMA_CxLLR from the memory during the link transfer.
      ULL            : Boolean := False;
      --  unspecified
      Reserved_17_26 : HAL.UInt10 := 16#0#;
      --  Update GPDMA_CxDAR register from memory 	This bit is used to control
      --  the update of GPDMA_CxDAR from the memory during the link transfer.
      UDA            : Boolean := False;
      --  update GPDMA_CxSAR from memory 	This bit controls the update of
      --  GPDMA_CxSAR from the memory during the link transfer.
      USA            : Boolean := False;
      --  Update GPDMA_CxBR1 from memory 	This bit controls the update of
      --  GPDMA_CxBR1 from the memory during the link transfer. If UB1 = 0
      --  and if GPDMA_CxLLR ≠ 0, the linked-list is not completed.
      --  GPDMA_CxBR1.BNDT[15:0] is then restored to the programmed value after
      --  data transfer is completed and before the link transfer.
      UB1            : Boolean := False;
      --  Update GPDMA_CxTR2 from memory 	This bit controls the update of
      --  GPDMA_CxTR2 from the memory during the link transfer.
      UT2            : Boolean := False;
      --  Update GPDMA_CxTR1 from memory 	This bit controls the update of
      --  GPDMA_CxTR1 from the memory during the link transfer.
      UT1            : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C2LLR_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      LA             at 0 range 2 .. 15;
      ULL            at 0 range 16 .. 16;
      Reserved_17_26 at 0 range 17 .. 26;
      UDA            at 0 range 27 .. 27;
      USA            at 0 range 28 .. 28;
      UB1            at 0 range 29 .. 29;
      UT2            at 0 range 30 .. 30;
      UT1            at 0 range 31 .. 31;
   end record;

   subtype GPDMA_C3LBAR_LBA_Field is HAL.UInt16;

   --  GPDMA channel 3 linked-list base address register
   type GPDMA_C3LBAR_Register is record
      --  unspecified
      Reserved_0_15 : HAL.UInt16 := 16#0#;
      --  linked-list base address of GPDMA channel x
      LBA           : GPDMA_C3LBAR_LBA_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C3LBAR_Register use record
      Reserved_0_15 at 0 range 0 .. 15;
      LBA           at 0 range 16 .. 31;
   end record;

   --  GPDMA channel 3 flag clear register
   type GPDMA_C3FCR_Register is record
      --  unspecified
      Reserved_0_7   : HAL.UInt8 := 16#0#;
      --  Write-only. transfer complete flag clear
      TCF            : Boolean := False;
      --  Write-only. half transfer flag clear
      HTF            : Boolean := False;
      --  Write-only. data transfer error flag clear
      DTEF           : Boolean := False;
      --  Write-only. update link transfer error flag clear
      ULEF           : Boolean := False;
      --  Write-only. user setting error flag clear
      USEF           : Boolean := False;
      --  Write-only. completed suspension flag clear
      SUSPF          : Boolean := False;
      --  Write-only. trigger overrun flag clear
      TOF            : Boolean := False;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C3FCR_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      TCF            at 0 range 8 .. 8;
      HTF            at 0 range 9 .. 9;
      DTEF           at 0 range 10 .. 10;
      ULEF           at 0 range 11 .. 11;
      USEF           at 0 range 12 .. 12;
      SUSPF          at 0 range 13 .. 13;
      TOF            at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype GPDMA_C3SR_FIFOL_Field is HAL.UInt8;

   --  GPDMA channel 3 status register
   type GPDMA_C3SR_Register is record
      --  Read-only. idle flag 	This idle flag is de-asserted by hardware when
      --  the channel is enabled (GPDMA_CxCR.EN = 1) with a valid channel
      --  configuration (no USEF to be immediately reported). 	This idle flag
      --  is asserted after hard reset or by hardware when the channel is back
      --  in idle state (in suspended or disabled state).
      IDLEF          : Boolean;
      --  unspecified
      Reserved_1_7   : HAL.UInt7;
      --  Read-only. transfer complete flag 	A transfer complete event is
      --  either a block transfer complete, a 2D/repeated block transfer
      --  complete, a LLI transfer complete including the upload of the next
      --  LLI if any, or the full linked-list completion, depending on the
      --  transfer complete event mode (GPDMA_CxTR2.TCEM[1:0]).
      TCF            : Boolean;
      --  Read-only. half transfer flag 	An half transfer event is either an
      --  half block transfer or an half 2D/repeated block transfer, depending
      --  on the transfer complete event mode (GPDMA_CxTR2.TCEM[1:0]). 	An half
      --  block transfer occurs when half of the bytes of the source block size
      --  (rounded up integer of GPDMA_CxBR1.BNDT[15:0]/2) has been transferred
      --  to the destination. 	An half 2D/repeated block transfer occurs when
      --  half of the repeated blocks (rounded up integer of
      --  (GPDMA_CxBR1.BRC[10:0]+1)/2)) has been transferred to the
      --  destination.
      HTF            : Boolean;
      --  Read-only. data transfer error flag
      DTEF           : Boolean;
      --  Read-only. update link transfer error flag
      ULEF           : Boolean;
      --  Read-only. user setting error flag
      USEF           : Boolean;
      --  Read-only. completed suspension flag
      SUSPF          : Boolean;
      --  Read-only. trigger overrun flag
      TOF            : Boolean;
      --  unspecified
      Reserved_15_15 : HAL.Bit;
      --  Read-only. monitored FIFO level 	Number of available write beats in
      --  the FIFO, in units of the programmed destination data width (see
      --  GPDMA_CxTR1.DDW_LOG2[1:0], in units of bytes, half-words, or words).
      --  	Note: After having suspended an active transfer, the user may need
      --  to read FIFOL[7:0], additionally to GPDMA_CxBR1.BDNT[15:0] and
      --  GPDMA_CxBR1.BRC[10:0], to know how many data have been transferred to
      --  the destination. Before reading, the user may wait for the transfer
      --  to be suspended (GPDMA_CxSR.SUSPF = 1).
      FIFOL          : GPDMA_C3SR_FIFOL_Field;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C3SR_Register use record
      IDLEF          at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      TCF            at 0 range 8 .. 8;
      HTF            at 0 range 9 .. 9;
      DTEF           at 0 range 10 .. 10;
      ULEF           at 0 range 11 .. 11;
      USEF           at 0 range 12 .. 12;
      SUSPF          at 0 range 13 .. 13;
      TOF            at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      FIFOL          at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype GPDMA_C3CR_PRIO_Field is HAL.UInt2;

   --  GPDMA channel 3 control register
   type GPDMA_C3CR_Register is record
      --  enable 	Writing 1 into the field RESET (bit 1) causes the hardware to
      --  de-assert this bit, whatever is written into this bit 0. Else: 	this
      --  bit is de-asserted by hardware when there is a transfer error (master
      --  bus error or user setting error) or when there is a channel transfer
      --  complete (channel ready to be configured, e.g. if LSM=1 at the end of
      --  a single execution of the LLI). 	Else, this bit can be asserted by
      --  software. 	Writing 0 into this EN bit is ignored.
      EN             : Boolean := False;
      --  Write-only. reset 	This bit is write only. Writing 0 has no impact.
      --  Writing 1 implies the reset of the following: the FIFO, the channel
      --  internal state, SUSP and EN bits (whatever is written receptively in
      --  bit 2 and bit 0). 	The reset is effective when the channel is in
      --  steady state, meaning one of the following: 	- active channel in
      --  suspended state (GPDMA_CxSR.SUSPF = 1 and
      --  GPDMA_CxSR.IDLEF = GPDMA_CxCR.EN = 1) 	- channel in disabled
      --  state (GPDMA_CxSR.IDLEF = 1 and GPDMA_CxCR.EN = 0). 	After
      --  writing a RESET, to continue using this channel, the user must
      --  explicitly reconfigure the channel including the hardware-modified
      --  configuration registers (GPDMA_CxBR1, GPDMA_CxSAR and GPDMA_CxDAR)
      --  before enabling again the channel (see the programming sequence in ).
      RESET          : Boolean := False;
      --  suspend 	Writing 1 into the field RESET (bit 1) causes the hardware
      --  to de-assert this bit, whatever is written into this bit 2. Else:
      --  	Software must write 1 in order to suspend an active channel i.e. a
      --  channel with an on-going GPDMA transfer over its master ports. 	The
      --  software must write 0 in order to resume a suspended channel,
      --  following the programming sequence detailed in .
      SUSP           : Boolean := False;
      --  unspecified
      Reserved_3_7   : HAL.UInt5 := 16#0#;
      --  transfer complete interrupt enable
      TCIE           : Boolean := False;
      --  half transfer complete interrupt enable
      HTIE           : Boolean := False;
      --  data transfer error interrupt enable
      DTEIE          : Boolean := False;
      --  update link transfer error interrupt enable
      ULEIE          : Boolean := False;
      --  user setting error interrupt enable
      USEIE          : Boolean := False;
      --  completed suspension interrupt enable
      SUSPIE         : Boolean := False;
      --  trigger overrun interrupt enable
      TOIE           : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Link step mode 	First the (possible 1D/repeated) block transfer is
      --  executed as defined by the current internal register file until
      --  GPDMA_CxBR1.BNDT[15:0] = 0 and GPDMA_CxBR1.BRC[10:0] = 0 if
      --  present. Secondly the next linked-list data structure is
      --  conditionally uploaded from memory as defined by GPDMA_CxLLR. Then
      --  channel execution is completed. 	Note: This bit must be written when
      --  EN=0. This bit is read-only when EN=1.
      LSM            : Boolean := False;
      --  linked-list allocated port 	This bit is used to allocate the master
      --  port for the update of the GPDMA linked-list registers from the
      --  memory. 	Note: This bit must be written when EN=0. This bit is
      --  read-only when EN=1.
      LAP            : Boolean := False;
      --  unspecified
      Reserved_18_21 : HAL.UInt4 := 16#0#;
      --  priority level of the channel x GPDMA transfer versus others 	Note:
      --  This bit must be written when EN = 0. This bit is read-only when
      --  EN = 1.
      PRIO           : GPDMA_C3CR_PRIO_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C3CR_Register use record
      EN             at 0 range 0 .. 0;
      RESET          at 0 range 1 .. 1;
      SUSP           at 0 range 2 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      TCIE           at 0 range 8 .. 8;
      HTIE           at 0 range 9 .. 9;
      DTEIE          at 0 range 10 .. 10;
      ULEIE          at 0 range 11 .. 11;
      USEIE          at 0 range 12 .. 12;
      SUSPIE         at 0 range 13 .. 13;
      TOIE           at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      LSM            at 0 range 16 .. 16;
      LAP            at 0 range 17 .. 17;
      Reserved_18_21 at 0 range 18 .. 21;
      PRIO           at 0 range 22 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype GPDMA_C3TR1_SDW_LOG2_Field is HAL.UInt2;
   subtype GPDMA_C3TR1_SBL_1_Field is HAL.UInt6;
   subtype GPDMA_C3TR1_PAM_Field is HAL.UInt2;
   subtype GPDMA_C3TR1_DDW_LOG2_Field is HAL.UInt2;
   subtype GPDMA_C3TR1_DBL_1_Field is HAL.UInt6;

   --  GPDMA channel 3 transfer register 1
   type GPDMA_C3TR1_Register is record
      --  binary logarithm of the source data width of a burst in bytes 	Note:
      --  Setting a 8-byte data width causes a user setting error to be
      --  reported and no transfer is issued. 	A source block size must be a
      --  multiple of the source data width (GPDMA_CxBR1.BNDT[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and no
      --  transfer is issued. 	A source single transfer must have an aligned
      --  address with its data width (start address GPDMA_CxSAR[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and none
      --  transfer is issued.
      SDW_LOG2       : GPDMA_C3TR1_SDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  source incrementing burst 	The source address, pointed by
      --  GPDMA_CxSAR, is kept constant after a burst beat/single transfer or
      --  is incremented by the offset value corresponding to a contiguous data
      --  after a burst beat/single transfer.
      SINC           : Boolean := False;
      --  source burst length minus 1, between 0 and 63 	The burst length unit
      --  is one data named beat within a burst. If SBL_1[5:0] =0 , the burst
      --  can be named as single. Each data/beat has a width defined by the
      --  destination data width SDW_LOG2[1:0]. 	Note: If a burst transfer
      --  crossed a 1-Kbyte address boundary on a AHB transfer, the GPDMA
      --  modifies and shortens the programmed burst into singles or bursts of
      --  lower length, to be compliant with the AHB protocol. 	If a burst
      --  transfer is of length greater than the FIFO size of the channel x,
      --  the GPDMA modifies and shortens the programmed burst into singles or
      --  bursts of lower length, to be compliant with the FIFO size. Transfer
      --  performance is lower, with GPDMA re-arbitration between effective and
      --  lower bursts/singles, but the data integrity is guaranteed.
      SBL_1          : GPDMA_C3TR1_SBL_1_Field := 16#0#;
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  padding/alignment mode 	If DDW_LOG2[1:0] = SDW_LOG2[1:0]: if the data
      --  width of a burst destination transfer is equal to the data width of a
      --  burst source transfer, these bits are ignored. 	Else: 	- Case 1: If
      --  destination data width > source data width 	1x: successive source
      --  data are FIFO queued and packed at the destination data width, in a
      --  left (LSB) to right (MSB) order (named little endian), before a
      --  destination transfer 	- Case 2: If destination data width < source
      --  data width 	1x: source data is FIFO queued and unpacked at the
      --  destination data width, to be transferred in a left (LSB) to right
      --  (MSB) order (named little endian) to the destination 	Note:
      PAM            : GPDMA_C3TR1_PAM_Field := 16#0#;
      --  source byte exchange within the unaligned half-word of each source
      --  word 	If the source data width is shorter than a word, this bit is
      --  ignored. 	If the source data width is a word:
      SBX            : Boolean := False;
      --  source allocated port 	This bit is used to allocate the master port
      --  for the source transfer 	Note: This bit must be written when
      --  EN = 0. This bit is read-only when EN = 1.
      SAP            : Boolean := False;
      --  security attribute of the GPDMA transfer from the source 	If
      --  GPDMA_SECCFGR.SECx = 1 and the access is secure: 	This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  GPDMA_SECCFGR.SECx =1 . A secure write is ignored when
      --  GPDMA_SECCFGR.SECx = 0. 	When GPDMA_SECCFGR.SECx is de-asserted,
      --  this SSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the GPDMA transfer
      --  from the source is non-secure.
      SSEC           : Boolean := False;
      --  binary logarithm of the destination data width of a burst, in bytes
      --  	Note: Setting a 8-byte data width causes a user setting error to be
      --  reported and none transfer is issued. 	A destination burst transfer
      --  must have an aligned address with its data width (start address
      --  GPDMA_CxDAR[2:0] and address offset GPDMA_CxTR3.DAO[2:0], versus
      --  DDW_LOG2[1:0]). Otherwise a user setting error is reported and no
      --  transfer is issued.
      DDW_LOG2       : GPDMA_C3TR1_DDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_18_18 : HAL.Bit := 16#0#;
      --  destination incrementing burst 	The destination address, pointed by
      --  GPDMA_CxDAR, is kept constant after a burst beat/single transfer, or
      --  is incremented by the offset value corresponding to a contiguous data
      --  after a burst beat/single transfer.
      DINC           : Boolean := False;
      --  destination burst length minus 1, between 0 and 63 	The burst length
      --  unit is one data named beat within a burst. If DBL_1[5:0] =0 , the
      --  burst can be named as single. Each data/beat has a width defined by
      --  the destination data width DDW_LOG2[1:0]. 	Note: If a burst transfer
      --  crossed a 1-Kbyte address boundary on a AHB transfer, the GPDMA
      --  modifies and shortens the programmed burst into singles or bursts of
      --  lower length, to be compliant with the AHB protocol. 	If a burst
      --  transfer is of length greater than the FIFO size of the channel x,
      --  the GPDMA modifies and shortens the programmed burst into singles or
      --  bursts of lower length, to be compliant with the FIFO size. Transfer
      --  performance is lower, with GPDMA re-arbitration between effective and
      --  lower bursts/singles, but the data integrity is guaranteed.
      DBL_1          : GPDMA_C3TR1_DBL_1_Field := 16#0#;
      --  destination byte exchange 	If the destination data size is a byte,
      --  this bit is ignored. 	If the destination data size is not a byte:
      DBX            : Boolean := False;
      --  destination half-word exchange 	If the destination data size is
      --  shorter than a word, this bit is ignored. 	If the destination data
      --  size is a word:
      DHX            : Boolean := False;
      --  unspecified
      Reserved_28_29 : HAL.UInt2 := 16#0#;
      --  destination allocated port 	This bit is used to allocate the master
      --  port for the destination transfer 	Note: This bit must be written
      --  when EN = 0. This bit is read-only when EN = 1.
      DAP            : Boolean := False;
      --  security attribute of the GPDMA transfer to the destination 	If
      --  GPDMA_SECCFGR.SECx = 1 and the access is secure: 	This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  GPDMA_SECCFGR.SECx = 1. A secure write is ignored when
      --  GPDMA_SECCFGR.SECx = 0. 	When GPDMA_SECCFGR.SECx is de-asserted,
      --  this DSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the GPDMA transfer
      --  to the destination is non-secure.
      DSEC           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C3TR1_Register use record
      SDW_LOG2       at 0 range 0 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      SINC           at 0 range 3 .. 3;
      SBL_1          at 0 range 4 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      PAM            at 0 range 11 .. 12;
      SBX            at 0 range 13 .. 13;
      SAP            at 0 range 14 .. 14;
      SSEC           at 0 range 15 .. 15;
      DDW_LOG2       at 0 range 16 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      DINC           at 0 range 19 .. 19;
      DBL_1          at 0 range 20 .. 25;
      DBX            at 0 range 26 .. 26;
      DHX            at 0 range 27 .. 27;
      Reserved_28_29 at 0 range 28 .. 29;
      DAP            at 0 range 30 .. 30;
      DSEC           at 0 range 31 .. 31;
   end record;

   subtype GPDMA_C3TR2_REQSEL_Field is HAL.UInt7;
   subtype GPDMA_C3TR2_TRIGM_Field is HAL.UInt2;
   subtype GPDMA_C3TR2_TRIGSEL_Field is HAL.UInt6;
   subtype GPDMA_C3TR2_TRIGPOL_Field is HAL.UInt2;
   subtype GPDMA_C3TR2_TCEM_Field is HAL.UInt2;

   --  GPDMA channel 3 transfer register 2
   type GPDMA_C3TR2_Register is record
      --  GPDMA hardware request selection 	These bits are ignored if channel x
      --  is activated (GPDMA_CxCR.EN asserted) with SWREQ = 1 (software
      --  request for a memory-to-memory transfer). Else, the selected hardware
      --  request is internally taken into account as per . 	The user must not
      --  assign a same input hardware request (same REQSEL[6:0] value) to
      --  different active GPDMA channels (GPDMA_CxCR.EN = 1 and
      --  GPDMA_CxTR2.SWREQ = 0 for these channels). GPDMA is not intended to
      --  hardware support the case of simultaneous enabled channels
      --  incorrectly configured with a same hardware peripheral request
      --  signal, and there is no user setting error reporting.
      REQSEL         : GPDMA_C3TR2_REQSEL_Field := 16#0#;
      --  unspecified
      Reserved_7_8   : HAL.UInt2 := 16#0#;
      --  software request 	This bit is internally taken into account when
      --  GPDMA_CxCR.EN is asserted.
      SWREQ          : Boolean := False;
      --  destination hardware request 	This bit is ignored if channel x is
      --  activated (GPDMA_CxCR.EN asserted) with SWREQ = 1 (software request
      --  for a memory-to-memory transfer). Else: 	Note:
      DREQ           : Boolean := False;
      --  Block hardware request 	If the channel x is activated (GPDMA_CxCR.EN
      --  asserted) with SWREQ = 1 (software request for a memory-to-memory
      --  transfer), this bit is ignored. Else:
      BREQ           : Boolean := False;
      --  unspecified
      Reserved_12_13 : HAL.UInt2 := 16#0#;
      --  trigger mode 	These bits define the transfer granularity for its
      --  conditioning by the trigger. 	If the channel x is enabled
      --  (GPDMA_CxCR.EN asserted) with TRIGPOL[1:0] = 00 or 11, these
      --  TRIGM[1:0] bits are ignored. 	Else, a GPDMA transfer is conditioned
      --  by at least one trigger hit: 	first burst read of a 2D/repeated block
      --  transfer is conditioned by one hit trigger. 	– If the peripheral is
      --  programmed as a source (DREQ = 0) of the LLI data transfer, each
      --  programmed burst read is conditioned. 	– If the peripheral is
      --  programmed as a destination (DREQ = 1) of the LLI data transfer,
      --  each programmed burst write is conditioned. The first memory burst
      --  read of a (possibly 2D/repeated) block, also named as the first ready
      --  FIFO-based source burst, is gated by the occurrence of both the
      --  hardware request and the first trigger hit. 	The GPDMA monitoring of
      --  a trigger for channel x is started when the channel is enabled/loaded
      --  with a new active trigger configuration: rising or falling edge on a
      --  selected trigger (TRIGPOL[1:0] = 01 or respectively
      --  TRIGPOL[1:0] = 10). 	The monitoring of this trigger is kept active
      --  during the triggered and uncompleted (data or link) transfer; and if
      --  a new trigger is detected then, this hit is internally memorized to
      --  grant the next transfer, as long as the defined rising or falling
      --  edge is not modified, and the TRIGSEL[5:0] is not modified, and the
      --  channel is enabled. 	Transferring a next LLIn+1 that updates the
      --  GPDMA_CxTR2 with a new value for any of TRIGSEL[5:0] or TRIGPOL[1:0],
      --  resets the monitoring, trashing the memorized hit of the formerly
      --  defined LLIn trigger. 	After a first new trigger hitn+1 is memorized,
      --  if another second trigger hitn+2 is detected and if the hitn
      --  triggered transfer is still not completed, hitn+2 is lost and not
      --  memorized.memorized. A trigger overrun flag is reported
      --  (GPDMA_CxSR.TOF =1 ), and an interrupt is generated if enabled
      --  (GPDMA_CxCR.TOIE = 1). The channel is not automatically disabled by
      --  hardware due to a trigger overrun. 	Note: When the source block size
      --  is not a multiple of the source burst size and is a multiple of the
      --  source data width, then the last programmed source burst is not
      --  completed and is internally shorten to match the block size. In this
      --  case, if TRIGM[1:0] = 11 and (SWREQ =1  or (SWREQ = 0 and
      --  DREQ =0 )), the shortened burst transfer (by singles or/and by
      --  bursts of lower length) is conditioned once by the trigger. 	When the
      --  programmed destination burst is internally shortened by singles
      --  or/and by bursts of lower length (versus FIFO size, versus block
      --  size, 1-Kbyte boundary address crossing): if the trigger is
      --  conditioning the programmed destination burst (if TRIGM[1:0] = 11
      --  and SWREQ = 0 and DREQ = 1), this shortened destination burst
      --  transfer is conditioned once by the trigger.
      TRIGM          : GPDMA_C3TR2_TRIGM_Field := 16#0#;
      --  trigger event input selection 	These bits select the trigger event
      --  input of the GPDMA transfer (as per ), with an active trigger event
      --  if TRIGPOL[1:0] ≠ 00.
      TRIGSEL        : GPDMA_C3TR2_TRIGSEL_Field := 16#0#;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  trigger event polarity 	These bits define the polarity of the
      --  selected trigger event input defined by TRIGSEL[5:0].
      TRIGPOL        : GPDMA_C3TR2_TRIGPOL_Field := 16#0#;
      --  unspecified
      Reserved_26_29 : HAL.UInt4 := 16#0#;
      --  transfer complete event mode 	These bits define the transfer
      --  granularity for the transfer complete and half transfer complete
      --  events generation. 	Note: If the initial LLI0 data transfer is
      --  null/void (directly programmed by the internal register file with
      --  GPDMA_CxBR1.BNDT[15:0] = 0), then neither the complete transfer
      --  event nor the half transfer event is generated. 	Note: If the initial
      --  LLI0 data transfer is null/void (directly programmed by the internal
      --  register file with GPDMA_CxBR1.BNDT[15:0] = 0), then neither the
      --  complete transfer event nor the half transfer event is generated.
      --  	Note: If the initial LLI0 data transfer is null/void (i.e. directly
      --  programmed by the internal register file with
      --  GPDMA_CxBR1.BNDT[15:0] =0 ), then the half transfer event is not
      --  generated, and the transfer complete event is generated when is
      --  completed the loading of the LLI1.
      TCEM           : GPDMA_C3TR2_TCEM_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C3TR2_Register use record
      REQSEL         at 0 range 0 .. 6;
      Reserved_7_8   at 0 range 7 .. 8;
      SWREQ          at 0 range 9 .. 9;
      DREQ           at 0 range 10 .. 10;
      BREQ           at 0 range 11 .. 11;
      Reserved_12_13 at 0 range 12 .. 13;
      TRIGM          at 0 range 14 .. 15;
      TRIGSEL        at 0 range 16 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      TRIGPOL        at 0 range 24 .. 25;
      Reserved_26_29 at 0 range 26 .. 29;
      TCEM           at 0 range 30 .. 31;
   end record;

   subtype GPDMA_C3BR1_BNDT_Field is HAL.UInt16;

   --  GPDMA channel 3 block register 1
   type GPDMA_C3BR1_Register is record
      --  block number of data bytes to transfer from the source 	Block size
      --  transferred from the source. When the channel is enabled, this field
      --  becomes read-only and is decremented, indicating the remaining number
      --  of data items in the current source block to be transferred.
      --  BNDT[15:0] is programmed in number of bytes, maximum source block
      --  size is 64 Kbytes -1. 	Once the last data transfer is completed
      --  (BNDT[15:0] = 0): 	- if GPDMA_CxLLR.UB1 = 1, this field is
      --  updated by the LLI in the memory. 	- if GPDMA_CxLLR.UB1 = 0 and if
      --  there is at least one non null Uxx update bit, this field is
      --  internally restored to the programmed value. 	- if all
      --  GPDMA_CxLLR.Uxx = 0 and if GPDMA_CxLLR.LA[15:0] = 0, this field
      --  is internally restored to the programmed value (infinite/continuous
      --  last LLI). 	- if GPDMA_CxLLR = 0, this field is kept as zero
      --  following the last LLI data transfer. 	Note: A non-null source block
      --  size must be a multiple of the source data width (BNDT[2:0] versus
      --  GPDMA_CxTR1.SDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued. 	When configured in packing mode
      --  (GPDMA_CxTR1.PAM[1] = 1 and destination data width different from
      --  source data width), a non-null source block size must be a multiple
      --  of the destination data width (BNDT[2:0] versus
      --  GPDMA_CxTR1.DDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued.
      BNDT           : GPDMA_C3BR1_BNDT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C3BR1_Register use record
      BNDT           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype GPDMA_C3LLR_LA_Field is HAL.UInt14;

   --  GPDMA channel 3 linked-list address register
   type GPDMA_C3LLR_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  pointer (16-bit low-significant address) to the next linked-list data
      --  structure 	If UT1 = UT2 = UB1 = USA = UDA = ULL = 0 and
      --  if LA[15:20] = 0, the current LLI is the last one. The channel
      --  transfer is completed without any update of the linked-list GPDMA
      --  register file. 	Else, this field is the pointer to the memory address
      --  offset from which the next linked-list data structure is
      --  automatically fetched from, once the data transfer is completed, in
      --  order to conditionally update the linked-list GPDMA internal register
      --  file (GPDMA_CxCTR1, GPDMA_CxTR2, GPDMA_CxBR1, GPDMA_CxSAR,
      --  GPDMA_CxDAR and GPDMA_CxLLR). 	Note: The user must program the
      --  pointer to be 32-bit aligned. The two low-significant bits are write
      --  ignored.
      LA             : GPDMA_C3LLR_LA_Field := 16#0#;
      --  Update GPDMA_CxLLR register from memory 	This bit is used to control
      --  the update of GPDMA_CxLLR from the memory during the link transfer.
      ULL            : Boolean := False;
      --  unspecified
      Reserved_17_26 : HAL.UInt10 := 16#0#;
      --  Update GPDMA_CxDAR register from memory 	This bit is used to control
      --  the update of GPDMA_CxDAR from the memory during the link transfer.
      UDA            : Boolean := False;
      --  update GPDMA_CxSAR from memory 	This bit controls the update of
      --  GPDMA_CxSAR from the memory during the link transfer.
      USA            : Boolean := False;
      --  Update GPDMA_CxBR1 from memory 	This bit controls the update of
      --  GPDMA_CxBR1 from the memory during the link transfer. If UB1 = 0
      --  and if GPDMA_CxLLR ≠ 0, the linked-list is not completed.
      --  GPDMA_CxBR1.BNDT[15:0] is then restored to the programmed value after
      --  data transfer is completed and before the link transfer.
      UB1            : Boolean := False;
      --  Update GPDMA_CxTR2 from memory 	This bit controls the update of
      --  GPDMA_CxTR2 from the memory during the link transfer.
      UT2            : Boolean := False;
      --  Update GPDMA_CxTR1 from memory 	This bit controls the update of
      --  GPDMA_CxTR1 from the memory during the link transfer.
      UT1            : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C3LLR_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      LA             at 0 range 2 .. 15;
      ULL            at 0 range 16 .. 16;
      Reserved_17_26 at 0 range 17 .. 26;
      UDA            at 0 range 27 .. 27;
      USA            at 0 range 28 .. 28;
      UB1            at 0 range 29 .. 29;
      UT2            at 0 range 30 .. 30;
      UT1            at 0 range 31 .. 31;
   end record;

   subtype GPDMA_C4LBAR_LBA_Field is HAL.UInt16;

   --  GPDMA channel 4 linked-list base address register
   type GPDMA_C4LBAR_Register is record
      --  unspecified
      Reserved_0_15 : HAL.UInt16 := 16#0#;
      --  linked-list base address of GPDMA channel x
      LBA           : GPDMA_C4LBAR_LBA_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C4LBAR_Register use record
      Reserved_0_15 at 0 range 0 .. 15;
      LBA           at 0 range 16 .. 31;
   end record;

   --  GPDMA channel 4 flag clear register
   type GPDMA_C4FCR_Register is record
      --  unspecified
      Reserved_0_7   : HAL.UInt8 := 16#0#;
      --  Write-only. transfer complete flag clear
      TCF            : Boolean := False;
      --  Write-only. half transfer flag clear
      HTF            : Boolean := False;
      --  Write-only. data transfer error flag clear
      DTEF           : Boolean := False;
      --  Write-only. update link transfer error flag clear
      ULEF           : Boolean := False;
      --  Write-only. user setting error flag clear
      USEF           : Boolean := False;
      --  Write-only. completed suspension flag clear
      SUSPF          : Boolean := False;
      --  Write-only. trigger overrun flag clear
      TOF            : Boolean := False;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C4FCR_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      TCF            at 0 range 8 .. 8;
      HTF            at 0 range 9 .. 9;
      DTEF           at 0 range 10 .. 10;
      ULEF           at 0 range 11 .. 11;
      USEF           at 0 range 12 .. 12;
      SUSPF          at 0 range 13 .. 13;
      TOF            at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype GPDMA_C4SR_FIFOL_Field is HAL.UInt8;

   --  GPDMA channel 4 status register
   type GPDMA_C4SR_Register is record
      --  Read-only. idle flag 	This idle flag is de-asserted by hardware when
      --  the channel is enabled (GPDMA_CxCR.EN = 1) with a valid channel
      --  configuration (no USEF to be immediately reported). 	This idle flag
      --  is asserted after hard reset or by hardware when the channel is back
      --  in idle state (in suspended or disabled state).
      IDLEF          : Boolean;
      --  unspecified
      Reserved_1_7   : HAL.UInt7;
      --  Read-only. transfer complete flag 	A transfer complete event is
      --  either a block transfer complete, a 2D/repeated block transfer
      --  complete, a LLI transfer complete including the upload of the next
      --  LLI if any, or the full linked-list completion, depending on the
      --  transfer complete event mode (GPDMA_CxTR2.TCEM[1:0]).
      TCF            : Boolean;
      --  Read-only. half transfer flag 	An half transfer event is either an
      --  half block transfer or an half 2D/repeated block transfer, depending
      --  on the transfer complete event mode (GPDMA_CxTR2.TCEM[1:0]). 	An half
      --  block transfer occurs when half of the bytes of the source block size
      --  (rounded up integer of GPDMA_CxBR1.BNDT[15:0]/2) has been transferred
      --  to the destination. 	An half 2D/repeated block transfer occurs when
      --  half of the repeated blocks (rounded up integer of
      --  (GPDMA_CxBR1.BRC[10:0]+1)/2)) has been transferred to the
      --  destination.
      HTF            : Boolean;
      --  Read-only. data transfer error flag
      DTEF           : Boolean;
      --  Read-only. update link transfer error flag
      ULEF           : Boolean;
      --  Read-only. user setting error flag
      USEF           : Boolean;
      --  Read-only. completed suspension flag
      SUSPF          : Boolean;
      --  Read-only. trigger overrun flag
      TOF            : Boolean;
      --  unspecified
      Reserved_15_15 : HAL.Bit;
      --  Read-only. monitored FIFO level 	Number of available write beats in
      --  the FIFO, in units of the programmed destination data width (see
      --  GPDMA_CxTR1.DDW_LOG2[1:0], in units of bytes, half-words, or words).
      --  	Note: After having suspended an active transfer, the user may need
      --  to read FIFOL[7:0], additionally to GPDMA_CxBR1.BDNT[15:0] and
      --  GPDMA_CxBR1.BRC[10:0], to know how many data have been transferred to
      --  the destination. Before reading, the user may wait for the transfer
      --  to be suspended (GPDMA_CxSR.SUSPF = 1).
      FIFOL          : GPDMA_C4SR_FIFOL_Field;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C4SR_Register use record
      IDLEF          at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      TCF            at 0 range 8 .. 8;
      HTF            at 0 range 9 .. 9;
      DTEF           at 0 range 10 .. 10;
      ULEF           at 0 range 11 .. 11;
      USEF           at 0 range 12 .. 12;
      SUSPF          at 0 range 13 .. 13;
      TOF            at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      FIFOL          at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype GPDMA_C4CR_PRIO_Field is HAL.UInt2;

   --  GPDMA channel 4 control register
   type GPDMA_C4CR_Register is record
      --  enable 	Writing 1 into the field RESET (bit 1) causes the hardware to
      --  de-assert this bit, whatever is written into this bit 0. Else: 	this
      --  bit is de-asserted by hardware when there is a transfer error (master
      --  bus error or user setting error) or when there is a channel transfer
      --  complete (channel ready to be configured, e.g. if LSM=1 at the end of
      --  a single execution of the LLI). 	Else, this bit can be asserted by
      --  software. 	Writing 0 into this EN bit is ignored.
      EN             : Boolean := False;
      --  Write-only. reset 	This bit is write only. Writing 0 has no impact.
      --  Writing 1 implies the reset of the following: the FIFO, the channel
      --  internal state, SUSP and EN bits (whatever is written receptively in
      --  bit 2 and bit 0). 	The reset is effective when the channel is in
      --  steady state, meaning one of the following: 	- active channel in
      --  suspended state (GPDMA_CxSR.SUSPF = 1 and
      --  GPDMA_CxSR.IDLEF = GPDMA_CxCR.EN = 1) 	- channel in disabled
      --  state (GPDMA_CxSR.IDLEF = 1 and GPDMA_CxCR.EN = 0). 	After
      --  writing a RESET, to continue using this channel, the user must
      --  explicitly reconfigure the channel including the hardware-modified
      --  configuration registers (GPDMA_CxBR1, GPDMA_CxSAR and GPDMA_CxDAR)
      --  before enabling again the channel (see the programming sequence in ).
      RESET          : Boolean := False;
      --  suspend 	Writing 1 into the field RESET (bit 1) causes the hardware
      --  to de-assert this bit, whatever is written into this bit 2. Else:
      --  	Software must write 1 in order to suspend an active channel i.e. a
      --  channel with an on-going GPDMA transfer over its master ports. 	The
      --  software must write 0 in order to resume a suspended channel,
      --  following the programming sequence detailed in .
      SUSP           : Boolean := False;
      --  unspecified
      Reserved_3_7   : HAL.UInt5 := 16#0#;
      --  transfer complete interrupt enable
      TCIE           : Boolean := False;
      --  half transfer complete interrupt enable
      HTIE           : Boolean := False;
      --  data transfer error interrupt enable
      DTEIE          : Boolean := False;
      --  update link transfer error interrupt enable
      ULEIE          : Boolean := False;
      --  user setting error interrupt enable
      USEIE          : Boolean := False;
      --  completed suspension interrupt enable
      SUSPIE         : Boolean := False;
      --  trigger overrun interrupt enable
      TOIE           : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Link step mode 	First the (possible 1D/repeated) block transfer is
      --  executed as defined by the current internal register file until
      --  GPDMA_CxBR1.BNDT[15:0] = 0 and GPDMA_CxBR1.BRC[10:0] = 0 if
      --  present. Secondly the next linked-list data structure is
      --  conditionally uploaded from memory as defined by GPDMA_CxLLR. Then
      --  channel execution is completed. 	Note: This bit must be written when
      --  EN=0. This bit is read-only when EN=1.
      LSM            : Boolean := False;
      --  linked-list allocated port 	This bit is used to allocate the master
      --  port for the update of the GPDMA linked-list registers from the
      --  memory. 	Note: This bit must be written when EN=0. This bit is
      --  read-only when EN=1.
      LAP            : Boolean := False;
      --  unspecified
      Reserved_18_21 : HAL.UInt4 := 16#0#;
      --  priority level of the channel x GPDMA transfer versus others 	Note:
      --  This bit must be written when EN = 0. This bit is read-only when
      --  EN = 1.
      PRIO           : GPDMA_C4CR_PRIO_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C4CR_Register use record
      EN             at 0 range 0 .. 0;
      RESET          at 0 range 1 .. 1;
      SUSP           at 0 range 2 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      TCIE           at 0 range 8 .. 8;
      HTIE           at 0 range 9 .. 9;
      DTEIE          at 0 range 10 .. 10;
      ULEIE          at 0 range 11 .. 11;
      USEIE          at 0 range 12 .. 12;
      SUSPIE         at 0 range 13 .. 13;
      TOIE           at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      LSM            at 0 range 16 .. 16;
      LAP            at 0 range 17 .. 17;
      Reserved_18_21 at 0 range 18 .. 21;
      PRIO           at 0 range 22 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype GPDMA_C4TR1_SDW_LOG2_Field is HAL.UInt2;
   subtype GPDMA_C4TR1_SBL_1_Field is HAL.UInt6;
   subtype GPDMA_C4TR1_PAM_Field is HAL.UInt2;
   subtype GPDMA_C4TR1_DDW_LOG2_Field is HAL.UInt2;
   subtype GPDMA_C4TR1_DBL_1_Field is HAL.UInt6;

   --  GPDMA channel 4 transfer register 1
   type GPDMA_C4TR1_Register is record
      --  binary logarithm of the source data width of a burst in bytes 	Note:
      --  Setting a 8-byte data width causes a user setting error to be
      --  reported and no transfer is issued. 	A source block size must be a
      --  multiple of the source data width (GPDMA_CxBR1.BNDT[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and no
      --  transfer is issued. 	A source single transfer must have an aligned
      --  address with its data width (start address GPDMA_CxSAR[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and none
      --  transfer is issued.
      SDW_LOG2       : GPDMA_C4TR1_SDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  source incrementing burst 	The source address, pointed by
      --  GPDMA_CxSAR, is kept constant after a burst beat/single transfer or
      --  is incremented by the offset value corresponding to a contiguous data
      --  after a burst beat/single transfer.
      SINC           : Boolean := False;
      --  source burst length minus 1, between 0 and 63 	The burst length unit
      --  is one data named beat within a burst. If SBL_1[5:0] =0 , the burst
      --  can be named as single. Each data/beat has a width defined by the
      --  destination data width SDW_LOG2[1:0]. 	Note: If a burst transfer
      --  crossed a 1-Kbyte address boundary on a AHB transfer, the GPDMA
      --  modifies and shortens the programmed burst into singles or bursts of
      --  lower length, to be compliant with the AHB protocol. 	If a burst
      --  transfer is of length greater than the FIFO size of the channel x,
      --  the GPDMA modifies and shortens the programmed burst into singles or
      --  bursts of lower length, to be compliant with the FIFO size. Transfer
      --  performance is lower, with GPDMA re-arbitration between effective and
      --  lower bursts/singles, but the data integrity is guaranteed.
      SBL_1          : GPDMA_C4TR1_SBL_1_Field := 16#0#;
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  padding/alignment mode 	If DDW_LOG2[1:0] = SDW_LOG2[1:0]: if the data
      --  width of a burst destination transfer is equal to the data width of a
      --  burst source transfer, these bits are ignored. 	Else: 	- Case 1: If
      --  destination data width > source data width 	1x: successive source
      --  data are FIFO queued and packed at the destination data width, in a
      --  left (LSB) to right (MSB) order (named little endian), before a
      --  destination transfer 	- Case 2: If destination data width < source
      --  data width 	1x: source data is FIFO queued and unpacked at the
      --  destination data width, to be transferred in a left (LSB) to right
      --  (MSB) order (named little endian) to the destination 	Note:
      PAM            : GPDMA_C4TR1_PAM_Field := 16#0#;
      --  source byte exchange within the unaligned half-word of each source
      --  word 	If the source data width is shorter than a word, this bit is
      --  ignored. 	If the source data width is a word:
      SBX            : Boolean := False;
      --  source allocated port 	This bit is used to allocate the master port
      --  for the source transfer 	Note: This bit must be written when
      --  EN = 0. This bit is read-only when EN = 1.
      SAP            : Boolean := False;
      --  security attribute of the GPDMA transfer from the source 	If
      --  GPDMA_SECCFGR.SECx = 1 and the access is secure: 	This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  GPDMA_SECCFGR.SECx =1 . A secure write is ignored when
      --  GPDMA_SECCFGR.SECx = 0. 	When GPDMA_SECCFGR.SECx is de-asserted,
      --  this SSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the GPDMA transfer
      --  from the source is non-secure.
      SSEC           : Boolean := False;
      --  binary logarithm of the destination data width of a burst, in bytes
      --  	Note: Setting a 8-byte data width causes a user setting error to be
      --  reported and none transfer is issued. 	A destination burst transfer
      --  must have an aligned address with its data width (start address
      --  GPDMA_CxDAR[2:0] and address offset GPDMA_CxTR3.DAO[2:0], versus
      --  DDW_LOG2[1:0]). Otherwise a user setting error is reported and no
      --  transfer is issued.
      DDW_LOG2       : GPDMA_C4TR1_DDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_18_18 : HAL.Bit := 16#0#;
      --  destination incrementing burst 	The destination address, pointed by
      --  GPDMA_CxDAR, is kept constant after a burst beat/single transfer, or
      --  is incremented by the offset value corresponding to a contiguous data
      --  after a burst beat/single transfer.
      DINC           : Boolean := False;
      --  destination burst length minus 1, between 0 and 63 	The burst length
      --  unit is one data named beat within a burst. If DBL_1[5:0] =0 , the
      --  burst can be named as single. Each data/beat has a width defined by
      --  the destination data width DDW_LOG2[1:0]. 	Note: If a burst transfer
      --  crossed a 1-Kbyte address boundary on a AHB transfer, the GPDMA
      --  modifies and shortens the programmed burst into singles or bursts of
      --  lower length, to be compliant with the AHB protocol. 	If a burst
      --  transfer is of length greater than the FIFO size of the channel x,
      --  the GPDMA modifies and shortens the programmed burst into singles or
      --  bursts of lower length, to be compliant with the FIFO size. Transfer
      --  performance is lower, with GPDMA re-arbitration between effective and
      --  lower bursts/singles, but the data integrity is guaranteed.
      DBL_1          : GPDMA_C4TR1_DBL_1_Field := 16#0#;
      --  destination byte exchange 	If the destination data size is a byte,
      --  this bit is ignored. 	If the destination data size is not a byte:
      DBX            : Boolean := False;
      --  destination half-word exchange 	If the destination data size is
      --  shorter than a word, this bit is ignored. 	If the destination data
      --  size is a word:
      DHX            : Boolean := False;
      --  unspecified
      Reserved_28_29 : HAL.UInt2 := 16#0#;
      --  destination allocated port 	This bit is used to allocate the master
      --  port for the destination transfer 	Note: This bit must be written
      --  when EN = 0. This bit is read-only when EN = 1.
      DAP            : Boolean := False;
      --  security attribute of the GPDMA transfer to the destination 	If
      --  GPDMA_SECCFGR.SECx = 1 and the access is secure: 	This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  GPDMA_SECCFGR.SECx = 1. A secure write is ignored when
      --  GPDMA_SECCFGR.SECx = 0. 	When GPDMA_SECCFGR.SECx is de-asserted,
      --  this DSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the GPDMA transfer
      --  to the destination is non-secure.
      DSEC           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C4TR1_Register use record
      SDW_LOG2       at 0 range 0 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      SINC           at 0 range 3 .. 3;
      SBL_1          at 0 range 4 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      PAM            at 0 range 11 .. 12;
      SBX            at 0 range 13 .. 13;
      SAP            at 0 range 14 .. 14;
      SSEC           at 0 range 15 .. 15;
      DDW_LOG2       at 0 range 16 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      DINC           at 0 range 19 .. 19;
      DBL_1          at 0 range 20 .. 25;
      DBX            at 0 range 26 .. 26;
      DHX            at 0 range 27 .. 27;
      Reserved_28_29 at 0 range 28 .. 29;
      DAP            at 0 range 30 .. 30;
      DSEC           at 0 range 31 .. 31;
   end record;

   subtype GPDMA_C4TR2_REQSEL_Field is HAL.UInt7;
   subtype GPDMA_C4TR2_TRIGM_Field is HAL.UInt2;
   subtype GPDMA_C4TR2_TRIGSEL_Field is HAL.UInt6;
   subtype GPDMA_C4TR2_TRIGPOL_Field is HAL.UInt2;
   subtype GPDMA_C4TR2_TCEM_Field is HAL.UInt2;

   --  GPDMA channel 4 transfer register 2
   type GPDMA_C4TR2_Register is record
      --  GPDMA hardware request selection 	These bits are ignored if channel x
      --  is activated (GPDMA_CxCR.EN asserted) with SWREQ = 1 (software
      --  request for a memory-to-memory transfer). Else, the selected hardware
      --  request is internally taken into account as per . 	The user must not
      --  assign a same input hardware request (same REQSEL[6:0] value) to
      --  different active GPDMA channels (GPDMA_CxCR.EN = 1 and
      --  GPDMA_CxTR2.SWREQ = 0 for these channels). GPDMA is not intended to
      --  hardware support the case of simultaneous enabled channels
      --  incorrectly configured with a same hardware peripheral request
      --  signal, and there is no user setting error reporting.
      REQSEL         : GPDMA_C4TR2_REQSEL_Field := 16#0#;
      --  unspecified
      Reserved_7_8   : HAL.UInt2 := 16#0#;
      --  software request 	This bit is internally taken into account when
      --  GPDMA_CxCR.EN is asserted.
      SWREQ          : Boolean := False;
      --  destination hardware request 	This bit is ignored if channel x is
      --  activated (GPDMA_CxCR.EN asserted) with SWREQ = 1 (software request
      --  for a memory-to-memory transfer). Else: 	Note:
      DREQ           : Boolean := False;
      --  Block hardware request 	If the channel x is activated (GPDMA_CxCR.EN
      --  asserted) with SWREQ = 1 (software request for a memory-to-memory
      --  transfer), this bit is ignored. Else:
      BREQ           : Boolean := False;
      --  unspecified
      Reserved_12_13 : HAL.UInt2 := 16#0#;
      --  trigger mode 	These bits define the transfer granularity for its
      --  conditioning by the trigger. 	If the channel x is enabled
      --  (GPDMA_CxCR.EN asserted) with TRIGPOL[1:0] = 00 or 11, these
      --  TRIGM[1:0] bits are ignored. 	Else, a GPDMA transfer is conditioned
      --  by at least one trigger hit: 	first burst read of a 2D/repeated block
      --  transfer is conditioned by one hit trigger. 	– If the peripheral is
      --  programmed as a source (DREQ = 0) of the LLI data transfer, each
      --  programmed burst read is conditioned. 	– If the peripheral is
      --  programmed as a destination (DREQ = 1) of the LLI data transfer,
      --  each programmed burst write is conditioned. The first memory burst
      --  read of a (possibly 2D/repeated) block, also named as the first ready
      --  FIFO-based source burst, is gated by the occurrence of both the
      --  hardware request and the first trigger hit. 	The GPDMA monitoring of
      --  a trigger for channel x is started when the channel is enabled/loaded
      --  with a new active trigger configuration: rising or falling edge on a
      --  selected trigger (TRIGPOL[1:0] = 01 or respectively
      --  TRIGPOL[1:0] = 10). 	The monitoring of this trigger is kept active
      --  during the triggered and uncompleted (data or link) transfer; and if
      --  a new trigger is detected then, this hit is internally memorized to
      --  grant the next transfer, as long as the defined rising or falling
      --  edge is not modified, and the TRIGSEL[5:0] is not modified, and the
      --  channel is enabled. 	Transferring a next LLIn+1 that updates the
      --  GPDMA_CxTR2 with a new value for any of TRIGSEL[5:0] or TRIGPOL[1:0],
      --  resets the monitoring, trashing the memorized hit of the formerly
      --  defined LLIn trigger. 	After a first new trigger hitn+1 is memorized,
      --  if another second trigger hitn+2 is detected and if the hitn
      --  triggered transfer is still not completed, hitn+2 is lost and not
      --  memorized.memorized. A trigger overrun flag is reported
      --  (GPDMA_CxSR.TOF =1 ), and an interrupt is generated if enabled
      --  (GPDMA_CxCR.TOIE = 1). The channel is not automatically disabled by
      --  hardware due to a trigger overrun. 	Note: When the source block size
      --  is not a multiple of the source burst size and is a multiple of the
      --  source data width, then the last programmed source burst is not
      --  completed and is internally shorten to match the block size. In this
      --  case, if TRIGM[1:0] = 11 and (SWREQ =1  or (SWREQ = 0 and
      --  DREQ =0 )), the shortened burst transfer (by singles or/and by
      --  bursts of lower length) is conditioned once by the trigger. 	When the
      --  programmed destination burst is internally shortened by singles
      --  or/and by bursts of lower length (versus FIFO size, versus block
      --  size, 1-Kbyte boundary address crossing): if the trigger is
      --  conditioning the programmed destination burst (if TRIGM[1:0] = 11
      --  and SWREQ = 0 and DREQ = 1), this shortened destination burst
      --  transfer is conditioned once by the trigger.
      TRIGM          : GPDMA_C4TR2_TRIGM_Field := 16#0#;
      --  trigger event input selection 	These bits select the trigger event
      --  input of the GPDMA transfer (as per ), with an active trigger event
      --  if TRIGPOL[1:0] ≠ 00.
      TRIGSEL        : GPDMA_C4TR2_TRIGSEL_Field := 16#0#;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  trigger event polarity 	These bits define the polarity of the
      --  selected trigger event input defined by TRIGSEL[5:0].
      TRIGPOL        : GPDMA_C4TR2_TRIGPOL_Field := 16#0#;
      --  unspecified
      Reserved_26_29 : HAL.UInt4 := 16#0#;
      --  transfer complete event mode 	These bits define the transfer
      --  granularity for the transfer complete and half transfer complete
      --  events generation. 	Note: If the initial LLI0 data transfer is
      --  null/void (directly programmed by the internal register file with
      --  GPDMA_CxBR1.BNDT[15:0] = 0), then neither the complete transfer
      --  event nor the half transfer event is generated. 	Note: If the initial
      --  LLI0 data transfer is null/void (directly programmed by the internal
      --  register file with GPDMA_CxBR1.BNDT[15:0] = 0), then neither the
      --  complete transfer event nor the half transfer event is generated.
      --  	Note: If the initial LLI0 data transfer is null/void (i.e. directly
      --  programmed by the internal register file with
      --  GPDMA_CxBR1.BNDT[15:0] =0 ), then the half transfer event is not
      --  generated, and the transfer complete event is generated when is
      --  completed the loading of the LLI1.
      TCEM           : GPDMA_C4TR2_TCEM_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C4TR2_Register use record
      REQSEL         at 0 range 0 .. 6;
      Reserved_7_8   at 0 range 7 .. 8;
      SWREQ          at 0 range 9 .. 9;
      DREQ           at 0 range 10 .. 10;
      BREQ           at 0 range 11 .. 11;
      Reserved_12_13 at 0 range 12 .. 13;
      TRIGM          at 0 range 14 .. 15;
      TRIGSEL        at 0 range 16 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      TRIGPOL        at 0 range 24 .. 25;
      Reserved_26_29 at 0 range 26 .. 29;
      TCEM           at 0 range 30 .. 31;
   end record;

   subtype GPDMA_C4BR1_BNDT_Field is HAL.UInt16;

   --  GPDMA channel 4 block register 1
   type GPDMA_C4BR1_Register is record
      --  block number of data bytes to transfer from the source 	Block size
      --  transferred from the source. When the channel is enabled, this field
      --  becomes read-only and is decremented, indicating the remaining number
      --  of data items in the current source block to be transferred.
      --  BNDT[15:0] is programmed in number of bytes, maximum source block
      --  size is 64 Kbytes -1. 	Once the last data transfer is completed
      --  (BNDT[15:0] = 0): 	- if GPDMA_CxLLR.UB1 = 1, this field is
      --  updated by the LLI in the memory. 	- if GPDMA_CxLLR.UB1 = 0 and if
      --  there is at least one non null Uxx update bit, this field is
      --  internally restored to the programmed value. 	- if all
      --  GPDMA_CxLLR.Uxx = 0 and if GPDMA_CxLLR.LA[15:0] = 0, this field
      --  is internally restored to the programmed value (infinite/continuous
      --  last LLI). 	- if GPDMA_CxLLR = 0, this field is kept as zero
      --  following the last LLI data transfer. 	Note: A non-null source block
      --  size must be a multiple of the source data width (BNDT[2:0] versus
      --  GPDMA_CxTR1.SDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued. 	When configured in packing mode
      --  (GPDMA_CxTR1.PAM[1] = 1 and destination data width different from
      --  source data width), a non-null source block size must be a multiple
      --  of the destination data width (BNDT[2:0] versus
      --  GPDMA_CxTR1.DDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued.
      BNDT           : GPDMA_C4BR1_BNDT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C4BR1_Register use record
      BNDT           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype GPDMA_C4LLR_LA_Field is HAL.UInt14;

   --  GPDMA channel 4 linked-list address register
   type GPDMA_C4LLR_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  pointer (16-bit low-significant address) to the next linked-list data
      --  structure 	If UT1 = UT2 = UB1 = USA = UDA = ULL = 0 and
      --  if LA[15:20] = 0, the current LLI is the last one. The channel
      --  transfer is completed without any update of the linked-list GPDMA
      --  register file. 	Else, this field is the pointer to the memory address
      --  offset from which the next linked-list data structure is
      --  automatically fetched from, once the data transfer is completed, in
      --  order to conditionally update the linked-list GPDMA internal register
      --  file (GPDMA_CxCTR1, GPDMA_CxTR2, GPDMA_CxBR1, GPDMA_CxSAR,
      --  GPDMA_CxDAR and GPDMA_CxLLR). 	Note: The user must program the
      --  pointer to be 32-bit aligned. The two low-significant bits are write
      --  ignored.
      LA             : GPDMA_C4LLR_LA_Field := 16#0#;
      --  Update GPDMA_CxLLR register from memory 	This bit is used to control
      --  the update of GPDMA_CxLLR from the memory during the link transfer.
      ULL            : Boolean := False;
      --  unspecified
      Reserved_17_26 : HAL.UInt10 := 16#0#;
      --  Update GPDMA_CxDAR register from memory 	This bit is used to control
      --  the update of GPDMA_CxDAR from the memory during the link transfer.
      UDA            : Boolean := False;
      --  update GPDMA_CxSAR from memory 	This bit controls the update of
      --  GPDMA_CxSAR from the memory during the link transfer.
      USA            : Boolean := False;
      --  Update GPDMA_CxBR1 from memory 	This bit controls the update of
      --  GPDMA_CxBR1 from the memory during the link transfer. If UB1 = 0
      --  and if GPDMA_CxLLR ≠ 0, the linked-list is not completed.
      --  GPDMA_CxBR1.BNDT[15:0] is then restored to the programmed value after
      --  data transfer is completed and before the link transfer.
      UB1            : Boolean := False;
      --  Update GPDMA_CxTR2 from memory 	This bit controls the update of
      --  GPDMA_CxTR2 from the memory during the link transfer.
      UT2            : Boolean := False;
      --  Update GPDMA_CxTR1 from memory 	This bit controls the update of
      --  GPDMA_CxTR1 from the memory during the link transfer.
      UT1            : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C4LLR_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      LA             at 0 range 2 .. 15;
      ULL            at 0 range 16 .. 16;
      Reserved_17_26 at 0 range 17 .. 26;
      UDA            at 0 range 27 .. 27;
      USA            at 0 range 28 .. 28;
      UB1            at 0 range 29 .. 29;
      UT2            at 0 range 30 .. 30;
      UT1            at 0 range 31 .. 31;
   end record;

   subtype GPDMA_C5LBAR_LBA_Field is HAL.UInt16;

   --  GPDMA channel 5 linked-list base address register
   type GPDMA_C5LBAR_Register is record
      --  unspecified
      Reserved_0_15 : HAL.UInt16 := 16#0#;
      --  linked-list base address of GPDMA channel x
      LBA           : GPDMA_C5LBAR_LBA_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C5LBAR_Register use record
      Reserved_0_15 at 0 range 0 .. 15;
      LBA           at 0 range 16 .. 31;
   end record;

   --  GPDMA channel 5 flag clear register
   type GPDMA_C5FCR_Register is record
      --  unspecified
      Reserved_0_7   : HAL.UInt8 := 16#0#;
      --  Write-only. transfer complete flag clear
      TCF            : Boolean := False;
      --  Write-only. half transfer flag clear
      HTF            : Boolean := False;
      --  Write-only. data transfer error flag clear
      DTEF           : Boolean := False;
      --  Write-only. update link transfer error flag clear
      ULEF           : Boolean := False;
      --  Write-only. user setting error flag clear
      USEF           : Boolean := False;
      --  Write-only. completed suspension flag clear
      SUSPF          : Boolean := False;
      --  Write-only. trigger overrun flag clear
      TOF            : Boolean := False;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C5FCR_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      TCF            at 0 range 8 .. 8;
      HTF            at 0 range 9 .. 9;
      DTEF           at 0 range 10 .. 10;
      ULEF           at 0 range 11 .. 11;
      USEF           at 0 range 12 .. 12;
      SUSPF          at 0 range 13 .. 13;
      TOF            at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype GPDMA_C5SR_FIFOL_Field is HAL.UInt8;

   --  GPDMA channel 5 status register
   type GPDMA_C5SR_Register is record
      --  Read-only. idle flag 	This idle flag is de-asserted by hardware when
      --  the channel is enabled (GPDMA_CxCR.EN = 1) with a valid channel
      --  configuration (no USEF to be immediately reported). 	This idle flag
      --  is asserted after hard reset or by hardware when the channel is back
      --  in idle state (in suspended or disabled state).
      IDLEF          : Boolean;
      --  unspecified
      Reserved_1_7   : HAL.UInt7;
      --  Read-only. transfer complete flag 	A transfer complete event is
      --  either a block transfer complete, a 2D/repeated block transfer
      --  complete, a LLI transfer complete including the upload of the next
      --  LLI if any, or the full linked-list completion, depending on the
      --  transfer complete event mode (GPDMA_CxTR2.TCEM[1:0]).
      TCF            : Boolean;
      --  Read-only. half transfer flag 	An half transfer event is either an
      --  half block transfer or an half 2D/repeated block transfer, depending
      --  on the transfer complete event mode (GPDMA_CxTR2.TCEM[1:0]). 	An half
      --  block transfer occurs when half of the bytes of the source block size
      --  (rounded up integer of GPDMA_CxBR1.BNDT[15:0]/2) has been transferred
      --  to the destination. 	An half 2D/repeated block transfer occurs when
      --  half of the repeated blocks (rounded up integer of
      --  (GPDMA_CxBR1.BRC[10:0]+1)/2)) has been transferred to the
      --  destination.
      HTF            : Boolean;
      --  Read-only. data transfer error flag
      DTEF           : Boolean;
      --  Read-only. update link transfer error flag
      ULEF           : Boolean;
      --  Read-only. user setting error flag
      USEF           : Boolean;
      --  Read-only. completed suspension flag
      SUSPF          : Boolean;
      --  Read-only. trigger overrun flag
      TOF            : Boolean;
      --  unspecified
      Reserved_15_15 : HAL.Bit;
      --  Read-only. monitored FIFO level 	Number of available write beats in
      --  the FIFO, in units of the programmed destination data width (see
      --  GPDMA_CxTR1.DDW_LOG2[1:0], in units of bytes, half-words, or words).
      --  	Note: After having suspended an active transfer, the user may need
      --  to read FIFOL[7:0], additionally to GPDMA_CxBR1.BDNT[15:0] and
      --  GPDMA_CxBR1.BRC[10:0], to know how many data have been transferred to
      --  the destination. Before reading, the user may wait for the transfer
      --  to be suspended (GPDMA_CxSR.SUSPF = 1).
      FIFOL          : GPDMA_C5SR_FIFOL_Field;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C5SR_Register use record
      IDLEF          at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      TCF            at 0 range 8 .. 8;
      HTF            at 0 range 9 .. 9;
      DTEF           at 0 range 10 .. 10;
      ULEF           at 0 range 11 .. 11;
      USEF           at 0 range 12 .. 12;
      SUSPF          at 0 range 13 .. 13;
      TOF            at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      FIFOL          at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype GPDMA_C5CR_PRIO_Field is HAL.UInt2;

   --  GPDMA channel 5 control register
   type GPDMA_C5CR_Register is record
      --  enable 	Writing 1 into the field RESET (bit 1) causes the hardware to
      --  de-assert this bit, whatever is written into this bit 0. Else: 	this
      --  bit is de-asserted by hardware when there is a transfer error (master
      --  bus error or user setting error) or when there is a channel transfer
      --  complete (channel ready to be configured, e.g. if LSM=1 at the end of
      --  a single execution of the LLI). 	Else, this bit can be asserted by
      --  software. 	Writing 0 into this EN bit is ignored.
      EN             : Boolean := False;
      --  Write-only. reset 	This bit is write only. Writing 0 has no impact.
      --  Writing 1 implies the reset of the following: the FIFO, the channel
      --  internal state, SUSP and EN bits (whatever is written receptively in
      --  bit 2 and bit 0). 	The reset is effective when the channel is in
      --  steady state, meaning one of the following: 	- active channel in
      --  suspended state (GPDMA_CxSR.SUSPF = 1 and
      --  GPDMA_CxSR.IDLEF = GPDMA_CxCR.EN = 1) 	- channel in disabled
      --  state (GPDMA_CxSR.IDLEF = 1 and GPDMA_CxCR.EN = 0). 	After
      --  writing a RESET, to continue using this channel, the user must
      --  explicitly reconfigure the channel including the hardware-modified
      --  configuration registers (GPDMA_CxBR1, GPDMA_CxSAR and GPDMA_CxDAR)
      --  before enabling again the channel (see the programming sequence in ).
      RESET          : Boolean := False;
      --  suspend 	Writing 1 into the field RESET (bit 1) causes the hardware
      --  to de-assert this bit, whatever is written into this bit 2. Else:
      --  	Software must write 1 in order to suspend an active channel i.e. a
      --  channel with an on-going GPDMA transfer over its master ports. 	The
      --  software must write 0 in order to resume a suspended channel,
      --  following the programming sequence detailed in .
      SUSP           : Boolean := False;
      --  unspecified
      Reserved_3_7   : HAL.UInt5 := 16#0#;
      --  transfer complete interrupt enable
      TCIE           : Boolean := False;
      --  half transfer complete interrupt enable
      HTIE           : Boolean := False;
      --  data transfer error interrupt enable
      DTEIE          : Boolean := False;
      --  update link transfer error interrupt enable
      ULEIE          : Boolean := False;
      --  user setting error interrupt enable
      USEIE          : Boolean := False;
      --  completed suspension interrupt enable
      SUSPIE         : Boolean := False;
      --  trigger overrun interrupt enable
      TOIE           : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Link step mode 	First the (possible 1D/repeated) block transfer is
      --  executed as defined by the current internal register file until
      --  GPDMA_CxBR1.BNDT[15:0] = 0 and GPDMA_CxBR1.BRC[10:0] = 0 if
      --  present. Secondly the next linked-list data structure is
      --  conditionally uploaded from memory as defined by GPDMA_CxLLR. Then
      --  channel execution is completed. 	Note: This bit must be written when
      --  EN=0. This bit is read-only when EN=1.
      LSM            : Boolean := False;
      --  linked-list allocated port 	This bit is used to allocate the master
      --  port for the update of the GPDMA linked-list registers from the
      --  memory. 	Note: This bit must be written when EN=0. This bit is
      --  read-only when EN=1.
      LAP            : Boolean := False;
      --  unspecified
      Reserved_18_21 : HAL.UInt4 := 16#0#;
      --  priority level of the channel x GPDMA transfer versus others 	Note:
      --  This bit must be written when EN = 0. This bit is read-only when
      --  EN = 1.
      PRIO           : GPDMA_C5CR_PRIO_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C5CR_Register use record
      EN             at 0 range 0 .. 0;
      RESET          at 0 range 1 .. 1;
      SUSP           at 0 range 2 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      TCIE           at 0 range 8 .. 8;
      HTIE           at 0 range 9 .. 9;
      DTEIE          at 0 range 10 .. 10;
      ULEIE          at 0 range 11 .. 11;
      USEIE          at 0 range 12 .. 12;
      SUSPIE         at 0 range 13 .. 13;
      TOIE           at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      LSM            at 0 range 16 .. 16;
      LAP            at 0 range 17 .. 17;
      Reserved_18_21 at 0 range 18 .. 21;
      PRIO           at 0 range 22 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype GPDMA_C5TR1_SDW_LOG2_Field is HAL.UInt2;
   subtype GPDMA_C5TR1_SBL_1_Field is HAL.UInt6;
   subtype GPDMA_C5TR1_PAM_Field is HAL.UInt2;
   subtype GPDMA_C5TR1_DDW_LOG2_Field is HAL.UInt2;
   subtype GPDMA_C5TR1_DBL_1_Field is HAL.UInt6;

   --  GPDMA channel 5 transfer register 1
   type GPDMA_C5TR1_Register is record
      --  binary logarithm of the source data width of a burst in bytes 	Note:
      --  Setting a 8-byte data width causes a user setting error to be
      --  reported and no transfer is issued. 	A source block size must be a
      --  multiple of the source data width (GPDMA_CxBR1.BNDT[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and no
      --  transfer is issued. 	A source single transfer must have an aligned
      --  address with its data width (start address GPDMA_CxSAR[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and none
      --  transfer is issued.
      SDW_LOG2       : GPDMA_C5TR1_SDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  source incrementing burst 	The source address, pointed by
      --  GPDMA_CxSAR, is kept constant after a burst beat/single transfer or
      --  is incremented by the offset value corresponding to a contiguous data
      --  after a burst beat/single transfer.
      SINC           : Boolean := False;
      --  source burst length minus 1, between 0 and 63 	The burst length unit
      --  is one data named beat within a burst. If SBL_1[5:0] =0 , the burst
      --  can be named as single. Each data/beat has a width defined by the
      --  destination data width SDW_LOG2[1:0]. 	Note: If a burst transfer
      --  crossed a 1-Kbyte address boundary on a AHB transfer, the GPDMA
      --  modifies and shortens the programmed burst into singles or bursts of
      --  lower length, to be compliant with the AHB protocol. 	If a burst
      --  transfer is of length greater than the FIFO size of the channel x,
      --  the GPDMA modifies and shortens the programmed burst into singles or
      --  bursts of lower length, to be compliant with the FIFO size. Transfer
      --  performance is lower, with GPDMA re-arbitration between effective and
      --  lower bursts/singles, but the data integrity is guaranteed.
      SBL_1          : GPDMA_C5TR1_SBL_1_Field := 16#0#;
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  padding/alignment mode 	If DDW_LOG2[1:0] = SDW_LOG2[1:0]: if the data
      --  width of a burst destination transfer is equal to the data width of a
      --  burst source transfer, these bits are ignored. 	Else: 	- Case 1: If
      --  destination data width > source data width 	1x: successive source
      --  data are FIFO queued and packed at the destination data width, in a
      --  left (LSB) to right (MSB) order (named little endian), before a
      --  destination transfer 	- Case 2: If destination data width < source
      --  data width 	1x: source data is FIFO queued and unpacked at the
      --  destination data width, to be transferred in a left (LSB) to right
      --  (MSB) order (named little endian) to the destination 	Note:
      PAM            : GPDMA_C5TR1_PAM_Field := 16#0#;
      --  source byte exchange within the unaligned half-word of each source
      --  word 	If the source data width is shorter than a word, this bit is
      --  ignored. 	If the source data width is a word:
      SBX            : Boolean := False;
      --  source allocated port 	This bit is used to allocate the master port
      --  for the source transfer 	Note: This bit must be written when
      --  EN = 0. This bit is read-only when EN = 1.
      SAP            : Boolean := False;
      --  security attribute of the GPDMA transfer from the source 	If
      --  GPDMA_SECCFGR.SECx = 1 and the access is secure: 	This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  GPDMA_SECCFGR.SECx =1 . A secure write is ignored when
      --  GPDMA_SECCFGR.SECx = 0. 	When GPDMA_SECCFGR.SECx is de-asserted,
      --  this SSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the GPDMA transfer
      --  from the source is non-secure.
      SSEC           : Boolean := False;
      --  binary logarithm of the destination data width of a burst, in bytes
      --  	Note: Setting a 8-byte data width causes a user setting error to be
      --  reported and none transfer is issued. 	A destination burst transfer
      --  must have an aligned address with its data width (start address
      --  GPDMA_CxDAR[2:0] and address offset GPDMA_CxTR3.DAO[2:0], versus
      --  DDW_LOG2[1:0]). Otherwise a user setting error is reported and no
      --  transfer is issued.
      DDW_LOG2       : GPDMA_C5TR1_DDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_18_18 : HAL.Bit := 16#0#;
      --  destination incrementing burst 	The destination address, pointed by
      --  GPDMA_CxDAR, is kept constant after a burst beat/single transfer, or
      --  is incremented by the offset value corresponding to a contiguous data
      --  after a burst beat/single transfer.
      DINC           : Boolean := False;
      --  destination burst length minus 1, between 0 and 63 	The burst length
      --  unit is one data named beat within a burst. If DBL_1[5:0] =0 , the
      --  burst can be named as single. Each data/beat has a width defined by
      --  the destination data width DDW_LOG2[1:0]. 	Note: If a burst transfer
      --  crossed a 1-Kbyte address boundary on a AHB transfer, the GPDMA
      --  modifies and shortens the programmed burst into singles or bursts of
      --  lower length, to be compliant with the AHB protocol. 	If a burst
      --  transfer is of length greater than the FIFO size of the channel x,
      --  the GPDMA modifies and shortens the programmed burst into singles or
      --  bursts of lower length, to be compliant with the FIFO size. Transfer
      --  performance is lower, with GPDMA re-arbitration between effective and
      --  lower bursts/singles, but the data integrity is guaranteed.
      DBL_1          : GPDMA_C5TR1_DBL_1_Field := 16#0#;
      --  destination byte exchange 	If the destination data size is a byte,
      --  this bit is ignored. 	If the destination data size is not a byte:
      DBX            : Boolean := False;
      --  destination half-word exchange 	If the destination data size is
      --  shorter than a word, this bit is ignored. 	If the destination data
      --  size is a word:
      DHX            : Boolean := False;
      --  unspecified
      Reserved_28_29 : HAL.UInt2 := 16#0#;
      --  destination allocated port 	This bit is used to allocate the master
      --  port for the destination transfer 	Note: This bit must be written
      --  when EN = 0. This bit is read-only when EN = 1.
      DAP            : Boolean := False;
      --  security attribute of the GPDMA transfer to the destination 	If
      --  GPDMA_SECCFGR.SECx = 1 and the access is secure: 	This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  GPDMA_SECCFGR.SECx = 1. A secure write is ignored when
      --  GPDMA_SECCFGR.SECx = 0. 	When GPDMA_SECCFGR.SECx is de-asserted,
      --  this DSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the GPDMA transfer
      --  to the destination is non-secure.
      DSEC           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C5TR1_Register use record
      SDW_LOG2       at 0 range 0 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      SINC           at 0 range 3 .. 3;
      SBL_1          at 0 range 4 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      PAM            at 0 range 11 .. 12;
      SBX            at 0 range 13 .. 13;
      SAP            at 0 range 14 .. 14;
      SSEC           at 0 range 15 .. 15;
      DDW_LOG2       at 0 range 16 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      DINC           at 0 range 19 .. 19;
      DBL_1          at 0 range 20 .. 25;
      DBX            at 0 range 26 .. 26;
      DHX            at 0 range 27 .. 27;
      Reserved_28_29 at 0 range 28 .. 29;
      DAP            at 0 range 30 .. 30;
      DSEC           at 0 range 31 .. 31;
   end record;

   subtype GPDMA_C5TR2_REQSEL_Field is HAL.UInt7;
   subtype GPDMA_C5TR2_TRIGM_Field is HAL.UInt2;
   subtype GPDMA_C5TR2_TRIGSEL_Field is HAL.UInt6;
   subtype GPDMA_C5TR2_TRIGPOL_Field is HAL.UInt2;
   subtype GPDMA_C5TR2_TCEM_Field is HAL.UInt2;

   --  GPDMA channel 5 transfer register 2
   type GPDMA_C5TR2_Register is record
      --  GPDMA hardware request selection 	These bits are ignored if channel x
      --  is activated (GPDMA_CxCR.EN asserted) with SWREQ = 1 (software
      --  request for a memory-to-memory transfer). Else, the selected hardware
      --  request is internally taken into account as per . 	The user must not
      --  assign a same input hardware request (same REQSEL[6:0] value) to
      --  different active GPDMA channels (GPDMA_CxCR.EN = 1 and
      --  GPDMA_CxTR2.SWREQ = 0 for these channels). GPDMA is not intended to
      --  hardware support the case of simultaneous enabled channels
      --  incorrectly configured with a same hardware peripheral request
      --  signal, and there is no user setting error reporting.
      REQSEL         : GPDMA_C5TR2_REQSEL_Field := 16#0#;
      --  unspecified
      Reserved_7_8   : HAL.UInt2 := 16#0#;
      --  software request 	This bit is internally taken into account when
      --  GPDMA_CxCR.EN is asserted.
      SWREQ          : Boolean := False;
      --  destination hardware request 	This bit is ignored if channel x is
      --  activated (GPDMA_CxCR.EN asserted) with SWREQ = 1 (software request
      --  for a memory-to-memory transfer). Else: 	Note:
      DREQ           : Boolean := False;
      --  Block hardware request 	If the channel x is activated (GPDMA_CxCR.EN
      --  asserted) with SWREQ = 1 (software request for a memory-to-memory
      --  transfer), this bit is ignored. Else:
      BREQ           : Boolean := False;
      --  unspecified
      Reserved_12_13 : HAL.UInt2 := 16#0#;
      --  trigger mode 	These bits define the transfer granularity for its
      --  conditioning by the trigger. 	If the channel x is enabled
      --  (GPDMA_CxCR.EN asserted) with TRIGPOL[1:0] = 00 or 11, these
      --  TRIGM[1:0] bits are ignored. 	Else, a GPDMA transfer is conditioned
      --  by at least one trigger hit: 	first burst read of a 2D/repeated block
      --  transfer is conditioned by one hit trigger. 	– If the peripheral is
      --  programmed as a source (DREQ = 0) of the LLI data transfer, each
      --  programmed burst read is conditioned. 	– If the peripheral is
      --  programmed as a destination (DREQ = 1) of the LLI data transfer,
      --  each programmed burst write is conditioned. The first memory burst
      --  read of a (possibly 2D/repeated) block, also named as the first ready
      --  FIFO-based source burst, is gated by the occurrence of both the
      --  hardware request and the first trigger hit. 	The GPDMA monitoring of
      --  a trigger for channel x is started when the channel is enabled/loaded
      --  with a new active trigger configuration: rising or falling edge on a
      --  selected trigger (TRIGPOL[1:0] = 01 or respectively
      --  TRIGPOL[1:0] = 10). 	The monitoring of this trigger is kept active
      --  during the triggered and uncompleted (data or link) transfer; and if
      --  a new trigger is detected then, this hit is internally memorized to
      --  grant the next transfer, as long as the defined rising or falling
      --  edge is not modified, and the TRIGSEL[5:0] is not modified, and the
      --  channel is enabled. 	Transferring a next LLIn+1 that updates the
      --  GPDMA_CxTR2 with a new value for any of TRIGSEL[5:0] or TRIGPOL[1:0],
      --  resets the monitoring, trashing the memorized hit of the formerly
      --  defined LLIn trigger. 	After a first new trigger hitn+1 is memorized,
      --  if another second trigger hitn+2 is detected and if the hitn
      --  triggered transfer is still not completed, hitn+2 is lost and not
      --  memorized.memorized. A trigger overrun flag is reported
      --  (GPDMA_CxSR.TOF =1 ), and an interrupt is generated if enabled
      --  (GPDMA_CxCR.TOIE = 1). The channel is not automatically disabled by
      --  hardware due to a trigger overrun. 	Note: When the source block size
      --  is not a multiple of the source burst size and is a multiple of the
      --  source data width, then the last programmed source burst is not
      --  completed and is internally shorten to match the block size. In this
      --  case, if TRIGM[1:0] = 11 and (SWREQ =1  or (SWREQ = 0 and
      --  DREQ =0 )), the shortened burst transfer (by singles or/and by
      --  bursts of lower length) is conditioned once by the trigger. 	When the
      --  programmed destination burst is internally shortened by singles
      --  or/and by bursts of lower length (versus FIFO size, versus block
      --  size, 1-Kbyte boundary address crossing): if the trigger is
      --  conditioning the programmed destination burst (if TRIGM[1:0] = 11
      --  and SWREQ = 0 and DREQ = 1), this shortened destination burst
      --  transfer is conditioned once by the trigger.
      TRIGM          : GPDMA_C5TR2_TRIGM_Field := 16#0#;
      --  trigger event input selection 	These bits select the trigger event
      --  input of the GPDMA transfer (as per ), with an active trigger event
      --  if TRIGPOL[1:0] ≠ 00.
      TRIGSEL        : GPDMA_C5TR2_TRIGSEL_Field := 16#0#;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  trigger event polarity 	These bits define the polarity of the
      --  selected trigger event input defined by TRIGSEL[5:0].
      TRIGPOL        : GPDMA_C5TR2_TRIGPOL_Field := 16#0#;
      --  unspecified
      Reserved_26_29 : HAL.UInt4 := 16#0#;
      --  transfer complete event mode 	These bits define the transfer
      --  granularity for the transfer complete and half transfer complete
      --  events generation. 	Note: If the initial LLI0 data transfer is
      --  null/void (directly programmed by the internal register file with
      --  GPDMA_CxBR1.BNDT[15:0] = 0), then neither the complete transfer
      --  event nor the half transfer event is generated. 	Note: If the initial
      --  LLI0 data transfer is null/void (directly programmed by the internal
      --  register file with GPDMA_CxBR1.BNDT[15:0] = 0), then neither the
      --  complete transfer event nor the half transfer event is generated.
      --  	Note: If the initial LLI0 data transfer is null/void (i.e. directly
      --  programmed by the internal register file with
      --  GPDMA_CxBR1.BNDT[15:0] =0 ), then the half transfer event is not
      --  generated, and the transfer complete event is generated when is
      --  completed the loading of the LLI1.
      TCEM           : GPDMA_C5TR2_TCEM_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C5TR2_Register use record
      REQSEL         at 0 range 0 .. 6;
      Reserved_7_8   at 0 range 7 .. 8;
      SWREQ          at 0 range 9 .. 9;
      DREQ           at 0 range 10 .. 10;
      BREQ           at 0 range 11 .. 11;
      Reserved_12_13 at 0 range 12 .. 13;
      TRIGM          at 0 range 14 .. 15;
      TRIGSEL        at 0 range 16 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      TRIGPOL        at 0 range 24 .. 25;
      Reserved_26_29 at 0 range 26 .. 29;
      TCEM           at 0 range 30 .. 31;
   end record;

   subtype GPDMA_C5BR1_BNDT_Field is HAL.UInt16;

   --  GPDMA channel 5 block register 1
   type GPDMA_C5BR1_Register is record
      --  block number of data bytes to transfer from the source 	Block size
      --  transferred from the source. When the channel is enabled, this field
      --  becomes read-only and is decremented, indicating the remaining number
      --  of data items in the current source block to be transferred.
      --  BNDT[15:0] is programmed in number of bytes, maximum source block
      --  size is 64 Kbytes -1. 	Once the last data transfer is completed
      --  (BNDT[15:0] = 0): 	- if GPDMA_CxLLR.UB1 = 1, this field is
      --  updated by the LLI in the memory. 	- if GPDMA_CxLLR.UB1 = 0 and if
      --  there is at least one non null Uxx update bit, this field is
      --  internally restored to the programmed value. 	- if all
      --  GPDMA_CxLLR.Uxx = 0 and if GPDMA_CxLLR.LA[15:0] = 0, this field
      --  is internally restored to the programmed value (infinite/continuous
      --  last LLI). 	- if GPDMA_CxLLR = 0, this field is kept as zero
      --  following the last LLI data transfer. 	Note: A non-null source block
      --  size must be a multiple of the source data width (BNDT[2:0] versus
      --  GPDMA_CxTR1.SDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued. 	When configured in packing mode
      --  (GPDMA_CxTR1.PAM[1] = 1 and destination data width different from
      --  source data width), a non-null source block size must be a multiple
      --  of the destination data width (BNDT[2:0] versus
      --  GPDMA_CxTR1.DDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued.
      BNDT           : GPDMA_C5BR1_BNDT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C5BR1_Register use record
      BNDT           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype GPDMA_C5LLR_LA_Field is HAL.UInt14;

   --  GPDMA channel 5 linked-list address register
   type GPDMA_C5LLR_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  pointer (16-bit low-significant address) to the next linked-list data
      --  structure 	If UT1 = UT2 = UB1 = USA = UDA = ULL = 0 and
      --  if LA[15:20] = 0, the current LLI is the last one. The channel
      --  transfer is completed without any update of the linked-list GPDMA
      --  register file. 	Else, this field is the pointer to the memory address
      --  offset from which the next linked-list data structure is
      --  automatically fetched from, once the data transfer is completed, in
      --  order to conditionally update the linked-list GPDMA internal register
      --  file (GPDMA_CxCTR1, GPDMA_CxTR2, GPDMA_CxBR1, GPDMA_CxSAR,
      --  GPDMA_CxDAR and GPDMA_CxLLR). 	Note: The user must program the
      --  pointer to be 32-bit aligned. The two low-significant bits are write
      --  ignored.
      LA             : GPDMA_C5LLR_LA_Field := 16#0#;
      --  Update GPDMA_CxLLR register from memory 	This bit is used to control
      --  the update of GPDMA_CxLLR from the memory during the link transfer.
      ULL            : Boolean := False;
      --  unspecified
      Reserved_17_26 : HAL.UInt10 := 16#0#;
      --  Update GPDMA_CxDAR register from memory 	This bit is used to control
      --  the update of GPDMA_CxDAR from the memory during the link transfer.
      UDA            : Boolean := False;
      --  update GPDMA_CxSAR from memory 	This bit controls the update of
      --  GPDMA_CxSAR from the memory during the link transfer.
      USA            : Boolean := False;
      --  Update GPDMA_CxBR1 from memory 	This bit controls the update of
      --  GPDMA_CxBR1 from the memory during the link transfer. If UB1 = 0
      --  and if GPDMA_CxLLR ≠ 0, the linked-list is not completed.
      --  GPDMA_CxBR1.BNDT[15:0] is then restored to the programmed value after
      --  data transfer is completed and before the link transfer.
      UB1            : Boolean := False;
      --  Update GPDMA_CxTR2 from memory 	This bit controls the update of
      --  GPDMA_CxTR2 from the memory during the link transfer.
      UT2            : Boolean := False;
      --  Update GPDMA_CxTR1 from memory 	This bit controls the update of
      --  GPDMA_CxTR1 from the memory during the link transfer.
      UT1            : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C5LLR_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      LA             at 0 range 2 .. 15;
      ULL            at 0 range 16 .. 16;
      Reserved_17_26 at 0 range 17 .. 26;
      UDA            at 0 range 27 .. 27;
      USA            at 0 range 28 .. 28;
      UB1            at 0 range 29 .. 29;
      UT2            at 0 range 30 .. 30;
      UT1            at 0 range 31 .. 31;
   end record;

   subtype GPDMA_C6LBAR_LBA_Field is HAL.UInt16;

   --  GPDMA channel 6 linked-list base address register
   type GPDMA_C6LBAR_Register is record
      --  unspecified
      Reserved_0_15 : HAL.UInt16 := 16#0#;
      --  linked-list base address of GPDMA channel x
      LBA           : GPDMA_C6LBAR_LBA_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C6LBAR_Register use record
      Reserved_0_15 at 0 range 0 .. 15;
      LBA           at 0 range 16 .. 31;
   end record;

   --  GPDMA channel 6 flag clear register
   type GPDMA_C6FCR_Register is record
      --  unspecified
      Reserved_0_7   : HAL.UInt8 := 16#0#;
      --  Write-only. transfer complete flag clear
      TCF            : Boolean := False;
      --  Write-only. half transfer flag clear
      HTF            : Boolean := False;
      --  Write-only. data transfer error flag clear
      DTEF           : Boolean := False;
      --  Write-only. update link transfer error flag clear
      ULEF           : Boolean := False;
      --  Write-only. user setting error flag clear
      USEF           : Boolean := False;
      --  Write-only. completed suspension flag clear
      SUSPF          : Boolean := False;
      --  Write-only. trigger overrun flag clear
      TOF            : Boolean := False;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C6FCR_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      TCF            at 0 range 8 .. 8;
      HTF            at 0 range 9 .. 9;
      DTEF           at 0 range 10 .. 10;
      ULEF           at 0 range 11 .. 11;
      USEF           at 0 range 12 .. 12;
      SUSPF          at 0 range 13 .. 13;
      TOF            at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype GPDMA_C6SR_FIFOL_Field is HAL.UInt8;

   --  GPDMA channel 6 status register
   type GPDMA_C6SR_Register is record
      --  Read-only. idle flag 	This idle flag is de-asserted by hardware when
      --  the channel is enabled (GPDMA_CxCR.EN = 1) with a valid channel
      --  configuration (no USEF to be immediately reported). 	This idle flag
      --  is asserted after hard reset or by hardware when the channel is back
      --  in idle state (in suspended or disabled state).
      IDLEF          : Boolean;
      --  unspecified
      Reserved_1_7   : HAL.UInt7;
      --  Read-only. transfer complete flag 	A transfer complete event is
      --  either a block transfer complete, a 2D/repeated block transfer
      --  complete, a LLI transfer complete including the upload of the next
      --  LLI if any, or the full linked-list completion, depending on the
      --  transfer complete event mode (GPDMA_CxTR2.TCEM[1:0]).
      TCF            : Boolean;
      --  Read-only. half transfer flag 	An half transfer event is either an
      --  half block transfer or an half 2D/repeated block transfer, depending
      --  on the transfer complete event mode (GPDMA_CxTR2.TCEM[1:0]). 	An half
      --  block transfer occurs when half of the bytes of the source block size
      --  (rounded up integer of GPDMA_CxBR1.BNDT[15:0]/2) has been transferred
      --  to the destination. 	An half 2D/repeated block transfer occurs when
      --  half of the repeated blocks (rounded up integer of
      --  (GPDMA_CxBR1.BRC[10:0]+1)/2)) has been transferred to the
      --  destination.
      HTF            : Boolean;
      --  Read-only. data transfer error flag
      DTEF           : Boolean;
      --  Read-only. update link transfer error flag
      ULEF           : Boolean;
      --  Read-only. user setting error flag
      USEF           : Boolean;
      --  Read-only. completed suspension flag
      SUSPF          : Boolean;
      --  Read-only. trigger overrun flag
      TOF            : Boolean;
      --  unspecified
      Reserved_15_15 : HAL.Bit;
      --  Read-only. monitored FIFO level 	Number of available write beats in
      --  the FIFO, in units of the programmed destination data width (see
      --  GPDMA_CxTR1.DDW_LOG2[1:0], in units of bytes, half-words, or words).
      --  	Note: After having suspended an active transfer, the user may need
      --  to read FIFOL[7:0], additionally to GPDMA_CxBR1.BDNT[15:0] and
      --  GPDMA_CxBR1.BRC[10:0], to know how many data have been transferred to
      --  the destination. Before reading, the user may wait for the transfer
      --  to be suspended (GPDMA_CxSR.SUSPF = 1).
      FIFOL          : GPDMA_C6SR_FIFOL_Field;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C6SR_Register use record
      IDLEF          at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      TCF            at 0 range 8 .. 8;
      HTF            at 0 range 9 .. 9;
      DTEF           at 0 range 10 .. 10;
      ULEF           at 0 range 11 .. 11;
      USEF           at 0 range 12 .. 12;
      SUSPF          at 0 range 13 .. 13;
      TOF            at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      FIFOL          at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype GPDMA_C6CR_PRIO_Field is HAL.UInt2;

   --  GPDMA channel 6 control register
   type GPDMA_C6CR_Register is record
      --  enable 	Writing 1 into the field RESET (bit 1) causes the hardware to
      --  de-assert this bit, whatever is written into this bit 0. Else: 	this
      --  bit is de-asserted by hardware when there is a transfer error (master
      --  bus error or user setting error) or when there is a channel transfer
      --  complete (channel ready to be configured, e.g. if LSM=1 at the end of
      --  a single execution of the LLI). 	Else, this bit can be asserted by
      --  software. 	Writing 0 into this EN bit is ignored.
      EN             : Boolean := False;
      --  Write-only. reset 	This bit is write only. Writing 0 has no impact.
      --  Writing 1 implies the reset of the following: the FIFO, the channel
      --  internal state, SUSP and EN bits (whatever is written receptively in
      --  bit 2 and bit 0). 	The reset is effective when the channel is in
      --  steady state, meaning one of the following: 	- active channel in
      --  suspended state (GPDMA_CxSR.SUSPF = 1 and
      --  GPDMA_CxSR.IDLEF = GPDMA_CxCR.EN = 1) 	- channel in disabled
      --  state (GPDMA_CxSR.IDLEF = 1 and GPDMA_CxCR.EN = 0). 	After
      --  writing a RESET, to continue using this channel, the user must
      --  explicitly reconfigure the channel including the hardware-modified
      --  configuration registers (GPDMA_CxBR1, GPDMA_CxSAR and GPDMA_CxDAR)
      --  before enabling again the channel (see the programming sequence in ).
      RESET          : Boolean := False;
      --  suspend 	Writing 1 into the field RESET (bit 1) causes the hardware
      --  to de-assert this bit, whatever is written into this bit 2. Else:
      --  	Software must write 1 in order to suspend an active channel i.e. a
      --  channel with an on-going GPDMA transfer over its master ports. 	The
      --  software must write 0 in order to resume a suspended channel,
      --  following the programming sequence detailed in .
      SUSP           : Boolean := False;
      --  unspecified
      Reserved_3_7   : HAL.UInt5 := 16#0#;
      --  transfer complete interrupt enable
      TCIE           : Boolean := False;
      --  half transfer complete interrupt enable
      HTIE           : Boolean := False;
      --  data transfer error interrupt enable
      DTEIE          : Boolean := False;
      --  update link transfer error interrupt enable
      ULEIE          : Boolean := False;
      --  user setting error interrupt enable
      USEIE          : Boolean := False;
      --  completed suspension interrupt enable
      SUSPIE         : Boolean := False;
      --  trigger overrun interrupt enable
      TOIE           : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Link step mode 	First the (possible 1D/repeated) block transfer is
      --  executed as defined by the current internal register file until
      --  GPDMA_CxBR1.BNDT[15:0] = 0 and GPDMA_CxBR1.BRC[10:0] = 0 if
      --  present. Secondly the next linked-list data structure is
      --  conditionally uploaded from memory as defined by GPDMA_CxLLR. Then
      --  channel execution is completed. 	Note: This bit must be written when
      --  EN=0. This bit is read-only when EN=1.
      LSM            : Boolean := False;
      --  linked-list allocated port 	This bit is used to allocate the master
      --  port for the update of the GPDMA linked-list registers from the
      --  memory. 	Note: This bit must be written when EN=0. This bit is
      --  read-only when EN=1.
      LAP            : Boolean := False;
      --  unspecified
      Reserved_18_21 : HAL.UInt4 := 16#0#;
      --  priority level of the channel x GPDMA transfer versus others 	Note:
      --  This bit must be written when EN = 0. This bit is read-only when
      --  EN = 1.
      PRIO           : GPDMA_C6CR_PRIO_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C6CR_Register use record
      EN             at 0 range 0 .. 0;
      RESET          at 0 range 1 .. 1;
      SUSP           at 0 range 2 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      TCIE           at 0 range 8 .. 8;
      HTIE           at 0 range 9 .. 9;
      DTEIE          at 0 range 10 .. 10;
      ULEIE          at 0 range 11 .. 11;
      USEIE          at 0 range 12 .. 12;
      SUSPIE         at 0 range 13 .. 13;
      TOIE           at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      LSM            at 0 range 16 .. 16;
      LAP            at 0 range 17 .. 17;
      Reserved_18_21 at 0 range 18 .. 21;
      PRIO           at 0 range 22 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype GPDMA_C6TR1_SDW_LOG2_Field is HAL.UInt2;
   subtype GPDMA_C6TR1_SBL_1_Field is HAL.UInt6;
   subtype GPDMA_C6TR1_PAM_Field is HAL.UInt2;
   subtype GPDMA_C6TR1_DDW_LOG2_Field is HAL.UInt2;
   subtype GPDMA_C6TR1_DBL_1_Field is HAL.UInt6;

   --  GPDMA channel 6 transfer register 1
   type GPDMA_C6TR1_Register is record
      --  binary logarithm of the source data width of a burst in bytes 	Note:
      --  Setting a 8-byte data width causes a user setting error to be
      --  reported and no transfer is issued. 	A source block size must be a
      --  multiple of the source data width (GPDMA_CxBR1.BNDT[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and no
      --  transfer is issued. 	A source single transfer must have an aligned
      --  address with its data width (start address GPDMA_CxSAR[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and none
      --  transfer is issued.
      SDW_LOG2       : GPDMA_C6TR1_SDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  source incrementing burst 	The source address, pointed by
      --  GPDMA_CxSAR, is kept constant after a burst beat/single transfer or
      --  is incremented by the offset value corresponding to a contiguous data
      --  after a burst beat/single transfer.
      SINC           : Boolean := False;
      --  source burst length minus 1, between 0 and 63 	The burst length unit
      --  is one data named beat within a burst. If SBL_1[5:0] =0 , the burst
      --  can be named as single. Each data/beat has a width defined by the
      --  destination data width SDW_LOG2[1:0]. 	Note: If a burst transfer
      --  crossed a 1-Kbyte address boundary on a AHB transfer, the GPDMA
      --  modifies and shortens the programmed burst into singles or bursts of
      --  lower length, to be compliant with the AHB protocol. 	If a burst
      --  transfer is of length greater than the FIFO size of the channel x,
      --  the GPDMA modifies and shortens the programmed burst into singles or
      --  bursts of lower length, to be compliant with the FIFO size. Transfer
      --  performance is lower, with GPDMA re-arbitration between effective and
      --  lower bursts/singles, but the data integrity is guaranteed.
      SBL_1          : GPDMA_C6TR1_SBL_1_Field := 16#0#;
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  padding/alignment mode 	If DDW_LOG2[1:0] = SDW_LOG2[1:0]: if the data
      --  width of a burst destination transfer is equal to the data width of a
      --  burst source transfer, these bits are ignored. 	Else: 	- Case 1: If
      --  destination data width > source data width 	1x: successive source
      --  data are FIFO queued and packed at the destination data width, in a
      --  left (LSB) to right (MSB) order (named little endian), before a
      --  destination transfer 	- Case 2: If destination data width < source
      --  data width 	1x: source data is FIFO queued and unpacked at the
      --  destination data width, to be transferred in a left (LSB) to right
      --  (MSB) order (named little endian) to the destination 	Note:
      PAM            : GPDMA_C6TR1_PAM_Field := 16#0#;
      --  source byte exchange within the unaligned half-word of each source
      --  word 	If the source data width is shorter than a word, this bit is
      --  ignored. 	If the source data width is a word:
      SBX            : Boolean := False;
      --  source allocated port 	This bit is used to allocate the master port
      --  for the source transfer 	Note: This bit must be written when
      --  EN = 0. This bit is read-only when EN = 1.
      SAP            : Boolean := False;
      --  security attribute of the GPDMA transfer from the source 	If
      --  GPDMA_SECCFGR.SECx = 1 and the access is secure: 	This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  GPDMA_SECCFGR.SECx =1 . A secure write is ignored when
      --  GPDMA_SECCFGR.SECx = 0. 	When GPDMA_SECCFGR.SECx is de-asserted,
      --  this SSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the GPDMA transfer
      --  from the source is non-secure.
      SSEC           : Boolean := False;
      --  binary logarithm of the destination data width of a burst, in bytes
      --  	Note: Setting a 8-byte data width causes a user setting error to be
      --  reported and none transfer is issued. 	A destination burst transfer
      --  must have an aligned address with its data width (start address
      --  GPDMA_CxDAR[2:0] and address offset GPDMA_CxTR3.DAO[2:0], versus
      --  DDW_LOG2[1:0]). Otherwise a user setting error is reported and no
      --  transfer is issued.
      DDW_LOG2       : GPDMA_C6TR1_DDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_18_18 : HAL.Bit := 16#0#;
      --  destination incrementing burst 	The destination address, pointed by
      --  GPDMA_CxDAR, is kept constant after a burst beat/single transfer, or
      --  is incremented by the offset value corresponding to a contiguous data
      --  after a burst beat/single transfer.
      DINC           : Boolean := False;
      --  destination burst length minus 1, between 0 and 63 	The burst length
      --  unit is one data named beat within a burst. If DBL_1[5:0] =0 , the
      --  burst can be named as single. Each data/beat has a width defined by
      --  the destination data width DDW_LOG2[1:0]. 	Note: If a burst transfer
      --  crossed a 1-Kbyte address boundary on a AHB transfer, the GPDMA
      --  modifies and shortens the programmed burst into singles or bursts of
      --  lower length, to be compliant with the AHB protocol. 	If a burst
      --  transfer is of length greater than the FIFO size of the channel x,
      --  the GPDMA modifies and shortens the programmed burst into singles or
      --  bursts of lower length, to be compliant with the FIFO size. Transfer
      --  performance is lower, with GPDMA re-arbitration between effective and
      --  lower bursts/singles, but the data integrity is guaranteed.
      DBL_1          : GPDMA_C6TR1_DBL_1_Field := 16#0#;
      --  destination byte exchange 	If the destination data size is a byte,
      --  this bit is ignored. 	If the destination data size is not a byte:
      DBX            : Boolean := False;
      --  destination half-word exchange 	If the destination data size is
      --  shorter than a word, this bit is ignored. 	If the destination data
      --  size is a word:
      DHX            : Boolean := False;
      --  unspecified
      Reserved_28_29 : HAL.UInt2 := 16#0#;
      --  destination allocated port 	This bit is used to allocate the master
      --  port for the destination transfer 	Note: This bit must be written
      --  when EN = 0. This bit is read-only when EN = 1.
      DAP            : Boolean := False;
      --  security attribute of the GPDMA transfer to the destination 	If
      --  GPDMA_SECCFGR.SECx = 1 and the access is secure: 	This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  GPDMA_SECCFGR.SECx = 1. A secure write is ignored when
      --  GPDMA_SECCFGR.SECx = 0. 	When GPDMA_SECCFGR.SECx is de-asserted,
      --  this DSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the GPDMA transfer
      --  to the destination is non-secure.
      DSEC           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C6TR1_Register use record
      SDW_LOG2       at 0 range 0 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      SINC           at 0 range 3 .. 3;
      SBL_1          at 0 range 4 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      PAM            at 0 range 11 .. 12;
      SBX            at 0 range 13 .. 13;
      SAP            at 0 range 14 .. 14;
      SSEC           at 0 range 15 .. 15;
      DDW_LOG2       at 0 range 16 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      DINC           at 0 range 19 .. 19;
      DBL_1          at 0 range 20 .. 25;
      DBX            at 0 range 26 .. 26;
      DHX            at 0 range 27 .. 27;
      Reserved_28_29 at 0 range 28 .. 29;
      DAP            at 0 range 30 .. 30;
      DSEC           at 0 range 31 .. 31;
   end record;

   subtype GPDMA_C6TR2_REQSEL_Field is HAL.UInt7;
   subtype GPDMA_C6TR2_TRIGM_Field is HAL.UInt2;
   subtype GPDMA_C6TR2_TRIGSEL_Field is HAL.UInt6;
   subtype GPDMA_C6TR2_TRIGPOL_Field is HAL.UInt2;
   subtype GPDMA_C6TR2_TCEM_Field is HAL.UInt2;

   --  GPDMA channel 6 transfer register 2
   type GPDMA_C6TR2_Register is record
      --  GPDMA hardware request selection 	These bits are ignored if channel x
      --  is activated (GPDMA_CxCR.EN asserted) with SWREQ = 1 (software
      --  request for a memory-to-memory transfer). Else, the selected hardware
      --  request is internally taken into account as per . 	The user must not
      --  assign a same input hardware request (same REQSEL[6:0] value) to
      --  different active GPDMA channels (GPDMA_CxCR.EN = 1 and
      --  GPDMA_CxTR2.SWREQ = 0 for these channels). GPDMA is not intended to
      --  hardware support the case of simultaneous enabled channels
      --  incorrectly configured with a same hardware peripheral request
      --  signal, and there is no user setting error reporting.
      REQSEL         : GPDMA_C6TR2_REQSEL_Field := 16#0#;
      --  unspecified
      Reserved_7_8   : HAL.UInt2 := 16#0#;
      --  software request 	This bit is internally taken into account when
      --  GPDMA_CxCR.EN is asserted.
      SWREQ          : Boolean := False;
      --  destination hardware request 	This bit is ignored if channel x is
      --  activated (GPDMA_CxCR.EN asserted) with SWREQ = 1 (software request
      --  for a memory-to-memory transfer). Else: 	Note:
      DREQ           : Boolean := False;
      --  Block hardware request 	If the channel x is activated (GPDMA_CxCR.EN
      --  asserted) with SWREQ = 1 (software request for a memory-to-memory
      --  transfer), this bit is ignored. Else:
      BREQ           : Boolean := False;
      --  unspecified
      Reserved_12_13 : HAL.UInt2 := 16#0#;
      --  trigger mode 	These bits define the transfer granularity for its
      --  conditioning by the trigger. 	If the channel x is enabled
      --  (GPDMA_CxCR.EN asserted) with TRIGPOL[1:0] = 00 or 11, these
      --  TRIGM[1:0] bits are ignored. 	Else, a GPDMA transfer is conditioned
      --  by at least one trigger hit: 	first burst read of a 2D/repeated block
      --  transfer is conditioned by one hit trigger. 	– If the peripheral is
      --  programmed as a source (DREQ = 0) of the LLI data transfer, each
      --  programmed burst read is conditioned. 	– If the peripheral is
      --  programmed as a destination (DREQ = 1) of the LLI data transfer,
      --  each programmed burst write is conditioned. The first memory burst
      --  read of a (possibly 2D/repeated) block, also named as the first ready
      --  FIFO-based source burst, is gated by the occurrence of both the
      --  hardware request and the first trigger hit. 	The GPDMA monitoring of
      --  a trigger for channel x is started when the channel is enabled/loaded
      --  with a new active trigger configuration: rising or falling edge on a
      --  selected trigger (TRIGPOL[1:0] = 01 or respectively
      --  TRIGPOL[1:0] = 10). 	The monitoring of this trigger is kept active
      --  during the triggered and uncompleted (data or link) transfer; and if
      --  a new trigger is detected then, this hit is internally memorized to
      --  grant the next transfer, as long as the defined rising or falling
      --  edge is not modified, and the TRIGSEL[5:0] is not modified, and the
      --  channel is enabled. 	Transferring a next LLIn+1 that updates the
      --  GPDMA_CxTR2 with a new value for any of TRIGSEL[5:0] or TRIGPOL[1:0],
      --  resets the monitoring, trashing the memorized hit of the formerly
      --  defined LLIn trigger. 	After a first new trigger hitn+1 is memorized,
      --  if another second trigger hitn+2 is detected and if the hitn
      --  triggered transfer is still not completed, hitn+2 is lost and not
      --  memorized.memorized. A trigger overrun flag is reported
      --  (GPDMA_CxSR.TOF =1 ), and an interrupt is generated if enabled
      --  (GPDMA_CxCR.TOIE = 1). The channel is not automatically disabled by
      --  hardware due to a trigger overrun. 	Note: When the source block size
      --  is not a multiple of the source burst size and is a multiple of the
      --  source data width, then the last programmed source burst is not
      --  completed and is internally shorten to match the block size. In this
      --  case, if TRIGM[1:0] = 11 and (SWREQ =1  or (SWREQ = 0 and
      --  DREQ =0 )), the shortened burst transfer (by singles or/and by
      --  bursts of lower length) is conditioned once by the trigger. 	When the
      --  programmed destination burst is internally shortened by singles
      --  or/and by bursts of lower length (versus FIFO size, versus block
      --  size, 1-Kbyte boundary address crossing): if the trigger is
      --  conditioning the programmed destination burst (if TRIGM[1:0] = 11
      --  and SWREQ = 0 and DREQ = 1), this shortened destination burst
      --  transfer is conditioned once by the trigger.
      TRIGM          : GPDMA_C6TR2_TRIGM_Field := 16#0#;
      --  trigger event input selection 	These bits select the trigger event
      --  input of the GPDMA transfer (as per ), with an active trigger event
      --  if TRIGPOL[1:0] ≠ 00.
      TRIGSEL        : GPDMA_C6TR2_TRIGSEL_Field := 16#0#;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  trigger event polarity 	These bits define the polarity of the
      --  selected trigger event input defined by TRIGSEL[5:0].
      TRIGPOL        : GPDMA_C6TR2_TRIGPOL_Field := 16#0#;
      --  unspecified
      Reserved_26_29 : HAL.UInt4 := 16#0#;
      --  transfer complete event mode 	These bits define the transfer
      --  granularity for the transfer complete and half transfer complete
      --  events generation. 	Note: If the initial LLI0 data transfer is
      --  null/void (directly programmed by the internal register file with
      --  GPDMA_CxBR1.BNDT[15:0] = 0), then neither the complete transfer
      --  event nor the half transfer event is generated. 	Note: If the initial
      --  LLI0 data transfer is null/void (directly programmed by the internal
      --  register file with GPDMA_CxBR1.BNDT[15:0] = 0), then neither the
      --  complete transfer event nor the half transfer event is generated.
      --  	Note: If the initial LLI0 data transfer is null/void (i.e. directly
      --  programmed by the internal register file with
      --  GPDMA_CxBR1.BNDT[15:0] =0 ), then the half transfer event is not
      --  generated, and the transfer complete event is generated when is
      --  completed the loading of the LLI1.
      TCEM           : GPDMA_C6TR2_TCEM_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C6TR2_Register use record
      REQSEL         at 0 range 0 .. 6;
      Reserved_7_8   at 0 range 7 .. 8;
      SWREQ          at 0 range 9 .. 9;
      DREQ           at 0 range 10 .. 10;
      BREQ           at 0 range 11 .. 11;
      Reserved_12_13 at 0 range 12 .. 13;
      TRIGM          at 0 range 14 .. 15;
      TRIGSEL        at 0 range 16 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      TRIGPOL        at 0 range 24 .. 25;
      Reserved_26_29 at 0 range 26 .. 29;
      TCEM           at 0 range 30 .. 31;
   end record;

   subtype GPDMA_C6BR1_BNDT_Field is HAL.UInt16;

   --  GPDMA channel 6 block register 1
   type GPDMA_C6BR1_Register is record
      --  block number of data bytes to transfer from the source 	Block size
      --  transferred from the source. When the channel is enabled, this field
      --  becomes read-only and is decremented, indicating the remaining number
      --  of data items in the current source block to be transferred.
      --  BNDT[15:0] is programmed in number of bytes, maximum source block
      --  size is 64 Kbytes -1. 	Once the last data transfer is completed
      --  (BNDT[15:0] = 0): 	- if GPDMA_CxLLR.UB1 = 1, this field is
      --  updated by the LLI in the memory. 	- if GPDMA_CxLLR.UB1 = 0 and if
      --  there is at least one non null Uxx update bit, this field is
      --  internally restored to the programmed value. 	- if all
      --  GPDMA_CxLLR.Uxx = 0 and if GPDMA_CxLLR.LA[15:0] = 0, this field
      --  is internally restored to the programmed value (infinite/continuous
      --  last LLI). 	- if GPDMA_CxLLR = 0, this field is kept as zero
      --  following the last LLI data transfer. 	Note: A non-null source block
      --  size must be a multiple of the source data width (BNDT[2:0] versus
      --  GPDMA_CxTR1.SDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued. 	When configured in packing mode
      --  (GPDMA_CxTR1.PAM[1] = 1 and destination data width different from
      --  source data width), a non-null source block size must be a multiple
      --  of the destination data width (BNDT[2:0] versus
      --  GPDMA_CxTR1.DDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued.
      BNDT           : GPDMA_C6BR1_BNDT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C6BR1_Register use record
      BNDT           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype GPDMA_C6LLR_LA_Field is HAL.UInt14;

   --  GPDMA channel 6 linked-list address register
   type GPDMA_C6LLR_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  pointer (16-bit low-significant address) to the next linked-list data
      --  structure 	If UT1 = UT2 = UB1 = USA = UDA = ULL = 0 and
      --  if LA[15:20] = 0, the current LLI is the last one. The channel
      --  transfer is completed without any update of the linked-list GPDMA
      --  register file. 	Else, this field is the pointer to the memory address
      --  offset from which the next linked-list data structure is
      --  automatically fetched from, once the data transfer is completed, in
      --  order to conditionally update the linked-list GPDMA internal register
      --  file (GPDMA_CxCTR1, GPDMA_CxTR2, GPDMA_CxBR1, GPDMA_CxSAR,
      --  GPDMA_CxDAR and GPDMA_CxLLR). 	Note: The user must program the
      --  pointer to be 32-bit aligned. The two low-significant bits are write
      --  ignored.
      LA             : GPDMA_C6LLR_LA_Field := 16#0#;
      --  Update GPDMA_CxLLR register from memory 	This bit is used to control
      --  the update of GPDMA_CxLLR from the memory during the link transfer.
      ULL            : Boolean := False;
      --  unspecified
      Reserved_17_26 : HAL.UInt10 := 16#0#;
      --  Update GPDMA_CxDAR register from memory 	This bit is used to control
      --  the update of GPDMA_CxDAR from the memory during the link transfer.
      UDA            : Boolean := False;
      --  update GPDMA_CxSAR from memory 	This bit controls the update of
      --  GPDMA_CxSAR from the memory during the link transfer.
      USA            : Boolean := False;
      --  Update GPDMA_CxBR1 from memory 	This bit controls the update of
      --  GPDMA_CxBR1 from the memory during the link transfer. If UB1 = 0
      --  and if GPDMA_CxLLR ≠ 0, the linked-list is not completed.
      --  GPDMA_CxBR1.BNDT[15:0] is then restored to the programmed value after
      --  data transfer is completed and before the link transfer.
      UB1            : Boolean := False;
      --  Update GPDMA_CxTR2 from memory 	This bit controls the update of
      --  GPDMA_CxTR2 from the memory during the link transfer.
      UT2            : Boolean := False;
      --  Update GPDMA_CxTR1 from memory 	This bit controls the update of
      --  GPDMA_CxTR1 from the memory during the link transfer.
      UT1            : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C6LLR_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      LA             at 0 range 2 .. 15;
      ULL            at 0 range 16 .. 16;
      Reserved_17_26 at 0 range 17 .. 26;
      UDA            at 0 range 27 .. 27;
      USA            at 0 range 28 .. 28;
      UB1            at 0 range 29 .. 29;
      UT2            at 0 range 30 .. 30;
      UT1            at 0 range 31 .. 31;
   end record;

   subtype GPDMA_C7LBAR_LBA_Field is HAL.UInt16;

   --  GPDMA channel 7 linked-list base address register
   type GPDMA_C7LBAR_Register is record
      --  unspecified
      Reserved_0_15 : HAL.UInt16 := 16#0#;
      --  linked-list base address of GPDMA channel x
      LBA           : GPDMA_C7LBAR_LBA_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C7LBAR_Register use record
      Reserved_0_15 at 0 range 0 .. 15;
      LBA           at 0 range 16 .. 31;
   end record;

   --  GPDMA channel 7 flag clear register
   type GPDMA_C7FCR_Register is record
      --  unspecified
      Reserved_0_7   : HAL.UInt8 := 16#0#;
      --  Write-only. transfer complete flag clear
      TCF            : Boolean := False;
      --  Write-only. half transfer flag clear
      HTF            : Boolean := False;
      --  Write-only. data transfer error flag clear
      DTEF           : Boolean := False;
      --  Write-only. update link transfer error flag clear
      ULEF           : Boolean := False;
      --  Write-only. user setting error flag clear
      USEF           : Boolean := False;
      --  Write-only. completed suspension flag clear
      SUSPF          : Boolean := False;
      --  Write-only. trigger overrun flag clear
      TOF            : Boolean := False;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C7FCR_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      TCF            at 0 range 8 .. 8;
      HTF            at 0 range 9 .. 9;
      DTEF           at 0 range 10 .. 10;
      ULEF           at 0 range 11 .. 11;
      USEF           at 0 range 12 .. 12;
      SUSPF          at 0 range 13 .. 13;
      TOF            at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype GPDMA_C7SR_FIFOL_Field is HAL.UInt8;

   --  GPDMA channel 7 status register
   type GPDMA_C7SR_Register is record
      --  Read-only. idle flag 	This idle flag is de-asserted by hardware when
      --  the channel is enabled (GPDMA_CxCR.EN = 1) with a valid channel
      --  configuration (no USEF to be immediately reported). 	This idle flag
      --  is asserted after hard reset or by hardware when the channel is back
      --  in idle state (in suspended or disabled state).
      IDLEF          : Boolean;
      --  unspecified
      Reserved_1_7   : HAL.UInt7;
      --  Read-only. transfer complete flag 	A transfer complete event is
      --  either a block transfer complete, a 2D/repeated block transfer
      --  complete, a LLI transfer complete including the upload of the next
      --  LLI if any, or the full linked-list completion, depending on the
      --  transfer complete event mode (GPDMA_CxTR2.TCEM[1:0]).
      TCF            : Boolean;
      --  Read-only. half transfer flag 	An half transfer event is either an
      --  half block transfer or an half 2D/repeated block transfer, depending
      --  on the transfer complete event mode (GPDMA_CxTR2.TCEM[1:0]). 	An half
      --  block transfer occurs when half of the bytes of the source block size
      --  (rounded up integer of GPDMA_CxBR1.BNDT[15:0]/2) has been transferred
      --  to the destination. 	An half 2D/repeated block transfer occurs when
      --  half of the repeated blocks (rounded up integer of
      --  (GPDMA_CxBR1.BRC[10:0]+1)/2)) has been transferred to the
      --  destination.
      HTF            : Boolean;
      --  Read-only. data transfer error flag
      DTEF           : Boolean;
      --  Read-only. update link transfer error flag
      ULEF           : Boolean;
      --  Read-only. user setting error flag
      USEF           : Boolean;
      --  Read-only. completed suspension flag
      SUSPF          : Boolean;
      --  Read-only. trigger overrun flag
      TOF            : Boolean;
      --  unspecified
      Reserved_15_15 : HAL.Bit;
      --  Read-only. monitored FIFO level 	Number of available write beats in
      --  the FIFO, in units of the programmed destination data width (see
      --  GPDMA_CxTR1.DDW_LOG2[1:0], in units of bytes, half-words, or words).
      --  	Note: After having suspended an active transfer, the user may need
      --  to read FIFOL[7:0], additionally to GPDMA_CxBR1.BDNT[15:0] and
      --  GPDMA_CxBR1.BRC[10:0], to know how many data have been transferred to
      --  the destination. Before reading, the user may wait for the transfer
      --  to be suspended (GPDMA_CxSR.SUSPF = 1).
      FIFOL          : GPDMA_C7SR_FIFOL_Field;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C7SR_Register use record
      IDLEF          at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      TCF            at 0 range 8 .. 8;
      HTF            at 0 range 9 .. 9;
      DTEF           at 0 range 10 .. 10;
      ULEF           at 0 range 11 .. 11;
      USEF           at 0 range 12 .. 12;
      SUSPF          at 0 range 13 .. 13;
      TOF            at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      FIFOL          at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype GPDMA_C7CR_PRIO_Field is HAL.UInt2;

   --  GPDMA channel 7 control register
   type GPDMA_C7CR_Register is record
      --  enable 	Writing 1 into the field RESET (bit 1) causes the hardware to
      --  de-assert this bit, whatever is written into this bit 0. Else: 	this
      --  bit is de-asserted by hardware when there is a transfer error (master
      --  bus error or user setting error) or when there is a channel transfer
      --  complete (channel ready to be configured, e.g. if LSM=1 at the end of
      --  a single execution of the LLI). 	Else, this bit can be asserted by
      --  software. 	Writing 0 into this EN bit is ignored.
      EN             : Boolean := False;
      --  Write-only. reset 	This bit is write only. Writing 0 has no impact.
      --  Writing 1 implies the reset of the following: the FIFO, the channel
      --  internal state, SUSP and EN bits (whatever is written receptively in
      --  bit 2 and bit 0). 	The reset is effective when the channel is in
      --  steady state, meaning one of the following: 	- active channel in
      --  suspended state (GPDMA_CxSR.SUSPF = 1 and
      --  GPDMA_CxSR.IDLEF = GPDMA_CxCR.EN = 1) 	- channel in disabled
      --  state (GPDMA_CxSR.IDLEF = 1 and GPDMA_CxCR.EN = 0). 	After
      --  writing a RESET, to continue using this channel, the user must
      --  explicitly reconfigure the channel including the hardware-modified
      --  configuration registers (GPDMA_CxBR1, GPDMA_CxSAR and GPDMA_CxDAR)
      --  before enabling again the channel (see the programming sequence in ).
      RESET          : Boolean := False;
      --  suspend 	Writing 1 into the field RESET (bit 1) causes the hardware
      --  to de-assert this bit, whatever is written into this bit 2. Else:
      --  	Software must write 1 in order to suspend an active channel i.e. a
      --  channel with an on-going GPDMA transfer over its master ports. 	The
      --  software must write 0 in order to resume a suspended channel,
      --  following the programming sequence detailed in .
      SUSP           : Boolean := False;
      --  unspecified
      Reserved_3_7   : HAL.UInt5 := 16#0#;
      --  transfer complete interrupt enable
      TCIE           : Boolean := False;
      --  half transfer complete interrupt enable
      HTIE           : Boolean := False;
      --  data transfer error interrupt enable
      DTEIE          : Boolean := False;
      --  update link transfer error interrupt enable
      ULEIE          : Boolean := False;
      --  user setting error interrupt enable
      USEIE          : Boolean := False;
      --  completed suspension interrupt enable
      SUSPIE         : Boolean := False;
      --  trigger overrun interrupt enable
      TOIE           : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Link step mode 	First the (possible 1D/repeated) block transfer is
      --  executed as defined by the current internal register file until
      --  GPDMA_CxBR1.BNDT[15:0] = 0 and GPDMA_CxBR1.BRC[10:0] = 0 if
      --  present. Secondly the next linked-list data structure is
      --  conditionally uploaded from memory as defined by GPDMA_CxLLR. Then
      --  channel execution is completed. 	Note: This bit must be written when
      --  EN=0. This bit is read-only when EN=1.
      LSM            : Boolean := False;
      --  linked-list allocated port 	This bit is used to allocate the master
      --  port for the update of the GPDMA linked-list registers from the
      --  memory. 	Note: This bit must be written when EN=0. This bit is
      --  read-only when EN=1.
      LAP            : Boolean := False;
      --  unspecified
      Reserved_18_21 : HAL.UInt4 := 16#0#;
      --  priority level of the channel x GPDMA transfer versus others 	Note:
      --  This bit must be written when EN = 0. This bit is read-only when
      --  EN = 1.
      PRIO           : GPDMA_C7CR_PRIO_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C7CR_Register use record
      EN             at 0 range 0 .. 0;
      RESET          at 0 range 1 .. 1;
      SUSP           at 0 range 2 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      TCIE           at 0 range 8 .. 8;
      HTIE           at 0 range 9 .. 9;
      DTEIE          at 0 range 10 .. 10;
      ULEIE          at 0 range 11 .. 11;
      USEIE          at 0 range 12 .. 12;
      SUSPIE         at 0 range 13 .. 13;
      TOIE           at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      LSM            at 0 range 16 .. 16;
      LAP            at 0 range 17 .. 17;
      Reserved_18_21 at 0 range 18 .. 21;
      PRIO           at 0 range 22 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype GPDMA_C7TR1_SDW_LOG2_Field is HAL.UInt2;
   subtype GPDMA_C7TR1_SBL_1_Field is HAL.UInt6;
   subtype GPDMA_C7TR1_PAM_Field is HAL.UInt2;
   subtype GPDMA_C7TR1_DDW_LOG2_Field is HAL.UInt2;
   subtype GPDMA_C7TR1_DBL_1_Field is HAL.UInt6;

   --  GPDMA channel 7 transfer register 1
   type GPDMA_C7TR1_Register is record
      --  binary logarithm of the source data width of a burst in bytes 	Note:
      --  Setting a 8-byte data width causes a user setting error to be
      --  reported and no transfer is issued. 	A source block size must be a
      --  multiple of the source data width (GPDMA_CxBR1.BNDT[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and no
      --  transfer is issued. 	A source single transfer must have an aligned
      --  address with its data width (start address GPDMA_CxSAR[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and none
      --  transfer is issued.
      SDW_LOG2       : GPDMA_C7TR1_SDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  source incrementing burst 	The source address, pointed by
      --  GPDMA_CxSAR, is kept constant after a burst beat/single transfer or
      --  is incremented by the offset value corresponding to a contiguous data
      --  after a burst beat/single transfer.
      SINC           : Boolean := False;
      --  source burst length minus 1, between 0 and 63 	The burst length unit
      --  is one data named beat within a burst. If SBL_1[5:0] =0 , the burst
      --  can be named as single. Each data/beat has a width defined by the
      --  destination data width SDW_LOG2[1:0]. 	Note: If a burst transfer
      --  crossed a 1-Kbyte address boundary on a AHB transfer, the GPDMA
      --  modifies and shortens the programmed burst into singles or bursts of
      --  lower length, to be compliant with the AHB protocol. 	If a burst
      --  transfer is of length greater than the FIFO size of the channel x,
      --  the GPDMA modifies and shortens the programmed burst into singles or
      --  bursts of lower length, to be compliant with the FIFO size. Transfer
      --  performance is lower, with GPDMA re-arbitration between effective and
      --  lower bursts/singles, but the data integrity is guaranteed.
      SBL_1          : GPDMA_C7TR1_SBL_1_Field := 16#0#;
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  padding/alignment mode 	If DDW_LOG2[1:0] = SDW_LOG2[1:0]: if the data
      --  width of a burst destination transfer is equal to the data width of a
      --  burst source transfer, these bits are ignored. 	Else: 	- Case 1: If
      --  destination data width > source data width 	1x: successive source
      --  data are FIFO queued and packed at the destination data width, in a
      --  left (LSB) to right (MSB) order (named little endian), before a
      --  destination transfer 	- Case 2: If destination data width < source
      --  data width 	1x: source data is FIFO queued and unpacked at the
      --  destination data width, to be transferred in a left (LSB) to right
      --  (MSB) order (named little endian) to the destination 	Note:
      PAM            : GPDMA_C7TR1_PAM_Field := 16#0#;
      --  source byte exchange within the unaligned half-word of each source
      --  word 	If the source data width is shorter than a word, this bit is
      --  ignored. 	If the source data width is a word:
      SBX            : Boolean := False;
      --  source allocated port 	This bit is used to allocate the master port
      --  for the source transfer 	Note: This bit must be written when
      --  EN = 0. This bit is read-only when EN = 1.
      SAP            : Boolean := False;
      --  security attribute of the GPDMA transfer from the source 	If
      --  GPDMA_SECCFGR.SECx = 1 and the access is secure: 	This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  GPDMA_SECCFGR.SECx =1 . A secure write is ignored when
      --  GPDMA_SECCFGR.SECx = 0. 	When GPDMA_SECCFGR.SECx is de-asserted,
      --  this SSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the GPDMA transfer
      --  from the source is non-secure.
      SSEC           : Boolean := False;
      --  binary logarithm of the destination data width of a burst, in bytes
      --  	Note: Setting a 8-byte data width causes a user setting error to be
      --  reported and none transfer is issued. 	A destination burst transfer
      --  must have an aligned address with its data width (start address
      --  GPDMA_CxDAR[2:0] and address offset GPDMA_CxTR3.DAO[2:0], versus
      --  DDW_LOG2[1:0]). Otherwise a user setting error is reported and no
      --  transfer is issued.
      DDW_LOG2       : GPDMA_C7TR1_DDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_18_18 : HAL.Bit := 16#0#;
      --  destination incrementing burst 	The destination address, pointed by
      --  GPDMA_CxDAR, is kept constant after a burst beat/single transfer, or
      --  is incremented by the offset value corresponding to a contiguous data
      --  after a burst beat/single transfer.
      DINC           : Boolean := False;
      --  destination burst length minus 1, between 0 and 63 	The burst length
      --  unit is one data named beat within a burst. If DBL_1[5:0] =0 , the
      --  burst can be named as single. Each data/beat has a width defined by
      --  the destination data width DDW_LOG2[1:0]. 	Note: If a burst transfer
      --  crossed a 1-Kbyte address boundary on a AHB transfer, the GPDMA
      --  modifies and shortens the programmed burst into singles or bursts of
      --  lower length, to be compliant with the AHB protocol. 	If a burst
      --  transfer is of length greater than the FIFO size of the channel x,
      --  the GPDMA modifies and shortens the programmed burst into singles or
      --  bursts of lower length, to be compliant with the FIFO size. Transfer
      --  performance is lower, with GPDMA re-arbitration between effective and
      --  lower bursts/singles, but the data integrity is guaranteed.
      DBL_1          : GPDMA_C7TR1_DBL_1_Field := 16#0#;
      --  destination byte exchange 	If the destination data size is a byte,
      --  this bit is ignored. 	If the destination data size is not a byte:
      DBX            : Boolean := False;
      --  destination half-word exchange 	If the destination data size is
      --  shorter than a word, this bit is ignored. 	If the destination data
      --  size is a word:
      DHX            : Boolean := False;
      --  unspecified
      Reserved_28_29 : HAL.UInt2 := 16#0#;
      --  destination allocated port 	This bit is used to allocate the master
      --  port for the destination transfer 	Note: This bit must be written
      --  when EN = 0. This bit is read-only when EN = 1.
      DAP            : Boolean := False;
      --  security attribute of the GPDMA transfer to the destination 	If
      --  GPDMA_SECCFGR.SECx = 1 and the access is secure: 	This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  GPDMA_SECCFGR.SECx = 1. A secure write is ignored when
      --  GPDMA_SECCFGR.SECx = 0. 	When GPDMA_SECCFGR.SECx is de-asserted,
      --  this DSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the GPDMA transfer
      --  to the destination is non-secure.
      DSEC           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C7TR1_Register use record
      SDW_LOG2       at 0 range 0 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      SINC           at 0 range 3 .. 3;
      SBL_1          at 0 range 4 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      PAM            at 0 range 11 .. 12;
      SBX            at 0 range 13 .. 13;
      SAP            at 0 range 14 .. 14;
      SSEC           at 0 range 15 .. 15;
      DDW_LOG2       at 0 range 16 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      DINC           at 0 range 19 .. 19;
      DBL_1          at 0 range 20 .. 25;
      DBX            at 0 range 26 .. 26;
      DHX            at 0 range 27 .. 27;
      Reserved_28_29 at 0 range 28 .. 29;
      DAP            at 0 range 30 .. 30;
      DSEC           at 0 range 31 .. 31;
   end record;

   subtype GPDMA_C7TR2_REQSEL_Field is HAL.UInt7;
   subtype GPDMA_C7TR2_TRIGM_Field is HAL.UInt2;
   subtype GPDMA_C7TR2_TRIGSEL_Field is HAL.UInt6;
   subtype GPDMA_C7TR2_TRIGPOL_Field is HAL.UInt2;
   subtype GPDMA_C7TR2_TCEM_Field is HAL.UInt2;

   --  GPDMA channel 7 transfer register 2
   type GPDMA_C7TR2_Register is record
      --  GPDMA hardware request selection 	These bits are ignored if channel x
      --  is activated (GPDMA_CxCR.EN asserted) with SWREQ = 1 (software
      --  request for a memory-to-memory transfer). Else, the selected hardware
      --  request is internally taken into account as per . 	The user must not
      --  assign a same input hardware request (same REQSEL[6:0] value) to
      --  different active GPDMA channels (GPDMA_CxCR.EN = 1 and
      --  GPDMA_CxTR2.SWREQ = 0 for these channels). GPDMA is not intended to
      --  hardware support the case of simultaneous enabled channels
      --  incorrectly configured with a same hardware peripheral request
      --  signal, and there is no user setting error reporting.
      REQSEL         : GPDMA_C7TR2_REQSEL_Field := 16#0#;
      --  unspecified
      Reserved_7_8   : HAL.UInt2 := 16#0#;
      --  software request 	This bit is internally taken into account when
      --  GPDMA_CxCR.EN is asserted.
      SWREQ          : Boolean := False;
      --  destination hardware request 	This bit is ignored if channel x is
      --  activated (GPDMA_CxCR.EN asserted) with SWREQ = 1 (software request
      --  for a memory-to-memory transfer). Else: 	Note:
      DREQ           : Boolean := False;
      --  Block hardware request 	If the channel x is activated (GPDMA_CxCR.EN
      --  asserted) with SWREQ = 1 (software request for a memory-to-memory
      --  transfer), this bit is ignored. Else:
      BREQ           : Boolean := False;
      --  unspecified
      Reserved_12_13 : HAL.UInt2 := 16#0#;
      --  trigger mode 	These bits define the transfer granularity for its
      --  conditioning by the trigger. 	If the channel x is enabled
      --  (GPDMA_CxCR.EN asserted) with TRIGPOL[1:0] = 00 or 11, these
      --  TRIGM[1:0] bits are ignored. 	Else, a GPDMA transfer is conditioned
      --  by at least one trigger hit: 	first burst read of a 2D/repeated block
      --  transfer is conditioned by one hit trigger. 	– If the peripheral is
      --  programmed as a source (DREQ = 0) of the LLI data transfer, each
      --  programmed burst read is conditioned. 	– If the peripheral is
      --  programmed as a destination (DREQ = 1) of the LLI data transfer,
      --  each programmed burst write is conditioned. The first memory burst
      --  read of a (possibly 2D/repeated) block, also named as the first ready
      --  FIFO-based source burst, is gated by the occurrence of both the
      --  hardware request and the first trigger hit. 	The GPDMA monitoring of
      --  a trigger for channel x is started when the channel is enabled/loaded
      --  with a new active trigger configuration: rising or falling edge on a
      --  selected trigger (TRIGPOL[1:0] = 01 or respectively
      --  TRIGPOL[1:0] = 10). 	The monitoring of this trigger is kept active
      --  during the triggered and uncompleted (data or link) transfer; and if
      --  a new trigger is detected then, this hit is internally memorized to
      --  grant the next transfer, as long as the defined rising or falling
      --  edge is not modified, and the TRIGSEL[5:0] is not modified, and the
      --  channel is enabled. 	Transferring a next LLIn+1 that updates the
      --  GPDMA_CxTR2 with a new value for any of TRIGSEL[5:0] or TRIGPOL[1:0],
      --  resets the monitoring, trashing the memorized hit of the formerly
      --  defined LLIn trigger. 	After a first new trigger hitn+1 is memorized,
      --  if another second trigger hitn+2 is detected and if the hitn
      --  triggered transfer is still not completed, hitn+2 is lost and not
      --  memorized.memorized. A trigger overrun flag is reported
      --  (GPDMA_CxSR.TOF =1 ), and an interrupt is generated if enabled
      --  (GPDMA_CxCR.TOIE = 1). The channel is not automatically disabled by
      --  hardware due to a trigger overrun. 	Note: When the source block size
      --  is not a multiple of the source burst size and is a multiple of the
      --  source data width, then the last programmed source burst is not
      --  completed and is internally shorten to match the block size. In this
      --  case, if TRIGM[1:0] = 11 and (SWREQ =1  or (SWREQ = 0 and
      --  DREQ =0 )), the shortened burst transfer (by singles or/and by
      --  bursts of lower length) is conditioned once by the trigger. 	When the
      --  programmed destination burst is internally shortened by singles
      --  or/and by bursts of lower length (versus FIFO size, versus block
      --  size, 1-Kbyte boundary address crossing): if the trigger is
      --  conditioning the programmed destination burst (if TRIGM[1:0] = 11
      --  and SWREQ = 0 and DREQ = 1), this shortened destination burst
      --  transfer is conditioned once by the trigger.
      TRIGM          : GPDMA_C7TR2_TRIGM_Field := 16#0#;
      --  trigger event input selection 	These bits select the trigger event
      --  input of the GPDMA transfer (as per ), with an active trigger event
      --  if TRIGPOL[1:0] ≠ 00.
      TRIGSEL        : GPDMA_C7TR2_TRIGSEL_Field := 16#0#;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  trigger event polarity 	These bits define the polarity of the
      --  selected trigger event input defined by TRIGSEL[5:0].
      TRIGPOL        : GPDMA_C7TR2_TRIGPOL_Field := 16#0#;
      --  unspecified
      Reserved_26_29 : HAL.UInt4 := 16#0#;
      --  transfer complete event mode 	These bits define the transfer
      --  granularity for the transfer complete and half transfer complete
      --  events generation. 	Note: If the initial LLI0 data transfer is
      --  null/void (directly programmed by the internal register file with
      --  GPDMA_CxBR1.BNDT[15:0] = 0), then neither the complete transfer
      --  event nor the half transfer event is generated. 	Note: If the initial
      --  LLI0 data transfer is null/void (directly programmed by the internal
      --  register file with GPDMA_CxBR1.BNDT[15:0] = 0), then neither the
      --  complete transfer event nor the half transfer event is generated.
      --  	Note: If the initial LLI0 data transfer is null/void (i.e. directly
      --  programmed by the internal register file with
      --  GPDMA_CxBR1.BNDT[15:0] =0 ), then the half transfer event is not
      --  generated, and the transfer complete event is generated when is
      --  completed the loading of the LLI1.
      TCEM           : GPDMA_C7TR2_TCEM_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C7TR2_Register use record
      REQSEL         at 0 range 0 .. 6;
      Reserved_7_8   at 0 range 7 .. 8;
      SWREQ          at 0 range 9 .. 9;
      DREQ           at 0 range 10 .. 10;
      BREQ           at 0 range 11 .. 11;
      Reserved_12_13 at 0 range 12 .. 13;
      TRIGM          at 0 range 14 .. 15;
      TRIGSEL        at 0 range 16 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      TRIGPOL        at 0 range 24 .. 25;
      Reserved_26_29 at 0 range 26 .. 29;
      TCEM           at 0 range 30 .. 31;
   end record;

   subtype GPDMA_C7BR1_BNDT_Field is HAL.UInt16;

   --  GPDMA channel 7 block register 1
   type GPDMA_C7BR1_Register is record
      --  block number of data bytes to transfer from the source 	Block size
      --  transferred from the source. When the channel is enabled, this field
      --  becomes read-only and is decremented, indicating the remaining number
      --  of data items in the current source block to be transferred.
      --  BNDT[15:0] is programmed in number of bytes, maximum source block
      --  size is 64 Kbytes -1. 	Once the last data transfer is completed
      --  (BNDT[15:0] = 0): 	- if GPDMA_CxLLR.UB1 = 1, this field is
      --  updated by the LLI in the memory. 	- if GPDMA_CxLLR.UB1 = 0 and if
      --  there is at least one non null Uxx update bit, this field is
      --  internally restored to the programmed value. 	- if all
      --  GPDMA_CxLLR.Uxx = 0 and if GPDMA_CxLLR.LA[15:0] = 0, this field
      --  is internally restored to the programmed value (infinite/continuous
      --  last LLI). 	- if GPDMA_CxLLR = 0, this field is kept as zero
      --  following the last LLI data transfer. 	Note: A non-null source block
      --  size must be a multiple of the source data width (BNDT[2:0] versus
      --  GPDMA_CxTR1.SDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued. 	When configured in packing mode
      --  (GPDMA_CxTR1.PAM[1] = 1 and destination data width different from
      --  source data width), a non-null source block size must be a multiple
      --  of the destination data width (BNDT[2:0] versus
      --  GPDMA_CxTR1.DDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued.
      BNDT           : GPDMA_C7BR1_BNDT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C7BR1_Register use record
      BNDT           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype GPDMA_C7LLR_LA_Field is HAL.UInt14;

   --  GPDMA channel 7 linked-list address register
   type GPDMA_C7LLR_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  pointer (16-bit low-significant address) to the next linked-list data
      --  structure 	If UT1 = UT2 = UB1 = USA = UDA = ULL = 0 and
      --  if LA[15:20] = 0, the current LLI is the last one. The channel
      --  transfer is completed without any update of the linked-list GPDMA
      --  register file. 	Else, this field is the pointer to the memory address
      --  offset from which the next linked-list data structure is
      --  automatically fetched from, once the data transfer is completed, in
      --  order to conditionally update the linked-list GPDMA internal register
      --  file (GPDMA_CxCTR1, GPDMA_CxTR2, GPDMA_CxBR1, GPDMA_CxSAR,
      --  GPDMA_CxDAR and GPDMA_CxLLR). 	Note: The user must program the
      --  pointer to be 32-bit aligned. The two low-significant bits are write
      --  ignored.
      LA             : GPDMA_C7LLR_LA_Field := 16#0#;
      --  Update GPDMA_CxLLR register from memory 	This bit is used to control
      --  the update of GPDMA_CxLLR from the memory during the link transfer.
      ULL            : Boolean := False;
      --  unspecified
      Reserved_17_26 : HAL.UInt10 := 16#0#;
      --  Update GPDMA_CxDAR register from memory 	This bit is used to control
      --  the update of GPDMA_CxDAR from the memory during the link transfer.
      UDA            : Boolean := False;
      --  update GPDMA_CxSAR from memory 	This bit controls the update of
      --  GPDMA_CxSAR from the memory during the link transfer.
      USA            : Boolean := False;
      --  Update GPDMA_CxBR1 from memory 	This bit controls the update of
      --  GPDMA_CxBR1 from the memory during the link transfer. If UB1 = 0
      --  and if GPDMA_CxLLR ≠ 0, the linked-list is not completed.
      --  GPDMA_CxBR1.BNDT[15:0] is then restored to the programmed value after
      --  data transfer is completed and before the link transfer.
      UB1            : Boolean := False;
      --  Update GPDMA_CxTR2 from memory 	This bit controls the update of
      --  GPDMA_CxTR2 from the memory during the link transfer.
      UT2            : Boolean := False;
      --  Update GPDMA_CxTR1 from memory 	This bit controls the update of
      --  GPDMA_CxTR1 from the memory during the link transfer.
      UT1            : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C7LLR_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      LA             at 0 range 2 .. 15;
      ULL            at 0 range 16 .. 16;
      Reserved_17_26 at 0 range 17 .. 26;
      UDA            at 0 range 27 .. 27;
      USA            at 0 range 28 .. 28;
      UB1            at 0 range 29 .. 29;
      UT2            at 0 range 30 .. 30;
      UT1            at 0 range 31 .. 31;
   end record;

   subtype GPDMA_C8LBAR_LBA_Field is HAL.UInt16;

   --  GPDMA channel 8 linked-list base address register
   type GPDMA_C8LBAR_Register is record
      --  unspecified
      Reserved_0_15 : HAL.UInt16 := 16#0#;
      --  linked-list base address of GPDMA channel x
      LBA           : GPDMA_C8LBAR_LBA_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C8LBAR_Register use record
      Reserved_0_15 at 0 range 0 .. 15;
      LBA           at 0 range 16 .. 31;
   end record;

   --  GPDMA channel 8 flag clear register
   type GPDMA_C8FCR_Register is record
      --  unspecified
      Reserved_0_7   : HAL.UInt8 := 16#0#;
      --  Write-only. transfer complete flag clear
      TCF            : Boolean := False;
      --  Write-only. half transfer flag clear
      HTF            : Boolean := False;
      --  Write-only. data transfer error flag clear
      DTEF           : Boolean := False;
      --  Write-only. update link transfer error flag clear
      ULEF           : Boolean := False;
      --  Write-only. user setting error flag clear
      USEF           : Boolean := False;
      --  Write-only. completed suspension flag clear
      SUSPF          : Boolean := False;
      --  Write-only. trigger overrun flag clear
      TOF            : Boolean := False;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C8FCR_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      TCF            at 0 range 8 .. 8;
      HTF            at 0 range 9 .. 9;
      DTEF           at 0 range 10 .. 10;
      ULEF           at 0 range 11 .. 11;
      USEF           at 0 range 12 .. 12;
      SUSPF          at 0 range 13 .. 13;
      TOF            at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype GPDMA_C8SR_FIFOL_Field is HAL.UInt8;

   --  GPDMA channel 8 status register
   type GPDMA_C8SR_Register is record
      --  Read-only. idle flag 	This idle flag is de-asserted by hardware when
      --  the channel is enabled (GPDMA_CxCR.EN = 1) with a valid channel
      --  configuration (no USEF to be immediately reported). 	This idle flag
      --  is asserted after hard reset or by hardware when the channel is back
      --  in idle state (in suspended or disabled state).
      IDLEF          : Boolean;
      --  unspecified
      Reserved_1_7   : HAL.UInt7;
      --  Read-only. transfer complete flag 	A transfer complete event is
      --  either a block transfer complete, a 2D/repeated block transfer
      --  complete, a LLI transfer complete including the upload of the next
      --  LLI if any, or the full linked-list completion, depending on the
      --  transfer complete event mode (GPDMA_CxTR2.TCEM[1:0]).
      TCF            : Boolean;
      --  Read-only. half transfer flag 	An half transfer event is either an
      --  half block transfer or an half 2D/repeated block transfer, depending
      --  on the transfer complete event mode (GPDMA_CxTR2.TCEM[1:0]). 	An half
      --  block transfer occurs when half of the bytes of the source block size
      --  (rounded up integer of GPDMA_CxBR1.BNDT[15:0]/2) has been transferred
      --  to the destination. 	An half 2D/repeated block transfer occurs when
      --  half of the repeated blocks (rounded up integer of
      --  (GPDMA_CxBR1.BRC[10:0]+1)/2)) has been transferred to the
      --  destination.
      HTF            : Boolean;
      --  Read-only. data transfer error flag
      DTEF           : Boolean;
      --  Read-only. update link transfer error flag
      ULEF           : Boolean;
      --  Read-only. user setting error flag
      USEF           : Boolean;
      --  Read-only. completed suspension flag
      SUSPF          : Boolean;
      --  Read-only. trigger overrun flag
      TOF            : Boolean;
      --  unspecified
      Reserved_15_15 : HAL.Bit;
      --  Read-only. monitored FIFO level 	Number of available write beats in
      --  the FIFO, in units of the programmed destination data width (see
      --  GPDMA_CxTR1.DDW_LOG2[1:0], in units of bytes, half-words, or words).
      --  	Note: After having suspended an active transfer, the user may need
      --  to read FIFOL[7:0], additionally to GPDMA_CxBR1.BDNT[15:0] and
      --  GPDMA_CxBR1.BRC[10:0], to know how many data have been transferred to
      --  the destination. Before reading, the user may wait for the transfer
      --  to be suspended (GPDMA_CxSR.SUSPF = 1).
      FIFOL          : GPDMA_C8SR_FIFOL_Field;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C8SR_Register use record
      IDLEF          at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      TCF            at 0 range 8 .. 8;
      HTF            at 0 range 9 .. 9;
      DTEF           at 0 range 10 .. 10;
      ULEF           at 0 range 11 .. 11;
      USEF           at 0 range 12 .. 12;
      SUSPF          at 0 range 13 .. 13;
      TOF            at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      FIFOL          at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype GPDMA_C8CR_PRIO_Field is HAL.UInt2;

   --  GPDMA channel 8 control register
   type GPDMA_C8CR_Register is record
      --  enable 	Writing 1 into the field RESET (bit 1) causes the hardware to
      --  de-assert this bit, whatever is written into this bit 0. Else: 	this
      --  bit is de-asserted by hardware when there is a transfer error (master
      --  bus error or user setting error) or when there is a channel transfer
      --  complete (channel ready to be configured, e.g. if LSM=1 at the end of
      --  a single execution of the LLI). 	Else, this bit can be asserted by
      --  software. 	Writing 0 into this EN bit is ignored.
      EN             : Boolean := False;
      --  Write-only. reset 	This bit is write only. Writing 0 has no impact.
      --  Writing 1 implies the reset of the following: the FIFO, the channel
      --  internal state, SUSP and EN bits (whatever is written receptively in
      --  bit 2 and bit 0). 	The reset is effective when the channel is in
      --  steady state, meaning one of the following: 	- active channel in
      --  suspended state (GPDMA_CxSR.SUSPF = 1 and
      --  GPDMA_CxSR.IDLEF = GPDMA_CxCR.EN = 1) 	- channel in disabled
      --  state (GPDMA_CxSR.IDLEF = 1 and GPDMA_CxCR.EN = 0). 	After
      --  writing a RESET, to continue using this channel, the user must
      --  explicitly reconfigure the channel including the hardware-modified
      --  configuration registers (GPDMA_CxBR1, GPDMA_CxSAR and GPDMA_CxDAR)
      --  before enabling again the channel (see the programming sequence in ).
      RESET          : Boolean := False;
      --  suspend 	Writing 1 into the field RESET (bit 1) causes the hardware
      --  to de-assert this bit, whatever is written into this bit 2. Else:
      --  	Software must write 1 in order to suspend an active channel i.e. a
      --  channel with an on-going GPDMA transfer over its master ports. 	The
      --  software must write 0 in order to resume a suspended channel,
      --  following the programming sequence detailed in .
      SUSP           : Boolean := False;
      --  unspecified
      Reserved_3_7   : HAL.UInt5 := 16#0#;
      --  transfer complete interrupt enable
      TCIE           : Boolean := False;
      --  half transfer complete interrupt enable
      HTIE           : Boolean := False;
      --  data transfer error interrupt enable
      DTEIE          : Boolean := False;
      --  update link transfer error interrupt enable
      ULEIE          : Boolean := False;
      --  user setting error interrupt enable
      USEIE          : Boolean := False;
      --  completed suspension interrupt enable
      SUSPIE         : Boolean := False;
      --  trigger overrun interrupt enable
      TOIE           : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Link step mode 	First the (possible 1D/repeated) block transfer is
      --  executed as defined by the current internal register file until
      --  GPDMA_CxBR1.BNDT[15:0] = 0 and GPDMA_CxBR1.BRC[10:0] = 0 if
      --  present. Secondly the next linked-list data structure is
      --  conditionally uploaded from memory as defined by GPDMA_CxLLR. Then
      --  channel execution is completed. 	Note: This bit must be written when
      --  EN=0. This bit is read-only when EN=1.
      LSM            : Boolean := False;
      --  linked-list allocated port 	This bit is used to allocate the master
      --  port for the update of the GPDMA linked-list registers from the
      --  memory. 	Note: This bit must be written when EN=0. This bit is
      --  read-only when EN=1.
      LAP            : Boolean := False;
      --  unspecified
      Reserved_18_21 : HAL.UInt4 := 16#0#;
      --  priority level of the channel x GPDMA transfer versus others 	Note:
      --  This bit must be written when EN = 0. This bit is read-only when
      --  EN = 1.
      PRIO           : GPDMA_C8CR_PRIO_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C8CR_Register use record
      EN             at 0 range 0 .. 0;
      RESET          at 0 range 1 .. 1;
      SUSP           at 0 range 2 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      TCIE           at 0 range 8 .. 8;
      HTIE           at 0 range 9 .. 9;
      DTEIE          at 0 range 10 .. 10;
      ULEIE          at 0 range 11 .. 11;
      USEIE          at 0 range 12 .. 12;
      SUSPIE         at 0 range 13 .. 13;
      TOIE           at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      LSM            at 0 range 16 .. 16;
      LAP            at 0 range 17 .. 17;
      Reserved_18_21 at 0 range 18 .. 21;
      PRIO           at 0 range 22 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype GPDMA_C8TR1_SDW_LOG2_Field is HAL.UInt2;
   subtype GPDMA_C8TR1_SBL_1_Field is HAL.UInt6;
   subtype GPDMA_C8TR1_PAM_Field is HAL.UInt2;
   subtype GPDMA_C8TR1_DDW_LOG2_Field is HAL.UInt2;
   subtype GPDMA_C8TR1_DBL_1_Field is HAL.UInt6;

   --  GPDMA channel 8 transfer register 1
   type GPDMA_C8TR1_Register is record
      --  binary logarithm of the source data width of a burst in bytes 	Note:
      --  Setting a 8-byte data width causes a user setting error to be
      --  reported and no transfer is issued. 	A source block size must be a
      --  multiple of the source data width (GPDMA_CxBR1.BNDT[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and no
      --  transfer is issued. 	A source single transfer must have an aligned
      --  address with its data width (start address GPDMA_CxSAR[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and none
      --  transfer is issued.
      SDW_LOG2       : GPDMA_C8TR1_SDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  source incrementing burst 	The source address, pointed by
      --  GPDMA_CxSAR, is kept constant after a burst beat/single transfer or
      --  is incremented by the offset value corresponding to a contiguous data
      --  after a burst beat/single transfer.
      SINC           : Boolean := False;
      --  source burst length minus 1, between 0 and 63 	The burst length unit
      --  is one data named beat within a burst. If SBL_1[5:0] =0 , the burst
      --  can be named as single. Each data/beat has a width defined by the
      --  destination data width SDW_LOG2[1:0]. 	Note: If a burst transfer
      --  crossed a 1-Kbyte address boundary on a AHB transfer, the GPDMA
      --  modifies and shortens the programmed burst into singles or bursts of
      --  lower length, to be compliant with the AHB protocol. 	If a burst
      --  transfer is of length greater than the FIFO size of the channel x,
      --  the GPDMA modifies and shortens the programmed burst into singles or
      --  bursts of lower length, to be compliant with the FIFO size. Transfer
      --  performance is lower, with GPDMA re-arbitration between effective and
      --  lower bursts/singles, but the data integrity is guaranteed.
      SBL_1          : GPDMA_C8TR1_SBL_1_Field := 16#0#;
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  padding/alignment mode 	If DDW_LOG2[1:0] = SDW_LOG2[1:0]: if the data
      --  width of a burst destination transfer is equal to the data width of a
      --  burst source transfer, these bits are ignored. 	Else: 	- Case 1: If
      --  destination data width > source data width 	1x: successive source
      --  data are FIFO queued and packed at the destination data width, in a
      --  left (LSB) to right (MSB) order (named little endian), before a
      --  destination transfer 	- Case 2: If destination data width < source
      --  data width 	1x: source data is FIFO queued and unpacked at the
      --  destination data width, to be transferred in a left (LSB) to right
      --  (MSB) order (named little endian) to the destination 	Note:
      PAM            : GPDMA_C8TR1_PAM_Field := 16#0#;
      --  source byte exchange within the unaligned half-word of each source
      --  word 	If the source data width is shorter than a word, this bit is
      --  ignored. 	If the source data width is a word:
      SBX            : Boolean := False;
      --  source allocated port 	This bit is used to allocate the master port
      --  for the source transfer 	Note: This bit must be written when
      --  EN = 0. This bit is read-only when EN = 1.
      SAP            : Boolean := False;
      --  security attribute of the GPDMA transfer from the source 	If
      --  GPDMA_SECCFGR.SECx = 1 and the access is secure: 	This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  GPDMA_SECCFGR.SECx =1 . A secure write is ignored when
      --  GPDMA_SECCFGR.SECx = 0. 	When GPDMA_SECCFGR.SECx is de-asserted,
      --  this SSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the GPDMA transfer
      --  from the source is non-secure.
      SSEC           : Boolean := False;
      --  binary logarithm of the destination data width of a burst, in bytes
      --  	Note: Setting a 8-byte data width causes a user setting error to be
      --  reported and none transfer is issued. 	A destination burst transfer
      --  must have an aligned address with its data width (start address
      --  GPDMA_CxDAR[2:0] and address offset GPDMA_CxTR3.DAO[2:0], versus
      --  DDW_LOG2[1:0]). Otherwise a user setting error is reported and no
      --  transfer is issued.
      DDW_LOG2       : GPDMA_C8TR1_DDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_18_18 : HAL.Bit := 16#0#;
      --  destination incrementing burst 	The destination address, pointed by
      --  GPDMA_CxDAR, is kept constant after a burst beat/single transfer, or
      --  is incremented by the offset value corresponding to a contiguous data
      --  after a burst beat/single transfer.
      DINC           : Boolean := False;
      --  destination burst length minus 1, between 0 and 63 	The burst length
      --  unit is one data named beat within a burst. If DBL_1[5:0] =0 , the
      --  burst can be named as single. Each data/beat has a width defined by
      --  the destination data width DDW_LOG2[1:0]. 	Note: If a burst transfer
      --  crossed a 1-Kbyte address boundary on a AHB transfer, the GPDMA
      --  modifies and shortens the programmed burst into singles or bursts of
      --  lower length, to be compliant with the AHB protocol. 	If a burst
      --  transfer is of length greater than the FIFO size of the channel x,
      --  the GPDMA modifies and shortens the programmed burst into singles or
      --  bursts of lower length, to be compliant with the FIFO size. Transfer
      --  performance is lower, with GPDMA re-arbitration between effective and
      --  lower bursts/singles, but the data integrity is guaranteed.
      DBL_1          : GPDMA_C8TR1_DBL_1_Field := 16#0#;
      --  destination byte exchange 	If the destination data size is a byte,
      --  this bit is ignored. 	If the destination data size is not a byte:
      DBX            : Boolean := False;
      --  destination half-word exchange 	If the destination data size is
      --  shorter than a word, this bit is ignored. 	If the destination data
      --  size is a word:
      DHX            : Boolean := False;
      --  unspecified
      Reserved_28_29 : HAL.UInt2 := 16#0#;
      --  destination allocated port 	This bit is used to allocate the master
      --  port for the destination transfer 	Note: This bit must be written
      --  when EN = 0. This bit is read-only when EN = 1.
      DAP            : Boolean := False;
      --  security attribute of the GPDMA transfer to the destination 	If
      --  GPDMA_SECCFGR.SECx = 1 and the access is secure: 	This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  GPDMA_SECCFGR.SECx = 1. A secure write is ignored when
      --  GPDMA_SECCFGR.SECx = 0. 	When GPDMA_SECCFGR.SECx is de-asserted,
      --  this DSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the GPDMA transfer
      --  to the destination is non-secure.
      DSEC           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C8TR1_Register use record
      SDW_LOG2       at 0 range 0 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      SINC           at 0 range 3 .. 3;
      SBL_1          at 0 range 4 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      PAM            at 0 range 11 .. 12;
      SBX            at 0 range 13 .. 13;
      SAP            at 0 range 14 .. 14;
      SSEC           at 0 range 15 .. 15;
      DDW_LOG2       at 0 range 16 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      DINC           at 0 range 19 .. 19;
      DBL_1          at 0 range 20 .. 25;
      DBX            at 0 range 26 .. 26;
      DHX            at 0 range 27 .. 27;
      Reserved_28_29 at 0 range 28 .. 29;
      DAP            at 0 range 30 .. 30;
      DSEC           at 0 range 31 .. 31;
   end record;

   subtype GPDMA_C8TR2_REQSEL_Field is HAL.UInt7;
   subtype GPDMA_C8TR2_TRIGM_Field is HAL.UInt2;
   subtype GPDMA_C8TR2_TRIGSEL_Field is HAL.UInt6;
   subtype GPDMA_C8TR2_TRIGPOL_Field is HAL.UInt2;
   subtype GPDMA_C8TR2_TCEM_Field is HAL.UInt2;

   --  GPDMA channel 8 transfer register 2
   type GPDMA_C8TR2_Register is record
      --  GPDMA hardware request selection 	These bits are ignored if channel x
      --  is activated (GPDMA_CxCR.EN asserted) with SWREQ = 1 (software
      --  request for a memory-to-memory transfer). Else, the selected hardware
      --  request is internally taken into account as per . 	The user must not
      --  assign a same input hardware request (same REQSEL[6:0] value) to
      --  different active GPDMA channels (GPDMA_CxCR.EN = 1 and
      --  GPDMA_CxTR2.SWREQ = 0 for these channels). GPDMA is not intended to
      --  hardware support the case of simultaneous enabled channels
      --  incorrectly configured with a same hardware peripheral request
      --  signal, and there is no user setting error reporting.
      REQSEL         : GPDMA_C8TR2_REQSEL_Field := 16#0#;
      --  unspecified
      Reserved_7_8   : HAL.UInt2 := 16#0#;
      --  software request 	This bit is internally taken into account when
      --  GPDMA_CxCR.EN is asserted.
      SWREQ          : Boolean := False;
      --  destination hardware request 	This bit is ignored if channel x is
      --  activated (GPDMA_CxCR.EN asserted) with SWREQ = 1 (software request
      --  for a memory-to-memory transfer). Else: 	Note:
      DREQ           : Boolean := False;
      --  Block hardware request 	If the channel x is activated (GPDMA_CxCR.EN
      --  asserted) with SWREQ = 1 (software request for a memory-to-memory
      --  transfer), this bit is ignored. Else:
      BREQ           : Boolean := False;
      --  unspecified
      Reserved_12_13 : HAL.UInt2 := 16#0#;
      --  trigger mode 	These bits define the transfer granularity for its
      --  conditioning by the trigger. 	If the channel x is enabled
      --  (GPDMA_CxCR.EN asserted) with TRIGPOL[1:0] = 00 or 11, these
      --  TRIGM[1:0] bits are ignored. 	Else, a GPDMA transfer is conditioned
      --  by at least one trigger hit: 	first burst read of a 2D/repeated block
      --  transfer is conditioned by one hit trigger. 	– If the peripheral is
      --  programmed as a source (DREQ = 0) of the LLI data transfer, each
      --  programmed burst read is conditioned. 	– If the peripheral is
      --  programmed as a destination (DREQ = 1) of the LLI data transfer,
      --  each programmed burst write is conditioned. The first memory burst
      --  read of a (possibly 2D/repeated) block, also named as the first ready
      --  FIFO-based source burst, is gated by the occurrence of both the
      --  hardware request and the first trigger hit. 	The GPDMA monitoring of
      --  a trigger for channel x is started when the channel is enabled/loaded
      --  with a new active trigger configuration: rising or falling edge on a
      --  selected trigger (TRIGPOL[1:0] = 01 or respectively
      --  TRIGPOL[1:0] = 10). 	The monitoring of this trigger is kept active
      --  during the triggered and uncompleted (data or link) transfer; and if
      --  a new trigger is detected then, this hit is internally memorized to
      --  grant the next transfer, as long as the defined rising or falling
      --  edge is not modified, and the TRIGSEL[5:0] is not modified, and the
      --  channel is enabled. 	Transferring a next LLIn+1 that updates the
      --  GPDMA_CxTR2 with a new value for any of TRIGSEL[5:0] or TRIGPOL[1:0],
      --  resets the monitoring, trashing the memorized hit of the formerly
      --  defined LLIn trigger. 	After a first new trigger hitn+1 is memorized,
      --  if another second trigger hitn+2 is detected and if the hitn
      --  triggered transfer is still not completed, hitn+2 is lost and not
      --  memorized.memorized. A trigger overrun flag is reported
      --  (GPDMA_CxSR.TOF =1 ), and an interrupt is generated if enabled
      --  (GPDMA_CxCR.TOIE = 1). The channel is not automatically disabled by
      --  hardware due to a trigger overrun. 	Note: When the source block size
      --  is not a multiple of the source burst size and is a multiple of the
      --  source data width, then the last programmed source burst is not
      --  completed and is internally shorten to match the block size. In this
      --  case, if TRIGM[1:0] = 11 and (SWREQ =1  or (SWREQ = 0 and
      --  DREQ =0 )), the shortened burst transfer (by singles or/and by
      --  bursts of lower length) is conditioned once by the trigger. 	When the
      --  programmed destination burst is internally shortened by singles
      --  or/and by bursts of lower length (versus FIFO size, versus block
      --  size, 1-Kbyte boundary address crossing): if the trigger is
      --  conditioning the programmed destination burst (if TRIGM[1:0] = 11
      --  and SWREQ = 0 and DREQ = 1), this shortened destination burst
      --  transfer is conditioned once by the trigger.
      TRIGM          : GPDMA_C8TR2_TRIGM_Field := 16#0#;
      --  trigger event input selection 	These bits select the trigger event
      --  input of the GPDMA transfer (as per ), with an active trigger event
      --  if TRIGPOL[1:0] ≠ 00.
      TRIGSEL        : GPDMA_C8TR2_TRIGSEL_Field := 16#0#;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  trigger event polarity 	These bits define the polarity of the
      --  selected trigger event input defined by TRIGSEL[5:0].
      TRIGPOL        : GPDMA_C8TR2_TRIGPOL_Field := 16#0#;
      --  unspecified
      Reserved_26_29 : HAL.UInt4 := 16#0#;
      --  transfer complete event mode 	These bits define the transfer
      --  granularity for the transfer complete and half transfer complete
      --  events generation. 	Note: If the initial LLI0 data transfer is
      --  null/void (directly programmed by the internal register file with
      --  GPDMA_CxBR1.BNDT[15:0] = 0), then neither the complete transfer
      --  event nor the half transfer event is generated. 	Note: If the initial
      --  LLI0 data transfer is null/void (directly programmed by the internal
      --  register file with GPDMA_CxBR1.BNDT[15:0] = 0), then neither the
      --  complete transfer event nor the half transfer event is generated.
      --  	Note: If the initial LLI0 data transfer is null/void (i.e. directly
      --  programmed by the internal register file with
      --  GPDMA_CxBR1.BNDT[15:0] =0 ), then the half transfer event is not
      --  generated, and the transfer complete event is generated when is
      --  completed the loading of the LLI1.
      TCEM           : GPDMA_C8TR2_TCEM_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C8TR2_Register use record
      REQSEL         at 0 range 0 .. 6;
      Reserved_7_8   at 0 range 7 .. 8;
      SWREQ          at 0 range 9 .. 9;
      DREQ           at 0 range 10 .. 10;
      BREQ           at 0 range 11 .. 11;
      Reserved_12_13 at 0 range 12 .. 13;
      TRIGM          at 0 range 14 .. 15;
      TRIGSEL        at 0 range 16 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      TRIGPOL        at 0 range 24 .. 25;
      Reserved_26_29 at 0 range 26 .. 29;
      TCEM           at 0 range 30 .. 31;
   end record;

   subtype GPDMA_C8BR1_BNDT_Field is HAL.UInt16;

   --  GPDMA channel 8 block register 1
   type GPDMA_C8BR1_Register is record
      --  block number of data bytes to transfer from the source 	Block size
      --  transferred from the source. When the channel is enabled, this field
      --  becomes read-only and is decremented, indicating the remaining number
      --  of data items in the current source block to be transferred.
      --  BNDT[15:0] is programmed in number of bytes, maximum source block
      --  size is 64 Kbytes -1. 	Once the last data transfer is completed
      --  (BNDT[15:0] = 0): 	- if GPDMA_CxLLR.UB1 = 1, this field is
      --  updated by the LLI in the memory. 	- if GPDMA_CxLLR.UB1 = 0 and if
      --  there is at least one non null Uxx update bit, this field is
      --  internally restored to the programmed value. 	- if all
      --  GPDMA_CxLLR.Uxx = 0 and if GPDMA_CxLLR.LA[15:0] = 0, this field
      --  is internally restored to the programmed value (infinite/continuous
      --  last LLI). 	- if GPDMA_CxLLR = 0, this field is kept as zero
      --  following the last LLI data transfer. 	Note: A non-null source block
      --  size must be a multiple of the source data width (BNDT[2:0] versus
      --  GPDMA_CxTR1.SDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued. 	When configured in packing mode
      --  (GPDMA_CxTR1.PAM[1] = 1 and destination data width different from
      --  source data width), a non-null source block size must be a multiple
      --  of the destination data width (BNDT[2:0] versus
      --  GPDMA_CxTR1.DDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued.
      BNDT           : GPDMA_C8BR1_BNDT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C8BR1_Register use record
      BNDT           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype GPDMA_C8LLR_LA_Field is HAL.UInt14;

   --  GPDMA channel 8 linked-list address register
   type GPDMA_C8LLR_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  pointer (16-bit low-significant address) to the next linked-list data
      --  structure 	If UT1 = UT2 = UB1 = USA = UDA = ULL = 0 and
      --  if LA[15:20] = 0, the current LLI is the last one. The channel
      --  transfer is completed without any update of the linked-list GPDMA
      --  register file. 	Else, this field is the pointer to the memory address
      --  offset from which the next linked-list data structure is
      --  automatically fetched from, once the data transfer is completed, in
      --  order to conditionally update the linked-list GPDMA internal register
      --  file (GPDMA_CxCTR1, GPDMA_CxTR2, GPDMA_CxBR1, GPDMA_CxSAR,
      --  GPDMA_CxDAR and GPDMA_CxLLR). 	Note: The user must program the
      --  pointer to be 32-bit aligned. The two low-significant bits are write
      --  ignored.
      LA             : GPDMA_C8LLR_LA_Field := 16#0#;
      --  Update GPDMA_CxLLR register from memory 	This bit is used to control
      --  the update of GPDMA_CxLLR from the memory during the link transfer.
      ULL            : Boolean := False;
      --  unspecified
      Reserved_17_26 : HAL.UInt10 := 16#0#;
      --  Update GPDMA_CxDAR register from memory 	This bit is used to control
      --  the update of GPDMA_CxDAR from the memory during the link transfer.
      UDA            : Boolean := False;
      --  update GPDMA_CxSAR from memory 	This bit controls the update of
      --  GPDMA_CxSAR from the memory during the link transfer.
      USA            : Boolean := False;
      --  Update GPDMA_CxBR1 from memory 	This bit controls the update of
      --  GPDMA_CxBR1 from the memory during the link transfer. If UB1 = 0
      --  and if GPDMA_CxLLR ≠ 0, the linked-list is not completed.
      --  GPDMA_CxBR1.BNDT[15:0] is then restored to the programmed value after
      --  data transfer is completed and before the link transfer.
      UB1            : Boolean := False;
      --  Update GPDMA_CxTR2 from memory 	This bit controls the update of
      --  GPDMA_CxTR2 from the memory during the link transfer.
      UT2            : Boolean := False;
      --  Update GPDMA_CxTR1 from memory 	This bit controls the update of
      --  GPDMA_CxTR1 from the memory during the link transfer.
      UT1            : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C8LLR_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      LA             at 0 range 2 .. 15;
      ULL            at 0 range 16 .. 16;
      Reserved_17_26 at 0 range 17 .. 26;
      UDA            at 0 range 27 .. 27;
      USA            at 0 range 28 .. 28;
      UB1            at 0 range 29 .. 29;
      UT2            at 0 range 30 .. 30;
      UT1            at 0 range 31 .. 31;
   end record;

   subtype GPDMA_C9LBAR_LBA_Field is HAL.UInt16;

   --  GPDMA channel 9 linked-list base address register
   type GPDMA_C9LBAR_Register is record
      --  unspecified
      Reserved_0_15 : HAL.UInt16 := 16#0#;
      --  linked-list base address of GPDMA channel x
      LBA           : GPDMA_C9LBAR_LBA_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C9LBAR_Register use record
      Reserved_0_15 at 0 range 0 .. 15;
      LBA           at 0 range 16 .. 31;
   end record;

   --  GPDMA channel 9 flag clear register
   type GPDMA_C9FCR_Register is record
      --  unspecified
      Reserved_0_7   : HAL.UInt8 := 16#0#;
      --  Write-only. transfer complete flag clear
      TCF            : Boolean := False;
      --  Write-only. half transfer flag clear
      HTF            : Boolean := False;
      --  Write-only. data transfer error flag clear
      DTEF           : Boolean := False;
      --  Write-only. update link transfer error flag clear
      ULEF           : Boolean := False;
      --  Write-only. user setting error flag clear
      USEF           : Boolean := False;
      --  Write-only. completed suspension flag clear
      SUSPF          : Boolean := False;
      --  Write-only. trigger overrun flag clear
      TOF            : Boolean := False;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C9FCR_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      TCF            at 0 range 8 .. 8;
      HTF            at 0 range 9 .. 9;
      DTEF           at 0 range 10 .. 10;
      ULEF           at 0 range 11 .. 11;
      USEF           at 0 range 12 .. 12;
      SUSPF          at 0 range 13 .. 13;
      TOF            at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype GPDMA_C9SR_FIFOL_Field is HAL.UInt8;

   --  GPDMA channel 9 status register
   type GPDMA_C9SR_Register is record
      --  Read-only. idle flag 	This idle flag is de-asserted by hardware when
      --  the channel is enabled (GPDMA_CxCR.EN = 1) with a valid channel
      --  configuration (no USEF to be immediately reported). 	This idle flag
      --  is asserted after hard reset or by hardware when the channel is back
      --  in idle state (in suspended or disabled state).
      IDLEF          : Boolean;
      --  unspecified
      Reserved_1_7   : HAL.UInt7;
      --  Read-only. transfer complete flag 	A transfer complete event is
      --  either a block transfer complete, a 2D/repeated block transfer
      --  complete, a LLI transfer complete including the upload of the next
      --  LLI if any, or the full linked-list completion, depending on the
      --  transfer complete event mode (GPDMA_CxTR2.TCEM[1:0]).
      TCF            : Boolean;
      --  Read-only. half transfer flag 	An half transfer event is either an
      --  half block transfer or an half 2D/repeated block transfer, depending
      --  on the transfer complete event mode (GPDMA_CxTR2.TCEM[1:0]). 	An half
      --  block transfer occurs when half of the bytes of the source block size
      --  (rounded up integer of GPDMA_CxBR1.BNDT[15:0]/2) has been transferred
      --  to the destination. 	An half 2D/repeated block transfer occurs when
      --  half of the repeated blocks (rounded up integer of
      --  (GPDMA_CxBR1.BRC[10:0]+1)/2)) has been transferred to the
      --  destination.
      HTF            : Boolean;
      --  Read-only. data transfer error flag
      DTEF           : Boolean;
      --  Read-only. update link transfer error flag
      ULEF           : Boolean;
      --  Read-only. user setting error flag
      USEF           : Boolean;
      --  Read-only. completed suspension flag
      SUSPF          : Boolean;
      --  Read-only. trigger overrun flag
      TOF            : Boolean;
      --  unspecified
      Reserved_15_15 : HAL.Bit;
      --  Read-only. monitored FIFO level 	Number of available write beats in
      --  the FIFO, in units of the programmed destination data width (see
      --  GPDMA_CxTR1.DDW_LOG2[1:0], in units of bytes, half-words, or words).
      --  	Note: After having suspended an active transfer, the user may need
      --  to read FIFOL[7:0], additionally to GPDMA_CxBR1.BDNT[15:0] and
      --  GPDMA_CxBR1.BRC[10:0], to know how many data have been transferred to
      --  the destination. Before reading, the user may wait for the transfer
      --  to be suspended (GPDMA_CxSR.SUSPF = 1).
      FIFOL          : GPDMA_C9SR_FIFOL_Field;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C9SR_Register use record
      IDLEF          at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      TCF            at 0 range 8 .. 8;
      HTF            at 0 range 9 .. 9;
      DTEF           at 0 range 10 .. 10;
      ULEF           at 0 range 11 .. 11;
      USEF           at 0 range 12 .. 12;
      SUSPF          at 0 range 13 .. 13;
      TOF            at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      FIFOL          at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype GPDMA_C9CR_PRIO_Field is HAL.UInt2;

   --  GPDMA channel 9 control register
   type GPDMA_C9CR_Register is record
      --  enable 	Writing 1 into the field RESET (bit 1) causes the hardware to
      --  de-assert this bit, whatever is written into this bit 0. Else: 	this
      --  bit is de-asserted by hardware when there is a transfer error (master
      --  bus error or user setting error) or when there is a channel transfer
      --  complete (channel ready to be configured, e.g. if LSM=1 at the end of
      --  a single execution of the LLI). 	Else, this bit can be asserted by
      --  software. 	Writing 0 into this EN bit is ignored.
      EN             : Boolean := False;
      --  Write-only. reset 	This bit is write only. Writing 0 has no impact.
      --  Writing 1 implies the reset of the following: the FIFO, the channel
      --  internal state, SUSP and EN bits (whatever is written receptively in
      --  bit 2 and bit 0). 	The reset is effective when the channel is in
      --  steady state, meaning one of the following: 	- active channel in
      --  suspended state (GPDMA_CxSR.SUSPF = 1 and
      --  GPDMA_CxSR.IDLEF = GPDMA_CxCR.EN = 1) 	- channel in disabled
      --  state (GPDMA_CxSR.IDLEF = 1 and GPDMA_CxCR.EN = 0). 	After
      --  writing a RESET, to continue using this channel, the user must
      --  explicitly reconfigure the channel including the hardware-modified
      --  configuration registers (GPDMA_CxBR1, GPDMA_CxSAR and GPDMA_CxDAR)
      --  before enabling again the channel (see the programming sequence in ).
      RESET          : Boolean := False;
      --  suspend 	Writing 1 into the field RESET (bit 1) causes the hardware
      --  to de-assert this bit, whatever is written into this bit 2. Else:
      --  	Software must write 1 in order to suspend an active channel i.e. a
      --  channel with an on-going GPDMA transfer over its master ports. 	The
      --  software must write 0 in order to resume a suspended channel,
      --  following the programming sequence detailed in .
      SUSP           : Boolean := False;
      --  unspecified
      Reserved_3_7   : HAL.UInt5 := 16#0#;
      --  transfer complete interrupt enable
      TCIE           : Boolean := False;
      --  half transfer complete interrupt enable
      HTIE           : Boolean := False;
      --  data transfer error interrupt enable
      DTEIE          : Boolean := False;
      --  update link transfer error interrupt enable
      ULEIE          : Boolean := False;
      --  user setting error interrupt enable
      USEIE          : Boolean := False;
      --  completed suspension interrupt enable
      SUSPIE         : Boolean := False;
      --  trigger overrun interrupt enable
      TOIE           : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Link step mode 	First the (possible 1D/repeated) block transfer is
      --  executed as defined by the current internal register file until
      --  GPDMA_CxBR1.BNDT[15:0] = 0 and GPDMA_CxBR1.BRC[10:0] = 0 if
      --  present. Secondly the next linked-list data structure is
      --  conditionally uploaded from memory as defined by GPDMA_CxLLR. Then
      --  channel execution is completed. 	Note: This bit must be written when
      --  EN=0. This bit is read-only when EN=1.
      LSM            : Boolean := False;
      --  linked-list allocated port 	This bit is used to allocate the master
      --  port for the update of the GPDMA linked-list registers from the
      --  memory. 	Note: This bit must be written when EN=0. This bit is
      --  read-only when EN=1.
      LAP            : Boolean := False;
      --  unspecified
      Reserved_18_21 : HAL.UInt4 := 16#0#;
      --  priority level of the channel x GPDMA transfer versus others 	Note:
      --  This bit must be written when EN = 0. This bit is read-only when
      --  EN = 1.
      PRIO           : GPDMA_C9CR_PRIO_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C9CR_Register use record
      EN             at 0 range 0 .. 0;
      RESET          at 0 range 1 .. 1;
      SUSP           at 0 range 2 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      TCIE           at 0 range 8 .. 8;
      HTIE           at 0 range 9 .. 9;
      DTEIE          at 0 range 10 .. 10;
      ULEIE          at 0 range 11 .. 11;
      USEIE          at 0 range 12 .. 12;
      SUSPIE         at 0 range 13 .. 13;
      TOIE           at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      LSM            at 0 range 16 .. 16;
      LAP            at 0 range 17 .. 17;
      Reserved_18_21 at 0 range 18 .. 21;
      PRIO           at 0 range 22 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype GPDMA_C9TR1_SDW_LOG2_Field is HAL.UInt2;
   subtype GPDMA_C9TR1_SBL_1_Field is HAL.UInt6;
   subtype GPDMA_C9TR1_PAM_Field is HAL.UInt2;
   subtype GPDMA_C9TR1_DDW_LOG2_Field is HAL.UInt2;
   subtype GPDMA_C9TR1_DBL_1_Field is HAL.UInt6;

   --  GPDMA channel 9 transfer register 1
   type GPDMA_C9TR1_Register is record
      --  binary logarithm of the source data width of a burst in bytes 	Note:
      --  Setting a 8-byte data width causes a user setting error to be
      --  reported and no transfer is issued. 	A source block size must be a
      --  multiple of the source data width (GPDMA_CxBR1.BNDT[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and no
      --  transfer is issued. 	A source single transfer must have an aligned
      --  address with its data width (start address GPDMA_CxSAR[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and none
      --  transfer is issued.
      SDW_LOG2       : GPDMA_C9TR1_SDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  source incrementing burst 	The source address, pointed by
      --  GPDMA_CxSAR, is kept constant after a burst beat/single transfer or
      --  is incremented by the offset value corresponding to a contiguous data
      --  after a burst beat/single transfer.
      SINC           : Boolean := False;
      --  source burst length minus 1, between 0 and 63 	The burst length unit
      --  is one data named beat within a burst. If SBL_1[5:0] =0 , the burst
      --  can be named as single. Each data/beat has a width defined by the
      --  destination data width SDW_LOG2[1:0]. 	Note: If a burst transfer
      --  crossed a 1-Kbyte address boundary on a AHB transfer, the GPDMA
      --  modifies and shortens the programmed burst into singles or bursts of
      --  lower length, to be compliant with the AHB protocol. 	If a burst
      --  transfer is of length greater than the FIFO size of the channel x,
      --  the GPDMA modifies and shortens the programmed burst into singles or
      --  bursts of lower length, to be compliant with the FIFO size. Transfer
      --  performance is lower, with GPDMA re-arbitration between effective and
      --  lower bursts/singles, but the data integrity is guaranteed.
      SBL_1          : GPDMA_C9TR1_SBL_1_Field := 16#0#;
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  padding/alignment mode 	If DDW_LOG2[1:0] = SDW_LOG2[1:0]: if the data
      --  width of a burst destination transfer is equal to the data width of a
      --  burst source transfer, these bits are ignored. 	Else: 	- Case 1: If
      --  destination data width > source data width 	1x: successive source
      --  data are FIFO queued and packed at the destination data width, in a
      --  left (LSB) to right (MSB) order (named little endian), before a
      --  destination transfer 	- Case 2: If destination data width < source
      --  data width 	1x: source data is FIFO queued and unpacked at the
      --  destination data width, to be transferred in a left (LSB) to right
      --  (MSB) order (named little endian) to the destination 	Note:
      PAM            : GPDMA_C9TR1_PAM_Field := 16#0#;
      --  source byte exchange within the unaligned half-word of each source
      --  word 	If the source data width is shorter than a word, this bit is
      --  ignored. 	If the source data width is a word:
      SBX            : Boolean := False;
      --  source allocated port 	This bit is used to allocate the master port
      --  for the source transfer 	Note: This bit must be written when
      --  EN = 0. This bit is read-only when EN = 1.
      SAP            : Boolean := False;
      --  security attribute of the GPDMA transfer from the source 	If
      --  GPDMA_SECCFGR.SECx = 1 and the access is secure: 	This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  GPDMA_SECCFGR.SECx =1 . A secure write is ignored when
      --  GPDMA_SECCFGR.SECx = 0. 	When GPDMA_SECCFGR.SECx is de-asserted,
      --  this SSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the GPDMA transfer
      --  from the source is non-secure.
      SSEC           : Boolean := False;
      --  binary logarithm of the destination data width of a burst, in bytes
      --  	Note: Setting a 8-byte data width causes a user setting error to be
      --  reported and none transfer is issued. 	A destination burst transfer
      --  must have an aligned address with its data width (start address
      --  GPDMA_CxDAR[2:0] and address offset GPDMA_CxTR3.DAO[2:0], versus
      --  DDW_LOG2[1:0]). Otherwise a user setting error is reported and no
      --  transfer is issued.
      DDW_LOG2       : GPDMA_C9TR1_DDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_18_18 : HAL.Bit := 16#0#;
      --  destination incrementing burst 	The destination address, pointed by
      --  GPDMA_CxDAR, is kept constant after a burst beat/single transfer, or
      --  is incremented by the offset value corresponding to a contiguous data
      --  after a burst beat/single transfer.
      DINC           : Boolean := False;
      --  destination burst length minus 1, between 0 and 63 	The burst length
      --  unit is one data named beat within a burst. If DBL_1[5:0] =0 , the
      --  burst can be named as single. Each data/beat has a width defined by
      --  the destination data width DDW_LOG2[1:0]. 	Note: If a burst transfer
      --  crossed a 1-Kbyte address boundary on a AHB transfer, the GPDMA
      --  modifies and shortens the programmed burst into singles or bursts of
      --  lower length, to be compliant with the AHB protocol. 	If a burst
      --  transfer is of length greater than the FIFO size of the channel x,
      --  the GPDMA modifies and shortens the programmed burst into singles or
      --  bursts of lower length, to be compliant with the FIFO size. Transfer
      --  performance is lower, with GPDMA re-arbitration between effective and
      --  lower bursts/singles, but the data integrity is guaranteed.
      DBL_1          : GPDMA_C9TR1_DBL_1_Field := 16#0#;
      --  destination byte exchange 	If the destination data size is a byte,
      --  this bit is ignored. 	If the destination data size is not a byte:
      DBX            : Boolean := False;
      --  destination half-word exchange 	If the destination data size is
      --  shorter than a word, this bit is ignored. 	If the destination data
      --  size is a word:
      DHX            : Boolean := False;
      --  unspecified
      Reserved_28_29 : HAL.UInt2 := 16#0#;
      --  destination allocated port 	This bit is used to allocate the master
      --  port for the destination transfer 	Note: This bit must be written
      --  when EN = 0. This bit is read-only when EN = 1.
      DAP            : Boolean := False;
      --  security attribute of the GPDMA transfer to the destination 	If
      --  GPDMA_SECCFGR.SECx = 1 and the access is secure: 	This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  GPDMA_SECCFGR.SECx = 1. A secure write is ignored when
      --  GPDMA_SECCFGR.SECx = 0. 	When GPDMA_SECCFGR.SECx is de-asserted,
      --  this DSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the GPDMA transfer
      --  to the destination is non-secure.
      DSEC           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C9TR1_Register use record
      SDW_LOG2       at 0 range 0 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      SINC           at 0 range 3 .. 3;
      SBL_1          at 0 range 4 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      PAM            at 0 range 11 .. 12;
      SBX            at 0 range 13 .. 13;
      SAP            at 0 range 14 .. 14;
      SSEC           at 0 range 15 .. 15;
      DDW_LOG2       at 0 range 16 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      DINC           at 0 range 19 .. 19;
      DBL_1          at 0 range 20 .. 25;
      DBX            at 0 range 26 .. 26;
      DHX            at 0 range 27 .. 27;
      Reserved_28_29 at 0 range 28 .. 29;
      DAP            at 0 range 30 .. 30;
      DSEC           at 0 range 31 .. 31;
   end record;

   subtype GPDMA_C9TR2_REQSEL_Field is HAL.UInt7;
   subtype GPDMA_C9TR2_TRIGM_Field is HAL.UInt2;
   subtype GPDMA_C9TR2_TRIGSEL_Field is HAL.UInt6;
   subtype GPDMA_C9TR2_TRIGPOL_Field is HAL.UInt2;
   subtype GPDMA_C9TR2_TCEM_Field is HAL.UInt2;

   --  GPDMA channel 9 transfer register 2
   type GPDMA_C9TR2_Register is record
      --  GPDMA hardware request selection 	These bits are ignored if channel x
      --  is activated (GPDMA_CxCR.EN asserted) with SWREQ = 1 (software
      --  request for a memory-to-memory transfer). Else, the selected hardware
      --  request is internally taken into account as per . 	The user must not
      --  assign a same input hardware request (same REQSEL[6:0] value) to
      --  different active GPDMA channels (GPDMA_CxCR.EN = 1 and
      --  GPDMA_CxTR2.SWREQ = 0 for these channels). GPDMA is not intended to
      --  hardware support the case of simultaneous enabled channels
      --  incorrectly configured with a same hardware peripheral request
      --  signal, and there is no user setting error reporting.
      REQSEL         : GPDMA_C9TR2_REQSEL_Field := 16#0#;
      --  unspecified
      Reserved_7_8   : HAL.UInt2 := 16#0#;
      --  software request 	This bit is internally taken into account when
      --  GPDMA_CxCR.EN is asserted.
      SWREQ          : Boolean := False;
      --  destination hardware request 	This bit is ignored if channel x is
      --  activated (GPDMA_CxCR.EN asserted) with SWREQ = 1 (software request
      --  for a memory-to-memory transfer). Else: 	Note:
      DREQ           : Boolean := False;
      --  Block hardware request 	If the channel x is activated (GPDMA_CxCR.EN
      --  asserted) with SWREQ = 1 (software request for a memory-to-memory
      --  transfer), this bit is ignored. Else:
      BREQ           : Boolean := False;
      --  unspecified
      Reserved_12_13 : HAL.UInt2 := 16#0#;
      --  trigger mode 	These bits define the transfer granularity for its
      --  conditioning by the trigger. 	If the channel x is enabled
      --  (GPDMA_CxCR.EN asserted) with TRIGPOL[1:0] = 00 or 11, these
      --  TRIGM[1:0] bits are ignored. 	Else, a GPDMA transfer is conditioned
      --  by at least one trigger hit: 	first burst read of a 2D/repeated block
      --  transfer is conditioned by one hit trigger. 	– If the peripheral is
      --  programmed as a source (DREQ = 0) of the LLI data transfer, each
      --  programmed burst read is conditioned. 	– If the peripheral is
      --  programmed as a destination (DREQ = 1) of the LLI data transfer,
      --  each programmed burst write is conditioned. The first memory burst
      --  read of a (possibly 2D/repeated) block, also named as the first ready
      --  FIFO-based source burst, is gated by the occurrence of both the
      --  hardware request and the first trigger hit. 	The GPDMA monitoring of
      --  a trigger for channel x is started when the channel is enabled/loaded
      --  with a new active trigger configuration: rising or falling edge on a
      --  selected trigger (TRIGPOL[1:0] = 01 or respectively
      --  TRIGPOL[1:0] = 10). 	The monitoring of this trigger is kept active
      --  during the triggered and uncompleted (data or link) transfer; and if
      --  a new trigger is detected then, this hit is internally memorized to
      --  grant the next transfer, as long as the defined rising or falling
      --  edge is not modified, and the TRIGSEL[5:0] is not modified, and the
      --  channel is enabled. 	Transferring a next LLIn+1 that updates the
      --  GPDMA_CxTR2 with a new value for any of TRIGSEL[5:0] or TRIGPOL[1:0],
      --  resets the monitoring, trashing the memorized hit of the formerly
      --  defined LLIn trigger. 	After a first new trigger hitn+1 is memorized,
      --  if another second trigger hitn+2 is detected and if the hitn
      --  triggered transfer is still not completed, hitn+2 is lost and not
      --  memorized.memorized. A trigger overrun flag is reported
      --  (GPDMA_CxSR.TOF =1 ), and an interrupt is generated if enabled
      --  (GPDMA_CxCR.TOIE = 1). The channel is not automatically disabled by
      --  hardware due to a trigger overrun. 	Note: When the source block size
      --  is not a multiple of the source burst size and is a multiple of the
      --  source data width, then the last programmed source burst is not
      --  completed and is internally shorten to match the block size. In this
      --  case, if TRIGM[1:0] = 11 and (SWREQ =1  or (SWREQ = 0 and
      --  DREQ =0 )), the shortened burst transfer (by singles or/and by
      --  bursts of lower length) is conditioned once by the trigger. 	When the
      --  programmed destination burst is internally shortened by singles
      --  or/and by bursts of lower length (versus FIFO size, versus block
      --  size, 1-Kbyte boundary address crossing): if the trigger is
      --  conditioning the programmed destination burst (if TRIGM[1:0] = 11
      --  and SWREQ = 0 and DREQ = 1), this shortened destination burst
      --  transfer is conditioned once by the trigger.
      TRIGM          : GPDMA_C9TR2_TRIGM_Field := 16#0#;
      --  trigger event input selection 	These bits select the trigger event
      --  input of the GPDMA transfer (as per ), with an active trigger event
      --  if TRIGPOL[1:0] ≠ 00.
      TRIGSEL        : GPDMA_C9TR2_TRIGSEL_Field := 16#0#;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  trigger event polarity 	These bits define the polarity of the
      --  selected trigger event input defined by TRIGSEL[5:0].
      TRIGPOL        : GPDMA_C9TR2_TRIGPOL_Field := 16#0#;
      --  unspecified
      Reserved_26_29 : HAL.UInt4 := 16#0#;
      --  transfer complete event mode 	These bits define the transfer
      --  granularity for the transfer complete and half transfer complete
      --  events generation. 	Note: If the initial LLI0 data transfer is
      --  null/void (directly programmed by the internal register file with
      --  GPDMA_CxBR1.BNDT[15:0] = 0), then neither the complete transfer
      --  event nor the half transfer event is generated. 	Note: If the initial
      --  LLI0 data transfer is null/void (directly programmed by the internal
      --  register file with GPDMA_CxBR1.BNDT[15:0] = 0), then neither the
      --  complete transfer event nor the half transfer event is generated.
      --  	Note: If the initial LLI0 data transfer is null/void (i.e. directly
      --  programmed by the internal register file with
      --  GPDMA_CxBR1.BNDT[15:0] =0 ), then the half transfer event is not
      --  generated, and the transfer complete event is generated when is
      --  completed the loading of the LLI1.
      TCEM           : GPDMA_C9TR2_TCEM_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C9TR2_Register use record
      REQSEL         at 0 range 0 .. 6;
      Reserved_7_8   at 0 range 7 .. 8;
      SWREQ          at 0 range 9 .. 9;
      DREQ           at 0 range 10 .. 10;
      BREQ           at 0 range 11 .. 11;
      Reserved_12_13 at 0 range 12 .. 13;
      TRIGM          at 0 range 14 .. 15;
      TRIGSEL        at 0 range 16 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      TRIGPOL        at 0 range 24 .. 25;
      Reserved_26_29 at 0 range 26 .. 29;
      TCEM           at 0 range 30 .. 31;
   end record;

   subtype GPDMA_C9BR1_BNDT_Field is HAL.UInt16;

   --  GPDMA channel 9 block register 1
   type GPDMA_C9BR1_Register is record
      --  block number of data bytes to transfer from the source 	Block size
      --  transferred from the source. When the channel is enabled, this field
      --  becomes read-only and is decremented, indicating the remaining number
      --  of data items in the current source block to be transferred.
      --  BNDT[15:0] is programmed in number of bytes, maximum source block
      --  size is 64 Kbytes -1. 	Once the last data transfer is completed
      --  (BNDT[15:0] = 0): 	- if GPDMA_CxLLR.UB1 = 1, this field is
      --  updated by the LLI in the memory. 	- if GPDMA_CxLLR.UB1 = 0 and if
      --  there is at least one non null Uxx update bit, this field is
      --  internally restored to the programmed value. 	- if all
      --  GPDMA_CxLLR.Uxx = 0 and if GPDMA_CxLLR.LA[15:0] = 0, this field
      --  is internally restored to the programmed value (infinite/continuous
      --  last LLI). 	- if GPDMA_CxLLR = 0, this field is kept as zero
      --  following the last LLI data transfer. 	Note: A non-null source block
      --  size must be a multiple of the source data width (BNDT[2:0] versus
      --  GPDMA_CxTR1.SDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued. 	When configured in packing mode
      --  (GPDMA_CxTR1.PAM[1] = 1 and destination data width different from
      --  source data width), a non-null source block size must be a multiple
      --  of the destination data width (BNDT[2:0] versus
      --  GPDMA_CxTR1.DDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued.
      BNDT           : GPDMA_C9BR1_BNDT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C9BR1_Register use record
      BNDT           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype GPDMA_C9LLR_LA_Field is HAL.UInt14;

   --  GPDMA channel 9 linked-list address register
   type GPDMA_C9LLR_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  pointer (16-bit low-significant address) to the next linked-list data
      --  structure 	If UT1 = UT2 = UB1 = USA = UDA = ULL = 0 and
      --  if LA[15:20] = 0, the current LLI is the last one. The channel
      --  transfer is completed without any update of the linked-list GPDMA
      --  register file. 	Else, this field is the pointer to the memory address
      --  offset from which the next linked-list data structure is
      --  automatically fetched from, once the data transfer is completed, in
      --  order to conditionally update the linked-list GPDMA internal register
      --  file (GPDMA_CxCTR1, GPDMA_CxTR2, GPDMA_CxBR1, GPDMA_CxSAR,
      --  GPDMA_CxDAR and GPDMA_CxLLR). 	Note: The user must program the
      --  pointer to be 32-bit aligned. The two low-significant bits are write
      --  ignored.
      LA             : GPDMA_C9LLR_LA_Field := 16#0#;
      --  Update GPDMA_CxLLR register from memory 	This bit is used to control
      --  the update of GPDMA_CxLLR from the memory during the link transfer.
      ULL            : Boolean := False;
      --  unspecified
      Reserved_17_26 : HAL.UInt10 := 16#0#;
      --  Update GPDMA_CxDAR register from memory 	This bit is used to control
      --  the update of GPDMA_CxDAR from the memory during the link transfer.
      UDA            : Boolean := False;
      --  update GPDMA_CxSAR from memory 	This bit controls the update of
      --  GPDMA_CxSAR from the memory during the link transfer.
      USA            : Boolean := False;
      --  Update GPDMA_CxBR1 from memory 	This bit controls the update of
      --  GPDMA_CxBR1 from the memory during the link transfer. If UB1 = 0
      --  and if GPDMA_CxLLR ≠ 0, the linked-list is not completed.
      --  GPDMA_CxBR1.BNDT[15:0] is then restored to the programmed value after
      --  data transfer is completed and before the link transfer.
      UB1            : Boolean := False;
      --  Update GPDMA_CxTR2 from memory 	This bit controls the update of
      --  GPDMA_CxTR2 from the memory during the link transfer.
      UT2            : Boolean := False;
      --  Update GPDMA_CxTR1 from memory 	This bit controls the update of
      --  GPDMA_CxTR1 from the memory during the link transfer.
      UT1            : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C9LLR_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      LA             at 0 range 2 .. 15;
      ULL            at 0 range 16 .. 16;
      Reserved_17_26 at 0 range 17 .. 26;
      UDA            at 0 range 27 .. 27;
      USA            at 0 range 28 .. 28;
      UB1            at 0 range 29 .. 29;
      UT2            at 0 range 30 .. 30;
      UT1            at 0 range 31 .. 31;
   end record;

   subtype GPDMA_C10LBAR_LBA_Field is HAL.UInt16;

   --  GPDMA channel 10 linked-list base address register
   type GPDMA_C10LBAR_Register is record
      --  unspecified
      Reserved_0_15 : HAL.UInt16 := 16#0#;
      --  linked-list base address of GPDMA channel x
      LBA           : GPDMA_C10LBAR_LBA_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C10LBAR_Register use record
      Reserved_0_15 at 0 range 0 .. 15;
      LBA           at 0 range 16 .. 31;
   end record;

   --  GPDMA channel 10 flag clear register
   type GPDMA_C10FCR_Register is record
      --  unspecified
      Reserved_0_7   : HAL.UInt8 := 16#0#;
      --  Write-only. transfer complete flag clear
      TCF            : Boolean := False;
      --  Write-only. half transfer flag clear
      HTF            : Boolean := False;
      --  Write-only. data transfer error flag clear
      DTEF           : Boolean := False;
      --  Write-only. update link transfer error flag clear
      ULEF           : Boolean := False;
      --  Write-only. user setting error flag clear
      USEF           : Boolean := False;
      --  Write-only. completed suspension flag clear
      SUSPF          : Boolean := False;
      --  Write-only. trigger overrun flag clear
      TOF            : Boolean := False;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C10FCR_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      TCF            at 0 range 8 .. 8;
      HTF            at 0 range 9 .. 9;
      DTEF           at 0 range 10 .. 10;
      ULEF           at 0 range 11 .. 11;
      USEF           at 0 range 12 .. 12;
      SUSPF          at 0 range 13 .. 13;
      TOF            at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype GPDMA_C10SR_FIFOL_Field is HAL.UInt8;

   --  GPDMA channel 10 status register
   type GPDMA_C10SR_Register is record
      --  Read-only. idle flag 	This idle flag is de-asserted by hardware when
      --  the channel is enabled (GPDMA_CxCR.EN = 1) with a valid channel
      --  configuration (no USEF to be immediately reported). 	This idle flag
      --  is asserted after hard reset or by hardware when the channel is back
      --  in idle state (in suspended or disabled state).
      IDLEF          : Boolean;
      --  unspecified
      Reserved_1_7   : HAL.UInt7;
      --  Read-only. transfer complete flag 	A transfer complete event is
      --  either a block transfer complete, a 2D/repeated block transfer
      --  complete, a LLI transfer complete including the upload of the next
      --  LLI if any, or the full linked-list completion, depending on the
      --  transfer complete event mode (GPDMA_CxTR2.TCEM[1:0]).
      TCF            : Boolean;
      --  Read-only. half transfer flag 	An half transfer event is either an
      --  half block transfer or an half 2D/repeated block transfer, depending
      --  on the transfer complete event mode (GPDMA_CxTR2.TCEM[1:0]). 	An half
      --  block transfer occurs when half of the bytes of the source block size
      --  (rounded up integer of GPDMA_CxBR1.BNDT[15:0]/2) has been transferred
      --  to the destination. 	An half 2D/repeated block transfer occurs when
      --  half of the repeated blocks (rounded up integer of
      --  (GPDMA_CxBR1.BRC[10:0]+1)/2)) has been transferred to the
      --  destination.
      HTF            : Boolean;
      --  Read-only. data transfer error flag
      DTEF           : Boolean;
      --  Read-only. update link transfer error flag
      ULEF           : Boolean;
      --  Read-only. user setting error flag
      USEF           : Boolean;
      --  Read-only. completed suspension flag
      SUSPF          : Boolean;
      --  Read-only. trigger overrun flag
      TOF            : Boolean;
      --  unspecified
      Reserved_15_15 : HAL.Bit;
      --  Read-only. monitored FIFO level 	Number of available write beats in
      --  the FIFO, in units of the programmed destination data width (see
      --  GPDMA_CxTR1.DDW_LOG2[1:0], in units of bytes, half-words, or words).
      --  	Note: After having suspended an active transfer, the user may need
      --  to read FIFOL[7:0], additionally to GPDMA_CxBR1.BDNT[15:0] and
      --  GPDMA_CxBR1.BRC[10:0], to know how many data have been transferred to
      --  the destination. Before reading, the user may wait for the transfer
      --  to be suspended (GPDMA_CxSR.SUSPF = 1).
      FIFOL          : GPDMA_C10SR_FIFOL_Field;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C10SR_Register use record
      IDLEF          at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      TCF            at 0 range 8 .. 8;
      HTF            at 0 range 9 .. 9;
      DTEF           at 0 range 10 .. 10;
      ULEF           at 0 range 11 .. 11;
      USEF           at 0 range 12 .. 12;
      SUSPF          at 0 range 13 .. 13;
      TOF            at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      FIFOL          at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype GPDMA_C10CR_PRIO_Field is HAL.UInt2;

   --  GPDMA channel 10 control register
   type GPDMA_C10CR_Register is record
      --  enable 	Writing 1 into the field RESET (bit 1) causes the hardware to
      --  de-assert this bit, whatever is written into this bit 0. Else: 	this
      --  bit is de-asserted by hardware when there is a transfer error (master
      --  bus error or user setting error) or when there is a channel transfer
      --  complete (channel ready to be configured, e.g. if LSM=1 at the end of
      --  a single execution of the LLI). 	Else, this bit can be asserted by
      --  software. 	Writing 0 into this EN bit is ignored.
      EN             : Boolean := False;
      --  Write-only. reset 	This bit is write only. Writing 0 has no impact.
      --  Writing 1 implies the reset of the following: the FIFO, the channel
      --  internal state, SUSP and EN bits (whatever is written receptively in
      --  bit 2 and bit 0). 	The reset is effective when the channel is in
      --  steady state, meaning one of the following: 	- active channel in
      --  suspended state (GPDMA_CxSR.SUSPF = 1 and
      --  GPDMA_CxSR.IDLEF = GPDMA_CxCR.EN = 1) 	- channel in disabled
      --  state (GPDMA_CxSR.IDLEF = 1 and GPDMA_CxCR.EN = 0). 	After
      --  writing a RESET, to continue using this channel, the user must
      --  explicitly reconfigure the channel including the hardware-modified
      --  configuration registers (GPDMA_CxBR1, GPDMA_CxSAR and GPDMA_CxDAR)
      --  before enabling again the channel (see the programming sequence in ).
      RESET          : Boolean := False;
      --  suspend 	Writing 1 into the field RESET (bit 1) causes the hardware
      --  to de-assert this bit, whatever is written into this bit 2. Else:
      --  	Software must write 1 in order to suspend an active channel i.e. a
      --  channel with an on-going GPDMA transfer over its master ports. 	The
      --  software must write 0 in order to resume a suspended channel,
      --  following the programming sequence detailed in .
      SUSP           : Boolean := False;
      --  unspecified
      Reserved_3_7   : HAL.UInt5 := 16#0#;
      --  transfer complete interrupt enable
      TCIE           : Boolean := False;
      --  half transfer complete interrupt enable
      HTIE           : Boolean := False;
      --  data transfer error interrupt enable
      DTEIE          : Boolean := False;
      --  update link transfer error interrupt enable
      ULEIE          : Boolean := False;
      --  user setting error interrupt enable
      USEIE          : Boolean := False;
      --  completed suspension interrupt enable
      SUSPIE         : Boolean := False;
      --  trigger overrun interrupt enable
      TOIE           : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Link step mode 	First the (possible 1D/repeated) block transfer is
      --  executed as defined by the current internal register file until
      --  GPDMA_CxBR1.BNDT[15:0] = 0 and GPDMA_CxBR1.BRC[10:0] = 0 if
      --  present. Secondly the next linked-list data structure is
      --  conditionally uploaded from memory as defined by GPDMA_CxLLR. Then
      --  channel execution is completed. 	Note: This bit must be written when
      --  EN=0. This bit is read-only when EN=1.
      LSM            : Boolean := False;
      --  linked-list allocated port 	This bit is used to allocate the master
      --  port for the update of the GPDMA linked-list registers from the
      --  memory. 	Note: This bit must be written when EN=0. This bit is
      --  read-only when EN=1.
      LAP            : Boolean := False;
      --  unspecified
      Reserved_18_21 : HAL.UInt4 := 16#0#;
      --  priority level of the channel x GPDMA transfer versus others 	Note:
      --  This bit must be written when EN = 0. This bit is read-only when
      --  EN = 1.
      PRIO           : GPDMA_C10CR_PRIO_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C10CR_Register use record
      EN             at 0 range 0 .. 0;
      RESET          at 0 range 1 .. 1;
      SUSP           at 0 range 2 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      TCIE           at 0 range 8 .. 8;
      HTIE           at 0 range 9 .. 9;
      DTEIE          at 0 range 10 .. 10;
      ULEIE          at 0 range 11 .. 11;
      USEIE          at 0 range 12 .. 12;
      SUSPIE         at 0 range 13 .. 13;
      TOIE           at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      LSM            at 0 range 16 .. 16;
      LAP            at 0 range 17 .. 17;
      Reserved_18_21 at 0 range 18 .. 21;
      PRIO           at 0 range 22 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype GPDMA_C10TR1_SDW_LOG2_Field is HAL.UInt2;
   subtype GPDMA_C10TR1_SBL_1_Field is HAL.UInt6;
   subtype GPDMA_C10TR1_PAM_Field is HAL.UInt2;
   subtype GPDMA_C10TR1_DDW_LOG2_Field is HAL.UInt2;
   subtype GPDMA_C10TR1_DBL_1_Field is HAL.UInt6;

   --  GPDMA channel 10 transfer register 1
   type GPDMA_C10TR1_Register is record
      --  binary logarithm of the source data width of a burst in bytes 	Note:
      --  Setting a 8-byte data width causes a user setting error to be
      --  reported and no transfer is issued. 	A source block size must be a
      --  multiple of the source data width (GPDMA_CxBR1.BNDT[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and no
      --  transfer is issued. 	A source single transfer must have an aligned
      --  address with its data width (start address GPDMA_CxSAR[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and none
      --  transfer is issued.
      SDW_LOG2       : GPDMA_C10TR1_SDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  source incrementing burst 	The source address, pointed by
      --  GPDMA_CxSAR, is kept constant after a burst beat/single transfer or
      --  is incremented by the offset value corresponding to a contiguous data
      --  after a burst beat/single transfer.
      SINC           : Boolean := False;
      --  source burst length minus 1, between 0 and 63 	The burst length unit
      --  is one data named beat within a burst. If SBL_1[5:0] =0 , the burst
      --  can be named as single. Each data/beat has a width defined by the
      --  destination data width SDW_LOG2[1:0]. 	Note: If a burst transfer
      --  crossed a 1-Kbyte address boundary on a AHB transfer, the GPDMA
      --  modifies and shortens the programmed burst into singles or bursts of
      --  lower length, to be compliant with the AHB protocol. 	If a burst
      --  transfer is of length greater than the FIFO size of the channel x,
      --  the GPDMA modifies and shortens the programmed burst into singles or
      --  bursts of lower length, to be compliant with the FIFO size. Transfer
      --  performance is lower, with GPDMA re-arbitration between effective and
      --  lower bursts/singles, but the data integrity is guaranteed.
      SBL_1          : GPDMA_C10TR1_SBL_1_Field := 16#0#;
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  padding/alignment mode 	If DDW_LOG2[1:0] = SDW_LOG2[1:0]: if the data
      --  width of a burst destination transfer is equal to the data width of a
      --  burst source transfer, these bits are ignored. 	Else: 	- Case 1: If
      --  destination data width > source data width 	1x: successive source
      --  data are FIFO queued and packed at the destination data width, in a
      --  left (LSB) to right (MSB) order (named little endian), before a
      --  destination transfer 	- Case 2: If destination data width < source
      --  data width 	1x: source data is FIFO queued and unpacked at the
      --  destination data width, to be transferred in a left (LSB) to right
      --  (MSB) order (named little endian) to the destination 	Note:
      PAM            : GPDMA_C10TR1_PAM_Field := 16#0#;
      --  source byte exchange within the unaligned half-word of each source
      --  word 	If the source data width is shorter than a word, this bit is
      --  ignored. 	If the source data width is a word:
      SBX            : Boolean := False;
      --  source allocated port 	This bit is used to allocate the master port
      --  for the source transfer 	Note: This bit must be written when
      --  EN = 0. This bit is read-only when EN = 1.
      SAP            : Boolean := False;
      --  security attribute of the GPDMA transfer from the source 	If
      --  GPDMA_SECCFGR.SECx = 1 and the access is secure: 	This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  GPDMA_SECCFGR.SECx =1 . A secure write is ignored when
      --  GPDMA_SECCFGR.SECx = 0. 	When GPDMA_SECCFGR.SECx is de-asserted,
      --  this SSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the GPDMA transfer
      --  from the source is non-secure.
      SSEC           : Boolean := False;
      --  binary logarithm of the destination data width of a burst, in bytes
      --  	Note: Setting a 8-byte data width causes a user setting error to be
      --  reported and none transfer is issued. 	A destination burst transfer
      --  must have an aligned address with its data width (start address
      --  GPDMA_CxDAR[2:0] and address offset GPDMA_CxTR3.DAO[2:0], versus
      --  DDW_LOG2[1:0]). Otherwise a user setting error is reported and no
      --  transfer is issued.
      DDW_LOG2       : GPDMA_C10TR1_DDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_18_18 : HAL.Bit := 16#0#;
      --  destination incrementing burst 	The destination address, pointed by
      --  GPDMA_CxDAR, is kept constant after a burst beat/single transfer, or
      --  is incremented by the offset value corresponding to a contiguous data
      --  after a burst beat/single transfer.
      DINC           : Boolean := False;
      --  destination burst length minus 1, between 0 and 63 	The burst length
      --  unit is one data named beat within a burst. If DBL_1[5:0] =0 , the
      --  burst can be named as single. Each data/beat has a width defined by
      --  the destination data width DDW_LOG2[1:0]. 	Note: If a burst transfer
      --  crossed a 1-Kbyte address boundary on a AHB transfer, the GPDMA
      --  modifies and shortens the programmed burst into singles or bursts of
      --  lower length, to be compliant with the AHB protocol. 	If a burst
      --  transfer is of length greater than the FIFO size of the channel x,
      --  the GPDMA modifies and shortens the programmed burst into singles or
      --  bursts of lower length, to be compliant with the FIFO size. Transfer
      --  performance is lower, with GPDMA re-arbitration between effective and
      --  lower bursts/singles, but the data integrity is guaranteed.
      DBL_1          : GPDMA_C10TR1_DBL_1_Field := 16#0#;
      --  destination byte exchange 	If the destination data size is a byte,
      --  this bit is ignored. 	If the destination data size is not a byte:
      DBX            : Boolean := False;
      --  destination half-word exchange 	If the destination data size is
      --  shorter than a word, this bit is ignored. 	If the destination data
      --  size is a word:
      DHX            : Boolean := False;
      --  unspecified
      Reserved_28_29 : HAL.UInt2 := 16#0#;
      --  destination allocated port 	This bit is used to allocate the master
      --  port for the destination transfer 	Note: This bit must be written
      --  when EN = 0. This bit is read-only when EN = 1.
      DAP            : Boolean := False;
      --  security attribute of the GPDMA transfer to the destination 	If
      --  GPDMA_SECCFGR.SECx = 1 and the access is secure: 	This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  GPDMA_SECCFGR.SECx = 1. A secure write is ignored when
      --  GPDMA_SECCFGR.SECx = 0. 	When GPDMA_SECCFGR.SECx is de-asserted,
      --  this DSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the GPDMA transfer
      --  to the destination is non-secure.
      DSEC           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C10TR1_Register use record
      SDW_LOG2       at 0 range 0 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      SINC           at 0 range 3 .. 3;
      SBL_1          at 0 range 4 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      PAM            at 0 range 11 .. 12;
      SBX            at 0 range 13 .. 13;
      SAP            at 0 range 14 .. 14;
      SSEC           at 0 range 15 .. 15;
      DDW_LOG2       at 0 range 16 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      DINC           at 0 range 19 .. 19;
      DBL_1          at 0 range 20 .. 25;
      DBX            at 0 range 26 .. 26;
      DHX            at 0 range 27 .. 27;
      Reserved_28_29 at 0 range 28 .. 29;
      DAP            at 0 range 30 .. 30;
      DSEC           at 0 range 31 .. 31;
   end record;

   subtype GPDMA_C10TR2_REQSEL_Field is HAL.UInt7;
   subtype GPDMA_C10TR2_TRIGM_Field is HAL.UInt2;
   subtype GPDMA_C10TR2_TRIGSEL_Field is HAL.UInt6;
   subtype GPDMA_C10TR2_TRIGPOL_Field is HAL.UInt2;
   subtype GPDMA_C10TR2_TCEM_Field is HAL.UInt2;

   --  GPDMA channel 10 transfer register 2
   type GPDMA_C10TR2_Register is record
      --  GPDMA hardware request selection 	These bits are ignored if channel x
      --  is activated (GPDMA_CxCR.EN asserted) with SWREQ = 1 (software
      --  request for a memory-to-memory transfer). Else, the selected hardware
      --  request is internally taken into account as per . 	The user must not
      --  assign a same input hardware request (same REQSEL[6:0] value) to
      --  different active GPDMA channels (GPDMA_CxCR.EN = 1 and
      --  GPDMA_CxTR2.SWREQ = 0 for these channels). GPDMA is not intended to
      --  hardware support the case of simultaneous enabled channels
      --  incorrectly configured with a same hardware peripheral request
      --  signal, and there is no user setting error reporting.
      REQSEL         : GPDMA_C10TR2_REQSEL_Field := 16#0#;
      --  unspecified
      Reserved_7_8   : HAL.UInt2 := 16#0#;
      --  software request 	This bit is internally taken into account when
      --  GPDMA_CxCR.EN is asserted.
      SWREQ          : Boolean := False;
      --  destination hardware request 	This bit is ignored if channel x is
      --  activated (GPDMA_CxCR.EN asserted) with SWREQ = 1 (software request
      --  for a memory-to-memory transfer). Else: 	Note:
      DREQ           : Boolean := False;
      --  Block hardware request 	If the channel x is activated (GPDMA_CxCR.EN
      --  asserted) with SWREQ = 1 (software request for a memory-to-memory
      --  transfer), this bit is ignored. Else:
      BREQ           : Boolean := False;
      --  unspecified
      Reserved_12_13 : HAL.UInt2 := 16#0#;
      --  trigger mode 	These bits define the transfer granularity for its
      --  conditioning by the trigger. 	If the channel x is enabled
      --  (GPDMA_CxCR.EN asserted) with TRIGPOL[1:0] = 00 or 11, these
      --  TRIGM[1:0] bits are ignored. 	Else, a GPDMA transfer is conditioned
      --  by at least one trigger hit: 	first burst read of a 2D/repeated block
      --  transfer is conditioned by one hit trigger. 	– If the peripheral is
      --  programmed as a source (DREQ = 0) of the LLI data transfer, each
      --  programmed burst read is conditioned. 	– If the peripheral is
      --  programmed as a destination (DREQ = 1) of the LLI data transfer,
      --  each programmed burst write is conditioned. The first memory burst
      --  read of a (possibly 2D/repeated) block, also named as the first ready
      --  FIFO-based source burst, is gated by the occurrence of both the
      --  hardware request and the first trigger hit. 	The GPDMA monitoring of
      --  a trigger for channel x is started when the channel is enabled/loaded
      --  with a new active trigger configuration: rising or falling edge on a
      --  selected trigger (TRIGPOL[1:0] = 01 or respectively
      --  TRIGPOL[1:0] = 10). 	The monitoring of this trigger is kept active
      --  during the triggered and uncompleted (data or link) transfer; and if
      --  a new trigger is detected then, this hit is internally memorized to
      --  grant the next transfer, as long as the defined rising or falling
      --  edge is not modified, and the TRIGSEL[5:0] is not modified, and the
      --  channel is enabled. 	Transferring a next LLIn+1 that updates the
      --  GPDMA_CxTR2 with a new value for any of TRIGSEL[5:0] or TRIGPOL[1:0],
      --  resets the monitoring, trashing the memorized hit of the formerly
      --  defined LLIn trigger. 	After a first new trigger hitn+1 is memorized,
      --  if another second trigger hitn+2 is detected and if the hitn
      --  triggered transfer is still not completed, hitn+2 is lost and not
      --  memorized.memorized. A trigger overrun flag is reported
      --  (GPDMA_CxSR.TOF =1 ), and an interrupt is generated if enabled
      --  (GPDMA_CxCR.TOIE = 1). The channel is not automatically disabled by
      --  hardware due to a trigger overrun. 	Note: When the source block size
      --  is not a multiple of the source burst size and is a multiple of the
      --  source data width, then the last programmed source burst is not
      --  completed and is internally shorten to match the block size. In this
      --  case, if TRIGM[1:0] = 11 and (SWREQ =1  or (SWREQ = 0 and
      --  DREQ =0 )), the shortened burst transfer (by singles or/and by
      --  bursts of lower length) is conditioned once by the trigger. 	When the
      --  programmed destination burst is internally shortened by singles
      --  or/and by bursts of lower length (versus FIFO size, versus block
      --  size, 1-Kbyte boundary address crossing): if the trigger is
      --  conditioning the programmed destination burst (if TRIGM[1:0] = 11
      --  and SWREQ = 0 and DREQ = 1), this shortened destination burst
      --  transfer is conditioned once by the trigger.
      TRIGM          : GPDMA_C10TR2_TRIGM_Field := 16#0#;
      --  trigger event input selection 	These bits select the trigger event
      --  input of the GPDMA transfer (as per ), with an active trigger event
      --  if TRIGPOL[1:0] ≠ 00.
      TRIGSEL        : GPDMA_C10TR2_TRIGSEL_Field := 16#0#;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  trigger event polarity 	These bits define the polarity of the
      --  selected trigger event input defined by TRIGSEL[5:0].
      TRIGPOL        : GPDMA_C10TR2_TRIGPOL_Field := 16#0#;
      --  unspecified
      Reserved_26_29 : HAL.UInt4 := 16#0#;
      --  transfer complete event mode 	These bits define the transfer
      --  granularity for the transfer complete and half transfer complete
      --  events generation. 	Note: If the initial LLI0 data transfer is
      --  null/void (directly programmed by the internal register file with
      --  GPDMA_CxBR1.BNDT[15:0] = 0), then neither the complete transfer
      --  event nor the half transfer event is generated. 	Note: If the initial
      --  LLI0 data transfer is null/void (directly programmed by the internal
      --  register file with GPDMA_CxBR1.BNDT[15:0] = 0), then neither the
      --  complete transfer event nor the half transfer event is generated.
      --  	Note: If the initial LLI0 data transfer is null/void (i.e. directly
      --  programmed by the internal register file with
      --  GPDMA_CxBR1.BNDT[15:0] =0 ), then the half transfer event is not
      --  generated, and the transfer complete event is generated when is
      --  completed the loading of the LLI1.
      TCEM           : GPDMA_C10TR2_TCEM_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C10TR2_Register use record
      REQSEL         at 0 range 0 .. 6;
      Reserved_7_8   at 0 range 7 .. 8;
      SWREQ          at 0 range 9 .. 9;
      DREQ           at 0 range 10 .. 10;
      BREQ           at 0 range 11 .. 11;
      Reserved_12_13 at 0 range 12 .. 13;
      TRIGM          at 0 range 14 .. 15;
      TRIGSEL        at 0 range 16 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      TRIGPOL        at 0 range 24 .. 25;
      Reserved_26_29 at 0 range 26 .. 29;
      TCEM           at 0 range 30 .. 31;
   end record;

   subtype GPDMA_C10BR1_BNDT_Field is HAL.UInt16;

   --  GPDMA channel 10 block register 1
   type GPDMA_C10BR1_Register is record
      --  block number of data bytes to transfer from the source 	Block size
      --  transferred from the source. When the channel is enabled, this field
      --  becomes read-only and is decremented, indicating the remaining number
      --  of data items in the current source block to be transferred.
      --  BNDT[15:0] is programmed in number of bytes, maximum source block
      --  size is 64 Kbytes -1. 	Once the last data transfer is completed
      --  (BNDT[15:0] = 0): 	- if GPDMA_CxLLR.UB1 = 1, this field is
      --  updated by the LLI in the memory. 	- if GPDMA_CxLLR.UB1 = 0 and if
      --  there is at least one non null Uxx update bit, this field is
      --  internally restored to the programmed value. 	- if all
      --  GPDMA_CxLLR.Uxx = 0 and if GPDMA_CxLLR.LA[15:0] = 0, this field
      --  is internally restored to the programmed value (infinite/continuous
      --  last LLI). 	- if GPDMA_CxLLR = 0, this field is kept as zero
      --  following the last LLI data transfer. 	Note: A non-null source block
      --  size must be a multiple of the source data width (BNDT[2:0] versus
      --  GPDMA_CxTR1.SDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued. 	When configured in packing mode
      --  (GPDMA_CxTR1.PAM[1] = 1 and destination data width different from
      --  source data width), a non-null source block size must be a multiple
      --  of the destination data width (BNDT[2:0] versus
      --  GPDMA_CxTR1.DDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued.
      BNDT           : GPDMA_C10BR1_BNDT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C10BR1_Register use record
      BNDT           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype GPDMA_C10LLR_LA_Field is HAL.UInt14;

   --  GPDMA channel 10 linked-list address register
   type GPDMA_C10LLR_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  pointer (16-bit low-significant address) to the next linked-list data
      --  structure 	If UT1 = UT2 = UB1 = USA = UDA = ULL = 0 and
      --  if LA[15:20] = 0, the current LLI is the last one. The channel
      --  transfer is completed without any update of the linked-list GPDMA
      --  register file. 	Else, this field is the pointer to the memory address
      --  offset from which the next linked-list data structure is
      --  automatically fetched from, once the data transfer is completed, in
      --  order to conditionally update the linked-list GPDMA internal register
      --  file (GPDMA_CxCTR1, GPDMA_CxTR2, GPDMA_CxBR1, GPDMA_CxSAR,
      --  GPDMA_CxDAR and GPDMA_CxLLR). 	Note: The user must program the
      --  pointer to be 32-bit aligned. The two low-significant bits are write
      --  ignored.
      LA             : GPDMA_C10LLR_LA_Field := 16#0#;
      --  Update GPDMA_CxLLR register from memory 	This bit is used to control
      --  the update of GPDMA_CxLLR from the memory during the link transfer.
      ULL            : Boolean := False;
      --  unspecified
      Reserved_17_26 : HAL.UInt10 := 16#0#;
      --  Update GPDMA_CxDAR register from memory 	This bit is used to control
      --  the update of GPDMA_CxDAR from the memory during the link transfer.
      UDA            : Boolean := False;
      --  update GPDMA_CxSAR from memory 	This bit controls the update of
      --  GPDMA_CxSAR from the memory during the link transfer.
      USA            : Boolean := False;
      --  Update GPDMA_CxBR1 from memory 	This bit controls the update of
      --  GPDMA_CxBR1 from the memory during the link transfer. If UB1 = 0
      --  and if GPDMA_CxLLR ≠ 0, the linked-list is not completed.
      --  GPDMA_CxBR1.BNDT[15:0] is then restored to the programmed value after
      --  data transfer is completed and before the link transfer.
      UB1            : Boolean := False;
      --  Update GPDMA_CxTR2 from memory 	This bit controls the update of
      --  GPDMA_CxTR2 from the memory during the link transfer.
      UT2            : Boolean := False;
      --  Update GPDMA_CxTR1 from memory 	This bit controls the update of
      --  GPDMA_CxTR1 from the memory during the link transfer.
      UT1            : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C10LLR_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      LA             at 0 range 2 .. 15;
      ULL            at 0 range 16 .. 16;
      Reserved_17_26 at 0 range 17 .. 26;
      UDA            at 0 range 27 .. 27;
      USA            at 0 range 28 .. 28;
      UB1            at 0 range 29 .. 29;
      UT2            at 0 range 30 .. 30;
      UT1            at 0 range 31 .. 31;
   end record;

   subtype GPDMA_C11LBAR_LBA_Field is HAL.UInt16;

   --  GPDMA channel 11 linked-list base address register
   type GPDMA_C11LBAR_Register is record
      --  unspecified
      Reserved_0_15 : HAL.UInt16 := 16#0#;
      --  linked-list base address of GPDMA channel x
      LBA           : GPDMA_C11LBAR_LBA_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C11LBAR_Register use record
      Reserved_0_15 at 0 range 0 .. 15;
      LBA           at 0 range 16 .. 31;
   end record;

   --  GPDMA channel 11 flag clear register
   type GPDMA_C11FCR_Register is record
      --  unspecified
      Reserved_0_7   : HAL.UInt8 := 16#0#;
      --  Write-only. transfer complete flag clear
      TCF            : Boolean := False;
      --  Write-only. half transfer flag clear
      HTF            : Boolean := False;
      --  Write-only. data transfer error flag clear
      DTEF           : Boolean := False;
      --  Write-only. update link transfer error flag clear
      ULEF           : Boolean := False;
      --  Write-only. user setting error flag clear
      USEF           : Boolean := False;
      --  Write-only. completed suspension flag clear
      SUSPF          : Boolean := False;
      --  Write-only. trigger overrun flag clear
      TOF            : Boolean := False;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C11FCR_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      TCF            at 0 range 8 .. 8;
      HTF            at 0 range 9 .. 9;
      DTEF           at 0 range 10 .. 10;
      ULEF           at 0 range 11 .. 11;
      USEF           at 0 range 12 .. 12;
      SUSPF          at 0 range 13 .. 13;
      TOF            at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype GPDMA_C11SR_FIFOL_Field is HAL.UInt8;

   --  GPDMA channel 11 status register
   type GPDMA_C11SR_Register is record
      --  Read-only. idle flag 	This idle flag is de-asserted by hardware when
      --  the channel is enabled (GPDMA_CxCR.EN = 1) with a valid channel
      --  configuration (no USEF to be immediately reported). 	This idle flag
      --  is asserted after hard reset or by hardware when the channel is back
      --  in idle state (in suspended or disabled state).
      IDLEF          : Boolean;
      --  unspecified
      Reserved_1_7   : HAL.UInt7;
      --  Read-only. transfer complete flag 	A transfer complete event is
      --  either a block transfer complete, a 2D/repeated block transfer
      --  complete, a LLI transfer complete including the upload of the next
      --  LLI if any, or the full linked-list completion, depending on the
      --  transfer complete event mode (GPDMA_CxTR2.TCEM[1:0]).
      TCF            : Boolean;
      --  Read-only. half transfer flag 	An half transfer event is either an
      --  half block transfer or an half 2D/repeated block transfer, depending
      --  on the transfer complete event mode (GPDMA_CxTR2.TCEM[1:0]). 	An half
      --  block transfer occurs when half of the bytes of the source block size
      --  (rounded up integer of GPDMA_CxBR1.BNDT[15:0]/2) has been transferred
      --  to the destination. 	An half 2D/repeated block transfer occurs when
      --  half of the repeated blocks (rounded up integer of
      --  (GPDMA_CxBR1.BRC[10:0]+1)/2)) has been transferred to the
      --  destination.
      HTF            : Boolean;
      --  Read-only. data transfer error flag
      DTEF           : Boolean;
      --  Read-only. update link transfer error flag
      ULEF           : Boolean;
      --  Read-only. user setting error flag
      USEF           : Boolean;
      --  Read-only. completed suspension flag
      SUSPF          : Boolean;
      --  Read-only. trigger overrun flag
      TOF            : Boolean;
      --  unspecified
      Reserved_15_15 : HAL.Bit;
      --  Read-only. monitored FIFO level 	Number of available write beats in
      --  the FIFO, in units of the programmed destination data width (see
      --  GPDMA_CxTR1.DDW_LOG2[1:0], in units of bytes, half-words, or words).
      --  	Note: After having suspended an active transfer, the user may need
      --  to read FIFOL[7:0], additionally to GPDMA_CxBR1.BDNT[15:0] and
      --  GPDMA_CxBR1.BRC[10:0], to know how many data have been transferred to
      --  the destination. Before reading, the user may wait for the transfer
      --  to be suspended (GPDMA_CxSR.SUSPF = 1).
      FIFOL          : GPDMA_C11SR_FIFOL_Field;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C11SR_Register use record
      IDLEF          at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      TCF            at 0 range 8 .. 8;
      HTF            at 0 range 9 .. 9;
      DTEF           at 0 range 10 .. 10;
      ULEF           at 0 range 11 .. 11;
      USEF           at 0 range 12 .. 12;
      SUSPF          at 0 range 13 .. 13;
      TOF            at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      FIFOL          at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype GPDMA_C11CR_PRIO_Field is HAL.UInt2;

   --  GPDMA channel 11 control register
   type GPDMA_C11CR_Register is record
      --  enable 	Writing 1 into the field RESET (bit 1) causes the hardware to
      --  de-assert this bit, whatever is written into this bit 0. Else: 	this
      --  bit is de-asserted by hardware when there is a transfer error (master
      --  bus error or user setting error) or when there is a channel transfer
      --  complete (channel ready to be configured, e.g. if LSM=1 at the end of
      --  a single execution of the LLI). 	Else, this bit can be asserted by
      --  software. 	Writing 0 into this EN bit is ignored.
      EN             : Boolean := False;
      --  Write-only. reset 	This bit is write only. Writing 0 has no impact.
      --  Writing 1 implies the reset of the following: the FIFO, the channel
      --  internal state, SUSP and EN bits (whatever is written receptively in
      --  bit 2 and bit 0). 	The reset is effective when the channel is in
      --  steady state, meaning one of the following: 	- active channel in
      --  suspended state (GPDMA_CxSR.SUSPF = 1 and
      --  GPDMA_CxSR.IDLEF = GPDMA_CxCR.EN = 1) 	- channel in disabled
      --  state (GPDMA_CxSR.IDLEF = 1 and GPDMA_CxCR.EN = 0). 	After
      --  writing a RESET, to continue using this channel, the user must
      --  explicitly reconfigure the channel including the hardware-modified
      --  configuration registers (GPDMA_CxBR1, GPDMA_CxSAR and GPDMA_CxDAR)
      --  before enabling again the channel (see the programming sequence in ).
      RESET          : Boolean := False;
      --  suspend 	Writing 1 into the field RESET (bit 1) causes the hardware
      --  to de-assert this bit, whatever is written into this bit 2. Else:
      --  	Software must write 1 in order to suspend an active channel i.e. a
      --  channel with an on-going GPDMA transfer over its master ports. 	The
      --  software must write 0 in order to resume a suspended channel,
      --  following the programming sequence detailed in .
      SUSP           : Boolean := False;
      --  unspecified
      Reserved_3_7   : HAL.UInt5 := 16#0#;
      --  transfer complete interrupt enable
      TCIE           : Boolean := False;
      --  half transfer complete interrupt enable
      HTIE           : Boolean := False;
      --  data transfer error interrupt enable
      DTEIE          : Boolean := False;
      --  update link transfer error interrupt enable
      ULEIE          : Boolean := False;
      --  user setting error interrupt enable
      USEIE          : Boolean := False;
      --  completed suspension interrupt enable
      SUSPIE         : Boolean := False;
      --  trigger overrun interrupt enable
      TOIE           : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Link step mode 	First the (possible 1D/repeated) block transfer is
      --  executed as defined by the current internal register file until
      --  GPDMA_CxBR1.BNDT[15:0] = 0 and GPDMA_CxBR1.BRC[10:0] = 0 if
      --  present. Secondly the next linked-list data structure is
      --  conditionally uploaded from memory as defined by GPDMA_CxLLR. Then
      --  channel execution is completed. 	Note: This bit must be written when
      --  EN=0. This bit is read-only when EN=1.
      LSM            : Boolean := False;
      --  linked-list allocated port 	This bit is used to allocate the master
      --  port for the update of the GPDMA linked-list registers from the
      --  memory. 	Note: This bit must be written when EN=0. This bit is
      --  read-only when EN=1.
      LAP            : Boolean := False;
      --  unspecified
      Reserved_18_21 : HAL.UInt4 := 16#0#;
      --  priority level of the channel x GPDMA transfer versus others 	Note:
      --  This bit must be written when EN = 0. This bit is read-only when
      --  EN = 1.
      PRIO           : GPDMA_C11CR_PRIO_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C11CR_Register use record
      EN             at 0 range 0 .. 0;
      RESET          at 0 range 1 .. 1;
      SUSP           at 0 range 2 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      TCIE           at 0 range 8 .. 8;
      HTIE           at 0 range 9 .. 9;
      DTEIE          at 0 range 10 .. 10;
      ULEIE          at 0 range 11 .. 11;
      USEIE          at 0 range 12 .. 12;
      SUSPIE         at 0 range 13 .. 13;
      TOIE           at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      LSM            at 0 range 16 .. 16;
      LAP            at 0 range 17 .. 17;
      Reserved_18_21 at 0 range 18 .. 21;
      PRIO           at 0 range 22 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype GPDMA_C11TR1_SDW_LOG2_Field is HAL.UInt2;
   subtype GPDMA_C11TR1_SBL_1_Field is HAL.UInt6;
   subtype GPDMA_C11TR1_PAM_Field is HAL.UInt2;
   subtype GPDMA_C11TR1_DDW_LOG2_Field is HAL.UInt2;
   subtype GPDMA_C11TR1_DBL_1_Field is HAL.UInt6;

   --  GPDMA channel 11 transfer register 1
   type GPDMA_C11TR1_Register is record
      --  binary logarithm of the source data width of a burst in bytes 	Note:
      --  Setting a 8-byte data width causes a user setting error to be
      --  reported and no transfer is issued. 	A source block size must be a
      --  multiple of the source data width (GPDMA_CxBR1.BNDT[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and no
      --  transfer is issued. 	A source single transfer must have an aligned
      --  address with its data width (start address GPDMA_CxSAR[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and none
      --  transfer is issued.
      SDW_LOG2       : GPDMA_C11TR1_SDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  source incrementing burst 	The source address, pointed by
      --  GPDMA_CxSAR, is kept constant after a burst beat/single transfer or
      --  is incremented by the offset value corresponding to a contiguous data
      --  after a burst beat/single transfer.
      SINC           : Boolean := False;
      --  source burst length minus 1, between 0 and 63 	The burst length unit
      --  is one data named beat within a burst. If SBL_1[5:0] =0 , the burst
      --  can be named as single. Each data/beat has a width defined by the
      --  destination data width SDW_LOG2[1:0]. 	Note: If a burst transfer
      --  crossed a 1-Kbyte address boundary on a AHB transfer, the GPDMA
      --  modifies and shortens the programmed burst into singles or bursts of
      --  lower length, to be compliant with the AHB protocol. 	If a burst
      --  transfer is of length greater than the FIFO size of the channel x,
      --  the GPDMA modifies and shortens the programmed burst into singles or
      --  bursts of lower length, to be compliant with the FIFO size. Transfer
      --  performance is lower, with GPDMA re-arbitration between effective and
      --  lower bursts/singles, but the data integrity is guaranteed.
      SBL_1          : GPDMA_C11TR1_SBL_1_Field := 16#0#;
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  padding/alignment mode 	If DDW_LOG2[1:0] = SDW_LOG2[1:0]: if the data
      --  width of a burst destination transfer is equal to the data width of a
      --  burst source transfer, these bits are ignored. 	Else: 	- Case 1: If
      --  destination data width > source data width 	1x: successive source
      --  data are FIFO queued and packed at the destination data width, in a
      --  left (LSB) to right (MSB) order (named little endian), before a
      --  destination transfer 	- Case 2: If destination data width < source
      --  data width 	1x: source data is FIFO queued and unpacked at the
      --  destination data width, to be transferred in a left (LSB) to right
      --  (MSB) order (named little endian) to the destination 	Note:
      PAM            : GPDMA_C11TR1_PAM_Field := 16#0#;
      --  source byte exchange within the unaligned half-word of each source
      --  word 	If the source data width is shorter than a word, this bit is
      --  ignored. 	If the source data width is a word:
      SBX            : Boolean := False;
      --  source allocated port 	This bit is used to allocate the master port
      --  for the source transfer 	Note: This bit must be written when
      --  EN = 0. This bit is read-only when EN = 1.
      SAP            : Boolean := False;
      --  security attribute of the GPDMA transfer from the source 	If
      --  GPDMA_SECCFGR.SECx = 1 and the access is secure: 	This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  GPDMA_SECCFGR.SECx =1 . A secure write is ignored when
      --  GPDMA_SECCFGR.SECx = 0. 	When GPDMA_SECCFGR.SECx is de-asserted,
      --  this SSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the GPDMA transfer
      --  from the source is non-secure.
      SSEC           : Boolean := False;
      --  binary logarithm of the destination data width of a burst, in bytes
      --  	Note: Setting a 8-byte data width causes a user setting error to be
      --  reported and none transfer is issued. 	A destination burst transfer
      --  must have an aligned address with its data width (start address
      --  GPDMA_CxDAR[2:0] and address offset GPDMA_CxTR3.DAO[2:0], versus
      --  DDW_LOG2[1:0]). Otherwise a user setting error is reported and no
      --  transfer is issued.
      DDW_LOG2       : GPDMA_C11TR1_DDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_18_18 : HAL.Bit := 16#0#;
      --  destination incrementing burst 	The destination address, pointed by
      --  GPDMA_CxDAR, is kept constant after a burst beat/single transfer, or
      --  is incremented by the offset value corresponding to a contiguous data
      --  after a burst beat/single transfer.
      DINC           : Boolean := False;
      --  destination burst length minus 1, between 0 and 63 	The burst length
      --  unit is one data named beat within a burst. If DBL_1[5:0] =0 , the
      --  burst can be named as single. Each data/beat has a width defined by
      --  the destination data width DDW_LOG2[1:0]. 	Note: If a burst transfer
      --  crossed a 1-Kbyte address boundary on a AHB transfer, the GPDMA
      --  modifies and shortens the programmed burst into singles or bursts of
      --  lower length, to be compliant with the AHB protocol. 	If a burst
      --  transfer is of length greater than the FIFO size of the channel x,
      --  the GPDMA modifies and shortens the programmed burst into singles or
      --  bursts of lower length, to be compliant with the FIFO size. Transfer
      --  performance is lower, with GPDMA re-arbitration between effective and
      --  lower bursts/singles, but the data integrity is guaranteed.
      DBL_1          : GPDMA_C11TR1_DBL_1_Field := 16#0#;
      --  destination byte exchange 	If the destination data size is a byte,
      --  this bit is ignored. 	If the destination data size is not a byte:
      DBX            : Boolean := False;
      --  destination half-word exchange 	If the destination data size is
      --  shorter than a word, this bit is ignored. 	If the destination data
      --  size is a word:
      DHX            : Boolean := False;
      --  unspecified
      Reserved_28_29 : HAL.UInt2 := 16#0#;
      --  destination allocated port 	This bit is used to allocate the master
      --  port for the destination transfer 	Note: This bit must be written
      --  when EN = 0. This bit is read-only when EN = 1.
      DAP            : Boolean := False;
      --  security attribute of the GPDMA transfer to the destination 	If
      --  GPDMA_SECCFGR.SECx = 1 and the access is secure: 	This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  GPDMA_SECCFGR.SECx = 1. A secure write is ignored when
      --  GPDMA_SECCFGR.SECx = 0. 	When GPDMA_SECCFGR.SECx is de-asserted,
      --  this DSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the GPDMA transfer
      --  to the destination is non-secure.
      DSEC           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C11TR1_Register use record
      SDW_LOG2       at 0 range 0 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      SINC           at 0 range 3 .. 3;
      SBL_1          at 0 range 4 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      PAM            at 0 range 11 .. 12;
      SBX            at 0 range 13 .. 13;
      SAP            at 0 range 14 .. 14;
      SSEC           at 0 range 15 .. 15;
      DDW_LOG2       at 0 range 16 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      DINC           at 0 range 19 .. 19;
      DBL_1          at 0 range 20 .. 25;
      DBX            at 0 range 26 .. 26;
      DHX            at 0 range 27 .. 27;
      Reserved_28_29 at 0 range 28 .. 29;
      DAP            at 0 range 30 .. 30;
      DSEC           at 0 range 31 .. 31;
   end record;

   subtype GPDMA_C11TR2_REQSEL_Field is HAL.UInt7;
   subtype GPDMA_C11TR2_TRIGM_Field is HAL.UInt2;
   subtype GPDMA_C11TR2_TRIGSEL_Field is HAL.UInt6;
   subtype GPDMA_C11TR2_TRIGPOL_Field is HAL.UInt2;
   subtype GPDMA_C11TR2_TCEM_Field is HAL.UInt2;

   --  GPDMA channel 11 transfer register 2
   type GPDMA_C11TR2_Register is record
      --  GPDMA hardware request selection 	These bits are ignored if channel x
      --  is activated (GPDMA_CxCR.EN asserted) with SWREQ = 1 (software
      --  request for a memory-to-memory transfer). Else, the selected hardware
      --  request is internally taken into account as per . 	The user must not
      --  assign a same input hardware request (same REQSEL[6:0] value) to
      --  different active GPDMA channels (GPDMA_CxCR.EN = 1 and
      --  GPDMA_CxTR2.SWREQ = 0 for these channels). GPDMA is not intended to
      --  hardware support the case of simultaneous enabled channels
      --  incorrectly configured with a same hardware peripheral request
      --  signal, and there is no user setting error reporting.
      REQSEL         : GPDMA_C11TR2_REQSEL_Field := 16#0#;
      --  unspecified
      Reserved_7_8   : HAL.UInt2 := 16#0#;
      --  software request 	This bit is internally taken into account when
      --  GPDMA_CxCR.EN is asserted.
      SWREQ          : Boolean := False;
      --  destination hardware request 	This bit is ignored if channel x is
      --  activated (GPDMA_CxCR.EN asserted) with SWREQ = 1 (software request
      --  for a memory-to-memory transfer). Else: 	Note:
      DREQ           : Boolean := False;
      --  Block hardware request 	If the channel x is activated (GPDMA_CxCR.EN
      --  asserted) with SWREQ = 1 (software request for a memory-to-memory
      --  transfer), this bit is ignored. Else:
      BREQ           : Boolean := False;
      --  unspecified
      Reserved_12_13 : HAL.UInt2 := 16#0#;
      --  trigger mode 	These bits define the transfer granularity for its
      --  conditioning by the trigger. 	If the channel x is enabled
      --  (GPDMA_CxCR.EN asserted) with TRIGPOL[1:0] = 00 or 11, these
      --  TRIGM[1:0] bits are ignored. 	Else, a GPDMA transfer is conditioned
      --  by at least one trigger hit: 	first burst read of a 2D/repeated block
      --  transfer is conditioned by one hit trigger. 	– If the peripheral is
      --  programmed as a source (DREQ = 0) of the LLI data transfer, each
      --  programmed burst read is conditioned. 	– If the peripheral is
      --  programmed as a destination (DREQ = 1) of the LLI data transfer,
      --  each programmed burst write is conditioned. The first memory burst
      --  read of a (possibly 2D/repeated) block, also named as the first ready
      --  FIFO-based source burst, is gated by the occurrence of both the
      --  hardware request and the first trigger hit. 	The GPDMA monitoring of
      --  a trigger for channel x is started when the channel is enabled/loaded
      --  with a new active trigger configuration: rising or falling edge on a
      --  selected trigger (TRIGPOL[1:0] = 01 or respectively
      --  TRIGPOL[1:0] = 10). 	The monitoring of this trigger is kept active
      --  during the triggered and uncompleted (data or link) transfer; and if
      --  a new trigger is detected then, this hit is internally memorized to
      --  grant the next transfer, as long as the defined rising or falling
      --  edge is not modified, and the TRIGSEL[5:0] is not modified, and the
      --  channel is enabled. 	Transferring a next LLIn+1 that updates the
      --  GPDMA_CxTR2 with a new value for any of TRIGSEL[5:0] or TRIGPOL[1:0],
      --  resets the monitoring, trashing the memorized hit of the formerly
      --  defined LLIn trigger. 	After a first new trigger hitn+1 is memorized,
      --  if another second trigger hitn+2 is detected and if the hitn
      --  triggered transfer is still not completed, hitn+2 is lost and not
      --  memorized.memorized. A trigger overrun flag is reported
      --  (GPDMA_CxSR.TOF =1 ), and an interrupt is generated if enabled
      --  (GPDMA_CxCR.TOIE = 1). The channel is not automatically disabled by
      --  hardware due to a trigger overrun. 	Note: When the source block size
      --  is not a multiple of the source burst size and is a multiple of the
      --  source data width, then the last programmed source burst is not
      --  completed and is internally shorten to match the block size. In this
      --  case, if TRIGM[1:0] = 11 and (SWREQ =1  or (SWREQ = 0 and
      --  DREQ =0 )), the shortened burst transfer (by singles or/and by
      --  bursts of lower length) is conditioned once by the trigger. 	When the
      --  programmed destination burst is internally shortened by singles
      --  or/and by bursts of lower length (versus FIFO size, versus block
      --  size, 1-Kbyte boundary address crossing): if the trigger is
      --  conditioning the programmed destination burst (if TRIGM[1:0] = 11
      --  and SWREQ = 0 and DREQ = 1), this shortened destination burst
      --  transfer is conditioned once by the trigger.
      TRIGM          : GPDMA_C11TR2_TRIGM_Field := 16#0#;
      --  trigger event input selection 	These bits select the trigger event
      --  input of the GPDMA transfer (as per ), with an active trigger event
      --  if TRIGPOL[1:0] ≠ 00.
      TRIGSEL        : GPDMA_C11TR2_TRIGSEL_Field := 16#0#;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  trigger event polarity 	These bits define the polarity of the
      --  selected trigger event input defined by TRIGSEL[5:0].
      TRIGPOL        : GPDMA_C11TR2_TRIGPOL_Field := 16#0#;
      --  unspecified
      Reserved_26_29 : HAL.UInt4 := 16#0#;
      --  transfer complete event mode 	These bits define the transfer
      --  granularity for the transfer complete and half transfer complete
      --  events generation. 	Note: If the initial LLI0 data transfer is
      --  null/void (directly programmed by the internal register file with
      --  GPDMA_CxBR1.BNDT[15:0] = 0), then neither the complete transfer
      --  event nor the half transfer event is generated. 	Note: If the initial
      --  LLI0 data transfer is null/void (directly programmed by the internal
      --  register file with GPDMA_CxBR1.BNDT[15:0] = 0), then neither the
      --  complete transfer event nor the half transfer event is generated.
      --  	Note: If the initial LLI0 data transfer is null/void (i.e. directly
      --  programmed by the internal register file with
      --  GPDMA_CxBR1.BNDT[15:0] =0 ), then the half transfer event is not
      --  generated, and the transfer complete event is generated when is
      --  completed the loading of the LLI1.
      TCEM           : GPDMA_C11TR2_TCEM_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C11TR2_Register use record
      REQSEL         at 0 range 0 .. 6;
      Reserved_7_8   at 0 range 7 .. 8;
      SWREQ          at 0 range 9 .. 9;
      DREQ           at 0 range 10 .. 10;
      BREQ           at 0 range 11 .. 11;
      Reserved_12_13 at 0 range 12 .. 13;
      TRIGM          at 0 range 14 .. 15;
      TRIGSEL        at 0 range 16 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      TRIGPOL        at 0 range 24 .. 25;
      Reserved_26_29 at 0 range 26 .. 29;
      TCEM           at 0 range 30 .. 31;
   end record;

   subtype GPDMA_C11BR1_BNDT_Field is HAL.UInt16;

   --  GPDMA channel 11 block register 1
   type GPDMA_C11BR1_Register is record
      --  block number of data bytes to transfer from the source 	Block size
      --  transferred from the source. When the channel is enabled, this field
      --  becomes read-only and is decremented, indicating the remaining number
      --  of data items in the current source block to be transferred.
      --  BNDT[15:0] is programmed in number of bytes, maximum source block
      --  size is 64 Kbytes -1. 	Once the last data transfer is completed
      --  (BNDT[15:0] = 0): 	- if GPDMA_CxLLR.UB1 = 1, this field is
      --  updated by the LLI in the memory. 	- if GPDMA_CxLLR.UB1 = 0 and if
      --  there is at least one non null Uxx update bit, this field is
      --  internally restored to the programmed value. 	- if all
      --  GPDMA_CxLLR.Uxx = 0 and if GPDMA_CxLLR.LA[15:0] = 0, this field
      --  is internally restored to the programmed value (infinite/continuous
      --  last LLI). 	- if GPDMA_CxLLR = 0, this field is kept as zero
      --  following the last LLI data transfer. 	Note: A non-null source block
      --  size must be a multiple of the source data width (BNDT[2:0] versus
      --  GPDMA_CxTR1.SDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued. 	When configured in packing mode
      --  (GPDMA_CxTR1.PAM[1] = 1 and destination data width different from
      --  source data width), a non-null source block size must be a multiple
      --  of the destination data width (BNDT[2:0] versus
      --  GPDMA_CxTR1.DDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued.
      BNDT           : GPDMA_C11BR1_BNDT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C11BR1_Register use record
      BNDT           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype GPDMA_C11LLR_LA_Field is HAL.UInt14;

   --  GPDMA channel 11 linked-list address register
   type GPDMA_C11LLR_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  pointer (16-bit low-significant address) to the next linked-list data
      --  structure 	If UT1 = UT2 = UB1 = USA = UDA = ULL = 0 and
      --  if LA[15:20] = 0, the current LLI is the last one. The channel
      --  transfer is completed without any update of the linked-list GPDMA
      --  register file. 	Else, this field is the pointer to the memory address
      --  offset from which the next linked-list data structure is
      --  automatically fetched from, once the data transfer is completed, in
      --  order to conditionally update the linked-list GPDMA internal register
      --  file (GPDMA_CxCTR1, GPDMA_CxTR2, GPDMA_CxBR1, GPDMA_CxSAR,
      --  GPDMA_CxDAR and GPDMA_CxLLR). 	Note: The user must program the
      --  pointer to be 32-bit aligned. The two low-significant bits are write
      --  ignored.
      LA             : GPDMA_C11LLR_LA_Field := 16#0#;
      --  Update GPDMA_CxLLR register from memory 	This bit is used to control
      --  the update of GPDMA_CxLLR from the memory during the link transfer.
      ULL            : Boolean := False;
      --  unspecified
      Reserved_17_26 : HAL.UInt10 := 16#0#;
      --  Update GPDMA_CxDAR register from memory 	This bit is used to control
      --  the update of GPDMA_CxDAR from the memory during the link transfer.
      UDA            : Boolean := False;
      --  update GPDMA_CxSAR from memory 	This bit controls the update of
      --  GPDMA_CxSAR from the memory during the link transfer.
      USA            : Boolean := False;
      --  Update GPDMA_CxBR1 from memory 	This bit controls the update of
      --  GPDMA_CxBR1 from the memory during the link transfer. If UB1 = 0
      --  and if GPDMA_CxLLR ≠ 0, the linked-list is not completed.
      --  GPDMA_CxBR1.BNDT[15:0] is then restored to the programmed value after
      --  data transfer is completed and before the link transfer.
      UB1            : Boolean := False;
      --  Update GPDMA_CxTR2 from memory 	This bit controls the update of
      --  GPDMA_CxTR2 from the memory during the link transfer.
      UT2            : Boolean := False;
      --  Update GPDMA_CxTR1 from memory 	This bit controls the update of
      --  GPDMA_CxTR1 from the memory during the link transfer.
      UT1            : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C11LLR_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      LA             at 0 range 2 .. 15;
      ULL            at 0 range 16 .. 16;
      Reserved_17_26 at 0 range 17 .. 26;
      UDA            at 0 range 27 .. 27;
      USA            at 0 range 28 .. 28;
      UB1            at 0 range 29 .. 29;
      UT2            at 0 range 30 .. 30;
      UT1            at 0 range 31 .. 31;
   end record;

   subtype GPDMA_C12LBAR_LBA_Field is HAL.UInt16;

   --  GPDMA channel 12 linked-list base address register
   type GPDMA_C12LBAR_Register is record
      --  unspecified
      Reserved_0_15 : HAL.UInt16 := 16#0#;
      --  linked-list base address of GPDMA channel x
      LBA           : GPDMA_C12LBAR_LBA_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C12LBAR_Register use record
      Reserved_0_15 at 0 range 0 .. 15;
      LBA           at 0 range 16 .. 31;
   end record;

   --  GPDMA channel 12 flag clear register
   type GPDMA_C12FCR_Register is record
      --  unspecified
      Reserved_0_7   : HAL.UInt8 := 16#0#;
      --  Write-only. transfer complete flag clear
      TCF            : Boolean := False;
      --  Write-only. half transfer flag clear
      HTF            : Boolean := False;
      --  Write-only. data transfer error flag clear
      DTEF           : Boolean := False;
      --  Write-only. update link transfer error flag clear
      ULEF           : Boolean := False;
      --  Write-only. user setting error flag clear
      USEF           : Boolean := False;
      --  Write-only. completed suspension flag clear
      SUSPF          : Boolean := False;
      --  Write-only. trigger overrun flag clear
      TOF            : Boolean := False;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C12FCR_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      TCF            at 0 range 8 .. 8;
      HTF            at 0 range 9 .. 9;
      DTEF           at 0 range 10 .. 10;
      ULEF           at 0 range 11 .. 11;
      USEF           at 0 range 12 .. 12;
      SUSPF          at 0 range 13 .. 13;
      TOF            at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype GPDMA_C12SR_FIFOL_Field is HAL.UInt8;

   --  GPDMA channel 12 status register
   type GPDMA_C12SR_Register is record
      --  Read-only. idle flag 	This idle flag is de-asserted by hardware when
      --  the channel is enabled (GPDMA_CxCR.EN = 1) with a valid channel
      --  configuration (no USEF to be immediately reported). 	This idle flag
      --  is asserted after hard reset or by hardware when the channel is back
      --  in idle state (in suspended or disabled state).
      IDLEF          : Boolean;
      --  unspecified
      Reserved_1_7   : HAL.UInt7;
      --  Read-only. transfer complete flag 	A transfer complete event is
      --  either a block transfer complete, a 2D/repeated block transfer
      --  complete, a LLI transfer complete including the upload of the next
      --  LLI if any, or the full linked-list completion, depending on the
      --  transfer complete event mode (GPDMA_CxTR2.TCEM[1:0]).
      TCF            : Boolean;
      --  Read-only. half transfer flag 	An half transfer event is either an
      --  half block transfer or an half 2D/repeated block transfer, depending
      --  on the transfer complete event mode (GPDMA_CxTR2.TCEM[1:0]). 	An half
      --  block transfer occurs when half of the bytes of the source block size
      --  (rounded up integer of GPDMA_CxBR1.BNDT[15:0]/2) has been transferred
      --  to the destination. 	An half 2D/repeated block transfer occurs when
      --  half of the repeated blocks (rounded up integer of
      --  (GPDMA_CxBR1.BRC[10:0]+1)/2)) has been transferred to the
      --  destination.
      HTF            : Boolean;
      --  Read-only. data transfer error flag
      DTEF           : Boolean;
      --  Read-only. update link transfer error flag
      ULEF           : Boolean;
      --  Read-only. user setting error flag
      USEF           : Boolean;
      --  Read-only. completed suspension flag
      SUSPF          : Boolean;
      --  Read-only. trigger overrun flag
      TOF            : Boolean;
      --  unspecified
      Reserved_15_15 : HAL.Bit;
      --  Read-only. monitored FIFO level 	Number of available write beats in
      --  the FIFO, in units of the programmed destination data width (see
      --  GPDMA_CxTR1.DDW_LOG2[1:0], in units of bytes, half-words, or words).
      --  	Note: After having suspended an active transfer, the user may need
      --  to read FIFOL[7:0], additionally to GPDMA_CxBR1.BDNT[15:0] and
      --  GPDMA_CxBR1.BRC[10:0], to know how many data have been transferred to
      --  the destination. Before reading, the user may wait for the transfer
      --  to be suspended (GPDMA_CxSR.SUSPF = 1).
      FIFOL          : GPDMA_C12SR_FIFOL_Field;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C12SR_Register use record
      IDLEF          at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      TCF            at 0 range 8 .. 8;
      HTF            at 0 range 9 .. 9;
      DTEF           at 0 range 10 .. 10;
      ULEF           at 0 range 11 .. 11;
      USEF           at 0 range 12 .. 12;
      SUSPF          at 0 range 13 .. 13;
      TOF            at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      FIFOL          at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype GPDMA_C12CR_PRIO_Field is HAL.UInt2;

   --  GPDMA channel 12 control register
   type GPDMA_C12CR_Register is record
      --  enable 	Writing 1 into the field RESET (bit 1) causes the hardware to
      --  de-assert this bit, whatever is written into this bit 0. Else: 	this
      --  bit is de-asserted by hardware when there is a transfer error (master
      --  bus error or user setting error) or when there is a channel transfer
      --  complete (channel ready to be configured, e.g. if LSM=1 at the end of
      --  a single execution of the LLI). 	Else, this bit can be asserted by
      --  software. 	Writing 0 into this EN bit is ignored.
      EN             : Boolean := False;
      --  Write-only. reset 	This bit is write only. Writing 0 has no impact.
      --  Writing 1 implies the reset of the following: the FIFO, the channel
      --  internal state, SUSP and EN bits (whatever is written receptively in
      --  bit 2 and bit 0). 	The reset is effective when the channel is in
      --  steady state, meaning one of the following: 	- active channel in
      --  suspended state (GPDMA_CxSR.SUSPF = 1 and
      --  GPDMA_CxSR.IDLEF = GPDMA_CxCR.EN = 1) 	- channel in disabled
      --  state (GPDMA_CxSR.IDLEF = 1 and GPDMA_CxCR.EN = 0). 	After
      --  writing a RESET, to continue using this channel, the user must
      --  explicitly reconfigure the channel including the hardware-modified
      --  configuration registers (GPDMA_CxBR1, GPDMA_CxSAR and GPDMA_CxDAR)
      --  before enabling again the channel (see the programming sequence in ).
      RESET          : Boolean := False;
      --  suspend 	Writing 1 into the field RESET (bit 1) causes the hardware
      --  to de-assert this bit, whatever is written into this bit 2. Else:
      --  	Software must write 1 in order to suspend an active channel i.e. a
      --  channel with an on-going GPDMA transfer over its master ports. 	The
      --  software must write 0 in order to resume a suspended channel,
      --  following the programming sequence detailed in .
      SUSP           : Boolean := False;
      --  unspecified
      Reserved_3_7   : HAL.UInt5 := 16#0#;
      --  transfer complete interrupt enable
      TCIE           : Boolean := False;
      --  half transfer complete interrupt enable
      HTIE           : Boolean := False;
      --  data transfer error interrupt enable
      DTEIE          : Boolean := False;
      --  update link transfer error interrupt enable
      ULEIE          : Boolean := False;
      --  user setting error interrupt enable
      USEIE          : Boolean := False;
      --  completed suspension interrupt enable
      SUSPIE         : Boolean := False;
      --  trigger overrun interrupt enable
      TOIE           : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Link step mode 	First the (possible 1D/repeated) block transfer is
      --  executed as defined by the current internal register file until
      --  GPDMA_CxBR1.BNDT[15:0] = 0 and GPDMA_CxBR1.BRC[10:0] = 0 if
      --  present. Secondly the next linked-list data structure is
      --  conditionally uploaded from memory as defined by GPDMA_CxLLR. Then
      --  channel execution is completed. 	Note: This bit must be written when
      --  EN=0. This bit is read-only when EN=1.
      LSM            : Boolean := False;
      --  linked-list allocated port 	This bit is used to allocate the master
      --  port for the update of the GPDMA linked-list registers from the
      --  memory. 	Note: This bit must be written when EN=0. This bit is
      --  read-only when EN=1.
      LAP            : Boolean := False;
      --  unspecified
      Reserved_18_21 : HAL.UInt4 := 16#0#;
      --  priority level of the channel x GPDMA transfer versus others 	Note:
      --  This bit must be written when EN = 0. This bit is read-only when
      --  EN = 1.
      PRIO           : GPDMA_C12CR_PRIO_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C12CR_Register use record
      EN             at 0 range 0 .. 0;
      RESET          at 0 range 1 .. 1;
      SUSP           at 0 range 2 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      TCIE           at 0 range 8 .. 8;
      HTIE           at 0 range 9 .. 9;
      DTEIE          at 0 range 10 .. 10;
      ULEIE          at 0 range 11 .. 11;
      USEIE          at 0 range 12 .. 12;
      SUSPIE         at 0 range 13 .. 13;
      TOIE           at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      LSM            at 0 range 16 .. 16;
      LAP            at 0 range 17 .. 17;
      Reserved_18_21 at 0 range 18 .. 21;
      PRIO           at 0 range 22 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype GPDMA_C12TR1_SDW_LOG2_Field is HAL.UInt2;
   subtype GPDMA_C12TR1_SBL_1_Field is HAL.UInt6;
   subtype GPDMA_C12TR1_PAM_Field is HAL.UInt2;
   subtype GPDMA_C12TR1_DDW_LOG2_Field is HAL.UInt2;
   subtype GPDMA_C12TR1_DBL_1_Field is HAL.UInt6;

   --  GPDMA channel 12 transfer register 1
   type GPDMA_C12TR1_Register is record
      --  binary logarithm of the source data width of a burst in bytes 	Note:
      --  Setting a 8-byte data width causes a user setting error to be
      --  reported and no transfer is issued. 	A source block size must be a
      --  multiple of the source data width (GPDMA_CxBR1.BNDT[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and no
      --  transfer is issued. 	A source single transfer must have an aligned
      --  address with its data width (start address GPDMA_CxSAR[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and none
      --  transfer is issued.
      SDW_LOG2       : GPDMA_C12TR1_SDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  source incrementing burst 	The source address, pointed by
      --  GPDMA_CxSAR, is kept constant after a burst beat/single transfer or
      --  is incremented by the offset value corresponding to a contiguous data
      --  after a burst beat/single transfer.
      SINC           : Boolean := False;
      --  source burst length minus 1, between 0 and 63 	The burst length unit
      --  is one data named beat within a burst. If SBL_1[5:0] =0 , the burst
      --  can be named as single. Each data/beat has a width defined by the
      --  destination data width SDW_LOG2[1:0]. 	Note: If a burst transfer
      --  crossed a 1-Kbyte address boundary on a AHB transfer, the GPDMA
      --  modifies and shortens the programmed burst into singles or bursts of
      --  lower length, to be compliant with the AHB protocol. 	If a burst
      --  transfer is of length greater than the FIFO size of the channel x,
      --  the GPDMA modifies and shortens the programmed burst into singles or
      --  bursts of lower length, to be compliant with the FIFO size. Transfer
      --  performance is lower, with GPDMA re-arbitration between effective and
      --  lower bursts/singles, but the data integrity is guaranteed.
      SBL_1          : GPDMA_C12TR1_SBL_1_Field := 16#0#;
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  padding/alignment mode 	If DDW_LOG2[1:0] = SDW_LOG2[1:0]: if the data
      --  width of a burst destination transfer is equal to the data width of a
      --  burst source transfer, these bits are ignored. 	Else: 	- Case 1: If
      --  destination data width > source data width 	1x: successive source
      --  data are FIFO queued and packed at the destination data width, in a
      --  left (LSB) to right (MSB) order (named little endian), before a
      --  destination transfer 	- Case 2: If destination data width < source
      --  data width 	1x: source data is FIFO queued and unpacked at the
      --  destination data width, to be transferred in a left (LSB) to right
      --  (MSB) order (named little endian) to the destination 	Note:
      PAM            : GPDMA_C12TR1_PAM_Field := 16#0#;
      --  source byte exchange within the unaligned half-word of each source
      --  word 	If the source data width is shorter than a word, this bit is
      --  ignored. 	If the source data width is a word:
      SBX            : Boolean := False;
      --  source allocated port 	This bit is used to allocate the master port
      --  for the source transfer 	Note: This bit must be written when
      --  EN = 0. This bit is read-only when EN = 1.
      SAP            : Boolean := False;
      --  security attribute of the GPDMA transfer from the source 	If
      --  GPDMA_SECCFGR.SECx = 1 and the access is secure: 	This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  GPDMA_SECCFGR.SECx =1 . A secure write is ignored when
      --  GPDMA_SECCFGR.SECx = 0. 	When GPDMA_SECCFGR.SECx is de-asserted,
      --  this SSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the GPDMA transfer
      --  from the source is non-secure.
      SSEC           : Boolean := False;
      --  binary logarithm of the destination data width of a burst, in bytes
      --  	Note: Setting a 8-byte data width causes a user setting error to be
      --  reported and none transfer is issued. 	A destination burst transfer
      --  must have an aligned address with its data width (start address
      --  GPDMA_CxDAR[2:0] and address offset GPDMA_CxTR3.DAO[2:0], versus
      --  DDW_LOG2[1:0]). Otherwise a user setting error is reported and no
      --  transfer is issued.
      DDW_LOG2       : GPDMA_C12TR1_DDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_18_18 : HAL.Bit := 16#0#;
      --  destination incrementing burst 	The destination address, pointed by
      --  GPDMA_CxDAR, is kept constant after a burst beat/single transfer, or
      --  is incremented by the offset value corresponding to a contiguous data
      --  after a burst beat/single transfer.
      DINC           : Boolean := False;
      --  destination burst length minus 1, between 0 and 63 	The burst length
      --  unit is one data named beat within a burst. If DBL_1[5:0] =0 , the
      --  burst can be named as single. Each data/beat has a width defined by
      --  the destination data width DDW_LOG2[1:0]. 	Note: If a burst transfer
      --  crossed a 1-Kbyte address boundary on a AHB transfer, the GPDMA
      --  modifies and shortens the programmed burst into singles or bursts of
      --  lower length, to be compliant with the AHB protocol. 	If a burst
      --  transfer is of length greater than the FIFO size of the channel x,
      --  the GPDMA modifies and shortens the programmed burst into singles or
      --  bursts of lower length, to be compliant with the FIFO size. Transfer
      --  performance is lower, with GPDMA re-arbitration between effective and
      --  lower bursts/singles, but the data integrity is guaranteed.
      DBL_1          : GPDMA_C12TR1_DBL_1_Field := 16#0#;
      --  destination byte exchange 	If the destination data size is a byte,
      --  this bit is ignored. 	If the destination data size is not a byte:
      DBX            : Boolean := False;
      --  destination half-word exchange 	If the destination data size is
      --  shorter than a word, this bit is ignored. 	If the destination data
      --  size is a word:
      DHX            : Boolean := False;
      --  unspecified
      Reserved_28_29 : HAL.UInt2 := 16#0#;
      --  destination allocated port 	This bit is used to allocate the master
      --  port for the destination transfer 	Note: This bit must be written
      --  when EN = 0. This bit is read-only when EN = 1.
      DAP            : Boolean := False;
      --  security attribute of the GPDMA transfer to the destination 	If
      --  GPDMA_SECCFGR.SECx = 1 and the access is secure: 	This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  GPDMA_SECCFGR.SECx = 1. A secure write is ignored when
      --  GPDMA_SECCFGR.SECx = 0. 	When GPDMA_SECCFGR.SECx is de-asserted,
      --  this DSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the GPDMA transfer
      --  to the destination is non-secure.
      DSEC           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C12TR1_Register use record
      SDW_LOG2       at 0 range 0 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      SINC           at 0 range 3 .. 3;
      SBL_1          at 0 range 4 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      PAM            at 0 range 11 .. 12;
      SBX            at 0 range 13 .. 13;
      SAP            at 0 range 14 .. 14;
      SSEC           at 0 range 15 .. 15;
      DDW_LOG2       at 0 range 16 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      DINC           at 0 range 19 .. 19;
      DBL_1          at 0 range 20 .. 25;
      DBX            at 0 range 26 .. 26;
      DHX            at 0 range 27 .. 27;
      Reserved_28_29 at 0 range 28 .. 29;
      DAP            at 0 range 30 .. 30;
      DSEC           at 0 range 31 .. 31;
   end record;

   subtype GPDMA_C12TR2_REQSEL_Field is HAL.UInt7;
   subtype GPDMA_C12TR2_TRIGM_Field is HAL.UInt2;
   subtype GPDMA_C12TR2_TRIGSEL_Field is HAL.UInt6;
   subtype GPDMA_C12TR2_TRIGPOL_Field is HAL.UInt2;
   subtype GPDMA_C12TR2_TCEM_Field is HAL.UInt2;

   --  GPDMA channel 12 transfer register 2
   type GPDMA_C12TR2_Register is record
      --  GPDMA hardware request selection 	These bits are ignored if channel x
      --  is activated (GPDMA_CxCR.EN asserted) with SWREQ = 1 (software
      --  request for a memory-to-memory transfer). Else, the selected hardware
      --  request is internally taken into account as per . 	The user must not
      --  assign a same input hardware request (same REQSEL[6:0] value) to
      --  different active GPDMA channels (GPDMA_CxCR.EN = 1 and
      --  GPDMA_CxTR2.SWREQ = 0 for these channels). GPDMA is not intended to
      --  hardware support the case of simultaneous enabled channels
      --  incorrectly configured with a same hardware peripheral request
      --  signal, and there is no user setting error reporting.
      REQSEL         : GPDMA_C12TR2_REQSEL_Field := 16#0#;
      --  unspecified
      Reserved_7_8   : HAL.UInt2 := 16#0#;
      --  software request 	This bit is internally taken into account when
      --  GPDMA_CxCR.EN is asserted.
      SWREQ          : Boolean := False;
      --  destination hardware request 	This bit is ignored if channel x is
      --  activated (GPDMA_CxCR.EN asserted) with SWREQ = 1 (software request
      --  for a memory-to-memory transfer). Else: 	Note:
      DREQ           : Boolean := False;
      --  Block hardware request 	If the channel x is activated (GPDMA_CxCR.EN
      --  asserted) with SWREQ = 1 (software request for a memory-to-memory
      --  transfer), this bit is ignored. Else:
      BREQ           : Boolean := False;
      --  unspecified
      Reserved_12_13 : HAL.UInt2 := 16#0#;
      --  trigger mode 	These bits define the transfer granularity for its
      --  conditioning by the trigger. 	If the channel x is enabled
      --  (GPDMA_CxCR.EN asserted) with TRIGPOL[1:0] = 00 or 11, these
      --  TRIGM[1:0] bits are ignored. 	Else, a GPDMA transfer is conditioned
      --  by at least one trigger hit: 	first burst read of a 2D/repeated block
      --  transfer is conditioned by one hit trigger. 	– If the peripheral is
      --  programmed as a source (DREQ = 0) of the LLI data transfer, each
      --  programmed burst read is conditioned. 	– If the peripheral is
      --  programmed as a destination (DREQ = 1) of the LLI data transfer,
      --  each programmed burst write is conditioned. The first memory burst
      --  read of a (possibly 2D/repeated) block, also named as the first ready
      --  FIFO-based source burst, is gated by the occurrence of both the
      --  hardware request and the first trigger hit. 	The GPDMA monitoring of
      --  a trigger for channel x is started when the channel is enabled/loaded
      --  with a new active trigger configuration: rising or falling edge on a
      --  selected trigger (TRIGPOL[1:0] = 01 or respectively
      --  TRIGPOL[1:0] = 10). 	The monitoring of this trigger is kept active
      --  during the triggered and uncompleted (data or link) transfer; and if
      --  a new trigger is detected then, this hit is internally memorized to
      --  grant the next transfer, as long as the defined rising or falling
      --  edge is not modified, and the TRIGSEL[5:0] is not modified, and the
      --  channel is enabled. 	Transferring a next LLIn+1 that updates the
      --  GPDMA_CxTR2 with a new value for any of TRIGSEL[5:0] or TRIGPOL[1:0],
      --  resets the monitoring, trashing the memorized hit of the formerly
      --  defined LLIn trigger. 	After a first new trigger hitn+1 is memorized,
      --  if another second trigger hitn+2 is detected and if the hitn
      --  triggered transfer is still not completed, hitn+2 is lost and not
      --  memorized.memorized. A trigger overrun flag is reported
      --  (GPDMA_CxSR.TOF =1 ), and an interrupt is generated if enabled
      --  (GPDMA_CxCR.TOIE = 1). The channel is not automatically disabled by
      --  hardware due to a trigger overrun. 	Note: When the source block size
      --  is not a multiple of the source burst size and is a multiple of the
      --  source data width, then the last programmed source burst is not
      --  completed and is internally shorten to match the block size. In this
      --  case, if TRIGM[1:0] = 11 and (SWREQ =1  or (SWREQ = 0 and
      --  DREQ =0 )), the shortened burst transfer (by singles or/and by
      --  bursts of lower length) is conditioned once by the trigger. 	When the
      --  programmed destination burst is internally shortened by singles
      --  or/and by bursts of lower length (versus FIFO size, versus block
      --  size, 1-Kbyte boundary address crossing): if the trigger is
      --  conditioning the programmed destination burst (if TRIGM[1:0] = 11
      --  and SWREQ = 0 and DREQ = 1), this shortened destination burst
      --  transfer is conditioned once by the trigger.
      TRIGM          : GPDMA_C12TR2_TRIGM_Field := 16#0#;
      --  trigger event input selection 	These bits select the trigger event
      --  input of the GPDMA transfer (as per ), with an active trigger event
      --  if TRIGPOL[1:0] ≠ 00.
      TRIGSEL        : GPDMA_C12TR2_TRIGSEL_Field := 16#0#;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  trigger event polarity 	These bits define the polarity of the
      --  selected trigger event input defined by TRIGSEL[5:0].
      TRIGPOL        : GPDMA_C12TR2_TRIGPOL_Field := 16#0#;
      --  unspecified
      Reserved_26_29 : HAL.UInt4 := 16#0#;
      --  transfer complete event mode 	These bits define the transfer
      --  granularity for the transfer complete and half transfer complete
      --  events generation. 	Note: If the initial LLI0 data transfer is
      --  null/void (directly programmed by the internal register file with
      --  GPDMA_CxBR1.BNDT[15:0] = 0), then neither the complete transfer
      --  event nor the half transfer event is generated. 	Note: If the initial
      --  LLI0 data transfer is null/void (directly programmed by the internal
      --  register file with GPDMA_CxBR1.BNDT[15:0] = 0), then neither the
      --  complete transfer event nor the half transfer event is generated.
      --  	Note: If the initial LLI0 data transfer is null/void (i.e. directly
      --  programmed by the internal register file with
      --  GPDMA_CxBR1.BNDT[15:0] =0 ), then the half transfer event is not
      --  generated, and the transfer complete event is generated when is
      --  completed the loading of the LLI1.
      TCEM           : GPDMA_C12TR2_TCEM_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C12TR2_Register use record
      REQSEL         at 0 range 0 .. 6;
      Reserved_7_8   at 0 range 7 .. 8;
      SWREQ          at 0 range 9 .. 9;
      DREQ           at 0 range 10 .. 10;
      BREQ           at 0 range 11 .. 11;
      Reserved_12_13 at 0 range 12 .. 13;
      TRIGM          at 0 range 14 .. 15;
      TRIGSEL        at 0 range 16 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      TRIGPOL        at 0 range 24 .. 25;
      Reserved_26_29 at 0 range 26 .. 29;
      TCEM           at 0 range 30 .. 31;
   end record;

   subtype GPDMA_C12BR1_BNDT_Field is HAL.UInt16;
   subtype GPDMA_C12BR1_BRC_Field is HAL.UInt11;

   --  GPDMA channel 12 alternate block register 1
   type GPDMA_C12BR1_Register is record
      --  block number of data bytes to transfer from the source 	Block size
      --  transferred from the source. When the channel is enabled, this field
      --  becomes read-only and is decremented, indicating the remaining number
      --  of data items in the current source block to be transferred.
      --  BNDT[15:0] is programmed in number of bytes, maximum source block
      --  size is 64 Kbytes -1. 	Once the last data transfer is completed
      --  (BNDT[15:0] = 0): 	- if GPDMA_CxLLR.UB1 = 1, this field is
      --  updated by the LLI in the memory. 	- if GPDMA_CxLLR.UB1 = 0 and if
      --  there is at least one not null Uxx update bit, this field is
      --  internally restored to the programmed value. 	- if all
      --  GPDMA_CxLLR.Uxx = 0 and if GPDMA_CxLLR.LA[15:0] ≠ 0, this field
      --  is internally restored to the programmed value (infinite/continuous
      --  last LLI). 	- if GPDMA_CxLLR = 0, this field is kept as zero
      --  following the last LLI data transfer. 	Note: A non-null source block
      --  size must be a multiple of the source data width (BNDT[2:0] versus
      --  GPDMA_CxTR1.SDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued. 	When configured in packing mode
      --  (GPDMA_CxTR1.PAM[1]=1 and destination data width different from
      --  source data width), a non-null source block size must be a multiple
      --  of the destination data width (BNDT[2:0] versus
      --  GPDMA_CxTR1.DDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued.
      BNDT           : GPDMA_C12BR1_BNDT_Field := 16#0#;
      --  Block repeat counter 	This field contains the number of repetitions
      --  of the current block (0 to 2047). 	When the channel is enabled, this
      --  field becomes read-only. After decrements, this field indicates the
      --  remaining number of blocks, excluding the current one. This counter
      --  is hardware decremented for each completed block transfer. 	Once the
      --  last block transfer is completed (BRC[10:0] = BNDT[15:0] = 0):
      --  	If GPDMA_CxLLR.UB1 = 1, all GPDMA_CxBR1 fields are updated by the
      --  next LLI in the memory. 	If GPDMA_CxLLR.UB1 = 0 and if there is at
      --  least one not null Uxx update bit, this field is internally restored
      --  to the programmed value. 	if all GPDMA_CxLLR.Uxx = 0 and if
      --  GPDMA_CxLLR.LA[15:0] ≠ 0, this field is internally restored to
      --  the programmed value (infinite/continuous last LLI). 	if
      --  GPDMA_CxLLR = 0, this field is kept as zero following the last LLI
      --  and data transfer.
      BRC            : GPDMA_C12BR1_BRC_Field := 16#0#;
      --  unspecified
      Reserved_27_27 : HAL.Bit := 16#0#;
      --  source address decrement
      SDEC           : Boolean := False;
      --  destination address decrement
      DDEC           : Boolean := False;
      --  Block repeat source address decrement 	Note: On top of this
      --  increment/decrement (depending on BRSDEC), GPDMA_CxSAR is in the same
      --  time also updated by the increment/decrement (depending on SDEC) of
      --  the GPDMA_CxTR3.SAO value, as it is done after any programmed burst
      --  transfer.
      BRSDEC         : Boolean := False;
      --  Block repeat destination address decrement 	Note: On top of this
      --  increment/decrement (depending on BRDDEC), GPDMA_CxDAR is in the same
      --  time also updated by the increment/decrement (depending on DDEC) of
      --  the GPDMA_CxTR3.DAO value, as it is usually done at the end of each
      --  programmed burst transfer.
      BRDDEC         : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C12BR1_Register use record
      BNDT           at 0 range 0 .. 15;
      BRC            at 0 range 16 .. 26;
      Reserved_27_27 at 0 range 27 .. 27;
      SDEC           at 0 range 28 .. 28;
      DDEC           at 0 range 29 .. 29;
      BRSDEC         at 0 range 30 .. 30;
      BRDDEC         at 0 range 31 .. 31;
   end record;

   subtype GPDMA_C12TR3_SAO_Field is HAL.UInt13;
   subtype GPDMA_C12TR3_DAO_Field is HAL.UInt13;

   --  GPDMA channel 12 transfer register 3
   type GPDMA_C12TR3_Register is record
      --  source address offset increment 	The source address, pointed by
      --  GPDMA_CxSAR, is incremented or decremented (depending on
      --  GPDMA_CxBR1.SDEC) by this offset SAO[12:0] for each programmed source
      --  burst. This offset is not including and is added to the programmed
      --  burst size when the completed burst is addressed in incremented mode
      --  (GPDMA_CxTR1.SINC = 1). 	Note: A source address offset must be
      --  aligned with the programmed data width of a source burst (SAO[2:0]
      --  versus GPDMA_CxTR1.SDW_LOG2[1:0]). Else a user setting error is
      --  reported and none transfer is issued. 	When the source block size is
      --  not a multiple of the destination burst size and is a multiple of the
      --  source data width, then the last programmed source burst is not
      --  completed and is internally shorten to match the block size. In this
      --  case, the additional GPDMA_CxTR3.SAO[12:0] is not applied.
      SAO            : GPDMA_C12TR3_SAO_Field := 16#0#;
      --  unspecified
      Reserved_13_15 : HAL.UInt3 := 16#0#;
      --  destination address offset increment 	The destination address,
      --  pointed by GPDMA_CxDAR, is incremented or decremented (depending on
      --  GPDMA_CxBR1.DDEC) by this offset DAO[12:0] for each programmed
      --  destination burst. This offset is not including and is added to the
      --  programmed burst size when the completed burst is addressed in
      --  incremented mode (GPDMA_CxTR1.DINC = 1). 	Note: A destination
      --  address offset must be aligned with the programmed data width of a
      --  destination burst (DAO[2:0] versus GPDMA_CxTR1.DDW_LOG2[1:0]). Else,
      --  a user setting error is reported and no transfer is issued.
      DAO            : GPDMA_C12TR3_DAO_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C12TR3_Register use record
      SAO            at 0 range 0 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      DAO            at 0 range 16 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype GPDMA_C12BR2_BRSAO_Field is HAL.UInt16;
   subtype GPDMA_C12BR2_BRDAO_Field is HAL.UInt16;

   --  GPDMA channel 12 block register 2
   type GPDMA_C12BR2_Register is record
      --  Block repeated source address offset 	For a channel with 2D
      --  addressing capability, this field is used to update (by addition or
      --  subtraction depending on GPDMA_CxBR1.BRSDEC) the current source
      --  address (GPDMA_CxSAR) at the end of a block transfer. 	Note: A block
      --  repeated source address offset must be aligned with the programmed
      --  data width of a source burst (BRSAO[2:0] versus
      --  GPDMA_CxTR1.SDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued.
      BRSAO : GPDMA_C12BR2_BRSAO_Field := 16#0#;
      --  Block repeated destination address offset 	For a channel with 2D
      --  addressing capability, this field is used to update (by addition or
      --  subtraction depending on GPDMA_CxBR1.BRDDEC) the current destination
      --  address (GPDMA_CxDAR) at the end of a block transfer. 	Note: A block
      --  repeated destination address offset must be aligned with the
      --  programmed data width of a destination burst (BRDAO[2:0] versus
      --  GPDMA_CxTR1.DDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued.
      BRDAO : GPDMA_C12BR2_BRDAO_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C12BR2_Register use record
      BRSAO at 0 range 0 .. 15;
      BRDAO at 0 range 16 .. 31;
   end record;

   subtype GPDMA_C12LLR_LA_Field is HAL.UInt14;

   --  GPDMA channel 12 alternate linked-list address register
   type GPDMA_C12LLR_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  pointer (16-bit low-significant address) to the next linked-list data
      --  structure 	If UT1 = UT2 = UB1 = USA = UDA = ULL = 0 and
      --  if LA[15:20] = 0, the current LLI is the last one. The channel
      --  transfer is completed without any update of the linked-list GPDMA
      --  register file. 	Else, this field is the pointer to the memory address
      --  offset from which the next linked-list data structure is
      --  automatically fetched from, once the data transfer is completed, in
      --  order to conditionally update the linked-list GPDMA internal register
      --  file (GPDMA_CxCTR1, GPDMA_CxTR2, GPDMA_CxBR1, GPDMA_CxSAR,
      --  GPDMA_CxDAR and GPDMA_CxLLR). 	Note: The user must program the
      --  pointer to be 32-bit aligned. The two low-significant bits are write
      --  ignored.
      LA             : GPDMA_C12LLR_LA_Field := 16#0#;
      --  Update GPDMA_CxLLR register from memory 	This bit is used to control
      --  the update of GPDMA_CxLLR from the memory during the link transfer.
      ULL            : Boolean := False;
      --  unspecified
      Reserved_17_24 : HAL.UInt8 := 16#0#;
      --  Update GPDMA_CxBR2 from memory 	This bit controls the update of
      --  GPDMA_CxBR2 from the memory during the link transfer.
      UB2            : Boolean := False;
      --  Update GPDMA_CxTR3 from memory 	This bit controls the update of
      --  GPDMA_CxTR3 from the memory during the link transfer.
      UT3            : Boolean := False;
      --  Update GPDMA_CxDAR register from memory 	This bit is used to control
      --  the update of GPDMA_CxDAR from the memory during the link transfer.
      UDA            : Boolean := False;
      --  update GPDMA_CxSAR from memory 	This bit controls the update of
      --  GPDMA_CxSAR from the memory during the link transfer.
      USA            : Boolean := False;
      --  Update GPDMA_CxBR1 from memory 	This bit controls the update of
      --  GPDMA_CxBR1 from the memory during the link transfer. If UB1 = 0
      --  and if GPDMA_CxLLR ≠ 0, the linked-list is not completed.
      --  GPDMA_CxBR1.BNDT[15:0] is then restored to the programmed value after
      --  data transfer is completed and before the link transfer.
      UB1            : Boolean := False;
      --  Update GPDMA_CxTR2 from memory 	This bit controls the update of
      --  GPDMA_CxTR2 from the memory during the link transfer.
      UT2            : Boolean := False;
      --  Update GPDMA_CxTR1 from memory 	This bit controls the update of
      --  GPDMA_CxTR1 from the memory during the link transfer.
      UT1            : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C12LLR_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      LA             at 0 range 2 .. 15;
      ULL            at 0 range 16 .. 16;
      Reserved_17_24 at 0 range 17 .. 24;
      UB2            at 0 range 25 .. 25;
      UT3            at 0 range 26 .. 26;
      UDA            at 0 range 27 .. 27;
      USA            at 0 range 28 .. 28;
      UB1            at 0 range 29 .. 29;
      UT2            at 0 range 30 .. 30;
      UT1            at 0 range 31 .. 31;
   end record;

   subtype GPDMA_C13LBAR_LBA_Field is HAL.UInt16;

   --  GPDMA channel 13 linked-list base address register
   type GPDMA_C13LBAR_Register is record
      --  unspecified
      Reserved_0_15 : HAL.UInt16 := 16#0#;
      --  linked-list base address of GPDMA channel x
      LBA           : GPDMA_C13LBAR_LBA_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C13LBAR_Register use record
      Reserved_0_15 at 0 range 0 .. 15;
      LBA           at 0 range 16 .. 31;
   end record;

   --  GPDMA channel 13 flag clear register
   type GPDMA_C13FCR_Register is record
      --  unspecified
      Reserved_0_7   : HAL.UInt8 := 16#0#;
      --  Write-only. transfer complete flag clear
      TCF            : Boolean := False;
      --  Write-only. half transfer flag clear
      HTF            : Boolean := False;
      --  Write-only. data transfer error flag clear
      DTEF           : Boolean := False;
      --  Write-only. update link transfer error flag clear
      ULEF           : Boolean := False;
      --  Write-only. user setting error flag clear
      USEF           : Boolean := False;
      --  Write-only. completed suspension flag clear
      SUSPF          : Boolean := False;
      --  Write-only. trigger overrun flag clear
      TOF            : Boolean := False;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C13FCR_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      TCF            at 0 range 8 .. 8;
      HTF            at 0 range 9 .. 9;
      DTEF           at 0 range 10 .. 10;
      ULEF           at 0 range 11 .. 11;
      USEF           at 0 range 12 .. 12;
      SUSPF          at 0 range 13 .. 13;
      TOF            at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype GPDMA_C13SR_FIFOL_Field is HAL.UInt8;

   --  GPDMA channel 13 status register
   type GPDMA_C13SR_Register is record
      --  Read-only. idle flag 	This idle flag is de-asserted by hardware when
      --  the channel is enabled (GPDMA_CxCR.EN = 1) with a valid channel
      --  configuration (no USEF to be immediately reported). 	This idle flag
      --  is asserted after hard reset or by hardware when the channel is back
      --  in idle state (in suspended or disabled state).
      IDLEF          : Boolean;
      --  unspecified
      Reserved_1_7   : HAL.UInt7;
      --  Read-only. transfer complete flag 	A transfer complete event is
      --  either a block transfer complete, a 2D/repeated block transfer
      --  complete, a LLI transfer complete including the upload of the next
      --  LLI if any, or the full linked-list completion, depending on the
      --  transfer complete event mode (GPDMA_CxTR2.TCEM[1:0]).
      TCF            : Boolean;
      --  Read-only. half transfer flag 	An half transfer event is either an
      --  half block transfer or an half 2D/repeated block transfer, depending
      --  on the transfer complete event mode (GPDMA_CxTR2.TCEM[1:0]). 	An half
      --  block transfer occurs when half of the bytes of the source block size
      --  (rounded up integer of GPDMA_CxBR1.BNDT[15:0]/2) has been transferred
      --  to the destination. 	An half 2D/repeated block transfer occurs when
      --  half of the repeated blocks (rounded up integer of
      --  (GPDMA_CxBR1.BRC[10:0]+1)/2)) has been transferred to the
      --  destination.
      HTF            : Boolean;
      --  Read-only. data transfer error flag
      DTEF           : Boolean;
      --  Read-only. update link transfer error flag
      ULEF           : Boolean;
      --  Read-only. user setting error flag
      USEF           : Boolean;
      --  Read-only. completed suspension flag
      SUSPF          : Boolean;
      --  Read-only. trigger overrun flag
      TOF            : Boolean;
      --  unspecified
      Reserved_15_15 : HAL.Bit;
      --  Read-only. monitored FIFO level 	Number of available write beats in
      --  the FIFO, in units of the programmed destination data width (see
      --  GPDMA_CxTR1.DDW_LOG2[1:0], in units of bytes, half-words, or words).
      --  	Note: After having suspended an active transfer, the user may need
      --  to read FIFOL[7:0], additionally to GPDMA_CxBR1.BDNT[15:0] and
      --  GPDMA_CxBR1.BRC[10:0], to know how many data have been transferred to
      --  the destination. Before reading, the user may wait for the transfer
      --  to be suspended (GPDMA_CxSR.SUSPF = 1).
      FIFOL          : GPDMA_C13SR_FIFOL_Field;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C13SR_Register use record
      IDLEF          at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      TCF            at 0 range 8 .. 8;
      HTF            at 0 range 9 .. 9;
      DTEF           at 0 range 10 .. 10;
      ULEF           at 0 range 11 .. 11;
      USEF           at 0 range 12 .. 12;
      SUSPF          at 0 range 13 .. 13;
      TOF            at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      FIFOL          at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype GPDMA_C13CR_PRIO_Field is HAL.UInt2;

   --  GPDMA channel 13 control register
   type GPDMA_C13CR_Register is record
      --  enable 	Writing 1 into the field RESET (bit 1) causes the hardware to
      --  de-assert this bit, whatever is written into this bit 0. Else: 	this
      --  bit is de-asserted by hardware when there is a transfer error (master
      --  bus error or user setting error) or when there is a channel transfer
      --  complete (channel ready to be configured, e.g. if LSM=1 at the end of
      --  a single execution of the LLI). 	Else, this bit can be asserted by
      --  software. 	Writing 0 into this EN bit is ignored.
      EN             : Boolean := False;
      --  Write-only. reset 	This bit is write only. Writing 0 has no impact.
      --  Writing 1 implies the reset of the following: the FIFO, the channel
      --  internal state, SUSP and EN bits (whatever is written receptively in
      --  bit 2 and bit 0). 	The reset is effective when the channel is in
      --  steady state, meaning one of the following: 	- active channel in
      --  suspended state (GPDMA_CxSR.SUSPF = 1 and
      --  GPDMA_CxSR.IDLEF = GPDMA_CxCR.EN = 1) 	- channel in disabled
      --  state (GPDMA_CxSR.IDLEF = 1 and GPDMA_CxCR.EN = 0). 	After
      --  writing a RESET, to continue using this channel, the user must
      --  explicitly reconfigure the channel including the hardware-modified
      --  configuration registers (GPDMA_CxBR1, GPDMA_CxSAR and GPDMA_CxDAR)
      --  before enabling again the channel (see the programming sequence in ).
      RESET          : Boolean := False;
      --  suspend 	Writing 1 into the field RESET (bit 1) causes the hardware
      --  to de-assert this bit, whatever is written into this bit 2. Else:
      --  	Software must write 1 in order to suspend an active channel i.e. a
      --  channel with an on-going GPDMA transfer over its master ports. 	The
      --  software must write 0 in order to resume a suspended channel,
      --  following the programming sequence detailed in .
      SUSP           : Boolean := False;
      --  unspecified
      Reserved_3_7   : HAL.UInt5 := 16#0#;
      --  transfer complete interrupt enable
      TCIE           : Boolean := False;
      --  half transfer complete interrupt enable
      HTIE           : Boolean := False;
      --  data transfer error interrupt enable
      DTEIE          : Boolean := False;
      --  update link transfer error interrupt enable
      ULEIE          : Boolean := False;
      --  user setting error interrupt enable
      USEIE          : Boolean := False;
      --  completed suspension interrupt enable
      SUSPIE         : Boolean := False;
      --  trigger overrun interrupt enable
      TOIE           : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Link step mode 	First the (possible 1D/repeated) block transfer is
      --  executed as defined by the current internal register file until
      --  GPDMA_CxBR1.BNDT[15:0] = 0 and GPDMA_CxBR1.BRC[10:0] = 0 if
      --  present. Secondly the next linked-list data structure is
      --  conditionally uploaded from memory as defined by GPDMA_CxLLR. Then
      --  channel execution is completed. 	Note: This bit must be written when
      --  EN=0. This bit is read-only when EN=1.
      LSM            : Boolean := False;
      --  linked-list allocated port 	This bit is used to allocate the master
      --  port for the update of the GPDMA linked-list registers from the
      --  memory. 	Note: This bit must be written when EN=0. This bit is
      --  read-only when EN=1.
      LAP            : Boolean := False;
      --  unspecified
      Reserved_18_21 : HAL.UInt4 := 16#0#;
      --  priority level of the channel x GPDMA transfer versus others 	Note:
      --  This bit must be written when EN = 0. This bit is read-only when
      --  EN = 1.
      PRIO           : GPDMA_C13CR_PRIO_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C13CR_Register use record
      EN             at 0 range 0 .. 0;
      RESET          at 0 range 1 .. 1;
      SUSP           at 0 range 2 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      TCIE           at 0 range 8 .. 8;
      HTIE           at 0 range 9 .. 9;
      DTEIE          at 0 range 10 .. 10;
      ULEIE          at 0 range 11 .. 11;
      USEIE          at 0 range 12 .. 12;
      SUSPIE         at 0 range 13 .. 13;
      TOIE           at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      LSM            at 0 range 16 .. 16;
      LAP            at 0 range 17 .. 17;
      Reserved_18_21 at 0 range 18 .. 21;
      PRIO           at 0 range 22 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype GPDMA_C13TR1_SDW_LOG2_Field is HAL.UInt2;
   subtype GPDMA_C13TR1_SBL_1_Field is HAL.UInt6;
   subtype GPDMA_C13TR1_PAM_Field is HAL.UInt2;
   subtype GPDMA_C13TR1_DDW_LOG2_Field is HAL.UInt2;
   subtype GPDMA_C13TR1_DBL_1_Field is HAL.UInt6;

   --  GPDMA channel 13 transfer register 1
   type GPDMA_C13TR1_Register is record
      --  binary logarithm of the source data width of a burst in bytes 	Note:
      --  Setting a 8-byte data width causes a user setting error to be
      --  reported and no transfer is issued. 	A source block size must be a
      --  multiple of the source data width (GPDMA_CxBR1.BNDT[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and no
      --  transfer is issued. 	A source single transfer must have an aligned
      --  address with its data width (start address GPDMA_CxSAR[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and none
      --  transfer is issued.
      SDW_LOG2       : GPDMA_C13TR1_SDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  source incrementing burst 	The source address, pointed by
      --  GPDMA_CxSAR, is kept constant after a burst beat/single transfer or
      --  is incremented by the offset value corresponding to a contiguous data
      --  after a burst beat/single transfer.
      SINC           : Boolean := False;
      --  source burst length minus 1, between 0 and 63 	The burst length unit
      --  is one data named beat within a burst. If SBL_1[5:0] =0 , the burst
      --  can be named as single. Each data/beat has a width defined by the
      --  destination data width SDW_LOG2[1:0]. 	Note: If a burst transfer
      --  crossed a 1-Kbyte address boundary on a AHB transfer, the GPDMA
      --  modifies and shortens the programmed burst into singles or bursts of
      --  lower length, to be compliant with the AHB protocol. 	If a burst
      --  transfer is of length greater than the FIFO size of the channel x,
      --  the GPDMA modifies and shortens the programmed burst into singles or
      --  bursts of lower length, to be compliant with the FIFO size. Transfer
      --  performance is lower, with GPDMA re-arbitration between effective and
      --  lower bursts/singles, but the data integrity is guaranteed.
      SBL_1          : GPDMA_C13TR1_SBL_1_Field := 16#0#;
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  padding/alignment mode 	If DDW_LOG2[1:0] = SDW_LOG2[1:0]: if the data
      --  width of a burst destination transfer is equal to the data width of a
      --  burst source transfer, these bits are ignored. 	Else: 	- Case 1: If
      --  destination data width > source data width 	1x: successive source
      --  data are FIFO queued and packed at the destination data width, in a
      --  left (LSB) to right (MSB) order (named little endian), before a
      --  destination transfer 	- Case 2: If destination data width < source
      --  data width 	1x: source data is FIFO queued and unpacked at the
      --  destination data width, to be transferred in a left (LSB) to right
      --  (MSB) order (named little endian) to the destination 	Note:
      PAM            : GPDMA_C13TR1_PAM_Field := 16#0#;
      --  source byte exchange within the unaligned half-word of each source
      --  word 	If the source data width is shorter than a word, this bit is
      --  ignored. 	If the source data width is a word:
      SBX            : Boolean := False;
      --  source allocated port 	This bit is used to allocate the master port
      --  for the source transfer 	Note: This bit must be written when
      --  EN = 0. This bit is read-only when EN = 1.
      SAP            : Boolean := False;
      --  security attribute of the GPDMA transfer from the source 	If
      --  GPDMA_SECCFGR.SECx = 1 and the access is secure: 	This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  GPDMA_SECCFGR.SECx =1 . A secure write is ignored when
      --  GPDMA_SECCFGR.SECx = 0. 	When GPDMA_SECCFGR.SECx is de-asserted,
      --  this SSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the GPDMA transfer
      --  from the source is non-secure.
      SSEC           : Boolean := False;
      --  binary logarithm of the destination data width of a burst, in bytes
      --  	Note: Setting a 8-byte data width causes a user setting error to be
      --  reported and none transfer is issued. 	A destination burst transfer
      --  must have an aligned address with its data width (start address
      --  GPDMA_CxDAR[2:0] and address offset GPDMA_CxTR3.DAO[2:0], versus
      --  DDW_LOG2[1:0]). Otherwise a user setting error is reported and no
      --  transfer is issued.
      DDW_LOG2       : GPDMA_C13TR1_DDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_18_18 : HAL.Bit := 16#0#;
      --  destination incrementing burst 	The destination address, pointed by
      --  GPDMA_CxDAR, is kept constant after a burst beat/single transfer, or
      --  is incremented by the offset value corresponding to a contiguous data
      --  after a burst beat/single transfer.
      DINC           : Boolean := False;
      --  destination burst length minus 1, between 0 and 63 	The burst length
      --  unit is one data named beat within a burst. If DBL_1[5:0] =0 , the
      --  burst can be named as single. Each data/beat has a width defined by
      --  the destination data width DDW_LOG2[1:0]. 	Note: If a burst transfer
      --  crossed a 1-Kbyte address boundary on a AHB transfer, the GPDMA
      --  modifies and shortens the programmed burst into singles or bursts of
      --  lower length, to be compliant with the AHB protocol. 	If a burst
      --  transfer is of length greater than the FIFO size of the channel x,
      --  the GPDMA modifies and shortens the programmed burst into singles or
      --  bursts of lower length, to be compliant with the FIFO size. Transfer
      --  performance is lower, with GPDMA re-arbitration between effective and
      --  lower bursts/singles, but the data integrity is guaranteed.
      DBL_1          : GPDMA_C13TR1_DBL_1_Field := 16#0#;
      --  destination byte exchange 	If the destination data size is a byte,
      --  this bit is ignored. 	If the destination data size is not a byte:
      DBX            : Boolean := False;
      --  destination half-word exchange 	If the destination data size is
      --  shorter than a word, this bit is ignored. 	If the destination data
      --  size is a word:
      DHX            : Boolean := False;
      --  unspecified
      Reserved_28_29 : HAL.UInt2 := 16#0#;
      --  destination allocated port 	This bit is used to allocate the master
      --  port for the destination transfer 	Note: This bit must be written
      --  when EN = 0. This bit is read-only when EN = 1.
      DAP            : Boolean := False;
      --  security attribute of the GPDMA transfer to the destination 	If
      --  GPDMA_SECCFGR.SECx = 1 and the access is secure: 	This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  GPDMA_SECCFGR.SECx = 1. A secure write is ignored when
      --  GPDMA_SECCFGR.SECx = 0. 	When GPDMA_SECCFGR.SECx is de-asserted,
      --  this DSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the GPDMA transfer
      --  to the destination is non-secure.
      DSEC           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C13TR1_Register use record
      SDW_LOG2       at 0 range 0 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      SINC           at 0 range 3 .. 3;
      SBL_1          at 0 range 4 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      PAM            at 0 range 11 .. 12;
      SBX            at 0 range 13 .. 13;
      SAP            at 0 range 14 .. 14;
      SSEC           at 0 range 15 .. 15;
      DDW_LOG2       at 0 range 16 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      DINC           at 0 range 19 .. 19;
      DBL_1          at 0 range 20 .. 25;
      DBX            at 0 range 26 .. 26;
      DHX            at 0 range 27 .. 27;
      Reserved_28_29 at 0 range 28 .. 29;
      DAP            at 0 range 30 .. 30;
      DSEC           at 0 range 31 .. 31;
   end record;

   subtype GPDMA_C13TR2_REQSEL_Field is HAL.UInt7;
   subtype GPDMA_C13TR2_TRIGM_Field is HAL.UInt2;
   subtype GPDMA_C13TR2_TRIGSEL_Field is HAL.UInt6;
   subtype GPDMA_C13TR2_TRIGPOL_Field is HAL.UInt2;
   subtype GPDMA_C13TR2_TCEM_Field is HAL.UInt2;

   --  GPDMA channel 13 transfer register 2
   type GPDMA_C13TR2_Register is record
      --  GPDMA hardware request selection 	These bits are ignored if channel x
      --  is activated (GPDMA_CxCR.EN asserted) with SWREQ = 1 (software
      --  request for a memory-to-memory transfer). Else, the selected hardware
      --  request is internally taken into account as per . 	The user must not
      --  assign a same input hardware request (same REQSEL[6:0] value) to
      --  different active GPDMA channels (GPDMA_CxCR.EN = 1 and
      --  GPDMA_CxTR2.SWREQ = 0 for these channels). GPDMA is not intended to
      --  hardware support the case of simultaneous enabled channels
      --  incorrectly configured with a same hardware peripheral request
      --  signal, and there is no user setting error reporting.
      REQSEL         : GPDMA_C13TR2_REQSEL_Field := 16#0#;
      --  unspecified
      Reserved_7_8   : HAL.UInt2 := 16#0#;
      --  software request 	This bit is internally taken into account when
      --  GPDMA_CxCR.EN is asserted.
      SWREQ          : Boolean := False;
      --  destination hardware request 	This bit is ignored if channel x is
      --  activated (GPDMA_CxCR.EN asserted) with SWREQ = 1 (software request
      --  for a memory-to-memory transfer). Else: 	Note:
      DREQ           : Boolean := False;
      --  Block hardware request 	If the channel x is activated (GPDMA_CxCR.EN
      --  asserted) with SWREQ = 1 (software request for a memory-to-memory
      --  transfer), this bit is ignored. Else:
      BREQ           : Boolean := False;
      --  unspecified
      Reserved_12_13 : HAL.UInt2 := 16#0#;
      --  trigger mode 	These bits define the transfer granularity for its
      --  conditioning by the trigger. 	If the channel x is enabled
      --  (GPDMA_CxCR.EN asserted) with TRIGPOL[1:0] = 00 or 11, these
      --  TRIGM[1:0] bits are ignored. 	Else, a GPDMA transfer is conditioned
      --  by at least one trigger hit: 	first burst read of a 2D/repeated block
      --  transfer is conditioned by one hit trigger. 	– If the peripheral is
      --  programmed as a source (DREQ = 0) of the LLI data transfer, each
      --  programmed burst read is conditioned. 	– If the peripheral is
      --  programmed as a destination (DREQ = 1) of the LLI data transfer,
      --  each programmed burst write is conditioned. The first memory burst
      --  read of a (possibly 2D/repeated) block, also named as the first ready
      --  FIFO-based source burst, is gated by the occurrence of both the
      --  hardware request and the first trigger hit. 	The GPDMA monitoring of
      --  a trigger for channel x is started when the channel is enabled/loaded
      --  with a new active trigger configuration: rising or falling edge on a
      --  selected trigger (TRIGPOL[1:0] = 01 or respectively
      --  TRIGPOL[1:0] = 10). 	The monitoring of this trigger is kept active
      --  during the triggered and uncompleted (data or link) transfer; and if
      --  a new trigger is detected then, this hit is internally memorized to
      --  grant the next transfer, as long as the defined rising or falling
      --  edge is not modified, and the TRIGSEL[5:0] is not modified, and the
      --  channel is enabled. 	Transferring a next LLIn+1 that updates the
      --  GPDMA_CxTR2 with a new value for any of TRIGSEL[5:0] or TRIGPOL[1:0],
      --  resets the monitoring, trashing the memorized hit of the formerly
      --  defined LLIn trigger. 	After a first new trigger hitn+1 is memorized,
      --  if another second trigger hitn+2 is detected and if the hitn
      --  triggered transfer is still not completed, hitn+2 is lost and not
      --  memorized.memorized. A trigger overrun flag is reported
      --  (GPDMA_CxSR.TOF =1 ), and an interrupt is generated if enabled
      --  (GPDMA_CxCR.TOIE = 1). The channel is not automatically disabled by
      --  hardware due to a trigger overrun. 	Note: When the source block size
      --  is not a multiple of the source burst size and is a multiple of the
      --  source data width, then the last programmed source burst is not
      --  completed and is internally shorten to match the block size. In this
      --  case, if TRIGM[1:0] = 11 and (SWREQ =1  or (SWREQ = 0 and
      --  DREQ =0 )), the shortened burst transfer (by singles or/and by
      --  bursts of lower length) is conditioned once by the trigger. 	When the
      --  programmed destination burst is internally shortened by singles
      --  or/and by bursts of lower length (versus FIFO size, versus block
      --  size, 1-Kbyte boundary address crossing): if the trigger is
      --  conditioning the programmed destination burst (if TRIGM[1:0] = 11
      --  and SWREQ = 0 and DREQ = 1), this shortened destination burst
      --  transfer is conditioned once by the trigger.
      TRIGM          : GPDMA_C13TR2_TRIGM_Field := 16#0#;
      --  trigger event input selection 	These bits select the trigger event
      --  input of the GPDMA transfer (as per ), with an active trigger event
      --  if TRIGPOL[1:0] ≠ 00.
      TRIGSEL        : GPDMA_C13TR2_TRIGSEL_Field := 16#0#;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  trigger event polarity 	These bits define the polarity of the
      --  selected trigger event input defined by TRIGSEL[5:0].
      TRIGPOL        : GPDMA_C13TR2_TRIGPOL_Field := 16#0#;
      --  unspecified
      Reserved_26_29 : HAL.UInt4 := 16#0#;
      --  transfer complete event mode 	These bits define the transfer
      --  granularity for the transfer complete and half transfer complete
      --  events generation. 	Note: If the initial LLI0 data transfer is
      --  null/void (directly programmed by the internal register file with
      --  GPDMA_CxBR1.BNDT[15:0] = 0), then neither the complete transfer
      --  event nor the half transfer event is generated. 	Note: If the initial
      --  LLI0 data transfer is null/void (directly programmed by the internal
      --  register file with GPDMA_CxBR1.BNDT[15:0] = 0), then neither the
      --  complete transfer event nor the half transfer event is generated.
      --  	Note: If the initial LLI0 data transfer is null/void (i.e. directly
      --  programmed by the internal register file with
      --  GPDMA_CxBR1.BNDT[15:0] =0 ), then the half transfer event is not
      --  generated, and the transfer complete event is generated when is
      --  completed the loading of the LLI1.
      TCEM           : GPDMA_C13TR2_TCEM_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C13TR2_Register use record
      REQSEL         at 0 range 0 .. 6;
      Reserved_7_8   at 0 range 7 .. 8;
      SWREQ          at 0 range 9 .. 9;
      DREQ           at 0 range 10 .. 10;
      BREQ           at 0 range 11 .. 11;
      Reserved_12_13 at 0 range 12 .. 13;
      TRIGM          at 0 range 14 .. 15;
      TRIGSEL        at 0 range 16 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      TRIGPOL        at 0 range 24 .. 25;
      Reserved_26_29 at 0 range 26 .. 29;
      TCEM           at 0 range 30 .. 31;
   end record;

   subtype GPDMA_C13BR1_BNDT_Field is HAL.UInt16;
   subtype GPDMA_C13BR1_BRC_Field is HAL.UInt11;

   --  GPDMA channel 13 alternate block register 1
   type GPDMA_C13BR1_Register is record
      --  block number of data bytes to transfer from the source 	Block size
      --  transferred from the source. When the channel is enabled, this field
      --  becomes read-only and is decremented, indicating the remaining number
      --  of data items in the current source block to be transferred.
      --  BNDT[15:0] is programmed in number of bytes, maximum source block
      --  size is 64 Kbytes -1. 	Once the last data transfer is completed
      --  (BNDT[15:0] = 0): 	- if GPDMA_CxLLR.UB1 = 1, this field is
      --  updated by the LLI in the memory. 	- if GPDMA_CxLLR.UB1 = 0 and if
      --  there is at least one not null Uxx update bit, this field is
      --  internally restored to the programmed value. 	- if all
      --  GPDMA_CxLLR.Uxx = 0 and if GPDMA_CxLLR.LA[15:0] ≠ 0, this field
      --  is internally restored to the programmed value (infinite/continuous
      --  last LLI). 	- if GPDMA_CxLLR = 0, this field is kept as zero
      --  following the last LLI data transfer. 	Note: A non-null source block
      --  size must be a multiple of the source data width (BNDT[2:0] versus
      --  GPDMA_CxTR1.SDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued. 	When configured in packing mode
      --  (GPDMA_CxTR1.PAM[1]=1 and destination data width different from
      --  source data width), a non-null source block size must be a multiple
      --  of the destination data width (BNDT[2:0] versus
      --  GPDMA_CxTR1.DDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued.
      BNDT           : GPDMA_C13BR1_BNDT_Field := 16#0#;
      --  Block repeat counter 	This field contains the number of repetitions
      --  of the current block (0 to 2047). 	When the channel is enabled, this
      --  field becomes read-only. After decrements, this field indicates the
      --  remaining number of blocks, excluding the current one. This counter
      --  is hardware decremented for each completed block transfer. 	Once the
      --  last block transfer is completed (BRC[10:0] = BNDT[15:0] = 0):
      --  	If GPDMA_CxLLR.UB1 = 1, all GPDMA_CxBR1 fields are updated by the
      --  next LLI in the memory. 	If GPDMA_CxLLR.UB1 = 0 and if there is at
      --  least one not null Uxx update bit, this field is internally restored
      --  to the programmed value. 	if all GPDMA_CxLLR.Uxx = 0 and if
      --  GPDMA_CxLLR.LA[15:0] ≠ 0, this field is internally restored to
      --  the programmed value (infinite/continuous last LLI). 	if
      --  GPDMA_CxLLR = 0, this field is kept as zero following the last LLI
      --  and data transfer.
      BRC            : GPDMA_C13BR1_BRC_Field := 16#0#;
      --  unspecified
      Reserved_27_27 : HAL.Bit := 16#0#;
      --  source address decrement
      SDEC           : Boolean := False;
      --  destination address decrement
      DDEC           : Boolean := False;
      --  Block repeat source address decrement 	Note: On top of this
      --  increment/decrement (depending on BRSDEC), GPDMA_CxSAR is in the same
      --  time also updated by the increment/decrement (depending on SDEC) of
      --  the GPDMA_CxTR3.SAO value, as it is done after any programmed burst
      --  transfer.
      BRSDEC         : Boolean := False;
      --  Block repeat destination address decrement 	Note: On top of this
      --  increment/decrement (depending on BRDDEC), GPDMA_CxDAR is in the same
      --  time also updated by the increment/decrement (depending on DDEC) of
      --  the GPDMA_CxTR3.DAO value, as it is usually done at the end of each
      --  programmed burst transfer.
      BRDDEC         : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C13BR1_Register use record
      BNDT           at 0 range 0 .. 15;
      BRC            at 0 range 16 .. 26;
      Reserved_27_27 at 0 range 27 .. 27;
      SDEC           at 0 range 28 .. 28;
      DDEC           at 0 range 29 .. 29;
      BRSDEC         at 0 range 30 .. 30;
      BRDDEC         at 0 range 31 .. 31;
   end record;

   subtype GPDMA_C13TR3_SAO_Field is HAL.UInt13;
   subtype GPDMA_C13TR3_DAO_Field is HAL.UInt13;

   --  GPDMA channel 13 transfer register 3
   type GPDMA_C13TR3_Register is record
      --  source address offset increment 	The source address, pointed by
      --  GPDMA_CxSAR, is incremented or decremented (depending on
      --  GPDMA_CxBR1.SDEC) by this offset SAO[12:0] for each programmed source
      --  burst. This offset is not including and is added to the programmed
      --  burst size when the completed burst is addressed in incremented mode
      --  (GPDMA_CxTR1.SINC = 1). 	Note: A source address offset must be
      --  aligned with the programmed data width of a source burst (SAO[2:0]
      --  versus GPDMA_CxTR1.SDW_LOG2[1:0]). Else a user setting error is
      --  reported and none transfer is issued. 	When the source block size is
      --  not a multiple of the destination burst size and is a multiple of the
      --  source data width, then the last programmed source burst is not
      --  completed and is internally shorten to match the block size. In this
      --  case, the additional GPDMA_CxTR3.SAO[12:0] is not applied.
      SAO            : GPDMA_C13TR3_SAO_Field := 16#0#;
      --  unspecified
      Reserved_13_15 : HAL.UInt3 := 16#0#;
      --  destination address offset increment 	The destination address,
      --  pointed by GPDMA_CxDAR, is incremented or decremented (depending on
      --  GPDMA_CxBR1.DDEC) by this offset DAO[12:0] for each programmed
      --  destination burst. This offset is not including and is added to the
      --  programmed burst size when the completed burst is addressed in
      --  incremented mode (GPDMA_CxTR1.DINC = 1). 	Note: A destination
      --  address offset must be aligned with the programmed data width of a
      --  destination burst (DAO[2:0] versus GPDMA_CxTR1.DDW_LOG2[1:0]). Else,
      --  a user setting error is reported and no transfer is issued.
      DAO            : GPDMA_C13TR3_DAO_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C13TR3_Register use record
      SAO            at 0 range 0 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      DAO            at 0 range 16 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype GPDMA_C13BR2_BRSAO_Field is HAL.UInt16;
   subtype GPDMA_C13BR2_BRDAO_Field is HAL.UInt16;

   --  GPDMA channel 13 block register 2
   type GPDMA_C13BR2_Register is record
      --  Block repeated source address offset 	For a channel with 2D
      --  addressing capability, this field is used to update (by addition or
      --  subtraction depending on GPDMA_CxBR1.BRSDEC) the current source
      --  address (GPDMA_CxSAR) at the end of a block transfer. 	Note: A block
      --  repeated source address offset must be aligned with the programmed
      --  data width of a source burst (BRSAO[2:0] versus
      --  GPDMA_CxTR1.SDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued.
      BRSAO : GPDMA_C13BR2_BRSAO_Field := 16#0#;
      --  Block repeated destination address offset 	For a channel with 2D
      --  addressing capability, this field is used to update (by addition or
      --  subtraction depending on GPDMA_CxBR1.BRDDEC) the current destination
      --  address (GPDMA_CxDAR) at the end of a block transfer. 	Note: A block
      --  repeated destination address offset must be aligned with the
      --  programmed data width of a destination burst (BRDAO[2:0] versus
      --  GPDMA_CxTR1.DDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued.
      BRDAO : GPDMA_C13BR2_BRDAO_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C13BR2_Register use record
      BRSAO at 0 range 0 .. 15;
      BRDAO at 0 range 16 .. 31;
   end record;

   subtype GPDMA_C13LLR_LA_Field is HAL.UInt14;

   --  GPDMA channel 13 alternate linked-list address register
   type GPDMA_C13LLR_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  pointer (16-bit low-significant address) to the next linked-list data
      --  structure 	If UT1 = UT2 = UB1 = USA = UDA = ULL = 0 and
      --  if LA[15:20] = 0, the current LLI is the last one. The channel
      --  transfer is completed without any update of the linked-list GPDMA
      --  register file. 	Else, this field is the pointer to the memory address
      --  offset from which the next linked-list data structure is
      --  automatically fetched from, once the data transfer is completed, in
      --  order to conditionally update the linked-list GPDMA internal register
      --  file (GPDMA_CxCTR1, GPDMA_CxTR2, GPDMA_CxBR1, GPDMA_CxSAR,
      --  GPDMA_CxDAR and GPDMA_CxLLR). 	Note: The user must program the
      --  pointer to be 32-bit aligned. The two low-significant bits are write
      --  ignored.
      LA             : GPDMA_C13LLR_LA_Field := 16#0#;
      --  Update GPDMA_CxLLR register from memory 	This bit is used to control
      --  the update of GPDMA_CxLLR from the memory during the link transfer.
      ULL            : Boolean := False;
      --  unspecified
      Reserved_17_24 : HAL.UInt8 := 16#0#;
      --  Update GPDMA_CxBR2 from memory 	This bit controls the update of
      --  GPDMA_CxBR2 from the memory during the link transfer.
      UB2            : Boolean := False;
      --  Update GPDMA_CxTR3 from memory 	This bit controls the update of
      --  GPDMA_CxTR3 from the memory during the link transfer.
      UT3            : Boolean := False;
      --  Update GPDMA_CxDAR register from memory 	This bit is used to control
      --  the update of GPDMA_CxDAR from the memory during the link transfer.
      UDA            : Boolean := False;
      --  update GPDMA_CxSAR from memory 	This bit controls the update of
      --  GPDMA_CxSAR from the memory during the link transfer.
      USA            : Boolean := False;
      --  Update GPDMA_CxBR1 from memory 	This bit controls the update of
      --  GPDMA_CxBR1 from the memory during the link transfer. If UB1 = 0
      --  and if GPDMA_CxLLR ≠ 0, the linked-list is not completed.
      --  GPDMA_CxBR1.BNDT[15:0] is then restored to the programmed value after
      --  data transfer is completed and before the link transfer.
      UB1            : Boolean := False;
      --  Update GPDMA_CxTR2 from memory 	This bit controls the update of
      --  GPDMA_CxTR2 from the memory during the link transfer.
      UT2            : Boolean := False;
      --  Update GPDMA_CxTR1 from memory 	This bit controls the update of
      --  GPDMA_CxTR1 from the memory during the link transfer.
      UT1            : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C13LLR_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      LA             at 0 range 2 .. 15;
      ULL            at 0 range 16 .. 16;
      Reserved_17_24 at 0 range 17 .. 24;
      UB2            at 0 range 25 .. 25;
      UT3            at 0 range 26 .. 26;
      UDA            at 0 range 27 .. 27;
      USA            at 0 range 28 .. 28;
      UB1            at 0 range 29 .. 29;
      UT2            at 0 range 30 .. 30;
      UT1            at 0 range 31 .. 31;
   end record;

   subtype GPDMA_C14LBAR_LBA_Field is HAL.UInt16;

   --  GPDMA channel 14 linked-list base address register
   type GPDMA_C14LBAR_Register is record
      --  unspecified
      Reserved_0_15 : HAL.UInt16 := 16#0#;
      --  linked-list base address of GPDMA channel x
      LBA           : GPDMA_C14LBAR_LBA_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C14LBAR_Register use record
      Reserved_0_15 at 0 range 0 .. 15;
      LBA           at 0 range 16 .. 31;
   end record;

   --  GPDMA channel 14 flag clear register
   type GPDMA_C14FCR_Register is record
      --  unspecified
      Reserved_0_7   : HAL.UInt8 := 16#0#;
      --  Write-only. transfer complete flag clear
      TCF            : Boolean := False;
      --  Write-only. half transfer flag clear
      HTF            : Boolean := False;
      --  Write-only. data transfer error flag clear
      DTEF           : Boolean := False;
      --  Write-only. update link transfer error flag clear
      ULEF           : Boolean := False;
      --  Write-only. user setting error flag clear
      USEF           : Boolean := False;
      --  Write-only. completed suspension flag clear
      SUSPF          : Boolean := False;
      --  Write-only. trigger overrun flag clear
      TOF            : Boolean := False;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C14FCR_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      TCF            at 0 range 8 .. 8;
      HTF            at 0 range 9 .. 9;
      DTEF           at 0 range 10 .. 10;
      ULEF           at 0 range 11 .. 11;
      USEF           at 0 range 12 .. 12;
      SUSPF          at 0 range 13 .. 13;
      TOF            at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype GPDMA_C14SR_FIFOL_Field is HAL.UInt8;

   --  GPDMA channel 14 status register
   type GPDMA_C14SR_Register is record
      --  Read-only. idle flag 	This idle flag is de-asserted by hardware when
      --  the channel is enabled (GPDMA_CxCR.EN = 1) with a valid channel
      --  configuration (no USEF to be immediately reported). 	This idle flag
      --  is asserted after hard reset or by hardware when the channel is back
      --  in idle state (in suspended or disabled state).
      IDLEF          : Boolean;
      --  unspecified
      Reserved_1_7   : HAL.UInt7;
      --  Read-only. transfer complete flag 	A transfer complete event is
      --  either a block transfer complete, a 2D/repeated block transfer
      --  complete, a LLI transfer complete including the upload of the next
      --  LLI if any, or the full linked-list completion, depending on the
      --  transfer complete event mode (GPDMA_CxTR2.TCEM[1:0]).
      TCF            : Boolean;
      --  Read-only. half transfer flag 	An half transfer event is either an
      --  half block transfer or an half 2D/repeated block transfer, depending
      --  on the transfer complete event mode (GPDMA_CxTR2.TCEM[1:0]). 	An half
      --  block transfer occurs when half of the bytes of the source block size
      --  (rounded up integer of GPDMA_CxBR1.BNDT[15:0]/2) has been transferred
      --  to the destination. 	An half 2D/repeated block transfer occurs when
      --  half of the repeated blocks (rounded up integer of
      --  (GPDMA_CxBR1.BRC[10:0]+1)/2)) has been transferred to the
      --  destination.
      HTF            : Boolean;
      --  Read-only. data transfer error flag
      DTEF           : Boolean;
      --  Read-only. update link transfer error flag
      ULEF           : Boolean;
      --  Read-only. user setting error flag
      USEF           : Boolean;
      --  Read-only. completed suspension flag
      SUSPF          : Boolean;
      --  Read-only. trigger overrun flag
      TOF            : Boolean;
      --  unspecified
      Reserved_15_15 : HAL.Bit;
      --  Read-only. monitored FIFO level 	Number of available write beats in
      --  the FIFO, in units of the programmed destination data width (see
      --  GPDMA_CxTR1.DDW_LOG2[1:0], in units of bytes, half-words, or words).
      --  	Note: After having suspended an active transfer, the user may need
      --  to read FIFOL[7:0], additionally to GPDMA_CxBR1.BDNT[15:0] and
      --  GPDMA_CxBR1.BRC[10:0], to know how many data have been transferred to
      --  the destination. Before reading, the user may wait for the transfer
      --  to be suspended (GPDMA_CxSR.SUSPF = 1).
      FIFOL          : GPDMA_C14SR_FIFOL_Field;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C14SR_Register use record
      IDLEF          at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      TCF            at 0 range 8 .. 8;
      HTF            at 0 range 9 .. 9;
      DTEF           at 0 range 10 .. 10;
      ULEF           at 0 range 11 .. 11;
      USEF           at 0 range 12 .. 12;
      SUSPF          at 0 range 13 .. 13;
      TOF            at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      FIFOL          at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype GPDMA_C14CR_PRIO_Field is HAL.UInt2;

   --  GPDMA channel 14 control register
   type GPDMA_C14CR_Register is record
      --  enable 	Writing 1 into the field RESET (bit 1) causes the hardware to
      --  de-assert this bit, whatever is written into this bit 0. Else: 	this
      --  bit is de-asserted by hardware when there is a transfer error (master
      --  bus error or user setting error) or when there is a channel transfer
      --  complete (channel ready to be configured, e.g. if LSM=1 at the end of
      --  a single execution of the LLI). 	Else, this bit can be asserted by
      --  software. 	Writing 0 into this EN bit is ignored.
      EN             : Boolean := False;
      --  Write-only. reset 	This bit is write only. Writing 0 has no impact.
      --  Writing 1 implies the reset of the following: the FIFO, the channel
      --  internal state, SUSP and EN bits (whatever is written receptively in
      --  bit 2 and bit 0). 	The reset is effective when the channel is in
      --  steady state, meaning one of the following: 	- active channel in
      --  suspended state (GPDMA_CxSR.SUSPF = 1 and
      --  GPDMA_CxSR.IDLEF = GPDMA_CxCR.EN = 1) 	- channel in disabled
      --  state (GPDMA_CxSR.IDLEF = 1 and GPDMA_CxCR.EN = 0). 	After
      --  writing a RESET, to continue using this channel, the user must
      --  explicitly reconfigure the channel including the hardware-modified
      --  configuration registers (GPDMA_CxBR1, GPDMA_CxSAR and GPDMA_CxDAR)
      --  before enabling again the channel (see the programming sequence in ).
      RESET          : Boolean := False;
      --  suspend 	Writing 1 into the field RESET (bit 1) causes the hardware
      --  to de-assert this bit, whatever is written into this bit 2. Else:
      --  	Software must write 1 in order to suspend an active channel i.e. a
      --  channel with an on-going GPDMA transfer over its master ports. 	The
      --  software must write 0 in order to resume a suspended channel,
      --  following the programming sequence detailed in .
      SUSP           : Boolean := False;
      --  unspecified
      Reserved_3_7   : HAL.UInt5 := 16#0#;
      --  transfer complete interrupt enable
      TCIE           : Boolean := False;
      --  half transfer complete interrupt enable
      HTIE           : Boolean := False;
      --  data transfer error interrupt enable
      DTEIE          : Boolean := False;
      --  update link transfer error interrupt enable
      ULEIE          : Boolean := False;
      --  user setting error interrupt enable
      USEIE          : Boolean := False;
      --  completed suspension interrupt enable
      SUSPIE         : Boolean := False;
      --  trigger overrun interrupt enable
      TOIE           : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Link step mode 	First the (possible 1D/repeated) block transfer is
      --  executed as defined by the current internal register file until
      --  GPDMA_CxBR1.BNDT[15:0] = 0 and GPDMA_CxBR1.BRC[10:0] = 0 if
      --  present. Secondly the next linked-list data structure is
      --  conditionally uploaded from memory as defined by GPDMA_CxLLR. Then
      --  channel execution is completed. 	Note: This bit must be written when
      --  EN=0. This bit is read-only when EN=1.
      LSM            : Boolean := False;
      --  linked-list allocated port 	This bit is used to allocate the master
      --  port for the update of the GPDMA linked-list registers from the
      --  memory. 	Note: This bit must be written when EN=0. This bit is
      --  read-only when EN=1.
      LAP            : Boolean := False;
      --  unspecified
      Reserved_18_21 : HAL.UInt4 := 16#0#;
      --  priority level of the channel x GPDMA transfer versus others 	Note:
      --  This bit must be written when EN = 0. This bit is read-only when
      --  EN = 1.
      PRIO           : GPDMA_C14CR_PRIO_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C14CR_Register use record
      EN             at 0 range 0 .. 0;
      RESET          at 0 range 1 .. 1;
      SUSP           at 0 range 2 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      TCIE           at 0 range 8 .. 8;
      HTIE           at 0 range 9 .. 9;
      DTEIE          at 0 range 10 .. 10;
      ULEIE          at 0 range 11 .. 11;
      USEIE          at 0 range 12 .. 12;
      SUSPIE         at 0 range 13 .. 13;
      TOIE           at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      LSM            at 0 range 16 .. 16;
      LAP            at 0 range 17 .. 17;
      Reserved_18_21 at 0 range 18 .. 21;
      PRIO           at 0 range 22 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype GPDMA_C14TR1_SDW_LOG2_Field is HAL.UInt2;
   subtype GPDMA_C14TR1_SBL_1_Field is HAL.UInt6;
   subtype GPDMA_C14TR1_PAM_Field is HAL.UInt2;
   subtype GPDMA_C14TR1_DDW_LOG2_Field is HAL.UInt2;
   subtype GPDMA_C14TR1_DBL_1_Field is HAL.UInt6;

   --  GPDMA channel 14 transfer register 1
   type GPDMA_C14TR1_Register is record
      --  binary logarithm of the source data width of a burst in bytes 	Note:
      --  Setting a 8-byte data width causes a user setting error to be
      --  reported and no transfer is issued. 	A source block size must be a
      --  multiple of the source data width (GPDMA_CxBR1.BNDT[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and no
      --  transfer is issued. 	A source single transfer must have an aligned
      --  address with its data width (start address GPDMA_CxSAR[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and none
      --  transfer is issued.
      SDW_LOG2       : GPDMA_C14TR1_SDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  source incrementing burst 	The source address, pointed by
      --  GPDMA_CxSAR, is kept constant after a burst beat/single transfer or
      --  is incremented by the offset value corresponding to a contiguous data
      --  after a burst beat/single transfer.
      SINC           : Boolean := False;
      --  source burst length minus 1, between 0 and 63 	The burst length unit
      --  is one data named beat within a burst. If SBL_1[5:0] =0 , the burst
      --  can be named as single. Each data/beat has a width defined by the
      --  destination data width SDW_LOG2[1:0]. 	Note: If a burst transfer
      --  crossed a 1-Kbyte address boundary on a AHB transfer, the GPDMA
      --  modifies and shortens the programmed burst into singles or bursts of
      --  lower length, to be compliant with the AHB protocol. 	If a burst
      --  transfer is of length greater than the FIFO size of the channel x,
      --  the GPDMA modifies and shortens the programmed burst into singles or
      --  bursts of lower length, to be compliant with the FIFO size. Transfer
      --  performance is lower, with GPDMA re-arbitration between effective and
      --  lower bursts/singles, but the data integrity is guaranteed.
      SBL_1          : GPDMA_C14TR1_SBL_1_Field := 16#0#;
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  padding/alignment mode 	If DDW_LOG2[1:0] = SDW_LOG2[1:0]: if the data
      --  width of a burst destination transfer is equal to the data width of a
      --  burst source transfer, these bits are ignored. 	Else: 	- Case 1: If
      --  destination data width > source data width 	1x: successive source
      --  data are FIFO queued and packed at the destination data width, in a
      --  left (LSB) to right (MSB) order (named little endian), before a
      --  destination transfer 	- Case 2: If destination data width < source
      --  data width 	1x: source data is FIFO queued and unpacked at the
      --  destination data width, to be transferred in a left (LSB) to right
      --  (MSB) order (named little endian) to the destination 	Note:
      PAM            : GPDMA_C14TR1_PAM_Field := 16#0#;
      --  source byte exchange within the unaligned half-word of each source
      --  word 	If the source data width is shorter than a word, this bit is
      --  ignored. 	If the source data width is a word:
      SBX            : Boolean := False;
      --  source allocated port 	This bit is used to allocate the master port
      --  for the source transfer 	Note: This bit must be written when
      --  EN = 0. This bit is read-only when EN = 1.
      SAP            : Boolean := False;
      --  security attribute of the GPDMA transfer from the source 	If
      --  GPDMA_SECCFGR.SECx = 1 and the access is secure: 	This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  GPDMA_SECCFGR.SECx =1 . A secure write is ignored when
      --  GPDMA_SECCFGR.SECx = 0. 	When GPDMA_SECCFGR.SECx is de-asserted,
      --  this SSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the GPDMA transfer
      --  from the source is non-secure.
      SSEC           : Boolean := False;
      --  binary logarithm of the destination data width of a burst, in bytes
      --  	Note: Setting a 8-byte data width causes a user setting error to be
      --  reported and none transfer is issued. 	A destination burst transfer
      --  must have an aligned address with its data width (start address
      --  GPDMA_CxDAR[2:0] and address offset GPDMA_CxTR3.DAO[2:0], versus
      --  DDW_LOG2[1:0]). Otherwise a user setting error is reported and no
      --  transfer is issued.
      DDW_LOG2       : GPDMA_C14TR1_DDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_18_18 : HAL.Bit := 16#0#;
      --  destination incrementing burst 	The destination address, pointed by
      --  GPDMA_CxDAR, is kept constant after a burst beat/single transfer, or
      --  is incremented by the offset value corresponding to a contiguous data
      --  after a burst beat/single transfer.
      DINC           : Boolean := False;
      --  destination burst length minus 1, between 0 and 63 	The burst length
      --  unit is one data named beat within a burst. If DBL_1[5:0] =0 , the
      --  burst can be named as single. Each data/beat has a width defined by
      --  the destination data width DDW_LOG2[1:0]. 	Note: If a burst transfer
      --  crossed a 1-Kbyte address boundary on a AHB transfer, the GPDMA
      --  modifies and shortens the programmed burst into singles or bursts of
      --  lower length, to be compliant with the AHB protocol. 	If a burst
      --  transfer is of length greater than the FIFO size of the channel x,
      --  the GPDMA modifies and shortens the programmed burst into singles or
      --  bursts of lower length, to be compliant with the FIFO size. Transfer
      --  performance is lower, with GPDMA re-arbitration between effective and
      --  lower bursts/singles, but the data integrity is guaranteed.
      DBL_1          : GPDMA_C14TR1_DBL_1_Field := 16#0#;
      --  destination byte exchange 	If the destination data size is a byte,
      --  this bit is ignored. 	If the destination data size is not a byte:
      DBX            : Boolean := False;
      --  destination half-word exchange 	If the destination data size is
      --  shorter than a word, this bit is ignored. 	If the destination data
      --  size is a word:
      DHX            : Boolean := False;
      --  unspecified
      Reserved_28_29 : HAL.UInt2 := 16#0#;
      --  destination allocated port 	This bit is used to allocate the master
      --  port for the destination transfer 	Note: This bit must be written
      --  when EN = 0. This bit is read-only when EN = 1.
      DAP            : Boolean := False;
      --  security attribute of the GPDMA transfer to the destination 	If
      --  GPDMA_SECCFGR.SECx = 1 and the access is secure: 	This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  GPDMA_SECCFGR.SECx = 1. A secure write is ignored when
      --  GPDMA_SECCFGR.SECx = 0. 	When GPDMA_SECCFGR.SECx is de-asserted,
      --  this DSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the GPDMA transfer
      --  to the destination is non-secure.
      DSEC           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C14TR1_Register use record
      SDW_LOG2       at 0 range 0 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      SINC           at 0 range 3 .. 3;
      SBL_1          at 0 range 4 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      PAM            at 0 range 11 .. 12;
      SBX            at 0 range 13 .. 13;
      SAP            at 0 range 14 .. 14;
      SSEC           at 0 range 15 .. 15;
      DDW_LOG2       at 0 range 16 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      DINC           at 0 range 19 .. 19;
      DBL_1          at 0 range 20 .. 25;
      DBX            at 0 range 26 .. 26;
      DHX            at 0 range 27 .. 27;
      Reserved_28_29 at 0 range 28 .. 29;
      DAP            at 0 range 30 .. 30;
      DSEC           at 0 range 31 .. 31;
   end record;

   subtype GPDMA_C14TR2_REQSEL_Field is HAL.UInt7;
   subtype GPDMA_C14TR2_TRIGM_Field is HAL.UInt2;
   subtype GPDMA_C14TR2_TRIGSEL_Field is HAL.UInt6;
   subtype GPDMA_C14TR2_TRIGPOL_Field is HAL.UInt2;
   subtype GPDMA_C14TR2_TCEM_Field is HAL.UInt2;

   --  GPDMA channel 14 transfer register 2
   type GPDMA_C14TR2_Register is record
      --  GPDMA hardware request selection 	These bits are ignored if channel x
      --  is activated (GPDMA_CxCR.EN asserted) with SWREQ = 1 (software
      --  request for a memory-to-memory transfer). Else, the selected hardware
      --  request is internally taken into account as per . 	The user must not
      --  assign a same input hardware request (same REQSEL[6:0] value) to
      --  different active GPDMA channels (GPDMA_CxCR.EN = 1 and
      --  GPDMA_CxTR2.SWREQ = 0 for these channels). GPDMA is not intended to
      --  hardware support the case of simultaneous enabled channels
      --  incorrectly configured with a same hardware peripheral request
      --  signal, and there is no user setting error reporting.
      REQSEL         : GPDMA_C14TR2_REQSEL_Field := 16#0#;
      --  unspecified
      Reserved_7_8   : HAL.UInt2 := 16#0#;
      --  software request 	This bit is internally taken into account when
      --  GPDMA_CxCR.EN is asserted.
      SWREQ          : Boolean := False;
      --  destination hardware request 	This bit is ignored if channel x is
      --  activated (GPDMA_CxCR.EN asserted) with SWREQ = 1 (software request
      --  for a memory-to-memory transfer). Else: 	Note:
      DREQ           : Boolean := False;
      --  Block hardware request 	If the channel x is activated (GPDMA_CxCR.EN
      --  asserted) with SWREQ = 1 (software request for a memory-to-memory
      --  transfer), this bit is ignored. Else:
      BREQ           : Boolean := False;
      --  unspecified
      Reserved_12_13 : HAL.UInt2 := 16#0#;
      --  trigger mode 	These bits define the transfer granularity for its
      --  conditioning by the trigger. 	If the channel x is enabled
      --  (GPDMA_CxCR.EN asserted) with TRIGPOL[1:0] = 00 or 11, these
      --  TRIGM[1:0] bits are ignored. 	Else, a GPDMA transfer is conditioned
      --  by at least one trigger hit: 	first burst read of a 2D/repeated block
      --  transfer is conditioned by one hit trigger. 	– If the peripheral is
      --  programmed as a source (DREQ = 0) of the LLI data transfer, each
      --  programmed burst read is conditioned. 	– If the peripheral is
      --  programmed as a destination (DREQ = 1) of the LLI data transfer,
      --  each programmed burst write is conditioned. The first memory burst
      --  read of a (possibly 2D/repeated) block, also named as the first ready
      --  FIFO-based source burst, is gated by the occurrence of both the
      --  hardware request and the first trigger hit. 	The GPDMA monitoring of
      --  a trigger for channel x is started when the channel is enabled/loaded
      --  with a new active trigger configuration: rising or falling edge on a
      --  selected trigger (TRIGPOL[1:0] = 01 or respectively
      --  TRIGPOL[1:0] = 10). 	The monitoring of this trigger is kept active
      --  during the triggered and uncompleted (data or link) transfer; and if
      --  a new trigger is detected then, this hit is internally memorized to
      --  grant the next transfer, as long as the defined rising or falling
      --  edge is not modified, and the TRIGSEL[5:0] is not modified, and the
      --  channel is enabled. 	Transferring a next LLIn+1 that updates the
      --  GPDMA_CxTR2 with a new value for any of TRIGSEL[5:0] or TRIGPOL[1:0],
      --  resets the monitoring, trashing the memorized hit of the formerly
      --  defined LLIn trigger. 	After a first new trigger hitn+1 is memorized,
      --  if another second trigger hitn+2 is detected and if the hitn
      --  triggered transfer is still not completed, hitn+2 is lost and not
      --  memorized.memorized. A trigger overrun flag is reported
      --  (GPDMA_CxSR.TOF =1 ), and an interrupt is generated if enabled
      --  (GPDMA_CxCR.TOIE = 1). The channel is not automatically disabled by
      --  hardware due to a trigger overrun. 	Note: When the source block size
      --  is not a multiple of the source burst size and is a multiple of the
      --  source data width, then the last programmed source burst is not
      --  completed and is internally shorten to match the block size. In this
      --  case, if TRIGM[1:0] = 11 and (SWREQ =1  or (SWREQ = 0 and
      --  DREQ =0 )), the shortened burst transfer (by singles or/and by
      --  bursts of lower length) is conditioned once by the trigger. 	When the
      --  programmed destination burst is internally shortened by singles
      --  or/and by bursts of lower length (versus FIFO size, versus block
      --  size, 1-Kbyte boundary address crossing): if the trigger is
      --  conditioning the programmed destination burst (if TRIGM[1:0] = 11
      --  and SWREQ = 0 and DREQ = 1), this shortened destination burst
      --  transfer is conditioned once by the trigger.
      TRIGM          : GPDMA_C14TR2_TRIGM_Field := 16#0#;
      --  trigger event input selection 	These bits select the trigger event
      --  input of the GPDMA transfer (as per ), with an active trigger event
      --  if TRIGPOL[1:0] ≠ 00.
      TRIGSEL        : GPDMA_C14TR2_TRIGSEL_Field := 16#0#;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  trigger event polarity 	These bits define the polarity of the
      --  selected trigger event input defined by TRIGSEL[5:0].
      TRIGPOL        : GPDMA_C14TR2_TRIGPOL_Field := 16#0#;
      --  unspecified
      Reserved_26_29 : HAL.UInt4 := 16#0#;
      --  transfer complete event mode 	These bits define the transfer
      --  granularity for the transfer complete and half transfer complete
      --  events generation. 	Note: If the initial LLI0 data transfer is
      --  null/void (directly programmed by the internal register file with
      --  GPDMA_CxBR1.BNDT[15:0] = 0), then neither the complete transfer
      --  event nor the half transfer event is generated. 	Note: If the initial
      --  LLI0 data transfer is null/void (directly programmed by the internal
      --  register file with GPDMA_CxBR1.BNDT[15:0] = 0), then neither the
      --  complete transfer event nor the half transfer event is generated.
      --  	Note: If the initial LLI0 data transfer is null/void (i.e. directly
      --  programmed by the internal register file with
      --  GPDMA_CxBR1.BNDT[15:0] =0 ), then the half transfer event is not
      --  generated, and the transfer complete event is generated when is
      --  completed the loading of the LLI1.
      TCEM           : GPDMA_C14TR2_TCEM_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C14TR2_Register use record
      REQSEL         at 0 range 0 .. 6;
      Reserved_7_8   at 0 range 7 .. 8;
      SWREQ          at 0 range 9 .. 9;
      DREQ           at 0 range 10 .. 10;
      BREQ           at 0 range 11 .. 11;
      Reserved_12_13 at 0 range 12 .. 13;
      TRIGM          at 0 range 14 .. 15;
      TRIGSEL        at 0 range 16 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      TRIGPOL        at 0 range 24 .. 25;
      Reserved_26_29 at 0 range 26 .. 29;
      TCEM           at 0 range 30 .. 31;
   end record;

   subtype GPDMA_C14BR1_BNDT_Field is HAL.UInt16;
   subtype GPDMA_C14BR1_BRC_Field is HAL.UInt11;

   --  GPDMA channel 14 alternate block register 1
   type GPDMA_C14BR1_Register is record
      --  block number of data bytes to transfer from the source 	Block size
      --  transferred from the source. When the channel is enabled, this field
      --  becomes read-only and is decremented, indicating the remaining number
      --  of data items in the current source block to be transferred.
      --  BNDT[15:0] is programmed in number of bytes, maximum source block
      --  size is 64 Kbytes -1. 	Once the last data transfer is completed
      --  (BNDT[15:0] = 0): 	- if GPDMA_CxLLR.UB1 = 1, this field is
      --  updated by the LLI in the memory. 	- if GPDMA_CxLLR.UB1 = 0 and if
      --  there is at least one not null Uxx update bit, this field is
      --  internally restored to the programmed value. 	- if all
      --  GPDMA_CxLLR.Uxx = 0 and if GPDMA_CxLLR.LA[15:0] ≠ 0, this field
      --  is internally restored to the programmed value (infinite/continuous
      --  last LLI). 	- if GPDMA_CxLLR = 0, this field is kept as zero
      --  following the last LLI data transfer. 	Note: A non-null source block
      --  size must be a multiple of the source data width (BNDT[2:0] versus
      --  GPDMA_CxTR1.SDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued. 	When configured in packing mode
      --  (GPDMA_CxTR1.PAM[1]=1 and destination data width different from
      --  source data width), a non-null source block size must be a multiple
      --  of the destination data width (BNDT[2:0] versus
      --  GPDMA_CxTR1.DDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued.
      BNDT           : GPDMA_C14BR1_BNDT_Field := 16#0#;
      --  Block repeat counter 	This field contains the number of repetitions
      --  of the current block (0 to 2047). 	When the channel is enabled, this
      --  field becomes read-only. After decrements, this field indicates the
      --  remaining number of blocks, excluding the current one. This counter
      --  is hardware decremented for each completed block transfer. 	Once the
      --  last block transfer is completed (BRC[10:0] = BNDT[15:0] = 0):
      --  	If GPDMA_CxLLR.UB1 = 1, all GPDMA_CxBR1 fields are updated by the
      --  next LLI in the memory. 	If GPDMA_CxLLR.UB1 = 0 and if there is at
      --  least one not null Uxx update bit, this field is internally restored
      --  to the programmed value. 	if all GPDMA_CxLLR.Uxx = 0 and if
      --  GPDMA_CxLLR.LA[15:0] ≠ 0, this field is internally restored to
      --  the programmed value (infinite/continuous last LLI). 	if
      --  GPDMA_CxLLR = 0, this field is kept as zero following the last LLI
      --  and data transfer.
      BRC            : GPDMA_C14BR1_BRC_Field := 16#0#;
      --  unspecified
      Reserved_27_27 : HAL.Bit := 16#0#;
      --  source address decrement
      SDEC           : Boolean := False;
      --  destination address decrement
      DDEC           : Boolean := False;
      --  Block repeat source address decrement 	Note: On top of this
      --  increment/decrement (depending on BRSDEC), GPDMA_CxSAR is in the same
      --  time also updated by the increment/decrement (depending on SDEC) of
      --  the GPDMA_CxTR3.SAO value, as it is done after any programmed burst
      --  transfer.
      BRSDEC         : Boolean := False;
      --  Block repeat destination address decrement 	Note: On top of this
      --  increment/decrement (depending on BRDDEC), GPDMA_CxDAR is in the same
      --  time also updated by the increment/decrement (depending on DDEC) of
      --  the GPDMA_CxTR3.DAO value, as it is usually done at the end of each
      --  programmed burst transfer.
      BRDDEC         : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C14BR1_Register use record
      BNDT           at 0 range 0 .. 15;
      BRC            at 0 range 16 .. 26;
      Reserved_27_27 at 0 range 27 .. 27;
      SDEC           at 0 range 28 .. 28;
      DDEC           at 0 range 29 .. 29;
      BRSDEC         at 0 range 30 .. 30;
      BRDDEC         at 0 range 31 .. 31;
   end record;

   subtype GPDMA_C14TR3_SAO_Field is HAL.UInt13;
   subtype GPDMA_C14TR3_DAO_Field is HAL.UInt13;

   --  GPDMA channel 14 transfer register 3
   type GPDMA_C14TR3_Register is record
      --  source address offset increment 	The source address, pointed by
      --  GPDMA_CxSAR, is incremented or decremented (depending on
      --  GPDMA_CxBR1.SDEC) by this offset SAO[12:0] for each programmed source
      --  burst. This offset is not including and is added to the programmed
      --  burst size when the completed burst is addressed in incremented mode
      --  (GPDMA_CxTR1.SINC = 1). 	Note: A source address offset must be
      --  aligned with the programmed data width of a source burst (SAO[2:0]
      --  versus GPDMA_CxTR1.SDW_LOG2[1:0]). Else a user setting error is
      --  reported and none transfer is issued. 	When the source block size is
      --  not a multiple of the destination burst size and is a multiple of the
      --  source data width, then the last programmed source burst is not
      --  completed and is internally shorten to match the block size. In this
      --  case, the additional GPDMA_CxTR3.SAO[12:0] is not applied.
      SAO            : GPDMA_C14TR3_SAO_Field := 16#0#;
      --  unspecified
      Reserved_13_15 : HAL.UInt3 := 16#0#;
      --  destination address offset increment 	The destination address,
      --  pointed by GPDMA_CxDAR, is incremented or decremented (depending on
      --  GPDMA_CxBR1.DDEC) by this offset DAO[12:0] for each programmed
      --  destination burst. This offset is not including and is added to the
      --  programmed burst size when the completed burst is addressed in
      --  incremented mode (GPDMA_CxTR1.DINC = 1). 	Note: A destination
      --  address offset must be aligned with the programmed data width of a
      --  destination burst (DAO[2:0] versus GPDMA_CxTR1.DDW_LOG2[1:0]). Else,
      --  a user setting error is reported and no transfer is issued.
      DAO            : GPDMA_C14TR3_DAO_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C14TR3_Register use record
      SAO            at 0 range 0 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      DAO            at 0 range 16 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype GPDMA_C14BR2_BRSAO_Field is HAL.UInt16;
   subtype GPDMA_C14BR2_BRDAO_Field is HAL.UInt16;

   --  GPDMA channel 14 block register 2
   type GPDMA_C14BR2_Register is record
      --  Block repeated source address offset 	For a channel with 2D
      --  addressing capability, this field is used to update (by addition or
      --  subtraction depending on GPDMA_CxBR1.BRSDEC) the current source
      --  address (GPDMA_CxSAR) at the end of a block transfer. 	Note: A block
      --  repeated source address offset must be aligned with the programmed
      --  data width of a source burst (BRSAO[2:0] versus
      --  GPDMA_CxTR1.SDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued.
      BRSAO : GPDMA_C14BR2_BRSAO_Field := 16#0#;
      --  Block repeated destination address offset 	For a channel with 2D
      --  addressing capability, this field is used to update (by addition or
      --  subtraction depending on GPDMA_CxBR1.BRDDEC) the current destination
      --  address (GPDMA_CxDAR) at the end of a block transfer. 	Note: A block
      --  repeated destination address offset must be aligned with the
      --  programmed data width of a destination burst (BRDAO[2:0] versus
      --  GPDMA_CxTR1.DDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued.
      BRDAO : GPDMA_C14BR2_BRDAO_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C14BR2_Register use record
      BRSAO at 0 range 0 .. 15;
      BRDAO at 0 range 16 .. 31;
   end record;

   subtype GPDMA_C14LLR_LA_Field is HAL.UInt14;

   --  GPDMA channel 14 alternate linked-list address register
   type GPDMA_C14LLR_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  pointer (16-bit low-significant address) to the next linked-list data
      --  structure 	If UT1 = UT2 = UB1 = USA = UDA = ULL = 0 and
      --  if LA[15:20] = 0, the current LLI is the last one. The channel
      --  transfer is completed without any update of the linked-list GPDMA
      --  register file. 	Else, this field is the pointer to the memory address
      --  offset from which the next linked-list data structure is
      --  automatically fetched from, once the data transfer is completed, in
      --  order to conditionally update the linked-list GPDMA internal register
      --  file (GPDMA_CxCTR1, GPDMA_CxTR2, GPDMA_CxBR1, GPDMA_CxSAR,
      --  GPDMA_CxDAR and GPDMA_CxLLR). 	Note: The user must program the
      --  pointer to be 32-bit aligned. The two low-significant bits are write
      --  ignored.
      LA             : GPDMA_C14LLR_LA_Field := 16#0#;
      --  Update GPDMA_CxLLR register from memory 	This bit is used to control
      --  the update of GPDMA_CxLLR from the memory during the link transfer.
      ULL            : Boolean := False;
      --  unspecified
      Reserved_17_24 : HAL.UInt8 := 16#0#;
      --  Update GPDMA_CxBR2 from memory 	This bit controls the update of
      --  GPDMA_CxBR2 from the memory during the link transfer.
      UB2            : Boolean := False;
      --  Update GPDMA_CxTR3 from memory 	This bit controls the update of
      --  GPDMA_CxTR3 from the memory during the link transfer.
      UT3            : Boolean := False;
      --  Update GPDMA_CxDAR register from memory 	This bit is used to control
      --  the update of GPDMA_CxDAR from the memory during the link transfer.
      UDA            : Boolean := False;
      --  update GPDMA_CxSAR from memory 	This bit controls the update of
      --  GPDMA_CxSAR from the memory during the link transfer.
      USA            : Boolean := False;
      --  Update GPDMA_CxBR1 from memory 	This bit controls the update of
      --  GPDMA_CxBR1 from the memory during the link transfer. If UB1 = 0
      --  and if GPDMA_CxLLR ≠ 0, the linked-list is not completed.
      --  GPDMA_CxBR1.BNDT[15:0] is then restored to the programmed value after
      --  data transfer is completed and before the link transfer.
      UB1            : Boolean := False;
      --  Update GPDMA_CxTR2 from memory 	This bit controls the update of
      --  GPDMA_CxTR2 from the memory during the link transfer.
      UT2            : Boolean := False;
      --  Update GPDMA_CxTR1 from memory 	This bit controls the update of
      --  GPDMA_CxTR1 from the memory during the link transfer.
      UT1            : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C14LLR_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      LA             at 0 range 2 .. 15;
      ULL            at 0 range 16 .. 16;
      Reserved_17_24 at 0 range 17 .. 24;
      UB2            at 0 range 25 .. 25;
      UT3            at 0 range 26 .. 26;
      UDA            at 0 range 27 .. 27;
      USA            at 0 range 28 .. 28;
      UB1            at 0 range 29 .. 29;
      UT2            at 0 range 30 .. 30;
      UT1            at 0 range 31 .. 31;
   end record;

   subtype GPDMA_C15LBAR_LBA_Field is HAL.UInt16;

   --  GPDMA channel 15 linked-list base address register
   type GPDMA_C15LBAR_Register is record
      --  unspecified
      Reserved_0_15 : HAL.UInt16 := 16#0#;
      --  linked-list base address of GPDMA channel x
      LBA           : GPDMA_C15LBAR_LBA_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C15LBAR_Register use record
      Reserved_0_15 at 0 range 0 .. 15;
      LBA           at 0 range 16 .. 31;
   end record;

   --  GPDMA channel 15 flag clear register
   type GPDMA_C15FCR_Register is record
      --  unspecified
      Reserved_0_7   : HAL.UInt8 := 16#0#;
      --  Write-only. transfer complete flag clear
      TCF            : Boolean := False;
      --  Write-only. half transfer flag clear
      HTF            : Boolean := False;
      --  Write-only. data transfer error flag clear
      DTEF           : Boolean := False;
      --  Write-only. update link transfer error flag clear
      ULEF           : Boolean := False;
      --  Write-only. user setting error flag clear
      USEF           : Boolean := False;
      --  Write-only. completed suspension flag clear
      SUSPF          : Boolean := False;
      --  Write-only. trigger overrun flag clear
      TOF            : Boolean := False;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C15FCR_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      TCF            at 0 range 8 .. 8;
      HTF            at 0 range 9 .. 9;
      DTEF           at 0 range 10 .. 10;
      ULEF           at 0 range 11 .. 11;
      USEF           at 0 range 12 .. 12;
      SUSPF          at 0 range 13 .. 13;
      TOF            at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype GPDMA_C15SR_FIFOL_Field is HAL.UInt8;

   --  GPDMA channel 15 status register
   type GPDMA_C15SR_Register is record
      --  Read-only. idle flag 	This idle flag is de-asserted by hardware when
      --  the channel is enabled (GPDMA_CxCR.EN = 1) with a valid channel
      --  configuration (no USEF to be immediately reported). 	This idle flag
      --  is asserted after hard reset or by hardware when the channel is back
      --  in idle state (in suspended or disabled state).
      IDLEF          : Boolean;
      --  unspecified
      Reserved_1_7   : HAL.UInt7;
      --  Read-only. transfer complete flag 	A transfer complete event is
      --  either a block transfer complete, a 2D/repeated block transfer
      --  complete, a LLI transfer complete including the upload of the next
      --  LLI if any, or the full linked-list completion, depending on the
      --  transfer complete event mode (GPDMA_CxTR2.TCEM[1:0]).
      TCF            : Boolean;
      --  Read-only. half transfer flag 	An half transfer event is either an
      --  half block transfer or an half 2D/repeated block transfer, depending
      --  on the transfer complete event mode (GPDMA_CxTR2.TCEM[1:0]). 	An half
      --  block transfer occurs when half of the bytes of the source block size
      --  (rounded up integer of GPDMA_CxBR1.BNDT[15:0]/2) has been transferred
      --  to the destination. 	An half 2D/repeated block transfer occurs when
      --  half of the repeated blocks (rounded up integer of
      --  (GPDMA_CxBR1.BRC[10:0]+1)/2)) has been transferred to the
      --  destination.
      HTF            : Boolean;
      --  Read-only. data transfer error flag
      DTEF           : Boolean;
      --  Read-only. update link transfer error flag
      ULEF           : Boolean;
      --  Read-only. user setting error flag
      USEF           : Boolean;
      --  Read-only. completed suspension flag
      SUSPF          : Boolean;
      --  Read-only. trigger overrun flag
      TOF            : Boolean;
      --  unspecified
      Reserved_15_15 : HAL.Bit;
      --  Read-only. monitored FIFO level 	Number of available write beats in
      --  the FIFO, in units of the programmed destination data width (see
      --  GPDMA_CxTR1.DDW_LOG2[1:0], in units of bytes, half-words, or words).
      --  	Note: After having suspended an active transfer, the user may need
      --  to read FIFOL[7:0], additionally to GPDMA_CxBR1.BDNT[15:0] and
      --  GPDMA_CxBR1.BRC[10:0], to know how many data have been transferred to
      --  the destination. Before reading, the user may wait for the transfer
      --  to be suspended (GPDMA_CxSR.SUSPF = 1).
      FIFOL          : GPDMA_C15SR_FIFOL_Field;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C15SR_Register use record
      IDLEF          at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      TCF            at 0 range 8 .. 8;
      HTF            at 0 range 9 .. 9;
      DTEF           at 0 range 10 .. 10;
      ULEF           at 0 range 11 .. 11;
      USEF           at 0 range 12 .. 12;
      SUSPF          at 0 range 13 .. 13;
      TOF            at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      FIFOL          at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype GPDMA_C15CR_PRIO_Field is HAL.UInt2;

   --  GPDMA channel 15 control register
   type GPDMA_C15CR_Register is record
      --  enable 	Writing 1 into the field RESET (bit 1) causes the hardware to
      --  de-assert this bit, whatever is written into this bit 0. Else: 	this
      --  bit is de-asserted by hardware when there is a transfer error (master
      --  bus error or user setting error) or when there is a channel transfer
      --  complete (channel ready to be configured, e.g. if LSM=1 at the end of
      --  a single execution of the LLI). 	Else, this bit can be asserted by
      --  software. 	Writing 0 into this EN bit is ignored.
      EN             : Boolean := False;
      --  Write-only. reset 	This bit is write only. Writing 0 has no impact.
      --  Writing 1 implies the reset of the following: the FIFO, the channel
      --  internal state, SUSP and EN bits (whatever is written receptively in
      --  bit 2 and bit 0). 	The reset is effective when the channel is in
      --  steady state, meaning one of the following: 	- active channel in
      --  suspended state (GPDMA_CxSR.SUSPF = 1 and
      --  GPDMA_CxSR.IDLEF = GPDMA_CxCR.EN = 1) 	- channel in disabled
      --  state (GPDMA_CxSR.IDLEF = 1 and GPDMA_CxCR.EN = 0). 	After
      --  writing a RESET, to continue using this channel, the user must
      --  explicitly reconfigure the channel including the hardware-modified
      --  configuration registers (GPDMA_CxBR1, GPDMA_CxSAR and GPDMA_CxDAR)
      --  before enabling again the channel (see the programming sequence in ).
      RESET          : Boolean := False;
      --  suspend 	Writing 1 into the field RESET (bit 1) causes the hardware
      --  to de-assert this bit, whatever is written into this bit 2. Else:
      --  	Software must write 1 in order to suspend an active channel i.e. a
      --  channel with an on-going GPDMA transfer over its master ports. 	The
      --  software must write 0 in order to resume a suspended channel,
      --  following the programming sequence detailed in .
      SUSP           : Boolean := False;
      --  unspecified
      Reserved_3_7   : HAL.UInt5 := 16#0#;
      --  transfer complete interrupt enable
      TCIE           : Boolean := False;
      --  half transfer complete interrupt enable
      HTIE           : Boolean := False;
      --  data transfer error interrupt enable
      DTEIE          : Boolean := False;
      --  update link transfer error interrupt enable
      ULEIE          : Boolean := False;
      --  user setting error interrupt enable
      USEIE          : Boolean := False;
      --  completed suspension interrupt enable
      SUSPIE         : Boolean := False;
      --  trigger overrun interrupt enable
      TOIE           : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Link step mode 	First the (possible 1D/repeated) block transfer is
      --  executed as defined by the current internal register file until
      --  GPDMA_CxBR1.BNDT[15:0] = 0 and GPDMA_CxBR1.BRC[10:0] = 0 if
      --  present. Secondly the next linked-list data structure is
      --  conditionally uploaded from memory as defined by GPDMA_CxLLR. Then
      --  channel execution is completed. 	Note: This bit must be written when
      --  EN=0. This bit is read-only when EN=1.
      LSM            : Boolean := False;
      --  linked-list allocated port 	This bit is used to allocate the master
      --  port for the update of the GPDMA linked-list registers from the
      --  memory. 	Note: This bit must be written when EN=0. This bit is
      --  read-only when EN=1.
      LAP            : Boolean := False;
      --  unspecified
      Reserved_18_21 : HAL.UInt4 := 16#0#;
      --  priority level of the channel x GPDMA transfer versus others 	Note:
      --  This bit must be written when EN = 0. This bit is read-only when
      --  EN = 1.
      PRIO           : GPDMA_C15CR_PRIO_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C15CR_Register use record
      EN             at 0 range 0 .. 0;
      RESET          at 0 range 1 .. 1;
      SUSP           at 0 range 2 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      TCIE           at 0 range 8 .. 8;
      HTIE           at 0 range 9 .. 9;
      DTEIE          at 0 range 10 .. 10;
      ULEIE          at 0 range 11 .. 11;
      USEIE          at 0 range 12 .. 12;
      SUSPIE         at 0 range 13 .. 13;
      TOIE           at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      LSM            at 0 range 16 .. 16;
      LAP            at 0 range 17 .. 17;
      Reserved_18_21 at 0 range 18 .. 21;
      PRIO           at 0 range 22 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype GPDMA_C15TR1_SDW_LOG2_Field is HAL.UInt2;
   subtype GPDMA_C15TR1_SBL_1_Field is HAL.UInt6;
   subtype GPDMA_C15TR1_PAM_Field is HAL.UInt2;
   subtype GPDMA_C15TR1_DDW_LOG2_Field is HAL.UInt2;
   subtype GPDMA_C15TR1_DBL_1_Field is HAL.UInt6;

   --  GPDMA channel 15 transfer register 1
   type GPDMA_C15TR1_Register is record
      --  binary logarithm of the source data width of a burst in bytes 	Note:
      --  Setting a 8-byte data width causes a user setting error to be
      --  reported and no transfer is issued. 	A source block size must be a
      --  multiple of the source data width (GPDMA_CxBR1.BNDT[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and no
      --  transfer is issued. 	A source single transfer must have an aligned
      --  address with its data width (start address GPDMA_CxSAR[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and none
      --  transfer is issued.
      SDW_LOG2       : GPDMA_C15TR1_SDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  source incrementing burst 	The source address, pointed by
      --  GPDMA_CxSAR, is kept constant after a burst beat/single transfer or
      --  is incremented by the offset value corresponding to a contiguous data
      --  after a burst beat/single transfer.
      SINC           : Boolean := False;
      --  source burst length minus 1, between 0 and 63 	The burst length unit
      --  is one data named beat within a burst. If SBL_1[5:0] =0 , the burst
      --  can be named as single. Each data/beat has a width defined by the
      --  destination data width SDW_LOG2[1:0]. 	Note: If a burst transfer
      --  crossed a 1-Kbyte address boundary on a AHB transfer, the GPDMA
      --  modifies and shortens the programmed burst into singles or bursts of
      --  lower length, to be compliant with the AHB protocol. 	If a burst
      --  transfer is of length greater than the FIFO size of the channel x,
      --  the GPDMA modifies and shortens the programmed burst into singles or
      --  bursts of lower length, to be compliant with the FIFO size. Transfer
      --  performance is lower, with GPDMA re-arbitration between effective and
      --  lower bursts/singles, but the data integrity is guaranteed.
      SBL_1          : GPDMA_C15TR1_SBL_1_Field := 16#0#;
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  padding/alignment mode 	If DDW_LOG2[1:0] = SDW_LOG2[1:0]: if the data
      --  width of a burst destination transfer is equal to the data width of a
      --  burst source transfer, these bits are ignored. 	Else: 	- Case 1: If
      --  destination data width > source data width 	1x: successive source
      --  data are FIFO queued and packed at the destination data width, in a
      --  left (LSB) to right (MSB) order (named little endian), before a
      --  destination transfer 	- Case 2: If destination data width < source
      --  data width 	1x: source data is FIFO queued and unpacked at the
      --  destination data width, to be transferred in a left (LSB) to right
      --  (MSB) order (named little endian) to the destination 	Note:
      PAM            : GPDMA_C15TR1_PAM_Field := 16#0#;
      --  source byte exchange within the unaligned half-word of each source
      --  word 	If the source data width is shorter than a word, this bit is
      --  ignored. 	If the source data width is a word:
      SBX            : Boolean := False;
      --  source allocated port 	This bit is used to allocate the master port
      --  for the source transfer 	Note: This bit must be written when
      --  EN = 0. This bit is read-only when EN = 1.
      SAP            : Boolean := False;
      --  security attribute of the GPDMA transfer from the source 	If
      --  GPDMA_SECCFGR.SECx = 1 and the access is secure: 	This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  GPDMA_SECCFGR.SECx =1 . A secure write is ignored when
      --  GPDMA_SECCFGR.SECx = 0. 	When GPDMA_SECCFGR.SECx is de-asserted,
      --  this SSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the GPDMA transfer
      --  from the source is non-secure.
      SSEC           : Boolean := False;
      --  binary logarithm of the destination data width of a burst, in bytes
      --  	Note: Setting a 8-byte data width causes a user setting error to be
      --  reported and none transfer is issued. 	A destination burst transfer
      --  must have an aligned address with its data width (start address
      --  GPDMA_CxDAR[2:0] and address offset GPDMA_CxTR3.DAO[2:0], versus
      --  DDW_LOG2[1:0]). Otherwise a user setting error is reported and no
      --  transfer is issued.
      DDW_LOG2       : GPDMA_C15TR1_DDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_18_18 : HAL.Bit := 16#0#;
      --  destination incrementing burst 	The destination address, pointed by
      --  GPDMA_CxDAR, is kept constant after a burst beat/single transfer, or
      --  is incremented by the offset value corresponding to a contiguous data
      --  after a burst beat/single transfer.
      DINC           : Boolean := False;
      --  destination burst length minus 1, between 0 and 63 	The burst length
      --  unit is one data named beat within a burst. If DBL_1[5:0] =0 , the
      --  burst can be named as single. Each data/beat has a width defined by
      --  the destination data width DDW_LOG2[1:0]. 	Note: If a burst transfer
      --  crossed a 1-Kbyte address boundary on a AHB transfer, the GPDMA
      --  modifies and shortens the programmed burst into singles or bursts of
      --  lower length, to be compliant with the AHB protocol. 	If a burst
      --  transfer is of length greater than the FIFO size of the channel x,
      --  the GPDMA modifies and shortens the programmed burst into singles or
      --  bursts of lower length, to be compliant with the FIFO size. Transfer
      --  performance is lower, with GPDMA re-arbitration between effective and
      --  lower bursts/singles, but the data integrity is guaranteed.
      DBL_1          : GPDMA_C15TR1_DBL_1_Field := 16#0#;
      --  destination byte exchange 	If the destination data size is a byte,
      --  this bit is ignored. 	If the destination data size is not a byte:
      DBX            : Boolean := False;
      --  destination half-word exchange 	If the destination data size is
      --  shorter than a word, this bit is ignored. 	If the destination data
      --  size is a word:
      DHX            : Boolean := False;
      --  unspecified
      Reserved_28_29 : HAL.UInt2 := 16#0#;
      --  destination allocated port 	This bit is used to allocate the master
      --  port for the destination transfer 	Note: This bit must be written
      --  when EN = 0. This bit is read-only when EN = 1.
      DAP            : Boolean := False;
      --  security attribute of the GPDMA transfer to the destination 	If
      --  GPDMA_SECCFGR.SECx = 1 and the access is secure: 	This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  GPDMA_SECCFGR.SECx = 1. A secure write is ignored when
      --  GPDMA_SECCFGR.SECx = 0. 	When GPDMA_SECCFGR.SECx is de-asserted,
      --  this DSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the GPDMA transfer
      --  to the destination is non-secure.
      DSEC           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C15TR1_Register use record
      SDW_LOG2       at 0 range 0 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      SINC           at 0 range 3 .. 3;
      SBL_1          at 0 range 4 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      PAM            at 0 range 11 .. 12;
      SBX            at 0 range 13 .. 13;
      SAP            at 0 range 14 .. 14;
      SSEC           at 0 range 15 .. 15;
      DDW_LOG2       at 0 range 16 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      DINC           at 0 range 19 .. 19;
      DBL_1          at 0 range 20 .. 25;
      DBX            at 0 range 26 .. 26;
      DHX            at 0 range 27 .. 27;
      Reserved_28_29 at 0 range 28 .. 29;
      DAP            at 0 range 30 .. 30;
      DSEC           at 0 range 31 .. 31;
   end record;

   subtype GPDMA_C15TR2_REQSEL_Field is HAL.UInt7;
   subtype GPDMA_C15TR2_TRIGM_Field is HAL.UInt2;
   subtype GPDMA_C15TR2_TRIGSEL_Field is HAL.UInt6;
   subtype GPDMA_C15TR2_TRIGPOL_Field is HAL.UInt2;
   subtype GPDMA_C15TR2_TCEM_Field is HAL.UInt2;

   --  GPDMA channel 15 transfer register 2
   type GPDMA_C15TR2_Register is record
      --  GPDMA hardware request selection 	These bits are ignored if channel x
      --  is activated (GPDMA_CxCR.EN asserted) with SWREQ = 1 (software
      --  request for a memory-to-memory transfer). Else, the selected hardware
      --  request is internally taken into account as per . 	The user must not
      --  assign a same input hardware request (same REQSEL[6:0] value) to
      --  different active GPDMA channels (GPDMA_CxCR.EN = 1 and
      --  GPDMA_CxTR2.SWREQ = 0 for these channels). GPDMA is not intended to
      --  hardware support the case of simultaneous enabled channels
      --  incorrectly configured with a same hardware peripheral request
      --  signal, and there is no user setting error reporting.
      REQSEL         : GPDMA_C15TR2_REQSEL_Field := 16#0#;
      --  unspecified
      Reserved_7_8   : HAL.UInt2 := 16#0#;
      --  software request 	This bit is internally taken into account when
      --  GPDMA_CxCR.EN is asserted.
      SWREQ          : Boolean := False;
      --  destination hardware request 	This bit is ignored if channel x is
      --  activated (GPDMA_CxCR.EN asserted) with SWREQ = 1 (software request
      --  for a memory-to-memory transfer). Else: 	Note:
      DREQ           : Boolean := False;
      --  Block hardware request 	If the channel x is activated (GPDMA_CxCR.EN
      --  asserted) with SWREQ = 1 (software request for a memory-to-memory
      --  transfer), this bit is ignored. Else:
      BREQ           : Boolean := False;
      --  unspecified
      Reserved_12_13 : HAL.UInt2 := 16#0#;
      --  trigger mode 	These bits define the transfer granularity for its
      --  conditioning by the trigger. 	If the channel x is enabled
      --  (GPDMA_CxCR.EN asserted) with TRIGPOL[1:0] = 00 or 11, these
      --  TRIGM[1:0] bits are ignored. 	Else, a GPDMA transfer is conditioned
      --  by at least one trigger hit: 	first burst read of a 2D/repeated block
      --  transfer is conditioned by one hit trigger. 	– If the peripheral is
      --  programmed as a source (DREQ = 0) of the LLI data transfer, each
      --  programmed burst read is conditioned. 	– If the peripheral is
      --  programmed as a destination (DREQ = 1) of the LLI data transfer,
      --  each programmed burst write is conditioned. The first memory burst
      --  read of a (possibly 2D/repeated) block, also named as the first ready
      --  FIFO-based source burst, is gated by the occurrence of both the
      --  hardware request and the first trigger hit. 	The GPDMA monitoring of
      --  a trigger for channel x is started when the channel is enabled/loaded
      --  with a new active trigger configuration: rising or falling edge on a
      --  selected trigger (TRIGPOL[1:0] = 01 or respectively
      --  TRIGPOL[1:0] = 10). 	The monitoring of this trigger is kept active
      --  during the triggered and uncompleted (data or link) transfer; and if
      --  a new trigger is detected then, this hit is internally memorized to
      --  grant the next transfer, as long as the defined rising or falling
      --  edge is not modified, and the TRIGSEL[5:0] is not modified, and the
      --  channel is enabled. 	Transferring a next LLIn+1 that updates the
      --  GPDMA_CxTR2 with a new value for any of TRIGSEL[5:0] or TRIGPOL[1:0],
      --  resets the monitoring, trashing the memorized hit of the formerly
      --  defined LLIn trigger. 	After a first new trigger hitn+1 is memorized,
      --  if another second trigger hitn+2 is detected and if the hitn
      --  triggered transfer is still not completed, hitn+2 is lost and not
      --  memorized.memorized. A trigger overrun flag is reported
      --  (GPDMA_CxSR.TOF =1 ), and an interrupt is generated if enabled
      --  (GPDMA_CxCR.TOIE = 1). The channel is not automatically disabled by
      --  hardware due to a trigger overrun. 	Note: When the source block size
      --  is not a multiple of the source burst size and is a multiple of the
      --  source data width, then the last programmed source burst is not
      --  completed and is internally shorten to match the block size. In this
      --  case, if TRIGM[1:0] = 11 and (SWREQ =1  or (SWREQ = 0 and
      --  DREQ =0 )), the shortened burst transfer (by singles or/and by
      --  bursts of lower length) is conditioned once by the trigger. 	When the
      --  programmed destination burst is internally shortened by singles
      --  or/and by bursts of lower length (versus FIFO size, versus block
      --  size, 1-Kbyte boundary address crossing): if the trigger is
      --  conditioning the programmed destination burst (if TRIGM[1:0] = 11
      --  and SWREQ = 0 and DREQ = 1), this shortened destination burst
      --  transfer is conditioned once by the trigger.
      TRIGM          : GPDMA_C15TR2_TRIGM_Field := 16#0#;
      --  trigger event input selection 	These bits select the trigger event
      --  input of the GPDMA transfer (as per ), with an active trigger event
      --  if TRIGPOL[1:0] ≠ 00.
      TRIGSEL        : GPDMA_C15TR2_TRIGSEL_Field := 16#0#;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  trigger event polarity 	These bits define the polarity of the
      --  selected trigger event input defined by TRIGSEL[5:0].
      TRIGPOL        : GPDMA_C15TR2_TRIGPOL_Field := 16#0#;
      --  unspecified
      Reserved_26_29 : HAL.UInt4 := 16#0#;
      --  transfer complete event mode 	These bits define the transfer
      --  granularity for the transfer complete and half transfer complete
      --  events generation. 	Note: If the initial LLI0 data transfer is
      --  null/void (directly programmed by the internal register file with
      --  GPDMA_CxBR1.BNDT[15:0] = 0), then neither the complete transfer
      --  event nor the half transfer event is generated. 	Note: If the initial
      --  LLI0 data transfer is null/void (directly programmed by the internal
      --  register file with GPDMA_CxBR1.BNDT[15:0] = 0), then neither the
      --  complete transfer event nor the half transfer event is generated.
      --  	Note: If the initial LLI0 data transfer is null/void (i.e. directly
      --  programmed by the internal register file with
      --  GPDMA_CxBR1.BNDT[15:0] =0 ), then the half transfer event is not
      --  generated, and the transfer complete event is generated when is
      --  completed the loading of the LLI1.
      TCEM           : GPDMA_C15TR2_TCEM_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C15TR2_Register use record
      REQSEL         at 0 range 0 .. 6;
      Reserved_7_8   at 0 range 7 .. 8;
      SWREQ          at 0 range 9 .. 9;
      DREQ           at 0 range 10 .. 10;
      BREQ           at 0 range 11 .. 11;
      Reserved_12_13 at 0 range 12 .. 13;
      TRIGM          at 0 range 14 .. 15;
      TRIGSEL        at 0 range 16 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      TRIGPOL        at 0 range 24 .. 25;
      Reserved_26_29 at 0 range 26 .. 29;
      TCEM           at 0 range 30 .. 31;
   end record;

   subtype GPDMA_C15BR1_BNDT_Field is HAL.UInt16;
   subtype GPDMA_C15BR1_BRC_Field is HAL.UInt11;

   --  GPDMA channel 15 alternate block register 1
   type GPDMA_C15BR1_Register is record
      --  block number of data bytes to transfer from the source 	Block size
      --  transferred from the source. When the channel is enabled, this field
      --  becomes read-only and is decremented, indicating the remaining number
      --  of data items in the current source block to be transferred.
      --  BNDT[15:0] is programmed in number of bytes, maximum source block
      --  size is 64 Kbytes -1. 	Once the last data transfer is completed
      --  (BNDT[15:0] = 0): 	- if GPDMA_CxLLR.UB1 = 1, this field is
      --  updated by the LLI in the memory. 	- if GPDMA_CxLLR.UB1 = 0 and if
      --  there is at least one not null Uxx update bit, this field is
      --  internally restored to the programmed value. 	- if all
      --  GPDMA_CxLLR.Uxx = 0 and if GPDMA_CxLLR.LA[15:0] ≠ 0, this field
      --  is internally restored to the programmed value (infinite/continuous
      --  last LLI). 	- if GPDMA_CxLLR = 0, this field is kept as zero
      --  following the last LLI data transfer. 	Note: A non-null source block
      --  size must be a multiple of the source data width (BNDT[2:0] versus
      --  GPDMA_CxTR1.SDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued. 	When configured in packing mode
      --  (GPDMA_CxTR1.PAM[1]=1 and destination data width different from
      --  source data width), a non-null source block size must be a multiple
      --  of the destination data width (BNDT[2:0] versus
      --  GPDMA_CxTR1.DDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued.
      BNDT           : GPDMA_C15BR1_BNDT_Field := 16#0#;
      --  Block repeat counter 	This field contains the number of repetitions
      --  of the current block (0 to 2047). 	When the channel is enabled, this
      --  field becomes read-only. After decrements, this field indicates the
      --  remaining number of blocks, excluding the current one. This counter
      --  is hardware decremented for each completed block transfer. 	Once the
      --  last block transfer is completed (BRC[10:0] = BNDT[15:0] = 0):
      --  	If GPDMA_CxLLR.UB1 = 1, all GPDMA_CxBR1 fields are updated by the
      --  next LLI in the memory. 	If GPDMA_CxLLR.UB1 = 0 and if there is at
      --  least one not null Uxx update bit, this field is internally restored
      --  to the programmed value. 	if all GPDMA_CxLLR.Uxx = 0 and if
      --  GPDMA_CxLLR.LA[15:0] ≠ 0, this field is internally restored to
      --  the programmed value (infinite/continuous last LLI). 	if
      --  GPDMA_CxLLR = 0, this field is kept as zero following the last LLI
      --  and data transfer.
      BRC            : GPDMA_C15BR1_BRC_Field := 16#0#;
      --  unspecified
      Reserved_27_27 : HAL.Bit := 16#0#;
      --  source address decrement
      SDEC           : Boolean := False;
      --  destination address decrement
      DDEC           : Boolean := False;
      --  Block repeat source address decrement 	Note: On top of this
      --  increment/decrement (depending on BRSDEC), GPDMA_CxSAR is in the same
      --  time also updated by the increment/decrement (depending on SDEC) of
      --  the GPDMA_CxTR3.SAO value, as it is done after any programmed burst
      --  transfer.
      BRSDEC         : Boolean := False;
      --  Block repeat destination address decrement 	Note: On top of this
      --  increment/decrement (depending on BRDDEC), GPDMA_CxDAR is in the same
      --  time also updated by the increment/decrement (depending on DDEC) of
      --  the GPDMA_CxTR3.DAO value, as it is usually done at the end of each
      --  programmed burst transfer.
      BRDDEC         : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C15BR1_Register use record
      BNDT           at 0 range 0 .. 15;
      BRC            at 0 range 16 .. 26;
      Reserved_27_27 at 0 range 27 .. 27;
      SDEC           at 0 range 28 .. 28;
      DDEC           at 0 range 29 .. 29;
      BRSDEC         at 0 range 30 .. 30;
      BRDDEC         at 0 range 31 .. 31;
   end record;

   subtype GPDMA_C15TR3_SAO_Field is HAL.UInt13;
   subtype GPDMA_C15TR3_DAO_Field is HAL.UInt13;

   --  GPDMA channel 15 transfer register 3
   type GPDMA_C15TR3_Register is record
      --  source address offset increment 	The source address, pointed by
      --  GPDMA_CxSAR, is incremented or decremented (depending on
      --  GPDMA_CxBR1.SDEC) by this offset SAO[12:0] for each programmed source
      --  burst. This offset is not including and is added to the programmed
      --  burst size when the completed burst is addressed in incremented mode
      --  (GPDMA_CxTR1.SINC = 1). 	Note: A source address offset must be
      --  aligned with the programmed data width of a source burst (SAO[2:0]
      --  versus GPDMA_CxTR1.SDW_LOG2[1:0]). Else a user setting error is
      --  reported and none transfer is issued. 	When the source block size is
      --  not a multiple of the destination burst size and is a multiple of the
      --  source data width, then the last programmed source burst is not
      --  completed and is internally shorten to match the block size. In this
      --  case, the additional GPDMA_CxTR3.SAO[12:0] is not applied.
      SAO            : GPDMA_C15TR3_SAO_Field := 16#0#;
      --  unspecified
      Reserved_13_15 : HAL.UInt3 := 16#0#;
      --  destination address offset increment 	The destination address,
      --  pointed by GPDMA_CxDAR, is incremented or decremented (depending on
      --  GPDMA_CxBR1.DDEC) by this offset DAO[12:0] for each programmed
      --  destination burst. This offset is not including and is added to the
      --  programmed burst size when the completed burst is addressed in
      --  incremented mode (GPDMA_CxTR1.DINC = 1). 	Note: A destination
      --  address offset must be aligned with the programmed data width of a
      --  destination burst (DAO[2:0] versus GPDMA_CxTR1.DDW_LOG2[1:0]). Else,
      --  a user setting error is reported and no transfer is issued.
      DAO            : GPDMA_C15TR3_DAO_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C15TR3_Register use record
      SAO            at 0 range 0 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      DAO            at 0 range 16 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype GPDMA_C15BR2_BRSAO_Field is HAL.UInt16;
   subtype GPDMA_C15BR2_BRDAO_Field is HAL.UInt16;

   --  GPDMA channel 15 block register 2
   type GPDMA_C15BR2_Register is record
      --  Block repeated source address offset 	For a channel with 2D
      --  addressing capability, this field is used to update (by addition or
      --  subtraction depending on GPDMA_CxBR1.BRSDEC) the current source
      --  address (GPDMA_CxSAR) at the end of a block transfer. 	Note: A block
      --  repeated source address offset must be aligned with the programmed
      --  data width of a source burst (BRSAO[2:0] versus
      --  GPDMA_CxTR1.SDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued.
      BRSAO : GPDMA_C15BR2_BRSAO_Field := 16#0#;
      --  Block repeated destination address offset 	For a channel with 2D
      --  addressing capability, this field is used to update (by addition or
      --  subtraction depending on GPDMA_CxBR1.BRDDEC) the current destination
      --  address (GPDMA_CxDAR) at the end of a block transfer. 	Note: A block
      --  repeated destination address offset must be aligned with the
      --  programmed data width of a destination burst (BRDAO[2:0] versus
      --  GPDMA_CxTR1.DDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued.
      BRDAO : GPDMA_C15BR2_BRDAO_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C15BR2_Register use record
      BRSAO at 0 range 0 .. 15;
      BRDAO at 0 range 16 .. 31;
   end record;

   subtype GPDMA_C15LLR_LA_Field is HAL.UInt14;

   --  GPDMA channel 15 alternate linked-list address register
   type GPDMA_C15LLR_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  pointer (16-bit low-significant address) to the next linked-list data
      --  structure 	If UT1 = UT2 = UB1 = USA = UDA = ULL = 0 and
      --  if LA[15:20] = 0, the current LLI is the last one. The channel
      --  transfer is completed without any update of the linked-list GPDMA
      --  register file. 	Else, this field is the pointer to the memory address
      --  offset from which the next linked-list data structure is
      --  automatically fetched from, once the data transfer is completed, in
      --  order to conditionally update the linked-list GPDMA internal register
      --  file (GPDMA_CxCTR1, GPDMA_CxTR2, GPDMA_CxBR1, GPDMA_CxSAR,
      --  GPDMA_CxDAR and GPDMA_CxLLR). 	Note: The user must program the
      --  pointer to be 32-bit aligned. The two low-significant bits are write
      --  ignored.
      LA             : GPDMA_C15LLR_LA_Field := 16#0#;
      --  Update GPDMA_CxLLR register from memory 	This bit is used to control
      --  the update of GPDMA_CxLLR from the memory during the link transfer.
      ULL            : Boolean := False;
      --  unspecified
      Reserved_17_24 : HAL.UInt8 := 16#0#;
      --  Update GPDMA_CxBR2 from memory 	This bit controls the update of
      --  GPDMA_CxBR2 from the memory during the link transfer.
      UB2            : Boolean := False;
      --  Update GPDMA_CxTR3 from memory 	This bit controls the update of
      --  GPDMA_CxTR3 from the memory during the link transfer.
      UT3            : Boolean := False;
      --  Update GPDMA_CxDAR register from memory 	This bit is used to control
      --  the update of GPDMA_CxDAR from the memory during the link transfer.
      UDA            : Boolean := False;
      --  update GPDMA_CxSAR from memory 	This bit controls the update of
      --  GPDMA_CxSAR from the memory during the link transfer.
      USA            : Boolean := False;
      --  Update GPDMA_CxBR1 from memory 	This bit controls the update of
      --  GPDMA_CxBR1 from the memory during the link transfer. If UB1 = 0
      --  and if GPDMA_CxLLR ≠ 0, the linked-list is not completed.
      --  GPDMA_CxBR1.BNDT[15:0] is then restored to the programmed value after
      --  data transfer is completed and before the link transfer.
      UB1            : Boolean := False;
      --  Update GPDMA_CxTR2 from memory 	This bit controls the update of
      --  GPDMA_CxTR2 from the memory during the link transfer.
      UT2            : Boolean := False;
      --  Update GPDMA_CxTR1 from memory 	This bit controls the update of
      --  GPDMA_CxTR1 from the memory during the link transfer.
      UT1            : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPDMA_C15LLR_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      LA             at 0 range 2 .. 15;
      ULL            at 0 range 16 .. 16;
      Reserved_17_24 at 0 range 17 .. 24;
      UB2            at 0 range 25 .. 25;
      UT3            at 0 range 26 .. 26;
      UDA            at 0 range 27 .. 27;
      USA            at 0 range 28 .. 28;
      UB1            at 0 range 29 .. 29;
      UT2            at 0 range 30 .. 30;
      UT1            at 0 range 31 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  GPDMA1
   type GPDMA_Peripheral is record
      --  GPDMA secure configuration register
      GPDMA_SECCFGR   : aliased GPDMA_SECCFGR_Register;
      --  GPDMA privileged configuration register
      GPDMA_PRIVCFGR  : aliased GPDMA_PRIVCFGR_Register;
      --  GPDMA configuration lock register
      GPDMA_RCFGLOCKR : aliased GPDMA_RCFGLOCKR_Register;
      --  GPDMA non-secure masked interrupt status register	
      GPDMA_MISR      : aliased GPDMA_MISR_Register;
      --  GPDMA secure masked interrupt status register
      GPDMA_SMISR     : aliased GPDMA_SMISR_Register;
      --  GPDMA channel 0 linked-list base address register
      GPDMA_C0LBAR    : aliased GPDMA_C0LBAR_Register;
      --  GPDMA channel 0 flag clear register
      GPDMA_C0FCR     : aliased GPDMA_C0FCR_Register;
      --  GPDMA channel 0 status register
      GPDMA_C0SR      : aliased GPDMA_C0SR_Register;
      --  GPDMA channel 0 control register
      GPDMA_C0CR      : aliased GPDMA_C0CR_Register;
      --  GPDMA channel 0 transfer register 1
      GPDMA_C0TR1     : aliased GPDMA_C0TR1_Register;
      --  GPDMA channel 0 transfer register 2
      GPDMA_C0TR2     : aliased GPDMA_C0TR2_Register;
      --  GPDMA channel 0 block register 1
      GPDMA_C0BR1     : aliased GPDMA_C0BR1_Register;
      --  GPDMA channel 0 source address register
      GPDMA_C0SAR     : aliased HAL.UInt32;
      --  GPDMA channel 0 destination address register
      GPDMA_C0DAR     : aliased HAL.UInt32;
      --  GPDMA channel 0 linked-list address register
      GPDMA_C0LLR     : aliased GPDMA_C0LLR_Register;
      --  GPDMA channel 1 linked-list base address register
      GPDMA_C1LBAR    : aliased GPDMA_C1LBAR_Register;
      --  GPDMA channel 1 flag clear register
      GPDMA_C1FCR     : aliased GPDMA_C1FCR_Register;
      --  GPDMA channel 1 status register
      GPDMA_C1SR      : aliased GPDMA_C1SR_Register;
      --  GPDMA channel 1 control register
      GPDMA_C1CR      : aliased GPDMA_C1CR_Register;
      --  GPDMA channel 1 transfer register 1
      GPDMA_C1TR1     : aliased GPDMA_C1TR1_Register;
      --  GPDMA channel 1 transfer register 2
      GPDMA_C1TR2     : aliased GPDMA_C1TR2_Register;
      --  GPDMA channel 1 block register 1
      GPDMA_C1BR1     : aliased GPDMA_C1BR1_Register;
      --  GPDMA channel 1 source address register
      GPDMA_C1SAR     : aliased HAL.UInt32;
      --  GPDMA channel 1 destination address register
      GPDMA_C1DAR     : aliased HAL.UInt32;
      --  GPDMA channel 1 linked-list address register
      GPDMA_C1LLR     : aliased GPDMA_C1LLR_Register;
      --  GPDMA channel 2 linked-list base address register
      GPDMA_C2LBAR    : aliased GPDMA_C2LBAR_Register;
      --  GPDMA channel 2 flag clear register
      GPDMA_C2FCR     : aliased GPDMA_C2FCR_Register;
      --  GPDMA channel 2 status register
      GPDMA_C2SR      : aliased GPDMA_C2SR_Register;
      --  GPDMA channel 2 control register
      GPDMA_C2CR      : aliased GPDMA_C2CR_Register;
      --  GPDMA channel 2 transfer register 1
      GPDMA_C2TR1     : aliased GPDMA_C2TR1_Register;
      --  GPDMA channel 2 transfer register 2
      GPDMA_C2TR2     : aliased GPDMA_C2TR2_Register;
      --  GPDMA channel 2 block register 1
      GPDMA_C2BR1     : aliased GPDMA_C2BR1_Register;
      --  GPDMA channel 2 source address register
      GPDMA_C2SAR     : aliased HAL.UInt32;
      --  GPDMA channel 2 destination address register
      GPDMA_C2DAR     : aliased HAL.UInt32;
      --  GPDMA channel 2 linked-list address register
      GPDMA_C2LLR     : aliased GPDMA_C2LLR_Register;
      --  GPDMA channel 3 linked-list base address register
      GPDMA_C3LBAR    : aliased GPDMA_C3LBAR_Register;
      --  GPDMA channel 3 flag clear register
      GPDMA_C3FCR     : aliased GPDMA_C3FCR_Register;
      --  GPDMA channel 3 status register
      GPDMA_C3SR      : aliased GPDMA_C3SR_Register;
      --  GPDMA channel 3 control register
      GPDMA_C3CR      : aliased GPDMA_C3CR_Register;
      --  GPDMA channel 3 transfer register 1
      GPDMA_C3TR1     : aliased GPDMA_C3TR1_Register;
      --  GPDMA channel 3 transfer register 2
      GPDMA_C3TR2     : aliased GPDMA_C3TR2_Register;
      --  GPDMA channel 3 block register 1
      GPDMA_C3BR1     : aliased GPDMA_C3BR1_Register;
      --  GPDMA channel 3 source address register
      GPDMA_C3SAR     : aliased HAL.UInt32;
      --  GPDMA channel 3 destination address register
      GPDMA_C3DAR     : aliased HAL.UInt32;
      --  GPDMA channel 3 linked-list address register
      GPDMA_C3LLR     : aliased GPDMA_C3LLR_Register;
      --  GPDMA channel 4 linked-list base address register
      GPDMA_C4LBAR    : aliased GPDMA_C4LBAR_Register;
      --  GPDMA channel 4 flag clear register
      GPDMA_C4FCR     : aliased GPDMA_C4FCR_Register;
      --  GPDMA channel 4 status register
      GPDMA_C4SR      : aliased GPDMA_C4SR_Register;
      --  GPDMA channel 4 control register
      GPDMA_C4CR      : aliased GPDMA_C4CR_Register;
      --  GPDMA channel 4 transfer register 1
      GPDMA_C4TR1     : aliased GPDMA_C4TR1_Register;
      --  GPDMA channel 4 transfer register 2
      GPDMA_C4TR2     : aliased GPDMA_C4TR2_Register;
      --  GPDMA channel 4 block register 1
      GPDMA_C4BR1     : aliased GPDMA_C4BR1_Register;
      --  GPDMA channel 4 source address register
      GPDMA_C4SAR     : aliased HAL.UInt32;
      --  GPDMA channel 4 destination address register
      GPDMA_C4DAR     : aliased HAL.UInt32;
      --  GPDMA channel 4 linked-list address register
      GPDMA_C4LLR     : aliased GPDMA_C4LLR_Register;
      --  GPDMA channel 5 linked-list base address register
      GPDMA_C5LBAR    : aliased GPDMA_C5LBAR_Register;
      --  GPDMA channel 5 flag clear register
      GPDMA_C5FCR     : aliased GPDMA_C5FCR_Register;
      --  GPDMA channel 5 status register
      GPDMA_C5SR      : aliased GPDMA_C5SR_Register;
      --  GPDMA channel 5 control register
      GPDMA_C5CR      : aliased GPDMA_C5CR_Register;
      --  GPDMA channel 5 transfer register 1
      GPDMA_C5TR1     : aliased GPDMA_C5TR1_Register;
      --  GPDMA channel 5 transfer register 2
      GPDMA_C5TR2     : aliased GPDMA_C5TR2_Register;
      --  GPDMA channel 5 block register 1
      GPDMA_C5BR1     : aliased GPDMA_C5BR1_Register;
      --  GPDMA channel 5 source address register
      GPDMA_C5SAR     : aliased HAL.UInt32;
      --  GPDMA channel 5 destination address register
      GPDMA_C5DAR     : aliased HAL.UInt32;
      --  GPDMA channel 5 linked-list address register
      GPDMA_C5LLR     : aliased GPDMA_C5LLR_Register;
      --  GPDMA channel 6 linked-list base address register
      GPDMA_C6LBAR    : aliased GPDMA_C6LBAR_Register;
      --  GPDMA channel 6 flag clear register
      GPDMA_C6FCR     : aliased GPDMA_C6FCR_Register;
      --  GPDMA channel 6 status register
      GPDMA_C6SR      : aliased GPDMA_C6SR_Register;
      --  GPDMA channel 6 control register
      GPDMA_C6CR      : aliased GPDMA_C6CR_Register;
      --  GPDMA channel 6 transfer register 1
      GPDMA_C6TR1     : aliased GPDMA_C6TR1_Register;
      --  GPDMA channel 6 transfer register 2
      GPDMA_C6TR2     : aliased GPDMA_C6TR2_Register;
      --  GPDMA channel 6 block register 1
      GPDMA_C6BR1     : aliased GPDMA_C6BR1_Register;
      --  GPDMA channel 6 source address register
      GPDMA_C6SAR     : aliased HAL.UInt32;
      --  GPDMA channel 6 destination address register
      GPDMA_C6DAR     : aliased HAL.UInt32;
      --  GPDMA channel 6 linked-list address register
      GPDMA_C6LLR     : aliased GPDMA_C6LLR_Register;
      --  GPDMA channel 7 linked-list base address register
      GPDMA_C7LBAR    : aliased GPDMA_C7LBAR_Register;
      --  GPDMA channel 7 flag clear register
      GPDMA_C7FCR     : aliased GPDMA_C7FCR_Register;
      --  GPDMA channel 7 status register
      GPDMA_C7SR      : aliased GPDMA_C7SR_Register;
      --  GPDMA channel 7 control register
      GPDMA_C7CR      : aliased GPDMA_C7CR_Register;
      --  GPDMA channel 7 transfer register 1
      GPDMA_C7TR1     : aliased GPDMA_C7TR1_Register;
      --  GPDMA channel 7 transfer register 2
      GPDMA_C7TR2     : aliased GPDMA_C7TR2_Register;
      --  GPDMA channel 7 block register 1
      GPDMA_C7BR1     : aliased GPDMA_C7BR1_Register;
      --  GPDMA channel 7 source address register
      GPDMA_C7SAR     : aliased HAL.UInt32;
      --  GPDMA channel 7 destination address register
      GPDMA_C7DAR     : aliased HAL.UInt32;
      --  GPDMA channel 7 linked-list address register
      GPDMA_C7LLR     : aliased GPDMA_C7LLR_Register;
      --  GPDMA channel 8 linked-list base address register
      GPDMA_C8LBAR    : aliased GPDMA_C8LBAR_Register;
      --  GPDMA channel 8 flag clear register
      GPDMA_C8FCR     : aliased GPDMA_C8FCR_Register;
      --  GPDMA channel 8 status register
      GPDMA_C8SR      : aliased GPDMA_C8SR_Register;
      --  GPDMA channel 8 control register
      GPDMA_C8CR      : aliased GPDMA_C8CR_Register;
      --  GPDMA channel 8 transfer register 1
      GPDMA_C8TR1     : aliased GPDMA_C8TR1_Register;
      --  GPDMA channel 8 transfer register 2
      GPDMA_C8TR2     : aliased GPDMA_C8TR2_Register;
      --  GPDMA channel 8 block register 1
      GPDMA_C8BR1     : aliased GPDMA_C8BR1_Register;
      --  GPDMA channel 8 source address register
      GPDMA_C8SAR     : aliased HAL.UInt32;
      --  GPDMA channel 8 destination address register
      GPDMA_C8DAR     : aliased HAL.UInt32;
      --  GPDMA channel 8 linked-list address register
      GPDMA_C8LLR     : aliased GPDMA_C8LLR_Register;
      --  GPDMA channel 9 linked-list base address register
      GPDMA_C9LBAR    : aliased GPDMA_C9LBAR_Register;
      --  GPDMA channel 9 flag clear register
      GPDMA_C9FCR     : aliased GPDMA_C9FCR_Register;
      --  GPDMA channel 9 status register
      GPDMA_C9SR      : aliased GPDMA_C9SR_Register;
      --  GPDMA channel 9 control register
      GPDMA_C9CR      : aliased GPDMA_C9CR_Register;
      --  GPDMA channel 9 transfer register 1
      GPDMA_C9TR1     : aliased GPDMA_C9TR1_Register;
      --  GPDMA channel 9 transfer register 2
      GPDMA_C9TR2     : aliased GPDMA_C9TR2_Register;
      --  GPDMA channel 9 block register 1
      GPDMA_C9BR1     : aliased GPDMA_C9BR1_Register;
      --  GPDMA channel 9 source address register
      GPDMA_C9SAR     : aliased HAL.UInt32;
      --  GPDMA channel 9 destination address register
      GPDMA_C9DAR     : aliased HAL.UInt32;
      --  GPDMA channel 9 linked-list address register
      GPDMA_C9LLR     : aliased GPDMA_C9LLR_Register;
      --  GPDMA channel 10 linked-list base address register
      GPDMA_C10LBAR   : aliased GPDMA_C10LBAR_Register;
      --  GPDMA channel 10 flag clear register
      GPDMA_C10FCR    : aliased GPDMA_C10FCR_Register;
      --  GPDMA channel 10 status register
      GPDMA_C10SR     : aliased GPDMA_C10SR_Register;
      --  GPDMA channel 10 control register
      GPDMA_C10CR     : aliased GPDMA_C10CR_Register;
      --  GPDMA channel 10 transfer register 1
      GPDMA_C10TR1    : aliased GPDMA_C10TR1_Register;
      --  GPDMA channel 10 transfer register 2
      GPDMA_C10TR2    : aliased GPDMA_C10TR2_Register;
      --  GPDMA channel 10 block register 1
      GPDMA_C10BR1    : aliased GPDMA_C10BR1_Register;
      --  GPDMA channel 10 source address register
      GPDMA_C10SAR    : aliased HAL.UInt32;
      --  GPDMA channel 10 destination address register
      GPDMA_C10DAR    : aliased HAL.UInt32;
      --  GPDMA channel 10 linked-list address register
      GPDMA_C10LLR    : aliased GPDMA_C10LLR_Register;
      --  GPDMA channel 11 linked-list base address register
      GPDMA_C11LBAR   : aliased GPDMA_C11LBAR_Register;
      --  GPDMA channel 11 flag clear register
      GPDMA_C11FCR    : aliased GPDMA_C11FCR_Register;
      --  GPDMA channel 11 status register
      GPDMA_C11SR     : aliased GPDMA_C11SR_Register;
      --  GPDMA channel 11 control register
      GPDMA_C11CR     : aliased GPDMA_C11CR_Register;
      --  GPDMA channel 11 transfer register 1
      GPDMA_C11TR1    : aliased GPDMA_C11TR1_Register;
      --  GPDMA channel 11 transfer register 2
      GPDMA_C11TR2    : aliased GPDMA_C11TR2_Register;
      --  GPDMA channel 11 block register 1
      GPDMA_C11BR1    : aliased GPDMA_C11BR1_Register;
      --  GPDMA channel 11 source address register
      GPDMA_C11SAR    : aliased HAL.UInt32;
      --  GPDMA channel 11 destination address register
      GPDMA_C11DAR    : aliased HAL.UInt32;
      --  GPDMA channel 11 linked-list address register
      GPDMA_C11LLR    : aliased GPDMA_C11LLR_Register;
      --  GPDMA channel 12 linked-list base address register
      GPDMA_C12LBAR   : aliased GPDMA_C12LBAR_Register;
      --  GPDMA channel 12 flag clear register
      GPDMA_C12FCR    : aliased GPDMA_C12FCR_Register;
      --  GPDMA channel 12 status register
      GPDMA_C12SR     : aliased GPDMA_C12SR_Register;
      --  GPDMA channel 12 control register
      GPDMA_C12CR     : aliased GPDMA_C12CR_Register;
      --  GPDMA channel 12 transfer register 1
      GPDMA_C12TR1    : aliased GPDMA_C12TR1_Register;
      --  GPDMA channel 12 transfer register 2
      GPDMA_C12TR2    : aliased GPDMA_C12TR2_Register;
      --  GPDMA channel 12 alternate block register 1
      GPDMA_C12BR1    : aliased GPDMA_C12BR1_Register;
      --  GPDMA channel 12 source address register
      GPDMA_C12SAR    : aliased HAL.UInt32;
      --  GPDMA channel 12 destination address register
      GPDMA_C12DAR    : aliased HAL.UInt32;
      --  GPDMA channel 12 transfer register 3
      GPDMA_C12TR3    : aliased GPDMA_C12TR3_Register;
      --  GPDMA channel 12 block register 2
      GPDMA_C12BR2    : aliased GPDMA_C12BR2_Register;
      --  GPDMA channel 12 alternate linked-list address register
      GPDMA_C12LLR    : aliased GPDMA_C12LLR_Register;
      --  GPDMA channel 13 linked-list base address register
      GPDMA_C13LBAR   : aliased GPDMA_C13LBAR_Register;
      --  GPDMA channel 13 flag clear register
      GPDMA_C13FCR    : aliased GPDMA_C13FCR_Register;
      --  GPDMA channel 13 status register
      GPDMA_C13SR     : aliased GPDMA_C13SR_Register;
      --  GPDMA channel 13 control register
      GPDMA_C13CR     : aliased GPDMA_C13CR_Register;
      --  GPDMA channel 13 transfer register 1
      GPDMA_C13TR1    : aliased GPDMA_C13TR1_Register;
      --  GPDMA channel 13 transfer register 2
      GPDMA_C13TR2    : aliased GPDMA_C13TR2_Register;
      --  GPDMA channel 13 alternate block register 1
      GPDMA_C13BR1    : aliased GPDMA_C13BR1_Register;
      --  GPDMA channel 13 source address register
      GPDMA_C13SAR    : aliased HAL.UInt32;
      --  GPDMA channel 13 destination address register
      GPDMA_C13DAR    : aliased HAL.UInt32;
      --  GPDMA channel 13 transfer register 3
      GPDMA_C13TR3    : aliased GPDMA_C13TR3_Register;
      --  GPDMA channel 13 block register 2
      GPDMA_C13BR2    : aliased GPDMA_C13BR2_Register;
      --  GPDMA channel 13 alternate linked-list address register
      GPDMA_C13LLR    : aliased GPDMA_C13LLR_Register;
      --  GPDMA channel 14 linked-list base address register
      GPDMA_C14LBAR   : aliased GPDMA_C14LBAR_Register;
      --  GPDMA channel 14 flag clear register
      GPDMA_C14FCR    : aliased GPDMA_C14FCR_Register;
      --  GPDMA channel 14 status register
      GPDMA_C14SR     : aliased GPDMA_C14SR_Register;
      --  GPDMA channel 14 control register
      GPDMA_C14CR     : aliased GPDMA_C14CR_Register;
      --  GPDMA channel 14 transfer register 1
      GPDMA_C14TR1    : aliased GPDMA_C14TR1_Register;
      --  GPDMA channel 14 transfer register 2
      GPDMA_C14TR2    : aliased GPDMA_C14TR2_Register;
      --  GPDMA channel 14 alternate block register 1
      GPDMA_C14BR1    : aliased GPDMA_C14BR1_Register;
      --  GPDMA channel 14 source address register
      GPDMA_C14SAR    : aliased HAL.UInt32;
      --  GPDMA channel 14 destination address register
      GPDMA_C14DAR    : aliased HAL.UInt32;
      --  GPDMA channel 14 transfer register 3
      GPDMA_C14TR3    : aliased GPDMA_C14TR3_Register;
      --  GPDMA channel 14 block register 2
      GPDMA_C14BR2    : aliased GPDMA_C14BR2_Register;
      --  GPDMA channel 14 alternate linked-list address register
      GPDMA_C14LLR    : aliased GPDMA_C14LLR_Register;
      --  GPDMA channel 15 linked-list base address register
      GPDMA_C15LBAR   : aliased GPDMA_C15LBAR_Register;
      --  GPDMA channel 15 flag clear register
      GPDMA_C15FCR    : aliased GPDMA_C15FCR_Register;
      --  GPDMA channel 15 status register
      GPDMA_C15SR     : aliased GPDMA_C15SR_Register;
      --  GPDMA channel 15 control register
      GPDMA_C15CR     : aliased GPDMA_C15CR_Register;
      --  GPDMA channel 15 transfer register 1
      GPDMA_C15TR1    : aliased GPDMA_C15TR1_Register;
      --  GPDMA channel 15 transfer register 2
      GPDMA_C15TR2    : aliased GPDMA_C15TR2_Register;
      --  GPDMA channel 15 alternate block register 1
      GPDMA_C15BR1    : aliased GPDMA_C15BR1_Register;
      --  GPDMA channel 15 source address register
      GPDMA_C15SAR    : aliased HAL.UInt32;
      --  GPDMA channel 15 destination address register
      GPDMA_C15DAR    : aliased HAL.UInt32;
      --  GPDMA channel 15 transfer register 3
      GPDMA_C15TR3    : aliased GPDMA_C15TR3_Register;
      --  GPDMA channel 15 block register 2
      GPDMA_C15BR2    : aliased GPDMA_C15BR2_Register;
      --  GPDMA channel 15 alternate linked-list address register
      GPDMA_C15LLR    : aliased GPDMA_C15LLR_Register;
   end record
     with Volatile;

   for GPDMA_Peripheral use record
      GPDMA_SECCFGR   at 16#0# range 0 .. 31;
      GPDMA_PRIVCFGR  at 16#4# range 0 .. 31;
      GPDMA_RCFGLOCKR at 16#8# range 0 .. 31;
      GPDMA_MISR      at 16#C# range 0 .. 31;
      GPDMA_SMISR     at 16#10# range 0 .. 31;
      GPDMA_C0LBAR    at 16#50# range 0 .. 31;
      GPDMA_C0FCR     at 16#5C# range 0 .. 31;
      GPDMA_C0SR      at 16#60# range 0 .. 31;
      GPDMA_C0CR      at 16#64# range 0 .. 31;
      GPDMA_C0TR1     at 16#90# range 0 .. 31;
      GPDMA_C0TR2     at 16#94# range 0 .. 31;
      GPDMA_C0BR1     at 16#98# range 0 .. 31;
      GPDMA_C0SAR     at 16#9C# range 0 .. 31;
      GPDMA_C0DAR     at 16#A0# range 0 .. 31;
      GPDMA_C0LLR     at 16#CC# range 0 .. 31;
      GPDMA_C1LBAR    at 16#D0# range 0 .. 31;
      GPDMA_C1FCR     at 16#DC# range 0 .. 31;
      GPDMA_C1SR      at 16#E0# range 0 .. 31;
      GPDMA_C1CR      at 16#E4# range 0 .. 31;
      GPDMA_C1TR1     at 16#110# range 0 .. 31;
      GPDMA_C1TR2     at 16#114# range 0 .. 31;
      GPDMA_C1BR1     at 16#118# range 0 .. 31;
      GPDMA_C1SAR     at 16#11C# range 0 .. 31;
      GPDMA_C1DAR     at 16#120# range 0 .. 31;
      GPDMA_C1LLR     at 16#14C# range 0 .. 31;
      GPDMA_C2LBAR    at 16#150# range 0 .. 31;
      GPDMA_C2FCR     at 16#15C# range 0 .. 31;
      GPDMA_C2SR      at 16#160# range 0 .. 31;
      GPDMA_C2CR      at 16#164# range 0 .. 31;
      GPDMA_C2TR1     at 16#190# range 0 .. 31;
      GPDMA_C2TR2     at 16#194# range 0 .. 31;
      GPDMA_C2BR1     at 16#198# range 0 .. 31;
      GPDMA_C2SAR     at 16#19C# range 0 .. 31;
      GPDMA_C2DAR     at 16#1A0# range 0 .. 31;
      GPDMA_C2LLR     at 16#1CC# range 0 .. 31;
      GPDMA_C3LBAR    at 16#1D0# range 0 .. 31;
      GPDMA_C3FCR     at 16#1DC# range 0 .. 31;
      GPDMA_C3SR      at 16#1E0# range 0 .. 31;
      GPDMA_C3CR      at 16#1E4# range 0 .. 31;
      GPDMA_C3TR1     at 16#210# range 0 .. 31;
      GPDMA_C3TR2     at 16#214# range 0 .. 31;
      GPDMA_C3BR1     at 16#218# range 0 .. 31;
      GPDMA_C3SAR     at 16#21C# range 0 .. 31;
      GPDMA_C3DAR     at 16#220# range 0 .. 31;
      GPDMA_C3LLR     at 16#24C# range 0 .. 31;
      GPDMA_C4LBAR    at 16#250# range 0 .. 31;
      GPDMA_C4FCR     at 16#25C# range 0 .. 31;
      GPDMA_C4SR      at 16#260# range 0 .. 31;
      GPDMA_C4CR      at 16#264# range 0 .. 31;
      GPDMA_C4TR1     at 16#290# range 0 .. 31;
      GPDMA_C4TR2     at 16#294# range 0 .. 31;
      GPDMA_C4BR1     at 16#298# range 0 .. 31;
      GPDMA_C4SAR     at 16#29C# range 0 .. 31;
      GPDMA_C4DAR     at 16#2A0# range 0 .. 31;
      GPDMA_C4LLR     at 16#2CC# range 0 .. 31;
      GPDMA_C5LBAR    at 16#2D0# range 0 .. 31;
      GPDMA_C5FCR     at 16#2DC# range 0 .. 31;
      GPDMA_C5SR      at 16#2E0# range 0 .. 31;
      GPDMA_C5CR      at 16#2E4# range 0 .. 31;
      GPDMA_C5TR1     at 16#310# range 0 .. 31;
      GPDMA_C5TR2     at 16#314# range 0 .. 31;
      GPDMA_C5BR1     at 16#318# range 0 .. 31;
      GPDMA_C5SAR     at 16#31C# range 0 .. 31;
      GPDMA_C5DAR     at 16#320# range 0 .. 31;
      GPDMA_C5LLR     at 16#34C# range 0 .. 31;
      GPDMA_C6LBAR    at 16#350# range 0 .. 31;
      GPDMA_C6FCR     at 16#35C# range 0 .. 31;
      GPDMA_C6SR      at 16#360# range 0 .. 31;
      GPDMA_C6CR      at 16#364# range 0 .. 31;
      GPDMA_C6TR1     at 16#390# range 0 .. 31;
      GPDMA_C6TR2     at 16#394# range 0 .. 31;
      GPDMA_C6BR1     at 16#398# range 0 .. 31;
      GPDMA_C6SAR     at 16#39C# range 0 .. 31;
      GPDMA_C6DAR     at 16#3A0# range 0 .. 31;
      GPDMA_C6LLR     at 16#3CC# range 0 .. 31;
      GPDMA_C7LBAR    at 16#3D0# range 0 .. 31;
      GPDMA_C7FCR     at 16#3DC# range 0 .. 31;
      GPDMA_C7SR      at 16#3E0# range 0 .. 31;
      GPDMA_C7CR      at 16#3E4# range 0 .. 31;
      GPDMA_C7TR1     at 16#410# range 0 .. 31;
      GPDMA_C7TR2     at 16#414# range 0 .. 31;
      GPDMA_C7BR1     at 16#418# range 0 .. 31;
      GPDMA_C7SAR     at 16#41C# range 0 .. 31;
      GPDMA_C7DAR     at 16#420# range 0 .. 31;
      GPDMA_C7LLR     at 16#44C# range 0 .. 31;
      GPDMA_C8LBAR    at 16#450# range 0 .. 31;
      GPDMA_C8FCR     at 16#45C# range 0 .. 31;
      GPDMA_C8SR      at 16#460# range 0 .. 31;
      GPDMA_C8CR      at 16#464# range 0 .. 31;
      GPDMA_C8TR1     at 16#490# range 0 .. 31;
      GPDMA_C8TR2     at 16#494# range 0 .. 31;
      GPDMA_C8BR1     at 16#498# range 0 .. 31;
      GPDMA_C8SAR     at 16#49C# range 0 .. 31;
      GPDMA_C8DAR     at 16#4A0# range 0 .. 31;
      GPDMA_C8LLR     at 16#4CC# range 0 .. 31;
      GPDMA_C9LBAR    at 16#4D0# range 0 .. 31;
      GPDMA_C9FCR     at 16#4DC# range 0 .. 31;
      GPDMA_C9SR      at 16#4E0# range 0 .. 31;
      GPDMA_C9CR      at 16#4E4# range 0 .. 31;
      GPDMA_C9TR1     at 16#510# range 0 .. 31;
      GPDMA_C9TR2     at 16#514# range 0 .. 31;
      GPDMA_C9BR1     at 16#518# range 0 .. 31;
      GPDMA_C9SAR     at 16#51C# range 0 .. 31;
      GPDMA_C9DAR     at 16#520# range 0 .. 31;
      GPDMA_C9LLR     at 16#54C# range 0 .. 31;
      GPDMA_C10LBAR   at 16#550# range 0 .. 31;
      GPDMA_C10FCR    at 16#55C# range 0 .. 31;
      GPDMA_C10SR     at 16#560# range 0 .. 31;
      GPDMA_C10CR     at 16#564# range 0 .. 31;
      GPDMA_C10TR1    at 16#590# range 0 .. 31;
      GPDMA_C10TR2    at 16#594# range 0 .. 31;
      GPDMA_C10BR1    at 16#598# range 0 .. 31;
      GPDMA_C10SAR    at 16#59C# range 0 .. 31;
      GPDMA_C10DAR    at 16#5A0# range 0 .. 31;
      GPDMA_C10LLR    at 16#5CC# range 0 .. 31;
      GPDMA_C11LBAR   at 16#5D0# range 0 .. 31;
      GPDMA_C11FCR    at 16#5DC# range 0 .. 31;
      GPDMA_C11SR     at 16#5E0# range 0 .. 31;
      GPDMA_C11CR     at 16#5E4# range 0 .. 31;
      GPDMA_C11TR1    at 16#610# range 0 .. 31;
      GPDMA_C11TR2    at 16#614# range 0 .. 31;
      GPDMA_C11BR1    at 16#618# range 0 .. 31;
      GPDMA_C11SAR    at 16#61C# range 0 .. 31;
      GPDMA_C11DAR    at 16#620# range 0 .. 31;
      GPDMA_C11LLR    at 16#64C# range 0 .. 31;
      GPDMA_C12LBAR   at 16#650# range 0 .. 31;
      GPDMA_C12FCR    at 16#65C# range 0 .. 31;
      GPDMA_C12SR     at 16#660# range 0 .. 31;
      GPDMA_C12CR     at 16#664# range 0 .. 31;
      GPDMA_C12TR1    at 16#690# range 0 .. 31;
      GPDMA_C12TR2    at 16#694# range 0 .. 31;
      GPDMA_C12BR1    at 16#698# range 0 .. 31;
      GPDMA_C12SAR    at 16#69C# range 0 .. 31;
      GPDMA_C12DAR    at 16#6A0# range 0 .. 31;
      GPDMA_C12TR3    at 16#6A4# range 0 .. 31;
      GPDMA_C12BR2    at 16#6A8# range 0 .. 31;
      GPDMA_C12LLR    at 16#6CC# range 0 .. 31;
      GPDMA_C13LBAR   at 16#6D0# range 0 .. 31;
      GPDMA_C13FCR    at 16#6DC# range 0 .. 31;
      GPDMA_C13SR     at 16#6E0# range 0 .. 31;
      GPDMA_C13CR     at 16#6E4# range 0 .. 31;
      GPDMA_C13TR1    at 16#710# range 0 .. 31;
      GPDMA_C13TR2    at 16#714# range 0 .. 31;
      GPDMA_C13BR1    at 16#718# range 0 .. 31;
      GPDMA_C13SAR    at 16#71C# range 0 .. 31;
      GPDMA_C13DAR    at 16#720# range 0 .. 31;
      GPDMA_C13TR3    at 16#724# range 0 .. 31;
      GPDMA_C13BR2    at 16#728# range 0 .. 31;
      GPDMA_C13LLR    at 16#74C# range 0 .. 31;
      GPDMA_C14LBAR   at 16#750# range 0 .. 31;
      GPDMA_C14FCR    at 16#75C# range 0 .. 31;
      GPDMA_C14SR     at 16#760# range 0 .. 31;
      GPDMA_C14CR     at 16#764# range 0 .. 31;
      GPDMA_C14TR1    at 16#790# range 0 .. 31;
      GPDMA_C14TR2    at 16#794# range 0 .. 31;
      GPDMA_C14BR1    at 16#798# range 0 .. 31;
      GPDMA_C14SAR    at 16#79C# range 0 .. 31;
      GPDMA_C14DAR    at 16#7A0# range 0 .. 31;
      GPDMA_C14TR3    at 16#7A4# range 0 .. 31;
      GPDMA_C14BR2    at 16#7A8# range 0 .. 31;
      GPDMA_C14LLR    at 16#7CC# range 0 .. 31;
      GPDMA_C15LBAR   at 16#7D0# range 0 .. 31;
      GPDMA_C15FCR    at 16#7DC# range 0 .. 31;
      GPDMA_C15SR     at 16#7E0# range 0 .. 31;
      GPDMA_C15CR     at 16#7E4# range 0 .. 31;
      GPDMA_C15TR1    at 16#810# range 0 .. 31;
      GPDMA_C15TR2    at 16#814# range 0 .. 31;
      GPDMA_C15BR1    at 16#818# range 0 .. 31;
      GPDMA_C15SAR    at 16#81C# range 0 .. 31;
      GPDMA_C15DAR    at 16#820# range 0 .. 31;
      GPDMA_C15TR3    at 16#824# range 0 .. 31;
      GPDMA_C15BR2    at 16#828# range 0 .. 31;
      GPDMA_C15LLR    at 16#84C# range 0 .. 31;
   end record;

   --  GPDMA1
   GPDMA1_Periph : aliased GPDMA_Peripheral
     with Import, Address => GPDMA1_Base;

   --  GPDMA1
   SEC_GPDMA1_Periph : aliased GPDMA_Peripheral
     with Import, Address => SEC_GPDMA1_Base;

end STM32_SVD.GPDMA;
