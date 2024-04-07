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

   ----------------------------------
   -- GPDMA_CH cluster's Registers --
   ----------------------------------

   subtype LBAR_GPDMA_CH_LBA_Field is HAL.UInt16;

   --  GPDMA channel x linked-list base address register
   type LBAR_GPDMA_CH_Register is record
      --  unspecified
      Reserved_0_15 : HAL.UInt16 := 16#0#;
      --  linked-list base address of GPDMA channel x
      LBA           : LBAR_GPDMA_CH_LBA_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LBAR_GPDMA_CH_Register use record
      Reserved_0_15 at 0 range 0 .. 15;
      LBA           at 0 range 16 .. 31;
   end record;

   --  GPDMA channel x flag clear register
   type FCR_GPDMA_CH_Register is record
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

   for FCR_GPDMA_CH_Register use record
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

   subtype SR_GPDMA_CH_FIFOL_Field is HAL.UInt8;

   --  GPDMA channel x status register
   type SR_GPDMA_CH_Register is record
      --  Read-only. idle flag 		This idle flag is de-asserted by hardware when
      --  the channel is enabled (GPDMA_CxCR.EN = 1) with a valid channel
      --  configuration (no USEF to be immediately reported). 		This idle flag
      --  is asserted after hard reset or by hardware when the channel is back
      --  in idle state (in suspended or disabled state).
      IDLEF          : Boolean;
      --  unspecified
      Reserved_1_7   : HAL.UInt7;
      --  Read-only. transfer complete flag 		A transfer complete event is
      --  either a block transfer complete, a 2D/repeated block transfer
      --  complete, a LLI transfer complete including the upload of the next
      --  LLI if any, or the full linked-list completion, depending on the
      --  transfer complete event mode (GPDMA_CxTR2.TCEM[1:0]).
      TCF            : Boolean;
      --  Read-only. half transfer flag 		An half transfer event is either an
      --  half block transfer or an half 2D/repeated block transfer, depending
      --  on the transfer complete event mode (GPDMA_CxTR2.TCEM[1:0]). 		An
      --  half block transfer occurs when half of the bytes of the source block
      --  size (rounded up integer of GPDMA_CxBR1.BNDT[15:0]/2) has been
      --  transferred to the destination. 		An half 2D/repeated block transfer
      --  occurs when half of the repeated blocks (rounded up integer of
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
      --  Read-only. monitored FIFO level 		Number of available write beats in
      --  the FIFO, in units of the programmed destination data width (see
      --  GPDMA_CxTR1.DDW_LOG2[1:0], in units of bytes, half-words, or words).
      --  		Note: After having suspended an active transfer, the user may need
      --  to read FIFOL[7:0], additionally to GPDMA_CxBR1.BDNT[15:0] and
      --  GPDMA_CxBR1.BRC[10:0], to know how many data have been transferred to
      --  the destination. Before reading, the user may wait for the transfer
      --  to be suspended (GPDMA_CxSR.SUSPF = 1).
      FIFOL          : SR_GPDMA_CH_FIFOL_Field;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_GPDMA_CH_Register use record
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

   subtype CR_GPDMA_CH_PRIO_Field is HAL.UInt2;

   --  GPDMA channel x control register
   type CR_GPDMA_CH_Register is record
      --  enable 		Writing 1 into the field RESET (bit 1) causes the hardware
      --  to de-assert this bit, whatever is written into this bit 0. Else:
      --  		this bit is de-asserted by hardware when there is a transfer error
      --  (master bus error or user setting error) or when there is a channel
      --  transfer complete (channel ready to be configured, e.g. if LSM=1 at
      --  the end of a single execution of the LLI). 		Else, this bit can be
      --  asserted by software. 		Writing 0 into this EN bit is ignored.
      EN             : Boolean := False;
      --  Write-only. reset 		This bit is write only. Writing 0 has no impact.
      --  Writing 1 implies the reset of the following: the FIFO, the channel
      --  internal state, SUSP and EN bits (whatever is written receptively in
      --  bit 2 and bit 0). 		The reset is effective when the channel is in
      --  steady state, meaning one of the following: 		- active channel in
      --  suspended state (GPDMA_CxSR.SUSPF = 1 and
      --  GPDMA_CxSR.IDLEF = GPDMA_CxCR.EN = 1) 		- channel in disabled
      --  state (GPDMA_CxSR.IDLEF = 1 and GPDMA_CxCR.EN = 0). 		After
      --  writing a RESET, to continue using this channel, the user must
      --  explicitly reconfigure the channel including the hardware-modified
      --  configuration registers (GPDMA_CxBR1, GPDMA_CxSAR and GPDMA_CxDAR)
      --  before enabling again the channel (see the programming sequence in ).
      RESET          : Boolean := False;
      --  suspend 		Writing 1 into the field RESET (bit 1) causes the hardware
      --  to de-assert this bit, whatever is written into this bit 2. Else:
      --  		Software must write 1 in order to suspend an active channel i.e. a
      --  channel with an on-going GPDMA transfer over its master ports. 		The
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
      --  Link step mode 		First the (possible 1D/repeated) block transfer is
      --  executed as defined by the current internal register file until
      --  GPDMA_CxBR1.BNDT[15:0] = 0 and GPDMA_CxBR1.BRC[10:0] = 0 if
      --  present. Secondly the next linked-list data structure is
      --  conditionally uploaded from memory as defined by GPDMA_CxLLR. Then
      --  channel execution is completed. 		Note: This bit must be written when
      --  EN=0. This bit is read-only when EN=1.
      LSM            : Boolean := False;
      --  linked-list allocated port 		This bit is used to allocate the master
      --  port for the update of the GPDMA linked-list registers from the
      --  memory. 		Note: This bit must be written when EN=0. This bit is
      --  read-only when EN=1.
      LAP            : Boolean := False;
      --  unspecified
      Reserved_18_21 : HAL.UInt4 := 16#0#;
      --  priority level of the channel x GPDMA transfer versus others 		Note:
      --  This bit must be written when EN = 0. This bit is read-only when
      --  EN = 1.
      PRIO           : CR_GPDMA_CH_PRIO_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_GPDMA_CH_Register use record
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

   subtype TR1_GPDMA_CH_SDW_LOG2_Field is HAL.UInt2;
   subtype TR1_GPDMA_CH_SBL_1_Field is HAL.UInt6;
   subtype TR1_GPDMA_CH_PAM_Field is HAL.UInt2;
   subtype TR1_GPDMA_CH_DDW_LOG2_Field is HAL.UInt2;
   subtype TR1_GPDMA_CH_DBL_1_Field is HAL.UInt6;

   --  GPDMA channel x transfer register 1
   type TR1_GPDMA_CH_Register is record
      --  binary logarithm of the source data width of a burst in bytes 		Note:
      --  Setting a 8-byte data width causes a user setting error to be
      --  reported and no transfer is issued. 		A source block size must be a
      --  multiple of the source data width (GPDMA_CxBR1.BNDT[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and no
      --  transfer is issued. 		A source single transfer must have an aligned
      --  address with its data width (start address GPDMA_CxSAR[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and none
      --  transfer is issued.
      SDW_LOG2       : TR1_GPDMA_CH_SDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  source incrementing burst 		The source address, pointed by
      --  GPDMA_CxSAR, is kept constant after a burst beat/single transfer or
      --  is incremented by the offset value corresponding to a contiguous data
      --  after a burst beat/single transfer.
      SINC           : Boolean := False;
      --  source burst length minus 1, between 0 and 63 		The burst length unit
      --  is one data named beat within a burst. If SBL_1[5:0] =0 , the burst
      --  can be named as single. Each data/beat has a width defined by the
      --  destination data width SDW_LOG2[1:0]. 		Note: If a burst transfer
      --  crossed a 1-Kbyte address boundary on a AHB transfer, the GPDMA
      --  modifies and shortens the programmed burst into singles or bursts of
      --  lower length, to be compliant with the AHB protocol. 		If a burst
      --  transfer is of length greater than the FIFO size of the channel x,
      --  the GPDMA modifies and shortens the programmed burst into singles or
      --  bursts of lower length, to be compliant with the FIFO size. Transfer
      --  performance is lower, with GPDMA re-arbitration between effective and
      --  lower bursts/singles, but the data integrity is guaranteed.
      SBL_1          : TR1_GPDMA_CH_SBL_1_Field := 16#0#;
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  padding/alignment mode 		If DDW_LOG2[1:0] = SDW_LOG2[1:0]: if the
      --  data width of a burst destination transfer is equal to the data width
      --  of a burst source transfer, these bits are ignored. 		Else: 		- Case
      --  1: If destination data width > source data width 		1x: successive
      --  source data are FIFO queued and packed at the destination data width,
      --  in a left (LSB) to right (MSB) order (named little endian), before a
      --  destination transfer 		- Case 2: If destination data width < source
      --  data width 		1x: source data is FIFO queued and unpacked at the
      --  destination data width, to be transferred in a left (LSB) to right
      --  (MSB) order (named little endian) to the destination 		Note:
      PAM            : TR1_GPDMA_CH_PAM_Field := 16#0#;
      --  source byte exchange within the unaligned half-word of each source
      --  word 		If the source data width is shorter than a word, this bit is
      --  ignored. 		If the source data width is a word:
      SBX            : Boolean := False;
      --  source allocated port 		This bit is used to allocate the master port
      --  for the source transfer 		Note: This bit must be written when
      --  EN = 0. This bit is read-only when EN = 1.
      SAP            : Boolean := False;
      --  security attribute of the GPDMA transfer from the source 		If
      --  GPDMA_SECCFGR.SECx = 1 and the access is secure: 		This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  GPDMA_SECCFGR.SECx =1 . A secure write is ignored when
      --  GPDMA_SECCFGR.SECx = 0. 		When GPDMA_SECCFGR.SECx is de-asserted,
      --  this SSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the GPDMA transfer
      --  from the source is non-secure.
      SSEC           : Boolean := False;
      --  binary logarithm of the destination data width of a burst, in bytes
      --  		Note: Setting a 8-byte data width causes a user setting error to be
      --  reported and none transfer is issued. 		A destination burst transfer
      --  must have an aligned address with its data width (start address
      --  GPDMA_CxDAR[2:0] and address offset GPDMA_CxTR3.DAO[2:0], versus
      --  DDW_LOG2[1:0]). Otherwise a user setting error is reported and no
      --  transfer is issued.
      DDW_LOG2       : TR1_GPDMA_CH_DDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_18_18 : HAL.Bit := 16#0#;
      --  destination incrementing burst 		The destination address, pointed by
      --  GPDMA_CxDAR, is kept constant after a burst beat/single transfer, or
      --  is incremented by the offset value corresponding to a contiguous data
      --  after a burst beat/single transfer.
      DINC           : Boolean := False;
      --  destination burst length minus 1, between 0 and 63 		The burst length
      --  unit is one data named beat within a burst. If DBL_1[5:0] =0 , the
      --  burst can be named as single. Each data/beat has a width defined by
      --  the destination data width DDW_LOG2[1:0]. 		Note: If a burst transfer
      --  crossed a 1-Kbyte address boundary on a AHB transfer, the GPDMA
      --  modifies and shortens the programmed burst into singles or bursts of
      --  lower length, to be compliant with the AHB protocol. 		If a burst
      --  transfer is of length greater than the FIFO size of the channel x,
      --  the GPDMA modifies and shortens the programmed burst into singles or
      --  bursts of lower length, to be compliant with the FIFO size. Transfer
      --  performance is lower, with GPDMA re-arbitration between effective and
      --  lower bursts/singles, but the data integrity is guaranteed.
      DBL_1          : TR1_GPDMA_CH_DBL_1_Field := 16#0#;
      --  destination byte exchange 		If the destination data size is a byte,
      --  this bit is ignored. 		If the destination data size is not a byte:
      DBX            : Boolean := False;
      --  destination half-word exchange 		If the destination data size is
      --  shorter than a word, this bit is ignored. 		If the destination data
      --  size is a word:
      DHX            : Boolean := False;
      --  unspecified
      Reserved_28_29 : HAL.UInt2 := 16#0#;
      --  destination allocated port 		This bit is used to allocate the master
      --  port for the destination transfer 		Note: This bit must be written
      --  when EN = 0. This bit is read-only when EN = 1.
      DAP            : Boolean := False;
      --  security attribute of the GPDMA transfer to the destination 		If
      --  GPDMA_SECCFGR.SECx = 1 and the access is secure: 		This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  GPDMA_SECCFGR.SECx = 1. A secure write is ignored when
      --  GPDMA_SECCFGR.SECx = 0. 		When GPDMA_SECCFGR.SECx is de-asserted,
      --  this DSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the GPDMA transfer
      --  to the destination is non-secure.
      DSEC           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TR1_GPDMA_CH_Register use record
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

   subtype TR2_GPDMA_CH_REQSEL_Field is HAL.UInt7;
   subtype TR2_GPDMA_CH_TRIGM_Field is HAL.UInt2;
   subtype TR2_GPDMA_CH_TRIGSEL_Field is HAL.UInt6;
   subtype TR2_GPDMA_CH_TRIGPOL_Field is HAL.UInt2;
   subtype TR2_GPDMA_CH_TCEM_Field is HAL.UInt2;

   --  GPDMA channel x transfer register 2
   type TR2_GPDMA_CH_Register is record
      --  GPDMA hardware request selection 		These bits are ignored if channel
      --  x is activated (GPDMA_CxCR.EN asserted) with SWREQ = 1 (software
      --  request for a memory-to-memory transfer). Else, the selected hardware
      --  request is internally taken into account as per . 		The user must not
      --  assign a same input hardware request (same REQSEL[6:0] value) to
      --  different active GPDMA channels (GPDMA_CxCR.EN = 1 and
      --  GPDMA_CxTR2.SWREQ = 0 for these channels). GPDMA is not intended to
      --  hardware support the case of simultaneous enabled channels
      --  incorrectly configured with a same hardware peripheral request
      --  signal, and there is no user setting error reporting.
      REQSEL         : TR2_GPDMA_CH_REQSEL_Field := 16#0#;
      --  unspecified
      Reserved_7_8   : HAL.UInt2 := 16#0#;
      --  software request 		This bit is internally taken into account when
      --  GPDMA_CxCR.EN is asserted.
      SWREQ          : Boolean := False;
      --  destination hardware request 		This bit is ignored if channel x is
      --  activated (GPDMA_CxCR.EN asserted) with SWREQ = 1 (software request
      --  for a memory-to-memory transfer). Else: 		Note:
      DREQ           : Boolean := False;
      --  Block hardware request 		If the channel x is activated (GPDMA_CxCR.EN
      --  asserted) with SWREQ = 1 (software request for a memory-to-memory
      --  transfer), this bit is ignored. Else:
      BREQ           : Boolean := False;
      --  unspecified
      Reserved_12_13 : HAL.UInt2 := 16#0#;
      --  trigger mode 		These bits define the transfer granularity for its
      --  conditioning by the trigger. 		If the channel x is enabled
      --  (GPDMA_CxCR.EN asserted) with TRIGPOL[1:0] = 00 or 11, these
      --  TRIGM[1:0] bits are ignored. 		Else, a GPDMA transfer is conditioned
      --  by at least one trigger hit: 		first burst read of a 2D/repeated
      --  block transfer is conditioned by one hit trigger. 		– If the
      --  peripheral is programmed as a source (DREQ = 0) of the LLI data
      --  transfer, each programmed burst read is conditioned. 		– If the
      --  peripheral is programmed as a destination (DREQ = 1) of the LLI
      --  data transfer, each programmed burst write is conditioned. The first
      --  memory burst read of a (possibly 2D/repeated) block, also named as
      --  the first ready FIFO-based source burst, is gated by the occurrence
      --  of both the hardware request and the first trigger hit. 		The GPDMA
      --  monitoring of a trigger for channel x is started when the channel is
      --  enabled/loaded with a new active trigger configuration: rising or
      --  falling edge on a selected trigger (TRIGPOL[1:0] = 01 or
      --  respectively TRIGPOL[1:0] = 10). 		The monitoring of this trigger
      --  is kept active during the triggered and uncompleted (data or link)
      --  transfer; and if a new trigger is detected then, this hit is
      --  internally memorized to grant the next transfer, as long as the
      --  defined rising or falling edge is not modified, and the TRIGSEL[5:0]
      --  is not modified, and the channel is enabled. 		Transferring a next
      --  LLIn+1 that updates the GPDMA_CxTR2 with a new value for any of
      --  TRIGSEL[5:0] or TRIGPOL[1:0], resets the monitoring, trashing the
      --  memorized hit of the formerly defined LLIn trigger. 		After a first
      --  new trigger hitn+1 is memorized, if another second trigger hitn+2 is
      --  detected and if the hitn triggered transfer is still not completed,
      --  hitn+2 is lost and not memorized.memorized. A trigger overrun flag is
      --  reported (GPDMA_CxSR.TOF =1 ), and an interrupt is generated if
      --  enabled (GPDMA_CxCR.TOIE = 1). The channel is not automatically
      --  disabled by hardware due to a trigger overrun. 		Note: When the
      --  source block size is not a multiple of the source burst size and is a
      --  multiple of the source data width, then the last programmed source
      --  burst is not completed and is internally shorten to match the block
      --  size. In this case, if TRIGM[1:0] = 11 and (SWREQ =1  or
      --  (SWREQ = 0 and DREQ =0 )), the shortened burst transfer (by
      --  singles or/and by bursts of lower length) is conditioned once by the
      --  trigger. 		When the programmed destination burst is internally
      --  shortened by singles or/and by bursts of lower length (versus FIFO
      --  size, versus block size, 1-Kbyte boundary address crossing): if the
      --  trigger is conditioning the programmed destination burst (if
      --  TRIGM[1:0] = 11 and SWREQ = 0 and DREQ = 1), this shortened
      --  destination burst transfer is conditioned once by the trigger.
      TRIGM          : TR2_GPDMA_CH_TRIGM_Field := 16#0#;
      --  trigger event input selection 		These bits select the trigger event
      --  input of the GPDMA transfer (as per ), with an active trigger event
      --  if TRIGPOL[1:0] ≠ 00.
      TRIGSEL        : TR2_GPDMA_CH_TRIGSEL_Field := 16#0#;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  trigger event polarity 		These bits define the polarity of the
      --  selected trigger event input defined by TRIGSEL[5:0].
      TRIGPOL        : TR2_GPDMA_CH_TRIGPOL_Field := 16#0#;
      --  unspecified
      Reserved_26_29 : HAL.UInt4 := 16#0#;
      --  transfer complete event mode 		These bits define the transfer
      --  granularity for the transfer complete and half transfer complete
      --  events generation. 		Note: If the initial LLI0 data transfer is
      --  null/void (directly programmed by the internal register file with
      --  GPDMA_CxBR1.BNDT[15:0] = 0), then neither the complete transfer
      --  event nor the half transfer event is generated. 		Note: If the
      --  initial LLI0 data transfer is null/void (directly programmed by the
      --  internal register file with GPDMA_CxBR1.BNDT[15:0] = 0), then
      --  neither the complete transfer event nor the half transfer event is
      --  generated. 		Note: If the initial LLI0 data transfer is null/void
      --  (i.e. directly programmed by the internal register file with
      --  GPDMA_CxBR1.BNDT[15:0] =0 ), then the half transfer event is not
      --  generated, and the transfer complete event is generated when is
      --  completed the loading of the LLI1.
      TCEM           : TR2_GPDMA_CH_TCEM_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TR2_GPDMA_CH_Register use record
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

   subtype BR1_GPDMA_CH_BNDT_Field is HAL.UInt16;

   --  GPDMA channel x block register 1
   type BR1_GPDMA_CH_Register is record
      --  block number of data bytes to transfer from the source 		Block size
      --  transferred from the source. When the channel is enabled, this field
      --  becomes read-only and is decremented, indicating the remaining number
      --  of data items in the current source block to be transferred.
      --  BNDT[15:0] is programmed in number of bytes, maximum source block
      --  size is 64 Kbytes -1. 		Once the last data transfer is completed
      --  (BNDT[15:0] = 0): 		- if GPDMA_CxLLR.UB1 = 1, this field is
      --  updated by the LLI in the memory. 		- if GPDMA_CxLLR.UB1 = 0 and if
      --  there is at least one non null Uxx update bit, this field is
      --  internally restored to the programmed value. 		- if all
      --  GPDMA_CxLLR.Uxx = 0 and if GPDMA_CxLLR.LA[15:0] = 0, this field
      --  is internally restored to the programmed value (infinite/continuous
      --  last LLI). 		- if GPDMA_CxLLR = 0, this field is kept as zero
      --  following the last LLI data transfer. 		Note: A non-null source block
      --  size must be a multiple of the source data width (BNDT[2:0] versus
      --  GPDMA_CxTR1.SDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued. 		When configured in packing mode
      --  (GPDMA_CxTR1.PAM[1] = 1 and destination data width different from
      --  source data width), a non-null source block size must be a multiple
      --  of the destination data width (BNDT[2:0] versus
      --  GPDMA_CxTR1.DDW_LOG2[1:0]). Else a user setting error is reported and
      --  no transfer is issued.
      BNDT           : BR1_GPDMA_CH_BNDT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for BR1_GPDMA_CH_Register use record
      BNDT           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype LLR_GPDMA_CH_LA_Field is HAL.UInt14;

   --  GPDMA channel x linked-list address register
   type LLR_GPDMA_CH_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  pointer (16-bit low-significant address) to the next linked-list data
      --  structure 		If UT1 = UT2 = UB1 = USA = UDA = ULL = 0 and
      --  if LA[15:20] = 0, the current LLI is the last one. The channel
      --  transfer is completed without any update of the linked-list GPDMA
      --  register file. 		Else, this field is the pointer to the memory
      --  address offset from which the next linked-list data structure is
      --  automatically fetched from, once the data transfer is completed, in
      --  order to conditionally update the linked-list GPDMA internal register
      --  file (GPDMA_CxCTR1, GPDMA_CxTR2, GPDMA_CxBR1, GPDMA_CxSAR,
      --  GPDMA_CxDAR and GPDMA_CxLLR). 		Note: The user must program the
      --  pointer to be 32-bit aligned. The two low-significant bits are write
      --  ignored.
      LA             : LLR_GPDMA_CH_LA_Field := 16#0#;
      --  Update GPDMA_CxLLR register from memory 		This bit is used to control
      --  the update of GPDMA_CxLLR from the memory during the link transfer.
      ULL            : Boolean := False;
      --  unspecified
      Reserved_17_26 : HAL.UInt10 := 16#0#;
      --  Update GPDMA_CxDAR register from memory 		This bit is used to control
      --  the update of GPDMA_CxDAR from the memory during the link transfer.
      UDA            : Boolean := False;
      --  update GPDMA_CxSAR from memory 		This bit controls the update of
      --  GPDMA_CxSAR from the memory during the link transfer.
      USA            : Boolean := False;
      --  Update GPDMA_CxBR1 from memory 		This bit controls the update of
      --  GPDMA_CxBR1 from the memory during the link transfer. If UB1 = 0
      --  and if GPDMA_CxLLR ≠ 0, the linked-list is not completed.
      --  GPDMA_CxBR1.BNDT[15:0] is then restored to the programmed value after
      --  data transfer is completed and before the link transfer.
      UB1            : Boolean := False;
      --  Update GPDMA_CxTR2 from memory 		This bit controls the update of
      --  GPDMA_CxTR2 from the memory during the link transfer.
      UT2            : Boolean := False;
      --  Update GPDMA_CxTR1 from memory 		This bit controls the update of
      --  GPDMA_CxTR1 from the memory during the link transfer.
      UT1            : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LLR_GPDMA_CH_Register use record
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

   type GPDMA_CH_Cluster is record
      --  GPDMA channel x linked-list base address register
      LBAR : aliased LBAR_GPDMA_CH_Register;
      --  GPDMA channel x flag clear register
      FCR  : aliased FCR_GPDMA_CH_Register;
      --  GPDMA channel x status register
      SR   : aliased SR_GPDMA_CH_Register;
      --  GPDMA channel x control register
      CR   : aliased CR_GPDMA_CH_Register;
      --  GPDMA channel x transfer register 1
      TR1  : aliased TR1_GPDMA_CH_Register;
      --  GPDMA channel x transfer register 2
      TR2  : aliased TR2_GPDMA_CH_Register;
      --  GPDMA channel x block register 1
      BR1  : aliased BR1_GPDMA_CH_Register;
      --  GPDMA channel x source address register
      SAR  : aliased HAL.UInt32;
      --  GPDMA channel x destination address register
      DAR  : aliased HAL.UInt32;
      --  GPDMA channel x linked-list address register
      LLR  : aliased LLR_GPDMA_CH_Register;
   end record
     with Size => 1024;

   for GPDMA_CH_Cluster use record
      LBAR at 16#0# range 0 .. 31;
      FCR  at 16#C# range 0 .. 31;
      SR   at 16#10# range 0 .. 31;
      CR   at 16#14# range 0 .. 31;
      TR1  at 16#40# range 0 .. 31;
      TR2  at 16#44# range 0 .. 31;
      BR1  at 16#48# range 0 .. 31;
      SAR  at 16#4C# range 0 .. 31;
      DAR  at 16#50# range 0 .. 31;
      LLR  at 16#7C# range 0 .. 31;
   end record;

   type GPDMA_CH_Clusters is array (0 .. 15) of GPDMA_CH_Cluster;

   ----------------------------------
   -- GPDMA_CH cluster's Registers --
   ----------------------------------

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
      GPDMA_CH        : aliased GPDMA_CH_Clusters;
   end record
     with Volatile;

   for GPDMA_Peripheral use record
      GPDMA_SECCFGR   at 16#0# range 0 .. 31;
      GPDMA_PRIVCFGR  at 16#4# range 0 .. 31;
      GPDMA_RCFGLOCKR at 16#8# range 0 .. 31;
      GPDMA_MISR      at 16#C# range 0 .. 31;
      GPDMA_SMISR     at 16#10# range 0 .. 31;
      GPDMA_CH        at 16#50# range 0 .. 16383;
   end record;

   --  GPDMA1
   GPDMA1_Periph : aliased GPDMA_Peripheral
     with Import, Address => GPDMA1_Base;

   --  GPDMA1
   SEC_GPDMA1_Periph : aliased GPDMA_Peripheral
     with Import, Address => SEC_GPDMA1_Base;

end STM32_SVD.GPDMA;
