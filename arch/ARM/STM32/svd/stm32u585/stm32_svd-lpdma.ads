--  This spec has been automatically generated from STM32U585.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.LPDMA is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  LPDMA_SECCFGR_SEC array
   type LPDMA_SECCFGR_SEC_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for LPDMA_SECCFGR_SEC
   type LPDMA_SECCFGR_SEC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SEC as a value
            Val : HAL.UInt4;
         when True =>
            --  SEC as an array
            Arr : LPDMA_SECCFGR_SEC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for LPDMA_SECCFGR_SEC_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  LPDMA secure configuration register
   type LPDMA_SECCFGR_Register is record
      --  SEC0
      SEC           : LPDMA_SECCFGR_SEC_Field :=
                       (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPDMA_SECCFGR_Register use record
      SEC           at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  LPDMA_PRIVCFGR_PRIV array
   type LPDMA_PRIVCFGR_PRIV_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for LPDMA_PRIVCFGR_PRIV
   type LPDMA_PRIVCFGR_PRIV_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PRIV as a value
            Val : HAL.UInt4;
         when True =>
            --  PRIV as an array
            Arr : LPDMA_PRIVCFGR_PRIV_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for LPDMA_PRIVCFGR_PRIV_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  LPDMA privileged configuration register
   type LPDMA_PRIVCFGR_Register is record
      --  PRIV0
      PRIV          : LPDMA_PRIVCFGR_PRIV_Field :=
                       (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPDMA_PRIVCFGR_Register use record
      PRIV          at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  LPDMA_RCFGLOCKR_LOCK array
   type LPDMA_RCFGLOCKR_LOCK_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for LPDMA_RCFGLOCKR_LOCK
   type LPDMA_RCFGLOCKR_LOCK_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  LOCK as a value
            Val : HAL.UInt4;
         when True =>
            --  LOCK as an array
            Arr : LPDMA_RCFGLOCKR_LOCK_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for LPDMA_RCFGLOCKR_LOCK_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  LPDMA configuration lock register
   type LPDMA_RCFGLOCKR_Register is record
      --  LOCK0
      LOCK          : LPDMA_RCFGLOCKR_LOCK_Field :=
                       (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPDMA_RCFGLOCKR_Register use record
      LOCK          at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  LPDMA_MISR_MIS array
   type LPDMA_MISR_MIS_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for LPDMA_MISR_MIS
   type LPDMA_MISR_MIS_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MIS as a value
            Val : HAL.UInt4;
         when True =>
            --  MIS as an array
            Arr : LPDMA_MISR_MIS_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for LPDMA_MISR_MIS_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  LPDMA non-secure masked interrupt status register
   type LPDMA_MISR_Register is record
      --  Read-only. MIS0
      MIS           : LPDMA_MISR_MIS_Field;
      --  unspecified
      Reserved_4_31 : HAL.UInt28;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPDMA_MISR_Register use record
      MIS           at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  LPDMA_SMISR_MIS array
   type LPDMA_SMISR_MIS_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for LPDMA_SMISR_MIS
   type LPDMA_SMISR_MIS_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MIS as a value
            Val : HAL.UInt4;
         when True =>
            --  MIS as an array
            Arr : LPDMA_SMISR_MIS_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for LPDMA_SMISR_MIS_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  LPDMA secure masked interrupt status register
   type LPDMA_SMISR_Register is record
      --  Read-only. MIS0
      MIS           : LPDMA_SMISR_MIS_Field;
      --  unspecified
      Reserved_4_31 : HAL.UInt28;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPDMA_SMISR_Register use record
      MIS           at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   subtype LPDMA_C0LBAR_LBA_Field is HAL.UInt16;

   --  LPDMA channel 0 linked-list base address register
   type LPDMA_C0LBAR_Register is record
      --  unspecified
      Reserved_0_15 : HAL.UInt16 := 16#0#;
      --  linked-list base address of LPDMA channel x
      LBA           : LPDMA_C0LBAR_LBA_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPDMA_C0LBAR_Register use record
      Reserved_0_15 at 0 range 0 .. 15;
      LBA           at 0 range 16 .. 31;
   end record;

   --  LPDMA channel 0 flag clear register
   type LPDMA_C0FCR_Register is record
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

   for LPDMA_C0FCR_Register use record
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

   --  LPDMA channel 0 status register
   type LPDMA_C0SR_Register is record
      --  Read-only. idle flag This idle flag is de-asserted by hardware when
      --  the channel is enabled (LPDMA_CxCR.EN = 1) with a valid channel
      --  configuration (no USEF to be immediately reported). This idle flag is
      --  asserted after hard reset or by hardware when the channel is back in
      --  idle state (in suspended or disabled state).
      IDLEF          : Boolean;
      --  unspecified
      Reserved_1_7   : HAL.UInt7;
      --  Read-only. transfer complete flag A transfer complete event is a
      --  block transfer complete or a LLI transfer complete including the
      --  upload of the next LLI if any, or the full linked-list completion,
      --  depending on the transfer complete event mode LPDMA_CxTR2.TCEM[1:0].
      TCF            : Boolean;
      --  Read-only. half transfer flag An half transfer event is an half block
      --  transfer that occurs when half of the bytes of the source block size
      --  (rounded-up integer of LPDMA_CxBR1.BNDT[15:0] / 2) has been
      --  transferred to the destination.
      HTF            : Boolean;
      --  Read-only. data transfer error flag
      DTEF           : Boolean;
      --  Read-only. update link transfer error flag
      ULEF           : Boolean;
      --  Read-only. user setting error flag
      USEF           : Boolean;
      --  Read-only. completed suspension flag
      SUSPF          : Boolean;
      --  Read-only. trigger overrun flag clear
      TOF            : Boolean;
      --  unspecified
      Reserved_15_31 : HAL.UInt17;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPDMA_C0SR_Register use record
      IDLEF          at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      TCF            at 0 range 8 .. 8;
      HTF            at 0 range 9 .. 9;
      DTEF           at 0 range 10 .. 10;
      ULEF           at 0 range 11 .. 11;
      USEF           at 0 range 12 .. 12;
      SUSPF          at 0 range 13 .. 13;
      TOF            at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype LPDMA_C0CR_PRIO_Field is HAL.UInt2;

   --  LPDMA channel 0 control register
   type LPDMA_C0CR_Register is record
      --  enable Writing 1 into the field RESET (bit 1) causes the hardware to
      --  de-assert this bit, whatever is written into this bit 0. Else: this
      --  bit is de-asserted by hardware when there is a transfer error (master
      --  bus error or user setting error) or when there is a channel transfer
      --  complete (channel ready to be configured, e.g. if LSM=1 at the end of
      --  a single execution of the LLI). Else, this bit can be asserted by
      --  software. Writing 0 into this EN bit is ignored.
      EN             : Boolean := False;
      --  Write-only. reset This bit is write only. Writing 0 has no impact.
      --  Writing 1 implies the reset of the following: the channel internal
      --  state, SUSP and EN bits (whatever is written receptively in bit 2 and
      --  bit 0). The reset is effective when the channel is in steady state,
      --  meaning one of the following: - active channel in suspended state
      --  (LPDMA_CxSR.SUSPF = 1 and LPDMA_CxSR.IDLEF = LPDMA_CxCR.EN = 1)
      --  - channel in disabled state (LPDMA_CxSR.IDLEF = 1 and
      --  LPDMA_CxCR.EN = 0). After writing a RESET, to continue using this
      --  channel, the user must explicitly reconfigure the channel including
      --  the hardware-modified configuration registers (LPDMA_CxBR1,
      --  LPDMA_CxSAR and LPDMA_CxDAR) before enabling again the channel (see
      --  the programming sequence in ).
      RESET          : Boolean := False;
      --  suspend Writing 1 into the field RESET (bit 1) causes the hardware to
      --  de-assert this bit, whatever is written into this bit 2. Else:
      --  Software must write 1 in order to suspend an active channel i.e. a
      --  channel with an on-going DMA transfer over its master ports. The
      --  software must write 0 in order to resume a suspended channel,
      --  following the programming sequence detailed in sequence.
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
      --  Link step mode First the block transfer is executed as defined by the
      --  current internal register file until LPDMA_CxBR1.BNDT[15:0 ] =0).
      --  Secondly the next linked-list data structure is conditionally
      --  uploaded from memory as defined by LPDMA_CxLLR. Then channel
      --  execution is completed. Note: This bit must be written when EN=0.
      --  This bit is read-only when EN=1.
      LSM            : Boolean := False;
      --  unspecified
      Reserved_17_21 : HAL.UInt5 := 16#0#;
      --  priority level of the channel x LPDMA transfer versus others Note:
      --  This bit must be written when EN = 0. This bit is read-only when
      --  EN = 1.
      PRIO           : LPDMA_C0CR_PRIO_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPDMA_C0CR_Register use record
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
      Reserved_17_21 at 0 range 17 .. 21;
      PRIO           at 0 range 22 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype LPDMA_C0TR1_SDW_LOG2_Field is HAL.UInt2;
   subtype LPDMA_C0TR1_DDW_LOG2_Field is HAL.UInt2;

   --  LPDMA channel 0 transfer register 1
   type LPDMA_C0TR1_Register is record
      --  binary logarithm of the source data width of a single in bytes Note:
      --  Setting a 8-byte data width causes a user setting error to be
      --  reported and none transfer is issued. a source block size must be a
      --  multiple of the source data width (LPDMA_CxBR1.BNDT[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and no
      --  transfer is issued. A source single transfer must have an aligned
      --  address with its data width (start address LPDMA_CxSAR[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and none
      --  transfer is issued.
      SDW_LOG2       : LPDMA_C0TR1_SDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  source incrementing single The source address, pointed by
      --  LPDMA_CxSAR, is kept constant after a single transfer or is
      --  incremented by the offset value corresponding to a contiguous data
      --  after a single transfer.
      SINC           : Boolean := False;
      --  unspecified
      Reserved_4_10  : HAL.UInt7 := 16#0#;
      --  padding/alignment mode If DDW_LOG2[1:0]=SDW_LOG2[1:0]: if the data
      --  width of a single destination transfer is equal to the data width of
      --  a single source transfer, this bit is ignored. Else: Case 1: If
      --  destination data width > source data width Case 2: If destination
      --  data width < source data width
      PAM            : Boolean := False;
      --  unspecified
      Reserved_12_14 : HAL.UInt3 := 16#0#;
      --  security attribute of the LPDMA transfer from the source If
      --  LPDMA_SECCFGR.SECx = 1 and the access is secure: This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  LPDMA_SECCFGR.SECx =1 . A secure write is ignored when
      --  LPDMA_SECCFGR.SECx = 0. When LPDMA_SECCFGR.SECx is de-asserted,
      --  this SSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the LPDMA transfer
      --  from the source is non-secure.
      SSEC           : Boolean := False;
      --  binary logarithm of the destination data width of a single in bytes
      --  Note: Setting a 8-byte data width causes a user setting error to be
      --  reported and none transfer is issued. A destination single transfer
      --  must have an aligned address with its data width (start address
      --  LPDMA_CxDAR[2:0] versus DDW_LOG2[1:0]). Otherwise a user setting
      --  error is reported and none transfer is issued.
      DDW_LOG2       : LPDMA_C0TR1_DDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_18_18 : HAL.Bit := 16#0#;
      --  destination incrementing single The destination address, pointed by
      --  LPDMA_CxDAR, is kept constant after a single transfer, or is
      --  incremented by the offset value corresponding to a contiguous data
      --  after a single transfer.
      DINC           : Boolean := False;
      --  unspecified
      Reserved_20_30 : HAL.UInt11 := 16#0#;
      --  security attribute of the LPDMA transfer to the destination If
      --  LPDMA_SECCFGR.SECx = 1 and the access is secure: This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  LPDMA_SECCFGR.SECx = 1. A secure write is ignored when
      --  LPDMA_SECCFGR.SECx = 0. When LPDMA_SECCFGR.SECx is de-asserted,
      --  this DSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the LPDMA transfer
      --  to the destination is non-secure.
      DSEC           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPDMA_C0TR1_Register use record
      SDW_LOG2       at 0 range 0 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      SINC           at 0 range 3 .. 3;
      Reserved_4_10  at 0 range 4 .. 10;
      PAM            at 0 range 11 .. 11;
      Reserved_12_14 at 0 range 12 .. 14;
      SSEC           at 0 range 15 .. 15;
      DDW_LOG2       at 0 range 16 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      DINC           at 0 range 19 .. 19;
      Reserved_20_30 at 0 range 20 .. 30;
      DSEC           at 0 range 31 .. 31;
   end record;

   subtype LPDMA_C0TR2_REQSEL_Field is HAL.UInt5;
   subtype LPDMA_C0TR2_TRIGM_Field is HAL.UInt2;
   subtype LPDMA_C0TR2_TRIGSEL_Field is HAL.UInt5;
   subtype LPDMA_C0TR2_TRIGPOL_Field is HAL.UInt2;
   subtype LPDMA_C0TR2_TCEM_Field is HAL.UInt2;

   --  LPDMA channel 0 transfer register 2
   type LPDMA_C0TR2_Register is record
      --  DMA hardware request selection These bits are ignored if channel x is
      --  activated (LPDMA_CxCR.EN asserted) with SWREQ = 1 (software request
      --  for a memory-to-memory transfer). Else, the selected hardware request
      --  is internally taken into account as per request. Note: The user must
      --  not assign a same input hardware request (same REQSEL[4:0] value) to
      --  different active DMA channels (LPDMA_CxCR.EN = 1 and
      --  LPDMA_CxTR2.SWREQ = 0 for these channels). DMA is not intended to
      --  hardware support the case of simultaneous enabled channels
      --  incorrectly configured with a same hardware peripheral request
      --  signal, and there is no user setting error reporting.
      REQSEL         : LPDMA_C0TR2_REQSEL_Field := 16#0#;
      --  unspecified
      Reserved_5_8   : HAL.UInt4 := 16#0#;
      --  software request This bit is internally taken into account when
      --  LPDMA_CxCR.EN is asserted.
      SWREQ          : Boolean := False;
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  block hardware request If the channel x is activated (LPDMA_CxCR.EN
      --  asserted) with SWREQ = 1 (software request for a memory-to-memory
      --  transfer), this bit is ignored. Else:
      BREQ           : Boolean := False;
      --  unspecified
      Reserved_12_13 : HAL.UInt2 := 16#0#;
      --  trigger mode These bits define the transfer granularity for its
      --  conditioning by the trigger. If the channel x is enabled
      --  (LPDMA_CxCR.EN asserted) with TRIGPOL[1:0] = 0b00 or 0b11, these
      --  TRIGM[1:0] bits are ignored. Else, a DMA transfer is conditioned by
      --  at least one trigger hit: The LPDMA monitoring of a trigger for
      --  channel x is started when the channel is enabled/loaded with a new
      --  active trigger configuration: rising or falling edge on a selected
      --  trigger (TRIGPOL[1:0] = 0b01 or respectively
      --  TRIGPOL[1:0] = 0b10). The monitoring of this trigger is kept active
      --  during the triggered and uncompleted (data or link) transfer; and if
      --  a new trigger is detected then, this hit is internally memorized to
      --  grant the next transfer, as long as the defined rising or falling
      --  edge is not modified, and the TRIGSEL[4:0] is not modified, and the
      --  channel is enabled. Transferring a next LLIn+1 that updates the
      --  LPDMA_CxTR2 with a new value for any of TRIGSEL[4:0] or TRIGPOL[1:0],
      --  resets the monitoring, trashing the memorized hit of the formerly
      --  defined LLIn trigger. After a first new trigger hitn+1 is memorized,
      --  if another second trigger hitn+2 is detected and if the hitn
      --  triggered transfer is still not completed, hitn+2 is lost and not
      --  memorized, and a trigger overrun flag is reported
      --  (LPDMA_CxSR.TOF = 1), an interrupt is generated if enabled
      --  (LPDMA_CxCR.TOIE = 1). The channel is not automatically disabled by
      --  hardware due to a trigger overrun.
      TRIGM          : LPDMA_C0TR2_TRIGM_Field := 16#0#;
      --  trigger event input selection These bits select the trigger event
      --  input of the LPDMA transfer (as per Programmed LPDMA1 trigger), with
      --  an active trigger event if TRIGPOL[1:0] = 00.
      TRIGSEL        : LPDMA_C0TR2_TRIGSEL_Field := 16#0#;
      --  unspecified
      Reserved_21_23 : HAL.UInt3 := 16#0#;
      --  trigger event polarity These bits define the polarity of the selected
      --  trigger event input defined by TRIGSEL[4:0].
      TRIGPOL        : LPDMA_C0TR2_TRIGPOL_Field := 16#0#;
      --  unspecified
      Reserved_26_29 : HAL.UInt4 := 16#0#;
      --  transfer complete event mode These bits define the transfer
      --  granularity for the transfer complete and half transfer complete
      --  events generation. Note: If the initial LLI0 data transfer is
      --  null/void (directly programmed by the internal register file with
      --  LPDMA_CxBR1.BNDT[15:0] = 0), then neither the complete transfer
      --  event nor the half transfer event is generated. Note: If the initial
      --  LLI0 data transfer is null/void (i.e. directly programmed by the
      --  internal register file with LPDMA_CxBR1.BNDT[15:0] =0 ), then the
      --  half transfer event is not generated, and the transfer complete event
      --  is generated when is completed the loading of the LLI1.
      TCEM           : LPDMA_C0TR2_TCEM_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPDMA_C0TR2_Register use record
      REQSEL         at 0 range 0 .. 4;
      Reserved_5_8   at 0 range 5 .. 8;
      SWREQ          at 0 range 9 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      BREQ           at 0 range 11 .. 11;
      Reserved_12_13 at 0 range 12 .. 13;
      TRIGM          at 0 range 14 .. 15;
      TRIGSEL        at 0 range 16 .. 20;
      Reserved_21_23 at 0 range 21 .. 23;
      TRIGPOL        at 0 range 24 .. 25;
      Reserved_26_29 at 0 range 26 .. 29;
      TCEM           at 0 range 30 .. 31;
   end record;

   subtype LPDMA_C0BR1_BNDT_Field is HAL.UInt16;

   --  LPDMA channel 0 block register 1
   type LPDMA_C0BR1_Register is record
      --  block number of data bytes to transfer from the source Block size
      --  transferred from the source. When the channel is enabled, this field
      --  becomes read-only and is decremented, indicating the remaining number
      --  of data items in the current source block to be transferred.
      --  BNDT[15:0] is programmed in number of bytes, maximum source block
      --  size is 64 Kbytes -1. Once the last data transfer is completed
      --  (BNDT[15:0] = 0): - if LPDMA_CxLLR.UB1 = 1, this field is updated
      --  by the LLI in the memory. - if LPDMA_CxLLR.UB1 = 0 and if there is
      --  at least one non null Uxx update bit, this field is internally
      --  restored to the programmed value. - if all LPDMA_CxLLR.Uxx = 0 and
      --  if LPDMA_CxLLR.LA[15:0] = 0, this field is internally restored to
      --  the programmed value (infinite/continuous last LLI). - if
      --  LPDMA_CxLLR = 0, this field is kept as zero following the last LLI
      --  data transfer. Note: A non-null source block size must be a multiple
      --  of the source data width (BNDT[2:0] versus
      --  LPDMA_CxTR1.SDW_LOG2[1:0]). Else a user setting error is reported and
      --  none transfer is issued.
      BNDT           : LPDMA_C0BR1_BNDT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPDMA_C0BR1_Register use record
      BNDT           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype LPDMA_C0LLR_LA_Field is HAL.UInt14;

   --  LPDMA channel 0 linked-list address register
   type LPDMA_C0LLR_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  pointer (16-bit low-significant address) to the next linked-list data
      --  structure If UT1 = UT2 = UB1 = USA = UDA = ULL = 0 and if
      --  LA[15:20] = 0, the current LLI is the last one. The channel
      --  transfer is completed without any update of the linked-list DMA
      --  register file. Else, this field is the pointer to the memory address
      --  offset from which the next linked-list data structure is
      --  automatically fetched from, once the data transfer is completed, in
      --  order to conditionally update the linked-list DMA internal register
      --  file (LPDMA_CxCTR1, LPDMA_CxTR2, LPDMA_CxBR1, LPDMA_CxSAR,
      --  LPDMA_CxDAR and LPDMA_CxLLR). Note: The user must program the pointer
      --  to be 32-bit aligned. The two low-significant bits are write ignored.
      LA             : LPDMA_C0LLR_LA_Field := 16#0#;
      --  Update LPDMA_CxLLR register from memory This bit is used to control
      --  the update of the LPDMA_CxLLR register from the memory during the
      --  link transfer.
      ULL            : Boolean := False;
      --  unspecified
      Reserved_17_26 : HAL.UInt10 := 16#0#;
      --  Update LPDMA_CxDAR register from memory This bit is used to control
      --  the update of the LPDMA_CxDAR register from the memory during the
      --  link transfer.
      UDA            : Boolean := False;
      --  update LPDMA_CxSAR from memory This bit controls the update of the
      --  LPDMA_CxSAR register from the memory during the link transfer.
      USA            : Boolean := False;
      --  Update LPDMA_CxBR1 from memory This bit controls the update of the
      --  LPDMA_CxBR1 register from the memory during the link transfer.
      UB1            : Boolean := False;
      --  Update LPDMA_CxTR2 from memory This bit controls the update of the
      --  LPDMA_CxTR2 register from the memory during the link transfer.
      UT2            : Boolean := False;
      --  Update LPDMA_CxTR1 from memory This bit controls the update of the
      --  LPDMA_CxTR1 register from the memory during the link transfer.
      UT1            : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPDMA_C0LLR_Register use record
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

   subtype LPDMA_C1LBAR_LBA_Field is HAL.UInt16;

   --  LPDMA channel 1 linked-list base address register
   type LPDMA_C1LBAR_Register is record
      --  unspecified
      Reserved_0_15 : HAL.UInt16 := 16#0#;
      --  linked-list base address of LPDMA channel x
      LBA           : LPDMA_C1LBAR_LBA_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPDMA_C1LBAR_Register use record
      Reserved_0_15 at 0 range 0 .. 15;
      LBA           at 0 range 16 .. 31;
   end record;

   --  LPDMA channel 1 flag clear register
   type LPDMA_C1FCR_Register is record
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

   for LPDMA_C1FCR_Register use record
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

   --  LPDMA channel 1 status register
   type LPDMA_C1SR_Register is record
      --  Read-only. idle flag This idle flag is de-asserted by hardware when
      --  the channel is enabled (LPDMA_CxCR.EN = 1) with a valid channel
      --  configuration (no USEF to be immediately reported). This idle flag is
      --  asserted after hard reset or by hardware when the channel is back in
      --  idle state (in suspended or disabled state).
      IDLEF          : Boolean;
      --  unspecified
      Reserved_1_7   : HAL.UInt7;
      --  Read-only. transfer complete flag A transfer complete event is a
      --  block transfer complete or a LLI transfer complete including the
      --  upload of the next LLI if any, or the full linked-list completion,
      --  depending on the transfer complete event mode LPDMA_CxTR2.TCEM[1:0].
      TCF            : Boolean;
      --  Read-only. half transfer flag An half transfer event is an half block
      --  transfer that occurs when half of the bytes of the source block size
      --  (rounded-up integer of LPDMA_CxBR1.BNDT[15:0] / 2) has been
      --  transferred to the destination.
      HTF            : Boolean;
      --  Read-only. data transfer error flag
      DTEF           : Boolean;
      --  Read-only. update link transfer error flag
      ULEF           : Boolean;
      --  Read-only. user setting error flag
      USEF           : Boolean;
      --  Read-only. completed suspension flag
      SUSPF          : Boolean;
      --  Read-only. trigger overrun flag clear
      TOF            : Boolean;
      --  unspecified
      Reserved_15_31 : HAL.UInt17;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPDMA_C1SR_Register use record
      IDLEF          at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      TCF            at 0 range 8 .. 8;
      HTF            at 0 range 9 .. 9;
      DTEF           at 0 range 10 .. 10;
      ULEF           at 0 range 11 .. 11;
      USEF           at 0 range 12 .. 12;
      SUSPF          at 0 range 13 .. 13;
      TOF            at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype LPDMA_C1CR_PRIO_Field is HAL.UInt2;

   --  LPDMA channel 1 control register
   type LPDMA_C1CR_Register is record
      --  enable Writing 1 into the field RESET (bit 1) causes the hardware to
      --  de-assert this bit, whatever is written into this bit 0. Else: this
      --  bit is de-asserted by hardware when there is a transfer error (master
      --  bus error or user setting error) or when there is a channel transfer
      --  complete (channel ready to be configured, e.g. if LSM=1 at the end of
      --  a single execution of the LLI). Else, this bit can be asserted by
      --  software. Writing 0 into this EN bit is ignored.
      EN             : Boolean := False;
      --  Write-only. reset This bit is write only. Writing 0 has no impact.
      --  Writing 1 implies the reset of the following: the channel internal
      --  state, SUSP and EN bits (whatever is written receptively in bit 2 and
      --  bit 0). The reset is effective when the channel is in steady state,
      --  meaning one of the following: - active channel in suspended state
      --  (LPDMA_CxSR.SUSPF = 1 and LPDMA_CxSR.IDLEF = LPDMA_CxCR.EN = 1)
      --  - channel in disabled state (LPDMA_CxSR.IDLEF = 1 and
      --  LPDMA_CxCR.EN = 0). After writing a RESET, to continue using this
      --  channel, the user must explicitly reconfigure the channel including
      --  the hardware-modified configuration registers (LPDMA_CxBR1,
      --  LPDMA_CxSAR and LPDMA_CxDAR) before enabling again the channel (see
      --  the programming sequence in ).
      RESET          : Boolean := False;
      --  suspend Writing 1 into the field RESET (bit 1) causes the hardware to
      --  de-assert this bit, whatever is written into this bit 2. Else:
      --  Software must write 1 in order to suspend an active channel i.e. a
      --  channel with an on-going DMA transfer over its master ports. The
      --  software must write 0 in order to resume a suspended channel,
      --  following the programming sequence detailed in sequence.
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
      --  Link step mode First the block transfer is executed as defined by the
      --  current internal register file until LPDMA_CxBR1.BNDT[15:0 ] =0).
      --  Secondly the next linked-list data structure is conditionally
      --  uploaded from memory as defined by LPDMA_CxLLR. Then channel
      --  execution is completed. Note: This bit must be written when EN=0.
      --  This bit is read-only when EN=1.
      LSM            : Boolean := False;
      --  unspecified
      Reserved_17_21 : HAL.UInt5 := 16#0#;
      --  priority level of the channel x LPDMA transfer versus others Note:
      --  This bit must be written when EN = 0. This bit is read-only when
      --  EN = 1.
      PRIO           : LPDMA_C1CR_PRIO_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPDMA_C1CR_Register use record
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
      Reserved_17_21 at 0 range 17 .. 21;
      PRIO           at 0 range 22 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype LPDMA_C1TR1_SDW_LOG2_Field is HAL.UInt2;
   subtype LPDMA_C1TR1_DDW_LOG2_Field is HAL.UInt2;

   --  LPDMA channel 1 transfer register 1
   type LPDMA_C1TR1_Register is record
      --  binary logarithm of the source data width of a single in bytes Note:
      --  Setting a 8-byte data width causes a user setting error to be
      --  reported and none transfer is issued. a source block size must be a
      --  multiple of the source data width (LPDMA_CxBR1.BNDT[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and no
      --  transfer is issued. A source single transfer must have an aligned
      --  address with its data width (start address LPDMA_CxSAR[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and none
      --  transfer is issued.
      SDW_LOG2       : LPDMA_C1TR1_SDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  source incrementing single The source address, pointed by
      --  LPDMA_CxSAR, is kept constant after a single transfer or is
      --  incremented by the offset value corresponding to a contiguous data
      --  after a single transfer.
      SINC           : Boolean := False;
      --  unspecified
      Reserved_4_10  : HAL.UInt7 := 16#0#;
      --  padding/alignment mode If DDW_LOG2[1:0]=SDW_LOG2[1:0]: if the data
      --  width of a single destination transfer is equal to the data width of
      --  a single source transfer, this bit is ignored. Else: Case 1: If
      --  destination data width > source data width Case 2: If destination
      --  data width < source data width
      PAM            : Boolean := False;
      --  unspecified
      Reserved_12_14 : HAL.UInt3 := 16#0#;
      --  security attribute of the LPDMA transfer from the source If
      --  LPDMA_SECCFGR.SECx = 1 and the access is secure: This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  LPDMA_SECCFGR.SECx =1 . A secure write is ignored when
      --  LPDMA_SECCFGR.SECx = 0. When LPDMA_SECCFGR.SECx is de-asserted,
      --  this SSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the LPDMA transfer
      --  from the source is non-secure.
      SSEC           : Boolean := False;
      --  binary logarithm of the destination data width of a single in bytes
      --  Note: Setting a 8-byte data width causes a user setting error to be
      --  reported and none transfer is issued. A destination single transfer
      --  must have an aligned address with its data width (start address
      --  LPDMA_CxDAR[2:0] versus DDW_LOG2[1:0]). Otherwise a user setting
      --  error is reported and none transfer is issued.
      DDW_LOG2       : LPDMA_C1TR1_DDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_18_18 : HAL.Bit := 16#0#;
      --  destination incrementing single The destination address, pointed by
      --  LPDMA_CxDAR, is kept constant after a single transfer, or is
      --  incremented by the offset value corresponding to a contiguous data
      --  after a single transfer.
      DINC           : Boolean := False;
      --  unspecified
      Reserved_20_30 : HAL.UInt11 := 16#0#;
      --  security attribute of the LPDMA transfer to the destination If
      --  LPDMA_SECCFGR.SECx = 1 and the access is secure: This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  LPDMA_SECCFGR.SECx = 1. A secure write is ignored when
      --  LPDMA_SECCFGR.SECx = 0. When LPDMA_SECCFGR.SECx is de-asserted,
      --  this DSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the LPDMA transfer
      --  to the destination is non-secure.
      DSEC           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPDMA_C1TR1_Register use record
      SDW_LOG2       at 0 range 0 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      SINC           at 0 range 3 .. 3;
      Reserved_4_10  at 0 range 4 .. 10;
      PAM            at 0 range 11 .. 11;
      Reserved_12_14 at 0 range 12 .. 14;
      SSEC           at 0 range 15 .. 15;
      DDW_LOG2       at 0 range 16 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      DINC           at 0 range 19 .. 19;
      Reserved_20_30 at 0 range 20 .. 30;
      DSEC           at 0 range 31 .. 31;
   end record;

   subtype LPDMA_C1TR2_REQSEL_Field is HAL.UInt5;
   subtype LPDMA_C1TR2_TRIGM_Field is HAL.UInt2;
   subtype LPDMA_C1TR2_TRIGSEL_Field is HAL.UInt5;
   subtype LPDMA_C1TR2_TRIGPOL_Field is HAL.UInt2;
   subtype LPDMA_C1TR2_TCEM_Field is HAL.UInt2;

   --  LPDMA channel 1 transfer register 2
   type LPDMA_C1TR2_Register is record
      --  DMA hardware request selection These bits are ignored if channel x is
      --  activated (LPDMA_CxCR.EN asserted) with SWREQ = 1 (software request
      --  for a memory-to-memory transfer). Else, the selected hardware request
      --  is internally taken into account as per request. Note: The user must
      --  not assign a same input hardware request (same REQSEL[4:0] value) to
      --  different active DMA channels (LPDMA_CxCR.EN = 1 and
      --  LPDMA_CxTR2.SWREQ = 0 for these channels). DMA is not intended to
      --  hardware support the case of simultaneous enabled channels
      --  incorrectly configured with a same hardware peripheral request
      --  signal, and there is no user setting error reporting.
      REQSEL         : LPDMA_C1TR2_REQSEL_Field := 16#0#;
      --  unspecified
      Reserved_5_8   : HAL.UInt4 := 16#0#;
      --  software request This bit is internally taken into account when
      --  LPDMA_CxCR.EN is asserted.
      SWREQ          : Boolean := False;
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  block hardware request If the channel x is activated (LPDMA_CxCR.EN
      --  asserted) with SWREQ = 1 (software request for a memory-to-memory
      --  transfer), this bit is ignored. Else:
      BREQ           : Boolean := False;
      --  unspecified
      Reserved_12_13 : HAL.UInt2 := 16#0#;
      --  trigger mode These bits define the transfer granularity for its
      --  conditioning by the trigger. If the channel x is enabled
      --  (LPDMA_CxCR.EN asserted) with TRIGPOL[1:0] = 0b00 or 0b11, these
      --  TRIGM[1:0] bits are ignored. Else, a DMA transfer is conditioned by
      --  at least one trigger hit: The LPDMA monitoring of a trigger for
      --  channel x is started when the channel is enabled/loaded with a new
      --  active trigger configuration: rising or falling edge on a selected
      --  trigger (TRIGPOL[1:0] = 0b01 or respectively
      --  TRIGPOL[1:0] = 0b10). The monitoring of this trigger is kept active
      --  during the triggered and uncompleted (data or link) transfer; and if
      --  a new trigger is detected then, this hit is internally memorized to
      --  grant the next transfer, as long as the defined rising or falling
      --  edge is not modified, and the TRIGSEL[4:0] is not modified, and the
      --  channel is enabled. Transferring a next LLIn+1 that updates the
      --  LPDMA_CxTR2 with a new value for any of TRIGSEL[4:0] or TRIGPOL[1:0],
      --  resets the monitoring, trashing the memorized hit of the formerly
      --  defined LLIn trigger. After a first new trigger hitn+1 is memorized,
      --  if another second trigger hitn+2 is detected and if the hitn
      --  triggered transfer is still not completed, hitn+2 is lost and not
      --  memorized, and a trigger overrun flag is reported
      --  (LPDMA_CxSR.TOF = 1), an interrupt is generated if enabled
      --  (LPDMA_CxCR.TOIE = 1). The channel is not automatically disabled by
      --  hardware due to a trigger overrun.
      TRIGM          : LPDMA_C1TR2_TRIGM_Field := 16#0#;
      --  trigger event input selection These bits select the trigger event
      --  input of the LPDMA transfer (as per Programmed LPDMA1 trigger), with
      --  an active trigger event if TRIGPOL[1:0] = 00.
      TRIGSEL        : LPDMA_C1TR2_TRIGSEL_Field := 16#0#;
      --  unspecified
      Reserved_21_23 : HAL.UInt3 := 16#0#;
      --  trigger event polarity These bits define the polarity of the selected
      --  trigger event input defined by TRIGSEL[4:0].
      TRIGPOL        : LPDMA_C1TR2_TRIGPOL_Field := 16#0#;
      --  unspecified
      Reserved_26_29 : HAL.UInt4 := 16#0#;
      --  transfer complete event mode These bits define the transfer
      --  granularity for the transfer complete and half transfer complete
      --  events generation. Note: If the initial LLI0 data transfer is
      --  null/void (directly programmed by the internal register file with
      --  LPDMA_CxBR1.BNDT[15:0] = 0), then neither the complete transfer
      --  event nor the half transfer event is generated. Note: If the initial
      --  LLI0 data transfer is null/void (i.e. directly programmed by the
      --  internal register file with LPDMA_CxBR1.BNDT[15:0] =0 ), then the
      --  half transfer event is not generated, and the transfer complete event
      --  is generated when is completed the loading of the LLI1.
      TCEM           : LPDMA_C1TR2_TCEM_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPDMA_C1TR2_Register use record
      REQSEL         at 0 range 0 .. 4;
      Reserved_5_8   at 0 range 5 .. 8;
      SWREQ          at 0 range 9 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      BREQ           at 0 range 11 .. 11;
      Reserved_12_13 at 0 range 12 .. 13;
      TRIGM          at 0 range 14 .. 15;
      TRIGSEL        at 0 range 16 .. 20;
      Reserved_21_23 at 0 range 21 .. 23;
      TRIGPOL        at 0 range 24 .. 25;
      Reserved_26_29 at 0 range 26 .. 29;
      TCEM           at 0 range 30 .. 31;
   end record;

   subtype LPDMA_C1BR1_BNDT_Field is HAL.UInt16;

   --  LPDMA channel 1 block register 1
   type LPDMA_C1BR1_Register is record
      --  block number of data bytes to transfer from the source Block size
      --  transferred from the source. When the channel is enabled, this field
      --  becomes read-only and is decremented, indicating the remaining number
      --  of data items in the current source block to be transferred.
      --  BNDT[15:0] is programmed in number of bytes, maximum source block
      --  size is 64 Kbytes -1. Once the last data transfer is completed
      --  (BNDT[15:0] = 0): - if LPDMA_CxLLR.UB1 = 1, this field is updated
      --  by the LLI in the memory. - if LPDMA_CxLLR.UB1 = 0 and if there is
      --  at least one non null Uxx update bit, this field is internally
      --  restored to the programmed value. - if all LPDMA_CxLLR.Uxx = 0 and
      --  if LPDMA_CxLLR.LA[15:0] = 0, this field is internally restored to
      --  the programmed value (infinite/continuous last LLI). - if
      --  LPDMA_CxLLR = 0, this field is kept as zero following the last LLI
      --  data transfer. Note: A non-null source block size must be a multiple
      --  of the source data width (BNDT[2:0] versus
      --  LPDMA_CxTR1.SDW_LOG2[1:0]). Else a user setting error is reported and
      --  none transfer is issued.
      BNDT           : LPDMA_C1BR1_BNDT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPDMA_C1BR1_Register use record
      BNDT           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype LPDMA_C1LLR_LA_Field is HAL.UInt14;

   --  LPDMA channel 1 linked-list address register
   type LPDMA_C1LLR_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  pointer (16-bit low-significant address) to the next linked-list data
      --  structure If UT1 = UT2 = UB1 = USA = UDA = ULL = 0 and if
      --  LA[15:20] = 0, the current LLI is the last one. The channel
      --  transfer is completed without any update of the linked-list DMA
      --  register file. Else, this field is the pointer to the memory address
      --  offset from which the next linked-list data structure is
      --  automatically fetched from, once the data transfer is completed, in
      --  order to conditionally update the linked-list DMA internal register
      --  file (LPDMA_CxCTR1, LPDMA_CxTR2, LPDMA_CxBR1, LPDMA_CxSAR,
      --  LPDMA_CxDAR and LPDMA_CxLLR). Note: The user must program the pointer
      --  to be 32-bit aligned. The two low-significant bits are write ignored.
      LA             : LPDMA_C1LLR_LA_Field := 16#0#;
      --  Update LPDMA_CxLLR register from memory This bit is used to control
      --  the update of the LPDMA_CxLLR register from the memory during the
      --  link transfer.
      ULL            : Boolean := False;
      --  unspecified
      Reserved_17_26 : HAL.UInt10 := 16#0#;
      --  Update LPDMA_CxDAR register from memory This bit is used to control
      --  the update of the LPDMA_CxDAR register from the memory during the
      --  link transfer.
      UDA            : Boolean := False;
      --  update LPDMA_CxSAR from memory This bit controls the update of the
      --  LPDMA_CxSAR register from the memory during the link transfer.
      USA            : Boolean := False;
      --  Update LPDMA_CxBR1 from memory This bit controls the update of the
      --  LPDMA_CxBR1 register from the memory during the link transfer.
      UB1            : Boolean := False;
      --  Update LPDMA_CxTR2 from memory This bit controls the update of the
      --  LPDMA_CxTR2 register from the memory during the link transfer.
      UT2            : Boolean := False;
      --  Update LPDMA_CxTR1 from memory This bit controls the update of the
      --  LPDMA_CxTR1 register from the memory during the link transfer.
      UT1            : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPDMA_C1LLR_Register use record
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

   subtype LPDMA_C2LBAR_LBA_Field is HAL.UInt16;

   --  LPDMA channel 2 linked-list base address register
   type LPDMA_C2LBAR_Register is record
      --  unspecified
      Reserved_0_15 : HAL.UInt16 := 16#0#;
      --  linked-list base address of LPDMA channel x
      LBA           : LPDMA_C2LBAR_LBA_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPDMA_C2LBAR_Register use record
      Reserved_0_15 at 0 range 0 .. 15;
      LBA           at 0 range 16 .. 31;
   end record;

   --  LPDMA channel 2 flag clear register
   type LPDMA_C2FCR_Register is record
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

   for LPDMA_C2FCR_Register use record
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

   --  LPDMA channel 2 status register
   type LPDMA_C2SR_Register is record
      --  Read-only. idle flag This idle flag is de-asserted by hardware when
      --  the channel is enabled (LPDMA_CxCR.EN = 1) with a valid channel
      --  configuration (no USEF to be immediately reported). This idle flag is
      --  asserted after hard reset or by hardware when the channel is back in
      --  idle state (in suspended or disabled state).
      IDLEF          : Boolean;
      --  unspecified
      Reserved_1_7   : HAL.UInt7;
      --  Read-only. transfer complete flag A transfer complete event is a
      --  block transfer complete or a LLI transfer complete including the
      --  upload of the next LLI if any, or the full linked-list completion,
      --  depending on the transfer complete event mode LPDMA_CxTR2.TCEM[1:0].
      TCF            : Boolean;
      --  Read-only. half transfer flag An half transfer event is an half block
      --  transfer that occurs when half of the bytes of the source block size
      --  (rounded-up integer of LPDMA_CxBR1.BNDT[15:0] / 2) has been
      --  transferred to the destination.
      HTF            : Boolean;
      --  Read-only. data transfer error flag
      DTEF           : Boolean;
      --  Read-only. update link transfer error flag
      ULEF           : Boolean;
      --  Read-only. user setting error flag
      USEF           : Boolean;
      --  Read-only. completed suspension flag
      SUSPF          : Boolean;
      --  Read-only. trigger overrun flag clear
      TOF            : Boolean;
      --  unspecified
      Reserved_15_31 : HAL.UInt17;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPDMA_C2SR_Register use record
      IDLEF          at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      TCF            at 0 range 8 .. 8;
      HTF            at 0 range 9 .. 9;
      DTEF           at 0 range 10 .. 10;
      ULEF           at 0 range 11 .. 11;
      USEF           at 0 range 12 .. 12;
      SUSPF          at 0 range 13 .. 13;
      TOF            at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype LPDMA_C2CR_PRIO_Field is HAL.UInt2;

   --  LPDMA channel 2 control register
   type LPDMA_C2CR_Register is record
      --  enable Writing 1 into the field RESET (bit 1) causes the hardware to
      --  de-assert this bit, whatever is written into this bit 0. Else: this
      --  bit is de-asserted by hardware when there is a transfer error (master
      --  bus error or user setting error) or when there is a channel transfer
      --  complete (channel ready to be configured, e.g. if LSM=1 at the end of
      --  a single execution of the LLI). Else, this bit can be asserted by
      --  software. Writing 0 into this EN bit is ignored.
      EN             : Boolean := False;
      --  Write-only. reset This bit is write only. Writing 0 has no impact.
      --  Writing 1 implies the reset of the following: the channel internal
      --  state, SUSP and EN bits (whatever is written receptively in bit 2 and
      --  bit 0). The reset is effective when the channel is in steady state,
      --  meaning one of the following: - active channel in suspended state
      --  (LPDMA_CxSR.SUSPF = 1 and LPDMA_CxSR.IDLEF = LPDMA_CxCR.EN = 1)
      --  - channel in disabled state (LPDMA_CxSR.IDLEF = 1 and
      --  LPDMA_CxCR.EN = 0). After writing a RESET, to continue using this
      --  channel, the user must explicitly reconfigure the channel including
      --  the hardware-modified configuration registers (LPDMA_CxBR1,
      --  LPDMA_CxSAR and LPDMA_CxDAR) before enabling again the channel (see
      --  the programming sequence in ).
      RESET          : Boolean := False;
      --  suspend Writing 1 into the field RESET (bit 1) causes the hardware to
      --  de-assert this bit, whatever is written into this bit 2. Else:
      --  Software must write 1 in order to suspend an active channel i.e. a
      --  channel with an on-going DMA transfer over its master ports. The
      --  software must write 0 in order to resume a suspended channel,
      --  following the programming sequence detailed in sequence.
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
      --  Link step mode First the block transfer is executed as defined by the
      --  current internal register file until LPDMA_CxBR1.BNDT[15:0 ] =0).
      --  Secondly the next linked-list data structure is conditionally
      --  uploaded from memory as defined by LPDMA_CxLLR. Then channel
      --  execution is completed. Note: This bit must be written when EN=0.
      --  This bit is read-only when EN=1.
      LSM            : Boolean := False;
      --  unspecified
      Reserved_17_21 : HAL.UInt5 := 16#0#;
      --  priority level of the channel x LPDMA transfer versus others Note:
      --  This bit must be written when EN = 0. This bit is read-only when
      --  EN = 1.
      PRIO           : LPDMA_C2CR_PRIO_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPDMA_C2CR_Register use record
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
      Reserved_17_21 at 0 range 17 .. 21;
      PRIO           at 0 range 22 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype LPDMA_C2TR1_SDW_LOG2_Field is HAL.UInt2;
   subtype LPDMA_C2TR1_DDW_LOG2_Field is HAL.UInt2;

   --  LPDMA channel 2 transfer register 1
   type LPDMA_C2TR1_Register is record
      --  binary logarithm of the source data width of a single in bytes Note:
      --  Setting a 8-byte data width causes a user setting error to be
      --  reported and none transfer is issued. a source block size must be a
      --  multiple of the source data width (LPDMA_CxBR1.BNDT[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and no
      --  transfer is issued. A source single transfer must have an aligned
      --  address with its data width (start address LPDMA_CxSAR[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and none
      --  transfer is issued.
      SDW_LOG2       : LPDMA_C2TR1_SDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  source incrementing single The source address, pointed by
      --  LPDMA_CxSAR, is kept constant after a single transfer or is
      --  incremented by the offset value corresponding to a contiguous data
      --  after a single transfer.
      SINC           : Boolean := False;
      --  unspecified
      Reserved_4_10  : HAL.UInt7 := 16#0#;
      --  padding/alignment mode If DDW_LOG2[1:0]=SDW_LOG2[1:0]: if the data
      --  width of a single destination transfer is equal to the data width of
      --  a single source transfer, this bit is ignored. Else: Case 1: If
      --  destination data width > source data width Case 2: If destination
      --  data width < source data width
      PAM            : Boolean := False;
      --  unspecified
      Reserved_12_14 : HAL.UInt3 := 16#0#;
      --  security attribute of the LPDMA transfer from the source If
      --  LPDMA_SECCFGR.SECx = 1 and the access is secure: This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  LPDMA_SECCFGR.SECx =1 . A secure write is ignored when
      --  LPDMA_SECCFGR.SECx = 0. When LPDMA_SECCFGR.SECx is de-asserted,
      --  this SSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the LPDMA transfer
      --  from the source is non-secure.
      SSEC           : Boolean := False;
      --  binary logarithm of the destination data width of a single in bytes
      --  Note: Setting a 8-byte data width causes a user setting error to be
      --  reported and none transfer is issued. A destination single transfer
      --  must have an aligned address with its data width (start address
      --  LPDMA_CxDAR[2:0] versus DDW_LOG2[1:0]). Otherwise a user setting
      --  error is reported and none transfer is issued.
      DDW_LOG2       : LPDMA_C2TR1_DDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_18_18 : HAL.Bit := 16#0#;
      --  destination incrementing single The destination address, pointed by
      --  LPDMA_CxDAR, is kept constant after a single transfer, or is
      --  incremented by the offset value corresponding to a contiguous data
      --  after a single transfer.
      DINC           : Boolean := False;
      --  unspecified
      Reserved_20_30 : HAL.UInt11 := 16#0#;
      --  security attribute of the LPDMA transfer to the destination If
      --  LPDMA_SECCFGR.SECx = 1 and the access is secure: This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  LPDMA_SECCFGR.SECx = 1. A secure write is ignored when
      --  LPDMA_SECCFGR.SECx = 0. When LPDMA_SECCFGR.SECx is de-asserted,
      --  this DSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the LPDMA transfer
      --  to the destination is non-secure.
      DSEC           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPDMA_C2TR1_Register use record
      SDW_LOG2       at 0 range 0 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      SINC           at 0 range 3 .. 3;
      Reserved_4_10  at 0 range 4 .. 10;
      PAM            at 0 range 11 .. 11;
      Reserved_12_14 at 0 range 12 .. 14;
      SSEC           at 0 range 15 .. 15;
      DDW_LOG2       at 0 range 16 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      DINC           at 0 range 19 .. 19;
      Reserved_20_30 at 0 range 20 .. 30;
      DSEC           at 0 range 31 .. 31;
   end record;

   subtype LPDMA_C2TR2_REQSEL_Field is HAL.UInt5;
   subtype LPDMA_C2TR2_TRIGM_Field is HAL.UInt2;
   subtype LPDMA_C2TR2_TRIGSEL_Field is HAL.UInt5;
   subtype LPDMA_C2TR2_TRIGPOL_Field is HAL.UInt2;
   subtype LPDMA_C2TR2_TCEM_Field is HAL.UInt2;

   --  LPDMA channel 2 transfer register 2
   type LPDMA_C2TR2_Register is record
      --  DMA hardware request selection These bits are ignored if channel x is
      --  activated (LPDMA_CxCR.EN asserted) with SWREQ = 1 (software request
      --  for a memory-to-memory transfer). Else, the selected hardware request
      --  is internally taken into account as per request. Note: The user must
      --  not assign a same input hardware request (same REQSEL[4:0] value) to
      --  different active DMA channels (LPDMA_CxCR.EN = 1 and
      --  LPDMA_CxTR2.SWREQ = 0 for these channels). DMA is not intended to
      --  hardware support the case of simultaneous enabled channels
      --  incorrectly configured with a same hardware peripheral request
      --  signal, and there is no user setting error reporting.
      REQSEL         : LPDMA_C2TR2_REQSEL_Field := 16#0#;
      --  unspecified
      Reserved_5_8   : HAL.UInt4 := 16#0#;
      --  software request This bit is internally taken into account when
      --  LPDMA_CxCR.EN is asserted.
      SWREQ          : Boolean := False;
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  block hardware request If the channel x is activated (LPDMA_CxCR.EN
      --  asserted) with SWREQ = 1 (software request for a memory-to-memory
      --  transfer), this bit is ignored. Else:
      BREQ           : Boolean := False;
      --  unspecified
      Reserved_12_13 : HAL.UInt2 := 16#0#;
      --  trigger mode These bits define the transfer granularity for its
      --  conditioning by the trigger. If the channel x is enabled
      --  (LPDMA_CxCR.EN asserted) with TRIGPOL[1:0] = 0b00 or 0b11, these
      --  TRIGM[1:0] bits are ignored. Else, a DMA transfer is conditioned by
      --  at least one trigger hit: The LPDMA monitoring of a trigger for
      --  channel x is started when the channel is enabled/loaded with a new
      --  active trigger configuration: rising or falling edge on a selected
      --  trigger (TRIGPOL[1:0] = 0b01 or respectively
      --  TRIGPOL[1:0] = 0b10). The monitoring of this trigger is kept active
      --  during the triggered and uncompleted (data or link) transfer; and if
      --  a new trigger is detected then, this hit is internally memorized to
      --  grant the next transfer, as long as the defined rising or falling
      --  edge is not modified, and the TRIGSEL[4:0] is not modified, and the
      --  channel is enabled. Transferring a next LLIn+1 that updates the
      --  LPDMA_CxTR2 with a new value for any of TRIGSEL[4:0] or TRIGPOL[1:0],
      --  resets the monitoring, trashing the memorized hit of the formerly
      --  defined LLIn trigger. After a first new trigger hitn+1 is memorized,
      --  if another second trigger hitn+2 is detected and if the hitn
      --  triggered transfer is still not completed, hitn+2 is lost and not
      --  memorized, and a trigger overrun flag is reported
      --  (LPDMA_CxSR.TOF = 1), an interrupt is generated if enabled
      --  (LPDMA_CxCR.TOIE = 1). The channel is not automatically disabled by
      --  hardware due to a trigger overrun.
      TRIGM          : LPDMA_C2TR2_TRIGM_Field := 16#0#;
      --  trigger event input selection These bits select the trigger event
      --  input of the LPDMA transfer (as per Programmed LPDMA1 trigger), with
      --  an active trigger event if TRIGPOL[1:0] = 00.
      TRIGSEL        : LPDMA_C2TR2_TRIGSEL_Field := 16#0#;
      --  unspecified
      Reserved_21_23 : HAL.UInt3 := 16#0#;
      --  trigger event polarity These bits define the polarity of the selected
      --  trigger event input defined by TRIGSEL[4:0].
      TRIGPOL        : LPDMA_C2TR2_TRIGPOL_Field := 16#0#;
      --  unspecified
      Reserved_26_29 : HAL.UInt4 := 16#0#;
      --  transfer complete event mode These bits define the transfer
      --  granularity for the transfer complete and half transfer complete
      --  events generation. Note: If the initial LLI0 data transfer is
      --  null/void (directly programmed by the internal register file with
      --  LPDMA_CxBR1.BNDT[15:0] = 0), then neither the complete transfer
      --  event nor the half transfer event is generated. Note: If the initial
      --  LLI0 data transfer is null/void (i.e. directly programmed by the
      --  internal register file with LPDMA_CxBR1.BNDT[15:0] =0 ), then the
      --  half transfer event is not generated, and the transfer complete event
      --  is generated when is completed the loading of the LLI1.
      TCEM           : LPDMA_C2TR2_TCEM_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPDMA_C2TR2_Register use record
      REQSEL         at 0 range 0 .. 4;
      Reserved_5_8   at 0 range 5 .. 8;
      SWREQ          at 0 range 9 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      BREQ           at 0 range 11 .. 11;
      Reserved_12_13 at 0 range 12 .. 13;
      TRIGM          at 0 range 14 .. 15;
      TRIGSEL        at 0 range 16 .. 20;
      Reserved_21_23 at 0 range 21 .. 23;
      TRIGPOL        at 0 range 24 .. 25;
      Reserved_26_29 at 0 range 26 .. 29;
      TCEM           at 0 range 30 .. 31;
   end record;

   subtype LPDMA_C2BR1_BNDT_Field is HAL.UInt16;

   --  LPDMA channel 2 block register 1
   type LPDMA_C2BR1_Register is record
      --  block number of data bytes to transfer from the source Block size
      --  transferred from the source. When the channel is enabled, this field
      --  becomes read-only and is decremented, indicating the remaining number
      --  of data items in the current source block to be transferred.
      --  BNDT[15:0] is programmed in number of bytes, maximum source block
      --  size is 64 Kbytes -1. Once the last data transfer is completed
      --  (BNDT[15:0] = 0): - if LPDMA_CxLLR.UB1 = 1, this field is updated
      --  by the LLI in the memory. - if LPDMA_CxLLR.UB1 = 0 and if there is
      --  at least one non null Uxx update bit, this field is internally
      --  restored to the programmed value. - if all LPDMA_CxLLR.Uxx = 0 and
      --  if LPDMA_CxLLR.LA[15:0] = 0, this field is internally restored to
      --  the programmed value (infinite/continuous last LLI). - if
      --  LPDMA_CxLLR = 0, this field is kept as zero following the last LLI
      --  data transfer. Note: A non-null source block size must be a multiple
      --  of the source data width (BNDT[2:0] versus
      --  LPDMA_CxTR1.SDW_LOG2[1:0]). Else a user setting error is reported and
      --  none transfer is issued.
      BNDT           : LPDMA_C2BR1_BNDT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPDMA_C2BR1_Register use record
      BNDT           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype LPDMA_C2LLR_LA_Field is HAL.UInt14;

   --  LPDMA channel 2 linked-list address register
   type LPDMA_C2LLR_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  pointer (16-bit low-significant address) to the next linked-list data
      --  structure If UT1 = UT2 = UB1 = USA = UDA = ULL = 0 and if
      --  LA[15:20] = 0, the current LLI is the last one. The channel
      --  transfer is completed without any update of the linked-list DMA
      --  register file. Else, this field is the pointer to the memory address
      --  offset from which the next linked-list data structure is
      --  automatically fetched from, once the data transfer is completed, in
      --  order to conditionally update the linked-list DMA internal register
      --  file (LPDMA_CxCTR1, LPDMA_CxTR2, LPDMA_CxBR1, LPDMA_CxSAR,
      --  LPDMA_CxDAR and LPDMA_CxLLR). Note: The user must program the pointer
      --  to be 32-bit aligned. The two low-significant bits are write ignored.
      LA             : LPDMA_C2LLR_LA_Field := 16#0#;
      --  Update LPDMA_CxLLR register from memory This bit is used to control
      --  the update of the LPDMA_CxLLR register from the memory during the
      --  link transfer.
      ULL            : Boolean := False;
      --  unspecified
      Reserved_17_26 : HAL.UInt10 := 16#0#;
      --  Update LPDMA_CxDAR register from memory This bit is used to control
      --  the update of the LPDMA_CxDAR register from the memory during the
      --  link transfer.
      UDA            : Boolean := False;
      --  update LPDMA_CxSAR from memory This bit controls the update of the
      --  LPDMA_CxSAR register from the memory during the link transfer.
      USA            : Boolean := False;
      --  Update LPDMA_CxBR1 from memory This bit controls the update of the
      --  LPDMA_CxBR1 register from the memory during the link transfer.
      UB1            : Boolean := False;
      --  Update LPDMA_CxTR2 from memory This bit controls the update of the
      --  LPDMA_CxTR2 register from the memory during the link transfer.
      UT2            : Boolean := False;
      --  Update LPDMA_CxTR1 from memory This bit controls the update of the
      --  LPDMA_CxTR1 register from the memory during the link transfer.
      UT1            : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPDMA_C2LLR_Register use record
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

   subtype LPDMA_C3LBAR_LBA_Field is HAL.UInt16;

   --  LPDMA channel 3 linked-list base address register
   type LPDMA_C3LBAR_Register is record
      --  unspecified
      Reserved_0_15 : HAL.UInt16 := 16#0#;
      --  linked-list base address of LPDMA channel x
      LBA           : LPDMA_C3LBAR_LBA_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPDMA_C3LBAR_Register use record
      Reserved_0_15 at 0 range 0 .. 15;
      LBA           at 0 range 16 .. 31;
   end record;

   --  LPDMA channel 3 flag clear register
   type LPDMA_C3FCR_Register is record
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

   for LPDMA_C3FCR_Register use record
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

   --  LPDMA channel 3 status register
   type LPDMA_C3SR_Register is record
      --  Read-only. idle flag This idle flag is de-asserted by hardware when
      --  the channel is enabled (LPDMA_CxCR.EN = 1) with a valid channel
      --  configuration (no USEF to be immediately reported). This idle flag is
      --  asserted after hard reset or by hardware when the channel is back in
      --  idle state (in suspended or disabled state).
      IDLEF          : Boolean;
      --  unspecified
      Reserved_1_7   : HAL.UInt7;
      --  Read-only. transfer complete flag A transfer complete event is a
      --  block transfer complete or a LLI transfer complete including the
      --  upload of the next LLI if any, or the full linked-list completion,
      --  depending on the transfer complete event mode LPDMA_CxTR2.TCEM[1:0].
      TCF            : Boolean;
      --  Read-only. half transfer flag An half transfer event is an half block
      --  transfer that occurs when half of the bytes of the source block size
      --  (rounded-up integer of LPDMA_CxBR1.BNDT[15:0] / 2) has been
      --  transferred to the destination.
      HTF            : Boolean;
      --  Read-only. data transfer error flag
      DTEF           : Boolean;
      --  Read-only. update link transfer error flag
      ULEF           : Boolean;
      --  Read-only. user setting error flag
      USEF           : Boolean;
      --  Read-only. completed suspension flag
      SUSPF          : Boolean;
      --  Read-only. trigger overrun flag clear
      TOF            : Boolean;
      --  unspecified
      Reserved_15_31 : HAL.UInt17;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPDMA_C3SR_Register use record
      IDLEF          at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      TCF            at 0 range 8 .. 8;
      HTF            at 0 range 9 .. 9;
      DTEF           at 0 range 10 .. 10;
      ULEF           at 0 range 11 .. 11;
      USEF           at 0 range 12 .. 12;
      SUSPF          at 0 range 13 .. 13;
      TOF            at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype LPDMA_C3CR_PRIO_Field is HAL.UInt2;

   --  LPDMA channel 3 control register
   type LPDMA_C3CR_Register is record
      --  enable Writing 1 into the field RESET (bit 1) causes the hardware to
      --  de-assert this bit, whatever is written into this bit 0. Else: this
      --  bit is de-asserted by hardware when there is a transfer error (master
      --  bus error or user setting error) or when there is a channel transfer
      --  complete (channel ready to be configured, e.g. if LSM=1 at the end of
      --  a single execution of the LLI). Else, this bit can be asserted by
      --  software. Writing 0 into this EN bit is ignored.
      EN             : Boolean := False;
      --  Write-only. reset This bit is write only. Writing 0 has no impact.
      --  Writing 1 implies the reset of the following: the channel internal
      --  state, SUSP and EN bits (whatever is written receptively in bit 2 and
      --  bit 0). The reset is effective when the channel is in steady state,
      --  meaning one of the following: - active channel in suspended state
      --  (LPDMA_CxSR.SUSPF = 1 and LPDMA_CxSR.IDLEF = LPDMA_CxCR.EN = 1)
      --  - channel in disabled state (LPDMA_CxSR.IDLEF = 1 and
      --  LPDMA_CxCR.EN = 0). After writing a RESET, to continue using this
      --  channel, the user must explicitly reconfigure the channel including
      --  the hardware-modified configuration registers (LPDMA_CxBR1,
      --  LPDMA_CxSAR and LPDMA_CxDAR) before enabling again the channel (see
      --  the programming sequence in ).
      RESET          : Boolean := False;
      --  suspend Writing 1 into the field RESET (bit 1) causes the hardware to
      --  de-assert this bit, whatever is written into this bit 2. Else:
      --  Software must write 1 in order to suspend an active channel i.e. a
      --  channel with an on-going DMA transfer over its master ports. The
      --  software must write 0 in order to resume a suspended channel,
      --  following the programming sequence detailed in sequence.
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
      --  Link step mode First the block transfer is executed as defined by the
      --  current internal register file until LPDMA_CxBR1.BNDT[15:0 ] =0).
      --  Secondly the next linked-list data structure is conditionally
      --  uploaded from memory as defined by LPDMA_CxLLR. Then channel
      --  execution is completed. Note: This bit must be written when EN=0.
      --  This bit is read-only when EN=1.
      LSM            : Boolean := False;
      --  unspecified
      Reserved_17_21 : HAL.UInt5 := 16#0#;
      --  priority level of the channel x LPDMA transfer versus others Note:
      --  This bit must be written when EN = 0. This bit is read-only when
      --  EN = 1.
      PRIO           : LPDMA_C3CR_PRIO_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPDMA_C3CR_Register use record
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
      Reserved_17_21 at 0 range 17 .. 21;
      PRIO           at 0 range 22 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype LPDMA_C3TR1_SDW_LOG2_Field is HAL.UInt2;
   subtype LPDMA_C3TR1_DDW_LOG2_Field is HAL.UInt2;

   --  LPDMA channel 3 transfer register 1
   type LPDMA_C3TR1_Register is record
      --  binary logarithm of the source data width of a single in bytes Note:
      --  Setting a 8-byte data width causes a user setting error to be
      --  reported and none transfer is issued. a source block size must be a
      --  multiple of the source data width (LPDMA_CxBR1.BNDT[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and no
      --  transfer is issued. A source single transfer must have an aligned
      --  address with its data width (start address LPDMA_CxSAR[2:0] versus
      --  SDW_LOG2[1:0]). Otherwise, a user setting error is reported and none
      --  transfer is issued.
      SDW_LOG2       : LPDMA_C3TR1_SDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  source incrementing single The source address, pointed by
      --  LPDMA_CxSAR, is kept constant after a single transfer or is
      --  incremented by the offset value corresponding to a contiguous data
      --  after a single transfer.
      SINC           : Boolean := False;
      --  unspecified
      Reserved_4_10  : HAL.UInt7 := 16#0#;
      --  padding/alignment mode If DDW_LOG2[1:0]=SDW_LOG2[1:0]: if the data
      --  width of a single destination transfer is equal to the data width of
      --  a single source transfer, this bit is ignored. Else: Case 1: If
      --  destination data width > source data width Case 2: If destination
      --  data width < source data width
      PAM            : Boolean := False;
      --  unspecified
      Reserved_12_14 : HAL.UInt3 := 16#0#;
      --  security attribute of the LPDMA transfer from the source If
      --  LPDMA_SECCFGR.SECx = 1 and the access is secure: This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  LPDMA_SECCFGR.SECx =1 . A secure write is ignored when
      --  LPDMA_SECCFGR.SECx = 0. When LPDMA_SECCFGR.SECx is de-asserted,
      --  this SSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the LPDMA transfer
      --  from the source is non-secure.
      SSEC           : Boolean := False;
      --  binary logarithm of the destination data width of a single in bytes
      --  Note: Setting a 8-byte data width causes a user setting error to be
      --  reported and none transfer is issued. A destination single transfer
      --  must have an aligned address with its data width (start address
      --  LPDMA_CxDAR[2:0] versus DDW_LOG2[1:0]). Otherwise a user setting
      --  error is reported and none transfer is issued.
      DDW_LOG2       : LPDMA_C3TR1_DDW_LOG2_Field := 16#0#;
      --  unspecified
      Reserved_18_18 : HAL.Bit := 16#0#;
      --  destination incrementing single The destination address, pointed by
      --  LPDMA_CxDAR, is kept constant after a single transfer, or is
      --  incremented by the offset value corresponding to a contiguous data
      --  after a single transfer.
      DINC           : Boolean := False;
      --  unspecified
      Reserved_20_30 : HAL.UInt11 := 16#0#;
      --  security attribute of the LPDMA transfer to the destination If
      --  LPDMA_SECCFGR.SECx = 1 and the access is secure: This is a secure
      --  register bit. This bit can only be read by a secure software. This
      --  bit must be written by a secure software when
      --  LPDMA_SECCFGR.SECx = 1. A secure write is ignored when
      --  LPDMA_SECCFGR.SECx = 0. When LPDMA_SECCFGR.SECx is de-asserted,
      --  this DSEC bit is also de-asserted by hardware (on a secure
      --  reconfiguration of the channel as non-secure), and the LPDMA transfer
      --  to the destination is non-secure.
      DSEC           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPDMA_C3TR1_Register use record
      SDW_LOG2       at 0 range 0 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      SINC           at 0 range 3 .. 3;
      Reserved_4_10  at 0 range 4 .. 10;
      PAM            at 0 range 11 .. 11;
      Reserved_12_14 at 0 range 12 .. 14;
      SSEC           at 0 range 15 .. 15;
      DDW_LOG2       at 0 range 16 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      DINC           at 0 range 19 .. 19;
      Reserved_20_30 at 0 range 20 .. 30;
      DSEC           at 0 range 31 .. 31;
   end record;

   subtype LPDMA_C3TR2_REQSEL_Field is HAL.UInt5;
   subtype LPDMA_C3TR2_TRIGM_Field is HAL.UInt2;
   subtype LPDMA_C3TR2_TRIGSEL_Field is HAL.UInt5;
   subtype LPDMA_C3TR2_TRIGPOL_Field is HAL.UInt2;
   subtype LPDMA_C3TR2_TCEM_Field is HAL.UInt2;

   --  LPDMA channel 3 transfer register 2
   type LPDMA_C3TR2_Register is record
      --  DMA hardware request selection These bits are ignored if channel x is
      --  activated (LPDMA_CxCR.EN asserted) with SWREQ = 1 (software request
      --  for a memory-to-memory transfer). Else, the selected hardware request
      --  is internally taken into account as per request. Note: The user must
      --  not assign a same input hardware request (same REQSEL[4:0] value) to
      --  different active DMA channels (LPDMA_CxCR.EN = 1 and
      --  LPDMA_CxTR2.SWREQ = 0 for these channels). DMA is not intended to
      --  hardware support the case of simultaneous enabled channels
      --  incorrectly configured with a same hardware peripheral request
      --  signal, and there is no user setting error reporting.
      REQSEL         : LPDMA_C3TR2_REQSEL_Field := 16#0#;
      --  unspecified
      Reserved_5_8   : HAL.UInt4 := 16#0#;
      --  software request This bit is internally taken into account when
      --  LPDMA_CxCR.EN is asserted.
      SWREQ          : Boolean := False;
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  block hardware request If the channel x is activated (LPDMA_CxCR.EN
      --  asserted) with SWREQ = 1 (software request for a memory-to-memory
      --  transfer), this bit is ignored. Else:
      BREQ           : Boolean := False;
      --  unspecified
      Reserved_12_13 : HAL.UInt2 := 16#0#;
      --  trigger mode These bits define the transfer granularity for its
      --  conditioning by the trigger. If the channel x is enabled
      --  (LPDMA_CxCR.EN asserted) with TRIGPOL[1:0] = 0b00 or 0b11, these
      --  TRIGM[1:0] bits are ignored. Else, a DMA transfer is conditioned by
      --  at least one trigger hit: The LPDMA monitoring of a trigger for
      --  channel x is started when the channel is enabled/loaded with a new
      --  active trigger configuration: rising or falling edge on a selected
      --  trigger (TRIGPOL[1:0] = 0b01 or respectively
      --  TRIGPOL[1:0] = 0b10). The monitoring of this trigger is kept active
      --  during the triggered and uncompleted (data or link) transfer; and if
      --  a new trigger is detected then, this hit is internally memorized to
      --  grant the next transfer, as long as the defined rising or falling
      --  edge is not modified, and the TRIGSEL[4:0] is not modified, and the
      --  channel is enabled. Transferring a next LLIn+1 that updates the
      --  LPDMA_CxTR2 with a new value for any of TRIGSEL[4:0] or TRIGPOL[1:0],
      --  resets the monitoring, trashing the memorized hit of the formerly
      --  defined LLIn trigger. After a first new trigger hitn+1 is memorized,
      --  if another second trigger hitn+2 is detected and if the hitn
      --  triggered transfer is still not completed, hitn+2 is lost and not
      --  memorized, and a trigger overrun flag is reported
      --  (LPDMA_CxSR.TOF = 1), an interrupt is generated if enabled
      --  (LPDMA_CxCR.TOIE = 1). The channel is not automatically disabled by
      --  hardware due to a trigger overrun.
      TRIGM          : LPDMA_C3TR2_TRIGM_Field := 16#0#;
      --  trigger event input selection These bits select the trigger event
      --  input of the LPDMA transfer (as per Programmed LPDMA1 trigger), with
      --  an active trigger event if TRIGPOL[1:0] = 00.
      TRIGSEL        : LPDMA_C3TR2_TRIGSEL_Field := 16#0#;
      --  unspecified
      Reserved_21_23 : HAL.UInt3 := 16#0#;
      --  trigger event polarity These bits define the polarity of the selected
      --  trigger event input defined by TRIGSEL[4:0].
      TRIGPOL        : LPDMA_C3TR2_TRIGPOL_Field := 16#0#;
      --  unspecified
      Reserved_26_29 : HAL.UInt4 := 16#0#;
      --  transfer complete event mode These bits define the transfer
      --  granularity for the transfer complete and half transfer complete
      --  events generation. Note: If the initial LLI0 data transfer is
      --  null/void (directly programmed by the internal register file with
      --  LPDMA_CxBR1.BNDT[15:0] = 0), then neither the complete transfer
      --  event nor the half transfer event is generated. Note: If the initial
      --  LLI0 data transfer is null/void (i.e. directly programmed by the
      --  internal register file with LPDMA_CxBR1.BNDT[15:0] =0 ), then the
      --  half transfer event is not generated, and the transfer complete event
      --  is generated when is completed the loading of the LLI1.
      TCEM           : LPDMA_C3TR2_TCEM_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPDMA_C3TR2_Register use record
      REQSEL         at 0 range 0 .. 4;
      Reserved_5_8   at 0 range 5 .. 8;
      SWREQ          at 0 range 9 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      BREQ           at 0 range 11 .. 11;
      Reserved_12_13 at 0 range 12 .. 13;
      TRIGM          at 0 range 14 .. 15;
      TRIGSEL        at 0 range 16 .. 20;
      Reserved_21_23 at 0 range 21 .. 23;
      TRIGPOL        at 0 range 24 .. 25;
      Reserved_26_29 at 0 range 26 .. 29;
      TCEM           at 0 range 30 .. 31;
   end record;

   subtype LPDMA_C3BR1_BNDT_Field is HAL.UInt16;

   --  LPDMA channel 3 block register 1
   type LPDMA_C3BR1_Register is record
      --  block number of data bytes to transfer from the source Block size
      --  transferred from the source. When the channel is enabled, this field
      --  becomes read-only and is decremented, indicating the remaining number
      --  of data items in the current source block to be transferred.
      --  BNDT[15:0] is programmed in number of bytes, maximum source block
      --  size is 64 Kbytes -1. Once the last data transfer is completed
      --  (BNDT[15:0] = 0): - if LPDMA_CxLLR.UB1 = 1, this field is updated
      --  by the LLI in the memory. - if LPDMA_CxLLR.UB1 = 0 and if there is
      --  at least one non null Uxx update bit, this field is internally
      --  restored to the programmed value. - if all LPDMA_CxLLR.Uxx = 0 and
      --  if LPDMA_CxLLR.LA[15:0] = 0, this field is internally restored to
      --  the programmed value (infinite/continuous last LLI). - if
      --  LPDMA_CxLLR = 0, this field is kept as zero following the last LLI
      --  data transfer. Note: A non-null source block size must be a multiple
      --  of the source data width (BNDT[2:0] versus
      --  LPDMA_CxTR1.SDW_LOG2[1:0]). Else a user setting error is reported and
      --  none transfer is issued.
      BNDT           : LPDMA_C3BR1_BNDT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPDMA_C3BR1_Register use record
      BNDT           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype LPDMA_C3LLR_LA_Field is HAL.UInt14;

   --  LPDMA channel 3 linked-list address register
   type LPDMA_C3LLR_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  pointer (16-bit low-significant address) to the next linked-list data
      --  structure If UT1 = UT2 = UB1 = USA = UDA = ULL = 0 and if
      --  LA[15:20] = 0, the current LLI is the last one. The channel
      --  transfer is completed without any update of the linked-list DMA
      --  register file. Else, this field is the pointer to the memory address
      --  offset from which the next linked-list data structure is
      --  automatically fetched from, once the data transfer is completed, in
      --  order to conditionally update the linked-list DMA internal register
      --  file (LPDMA_CxCTR1, LPDMA_CxTR2, LPDMA_CxBR1, LPDMA_CxSAR,
      --  LPDMA_CxDAR and LPDMA_CxLLR). Note: The user must program the pointer
      --  to be 32-bit aligned. The two low-significant bits are write ignored.
      LA             : LPDMA_C3LLR_LA_Field := 16#0#;
      --  Update LPDMA_CxLLR register from memory This bit is used to control
      --  the update of the LPDMA_CxLLR register from the memory during the
      --  link transfer.
      ULL            : Boolean := False;
      --  unspecified
      Reserved_17_26 : HAL.UInt10 := 16#0#;
      --  Update LPDMA_CxDAR register from memory This bit is used to control
      --  the update of the LPDMA_CxDAR register from the memory during the
      --  link transfer.
      UDA            : Boolean := False;
      --  update LPDMA_CxSAR from memory This bit controls the update of the
      --  LPDMA_CxSAR register from the memory during the link transfer.
      USA            : Boolean := False;
      --  Update LPDMA_CxBR1 from memory This bit controls the update of the
      --  LPDMA_CxBR1 register from the memory during the link transfer.
      UB1            : Boolean := False;
      --  Update LPDMA_CxTR2 from memory This bit controls the update of the
      --  LPDMA_CxTR2 register from the memory during the link transfer.
      UT2            : Boolean := False;
      --  Update LPDMA_CxTR1 from memory This bit controls the update of the
      --  LPDMA_CxTR1 register from the memory during the link transfer.
      UT1            : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPDMA_C3LLR_Register use record
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

   -----------------
   -- Peripherals --
   -----------------

   --  LPDMA1
   type LPDMA_Peripheral is record
      --  LPDMA secure configuration register
      LPDMA_SECCFGR   : aliased LPDMA_SECCFGR_Register;
      --  LPDMA privileged configuration register
      LPDMA_PRIVCFGR  : aliased LPDMA_PRIVCFGR_Register;
      --  LPDMA configuration lock register
      LPDMA_RCFGLOCKR : aliased LPDMA_RCFGLOCKR_Register;
      --  LPDMA non-secure masked interrupt status register
      LPDMA_MISR      : aliased LPDMA_MISR_Register;
      --  LPDMA secure masked interrupt status register
      LPDMA_SMISR     : aliased LPDMA_SMISR_Register;
      --  LPDMA channel 0 linked-list base address register
      LPDMA_C0LBAR    : aliased LPDMA_C0LBAR_Register;
      --  LPDMA channel 0 flag clear register
      LPDMA_C0FCR     : aliased LPDMA_C0FCR_Register;
      --  LPDMA channel 0 status register
      LPDMA_C0SR      : aliased LPDMA_C0SR_Register;
      --  LPDMA channel 0 control register
      LPDMA_C0CR      : aliased LPDMA_C0CR_Register;
      --  LPDMA channel 0 transfer register 1
      LPDMA_C0TR1     : aliased LPDMA_C0TR1_Register;
      --  LPDMA channel 0 transfer register 2
      LPDMA_C0TR2     : aliased LPDMA_C0TR2_Register;
      --  LPDMA channel 0 block register 1
      LPDMA_C0BR1     : aliased LPDMA_C0BR1_Register;
      --  LPDMA channel 0 source address register
      LPDMA_C0SAR     : aliased HAL.UInt32;
      --  LPDMA channel 0 destination address register
      LPDMA_C0DAR     : aliased HAL.UInt32;
      --  LPDMA channel 0 linked-list address register
      LPDMA_C0LLR     : aliased LPDMA_C0LLR_Register;
      --  LPDMA channel 1 linked-list base address register
      LPDMA_C1LBAR    : aliased LPDMA_C1LBAR_Register;
      --  LPDMA channel 1 flag clear register
      LPDMA_C1FCR     : aliased LPDMA_C1FCR_Register;
      --  LPDMA channel 1 status register
      LPDMA_C1SR      : aliased LPDMA_C1SR_Register;
      --  LPDMA channel 1 control register
      LPDMA_C1CR      : aliased LPDMA_C1CR_Register;
      --  LPDMA channel 1 transfer register 1
      LPDMA_C1TR1     : aliased LPDMA_C1TR1_Register;
      --  LPDMA channel 1 transfer register 2
      LPDMA_C1TR2     : aliased LPDMA_C1TR2_Register;
      --  LPDMA channel 1 block register 1
      LPDMA_C1BR1     : aliased LPDMA_C1BR1_Register;
      --  LPDMA channel 1 source address register
      LPDMA_C1SAR     : aliased HAL.UInt32;
      --  LPDMA channel 1 destination address register
      LPDMA_C1DAR     : aliased HAL.UInt32;
      --  LPDMA channel 1 linked-list address register
      LPDMA_C1LLR     : aliased LPDMA_C1LLR_Register;
      --  LPDMA channel 2 linked-list base address register
      LPDMA_C2LBAR    : aliased LPDMA_C2LBAR_Register;
      --  LPDMA channel 2 flag clear register
      LPDMA_C2FCR     : aliased LPDMA_C2FCR_Register;
      --  LPDMA channel 2 status register
      LPDMA_C2SR      : aliased LPDMA_C2SR_Register;
      --  LPDMA channel 2 control register
      LPDMA_C2CR      : aliased LPDMA_C2CR_Register;
      --  LPDMA channel 2 transfer register 1
      LPDMA_C2TR1     : aliased LPDMA_C2TR1_Register;
      --  LPDMA channel 2 transfer register 2
      LPDMA_C2TR2     : aliased LPDMA_C2TR2_Register;
      --  LPDMA channel 2 block register 1
      LPDMA_C2BR1     : aliased LPDMA_C2BR1_Register;
      --  LPDMA channel 2 source address register
      LPDMA_C2SAR     : aliased HAL.UInt32;
      --  LPDMA channel 2 destination address register
      LPDMA_C2DAR     : aliased HAL.UInt32;
      --  LPDMA channel 2 linked-list address register
      LPDMA_C2LLR     : aliased LPDMA_C2LLR_Register;
      --  LPDMA channel 3 linked-list base address register
      LPDMA_C3LBAR    : aliased LPDMA_C3LBAR_Register;
      --  LPDMA channel 3 flag clear register
      LPDMA_C3FCR     : aliased LPDMA_C3FCR_Register;
      --  LPDMA channel 3 status register
      LPDMA_C3SR      : aliased LPDMA_C3SR_Register;
      --  LPDMA channel 3 control register
      LPDMA_C3CR      : aliased LPDMA_C3CR_Register;
      --  LPDMA channel 3 transfer register 1
      LPDMA_C3TR1     : aliased LPDMA_C3TR1_Register;
      --  LPDMA channel 3 transfer register 2
      LPDMA_C3TR2     : aliased LPDMA_C3TR2_Register;
      --  LPDMA channel 3 block register 1
      LPDMA_C3BR1     : aliased LPDMA_C3BR1_Register;
      --  LPDMA channel 3 source address register
      LPDMA_C3SAR     : aliased HAL.UInt32;
      --  LPDMA channel 3 destination address register
      LPDMA_C3DAR     : aliased HAL.UInt32;
      --  LPDMA channel 3 linked-list address register
      LPDMA_C3LLR     : aliased LPDMA_C3LLR_Register;
   end record
     with Volatile;

   for LPDMA_Peripheral use record
      LPDMA_SECCFGR   at 16#0# range 0 .. 31;
      LPDMA_PRIVCFGR  at 16#4# range 0 .. 31;
      LPDMA_RCFGLOCKR at 16#8# range 0 .. 31;
      LPDMA_MISR      at 16#C# range 0 .. 31;
      LPDMA_SMISR     at 16#10# range 0 .. 31;
      LPDMA_C0LBAR    at 16#50# range 0 .. 31;
      LPDMA_C0FCR     at 16#5C# range 0 .. 31;
      LPDMA_C0SR      at 16#60# range 0 .. 31;
      LPDMA_C0CR      at 16#64# range 0 .. 31;
      LPDMA_C0TR1     at 16#90# range 0 .. 31;
      LPDMA_C0TR2     at 16#94# range 0 .. 31;
      LPDMA_C0BR1     at 16#98# range 0 .. 31;
      LPDMA_C0SAR     at 16#9C# range 0 .. 31;
      LPDMA_C0DAR     at 16#A0# range 0 .. 31;
      LPDMA_C0LLR     at 16#CC# range 0 .. 31;
      LPDMA_C1LBAR    at 16#D0# range 0 .. 31;
      LPDMA_C1FCR     at 16#DC# range 0 .. 31;
      LPDMA_C1SR      at 16#E0# range 0 .. 31;
      LPDMA_C1CR      at 16#E4# range 0 .. 31;
      LPDMA_C1TR1     at 16#110# range 0 .. 31;
      LPDMA_C1TR2     at 16#114# range 0 .. 31;
      LPDMA_C1BR1     at 16#118# range 0 .. 31;
      LPDMA_C1SAR     at 16#11C# range 0 .. 31;
      LPDMA_C1DAR     at 16#120# range 0 .. 31;
      LPDMA_C1LLR     at 16#14C# range 0 .. 31;
      LPDMA_C2LBAR    at 16#150# range 0 .. 31;
      LPDMA_C2FCR     at 16#15C# range 0 .. 31;
      LPDMA_C2SR      at 16#160# range 0 .. 31;
      LPDMA_C2CR      at 16#164# range 0 .. 31;
      LPDMA_C2TR1     at 16#190# range 0 .. 31;
      LPDMA_C2TR2     at 16#194# range 0 .. 31;
      LPDMA_C2BR1     at 16#198# range 0 .. 31;
      LPDMA_C2SAR     at 16#19C# range 0 .. 31;
      LPDMA_C2DAR     at 16#1A0# range 0 .. 31;
      LPDMA_C2LLR     at 16#1CC# range 0 .. 31;
      LPDMA_C3LBAR    at 16#1D0# range 0 .. 31;
      LPDMA_C3FCR     at 16#1DC# range 0 .. 31;
      LPDMA_C3SR      at 16#1E0# range 0 .. 31;
      LPDMA_C3CR      at 16#1E4# range 0 .. 31;
      LPDMA_C3TR1     at 16#210# range 0 .. 31;
      LPDMA_C3TR2     at 16#214# range 0 .. 31;
      LPDMA_C3BR1     at 16#218# range 0 .. 31;
      LPDMA_C3SAR     at 16#21C# range 0 .. 31;
      LPDMA_C3DAR     at 16#220# range 0 .. 31;
      LPDMA_C3LLR     at 16#24C# range 0 .. 31;
   end record;

   --  LPDMA1
   LPDMA1_Periph : aliased LPDMA_Peripheral
     with Import, Address => LPDMA1_Base;

   --  LPDMA1
   SEC_LPDMA1_Periph : aliased LPDMA_Peripheral
     with Import, Address => SEC_LPDMA1_Base;

end STM32_SVD.LPDMA;
