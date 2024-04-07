--  This spec has been automatically generated from STM32U073.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.USB is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype USB_CHEP0R_EA_Field is HAL.UInt4;
   subtype USB_CHEP0R_STATTX_Field is HAL.UInt2;
   subtype USB_CHEP0R_UTYPE_Field is HAL.UInt2;
   subtype USB_CHEP0R_STATRX_Field is HAL.UInt2;
   subtype USB_CHEP0R_DEVADDR_Field is HAL.UInt7;
   subtype USB_CHEP0R_THREE_ERR_TX_Field is HAL.UInt2;
   subtype USB_CHEP0R_THREE_ERR_RX_Field is HAL.UInt2;

   --  USB endpoint/channel 0 register
   type USB_CHEP0R_Register is record
      --  endpoint/channel address Device mode Software must write in this
      --  field the 4-bit address used to identify the transactions directed to
      --  this endpoint. A value must be written before enabling the
      --  corresponding endpoint. Host mode Software must write in this field
      --  the 4-bit address used to identify the channel addressed by the host
      --  transaction.
      EA             : USB_CHEP0R_EA_Field := 16#0#;
      --  Write-only. Status bits, for transmission transfers
      STATTX         : USB_CHEP0R_STATTX_Field := 16#0#;
      --  Write-only. Data toggle, for transmission transfers If the
      --  endpoint/channel is non-isochronous, this bit contains the required
      --  value of the data toggle bit (01=1DATA0, 11=1DATA1) for the next data
      --  packet to be transmitted. Hardware toggles this bit when the ACK
      --  handshake is received from the USB host, following a data packet
      --  transmission. If the endpoint/channel is defined as a control one,
      --  hardware sets this bit to 1 at the reception of a SETUP PID addressed
      --  to this endpoint (in device mode) or when a SETUP transaction is
      --  acknowledged by the device (in host mode). If the endpoint/channel is
      --  using the double buffer feature, this bit is used to support packet
      --  buffer swapping too (Refer to Section134.5.3: Double-buffered
      --  endpoints and usage in Device mode). If the endpoint/channel is
      --  isochronous, this bit is used to support packet buffer swapping since
      --  no data toggling is used for this sort of endpoints and only DATA0
      --  packet are transmitted (refer to Section134.5.5: Isochronous
      --  transfers in Device mode). Hardware toggles this bit just after the
      --  end of data packet transmission, since no handshake is used for
      --  isochronous transfers. This bit can also be toggled by the software
      --  to initialize its value (mandatory when the endpoint/channel is not a
      --  control one) or to force a specific data toggle/packet buffer usage.
      --  When the application software writes 0, the value of DTOGTX remains
      --  unchanged, while writing 1 makes the bit value to toggle. This bit is
      --  read/write but it can only be toggled by writing 1.
      DTOGTX         : Boolean := False;
      --  Valid USB transaction transmitted Device mode This bit is set by the
      --  hardware when an IN transaction is successfully completed on this
      --  endpoint; the software can only clear this bit. If the CTRM bit in
      --  the USB_CNTR register is set accordingly, a generic interrupt
      --  condition is generated together with the endpoint related interrupt
      --  condition, which is always activated. A transaction ended with a NAK
      --  or STALL handshake does not set this bit, since no data is actually
      --  transferred, as in the case of protocol errors or data toggle
      --  mismatches. This bit is read/write but only 0 can be written. Host
      --  mode Same as VTRX behavior but for USB OUT and SETUP transactions.
      VTTX           : Boolean := False;
      --  endpoint/channel kind The meaning of this bit depends on the
      --  endpoint/channel type configured by the UTYPE bits. Table1217
      --  summarizes the different meanings. DBL_BUF: This bit is set by the
      --  software to enable the double-buffering feature for this bulk
      --  endpoint. The usage of double-buffered bulk endpoints is explained in
      --  Section134.5.3: Double-buffered endpoints and usage in Device mode.
      --  STATUS_OUT: This bit is set by the software to indicate that a status
      --  out transaction is expected: in this case all OUT transactions
      --  containing more than zero data bytes are answered STALL instead of
      --  ACK. This bit may be used to improve the robustness of the
      --  application to protocol errors during control transfers and its usage
      --  is intended for control endpoints only. When STATUS_OUT is reset, OUT
      --  transactions can have any number of bytes, as required.
      EPKIND         : Boolean := False;
      --  USB type of transaction These bits configure the behavior of this
      --  endpoint/channel as described in Table1216: Endpoint/channel type
      --  encoding. Channel0/Endpoint0 must always be a control
      --  endpoint/channel and each USB function must have at least one control
      --  endpoint/channel which has address 0, but there may be other control
      --  channels/endpoints if required. Only control channels/endpoints
      --  handle SETUP transactions, which are ignored by endpoints of other
      --  kinds. SETUP transactions cannot be answered with NAK or STALL. If a
      --  control endpoint/channel is defined as NAK, the USB peripheral does
      --  not answer, simulating a receive error, in the receive direction when
      --  a SETUP transaction is received. If the control endpoint/channel is
      --  defined as STALL in the receive direction, then the SETUP packet is
      --  accepted anyway, transferring data and issuing the CTR interrupt. The
      --  reception of OUT transactions is handled in the normal way, even if
      --  the endpoint/channel is a control one. Bulk and interrupt endpoints
      --  have very similar behavior and they differ only in the special
      --  feature available using the EPKIND configuration bit. The usage of
      --  isochronous channels/endpoints is explained in Section134.5.5:
      --  Isochronous transfers in Device mode
      UTYPE          : USB_CHEP0R_UTYPE_Field := 16#0#;
      --  Read-only. Setup transaction completed Device mode This bit is
      --  read-only and it is set by the hardware when the last completed
      --  transaction is a SETUP. This bit changes its value only for control
      --  endpoints. It must be examined, in the case of a successful receive
      --  transaction (VTRX event), to determine the type of transaction
      --  occurred. To protect the interrupt service routine from the changes
      --  in SETUP bits due to next incoming tokens, this bit is kept frozen
      --  while VTRX bit is at 1; its state changes when VTRX is at 0. This bit
      --  is read-only. Host mode This bit is set by the software to send a
      --  SETUP transaction on a control endpoint. This bit changes its value
      --  only for control endpoints. It is cleared by hardware when the SETUP
      --  transaction is acknowledged and VTTX interrupt generated.
      SETUP          : Boolean := False;
      --  Write-only. Status bits, for reception transfers Device mode These
      --  bits contain information about the endpoint status, which are listed
      --  in Table1215: Reception status encoding on page11025. These bits can
      --  be toggled by software to initialize their value. When the
      --  application software writes 0, the value remains unchanged, while
      --  writing 1 makes the bit value to toggle. Hardware sets the STATRX
      --  bits to NAK when a correct transfer has occurred (VTRX1=11)
      --  corresponding to a OUT or SETUP (control only) transaction addressed
      --  to this endpoint, so the software has the time to elaborate the
      --  received data before it acknowledges a new transaction.
      --  Double-buffered bulk endpoints implement a special transaction flow
      --  control, which control the status based upon buffer availability
      --  condition (Refer to Section134.5.3: Double-buffered endpoints and
      --  usage in Device mode). If the endpoint is defined as isochronous, its
      --  status can be only VALID or DISABLED, so that the hardware cannot
      --  change the status of the endpoint after a successful transaction. If
      --  the software sets the STATRX bits to STALL or NAK for an isochronous
      --  endpoint, the USB peripheral behavior is not defined. These bits are
      --  read/write but they can be only toggled by writing 1. Host mode These
      --  bits are the host application controls to start, retry, or abort host
      --  transactions driven by the channel. These bits also contain
      --  information about the device answer to the last IN channel
      --  transaction and report the current status of the channel according to
      --  the following STATRX table of states: -	DISABLE DISABLE value is
      --  reported in case of ACK acknowledge is received on a single-buffer
      --  channel. When in DISABLE state the channel is unused or not active
      --  waiting for application to restart it by writing VALID. Application
      --  can reset a VALID channel to DISABLE to abort a transaction. In this
      --  case the transaction is immediately removed from the host execution
      --  list. If the aborted transaction was already under execution it is
      --  regularly terminated on the USB but the relative VTRX interrupt is
      --  not generated. -	VALID A host channel is actively trying to submit
      --  USB transaction to device only when in VALID state.VALID state can be
      --  set by software or automatically by hardware on a NAKED channel at
      --  the start of a new frame. When set to VALID, an host channel enters
      --  the host execution queue and waits permission from the host frame
      --  scheduler to submit its configured transaction. VALID value is also
      --  reported in case of ACK acknowledge is received on a double-buffered
      --  channel. In this case the channel remains active on the alternate
      --  buffer while application needs to read the current buffer and toggle
      --  DTOGTX. In case software is late in reading and the alternate buffer
      --  is not ready, the host channel is automatically suspended
      --  transparently to the application. The suspended double buffered
      --  channel is re-activated as soon as delay is recovered and DTOGTX is
      --  toggled. - NAK NAK value is reported in case of NAK acknowledge
      --  received. When in NAK state the channel is suspended and does not try
      --  to transmit. NAK state is moved to VALID by hardware at the start of
      --  the next frame, or software can change it to immediately retry
      --  transmission by writing it to VALID, or can disable it and abort the
      --  transaction by writing DISABLE - STALL STALL value is reported in
      --  case of STALL acknowledge received. When in STALL state the channel
      --  behaves as disabled. Application must not retry transmission but
      --  reset the USB and re-enumerate.
      STATRX         : USB_CHEP0R_STATRX_Field := 16#0#;
      --  Write-only. Data Toggle, for reception transfers If the
      --  endpoint/channel is not isochronous, this bit contains the expected
      --  value of the data toggle bit (01=1DATA0, 11=1DATA1) for the next data
      --  packet to be received. Hardware toggles this bit, when the ACK
      --  handshake is sent following a data packet reception having a matching
      --  data PID value; if the endpoint is defined as a control one, hardware
      --  clears this bit at the reception of a SETUP PID received from host
      --  (in device mode), while it sets this bit to 1 when SETUP transaction
      --  is acknowledged by device (in host mode). If the endpoint/channel is
      --  using the double-buffering feature this bit is used to support packet
      --  buffer swapping too (Refer to Section134.5.3: Double-buffered
      --  endpoints and usage in Device mode). If the endpoint/channel is
      --  isochronous, this bit is used only to support packet buffer swapping
      --  for data transmission since no data toggling is used for this kind of
      --  channels/endpoints and only DATA0 packet are transmitted (Refer to
      --  Section134.5.5: Isochronous transfers in Device mode). Hardware
      --  toggles this bit just after the end of data packet reception, since
      --  no handshake is used for isochronous transfers. This bit can also be
      --  toggled by the software to initialize its value (mandatory when the
      --  endpoint is not a control one) or to force specific data
      --  toggle/packet buffer usage. When the application software writes 0,
      --  the value of DTOGRX remains unchanged, while writing 1 makes the bit
      --  value toggle. This bit is read/write but it can be only toggled by
      --  writing 1.
      DTOGRX         : Boolean := False;
      --  USB valid transaction received Device mode This bit is set by the
      --  hardware when an OUT/SETUP transaction is successfully completed on
      --  this endpoint; the software can only clear this bit. If the CTRM bit
      --  in USB_CNTR register is set accordingly, a generic interrupt
      --  condition is generated together with the endpoint related interrupt
      --  condition, which is always activated. The type of occurred
      --  transaction, OUT or SETUP, can be determined from the SETUP bit
      --  described below. A transaction ended with a NAK or STALL handshake
      --  does not set this bit, since no data is actually transferred, as in
      --  the case of protocol errors or data toggle mismatches. This bit is
      --  read/write but only 0 can be written, writing 1 has no effect. Host
      --  mode This bit is set by the hardware when an IN transaction is
      --  successfully completed on this channel. The software can only clear
      --  this bit. If the CTRM bit in USB_CNTR register is set a generic
      --  interrupt condition is generated together with the channel related
      --  flag, which is always activated. - A transaction ended with a NAK
      --  sets this bit and NAK answer is reported to application reading the
      --  NAK state from the STATRX field of this register. One NAKed
      --  transaction keeps pending and is automatically retried by the host at
      --  the next frame, or the host can immediately retry by resetting STATRX
      --  state to VALID. - A transaction ended by STALL handshake sets this
      --  bit and the STALL answer is reported to application reading the STALL
      --  state from the STATRX field of this register. Host application must
      --  consequently disable the channel and re-enumerate. - A transaction
      --  ended with ACK handshake sets this bit If double buffering is
      --  disabled, ACK answer is reported by application reading the DISABLE
      --  state from the STATRX field of this register. Host application must
      --  read received data from USBRAM and re-arm the channel by writing
      --  VALID to the STATRX field of this register. If double buffering is
      --  enabled, ACK answer is reported by application reading VALID state
      --  from the STATRX field of this register. Host application must read
      --  received data from USBRAM and toggle the DTOGTX bit of this register.
      --  - A transaction ended with error sets this bit. Errors can be seen
      --  via the bits ERR_RX (host mode only). This bit is read/write but only
      --  0 can be written, writing 1 has no effect.
      VTRX           : Boolean := False;
      --  Host mode Device address assigned to the endpoint during the
      --  enumeration process.
      DEVADDR        : USB_CHEP0R_DEVADDR_Field := 16#0#;
      --  Host mode This bit is set by the hardware when a device responds with
      --  a NAK. Software can use this bit to monitor the number of NAKs
      --  received from a device.
      NAK            : Boolean := False;
      --  Low speed endpoint
      LS_EP          : Boolean := False;
      --  Received error for an OUT/SETUP transaction Host mode This bit is set
      --  by the hardware when an error (for example no answer by the device,
      --  CRC error, bit stuffing error, framing format violation, etc.) has
      --  occurred during an OUT or SETUP transaction on this channel. The
      --  software can only clear this bit. If the ERRM bit in USB_CNTR
      --  register is set, a generic interrupt condition is generated together
      --  with the channel related flag, which is always activated.
      ERR_TX         : Boolean := False;
      --  Received error for an IN transaction Host mode This bit is set by the
      --  hardware when an error (for example no answer by the device, CRC
      --  error, bit stuffing error, framing format violation, etc.) has
      --  occurred during an IN transaction on this channel. The software can
      --  only clear this bit. If the ERRM bit in USB_CNTR register is set, a
      --  generic interrupt condition is generated together with the channel
      --  related flag, which is always activated.
      ERR_RX         : Boolean := False;
      --  Three errors for an OUT or SETUP transaction Host mode This bit is
      --  set by the hardware when 3 consecutive transaction errors occurred on
      --  the USB bus for an OUT transaction. THREE_ERR_TX is not generated for
      --  isochronous transactions. The software can only clear this bit.
      --  Coding of the received error:
      THREE_ERR_TX   : USB_CHEP0R_THREE_ERR_TX_Field := 16#0#;
      --  Three errors for an IN transaction Host mode This bit is set by the
      --  hardware when 3 consecutive transaction errors occurred on the USB
      --  bus for an IN transaction. THREE_ERR_RX is not generated for
      --  isochronous transactions. The software can only clear this bit.
      --  Coding of the received error:
      THREE_ERR_RX   : USB_CHEP0R_THREE_ERR_RX_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for USB_CHEP0R_Register use record
      EA             at 0 range 0 .. 3;
      STATTX         at 0 range 4 .. 5;
      DTOGTX         at 0 range 6 .. 6;
      VTTX           at 0 range 7 .. 7;
      EPKIND         at 0 range 8 .. 8;
      UTYPE          at 0 range 9 .. 10;
      SETUP          at 0 range 11 .. 11;
      STATRX         at 0 range 12 .. 13;
      DTOGRX         at 0 range 14 .. 14;
      VTRX           at 0 range 15 .. 15;
      DEVADDR        at 0 range 16 .. 22;
      NAK            at 0 range 23 .. 23;
      LS_EP          at 0 range 24 .. 24;
      ERR_TX         at 0 range 25 .. 25;
      ERR_RX         at 0 range 26 .. 26;
      THREE_ERR_TX   at 0 range 27 .. 28;
      THREE_ERR_RX   at 0 range 29 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype USB_CHEP1R_EA_Field is HAL.UInt4;
   subtype USB_CHEP1R_STATTX_Field is HAL.UInt2;
   subtype USB_CHEP1R_UTYPE_Field is HAL.UInt2;
   subtype USB_CHEP1R_STATRX_Field is HAL.UInt2;
   subtype USB_CHEP1R_DEVADDR_Field is HAL.UInt7;
   subtype USB_CHEP1R_THREE_ERR_TX_Field is HAL.UInt2;
   subtype USB_CHEP1R_THREE_ERR_RX_Field is HAL.UInt2;

   --  USB endpoint/channel 1 register
   type USB_CHEP1R_Register is record
      --  endpoint/channel address Device mode Software must write in this
      --  field the 4-bit address used to identify the transactions directed to
      --  this endpoint. A value must be written before enabling the
      --  corresponding endpoint. Host mode Software must write in this field
      --  the 4-bit address used to identify the channel addressed by the host
      --  transaction.
      EA             : USB_CHEP1R_EA_Field := 16#0#;
      --  Write-only. Status bits, for transmission transfers
      STATTX         : USB_CHEP1R_STATTX_Field := 16#0#;
      --  Write-only. Data toggle, for transmission transfers If the
      --  endpoint/channel is non-isochronous, this bit contains the required
      --  value of the data toggle bit (01=1DATA0, 11=1DATA1) for the next data
      --  packet to be transmitted. Hardware toggles this bit when the ACK
      --  handshake is received from the USB host, following a data packet
      --  transmission. If the endpoint/channel is defined as a control one,
      --  hardware sets this bit to 1 at the reception of a SETUP PID addressed
      --  to this endpoint (in device mode) or when a SETUP transaction is
      --  acknowledged by the device (in host mode). If the endpoint/channel is
      --  using the double buffer feature, this bit is used to support packet
      --  buffer swapping too (Refer to Section134.5.3: Double-buffered
      --  endpoints and usage in Device mode). If the endpoint/channel is
      --  isochronous, this bit is used to support packet buffer swapping since
      --  no data toggling is used for this sort of endpoints and only DATA0
      --  packet are transmitted (refer to Section134.5.5: Isochronous
      --  transfers in Device mode). Hardware toggles this bit just after the
      --  end of data packet transmission, since no handshake is used for
      --  isochronous transfers. This bit can also be toggled by the software
      --  to initialize its value (mandatory when the endpoint/channel is not a
      --  control one) or to force a specific data toggle/packet buffer usage.
      --  When the application software writes 0, the value of DTOGTX remains
      --  unchanged, while writing 1 makes the bit value to toggle. This bit is
      --  read/write but it can only be toggled by writing 1.
      DTOGTX         : Boolean := False;
      --  Valid USB transaction transmitted Device mode This bit is set by the
      --  hardware when an IN transaction is successfully completed on this
      --  endpoint; the software can only clear this bit. If the CTRM bit in
      --  the USB_CNTR register is set accordingly, a generic interrupt
      --  condition is generated together with the endpoint related interrupt
      --  condition, which is always activated. A transaction ended with a NAK
      --  or STALL handshake does not set this bit, since no data is actually
      --  transferred, as in the case of protocol errors or data toggle
      --  mismatches. This bit is read/write but only 0 can be written. Host
      --  mode Same as VTRX behavior but for USB OUT and SETUP transactions.
      VTTX           : Boolean := False;
      --  endpoint/channel kind The meaning of this bit depends on the
      --  endpoint/channel type configured by the UTYPE bits. Table1217
      --  summarizes the different meanings. DBL_BUF: This bit is set by the
      --  software to enable the double-buffering feature for this bulk
      --  endpoint. The usage of double-buffered bulk endpoints is explained in
      --  Section134.5.3: Double-buffered endpoints and usage in Device mode.
      --  STATUS_OUT: This bit is set by the software to indicate that a status
      --  out transaction is expected: in this case all OUT transactions
      --  containing more than zero data bytes are answered STALL instead of
      --  ACK. This bit may be used to improve the robustness of the
      --  application to protocol errors during control transfers and its usage
      --  is intended for control endpoints only. When STATUS_OUT is reset, OUT
      --  transactions can have any number of bytes, as required.
      EPKIND         : Boolean := False;
      --  USB type of transaction These bits configure the behavior of this
      --  endpoint/channel as described in Table1216: Endpoint/channel type
      --  encoding. Channel0/Endpoint0 must always be a control
      --  endpoint/channel and each USB function must have at least one control
      --  endpoint/channel which has address 0, but there may be other control
      --  channels/endpoints if required. Only control channels/endpoints
      --  handle SETUP transactions, which are ignored by endpoints of other
      --  kinds. SETUP transactions cannot be answered with NAK or STALL. If a
      --  control endpoint/channel is defined as NAK, the USB peripheral does
      --  not answer, simulating a receive error, in the receive direction when
      --  a SETUP transaction is received. If the control endpoint/channel is
      --  defined as STALL in the receive direction, then the SETUP packet is
      --  accepted anyway, transferring data and issuing the CTR interrupt. The
      --  reception of OUT transactions is handled in the normal way, even if
      --  the endpoint/channel is a control one. Bulk and interrupt endpoints
      --  have very similar behavior and they differ only in the special
      --  feature available using the EPKIND configuration bit. The usage of
      --  isochronous channels/endpoints is explained in Section134.5.5:
      --  Isochronous transfers in Device mode
      UTYPE          : USB_CHEP1R_UTYPE_Field := 16#0#;
      --  Read-only. Setup transaction completed Device mode This bit is
      --  read-only and it is set by the hardware when the last completed
      --  transaction is a SETUP. This bit changes its value only for control
      --  endpoints. It must be examined, in the case of a successful receive
      --  transaction (VTRX event), to determine the type of transaction
      --  occurred. To protect the interrupt service routine from the changes
      --  in SETUP bits due to next incoming tokens, this bit is kept frozen
      --  while VTRX bit is at 1; its state changes when VTRX is at 0. This bit
      --  is read-only. Host mode This bit is set by the software to send a
      --  SETUP transaction on a control endpoint. This bit changes its value
      --  only for control endpoints. It is cleared by hardware when the SETUP
      --  transaction is acknowledged and VTTX interrupt generated.
      SETUP          : Boolean := False;
      --  Write-only. Status bits, for reception transfers Device mode These
      --  bits contain information about the endpoint status, which are listed
      --  in Table1215: Reception status encoding on page11025. These bits can
      --  be toggled by software to initialize their value. When the
      --  application software writes 0, the value remains unchanged, while
      --  writing 1 makes the bit value to toggle. Hardware sets the STATRX
      --  bits to NAK when a correct transfer has occurred (VTRX1=11)
      --  corresponding to a OUT or SETUP (control only) transaction addressed
      --  to this endpoint, so the software has the time to elaborate the
      --  received data before it acknowledges a new transaction.
      --  Double-buffered bulk endpoints implement a special transaction flow
      --  control, which control the status based upon buffer availability
      --  condition (Refer to Section134.5.3: Double-buffered endpoints and
      --  usage in Device mode). If the endpoint is defined as isochronous, its
      --  status can be only VALID or DISABLED, so that the hardware cannot
      --  change the status of the endpoint after a successful transaction. If
      --  the software sets the STATRX bits to STALL or NAK for an isochronous
      --  endpoint, the USB peripheral behavior is not defined. These bits are
      --  read/write but they can be only toggled by writing 1. Host mode These
      --  bits are the host application controls to start, retry, or abort host
      --  transactions driven by the channel. These bits also contain
      --  information about the device answer to the last IN channel
      --  transaction and report the current status of the channel according to
      --  the following STATRX table of states: -	DISABLE DISABLE value is
      --  reported in case of ACK acknowledge is received on a single-buffer
      --  channel. When in DISABLE state the channel is unused or not active
      --  waiting for application to restart it by writing VALID. Application
      --  can reset a VALID channel to DISABLE to abort a transaction. In this
      --  case the transaction is immediately removed from the host execution
      --  list. If the aborted transaction was already under execution it is
      --  regularly terminated on the USB but the relative VTRX interrupt is
      --  not generated. -	VALID A host channel is actively trying to submit
      --  USB transaction to device only when in VALID state.VALID state can be
      --  set by software or automatically by hardware on a NAKED channel at
      --  the start of a new frame. When set to VALID, an host channel enters
      --  the host execution queue and waits permission from the host frame
      --  scheduler to submit its configured transaction. VALID value is also
      --  reported in case of ACK acknowledge is received on a double-buffered
      --  channel. In this case the channel remains active on the alternate
      --  buffer while application needs to read the current buffer and toggle
      --  DTOGTX. In case software is late in reading and the alternate buffer
      --  is not ready, the host channel is automatically suspended
      --  transparently to the application. The suspended double buffered
      --  channel is re-activated as soon as delay is recovered and DTOGTX is
      --  toggled. - NAK NAK value is reported in case of NAK acknowledge
      --  received. When in NAK state the channel is suspended and does not try
      --  to transmit. NAK state is moved to VALID by hardware at the start of
      --  the next frame, or software can change it to immediately retry
      --  transmission by writing it to VALID, or can disable it and abort the
      --  transaction by writing DISABLE - STALL STALL value is reported in
      --  case of STALL acknowledge received. When in STALL state the channel
      --  behaves as disabled. Application must not retry transmission but
      --  reset the USB and re-enumerate.
      STATRX         : USB_CHEP1R_STATRX_Field := 16#0#;
      --  Write-only. Data Toggle, for reception transfers If the
      --  endpoint/channel is not isochronous, this bit contains the expected
      --  value of the data toggle bit (01=1DATA0, 11=1DATA1) for the next data
      --  packet to be received. Hardware toggles this bit, when the ACK
      --  handshake is sent following a data packet reception having a matching
      --  data PID value; if the endpoint is defined as a control one, hardware
      --  clears this bit at the reception of a SETUP PID received from host
      --  (in device mode), while it sets this bit to 1 when SETUP transaction
      --  is acknowledged by device (in host mode). If the endpoint/channel is
      --  using the double-buffering feature this bit is used to support packet
      --  buffer swapping too (Refer to Section134.5.3: Double-buffered
      --  endpoints and usage in Device mode). If the endpoint/channel is
      --  isochronous, this bit is used only to support packet buffer swapping
      --  for data transmission since no data toggling is used for this kind of
      --  channels/endpoints and only DATA0 packet are transmitted (Refer to
      --  Section134.5.5: Isochronous transfers in Device mode). Hardware
      --  toggles this bit just after the end of data packet reception, since
      --  no handshake is used for isochronous transfers. This bit can also be
      --  toggled by the software to initialize its value (mandatory when the
      --  endpoint is not a control one) or to force specific data
      --  toggle/packet buffer usage. When the application software writes 0,
      --  the value of DTOGRX remains unchanged, while writing 1 makes the bit
      --  value toggle. This bit is read/write but it can be only toggled by
      --  writing 1.
      DTOGRX         : Boolean := False;
      --  USB valid transaction received Device mode This bit is set by the
      --  hardware when an OUT/SETUP transaction is successfully completed on
      --  this endpoint; the software can only clear this bit. If the CTRM bit
      --  in USB_CNTR register is set accordingly, a generic interrupt
      --  condition is generated together with the endpoint related interrupt
      --  condition, which is always activated. The type of occurred
      --  transaction, OUT or SETUP, can be determined from the SETUP bit
      --  described below. A transaction ended with a NAK or STALL handshake
      --  does not set this bit, since no data is actually transferred, as in
      --  the case of protocol errors or data toggle mismatches. This bit is
      --  read/write but only 0 can be written, writing 1 has no effect. Host
      --  mode This bit is set by the hardware when an IN transaction is
      --  successfully completed on this channel. The software can only clear
      --  this bit. If the CTRM bit in USB_CNTR register is set a generic
      --  interrupt condition is generated together with the channel related
      --  flag, which is always activated. - A transaction ended with a NAK
      --  sets this bit and NAK answer is reported to application reading the
      --  NAK state from the STATRX field of this register. One NAKed
      --  transaction keeps pending and is automatically retried by the host at
      --  the next frame, or the host can immediately retry by resetting STATRX
      --  state to VALID. - A transaction ended by STALL handshake sets this
      --  bit and the STALL answer is reported to application reading the STALL
      --  state from the STATRX field of this register. Host application must
      --  consequently disable the channel and re-enumerate. - A transaction
      --  ended with ACK handshake sets this bit If double buffering is
      --  disabled, ACK answer is reported by application reading the DISABLE
      --  state from the STATRX field of this register. Host application must
      --  read received data from USBRAM and re-arm the channel by writing
      --  VALID to the STATRX field of this register. If double buffering is
      --  enabled, ACK answer is reported by application reading VALID state
      --  from the STATRX field of this register. Host application must read
      --  received data from USBRAM and toggle the DTOGTX bit of this register.
      --  - A transaction ended with error sets this bit. Errors can be seen
      --  via the bits ERR_RX (host mode only). This bit is read/write but only
      --  0 can be written, writing 1 has no effect.
      VTRX           : Boolean := False;
      --  Host mode Device address assigned to the endpoint during the
      --  enumeration process.
      DEVADDR        : USB_CHEP1R_DEVADDR_Field := 16#0#;
      --  Host mode This bit is set by the hardware when a device responds with
      --  a NAK. Software can use this bit to monitor the number of NAKs
      --  received from a device.
      NAK            : Boolean := False;
      --  Low speed endpoint
      LS_EP          : Boolean := False;
      --  Received error for an OUT/SETUP transaction Host mode This bit is set
      --  by the hardware when an error (for example no answer by the device,
      --  CRC error, bit stuffing error, framing format violation, etc.) has
      --  occurred during an OUT or SETUP transaction on this channel. The
      --  software can only clear this bit. If the ERRM bit in USB_CNTR
      --  register is set, a generic interrupt condition is generated together
      --  with the channel related flag, which is always activated.
      ERR_TX         : Boolean := False;
      --  Received error for an IN transaction Host mode This bit is set by the
      --  hardware when an error (for example no answer by the device, CRC
      --  error, bit stuffing error, framing format violation, etc.) has
      --  occurred during an IN transaction on this channel. The software can
      --  only clear this bit. If the ERRM bit in USB_CNTR register is set, a
      --  generic interrupt condition is generated together with the channel
      --  related flag, which is always activated.
      ERR_RX         : Boolean := False;
      --  Three errors for an OUT or SETUP transaction Host mode This bit is
      --  set by the hardware when 3 consecutive transaction errors occurred on
      --  the USB bus for an OUT transaction. THREE_ERR_TX is not generated for
      --  isochronous transactions. The software can only clear this bit.
      --  Coding of the received error:
      THREE_ERR_TX   : USB_CHEP1R_THREE_ERR_TX_Field := 16#0#;
      --  Three errors for an IN transaction Host mode This bit is set by the
      --  hardware when 3 consecutive transaction errors occurred on the USB
      --  bus for an IN transaction. THREE_ERR_RX is not generated for
      --  isochronous transactions. The software can only clear this bit.
      --  Coding of the received error:
      THREE_ERR_RX   : USB_CHEP1R_THREE_ERR_RX_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for USB_CHEP1R_Register use record
      EA             at 0 range 0 .. 3;
      STATTX         at 0 range 4 .. 5;
      DTOGTX         at 0 range 6 .. 6;
      VTTX           at 0 range 7 .. 7;
      EPKIND         at 0 range 8 .. 8;
      UTYPE          at 0 range 9 .. 10;
      SETUP          at 0 range 11 .. 11;
      STATRX         at 0 range 12 .. 13;
      DTOGRX         at 0 range 14 .. 14;
      VTRX           at 0 range 15 .. 15;
      DEVADDR        at 0 range 16 .. 22;
      NAK            at 0 range 23 .. 23;
      LS_EP          at 0 range 24 .. 24;
      ERR_TX         at 0 range 25 .. 25;
      ERR_RX         at 0 range 26 .. 26;
      THREE_ERR_TX   at 0 range 27 .. 28;
      THREE_ERR_RX   at 0 range 29 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype USB_CHEP2R_EA_Field is HAL.UInt4;
   subtype USB_CHEP2R_STATTX_Field is HAL.UInt2;
   subtype USB_CHEP2R_UTYPE_Field is HAL.UInt2;
   subtype USB_CHEP2R_STATRX_Field is HAL.UInt2;
   subtype USB_CHEP2R_DEVADDR_Field is HAL.UInt7;
   subtype USB_CHEP2R_THREE_ERR_TX_Field is HAL.UInt2;
   subtype USB_CHEP2R_THREE_ERR_RX_Field is HAL.UInt2;

   --  USB endpoint/channel 2 register
   type USB_CHEP2R_Register is record
      --  endpoint/channel address Device mode Software must write in this
      --  field the 4-bit address used to identify the transactions directed to
      --  this endpoint. A value must be written before enabling the
      --  corresponding endpoint. Host mode Software must write in this field
      --  the 4-bit address used to identify the channel addressed by the host
      --  transaction.
      EA             : USB_CHEP2R_EA_Field := 16#0#;
      --  Write-only. Status bits, for transmission transfers
      STATTX         : USB_CHEP2R_STATTX_Field := 16#0#;
      --  Write-only. Data toggle, for transmission transfers If the
      --  endpoint/channel is non-isochronous, this bit contains the required
      --  value of the data toggle bit (01=1DATA0, 11=1DATA1) for the next data
      --  packet to be transmitted. Hardware toggles this bit when the ACK
      --  handshake is received from the USB host, following a data packet
      --  transmission. If the endpoint/channel is defined as a control one,
      --  hardware sets this bit to 1 at the reception of a SETUP PID addressed
      --  to this endpoint (in device mode) or when a SETUP transaction is
      --  acknowledged by the device (in host mode). If the endpoint/channel is
      --  using the double buffer feature, this bit is used to support packet
      --  buffer swapping too (Refer to Section134.5.3: Double-buffered
      --  endpoints and usage in Device mode). If the endpoint/channel is
      --  isochronous, this bit is used to support packet buffer swapping since
      --  no data toggling is used for this sort of endpoints and only DATA0
      --  packet are transmitted (refer to Section134.5.5: Isochronous
      --  transfers in Device mode). Hardware toggles this bit just after the
      --  end of data packet transmission, since no handshake is used for
      --  isochronous transfers. This bit can also be toggled by the software
      --  to initialize its value (mandatory when the endpoint/channel is not a
      --  control one) or to force a specific data toggle/packet buffer usage.
      --  When the application software writes 0, the value of DTOGTX remains
      --  unchanged, while writing 1 makes the bit value to toggle. This bit is
      --  read/write but it can only be toggled by writing 1.
      DTOGTX         : Boolean := False;
      --  Valid USB transaction transmitted Device mode This bit is set by the
      --  hardware when an IN transaction is successfully completed on this
      --  endpoint; the software can only clear this bit. If the CTRM bit in
      --  the USB_CNTR register is set accordingly, a generic interrupt
      --  condition is generated together with the endpoint related interrupt
      --  condition, which is always activated. A transaction ended with a NAK
      --  or STALL handshake does not set this bit, since no data is actually
      --  transferred, as in the case of protocol errors or data toggle
      --  mismatches. This bit is read/write but only 0 can be written. Host
      --  mode Same as VTRX behavior but for USB OUT and SETUP transactions.
      VTTX           : Boolean := False;
      --  endpoint/channel kind The meaning of this bit depends on the
      --  endpoint/channel type configured by the UTYPE bits. Table1217
      --  summarizes the different meanings. DBL_BUF: This bit is set by the
      --  software to enable the double-buffering feature for this bulk
      --  endpoint. The usage of double-buffered bulk endpoints is explained in
      --  Section134.5.3: Double-buffered endpoints and usage in Device mode.
      --  STATUS_OUT: This bit is set by the software to indicate that a status
      --  out transaction is expected: in this case all OUT transactions
      --  containing more than zero data bytes are answered STALL instead of
      --  ACK. This bit may be used to improve the robustness of the
      --  application to protocol errors during control transfers and its usage
      --  is intended for control endpoints only. When STATUS_OUT is reset, OUT
      --  transactions can have any number of bytes, as required.
      EPKIND         : Boolean := False;
      --  USB type of transaction These bits configure the behavior of this
      --  endpoint/channel as described in Table1216: Endpoint/channel type
      --  encoding. Channel0/Endpoint0 must always be a control
      --  endpoint/channel and each USB function must have at least one control
      --  endpoint/channel which has address 0, but there may be other control
      --  channels/endpoints if required. Only control channels/endpoints
      --  handle SETUP transactions, which are ignored by endpoints of other
      --  kinds. SETUP transactions cannot be answered with NAK or STALL. If a
      --  control endpoint/channel is defined as NAK, the USB peripheral does
      --  not answer, simulating a receive error, in the receive direction when
      --  a SETUP transaction is received. If the control endpoint/channel is
      --  defined as STALL in the receive direction, then the SETUP packet is
      --  accepted anyway, transferring data and issuing the CTR interrupt. The
      --  reception of OUT transactions is handled in the normal way, even if
      --  the endpoint/channel is a control one. Bulk and interrupt endpoints
      --  have very similar behavior and they differ only in the special
      --  feature available using the EPKIND configuration bit. The usage of
      --  isochronous channels/endpoints is explained in Section134.5.5:
      --  Isochronous transfers in Device mode
      UTYPE          : USB_CHEP2R_UTYPE_Field := 16#0#;
      --  Read-only. Setup transaction completed Device mode This bit is
      --  read-only and it is set by the hardware when the last completed
      --  transaction is a SETUP. This bit changes its value only for control
      --  endpoints. It must be examined, in the case of a successful receive
      --  transaction (VTRX event), to determine the type of transaction
      --  occurred. To protect the interrupt service routine from the changes
      --  in SETUP bits due to next incoming tokens, this bit is kept frozen
      --  while VTRX bit is at 1; its state changes when VTRX is at 0. This bit
      --  is read-only. Host mode This bit is set by the software to send a
      --  SETUP transaction on a control endpoint. This bit changes its value
      --  only for control endpoints. It is cleared by hardware when the SETUP
      --  transaction is acknowledged and VTTX interrupt generated.
      SETUP          : Boolean := False;
      --  Write-only. Status bits, for reception transfers Device mode These
      --  bits contain information about the endpoint status, which are listed
      --  in Table1215: Reception status encoding on page11025. These bits can
      --  be toggled by software to initialize their value. When the
      --  application software writes 0, the value remains unchanged, while
      --  writing 1 makes the bit value to toggle. Hardware sets the STATRX
      --  bits to NAK when a correct transfer has occurred (VTRX1=11)
      --  corresponding to a OUT or SETUP (control only) transaction addressed
      --  to this endpoint, so the software has the time to elaborate the
      --  received data before it acknowledges a new transaction.
      --  Double-buffered bulk endpoints implement a special transaction flow
      --  control, which control the status based upon buffer availability
      --  condition (Refer to Section134.5.3: Double-buffered endpoints and
      --  usage in Device mode). If the endpoint is defined as isochronous, its
      --  status can be only VALID or DISABLED, so that the hardware cannot
      --  change the status of the endpoint after a successful transaction. If
      --  the software sets the STATRX bits to STALL or NAK for an isochronous
      --  endpoint, the USB peripheral behavior is not defined. These bits are
      --  read/write but they can be only toggled by writing 1. Host mode These
      --  bits are the host application controls to start, retry, or abort host
      --  transactions driven by the channel. These bits also contain
      --  information about the device answer to the last IN channel
      --  transaction and report the current status of the channel according to
      --  the following STATRX table of states: -	DISABLE DISABLE value is
      --  reported in case of ACK acknowledge is received on a single-buffer
      --  channel. When in DISABLE state the channel is unused or not active
      --  waiting for application to restart it by writing VALID. Application
      --  can reset a VALID channel to DISABLE to abort a transaction. In this
      --  case the transaction is immediately removed from the host execution
      --  list. If the aborted transaction was already under execution it is
      --  regularly terminated on the USB but the relative VTRX interrupt is
      --  not generated. -	VALID A host channel is actively trying to submit
      --  USB transaction to device only when in VALID state.VALID state can be
      --  set by software or automatically by hardware on a NAKED channel at
      --  the start of a new frame. When set to VALID, an host channel enters
      --  the host execution queue and waits permission from the host frame
      --  scheduler to submit its configured transaction. VALID value is also
      --  reported in case of ACK acknowledge is received on a double-buffered
      --  channel. In this case the channel remains active on the alternate
      --  buffer while application needs to read the current buffer and toggle
      --  DTOGTX. In case software is late in reading and the alternate buffer
      --  is not ready, the host channel is automatically suspended
      --  transparently to the application. The suspended double buffered
      --  channel is re-activated as soon as delay is recovered and DTOGTX is
      --  toggled. - NAK NAK value is reported in case of NAK acknowledge
      --  received. When in NAK state the channel is suspended and does not try
      --  to transmit. NAK state is moved to VALID by hardware at the start of
      --  the next frame, or software can change it to immediately retry
      --  transmission by writing it to VALID, or can disable it and abort the
      --  transaction by writing DISABLE - STALL STALL value is reported in
      --  case of STALL acknowledge received. When in STALL state the channel
      --  behaves as disabled. Application must not retry transmission but
      --  reset the USB and re-enumerate.
      STATRX         : USB_CHEP2R_STATRX_Field := 16#0#;
      --  Write-only. Data Toggle, for reception transfers If the
      --  endpoint/channel is not isochronous, this bit contains the expected
      --  value of the data toggle bit (01=1DATA0, 11=1DATA1) for the next data
      --  packet to be received. Hardware toggles this bit, when the ACK
      --  handshake is sent following a data packet reception having a matching
      --  data PID value; if the endpoint is defined as a control one, hardware
      --  clears this bit at the reception of a SETUP PID received from host
      --  (in device mode), while it sets this bit to 1 when SETUP transaction
      --  is acknowledged by device (in host mode). If the endpoint/channel is
      --  using the double-buffering feature this bit is used to support packet
      --  buffer swapping too (Refer to Section134.5.3: Double-buffered
      --  endpoints and usage in Device mode). If the endpoint/channel is
      --  isochronous, this bit is used only to support packet buffer swapping
      --  for data transmission since no data toggling is used for this kind of
      --  channels/endpoints and only DATA0 packet are transmitted (Refer to
      --  Section134.5.5: Isochronous transfers in Device mode). Hardware
      --  toggles this bit just after the end of data packet reception, since
      --  no handshake is used for isochronous transfers. This bit can also be
      --  toggled by the software to initialize its value (mandatory when the
      --  endpoint is not a control one) or to force specific data
      --  toggle/packet buffer usage. When the application software writes 0,
      --  the value of DTOGRX remains unchanged, while writing 1 makes the bit
      --  value toggle. This bit is read/write but it can be only toggled by
      --  writing 1.
      DTOGRX         : Boolean := False;
      --  USB valid transaction received Device mode This bit is set by the
      --  hardware when an OUT/SETUP transaction is successfully completed on
      --  this endpoint; the software can only clear this bit. If the CTRM bit
      --  in USB_CNTR register is set accordingly, a generic interrupt
      --  condition is generated together with the endpoint related interrupt
      --  condition, which is always activated. The type of occurred
      --  transaction, OUT or SETUP, can be determined from the SETUP bit
      --  described below. A transaction ended with a NAK or STALL handshake
      --  does not set this bit, since no data is actually transferred, as in
      --  the case of protocol errors or data toggle mismatches. This bit is
      --  read/write but only 0 can be written, writing 1 has no effect. Host
      --  mode This bit is set by the hardware when an IN transaction is
      --  successfully completed on this channel. The software can only clear
      --  this bit. If the CTRM bit in USB_CNTR register is set a generic
      --  interrupt condition is generated together with the channel related
      --  flag, which is always activated. - A transaction ended with a NAK
      --  sets this bit and NAK answer is reported to application reading the
      --  NAK state from the STATRX field of this register. One NAKed
      --  transaction keeps pending and is automatically retried by the host at
      --  the next frame, or the host can immediately retry by resetting STATRX
      --  state to VALID. - A transaction ended by STALL handshake sets this
      --  bit and the STALL answer is reported to application reading the STALL
      --  state from the STATRX field of this register. Host application must
      --  consequently disable the channel and re-enumerate. - A transaction
      --  ended with ACK handshake sets this bit If double buffering is
      --  disabled, ACK answer is reported by application reading the DISABLE
      --  state from the STATRX field of this register. Host application must
      --  read received data from USBRAM and re-arm the channel by writing
      --  VALID to the STATRX field of this register. If double buffering is
      --  enabled, ACK answer is reported by application reading VALID state
      --  from the STATRX field of this register. Host application must read
      --  received data from USBRAM and toggle the DTOGTX bit of this register.
      --  - A transaction ended with error sets this bit. Errors can be seen
      --  via the bits ERR_RX (host mode only). This bit is read/write but only
      --  0 can be written, writing 1 has no effect.
      VTRX           : Boolean := False;
      --  Host mode Device address assigned to the endpoint during the
      --  enumeration process.
      DEVADDR        : USB_CHEP2R_DEVADDR_Field := 16#0#;
      --  Host mode This bit is set by the hardware when a device responds with
      --  a NAK. Software can use this bit to monitor the number of NAKs
      --  received from a device.
      NAK            : Boolean := False;
      --  Low speed endpoint
      LS_EP          : Boolean := False;
      --  Received error for an OUT/SETUP transaction Host mode This bit is set
      --  by the hardware when an error (for example no answer by the device,
      --  CRC error, bit stuffing error, framing format violation, etc.) has
      --  occurred during an OUT or SETUP transaction on this channel. The
      --  software can only clear this bit. If the ERRM bit in USB_CNTR
      --  register is set, a generic interrupt condition is generated together
      --  with the channel related flag, which is always activated.
      ERR_TX         : Boolean := False;
      --  Received error for an IN transaction Host mode This bit is set by the
      --  hardware when an error (for example no answer by the device, CRC
      --  error, bit stuffing error, framing format violation, etc.) has
      --  occurred during an IN transaction on this channel. The software can
      --  only clear this bit. If the ERRM bit in USB_CNTR register is set, a
      --  generic interrupt condition is generated together with the channel
      --  related flag, which is always activated.
      ERR_RX         : Boolean := False;
      --  Three errors for an OUT or SETUP transaction Host mode This bit is
      --  set by the hardware when 3 consecutive transaction errors occurred on
      --  the USB bus for an OUT transaction. THREE_ERR_TX is not generated for
      --  isochronous transactions. The software can only clear this bit.
      --  Coding of the received error:
      THREE_ERR_TX   : USB_CHEP2R_THREE_ERR_TX_Field := 16#0#;
      --  Three errors for an IN transaction Host mode This bit is set by the
      --  hardware when 3 consecutive transaction errors occurred on the USB
      --  bus for an IN transaction. THREE_ERR_RX is not generated for
      --  isochronous transactions. The software can only clear this bit.
      --  Coding of the received error:
      THREE_ERR_RX   : USB_CHEP2R_THREE_ERR_RX_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for USB_CHEP2R_Register use record
      EA             at 0 range 0 .. 3;
      STATTX         at 0 range 4 .. 5;
      DTOGTX         at 0 range 6 .. 6;
      VTTX           at 0 range 7 .. 7;
      EPKIND         at 0 range 8 .. 8;
      UTYPE          at 0 range 9 .. 10;
      SETUP          at 0 range 11 .. 11;
      STATRX         at 0 range 12 .. 13;
      DTOGRX         at 0 range 14 .. 14;
      VTRX           at 0 range 15 .. 15;
      DEVADDR        at 0 range 16 .. 22;
      NAK            at 0 range 23 .. 23;
      LS_EP          at 0 range 24 .. 24;
      ERR_TX         at 0 range 25 .. 25;
      ERR_RX         at 0 range 26 .. 26;
      THREE_ERR_TX   at 0 range 27 .. 28;
      THREE_ERR_RX   at 0 range 29 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype USB_CHEP3R_EA_Field is HAL.UInt4;
   subtype USB_CHEP3R_STATTX_Field is HAL.UInt2;
   subtype USB_CHEP3R_UTYPE_Field is HAL.UInt2;
   subtype USB_CHEP3R_STATRX_Field is HAL.UInt2;
   subtype USB_CHEP3R_DEVADDR_Field is HAL.UInt7;
   subtype USB_CHEP3R_THREE_ERR_TX_Field is HAL.UInt2;
   subtype USB_CHEP3R_THREE_ERR_RX_Field is HAL.UInt2;

   --  USB endpoint/channel 3 register
   type USB_CHEP3R_Register is record
      --  endpoint/channel address Device mode Software must write in this
      --  field the 4-bit address used to identify the transactions directed to
      --  this endpoint. A value must be written before enabling the
      --  corresponding endpoint. Host mode Software must write in this field
      --  the 4-bit address used to identify the channel addressed by the host
      --  transaction.
      EA             : USB_CHEP3R_EA_Field := 16#0#;
      --  Write-only. Status bits, for transmission transfers
      STATTX         : USB_CHEP3R_STATTX_Field := 16#0#;
      --  Write-only. Data toggle, for transmission transfers If the
      --  endpoint/channel is non-isochronous, this bit contains the required
      --  value of the data toggle bit (01=1DATA0, 11=1DATA1) for the next data
      --  packet to be transmitted. Hardware toggles this bit when the ACK
      --  handshake is received from the USB host, following a data packet
      --  transmission. If the endpoint/channel is defined as a control one,
      --  hardware sets this bit to 1 at the reception of a SETUP PID addressed
      --  to this endpoint (in device mode) or when a SETUP transaction is
      --  acknowledged by the device (in host mode). If the endpoint/channel is
      --  using the double buffer feature, this bit is used to support packet
      --  buffer swapping too (Refer to Section134.5.3: Double-buffered
      --  endpoints and usage in Device mode). If the endpoint/channel is
      --  isochronous, this bit is used to support packet buffer swapping since
      --  no data toggling is used for this sort of endpoints and only DATA0
      --  packet are transmitted (refer to Section134.5.5: Isochronous
      --  transfers in Device mode). Hardware toggles this bit just after the
      --  end of data packet transmission, since no handshake is used for
      --  isochronous transfers. This bit can also be toggled by the software
      --  to initialize its value (mandatory when the endpoint/channel is not a
      --  control one) or to force a specific data toggle/packet buffer usage.
      --  When the application software writes 0, the value of DTOGTX remains
      --  unchanged, while writing 1 makes the bit value to toggle. This bit is
      --  read/write but it can only be toggled by writing 1.
      DTOGTX         : Boolean := False;
      --  Valid USB transaction transmitted Device mode This bit is set by the
      --  hardware when an IN transaction is successfully completed on this
      --  endpoint; the software can only clear this bit. If the CTRM bit in
      --  the USB_CNTR register is set accordingly, a generic interrupt
      --  condition is generated together with the endpoint related interrupt
      --  condition, which is always activated. A transaction ended with a NAK
      --  or STALL handshake does not set this bit, since no data is actually
      --  transferred, as in the case of protocol errors or data toggle
      --  mismatches. This bit is read/write but only 0 can be written. Host
      --  mode Same as VTRX behavior but for USB OUT and SETUP transactions.
      VTTX           : Boolean := False;
      --  endpoint/channel kind The meaning of this bit depends on the
      --  endpoint/channel type configured by the UTYPE bits. Table1217
      --  summarizes the different meanings. DBL_BUF: This bit is set by the
      --  software to enable the double-buffering feature for this bulk
      --  endpoint. The usage of double-buffered bulk endpoints is explained in
      --  Section134.5.3: Double-buffered endpoints and usage in Device mode.
      --  STATUS_OUT: This bit is set by the software to indicate that a status
      --  out transaction is expected: in this case all OUT transactions
      --  containing more than zero data bytes are answered STALL instead of
      --  ACK. This bit may be used to improve the robustness of the
      --  application to protocol errors during control transfers and its usage
      --  is intended for control endpoints only. When STATUS_OUT is reset, OUT
      --  transactions can have any number of bytes, as required.
      EPKIND         : Boolean := False;
      --  USB type of transaction These bits configure the behavior of this
      --  endpoint/channel as described in Table1216: Endpoint/channel type
      --  encoding. Channel0/Endpoint0 must always be a control
      --  endpoint/channel and each USB function must have at least one control
      --  endpoint/channel which has address 0, but there may be other control
      --  channels/endpoints if required. Only control channels/endpoints
      --  handle SETUP transactions, which are ignored by endpoints of other
      --  kinds. SETUP transactions cannot be answered with NAK or STALL. If a
      --  control endpoint/channel is defined as NAK, the USB peripheral does
      --  not answer, simulating a receive error, in the receive direction when
      --  a SETUP transaction is received. If the control endpoint/channel is
      --  defined as STALL in the receive direction, then the SETUP packet is
      --  accepted anyway, transferring data and issuing the CTR interrupt. The
      --  reception of OUT transactions is handled in the normal way, even if
      --  the endpoint/channel is a control one. Bulk and interrupt endpoints
      --  have very similar behavior and they differ only in the special
      --  feature available using the EPKIND configuration bit. The usage of
      --  isochronous channels/endpoints is explained in Section134.5.5:
      --  Isochronous transfers in Device mode
      UTYPE          : USB_CHEP3R_UTYPE_Field := 16#0#;
      --  Read-only. Setup transaction completed Device mode This bit is
      --  read-only and it is set by the hardware when the last completed
      --  transaction is a SETUP. This bit changes its value only for control
      --  endpoints. It must be examined, in the case of a successful receive
      --  transaction (VTRX event), to determine the type of transaction
      --  occurred. To protect the interrupt service routine from the changes
      --  in SETUP bits due to next incoming tokens, this bit is kept frozen
      --  while VTRX bit is at 1; its state changes when VTRX is at 0. This bit
      --  is read-only. Host mode This bit is set by the software to send a
      --  SETUP transaction on a control endpoint. This bit changes its value
      --  only for control endpoints. It is cleared by hardware when the SETUP
      --  transaction is acknowledged and VTTX interrupt generated.
      SETUP          : Boolean := False;
      --  Write-only. Status bits, for reception transfers Device mode These
      --  bits contain information about the endpoint status, which are listed
      --  in Table1215: Reception status encoding on page11025. These bits can
      --  be toggled by software to initialize their value. When the
      --  application software writes 0, the value remains unchanged, while
      --  writing 1 makes the bit value to toggle. Hardware sets the STATRX
      --  bits to NAK when a correct transfer has occurred (VTRX1=11)
      --  corresponding to a OUT or SETUP (control only) transaction addressed
      --  to this endpoint, so the software has the time to elaborate the
      --  received data before it acknowledges a new transaction.
      --  Double-buffered bulk endpoints implement a special transaction flow
      --  control, which control the status based upon buffer availability
      --  condition (Refer to Section134.5.3: Double-buffered endpoints and
      --  usage in Device mode). If the endpoint is defined as isochronous, its
      --  status can be only VALID or DISABLED, so that the hardware cannot
      --  change the status of the endpoint after a successful transaction. If
      --  the software sets the STATRX bits to STALL or NAK for an isochronous
      --  endpoint, the USB peripheral behavior is not defined. These bits are
      --  read/write but they can be only toggled by writing 1. Host mode These
      --  bits are the host application controls to start, retry, or abort host
      --  transactions driven by the channel. These bits also contain
      --  information about the device answer to the last IN channel
      --  transaction and report the current status of the channel according to
      --  the following STATRX table of states: -	DISABLE DISABLE value is
      --  reported in case of ACK acknowledge is received on a single-buffer
      --  channel. When in DISABLE state the channel is unused or not active
      --  waiting for application to restart it by writing VALID. Application
      --  can reset a VALID channel to DISABLE to abort a transaction. In this
      --  case the transaction is immediately removed from the host execution
      --  list. If the aborted transaction was already under execution it is
      --  regularly terminated on the USB but the relative VTRX interrupt is
      --  not generated. -	VALID A host channel is actively trying to submit
      --  USB transaction to device only when in VALID state.VALID state can be
      --  set by software or automatically by hardware on a NAKED channel at
      --  the start of a new frame. When set to VALID, an host channel enters
      --  the host execution queue and waits permission from the host frame
      --  scheduler to submit its configured transaction. VALID value is also
      --  reported in case of ACK acknowledge is received on a double-buffered
      --  channel. In this case the channel remains active on the alternate
      --  buffer while application needs to read the current buffer and toggle
      --  DTOGTX. In case software is late in reading and the alternate buffer
      --  is not ready, the host channel is automatically suspended
      --  transparently to the application. The suspended double buffered
      --  channel is re-activated as soon as delay is recovered and DTOGTX is
      --  toggled. - NAK NAK value is reported in case of NAK acknowledge
      --  received. When in NAK state the channel is suspended and does not try
      --  to transmit. NAK state is moved to VALID by hardware at the start of
      --  the next frame, or software can change it to immediately retry
      --  transmission by writing it to VALID, or can disable it and abort the
      --  transaction by writing DISABLE - STALL STALL value is reported in
      --  case of STALL acknowledge received. When in STALL state the channel
      --  behaves as disabled. Application must not retry transmission but
      --  reset the USB and re-enumerate.
      STATRX         : USB_CHEP3R_STATRX_Field := 16#0#;
      --  Write-only. Data Toggle, for reception transfers If the
      --  endpoint/channel is not isochronous, this bit contains the expected
      --  value of the data toggle bit (01=1DATA0, 11=1DATA1) for the next data
      --  packet to be received. Hardware toggles this bit, when the ACK
      --  handshake is sent following a data packet reception having a matching
      --  data PID value; if the endpoint is defined as a control one, hardware
      --  clears this bit at the reception of a SETUP PID received from host
      --  (in device mode), while it sets this bit to 1 when SETUP transaction
      --  is acknowledged by device (in host mode). If the endpoint/channel is
      --  using the double-buffering feature this bit is used to support packet
      --  buffer swapping too (Refer to Section134.5.3: Double-buffered
      --  endpoints and usage in Device mode). If the endpoint/channel is
      --  isochronous, this bit is used only to support packet buffer swapping
      --  for data transmission since no data toggling is used for this kind of
      --  channels/endpoints and only DATA0 packet are transmitted (Refer to
      --  Section134.5.5: Isochronous transfers in Device mode). Hardware
      --  toggles this bit just after the end of data packet reception, since
      --  no handshake is used for isochronous transfers. This bit can also be
      --  toggled by the software to initialize its value (mandatory when the
      --  endpoint is not a control one) or to force specific data
      --  toggle/packet buffer usage. When the application software writes 0,
      --  the value of DTOGRX remains unchanged, while writing 1 makes the bit
      --  value toggle. This bit is read/write but it can be only toggled by
      --  writing 1.
      DTOGRX         : Boolean := False;
      --  USB valid transaction received Device mode This bit is set by the
      --  hardware when an OUT/SETUP transaction is successfully completed on
      --  this endpoint; the software can only clear this bit. If the CTRM bit
      --  in USB_CNTR register is set accordingly, a generic interrupt
      --  condition is generated together with the endpoint related interrupt
      --  condition, which is always activated. The type of occurred
      --  transaction, OUT or SETUP, can be determined from the SETUP bit
      --  described below. A transaction ended with a NAK or STALL handshake
      --  does not set this bit, since no data is actually transferred, as in
      --  the case of protocol errors or data toggle mismatches. This bit is
      --  read/write but only 0 can be written, writing 1 has no effect. Host
      --  mode This bit is set by the hardware when an IN transaction is
      --  successfully completed on this channel. The software can only clear
      --  this bit. If the CTRM bit in USB_CNTR register is set a generic
      --  interrupt condition is generated together with the channel related
      --  flag, which is always activated. - A transaction ended with a NAK
      --  sets this bit and NAK answer is reported to application reading the
      --  NAK state from the STATRX field of this register. One NAKed
      --  transaction keeps pending and is automatically retried by the host at
      --  the next frame, or the host can immediately retry by resetting STATRX
      --  state to VALID. - A transaction ended by STALL handshake sets this
      --  bit and the STALL answer is reported to application reading the STALL
      --  state from the STATRX field of this register. Host application must
      --  consequently disable the channel and re-enumerate. - A transaction
      --  ended with ACK handshake sets this bit If double buffering is
      --  disabled, ACK answer is reported by application reading the DISABLE
      --  state from the STATRX field of this register. Host application must
      --  read received data from USBRAM and re-arm the channel by writing
      --  VALID to the STATRX field of this register. If double buffering is
      --  enabled, ACK answer is reported by application reading VALID state
      --  from the STATRX field of this register. Host application must read
      --  received data from USBRAM and toggle the DTOGTX bit of this register.
      --  - A transaction ended with error sets this bit. Errors can be seen
      --  via the bits ERR_RX (host mode only). This bit is read/write but only
      --  0 can be written, writing 1 has no effect.
      VTRX           : Boolean := False;
      --  Host mode Device address assigned to the endpoint during the
      --  enumeration process.
      DEVADDR        : USB_CHEP3R_DEVADDR_Field := 16#0#;
      --  Host mode This bit is set by the hardware when a device responds with
      --  a NAK. Software can use this bit to monitor the number of NAKs
      --  received from a device.
      NAK            : Boolean := False;
      --  Low speed endpoint
      LS_EP          : Boolean := False;
      --  Received error for an OUT/SETUP transaction Host mode This bit is set
      --  by the hardware when an error (for example no answer by the device,
      --  CRC error, bit stuffing error, framing format violation, etc.) has
      --  occurred during an OUT or SETUP transaction on this channel. The
      --  software can only clear this bit. If the ERRM bit in USB_CNTR
      --  register is set, a generic interrupt condition is generated together
      --  with the channel related flag, which is always activated.
      ERR_TX         : Boolean := False;
      --  Received error for an IN transaction Host mode This bit is set by the
      --  hardware when an error (for example no answer by the device, CRC
      --  error, bit stuffing error, framing format violation, etc.) has
      --  occurred during an IN transaction on this channel. The software can
      --  only clear this bit. If the ERRM bit in USB_CNTR register is set, a
      --  generic interrupt condition is generated together with the channel
      --  related flag, which is always activated.
      ERR_RX         : Boolean := False;
      --  Three errors for an OUT or SETUP transaction Host mode This bit is
      --  set by the hardware when 3 consecutive transaction errors occurred on
      --  the USB bus for an OUT transaction. THREE_ERR_TX is not generated for
      --  isochronous transactions. The software can only clear this bit.
      --  Coding of the received error:
      THREE_ERR_TX   : USB_CHEP3R_THREE_ERR_TX_Field := 16#0#;
      --  Three errors for an IN transaction Host mode This bit is set by the
      --  hardware when 3 consecutive transaction errors occurred on the USB
      --  bus for an IN transaction. THREE_ERR_RX is not generated for
      --  isochronous transactions. The software can only clear this bit.
      --  Coding of the received error:
      THREE_ERR_RX   : USB_CHEP3R_THREE_ERR_RX_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for USB_CHEP3R_Register use record
      EA             at 0 range 0 .. 3;
      STATTX         at 0 range 4 .. 5;
      DTOGTX         at 0 range 6 .. 6;
      VTTX           at 0 range 7 .. 7;
      EPKIND         at 0 range 8 .. 8;
      UTYPE          at 0 range 9 .. 10;
      SETUP          at 0 range 11 .. 11;
      STATRX         at 0 range 12 .. 13;
      DTOGRX         at 0 range 14 .. 14;
      VTRX           at 0 range 15 .. 15;
      DEVADDR        at 0 range 16 .. 22;
      NAK            at 0 range 23 .. 23;
      LS_EP          at 0 range 24 .. 24;
      ERR_TX         at 0 range 25 .. 25;
      ERR_RX         at 0 range 26 .. 26;
      THREE_ERR_TX   at 0 range 27 .. 28;
      THREE_ERR_RX   at 0 range 29 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype USB_CHEP4R_EA_Field is HAL.UInt4;
   subtype USB_CHEP4R_STATTX_Field is HAL.UInt2;
   subtype USB_CHEP4R_UTYPE_Field is HAL.UInt2;
   subtype USB_CHEP4R_STATRX_Field is HAL.UInt2;
   subtype USB_CHEP4R_DEVADDR_Field is HAL.UInt7;
   subtype USB_CHEP4R_THREE_ERR_TX_Field is HAL.UInt2;
   subtype USB_CHEP4R_THREE_ERR_RX_Field is HAL.UInt2;

   --  USB endpoint/channel 4 register
   type USB_CHEP4R_Register is record
      --  endpoint/channel address Device mode Software must write in this
      --  field the 4-bit address used to identify the transactions directed to
      --  this endpoint. A value must be written before enabling the
      --  corresponding endpoint. Host mode Software must write in this field
      --  the 4-bit address used to identify the channel addressed by the host
      --  transaction.
      EA             : USB_CHEP4R_EA_Field := 16#0#;
      --  Write-only. Status bits, for transmission transfers
      STATTX         : USB_CHEP4R_STATTX_Field := 16#0#;
      --  Write-only. Data toggle, for transmission transfers If the
      --  endpoint/channel is non-isochronous, this bit contains the required
      --  value of the data toggle bit (01=1DATA0, 11=1DATA1) for the next data
      --  packet to be transmitted. Hardware toggles this bit when the ACK
      --  handshake is received from the USB host, following a data packet
      --  transmission. If the endpoint/channel is defined as a control one,
      --  hardware sets this bit to 1 at the reception of a SETUP PID addressed
      --  to this endpoint (in device mode) or when a SETUP transaction is
      --  acknowledged by the device (in host mode). If the endpoint/channel is
      --  using the double buffer feature, this bit is used to support packet
      --  buffer swapping too (Refer to Section134.5.3: Double-buffered
      --  endpoints and usage in Device mode). If the endpoint/channel is
      --  isochronous, this bit is used to support packet buffer swapping since
      --  no data toggling is used for this sort of endpoints and only DATA0
      --  packet are transmitted (refer to Section134.5.5: Isochronous
      --  transfers in Device mode). Hardware toggles this bit just after the
      --  end of data packet transmission, since no handshake is used for
      --  isochronous transfers. This bit can also be toggled by the software
      --  to initialize its value (mandatory when the endpoint/channel is not a
      --  control one) or to force a specific data toggle/packet buffer usage.
      --  When the application software writes 0, the value of DTOGTX remains
      --  unchanged, while writing 1 makes the bit value to toggle. This bit is
      --  read/write but it can only be toggled by writing 1.
      DTOGTX         : Boolean := False;
      --  Valid USB transaction transmitted Device mode This bit is set by the
      --  hardware when an IN transaction is successfully completed on this
      --  endpoint; the software can only clear this bit. If the CTRM bit in
      --  the USB_CNTR register is set accordingly, a generic interrupt
      --  condition is generated together with the endpoint related interrupt
      --  condition, which is always activated. A transaction ended with a NAK
      --  or STALL handshake does not set this bit, since no data is actually
      --  transferred, as in the case of protocol errors or data toggle
      --  mismatches. This bit is read/write but only 0 can be written. Host
      --  mode Same as VTRX behavior but for USB OUT and SETUP transactions.
      VTTX           : Boolean := False;
      --  endpoint/channel kind The meaning of this bit depends on the
      --  endpoint/channel type configured by the UTYPE bits. Table1217
      --  summarizes the different meanings. DBL_BUF: This bit is set by the
      --  software to enable the double-buffering feature for this bulk
      --  endpoint. The usage of double-buffered bulk endpoints is explained in
      --  Section134.5.3: Double-buffered endpoints and usage in Device mode.
      --  STATUS_OUT: This bit is set by the software to indicate that a status
      --  out transaction is expected: in this case all OUT transactions
      --  containing more than zero data bytes are answered STALL instead of
      --  ACK. This bit may be used to improve the robustness of the
      --  application to protocol errors during control transfers and its usage
      --  is intended for control endpoints only. When STATUS_OUT is reset, OUT
      --  transactions can have any number of bytes, as required.
      EPKIND         : Boolean := False;
      --  USB type of transaction These bits configure the behavior of this
      --  endpoint/channel as described in Table1216: Endpoint/channel type
      --  encoding. Channel0/Endpoint0 must always be a control
      --  endpoint/channel and each USB function must have at least one control
      --  endpoint/channel which has address 0, but there may be other control
      --  channels/endpoints if required. Only control channels/endpoints
      --  handle SETUP transactions, which are ignored by endpoints of other
      --  kinds. SETUP transactions cannot be answered with NAK or STALL. If a
      --  control endpoint/channel is defined as NAK, the USB peripheral does
      --  not answer, simulating a receive error, in the receive direction when
      --  a SETUP transaction is received. If the control endpoint/channel is
      --  defined as STALL in the receive direction, then the SETUP packet is
      --  accepted anyway, transferring data and issuing the CTR interrupt. The
      --  reception of OUT transactions is handled in the normal way, even if
      --  the endpoint/channel is a control one. Bulk and interrupt endpoints
      --  have very similar behavior and they differ only in the special
      --  feature available using the EPKIND configuration bit. The usage of
      --  isochronous channels/endpoints is explained in Section134.5.5:
      --  Isochronous transfers in Device mode
      UTYPE          : USB_CHEP4R_UTYPE_Field := 16#0#;
      --  Read-only. Setup transaction completed Device mode This bit is
      --  read-only and it is set by the hardware when the last completed
      --  transaction is a SETUP. This bit changes its value only for control
      --  endpoints. It must be examined, in the case of a successful receive
      --  transaction (VTRX event), to determine the type of transaction
      --  occurred. To protect the interrupt service routine from the changes
      --  in SETUP bits due to next incoming tokens, this bit is kept frozen
      --  while VTRX bit is at 1; its state changes when VTRX is at 0. This bit
      --  is read-only. Host mode This bit is set by the software to send a
      --  SETUP transaction on a control endpoint. This bit changes its value
      --  only for control endpoints. It is cleared by hardware when the SETUP
      --  transaction is acknowledged and VTTX interrupt generated.
      SETUP          : Boolean := False;
      --  Write-only. Status bits, for reception transfers Device mode These
      --  bits contain information about the endpoint status, which are listed
      --  in Table1215: Reception status encoding on page11025. These bits can
      --  be toggled by software to initialize their value. When the
      --  application software writes 0, the value remains unchanged, while
      --  writing 1 makes the bit value to toggle. Hardware sets the STATRX
      --  bits to NAK when a correct transfer has occurred (VTRX1=11)
      --  corresponding to a OUT or SETUP (control only) transaction addressed
      --  to this endpoint, so the software has the time to elaborate the
      --  received data before it acknowledges a new transaction.
      --  Double-buffered bulk endpoints implement a special transaction flow
      --  control, which control the status based upon buffer availability
      --  condition (Refer to Section134.5.3: Double-buffered endpoints and
      --  usage in Device mode). If the endpoint is defined as isochronous, its
      --  status can be only VALID or DISABLED, so that the hardware cannot
      --  change the status of the endpoint after a successful transaction. If
      --  the software sets the STATRX bits to STALL or NAK for an isochronous
      --  endpoint, the USB peripheral behavior is not defined. These bits are
      --  read/write but they can be only toggled by writing 1. Host mode These
      --  bits are the host application controls to start, retry, or abort host
      --  transactions driven by the channel. These bits also contain
      --  information about the device answer to the last IN channel
      --  transaction and report the current status of the channel according to
      --  the following STATRX table of states: -	DISABLE DISABLE value is
      --  reported in case of ACK acknowledge is received on a single-buffer
      --  channel. When in DISABLE state the channel is unused or not active
      --  waiting for application to restart it by writing VALID. Application
      --  can reset a VALID channel to DISABLE to abort a transaction. In this
      --  case the transaction is immediately removed from the host execution
      --  list. If the aborted transaction was already under execution it is
      --  regularly terminated on the USB but the relative VTRX interrupt is
      --  not generated. -	VALID A host channel is actively trying to submit
      --  USB transaction to device only when in VALID state.VALID state can be
      --  set by software or automatically by hardware on a NAKED channel at
      --  the start of a new frame. When set to VALID, an host channel enters
      --  the host execution queue and waits permission from the host frame
      --  scheduler to submit its configured transaction. VALID value is also
      --  reported in case of ACK acknowledge is received on a double-buffered
      --  channel. In this case the channel remains active on the alternate
      --  buffer while application needs to read the current buffer and toggle
      --  DTOGTX. In case software is late in reading and the alternate buffer
      --  is not ready, the host channel is automatically suspended
      --  transparently to the application. The suspended double buffered
      --  channel is re-activated as soon as delay is recovered and DTOGTX is
      --  toggled. - NAK NAK value is reported in case of NAK acknowledge
      --  received. When in NAK state the channel is suspended and does not try
      --  to transmit. NAK state is moved to VALID by hardware at the start of
      --  the next frame, or software can change it to immediately retry
      --  transmission by writing it to VALID, or can disable it and abort the
      --  transaction by writing DISABLE - STALL STALL value is reported in
      --  case of STALL acknowledge received. When in STALL state the channel
      --  behaves as disabled. Application must not retry transmission but
      --  reset the USB and re-enumerate.
      STATRX         : USB_CHEP4R_STATRX_Field := 16#0#;
      --  Write-only. Data Toggle, for reception transfers If the
      --  endpoint/channel is not isochronous, this bit contains the expected
      --  value of the data toggle bit (01=1DATA0, 11=1DATA1) for the next data
      --  packet to be received. Hardware toggles this bit, when the ACK
      --  handshake is sent following a data packet reception having a matching
      --  data PID value; if the endpoint is defined as a control one, hardware
      --  clears this bit at the reception of a SETUP PID received from host
      --  (in device mode), while it sets this bit to 1 when SETUP transaction
      --  is acknowledged by device (in host mode). If the endpoint/channel is
      --  using the double-buffering feature this bit is used to support packet
      --  buffer swapping too (Refer to Section134.5.3: Double-buffered
      --  endpoints and usage in Device mode). If the endpoint/channel is
      --  isochronous, this bit is used only to support packet buffer swapping
      --  for data transmission since no data toggling is used for this kind of
      --  channels/endpoints and only DATA0 packet are transmitted (Refer to
      --  Section134.5.5: Isochronous transfers in Device mode). Hardware
      --  toggles this bit just after the end of data packet reception, since
      --  no handshake is used for isochronous transfers. This bit can also be
      --  toggled by the software to initialize its value (mandatory when the
      --  endpoint is not a control one) or to force specific data
      --  toggle/packet buffer usage. When the application software writes 0,
      --  the value of DTOGRX remains unchanged, while writing 1 makes the bit
      --  value toggle. This bit is read/write but it can be only toggled by
      --  writing 1.
      DTOGRX         : Boolean := False;
      --  USB valid transaction received Device mode This bit is set by the
      --  hardware when an OUT/SETUP transaction is successfully completed on
      --  this endpoint; the software can only clear this bit. If the CTRM bit
      --  in USB_CNTR register is set accordingly, a generic interrupt
      --  condition is generated together with the endpoint related interrupt
      --  condition, which is always activated. The type of occurred
      --  transaction, OUT or SETUP, can be determined from the SETUP bit
      --  described below. A transaction ended with a NAK or STALL handshake
      --  does not set this bit, since no data is actually transferred, as in
      --  the case of protocol errors or data toggle mismatches. This bit is
      --  read/write but only 0 can be written, writing 1 has no effect. Host
      --  mode This bit is set by the hardware when an IN transaction is
      --  successfully completed on this channel. The software can only clear
      --  this bit. If the CTRM bit in USB_CNTR register is set a generic
      --  interrupt condition is generated together with the channel related
      --  flag, which is always activated. - A transaction ended with a NAK
      --  sets this bit and NAK answer is reported to application reading the
      --  NAK state from the STATRX field of this register. One NAKed
      --  transaction keeps pending and is automatically retried by the host at
      --  the next frame, or the host can immediately retry by resetting STATRX
      --  state to VALID. - A transaction ended by STALL handshake sets this
      --  bit and the STALL answer is reported to application reading the STALL
      --  state from the STATRX field of this register. Host application must
      --  consequently disable the channel and re-enumerate. - A transaction
      --  ended with ACK handshake sets this bit If double buffering is
      --  disabled, ACK answer is reported by application reading the DISABLE
      --  state from the STATRX field of this register. Host application must
      --  read received data from USBRAM and re-arm the channel by writing
      --  VALID to the STATRX field of this register. If double buffering is
      --  enabled, ACK answer is reported by application reading VALID state
      --  from the STATRX field of this register. Host application must read
      --  received data from USBRAM and toggle the DTOGTX bit of this register.
      --  - A transaction ended with error sets this bit. Errors can be seen
      --  via the bits ERR_RX (host mode only). This bit is read/write but only
      --  0 can be written, writing 1 has no effect.
      VTRX           : Boolean := False;
      --  Host mode Device address assigned to the endpoint during the
      --  enumeration process.
      DEVADDR        : USB_CHEP4R_DEVADDR_Field := 16#0#;
      --  Host mode This bit is set by the hardware when a device responds with
      --  a NAK. Software can use this bit to monitor the number of NAKs
      --  received from a device.
      NAK            : Boolean := False;
      --  Low speed endpoint
      LS_EP          : Boolean := False;
      --  Received error for an OUT/SETUP transaction Host mode This bit is set
      --  by the hardware when an error (for example no answer by the device,
      --  CRC error, bit stuffing error, framing format violation, etc.) has
      --  occurred during an OUT or SETUP transaction on this channel. The
      --  software can only clear this bit. If the ERRM bit in USB_CNTR
      --  register is set, a generic interrupt condition is generated together
      --  with the channel related flag, which is always activated.
      ERR_TX         : Boolean := False;
      --  Received error for an IN transaction Host mode This bit is set by the
      --  hardware when an error (for example no answer by the device, CRC
      --  error, bit stuffing error, framing format violation, etc.) has
      --  occurred during an IN transaction on this channel. The software can
      --  only clear this bit. If the ERRM bit in USB_CNTR register is set, a
      --  generic interrupt condition is generated together with the channel
      --  related flag, which is always activated.
      ERR_RX         : Boolean := False;
      --  Three errors for an OUT or SETUP transaction Host mode This bit is
      --  set by the hardware when 3 consecutive transaction errors occurred on
      --  the USB bus for an OUT transaction. THREE_ERR_TX is not generated for
      --  isochronous transactions. The software can only clear this bit.
      --  Coding of the received error:
      THREE_ERR_TX   : USB_CHEP4R_THREE_ERR_TX_Field := 16#0#;
      --  Three errors for an IN transaction Host mode This bit is set by the
      --  hardware when 3 consecutive transaction errors occurred on the USB
      --  bus for an IN transaction. THREE_ERR_RX is not generated for
      --  isochronous transactions. The software can only clear this bit.
      --  Coding of the received error:
      THREE_ERR_RX   : USB_CHEP4R_THREE_ERR_RX_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for USB_CHEP4R_Register use record
      EA             at 0 range 0 .. 3;
      STATTX         at 0 range 4 .. 5;
      DTOGTX         at 0 range 6 .. 6;
      VTTX           at 0 range 7 .. 7;
      EPKIND         at 0 range 8 .. 8;
      UTYPE          at 0 range 9 .. 10;
      SETUP          at 0 range 11 .. 11;
      STATRX         at 0 range 12 .. 13;
      DTOGRX         at 0 range 14 .. 14;
      VTRX           at 0 range 15 .. 15;
      DEVADDR        at 0 range 16 .. 22;
      NAK            at 0 range 23 .. 23;
      LS_EP          at 0 range 24 .. 24;
      ERR_TX         at 0 range 25 .. 25;
      ERR_RX         at 0 range 26 .. 26;
      THREE_ERR_TX   at 0 range 27 .. 28;
      THREE_ERR_RX   at 0 range 29 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype USB_CHEP5R_EA_Field is HAL.UInt4;
   subtype USB_CHEP5R_STATTX_Field is HAL.UInt2;
   subtype USB_CHEP5R_UTYPE_Field is HAL.UInt2;
   subtype USB_CHEP5R_STATRX_Field is HAL.UInt2;
   subtype USB_CHEP5R_DEVADDR_Field is HAL.UInt7;
   subtype USB_CHEP5R_THREE_ERR_TX_Field is HAL.UInt2;
   subtype USB_CHEP5R_THREE_ERR_RX_Field is HAL.UInt2;

   --  USB endpoint/channel 5 register
   type USB_CHEP5R_Register is record
      --  endpoint/channel address Device mode Software must write in this
      --  field the 4-bit address used to identify the transactions directed to
      --  this endpoint. A value must be written before enabling the
      --  corresponding endpoint. Host mode Software must write in this field
      --  the 4-bit address used to identify the channel addressed by the host
      --  transaction.
      EA             : USB_CHEP5R_EA_Field := 16#0#;
      --  Write-only. Status bits, for transmission transfers
      STATTX         : USB_CHEP5R_STATTX_Field := 16#0#;
      --  Write-only. Data toggle, for transmission transfers If the
      --  endpoint/channel is non-isochronous, this bit contains the required
      --  value of the data toggle bit (01=1DATA0, 11=1DATA1) for the next data
      --  packet to be transmitted. Hardware toggles this bit when the ACK
      --  handshake is received from the USB host, following a data packet
      --  transmission. If the endpoint/channel is defined as a control one,
      --  hardware sets this bit to 1 at the reception of a SETUP PID addressed
      --  to this endpoint (in device mode) or when a SETUP transaction is
      --  acknowledged by the device (in host mode). If the endpoint/channel is
      --  using the double buffer feature, this bit is used to support packet
      --  buffer swapping too (Refer to Section134.5.3: Double-buffered
      --  endpoints and usage in Device mode). If the endpoint/channel is
      --  isochronous, this bit is used to support packet buffer swapping since
      --  no data toggling is used for this sort of endpoints and only DATA0
      --  packet are transmitted (refer to Section134.5.5: Isochronous
      --  transfers in Device mode). Hardware toggles this bit just after the
      --  end of data packet transmission, since no handshake is used for
      --  isochronous transfers. This bit can also be toggled by the software
      --  to initialize its value (mandatory when the endpoint/channel is not a
      --  control one) or to force a specific data toggle/packet buffer usage.
      --  When the application software writes 0, the value of DTOGTX remains
      --  unchanged, while writing 1 makes the bit value to toggle. This bit is
      --  read/write but it can only be toggled by writing 1.
      DTOGTX         : Boolean := False;
      --  Valid USB transaction transmitted Device mode This bit is set by the
      --  hardware when an IN transaction is successfully completed on this
      --  endpoint; the software can only clear this bit. If the CTRM bit in
      --  the USB_CNTR register is set accordingly, a generic interrupt
      --  condition is generated together with the endpoint related interrupt
      --  condition, which is always activated. A transaction ended with a NAK
      --  or STALL handshake does not set this bit, since no data is actually
      --  transferred, as in the case of protocol errors or data toggle
      --  mismatches. This bit is read/write but only 0 can be written. Host
      --  mode Same as VTRX behavior but for USB OUT and SETUP transactions.
      VTTX           : Boolean := False;
      --  endpoint/channel kind The meaning of this bit depends on the
      --  endpoint/channel type configured by the UTYPE bits. Table1217
      --  summarizes the different meanings. DBL_BUF: This bit is set by the
      --  software to enable the double-buffering feature for this bulk
      --  endpoint. The usage of double-buffered bulk endpoints is explained in
      --  Section134.5.3: Double-buffered endpoints and usage in Device mode.
      --  STATUS_OUT: This bit is set by the software to indicate that a status
      --  out transaction is expected: in this case all OUT transactions
      --  containing more than zero data bytes are answered STALL instead of
      --  ACK. This bit may be used to improve the robustness of the
      --  application to protocol errors during control transfers and its usage
      --  is intended for control endpoints only. When STATUS_OUT is reset, OUT
      --  transactions can have any number of bytes, as required.
      EPKIND         : Boolean := False;
      --  USB type of transaction These bits configure the behavior of this
      --  endpoint/channel as described in Table1216: Endpoint/channel type
      --  encoding. Channel0/Endpoint0 must always be a control
      --  endpoint/channel and each USB function must have at least one control
      --  endpoint/channel which has address 0, but there may be other control
      --  channels/endpoints if required. Only control channels/endpoints
      --  handle SETUP transactions, which are ignored by endpoints of other
      --  kinds. SETUP transactions cannot be answered with NAK or STALL. If a
      --  control endpoint/channel is defined as NAK, the USB peripheral does
      --  not answer, simulating a receive error, in the receive direction when
      --  a SETUP transaction is received. If the control endpoint/channel is
      --  defined as STALL in the receive direction, then the SETUP packet is
      --  accepted anyway, transferring data and issuing the CTR interrupt. The
      --  reception of OUT transactions is handled in the normal way, even if
      --  the endpoint/channel is a control one. Bulk and interrupt endpoints
      --  have very similar behavior and they differ only in the special
      --  feature available using the EPKIND configuration bit. The usage of
      --  isochronous channels/endpoints is explained in Section134.5.5:
      --  Isochronous transfers in Device mode
      UTYPE          : USB_CHEP5R_UTYPE_Field := 16#0#;
      --  Read-only. Setup transaction completed Device mode This bit is
      --  read-only and it is set by the hardware when the last completed
      --  transaction is a SETUP. This bit changes its value only for control
      --  endpoints. It must be examined, in the case of a successful receive
      --  transaction (VTRX event), to determine the type of transaction
      --  occurred. To protect the interrupt service routine from the changes
      --  in SETUP bits due to next incoming tokens, this bit is kept frozen
      --  while VTRX bit is at 1; its state changes when VTRX is at 0. This bit
      --  is read-only. Host mode This bit is set by the software to send a
      --  SETUP transaction on a control endpoint. This bit changes its value
      --  only for control endpoints. It is cleared by hardware when the SETUP
      --  transaction is acknowledged and VTTX interrupt generated.
      SETUP          : Boolean := False;
      --  Write-only. Status bits, for reception transfers Device mode These
      --  bits contain information about the endpoint status, which are listed
      --  in Table1215: Reception status encoding on page11025. These bits can
      --  be toggled by software to initialize their value. When the
      --  application software writes 0, the value remains unchanged, while
      --  writing 1 makes the bit value to toggle. Hardware sets the STATRX
      --  bits to NAK when a correct transfer has occurred (VTRX1=11)
      --  corresponding to a OUT or SETUP (control only) transaction addressed
      --  to this endpoint, so the software has the time to elaborate the
      --  received data before it acknowledges a new transaction.
      --  Double-buffered bulk endpoints implement a special transaction flow
      --  control, which control the status based upon buffer availability
      --  condition (Refer to Section134.5.3: Double-buffered endpoints and
      --  usage in Device mode). If the endpoint is defined as isochronous, its
      --  status can be only VALID or DISABLED, so that the hardware cannot
      --  change the status of the endpoint after a successful transaction. If
      --  the software sets the STATRX bits to STALL or NAK for an isochronous
      --  endpoint, the USB peripheral behavior is not defined. These bits are
      --  read/write but they can be only toggled by writing 1. Host mode These
      --  bits are the host application controls to start, retry, or abort host
      --  transactions driven by the channel. These bits also contain
      --  information about the device answer to the last IN channel
      --  transaction and report the current status of the channel according to
      --  the following STATRX table of states: -	DISABLE DISABLE value is
      --  reported in case of ACK acknowledge is received on a single-buffer
      --  channel. When in DISABLE state the channel is unused or not active
      --  waiting for application to restart it by writing VALID. Application
      --  can reset a VALID channel to DISABLE to abort a transaction. In this
      --  case the transaction is immediately removed from the host execution
      --  list. If the aborted transaction was already under execution it is
      --  regularly terminated on the USB but the relative VTRX interrupt is
      --  not generated. -	VALID A host channel is actively trying to submit
      --  USB transaction to device only when in VALID state.VALID state can be
      --  set by software or automatically by hardware on a NAKED channel at
      --  the start of a new frame. When set to VALID, an host channel enters
      --  the host execution queue and waits permission from the host frame
      --  scheduler to submit its configured transaction. VALID value is also
      --  reported in case of ACK acknowledge is received on a double-buffered
      --  channel. In this case the channel remains active on the alternate
      --  buffer while application needs to read the current buffer and toggle
      --  DTOGTX. In case software is late in reading and the alternate buffer
      --  is not ready, the host channel is automatically suspended
      --  transparently to the application. The suspended double buffered
      --  channel is re-activated as soon as delay is recovered and DTOGTX is
      --  toggled. - NAK NAK value is reported in case of NAK acknowledge
      --  received. When in NAK state the channel is suspended and does not try
      --  to transmit. NAK state is moved to VALID by hardware at the start of
      --  the next frame, or software can change it to immediately retry
      --  transmission by writing it to VALID, or can disable it and abort the
      --  transaction by writing DISABLE - STALL STALL value is reported in
      --  case of STALL acknowledge received. When in STALL state the channel
      --  behaves as disabled. Application must not retry transmission but
      --  reset the USB and re-enumerate.
      STATRX         : USB_CHEP5R_STATRX_Field := 16#0#;
      --  Write-only. Data Toggle, for reception transfers If the
      --  endpoint/channel is not isochronous, this bit contains the expected
      --  value of the data toggle bit (01=1DATA0, 11=1DATA1) for the next data
      --  packet to be received. Hardware toggles this bit, when the ACK
      --  handshake is sent following a data packet reception having a matching
      --  data PID value; if the endpoint is defined as a control one, hardware
      --  clears this bit at the reception of a SETUP PID received from host
      --  (in device mode), while it sets this bit to 1 when SETUP transaction
      --  is acknowledged by device (in host mode). If the endpoint/channel is
      --  using the double-buffering feature this bit is used to support packet
      --  buffer swapping too (Refer to Section134.5.3: Double-buffered
      --  endpoints and usage in Device mode). If the endpoint/channel is
      --  isochronous, this bit is used only to support packet buffer swapping
      --  for data transmission since no data toggling is used for this kind of
      --  channels/endpoints and only DATA0 packet are transmitted (Refer to
      --  Section134.5.5: Isochronous transfers in Device mode). Hardware
      --  toggles this bit just after the end of data packet reception, since
      --  no handshake is used for isochronous transfers. This bit can also be
      --  toggled by the software to initialize its value (mandatory when the
      --  endpoint is not a control one) or to force specific data
      --  toggle/packet buffer usage. When the application software writes 0,
      --  the value of DTOGRX remains unchanged, while writing 1 makes the bit
      --  value toggle. This bit is read/write but it can be only toggled by
      --  writing 1.
      DTOGRX         : Boolean := False;
      --  USB valid transaction received Device mode This bit is set by the
      --  hardware when an OUT/SETUP transaction is successfully completed on
      --  this endpoint; the software can only clear this bit. If the CTRM bit
      --  in USB_CNTR register is set accordingly, a generic interrupt
      --  condition is generated together with the endpoint related interrupt
      --  condition, which is always activated. The type of occurred
      --  transaction, OUT or SETUP, can be determined from the SETUP bit
      --  described below. A transaction ended with a NAK or STALL handshake
      --  does not set this bit, since no data is actually transferred, as in
      --  the case of protocol errors or data toggle mismatches. This bit is
      --  read/write but only 0 can be written, writing 1 has no effect. Host
      --  mode This bit is set by the hardware when an IN transaction is
      --  successfully completed on this channel. The software can only clear
      --  this bit. If the CTRM bit in USB_CNTR register is set a generic
      --  interrupt condition is generated together with the channel related
      --  flag, which is always activated. - A transaction ended with a NAK
      --  sets this bit and NAK answer is reported to application reading the
      --  NAK state from the STATRX field of this register. One NAKed
      --  transaction keeps pending and is automatically retried by the host at
      --  the next frame, or the host can immediately retry by resetting STATRX
      --  state to VALID. - A transaction ended by STALL handshake sets this
      --  bit and the STALL answer is reported to application reading the STALL
      --  state from the STATRX field of this register. Host application must
      --  consequently disable the channel and re-enumerate. - A transaction
      --  ended with ACK handshake sets this bit If double buffering is
      --  disabled, ACK answer is reported by application reading the DISABLE
      --  state from the STATRX field of this register. Host application must
      --  read received data from USBRAM and re-arm the channel by writing
      --  VALID to the STATRX field of this register. If double buffering is
      --  enabled, ACK answer is reported by application reading VALID state
      --  from the STATRX field of this register. Host application must read
      --  received data from USBRAM and toggle the DTOGTX bit of this register.
      --  - A transaction ended with error sets this bit. Errors can be seen
      --  via the bits ERR_RX (host mode only). This bit is read/write but only
      --  0 can be written, writing 1 has no effect.
      VTRX           : Boolean := False;
      --  Host mode Device address assigned to the endpoint during the
      --  enumeration process.
      DEVADDR        : USB_CHEP5R_DEVADDR_Field := 16#0#;
      --  Host mode This bit is set by the hardware when a device responds with
      --  a NAK. Software can use this bit to monitor the number of NAKs
      --  received from a device.
      NAK            : Boolean := False;
      --  Low speed endpoint
      LS_EP          : Boolean := False;
      --  Received error for an OUT/SETUP transaction Host mode This bit is set
      --  by the hardware when an error (for example no answer by the device,
      --  CRC error, bit stuffing error, framing format violation, etc.) has
      --  occurred during an OUT or SETUP transaction on this channel. The
      --  software can only clear this bit. If the ERRM bit in USB_CNTR
      --  register is set, a generic interrupt condition is generated together
      --  with the channel related flag, which is always activated.
      ERR_TX         : Boolean := False;
      --  Received error for an IN transaction Host mode This bit is set by the
      --  hardware when an error (for example no answer by the device, CRC
      --  error, bit stuffing error, framing format violation, etc.) has
      --  occurred during an IN transaction on this channel. The software can
      --  only clear this bit. If the ERRM bit in USB_CNTR register is set, a
      --  generic interrupt condition is generated together with the channel
      --  related flag, which is always activated.
      ERR_RX         : Boolean := False;
      --  Three errors for an OUT or SETUP transaction Host mode This bit is
      --  set by the hardware when 3 consecutive transaction errors occurred on
      --  the USB bus for an OUT transaction. THREE_ERR_TX is not generated for
      --  isochronous transactions. The software can only clear this bit.
      --  Coding of the received error:
      THREE_ERR_TX   : USB_CHEP5R_THREE_ERR_TX_Field := 16#0#;
      --  Three errors for an IN transaction Host mode This bit is set by the
      --  hardware when 3 consecutive transaction errors occurred on the USB
      --  bus for an IN transaction. THREE_ERR_RX is not generated for
      --  isochronous transactions. The software can only clear this bit.
      --  Coding of the received error:
      THREE_ERR_RX   : USB_CHEP5R_THREE_ERR_RX_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for USB_CHEP5R_Register use record
      EA             at 0 range 0 .. 3;
      STATTX         at 0 range 4 .. 5;
      DTOGTX         at 0 range 6 .. 6;
      VTTX           at 0 range 7 .. 7;
      EPKIND         at 0 range 8 .. 8;
      UTYPE          at 0 range 9 .. 10;
      SETUP          at 0 range 11 .. 11;
      STATRX         at 0 range 12 .. 13;
      DTOGRX         at 0 range 14 .. 14;
      VTRX           at 0 range 15 .. 15;
      DEVADDR        at 0 range 16 .. 22;
      NAK            at 0 range 23 .. 23;
      LS_EP          at 0 range 24 .. 24;
      ERR_TX         at 0 range 25 .. 25;
      ERR_RX         at 0 range 26 .. 26;
      THREE_ERR_TX   at 0 range 27 .. 28;
      THREE_ERR_RX   at 0 range 29 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype USB_CHEP6R_EA_Field is HAL.UInt4;
   subtype USB_CHEP6R_STATTX_Field is HAL.UInt2;
   subtype USB_CHEP6R_UTYPE_Field is HAL.UInt2;
   subtype USB_CHEP6R_STATRX_Field is HAL.UInt2;
   subtype USB_CHEP6R_DEVADDR_Field is HAL.UInt7;
   subtype USB_CHEP6R_THREE_ERR_TX_Field is HAL.UInt2;
   subtype USB_CHEP6R_THREE_ERR_RX_Field is HAL.UInt2;

   --  USB endpoint/channel 6 register
   type USB_CHEP6R_Register is record
      --  endpoint/channel address Device mode Software must write in this
      --  field the 4-bit address used to identify the transactions directed to
      --  this endpoint. A value must be written before enabling the
      --  corresponding endpoint. Host mode Software must write in this field
      --  the 4-bit address used to identify the channel addressed by the host
      --  transaction.
      EA             : USB_CHEP6R_EA_Field := 16#0#;
      --  Write-only. Status bits, for transmission transfers
      STATTX         : USB_CHEP6R_STATTX_Field := 16#0#;
      --  Write-only. Data toggle, for transmission transfers If the
      --  endpoint/channel is non-isochronous, this bit contains the required
      --  value of the data toggle bit (01=1DATA0, 11=1DATA1) for the next data
      --  packet to be transmitted. Hardware toggles this bit when the ACK
      --  handshake is received from the USB host, following a data packet
      --  transmission. If the endpoint/channel is defined as a control one,
      --  hardware sets this bit to 1 at the reception of a SETUP PID addressed
      --  to this endpoint (in device mode) or when a SETUP transaction is
      --  acknowledged by the device (in host mode). If the endpoint/channel is
      --  using the double buffer feature, this bit is used to support packet
      --  buffer swapping too (Refer to Section134.5.3: Double-buffered
      --  endpoints and usage in Device mode). If the endpoint/channel is
      --  isochronous, this bit is used to support packet buffer swapping since
      --  no data toggling is used for this sort of endpoints and only DATA0
      --  packet are transmitted (refer to Section134.5.5: Isochronous
      --  transfers in Device mode). Hardware toggles this bit just after the
      --  end of data packet transmission, since no handshake is used for
      --  isochronous transfers. This bit can also be toggled by the software
      --  to initialize its value (mandatory when the endpoint/channel is not a
      --  control one) or to force a specific data toggle/packet buffer usage.
      --  When the application software writes 0, the value of DTOGTX remains
      --  unchanged, while writing 1 makes the bit value to toggle. This bit is
      --  read/write but it can only be toggled by writing 1.
      DTOGTX         : Boolean := False;
      --  Valid USB transaction transmitted Device mode This bit is set by the
      --  hardware when an IN transaction is successfully completed on this
      --  endpoint; the software can only clear this bit. If the CTRM bit in
      --  the USB_CNTR register is set accordingly, a generic interrupt
      --  condition is generated together with the endpoint related interrupt
      --  condition, which is always activated. A transaction ended with a NAK
      --  or STALL handshake does not set this bit, since no data is actually
      --  transferred, as in the case of protocol errors or data toggle
      --  mismatches. This bit is read/write but only 0 can be written. Host
      --  mode Same as VTRX behavior but for USB OUT and SETUP transactions.
      VTTX           : Boolean := False;
      --  endpoint/channel kind The meaning of this bit depends on the
      --  endpoint/channel type configured by the UTYPE bits. Table1217
      --  summarizes the different meanings. DBL_BUF: This bit is set by the
      --  software to enable the double-buffering feature for this bulk
      --  endpoint. The usage of double-buffered bulk endpoints is explained in
      --  Section134.5.3: Double-buffered endpoints and usage in Device mode.
      --  STATUS_OUT: This bit is set by the software to indicate that a status
      --  out transaction is expected: in this case all OUT transactions
      --  containing more than zero data bytes are answered STALL instead of
      --  ACK. This bit may be used to improve the robustness of the
      --  application to protocol errors during control transfers and its usage
      --  is intended for control endpoints only. When STATUS_OUT is reset, OUT
      --  transactions can have any number of bytes, as required.
      EPKIND         : Boolean := False;
      --  USB type of transaction These bits configure the behavior of this
      --  endpoint/channel as described in Table1216: Endpoint/channel type
      --  encoding. Channel0/Endpoint0 must always be a control
      --  endpoint/channel and each USB function must have at least one control
      --  endpoint/channel which has address 0, but there may be other control
      --  channels/endpoints if required. Only control channels/endpoints
      --  handle SETUP transactions, which are ignored by endpoints of other
      --  kinds. SETUP transactions cannot be answered with NAK or STALL. If a
      --  control endpoint/channel is defined as NAK, the USB peripheral does
      --  not answer, simulating a receive error, in the receive direction when
      --  a SETUP transaction is received. If the control endpoint/channel is
      --  defined as STALL in the receive direction, then the SETUP packet is
      --  accepted anyway, transferring data and issuing the CTR interrupt. The
      --  reception of OUT transactions is handled in the normal way, even if
      --  the endpoint/channel is a control one. Bulk and interrupt endpoints
      --  have very similar behavior and they differ only in the special
      --  feature available using the EPKIND configuration bit. The usage of
      --  isochronous channels/endpoints is explained in Section134.5.5:
      --  Isochronous transfers in Device mode
      UTYPE          : USB_CHEP6R_UTYPE_Field := 16#0#;
      --  Read-only. Setup transaction completed Device mode This bit is
      --  read-only and it is set by the hardware when the last completed
      --  transaction is a SETUP. This bit changes its value only for control
      --  endpoints. It must be examined, in the case of a successful receive
      --  transaction (VTRX event), to determine the type of transaction
      --  occurred. To protect the interrupt service routine from the changes
      --  in SETUP bits due to next incoming tokens, this bit is kept frozen
      --  while VTRX bit is at 1; its state changes when VTRX is at 0. This bit
      --  is read-only. Host mode This bit is set by the software to send a
      --  SETUP transaction on a control endpoint. This bit changes its value
      --  only for control endpoints. It is cleared by hardware when the SETUP
      --  transaction is acknowledged and VTTX interrupt generated.
      SETUP          : Boolean := False;
      --  Write-only. Status bits, for reception transfers Device mode These
      --  bits contain information about the endpoint status, which are listed
      --  in Table1215: Reception status encoding on page11025. These bits can
      --  be toggled by software to initialize their value. When the
      --  application software writes 0, the value remains unchanged, while
      --  writing 1 makes the bit value to toggle. Hardware sets the STATRX
      --  bits to NAK when a correct transfer has occurred (VTRX1=11)
      --  corresponding to a OUT or SETUP (control only) transaction addressed
      --  to this endpoint, so the software has the time to elaborate the
      --  received data before it acknowledges a new transaction.
      --  Double-buffered bulk endpoints implement a special transaction flow
      --  control, which control the status based upon buffer availability
      --  condition (Refer to Section134.5.3: Double-buffered endpoints and
      --  usage in Device mode). If the endpoint is defined as isochronous, its
      --  status can be only VALID or DISABLED, so that the hardware cannot
      --  change the status of the endpoint after a successful transaction. If
      --  the software sets the STATRX bits to STALL or NAK for an isochronous
      --  endpoint, the USB peripheral behavior is not defined. These bits are
      --  read/write but they can be only toggled by writing 1. Host mode These
      --  bits are the host application controls to start, retry, or abort host
      --  transactions driven by the channel. These bits also contain
      --  information about the device answer to the last IN channel
      --  transaction and report the current status of the channel according to
      --  the following STATRX table of states: -	DISABLE DISABLE value is
      --  reported in case of ACK acknowledge is received on a single-buffer
      --  channel. When in DISABLE state the channel is unused or not active
      --  waiting for application to restart it by writing VALID. Application
      --  can reset a VALID channel to DISABLE to abort a transaction. In this
      --  case the transaction is immediately removed from the host execution
      --  list. If the aborted transaction was already under execution it is
      --  regularly terminated on the USB but the relative VTRX interrupt is
      --  not generated. -	VALID A host channel is actively trying to submit
      --  USB transaction to device only when in VALID state.VALID state can be
      --  set by software or automatically by hardware on a NAKED channel at
      --  the start of a new frame. When set to VALID, an host channel enters
      --  the host execution queue and waits permission from the host frame
      --  scheduler to submit its configured transaction. VALID value is also
      --  reported in case of ACK acknowledge is received on a double-buffered
      --  channel. In this case the channel remains active on the alternate
      --  buffer while application needs to read the current buffer and toggle
      --  DTOGTX. In case software is late in reading and the alternate buffer
      --  is not ready, the host channel is automatically suspended
      --  transparently to the application. The suspended double buffered
      --  channel is re-activated as soon as delay is recovered and DTOGTX is
      --  toggled. - NAK NAK value is reported in case of NAK acknowledge
      --  received. When in NAK state the channel is suspended and does not try
      --  to transmit. NAK state is moved to VALID by hardware at the start of
      --  the next frame, or software can change it to immediately retry
      --  transmission by writing it to VALID, or can disable it and abort the
      --  transaction by writing DISABLE - STALL STALL value is reported in
      --  case of STALL acknowledge received. When in STALL state the channel
      --  behaves as disabled. Application must not retry transmission but
      --  reset the USB and re-enumerate.
      STATRX         : USB_CHEP6R_STATRX_Field := 16#0#;
      --  Write-only. Data Toggle, for reception transfers If the
      --  endpoint/channel is not isochronous, this bit contains the expected
      --  value of the data toggle bit (01=1DATA0, 11=1DATA1) for the next data
      --  packet to be received. Hardware toggles this bit, when the ACK
      --  handshake is sent following a data packet reception having a matching
      --  data PID value; if the endpoint is defined as a control one, hardware
      --  clears this bit at the reception of a SETUP PID received from host
      --  (in device mode), while it sets this bit to 1 when SETUP transaction
      --  is acknowledged by device (in host mode). If the endpoint/channel is
      --  using the double-buffering feature this bit is used to support packet
      --  buffer swapping too (Refer to Section134.5.3: Double-buffered
      --  endpoints and usage in Device mode). If the endpoint/channel is
      --  isochronous, this bit is used only to support packet buffer swapping
      --  for data transmission since no data toggling is used for this kind of
      --  channels/endpoints and only DATA0 packet are transmitted (Refer to
      --  Section134.5.5: Isochronous transfers in Device mode). Hardware
      --  toggles this bit just after the end of data packet reception, since
      --  no handshake is used for isochronous transfers. This bit can also be
      --  toggled by the software to initialize its value (mandatory when the
      --  endpoint is not a control one) or to force specific data
      --  toggle/packet buffer usage. When the application software writes 0,
      --  the value of DTOGRX remains unchanged, while writing 1 makes the bit
      --  value toggle. This bit is read/write but it can be only toggled by
      --  writing 1.
      DTOGRX         : Boolean := False;
      --  USB valid transaction received Device mode This bit is set by the
      --  hardware when an OUT/SETUP transaction is successfully completed on
      --  this endpoint; the software can only clear this bit. If the CTRM bit
      --  in USB_CNTR register is set accordingly, a generic interrupt
      --  condition is generated together with the endpoint related interrupt
      --  condition, which is always activated. The type of occurred
      --  transaction, OUT or SETUP, can be determined from the SETUP bit
      --  described below. A transaction ended with a NAK or STALL handshake
      --  does not set this bit, since no data is actually transferred, as in
      --  the case of protocol errors or data toggle mismatches. This bit is
      --  read/write but only 0 can be written, writing 1 has no effect. Host
      --  mode This bit is set by the hardware when an IN transaction is
      --  successfully completed on this channel. The software can only clear
      --  this bit. If the CTRM bit in USB_CNTR register is set a generic
      --  interrupt condition is generated together with the channel related
      --  flag, which is always activated. - A transaction ended with a NAK
      --  sets this bit and NAK answer is reported to application reading the
      --  NAK state from the STATRX field of this register. One NAKed
      --  transaction keeps pending and is automatically retried by the host at
      --  the next frame, or the host can immediately retry by resetting STATRX
      --  state to VALID. - A transaction ended by STALL handshake sets this
      --  bit and the STALL answer is reported to application reading the STALL
      --  state from the STATRX field of this register. Host application must
      --  consequently disable the channel and re-enumerate. - A transaction
      --  ended with ACK handshake sets this bit If double buffering is
      --  disabled, ACK answer is reported by application reading the DISABLE
      --  state from the STATRX field of this register. Host application must
      --  read received data from USBRAM and re-arm the channel by writing
      --  VALID to the STATRX field of this register. If double buffering is
      --  enabled, ACK answer is reported by application reading VALID state
      --  from the STATRX field of this register. Host application must read
      --  received data from USBRAM and toggle the DTOGTX bit of this register.
      --  - A transaction ended with error sets this bit. Errors can be seen
      --  via the bits ERR_RX (host mode only). This bit is read/write but only
      --  0 can be written, writing 1 has no effect.
      VTRX           : Boolean := False;
      --  Host mode Device address assigned to the endpoint during the
      --  enumeration process.
      DEVADDR        : USB_CHEP6R_DEVADDR_Field := 16#0#;
      --  Host mode This bit is set by the hardware when a device responds with
      --  a NAK. Software can use this bit to monitor the number of NAKs
      --  received from a device.
      NAK            : Boolean := False;
      --  Low speed endpoint
      LS_EP          : Boolean := False;
      --  Received error for an OUT/SETUP transaction Host mode This bit is set
      --  by the hardware when an error (for example no answer by the device,
      --  CRC error, bit stuffing error, framing format violation, etc.) has
      --  occurred during an OUT or SETUP transaction on this channel. The
      --  software can only clear this bit. If the ERRM bit in USB_CNTR
      --  register is set, a generic interrupt condition is generated together
      --  with the channel related flag, which is always activated.
      ERR_TX         : Boolean := False;
      --  Received error for an IN transaction Host mode This bit is set by the
      --  hardware when an error (for example no answer by the device, CRC
      --  error, bit stuffing error, framing format violation, etc.) has
      --  occurred during an IN transaction on this channel. The software can
      --  only clear this bit. If the ERRM bit in USB_CNTR register is set, a
      --  generic interrupt condition is generated together with the channel
      --  related flag, which is always activated.
      ERR_RX         : Boolean := False;
      --  Three errors for an OUT or SETUP transaction Host mode This bit is
      --  set by the hardware when 3 consecutive transaction errors occurred on
      --  the USB bus for an OUT transaction. THREE_ERR_TX is not generated for
      --  isochronous transactions. The software can only clear this bit.
      --  Coding of the received error:
      THREE_ERR_TX   : USB_CHEP6R_THREE_ERR_TX_Field := 16#0#;
      --  Three errors for an IN transaction Host mode This bit is set by the
      --  hardware when 3 consecutive transaction errors occurred on the USB
      --  bus for an IN transaction. THREE_ERR_RX is not generated for
      --  isochronous transactions. The software can only clear this bit.
      --  Coding of the received error:
      THREE_ERR_RX   : USB_CHEP6R_THREE_ERR_RX_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for USB_CHEP6R_Register use record
      EA             at 0 range 0 .. 3;
      STATTX         at 0 range 4 .. 5;
      DTOGTX         at 0 range 6 .. 6;
      VTTX           at 0 range 7 .. 7;
      EPKIND         at 0 range 8 .. 8;
      UTYPE          at 0 range 9 .. 10;
      SETUP          at 0 range 11 .. 11;
      STATRX         at 0 range 12 .. 13;
      DTOGRX         at 0 range 14 .. 14;
      VTRX           at 0 range 15 .. 15;
      DEVADDR        at 0 range 16 .. 22;
      NAK            at 0 range 23 .. 23;
      LS_EP          at 0 range 24 .. 24;
      ERR_TX         at 0 range 25 .. 25;
      ERR_RX         at 0 range 26 .. 26;
      THREE_ERR_TX   at 0 range 27 .. 28;
      THREE_ERR_RX   at 0 range 29 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype USB_CHEP7R_EA_Field is HAL.UInt4;
   subtype USB_CHEP7R_STATTX_Field is HAL.UInt2;
   subtype USB_CHEP7R_UTYPE_Field is HAL.UInt2;
   subtype USB_CHEP7R_STATRX_Field is HAL.UInt2;
   subtype USB_CHEP7R_DEVADDR_Field is HAL.UInt7;
   subtype USB_CHEP7R_THREE_ERR_TX_Field is HAL.UInt2;
   subtype USB_CHEP7R_THREE_ERR_RX_Field is HAL.UInt2;

   --  USB endpoint/channel 7 register
   type USB_CHEP7R_Register is record
      --  endpoint/channel address Device mode Software must write in this
      --  field the 4-bit address used to identify the transactions directed to
      --  this endpoint. A value must be written before enabling the
      --  corresponding endpoint. Host mode Software must write in this field
      --  the 4-bit address used to identify the channel addressed by the host
      --  transaction.
      EA             : USB_CHEP7R_EA_Field := 16#0#;
      --  Write-only. Status bits, for transmission transfers
      STATTX         : USB_CHEP7R_STATTX_Field := 16#0#;
      --  Write-only. Data toggle, for transmission transfers If the
      --  endpoint/channel is non-isochronous, this bit contains the required
      --  value of the data toggle bit (01=1DATA0, 11=1DATA1) for the next data
      --  packet to be transmitted. Hardware toggles this bit when the ACK
      --  handshake is received from the USB host, following a data packet
      --  transmission. If the endpoint/channel is defined as a control one,
      --  hardware sets this bit to 1 at the reception of a SETUP PID addressed
      --  to this endpoint (in device mode) or when a SETUP transaction is
      --  acknowledged by the device (in host mode). If the endpoint/channel is
      --  using the double buffer feature, this bit is used to support packet
      --  buffer swapping too (Refer to Section134.5.3: Double-buffered
      --  endpoints and usage in Device mode). If the endpoint/channel is
      --  isochronous, this bit is used to support packet buffer swapping since
      --  no data toggling is used for this sort of endpoints and only DATA0
      --  packet are transmitted (refer to Section134.5.5: Isochronous
      --  transfers in Device mode). Hardware toggles this bit just after the
      --  end of data packet transmission, since no handshake is used for
      --  isochronous transfers. This bit can also be toggled by the software
      --  to initialize its value (mandatory when the endpoint/channel is not a
      --  control one) or to force a specific data toggle/packet buffer usage.
      --  When the application software writes 0, the value of DTOGTX remains
      --  unchanged, while writing 1 makes the bit value to toggle. This bit is
      --  read/write but it can only be toggled by writing 1.
      DTOGTX         : Boolean := False;
      --  Valid USB transaction transmitted Device mode This bit is set by the
      --  hardware when an IN transaction is successfully completed on this
      --  endpoint; the software can only clear this bit. If the CTRM bit in
      --  the USB_CNTR register is set accordingly, a generic interrupt
      --  condition is generated together with the endpoint related interrupt
      --  condition, which is always activated. A transaction ended with a NAK
      --  or STALL handshake does not set this bit, since no data is actually
      --  transferred, as in the case of protocol errors or data toggle
      --  mismatches. This bit is read/write but only 0 can be written. Host
      --  mode Same as VTRX behavior but for USB OUT and SETUP transactions.
      VTTX           : Boolean := False;
      --  endpoint/channel kind The meaning of this bit depends on the
      --  endpoint/channel type configured by the UTYPE bits. Table1217
      --  summarizes the different meanings. DBL_BUF: This bit is set by the
      --  software to enable the double-buffering feature for this bulk
      --  endpoint. The usage of double-buffered bulk endpoints is explained in
      --  Section134.5.3: Double-buffered endpoints and usage in Device mode.
      --  STATUS_OUT: This bit is set by the software to indicate that a status
      --  out transaction is expected: in this case all OUT transactions
      --  containing more than zero data bytes are answered STALL instead of
      --  ACK. This bit may be used to improve the robustness of the
      --  application to protocol errors during control transfers and its usage
      --  is intended for control endpoints only. When STATUS_OUT is reset, OUT
      --  transactions can have any number of bytes, as required.
      EPKIND         : Boolean := False;
      --  USB type of transaction These bits configure the behavior of this
      --  endpoint/channel as described in Table1216: Endpoint/channel type
      --  encoding. Channel0/Endpoint0 must always be a control
      --  endpoint/channel and each USB function must have at least one control
      --  endpoint/channel which has address 0, but there may be other control
      --  channels/endpoints if required. Only control channels/endpoints
      --  handle SETUP transactions, which are ignored by endpoints of other
      --  kinds. SETUP transactions cannot be answered with NAK or STALL. If a
      --  control endpoint/channel is defined as NAK, the USB peripheral does
      --  not answer, simulating a receive error, in the receive direction when
      --  a SETUP transaction is received. If the control endpoint/channel is
      --  defined as STALL in the receive direction, then the SETUP packet is
      --  accepted anyway, transferring data and issuing the CTR interrupt. The
      --  reception of OUT transactions is handled in the normal way, even if
      --  the endpoint/channel is a control one. Bulk and interrupt endpoints
      --  have very similar behavior and they differ only in the special
      --  feature available using the EPKIND configuration bit. The usage of
      --  isochronous channels/endpoints is explained in Section134.5.5:
      --  Isochronous transfers in Device mode
      UTYPE          : USB_CHEP7R_UTYPE_Field := 16#0#;
      --  Read-only. Setup transaction completed Device mode This bit is
      --  read-only and it is set by the hardware when the last completed
      --  transaction is a SETUP. This bit changes its value only for control
      --  endpoints. It must be examined, in the case of a successful receive
      --  transaction (VTRX event), to determine the type of transaction
      --  occurred. To protect the interrupt service routine from the changes
      --  in SETUP bits due to next incoming tokens, this bit is kept frozen
      --  while VTRX bit is at 1; its state changes when VTRX is at 0. This bit
      --  is read-only. Host mode This bit is set by the software to send a
      --  SETUP transaction on a control endpoint. This bit changes its value
      --  only for control endpoints. It is cleared by hardware when the SETUP
      --  transaction is acknowledged and VTTX interrupt generated.
      SETUP          : Boolean := False;
      --  Write-only. Status bits, for reception transfers Device mode These
      --  bits contain information about the endpoint status, which are listed
      --  in Table1215: Reception status encoding on page11025. These bits can
      --  be toggled by software to initialize their value. When the
      --  application software writes 0, the value remains unchanged, while
      --  writing 1 makes the bit value to toggle. Hardware sets the STATRX
      --  bits to NAK when a correct transfer has occurred (VTRX1=11)
      --  corresponding to a OUT or SETUP (control only) transaction addressed
      --  to this endpoint, so the software has the time to elaborate the
      --  received data before it acknowledges a new transaction.
      --  Double-buffered bulk endpoints implement a special transaction flow
      --  control, which control the status based upon buffer availability
      --  condition (Refer to Section134.5.3: Double-buffered endpoints and
      --  usage in Device mode). If the endpoint is defined as isochronous, its
      --  status can be only VALID or DISABLED, so that the hardware cannot
      --  change the status of the endpoint after a successful transaction. If
      --  the software sets the STATRX bits to STALL or NAK for an isochronous
      --  endpoint, the USB peripheral behavior is not defined. These bits are
      --  read/write but they can be only toggled by writing 1. Host mode These
      --  bits are the host application controls to start, retry, or abort host
      --  transactions driven by the channel. These bits also contain
      --  information about the device answer to the last IN channel
      --  transaction and report the current status of the channel according to
      --  the following STATRX table of states: -	DISABLE DISABLE value is
      --  reported in case of ACK acknowledge is received on a single-buffer
      --  channel. When in DISABLE state the channel is unused or not active
      --  waiting for application to restart it by writing VALID. Application
      --  can reset a VALID channel to DISABLE to abort a transaction. In this
      --  case the transaction is immediately removed from the host execution
      --  list. If the aborted transaction was already under execution it is
      --  regularly terminated on the USB but the relative VTRX interrupt is
      --  not generated. -	VALID A host channel is actively trying to submit
      --  USB transaction to device only when in VALID state.VALID state can be
      --  set by software or automatically by hardware on a NAKED channel at
      --  the start of a new frame. When set to VALID, an host channel enters
      --  the host execution queue and waits permission from the host frame
      --  scheduler to submit its configured transaction. VALID value is also
      --  reported in case of ACK acknowledge is received on a double-buffered
      --  channel. In this case the channel remains active on the alternate
      --  buffer while application needs to read the current buffer and toggle
      --  DTOGTX. In case software is late in reading and the alternate buffer
      --  is not ready, the host channel is automatically suspended
      --  transparently to the application. The suspended double buffered
      --  channel is re-activated as soon as delay is recovered and DTOGTX is
      --  toggled. - NAK NAK value is reported in case of NAK acknowledge
      --  received. When in NAK state the channel is suspended and does not try
      --  to transmit. NAK state is moved to VALID by hardware at the start of
      --  the next frame, or software can change it to immediately retry
      --  transmission by writing it to VALID, or can disable it and abort the
      --  transaction by writing DISABLE - STALL STALL value is reported in
      --  case of STALL acknowledge received. When in STALL state the channel
      --  behaves as disabled. Application must not retry transmission but
      --  reset the USB and re-enumerate.
      STATRX         : USB_CHEP7R_STATRX_Field := 16#0#;
      --  Write-only. Data Toggle, for reception transfers If the
      --  endpoint/channel is not isochronous, this bit contains the expected
      --  value of the data toggle bit (01=1DATA0, 11=1DATA1) for the next data
      --  packet to be received. Hardware toggles this bit, when the ACK
      --  handshake is sent following a data packet reception having a matching
      --  data PID value; if the endpoint is defined as a control one, hardware
      --  clears this bit at the reception of a SETUP PID received from host
      --  (in device mode), while it sets this bit to 1 when SETUP transaction
      --  is acknowledged by device (in host mode). If the endpoint/channel is
      --  using the double-buffering feature this bit is used to support packet
      --  buffer swapping too (Refer to Section134.5.3: Double-buffered
      --  endpoints and usage in Device mode). If the endpoint/channel is
      --  isochronous, this bit is used only to support packet buffer swapping
      --  for data transmission since no data toggling is used for this kind of
      --  channels/endpoints and only DATA0 packet are transmitted (Refer to
      --  Section134.5.5: Isochronous transfers in Device mode). Hardware
      --  toggles this bit just after the end of data packet reception, since
      --  no handshake is used for isochronous transfers. This bit can also be
      --  toggled by the software to initialize its value (mandatory when the
      --  endpoint is not a control one) or to force specific data
      --  toggle/packet buffer usage. When the application software writes 0,
      --  the value of DTOGRX remains unchanged, while writing 1 makes the bit
      --  value toggle. This bit is read/write but it can be only toggled by
      --  writing 1.
      DTOGRX         : Boolean := False;
      --  USB valid transaction received Device mode This bit is set by the
      --  hardware when an OUT/SETUP transaction is successfully completed on
      --  this endpoint; the software can only clear this bit. If the CTRM bit
      --  in USB_CNTR register is set accordingly, a generic interrupt
      --  condition is generated together with the endpoint related interrupt
      --  condition, which is always activated. The type of occurred
      --  transaction, OUT or SETUP, can be determined from the SETUP bit
      --  described below. A transaction ended with a NAK or STALL handshake
      --  does not set this bit, since no data is actually transferred, as in
      --  the case of protocol errors or data toggle mismatches. This bit is
      --  read/write but only 0 can be written, writing 1 has no effect. Host
      --  mode This bit is set by the hardware when an IN transaction is
      --  successfully completed on this channel. The software can only clear
      --  this bit. If the CTRM bit in USB_CNTR register is set a generic
      --  interrupt condition is generated together with the channel related
      --  flag, which is always activated. - A transaction ended with a NAK
      --  sets this bit and NAK answer is reported to application reading the
      --  NAK state from the STATRX field of this register. One NAKed
      --  transaction keeps pending and is automatically retried by the host at
      --  the next frame, or the host can immediately retry by resetting STATRX
      --  state to VALID. - A transaction ended by STALL handshake sets this
      --  bit and the STALL answer is reported to application reading the STALL
      --  state from the STATRX field of this register. Host application must
      --  consequently disable the channel and re-enumerate. - A transaction
      --  ended with ACK handshake sets this bit If double buffering is
      --  disabled, ACK answer is reported by application reading the DISABLE
      --  state from the STATRX field of this register. Host application must
      --  read received data from USBRAM and re-arm the channel by writing
      --  VALID to the STATRX field of this register. If double buffering is
      --  enabled, ACK answer is reported by application reading VALID state
      --  from the STATRX field of this register. Host application must read
      --  received data from USBRAM and toggle the DTOGTX bit of this register.
      --  - A transaction ended with error sets this bit. Errors can be seen
      --  via the bits ERR_RX (host mode only). This bit is read/write but only
      --  0 can be written, writing 1 has no effect.
      VTRX           : Boolean := False;
      --  Host mode Device address assigned to the endpoint during the
      --  enumeration process.
      DEVADDR        : USB_CHEP7R_DEVADDR_Field := 16#0#;
      --  Host mode This bit is set by the hardware when a device responds with
      --  a NAK. Software can use this bit to monitor the number of NAKs
      --  received from a device.
      NAK            : Boolean := False;
      --  Low speed endpoint
      LS_EP          : Boolean := False;
      --  Received error for an OUT/SETUP transaction Host mode This bit is set
      --  by the hardware when an error (for example no answer by the device,
      --  CRC error, bit stuffing error, framing format violation, etc.) has
      --  occurred during an OUT or SETUP transaction on this channel. The
      --  software can only clear this bit. If the ERRM bit in USB_CNTR
      --  register is set, a generic interrupt condition is generated together
      --  with the channel related flag, which is always activated.
      ERR_TX         : Boolean := False;
      --  Received error for an IN transaction Host mode This bit is set by the
      --  hardware when an error (for example no answer by the device, CRC
      --  error, bit stuffing error, framing format violation, etc.) has
      --  occurred during an IN transaction on this channel. The software can
      --  only clear this bit. If the ERRM bit in USB_CNTR register is set, a
      --  generic interrupt condition is generated together with the channel
      --  related flag, which is always activated.
      ERR_RX         : Boolean := False;
      --  Three errors for an OUT or SETUP transaction Host mode This bit is
      --  set by the hardware when 3 consecutive transaction errors occurred on
      --  the USB bus for an OUT transaction. THREE_ERR_TX is not generated for
      --  isochronous transactions. The software can only clear this bit.
      --  Coding of the received error:
      THREE_ERR_TX   : USB_CHEP7R_THREE_ERR_TX_Field := 16#0#;
      --  Three errors for an IN transaction Host mode This bit is set by the
      --  hardware when 3 consecutive transaction errors occurred on the USB
      --  bus for an IN transaction. THREE_ERR_RX is not generated for
      --  isochronous transactions. The software can only clear this bit.
      --  Coding of the received error:
      THREE_ERR_RX   : USB_CHEP7R_THREE_ERR_RX_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for USB_CHEP7R_Register use record
      EA             at 0 range 0 .. 3;
      STATTX         at 0 range 4 .. 5;
      DTOGTX         at 0 range 6 .. 6;
      VTTX           at 0 range 7 .. 7;
      EPKIND         at 0 range 8 .. 8;
      UTYPE          at 0 range 9 .. 10;
      SETUP          at 0 range 11 .. 11;
      STATRX         at 0 range 12 .. 13;
      DTOGRX         at 0 range 14 .. 14;
      VTRX           at 0 range 15 .. 15;
      DEVADDR        at 0 range 16 .. 22;
      NAK            at 0 range 23 .. 23;
      LS_EP          at 0 range 24 .. 24;
      ERR_TX         at 0 range 25 .. 25;
      ERR_RX         at 0 range 26 .. 26;
      THREE_ERR_TX   at 0 range 27 .. 28;
      THREE_ERR_RX   at 0 range 29 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   --  USB control register
   type USB_CNTR_Register is record
      --  USB Reset Software can set this bit to reset the USB core, exactly as
      --  it happens when receiving a RESET signaling on the USB.The USB
      --  peripheral, in response to a RESET, resets its internal protocol
      --  state machine. Reception and transmission are disabled until the
      --  RST_DCON bit is cleared. All configuration registers do not reset:
      --  the microcontroller must explicitly clear these registers (this is to
      --  ensure that the RST_DCON interrupt can be safely delivered, and any
      --  transaction immediately followed by a RESET can be completed). The
      --  function address and endpoint registers are reset by an USB reset
      --  event. Software sets this bit to drive USB reset state on the bus and
      --  initialize the device. USB reset terminates as soon as this bit is
      --  cleared by software.
      USBRST         : Boolean := True;
      --  Power down This bit is used to completely switch off all USB-related
      --  analog parts if it is required to completely disable the USB
      --  peripheral for any reason. When this bit is set, the USB peripheral
      --  is disconnected from the transceivers and it cannot be used.
      PDWN           : Boolean := True;
      --  Read-only. Suspend state effective This bit is set by hardware as
      --  soon as the suspend state entered through the SUSPEN control gets
      --  internally effective. In this state USB activity is suspended, USB
      --  clock is gated, transceiver is set in low power mode by disabling the
      --  differential receiver. Only asynchronous wake-up logic and single
      --  ended receiver is kept alive to detect remote wake-up or resume
      --  events. Software must poll this bit to confirm it to be set before
      --  any STOP mode entry. This bit is cleared by hardware simultaneously
      --  to the WAKEUP flag being set.
      SUSPRDY        : Boolean := False;
      --  Suspend state enable Software can set this bit when the SUSP
      --  interrupt is received, which is issued when no traffic is received by
      --  the USB peripheral for 31ms. Software can also set this bit when the
      --  L1REQ interrupt is received with positive acknowledge sent. As soon
      --  as the suspend state is propagated internally all device activity is
      --  stopped, USB clock is gated, USB transceiver is set into low power
      --  mode and the SUSPRDY bit is set by hardware. In the case that device
      --  application wants to pursue more aggressive power saving by stopping
      --  the USB clock source and by moving the microcontroller to stop mode,
      --  as in the case of bus powered device application, it must first wait
      --  few cycles to see the SUSPRDY1=11 acknowledge the suspend request.
      --  This bit is cleared by hardware simultaneous with the WAKEUP flag
      --  set. Software can set this bit when host application has nothing
      --  scheduled for the next frames and wants to enter long term power
      --  saving. When set, it stops immediately SOF generation and any other
      --  host activity, gates the USB clock and sets the transceiver in low
      --  power mode. If any USB transaction is on-going at the time SUSPEN is
      --  set, suspend is entered at the end of the current transaction. As
      --  soon as suspend state is propagated internally and gets effective the
      --  SUSPRDY bit is set. In the case that host application wants to pursue
      --  more aggressive power saving by stopping the USB clock source and by
      --  moving the micro-controller to STOP mode, it must first wait few
      --  cycles to see SUSPRDY=1 acknowledge to the suspend request. This bit
      --  is cleared by hardware simultaneous with the WAKEUP flag set.
      SUSPEN         : Boolean := False;
      --  L2 remote wake-up / resume driver Device mode The microcontroller can
      --  set this bit to send remote wake-up signaling to the host. It must be
      --  activated, according to USB specifications, for no less than 11ms and
      --  no more than 151ms after which the host PC is ready to drive the
      --  resume sequence up to its end. Host mode Software sets this bit to
      --  send resume signaling to the device. Software clears this bit to send
      --  end of resume to device and restart SOF generation. In the context of
      --  remote wake up, this bit is to be set following the WAKEUP interrupt.
      L2RES          : Boolean := False;
      --  L1 remote wake-up / resume driver
      L1RES          : Boolean := False;
      --  unspecified
      Reserved_6_6   : HAL.Bit := 16#0#;
      --  LPM L1 state request interrupt mask
      L1REQM         : Boolean := False;
      --  Expected start of frame interrupt mask
      ESOFM          : Boolean := False;
      --  Start of frame interrupt mask
      SOFM           : Boolean := False;
      --  USB reset request (Device mode) or device connect/disconnect (Host
      --  mode) interrupt mask
      RST_DCONM      : Boolean := False;
      --  Suspend mode interrupt mask
      SUSPM          : Boolean := False;
      --  Wake-up interrupt mask
      WKUPM          : Boolean := False;
      --  Error interrupt mask
      ERRM           : Boolean := False;
      --  Packet memory area over / underrun interrupt mask
      PMAOVRM        : Boolean := False;
      --  Correct transfer interrupt mask
      CTRM           : Boolean := False;
      --  512 byte threshold interrupt mask
      THR512M        : Boolean := False;
      --  Device disconnection mask Host mode
      DDISCM         : Boolean := False;
      --  unspecified
      Reserved_18_30 : HAL.UInt13 := 16#0#;
      --  HOST mode HOST bit selects betweens host or device USB mode of
      --  operation. It must be set before enabling the USB peripheral by the
      --  function enable bit.
      HOST           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for USB_CNTR_Register use record
      USBRST         at 0 range 0 .. 0;
      PDWN           at 0 range 1 .. 1;
      SUSPRDY        at 0 range 2 .. 2;
      SUSPEN         at 0 range 3 .. 3;
      L2RES          at 0 range 4 .. 4;
      L1RES          at 0 range 5 .. 5;
      Reserved_6_6   at 0 range 6 .. 6;
      L1REQM         at 0 range 7 .. 7;
      ESOFM          at 0 range 8 .. 8;
      SOFM           at 0 range 9 .. 9;
      RST_DCONM      at 0 range 10 .. 10;
      SUSPM          at 0 range 11 .. 11;
      WKUPM          at 0 range 12 .. 12;
      ERRM           at 0 range 13 .. 13;
      PMAOVRM        at 0 range 14 .. 14;
      CTRM           at 0 range 15 .. 15;
      THR512M        at 0 range 16 .. 16;
      DDISCM         at 0 range 17 .. 17;
      Reserved_18_30 at 0 range 18 .. 30;
      HOST           at 0 range 31 .. 31;
   end record;

   subtype USB_ISTR_IDN_Field is HAL.UInt4;

   --  USB interrupt status register
   type USB_ISTR_Register is record
      --  Read-only. Device Endpoint / host channel identification number These
      --  bits are written by the hardware according to the host channel or
      --  device endpoint number, which generated the interrupt request. If
      --  several endpoint/channel transactions are pending, the hardware
      --  writes the identification number related to the endpoint/channel
      --  having the highest priority defined in the following way: two levels
      --  are defined, in order of priority: isochronous and double-buffered
      --  bulk channels/endpoints are considered first and then the others are
      --  examined. If more than one endpoint/channel from the same set is
      --  requesting an interrupt, the IDN bits in USB_ISTR register are
      --  assigned according to the lowest requesting register, CHEP0R having
      --  the highest priority followed by CHEP1R and so on. The application
      --  software can assign a register to each endpoint/channel according to
      --  this priority scheme, so as to order the concurring endpoint/channel
      --  requests in a suitable way. These bits are read only.
      IDN            : USB_ISTR_IDN_Field := 16#0#;
      --  Read-only. Direction of transaction This bit is written by the
      --  hardware according to the direction of the successful transaction,
      --  which generated the interrupt request. If DIR bit1=10, VTTX bit is
      --  set in the USB_CHEPnR register related to the interrupting endpoint.
      --  The interrupting transaction is of IN type (data transmitted by the
      --  USB peripheral to the host PC). If DIR bit1=11, VTRX bit or both
      --  VTTX/VTRX are set in the USB_CHEPnR register related to the
      --  interrupting endpoint. The interrupting transaction is of OUT type
      --  (data received by the USB peripheral from the host PC) or two pending
      --  transactions are waiting to be processed. This information can be
      --  used by the application software to access the USB_CHEPnR bits
      --  related to the triggering transaction since it represents the
      --  direction having the interrupt pending. This bit is read-only.
      DIR            : Boolean := False;
      --  unspecified
      Reserved_5_6   : HAL.UInt2 := 16#0#;
      --  LPM L1 state request Device mode This bit is set by the hardware when
      --  LPM command to enter the L1 state is successfully received and
      --  acknowledged. This bit is read/write but only 0 can be written and
      --  writing 1 has no effect.
      L1REQ          : Boolean := False;
      --  Expected start of frame Device mode This bit is set by the hardware
      --  when an SOF packet is expected but not received. The host sends an
      --  SOF packet each 11ms, but if the device does not receive it properly,
      --  the suspend timer issues this interrupt. If three consecutive ESOF
      --  interrupts are generated (for example three SOF packets are lost)
      --  without any traffic occurring in between, a SUSP interrupt is
      --  generated. This bit is set even when the missing SOF packets occur
      --  while the suspend timer is not yet locked. This bit is read/write but
      --  only 0 can be written and writing 1 has no effect.
      ESOF           : Boolean := False;
      --  Start of frame This bit signals the beginning of a new USB frame and
      --  it is set when a SOF packet arrives through the USB bus. The
      --  interrupt service routine may monitor the SOF events to have a 11ms
      --  synchronization event to the USB host and to safely read the USB_FNR
      --  register which is updated at the SOF packet reception (this can be
      --  useful for isochronous applications). This bit is read/write but only
      --  0 can be written and writing 1 has no effect.
      SOF            : Boolean := False;
      --  USB reset request (Device mode) or device connect/disconnect (Host
      --  mode) Device mode This bit is set by hardware when an USB reset is
      --  released by the host and the bus returns to idle. USB reset state is
      --  internally detected after the sampling of 60 consecutive SE0 cycles.
      --  Host mode This bit is set by hardware when device connection or
      --  device disconnection is detected. Device connection is signaled after
      --  J state is sampled for 22 cycles consecutively from unconnected
      --  state. Device disconnection is signaled after SE0 state is seen for
      --  22 bit times consecutively from connected state.
      RST_DCON       : Boolean := False;
      --  Suspend mode request Device mode This bit is set by the hardware when
      --  no traffic has been received for 31ms, indicating a suspend mode
      --  request from the USB bus. The suspend condition check is enabled
      --  immediately after any USB reset and it is disabled by the hardware
      --  when the suspend mode is active (SUSPEN=1) until the end of resume
      --  sequence. This bit is read/write but only 0 can be written and
      --  writing 1 has no effect.
      SUSP           : Boolean := False;
      --  Wake-up This bit is set to 1 by the hardware when, during suspend
      --  mode, activity is detected that wakes up the USB peripheral. This
      --  event asynchronously clears the SUSPRDY bit in the CTLR register and
      --  activates the USB_WAKEUP line, which can be used to notify the rest
      --  of the device (for example wake-up unit) about the start of the
      --  resume process. This bit is read/write but only 0 can be written and
      --  writing 1 has no effect.
      WKUP           : Boolean := False;
      --  Error This flag is set whenever one of the errors listed below has
      --  occurred: NANS:	No ANSwer. The timeout for a host response has
      --  expired. CRC:	Cyclic redundancy check error. One of the received
      --  CRCs, either in the token or in the data, was wrong. BST:	Bit
      --  stuffing error. A bit stuffing error was detected anywhere in the
      --  PID, data, and/or CRC. FVIO:	Framing format violation. A non-standard
      --  frame was received (EOP not in the right place, wrong token sequence,
      --  etc.). The USB software can usually ignore errors, since the USB
      --  peripheral and the PC host manage retransmission in case of errors in
      --  a fully transparent way. This interrupt can be useful during the
      --  software development phase, or to monitor the quality of transmission
      --  over the USB bus, to flag possible problems to the user (for example
      --  loose connector, too noisy environment, broken conductor in the USB
      --  cable and so on). This bit is read/write but only 0 can be written
      --  and writing 1 has no effect.
      ERR            : Boolean := False;
      --  Packet memory area over / underrun This bit is set if the
      --  microcontroller has not been able to respond in time to an USB memory
      --  request. The USB peripheral handles this event in the following way:
      --  During reception an ACK handshake packet is not sent, during
      --  transmission a bit-stuff error is forced on the transmitted stream;
      --  in both cases the host retries the transaction. The PMAOVR interrupt
      --  must never occur during normal operations. Since the failed
      --  transaction is retried by the host, the application software has the
      --  chance to speed-up device operations during this interrupt handling,
      --  to be ready for the next transaction retry; however this does not
      --  happen during isochronous transfers (no isochronous transaction is
      --  anyway retried) leading to a loss of data in this case. This bit is
      --  read/write but only 0 can be written and writing 1 has no effect.
      PMAOVR         : Boolean := False;
      --  Read-only. Completed transfer in host mode This bit is set by the
      --  hardware to indicate that an endpoint/channel has successfully
      --  completed a transaction; using DIR and IDN bits software can
      --  determine which endpoint/channel requested the interrupt. This bit is
      --  read-only.
      CTR            : Boolean := False;
      --  512 byte threshold interrupt This bit is set to 1 by the hardware
      --  when 512 bytes have been transmitted or received during isochronous
      --  transfers. This bit is read/write but only 0 can be written and
      --  writing 1 has no effect. Note that no information is available to
      --  indicate the associated channel/endpoint, however in practice only
      --  one ISO endpoint/channel with such large packets can be supported, so
      --  that channel.
      THR512         : Boolean := False;
      --  Device connection Host mode This bit is set when a device connection
      --  is detected. This bit is read/write but only 0 can be written and
      --  writing 1 has no effect.
      DDISC          : Boolean := False;
      --  unspecified
      Reserved_18_28 : HAL.UInt11 := 16#0#;
      --  Read-only. Device connection status Host mode: This bit contains
      --  information about device connection status. It is set by hardware
      --  when a LS/FS device is attached to the host while it is reset when
      --  the device is disconnected.
      DCON_STAT      : Boolean := False;
      --  Read-only. Low speed device connected Host mode: This bit is set by
      --  hardware when an LS device connection is detected. Device connection
      --  is signaled after LS J-state is sampled for 22 consecutive cycles of
      --  the USB clock (481MHz) from the unconnected state.
      LS_DCON        : Boolean := False;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for USB_ISTR_Register use record
      IDN            at 0 range 0 .. 3;
      DIR            at 0 range 4 .. 4;
      Reserved_5_6   at 0 range 5 .. 6;
      L1REQ          at 0 range 7 .. 7;
      ESOF           at 0 range 8 .. 8;
      SOF            at 0 range 9 .. 9;
      RST_DCON       at 0 range 10 .. 10;
      SUSP           at 0 range 11 .. 11;
      WKUP           at 0 range 12 .. 12;
      ERR            at 0 range 13 .. 13;
      PMAOVR         at 0 range 14 .. 14;
      CTR            at 0 range 15 .. 15;
      THR512         at 0 range 16 .. 16;
      DDISC          at 0 range 17 .. 17;
      Reserved_18_28 at 0 range 18 .. 28;
      DCON_STAT      at 0 range 29 .. 29;
      LS_DCON        at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype USB_FNR_FN_Field is HAL.UInt11;
   subtype USB_FNR_LSOF_Field is HAL.UInt2;

   --  USB frame number register
   type USB_FNR_Register is record
      --  Read-only. Frame number This bit field contains the 11-bits frame
      --  number contained in the last received SOF packet. The frame number is
      --  incremented for every frame sent by the host and it is useful for
      --  isochronous transfers. This bit field is updated on the generation of
      --  an SOF interrupt.
      FN             : USB_FNR_FN_Field;
      --  Read-only. Lost SOF Device mode These bits are written by the
      --  hardware when an ESOF interrupt is generated, counting the number of
      --  consecutive SOF packets lost. At the reception of an SOF packet,
      --  these bits are cleared.
      LSOF           : USB_FNR_LSOF_Field;
      --  Read-only. Locked Device mode This bit is set by the hardware when at
      --  least two consecutive SOF packets have been received after the end of
      --  an USB reset condition or after the end of an USB resume sequence.
      --  Once locked, the frame timer remains in this state until an USB reset
      --  or USB suspend event occurs.
      LCK            : Boolean;
      --  Read-only. Receive data - line status This bit can be used to observe
      --  the status of received data minus upstream port data line. It can be
      --  used during end-of-suspend routines to help determining the wake-up
      --  event.
      RXDM           : Boolean;
      --  Read-only. Receive data + line status This bit can be used to observe
      --  the status of received data plus upstream port data line. It can be
      --  used during end-of-suspend routines to help determining the wake-up
      --  event.
      RXDP           : Boolean;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for USB_FNR_Register use record
      FN             at 0 range 0 .. 10;
      LSOF           at 0 range 11 .. 12;
      LCK            at 0 range 13 .. 13;
      RXDM           at 0 range 14 .. 14;
      RXDP           at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype USB_DADDR_ADD_Field is HAL.UInt7;

   --  USB Device address
   type USB_DADDR_Register is record
      --  Device address Device mode These bits contain the USB function
      --  address assigned by the host PC during the enumeration process. Both
      --  this field and the endpoint/channel address (EA) field in the
      --  associated USB_CHEPnR register must match with the information
      --  contained in a USB token in order to handle a transaction to the
      --  required endpoint. Host mode These bits contain the address
      --  transmitted with the LPM transaction
      ADD           : USB_DADDR_ADD_Field := 16#0#;
      --  Enable function This bit is set by the software to enable the USB
      --  Device. The address of this device is contained in the following
      --  ADD[6:0] bits. If this bit is at 0 no transactions are handled,
      --  irrespective of the settings of USB_CHEPnR registers.
      EF            : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for USB_DADDR_Register use record
      ADD           at 0 range 0 .. 6;
      EF            at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype USB_LPMCSR_BESL_Field is HAL.UInt4;

   --  LPM control and status register
   type USB_LPMCSR_Register is record
      --  LPM support enable Device mode This bit is set by the software to
      --  enable the LPM support within the USB Device. If this bit is at 0 no
      --  LPM transactions are handled.
      LPMEN         : Boolean := False;
      --  LPM token acknowledge enable Device mode: The NYET/ACK is returned
      --  only on a successful LPM transaction: No errors in both the EXT token
      --  and the LPM token (else ERROR) A valid bLinkState = 0001B (L1) is
      --  received (else STALL)
      LPMACK        : Boolean := False;
      --  unspecified
      Reserved_2_2  : HAL.Bit := 16#0#;
      --  Read-only. bRemoteWake value Device mode This bit contains the
      --  bRemoteWake value received with last ACKed LPM Token
      REMWAKE       : Boolean := False;
      --  Read-only. BESL value Device mode These bits contain the BESL value
      --  received with last ACKed LPM Token
      BESL          : USB_LPMCSR_BESL_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for USB_LPMCSR_Register use record
      LPMEN         at 0 range 0 .. 0;
      LPMACK        at 0 range 1 .. 1;
      Reserved_2_2  at 0 range 2 .. 2;
      REMWAKE       at 0 range 3 .. 3;
      BESL          at 0 range 4 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  Battery charging detector
   type USB_BCDR_Register is record
      --  Battery charging detector (BCD) enable Device mode This bit is set by
      --  the software to enable the BCD support within the USB Device. When
      --  enabled, the USB PHY is fully controlled by BCD and cannot be used
      --  for normal communication. Once the BCD discovery is finished, the BCD
      --  must be placed in OFF mode by clearing this bit to 0 in order to
      --  allow the normal USB operation.
      BCDEN          : Boolean := False;
      --  Data contact detection (DCD) mode enable Device mode This bit is set
      --  by the software to put the BCD into DCD mode. Only one detection mode
      --  (DCD, PD, SD or OFF) must be selected to work correctly.
      DCDEN          : Boolean := False;
      --  Primary detection (PD) mode enable Device mode This bit is set by the
      --  software to put the BCD into PD mode. Only one detection mode (DCD,
      --  PD, SD or OFF) must be selected to work correctly.
      PDEN           : Boolean := False;
      --  Secondary detection (SD) mode enable Device mode This bit is set by
      --  the software to put the BCD into SD mode. Only one detection mode
      --  (DCD, PD, SD or OFF) must be selected to work correctly.
      SDEN           : Boolean := False;
      --  Read-only. Data contact detection (DCD) status Device mode This bit
      --  gives the result of DCD.
      DCDET          : Boolean := False;
      --  Read-only. Primary detection (PD) status Device mode This bit gives
      --  the result of PD.
      PDET           : Boolean := False;
      --  Read-only. Secondary detection (SD) status Device mode This bit gives
      --  the result of SD.
      SDET           : Boolean := False;
      --  Read-only. DM pull-up detection status Device mode This bit is active
      --  only during PD and gives the result of comparison between DM voltage
      --  level and V<sub>LGC</sub> threshold. In normal situation, the DM
      --  level must be below this threshold. If it is above, it means that the
      --  DM is externally pulled high. This can be caused by connection to a
      --  PS2 port (which pulls-up both DP and DM lines) or to some proprietary
      --  charger not following the BCD specification.
      PS2DET         : Boolean := False;
      --  unspecified
      Reserved_8_14  : HAL.UInt7 := 16#0#;
      --  DP pull-up / DPDM pull-down Device mode This bit is set by software
      --  to enable the embedded pull-up on DP line. Clearing it to 0 can be
      --  used to signal disconnect to the host when needed by the user
      --  software. Host mode This bit is set by software to enable the
      --  embedded pull-down on DP and DM lines.
      DPPU_DPD       : Boolean := False;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for USB_BCDR_Register use record
      BCDEN          at 0 range 0 .. 0;
      DCDEN          at 0 range 1 .. 1;
      PDEN           at 0 range 2 .. 2;
      SDEN           at 0 range 3 .. 3;
      DCDET          at 0 range 4 .. 4;
      PDET           at 0 range 5 .. 5;
      SDET           at 0 range 6 .. 6;
      PS2DET         at 0 range 7 .. 7;
      Reserved_8_14  at 0 range 8 .. 14;
      DPPU_DPD       at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  USB address block description
   type USB_Peripheral is record
      --  USB endpoint/channel 0 register
      USB_CHEP0R : aliased USB_CHEP0R_Register;
      --  USB endpoint/channel 1 register
      USB_CHEP1R : aliased USB_CHEP1R_Register;
      --  USB endpoint/channel 2 register
      USB_CHEP2R : aliased USB_CHEP2R_Register;
      --  USB endpoint/channel 3 register
      USB_CHEP3R : aliased USB_CHEP3R_Register;
      --  USB endpoint/channel 4 register
      USB_CHEP4R : aliased USB_CHEP4R_Register;
      --  USB endpoint/channel 5 register
      USB_CHEP5R : aliased USB_CHEP5R_Register;
      --  USB endpoint/channel 6 register
      USB_CHEP6R : aliased USB_CHEP6R_Register;
      --  USB endpoint/channel 7 register
      USB_CHEP7R : aliased USB_CHEP7R_Register;
      --  USB control register
      USB_CNTR   : aliased USB_CNTR_Register;
      --  USB interrupt status register
      USB_ISTR   : aliased USB_ISTR_Register;
      --  USB frame number register
      USB_FNR    : aliased USB_FNR_Register;
      --  USB Device address
      USB_DADDR  : aliased USB_DADDR_Register;
      --  LPM control and status register
      USB_LPMCSR : aliased USB_LPMCSR_Register;
      --  Battery charging detector
      USB_BCDR   : aliased USB_BCDR_Register;
   end record
     with Volatile;

   for USB_Peripheral use record
      USB_CHEP0R at 16#0# range 0 .. 31;
      USB_CHEP1R at 16#4# range 0 .. 31;
      USB_CHEP2R at 16#8# range 0 .. 31;
      USB_CHEP3R at 16#C# range 0 .. 31;
      USB_CHEP4R at 16#10# range 0 .. 31;
      USB_CHEP5R at 16#14# range 0 .. 31;
      USB_CHEP6R at 16#18# range 0 .. 31;
      USB_CHEP7R at 16#1C# range 0 .. 31;
      USB_CNTR   at 16#40# range 0 .. 31;
      USB_ISTR   at 16#44# range 0 .. 31;
      USB_FNR    at 16#48# range 0 .. 31;
      USB_DADDR  at 16#4C# range 0 .. 31;
      USB_LPMCSR at 16#54# range 0 .. 31;
      USB_BCDR   at 16#58# range 0 .. 31;
   end record;

   --  USB address block description
   USB_Periph : aliased USB_Peripheral
     with Import, Address => USB_Base;

end STM32_SVD.USB;
