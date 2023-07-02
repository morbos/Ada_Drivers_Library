--  This spec has been automatically generated from STM32U585.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.UCPD is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype UCPD_CFGR1_HBITCLKDIV_Field is HAL.UInt6;
   subtype UCPD_CFGR1_IFRGAP_Field is HAL.UInt5;
   subtype UCPD_CFGR1_TRANSWIN_Field is HAL.UInt5;
   subtype UCPD_CFGR1_PSC_USBPDCLK_Field is HAL.UInt3;
   subtype UCPD_CFGR1_RXORDSETEN_Field is HAL.UInt9;

   --  UCPD configuration register 1
   type UCPD_CFGR1_Register is record
      --  Division ratio for producing half-bit clock The bitfield determines
      --  the division ratio (the bitfield value plus one) of a ucpd_clk
      --  divider producing half-bit clock (hbit_clk).
      HBITCLKDIV     : UCPD_CFGR1_HBITCLKDIV_Field := 16#0#;
      --  Division ratio for producing inter-frame gap timer clock The bitfield
      --  determines the division ratio (the bitfield value minus one) of a
      --  ucpd_clk divider producing inter-frame gap timer clock
      --  (tInterFrameGap). The division ratio 15 is to apply for Tx clock at
      --  the USB PD 2.0 specification nominal value. The division ratios below
      --  15 are to apply for Tx clock below nominal, and the division ratios
      --  above 15 for Tx clock above nominal.
      IFRGAP         : UCPD_CFGR1_IFRGAP_Field := 16#0#;
      --  Transition window duration The bitfield determines the division ratio
      --  (the bitfield value minus one) of a hbit_clk divider producing
      --  tTransitionWindow interval. Set a value that produces an interval of
      --  12 to 20 us, taking into account the ucpd_clk frequency and the
      --  HBITCLKDIV[5:0] bitfield setting.
      TRANSWIN       : UCPD_CFGR1_TRANSWIN_Field := 16#0#;
      --  unspecified
      Reserved_16_16 : HAL.Bit := 16#0#;
      --  Pre-scaler division ratio for generating ucpd_clk The bitfield
      --  determines the division ratio of a kernel clock pre-scaler producing
      --  UCPD peripheral clock (ucpd_clk). It is recommended to use the
      --  pre-scaler so as to set the ucpd_clk frequency in the range from 6 to
      --  9 MHz.
      PSC_USBPDCLK   : UCPD_CFGR1_PSC_USBPDCLK_Field := 16#0#;
      --  Receiver ordered set enable The bitfield determines the types of
      --  ordered sets that the receiver must detect. When set/cleared, each
      --  bit enables/disables a specific function: 0bxxxxxxxx1: SOP detect
      --  enabled 0bxxxxxxx1x: SOP' detect enabled 0bxxxxxx1xx: SOP'' detect
      --  enabled 0bxxxxx1xxx: Hard Reset detect enabled 0bxxxx1xxxx: Cable
      --  Detect reset enabled 0bxxx1xxxxx: SOP'_Debug enabled 0bxx1xxxxxx:
      --  SOP''_Debug enabled 0bx1xxxxxxx: SOP extension#1 enabled 0b1xxxxxxxx:
      --  SOP extension#2 enabled
      RXORDSETEN     : UCPD_CFGR1_RXORDSETEN_Field := 16#0#;
      --  Transmission DMA mode enable When set, the bit enables DMA mode for
      --  transmission.
      TXDMAEN        : Boolean := False;
      --  Reception DMA mode enable When set, the bit enables DMA mode for
      --  reception.
      RXDMAEN        : Boolean := False;
      --  UCPD peripheral enable General enable of the UCPD peripheral. Upon
      --  disabling, the peripheral instantly quits any ongoing activity and
      --  all control bits and bitfields default to their reset values. They
      --  must be set to their desired values each time the peripheral transits
      --  from disabled to enabled state.
      UCPDEN         : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for UCPD_CFGR1_Register use record
      HBITCLKDIV     at 0 range 0 .. 5;
      IFRGAP         at 0 range 6 .. 10;
      TRANSWIN       at 0 range 11 .. 15;
      Reserved_16_16 at 0 range 16 .. 16;
      PSC_USBPDCLK   at 0 range 17 .. 19;
      RXORDSETEN     at 0 range 20 .. 28;
      TXDMAEN        at 0 range 29 .. 29;
      RXDMAEN        at 0 range 30 .. 30;
      UCPDEN         at 0 range 31 .. 31;
   end record;

   --  UCPD configuration register 2
   type UCPD_CFGR2_Register is record
      --  BMC decoder Rx pre-filter enable The sampling clock is that of the
      --  receiver (that is, after pre-scaler).
      RXFILTDIS     : Boolean := False;
      --  BMC decoder Rx pre-filter sampling method Number of consistent
      --  consecutive samples before confirming a new value.
      RXFILT2N3     : Boolean := False;
      --  Force ClkReq clock request
      FORCECLK      : Boolean := False;
      --  Wakeup from Stop mode enable Setting the bit enables the
      --  UCPD_ASYNC_INT signal.
      WUPEN         : Boolean := False;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for UCPD_CFGR2_Register use record
      RXFILTDIS     at 0 range 0 .. 0;
      RXFILT2N3     at 0 range 1 .. 1;
      FORCECLK      at 0 range 2 .. 2;
      WUPEN         at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   subtype UCPD_CFGR3_TRIM1_NG_CCRPD_Field is HAL.UInt4;
   subtype UCPD_CFGR3_TRIM1_NG_CC3A0_Field is HAL.UInt4;
   subtype UCPD_CFGR3_TRIM2_NG_CCRPD_Field is HAL.UInt4;
   subtype UCPD_CFGR3_TRIM2_NG_CC3A0_Field is HAL.UInt4;

   --  UCPD configuration register 3
   type UCPD_CFGR3_Register is record
      --  SW trim value for RPD resistors on the CC1 line
      TRIM1_NG_CCRPD : UCPD_CFGR3_TRIM1_NG_CCRPD_Field := 16#0#;
      --  unspecified
      Reserved_4_8   : HAL.UInt5 := 16#0#;
      --  SW trim value for Iref on the CC1 line
      TRIM1_NG_CC3A0 : UCPD_CFGR3_TRIM1_NG_CC3A0_Field := 16#0#;
      --  unspecified
      Reserved_13_15 : HAL.UInt3 := 16#0#;
      --  SW trim value for RPD resistors on the CC2 line
      TRIM2_NG_CCRPD : UCPD_CFGR3_TRIM2_NG_CCRPD_Field := 16#0#;
      --  unspecified
      Reserved_20_24 : HAL.UInt5 := 16#0#;
      --  SW trim value for Iref on the CC2 line
      TRIM2_NG_CC3A0 : UCPD_CFGR3_TRIM2_NG_CC3A0_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for UCPD_CFGR3_Register use record
      TRIM1_NG_CCRPD at 0 range 0 .. 3;
      Reserved_4_8   at 0 range 4 .. 8;
      TRIM1_NG_CC3A0 at 0 range 9 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      TRIM2_NG_CCRPD at 0 range 16 .. 19;
      Reserved_20_24 at 0 range 20 .. 24;
      TRIM2_NG_CC3A0 at 0 range 25 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype UCPD_CR_TXMODE_Field is HAL.UInt2;
   subtype UCPD_CR_ANASUBMODE_Field is HAL.UInt2;
   subtype UCPD_CR_CCENABLE_Field is HAL.UInt2;

   --  UCPD control register
   type UCPD_CR_Register is record
      --  Type of Tx packet Writing the bitfield triggers the action as
      --  follows, depending on the value: Others: invalid From V1.1 of the USB
      --  PD specification, there is a counter defined for the duration of the
      --  BIST Carrier Mode 2. To quit this mode correctly (after the
      --  "tBISTContMode" delay), disable the peripheral (UCPDEN = 0).
      TXMODE         : UCPD_CR_TXMODE_Field := 16#0#;
      --  Command to send a Tx packet The bit is cleared by hardware as soon as
      --  the packet transmission begins or is discarded.
      TXSEND         : Boolean := False;
      --  Command to send a Tx Hard Reset The bit is cleared by hardware as
      --  soon as the message transmission begins or is discarded.
      TXHRST         : Boolean := False;
      --  Receiver mode Determines the mode of the receiver. When the bit is
      --  set, RXORDSET behaves normally, RXDR no longer receives bytes yet the
      --  CRC checking still proceeds as for a normal message.
      RXMODE         : Boolean := False;
      --  USB Power Delivery receiver enable Both CC1 and CC2 receivers are
      --  disabled when the bit is cleared. Only the CC receiver selected via
      --  the PHYCCSEL bit is enabled when the bit is set.
      PHYRXEN        : Boolean := False;
      --  CC1/CC2 line selector for USB Power Delivery signaling The selection
      --  depends on the cable orientation as discovered at attach.
      PHYCCSEL       : Boolean := False;
      --  Analog PHY sub-mode Refer to TYPEC_VSTATE_CCx for the effect of this
      --  bitfield.
      ANASUBMODE     : UCPD_CR_ANASUBMODE_Field := 16#0#;
      --  Analog PHY operating mode The use of CC1 and CC2 depends on CCENABLE.
      --  Refer to ANAMODE, ANASUBMODE and link with TYPEC_VSTATE_CCx for the
      --  effect of this bitfield in conjunction with ANASUBMODE[1:0].
      ANAMODE        : Boolean := False;
      --  CC line enable This bitfield enables CC1 and CC2 line analog PHYs
      --  (pull-ups and pull-downs) according to ANAMODE and ANASUBMODE[1:0]
      --  setting. A single line PHY can be enabled when, for example, the
      --  other line is driven by VCONN via an external VCONN switch. Enabling
      --  both PHYs is the normal usage for sink/source.
      CCENABLE       : UCPD_CR_CCENABLE_Field := 16#0#;
      --  unspecified
      Reserved_12_12 : HAL.Bit := 16#0#;
      --  VCONN switch enable for CC1
      CC1VCONNEN     : Boolean := False;
      --  VCONN switch enable for CC2
      CC2VCONNEN     : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  FRS event detection enable Setting the bit enables FRS Rx event
      --  (FRSEVT) detection on the CC line selected through the PHYCCSEL bit.
      --  0: Disable Clear the bit when the device is attached to an
      --  FRS-incapable source/sink.
      FRSRXEN        : Boolean := False;
      --  FRS Tx signaling enable. Setting the bit enables FRS Tx signaling.
      --  The bit is cleared by hardware after a delay respecting the USB Power
      --  Delivery specification Revision 3.0.
      FRSTX          : Boolean := False;
      --  Rdch condition drive The bit drives Rdch condition on the CC line
      --  selected through the PHYCCSEL bit (thus associated with VCONN), by
      --  remaining set during the source-only UnattachedWait.SRC state, to
      --  respect the Type-C state. Refer to "USB Type-C ECN for Source VCONN
      --  Discharge". The CCENABLE[1:0] bitfield must be set accordingly, too.
      RDCH           : Boolean := False;
      --  unspecified
      Reserved_19_19 : HAL.Bit := 16#0#;
      --  CC1 Type-C detector disable The bit disables the Type-C detector on
      --  the CC1 line. When enabled, the Type-C detector for CC1 is configured
      --  through ANAMODE and ANASUBMODE[1:0].
      CC1TCDIS       : Boolean := False;
      --  CC2 Type-C detector disable The bit disables the Type-C detector on
      --  the CC2 line. When enabled, the Type-C detector for CC2 is configured
      --  through ANAMODE and ANASUBMODE[1:0].
      CC2TCDIS       : Boolean := False;
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for UCPD_CR_Register use record
      TXMODE         at 0 range 0 .. 1;
      TXSEND         at 0 range 2 .. 2;
      TXHRST         at 0 range 3 .. 3;
      RXMODE         at 0 range 4 .. 4;
      PHYRXEN        at 0 range 5 .. 5;
      PHYCCSEL       at 0 range 6 .. 6;
      ANASUBMODE     at 0 range 7 .. 8;
      ANAMODE        at 0 range 9 .. 9;
      CCENABLE       at 0 range 10 .. 11;
      Reserved_12_12 at 0 range 12 .. 12;
      CC1VCONNEN     at 0 range 13 .. 13;
      CC2VCONNEN     at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      FRSRXEN        at 0 range 16 .. 16;
      FRSTX          at 0 range 17 .. 17;
      RDCH           at 0 range 18 .. 18;
      Reserved_19_19 at 0 range 19 .. 19;
      CC1TCDIS       at 0 range 20 .. 20;
      CC2TCDIS       at 0 range 21 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  UCPD interrupt mask register
   type UCPD_IMR_Register is record
      --  TXIS interrupt enable
      TXISIE         : Boolean := False;
      --  TXMSGDISC interrupt enable
      TXMSGDISCIE    : Boolean := False;
      --  TXMSGSENT interrupt enable
      TXMSGSENTIE    : Boolean := False;
      --  TXMSGABT interrupt enable
      TXMSGABTIE     : Boolean := False;
      --  HRSTDISC interrupt enable
      HRSTDISCIE     : Boolean := False;
      --  HRSTSENT interrupt enable
      HRSTSENTIE     : Boolean := False;
      --  TXUND interrupt enable
      TXUNDIE        : Boolean := False;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  RXNE interrupt enable
      RXNEIE         : Boolean := False;
      --  RXORDDET interrupt enable
      RXORDDETIE     : Boolean := False;
      --  RXHRSTDET interrupt enable
      RXHRSTDETIE    : Boolean := False;
      --  RXOVR interrupt enable
      RXOVRIE        : Boolean := False;
      --  RXMSGEND interrupt enable
      RXMSGENDIE     : Boolean := False;
      --  unspecified
      Reserved_13_13 : HAL.Bit := 16#0#;
      --  TYPECEVT1 interrupt enable
      TYPECEVT1IE    : Boolean := False;
      --  TYPECEVT2 interrupt enable
      TYPECEVT2IE    : Boolean := False;
      --  unspecified
      Reserved_16_19 : HAL.UInt4 := 16#0#;
      --  Read-only. FRSEVT interrupt enable
      FRSEVTIE       : Boolean := False;
      --  unspecified
      Reserved_21_31 : HAL.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for UCPD_IMR_Register use record
      TXISIE         at 0 range 0 .. 0;
      TXMSGDISCIE    at 0 range 1 .. 1;
      TXMSGSENTIE    at 0 range 2 .. 2;
      TXMSGABTIE     at 0 range 3 .. 3;
      HRSTDISCIE     at 0 range 4 .. 4;
      HRSTSENTIE     at 0 range 5 .. 5;
      TXUNDIE        at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      RXNEIE         at 0 range 8 .. 8;
      RXORDDETIE     at 0 range 9 .. 9;
      RXHRSTDETIE    at 0 range 10 .. 10;
      RXOVRIE        at 0 range 11 .. 11;
      RXMSGENDIE     at 0 range 12 .. 12;
      Reserved_13_13 at 0 range 13 .. 13;
      TYPECEVT1IE    at 0 range 14 .. 14;
      TYPECEVT2IE    at 0 range 15 .. 15;
      Reserved_16_19 at 0 range 16 .. 19;
      FRSEVTIE       at 0 range 20 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   --  UCPD_SR_TYPECEVT array
   type UCPD_SR_TYPECEVT_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for UCPD_SR_TYPECEVT
   type UCPD_SR_TYPECEVT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TYPECEVT as a value
            Val : HAL.UInt2;
         when True =>
            --  TYPECEVT as an array
            Arr : UCPD_SR_TYPECEVT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for UCPD_SR_TYPECEVT_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  UCPD_SR_TYPEC_VSTATE_CC array element
   subtype UCPD_SR_TYPEC_VSTATE_CC_Element is HAL.UInt2;

   --  UCPD_SR_TYPEC_VSTATE_CC array
   type UCPD_SR_TYPEC_VSTATE_CC_Field_Array is array (1 .. 2)
     of UCPD_SR_TYPEC_VSTATE_CC_Element
     with Component_Size => 2, Size => 4;

   --  Type definition for UCPD_SR_TYPEC_VSTATE_CC
   type UCPD_SR_TYPEC_VSTATE_CC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TYPEC_VSTATE_CC as a value
            Val : HAL.UInt4;
         when True =>
            --  TYPEC_VSTATE_CC as an array
            Arr : UCPD_SR_TYPEC_VSTATE_CC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for UCPD_SR_TYPEC_VSTATE_CC_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  UCPD status register
   type UCPD_SR_Register is record
      --  Read-only. Transmit interrupt status The flag indicates that the
      --  UCPD_TXDR register is empty and new data write is required (as the
      --  amount of data sent has not reached the payload size defined in the
      --  TXPAYSZ bitfield). The flag is cleared with the data write into the
      --  UCPD_TXDR register.
      TXIS            : Boolean;
      --  Read-only. Message transmission discarded The flag indicates that a
      --  message transmission was dropped. The flag is cleared by setting the
      --  TXMSGDISCCF bit. Transmission of a message can be dropped if there is
      --  a concurrent receive in progress or at excessive noise on the line.
      --  After a Tx message is discarded, the flag is only raised when the CC
      --  line becomes idle.
      TXMSGDISC       : Boolean;
      --  Read-only. Message transmission completed The flag indicates the
      --  completion of packet transmission. It is cleared by setting the
      --  TXMSGSENTCF bit. In the event of a message transmission interrupted
      --  by a Hard Reset, the flag is not raised.
      TXMSGSENT       : Boolean;
      --  Read-only. Transmit message abort The flag indicates that a Tx
      --  message is aborted due to a subsequent Hard Reset message send
      --  request taking priority during transmit. It is cleared by setting the
      --  TXMSGABTCF bit.
      TXMSGABT        : Boolean;
      --  Read-only. Hard Reset discarded The flag indicates that the Hard
      --  Reset message is discarded. The flag is cleared by setting the
      --  HRSTDISCCF bit.
      HRSTDISC        : Boolean;
      --  Read-only. Hard Reset message sent The flag indicates that the Hard
      --  Reset message is sent. The flag is cleared by setting the HRSTSENTCF
      --  bit.
      HRSTSENT        : Boolean;
      --  Read-only. Tx data underrun detection The flag indicates that the Tx
      --  data register (UCPD_TXDR) was not written in time for a transmit
      --  message to execute normally. It is cleared by setting the TXUNDCF
      --  bit.
      TXUND           : Boolean;
      --  unspecified
      Reserved_7_7    : HAL.Bit;
      --  Read-only. Receive data register not empty detection The flag
      --  indicates that the UCPD_RXDR register is not empty. It is
      --  automatically cleared upon reading UCPD_RXDR.
      RXNE            : Boolean;
      --  Read-only. Rx ordered set (4 K-codes) detection The flag indicates
      --  the detection of an ordered set. The relevant information is stored
      --  in the RXORDSET[2:0] bitfield of the UCPD_RX_ORDSET register. It is
      --  cleared by setting the RXORDDETCF bit.
      RXORDDET        : Boolean;
      --  Read-only. Rx Hard Reset receipt detection The flag indicates the
      --  receipt of valid Hard Reset message. It is cleared by setting the
      --  RXHRSTDETCF bit.
      RXHRSTDET       : Boolean;
      --  Read-only. Rx data overflow detection The flag indicates Rx data
      --  buffer overflow. It is cleared by setting the RXOVRCF bit. The buffer
      --  overflow can occur if the received data are not read fast enough.
      RXOVR           : Boolean;
      --  Read-only. Rx message received The flag indicates whether a message
      --  (except Hard Reset message) has been received, regardless the CRC
      --  value. The flag is cleared by setting the RXMSGENDCF bit. The RXERR
      --  flag set when the RXMSGEND flag goes high indicates errors in the
      --  last-received message.
      RXMSGEND        : Boolean;
      --  Read-only. Receive message error The flag indicates errors of the
      --  last Rx message declared (via RXMSGEND), such as incorrect CRC or
      --  truncated message (a line becoming static before EOP is met). It is
      --  asserted whenever the RXMSGEND flag is set.
      RXERR           : Boolean;
      --  Read-only. Type-C voltage level event on CC1 line The flag indicates
      --  a change of the TYPEC_VSTATE_CC1[1:0] bitfield value, which
      --  corresponds to a new Type-C event. It is cleared by setting the
      --  TYPECEVT2CF bit.
      TYPECEVT        : UCPD_SR_TYPECEVT_Field;
      --  Read-only. The status bitfield indicates the voltage level on the CC1
      --  line in its steady state. The voltage variation on the CC1 line
      --  during USB PD messages due to the BMC PHY modulation does not impact
      --  the bitfield value.
      TYPEC_VSTATE_CC : UCPD_SR_TYPEC_VSTATE_CC_Field;
      --  Read-only. FRS detection event The flag is cleared by setting the
      --  FRSEVTCF bit.
      FRSEVT          : Boolean;
      --  unspecified
      Reserved_21_31  : HAL.UInt11;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for UCPD_SR_Register use record
      TXIS            at 0 range 0 .. 0;
      TXMSGDISC       at 0 range 1 .. 1;
      TXMSGSENT       at 0 range 2 .. 2;
      TXMSGABT        at 0 range 3 .. 3;
      HRSTDISC        at 0 range 4 .. 4;
      HRSTSENT        at 0 range 5 .. 5;
      TXUND           at 0 range 6 .. 6;
      Reserved_7_7    at 0 range 7 .. 7;
      RXNE            at 0 range 8 .. 8;
      RXORDDET        at 0 range 9 .. 9;
      RXHRSTDET       at 0 range 10 .. 10;
      RXOVR           at 0 range 11 .. 11;
      RXMSGEND        at 0 range 12 .. 12;
      RXERR           at 0 range 13 .. 13;
      TYPECEVT        at 0 range 14 .. 15;
      TYPEC_VSTATE_CC at 0 range 16 .. 19;
      FRSEVT          at 0 range 20 .. 20;
      Reserved_21_31  at 0 range 21 .. 31;
   end record;

   --  UCPD interrupt clear register
   type UCPD_ICR_Register is record
      --  unspecified
      Reserved_0_0   : HAL.Bit := 16#0#;
      --  Write-only. Tx message discard flag (TXMSGDISC) clear Setting the bit
      --  clears the TXMSGDISC flag in the UCPD_SR register.
      TXMSGDISCCF    : Boolean := False;
      --  Write-only. Tx message send flag (TXMSGSENT) clear Setting the bit
      --  clears the TXMSGSENT flag in the UCPD_SR register.
      TXMSGSENTCF    : Boolean := False;
      --  Write-only. Tx message abort flag (TXMSGABT) clear Setting the bit
      --  clears the TXMSGABT flag in the UCPD_SR register.
      TXMSGABTCF     : Boolean := False;
      --  Write-only. Hard reset discard flag (HRSTDISC) clear Setting the bit
      --  clears the HRSTDISC flag in the UCPD_SR register.
      HRSTDISCCF     : Boolean := False;
      --  Write-only. Hard reset send flag (HRSTSENT) clear Setting the bit
      --  clears the HRSTSENT flag in the UCPD_SR register.
      HRSTSENTCF     : Boolean := False;
      --  Write-only. Tx underflow flag (TXUND) clear Setting the bit clears
      --  the TXUND flag in the UCPD_SR register.
      TXUNDCF        : Boolean := False;
      --  unspecified
      Reserved_7_8   : HAL.UInt2 := 16#0#;
      --  Write-only. Rx ordered set detect flag (RXORDDET) clear Setting the
      --  bit clears the RXORDDET flag in the UCPD_SR register.
      RXORDDETCF     : Boolean := False;
      --  Write-only. Rx Hard Reset detect flag (RXHRSTDET) clear Setting the
      --  bit clears the RXHRSTDET flag in the UCPD_SR register.
      RXHRSTDETCF    : Boolean := False;
      --  Write-only. Rx overflow flag (RXOVR) clear Setting the bit clears the
      --  RXOVR flag in the UCPD_SR register.
      RXOVRCF        : Boolean := False;
      --  Write-only. Rx message received flag (RXMSGEND) clear Setting the bit
      --  clears the RXMSGEND flag in the UCPD_SR register.
      RXMSGENDCF     : Boolean := False;
      --  unspecified
      Reserved_13_13 : HAL.Bit := 16#0#;
      --  Write-only. Type-C CC1 event flag (TYPECEVT1) clear Setting the bit
      --  clears the TYPECEVT1 flag in the UCPD_SR register
      TYPECEVT1CF    : Boolean := False;
      --  Write-only. Type-C CC2 line event flag (TYPECEVT2) clear Setting the
      --  bit clears the TYPECEVT2 flag in the UCPD_SR register
      TYPECEVT2CF    : Boolean := False;
      --  unspecified
      Reserved_16_19 : HAL.UInt4 := 16#0#;
      --  Write-only. FRS event flag (FRSEVT) clear Setting the bit clears the
      --  FRSEVT flag in the UCPD_SR register.
      FRSEVTCF       : Boolean := False;
      --  unspecified
      Reserved_21_31 : HAL.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for UCPD_ICR_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      TXMSGDISCCF    at 0 range 1 .. 1;
      TXMSGSENTCF    at 0 range 2 .. 2;
      TXMSGABTCF     at 0 range 3 .. 3;
      HRSTDISCCF     at 0 range 4 .. 4;
      HRSTSENTCF     at 0 range 5 .. 5;
      TXUNDCF        at 0 range 6 .. 6;
      Reserved_7_8   at 0 range 7 .. 8;
      RXORDDETCF     at 0 range 9 .. 9;
      RXHRSTDETCF    at 0 range 10 .. 10;
      RXOVRCF        at 0 range 11 .. 11;
      RXMSGENDCF     at 0 range 12 .. 12;
      Reserved_13_13 at 0 range 13 .. 13;
      TYPECEVT1CF    at 0 range 14 .. 14;
      TYPECEVT2CF    at 0 range 15 .. 15;
      Reserved_16_19 at 0 range 16 .. 19;
      FRSEVTCF       at 0 range 20 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   subtype UCPD_TX_ORDSETR_TXORDSET_Field is HAL.UInt20;

   --  UCPD Tx ordered set type register
   type UCPD_TX_ORDSETR_Register is record
      --  Ordered set to transmit The bitfield determines a full 20-bit
      --  sequence to transmit, consisting of four K-codes, each of five bits,
      --  defining the packet to transmit. The bit 0 (bit 0 of K-code1) is the
      --  first, the bit 19 (bit 4 of K‑code4) the last.
      TXORDSET       : UCPD_TX_ORDSETR_TXORDSET_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for UCPD_TX_ORDSETR_Register use record
      TXORDSET       at 0 range 0 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   subtype UCPD_TX_PAYSZR_TXPAYSZ_Field is HAL.UInt10;

   --  UCPD Tx payload size register
   type UCPD_TX_PAYSZR_Register is record
      --  Payload size yet to transmit The bitfield is modified by software and
      --  by hardware. It contains the number of bytes of a payload (including
      --  header but excluding CRC) yet to transmit: each time a data byte is
      --  written into the UCPD_TXDR register, the bitfield value decrements
      --  and the TXIS bit is set, except when the bitfield value reaches zero.
      --  The enumerated values are standard payload sizes before the start of
      --  transmission.
      TXPAYSZ        : UCPD_TX_PAYSZR_TXPAYSZ_Field := 16#0#;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for UCPD_TX_PAYSZR_Register use record
      TXPAYSZ        at 0 range 0 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype UCPD_TXDR_TXDATA_Field is HAL.UInt8;

   --  UCPD Tx data register
   type UCPD_TXDR_Register is record
      --  Data byte to transmit
      TXDATA        : UCPD_TXDR_TXDATA_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for UCPD_TXDR_Register use record
      TXDATA        at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype UCPD_RX_ORDSETR_RXORDSET_Field is HAL.UInt3;
   subtype UCPD_RX_ORDSETR_RXSOPKINVALID_Field is HAL.UInt3;

   type UCPD_RX_ORDSETR_Register is record
      --  Read-only. Rx ordered set code detected
      RXORDSET      : UCPD_RX_ORDSETR_RXORDSET_Field;
      --  Read-only. The bit indicates the number of correct K‑codes. For
      --  debug purposes only.
      RXSOP3OF4     : Boolean;
      --  Read-only. The bitfield is for debug purposes only. Others: Invalid
      RXSOPKINVALID : UCPD_RX_ORDSETR_RXSOPKINVALID_Field;
      --  unspecified
      Reserved_7_31 : HAL.UInt25;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for UCPD_RX_ORDSETR_Register use record
      RXORDSET      at 0 range 0 .. 2;
      RXSOP3OF4     at 0 range 3 .. 3;
      RXSOPKINVALID at 0 range 4 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   subtype UCPD_RX_PAYSZR_RXPAYSZ_Field is HAL.UInt10;

   type UCPD_RX_PAYSZR_Register is record
      --  Read-only. Rx payload size received This bitfield contains the number
      --  of bytes of a payload (including header but excluding CRC) received:
      --  each time a new data byte is received in the UCPD_RXDR register, the
      --  bitfield value increments and the RXMSGEND flag is set (and an
      --  interrupt generated if enabled). The bitfield may return a spurious
      --  value when a byte reception is ongoing (the RXMSGEND flag is low).
      RXPAYSZ        : UCPD_RX_PAYSZR_RXPAYSZ_Field;
      --  unspecified
      Reserved_10_31 : HAL.UInt22;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for UCPD_RX_PAYSZR_Register use record
      RXPAYSZ        at 0 range 0 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype UCPD_RXDR_RXDATA_Field is HAL.UInt8;

   type UCPD_RXDR_Register is record
      --  Read-only. Data byte received
      RXDATA        : UCPD_RXDR_RXDATA_Field;
      --  unspecified
      Reserved_8_31 : HAL.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for UCPD_RXDR_Register use record
      RXDATA        at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype UCPD_RX_ORDEXTR1_RXSOPX1_Field is HAL.UInt20;

   --  UCPD Rx ordered set extension register 1 	
   type UCPD_RX_ORDEXTR1_Register is record
      --  Ordered set 1 received The bitfield contains a full 20-bit sequence
      --  received, consisting of four K‑codes, each of five bits. The bit 0
      --  (bit 0 of K‑code1) is receive first, the bit 19 (bit 4 of
      --  K‑code4) last.
      RXSOPX1        : UCPD_RX_ORDEXTR1_RXSOPX1_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for UCPD_RX_ORDEXTR1_Register use record
      RXSOPX1        at 0 range 0 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   subtype UCPD_RX_ORDEXTR2_RXSOPX2_Field is HAL.UInt20;

   --  UCPD Rx ordered set extension register 2 	
   type UCPD_RX_ORDEXTR2_Register is record
      --  Ordered set 2 received The bitfield contains a full 20-bit sequence
      --  received, consisting of four K‑codes, each of five bits. The bit 0
      --  (bit 0 of K‑code1) is receive first, the bit 19 (bit 4 of
      --  K‑code4) last.
      RXSOPX2        : UCPD_RX_ORDEXTR2_RXSOPX2_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for UCPD_RX_ORDEXTR2_Register use record
      RXSOPX2        at 0 range 0 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  USB Power Delivery interface
   type UCPD_Peripheral is record
      --  UCPD configuration register 1
      UCPD_CFGR1       : aliased UCPD_CFGR1_Register;
      --  UCPD configuration register 2
      UCPD_CFGR2       : aliased UCPD_CFGR2_Register;
      --  UCPD configuration register 3
      UCPD_CFGR3       : aliased UCPD_CFGR3_Register;
      --  UCPD control register
      UCPD_CR          : aliased UCPD_CR_Register;
      --  UCPD interrupt mask register
      UCPD_IMR         : aliased UCPD_IMR_Register;
      --  UCPD status register
      UCPD_SR          : aliased UCPD_SR_Register;
      --  UCPD interrupt clear register
      UCPD_ICR         : aliased UCPD_ICR_Register;
      --  UCPD Tx ordered set type register
      UCPD_TX_ORDSETR  : aliased UCPD_TX_ORDSETR_Register;
      --  UCPD Tx payload size register
      UCPD_TX_PAYSZR   : aliased UCPD_TX_PAYSZR_Register;
      --  UCPD Tx data register
      UCPD_TXDR        : aliased UCPD_TXDR_Register;
      UCPD_RX_ORDSETR  : aliased UCPD_RX_ORDSETR_Register;
      UCPD_RX_PAYSZR   : aliased UCPD_RX_PAYSZR_Register;
      UCPD_RXDR        : aliased UCPD_RXDR_Register;
      --  UCPD Rx ordered set extension register 1 	
      UCPD_RX_ORDEXTR1 : aliased UCPD_RX_ORDEXTR1_Register;
      --  UCPD Rx ordered set extension register 2 	
      UCPD_RX_ORDEXTR2 : aliased UCPD_RX_ORDEXTR2_Register;
   end record
     with Volatile;

   for UCPD_Peripheral use record
      UCPD_CFGR1       at 16#0# range 0 .. 31;
      UCPD_CFGR2       at 16#4# range 0 .. 31;
      UCPD_CFGR3       at 16#8# range 0 .. 31;
      UCPD_CR          at 16#C# range 0 .. 31;
      UCPD_IMR         at 16#10# range 0 .. 31;
      UCPD_SR          at 16#14# range 0 .. 31;
      UCPD_ICR         at 16#18# range 0 .. 31;
      UCPD_TX_ORDSETR  at 16#1C# range 0 .. 31;
      UCPD_TX_PAYSZR   at 16#20# range 0 .. 31;
      UCPD_TXDR        at 16#24# range 0 .. 31;
      UCPD_RX_ORDSETR  at 16#28# range 0 .. 31;
      UCPD_RX_PAYSZR   at 16#2C# range 0 .. 31;
      UCPD_RXDR        at 16#30# range 0 .. 31;
      UCPD_RX_ORDEXTR1 at 16#34# range 0 .. 31;
      UCPD_RX_ORDEXTR2 at 16#38# range 0 .. 31;
   end record;

   --  USB Power Delivery interface
   SEC_UCPD1_Periph : aliased UCPD_Peripheral
     with Import, Address => SEC_UCPD1_Base;

   --  USB Power Delivery interface
   UCPD1_Periph : aliased UCPD_Peripheral
     with Import, Address => UCPD1_Base;

end STM32_SVD.UCPD;
