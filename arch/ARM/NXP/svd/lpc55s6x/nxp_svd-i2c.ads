--  Copyright 2016-2019 NXP
--  All rights reserved.SPDX-License-Identifier: BSD-3-Clause


--  This spec has been automatically generated from LPC55S6x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NXP_SVD.I2C is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Master Enable. When disabled, configurations settings for the Master
   --  function are not changed, but the Master function is internally reset.
   type CFG_MSTEN_Field is
     (
      --  Disabled. The I2C Master function is disabled.
      Disabled,
      --  Enabled. The I2C Master function is enabled.
      Enabled)
     with Size => 1;
   for CFG_MSTEN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Slave Enable. When disabled, configurations settings for the Slave
   --  function are not changed, but the Slave function is internally reset.
   type CFG_SLVEN_Field is
     (
      --  Disabled. The I2C slave function is disabled.
      Disabled,
      --  Enabled. The I2C slave function is enabled.
      Enabled)
     with Size => 1;
   for CFG_SLVEN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Monitor Enable. When disabled, configurations settings for the Monitor
   --  function are not changed, but the Monitor function is internally reset.
   type CFG_MONEN_Field is
     (
      --  Disabled. The I2C Monitor function is disabled.
      Disabled,
      --  Enabled. The I2C Monitor function is enabled.
      Enabled)
     with Size => 1;
   for CFG_MONEN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  I2C bus Time-out Enable. When disabled, the time-out function is
   --  internally reset.
   type CFG_TIMEOUTEN_Field is
     (
      --  Disabled. Time-out function is disabled.
      Disabled,
      --  Enabled. Time-out function is enabled. Both types of time-out flags
      --  will be generated and will cause interrupts if they are enabled.
      --  Typically, only one time-out will be used in a system.
      Enabled)
     with Size => 1;
   for CFG_TIMEOUTEN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Monitor function Clock Stretching.
   type CFG_MONCLKSTR_Field is
     (
      --  Disabled. The Monitor function will not perform clock stretching.
      --  Software or DMA may not always be able to read data provided by the
      --  Monitor function before it is overwritten. This mode may be used when
      --  non-invasive monitoring is critical.
      Disabled,
      --  Enabled. The Monitor function will perform clock stretching in order
      --  to ensure that software or DMA can read all incoming data supplied by
      --  the Monitor function.
      Enabled)
     with Size => 1;
   for CFG_MONCLKSTR_Field use
     (Disabled => 0,
      Enabled => 1);

   --  High-speed mode Capable enable. Since High Speed mode alters the way I2C
   --  pins drive and filter, as well as the timing for certain I2C signalling,
   --  enabling High-speed mode applies to all functions: Master, Slave, and
   --  Monitor.
   type CFG_HSCAPABLE_Field is
     (
      --  Fast-mode plus. The I 2C interface will support Standard-mode,
      --  Fast-mode, and Fast-mode Plus, to the extent that the pin electronics
      --  support these modes. Any changes that need to be made to the pin
      --  controls, such as changing the drive strength or filtering, must be
      --  made by software via the IOCON register associated with each I2C pin,
      Fast_Mode_Plus,
      --  High-speed. In addition to Standard-mode, Fast-mode, and Fast-mode
      --  Plus, the I 2C interface will support High-speed mode to the extent
      --  that the pin electronics support these modes. See Section 25.7.2.2
      --  for more information.
      High_Speed)
     with Size => 1;
   for CFG_HSCAPABLE_Field use
     (Fast_Mode_Plus => 0,
      High_Speed => 1);

   --  Configuration for shared functions.
   type CFG_Register is record
      --  Master Enable. When disabled, configurations settings for the Master
      --  function are not changed, but the Master function is internally
      --  reset.
      MSTEN         : CFG_MSTEN_Field := NXP_SVD.I2C.Disabled;
      --  Slave Enable. When disabled, configurations settings for the Slave
      --  function are not changed, but the Slave function is internally reset.
      SLVEN         : CFG_SLVEN_Field := NXP_SVD.I2C.Disabled;
      --  Monitor Enable. When disabled, configurations settings for the
      --  Monitor function are not changed, but the Monitor function is
      --  internally reset.
      MONEN         : CFG_MONEN_Field := NXP_SVD.I2C.Disabled;
      --  I2C bus Time-out Enable. When disabled, the time-out function is
      --  internally reset.
      TIMEOUTEN     : CFG_TIMEOUTEN_Field := NXP_SVD.I2C.Disabled;
      --  Monitor function Clock Stretching.
      MONCLKSTR     : CFG_MONCLKSTR_Field := NXP_SVD.I2C.Disabled;
      --  High-speed mode Capable enable. Since High Speed mode alters the way
      --  I2C pins drive and filter, as well as the timing for certain I2C
      --  signalling, enabling High-speed mode applies to all functions:
      --  Master, Slave, and Monitor.
      HSCAPABLE     : CFG_HSCAPABLE_Field := NXP_SVD.I2C.Fast_Mode_Plus;
      --  unspecified
      Reserved_6_31 : HAL.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CFG_Register use record
      MSTEN         at 0 range 0 .. 0;
      SLVEN         at 0 range 1 .. 1;
      MONEN         at 0 range 2 .. 2;
      TIMEOUTEN     at 0 range 3 .. 3;
      MONCLKSTR     at 0 range 4 .. 4;
      HSCAPABLE     at 0 range 5 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   --  Master Pending. Indicates that the Master is waiting to continue
   --  communication on the I2C-bus (pending) or is idle. When the master is
   --  pending, the MSTSTATE bits indicate what type of software service if any
   --  the master expects. This flag will cause an interrupt when set if,
   --  enabled via the INTENSET register. The MSTPENDING flag is not set when
   --  the DMA is handling an event (if the MSTDMA bit in the MSTCTL register
   --  is set). If the master is in the idle state, and no communication is
   --  needed, mask this interrupt.
   type STAT_MSTPENDING_Field is
     (
      --  In progress. Communication is in progress and the Master function is
      --  busy and cannot currently accept a command.
      In_Progress,
      --  Pending. The Master function needs software service or is in the idle
      --  state. If the master is not in the idle state, it is waiting to
      --  receive or transmit data or the NACK bit.
      Pending)
     with Size => 1;
   for STAT_MSTPENDING_Field use
     (In_Progress => 0,
      Pending => 1);

   --  Master State code. The master state code reflects the master state when
   --  the MSTPENDING bit is set, that is the master is pending or in the idle
   --  state. Each value of this field indicates a specific required service
   --  for the Master function. All other values are reserved. See Table 400
   --  for details of state values and appropriate responses.
   type STAT_MSTSTATE_Field is
     (
      --  Idle. The Master function is available to be used for a new
      --  transaction.
      Idle,
      --  Receive ready. Received data available (Master Receiver mode).
      --  Address plus Read was previously sent and Acknowledged by slave.
      Receive_Ready,
      --  Transmit ready. Data can be transmitted (Master Transmitter mode).
      --  Address plus Write was previously sent and Acknowledged by slave.
      Transmit_Ready,
      --  NACK Address. Slave NACKed address.
      Nack_Address,
      --  NACK Data. Slave NACKed transmitted data.
      Nack_Data)
     with Size => 3;
   for STAT_MSTSTATE_Field use
     (Idle => 0,
      Receive_Ready => 1,
      Transmit_Ready => 2,
      Nack_Address => 3,
      Nack_Data => 4);

   --  Master Arbitration Loss flag. This flag can be cleared by software
   --  writing a 1 to this bit. It is also cleared automatically a 1 is written
   --  to MSTCONTINUE.
   type STAT_MSTARBLOSS_Field is
     (
      --  No Arbitration Loss has occurred.
      No_Loss,
      --  Arbitration loss. The Master function has experienced an Arbitration
      --  Loss. At this point, the Master function has already stopped driving
      --  the bus and gone to an idle state. Software can respond by doing
      --  nothing, or by sending a Start in order to attempt to gain control of
      --  the bus when it next becomes idle.
      Arbitration_Loss)
     with Size => 1;
   for STAT_MSTARBLOSS_Field use
     (No_Loss => 0,
      Arbitration_Loss => 1);

   --  Master Start/Stop Error flag. This flag can be cleared by software
   --  writing a 1 to this bit. It is also cleared automatically a 1 is written
   --  to MSTCONTINUE.
   type STAT_MSTSTSTPERR_Field is
     (
      --  No Start/Stop Error has occurred.
      No_Error,
      --  The Master function has experienced a Start/Stop Error. A Start or
      --  Stop was detected at a time when it is not allowed by the I2C
      --  specification. The Master interface has stopped driving the bus and
      --  gone to an idle state, no action is required. A request for a Start
      --  could be made, or software could attempt to insure that the bus has
      --  not stalled.
      Error)
     with Size => 1;
   for STAT_MSTSTSTPERR_Field use
     (No_Error => 0,
      Error => 1);

   --  Slave Pending. Indicates that the Slave function is waiting to continue
   --  communication on the I2C-bus and needs software service. This flag will
   --  cause an interrupt when set if enabled via INTENSET. The SLVPENDING flag
   --  is not set when the DMA is handling an event (if the SLVDMA bit in the
   --  SLVCTL register is set). The SLVPENDING flag is read-only and is
   --  automatically cleared when a 1 is written to the SLVCONTINUE bit in the
   --  SLVCTL register. The point in time when SlvPending is set depends on
   --  whether the I2C interface is in HSCAPABLE mode. See Section 25.7.2.2.2.
   --  When the I2C interface is configured to be HSCAPABLE, HS master codes
   --  are detected automatically. Due to the requirements of the HS I2C
   --  specification, slave addresses must also be detected automatically,
   --  since the address must be acknowledged before the clock can be
   --  stretched.
   type STAT_SLVPENDING_Field is
     (
      --  In progress. The Slave function does not currently need service.
      In_Progress,
      --  Pending. The Slave function needs service. Information on what is
      --  needed can be found in the adjacent SLVSTATE field.
      Pending)
     with Size => 1;
   for STAT_SLVPENDING_Field use
     (In_Progress => 0,
      Pending => 1);

   --  Slave State code. Each value of this field indicates a specific required
   --  service for the Slave function. All other values are reserved. See Table
   --  401 for state values and actions. note that the occurrence of some
   --  states and how they are handled are affected by DMA mode and Automatic
   --  Operation modes.
   type STAT_SLVSTATE_Field is
     (
      --  Slave address. Address plus R/W received. At least one of the four
      --  slave addresses has been matched by hardware.
      Slave_Address,
      --  Slave receive. Received data is available (Slave Receiver mode).
      Slave_Receive,
      --  Slave transmit. Data can be transmitted (Slave Transmitter mode).
      Slave_Transmit)
     with Size => 2;
   for STAT_SLVSTATE_Field use
     (Slave_Address => 0,
      Slave_Receive => 1,
      Slave_Transmit => 2);

   --  Slave Not Stretching. Indicates when the slave function is stretching
   --  the I2C clock. This is needed in order to gracefully invoke Deep Sleep
   --  or Power-down modes during slave operation. This read-only flag reflects
   --  the slave function status in real time.
   type STAT_SLVNOTSTR_Field is
     (
      --  Stretching. The slave function is currently stretching the I2C bus
      --  clock. Deep-Sleep or Power-down mode cannot be entered at this time.
      Stretching,
      --  Not stretching. The slave function is not currently stretching the I
      --  2C bus clock. Deep-sleep or Power-down mode could be entered at this
      --  time.
      Not_Stretching)
     with Size => 1;
   for STAT_SLVNOTSTR_Field use
     (Stretching => 0,
      Not_Stretching => 1);

   --  Slave address match Index. This field is valid when the I2C slave
   --  function has been selected by receiving an address that matches one of
   --  the slave addresses defined by any enabled slave address registers, and
   --  provides an identification of the address that was matched. It is
   --  possible that more than one address could be matched, but only one match
   --  can be reported here.
   type STAT_SLVIDX_Field is
     (
      --  Address 0. Slave address 0 was matched.
      Address0,
      --  Address 1. Slave address 1 was matched.
      Address1,
      --  Address 2. Slave address 2 was matched.
      Address2,
      --  Address 3. Slave address 3 was matched.
      Address3)
     with Size => 2;
   for STAT_SLVIDX_Field use
     (Address0 => 0,
      Address1 => 1,
      Address2 => 2,
      Address3 => 3);

   --  Slave selected flag. SLVSEL is set after an address match when software
   --  tells the Slave function to acknowledge the address, or when the address
   --  has been automatically acknowledged. It is cleared when another address
   --  cycle presents an address that does not match an enabled address on the
   --  Slave function, when slave software decides to NACK a matched address,
   --  when there is a Stop detected on the bus, when the master NACKs slave
   --  data, and in some combinations of Automatic Operation. SLVSEL is not
   --  cleared if software NACKs data.
   type STAT_SLVSEL_Field is
     (
      --  Not selected. The Slave function is not currently selected.
      Not_Selected,
      --  Selected. The Slave function is currently selected.
      Selected)
     with Size => 1;
   for STAT_SLVSEL_Field use
     (Not_Selected => 0,
      Selected => 1);

   --  Slave Deselected flag. This flag will cause an interrupt when set if
   --  enabled via INTENSET. This flag can be cleared by writing a 1 to this
   --  bit.
   type STAT_SLVDESEL_Field is
     (
      --  Not deselected. The Slave function has not become deselected. This
      --  does not mean that it is currently selected. That information can be
      --  found in the SLVSEL flag.
      Not_Deselected,
      --  Deselected. The Slave function has become deselected. This is
      --  specifically caused by the SLVSEL flag changing from 1 to 0. See the
      --  description of SLVSEL for details on when that event occurs.
      Deselected)
     with Size => 1;
   for STAT_SLVDESEL_Field use
     (Not_Deselected => 0,
      Deselected => 1);

   --  Monitor Ready. This flag is cleared when the MONRXDAT register is read.
   type STAT_MONRDY_Field is
     (
      --  No data. The Monitor function does not currently have data available.
      No_Data,
      --  Data waiting. The Monitor function has data waiting to be read.
      Data_Waiting)
     with Size => 1;
   for STAT_MONRDY_Field use
     (No_Data => 0,
      Data_Waiting => 1);

   --  Monitor Overflow flag.
   type STAT_MONOV_Field is
     (
      --  No overrun. Monitor data has not overrun.
      No_Overrun,
      --  Overrun. A Monitor data overrun has occurred. This can only happen
      --  when Monitor clock stretching not enabled via the MONCLKSTR bit in
      --  the CFG register. Writing 1 to this bit clears the flag.
      Overrun)
     with Size => 1;
   for STAT_MONOV_Field use
     (No_Overrun => 0,
      Overrun => 1);

   --  Monitor Active flag. Indicates when the Monitor function considers the I
   --  2C bus to be active. Active is defined here as when some Master is on
   --  the bus: a bus Start has occurred more recently than a bus Stop.
   type STAT_MONACTIVE_Field is
     (
      --  Inactive. The Monitor function considers the I2C bus to be inactive.
      Inactive,
      --  Active. The Monitor function considers the I2C bus to be active.
      Active)
     with Size => 1;
   for STAT_MONACTIVE_Field use
     (Inactive => 0,
      Active => 1);

   --  Monitor Idle flag. This flag is set when the Monitor function sees the
   --  I2C bus change from active to inactive. This can be used by software to
   --  decide when to process data accumulated by the Monitor function. This
   --  flag will cause an interrupt when set if enabled via the INTENSET
   --  register. The flag can be cleared by writing a 1 to this bit.
   type STAT_MONIDLE_Field is
     (
      --  Not idle. The I2C bus is not idle, or this flag has been cleared by
      --  software.
      Not_Idle,
      --  Idle. The I2C bus has gone idle at least once since the last time
      --  this flag was cleared by software.
      Idle)
     with Size => 1;
   for STAT_MONIDLE_Field use
     (Not_Idle => 0,
      Idle => 1);

   --  Event Time-out Interrupt flag. Indicates when the time between events
   --  has been longer than the time specified by the TIMEOUT register. Events
   --  include Start, Stop, and clock edges. The flag is cleared by writing a 1
   --  to this bit. No time-out is created when the I2C-bus is idle.
   type STAT_EVENTTIMEOUT_Field is
     (
      --  No time-out. I2C bus events have not caused a time-out.
      No_Timeout,
      --  Event time-out. The time between I2C bus events has been longer than
      --  the time specified by the TIMEOUT register.
      Even_Timeout)
     with Size => 1;
   for STAT_EVENTTIMEOUT_Field use
     (No_Timeout => 0,
      Even_Timeout => 1);

   --  SCL Time-out Interrupt flag. Indicates when SCL has remained low longer
   --  than the time specific by the TIMEOUT register. The flag is cleared by
   --  writing a 1 to this bit.
   type STAT_SCLTIMEOUT_Field is
     (
      --  No time-out. SCL low time has not caused a time-out.
      No_Timeout,
      --  Time-out. SCL low time has caused a time-out.
      Timeout)
     with Size => 1;
   for STAT_SCLTIMEOUT_Field use
     (No_Timeout => 0,
      Timeout => 1);

   --  Status register for Master, Slave, and Monitor functions.
   type STAT_Register is record
      --  Read-only. Master Pending. Indicates that the Master is waiting to
      --  continue communication on the I2C-bus (pending) or is idle. When the
      --  master is pending, the MSTSTATE bits indicate what type of software
      --  service if any the master expects. This flag will cause an interrupt
      --  when set if, enabled via the INTENSET register. The MSTPENDING flag
      --  is not set when the DMA is handling an event (if the MSTDMA bit in
      --  the MSTCTL register is set). If the master is in the idle state, and
      --  no communication is needed, mask this interrupt.
      MSTPENDING     : STAT_MSTPENDING_Field := NXP_SVD.I2C.Pending;
      --  Read-only. Master State code. The master state code reflects the
      --  master state when the MSTPENDING bit is set, that is the master is
      --  pending or in the idle state. Each value of this field indicates a
      --  specific required service for the Master function. All other values
      --  are reserved. See Table 400 for details of state values and
      --  appropriate responses.
      MSTSTATE       : STAT_MSTSTATE_Field := NXP_SVD.I2C.Idle;
      --  Master Arbitration Loss flag. This flag can be cleared by software
      --  writing a 1 to this bit. It is also cleared automatically a 1 is
      --  written to MSTCONTINUE.
      MSTARBLOSS     : STAT_MSTARBLOSS_Field := NXP_SVD.I2C.No_Loss;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  Master Start/Stop Error flag. This flag can be cleared by software
      --  writing a 1 to this bit. It is also cleared automatically a 1 is
      --  written to MSTCONTINUE.
      MSTSTSTPERR    : STAT_MSTSTSTPERR_Field := NXP_SVD.I2C.No_Error;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Read-only. Slave Pending. Indicates that the Slave function is
      --  waiting to continue communication on the I2C-bus and needs software
      --  service. This flag will cause an interrupt when set if enabled via
      --  INTENSET. The SLVPENDING flag is not set when the DMA is handling an
      --  event (if the SLVDMA bit in the SLVCTL register is set). The
      --  SLVPENDING flag is read-only and is automatically cleared when a 1 is
      --  written to the SLVCONTINUE bit in the SLVCTL register. The point in
      --  time when SlvPending is set depends on whether the I2C interface is
      --  in HSCAPABLE mode. See Section 25.7.2.2.2. When the I2C interface is
      --  configured to be HSCAPABLE, HS master codes are detected
      --  automatically. Due to the requirements of the HS I2C specification,
      --  slave addresses must also be detected automatically, since the
      --  address must be acknowledged before the clock can be stretched.
      SLVPENDING     : STAT_SLVPENDING_Field := NXP_SVD.I2C.In_Progress;
      --  Read-only. Slave State code. Each value of this field indicates a
      --  specific required service for the Slave function. All other values
      --  are reserved. See Table 401 for state values and actions. note that
      --  the occurrence of some states and how they are handled are affected
      --  by DMA mode and Automatic Operation modes.
      SLVSTATE       : STAT_SLVSTATE_Field := NXP_SVD.I2C.Slave_Address;
      --  Read-only. Slave Not Stretching. Indicates when the slave function is
      --  stretching the I2C clock. This is needed in order to gracefully
      --  invoke Deep Sleep or Power-down modes during slave operation. This
      --  read-only flag reflects the slave function status in real time.
      SLVNOTSTR      : STAT_SLVNOTSTR_Field := NXP_SVD.I2C.Not_Stretching;
      --  Read-only. Slave address match Index. This field is valid when the
      --  I2C slave function has been selected by receiving an address that
      --  matches one of the slave addresses defined by any enabled slave
      --  address registers, and provides an identification of the address that
      --  was matched. It is possible that more than one address could be
      --  matched, but only one match can be reported here.
      SLVIDX         : STAT_SLVIDX_Field := NXP_SVD.I2C.Address0;
      --  Read-only. Slave selected flag. SLVSEL is set after an address match
      --  when software tells the Slave function to acknowledge the address, or
      --  when the address has been automatically acknowledged. It is cleared
      --  when another address cycle presents an address that does not match an
      --  enabled address on the Slave function, when slave software decides to
      --  NACK a matched address, when there is a Stop detected on the bus,
      --  when the master NACKs slave data, and in some combinations of
      --  Automatic Operation. SLVSEL is not cleared if software NACKs data.
      SLVSEL         : STAT_SLVSEL_Field := NXP_SVD.I2C.Not_Selected;
      --  Slave Deselected flag. This flag will cause an interrupt when set if
      --  enabled via INTENSET. This flag can be cleared by writing a 1 to this
      --  bit.
      SLVDESEL       : STAT_SLVDESEL_Field := NXP_SVD.I2C.Not_Deselected;
      --  Read-only. Monitor Ready. This flag is cleared when the MONRXDAT
      --  register is read.
      MONRDY         : STAT_MONRDY_Field := NXP_SVD.I2C.No_Data;
      --  Monitor Overflow flag.
      MONOV          : STAT_MONOV_Field := NXP_SVD.I2C.No_Overrun;
      --  Read-only. Monitor Active flag. Indicates when the Monitor function
      --  considers the I 2C bus to be active. Active is defined here as when
      --  some Master is on the bus: a bus Start has occurred more recently
      --  than a bus Stop.
      MONACTIVE      : STAT_MONACTIVE_Field := NXP_SVD.I2C.Inactive;
      --  Monitor Idle flag. This flag is set when the Monitor function sees
      --  the I2C bus change from active to inactive. This can be used by
      --  software to decide when to process data accumulated by the Monitor
      --  function. This flag will cause an interrupt when set if enabled via
      --  the INTENSET register. The flag can be cleared by writing a 1 to this
      --  bit.
      MONIDLE        : STAT_MONIDLE_Field := NXP_SVD.I2C.Not_Idle;
      --  unspecified
      Reserved_20_23 : HAL.UInt4 := 16#0#;
      --  Event Time-out Interrupt flag. Indicates when the time between events
      --  has been longer than the time specified by the TIMEOUT register.
      --  Events include Start, Stop, and clock edges. The flag is cleared by
      --  writing a 1 to this bit. No time-out is created when the I2C-bus is
      --  idle.
      EVENTTIMEOUT   : STAT_EVENTTIMEOUT_Field := NXP_SVD.I2C.No_Timeout;
      --  SCL Time-out Interrupt flag. Indicates when SCL has remained low
      --  longer than the time specific by the TIMEOUT register. The flag is
      --  cleared by writing a 1 to this bit.
      SCLTIMEOUT     : STAT_SCLTIMEOUT_Field := NXP_SVD.I2C.No_Timeout;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for STAT_Register use record
      MSTPENDING     at 0 range 0 .. 0;
      MSTSTATE       at 0 range 1 .. 3;
      MSTARBLOSS     at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      MSTSTSTPERR    at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SLVPENDING     at 0 range 8 .. 8;
      SLVSTATE       at 0 range 9 .. 10;
      SLVNOTSTR      at 0 range 11 .. 11;
      SLVIDX         at 0 range 12 .. 13;
      SLVSEL         at 0 range 14 .. 14;
      SLVDESEL       at 0 range 15 .. 15;
      MONRDY         at 0 range 16 .. 16;
      MONOV          at 0 range 17 .. 17;
      MONACTIVE      at 0 range 18 .. 18;
      MONIDLE        at 0 range 19 .. 19;
      Reserved_20_23 at 0 range 20 .. 23;
      EVENTTIMEOUT   at 0 range 24 .. 24;
      SCLTIMEOUT     at 0 range 25 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   --  Master Pending interrupt Enable.
   type INTENSET_MSTPENDINGEN_Field is
     (
      --  Disabled. The MstPending interrupt is disabled.
      Disabled,
      --  Enabled. The MstPending interrupt is enabled.
      Enabled)
     with Size => 1;
   for INTENSET_MSTPENDINGEN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Master Arbitration Loss interrupt Enable.
   type INTENSET_MSTARBLOSSEN_Field is
     (
      --  Disabled. The MstArbLoss interrupt is disabled.
      Disabled,
      --  Enabled. The MstArbLoss interrupt is enabled.
      Enabled)
     with Size => 1;
   for INTENSET_MSTARBLOSSEN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Master Start/Stop Error interrupt Enable.
   type INTENSET_MSTSTSTPERREN_Field is
     (
      --  Disabled. The MstStStpErr interrupt is disabled.
      Disabled,
      --  Enabled. The MstStStpErr interrupt is enabled.
      Enabled)
     with Size => 1;
   for INTENSET_MSTSTSTPERREN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Slave Pending interrupt Enable.
   type INTENSET_SLVPENDINGEN_Field is
     (
      --  Disabled. The SlvPending interrupt is disabled.
      Disabled,
      --  Enabled. The SlvPending interrupt is enabled.
      Enabled)
     with Size => 1;
   for INTENSET_SLVPENDINGEN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Slave Not Stretching interrupt Enable.
   type INTENSET_SLVNOTSTREN_Field is
     (
      --  Disabled. The SlvNotStr interrupt is disabled.
      Disabled,
      --  Enabled. The SlvNotStr interrupt is enabled.
      Enabled)
     with Size => 1;
   for INTENSET_SLVNOTSTREN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Slave Deselect interrupt Enable.
   type INTENSET_SLVDESELEN_Field is
     (
      --  Disabled. The SlvDeSel interrupt is disabled.
      Disabled,
      --  Enabled. The SlvDeSel interrupt is enabled.
      Enabled)
     with Size => 1;
   for INTENSET_SLVDESELEN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Monitor data Ready interrupt Enable.
   type INTENSET_MONRDYEN_Field is
     (
      --  Disabled. The MonRdy interrupt is disabled.
      Disabled,
      --  Enabled. The MonRdy interrupt is enabled.
      Enabled)
     with Size => 1;
   for INTENSET_MONRDYEN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Monitor Overrun interrupt Enable.
   type INTENSET_MONOVEN_Field is
     (
      --  Disabled. The MonOv interrupt is disabled.
      Disabled,
      --  Enabled. The MonOv interrupt is enabled.
      Enabled)
     with Size => 1;
   for INTENSET_MONOVEN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Monitor Idle interrupt Enable.
   type INTENSET_MONIDLEEN_Field is
     (
      --  Disabled. The MonIdle interrupt is disabled.
      Disabled,
      --  Enabled. The MonIdle interrupt is enabled.
      Enabled)
     with Size => 1;
   for INTENSET_MONIDLEEN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Event time-out interrupt Enable.
   type INTENSET_EVENTTIMEOUTEN_Field is
     (
      --  Disabled. The Event time-out interrupt is disabled.
      Disabled,
      --  Enabled. The Event time-out interrupt is enabled.
      Enabled)
     with Size => 1;
   for INTENSET_EVENTTIMEOUTEN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  SCL time-out interrupt Enable.
   type INTENSET_SCLTIMEOUTEN_Field is
     (
      --  Disabled. The SCL time-out interrupt is disabled.
      Disabled,
      --  Enabled. The SCL time-out interrupt is enabled.
      Enabled)
     with Size => 1;
   for INTENSET_SCLTIMEOUTEN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Interrupt Enable Set and read register.
   type INTENSET_Register is record
      --  Master Pending interrupt Enable.
      MSTPENDINGEN   : INTENSET_MSTPENDINGEN_Field := NXP_SVD.I2C.Disabled;
      --  unspecified
      Reserved_1_3   : HAL.UInt3 := 16#0#;
      --  Master Arbitration Loss interrupt Enable.
      MSTARBLOSSEN   : INTENSET_MSTARBLOSSEN_Field := NXP_SVD.I2C.Disabled;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  Master Start/Stop Error interrupt Enable.
      MSTSTSTPERREN  : INTENSET_MSTSTSTPERREN_Field := NXP_SVD.I2C.Disabled;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Slave Pending interrupt Enable.
      SLVPENDINGEN   : INTENSET_SLVPENDINGEN_Field := NXP_SVD.I2C.Disabled;
      --  unspecified
      Reserved_9_10  : HAL.UInt2 := 16#0#;
      --  Slave Not Stretching interrupt Enable.
      SLVNOTSTREN    : INTENSET_SLVNOTSTREN_Field := NXP_SVD.I2C.Disabled;
      --  unspecified
      Reserved_12_14 : HAL.UInt3 := 16#0#;
      --  Slave Deselect interrupt Enable.
      SLVDESELEN     : INTENSET_SLVDESELEN_Field := NXP_SVD.I2C.Disabled;
      --  Monitor data Ready interrupt Enable.
      MONRDYEN       : INTENSET_MONRDYEN_Field := NXP_SVD.I2C.Disabled;
      --  Monitor Overrun interrupt Enable.
      MONOVEN        : INTENSET_MONOVEN_Field := NXP_SVD.I2C.Disabled;
      --  unspecified
      Reserved_18_18 : HAL.Bit := 16#0#;
      --  Monitor Idle interrupt Enable.
      MONIDLEEN      : INTENSET_MONIDLEEN_Field := NXP_SVD.I2C.Disabled;
      --  unspecified
      Reserved_20_23 : HAL.UInt4 := 16#0#;
      --  Event time-out interrupt Enable.
      EVENTTIMEOUTEN : INTENSET_EVENTTIMEOUTEN_Field := NXP_SVD.I2C.Disabled;
      --  SCL time-out interrupt Enable.
      SCLTIMEOUTEN   : INTENSET_SCLTIMEOUTEN_Field := NXP_SVD.I2C.Disabled;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENSET_Register use record
      MSTPENDINGEN   at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      MSTARBLOSSEN   at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      MSTSTSTPERREN  at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SLVPENDINGEN   at 0 range 8 .. 8;
      Reserved_9_10  at 0 range 9 .. 10;
      SLVNOTSTREN    at 0 range 11 .. 11;
      Reserved_12_14 at 0 range 12 .. 14;
      SLVDESELEN     at 0 range 15 .. 15;
      MONRDYEN       at 0 range 16 .. 16;
      MONOVEN        at 0 range 17 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      MONIDLEEN      at 0 range 19 .. 19;
      Reserved_20_23 at 0 range 20 .. 23;
      EVENTTIMEOUTEN at 0 range 24 .. 24;
      SCLTIMEOUTEN   at 0 range 25 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   --  Interrupt Enable Clear register.
   type INTENCLR_Register is record
      --  Write-only. Master Pending interrupt clear. Writing 1 to this bit
      --  clears the corresponding bit in the INTENSET register if implemented.
      MSTPENDINGCLR   : Boolean := False;
      --  unspecified
      Reserved_1_3    : HAL.UInt3 := 16#0#;
      --  Write-only. Master Arbitration Loss interrupt clear.
      MSTARBLOSSCLR   : Boolean := False;
      --  unspecified
      Reserved_5_5    : HAL.Bit := 16#0#;
      --  Write-only. Master Start/Stop Error interrupt clear.
      MSTSTSTPERRCLR  : Boolean := False;
      --  unspecified
      Reserved_7_7    : HAL.Bit := 16#0#;
      --  Write-only. Slave Pending interrupt clear.
      SLVPENDINGCLR   : Boolean := False;
      --  unspecified
      Reserved_9_10   : HAL.UInt2 := 16#0#;
      --  Write-only. Slave Not Stretching interrupt clear.
      SLVNOTSTRCLR    : Boolean := False;
      --  unspecified
      Reserved_12_14  : HAL.UInt3 := 16#0#;
      --  Write-only. Slave Deselect interrupt clear.
      SLVDESELCLR     : Boolean := False;
      --  Write-only. Monitor data Ready interrupt clear.
      MONRDYCLR       : Boolean := False;
      --  Write-only. Monitor Overrun interrupt clear.
      MONOVCLR        : Boolean := False;
      --  unspecified
      Reserved_18_18  : HAL.Bit := 16#0#;
      --  Write-only. Monitor Idle interrupt clear.
      MONIDLECLR      : Boolean := False;
      --  unspecified
      Reserved_20_23  : HAL.UInt4 := 16#0#;
      --  Write-only. Event time-out interrupt clear.
      EVENTTIMEOUTCLR : Boolean := False;
      --  Write-only. SCL time-out interrupt clear.
      SCLTIMEOUTCLR   : Boolean := False;
      --  unspecified
      Reserved_26_31  : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENCLR_Register use record
      MSTPENDINGCLR   at 0 range 0 .. 0;
      Reserved_1_3    at 0 range 1 .. 3;
      MSTARBLOSSCLR   at 0 range 4 .. 4;
      Reserved_5_5    at 0 range 5 .. 5;
      MSTSTSTPERRCLR  at 0 range 6 .. 6;
      Reserved_7_7    at 0 range 7 .. 7;
      SLVPENDINGCLR   at 0 range 8 .. 8;
      Reserved_9_10   at 0 range 9 .. 10;
      SLVNOTSTRCLR    at 0 range 11 .. 11;
      Reserved_12_14  at 0 range 12 .. 14;
      SLVDESELCLR     at 0 range 15 .. 15;
      MONRDYCLR       at 0 range 16 .. 16;
      MONOVCLR        at 0 range 17 .. 17;
      Reserved_18_18  at 0 range 18 .. 18;
      MONIDLECLR      at 0 range 19 .. 19;
      Reserved_20_23  at 0 range 20 .. 23;
      EVENTTIMEOUTCLR at 0 range 24 .. 24;
      SCLTIMEOUTCLR   at 0 range 25 .. 25;
      Reserved_26_31  at 0 range 26 .. 31;
   end record;

   subtype TIMEOUT_TOMIN_Field is HAL.UInt4;
   subtype TIMEOUT_TO_Field is HAL.UInt12;

   --  Time-out value register.
   type TIMEOUT_Register is record
      --  Time-out time value, bottom four bits. These are hard-wired to 0xF.
      --  This gives a minimum time-out of 16 I2C function clocks and also a
      --  time-out resolution of 16 I2C function clocks.
      TOMIN          : TIMEOUT_TOMIN_Field := 16#F#;
      --  Time-out time value. Specifies the time-out interval value in
      --  increments of 16 I 2C function clocks, as defined by the CLKDIV
      --  register. To change this value while I2C is in operation, disable all
      --  time-outs, write a new value to TIMEOUT, then re-enable time-outs.
      --  0x000 = A time-out will occur after 16 counts of the I2C function
      --  clock. 0x001 = A time-out will occur after 32 counts of the I2C
      --  function clock. 0xFFF = A time-out will occur after 65,536 counts of
      --  the I2C function clock.
      TO             : TIMEOUT_TO_Field := 16#FFF#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIMEOUT_Register use record
      TOMIN          at 0 range 0 .. 3;
      TO             at 0 range 4 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CLKDIV_DIVVAL_Field is HAL.UInt16;

   --  Clock pre-divider for the entire I2C interface. This determines what
   --  time increments are used for the MSTTIME register, and controls some
   --  timing of the Slave function.
   type CLKDIV_Register is record
      --  This field controls how the Flexcomm clock (FCLK) is used by the I2C
      --  functions that need an internal clock in order to operate. 0x0000 =
      --  FCLK is used directly by the I2C. 0x0001 = FCLK is divided by 2
      --  before use. 0x0002 = FCLK is divided by 3 before use. 0xFFFF = FCLK
      --  is divided by 65,536 before use.
      DIVVAL         : CLKDIV_DIVVAL_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CLKDIV_Register use record
      DIVVAL         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  Interrupt Status register for Master, Slave, and Monitor functions.
   type INTSTAT_Register is record
      --  Read-only. Master Pending.
      MSTPENDING     : Boolean;
      --  unspecified
      Reserved_1_3   : HAL.UInt3;
      --  Read-only. Master Arbitration Loss flag.
      MSTARBLOSS     : Boolean;
      --  unspecified
      Reserved_5_5   : HAL.Bit;
      --  Read-only. Master Start/Stop Error flag.
      MSTSTSTPERR    : Boolean;
      --  unspecified
      Reserved_7_7   : HAL.Bit;
      --  Read-only. Slave Pending.
      SLVPENDING     : Boolean;
      --  unspecified
      Reserved_9_10  : HAL.UInt2;
      --  Read-only. Slave Not Stretching status.
      SLVNOTSTR      : Boolean;
      --  unspecified
      Reserved_12_14 : HAL.UInt3;
      --  Read-only. Slave Deselected flag.
      SLVDESEL       : Boolean;
      --  Read-only. Monitor Ready.
      MONRDY         : Boolean;
      --  Read-only. Monitor Overflow flag.
      MONOV          : Boolean;
      --  unspecified
      Reserved_18_18 : HAL.Bit;
      --  Read-only. Monitor Idle flag.
      MONIDLE        : Boolean;
      --  unspecified
      Reserved_20_23 : HAL.UInt4;
      --  Read-only. Event time-out Interrupt flag.
      EVENTTIMEOUT   : Boolean;
      --  Read-only. SCL time-out Interrupt flag.
      SCLTIMEOUT     : Boolean;
      --  unspecified
      Reserved_26_31 : HAL.UInt6;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTSTAT_Register use record
      MSTPENDING     at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      MSTARBLOSS     at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      MSTSTSTPERR    at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SLVPENDING     at 0 range 8 .. 8;
      Reserved_9_10  at 0 range 9 .. 10;
      SLVNOTSTR      at 0 range 11 .. 11;
      Reserved_12_14 at 0 range 12 .. 14;
      SLVDESEL       at 0 range 15 .. 15;
      MONRDY         at 0 range 16 .. 16;
      MONOV          at 0 range 17 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      MONIDLE        at 0 range 19 .. 19;
      Reserved_20_23 at 0 range 20 .. 23;
      EVENTTIMEOUT   at 0 range 24 .. 24;
      SCLTIMEOUT     at 0 range 25 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   --  Master Continue. This bit is write-only.
   type MSTCTL_MSTCONTINUE_Field is
     (
      --  No effect.
      No_Effect,
      --  Continue. Informs the Master function to continue to the next
      --  operation. This must done after writing transmit data, reading
      --  received data, or any other housekeeping related to the next bus
      --  operation.
      Continue)
     with Size => 1;
   for MSTCTL_MSTCONTINUE_Field use
     (No_Effect => 0,
      Continue => 1);

   --  Master Start control. This bit is write-only.
   type MSTCTL_MSTSTART_Field is
     (
      --  No effect.
      No_Effect,
      --  Start. A Start will be generated on the I2C bus at the next allowed
      --  time.
      Start)
     with Size => 1;
   for MSTCTL_MSTSTART_Field use
     (No_Effect => 0,
      Start => 1);

   --  Master Stop control. This bit is write-only.
   type MSTCTL_MSTSTOP_Field is
     (
      --  No effect.
      No_Effect,
      --  Stop. A Stop will be generated on the I2C bus at the next allowed
      --  time, preceded by a NACK to the slave if the master is receiving data
      --  from the slave (Master Receiver mode).
      Stop)
     with Size => 1;
   for MSTCTL_MSTSTOP_Field use
     (No_Effect => 0,
      Stop => 1);

   --  Master DMA enable. Data operations of the I2C can be performed with DMA.
   --  Protocol type operations such as Start, address, Stop, and address match
   --  must always be done with software, typically via an interrupt. Address
   --  acknowledgement must also be done by software except when the I2C is
   --  configured to be HSCAPABLE (and address acknowledgement is handled
   --  entirely by hardware) or when Automatic Operation is enabled. When a DMA
   --  data transfer is complete, MSTDMA must be cleared prior to beginning the
   --  next operation, typically a Start or Stop.This bit is read/write.
   type MSTCTL_MSTDMA_Field is
     (
      --  Disable. No DMA requests are generated for master operation.
      Disabled,
      --  Enable. A DMA request is generated for I2C master data operations.
      --  When this I2C master is generating Acknowledge bits in Master
      --  Receiver mode, the acknowledge is generated automatically.
      Enabled)
     with Size => 1;
   for MSTCTL_MSTDMA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Master control register.
   type MSTCTL_Register is record
      --  Write-only. Master Continue. This bit is write-only.
      MSTCONTINUE   : MSTCTL_MSTCONTINUE_Field := NXP_SVD.I2C.No_Effect;
      --  Master Start control. This bit is write-only.
      MSTSTART      : MSTCTL_MSTSTART_Field := NXP_SVD.I2C.No_Effect;
      --  Master Stop control. This bit is write-only.
      MSTSTOP       : MSTCTL_MSTSTOP_Field := NXP_SVD.I2C.No_Effect;
      --  Master DMA enable. Data operations of the I2C can be performed with
      --  DMA. Protocol type operations such as Start, address, Stop, and
      --  address match must always be done with software, typically via an
      --  interrupt. Address acknowledgement must also be done by software
      --  except when the I2C is configured to be HSCAPABLE (and address
      --  acknowledgement is handled entirely by hardware) or when Automatic
      --  Operation is enabled. When a DMA data transfer is complete, MSTDMA
      --  must be cleared prior to beginning the next operation, typically a
      --  Start or Stop.This bit is read/write.
      MSTDMA        : MSTCTL_MSTDMA_Field := NXP_SVD.I2C.Disabled;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MSTCTL_Register use record
      MSTCONTINUE   at 0 range 0 .. 0;
      MSTSTART      at 0 range 1 .. 1;
      MSTSTOP       at 0 range 2 .. 2;
      MSTDMA        at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  Master SCL Low time. Specifies the minimum low time that will be
   --  asserted by this master on SCL. Other devices on the bus (masters or
   --  slaves) could lengthen this time. This corresponds to the parameter t
   --  LOW in the I2C bus specification. I2C bus specification parameters tBUF
   --  and tSU;STA have the same values and are also controlled by MSTSCLLOW.
   type MSTTIME_MSTSCLLOW_Field is
     (
      --  2 clocks. Minimum SCL low time is 2 clocks of the I2C clock
      --  pre-divider.
      Clocks_2,
      --  3 clocks. Minimum SCL low time is 3 clocks of the I2C clock
      --  pre-divider.
      Clocks_3,
      --  4 clocks. Minimum SCL low time is 4 clocks of the I2C clock
      --  pre-divider.
      Clocks_4,
      --  5 clocks. Minimum SCL low time is 5 clocks of the I2C clock
      --  pre-divider.
      Clocks_5,
      --  6 clocks. Minimum SCL low time is 6 clocks of the I2C clock
      --  pre-divider.
      Clocks_6,
      --  7 clocks. Minimum SCL low time is 7 clocks of the I2C clock
      --  pre-divider.
      Clocks_7,
      --  8 clocks. Minimum SCL low time is 8 clocks of the I2C clock
      --  pre-divider.
      Clocks_8,
      --  9 clocks. Minimum SCL low time is 9 clocks of the I2C clock
      --  pre-divider.
      Clocks_9)
     with Size => 3;
   for MSTTIME_MSTSCLLOW_Field use
     (Clocks_2 => 0,
      Clocks_3 => 1,
      Clocks_4 => 2,
      Clocks_5 => 3,
      Clocks_6 => 4,
      Clocks_7 => 5,
      Clocks_8 => 6,
      Clocks_9 => 7);

   --  Master SCL High time. Specifies the minimum high time that will be
   --  asserted by this master on SCL. Other masters in a multi-master system
   --  could shorten this time. This corresponds to the parameter tHIGH in the
   --  I2C bus specification. I2C bus specification parameters tSU;STO and
   --  tHD;STA have the same values and are also controlled by MSTSCLHIGH.
   type MSTTIME_MSTSCLHIGH_Field is
     (
      --  2 clocks. Minimum SCL high time is 2 clock of the I2C clock
      --  pre-divider.
      Clocks_2,
      --  3 clocks. Minimum SCL high time is 3 clocks of the I2C clock
      --  pre-divider .
      Clocks_3,
      --  4 clocks. Minimum SCL high time is 4 clock of the I2C clock
      --  pre-divider.
      Clocks_4,
      --  5 clocks. Minimum SCL high time is 5 clock of the I2C clock
      --  pre-divider.
      Clocks_5,
      --  6 clocks. Minimum SCL high time is 6 clock of the I2C clock
      --  pre-divider.
      Clocks_6,
      --  7 clocks. Minimum SCL high time is 7 clock of the I2C clock
      --  pre-divider.
      Clocks_7,
      --  8 clocks. Minimum SCL high time is 8 clock of the I2C clock
      --  pre-divider.
      Clocks_8,
      --  9 clocks. Minimum SCL high time is 9 clocks of the I2C clock
      --  pre-divider.
      Clocks_9)
     with Size => 3;
   for MSTTIME_MSTSCLHIGH_Field use
     (Clocks_2 => 0,
      Clocks_3 => 1,
      Clocks_4 => 2,
      Clocks_5 => 3,
      Clocks_6 => 4,
      Clocks_7 => 5,
      Clocks_8 => 6,
      Clocks_9 => 7);

   --  Master timing configuration.
   type MSTTIME_Register is record
      --  Master SCL Low time. Specifies the minimum low time that will be
      --  asserted by this master on SCL. Other devices on the bus (masters or
      --  slaves) could lengthen this time. This corresponds to the parameter t
      --  LOW in the I2C bus specification. I2C bus specification parameters
      --  tBUF and tSU;STA have the same values and are also controlled by
      --  MSTSCLLOW.
      MSTSCLLOW     : MSTTIME_MSTSCLLOW_Field := NXP_SVD.I2C.Clocks_9;
      --  unspecified
      Reserved_3_3  : HAL.Bit := 16#0#;
      --  Master SCL High time. Specifies the minimum high time that will be
      --  asserted by this master on SCL. Other masters in a multi-master
      --  system could shorten this time. This corresponds to the parameter
      --  tHIGH in the I2C bus specification. I2C bus specification parameters
      --  tSU;STO and tHD;STA have the same values and are also controlled by
      --  MSTSCLHIGH.
      MSTSCLHIGH    : MSTTIME_MSTSCLHIGH_Field := NXP_SVD.I2C.Clocks_9;
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MSTTIME_Register use record
      MSTSCLLOW     at 0 range 0 .. 2;
      Reserved_3_3  at 0 range 3 .. 3;
      MSTSCLHIGH    at 0 range 4 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   subtype MSTDAT_DATA_Field is HAL.UInt8;

   --  Combined Master receiver and transmitter data register.
   type MSTDAT_Register is record
      --  Master function data register. Read: read the most recently received
      --  data for the Master function. Write: transmit data using the Master
      --  function.
      DATA          : MSTDAT_DATA_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MSTDAT_Register use record
      DATA          at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  Slave Continue.
   type SLVCTL_SLVCONTINUE_Field is
     (
      --  No effect.
      No_Effect,
      --  Continue. Informs the Slave function to continue to the next
      --  operation, by clearing the SLVPENDING flag in the STAT register. This
      --  must be done after writing transmit data, reading received data, or
      --  any other housekeeping related to the next bus operation. Automatic
      --  Operation has different requirements. SLVCONTINUE should not be set
      --  unless SLVPENDING = 1.
      Continue)
     with Size => 1;
   for SLVCTL_SLVCONTINUE_Field use
     (No_Effect => 0,
      Continue => 1);

   --  Slave NACK.
   type SLVCTL_SLVNACK_Field is
     (
      --  No effect.
      No_Effect,
      --  NACK. Causes the Slave function to NACK the master when the slave is
      --  receiving data from the master (Slave Receiver mode).
      Nack)
     with Size => 1;
   for SLVCTL_SLVNACK_Field use
     (No_Effect => 0,
      Nack => 1);

   --  Slave DMA enable.
   type SLVCTL_SLVDMA_Field is
     (
      --  Disabled. No DMA requests are issued for Slave mode operation.
      Disabled,
      --  Enabled. DMA requests are issued for I2C slave data transmission and
      --  reception.
      Enabled)
     with Size => 1;
   for SLVCTL_SLVDMA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Automatic Acknowledge.When this bit is set, it will cause an I2C header
   --  which matches SLVADR0 and the direction set by AUTOMATCHREAD to be ACKed
   --  immediately; this is used with DMA to allow processing of the data
   --  without intervention. If this bit is clear and a header matches SLVADR0,
   --  the behavior is controlled by AUTONACK in the SLVADR0 register: allowing
   --  NACK or interrupt.
   type SLVCTL_AUTOACK_Field is
     (
      --  Normal, non-automatic operation. If AUTONACK = 0, an SlvPending
      --  interrupt is generated when a matching address is received. If
      --  AUTONACK = 1, received addresses are NACKed (ignored).
      Normal,
      --  A header with matching SLVADR0 and matching direction as set by
      --  AUTOMATCHREAD will be ACKed immediately, allowing the master to move
      --  on to the data bytes. If the address matches SLVADR0, but the
      --  direction does not match AUTOMATCHREAD, the behavior will depend on
      --  the AUTONACK bit in the SLVADR0 register: if AUTONACK is set, then it
      --  will be Nacked; else if AUTONACK is clear, then a SlvPending
      --  interrupt is generated.
      Automatic_Ack)
     with Size => 1;
   for SLVCTL_AUTOACK_Field use
     (Normal => 0,
      Automatic_Ack => 1);

   --  When AUTOACK is set, this bit controls whether it matches a read or
   --  write request on the next header with an address matching SLVADR0. Since
   --  DMA needs to be configured to match the transfer direction, the
   --  direction needs to be specified. This bit allows a direction to be
   --  chosen for the next operation.
   type SLVCTL_AUTOMATCHREAD_Field is
     (
      --  The expected next operation in Automatic Mode is an I2C write.
      I2C_Write,
      --  The expected next operation in Automatic Mode is an I2C read.
      I2C_Read)
     with Size => 1;
   for SLVCTL_AUTOMATCHREAD_Field use
     (I2C_Write => 0,
      I2C_Read => 1);

   --  Slave control register.
   type SLVCTL_Register is record
      --  Slave Continue.
      SLVCONTINUE    : SLVCTL_SLVCONTINUE_Field := NXP_SVD.I2C.No_Effect;
      --  Slave NACK.
      SLVNACK        : SLVCTL_SLVNACK_Field := NXP_SVD.I2C.No_Effect;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  Slave DMA enable.
      SLVDMA         : SLVCTL_SLVDMA_Field := NXP_SVD.I2C.Disabled;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Automatic Acknowledge.When this bit is set, it will cause an I2C
      --  header which matches SLVADR0 and the direction set by AUTOMATCHREAD
      --  to be ACKed immediately; this is used with DMA to allow processing of
      --  the data without intervention. If this bit is clear and a header
      --  matches SLVADR0, the behavior is controlled by AUTONACK in the
      --  SLVADR0 register: allowing NACK or interrupt.
      AUTOACK        : SLVCTL_AUTOACK_Field := NXP_SVD.I2C.Normal;
      --  When AUTOACK is set, this bit controls whether it matches a read or
      --  write request on the next header with an address matching SLVADR0.
      --  Since DMA needs to be configured to match the transfer direction, the
      --  direction needs to be specified. This bit allows a direction to be
      --  chosen for the next operation.
      AUTOMATCHREAD  : SLVCTL_AUTOMATCHREAD_Field := NXP_SVD.I2C.I2C_Write;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SLVCTL_Register use record
      SLVCONTINUE    at 0 range 0 .. 0;
      SLVNACK        at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      SLVDMA         at 0 range 3 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      AUTOACK        at 0 range 8 .. 8;
      AUTOMATCHREAD  at 0 range 9 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype SLVDAT_DATA_Field is HAL.UInt8;

   --  Combined Slave receiver and transmitter data register.
   type SLVDAT_Register is record
      --  Slave function data register. Read: read the most recently received
      --  data for the Slave function. Write: transmit data using the Slave
      --  function.
      DATA          : SLVDAT_DATA_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SLVDAT_Register use record
      DATA          at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  Slave Address n Disable.
   type SLVADR0_SADISABLE_Field is
     (
      --  Enabled. Slave Address n is enabled.
      Enabled,
      --  Ignored Slave Address n is ignored.
      Disabled)
     with Size => 1;
   for SLVADR0_SADISABLE_Field use
     (Enabled => 0,
      Disabled => 1);

   subtype SLVADR0_SLVADR_Field is HAL.UInt7;

   --  Automatic NACK operation. Used in conjunction with AUTOACK and
   --  AUTOMATCHREAD, allows software to ignore I2C traffic while handling
   --  previous I2C data or other operations.
   type SLVADR0_AUTONACK_Field is
     (
      --  Normal operation, matching I2C addresses are not ignored.
      Normal,
      --  Automatic-only mode. All incoming addresses are ignored (NACKed),
      --  unless AUTOACK is set, it matches SLVADRn, and AUTOMATCHREAD matches
      --  the direction.
      Automatic)
     with Size => 1;
   for SLVADR0_AUTONACK_Field use
     (Normal => 0,
      Automatic => 1);

   --  Slave address register.
   type SLVADR0_Register is record
      --  Slave Address n Disable.
      SADISABLE      : SLVADR0_SADISABLE_Field := NXP_SVD.I2C.Disabled;
      --  Slave Address. Seven bit slave address that is compared to received
      --  addresses if enabled.
      SLVADR         : SLVADR0_SLVADR_Field := 16#0#;
      --  unspecified
      Reserved_8_14  : HAL.UInt7 := 16#0#;
      --  Automatic NACK operation. Used in conjunction with AUTOACK and
      --  AUTOMATCHREAD, allows software to ignore I2C traffic while handling
      --  previous I2C data or other operations.
      AUTONACK       : SLVADR0_AUTONACK_Field := NXP_SVD.I2C.Normal;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SLVADR0_Register use record
      SADISABLE      at 0 range 0 .. 0;
      SLVADR         at 0 range 1 .. 7;
      Reserved_8_14  at 0 range 8 .. 14;
      AUTONACK       at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  Slave Address n Disable.
   type SLVADR1_SADISABLE_Field is
     (
      --  Enabled. Slave Address n is enabled.
      Enabled,
      --  Ignored Slave Address n is ignored.
      Disabled)
     with Size => 1;
   for SLVADR1_SADISABLE_Field use
     (Enabled => 0,
      Disabled => 1);

   subtype SLVADR_SLVADR_Field is HAL.UInt7;

   --  Slave address register.
   type SLVADR_Register is record
      --  Slave Address n Disable.
      SADISABLE     : SLVADR1_SADISABLE_Field := NXP_SVD.I2C.Disabled;
      --  Slave Address. Seven bit slave address that is compared to received
      --  addresses if enabled.
      SLVADR        : SLVADR_SLVADR_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SLVADR_Register use record
      SADISABLE     at 0 range 0 .. 0;
      SLVADR        at 0 range 1 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  Qualify mode for slave address 0.
   type SLVQUAL0_QUALMODE0_Field is
     (
      --  Mask. The SLVQUAL0 field is used as a logical mask for matching
      --  address 0.
      Mask,
      --  Extend. The SLVQUAL0 field is used to extend address 0 matching in a
      --  range of addresses.
      Extend)
     with Size => 1;
   for SLVQUAL0_QUALMODE0_Field use
     (Mask => 0,
      Extend => 1);

   subtype SLVQUAL0_SLVQUAL0_Field is HAL.UInt7;

   --  Slave Qualification for address 0.
   type SLVQUAL0_Register is record
      --  Qualify mode for slave address 0.
      QUALMODE0     : SLVQUAL0_QUALMODE0_Field := NXP_SVD.I2C.Mask;
      --  Slave address Qualifier for address 0. A value of 0 causes the
      --  address in SLVADR0 to be used as-is, assuming that it is enabled. If
      --  QUALMODE0 = 0, any bit in this field which is set to 1 will cause an
      --  automatic match of the corresponding bit of the received address when
      --  it is compared to the SLVADR0 register. If QUALMODE0 = 1, an address
      --  range is matched for address 0. This range extends from the value
      --  defined by SLVADR0 to the address defined by SLVQUAL0 (address
      --  matches when SLVADR0[7:1] <= received address <= SLVQUAL0[7:1]).
      SLVQUAL0      : SLVQUAL0_SLVQUAL0_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SLVQUAL0_Register use record
      QUALMODE0     at 0 range 0 .. 0;
      SLVQUAL0      at 0 range 1 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype MONRXDAT_MONRXDAT_Field is HAL.UInt8;

   --  Monitor Received Start.
   type MONRXDAT_MONSTART_Field is
     (
      --  No start detected. The Monitor function has not detected a Start
      --  event on the I2C bus.
      No_Start_Detected,
      --  Start detected. The Monitor function has detected a Start event on
      --  the I2C bus.
      Start_Detected)
     with Size => 1;
   for MONRXDAT_MONSTART_Field use
     (No_Start_Detected => 0,
      Start_Detected => 1);

   --  Monitor Received Repeated Start.
   type MONRXDAT_MONRESTART_Field is
     (
      --  No repeated start detected. The Monitor function has not detected a
      --  Repeated Start event on the I2C bus.
      Not_Detected,
      --  Repeated start detected. The Monitor function has detected a Repeated
      --  Start event on the I2C bus.
      Detected)
     with Size => 1;
   for MONRXDAT_MONRESTART_Field use
     (Not_Detected => 0,
      Detected => 1);

   --  Monitor Received NACK.
   type MONRXDAT_MONNACK_Field is
     (
      --  Acknowledged. The data currently being provided by the Monitor
      --  function was acknowledged by at least one master or slave receiver.
      Acknowledged,
      --  Not acknowledged. The data currently being provided by the Monitor
      --  function was not acknowledged by any receiver.
      Not_Acknowledged)
     with Size => 1;
   for MONRXDAT_MONNACK_Field use
     (Acknowledged => 0,
      Not_Acknowledged => 1);

   --  Monitor receiver data register.
   type MONRXDAT_Register is record
      --  Read-only. Monitor function Receiver Data. This reflects every data
      --  byte that passes on the I2C pins.
      MONRXDAT       : MONRXDAT_MONRXDAT_Field;
      --  Read-only. Monitor Received Start.
      MONSTART       : MONRXDAT_MONSTART_Field;
      --  Read-only. Monitor Received Repeated Start.
      MONRESTART     : MONRXDAT_MONRESTART_Field;
      --  Read-only. Monitor Received NACK.
      MONNACK        : MONRXDAT_MONNACK_Field;
      --  unspecified
      Reserved_11_31 : HAL.UInt21;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MONRXDAT_Register use record
      MONRXDAT       at 0 range 0 .. 7;
      MONSTART       at 0 range 8 .. 8;
      MONRESTART     at 0 range 9 .. 9;
      MONNACK        at 0 range 10 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   subtype ID_APERTURE_Field is HAL.UInt8;
   subtype ID_MINOR_REV_Field is HAL.UInt4;
   subtype ID_MAJOR_REV_Field is HAL.UInt4;
   subtype ID_ID_Field is HAL.UInt16;

   --  Peripheral identification register.
   type ID_Register is record
      --  Read-only. Aperture: encoded as (aperture size/4K) -1, so 0x00 means
      --  a 4K aperture.
      APERTURE  : ID_APERTURE_Field;
      --  Read-only. Minor revision of module implementation.
      MINOR_REV : ID_MINOR_REV_Field;
      --  Read-only. Major revision of module implementation.
      MAJOR_REV : ID_MAJOR_REV_Field;
      --  Read-only. Module identifier for the selected function.
      ID        : ID_ID_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ID_Register use record
      APERTURE  at 0 range 0 .. 7;
      MINOR_REV at 0 range 8 .. 11;
      MAJOR_REV at 0 range 12 .. 15;
      ID        at 0 range 16 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  I2C-bus interfaces
   type I2C_Peripheral is record
      --  Configuration for shared functions.
      CFG      : aliased CFG_Register;
      --  Status register for Master, Slave, and Monitor functions.
      STAT     : aliased STAT_Register;
      --  Interrupt Enable Set and read register.
      INTENSET : aliased INTENSET_Register;
      --  Interrupt Enable Clear register.
      INTENCLR : aliased INTENCLR_Register;
      --  Time-out value register.
      TIMEOUT  : aliased TIMEOUT_Register;
      --  Clock pre-divider for the entire I2C interface. This determines what
      --  time increments are used for the MSTTIME register, and controls some
      --  timing of the Slave function.
      CLKDIV   : aliased CLKDIV_Register;
      --  Interrupt Status register for Master, Slave, and Monitor functions.
      INTSTAT  : aliased INTSTAT_Register;
      --  Master control register.
      MSTCTL   : aliased MSTCTL_Register;
      --  Master timing configuration.
      MSTTIME  : aliased MSTTIME_Register;
      --  Combined Master receiver and transmitter data register.
      MSTDAT   : aliased MSTDAT_Register;
      --  Slave control register.
      SLVCTL   : aliased SLVCTL_Register;
      --  Combined Slave receiver and transmitter data register.
      SLVDAT   : aliased SLVDAT_Register;
      --  Slave address register.
      SLVADR0  : aliased SLVADR0_Register;
      --  Slave address register.
      SLVADR1  : aliased SLVADR_Register;
      --  Slave address register.
      SLVADR2  : aliased SLVADR_Register;
      --  Slave address register.
      SLVADR3  : aliased SLVADR_Register;
      --  Slave Qualification for address 0.
      SLVQUAL0 : aliased SLVQUAL0_Register;
      --  Monitor receiver data register.
      MONRXDAT : aliased MONRXDAT_Register;
      --  Peripheral identification register.
      ID       : aliased ID_Register;
   end record
     with Volatile;

   for I2C_Peripheral use record
      CFG      at 16#800# range 0 .. 31;
      STAT     at 16#804# range 0 .. 31;
      INTENSET at 16#808# range 0 .. 31;
      INTENCLR at 16#80C# range 0 .. 31;
      TIMEOUT  at 16#810# range 0 .. 31;
      CLKDIV   at 16#814# range 0 .. 31;
      INTSTAT  at 16#818# range 0 .. 31;
      MSTCTL   at 16#820# range 0 .. 31;
      MSTTIME  at 16#824# range 0 .. 31;
      MSTDAT   at 16#828# range 0 .. 31;
      SLVCTL   at 16#840# range 0 .. 31;
      SLVDAT   at 16#844# range 0 .. 31;
      SLVADR0  at 16#848# range 0 .. 31;
      SLVADR1  at 16#84C# range 0 .. 31;
      SLVADR2  at 16#850# range 0 .. 31;
      SLVADR3  at 16#854# range 0 .. 31;
      SLVQUAL0 at 16#858# range 0 .. 31;
      MONRXDAT at 16#880# range 0 .. 31;
      ID       at 16#FFC# range 0 .. 31;
   end record;

   --  I2C-bus interfaces
   I2C0_Periph : aliased I2C_Peripheral
     with Import, Address => System'To_Address (16#40086000#);

   --  I2C-bus interfaces
   I2C1_Periph : aliased I2C_Peripheral
     with Import, Address => System'To_Address (16#40087000#);

   --  I2C-bus interfaces
   I2C2_Periph : aliased I2C_Peripheral
     with Import, Address => System'To_Address (16#40088000#);

   --  I2C-bus interfaces
   I2C3_Periph : aliased I2C_Peripheral
     with Import, Address => System'To_Address (16#40089000#);

   --  I2C-bus interfaces
   I2C4_Periph : aliased I2C_Peripheral
     with Import, Address => System'To_Address (16#4008A000#);

   --  I2C-bus interfaces
   I2C5_Periph : aliased I2C_Peripheral
     with Import, Address => System'To_Address (16#40096000#);

   --  I2C-bus interfaces
   I2C6_Periph : aliased I2C_Peripheral
     with Import, Address => System'To_Address (16#40097000#);

   --  I2C-bus interfaces
   I2C7_Periph : aliased I2C_Peripheral
     with Import, Address => System'To_Address (16#40098000#);

end NXP_SVD.I2C;
