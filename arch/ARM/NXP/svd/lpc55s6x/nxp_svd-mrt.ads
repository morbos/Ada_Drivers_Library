--  Copyright 2016-2019 NXP
--  All rights reserved.SPDX-License-Identifier: BSD-3-Clause


--  This spec has been automatically generated from LPC55S6x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NXP_SVD.MRT is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   ---------------------------------
   -- CHANNEL cluster's Registers --
   ---------------------------------

   subtype INTVAL_CHANNEL_IVALUE_Field is HAL.UInt24;

   --  Determines how the timer interval value IVALUE -1 is loaded into the
   --  TIMERn register. This bit is write-only. Reading this bit always returns
   --  0.
   type INTVAL_LOAD_Field is
     (
      --  No force load. The load from the INTVALn register to the TIMERn
      --  register is processed at the end of the time interval if the repeat
      --  mode is selected.
      No_Force_Load,
      --  Force load. The INTVALn interval value IVALUE -1 is immediately
      --  loaded into the TIMERn register while TIMERn is running.
      Force_Load)
     with Size => 1;
   for INTVAL_LOAD_Field use
     (No_Force_Load => 0,
      Force_Load => 1);

   --  MRT Time interval value register. This value is loaded into the TIMER
   --  register.
   type INTVAL_CHANNEL_Register is record
      --  Time interval load value. This value is loaded into the TIMERn
      --  register and the MRT channel n starts counting down from IVALUE -1.
      --  If the timer is idle, writing a non-zero value to this bit field
      --  starts the timer immediately. If the timer is running, writing a zero
      --  to this bit field does the following: If LOAD = 1, the timer stops
      --  immediately. If LOAD = 0, the timer stops at the end of the time
      --  interval.
      IVALUE         : INTVAL_CHANNEL_IVALUE_Field := 16#0#;
      --  unspecified
      Reserved_24_30 : HAL.UInt7 := 16#0#;
      --  Determines how the timer interval value IVALUE -1 is loaded into the
      --  TIMERn register. This bit is write-only. Reading this bit always
      --  returns 0.
      LOAD           : INTVAL_LOAD_Field := NXP_SVD.MRT.No_Force_Load;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTVAL_CHANNEL_Register use record
      IVALUE         at 0 range 0 .. 23;
      Reserved_24_30 at 0 range 24 .. 30;
      LOAD           at 0 range 31 .. 31;
   end record;

   subtype TIMER_CHANNEL_VALUE_Field is HAL.UInt24;

   --  MRT Timer register. This register reads the value of the down-counter.
   type TIMER_CHANNEL_Register is record
      --  Read-only. Holds the current timer value of the down-counter. The
      --  initial value of the TIMERn register is loaded as IVALUE - 1 from the
      --  INTVALn register either at the end of the time interval or
      --  immediately in the following cases: INTVALn register is updated in
      --  the idle state. INTVALn register is updated with LOAD = 1. When the
      --  timer is in idle state, reading this bit fields returns -1 (0x00FF
      --  FFFF).
      VALUE          : TIMER_CHANNEL_VALUE_Field;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIMER_CHANNEL_Register use record
      VALUE          at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  Enable the TIMERn interrupt.
   type CTRL_INTEN_Field is
     (
      --  Disabled. TIMERn interrupt is disabled.
      Disabled,
      --  Enabled. TIMERn interrupt is enabled.
      Enabled)
     with Size => 1;
   for CTRL_INTEN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Selects timer mode.
   type CTRL_MODE_Field is
     (
      --  Repeat interrupt mode.
      Repeat_Interrupt_Mode,
      --  One-shot interrupt mode.
      One_Shot_Interrupt_Mode,
      --  One-shot stall mode.
      One_Shot_Stall_Mode)
     with Size => 2;
   for CTRL_MODE_Field use
     (Repeat_Interrupt_Mode => 0,
      One_Shot_Interrupt_Mode => 1,
      One_Shot_Stall_Mode => 2);

   --  MRT Control register. This register controls the MRT modes.
   type CTRL_CHANNEL_Register is record
      --  Enable the TIMERn interrupt.
      INTEN         : CTRL_INTEN_Field := NXP_SVD.MRT.Disabled;
      --  Selects timer mode.
      MODE          : CTRL_MODE_Field := NXP_SVD.MRT.Repeat_Interrupt_Mode;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CTRL_CHANNEL_Register use record
      INTEN         at 0 range 0 .. 0;
      MODE          at 0 range 1 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  Monitors the interrupt flag.
   type STAT_INTFLAG_Field is
     (
      --  No pending interrupt. Writing a zero is equivalent to no operation.
      No_Pending_Interrupt,
      --  Pending interrupt. The interrupt is pending because TIMERn has
      --  reached the end of the time interval. If the INTEN bit in the
      --  CONTROLn is also set to 1, the interrupt for timer channel n and the
      --  global interrupt are raised. Writing a 1 to this bit clears the
      --  interrupt request.
      Pending_Interrupt)
     with Size => 1;
   for STAT_INTFLAG_Field use
     (No_Pending_Interrupt => 0,
      Pending_Interrupt => 1);

   --  Indicates the state of TIMERn. This bit is read-only.
   type STAT_RUN_Field is
     (
      --  Idle state. TIMERn is stopped.
      Idle_State,
      --  Running. TIMERn is running.
      Running)
     with Size => 1;
   for STAT_RUN_Field use
     (Idle_State => 0,
      Running => 1);

   --  Channel In Use flag. Operating details depend on the MULTITASK bit in
   --  the MODCFG register, and affects the use of IDLE_CH. See Idle channel
   --  register for details of the two operating modes.
   type STAT_INUSE_Field is
     (
      --  This channel is not in use.
      No,
      --  This channel is in use.
      Yes)
     with Size => 1;
   for STAT_INUSE_Field use
     (No => 0,
      Yes => 1);

   --  MRT Status register.
   type STAT_CHANNEL_Register is record
      --  Monitors the interrupt flag.
      INTFLAG       : STAT_INTFLAG_Field := NXP_SVD.MRT.No_Pending_Interrupt;
      --  Indicates the state of TIMERn. This bit is read-only.
      RUN           : STAT_RUN_Field := NXP_SVD.MRT.Idle_State;
      --  Channel In Use flag. Operating details depend on the MULTITASK bit in
      --  the MODCFG register, and affects the use of IDLE_CH. See Idle channel
      --  register for details of the two operating modes.
      INUSE         : STAT_INUSE_Field := NXP_SVD.MRT.No;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for STAT_CHANNEL_Register use record
      INTFLAG       at 0 range 0 .. 0;
      RUN           at 0 range 1 .. 1;
      INUSE         at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  no description available
   type CHANNEL_Cluster is record
      --  MRT Time interval value register. This value is loaded into the TIMER
      --  register.
      INTVAL : aliased INTVAL_CHANNEL_Register;
      --  MRT Timer register. This register reads the value of the
      --  down-counter.
      TIMER  : aliased TIMER_CHANNEL_Register;
      --  MRT Control register. This register controls the MRT modes.
      CTRL   : aliased CTRL_CHANNEL_Register;
      --  MRT Status register.
      STAT   : aliased STAT_CHANNEL_Register;
   end record
     with Volatile, Size => 128;

   for CHANNEL_Cluster use record
      INTVAL at 16#0# range 0 .. 31;
      TIMER  at 16#4# range 0 .. 31;
      CTRL   at 16#8# range 0 .. 31;
      STAT   at 16#C# range 0 .. 31;
   end record;

   --  no description available
   type CHANNEL_Clusters is array (0 .. 3) of CHANNEL_Cluster;

   subtype MODCFG_NOC_Field is HAL.UInt4;
   subtype MODCFG_NOB_Field is HAL.UInt5;

   --  Selects the operating mode for the INUSE flags and the IDLE_CH register.
   type MODCFG_MULTITASK_Field is
     (
      --  Hardware status mode. In this mode, the INUSE(n) flags for all
      --  channels are reset.
      Hardware_Status_Mode,
      --  Multi-task mode.
      Multi_Task_Mode)
     with Size => 1;
   for MODCFG_MULTITASK_Field use
     (Hardware_Status_Mode => 0,
      Multi_Task_Mode => 1);

   --  Module Configuration register. This register provides information about
   --  this particular MRT instance, and allows choosing an overall mode for
   --  the idle channel feature.
   type MODCFG_Register is record
      --  Identifies the number of channels in this MRT.(4 channels on this
      --  device.)
      NOC           : MODCFG_NOC_Field := 16#3#;
      --  Identifies the number of timer bits in this MRT. (24 bits wide on
      --  this device.)
      NOB           : MODCFG_NOB_Field := 16#17#;
      --  unspecified
      Reserved_9_30 : HAL.UInt22 := 16#0#;
      --  Selects the operating mode for the INUSE flags and the IDLE_CH
      --  register.
      MULTITASK     : MODCFG_MULTITASK_Field :=
                       NXP_SVD.MRT.Hardware_Status_Mode;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MODCFG_Register use record
      NOC           at 0 range 0 .. 3;
      NOB           at 0 range 4 .. 8;
      Reserved_9_30 at 0 range 9 .. 30;
      MULTITASK     at 0 range 31 .. 31;
   end record;

   subtype IDLE_CH_CHAN_Field is HAL.UInt4;

   --  Idle channel register. This register returns the number of the first
   --  idle channel.
   type IDLE_CH_Register is record
      --  unspecified
      Reserved_0_3  : HAL.UInt4;
      --  Read-only. Idle channel. Reading the CHAN bits, returns the lowest
      --  idle timer channel. The number is positioned such that it can be used
      --  as an offset from the MRT base address in order to access the
      --  registers for the allocated channel. If all timer channels are
      --  running, CHAN = 0xF. See text above for more details.
      CHAN          : IDLE_CH_CHAN_Field;
      --  unspecified
      Reserved_8_31 : HAL.UInt24;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IDLE_CH_Register use record
      Reserved_0_3  at 0 range 0 .. 3;
      CHAN          at 0 range 4 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  Monitors the interrupt flag of TIMER0.
   type IRQ_FLAG_GFLAG0_Field is
     (
      --  No pending interrupt. Writing a zero is equivalent to no operation.
      No_Pending_Interrupt,
      --  Pending interrupt. The interrupt is pending because TIMER0 has
      --  reached the end of the time interval. If the INTEN bit in the
      --  CONTROL0 register is also set to 1, the interrupt for timer channel 0
      --  and the global interrupt are raised. Writing a 1 to this bit clears
      --  the interrupt request.
      Pending_Interrupt)
     with Size => 1;
   for IRQ_FLAG_GFLAG0_Field use
     (No_Pending_Interrupt => 0,
      Pending_Interrupt => 1);

   --  IRQ_FLAG_GFLAG array
   type IRQ_FLAG_GFLAG_Field_Array is array (1 .. 3) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for IRQ_FLAG_GFLAG
   type IRQ_FLAG_GFLAG_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  GFLAG as a value
            Val : HAL.UInt3;
         when True =>
            --  GFLAG as an array
            Arr : IRQ_FLAG_GFLAG_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for IRQ_FLAG_GFLAG_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  Global interrupt flag register
   type IRQ_FLAG_Register is record
      --  Monitors the interrupt flag of TIMER0.
      GFLAG0        : IRQ_FLAG_GFLAG0_Field :=
                       NXP_SVD.MRT.No_Pending_Interrupt;
      --  Monitors the interrupt flag of TIMER1. See description of channel 0.
      GFLAG         : IRQ_FLAG_GFLAG_Field :=
                       (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IRQ_FLAG_Register use record
      GFLAG0        at 0 range 0 .. 0;
      GFLAG         at 0 range 1 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Multi-Rate Timer (MRT)
   type MRT0_Peripheral is record
      --  no description available
      CHANNEL  : aliased CHANNEL_Clusters;
      --  Module Configuration register. This register provides information
      --  about this particular MRT instance, and allows choosing an overall
      --  mode for the idle channel feature.
      MODCFG   : aliased MODCFG_Register;
      --  Idle channel register. This register returns the number of the first
      --  idle channel.
      IDLE_CH  : aliased IDLE_CH_Register;
      --  Global interrupt flag register
      IRQ_FLAG : aliased IRQ_FLAG_Register;
   end record
     with Volatile;

   for MRT0_Peripheral use record
      CHANNEL  at 16#0# range 0 .. 511;
      MODCFG   at 16#F0# range 0 .. 31;
      IDLE_CH  at 16#F4# range 0 .. 31;
      IRQ_FLAG at 16#F8# range 0 .. 31;
   end record;

   --  Multi-Rate Timer (MRT)
   MRT0_Periph : aliased MRT0_Peripheral
     with Import, Address => System'To_Address (16#4000D000#);

end NXP_SVD.MRT;
