--  Copyright 2016-2019 NXP
--  All rights reserved.SPDX-License-Identifier: BSD-3-Clause


--  This spec has been automatically generated from LPC55S6x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NXP_SVD.DMA is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  DMA controller master enable.
   type CTRL_ENABLE_Field is
     (
      --  Disabled. The DMA controller is disabled. This clears any triggers
      --  that were asserted at the point when disabled, but does not prevent
      --  re-triggering when the DMA controller is re-enabled.
      Disabled,
      --  Enabled. The DMA controller is enabled.
      Enabled)
     with Size => 1;
   for CTRL_ENABLE_Field use
     (Disabled => 0,
      Enabled => 1);

   --  DMA control.
   type CTRL_Register is record
      --  DMA controller master enable.
      ENABLE        : CTRL_ENABLE_Field := NXP_SVD.DMA.Disabled;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CTRL_Register use record
      ENABLE        at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Summarizes whether any enabled interrupts (other than error interrupts)
   --  are pending.
   type INTSTAT_ACTIVEINT_Field is
     (
      --  Not pending. No enabled interrupts are pending.
      Not_Pending,
      --  Pending. At least one enabled interrupt is pending.
      Pending)
     with Size => 1;
   for INTSTAT_ACTIVEINT_Field use
     (Not_Pending => 0,
      Pending => 1);

   --  Summarizes whether any error interrupts are pending.
   type INTSTAT_ACTIVEERRINT_Field is
     (
      --  Not pending. No error interrupts are pending.
      Not_Pending,
      --  Pending. At least one error interrupt is pending.
      Pending)
     with Size => 1;
   for INTSTAT_ACTIVEERRINT_Field use
     (Not_Pending => 0,
      Pending => 1);

   --  Interrupt status.
   type INTSTAT_Register is record
      --  unspecified
      Reserved_0_0  : HAL.Bit;
      --  Read-only. Summarizes whether any enabled interrupts (other than
      --  error interrupts) are pending.
      ACTIVEINT     : INTSTAT_ACTIVEINT_Field;
      --  Read-only. Summarizes whether any error interrupts are pending.
      ACTIVEERRINT  : INTSTAT_ACTIVEERRINT_Field;
      --  unspecified
      Reserved_3_31 : HAL.UInt29;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTSTAT_Register use record
      Reserved_0_0  at 0 range 0 .. 0;
      ACTIVEINT     at 0 range 1 .. 1;
      ACTIVEERRINT  at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype SRAMBASE_OFFSET_Field is HAL.UInt23;

   --  SRAM address of the channel configuration table.
   type SRAMBASE_Register is record
      --  unspecified
      Reserved_0_8 : HAL.UInt9 := 16#0#;
      --  Address bits 31:9 of the beginning of the DMA descriptor table. For
      --  18 channels, the table must begin on a 512 byte boundary.
      OFFSET       : SRAMBASE_OFFSET_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SRAMBASE_Register use record
      Reserved_0_8 at 0 range 0 .. 8;
      OFFSET       at 0 range 9 .. 31;
   end record;

   ---------------------------------
   -- CHANNEL cluster's Registers --
   ---------------------------------

   --  Peripheral request Enable. If a DMA channel is used to perform a
   --  memory-to-memory move, any peripheral DMA request associated with that
   --  channel can be disabled to prevent any interaction between the
   --  peripheral and the DMA controller.
   type CFG_PERIPHREQEN_Field is
     (
      --  Disabled. Peripheral DMA requests are disabled.
      Disabled,
      --  Enabled. Peripheral DMA requests are enabled.
      Enabled)
     with Size => 1;
   for CFG_PERIPHREQEN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Hardware Triggering Enable for this channel.
   type CFG_HWTRIGEN_Field is
     (
      --  Disabled. Hardware triggering is not used.
      Disabled,
      --  Enabled. Use hardware triggering.
      Enabled)
     with Size => 1;
   for CFG_HWTRIGEN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Trigger Polarity. Selects the polarity of a hardware trigger for this
   --  channel.
   type CFG_TRIGPOL_Field is
     (
      --  Active low - falling edge. Hardware trigger is active low or falling
      --  edge triggered, based on TRIGTYPE.
      Active_Low_Falling,
      --  Active high - rising edge. Hardware trigger is active high or rising
      --  edge triggered, based on TRIGTYPE.
      Active_High_Rising)
     with Size => 1;
   for CFG_TRIGPOL_Field use
     (Active_Low_Falling => 0,
      Active_High_Rising => 1);

   --  Trigger Type. Selects hardware trigger as edge triggered or level
   --  triggered.
   type CFG_TRIGTYPE_Field is
     (
      --  Edge. Hardware trigger is edge triggered. Transfers will be initiated
      --  and completed, as specified for a single trigger.
      Edge,
      --  Level. Hardware trigger is level triggered. Note that when level
      --  triggering without burst (BURSTPOWER = 0) is selected, only hardware
      --  triggers should be used on that channel. Transfers continue as long
      --  as the trigger level is asserted. Once the trigger is de-asserted,
      --  the transfer will be paused until the trigger is, again, asserted.
      --  However, the transfer will not be paused until any remaining
      --  transfers within the current BURSTPOWER length are completed.
      Level)
     with Size => 1;
   for CFG_TRIGTYPE_Field use
     (Edge => 0,
      Level => 1);

   --  Trigger Burst. Selects whether hardware triggers cause a single or burst
   --  transfer.
   type CFG_TRIGBURST_Field is
     (
      --  Single transfer. Hardware trigger causes a single transfer.
      Single,
      --  Burst transfer. When the trigger for this channel is set to edge
      --  triggered, a hardware trigger causes a burst transfer, as defined by
      --  BURSTPOWER. When the trigger for this channel is set to level
      --  triggered, a hardware trigger causes transfers to continue as long as
      --  the trigger is asserted, unless the transfer is complete.
      Burst)
     with Size => 1;
   for CFG_TRIGBURST_Field use
     (Single => 0,
      Burst => 1);

   subtype CFG_CHANNEL_BURSTPOWER_Field is HAL.UInt4;

   --  Source Burst Wrap. When enabled, the source data address for the DMA is
   --  'wrapped', meaning that the source address range for each burst will be
   --  the same. As an example, this could be used to read several sequential
   --  registers from a peripheral for each DMA burst, reading the same
   --  registers again for each burst.
   type CFG_SRCBURSTWRAP_Field is
     (
      --  Disabled. Source burst wrapping is not enabled for this DMA channel.
      Disabled,
      --  Enabled. Source burst wrapping is enabled for this DMA channel.
      Enabled)
     with Size => 1;
   for CFG_SRCBURSTWRAP_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Destination Burst Wrap. When enabled, the destination data address for
   --  the DMA is 'wrapped', meaning that the destination address range for
   --  each burst will be the same. As an example, this could be used to write
   --  several sequential registers to a peripheral for each DMA burst, writing
   --  the same registers again for each burst.
   type CFG_DSTBURSTWRAP_Field is
     (
      --  Disabled. Destination burst wrapping is not enabled for this DMA
      --  channel.
      Disabled,
      --  Enabled. Destination burst wrapping is enabled for this DMA channel.
      Enabled)
     with Size => 1;
   for CFG_DSTBURSTWRAP_Field use
     (Disabled => 0,
      Enabled => 1);

   subtype CFG_CHANNEL_CHPRIORITY_Field is HAL.UInt3;

   --  Configuration register for DMA channel .
   type CFG_CHANNEL_Register is record
      --  Peripheral request Enable. If a DMA channel is used to perform a
      --  memory-to-memory move, any peripheral DMA request associated with
      --  that channel can be disabled to prevent any interaction between the
      --  peripheral and the DMA controller.
      PERIPHREQEN    : CFG_PERIPHREQEN_Field := NXP_SVD.DMA.Disabled;
      --  Hardware Triggering Enable for this channel.
      HWTRIGEN       : CFG_HWTRIGEN_Field := NXP_SVD.DMA.Disabled;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  Trigger Polarity. Selects the polarity of a hardware trigger for this
      --  channel.
      TRIGPOL        : CFG_TRIGPOL_Field := NXP_SVD.DMA.Active_Low_Falling;
      --  Trigger Type. Selects hardware trigger as edge triggered or level
      --  triggered.
      TRIGTYPE       : CFG_TRIGTYPE_Field := NXP_SVD.DMA.Edge;
      --  Trigger Burst. Selects whether hardware triggers cause a single or
      --  burst transfer.
      TRIGBURST      : CFG_TRIGBURST_Field := NXP_SVD.DMA.Single;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Burst Power is used in two ways. It always selects the address wrap
      --  size when SRCBURSTWRAP and/or DSTBURSTWRAP modes are selected (see
      --  descriptions elsewhere in this register). When the TRIGBURST field
      --  elsewhere in this register = 1, Burst Power selects how many
      --  transfers are performed for each DMA trigger. This can be used, for
      --  example, with peripherals that contain a FIFO that can initiate a DMA
      --  operation when the FIFO reaches a certain level. 0000: Burst size = 1
      --  (20). 0001: Burst size = 2 (21). 0010: Burst size = 4 (22). 1010:
      --  Burst size = 1024 (210). This corresponds to the maximum supported
      --  transfer count. others: not supported. The total transfer length as
      --  defined in the XFERCOUNT bits in the XFERCFG register must be an even
      --  multiple of the burst size.
      BURSTPOWER     : CFG_CHANNEL_BURSTPOWER_Field := 16#0#;
      --  unspecified
      Reserved_12_13 : HAL.UInt2 := 16#0#;
      --  Source Burst Wrap. When enabled, the source data address for the DMA
      --  is 'wrapped', meaning that the source address range for each burst
      --  will be the same. As an example, this could be used to read several
      --  sequential registers from a peripheral for each DMA burst, reading
      --  the same registers again for each burst.
      SRCBURSTWRAP   : CFG_SRCBURSTWRAP_Field := NXP_SVD.DMA.Disabled;
      --  Destination Burst Wrap. When enabled, the destination data address
      --  for the DMA is 'wrapped', meaning that the destination address range
      --  for each burst will be the same. As an example, this could be used to
      --  write several sequential registers to a peripheral for each DMA
      --  burst, writing the same registers again for each burst.
      DSTBURSTWRAP   : CFG_DSTBURSTWRAP_Field := NXP_SVD.DMA.Disabled;
      --  Priority of this channel when multiple DMA requests are pending.
      --  Eight priority levels are supported: 0x0 = highest priority. 0x7 =
      --  lowest priority.
      CHPRIORITY     : CFG_CHANNEL_CHPRIORITY_Field := 16#0#;
      --  unspecified
      Reserved_19_31 : HAL.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CFG_CHANNEL_Register use record
      PERIPHREQEN    at 0 range 0 .. 0;
      HWTRIGEN       at 0 range 1 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      TRIGPOL        at 0 range 4 .. 4;
      TRIGTYPE       at 0 range 5 .. 5;
      TRIGBURST      at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      BURSTPOWER     at 0 range 8 .. 11;
      Reserved_12_13 at 0 range 12 .. 13;
      SRCBURSTWRAP   at 0 range 14 .. 14;
      DSTBURSTWRAP   at 0 range 15 .. 15;
      CHPRIORITY     at 0 range 16 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   --  Valid pending flag for this channel. This bit is set when a 1 is written
   --  to the corresponding bit in the related SETVALID register when CFGVALID
   --  = 1 for the same channel.
   type CTLSTAT_VALIDPENDING_Field is
     (
      --  No effect. No effect on DMA operation.
      No_Effect,
      --  Valid pending.
      Valid_Pending)
     with Size => 1;
   for CTLSTAT_VALIDPENDING_Field use
     (No_Effect => 0,
      Valid_Pending => 1);

   --  Trigger flag. Indicates that the trigger for this channel is currently
   --  set. This bit is cleared at the end of an entire transfer or upon reload
   --  when CLRTRIG = 1.
   type CTLSTAT_TRIG_Field is
     (
      --  Not triggered. The trigger for this DMA channel is not set. DMA
      --  operations will not be carried out.
      Not_Triggered,
      --  Triggered. The trigger for this DMA channel is set. DMA operations
      --  will be carried out.
      Triggered)
     with Size => 1;
   for CTLSTAT_TRIG_Field use
     (Not_Triggered => 0,
      Triggered => 1);

   --  Control and status register for DMA channel .
   type CTLSTAT_CHANNEL_Register is record
      --  Read-only. Valid pending flag for this channel. This bit is set when
      --  a 1 is written to the corresponding bit in the related SETVALID
      --  register when CFGVALID = 1 for the same channel.
      VALIDPENDING  : CTLSTAT_VALIDPENDING_Field;
      --  unspecified
      Reserved_1_1  : HAL.Bit;
      --  Read-only. Trigger flag. Indicates that the trigger for this channel
      --  is currently set. This bit is cleared at the end of an entire
      --  transfer or upon reload when CLRTRIG = 1.
      TRIG          : CTLSTAT_TRIG_Field;
      --  unspecified
      Reserved_3_31 : HAL.UInt29;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CTLSTAT_CHANNEL_Register use record
      VALIDPENDING  at 0 range 0 .. 0;
      Reserved_1_1  at 0 range 1 .. 1;
      TRIG          at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  Configuration Valid flag. This bit indicates whether the current channel
   --  descriptor is valid and can potentially be acted upon, if all other
   --  activation criteria are fulfilled.
   type XFERCFG_CFGVALID_Field is
     (
      --  Not valid. The channel descriptor is not considered valid until
      --  validated by an associated SETVALID0 setting.
      Not_Valid,
      --  Valid. The current channel descriptor is considered valid.
      Valid)
     with Size => 1;
   for XFERCFG_CFGVALID_Field use
     (Not_Valid => 0,
      Valid => 1);

   --  Indicates whether the channel's control structure will be reloaded when
   --  the current descriptor is exhausted. Reloading allows ping-pong and
   --  linked transfers.
   type XFERCFG_RELOAD_Field is
     (
      --  Disabled. Do not reload the channels' control structure when the
      --  current descriptor is exhausted.
      Disabled,
      --  Enabled. Reload the channels' control structure when the current
      --  descriptor is exhausted.
      Enabled)
     with Size => 1;
   for XFERCFG_RELOAD_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Software Trigger.
   type XFERCFG_SWTRIG_Field is
     (
      --  Not set. When written by software, the trigger for this channel is
      --  not set. A new trigger, as defined by the HWTRIGEN, TRIGPOL, and
      --  TRIGTYPE will be needed to start the channel.
      Not_Set,
      --  Set. When written by software, the trigger for this channel is set
      --  immediately. This feature should not be used with level triggering
      --  when TRIGBURST = 0.
      Set)
     with Size => 1;
   for XFERCFG_SWTRIG_Field use
     (Not_Set => 0,
      Set => 1);

   --  Clear Trigger.
   type XFERCFG_CLRTRIG_Field is
     (
      --  Not cleared. The trigger is not cleared when this descriptor is
      --  exhausted. If there is a reload, the next descriptor will be started.
      Not_Cleared,
      --  Cleared. The trigger is cleared when this descriptor is exhausted
      Cleared)
     with Size => 1;
   for XFERCFG_CLRTRIG_Field use
     (Not_Cleared => 0,
      Cleared => 1);

   --  Set Interrupt flag A for this channel. There is no hardware distinction
   --  between interrupt A and B. They can be used by software to assist with
   --  more complex descriptor usage. By convention, interrupt A may be used
   --  when only one interrupt flag is needed.
   type XFERCFG_SETINTA_Field is
     (
      --  No effect.
      No_Effect,
      --  Set. The INTA flag for this channel will be set when the current
      --  descriptor is exhausted.
      Set)
     with Size => 1;
   for XFERCFG_SETINTA_Field use
     (No_Effect => 0,
      Set => 1);

   --  Set Interrupt flag B for this channel. There is no hardware distinction
   --  between interrupt A and B. They can be used by software to assist with
   --  more complex descriptor usage. By convention, interrupt A may be used
   --  when only one interrupt flag is needed.
   type XFERCFG_SETINTB_Field is
     (
      --  No effect.
      No_Effect,
      --  Set. The INTB flag for this channel will be set when the current
      --  descriptor is exhausted.
      Set)
     with Size => 1;
   for XFERCFG_SETINTB_Field use
     (No_Effect => 0,
      Set => 1);

   --  Transfer width used for this DMA channel.
   type XFERCFG_WIDTH_Field is
     (
      --  8-bit. 8-bit transfers are performed (8-bit source reads and
      --  destination writes).
      Bit_8,
      --  16-bit. 6-bit transfers are performed (16-bit source reads and
      --  destination writes).
      Bit_16,
      --  32-bit. 32-bit transfers are performed (32-bit source reads and
      --  destination writes).
      Bit_32)
     with Size => 2;
   for XFERCFG_WIDTH_Field use
     (Bit_8 => 0,
      Bit_16 => 1,
      Bit_32 => 2);

   --  Determines whether the source address is incremented for each DMA
   --  transfer.
   type XFERCFG_SRCINC_Field is
     (
      --  No increment. The source address is not incremented for each
      --  transfer. This is the usual case when the source is a peripheral
      --  device.
      No_Increment,
      --  1 x width. The source address is incremented by the amount specified
      --  by Width for each transfer. This is the usual case when the source is
      --  memory.
      Width_X_1,
      --  2 x width. The source address is incremented by 2 times the amount
      --  specified by Width for each transfer.
      Width_X_2,
      --  4 x width. The source address is incremented by 4 times the amount
      --  specified by Width for each transfer.
      Width_X_4)
     with Size => 2;
   for XFERCFG_SRCINC_Field use
     (No_Increment => 0,
      Width_X_1 => 1,
      Width_X_2 => 2,
      Width_X_4 => 3);

   --  Determines whether the destination address is incremented for each DMA
   --  transfer.
   type XFERCFG_DSTINC_Field is
     (
      --  No increment. The destination address is not incremented for each
      --  transfer. This is the usual case when the destination is a peripheral
      --  device.
      No_Increment,
      --  1 x width. The destination address is incremented by the amount
      --  specified by Width for each transfer. This is the usual case when the
      --  destination is memory.
      Width_X_1,
      --  2 x width. The destination address is incremented by 2 times the
      --  amount specified by Width for each transfer.
      Width_X_2,
      --  4 x width. The destination address is incremented by 4 times the
      --  amount specified by Width for each transfer.
      Width_X_4)
     with Size => 2;
   for XFERCFG_DSTINC_Field use
     (No_Increment => 0,
      Width_X_1 => 1,
      Width_X_2 => 2,
      Width_X_4 => 3);

   subtype XFERCFG_CHANNEL_XFERCOUNT_Field is HAL.UInt10;

   --  Transfer configuration register for DMA channel .
   type XFERCFG_CHANNEL_Register is record
      --  Configuration Valid flag. This bit indicates whether the current
      --  channel descriptor is valid and can potentially be acted upon, if all
      --  other activation criteria are fulfilled.
      CFGVALID       : XFERCFG_CFGVALID_Field := NXP_SVD.DMA.Not_Valid;
      --  Indicates whether the channel's control structure will be reloaded
      --  when the current descriptor is exhausted. Reloading allows ping-pong
      --  and linked transfers.
      RELOAD         : XFERCFG_RELOAD_Field := NXP_SVD.DMA.Disabled;
      --  Software Trigger.
      SWTRIG         : XFERCFG_SWTRIG_Field := NXP_SVD.DMA.Not_Set;
      --  Clear Trigger.
      CLRTRIG        : XFERCFG_CLRTRIG_Field := NXP_SVD.DMA.Not_Cleared;
      --  Set Interrupt flag A for this channel. There is no hardware
      --  distinction between interrupt A and B. They can be used by software
      --  to assist with more complex descriptor usage. By convention,
      --  interrupt A may be used when only one interrupt flag is needed.
      SETINTA        : XFERCFG_SETINTA_Field := NXP_SVD.DMA.No_Effect;
      --  Set Interrupt flag B for this channel. There is no hardware
      --  distinction between interrupt A and B. They can be used by software
      --  to assist with more complex descriptor usage. By convention,
      --  interrupt A may be used when only one interrupt flag is needed.
      SETINTB        : XFERCFG_SETINTB_Field := NXP_SVD.DMA.No_Effect;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Transfer width used for this DMA channel.
      WIDTH          : XFERCFG_WIDTH_Field := NXP_SVD.DMA.Bit_8;
      --  unspecified
      Reserved_10_11 : HAL.UInt2 := 16#0#;
      --  Determines whether the source address is incremented for each DMA
      --  transfer.
      SRCINC         : XFERCFG_SRCINC_Field := NXP_SVD.DMA.No_Increment;
      --  Determines whether the destination address is incremented for each
      --  DMA transfer.
      DSTINC         : XFERCFG_DSTINC_Field := NXP_SVD.DMA.No_Increment;
      --  Total number of transfers to be performed, minus 1 encoded. The
      --  number of bytes transferred is: (XFERCOUNT + 1) x data width (as
      --  defined by the WIDTH field). The DMA controller uses this bit field
      --  during transfer to count down. Hence, it cannot be used by software
      --  to read back the size of the transfer, for instance, in an interrupt
      --  handler. 0x0 = a total of 1 transfer will be performed. 0x1 = a total
      --  of 2 transfers will be performed. 0x3FF = a total of 1,024 transfers
      --  will be performed.
      XFERCOUNT      : XFERCFG_CHANNEL_XFERCOUNT_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for XFERCFG_CHANNEL_Register use record
      CFGVALID       at 0 range 0 .. 0;
      RELOAD         at 0 range 1 .. 1;
      SWTRIG         at 0 range 2 .. 2;
      CLRTRIG        at 0 range 3 .. 3;
      SETINTA        at 0 range 4 .. 4;
      SETINTB        at 0 range 5 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      WIDTH          at 0 range 8 .. 9;
      Reserved_10_11 at 0 range 10 .. 11;
      SRCINC         at 0 range 12 .. 13;
      DSTINC         at 0 range 14 .. 15;
      XFERCOUNT      at 0 range 16 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   --  no description available
   type CHANNEL_Cluster is record
      --  Configuration register for DMA channel .
      CFG     : aliased CFG_CHANNEL_Register;
      --  Control and status register for DMA channel .
      CTLSTAT : aliased CTLSTAT_CHANNEL_Register;
      --  Transfer configuration register for DMA channel .
      XFERCFG : aliased XFERCFG_CHANNEL_Register;
   end record
     with Volatile, Size => 128;

   for CHANNEL_Cluster use record
      CFG     at 16#0# range 0 .. 31;
      CTLSTAT at 16#4# range 0 .. 31;
      XFERCFG at 16#8# range 0 .. 31;
   end record;

   --  no description available
   type CHANNEL_Clusters is array (0 .. 22) of CHANNEL_Cluster;

   ---------------------------------
   -- CHANNEL cluster's Registers --
   ---------------------------------

   -----------------
   -- Peripherals --
   -----------------

   --  DMA controller
   type DMA_Peripheral is record
      --  DMA control.
      CTRL       : aliased CTRL_Register;
      --  Interrupt status.
      INTSTAT    : aliased INTSTAT_Register;
      --  SRAM address of the channel configuration table.
      SRAMBASE   : aliased SRAMBASE_Register;
      --  Channel Enable read and Set for all DMA channels.
      ENABLESET0 : aliased HAL.UInt32;
      --  Channel Enable Clear for all DMA channels.
      ENABLECLR0 : aliased HAL.UInt32;
      --  Channel Active status for all DMA channels.
      ACTIVE0    : aliased HAL.UInt32;
      --  Channel Busy status for all DMA channels.
      BUSY0      : aliased HAL.UInt32;
      --  Error Interrupt status for all DMA channels.
      ERRINT0    : aliased HAL.UInt32;
      --  Interrupt Enable read and Set for all DMA channels.
      INTENSET0  : aliased HAL.UInt32;
      --  Interrupt Enable Clear for all DMA channels.
      INTENCLR0  : aliased HAL.UInt32;
      --  Interrupt A status for all DMA channels.
      INTA0      : aliased HAL.UInt32;
      --  Interrupt B status for all DMA channels.
      INTB0      : aliased HAL.UInt32;
      --  Set ValidPending control bits for all DMA channels.
      SETVALID0  : aliased HAL.UInt32;
      --  Set Trigger control bits for all DMA channels.
      SETTRIG0   : aliased HAL.UInt32;
      --  Channel Abort control for all DMA channels.
      ABORT0     : aliased HAL.UInt32;
      --  no description available
      CHANNEL    : aliased CHANNEL_Clusters;
   end record
     with Volatile;

   for DMA_Peripheral use record
      CTRL       at 16#0# range 0 .. 31;
      INTSTAT    at 16#4# range 0 .. 31;
      SRAMBASE   at 16#8# range 0 .. 31;
      ENABLESET0 at 16#20# range 0 .. 31;
      ENABLECLR0 at 16#28# range 0 .. 31;
      ACTIVE0    at 16#30# range 0 .. 31;
      BUSY0      at 16#38# range 0 .. 31;
      ERRINT0    at 16#40# range 0 .. 31;
      INTENSET0  at 16#48# range 0 .. 31;
      INTENCLR0  at 16#50# range 0 .. 31;
      INTA0      at 16#58# range 0 .. 31;
      INTB0      at 16#60# range 0 .. 31;
      SETVALID0  at 16#68# range 0 .. 31;
      SETTRIG0   at 16#70# range 0 .. 31;
      ABORT0     at 16#78# range 0 .. 31;
      CHANNEL    at 16#400# range 0 .. 2943;
   end record;

   --  DMA controller
   DMA0_Periph : aliased DMA_Peripheral
     with Import, Address => System'To_Address (16#40082000#);

   --  DMA controller
   DMA1_Periph : aliased DMA_Peripheral
     with Import, Address => System'To_Address (16#400A7000#);

end NXP_SVD.DMA;
