--  This spec has been automatically generated from STM32U073.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.IWDG is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype IWDG_KR_KEY_Field is HAL.UInt16;

   --  IWDG key register
   type IWDG_KR_Register is record
      --  Write-only. Key value (write only, read 0x0000) These bits can be
      --  used for several functions, depending upon the value written by the
      --  application: - 0xAAAA: reloads the RL[11:0] value into the IWDCNT
      --  down-counter (watchdog refresh), and write-protects registers. This
      --  value must be written by software at regular intervals, otherwise the
      --  watchdog generates a reset when the counter reaches 0. - 0x5555:
      --  enables write-accesses to the registers. - 0xCCCC: enables the
      --  watchdog (except if the hardware watchdog option is selected) and
      --  write-protects registers. - values different from 0x5555:
      --  write-protects registers. Note that only IWDG_PR, IWDG_RLR, IWDG_EWCR
      --  and IWDG_WINR registers have a write-protection mechanism.
      KEY            : IWDG_KR_KEY_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IWDG_KR_Register use record
      KEY            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype IWDG_PR_PR_Field is HAL.UInt4;

   --  IWDG prescaler register
   type IWDG_PR_Register is record
      --  Prescaler divider These bits are write access protected, see
      --  Section126.4.6. They are written by software to select the prescaler
      --  divider feeding the counter clock. PVU bit of the IWDG status
      --  register (IWDG_SR) must be reset to be able to change the prescaler
      --  divider. Others: divider / 1024 Note: Reading this register returns
      --  the prescaler value from the V<sub>DD</sub> voltage domain. This
      --  value may not be up to date/valid if a write operation to this
      --  register is ongoing. For this reason the value read from this
      --  register is valid only when the PVU bit in the IWDG status register
      --  (IWDG_SR) is reset.
      PR            : IWDG_PR_PR_Field := 16#0#;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IWDG_PR_Register use record
      PR            at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   subtype IWDG_RLR_RL_Field is HAL.UInt12;

   --  IWDG reload register
   type IWDG_RLR_Register is record
      --  Watchdog counter reload value These bits are write access protected,
      --  see Section126.4.6. They are written by software to define the value
      --  to be loaded in the watchdog counter each time the value 0xAAAA is
      --  written in the IWDG key register (IWDG_KR). The watchdog counter
      --  counts down from this value. The timeout period is a function of this
      --  value and the prescaler.clock. It is not recommended to set RL[11:0]
      --  to a value lower than 2. The RVU bit in the IWDG status register
      --  (IWDG_SR) must be reset to be able to change the reload value. Note:
      --  Reading this register returns the reload value from the
      --  V<sub>DD</sub> voltage domain. This value may not be up to date/valid
      --  if a write operation to this register is ongoing, hence the value
      --  read from this register is valid only when the RVU bit in the IWDG
      --  status register (IWDG_SR) is reset.
      RL             : IWDG_RLR_RL_Field := 16#FFF#;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IWDG_RLR_Register use record
      RL             at 0 range 0 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   --  IWDG status register
   type IWDG_SR_Register is record
      --  Read-only. Watchdog prescaler value update This bit is set by
      --  hardware to indicate that an update of the prescaler value is
      --  ongoing. It is reset by hardware when the prescaler update operation
      --  is completed in the V<sub>DD</sub> voltage domain (takes up to six
      --  periods of the IWDG kernel clock iwdg_ker_ck). The prescaler value
      --  can be updated only when PVU bit is reset.
      PVU            : Boolean;
      --  Read-only. Watchdog counter reload value update This bit is set by
      --  hardware to indicate that an update of the reload value is ongoing.
      --  It is reset by hardware when the reload value update operation is
      --  completed in the V<sub>DD</sub> voltage domain (takes up to six
      --  periods of the IWDG kernel clock iwdg_ker_ck). The reload value can
      --  be updated only when RVU bit is reset.
      RVU            : Boolean;
      --  Read-only. Watchdog counter window value update This bit is set by
      --  hardware to indicate that an update of the window value is ongoing.
      --  It is reset by hardware when the reload value update operation is
      --  completed in the V<sub>DD</sub> voltage domain (takes up to one
      --  period of presc_ck and two periods of the IWDG kernel clock
      --  iwdg_ker_ck). The window value can be updated only when WVU bit is
      --  reset. This bit is generated only if generic window = 1.
      WVU            : Boolean;
      --  Read-only. Watchdog interrupt comparator value update This bit is set
      --  by hardware to indicate that an update of the interrupt comparator
      --  value (EWIT[11:0]) or an update of the EWIE is ongoing. It is reset
      --  by hardware when the update operation is completed in the
      --  V<sub>DD</sub> voltage domain (takes up to one period of presc_ck and
      --  two periods of the IWDG kernel clock iwdg_ker_ck). The EWIT[11:0] and
      --  EWIE fields can be updated only when EWU bit is reset.
      EWU            : Boolean;
      --  unspecified
      Reserved_4_7   : HAL.UInt4;
      --  Read-only. Watchdog enable status bit Set to 1 by hardware as soon as
      --  the IWDG is started. In software mode, it remains to '1' until the
      --  IWDG is reset. In hardware mode, this bit is always set to '1'.
      ONF            : Boolean;
      --  unspecified
      Reserved_9_13  : HAL.UInt5;
      --  Read-only. Watchdog early interrupt flag This bit is set to 1 by
      --  hardware in order to indicate that an early interrupt is pending.
      --  This bit must be cleared by the software by writing the bit EWIC of
      --  IWDG_EWCR register to 1.
      EWIF           : Boolean;
      --  unspecified
      Reserved_15_31 : HAL.UInt17;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IWDG_SR_Register use record
      PVU            at 0 range 0 .. 0;
      RVU            at 0 range 1 .. 1;
      WVU            at 0 range 2 .. 2;
      EWU            at 0 range 3 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      ONF            at 0 range 8 .. 8;
      Reserved_9_13  at 0 range 9 .. 13;
      EWIF           at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype IWDG_WINR_WIN_Field is HAL.UInt12;

   --  IWDG window register
   type IWDG_WINR_Register is record
      --  Watchdog counter window value These bits are write access protected,
      --  see Section126.4.6.They contain the high limit of the window value to
      --  be compared with the downcounter. To prevent a reset, the IWDCNT
      --  downcounter must be reloaded when its value is lower than
      --  WIN[11:0]1+11 and greater than 1. The WVU bit in the IWDG status
      --  register (IWDG_SR) must be reset to be able to change the reload
      --  value. Note: Reading this register returns the reload value from the
      --  V<sub>DD</sub> voltage domain. This value may not be valid if a write
      --  operation to this register is ongoing. For this reason the value read
      --  from this register is valid only when the WVU bit in the IWDG status
      --  register (IWDG_SR) is reset.
      WIN            : IWDG_WINR_WIN_Field := 16#FFF#;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IWDG_WINR_Register use record
      WIN            at 0 range 0 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype IWDG_EWCR_EWIT_Field is HAL.UInt12;

   --  IWDG early wake-up interrupt register
   type IWDG_EWCR_Register is record
      --  Watchdog counter window value These bits are write access protected
      --  (see Section126.4.6). They are written by software to define at which
      --  position of the IWDCNT down-counter the early wake-up interrupt must
      --  be generated. The early interrupt is generated when the IWDCNT is
      --  lower or equal to EWIT[11:0]1-11. EWIT[11:0] must be bigger than 1.
      --  An interrupt is generated only if EWIE = 1. The EWU bit in the IWDG
      --  status register (IWDG_SR) must be reset to be able to change the
      --  reload value. Note: Reading this register returns the Early wake-up
      --  comparator value and the Interrupt enable bit from the V<sub>DD</sub>
      --  voltage domain. This value may not be up to date/valid if a write
      --  operation to this register is ongoing, hence the value read from this
      --  register is valid only when the EWU bit in the IWDG status register
      --  (IWDG_SR) is reset.
      EWIT           : IWDG_EWCR_EWIT_Field := 16#0#;
      --  unspecified
      Reserved_12_13 : HAL.UInt2 := 16#0#;
      --  Write-only. Watchdog early interrupt acknowledge The software must
      --  write a 1 into this bit in order to acknowledge the early wake-up
      --  interrupt and to clear the EWIF flag. Writing 0 has not effect,
      --  reading this flag returns a 0.
      EWIC           : Boolean := False;
      --  Watchdog early interrupt enable Set and reset by software. The EWU
      --  bit in the IWDG status register (IWDG_SR) must be reset to be able to
      --  change the value of this bit.
      EWIE           : Boolean := False;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IWDG_EWCR_Register use record
      EWIT           at 0 range 0 .. 11;
      Reserved_12_13 at 0 range 12 .. 13;
      EWIC           at 0 range 14 .. 14;
      EWIE           at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  IWDG address block description
   type IWDG_Peripheral is record
      --  IWDG key register
      IWDG_KR   : aliased IWDG_KR_Register;
      --  IWDG prescaler register
      IWDG_PR   : aliased IWDG_PR_Register;
      --  IWDG reload register
      IWDG_RLR  : aliased IWDG_RLR_Register;
      --  IWDG status register
      IWDG_SR   : aliased IWDG_SR_Register;
      --  IWDG window register
      IWDG_WINR : aliased IWDG_WINR_Register;
      --  IWDG early wake-up interrupt register
      IWDG_EWCR : aliased IWDG_EWCR_Register;
   end record
     with Volatile;

   for IWDG_Peripheral use record
      IWDG_KR   at 16#0# range 0 .. 31;
      IWDG_PR   at 16#4# range 0 .. 31;
      IWDG_RLR  at 16#8# range 0 .. 31;
      IWDG_SR   at 16#C# range 0 .. 31;
      IWDG_WINR at 16#10# range 0 .. 31;
      IWDG_EWCR at 16#14# range 0 .. 31;
   end record;

   --  IWDG address block description
   IWDG_Periph : aliased IWDG_Peripheral
     with Import, Address => IWDG_Base;

end STM32_SVD.IWDG;
