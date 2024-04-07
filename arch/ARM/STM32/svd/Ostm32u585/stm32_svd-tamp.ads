--  This spec has been automatically generated from STM32U5xx.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.TAMP is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  TAMP control register 1
   type TAMP_CR1_Register is record
      --  Tamper detection on TAMP_IN1 enable
      TAMP1E         : Boolean := False;
      --  Tamper detection on TAMP_IN2 enable
      TAMP2E         : Boolean := False;
      --  Tamper detection on TAMP_IN3 enable
      TAMP3E         : Boolean := False;
      --  Tamper detection on TAMP_IN4 enable
      TAMP4E         : Boolean := False;
      --  Tamper detection on TAMP_IN5 enable
      TAMP5E         : Boolean := False;
      --  Tamper detection on TAMP_IN6 enable
      TAMP6E         : Boolean := False;
      --  Tamper detection on TAMP_IN7 enable
      TAMP7E         : Boolean := False;
      --  Tamper detection on TAMP_IN8 enable
      TAMP8E         : Boolean := False;
      --  unspecified
      Reserved_8_15  : HAL.UInt8 := 16#0#;
      --  Internal tamper 1 enable
      ITAMP1E        : Boolean := False;
      --  Internal tamper 2 enable
      ITAMP2E        : Boolean := False;
      --  Internal tamper 3 enable
      ITAMP3E        : Boolean := False;
      --  unspecified
      Reserved_19_19 : HAL.Bit := 16#0#;
      --  Internal tamper 5 enable
      ITAMP5E        : Boolean := False;
      --  Internal tamper 6 enable
      ITAMP6E        : Boolean := False;
      --  Internal tamper 7 enable
      ITAMP7E        : Boolean := False;
      --  Internal tamper 8 enable
      ITAMP8E        : Boolean := False;
      --  Internal tamper 9 enable
      ITAMP9E        : Boolean := False;
      --  unspecified
      Reserved_25_25 : HAL.Bit := 16#0#;
      --  Internal tamper 11 enable
      ITAMP11E       : Boolean := False;
      --  Internal tamper 12 enable
      ITAMP12E       : Boolean := False;
      --  Internal tamper 13 enable
      ITAMP13E       : Boolean := False;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TAMP_CR1_Register use record
      TAMP1E         at 0 range 0 .. 0;
      TAMP2E         at 0 range 1 .. 1;
      TAMP3E         at 0 range 2 .. 2;
      TAMP4E         at 0 range 3 .. 3;
      TAMP5E         at 0 range 4 .. 4;
      TAMP6E         at 0 range 5 .. 5;
      TAMP7E         at 0 range 6 .. 6;
      TAMP8E         at 0 range 7 .. 7;
      Reserved_8_15  at 0 range 8 .. 15;
      ITAMP1E        at 0 range 16 .. 16;
      ITAMP2E        at 0 range 17 .. 17;
      ITAMP3E        at 0 range 18 .. 18;
      Reserved_19_19 at 0 range 19 .. 19;
      ITAMP5E        at 0 range 20 .. 20;
      ITAMP6E        at 0 range 21 .. 21;
      ITAMP7E        at 0 range 22 .. 22;
      ITAMP8E        at 0 range 23 .. 23;
      ITAMP9E        at 0 range 24 .. 24;
      Reserved_25_25 at 0 range 25 .. 25;
      ITAMP11E       at 0 range 26 .. 26;
      ITAMP12E       at 0 range 27 .. 27;
      ITAMP13E       at 0 range 28 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   --  TAMP control register 2
   type TAMP_CR2_Register is record
      --  Tamper 1 no erase
      TAMP1NOER      : Boolean := False;
      --  Tamper 2 no erase
      TAMP2NOER      : Boolean := False;
      --  Tamper 3 no erase
      TAMP3NOER      : Boolean := False;
      --  Tamper 4 no erase
      TAMP4NOER      : Boolean := False;
      --  Tamper 5 no erase
      TAMP5NOER      : Boolean := False;
      --  Tamper 6 no erase
      TAMP6NOER      : Boolean := False;
      --  Tamper 7 no erase
      TAMP7NOER      : Boolean := False;
      --  Tamper 8 no erase
      TAMP8NOER      : Boolean := False;
      --  unspecified
      Reserved_8_15  : HAL.UInt8 := 16#0#;
      --  Tamper 1 mask The tamper 1 interrupt must not be enabled when
      --  TAMP1MSK is set.
      TAMP1MSK       : Boolean := False;
      --  Tamper 2 mask The tamper 2 interrupt must not be enabled when
      --  TAMP2MSK is set.
      TAMP2MSK       : Boolean := False;
      --  Tamper 3 mask The tamper 3 interrupt must not be enabled when
      --  TAMP3MSK is set.
      TAMP3MSK       : Boolean := False;
      --  unspecified
      Reserved_19_21 : HAL.UInt3 := 16#0#;
      --  Backup registers and device secrets access blocked
      BKBLOCK        : Boolean := False;
      --  Write-only. Backup registers and device secrets erase Writing '1
      --  to this bit reset the backup registers and device secrets(1). Writing
      --  0 has no effect. This bit is always read as 0.
      BKERASE        : Boolean := False;
      --  Active level for tamper 1 input If TAMPFLT = 00 Tamper 1 input
      --  rising edge and high level triggers a tamper detection event. If
      --  TAMPFLT = 00 Tamper 1 input falling edge and low level triggers a
      --  tamper detection event.
      TAMP1TRG       : Boolean := False;
      --  Active level for tamper 2 input If TAMPFLT = 00 Tamper 2 input rising
      --  edge and high level triggers a tamper detection event. If
      --  TAMPFLT = 00 Tamper 2 input falling edge and low level triggers a
      --  tamper detection event.
      TAMP2TRG       : Boolean := False;
      --  Active level for tamper 3 input If TAMPFLT = 00 Tamper 3 input
      --  rising edge and high level triggers a tamper detection event. If
      --  TAMPFLT = 00 Tamper 3 input falling edge and low level triggers a
      --  tamper detection event.
      TAMP3TRG       : Boolean := False;
      --  Active level for tamper 4 input (active mode disabled) If
      --  TAMPFLT = 00 Tamper 4 input rising edge and high level triggers a
      --  tamper detection event. If TAMPFLT = 00 Tamper 4 input falling edge
      --  and low level triggers a tamper detection event.
      TAMP4TRG       : Boolean := False;
      --  Active level for tamper 5 input (active mode disabled) If
      --  TAMPFLT = 00 Tamper 5 input rising edge and high level triggers a
      --  tamper detection event. If TAMPFLT = 00 Tamper 5 input falling edge
      --  and low level triggers a tamper detection event.
      TAMP5TRG       : Boolean := False;
      --  Active level for tamper 6 input (active mode disabled) If
      --  TAMPFLT = 00 Tamper 6 input rising edge and high level triggers a
      --  tamper detection event. If TAMPFLT = 00 Tamper 6 input falling edge
      --  and low level triggers a tamper detection event.
      TAMP6TRG       : Boolean := False;
      --  Active level for tamper 7 input (active mode disabled) If
      --  TAMPFLT = 00 Tamper 7 input rising edge and high level triggers a
      --  tamper detection event. If TAMPFLT = 00 Tamper 7 input falling edge
      --  and low level triggers a tamper detection event.
      TAMP7TRG       : Boolean := False;
      --  Active level for tamper 8 input (active mode disabled) If
      --  TAMPFLT = 00 Tamper 8 input rising edge and high level triggers a
      --  tamper detection event. If TAMPFLT  = 00 Tamper 8 input falling
      --  edge and low level triggers a tamper detection event.
      TAMP8TRG       : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TAMP_CR2_Register use record
      TAMP1NOER      at 0 range 0 .. 0;
      TAMP2NOER      at 0 range 1 .. 1;
      TAMP3NOER      at 0 range 2 .. 2;
      TAMP4NOER      at 0 range 3 .. 3;
      TAMP5NOER      at 0 range 4 .. 4;
      TAMP6NOER      at 0 range 5 .. 5;
      TAMP7NOER      at 0 range 6 .. 6;
      TAMP8NOER      at 0 range 7 .. 7;
      Reserved_8_15  at 0 range 8 .. 15;
      TAMP1MSK       at 0 range 16 .. 16;
      TAMP2MSK       at 0 range 17 .. 17;
      TAMP3MSK       at 0 range 18 .. 18;
      Reserved_19_21 at 0 range 19 .. 21;
      BKBLOCK        at 0 range 22 .. 22;
      BKERASE        at 0 range 23 .. 23;
      TAMP1TRG       at 0 range 24 .. 24;
      TAMP2TRG       at 0 range 25 .. 25;
      TAMP3TRG       at 0 range 26 .. 26;
      TAMP4TRG       at 0 range 27 .. 27;
      TAMP5TRG       at 0 range 28 .. 28;
      TAMP6TRG       at 0 range 29 .. 29;
      TAMP7TRG       at 0 range 30 .. 30;
      TAMP8TRG       at 0 range 31 .. 31;
   end record;

   --  TAMP control register 3
   type TAMP_CR3_Register is record
      --  Internal Tamper 1 no erase
      ITAMP1NOER     : Boolean := False;
      --  Internal Tamper 2 no erase
      ITAMP2NOER     : Boolean := False;
      --  Internal Tamper 3 no erase
      ITAMP3NOER     : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Internal Tamper 5 no erase
      ITAMP5NOER     : Boolean := False;
      --  Internal Tamper 6 no erase
      ITAMP6NOER     : Boolean := False;
      --  Internal Tamper 7 no erase
      ITAMP7NOER     : Boolean := False;
      --  Internal Tamper 8 no erase
      ITAMP8NOER     : Boolean := False;
      --  Internal Tamper 9 no erase
      ITAMP9NOER     : Boolean := False;
      --  unspecified
      Reserved_9_9   : HAL.Bit := 16#0#;
      --  Internal Tamper 11 no erase
      ITAMP11NOER    : Boolean := False;
      --  Internal Tamper 12 no erase
      ITAMP12NOER    : Boolean := False;
      --  Internal Tamper 13 no erase
      ITAMP13NOER    : Boolean := False;
      --  unspecified
      Reserved_13_31 : HAL.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TAMP_CR3_Register use record
      ITAMP1NOER     at 0 range 0 .. 0;
      ITAMP2NOER     at 0 range 1 .. 1;
      ITAMP3NOER     at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      ITAMP5NOER     at 0 range 4 .. 4;
      ITAMP6NOER     at 0 range 5 .. 5;
      ITAMP7NOER     at 0 range 6 .. 6;
      ITAMP8NOER     at 0 range 7 .. 7;
      ITAMP9NOER     at 0 range 8 .. 8;
      Reserved_9_9   at 0 range 9 .. 9;
      ITAMP11NOER    at 0 range 10 .. 10;
      ITAMP12NOER    at 0 range 11 .. 11;
      ITAMP13NOER    at 0 range 12 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   subtype TAMP_FLTCR_TAMPFREQ_Field is HAL.UInt3;
   subtype TAMP_FLTCR_TAMPFLT_Field is HAL.UInt2;
   subtype TAMP_FLTCR_TAMPPRCH_Field is HAL.UInt2;

   --  TAMP filter control register
   type TAMP_FLTCR_Register is record
      --  Tamper sampling frequency Determines the frequency at which each of
      --  the TAMP_INx inputs are sampled.
      TAMPFREQ      : TAMP_FLTCR_TAMPFREQ_Field := 16#0#;
      --  TAMP_INx filter count These bits determines the number of consecutive
      --  samples at the specified level (TAMP*TRG) needed to activate a tamper
      --  event. TAMPFLT is valid for each of the TAMP_INx inputs.
      TAMPFLT       : TAMP_FLTCR_TAMPFLT_Field := 16#0#;
      --  TAMP_INx precharge duration These bit determines the duration of time
      --  during which the pull-up/is activated before each sample. TAMPPRCH is
      --  valid for each of the TAMP_INx inputs.
      TAMPPRCH      : TAMP_FLTCR_TAMPPRCH_Field := 16#0#;
      --  TAMP_INx pull-up disable This bit determines if each of the TAMPx
      --  pins are precharged before each sample.
      TAMPPUDIS     : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TAMP_FLTCR_Register use record
      TAMPFREQ      at 0 range 0 .. 2;
      TAMPFLT       at 0 range 3 .. 4;
      TAMPPRCH      at 0 range 5 .. 6;
      TAMPPUDIS     at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  TAMP_ATCR1_ATOSEL array element
   subtype TAMP_ATCR1_ATOSEL_Element is HAL.UInt2;

   --  TAMP_ATCR1_ATOSEL array
   type TAMP_ATCR1_ATOSEL_Field_Array is array (1 .. 4)
     of TAMP_ATCR1_ATOSEL_Element
     with Component_Size => 2, Size => 8;

   --  Type definition for TAMP_ATCR1_ATOSEL
   type TAMP_ATCR1_ATOSEL_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  ATOSEL as a value
            Val : HAL.UInt8;
         when True =>
            --  ATOSEL as an array
            Arr : TAMP_ATCR1_ATOSEL_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 8;

   for TAMP_ATCR1_ATOSEL_Field use record
      Val at 0 range 0 .. 7;
      Arr at 0 range 0 .. 7;
   end record;

   subtype TAMP_ATCR1_ATCKSEL_Field is HAL.UInt3;
   subtype TAMP_ATCR1_ATPER_Field is HAL.UInt3;

   --  TAMP active tamper control register 1
   type TAMP_ATCR1_Register is record
      --  Tamper 1 active mode
      TAMP1AM        : Boolean := False;
      --  Tamper 2 active mode
      TAMP2AM        : Boolean := False;
      --  Tamper 3 active mode
      TAMP3AM        : Boolean := False;
      --  Tamper 4 active mode
      TAMP4AM        : Boolean := False;
      --  Tamper 5 active mode
      TAMP5AM        : Boolean := False;
      --  Tamper 6 active mode
      TAMP6AM        : Boolean := False;
      --  Tamper 7 active mode
      TAMP7AM        : Boolean := False;
      --  Tamper 8 active mode
      TAMP8AM        : Boolean := False;
      --  Active tamper shared output 1 selection The selected output must be
      --  available in the package pinout
      ATOSEL         : TAMP_ATCR1_ATOSEL_Field :=
                        (As_Array => False, Val => 16#0#);
      --  Active tamper RTC asynchronous prescaler clock selection These bits
      --  selects the RTC asynchronous prescaler stage output.The selected
      --  clock is CK_ATPRE. fCK_ATPRE = fRTCCLK / 2ATCKSEL when (PREDIV_A+1) =
      --  128. ... These bits can be written only when all active tampers are
      --  disabled. The write protection remains for up to 1.5 ck_atpre cycles
      --  after all the active tampers are disable.
      ATCKSEL        : TAMP_ATCR1_ATCKSEL_Field := 16#7#;
      --  unspecified
      Reserved_19_23 : HAL.UInt5 := 16#0#;
      --  Active tamper output change period The tamper output is changed every
      --  CK_ATPER = (2ATPER x CK_ATPRE) cycles. Refer to .
      ATPER          : TAMP_ATCR1_ATPER_Field := 16#0#;
      --  unspecified
      Reserved_27_29 : HAL.UInt3 := 16#0#;
      --  Active tamper output sharing TAMP_IN1 is compared with TAMPOUTSEL1
      --  TAMP_IN2 is compared with TAMPOUTSEL2 TAMP_IN3 is compared with
      --  TAMPOUTSEL3 TAMP_IN4 is compared with TAMPOUTSEL4 TAMP_IN5 is
      --  compared with TAMPOUTSEL5 TAMP_IN6 is compared with TAMPOUTSEL6
      --  TAMP_IN7 is compared with TAMPOUTSEL7 TAMP_IN8 is compared with
      --  TAMPOUTSEL8
      ATOSHARE       : Boolean := False;
      --  Active tamper filter enable
      FLTEN          : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TAMP_ATCR1_Register use record
      TAMP1AM        at 0 range 0 .. 0;
      TAMP2AM        at 0 range 1 .. 1;
      TAMP3AM        at 0 range 2 .. 2;
      TAMP4AM        at 0 range 3 .. 3;
      TAMP5AM        at 0 range 4 .. 4;
      TAMP6AM        at 0 range 5 .. 5;
      TAMP7AM        at 0 range 6 .. 6;
      TAMP8AM        at 0 range 7 .. 7;
      ATOSEL         at 0 range 8 .. 15;
      ATCKSEL        at 0 range 16 .. 18;
      Reserved_19_23 at 0 range 19 .. 23;
      ATPER          at 0 range 24 .. 26;
      Reserved_27_29 at 0 range 27 .. 29;
      ATOSHARE       at 0 range 30 .. 30;
      FLTEN          at 0 range 31 .. 31;
   end record;

   subtype TAMP_ATOR_PRNG_Field is HAL.UInt8;

   --  TAMP active tamper output register
   type TAMP_ATOR_Register is record
      --  Read-only. Pseudo-random generator value This field provides the
      --  values of the PRNG output. Because of potential inconsistencies due
      --  to synchronization delays, PRNG must be read at least twice. The read
      --  value is correct if it is equal to previous read value. This field
      --  can only be read when the APB is in secure mode.
      PRNG           : TAMP_ATOR_PRNG_Field;
      --  unspecified
      Reserved_8_13  : HAL.UInt6;
      --  Read-only. Seed running flag This flag is set by hardware when a new
      --  seed is written in the TAMP_ATSEEDR. It is cleared by hardware when
      --  the PRNG has absorbed this new seed, and by system reset. The TAMP
      --  APB cock must not be switched off as long as SEEDF is set.
      SEEDF          : Boolean;
      --  Read-only. Active tamper initialization status This flag is set by
      --  hardware when the PRNG has absorbed the first 128-bit seed, meaning
      --  that the enabled active tampers are functional. This flag is cleared
      --  when the active tampers are disabled.
      INITS          : Boolean;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TAMP_ATOR_Register use record
      PRNG           at 0 range 0 .. 7;
      Reserved_8_13  at 0 range 8 .. 13;
      SEEDF          at 0 range 14 .. 14;
      INITS          at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  TAMP_ATCR2_ATOSEL array element
   subtype TAMP_ATCR2_ATOSEL_Element is HAL.UInt3;

   --  TAMP_ATCR2_ATOSEL array
   type TAMP_ATCR2_ATOSEL_Field_Array is array (1 .. 8)
     of TAMP_ATCR2_ATOSEL_Element
     with Component_Size => 3, Size => 24;

   --  Type definition for TAMP_ATCR2_ATOSEL
   type TAMP_ATCR2_ATOSEL_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  ATOSEL as a value
            Val : HAL.UInt24;
         when True =>
            --  ATOSEL as an array
            Arr : TAMP_ATCR2_ATOSEL_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 24;

   for TAMP_ATCR2_ATOSEL_Field use record
      Val at 0 range 0 .. 23;
      Arr at 0 range 0 .. 23;
   end record;

   --  TAMP active tamper control register 2
   type TAMP_ATCR2_Register is record
      --  unspecified
      Reserved_0_7 : HAL.UInt8 := 16#0#;
      --  Active tamper shared output 1 selection The selected output must be
      --  available in the package pinout. Bits 9:8 are the mirror of
      --  ATOSEL1[1:0] in the TAMP_ATCR1, and so can also be read or written
      --  through TAMP_ATCR1.
      ATOSEL       : TAMP_ATCR2_ATOSEL_Field :=
                      (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TAMP_ATCR2_Register use record
      Reserved_0_7 at 0 range 0 .. 7;
      ATOSEL       at 0 range 8 .. 31;
   end record;

   subtype TAMP_SECCFGR_BKPRWSEC_Field is HAL.UInt8;
   subtype TAMP_SECCFGR_BKPWSEC_Field is HAL.UInt8;

   --  TAMP secure mode register
   type TAMP_SECCFGR_Register is record
      --  Backup registers read/write protection offset Protection zone 1 is
      --  defined for backup registers from TAMP_BKP0R to TAMP_BKPxR (x =
      --  BKPRWSEC-1, from 0 to 128). if TZEN=1, these backup registers can be
      --  read and written only with secure access. If TZEN=0:	the protection
      --  zone 1 can be read and written with non-secure access. If BKPRWSEC =
      --  0: there is no protection zone 1. If BKPRWPRIV is set, BKPRWSEC[7:0]
      --  can be written only in privileged mode.
      BKPRWSEC       : TAMP_SECCFGR_BKPRWSEC_Field := 16#0#;
      --  unspecified
      Reserved_8_14  : HAL.UInt7 := 16#0#;
      --  Monotonic counter 1 secure protection
      CNT1SEC        : Boolean := False;
      --  Backup registers write protection offset Protection zone 2 is defined
      --  for backup registers from TAMP_BKPyR (y = BKPRWSEC, from 0 to 128) to
      --  TAMP_BKPzR (z = BKPWSEC-1, from 0 to 128, BKPWSEC ¥ BKPRWSEC):
      --  if TZEN=1, these backup registers can be written only with secure
      --  access. They can be read with secure or non-secure access. Protection
      --  zone 3 defined for backup registers from TAMP_BKPtR (t = BKPWSEC,
      --  from 0 to 127). They can be read or written with secure or non-secure
      --  access. If TZEN=0:	the protection zone 2 can be read and written with
      --  non-secure access. If BKPWSEC = 0 or if BKPWSEC ¤ BKPRWSEC:
      --  there is no protection zone 2. If BKPWPRIV is set, BKPRWSEC[7:0] can
      --  be written only in privileged mode.
      BKPWSEC        : TAMP_SECCFGR_BKPWSEC_Field := 16#0#;
      --  unspecified
      Reserved_24_29 : HAL.UInt6 := 16#0#;
      --  Boot hardware key lock This bit can be read and can only be written
      --  to 1 by software. It is cleared by hardware together with the backup
      --  registers following a tamper detection event or when the readout
      --  protection (RDP) is disabled.
      BHKLOCK        : Boolean := False;
      --  Tamper protection (excluding monotonic counters and backup registers)
      --  Note: Refer to for details on the read protection.
      TAMPSEC        : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TAMP_SECCFGR_Register use record
      BKPRWSEC       at 0 range 0 .. 7;
      Reserved_8_14  at 0 range 8 .. 14;
      CNT1SEC        at 0 range 15 .. 15;
      BKPWSEC        at 0 range 16 .. 23;
      Reserved_24_29 at 0 range 24 .. 29;
      BHKLOCK        at 0 range 30 .. 30;
      TAMPSEC        at 0 range 31 .. 31;
   end record;

   --  TAMP privilege mode control register
   type TAMP_PRIVCR_Register is record
      --  unspecified
      Reserved_0_14  : HAL.UInt15 := 16#0#;
      --  Monotonic counter 1 privilege protection
      CNT1PRIV       : Boolean := False;
      --  unspecified
      Reserved_16_28 : HAL.UInt13 := 16#0#;
      --  Backup registers zone 1 privilege protection
      BKPRWPRIV      : Boolean := False;
      --  Backup registers zone 2 privilege protection
      BKPWPRIV       : Boolean := False;
      --  Tamper privilege protection (excluding backup registers) Note: Refer
      --  to for details on the read protection.
      TAMPPRIV       : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TAMP_PRIVCR_Register use record
      Reserved_0_14  at 0 range 0 .. 14;
      CNT1PRIV       at 0 range 15 .. 15;
      Reserved_16_28 at 0 range 16 .. 28;
      BKPRWPRIV      at 0 range 29 .. 29;
      BKPWPRIV       at 0 range 30 .. 30;
      TAMPPRIV       at 0 range 31 .. 31;
   end record;

   --  TAMP interrupt enable register
   type TAMP_IER_Register is record
      --  Tamper 1 interrupt enable
      TAMP1IE        : Boolean := False;
      --  Tamper 2 interrupt enable
      TAMP2IE        : Boolean := False;
      --  Tamper 3 interrupt enable
      TAMP3IE        : Boolean := False;
      --  Tamper 4 interrupt enable
      TAMP4IE        : Boolean := False;
      --  Tamper 5 interrupt enable
      TAMP5IE        : Boolean := False;
      --  Tamper 6 interrupt enable
      TAMP6IE        : Boolean := False;
      --  Tamper 7interrupt enable
      TAMP7IE        : Boolean := False;
      --  Tamper 8 interrupt enable
      TAMP8IE        : Boolean := False;
      --  unspecified
      Reserved_8_15  : HAL.UInt8 := 16#0#;
      --  Internal tamper 1 interrupt enable
      ITAMP1IE       : Boolean := False;
      --  Internal tamper 2 interrupt enable
      ITAMP2IE       : Boolean := False;
      --  Internal tamper 3 interrupt enable
      ITAMP3IE       : Boolean := False;
      --  unspecified
      Reserved_19_19 : HAL.Bit := 16#0#;
      --  Internal tamper 5 interrupt enable
      ITAMP5IE       : Boolean := False;
      --  Internal tamper 6 interrupt enable
      ITAMP6IE       : Boolean := False;
      --  Internal tamper 7 interrupt enable
      ITAMP7IE       : Boolean := False;
      --  Internal tamper 8 interrupt enable
      ITAMP8IE       : Boolean := False;
      --  Internal tamper 9 interrupt enable
      ITAMP9IE       : Boolean := False;
      --  unspecified
      Reserved_25_25 : HAL.Bit := 16#0#;
      --  Internal tamper 11 interrupt enable
      ITAMP11IE      : Boolean := False;
      --  Internal tamper 12 interrupt enable
      ITAMP12IE      : Boolean := False;
      --  Internal tamper 13 interrupt enable
      ITAMP13IE      : Boolean := False;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TAMP_IER_Register use record
      TAMP1IE        at 0 range 0 .. 0;
      TAMP2IE        at 0 range 1 .. 1;
      TAMP3IE        at 0 range 2 .. 2;
      TAMP4IE        at 0 range 3 .. 3;
      TAMP5IE        at 0 range 4 .. 4;
      TAMP6IE        at 0 range 5 .. 5;
      TAMP7IE        at 0 range 6 .. 6;
      TAMP8IE        at 0 range 7 .. 7;
      Reserved_8_15  at 0 range 8 .. 15;
      ITAMP1IE       at 0 range 16 .. 16;
      ITAMP2IE       at 0 range 17 .. 17;
      ITAMP3IE       at 0 range 18 .. 18;
      Reserved_19_19 at 0 range 19 .. 19;
      ITAMP5IE       at 0 range 20 .. 20;
      ITAMP6IE       at 0 range 21 .. 21;
      ITAMP7IE       at 0 range 22 .. 22;
      ITAMP8IE       at 0 range 23 .. 23;
      ITAMP9IE       at 0 range 24 .. 24;
      Reserved_25_25 at 0 range 25 .. 25;
      ITAMP11IE      at 0 range 26 .. 26;
      ITAMP12IE      at 0 range 27 .. 27;
      ITAMP13IE      at 0 range 28 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   --  TAMP status register
   type TAMP_SR_Register is record
      --  Read-only. TAMP1 detection flag This flag is set by hardware when a
      --  tamper detection event is detected on the TAMP1 input.
      TAMP1F         : Boolean;
      --  Read-only. TAMP2 detection flag This flag is set by hardware when a
      --  tamper detection event is detected on the TAMP2 input.
      TAMP2F         : Boolean;
      --  Read-only. TAMP3 detection flag This flag is set by hardware when a
      --  tamper detection event is detected on the TAMP3 input.
      TAMP3F         : Boolean;
      --  Read-only. TAMP4 detection flag This flag is set by hardware when a
      --  tamper detection event is detected on the TAMP4 input.
      TAMP4F         : Boolean;
      --  Read-only. TAMP5 detection flag This flag is set by hardware when a
      --  tamper detection event is detected on the TAMP5 input.
      TAMP5F         : Boolean;
      --  Read-only. TAMP6 detection flag This flag is set by hardware when a
      --  tamper detection event is detected on the TAMP6 input.
      TAMP6F         : Boolean;
      --  Read-only. TAMP7 detection flag This flag is set by hardware when a
      --  tamper detection event is detected on the TAMP7 input.
      TAMP7F         : Boolean;
      --  Read-only. TAMP8 detection flag This flag is set by hardware when a
      --  tamper detection event is detected on the TAMP8 input
      TAMP8F         : Boolean;
      --  unspecified
      Reserved_8_15  : HAL.UInt8;
      --  Read-only. Internal tamper 1 flag This flag is set by hardware when a
      --  tamper detection event is detected on the internal tamper 1.
      ITAMP1F        : Boolean;
      --  Read-only. Internal tamper 2 flag This flag is set by hardware when a
      --  tamper detection event is detected on the internal tamper 2.
      ITAMP2F        : Boolean;
      --  Read-only. Internal tamper 3 flag This flag is set by hardware when a
      --  tamper detection event is detected on the internal tamper 3.
      ITAMP3F        : Boolean;
      --  unspecified
      Reserved_19_19 : HAL.Bit;
      --  Read-only. Internal tamper 5 flag This flag is set by hardware when a
      --  tamper detection event is detected on the internal tamper 5.
      ITAMP5F        : Boolean;
      --  Read-only. Internal tamper 6 flag This flag is set by hardware when a
      --  tamper detection event is detected on the internal tamper 6.
      ITAMP6F        : Boolean;
      --  Read-only. Internal tamper 7 flag This flag is set by hardware when a
      --  tamper detection event is detected on the internal tamper 7.
      ITAMP7F        : Boolean;
      --  Read-only. Internal tamper 8 flag This flag is set by hardware when a
      --  tamper detection event is detected on the internal tamper 8.
      ITAMP8F        : Boolean;
      --  Read-only. Internal tamper 9 flag This flag is set by hardware when a
      --  tamper detection event is detected on the internal tamper 9.
      ITAMP9F        : Boolean;
      --  unspecified
      Reserved_25_25 : HAL.Bit;
      --  Read-only. Internal tamper 11 flag This flag is set by hardware when
      --  a tamper detection event is detected on the internal tamper 11.
      ITAMP11F       : Boolean;
      --  Read-only. Internal tamper 12 flag This flag is set by hardware when
      --  a tamper detection event is detected on the internal tamper 12.
      ITAMP12F       : Boolean;
      --  Read-only. Internal tamper 13 flag This flag is set by hardware when
      --  a tamper detection event is detected on the internal tamper 13.
      ITAMP13F       : Boolean;
      --  unspecified
      Reserved_29_31 : HAL.UInt3;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TAMP_SR_Register use record
      TAMP1F         at 0 range 0 .. 0;
      TAMP2F         at 0 range 1 .. 1;
      TAMP3F         at 0 range 2 .. 2;
      TAMP4F         at 0 range 3 .. 3;
      TAMP5F         at 0 range 4 .. 4;
      TAMP6F         at 0 range 5 .. 5;
      TAMP7F         at 0 range 6 .. 6;
      TAMP8F         at 0 range 7 .. 7;
      Reserved_8_15  at 0 range 8 .. 15;
      ITAMP1F        at 0 range 16 .. 16;
      ITAMP2F        at 0 range 17 .. 17;
      ITAMP3F        at 0 range 18 .. 18;
      Reserved_19_19 at 0 range 19 .. 19;
      ITAMP5F        at 0 range 20 .. 20;
      ITAMP6F        at 0 range 21 .. 21;
      ITAMP7F        at 0 range 22 .. 22;
      ITAMP8F        at 0 range 23 .. 23;
      ITAMP9F        at 0 range 24 .. 24;
      Reserved_25_25 at 0 range 25 .. 25;
      ITAMP11F       at 0 range 26 .. 26;
      ITAMP12F       at 0 range 27 .. 27;
      ITAMP13F       at 0 range 28 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   --  TAMP non-secure masked interrupt status register
   type TAMP_MISR_Register is record
      --  Read-only. TAMP1 non-secure interrupt masked flag This flag is set by
      --  hardware when the tamper 1 non-secure interrupt is raised.
      TAMP1MF        : Boolean;
      --  Read-only. TAMP2 non-secure interrupt masked flag This flag is set by
      --  hardware when the tamper 2 non-secure interrupt is raised.
      TAMP2MF        : Boolean;
      --  Read-only. TAMP3 non-secure interrupt masked flag This flag is set by
      --  hardware when the tamper 3 non-secure interrupt is raised.
      TAMP3MF        : Boolean;
      --  Read-only. TAMP4 non-secure interrupt masked flag This flag is set by
      --  hardware when the tamper 4 non-secure interrupt is raised.
      TAMP4MF        : Boolean;
      --  Read-only. TAMP5 non-secure interrupt masked flag This flag is set by
      --  hardware when the tamper 5 non-secure interrupt is raised.
      TAMP5MF        : Boolean;
      --  Read-only. TAMP6 non-secure interrupt masked flag This flag is set by
      --  hardware when the tamper 6 non-secure interrupt is raised.
      TAMP6MF        : Boolean;
      --  Read-only. TAMP7 non-secure interrupt masked flag This flag is set by
      --  hardware when the tamper 7 non-secure interrupt is raised.
      TAMP7MF        : Boolean;
      --  Read-only. TAMP8 non-secure interrupt masked flag This flag is set by
      --  hardware when the tamper 8 non-secure interrupt is raised.
      TAMP8MF        : Boolean;
      --  unspecified
      Reserved_8_15  : HAL.UInt8;
      --  Read-only. Internal tamper 1 non-secure interrupt masked flag This
      --  flag is set by hardware when the internal tamper 1 non-secure
      --  interrupt is raised.
      ITAMP1MF       : Boolean;
      --  Read-only. Internal tamper 2 non-secure interrupt masked flag This
      --  flag is set by hardware when the internal tamper 2 non-secure
      --  interrupt is raised.
      ITAMP2MF       : Boolean;
      --  Read-only. Internal tamper 3 non-secure interrupt masked flag This
      --  flag is set by hardware when the internal tamper 3 non-secure
      --  interrupt is raised.
      ITAMP3MF       : Boolean;
      --  unspecified
      Reserved_19_19 : HAL.Bit;
      --  Read-only. Internal tamper 5 non-secure interrupt masked flag This
      --  flag is set by hardware when the internal tamper 5 non-secure
      --  interrupt is raised.
      ITAMP5MF       : Boolean;
      --  Read-only. Internal tamper 6 non-secure interrupt masked flag This
      --  flag is set by hardware when the internal tamper 6 non-secure
      --  interrupt is raised.
      ITAMP6MF       : Boolean;
      --  Read-only. VCORE monitoring tamper non-secure interrupt masked flag
      --  This flag is set by hardware when the internal tamper 7 non-secure
      --  interrupt is raised.
      ITAMP7MF       : Boolean;
      --  Read-only. Internal tamper 8 non-secure interrupt masked flag This
      --  flag is set by hardware when the internal tamper 8 non-secure
      --  interrupt is raised.
      ITAMP8MF       : Boolean;
      --  Read-only. internal tamper 9 non-secure interrupt masked flag This
      --  flag is set by hardware when the internal tamper 9 non-secure
      --  interrupt is raised.
      ITAMP9MF       : Boolean;
      --  unspecified
      Reserved_25_25 : HAL.Bit;
      --  Read-only. internal tamper 11 non-secure interrupt masked flag This
      --  flag is set by hardware when the internal tamper 11 non-secure
      --  interrupt is raised.
      ITAMP11MF      : Boolean;
      --  Read-only. internal tamper 12 non-secure interrupt masked flag This
      --  flag is set by hardware when the internal tamper 12 non-secure
      --  interrupt is raised.
      ITAMP12MF      : Boolean;
      --  Read-only. internal tamper 13 non-secure interrupt masked flag This
      --  flag is set by hardware when the internal tamper 13 non-secure
      --  interrupt is raised.
      ITAMP13MF      : Boolean;
      --  unspecified
      Reserved_29_31 : HAL.UInt3;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TAMP_MISR_Register use record
      TAMP1MF        at 0 range 0 .. 0;
      TAMP2MF        at 0 range 1 .. 1;
      TAMP3MF        at 0 range 2 .. 2;
      TAMP4MF        at 0 range 3 .. 3;
      TAMP5MF        at 0 range 4 .. 4;
      TAMP6MF        at 0 range 5 .. 5;
      TAMP7MF        at 0 range 6 .. 6;
      TAMP8MF        at 0 range 7 .. 7;
      Reserved_8_15  at 0 range 8 .. 15;
      ITAMP1MF       at 0 range 16 .. 16;
      ITAMP2MF       at 0 range 17 .. 17;
      ITAMP3MF       at 0 range 18 .. 18;
      Reserved_19_19 at 0 range 19 .. 19;
      ITAMP5MF       at 0 range 20 .. 20;
      ITAMP6MF       at 0 range 21 .. 21;
      ITAMP7MF       at 0 range 22 .. 22;
      ITAMP8MF       at 0 range 23 .. 23;
      ITAMP9MF       at 0 range 24 .. 24;
      Reserved_25_25 at 0 range 25 .. 25;
      ITAMP11MF      at 0 range 26 .. 26;
      ITAMP12MF      at 0 range 27 .. 27;
      ITAMP13MF      at 0 range 28 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   --  TAMP secure masked interrupt status register
   type TAMP_SMISR_Register is record
      --  Read-only. TAMP1 secure interrupt masked flag This flag is set by
      --  hardware when the tamper 1 secure interrupt is raised.
      TAMP1MF        : Boolean;
      --  Read-only. TAMP2 secure interrupt masked flag This flag is set by
      --  hardware when the tamper 2 secure interrupt is raised.
      TAMP2MF        : Boolean;
      --  Read-only. TAMP3 secure interrupt masked flag This flag is set by
      --  hardware when the tamper 3 secure interrupt is raised.
      TAMP3MF        : Boolean;
      --  Read-only. TAMP4 secure interrupt masked flag This flag is set by
      --  hardware when the tamper 4 secure interrupt is raised.
      TAMP4MF        : Boolean;
      --  Read-only. TAMP5 secure interrupt masked flag This flag is set by
      --  hardware when the tamper 5 secure interrupt is raised.
      TAMP5MF        : Boolean;
      --  Read-only. TAMP6 secure interrupt masked flag This flag is set by
      --  hardware when the tamper 6 secure interrupt is raised.
      TAMP6MF        : Boolean;
      --  Read-only. TAMP7 secure interrupt masked flag This flag is set by
      --  hardware when the tamper 7 secure interrupt is raised.
      TAMP7MF        : Boolean;
      --  Read-only. TAMP8 secure interrupt masked flag This flag is set by
      --  hardware when the tamper 8 secure interrupt is raised.
      TAMP8MF        : Boolean;
      --  unspecified
      Reserved_8_15  : HAL.UInt8;
      --  Read-only. Internal tamper 1 secure interrupt masked flag This flag
      --  is set by hardware when the internal tamper 1 secure interrupt is
      --  raised.
      ITAMP1MF       : Boolean;
      --  Read-only. Internal tamper 2 secure interrupt masked flag This flag
      --  is set by hardware when the internal tamper 2 secure interrupt is
      --  raised.
      ITAMP2MF       : Boolean;
      --  Read-only. Internal tamper 3 secure interrupt masked flag This flag
      --  is set by hardware when the internal tamper 3 secure interrupt is
      --  raised.
      ITAMP3MF       : Boolean;
      --  unspecified
      Reserved_19_19 : HAL.Bit;
      --  Read-only. Internal tamper 5 secure interrupt masked flag This flag
      --  is set by hardware when the internal tamper 5 secure interrupt is
      --  raised.
      ITAMP5MF       : Boolean;
      --  Read-only. Internal tamper 6 secure interrupt masked flag This flag
      --  is set by hardware when the internal tamper 6 secure interrupt is
      --  raised.
      ITAMP6MF       : Boolean;
      --  Read-only. VCORE monitoring tamper secure interrupt masked flag This
      --  flag is set by hardware when the internal tamper 7 secure interrupt
      --  is raised.
      ITAMP7MF       : Boolean;
      --  Read-only. Internal tamper 8 secure interrupt masked flag This flag
      --  is set by hardware when the internal tamper 8 secure interrupt is
      --  raised.
      ITAMP8MF       : Boolean;
      --  Read-only. internal tamper 9 secure interrupt masked flag This flag
      --  is set by hardware when the internal tamper 9 secure interrupt is
      --  raised.
      ITAMP9MF       : Boolean;
      --  unspecified
      Reserved_25_25 : HAL.Bit;
      --  Read-only. internal tamper 11 secure interrupt masked flag This flag
      --  is set by hardware when the internal tamper 11 secure interrupt is
      --  raised.
      ITAMP11MF      : Boolean;
      --  Read-only. internal tamper 12 secure interrupt masked flag This flag
      --  is set by hardware when the internal tamper 12 secure interrupt is
      --  raised.
      ITAMP12MF      : Boolean;
      --  Read-only. internal tamper 13 secure interrupt masked flag This flag
      --  is set by hardware when the internal tamper 13 secure interrupt is
      --  raised.
      ITAMP13MF      : Boolean;
      --  unspecified
      Reserved_29_31 : HAL.UInt3;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TAMP_SMISR_Register use record
      TAMP1MF        at 0 range 0 .. 0;
      TAMP2MF        at 0 range 1 .. 1;
      TAMP3MF        at 0 range 2 .. 2;
      TAMP4MF        at 0 range 3 .. 3;
      TAMP5MF        at 0 range 4 .. 4;
      TAMP6MF        at 0 range 5 .. 5;
      TAMP7MF        at 0 range 6 .. 6;
      TAMP8MF        at 0 range 7 .. 7;
      Reserved_8_15  at 0 range 8 .. 15;
      ITAMP1MF       at 0 range 16 .. 16;
      ITAMP2MF       at 0 range 17 .. 17;
      ITAMP3MF       at 0 range 18 .. 18;
      Reserved_19_19 at 0 range 19 .. 19;
      ITAMP5MF       at 0 range 20 .. 20;
      ITAMP6MF       at 0 range 21 .. 21;
      ITAMP7MF       at 0 range 22 .. 22;
      ITAMP8MF       at 0 range 23 .. 23;
      ITAMP9MF       at 0 range 24 .. 24;
      Reserved_25_25 at 0 range 25 .. 25;
      ITAMP11MF      at 0 range 26 .. 26;
      ITAMP12MF      at 0 range 27 .. 27;
      ITAMP13MF      at 0 range 28 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   --  TAMP status clear register
   type TAMP_SCR_Register is record
      --  Write-only. Clear TAMP1 detection flag Writing 1 in this bit clears
      --  the TAMP1F bit in the TAMP_SR register.
      CTAMP1F        : Boolean := False;
      --  Write-only. Clear TAMP2 detection flag Writing 1 in this bit clears
      --  the TAMP2F bit in the TAMP_SR register.
      CTAMP2F        : Boolean := False;
      --  Write-only. Clear TAMP3 detection flag Writing 1 in this bit clears
      --  the TAMP3F bit in the TAMP_SR register.
      CTAMP3F        : Boolean := False;
      --  Write-only. Clear TAMP4 detection flag Writing 1 in this bit clears
      --  the TAMP4F bit in the TAMP_SR register.
      CTAMP4F        : Boolean := False;
      --  Write-only. Clear TAMP5 detection flag Writing 1 in this bit clears
      --  the TAMP5F bit in the TAMP_SR register.
      CTAMP5F        : Boolean := False;
      --  Write-only. Clear TAMP6 detection flag Writing 1 in this bit clears
      --  the TAMP6F bit in the TAMP_SR register.
      CTAMP6F        : Boolean := False;
      --  Write-only. Clear TAMP7 detection flag Writing 1 in this bit clears
      --  the TAMP7F bit in the TAMP_SR register.
      CTAMP7F        : Boolean := False;
      --  Write-only. Clear TAMP8 detection flag Writing 1 in this bit clears
      --  the TAMP8F bit in the TAMP_SR register.
      CTAMP8F        : Boolean := False;
      --  unspecified
      Reserved_8_15  : HAL.UInt8 := 16#0#;
      --  Write-only. Clear ITAMP1 detection flag Writing 1 in this bit clears
      --  the ITAMP1F bit in the TAMP_SR register.
      CITAMP1F       : Boolean := False;
      --  Write-only. Clear ITAMP2 detection flag Writing 1 in this bit clears
      --  the ITAMP2F bit in the TAMP_SR register.
      CITAMP2F       : Boolean := False;
      --  Write-only. Clear ITAMP3 detection flag Writing 1 in this bit clears
      --  the ITAMP3F bit in the TAMP_SR register.
      CITAMP3F       : Boolean := False;
      --  unspecified
      Reserved_19_19 : HAL.Bit := 16#0#;
      --  Write-only. Clear ITAMP5 detection flag Writing 1 in this bit clears
      --  the ITAMP5F bit in the TAMP_SR register.
      CITAMP5F       : Boolean := False;
      --  Write-only. Clear ITAMP6 detection flag Writing 1 in this bit clears
      --  the ITAMP6F bit in the TAMP_SR register.
      CITAMP6F       : Boolean := False;
      --  Write-only. Clear ITAMP7 detection flag Writing 1 in this bit clears
      --  the ITAMP7F bit in the TAMP_SR register.
      CITAMP7F       : Boolean := False;
      --  Write-only. Clear ITAMP8 detection flag Writing 1 in this bit clears
      --  the ITAMP8F bit in the TAMP_SR register.
      CITAMP8F       : Boolean := False;
      --  Write-only. Clear ITAMP9 detection flag Writing 1 in this bit clears
      --  the ITAMP9F bit in the TAMP_SR register.
      CITAMP9F       : Boolean := False;
      --  unspecified
      Reserved_25_25 : HAL.Bit := 16#0#;
      --  Write-only. Clear ITAMP11 detection flag Writing 1 in this bit clears
      --  the ITAMP11F bit in the TAMP_SR register.
      CITAMP11F      : Boolean := False;
      --  Write-only. Clear ITAMP12 detection flag Writing 1 in this bit clears
      --  the ITAMP12F bit in the TAMP_SR register.
      CITAMP12F      : Boolean := False;
      --  Write-only. Clear ITAMP13 detection flag Writing 1 in this bit clears
      --  the ITAMP13F bit in the TAMP_SR register.
      CITAMP13F      : Boolean := False;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TAMP_SCR_Register use record
      CTAMP1F        at 0 range 0 .. 0;
      CTAMP2F        at 0 range 1 .. 1;
      CTAMP3F        at 0 range 2 .. 2;
      CTAMP4F        at 0 range 3 .. 3;
      CTAMP5F        at 0 range 4 .. 4;
      CTAMP6F        at 0 range 5 .. 5;
      CTAMP7F        at 0 range 6 .. 6;
      CTAMP8F        at 0 range 7 .. 7;
      Reserved_8_15  at 0 range 8 .. 15;
      CITAMP1F       at 0 range 16 .. 16;
      CITAMP2F       at 0 range 17 .. 17;
      CITAMP3F       at 0 range 18 .. 18;
      Reserved_19_19 at 0 range 19 .. 19;
      CITAMP5F       at 0 range 20 .. 20;
      CITAMP6F       at 0 range 21 .. 21;
      CITAMP7F       at 0 range 22 .. 22;
      CITAMP8F       at 0 range 23 .. 23;
      CITAMP9F       at 0 range 24 .. 24;
      Reserved_25_25 at 0 range 25 .. 25;
      CITAMP11F      at 0 range 26 .. 26;
      CITAMP12F      at 0 range 27 .. 27;
      CITAMP13F      at 0 range 28 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   --  TAMP erase configuration register
   type TAMP_ERCFGR_Register is record
      --  Configurable device secrets configuration
      ERCFG0        : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TAMP_ERCFGR_Register use record
      ERCFG0        at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Tamper and backup registers
   type TAMP_Peripheral is record
      --  TAMP control register 1
      TAMP_CR1     : aliased TAMP_CR1_Register;
      --  TAMP control register 2
      TAMP_CR2     : aliased TAMP_CR2_Register;
      --  TAMP control register 3
      TAMP_CR3     : aliased TAMP_CR3_Register;
      --  TAMP filter control register
      TAMP_FLTCR   : aliased TAMP_FLTCR_Register;
      --  TAMP active tamper control register 1
      TAMP_ATCR1   : aliased TAMP_ATCR1_Register;
      --  TAMP active tamper seed register
      TAMP_ATSEEDR : aliased HAL.UInt32;
      --  TAMP active tamper output register
      TAMP_ATOR    : aliased TAMP_ATOR_Register;
      --  TAMP active tamper control register 2
      TAMP_ATCR2   : aliased TAMP_ATCR2_Register;
      --  TAMP secure mode register
      TAMP_SECCFGR : aliased TAMP_SECCFGR_Register;
      --  TAMP privilege mode control register
      TAMP_PRIVCR  : aliased TAMP_PRIVCR_Register;
      --  TAMP interrupt enable register
      TAMP_IER     : aliased TAMP_IER_Register;
      --  TAMP status register
      TAMP_SR      : aliased TAMP_SR_Register;
      --  TAMP non-secure masked interrupt status register
      TAMP_MISR    : aliased TAMP_MISR_Register;
      --  TAMP secure masked interrupt status register
      TAMP_SMISR   : aliased TAMP_SMISR_Register;
      --  TAMP status clear register
      TAMP_SCR     : aliased TAMP_SCR_Register;
      --  TAMP monotonic counter 1 register
      TAMP_COUNT1R : aliased HAL.UInt32;
      --  TAMP erase configuration register
      TAMP_ERCFGR  : aliased TAMP_ERCFGR_Register;
      --  TAMP backup 0 register
      TAMP_BKP0R   : aliased HAL.UInt32;
      --  TAMP backup 1 register
      TAMP_BKP1R   : aliased HAL.UInt32;
      --  TAMP backup 2 register
      TAMP_BKP2R   : aliased HAL.UInt32;
      --  TAMP backup 3 register
      TAMP_BKP3R   : aliased HAL.UInt32;
      --  TAMP backup 4 register
      TAMP_BKP4R   : aliased HAL.UInt32;
      --  TAMP backup 5 register
      TAMP_BKP5R   : aliased HAL.UInt32;
      --  TAMP backup 6 register
      TAMP_BKP6R   : aliased HAL.UInt32;
      --  TAMP backup 7 register
      TAMP_BKP7R   : aliased HAL.UInt32;
      --  TAMP backup 8 register
      TAMP_BKP8R   : aliased HAL.UInt32;
      --  TAMP backup 9 register
      TAMP_BKP9R   : aliased HAL.UInt32;
      --  TAMP backup 10 register
      TAMP_BKP10R  : aliased HAL.UInt32;
      --  TAMP backup 11 register
      TAMP_BKP11R  : aliased HAL.UInt32;
      --  TAMP backup 12 register
      TAMP_BKP12R  : aliased HAL.UInt32;
      --  TAMP backup 13 register
      TAMP_BKP13R  : aliased HAL.UInt32;
      --  TAMP backup 14 register
      TAMP_BKP14R  : aliased HAL.UInt32;
      --  TAMP backup 15 register
      TAMP_BKP15R  : aliased HAL.UInt32;
      --  TAMP backup 16 register
      TAMP_BKP16R  : aliased HAL.UInt32;
      --  TAMP backup 17 register
      TAMP_BKP17R  : aliased HAL.UInt32;
      --  TAMP backup 18 register
      TAMP_BKP18R  : aliased HAL.UInt32;
      --  TAMP backup 19 register
      TAMP_BKP19R  : aliased HAL.UInt32;
      --  TAMP backup 20 register
      TAMP_BKP20R  : aliased HAL.UInt32;
      --  TAMP backup 21 register
      TAMP_BKP21R  : aliased HAL.UInt32;
      --  TAMP backup 22 register
      TAMP_BKP22R  : aliased HAL.UInt32;
      --  TAMP backup 23 register
      TAMP_BKP23R  : aliased HAL.UInt32;
      --  TAMP backup 24 register
      TAMP_BKP24R  : aliased HAL.UInt32;
      --  TAMP backup 25 register
      TAMP_BKP25R  : aliased HAL.UInt32;
      --  TAMP backup 26 register
      TAMP_BKP26R  : aliased HAL.UInt32;
      --  TAMP backup 27 register
      TAMP_BKP27R  : aliased HAL.UInt32;
      --  TAMP backup 28 register
      TAMP_BKP28R  : aliased HAL.UInt32;
      --  TAMP backup 29 register
      TAMP_BKP29R  : aliased HAL.UInt32;
      --  TAMP backup 30 register
      TAMP_BKP30R  : aliased HAL.UInt32;
      --  TAMP backup 31 register
      TAMP_BKP31R  : aliased HAL.UInt32;
   end record
     with Volatile;

   for TAMP_Peripheral use record
      TAMP_CR1     at 16#0# range 0 .. 31;
      TAMP_CR2     at 16#4# range 0 .. 31;
      TAMP_CR3     at 16#8# range 0 .. 31;
      TAMP_FLTCR   at 16#C# range 0 .. 31;
      TAMP_ATCR1   at 16#10# range 0 .. 31;
      TAMP_ATSEEDR at 16#14# range 0 .. 31;
      TAMP_ATOR    at 16#18# range 0 .. 31;
      TAMP_ATCR2   at 16#1C# range 0 .. 31;
      TAMP_SECCFGR at 16#20# range 0 .. 31;
      TAMP_PRIVCR  at 16#24# range 0 .. 31;
      TAMP_IER     at 16#2C# range 0 .. 31;
      TAMP_SR      at 16#30# range 0 .. 31;
      TAMP_MISR    at 16#34# range 0 .. 31;
      TAMP_SMISR   at 16#38# range 0 .. 31;
      TAMP_SCR     at 16#3C# range 0 .. 31;
      TAMP_COUNT1R at 16#40# range 0 .. 31;
      TAMP_ERCFGR  at 16#54# range 0 .. 31;
      TAMP_BKP0R   at 16#100# range 0 .. 31;
      TAMP_BKP1R   at 16#104# range 0 .. 31;
      TAMP_BKP2R   at 16#108# range 0 .. 31;
      TAMP_BKP3R   at 16#10C# range 0 .. 31;
      TAMP_BKP4R   at 16#110# range 0 .. 31;
      TAMP_BKP5R   at 16#114# range 0 .. 31;
      TAMP_BKP6R   at 16#118# range 0 .. 31;
      TAMP_BKP7R   at 16#11C# range 0 .. 31;
      TAMP_BKP8R   at 16#120# range 0 .. 31;
      TAMP_BKP9R   at 16#124# range 0 .. 31;
      TAMP_BKP10R  at 16#128# range 0 .. 31;
      TAMP_BKP11R  at 16#12C# range 0 .. 31;
      TAMP_BKP12R  at 16#130# range 0 .. 31;
      TAMP_BKP13R  at 16#134# range 0 .. 31;
      TAMP_BKP14R  at 16#138# range 0 .. 31;
      TAMP_BKP15R  at 16#13C# range 0 .. 31;
      TAMP_BKP16R  at 16#140# range 0 .. 31;
      TAMP_BKP17R  at 16#144# range 0 .. 31;
      TAMP_BKP18R  at 16#148# range 0 .. 31;
      TAMP_BKP19R  at 16#14C# range 0 .. 31;
      TAMP_BKP20R  at 16#150# range 0 .. 31;
      TAMP_BKP21R  at 16#154# range 0 .. 31;
      TAMP_BKP22R  at 16#158# range 0 .. 31;
      TAMP_BKP23R  at 16#15C# range 0 .. 31;
      TAMP_BKP24R  at 16#160# range 0 .. 31;
      TAMP_BKP25R  at 16#164# range 0 .. 31;
      TAMP_BKP26R  at 16#168# range 0 .. 31;
      TAMP_BKP27R  at 16#16C# range 0 .. 31;
      TAMP_BKP28R  at 16#170# range 0 .. 31;
      TAMP_BKP29R  at 16#174# range 0 .. 31;
      TAMP_BKP30R  at 16#178# range 0 .. 31;
      TAMP_BKP31R  at 16#17C# range 0 .. 31;
   end record;

   --  Tamper and backup registers
   SEC_TAMP_Periph : aliased TAMP_Peripheral
     with Import, Address => SEC_TAMP_Base;

   --  Tamper and backup registers
   TAMP_Periph : aliased TAMP_Peripheral
     with Import, Address => TAMP_Base;

end STM32_SVD.TAMP;
