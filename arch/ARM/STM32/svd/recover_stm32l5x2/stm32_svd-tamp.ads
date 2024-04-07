--  This spec has been automatically generated from xyzzy

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

   --  control register 1
   type CR1_Register is record
      --  TAMP1E
      TAMP1E         : Boolean := False;
      --  TAMP2E
      TAMP2E         : Boolean := False;
      --  TAMP3E
      TAMP3E         : Boolean := False;
      --  TAMP4E
      TAMP4E         : Boolean := False;
      --  TAMP5E
      TAMP5E         : Boolean := False;
      --  TAMP6E
      TAMP6E         : Boolean := False;
      --  TAMP7E
      TAMP7E         : Boolean := False;
      --  TAMP8E
      TAMP8E         : Boolean := False;
      --  unspecified
      Reserved_8_15  : HAL.UInt8 := 16#0#;
      --  ITAMP1E
      ITAMP1E        : Boolean := True;
      --  ITAMP2E
      ITAMP2E        : Boolean := True;
      --  ITAMP3E
      ITAMP3E        : Boolean := True;
      --  unspecified
      Reserved_19_19 : HAL.Bit := 16#1#;
      --  ITAMP5E
      ITAMP5E        : Boolean := True;
      --  unspecified
      Reserved_21_22 : HAL.UInt2 := 16#3#;
      --  ITAMP5E
      ITAMP8E        : Boolean := True;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#FF#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR1_Register use record
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
      Reserved_21_22 at 0 range 21 .. 22;
      ITAMP8E        at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  control register 2
   type CR2_Register is record
      --  TAMP1NOER
      TAMP1NOER      : Boolean := False;
      --  TAMP2NOER
      TAMP2NOER      : Boolean := False;
      --  TAMP3NOER
      TAMP3NOER      : Boolean := False;
      --  TAMP4NOER
      TAMP4NOER      : Boolean := False;
      --  TAMP5NOER
      TAMP5NOER      : Boolean := False;
      --  TAMP6NOER
      TAMP6NOER      : Boolean := False;
      --  TAMP7NOER
      TAMP7NOER      : Boolean := False;
      --  TAMP8NOER
      TAMP8NOER      : Boolean := False;
      --  unspecified
      Reserved_8_15  : HAL.UInt8 := 16#0#;
      --  TAMP1MSK
      TAMP1MSK       : Boolean := False;
      --  TAMP2MSK
      TAMP2MSK       : Boolean := False;
      --  TAMP3MSK
      TAMP3MSK       : Boolean := False;
      --  unspecified
      Reserved_19_22 : HAL.UInt4 := 16#0#;
      --  BKERASE
      BKERASE        : Boolean := False;
      --  TAMP1TRG
      TAMP1TRG       : Boolean := False;
      --  TAMP2TRG
      TAMP2TRG       : Boolean := False;
      --  TAMP3TRG
      TAMP3TRG       : Boolean := False;
      --  TAMP4TRG
      TAMP4TRG       : Boolean := False;
      --  TAMP5TRG
      TAMP5TRG       : Boolean := False;
      --  TAMP6TRG
      TAMP6TRG       : Boolean := False;
      --  TAMP7TRG
      TAMP7TRG       : Boolean := False;
      --  TAMP8TRG
      TAMP8TRG       : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR2_Register use record
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
      Reserved_19_22 at 0 range 19 .. 22;
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

   --  control register 3
   type CR3_Register is record
      --  ITAMP1NOER
      ITAMP1NOER    : Boolean := False;
      --  ITAMP2NOER
      ITAMP2NOER    : Boolean := False;
      --  ITAMP3NOER
      ITAMP3NOER    : Boolean := False;
      --  unspecified
      Reserved_3_3  : HAL.Bit := 16#0#;
      --  ITAMP5NOER
      ITAMP5NOER    : Boolean := False;
      --  unspecified
      Reserved_5_6  : HAL.UInt2 := 16#0#;
      --  ITAMP8NOER
      ITAMP8NOER    : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR3_Register use record
      ITAMP1NOER    at 0 range 0 .. 0;
      ITAMP2NOER    at 0 range 1 .. 1;
      ITAMP3NOER    at 0 range 2 .. 2;
      Reserved_3_3  at 0 range 3 .. 3;
      ITAMP5NOER    at 0 range 4 .. 4;
      Reserved_5_6  at 0 range 5 .. 6;
      ITAMP8NOER    at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype FLTCR_TAMPFREQ_Field is HAL.UInt3;
   subtype FLTCR_TAMPFLT_Field is HAL.UInt2;
   subtype FLTCR_TAMPPRCH_Field is HAL.UInt2;

   --  TAMP filter control register
   type FLTCR_Register is record
      --  TAMPFREQ
      TAMPFREQ      : FLTCR_TAMPFREQ_Field := 16#0#;
      --  TAMPFLT
      TAMPFLT       : FLTCR_TAMPFLT_Field := 16#0#;
      --  TAMPPRCH
      TAMPPRCH      : FLTCR_TAMPPRCH_Field := 16#0#;
      --  TAMPPUDIS
      TAMPPUDIS     : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FLTCR_Register use record
      TAMPFREQ      at 0 range 0 .. 2;
      TAMPFLT       at 0 range 3 .. 4;
      TAMPPRCH      at 0 range 5 .. 6;
      TAMPPUDIS     at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  ATCR1_ATOSEL array element
   subtype ATCR1_ATOSEL_Element is HAL.UInt2;

   --  ATCR1_ATOSEL array
   type ATCR1_ATOSEL_Field_Array is array (1 .. 4) of ATCR1_ATOSEL_Element
     with Component_Size => 2, Size => 8;

   --  Type definition for ATCR1_ATOSEL
   type ATCR1_ATOSEL_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  ATOSEL as a value
            Val : HAL.UInt8;
         when True =>
            --  ATOSEL as an array
            Arr : ATCR1_ATOSEL_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 8;

   for ATCR1_ATOSEL_Field use record
      Val at 0 range 0 .. 7;
      Arr at 0 range 0 .. 7;
   end record;

   subtype ATCR1_ATCKSEL_Field is HAL.UInt2;
   subtype ATCR1_ATPER_Field is HAL.UInt2;

   --  TAMP active tamper control register 1
   type ATCR1_Register is record
      --  TAMP1AM
      TAMP1AM        : Boolean := False;
      --  TAMP2AM
      TAMP2AM        : Boolean := False;
      --  TAMP3AM
      TAMP3AM        : Boolean := False;
      --  TAMP4AM
      TAMP4AM        : Boolean := False;
      --  TAMP5AM
      TAMP5AM        : Boolean := False;
      --  TAMP6AM
      TAMP6AM        : Boolean := False;
      --  TAMP7AM
      TAMP7AM        : Boolean := False;
      --  TAMP8AM
      TAMP8AM        : Boolean := False;
      --  ATOSEL1
      ATOSEL         : ATCR1_ATOSEL_Field :=
                        (As_Array => False, Val => 16#0#);
      --  ATCKSEL
      ATCKSEL        : ATCR1_ATCKSEL_Field := 16#3#;
      --  unspecified
      Reserved_18_23 : HAL.UInt6 := 16#1#;
      --  ATPER
      ATPER          : ATCR1_ATPER_Field := 16#0#;
      --  unspecified
      Reserved_26_29 : HAL.UInt4 := 16#0#;
      --  ATOSHARE
      ATOSHARE       : Boolean := False;
      --  FLTEN
      FLTEN          : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ATCR1_Register use record
      TAMP1AM        at 0 range 0 .. 0;
      TAMP2AM        at 0 range 1 .. 1;
      TAMP3AM        at 0 range 2 .. 2;
      TAMP4AM        at 0 range 3 .. 3;
      TAMP5AM        at 0 range 4 .. 4;
      TAMP6AM        at 0 range 5 .. 5;
      TAMP7AM        at 0 range 6 .. 6;
      TAMP8AM        at 0 range 7 .. 7;
      ATOSEL         at 0 range 8 .. 15;
      ATCKSEL        at 0 range 16 .. 17;
      Reserved_18_23 at 0 range 18 .. 23;
      ATPER          at 0 range 24 .. 25;
      Reserved_26_29 at 0 range 26 .. 29;
      ATOSHARE       at 0 range 30 .. 30;
      FLTEN          at 0 range 31 .. 31;
   end record;

   subtype ATOR_PRNG_Field is HAL.UInt8;

   --  TAMP active tamper output register
   type ATOR_Register is record
      --  Read-only. Pseudo-random generator value
      PRNG           : ATOR_PRNG_Field;
      --  unspecified
      Reserved_8_13  : HAL.UInt6;
      --  Read-only. Seed running flag
      SEEDF          : Boolean;
      --  Read-only. Active tamper initialization status
      INITS          : Boolean;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ATOR_Register use record
      PRNG           at 0 range 0 .. 7;
      Reserved_8_13  at 0 range 8 .. 13;
      SEEDF          at 0 range 14 .. 14;
      INITS          at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  ATCR2_ATOSEL array element
   subtype ATCR2_ATOSEL_Element is HAL.UInt3;

   --  ATCR2_ATOSEL array
   type ATCR2_ATOSEL_Field_Array is array (1 .. 8) of ATCR2_ATOSEL_Element
     with Component_Size => 3, Size => 24;

   --  Type definition for ATCR2_ATOSEL
   type ATCR2_ATOSEL_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  ATOSEL as a value
            Val : HAL.UInt24;
         when True =>
            --  ATOSEL as an array
            Arr : ATCR2_ATOSEL_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 24;

   for ATCR2_ATOSEL_Field use record
      Val at 0 range 0 .. 23;
      Arr at 0 range 0 .. 23;
   end record;

   --  TAMP active tamper control register 2
   type ATCR2_Register is record
      --  unspecified
      Reserved_0_7 : HAL.UInt8 := 16#0#;
      --  ATOSEL1
      ATOSEL       : ATCR2_ATOSEL_Field := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ATCR2_Register use record
      Reserved_0_7 at 0 range 0 .. 7;
      ATOSEL       at 0 range 8 .. 31;
   end record;

   subtype SMCR_BKPRWDPROT_Field is HAL.UInt8;
   subtype SMCR_BKPWDPROT_Field is HAL.UInt8;

   --  TAMP secure mode register
   type SMCR_Register is record
      --  Backup registers read/write protection offset
      BKPRWDPROT     : SMCR_BKPRWDPROT_Field := 16#0#;
      --  unspecified
      Reserved_8_15  : HAL.UInt8 := 16#0#;
      --  Backup registers write protection offset
      BKPWDPROT      : SMCR_BKPWDPROT_Field := 16#0#;
      --  unspecified
      Reserved_24_30 : HAL.UInt7 := 16#0#;
      --  Tamper protection
      TAMPDPROT      : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SMCR_Register use record
      BKPRWDPROT     at 0 range 0 .. 7;
      Reserved_8_15  at 0 range 8 .. 15;
      BKPWDPROT      at 0 range 16 .. 23;
      Reserved_24_30 at 0 range 24 .. 30;
      TAMPDPROT      at 0 range 31 .. 31;
   end record;

   --  TAMP privilege mode control register
   type PRIVCR_Register is record
      --  unspecified
      Reserved_0_28 : HAL.UInt29 := 16#0#;
      --  Backup registers zone 1 privilege protection
      BKPRWPRIV     : Boolean := False;
      --  Backup registers zone 2 privilege protection
      BKPWPRIV      : Boolean := False;
      --  Tamper privilege protection
      TAMPPRIV      : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PRIVCR_Register use record
      Reserved_0_28 at 0 range 0 .. 28;
      BKPRWPRIV     at 0 range 29 .. 29;
      BKPWPRIV      at 0 range 30 .. 30;
      TAMPPRIV      at 0 range 31 .. 31;
   end record;

   --  TAMP interrupt enable register
   type IER_Register is record
      --  TAMP1IE
      TAMP1IE        : Boolean := False;
      --  TAMP2IE
      TAMP2IE        : Boolean := False;
      --  TAMP3IE
      TAMP3IE        : Boolean := False;
      --  TAMP4IE
      TAMP4IE        : Boolean := False;
      --  TAMP5IE
      TAMP5IE        : Boolean := False;
      --  TAMP6IE
      TAMP6IE        : Boolean := False;
      --  TAMP7IE
      TAMP7IE        : Boolean := False;
      --  TAMP8IE
      TAMP8IE        : Boolean := False;
      --  unspecified
      Reserved_8_15  : HAL.UInt8 := 16#0#;
      --  ITAMP1IE
      ITAMP1IE       : Boolean := False;
      --  ITAMP2IE
      ITAMP2IE       : Boolean := False;
      --  ITAMP3IE
      ITAMP3IE       : Boolean := False;
      --  unspecified
      Reserved_19_19 : HAL.Bit := 16#0#;
      --  ITAMP5IE
      ITAMP5IE       : Boolean := False;
      --  unspecified
      Reserved_21_22 : HAL.UInt2 := 16#0#;
      --  ITAMP8IE
      ITAMP8IE       : Boolean := False;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IER_Register use record
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
      Reserved_21_22 at 0 range 21 .. 22;
      ITAMP8IE       at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  TAMP status register
   type SR_Register is record
      --  Read-only. TAMP1F
      TAMP1F         : Boolean;
      --  Read-only. TAMP2F
      TAMP2F         : Boolean;
      --  Read-only. TAMP3F
      TAMP3F         : Boolean;
      --  Read-only. TAMP4F
      TAMP4F         : Boolean;
      --  Read-only. TAMP5F
      TAMP5F         : Boolean;
      --  Read-only. TAMP6F
      TAMP6F         : Boolean;
      --  Read-only. TAMP7F
      TAMP7F         : Boolean;
      --  Read-only. TAMP8F
      TAMP8F         : Boolean;
      --  unspecified
      Reserved_8_15  : HAL.UInt8;
      --  Read-only. ITAMP1F
      ITAMP1F        : Boolean;
      --  Read-only. ITAMP2F
      ITAMP2F        : Boolean;
      --  Read-only. ITAMP3F
      ITAMP3F        : Boolean;
      --  unspecified
      Reserved_19_19 : HAL.Bit;
      --  Read-only. ITAMP5F
      ITAMP5F        : Boolean;
      --  unspecified
      Reserved_21_22 : HAL.UInt2;
      --  Read-only. ITAMP8F
      ITAMP8F        : Boolean;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register use record
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
      Reserved_21_22 at 0 range 21 .. 22;
      ITAMP8F        at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  TAMP masked interrupt status register
   type MISR_Register is record
      --  Read-only. TAMP1MF:
      TAMP1MF        : Boolean;
      --  Read-only. TAMP2MF
      TAMP2MF        : Boolean;
      --  Read-only. TAMP3MF
      TAMP3MF        : Boolean;
      --  Read-only. TAMP4MF
      TAMP4MF        : Boolean;
      --  Read-only. TAMP5MF
      TAMP5MF        : Boolean;
      --  Read-only. TAMP6MF
      TAMP6MF        : Boolean;
      --  Read-only. TAMP7MF:
      TAMP7MF        : Boolean;
      --  Read-only. TAMP8MF
      TAMP8MF        : Boolean;
      --  unspecified
      Reserved_8_15  : HAL.UInt8;
      --  Read-only. ITAMP1MF
      ITAMP1MF       : Boolean;
      --  Read-only. ITAMP2MF
      ITAMP2MF       : Boolean;
      --  Read-only. ITAMP3MF
      ITAMP3MF       : Boolean;
      --  unspecified
      Reserved_19_19 : HAL.Bit;
      --  Read-only. ITAMP5MF
      ITAMP5MF       : Boolean;
      --  unspecified
      Reserved_21_22 : HAL.UInt2;
      --  Read-only. ITAMP8MF
      ITAMP8MF       : Boolean;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MISR_Register use record
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
      Reserved_21_22 at 0 range 21 .. 22;
      ITAMP8MF       at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  TAMP secure masked interrupt status register
   type SMISR_Register is record
      --  Read-only. TAMP1MF:
      TAMP1MF        : Boolean;
      --  Read-only. TAMP2MF
      TAMP2MF        : Boolean;
      --  Read-only. TAMP3MF
      TAMP3MF        : Boolean;
      --  Read-only. TAMP4MF
      TAMP4MF        : Boolean;
      --  Read-only. TAMP5MF
      TAMP5MF        : Boolean;
      --  Read-only. TAMP6MF
      TAMP6MF        : Boolean;
      --  Read-only. TAMP7MF:
      TAMP7MF        : Boolean;
      --  Read-only. TAMP8MF
      TAMP8MF        : Boolean;
      --  unspecified
      Reserved_8_15  : HAL.UInt8;
      --  Read-only. ITAMP1MF
      ITAMP1MF       : Boolean;
      --  Read-only. ITAMP2MF
      ITAMP2MF       : Boolean;
      --  Read-only. ITAMP3MF
      ITAMP3MF       : Boolean;
      --  unspecified
      Reserved_19_19 : HAL.Bit;
      --  Read-only. ITAMP5MF
      ITAMP5MF       : Boolean;
      --  unspecified
      Reserved_21_22 : HAL.UInt2;
      --  Read-only. ITAMP8MF
      ITAMP8MF       : Boolean;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SMISR_Register use record
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
      Reserved_21_22 at 0 range 21 .. 22;
      ITAMP8MF       at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  TAMP status clear register
   type SCR_Register is record
      --  Write-only. CTAMP1F
      CTAMP1F        : Boolean := False;
      --  Write-only. CTAMP2F
      CTAMP2F        : Boolean := False;
      --  Write-only. CTAMP3F
      CTAMP3F        : Boolean := False;
      --  Write-only. CTAMP4F
      CTAMP4F        : Boolean := False;
      --  Write-only. CTAMP5F
      CTAMP5F        : Boolean := False;
      --  Write-only. CTAMP6F
      CTAMP6F        : Boolean := False;
      --  Write-only. CTAMP7F
      CTAMP7F        : Boolean := False;
      --  Write-only. CTAMP8F
      CTAMP8F        : Boolean := False;
      --  unspecified
      Reserved_8_15  : HAL.UInt8 := 16#0#;
      --  Write-only. CITAMP1F
      CITAMP1F       : Boolean := False;
      --  Write-only. CITAMP2F
      CITAMP2F       : Boolean := False;
      --  Write-only. CITAMP3F
      CITAMP3F       : Boolean := False;
      --  unspecified
      Reserved_19_19 : HAL.Bit := 16#0#;
      --  Write-only. CITAMP5F
      CITAMP5F       : Boolean := False;
      --  unspecified
      Reserved_21_22 : HAL.UInt2 := 16#0#;
      --  Write-only. CITAMP8F
      CITAMP8F       : Boolean := False;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SCR_Register use record
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
      Reserved_21_22 at 0 range 21 .. 22;
      CITAMP8F       at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  TAMP configuration register
   type CFGR_Register is record
      --  unspecified
      Reserved_0_0  : HAL.Bit := 16#0#;
      --  TMONEN
      TMONEN        : Boolean := False;
      --  VMONEN
      VMONEN        : Boolean := False;
      --  WUTMONEN
      WUTMONEN      : Boolean := False;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CFGR_Register use record
      Reserved_0_0  at 0 range 0 .. 0;
      TMONEN        at 0 range 1 .. 1;
      VMONEN        at 0 range 2 .. 2;
      WUTMONEN      at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  

   type BKPR_Registers is array (0 .. 31) of HAL.UInt32;

   -----------------
   -- Peripherals --
   -----------------

   --  Tamper and backup registers
   type TAMP_Peripheral is record
      --  control register 1
      CR1     : aliased CR1_Register;
      --  control register 2
      CR2     : aliased CR2_Register;
      --  control register 3
      CR3     : aliased CR3_Register;
      --  TAMP filter control register
      FLTCR   : aliased FLTCR_Register;
      --  TAMP active tamper control register 1
      ATCR1   : aliased ATCR1_Register;
      --  TAMP active tamper seed register
      ATSEEDR : aliased HAL.UInt32;
      --  TAMP active tamper output register
      ATOR    : aliased ATOR_Register;
      --  TAMP active tamper control register 2
      ATCR2   : aliased ATCR2_Register;
      --  TAMP secure mode register
      SMCR    : aliased SMCR_Register;
      --  TAMP privilege mode control register
      PRIVCR  : aliased PRIVCR_Register;
      --  TAMP interrupt enable register
      IER     : aliased IER_Register;
      --  TAMP status register
      SR      : aliased SR_Register;
      --  TAMP masked interrupt status register
      MISR    : aliased MISR_Register;
      --  TAMP secure masked interrupt status register
      SMISR   : aliased SMISR_Register;
      --  TAMP status clear register
      SCR     : aliased SCR_Register;
      --  TAMP monotonic counter register
      COUNTR  : aliased HAL.UInt32;
      --  TAMP configuration register
      CFGR    : aliased CFGR_Register;
      BKPR    : aliased BKPR_Registers;
   end record
     with Volatile;

   for TAMP_Peripheral use record
      CR1     at 16#0# range 0 .. 31;
      CR2     at 16#4# range 0 .. 31;
      CR3     at 16#8# range 0 .. 31;
      FLTCR   at 16#C# range 0 .. 31;
      ATCR1   at 16#10# range 0 .. 31;
      ATSEEDR at 16#14# range 0 .. 31;
      ATOR    at 16#18# range 0 .. 31;
      ATCR2   at 16#1C# range 0 .. 31;
      SMCR    at 16#20# range 0 .. 31;
      PRIVCR  at 16#24# range 0 .. 31;
      IER     at 16#2C# range 0 .. 31;
      SR      at 16#30# range 0 .. 31;
      MISR    at 16#34# range 0 .. 31;
      SMISR   at 16#38# range 0 .. 31;
      SCR     at 16#3C# range 0 .. 31;
      COUNTR  at 16#40# range 0 .. 31;
      CFGR    at 16#50# range 0 .. 31;
      BKPR    at 16#100# range 0 .. 1023;
   end record;

   --  Tamper and backup registers
   SEC_TAMP_Periph : aliased TAMP_Peripheral
     with Import, Address => SEC_TAMP_Base;

   --  Tamper and backup registers
   TAMP_Periph : aliased TAMP_Peripheral
     with Import, Address => TAMP_Base;

end STM32_SVD.TAMP;
