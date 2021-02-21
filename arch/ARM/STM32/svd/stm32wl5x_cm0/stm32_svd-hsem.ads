--  This spec has been automatically generated from STM32WL5x_CM0P.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.HSEM is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype HSEM_R_PROCID_Field is HAL.UInt8;
   subtype HSEM_R_COREID_Field is HAL.UInt4;

   --  HSEM register HSEM_R0 HSEM_R31
   type HSEM_R_Register is record
      --  Semaphore ProcessID
      PROCID         : HSEM_R_PROCID_Field := 16#0#;
      --  COREID
      COREID         : HSEM_R_COREID_Field := 16#0#;
      --  unspecified
      Reserved_12_30 : HAL.UInt19 := 16#0#;
      --  Lock indication
      LOCK           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSEM_R_Register use record
      PROCID         at 0 range 0 .. 7;
      COREID         at 0 range 8 .. 11;
      Reserved_12_30 at 0 range 12 .. 30;
      LOCK           at 0 range 31 .. 31;
   end record;

   subtype HSEM_RLR_PROCID_Field is HAL.UInt8;
   subtype HSEM_RLR_COREID_Field is HAL.UInt4;

   --  HSEM Read lock register
   type HSEM_RLR_Register is record
      --  Read-only. Semaphore ProcessID
      PROCID         : HSEM_RLR_PROCID_Field;
      --  Read-only. COREID
      COREID         : HSEM_RLR_COREID_Field;
      --  unspecified
      Reserved_12_30 : HAL.UInt19;
      --  Read-only. Lock indication
      LOCK           : Boolean;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSEM_RLR_Register use record
      PROCID         at 0 range 0 .. 7;
      COREID         at 0 range 8 .. 11;
      Reserved_12_30 at 0 range 12 .. 30;
      LOCK           at 0 range 31 .. 31;
   end record;

   --  HSEM_C1IER_ISE array
   type HSEM_C1IER_ISE_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for HSEM_C1IER_ISE
   type HSEM_C1IER_ISE_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  ISE as a value
            Val : HAL.UInt16;
         when True =>
            --  ISE as an array
            Arr : HSEM_C1IER_ISE_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for HSEM_C1IER_ISE_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  HSEM Interrupt enable register
   type HSEM_C1IER_Register is record
      --  Interrupt semaphore n enable bit
      ISE            : HSEM_C1IER_ISE_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSEM_C1IER_Register use record
      ISE            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  HSEM_C1ICR_ISC array
   type HSEM_C1ICR_ISC_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for HSEM_C1ICR_ISC
   type HSEM_C1ICR_ISC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  ISC as a value
            Val : HAL.UInt16;
         when True =>
            --  ISC as an array
            Arr : HSEM_C1ICR_ISC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for HSEM_C1ICR_ISC_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  HSEM Interrupt clear register
   type HSEM_C1ICR_Register is record
      --  Interrupt(N) semaphore n clear bit
      ISC            : HSEM_C1ICR_ISC_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSEM_C1ICR_Register use record
      ISC            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  HSEM_C1ISR_ISF array
   type HSEM_C1ISR_ISF_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for HSEM_C1ISR_ISF
   type HSEM_C1ISR_ISF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  ISF as a value
            Val : HAL.UInt16;
         when True =>
            --  ISF as an array
            Arr : HSEM_C1ISR_ISF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for HSEM_C1ISR_ISF_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  HSEM Interrupt status register
   type HSEM_C1ISR_Register is record
      --  Read-only. Interrupt(N) semaphore n status bit before enable (mask)
      ISF            : HSEM_C1ISR_ISF_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSEM_C1ISR_Register use record
      ISF            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  HSEM_C1MISR_MISF array
   type HSEM_C1MISR_MISF_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for HSEM_C1MISR_MISF
   type HSEM_C1MISR_MISF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MISF as a value
            Val : HAL.UInt16;
         when True =>
            --  MISF as an array
            Arr : HSEM_C1MISR_MISF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for HSEM_C1MISR_MISF_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  HSEM Masked interrupt status register
   type HSEM_C1MISR_Register is record
      --  Read-only. masked interrupt(N) semaphore n status bit after enable
      --  (mask)
      MISF           : HSEM_C1MISR_MISF_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSEM_C1MISR_Register use record
      MISF           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  HSEM_C2IER_ISE array
   type HSEM_C2IER_ISE_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for HSEM_C2IER_ISE
   type HSEM_C2IER_ISE_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  ISE as a value
            Val : HAL.UInt16;
         when True =>
            --  ISE as an array
            Arr : HSEM_C2IER_ISE_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for HSEM_C2IER_ISE_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  HSEM Interrupt enable register
   type HSEM_C2IER_Register is record
      --  Interrupt semaphore n enable bit
      ISE            : HSEM_C2IER_ISE_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSEM_C2IER_Register use record
      ISE            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  HSEM_C2ICR_ISC array
   type HSEM_C2ICR_ISC_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for HSEM_C2ICR_ISC
   type HSEM_C2ICR_ISC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  ISC as a value
            Val : HAL.UInt16;
         when True =>
            --  ISC as an array
            Arr : HSEM_C2ICR_ISC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for HSEM_C2ICR_ISC_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  HSEM Interrupt clear register
   type HSEM_C2ICR_Register is record
      --  Read-only. Interrupt(N) semaphore n clear bit
      ISC            : HSEM_C2ICR_ISC_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSEM_C2ICR_Register use record
      ISC            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  HSEM_C2ISR_ISF array
   type HSEM_C2ISR_ISF_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for HSEM_C2ISR_ISF
   type HSEM_C2ISR_ISF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  ISF as a value
            Val : HAL.UInt16;
         when True =>
            --  ISF as an array
            Arr : HSEM_C2ISR_ISF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for HSEM_C2ISR_ISF_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  HSEM Interrupt status register
   type HSEM_C2ISR_Register is record
      --  Read-only. Interrupt(N) semaphore n status bit before enable (mask)
      ISF            : HSEM_C2ISR_ISF_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSEM_C2ISR_Register use record
      ISF            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  HSEM_C2MISR_MISF array
   type HSEM_C2MISR_MISF_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for HSEM_C2MISR_MISF
   type HSEM_C2MISR_MISF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MISF as a value
            Val : HAL.UInt16;
         when True =>
            --  MISF as an array
            Arr : HSEM_C2MISR_MISF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for HSEM_C2MISR_MISF_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  HSEM Masked interrupt status register
   type HSEM_C2MISR_Register is record
      --  Read-only. masked interrupt(N) semaphore n status bit after enable
      --  (mask)
      MISF           : HSEM_C2MISR_MISF_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSEM_C2MISR_Register use record
      MISF           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype HSEM_CR_COREID_Field is HAL.UInt4;
   subtype HSEM_CR_KEY_Field is HAL.UInt16;

   --  HSEM Clear register
   type HSEM_CR_Register is record
      --  unspecified
      Reserved_0_7   : HAL.UInt8 := 16#0#;
      --  Write-only. COREID
      COREID         : HSEM_CR_COREID_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Write-only. Semaphore clear Key
      KEY            : HSEM_CR_KEY_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSEM_CR_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      COREID         at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      KEY            at 0 range 16 .. 31;
   end record;

   subtype HSEM_KEYR_KEY_Field is HAL.UInt16;

   --  HSEM Interrupt clear register
   type HSEM_KEYR_Register is record
      --  unspecified
      Reserved_0_15 : HAL.UInt16 := 16#0#;
      --  Semaphore Clear Key
      KEY           : HSEM_KEYR_KEY_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSEM_KEYR_Register use record
      Reserved_0_15 at 0 range 0 .. 15;
      KEY           at 0 range 16 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Hardware semaphore
   type HSEM_Peripheral is record
      --  HSEM register HSEM_R0 HSEM_R31
      HSEM_R0     : aliased HSEM_R_Register;
      --  HSEM register HSEM_R0 HSEM_R31
      HSEM_R1     : aliased HSEM_R_Register;
      --  HSEM register HSEM_R0 HSEM_R31
      HSEM_R2     : aliased HSEM_R_Register;
      --  HSEM register HSEM_R0 HSEM_R31
      HSEM_R3     : aliased HSEM_R_Register;
      --  HSEM register HSEM_R0 HSEM_R31
      HSEM_R4     : aliased HSEM_R_Register;
      --  HSEM register HSEM_R0 HSEM_R31
      HSEM_R5     : aliased HSEM_R_Register;
      --  HSEM register HSEM_R0 HSEM_R31
      HSEM_R6     : aliased HSEM_R_Register;
      --  HSEM register HSEM_R0 HSEM_R31
      HSEM_R7     : aliased HSEM_R_Register;
      --  HSEM register HSEM_R0 HSEM_R31
      HSEM_R8     : aliased HSEM_R_Register;
      --  HSEM register HSEM_R0 HSEM_R31
      HSEM_R9     : aliased HSEM_R_Register;
      --  HSEM register HSEM_R0 HSEM_R31
      HSEM_R10    : aliased HSEM_R_Register;
      --  HSEM register HSEM_R0 HSEM_R31
      HSEM_R11    : aliased HSEM_R_Register;
      --  HSEM register HSEM_R0 HSEM_R31
      HSEM_R12    : aliased HSEM_R_Register;
      --  HSEM register HSEM_R0 HSEM_R31
      HSEM_R13    : aliased HSEM_R_Register;
      --  HSEM register HSEM_R0 HSEM_R31
      HSEM_R14    : aliased HSEM_R_Register;
      --  HSEM register HSEM_R0 HSEM_R31
      HSEM_R15    : aliased HSEM_R_Register;
      --  HSEM Read lock register
      HSEM_RLR0   : aliased HSEM_RLR_Register;
      --  HSEM Read lock register
      HSEM_RLR1   : aliased HSEM_RLR_Register;
      --  HSEM Read lock register
      HSEM_RLR2   : aliased HSEM_RLR_Register;
      --  HSEM Read lock register
      HSEM_RLR3   : aliased HSEM_RLR_Register;
      --  HSEM Read lock register
      HSEM_RLR4   : aliased HSEM_RLR_Register;
      --  HSEM Read lock register
      HSEM_RLR5   : aliased HSEM_RLR_Register;
      --  HSEM Read lock register
      HSEM_RLR6   : aliased HSEM_RLR_Register;
      --  HSEM Read lock register
      HSEM_RLR7   : aliased HSEM_RLR_Register;
      --  HSEM Read lock register
      HSEM_RLR8   : aliased HSEM_RLR_Register;
      --  HSEM Read lock register
      HSEM_RLR9   : aliased HSEM_RLR_Register;
      --  HSEM Read lock register
      HSEM_RLR10  : aliased HSEM_RLR_Register;
      --  HSEM Read lock register
      HSEM_RLR11  : aliased HSEM_RLR_Register;
      --  HSEM Read lock register
      HSEM_RLR12  : aliased HSEM_RLR_Register;
      --  HSEM Read lock register
      HSEM_RLR13  : aliased HSEM_RLR_Register;
      --  HSEM Read lock register
      HSEM_RLR14  : aliased HSEM_RLR_Register;
      --  HSEM Read lock register
      HSEM_RLR15  : aliased HSEM_RLR_Register;
      --  HSEM Interrupt enable register
      HSEM_C1IER  : aliased HSEM_C1IER_Register;
      --  HSEM Interrupt clear register
      HSEM_C1ICR  : aliased HSEM_C1ICR_Register;
      --  HSEM Interrupt status register
      HSEM_C1ISR  : aliased HSEM_C1ISR_Register;
      --  HSEM Masked interrupt status register
      HSEM_C1MISR : aliased HSEM_C1MISR_Register;
      --  HSEM Interrupt enable register
      HSEM_C2IER  : aliased HSEM_C2IER_Register;
      --  HSEM Interrupt clear register
      HSEM_C2ICR  : aliased HSEM_C2ICR_Register;
      --  HSEM Interrupt status register
      HSEM_C2ISR  : aliased HSEM_C2ISR_Register;
      --  HSEM Masked interrupt status register
      HSEM_C2MISR : aliased HSEM_C2MISR_Register;
      --  HSEM Clear register
      HSEM_CR     : aliased HSEM_CR_Register;
      --  HSEM Interrupt clear register
      HSEM_KEYR   : aliased HSEM_KEYR_Register;
   end record
     with Volatile;

   for HSEM_Peripheral use record
      HSEM_R0     at 16#0# range 0 .. 31;
      HSEM_R1     at 16#4# range 0 .. 31;
      HSEM_R2     at 16#8# range 0 .. 31;
      HSEM_R3     at 16#C# range 0 .. 31;
      HSEM_R4     at 16#10# range 0 .. 31;
      HSEM_R5     at 16#14# range 0 .. 31;
      HSEM_R6     at 16#18# range 0 .. 31;
      HSEM_R7     at 16#1C# range 0 .. 31;
      HSEM_R8     at 16#20# range 0 .. 31;
      HSEM_R9     at 16#24# range 0 .. 31;
      HSEM_R10    at 16#28# range 0 .. 31;
      HSEM_R11    at 16#2C# range 0 .. 31;
      HSEM_R12    at 16#30# range 0 .. 31;
      HSEM_R13    at 16#34# range 0 .. 31;
      HSEM_R14    at 16#38# range 0 .. 31;
      HSEM_R15    at 16#3C# range 0 .. 31;
      HSEM_RLR0   at 16#80# range 0 .. 31;
      HSEM_RLR1   at 16#84# range 0 .. 31;
      HSEM_RLR2   at 16#88# range 0 .. 31;
      HSEM_RLR3   at 16#8C# range 0 .. 31;
      HSEM_RLR4   at 16#90# range 0 .. 31;
      HSEM_RLR5   at 16#94# range 0 .. 31;
      HSEM_RLR6   at 16#98# range 0 .. 31;
      HSEM_RLR7   at 16#9C# range 0 .. 31;
      HSEM_RLR8   at 16#A0# range 0 .. 31;
      HSEM_RLR9   at 16#A4# range 0 .. 31;
      HSEM_RLR10  at 16#A8# range 0 .. 31;
      HSEM_RLR11  at 16#AC# range 0 .. 31;
      HSEM_RLR12  at 16#B0# range 0 .. 31;
      HSEM_RLR13  at 16#B4# range 0 .. 31;
      HSEM_RLR14  at 16#B8# range 0 .. 31;
      HSEM_RLR15  at 16#BC# range 0 .. 31;
      HSEM_C1IER  at 16#100# range 0 .. 31;
      HSEM_C1ICR  at 16#104# range 0 .. 31;
      HSEM_C1ISR  at 16#108# range 0 .. 31;
      HSEM_C1MISR at 16#10C# range 0 .. 31;
      HSEM_C2IER  at 16#110# range 0 .. 31;
      HSEM_C2ICR  at 16#114# range 0 .. 31;
      HSEM_C2ISR  at 16#118# range 0 .. 31;
      HSEM_C2MISR at 16#11C# range 0 .. 31;
      HSEM_CR     at 16#140# range 0 .. 31;
      HSEM_KEYR   at 16#144# range 0 .. 31;
   end record;

   --  Hardware semaphore
   HSEM_Periph : aliased HSEM_Peripheral
     with Import, Address => HSEM_Base;

end STM32_SVD.HSEM;
