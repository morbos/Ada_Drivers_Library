--  This spec has been automatically generated from STM32U585.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.EXTI is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  RTSR1_RT array
   type RTSR1_RT_Field_Array is array (0 .. 22) of Boolean
     with Component_Size => 1, Size => 23;

   --  Type definition for RTSR1_RT
   type RTSR1_RT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  RT as a value
            Val : HAL.UInt23;
         when True =>
            --  RT as an array
            Arr : RTSR1_RT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 23;

   for RTSR1_RT_Field use record
      Val at 0 range 0 .. 22;
      Arr at 0 range 0 .. 22;
   end record;

   --  EXTI rising trigger selection register
   type RTSR1_Register is record
      --  Rising trigger event configuration bit of configurable event input x
      RT             : RTSR1_RT_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTSR1_Register use record
      RT             at 0 range 0 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  FTSR1_FT array
   type FTSR1_FT_Field_Array is array (0 .. 22) of Boolean
     with Component_Size => 1, Size => 23;

   --  Type definition for FTSR1_FT
   type FTSR1_FT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FT as a value
            Val : HAL.UInt23;
         when True =>
            --  FT as an array
            Arr : FTSR1_FT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 23;

   for FTSR1_FT_Field use record
      Val at 0 range 0 .. 22;
      Arr at 0 range 0 .. 22;
   end record;

   --  EXTI falling trigger selection register
   type FTSR1_Register is record
      --  Falling trigger event configuration bit of configurable event input x
      FT             : FTSR1_FT_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FTSR1_Register use record
      FT             at 0 range 0 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  SWIER1_SWI array
   type SWIER1_SWI_Field_Array is array (0 .. 22) of Boolean
     with Component_Size => 1, Size => 23;

   --  Type definition for SWIER1_SWI
   type SWIER1_SWI_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SWI as a value
            Val : HAL.UInt23;
         when True =>
            --  SWI as an array
            Arr : SWIER1_SWI_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 23;

   for SWIER1_SWI_Field use record
      Val at 0 range 0 .. 22;
      Arr at 0 range 0 .. 22;
   end record;

   --  EXTI software interrupt event register
   type SWIER1_Register is record
      --  Software interrupt on event x
      SWI            : SWIER1_SWI_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SWIER1_Register use record
      SWI            at 0 range 0 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  RPR1_RPIF array
   type RPR1_RPIF_Field_Array is array (0 .. 22) of Boolean
     with Component_Size => 1, Size => 23;

   --  Type definition for RPR1_RPIF
   type RPR1_RPIF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  RPIF as a value
            Val : HAL.UInt23;
         when True =>
            --  RPIF as an array
            Arr : RPR1_RPIF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 23;

   for RPR1_RPIF_Field use record
      Val at 0 range 0 .. 22;
      Arr at 0 range 0 .. 22;
   end record;

   --  EXTI rising edge pending register
   type RPR1_Register is record
      --  configurable event inputs x rising edge pending bit
      RPIF           : RPR1_RPIF_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RPR1_Register use record
      RPIF           at 0 range 0 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  FPR1_FPIF array
   type FPR1_FPIF_Field_Array is array (0 .. 22) of Boolean
     with Component_Size => 1, Size => 23;

   --  Type definition for FPR1_FPIF
   type FPR1_FPIF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FPIF as a value
            Val : HAL.UInt23;
         when True =>
            --  FPIF as an array
            Arr : FPR1_FPIF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 23;

   for FPR1_FPIF_Field use record
      Val at 0 range 0 .. 22;
      Arr at 0 range 0 .. 22;
   end record;

   --  EXTI falling edge pending register
   type FPR1_Register is record
      --  configurable event inputs x falling edge pending bit.
      FPIF           : FPR1_FPIF_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FPR1_Register use record
      FPIF           at 0 range 0 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  SECCFGR1_SEC array
   type SECCFGR1_SEC_Field_Array is array (0 .. 22) of Boolean
     with Component_Size => 1, Size => 23;

   --  Type definition for SECCFGR1_SEC
   type SECCFGR1_SEC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SEC as a value
            Val : HAL.UInt23;
         when True =>
            --  SEC as an array
            Arr : SECCFGR1_SEC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 23;

   for SECCFGR1_SEC_Field use record
      Val at 0 range 0 .. 22;
      Arr at 0 range 0 .. 22;
   end record;

   --  EXTI security configuration register
   type SECCFGR1_Register is record
      --  Security enable on event input x
      SEC            : SECCFGR1_SEC_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SECCFGR1_Register use record
      SEC            at 0 range 0 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  PRIVCFGR1_PRIV array
   type PRIVCFGR1_PRIV_Field_Array is array (0 .. 22) of Boolean
     with Component_Size => 1, Size => 23;

   --  Type definition for PRIVCFGR1_PRIV
   type PRIVCFGR1_PRIV_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PRIV as a value
            Val : HAL.UInt23;
         when True =>
            --  PRIV as an array
            Arr : PRIVCFGR1_PRIV_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 23;

   for PRIVCFGR1_PRIV_Field use record
      Val at 0 range 0 .. 22;
      Arr at 0 range 0 .. 22;
   end record;

   --  EXTI privilege configuration register
   type PRIVCFGR1_Register is record
      --  Security enable on event input x
      PRIV           : PRIVCFGR1_PRIV_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PRIVCFGR1_Register use record
      PRIV           at 0 range 0 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   subtype EXTICR_EXTI0_7_Field is HAL.UInt8;
   subtype EXTICR_EXTI8_15_Field is HAL.UInt8;
   subtype EXTICR_EXTI16_23_Field is HAL.UInt8;
   subtype EXTICR_EXTI24_31_Field is HAL.UInt8;

   --  EXTI external interrupt selection register
   type EXTICR_Register is record
      --  EXTIm GPIO port selection
      EXTI0_7   : EXTICR_EXTI0_7_Field := 16#0#;
      --  EXTIm+1 GPIO port selection
      EXTI8_15  : EXTICR_EXTI8_15_Field := 16#0#;
      --  EXTIm+2 GPIO port selection
      EXTI16_23 : EXTICR_EXTI16_23_Field := 16#0#;
      --  EXTIm+3 GPIO port selection
      EXTI24_31 : EXTICR_EXTI24_31_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EXTICR_Register use record
      EXTI0_7   at 0 range 0 .. 7;
      EXTI8_15  at 0 range 8 .. 15;
      EXTI16_23 at 0 range 16 .. 23;
      EXTI24_31 at 0 range 24 .. 31;
   end record;

   --  EXTI lock register
   type LOCKR_Register is record
      --  LOCK
      LOCK          : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LOCKR_Register use record
      LOCK          at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  IMR1_IM array
   type IMR1_IM_Field_Array is array (0 .. 22) of Boolean
     with Component_Size => 1, Size => 23;

   --  Type definition for IMR1_IM
   type IMR1_IM_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IM as a value
            Val : HAL.UInt23;
         when True =>
            --  IM as an array
            Arr : IMR1_IM_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 23;

   for IMR1_IM_Field use record
      Val at 0 range 0 .. 22;
      Arr at 0 range 0 .. 22;
   end record;

   --  EXTI CPU wakeup with interrupt mask register
   type IMR1_Register is record
      --  CPU wakeup with interrupt mask on event input
      IM             : IMR1_IM_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#1FF#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IMR1_Register use record
      IM             at 0 range 0 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  EMR1_EM array
   type EMR1_EM_Field_Array is array (0 .. 22) of Boolean
     with Component_Size => 1, Size => 23;

   --  Type definition for EMR1_EM
   type EMR1_EM_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EM as a value
            Val : HAL.UInt23;
         when True =>
            --  EM as an array
            Arr : EMR1_EM_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 23;

   for EMR1_EM_Field use record
      Val at 0 range 0 .. 22;
      Arr at 0 range 0 .. 22;
   end record;

   --  EXTI CPU wakeup with event mask register
   type EMR1_Register is record
      --  CPU wakeup with event generation mask on event input
      EM             : EMR1_EM_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EMR1_Register use record
      EM             at 0 range 0 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  External interrupt/event controller
   type EXTI_Peripheral is record
      --  EXTI rising trigger selection register
      RTSR1     : aliased RTSR1_Register;
      --  EXTI falling trigger selection register
      FTSR1     : aliased FTSR1_Register;
      --  EXTI software interrupt event register
      SWIER1    : aliased SWIER1_Register;
      --  EXTI rising edge pending register
      RPR1      : aliased RPR1_Register;
      --  EXTI falling edge pending register
      FPR1      : aliased FPR1_Register;
      --  EXTI security configuration register
      SECCFGR1  : aliased SECCFGR1_Register;
      --  EXTI privilege configuration register
      PRIVCFGR1 : aliased PRIVCFGR1_Register;
      --  EXTI external interrupt selection register
      EXTICR1   : aliased EXTICR_Register;
      --  EXTI external interrupt selection register
      EXTICR2   : aliased EXTICR_Register;
      --  EXTI external interrupt selection register
      EXTICR3   : aliased EXTICR_Register;
      --  EXTI external interrupt selection register
      EXTICR4   : aliased EXTICR_Register;
      --  EXTI lock register
      LOCKR     : aliased LOCKR_Register;
      --  EXTI CPU wakeup with interrupt mask register
      IMR1      : aliased IMR1_Register;
      --  EXTI CPU wakeup with event mask register
      EMR1      : aliased EMR1_Register;
   end record
     with Volatile;

   for EXTI_Peripheral use record
      RTSR1     at 16#0# range 0 .. 31;
      FTSR1     at 16#4# range 0 .. 31;
      SWIER1    at 16#8# range 0 .. 31;
      RPR1      at 16#C# range 0 .. 31;
      FPR1      at 16#10# range 0 .. 31;
      SECCFGR1  at 16#14# range 0 .. 31;
      PRIVCFGR1 at 16#18# range 0 .. 31;
      EXTICR1   at 16#60# range 0 .. 31;
      EXTICR2   at 16#64# range 0 .. 31;
      EXTICR3   at 16#68# range 0 .. 31;
      EXTICR4   at 16#6C# range 0 .. 31;
      LOCKR     at 16#70# range 0 .. 31;
      IMR1      at 16#80# range 0 .. 31;
      EMR1      at 16#84# range 0 .. 31;
   end record;

   --  External interrupt/event controller
   EXTI_Periph : aliased EXTI_Peripheral
     with Import, Address => EXTI_Base;

   --  External interrupt/event controller
   SEC_EXTI_Periph : aliased EXTI_Peripheral
     with Import, Address => SEC_EXTI_Base;

end STM32_SVD.EXTI;
