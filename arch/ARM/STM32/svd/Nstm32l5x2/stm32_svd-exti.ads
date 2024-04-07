--  This spec has been automatically generated from STM32L562.svd

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
   type RTSR1_RT_Field_Array is array (0 .. 16) of Boolean
     with Component_Size => 1, Size => 17;

   --  Type definition for RTSR1_RT
   type RTSR1_RT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  RT as a value
            Val : HAL.UInt17;
         when True =>
            --  RT as an array
            Arr : RTSR1_RT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 17;

   for RTSR1_RT_Field use record
      Val at 0 range 0 .. 16;
      Arr at 0 range 0 .. 16;
   end record;

   --  RTSR1_RT array
   type RTSR1_RT_Field_Array_1 is array (21 .. 22) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for RTSR1_RT
   type RTSR1_RT_Field_1
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  RT as a value
            Val : HAL.UInt2;
         when True =>
            --  RT as an array
            Arr : RTSR1_RT_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for RTSR1_RT_Field_1 use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  EXTI rising trigger selection register
   type RTSR1_Register is record
      --  Rising trigger event configuration bit of configurable event input x
      RT             : RTSR1_RT_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_17_20 : HAL.UInt4 := 16#0#;
      --  Rising trigger event configuration bit of configurable event input x
      RT_1           : RTSR1_RT_Field_1 := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTSR1_Register use record
      RT             at 0 range 0 .. 16;
      Reserved_17_20 at 0 range 17 .. 20;
      RT_1           at 0 range 21 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  FTSR1_FT array
   type FTSR1_FT_Field_Array is array (0 .. 16) of Boolean
     with Component_Size => 1, Size => 17;

   --  Type definition for FTSR1_FT
   type FTSR1_FT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FT as a value
            Val : HAL.UInt17;
         when True =>
            --  FT as an array
            Arr : FTSR1_FT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 17;

   for FTSR1_FT_Field use record
      Val at 0 range 0 .. 16;
      Arr at 0 range 0 .. 16;
   end record;

   --  FTSR1_FT array
   type FTSR1_FT_Field_Array_1 is array (21 .. 22) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for FTSR1_FT
   type FTSR1_FT_Field_1
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FT as a value
            Val : HAL.UInt2;
         when True =>
            --  FT as an array
            Arr : FTSR1_FT_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for FTSR1_FT_Field_1 use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  EXTI falling trigger selection register
   type FTSR1_Register is record
      --  Falling trigger event configuration bit of configurable event input x
      FT             : FTSR1_FT_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_17_20 : HAL.UInt4 := 16#0#;
      --  Falling trigger event configuration bit of configurable event input x
      FT_1           : FTSR1_FT_Field_1 := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FTSR1_Register use record
      FT             at 0 range 0 .. 16;
      Reserved_17_20 at 0 range 17 .. 20;
      FT_1           at 0 range 21 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  SWIER1_SWI array
   type SWIER1_SWI_Field_Array is array (0 .. 16) of Boolean
     with Component_Size => 1, Size => 17;

   --  Type definition for SWIER1_SWI
   type SWIER1_SWI_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SWI as a value
            Val : HAL.UInt17;
         when True =>
            --  SWI as an array
            Arr : SWIER1_SWI_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 17;

   for SWIER1_SWI_Field use record
      Val at 0 range 0 .. 16;
      Arr at 0 range 0 .. 16;
   end record;

   --  SWIER1_SWI array
   type SWIER1_SWI_Field_Array_1 is array (21 .. 22) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for SWIER1_SWI
   type SWIER1_SWI_Field_1
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SWI as a value
            Val : HAL.UInt2;
         when True =>
            --  SWI as an array
            Arr : SWIER1_SWI_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for SWIER1_SWI_Field_1 use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  EXTI software interrupt event register
   type SWIER1_Register is record
      --  Software interrupt on event x
      SWI            : SWIER1_SWI_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_17_20 : HAL.UInt4 := 16#0#;
      --  Software interrupt on event x
      SWI_1          : SWIER1_SWI_Field_1 :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SWIER1_Register use record
      SWI            at 0 range 0 .. 16;
      Reserved_17_20 at 0 range 17 .. 20;
      SWI_1          at 0 range 21 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  RPR1_RPIF array
   type RPR1_RPIF_Field_Array is array (0 .. 16) of Boolean
     with Component_Size => 1, Size => 17;

   --  Type definition for RPR1_RPIF
   type RPR1_RPIF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  RPIF as a value
            Val : HAL.UInt17;
         when True =>
            --  RPIF as an array
            Arr : RPR1_RPIF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 17;

   for RPR1_RPIF_Field use record
      Val at 0 range 0 .. 16;
      Arr at 0 range 0 .. 16;
   end record;

   --  RPR1_RPIF array
   type RPR1_RPIF_Field_Array_1 is array (21 .. 22) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for RPR1_RPIF
   type RPR1_RPIF_Field_1
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  RPIF as a value
            Val : HAL.UInt2;
         when True =>
            --  RPIF as an array
            Arr : RPR1_RPIF_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for RPR1_RPIF_Field_1 use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  EXTI rising edge pending register
   type RPR1_Register is record
      --  configurable event inputs x rising edge pending bit
      RPIF           : RPR1_RPIF_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_17_20 : HAL.UInt4 := 16#0#;
      --  configurable event inputs x rising edge pending bit
      RPIF_1         : RPR1_RPIF_Field_1 := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RPR1_Register use record
      RPIF           at 0 range 0 .. 16;
      Reserved_17_20 at 0 range 17 .. 20;
      RPIF_1         at 0 range 21 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  FPR1_FPIF array
   type FPR1_FPIF_Field_Array is array (0 .. 16) of Boolean
     with Component_Size => 1, Size => 17;

   --  Type definition for FPR1_FPIF
   type FPR1_FPIF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FPIF as a value
            Val : HAL.UInt17;
         when True =>
            --  FPIF as an array
            Arr : FPR1_FPIF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 17;

   for FPR1_FPIF_Field use record
      Val at 0 range 0 .. 16;
      Arr at 0 range 0 .. 16;
   end record;

   --  FPR1_FPIF array
   type FPR1_FPIF_Field_Array_1 is array (21 .. 22) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for FPR1_FPIF
   type FPR1_FPIF_Field_1
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FPIF as a value
            Val : HAL.UInt2;
         when True =>
            --  FPIF as an array
            Arr : FPR1_FPIF_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for FPR1_FPIF_Field_1 use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  EXTI falling edge pending register
   type FPR1_Register is record
      --  configurable event inputs x falling edge pending bit.
      FPIF           : FPR1_FPIF_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_17_20 : HAL.UInt4 := 16#0#;
      --  configurable event inputs x falling edge pending bit.
      FPIF_1         : FPR1_FPIF_Field_1 := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FPR1_Register use record
      FPIF           at 0 range 0 .. 16;
      Reserved_17_20 at 0 range 17 .. 20;
      FPIF_1         at 0 range 21 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  SECCFGR1_SEC array
   type SECCFGR1_SEC_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  EXTI security configuration register
   type SECCFGR1_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SEC as a value
            Val : HAL.UInt32;
         when True =>
            --  SEC as an array
            Arr : SECCFGR1_SEC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for SECCFGR1_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  PRIVCFGR1_PRIV array
   type PRIVCFGR1_PRIV_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  EXTI privilege configuration register
   type PRIVCFGR1_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PRIV as a value
            Val : HAL.UInt32;
         when True =>
            --  PRIV as an array
            Arr : PRIVCFGR1_PRIV_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for PRIVCFGR1_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  RTSR2_RT array
   type RTSR2_RT_Field_Array is array (35 .. 38) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for RTSR2_RT
   type RTSR2_RT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  RT as a value
            Val : HAL.UInt4;
         when True =>
            --  RT as an array
            Arr : RTSR2_RT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for RTSR2_RT_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  EXTI rising trigger selection register
   type RTSR2_Register is record
      --  unspecified
      Reserved_0_2  : HAL.UInt3 := 16#0#;
      --  Rising trigger event configuration bit of configurable event input x
      RT            : RTSR2_RT_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTSR2_Register use record
      Reserved_0_2  at 0 range 0 .. 2;
      RT            at 0 range 3 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   --  FTSR2_FT array
   type FTSR2_FT_Field_Array is array (35 .. 38) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for FTSR2_FT
   type FTSR2_FT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FT as a value
            Val : HAL.UInt4;
         when True =>
            --  FT as an array
            Arr : FTSR2_FT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for FTSR2_FT_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  EXTI falling trigger selection register
   type FTSR2_Register is record
      --  unspecified
      Reserved_0_2  : HAL.UInt3 := 16#0#;
      --  FT35
      FT            : FTSR2_FT_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FTSR2_Register use record
      Reserved_0_2  at 0 range 0 .. 2;
      FT            at 0 range 3 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   --  SWIER2_SWI array
   type SWIER2_SWI_Field_Array is array (35 .. 38) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for SWIER2_SWI
   type SWIER2_SWI_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SWI as a value
            Val : HAL.UInt4;
         when True =>
            --  SWI as an array
            Arr : SWIER2_SWI_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for SWIER2_SWI_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  EXTI software interrupt event register
   type SWIER2_Register is record
      --  unspecified
      Reserved_0_2  : HAL.UInt3 := 16#0#;
      --  SWI35
      SWI           : SWIER2_SWI_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SWIER2_Register use record
      Reserved_0_2  at 0 range 0 .. 2;
      SWI           at 0 range 3 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   --  RPR2_RPIF array
   type RPR2_RPIF_Field_Array is array (35 .. 38) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for RPR2_RPIF
   type RPR2_RPIF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  RPIF as a value
            Val : HAL.UInt4;
         when True =>
            --  RPIF as an array
            Arr : RPR2_RPIF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for RPR2_RPIF_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  EXTI rising edge pending register
   type RPR2_Register is record
      --  unspecified
      Reserved_0_2  : HAL.UInt3 := 16#0#;
      --  RPIF35
      RPIF          : RPR2_RPIF_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RPR2_Register use record
      Reserved_0_2  at 0 range 0 .. 2;
      RPIF          at 0 range 3 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   --  FPR2_FPIF array
   type FPR2_FPIF_Field_Array is array (35 .. 38) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for FPR2_FPIF
   type FPR2_FPIF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FPIF as a value
            Val : HAL.UInt4;
         when True =>
            --  FPIF as an array
            Arr : FPR2_FPIF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for FPR2_FPIF_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  EXTI falling edge pending register
   type FPR2_Register is record
      --  unspecified
      Reserved_0_2  : HAL.UInt3 := 16#0#;
      --  FPIF35
      FPIF          : FPR2_FPIF_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FPR2_Register use record
      Reserved_0_2  at 0 range 0 .. 2;
      FPIF          at 0 range 3 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   --  PRIVCFGR2_PRIV array
   type PRIVCFGR2_PRIV_Field_Array is array (32 .. 42) of Boolean
     with Component_Size => 1, Size => 11;

   --  Type definition for PRIVCFGR2_PRIV
   type PRIVCFGR2_PRIV_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PRIV as a value
            Val : HAL.UInt11;
         when True =>
            --  PRIV as an array
            Arr : PRIVCFGR2_PRIV_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 11;

   for PRIVCFGR2_PRIV_Field use record
      Val at 0 range 0 .. 10;
      Arr at 0 range 0 .. 10;
   end record;

   --  EXTI security enable register
   type PRIVCFGR2_Register is record
      --  PRIV32
      PRIV           : PRIVCFGR2_PRIV_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PRIVCFGR2_Register use record
      PRIV           at 0 range 0 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   --  SECCFGR2_SEC array
   type SECCFGR2_SEC_Field_Array is array (32 .. 42) of Boolean
     with Component_Size => 1, Size => 11;

   --  Type definition for SECCFGR2_SEC
   type SECCFGR2_SEC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SEC as a value
            Val : HAL.UInt11;
         when True =>
            --  SEC as an array
            Arr : SECCFGR2_SEC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 11;

   for SECCFGR2_SEC_Field use record
      Val at 0 range 0 .. 10;
      Arr at 0 range 0 .. 10;
   end record;

   --  EXTI security enable register
   type SECCFGR2_Register is record
      --  SEC32
      SEC            : SECCFGR2_SEC_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SECCFGR2_Register use record
      SEC            at 0 range 0 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
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
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EXTICR_Register use record
      EXTI0_7   at 0 range 0 .. 7;
      EXTI8_15  at 0 range 8 .. 15;
      EXTI16_23 at 0 range 16 .. 23;
      EXTI24_31 at 0 range 24 .. 31;
   end record;

   --  EXTI lock register
   type LOCKRG_Register is record
      --  LOCK
      LOCK          : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for LOCKRG_Register use record
      LOCK          at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  IMR1_IM array
   type IMR1_IM_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  EXTI CPU wakeup with interrupt mask register
   type IMR1_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IM as a value
            Val : HAL.UInt32;
         when True =>
            --  IM as an array
            Arr : IMR1_IM_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for IMR1_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  EMR1_EM array
   type EMR1_EM_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  EXTI CPU wakeup with event mask register
   type EMR1_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EM as a value
            Val : HAL.UInt32;
         when True =>
            --  EM as an array
            Arr : EMR1_EM_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for EMR1_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  IMR2_IM array
   type IMR2_IM_Field_Array is array (32 .. 38) of Boolean
     with Component_Size => 1, Size => 7;

   --  Type definition for IMR2_IM
   type IMR2_IM_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IM as a value
            Val : HAL.UInt7;
         when True =>
            --  IM as an array
            Arr : IMR2_IM_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 7;

   for IMR2_IM_Field use record
      Val at 0 range 0 .. 6;
      Arr at 0 range 0 .. 6;
   end record;

   --  IMR2_IM array
   type IMR2_IM_Field_Array_1 is array (40 .. 42) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for IMR2_IM
   type IMR2_IM_Field_1
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IM as a value
            Val : HAL.UInt3;
         when True =>
            --  IM as an array
            Arr : IMR2_IM_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for IMR2_IM_Field_1 use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  EXTI CPUm wakeup with interrupt mask register
   type IMR2_Register is record
      --  CPU wakeup with interrupt mask on event input
      IM             : IMR2_IM_Field := (As_Array => False, Val => 16#1#);
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#1#;
      --  CPU wakeup with interrupt mask on event input
      IM_1           : IMR2_IM_Field_1 := (As_Array => False, Val => 16#1#);
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IMR2_Register use record
      IM             at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      IM_1           at 0 range 8 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   --  EMR2_EM array
   type EMR2_EM_Field_Array is array (32 .. 38) of Boolean
     with Component_Size => 1, Size => 7;

   --  Type definition for EMR2_EM
   type EMR2_EM_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EM as a value
            Val : HAL.UInt7;
         when True =>
            --  EM as an array
            Arr : EMR2_EM_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 7;

   for EMR2_EM_Field use record
      Val at 0 range 0 .. 6;
      Arr at 0 range 0 .. 6;
   end record;

   --  EMR2_EM array
   type EMR2_EM_Field_Array_1 is array (40 .. 42) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for EMR2_EM
   type EMR2_EM_Field_1
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EM as a value
            Val : HAL.UInt3;
         when True =>
            --  EM as an array
            Arr : EMR2_EM_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for EMR2_EM_Field_1 use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  EXTI CPU wakeup with event mask register
   type EMR2_Register is record
      --  CPU wakeup with interrupt mask on event input
      EM             : EMR2_EM_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  CPU wakeup with interrupt mask on event input
      EM_1           : EMR2_EM_Field_1 := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EMR2_Register use record
      EM             at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      EM_1           at 0 range 8 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
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
      --  EXTI rising trigger selection register
      RTSR2     : aliased RTSR2_Register;
      --  EXTI falling trigger selection register
      FTSR2     : aliased FTSR2_Register;
      --  EXTI software interrupt event register
      SWIER2    : aliased SWIER2_Register;
      --  EXTI rising edge pending register
      RPR2      : aliased RPR2_Register;
      --  EXTI falling edge pending register
      FPR2      : aliased FPR2_Register;
      --  EXTI security enable register
      PRIVCFGR2 : aliased PRIVCFGR2_Register;
      --  EXTI security enable register
      SECCFGR2  : aliased SECCFGR2_Register;
      --  EXTI external interrupt selection register
      EXTICR1   : aliased EXTICR_Register;
      --  EXTI external interrupt selection register
      EXTICR2   : aliased EXTICR_Register;
      --  EXTI external interrupt selection register
      EXTICR3   : aliased EXTICR_Register;
      --  EXTI external interrupt selection register
      EXTICR4   : aliased EXTICR_Register;
      --  EXTI lock register
      LOCKRG    : aliased LOCKRG_Register;
      --  EXTI CPU wakeup with interrupt mask register
      IMR1      : aliased IMR1_Register;
      --  EXTI CPU wakeup with event mask register
      EMR1      : aliased EMR1_Register;
      --  EXTI CPUm wakeup with interrupt mask register
      IMR2      : aliased IMR2_Register;
      --  EXTI CPU wakeup with event mask register
      EMR2      : aliased EMR2_Register;
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
      RTSR2     at 16#20# range 0 .. 31;
      FTSR2     at 16#24# range 0 .. 31;
      SWIER2    at 16#28# range 0 .. 31;
      RPR2      at 16#2C# range 0 .. 31;
      FPR2      at 16#30# range 0 .. 31;
      PRIVCFGR2 at 16#34# range 0 .. 31;
      SECCFGR2  at 16#38# range 0 .. 31;
      EXTICR1   at 16#60# range 0 .. 31;
      EXTICR2   at 16#64# range 0 .. 31;
      EXTICR3   at 16#68# range 0 .. 31;
      EXTICR4   at 16#6C# range 0 .. 31;
      LOCKRG    at 16#70# range 0 .. 31;
      IMR1      at 16#80# range 0 .. 31;
      EMR1      at 16#84# range 0 .. 31;
      IMR2      at 16#90# range 0 .. 31;
      EMR2      at 16#94# range 0 .. 31;
   end record;

   --  External interrupt/event controller
   EXTI_Periph : aliased EXTI_Peripheral
     with Import, Address => System'To_Address (16#4002F400#);

   --  External interrupt/event controller
   SEC_EXTI_Periph : aliased EXTI_Peripheral
     with Import, Address => System'To_Address (16#5002F400#);

end STM32_SVD.EXTI;
