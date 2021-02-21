--  This spec has been automatically generated from STM32WL5x_CM0P.svd

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

   subtype RTSR1_RT_Field is HAL.UInt17;
   subtype RTSR1_RT21_Field is HAL.UInt2;

   --  rising trigger selection register
   type RTSR1_Register is record
      --  Rising trigger event configuration bit of Configurable Event input
      RT             : RTSR1_RT_Field := 16#0#;
      --  unspecified
      Reserved_17_20 : HAL.UInt4 := 16#0#;
      --  Rising trigger event configuration bit of Configurable Event input
      RT21           : RTSR1_RT21_Field := 16#0#;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTSR1_Register use record
      RT             at 0 range 0 .. 16;
      Reserved_17_20 at 0 range 17 .. 20;
      RT21           at 0 range 21 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   subtype FTSR1_FT_Field is HAL.UInt17;
   subtype FTSR1_FT21_Field is HAL.UInt2;

   --  falling trigger selection register
   type FTSR1_Register is record
      --  Falling trigger event configuration bit of Configurable Event input
      FT             : FTSR1_FT_Field := 16#0#;
      --  unspecified
      Reserved_17_20 : HAL.UInt4 := 16#0#;
      --  Falling trigger event configuration bit of Configurable Event input
      FT21           : FTSR1_FT21_Field := 16#0#;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FTSR1_Register use record
      FT             at 0 range 0 .. 16;
      Reserved_17_20 at 0 range 17 .. 20;
      FT21           at 0 range 21 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   subtype SWIER1_SWI_Field is HAL.UInt17;
   subtype SWIER1_SWI21_Field is HAL.UInt2;

   --  software interrupt event register
   type SWIER1_Register is record
      --  Software interrupt on event
      SWI            : SWIER1_SWI_Field := 16#0#;
      --  unspecified
      Reserved_17_20 : HAL.UInt4 := 16#0#;
      --  Software interrupt on event
      SWI21          : SWIER1_SWI21_Field := 16#0#;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SWIER1_Register use record
      SWI            at 0 range 0 .. 16;
      Reserved_17_20 at 0 range 17 .. 20;
      SWI21          at 0 range 21 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   subtype PR1_PIF_Field is HAL.UInt17;
   subtype PR1_PIF21_Field is HAL.UInt2;

   --  EXTI pending register
   type PR1_Register is record
      --  Configurable event inputs Pending bit
      PIF            : PR1_PIF_Field := 16#0#;
      --  unspecified
      Reserved_17_20 : HAL.UInt4 := 16#0#;
      --  Configurable event inputs Pending bit
      PIF21          : PR1_PIF21_Field := 16#0#;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PR1_Register use record
      PIF            at 0 range 0 .. 16;
      Reserved_17_20 at 0 range 17 .. 20;
      PIF21          at 0 range 21 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  RTSR2_RT array
   type RTSR2_RT_Field_Array is array (40 .. 41) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for RTSR2_RT
   type RTSR2_RT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  RT as a value
            Val : HAL.UInt2;
         when True =>
            --  RT as an array
            Arr : RTSR2_RT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for RTSR2_RT_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  rising trigger selection register
   type RTSR2_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  Rising trigger event configuration bit of Configurable Event input
      RT34           : Boolean := False;
      --  unspecified
      Reserved_3_7   : HAL.UInt5 := 16#0#;
      --  Rising trigger event configuration bit of Configurable Event input
      RT             : RTSR2_RT_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_10_12 : HAL.UInt3 := 16#0#;
      --  Rising trigger event configuration bit of Configurable Event input
      RT45           : Boolean := False;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTSR2_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      RT34           at 0 range 2 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      RT             at 0 range 8 .. 9;
      Reserved_10_12 at 0 range 10 .. 12;
      RT45           at 0 range 13 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   --  FTSR2_FT array
   type FTSR2_FT_Field_Array is array (40 .. 41) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for FTSR2_FT
   type FTSR2_FT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FT as a value
            Val : HAL.UInt2;
         when True =>
            --  FT as an array
            Arr : FTSR2_FT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for FTSR2_FT_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  falling trigger selection register
   type FTSR2_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  Falling trigger event configuration bit of Configurable Event input
      FT34           : Boolean := False;
      --  unspecified
      Reserved_3_7   : HAL.UInt5 := 16#0#;
      --  Falling trigger event configuration bit of Configurable Event input
      FT             : FTSR2_FT_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_10_12 : HAL.UInt3 := 16#0#;
      --  Falling trigger event configuration bit of Configurable Event input
      FT45           : Boolean := False;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FTSR2_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      FT34           at 0 range 2 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      FT             at 0 range 8 .. 9;
      Reserved_10_12 at 0 range 10 .. 12;
      FT45           at 0 range 13 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   --  SWIER2_SWI array
   type SWIER2_SWI_Field_Array is array (40 .. 41) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for SWIER2_SWI
   type SWIER2_SWI_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SWI as a value
            Val : HAL.UInt2;
         when True =>
            --  SWI as an array
            Arr : SWIER2_SWI_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for SWIER2_SWI_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  software interrupt event register
   type SWIER2_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  Software interrupt on event
      SWI34          : Boolean := False;
      --  unspecified
      Reserved_3_7   : HAL.UInt5 := 16#0#;
      --  Software interrupt on event
      SWI            : SWIER2_SWI_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_10_12 : HAL.UInt3 := 16#0#;
      --  Software interrupt on event 45
      SWI45          : Boolean := False;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SWIER2_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      SWI34          at 0 range 2 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      SWI            at 0 range 8 .. 9;
      Reserved_10_12 at 0 range 10 .. 12;
      SWI45          at 0 range 13 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   --  PR2_PIF array
   type PR2_PIF_Field_Array is array (40 .. 41) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for PR2_PIF
   type PR2_PIF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PIF as a value
            Val : HAL.UInt2;
         when True =>
            --  PIF as an array
            Arr : PR2_PIF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for PR2_PIF_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  pending register
   type PR2_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  Configurable event inputs 33 Pending bit.
      PIF34          : Boolean := False;
      --  unspecified
      Reserved_3_7   : HAL.UInt5 := 16#0#;
      --  Configurable event inputs 40_41 Pending bit.
      PIF            : PR2_PIF_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_10_12 : HAL.UInt3 := 16#0#;
      --  Configurable event inputs 45 Pending bit.
      PIF45          : Boolean := False;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PR2_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      PIF34          at 0 range 2 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      PIF            at 0 range 8 .. 9;
      Reserved_10_12 at 0 range 10 .. 12;
      PIF45          at 0 range 13 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   --  C1EMR1_EM array
   type C1EMR1_EM_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for C1EMR1_EM
   type C1EMR1_EM_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EM as a value
            Val : HAL.UInt16;
         when True =>
            --  EM as an array
            Arr : C1EMR1_EM_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for C1EMR1_EM_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  C1EMR1_EM array
   type C1EMR1_EM_Field_Array_1 is array (17 .. 22) of Boolean
     with Component_Size => 1, Size => 6;

   --  Type definition for C1EMR1_EM
   type C1EMR1_EM_Field_1
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EM as a value
            Val : HAL.UInt6;
         when True =>
            --  EM as an array
            Arr : C1EMR1_EM_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 6;

   for C1EMR1_EM_Field_1 use record
      Val at 0 range 0 .. 5;
      Arr at 0 range 0 .. 5;
   end record;

   --  event mask register
   type C1EMR1_Register is record
      --  Wakeup with event generation Mask on Event input
      EM             : C1EMR1_EM_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_16 : HAL.Bit := 16#0#;
      --  Wakeup with event generation Mask on Event input
      EM_1           : C1EMR1_EM_Field_1 := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C1EMR1_Register use record
      EM             at 0 range 0 .. 15;
      Reserved_16_16 at 0 range 16 .. 16;
      EM_1           at 0 range 17 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  C1IMR2_IM array
   type C1IMR2_IM_Field_Array is array (36 .. 46) of Boolean
     with Component_Size => 1, Size => 11;

   --  Type definition for C1IMR2_IM
   type C1IMR2_IM_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IM as a value
            Val : HAL.UInt11;
         when True =>
            --  IM as an array
            Arr : C1IMR2_IM_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 11;

   for C1IMR2_IM_Field use record
      Val at 0 range 0 .. 10;
      Arr at 0 range 0 .. 10;
   end record;

   --  wakeup with interrupt mask register
   type C1IMR2_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  wakeup with interrupt mask on event input
      IM34           : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  wakeup with interrupt mask on event input
      IM             : C1IMR2_IM_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C1IMR2_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      IM34           at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      IM             at 0 range 4 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   --  C1EMR2_EM array
   type C1EMR2_EM_Field_Array is array (40 .. 41) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for C1EMR2_EM
   type C1EMR2_EM_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EM as a value
            Val : HAL.UInt2;
         when True =>
            --  EM as an array
            Arr : C1EMR2_EM_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for C1EMR2_EM_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  wakeup with event mask register
   type C1EMR2_Register is record
      --  unspecified
      Reserved_0_7   : HAL.UInt8 := 16#0#;
      --  Wakeup with event generation Mask on Event input
      EM             : C1EMR2_EM_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C1EMR2_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      EM             at 0 range 8 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   --  C2EMR1_EM array
   type C2EMR1_EM_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for C2EMR1_EM
   type C2EMR1_EM_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EM as a value
            Val : HAL.UInt16;
         when True =>
            --  EM as an array
            Arr : C2EMR1_EM_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for C2EMR1_EM_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  C2EMR1_EM array
   type C2EMR1_EM_Field_Array_1 is array (17 .. 22) of Boolean
     with Component_Size => 1, Size => 6;

   --  Type definition for C2EMR1_EM
   type C2EMR1_EM_Field_1
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EM as a value
            Val : HAL.UInt6;
         when True =>
            --  EM as an array
            Arr : C2EMR1_EM_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 6;

   for C2EMR1_EM_Field_1 use record
      Val at 0 range 0 .. 5;
      Arr at 0 range 0 .. 5;
   end record;

   --  event mask register
   type C2EMR1_Register is record
      --  Wakeup with event generation Mask on Event input
      EM             : C2EMR1_EM_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_16 : HAL.Bit := 16#0#;
      --  Wakeup with event generation Mask on Event input
      EM_1           : C2EMR1_EM_Field_1 := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C2EMR1_Register use record
      EM             at 0 range 0 .. 15;
      Reserved_16_16 at 0 range 16 .. 16;
      EM_1           at 0 range 17 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  C2IMR2_IM array
   type C2IMR2_IM_Field_Array is array (36 .. 46) of Boolean
     with Component_Size => 1, Size => 11;

   --  Type definition for C2IMR2_IM
   type C2IMR2_IM_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IM as a value
            Val : HAL.UInt11;
         when True =>
            --  IM as an array
            Arr : C2IMR2_IM_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 11;

   for C2IMR2_IM_Field use record
      Val at 0 range 0 .. 10;
      Arr at 0 range 0 .. 10;
   end record;

   --  wakeup with interrupt mask register
   type C2IMR2_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  wakeup with interrupt mask on event input
      IM34           : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  wakeup with interrupt mask on event input
      IM             : C2IMR2_IM_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C2IMR2_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      IM34           at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      IM             at 0 range 4 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   --  C2EMR2_EM array
   type C2EMR2_EM_Field_Array is array (40 .. 41) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for C2EMR2_EM
   type C2EMR2_EM_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EM as a value
            Val : HAL.UInt2;
         when True =>
            --  EM as an array
            Arr : C2EMR2_EM_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for C2EMR2_EM_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  wakeup with event mask register
   type C2EMR2_Register is record
      --  unspecified
      Reserved_0_7   : HAL.UInt8 := 16#0#;
      --  Wakeup with event generation Mask on Event input
      EM             : C2EMR2_EM_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C2EMR2_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      EM             at 0 range 8 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  External interrupt/event controller
   type EXTI_Peripheral is record
      --  rising trigger selection register
      RTSR1  : aliased RTSR1_Register;
      --  falling trigger selection register
      FTSR1  : aliased FTSR1_Register;
      --  software interrupt event register
      SWIER1 : aliased SWIER1_Register;
      --  EXTI pending register
      PR1    : aliased PR1_Register;
      --  rising trigger selection register
      RTSR2  : aliased RTSR2_Register;
      --  falling trigger selection register
      FTSR2  : aliased FTSR2_Register;
      --  software interrupt event register
      SWIER2 : aliased SWIER2_Register;
      --  pending register
      PR2    : aliased PR2_Register;
      --  interrupt mask register
      C1IMR1 : aliased HAL.UInt32;
      --  event mask register
      C1EMR1 : aliased C1EMR1_Register;
      --  wakeup with interrupt mask register
      C1IMR2 : aliased C1IMR2_Register;
      --  wakeup with event mask register
      C1EMR2 : aliased C1EMR2_Register;
      --  interrupt mask register
      C2IMR1 : aliased HAL.UInt32;
      --  event mask register
      C2EMR1 : aliased C2EMR1_Register;
      --  wakeup with interrupt mask register
      C2IMR2 : aliased C2IMR2_Register;
      --  wakeup with event mask register
      C2EMR2 : aliased C2EMR2_Register;
   end record
     with Volatile;

   for EXTI_Peripheral use record
      RTSR1  at 16#0# range 0 .. 31;
      FTSR1  at 16#4# range 0 .. 31;
      SWIER1 at 16#8# range 0 .. 31;
      PR1    at 16#C# range 0 .. 31;
      RTSR2  at 16#20# range 0 .. 31;
      FTSR2  at 16#24# range 0 .. 31;
      SWIER2 at 16#28# range 0 .. 31;
      PR2    at 16#2C# range 0 .. 31;
      C1IMR1 at 16#80# range 0 .. 31;
      C1EMR1 at 16#84# range 0 .. 31;
      C1IMR2 at 16#90# range 0 .. 31;
      C1EMR2 at 16#94# range 0 .. 31;
      C2IMR1 at 16#C0# range 0 .. 31;
      C2EMR1 at 16#C4# range 0 .. 31;
      C2IMR2 at 16#D0# range 0 .. 31;
      C2EMR2 at 16#D4# range 0 .. 31;
   end record;

   --  External interrupt/event controller
   EXTI_Periph : aliased EXTI_Peripheral
     with Import, Address => EXTI_Base;

end STM32_SVD.EXTI;
