--  This spec has been automatically generated from STM32U073.svd

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

   --  EXTI_RTSR1_RT array
   type EXTI_RTSR1_RT_Field_Array is array (0 .. 21) of Boolean
     with Component_Size => 1, Size => 22;

   --  Type definition for EXTI_RTSR1_RT
   type EXTI_RTSR1_RT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  RT as a value
            Val : HAL.UInt22;
         when True =>
            --  RT as an array
            Arr : EXTI_RTSR1_RT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 22;

   for EXTI_RTSR1_RT_Field use record
      Val at 0 range 0 .. 21;
      Arr at 0 range 0 .. 21;
   end record;

   --  EXTI rising trigger selection register
   type EXTI_RTSR1_Register is record
      --  Rising trigger event configuration bit of configurable line x
      --  (x1=1211to10) Each bit enables/disables the rising edge trigger for
      --  the event and interrupt on the corresponding line. Bits 18 and 19 are
      --  available only on STM32U0x3xx devices. They are reserved on
      --  STM32U031xx devices.
      RT             : EXTI_RTSR1_RT_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EXTI_RTSR1_Register use record
      RT             at 0 range 0 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  EXTI_FTSR1_FT array
   type EXTI_FTSR1_FT_Field_Array is array (0 .. 21) of Boolean
     with Component_Size => 1, Size => 22;

   --  Type definition for EXTI_FTSR1_FT
   type EXTI_FTSR1_FT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FT as a value
            Val : HAL.UInt22;
         when True =>
            --  FT as an array
            Arr : EXTI_FTSR1_FT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 22;

   for EXTI_FTSR1_FT_Field use record
      Val at 0 range 0 .. 21;
      Arr at 0 range 0 .. 21;
   end record;

   --  EXTI falling trigger selection register 1
   type EXTI_FTSR1_Register is record
      --  Falling trigger event configuration bit of configurable line x
      --  (x1=1211to10) Each bit enables/disables the falling edge trigger for
      --  the event and interrupt on the corresponding line. Bits 18 and 19 are
      --  available only on STM32U0x3xx devices. They are reserved on
      --  STM32U031xx devices.
      FT             : EXTI_FTSR1_FT_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EXTI_FTSR1_Register use record
      FT             at 0 range 0 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  EXTI_SWIER1_SWI array
   type EXTI_SWIER1_SWI_Field_Array is array (0 .. 21) of Boolean
     with Component_Size => 1, Size => 22;

   --  Type definition for EXTI_SWIER1_SWI
   type EXTI_SWIER1_SWI_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SWI as a value
            Val : HAL.UInt22;
         when True =>
            --  SWI as an array
            Arr : EXTI_SWIER1_SWI_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 22;

   for EXTI_SWIER1_SWI_Field use record
      Val at 0 range 0 .. 21;
      Arr at 0 range 0 .. 21;
   end record;

   --  EXTI software interrupt event register 1
   type EXTI_SWIER1_Register is record
      --  Software rising edge event trigger on line x (x1=1211to10) Setting of
      --  any bit by software triggers a rising edge event on the corresponding
      --  line x, resulting in an interrupt, independently of EXTI_RTSR1 and
      --  EXTI_FTSR1 settings. The bits are automatically cleared by HW.
      --  Reading of any bit always returns 0. Bits 18 and 19 are available
      --  only on STM32U0x3xx devices. They are reserved on STM32U031xx
      --  devices.
      SWI            : EXTI_SWIER1_SWI_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EXTI_SWIER1_Register use record
      SWI            at 0 range 0 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  EXTI_RPR1_RPIF array
   type EXTI_RPR1_RPIF_Field_Array is array (0 .. 21) of Boolean
     with Component_Size => 1, Size => 22;

   --  Type definition for EXTI_RPR1_RPIF
   type EXTI_RPR1_RPIF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  RPIF as a value
            Val : HAL.UInt22;
         when True =>
            --  RPIF as an array
            Arr : EXTI_RPR1_RPIF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 22;

   for EXTI_RPR1_RPIF_Field use record
      Val at 0 range 0 .. 21;
      Arr at 0 range 0 .. 21;
   end record;

   --  EXTI rising edge pending register 1
   type EXTI_RPR1_Register is record
      --  Rising edge event pending for configurable line x (x1=1211to10) Each
      --  bit is set upon a rising edge event generated by hardware or by
      --  software (through the EXTI_SWIER1 register) on the corresponding
      --  line. Each bit is cleared by writing 1 into it. Bits 18 and 19 are
      --  available only on STM32U0x3xx devices. They are reserved on
      --  STM32U031xx devices.
      RPIF           : EXTI_RPR1_RPIF_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EXTI_RPR1_Register use record
      RPIF           at 0 range 0 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  EXTI_FPR1_FPIF array
   type EXTI_FPR1_FPIF_Field_Array is array (0 .. 21) of Boolean
     with Component_Size => 1, Size => 22;

   --  Type definition for EXTI_FPR1_FPIF
   type EXTI_FPR1_FPIF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FPIF as a value
            Val : HAL.UInt22;
         when True =>
            --  FPIF as an array
            Arr : EXTI_FPR1_FPIF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 22;

   for EXTI_FPR1_FPIF_Field use record
      Val at 0 range 0 .. 21;
      Arr at 0 range 0 .. 21;
   end record;

   --  EXTI falling edge pending register 1
   type EXTI_FPR1_Register is record
      --  Falling edge event pending for configurable line x (x1=1211to10) Each
      --  bit is set upon a falling edge event generated by hardware or by
      --  software (through the EXTI_SWIER1 register) on the corresponding
      --  line. Each bit is cleared by writing 1 into it. Bits 18 and 19 are
      --  available only on STM32U0x3xx devices. They are reserved on
      --  STM32U031xx devices.
      FPIF           : EXTI_FPR1_FPIF_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EXTI_FPR1_Register use record
      FPIF           at 0 range 0 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  EXTI_EXTICR1_EXTI array element
   subtype EXTI_EXTICR1_EXTI_Element is HAL.UInt8;

   --  EXTI_EXTICR1_EXTI array
   type EXTI_EXTICR1_EXTI_Field_Array is array (0 .. 3)
     of EXTI_EXTICR1_EXTI_Element
     with Component_Size => 8, Size => 32;

   --  EXTI external interrupt selection register 1
   type EXTI_EXTICR1_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTI as a value
            Val : HAL.UInt32;
         when True =>
            --  EXTI as an array
            Arr : EXTI_EXTICR1_EXTI_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EXTI_EXTICR1_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  EXTI_EXTICR2_EXTI array element
   subtype EXTI_EXTICR2_EXTI_Element is HAL.UInt8;

   --  EXTI_EXTICR2_EXTI array
   type EXTI_EXTICR2_EXTI_Field_Array is array (4 .. 7)
     of EXTI_EXTICR2_EXTI_Element
     with Component_Size => 8, Size => 32;

   --  EXTI external interrupt selection register 2
   type EXTI_EXTICR2_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTI as a value
            Val : HAL.UInt32;
         when True =>
            --  EXTI as an array
            Arr : EXTI_EXTICR2_EXTI_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EXTI_EXTICR2_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  EXTI_EXTICR3_EXTI array element
   subtype EXTI_EXTICR3_EXTI_Element is HAL.UInt8;

   --  EXTI_EXTICR3_EXTI array
   type EXTI_EXTICR3_EXTI_Field_Array is array (8 .. 11)
     of EXTI_EXTICR3_EXTI_Element
     with Component_Size => 8, Size => 32;

   --  EXTI external interrupt selection register 3
   type EXTI_EXTICR3_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTI as a value
            Val : HAL.UInt32;
         when True =>
            --  EXTI as an array
            Arr : EXTI_EXTICR3_EXTI_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EXTI_EXTICR3_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  EXTI_EXTICR4_EXTI array element
   subtype EXTI_EXTICR4_EXTI_Element is HAL.UInt8;

   --  EXTI_EXTICR4_EXTI array
   type EXTI_EXTICR4_EXTI_Field_Array is array (12 .. 15)
     of EXTI_EXTICR4_EXTI_Element
     with Component_Size => 8, Size => 32;

   --  EXTI external interrupt selection register 4
   type EXTI_EXTICR4_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTI as a value
            Val : HAL.UInt32;
         when True =>
            --  EXTI as an array
            Arr : EXTI_EXTICR4_EXTI_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EXTI_EXTICR4_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  EXTI_IMR1_IM array
   type EXTI_IMR1_IM_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  EXTI CPU wake-up with interrupt mask register
   type EXTI_IMR1_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IM as a value
            Val : HAL.UInt32;
         when True =>
            --  IM as an array
            Arr : EXTI_IMR1_IM_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EXTI_IMR1_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  EXTI_EMR1_EM array
   type EXTI_EMR1_EM_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  EXTI CPU wake-up with event mask register
   type EXTI_EMR1_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EM as a value
            Val : HAL.UInt32;
         when True =>
            --  EM as an array
            Arr : EXTI_EMR1_EM_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EXTI_EMR1_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  EXTI_IMR2_IM array
   type EXTI_IMR2_IM_Field_Array is array (32 .. 37) of Boolean
     with Component_Size => 1, Size => 6;

   --  Type definition for EXTI_IMR2_IM
   type EXTI_IMR2_IM_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IM as a value
            Val : HAL.UInt6;
         when True =>
            --  IM as an array
            Arr : EXTI_IMR2_IM_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 6;

   for EXTI_IMR2_IM_Field use record
      Val at 0 range 0 .. 5;
      Arr at 0 range 0 .. 5;
   end record;

   --  EXTI CPU wake-up with interrupt mask register
   type EXTI_IMR2_Register is record
      --  CPU wake-up with interrupt mask on line x (x1=1371to132)
      --  Setting/clearing this bit unmasks/masks the CPU wake-up with
      --  interrupt, by an event on the corresponding line. Bit IM36 is
      --  available only on STM32U0x3xx devices, it is reserved on STM32U031xx
      --  devices.
      IM            : EXTI_IMR2_IM_Field := (As_Array => False, Val => 16#1#);
      --  unspecified
      Reserved_6_31 : HAL.UInt26 := 16#3FFFFFF#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EXTI_IMR2_Register use record
      IM            at 0 range 0 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   --  EXTI_EMR2_EM array
   type EXTI_EMR2_EM_Field_Array is array (32 .. 37) of Boolean
     with Component_Size => 1, Size => 6;

   --  Type definition for EXTI_EMR2_EM
   type EXTI_EMR2_EM_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EM as a value
            Val : HAL.UInt6;
         when True =>
            --  EM as an array
            Arr : EXTI_EMR2_EM_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 6;

   for EXTI_EMR2_EM_Field use record
      Val at 0 range 0 .. 5;
      Arr at 0 range 0 .. 5;
   end record;

   --  EXTI CPU wake-up with event mask register
   type EXTI_EMR2_Register is record
      --  CPU wake-up with event generation mask on line x, (x1=1371to132)
      --  Setting/clearing each bit unmasks/masks the CPU wake-up with event
      --  generation on the corresponding line. Bit IM36 is available only on
      --  STM32U0x3xx devices, it is reserved on STM32U031xx devices.
      EM            : EXTI_EMR2_EM_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_6_31 : HAL.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EXTI_EMR2_Register use record
      EM            at 0 range 0 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  EXTI register block
   type EXTI_Peripheral is record
      --  EXTI rising trigger selection register
      EXTI_RTSR1   : aliased EXTI_RTSR1_Register;
      --  EXTI falling trigger selection register 1
      EXTI_FTSR1   : aliased EXTI_FTSR1_Register;
      --  EXTI software interrupt event register 1
      EXTI_SWIER1  : aliased EXTI_SWIER1_Register;
      --  EXTI rising edge pending register 1
      EXTI_RPR1    : aliased EXTI_RPR1_Register;
      --  EXTI falling edge pending register 1
      EXTI_FPR1    : aliased EXTI_FPR1_Register;
      --  EXTI external interrupt selection register 1
      EXTI_EXTICR1 : aliased EXTI_EXTICR1_Register;
      --  EXTI external interrupt selection register 2
      EXTI_EXTICR2 : aliased EXTI_EXTICR2_Register;
      --  EXTI external interrupt selection register 3
      EXTI_EXTICR3 : aliased EXTI_EXTICR3_Register;
      --  EXTI external interrupt selection register 4
      EXTI_EXTICR4 : aliased EXTI_EXTICR4_Register;
      --  EXTI CPU wake-up with interrupt mask register
      EXTI_IMR1    : aliased EXTI_IMR1_Register;
      --  EXTI CPU wake-up with event mask register
      EXTI_EMR1    : aliased EXTI_EMR1_Register;
      --  EXTI CPU wake-up with interrupt mask register
      EXTI_IMR2    : aliased EXTI_IMR2_Register;
      --  EXTI CPU wake-up with event mask register
      EXTI_EMR2    : aliased EXTI_EMR2_Register;
   end record
     with Volatile;

   for EXTI_Peripheral use record
      EXTI_RTSR1   at 16#0# range 0 .. 31;
      EXTI_FTSR1   at 16#4# range 0 .. 31;
      EXTI_SWIER1  at 16#8# range 0 .. 31;
      EXTI_RPR1    at 16#C# range 0 .. 31;
      EXTI_FPR1    at 16#10# range 0 .. 31;
      EXTI_EXTICR1 at 16#60# range 0 .. 31;
      EXTI_EXTICR2 at 16#64# range 0 .. 31;
      EXTI_EXTICR3 at 16#68# range 0 .. 31;
      EXTI_EXTICR4 at 16#6C# range 0 .. 31;
      EXTI_IMR1    at 16#80# range 0 .. 31;
      EXTI_EMR1    at 16#84# range 0 .. 31;
      EXTI_IMR2    at 16#90# range 0 .. 31;
      EXTI_EMR2    at 16#94# range 0 .. 31;
   end record;

   --  EXTI register block
   EXTI_Periph : aliased EXTI_Peripheral
     with Import, Address => EXTI_Base;

end STM32_SVD.EXTI;
