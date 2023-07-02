--  This spec has been automatically generated from STM32U585.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.LPGPIO is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  LPGPIO_MODER_MODE array
   type LPGPIO_MODER_MODE_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for LPGPIO_MODER_MODE
   type LPGPIO_MODER_MODE_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MODE as a value
            Val : HAL.UInt16;
         when True =>
            --  MODE as an array
            Arr : LPGPIO_MODER_MODE_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for LPGPIO_MODER_MODE_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  LPGPIO port mode register
   type LPGPIO_MODER_Register is record
      --  MODE0
      MODE           : LPGPIO_MODER_MODE_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPGPIO_MODER_Register use record
      MODE           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype LPGPIO_IDR_IDy_Field is HAL.UInt16;

   --  LPGPIO port input data register
   type LPGPIO_IDR_Register is record
      --  Read-only. IDy
      IDy            : LPGPIO_IDR_IDy_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPGPIO_IDR_Register use record
      IDy            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  LPGPIO_ODR_ODy array
   type LPGPIO_ODR_ODy_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for LPGPIO_ODR_ODy
   type LPGPIO_ODR_ODy_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  ODy as a value
            Val : HAL.UInt16;
         when True =>
            --  ODy as an array
            Arr : LPGPIO_ODR_ODy_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for LPGPIO_ODR_ODy_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  LPGPIO port output data register
   type LPGPIO_ODR_Register is record
      --  ODy0
      ODy            : LPGPIO_ODR_ODy_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPGPIO_ODR_Register use record
      ODy            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  LPGPIO_BSRR_BSy array
   type LPGPIO_BSRR_BSy_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for LPGPIO_BSRR_BSy
   type LPGPIO_BSRR_BSy_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  BSy as a value
            Val : HAL.UInt16;
         when True =>
            --  BSy as an array
            Arr : LPGPIO_BSRR_BSy_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for LPGPIO_BSRR_BSy_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  LPGPIO_BSRR_BRy array
   type LPGPIO_BSRR_BRy_Field_Array is array (16 .. 31) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for LPGPIO_BSRR_BRy
   type LPGPIO_BSRR_BRy_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  BRy as a value
            Val : HAL.UInt16;
         when True =>
            --  BRy as an array
            Arr : LPGPIO_BSRR_BRy_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for LPGPIO_BSRR_BRy_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  LPGPIO port bit set/reset register
   type LPGPIO_BSRR_Register is record
      --  Write-only. BSy0
      BSy : LPGPIO_BSRR_BSy_Field := (As_Array => False, Val => 16#0#);
      --  Write-only. BRy16
      BRy : LPGPIO_BSRR_BRy_Field := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPGPIO_BSRR_Register use record
      BSy at 0 range 0 .. 15;
      BRy at 0 range 16 .. 31;
   end record;

   --  LPGPIO_BRR_BRy array
   type LPGPIO_BRR_BRy_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for LPGPIO_BRR_BRy
   type LPGPIO_BRR_BRy_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  BRy as a value
            Val : HAL.UInt16;
         when True =>
            --  BRy as an array
            Arr : LPGPIO_BRR_BRy_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for LPGPIO_BRR_BRy_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  LPGPIO port bit reset register
   type LPGPIO_BRR_Register is record
      --  Read-only. BRy0
      BRy            : LPGPIO_BRR_BRy_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPGPIO_BRR_Register use record
      BRy            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  LPGPIO1
   type LPGPIO_Peripheral is record
      --  LPGPIO port mode register
      LPGPIO_MODER : aliased LPGPIO_MODER_Register;
      --  LPGPIO port input data register
      LPGPIO_IDR   : aliased LPGPIO_IDR_Register;
      --  LPGPIO port output data register
      LPGPIO_ODR   : aliased LPGPIO_ODR_Register;
      --  LPGPIO port bit set/reset register
      LPGPIO_BSRR  : aliased LPGPIO_BSRR_Register;
      --  LPGPIO port bit reset register
      LPGPIO_BRR   : aliased LPGPIO_BRR_Register;
   end record
     with Volatile;

   for LPGPIO_Peripheral use record
      LPGPIO_MODER at 16#0# range 0 .. 31;
      LPGPIO_IDR   at 16#10# range 0 .. 31;
      LPGPIO_ODR   at 16#14# range 0 .. 31;
      LPGPIO_BSRR  at 16#18# range 0 .. 31;
      LPGPIO_BRR   at 16#28# range 0 .. 31;
   end record;

   --  LPGPIO1
   LPGPIO1_Periph : aliased LPGPIO_Peripheral
     with Import, Address => LPGPIO1_Base;

   --  LPGPIO1
   SEC_LPGPIO1_Periph : aliased LPGPIO_Peripheral
     with Import, Address => SEC_LPGPIO1_Base;

end STM32_SVD.LPGPIO;
