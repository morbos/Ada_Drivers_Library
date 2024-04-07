--  This spec has been automatically generated from STM32L562.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.SEC_GTZC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  MPCBB control register
   type MPCBB1_CR_Register is record
      --  LCK
      LCK           : Boolean := False;
      --  unspecified
      Reserved_1_29 : HAL.UInt29 := 16#0#;
      --  INVSECSTATE
      INVSECSTATE   : Boolean := False;
      --  SRWILADIS
      SRWILADIS     : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_CR_Register use record
      LCK           at 0 range 0 .. 0;
      Reserved_1_29 at 0 range 1 .. 29;
      INVSECSTATE   at 0 range 30 .. 30;
      SRWILADIS     at 0 range 31 .. 31;
   end record;

   --  MPCBB1_LCKVTR1_LCKSB array
   type MPCBB1_LCKVTR1_LCKSB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBB control register
   type MPCBB1_LCKVTR1_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  LCKSB as a value
            Val : HAL.UInt32;
         when True =>
            --  LCKSB as an array
            Arr : MPCBB1_LCKVTR1_LCKSB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_LCKVTR1_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_LCKVTR2_LCKSB array
   type MPCBB1_LCKVTR2_LCKSB_Field_Array is array (32 .. 63) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBB control register
   type MPCBB1_LCKVTR2_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  LCKSB as a value
            Val : HAL.UInt32;
         when True =>
            --  LCKSB as an array
            Arr : MPCBB1_LCKVTR2_LCKSB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_LCKVTR2_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR0_B array
   type MPCBB1_VCTR0_B_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR0_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR0_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR0_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR1_B array
   type MPCBB1_VCTR1_B_Field_Array is array (32 .. 63) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR1_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR1_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR1_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR2_B array
   type MPCBB1_VCTR2_B_Field_Array is array (64 .. 95) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR2_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR2_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR2_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR3_B array
   type MPCBB1_VCTR3_B_Field_Array is array (96 .. 127) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR3_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR3_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR3_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR4_B array
   type MPCBB1_VCTR4_B_Field_Array is array (128 .. 159) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR4_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR4_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR4_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR5_B array
   type MPCBB1_VCTR5_B_Field_Array is array (160 .. 191) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR5_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR5_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR5_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR6_B array
   type MPCBB1_VCTR6_B_Field_Array is array (192 .. 223) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR6_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR6_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR6_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR7_B array
   type MPCBB1_VCTR7_B_Field_Array is array (224 .. 255) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR7_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR7_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR7_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR8_B array
   type MPCBB1_VCTR8_B_Field_Array is array (256 .. 287) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR8_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR8_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR8_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR9_B array
   type MPCBB1_VCTR9_B_Field_Array is array (288 .. 319) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR9_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR9_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR9_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR10_B array
   type MPCBB1_VCTR10_B_Field_Array is array (320 .. 351) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR10_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR10_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR10_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR11_B array
   type MPCBB1_VCTR11_B_Field_Array is array (352 .. 383) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR11_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR11_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR11_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR12_B array
   type MPCBB1_VCTR12_B_Field_Array is array (384 .. 415) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR12_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR12_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR12_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR13_B array
   type MPCBB1_VCTR13_B_Field_Array is array (416 .. 447) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR13_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR13_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR13_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR14_B array
   type MPCBB1_VCTR14_B_Field_Array is array (448 .. 479) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR14_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR14_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR14_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR15_B array
   type MPCBB1_VCTR15_B_Field_Array is array (480 .. 511) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR15_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR15_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR15_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR16_B array
   type MPCBB1_VCTR16_B_Field_Array is array (512 .. 543) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR16_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR16_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR16_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR17_B array
   type MPCBB1_VCTR17_B_Field_Array is array (544 .. 575) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR17_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR17_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR17_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR18_B array
   type MPCBB1_VCTR18_B_Field_Array is array (576 .. 607) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR18_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR18_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR18_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR19_B array
   type MPCBB1_VCTR19_B_Field_Array is array (608 .. 639) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR19_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR19_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR19_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR20_B array
   type MPCBB1_VCTR20_B_Field_Array is array (640 .. 671) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR20_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR20_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR20_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR21_B array
   type MPCBB1_VCTR21_B_Field_Array is array (672 .. 703) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR21_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR21_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR21_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR22_B array
   type MPCBB1_VCTR22_B_Field_Array is array (704 .. 735) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR22_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR22_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR22_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR23_B array
   type MPCBB1_VCTR23_B_Field_Array is array (736 .. 767) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR23_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR23_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR23_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR24_B array
   type MPCBB1_VCTR24_B_Field_Array is array (768 .. 799) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR24_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR24_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR24_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR25_B array
   type MPCBB1_VCTR25_B_Field_Array is array (800 .. 831) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR25_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR25_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR25_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR26_B array
   type MPCBB1_VCTR26_B_Field_Array is array (832 .. 863) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR26_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR26_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR26_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR27_B array
   type MPCBB1_VCTR27_B_Field_Array is array (864 .. 895) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR27_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR27_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR27_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR28_B array
   type MPCBB1_VCTR28_B_Field_Array is array (896 .. 927) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR28_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR28_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR28_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR29_B array
   type MPCBB1_VCTR29_B_Field_Array is array (928 .. 959) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR29_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR29_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR29_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR30_B array
   type MPCBB1_VCTR30_B_Field_Array is array (960 .. 991) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR30_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR30_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR30_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR31_B array
   type MPCBB1_VCTR31_B_Field_Array is array (992 .. 1023) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR31_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR31_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR31_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR32_B array
   type MPCBB1_VCTR32_B_Field_Array is array (1024 .. 1055) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR32_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR32_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR32_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR33_B array
   type MPCBB1_VCTR33_B_Field_Array is array (1056 .. 1087) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR33_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR33_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR33_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR34_B array
   type MPCBB1_VCTR34_B_Field_Array is array (1088 .. 1119) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR34_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR34_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR34_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR35_B array
   type MPCBB1_VCTR35_B_Field_Array is array (1120 .. 1151) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR35_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR35_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR35_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR36_B array
   type MPCBB1_VCTR36_B_Field_Array is array (1152 .. 1183) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR36_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR36_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR36_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR37_B array
   type MPCBB1_VCTR37_B_Field_Array is array (1184 .. 1215) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR37_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR37_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR37_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR38_B array
   type MPCBB1_VCTR38_B_Field_Array is array (1216 .. 1247) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR38_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR38_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR38_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR39_B array
   type MPCBB1_VCTR39_B_Field_Array is array (1248 .. 1279) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR39_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR39_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR39_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR40_B array
   type MPCBB1_VCTR40_B_Field_Array is array (1280 .. 1311) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR40_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR40_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR40_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR41_B array
   type MPCBB1_VCTR41_B_Field_Array is array (1312 .. 1343) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR41_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR41_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR41_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR42_B array
   type MPCBB1_VCTR42_B_Field_Array is array (1344 .. 1375) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR42_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR42_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR42_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR43_B array
   type MPCBB1_VCTR43_B_Field_Array is array (1376 .. 1407) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR43_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR43_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR43_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR44_B array
   type MPCBB1_VCTR44_B_Field_Array is array (1408 .. 1439) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR44_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR44_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR44_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR45_B array
   type MPCBB1_VCTR45_B_Field_Array is array (1440 .. 1471) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR45_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR45_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR45_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR46_B array
   type MPCBB1_VCTR46_B_Field_Array is array (1472 .. 1503) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR46_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR46_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR46_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR47_B array
   type MPCBB1_VCTR47_B_Field_Array is array (1504 .. 1535) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR47_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR47_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR47_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR48_B array
   type MPCBB1_VCTR48_B_Field_Array is array (1536 .. 1567) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR48_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR48_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR48_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR49_B array
   type MPCBB1_VCTR49_B_Field_Array is array (1568 .. 1599) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR49_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR49_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR49_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR50_B array
   type MPCBB1_VCTR50_B_Field_Array is array (1600 .. 1631) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR50_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR50_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR50_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR51_B array
   type MPCBB1_VCTR51_B_Field_Array is array (1632 .. 1663) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR51_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR51_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR51_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR52_B array
   type MPCBB1_VCTR52_B_Field_Array is array (1664 .. 1695) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR52_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR52_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR52_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR53_B array
   type MPCBB1_VCTR53_B_Field_Array is array (1696 .. 1727) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR53_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR53_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR53_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR54_B array
   type MPCBB1_VCTR54_B_Field_Array is array (1728 .. 1759) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR54_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR54_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR54_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR55_B array
   type MPCBB1_VCTR55_B_Field_Array is array (1760 .. 1791) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR55_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR55_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR55_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR56_B array
   type MPCBB1_VCTR56_B_Field_Array is array (1792 .. 1823) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR56_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR56_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR56_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR57_B array
   type MPCBB1_VCTR57_B_Field_Array is array (1824 .. 1855) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR57_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR57_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR57_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR58_B array
   type MPCBB1_VCTR58_B_Field_Array is array (1856 .. 1887) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR58_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR58_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR58_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR59_B array
   type MPCBB1_VCTR59_B_Field_Array is array (1888 .. 1919) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR59_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR59_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR59_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR60_B array
   type MPCBB1_VCTR60_B_Field_Array is array (1920 .. 1951) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR60_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR60_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR60_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR61_B array
   type MPCBB1_VCTR61_B_Field_Array is array (1952 .. 1983) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR61_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR61_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR61_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR62_B array
   type MPCBB1_VCTR62_B_Field_Array is array (1984 .. 2015) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR62_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR62_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR62_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB1_VCTR63_B array
   type MPCBB1_VCTR63_B_Field_Array is array (2016 .. 2047) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB1_VCTR63_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB1_VCTR63_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB1_VCTR63_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB control register
   type MPCBB2_CR_Register is record
      --  LCK
      LCK           : Boolean := False;
      --  unspecified
      Reserved_1_29 : HAL.UInt29 := 16#0#;
      --  INVSECSTATE
      INVSECSTATE   : Boolean := False;
      --  SRWILADIS
      SRWILADIS     : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_CR_Register use record
      LCK           at 0 range 0 .. 0;
      Reserved_1_29 at 0 range 1 .. 29;
      INVSECSTATE   at 0 range 30 .. 30;
      SRWILADIS     at 0 range 31 .. 31;
   end record;

   --  MPCBB2_LCKVTR1_LCKSB array
   type MPCBB2_LCKVTR1_LCKSB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBB control register
   type MPCBB2_LCKVTR1_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  LCKSB as a value
            Val : HAL.UInt32;
         when True =>
            --  LCKSB as an array
            Arr : MPCBB2_LCKVTR1_LCKSB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_LCKVTR1_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_LCKVTR2_LCKSB array
   type MPCBB2_LCKVTR2_LCKSB_Field_Array is array (32 .. 63) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBB control register
   type MPCBB2_LCKVTR2_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  LCKSB as a value
            Val : HAL.UInt32;
         when True =>
            --  LCKSB as an array
            Arr : MPCBB2_LCKVTR2_LCKSB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_LCKVTR2_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR0_B array
   type MPCBB2_VCTR0_B_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR0_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR0_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR0_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR1_B array
   type MPCBB2_VCTR1_B_Field_Array is array (32 .. 63) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR1_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR1_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR1_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR2_B array
   type MPCBB2_VCTR2_B_Field_Array is array (64 .. 95) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR2_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR2_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR2_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR3_B array
   type MPCBB2_VCTR3_B_Field_Array is array (96 .. 127) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR3_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR3_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR3_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR4_B array
   type MPCBB2_VCTR4_B_Field_Array is array (128 .. 159) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR4_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR4_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR4_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR5_B array
   type MPCBB2_VCTR5_B_Field_Array is array (160 .. 191) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR5_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR5_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR5_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR6_B array
   type MPCBB2_VCTR6_B_Field_Array is array (192 .. 223) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR6_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR6_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR6_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR7_B array
   type MPCBB2_VCTR7_B_Field_Array is array (224 .. 255) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR7_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR7_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR7_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR8_B array
   type MPCBB2_VCTR8_B_Field_Array is array (256 .. 287) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR8_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR8_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR8_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR9_B array
   type MPCBB2_VCTR9_B_Field_Array is array (288 .. 319) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR9_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR9_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR9_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR10_B array
   type MPCBB2_VCTR10_B_Field_Array is array (320 .. 351) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR10_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR10_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR10_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR11_B array
   type MPCBB2_VCTR11_B_Field_Array is array (352 .. 383) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR11_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR11_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR11_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR12_B array
   type MPCBB2_VCTR12_B_Field_Array is array (384 .. 415) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR12_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR12_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR12_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR13_B array
   type MPCBB2_VCTR13_B_Field_Array is array (416 .. 447) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR13_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR13_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR13_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR14_B array
   type MPCBB2_VCTR14_B_Field_Array is array (448 .. 479) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR14_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR14_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR14_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR15_B array
   type MPCBB2_VCTR15_B_Field_Array is array (480 .. 511) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR15_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR15_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR15_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR16_B array
   type MPCBB2_VCTR16_B_Field_Array is array (512 .. 543) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR16_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR16_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR16_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR17_B array
   type MPCBB2_VCTR17_B_Field_Array is array (544 .. 575) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR17_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR17_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR17_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR18_B array
   type MPCBB2_VCTR18_B_Field_Array is array (576 .. 607) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR18_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR18_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR18_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR19_B array
   type MPCBB2_VCTR19_B_Field_Array is array (608 .. 639) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR19_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR19_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR19_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR20_B array
   type MPCBB2_VCTR20_B_Field_Array is array (640 .. 671) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR20_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR20_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR20_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR21_B array
   type MPCBB2_VCTR21_B_Field_Array is array (672 .. 703) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR21_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR21_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR21_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR22_B array
   type MPCBB2_VCTR22_B_Field_Array is array (704 .. 735) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR22_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR22_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR22_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR23_B array
   type MPCBB2_VCTR23_B_Field_Array is array (736 .. 767) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR23_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR23_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR23_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR24_B array
   type MPCBB2_VCTR24_B_Field_Array is array (768 .. 799) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR24_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR24_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR24_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR25_B array
   type MPCBB2_VCTR25_B_Field_Array is array (800 .. 831) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR25_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR25_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR25_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR26_B array
   type MPCBB2_VCTR26_B_Field_Array is array (832 .. 863) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR26_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR26_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR26_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR27_B array
   type MPCBB2_VCTR27_B_Field_Array is array (864 .. 895) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR27_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR27_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR27_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR28_B array
   type MPCBB2_VCTR28_B_Field_Array is array (896 .. 927) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR28_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR28_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR28_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR29_B array
   type MPCBB2_VCTR29_B_Field_Array is array (928 .. 959) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR29_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR29_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR29_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR30_B array
   type MPCBB2_VCTR30_B_Field_Array is array (960 .. 991) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR30_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR30_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR30_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR31_B array
   type MPCBB2_VCTR31_B_Field_Array is array (992 .. 1023) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR31_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR31_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR31_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR32_B array
   type MPCBB2_VCTR32_B_Field_Array is array (1024 .. 1055) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR32_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR32_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR32_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR33_B array
   type MPCBB2_VCTR33_B_Field_Array is array (1056 .. 1087) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR33_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR33_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR33_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR34_B array
   type MPCBB2_VCTR34_B_Field_Array is array (1088 .. 1119) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR34_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR34_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR34_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR35_B array
   type MPCBB2_VCTR35_B_Field_Array is array (1120 .. 1151) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR35_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR35_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR35_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR36_B array
   type MPCBB2_VCTR36_B_Field_Array is array (1152 .. 1183) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR36_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR36_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR36_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR37_B array
   type MPCBB2_VCTR37_B_Field_Array is array (1184 .. 1215) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR37_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR37_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR37_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR38_B array
   type MPCBB2_VCTR38_B_Field_Array is array (1216 .. 1247) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR38_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR38_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR38_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR39_B array
   type MPCBB2_VCTR39_B_Field_Array is array (1248 .. 1279) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR39_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR39_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR39_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR40_B array
   type MPCBB2_VCTR40_B_Field_Array is array (1280 .. 1311) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR40_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR40_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR40_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR41_B array
   type MPCBB2_VCTR41_B_Field_Array is array (1312 .. 1343) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR41_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR41_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR41_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR42_B array
   type MPCBB2_VCTR42_B_Field_Array is array (1344 .. 1375) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR42_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR42_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR42_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR43_B array
   type MPCBB2_VCTR43_B_Field_Array is array (1376 .. 1407) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR43_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR43_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR43_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR44_B array
   type MPCBB2_VCTR44_B_Field_Array is array (1408 .. 1439) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR44_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR44_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR44_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR45_B array
   type MPCBB2_VCTR45_B_Field_Array is array (1440 .. 1471) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR45_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR45_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR45_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR46_B array
   type MPCBB2_VCTR46_B_Field_Array is array (1472 .. 1503) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR46_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR46_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR46_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR47_B array
   type MPCBB2_VCTR47_B_Field_Array is array (1504 .. 1535) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR47_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR47_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR47_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR48_B array
   type MPCBB2_VCTR48_B_Field_Array is array (1536 .. 1567) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR48_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR48_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR48_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR49_B array
   type MPCBB2_VCTR49_B_Field_Array is array (1568 .. 1599) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR49_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR49_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR49_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR50_B array
   type MPCBB2_VCTR50_B_Field_Array is array (1600 .. 1631) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR50_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR50_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR50_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR51_B array
   type MPCBB2_VCTR51_B_Field_Array is array (1632 .. 1663) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR51_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR51_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR51_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR52_B array
   type MPCBB2_VCTR52_B_Field_Array is array (1664 .. 1695) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR52_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR52_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR52_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR53_B array
   type MPCBB2_VCTR53_B_Field_Array is array (1696 .. 1727) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR53_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR53_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR53_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR54_B array
   type MPCBB2_VCTR54_B_Field_Array is array (1728 .. 1759) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR54_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR54_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR54_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR55_B array
   type MPCBB2_VCTR55_B_Field_Array is array (1760 .. 1791) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR55_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR55_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR55_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR56_B array
   type MPCBB2_VCTR56_B_Field_Array is array (1792 .. 1823) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR56_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR56_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR56_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR57_B array
   type MPCBB2_VCTR57_B_Field_Array is array (1824 .. 1855) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR57_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR57_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR57_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR58_B array
   type MPCBB2_VCTR58_B_Field_Array is array (1856 .. 1887) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR58_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR58_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR58_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR59_B array
   type MPCBB2_VCTR59_B_Field_Array is array (1888 .. 1919) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR59_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR59_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR59_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR60_B array
   type MPCBB2_VCTR60_B_Field_Array is array (1920 .. 1951) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR60_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR60_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR60_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR61_B array
   type MPCBB2_VCTR61_B_Field_Array is array (1952 .. 1983) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR61_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR61_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR61_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR62_B array
   type MPCBB2_VCTR62_B_Field_Array is array (1984 .. 2015) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR62_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR62_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR62_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  MPCBB2_VCTR63_B array
   type MPCBB2_VCTR63_B_Field_Array is array (2016 .. 2047) of Boolean
     with Component_Size => 1, Size => 32;

   --  MPCBBx vector register
   type MPCBB2_VCTR63_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  B as a value
            Val : HAL.UInt32;
         when True =>
            --  B as an array
            Arr : MPCBB2_VCTR63_B_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for MPCBB2_VCTR63_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  SEC_GTZC_MPCBB1
   type SEC_GTZC_MPCBB1_Peripheral is record
      --  MPCBB control register
      MPCBB1_CR      : aliased MPCBB1_CR_Register;
      --  MPCBB control register
      MPCBB1_LCKVTR1 : aliased MPCBB1_LCKVTR1_Register;
      --  MPCBB control register
      MPCBB1_LCKVTR2 : aliased MPCBB1_LCKVTR2_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR0   : aliased MPCBB1_VCTR0_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR1   : aliased MPCBB1_VCTR1_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR2   : aliased MPCBB1_VCTR2_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR3   : aliased MPCBB1_VCTR3_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR4   : aliased MPCBB1_VCTR4_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR5   : aliased MPCBB1_VCTR5_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR6   : aliased MPCBB1_VCTR6_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR7   : aliased MPCBB1_VCTR7_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR8   : aliased MPCBB1_VCTR8_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR9   : aliased MPCBB1_VCTR9_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR10  : aliased MPCBB1_VCTR10_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR11  : aliased MPCBB1_VCTR11_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR12  : aliased MPCBB1_VCTR12_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR13  : aliased MPCBB1_VCTR13_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR14  : aliased MPCBB1_VCTR14_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR15  : aliased MPCBB1_VCTR15_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR16  : aliased MPCBB1_VCTR16_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR17  : aliased MPCBB1_VCTR17_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR18  : aliased MPCBB1_VCTR18_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR19  : aliased MPCBB1_VCTR19_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR20  : aliased MPCBB1_VCTR20_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR21  : aliased MPCBB1_VCTR21_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR22  : aliased MPCBB1_VCTR22_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR23  : aliased MPCBB1_VCTR23_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR24  : aliased MPCBB1_VCTR24_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR25  : aliased MPCBB1_VCTR25_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR26  : aliased MPCBB1_VCTR26_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR27  : aliased MPCBB1_VCTR27_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR28  : aliased MPCBB1_VCTR28_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR29  : aliased MPCBB1_VCTR29_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR30  : aliased MPCBB1_VCTR30_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR31  : aliased MPCBB1_VCTR31_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR32  : aliased MPCBB1_VCTR32_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR33  : aliased MPCBB1_VCTR33_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR34  : aliased MPCBB1_VCTR34_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR35  : aliased MPCBB1_VCTR35_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR36  : aliased MPCBB1_VCTR36_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR37  : aliased MPCBB1_VCTR37_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR38  : aliased MPCBB1_VCTR38_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR39  : aliased MPCBB1_VCTR39_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR40  : aliased MPCBB1_VCTR40_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR41  : aliased MPCBB1_VCTR41_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR42  : aliased MPCBB1_VCTR42_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR43  : aliased MPCBB1_VCTR43_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR44  : aliased MPCBB1_VCTR44_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR45  : aliased MPCBB1_VCTR45_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR46  : aliased MPCBB1_VCTR46_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR47  : aliased MPCBB1_VCTR47_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR48  : aliased MPCBB1_VCTR48_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR49  : aliased MPCBB1_VCTR49_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR50  : aliased MPCBB1_VCTR50_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR51  : aliased MPCBB1_VCTR51_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR52  : aliased MPCBB1_VCTR52_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR53  : aliased MPCBB1_VCTR53_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR54  : aliased MPCBB1_VCTR54_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR55  : aliased MPCBB1_VCTR55_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR56  : aliased MPCBB1_VCTR56_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR57  : aliased MPCBB1_VCTR57_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR58  : aliased MPCBB1_VCTR58_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR59  : aliased MPCBB1_VCTR59_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR60  : aliased MPCBB1_VCTR60_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR61  : aliased MPCBB1_VCTR61_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR62  : aliased MPCBB1_VCTR62_Register;
      --  MPCBBx vector register
      MPCBB1_VCTR63  : aliased MPCBB1_VCTR63_Register;
   end record
     with Volatile;

   for SEC_GTZC_MPCBB1_Peripheral use record
      MPCBB1_CR      at 16#0# range 0 .. 31;
      MPCBB1_LCKVTR1 at 16#10# range 0 .. 31;
      MPCBB1_LCKVTR2 at 16#14# range 0 .. 31;
      MPCBB1_VCTR0   at 16#100# range 0 .. 31;
      MPCBB1_VCTR1   at 16#104# range 0 .. 31;
      MPCBB1_VCTR2   at 16#108# range 0 .. 31;
      MPCBB1_VCTR3   at 16#10C# range 0 .. 31;
      MPCBB1_VCTR4   at 16#110# range 0 .. 31;
      MPCBB1_VCTR5   at 16#114# range 0 .. 31;
      MPCBB1_VCTR6   at 16#118# range 0 .. 31;
      MPCBB1_VCTR7   at 16#11C# range 0 .. 31;
      MPCBB1_VCTR8   at 16#120# range 0 .. 31;
      MPCBB1_VCTR9   at 16#124# range 0 .. 31;
      MPCBB1_VCTR10  at 16#128# range 0 .. 31;
      MPCBB1_VCTR11  at 16#12C# range 0 .. 31;
      MPCBB1_VCTR12  at 16#130# range 0 .. 31;
      MPCBB1_VCTR13  at 16#134# range 0 .. 31;
      MPCBB1_VCTR14  at 16#138# range 0 .. 31;
      MPCBB1_VCTR15  at 16#13C# range 0 .. 31;
      MPCBB1_VCTR16  at 16#140# range 0 .. 31;
      MPCBB1_VCTR17  at 16#144# range 0 .. 31;
      MPCBB1_VCTR18  at 16#148# range 0 .. 31;
      MPCBB1_VCTR19  at 16#14C# range 0 .. 31;
      MPCBB1_VCTR20  at 16#150# range 0 .. 31;
      MPCBB1_VCTR21  at 16#154# range 0 .. 31;
      MPCBB1_VCTR22  at 16#158# range 0 .. 31;
      MPCBB1_VCTR23  at 16#15C# range 0 .. 31;
      MPCBB1_VCTR24  at 16#160# range 0 .. 31;
      MPCBB1_VCTR25  at 16#164# range 0 .. 31;
      MPCBB1_VCTR26  at 16#168# range 0 .. 31;
      MPCBB1_VCTR27  at 16#16C# range 0 .. 31;
      MPCBB1_VCTR28  at 16#170# range 0 .. 31;
      MPCBB1_VCTR29  at 16#174# range 0 .. 31;
      MPCBB1_VCTR30  at 16#178# range 0 .. 31;
      MPCBB1_VCTR31  at 16#17C# range 0 .. 31;
      MPCBB1_VCTR32  at 16#180# range 0 .. 31;
      MPCBB1_VCTR33  at 16#184# range 0 .. 31;
      MPCBB1_VCTR34  at 16#188# range 0 .. 31;
      MPCBB1_VCTR35  at 16#18C# range 0 .. 31;
      MPCBB1_VCTR36  at 16#190# range 0 .. 31;
      MPCBB1_VCTR37  at 16#194# range 0 .. 31;
      MPCBB1_VCTR38  at 16#198# range 0 .. 31;
      MPCBB1_VCTR39  at 16#19C# range 0 .. 31;
      MPCBB1_VCTR40  at 16#1A0# range 0 .. 31;
      MPCBB1_VCTR41  at 16#1A4# range 0 .. 31;
      MPCBB1_VCTR42  at 16#1A8# range 0 .. 31;
      MPCBB1_VCTR43  at 16#1AC# range 0 .. 31;
      MPCBB1_VCTR44  at 16#1B0# range 0 .. 31;
      MPCBB1_VCTR45  at 16#1B4# range 0 .. 31;
      MPCBB1_VCTR46  at 16#1B8# range 0 .. 31;
      MPCBB1_VCTR47  at 16#1BC# range 0 .. 31;
      MPCBB1_VCTR48  at 16#1C0# range 0 .. 31;
      MPCBB1_VCTR49  at 16#1C4# range 0 .. 31;
      MPCBB1_VCTR50  at 16#1C8# range 0 .. 31;
      MPCBB1_VCTR51  at 16#1CC# range 0 .. 31;
      MPCBB1_VCTR52  at 16#1D0# range 0 .. 31;
      MPCBB1_VCTR53  at 16#1D4# range 0 .. 31;
      MPCBB1_VCTR54  at 16#1D8# range 0 .. 31;
      MPCBB1_VCTR55  at 16#1DC# range 0 .. 31;
      MPCBB1_VCTR56  at 16#1E0# range 0 .. 31;
      MPCBB1_VCTR57  at 16#1E4# range 0 .. 31;
      MPCBB1_VCTR58  at 16#1E8# range 0 .. 31;
      MPCBB1_VCTR59  at 16#1EC# range 0 .. 31;
      MPCBB1_VCTR60  at 16#1F0# range 0 .. 31;
      MPCBB1_VCTR61  at 16#1F4# range 0 .. 31;
      MPCBB1_VCTR62  at 16#1F8# range 0 .. 31;
      MPCBB1_VCTR63  at 16#1FC# range 0 .. 31;
   end record;

   --  SEC_GTZC_MPCBB1
   SEC_GTZC_MPCBB1_Periph : aliased SEC_GTZC_MPCBB1_Peripheral
     with Import, Address => System'To_Address (16#50032C00#);

   --  SEC_GTZC_MPCBB2
   type SEC_GTZC_MPCBB2_Peripheral is record
      --  MPCBB control register
      MPCBB2_CR      : aliased MPCBB2_CR_Register;
      --  MPCBB control register
      MPCBB2_LCKVTR1 : aliased MPCBB2_LCKVTR1_Register;
      --  MPCBB control register
      MPCBB2_LCKVTR2 : aliased MPCBB2_LCKVTR2_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR0   : aliased MPCBB2_VCTR0_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR1   : aliased MPCBB2_VCTR1_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR2   : aliased MPCBB2_VCTR2_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR3   : aliased MPCBB2_VCTR3_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR4   : aliased MPCBB2_VCTR4_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR5   : aliased MPCBB2_VCTR5_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR6   : aliased MPCBB2_VCTR6_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR7   : aliased MPCBB2_VCTR7_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR8   : aliased MPCBB2_VCTR8_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR9   : aliased MPCBB2_VCTR9_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR10  : aliased MPCBB2_VCTR10_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR11  : aliased MPCBB2_VCTR11_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR12  : aliased MPCBB2_VCTR12_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR13  : aliased MPCBB2_VCTR13_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR14  : aliased MPCBB2_VCTR14_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR15  : aliased MPCBB2_VCTR15_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR16  : aliased MPCBB2_VCTR16_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR17  : aliased MPCBB2_VCTR17_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR18  : aliased MPCBB2_VCTR18_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR19  : aliased MPCBB2_VCTR19_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR20  : aliased MPCBB2_VCTR20_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR21  : aliased MPCBB2_VCTR21_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR22  : aliased MPCBB2_VCTR22_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR23  : aliased MPCBB2_VCTR23_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR24  : aliased MPCBB2_VCTR24_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR25  : aliased MPCBB2_VCTR25_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR26  : aliased MPCBB2_VCTR26_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR27  : aliased MPCBB2_VCTR27_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR28  : aliased MPCBB2_VCTR28_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR29  : aliased MPCBB2_VCTR29_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR30  : aliased MPCBB2_VCTR30_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR31  : aliased MPCBB2_VCTR31_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR32  : aliased MPCBB2_VCTR32_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR33  : aliased MPCBB2_VCTR33_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR34  : aliased MPCBB2_VCTR34_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR35  : aliased MPCBB2_VCTR35_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR36  : aliased MPCBB2_VCTR36_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR37  : aliased MPCBB2_VCTR37_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR38  : aliased MPCBB2_VCTR38_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR39  : aliased MPCBB2_VCTR39_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR40  : aliased MPCBB2_VCTR40_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR41  : aliased MPCBB2_VCTR41_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR42  : aliased MPCBB2_VCTR42_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR43  : aliased MPCBB2_VCTR43_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR44  : aliased MPCBB2_VCTR44_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR45  : aliased MPCBB2_VCTR45_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR46  : aliased MPCBB2_VCTR46_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR47  : aliased MPCBB2_VCTR47_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR48  : aliased MPCBB2_VCTR48_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR49  : aliased MPCBB2_VCTR49_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR50  : aliased MPCBB2_VCTR50_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR51  : aliased MPCBB2_VCTR51_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR52  : aliased MPCBB2_VCTR52_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR53  : aliased MPCBB2_VCTR53_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR54  : aliased MPCBB2_VCTR54_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR55  : aliased MPCBB2_VCTR55_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR56  : aliased MPCBB2_VCTR56_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR57  : aliased MPCBB2_VCTR57_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR58  : aliased MPCBB2_VCTR58_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR59  : aliased MPCBB2_VCTR59_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR60  : aliased MPCBB2_VCTR60_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR61  : aliased MPCBB2_VCTR61_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR62  : aliased MPCBB2_VCTR62_Register;
      --  MPCBBx vector register
      MPCBB2_VCTR63  : aliased MPCBB2_VCTR63_Register;
   end record
     with Volatile;

   for SEC_GTZC_MPCBB2_Peripheral use record
      MPCBB2_CR      at 16#0# range 0 .. 31;
      MPCBB2_LCKVTR1 at 16#10# range 0 .. 31;
      MPCBB2_LCKVTR2 at 16#14# range 0 .. 31;
      MPCBB2_VCTR0   at 16#100# range 0 .. 31;
      MPCBB2_VCTR1   at 16#104# range 0 .. 31;
      MPCBB2_VCTR2   at 16#108# range 0 .. 31;
      MPCBB2_VCTR3   at 16#10C# range 0 .. 31;
      MPCBB2_VCTR4   at 16#110# range 0 .. 31;
      MPCBB2_VCTR5   at 16#114# range 0 .. 31;
      MPCBB2_VCTR6   at 16#118# range 0 .. 31;
      MPCBB2_VCTR7   at 16#11C# range 0 .. 31;
      MPCBB2_VCTR8   at 16#120# range 0 .. 31;
      MPCBB2_VCTR9   at 16#124# range 0 .. 31;
      MPCBB2_VCTR10  at 16#128# range 0 .. 31;
      MPCBB2_VCTR11  at 16#12C# range 0 .. 31;
      MPCBB2_VCTR12  at 16#130# range 0 .. 31;
      MPCBB2_VCTR13  at 16#134# range 0 .. 31;
      MPCBB2_VCTR14  at 16#138# range 0 .. 31;
      MPCBB2_VCTR15  at 16#13C# range 0 .. 31;
      MPCBB2_VCTR16  at 16#140# range 0 .. 31;
      MPCBB2_VCTR17  at 16#144# range 0 .. 31;
      MPCBB2_VCTR18  at 16#148# range 0 .. 31;
      MPCBB2_VCTR19  at 16#14C# range 0 .. 31;
      MPCBB2_VCTR20  at 16#150# range 0 .. 31;
      MPCBB2_VCTR21  at 16#154# range 0 .. 31;
      MPCBB2_VCTR22  at 16#158# range 0 .. 31;
      MPCBB2_VCTR23  at 16#15C# range 0 .. 31;
      MPCBB2_VCTR24  at 16#160# range 0 .. 31;
      MPCBB2_VCTR25  at 16#164# range 0 .. 31;
      MPCBB2_VCTR26  at 16#168# range 0 .. 31;
      MPCBB2_VCTR27  at 16#16C# range 0 .. 31;
      MPCBB2_VCTR28  at 16#170# range 0 .. 31;
      MPCBB2_VCTR29  at 16#174# range 0 .. 31;
      MPCBB2_VCTR30  at 16#178# range 0 .. 31;
      MPCBB2_VCTR31  at 16#17C# range 0 .. 31;
      MPCBB2_VCTR32  at 16#180# range 0 .. 31;
      MPCBB2_VCTR33  at 16#184# range 0 .. 31;
      MPCBB2_VCTR34  at 16#188# range 0 .. 31;
      MPCBB2_VCTR35  at 16#18C# range 0 .. 31;
      MPCBB2_VCTR36  at 16#190# range 0 .. 31;
      MPCBB2_VCTR37  at 16#194# range 0 .. 31;
      MPCBB2_VCTR38  at 16#198# range 0 .. 31;
      MPCBB2_VCTR39  at 16#19C# range 0 .. 31;
      MPCBB2_VCTR40  at 16#1A0# range 0 .. 31;
      MPCBB2_VCTR41  at 16#1A4# range 0 .. 31;
      MPCBB2_VCTR42  at 16#1A8# range 0 .. 31;
      MPCBB2_VCTR43  at 16#1AC# range 0 .. 31;
      MPCBB2_VCTR44  at 16#1B0# range 0 .. 31;
      MPCBB2_VCTR45  at 16#1B4# range 0 .. 31;
      MPCBB2_VCTR46  at 16#1B8# range 0 .. 31;
      MPCBB2_VCTR47  at 16#1BC# range 0 .. 31;
      MPCBB2_VCTR48  at 16#1C0# range 0 .. 31;
      MPCBB2_VCTR49  at 16#1C4# range 0 .. 31;
      MPCBB2_VCTR50  at 16#1C8# range 0 .. 31;
      MPCBB2_VCTR51  at 16#1CC# range 0 .. 31;
      MPCBB2_VCTR52  at 16#1D0# range 0 .. 31;
      MPCBB2_VCTR53  at 16#1D4# range 0 .. 31;
      MPCBB2_VCTR54  at 16#1D8# range 0 .. 31;
      MPCBB2_VCTR55  at 16#1DC# range 0 .. 31;
      MPCBB2_VCTR56  at 16#1E0# range 0 .. 31;
      MPCBB2_VCTR57  at 16#1E4# range 0 .. 31;
      MPCBB2_VCTR58  at 16#1E8# range 0 .. 31;
      MPCBB2_VCTR59  at 16#1EC# range 0 .. 31;
      MPCBB2_VCTR60  at 16#1F0# range 0 .. 31;
      MPCBB2_VCTR61  at 16#1F4# range 0 .. 31;
      MPCBB2_VCTR62  at 16#1F8# range 0 .. 31;
      MPCBB2_VCTR63  at 16#1FC# range 0 .. 31;
   end record;

   --  SEC_GTZC_MPCBB2
   SEC_GTZC_MPCBB2_Periph : aliased SEC_GTZC_MPCBB2_Peripheral
     with Import, Address => System'To_Address (16#50033000#);

end STM32_SVD.SEC_GTZC;
