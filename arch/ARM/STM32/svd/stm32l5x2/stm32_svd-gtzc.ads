--  This spec has been automatically generated from STM32L5x2.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.GTZC is
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

   --  TZIC interrupt enable register 1
   type IER1_Register is record
      --  TIM2IE
      TIM2IE    : Boolean := False;
      --  TIM3IE
      TIM3IE    : Boolean := False;
      --  TIM4IE
      TIM4IE    : Boolean := False;
      --  TIM5IE
      TIM5IE    : Boolean := False;
      --  TIM6IE
      TIM6IE    : Boolean := False;
      --  TIM7IE
      TIM7IE    : Boolean := False;
      --  WWDGIE
      WWDGIE    : Boolean := False;
      --  IWDGIE
      IWDGIE    : Boolean := False;
      --  SPI2IE
      SPI2IE    : Boolean := False;
      --  SPI3IE
      SPI3IE    : Boolean := False;
      --  USART2IE
      USART2IE  : Boolean := False;
      --  USART3IE
      USART3IE  : Boolean := False;
      --  UART4IE
      UART4IE   : Boolean := False;
      --  UART5IE
      UART5IE   : Boolean := False;
      --  I2C1IE
      I2C1IE    : Boolean := False;
      --  I2C2IE
      I2C2IE    : Boolean := False;
      --  I2C3IE
      I2C3IE    : Boolean := False;
      --  CRSIE
      CRSIE     : Boolean := False;
      --  DACIE
      DACIE     : Boolean := False;
      --  OPAMPIE
      OPAMPIE   : Boolean := False;
      --  LPTIM1IE
      LPTIM1IE  : Boolean := False;
      --  LPUART1IE
      LPUART1IE : Boolean := False;
      --  I2C4IE
      I2C4IE    : Boolean := False;
      --  LPTIM2IE
      LPTIM2IE  : Boolean := False;
      --  LPTIM3IE
      LPTIM3IE  : Boolean := False;
      --  FDCAN1IE
      FDCAN1IE  : Boolean := False;
      --  USBFSIE
      USBFSIE   : Boolean := False;
      --  UCPD1IE
      UCPD1IE   : Boolean := False;
      --  VREFBUFIE
      VREFBUFIE : Boolean := False;
      --  COMPIE
      COMPIE    : Boolean := False;
      --  TIM1IE
      TIM1IE    : Boolean := False;
      --  SPI1IE
      SPI1IE    : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IER1_Register use record
      TIM2IE    at 0 range 0 .. 0;
      TIM3IE    at 0 range 1 .. 1;
      TIM4IE    at 0 range 2 .. 2;
      TIM5IE    at 0 range 3 .. 3;
      TIM6IE    at 0 range 4 .. 4;
      TIM7IE    at 0 range 5 .. 5;
      WWDGIE    at 0 range 6 .. 6;
      IWDGIE    at 0 range 7 .. 7;
      SPI2IE    at 0 range 8 .. 8;
      SPI3IE    at 0 range 9 .. 9;
      USART2IE  at 0 range 10 .. 10;
      USART3IE  at 0 range 11 .. 11;
      UART4IE   at 0 range 12 .. 12;
      UART5IE   at 0 range 13 .. 13;
      I2C1IE    at 0 range 14 .. 14;
      I2C2IE    at 0 range 15 .. 15;
      I2C3IE    at 0 range 16 .. 16;
      CRSIE     at 0 range 17 .. 17;
      DACIE     at 0 range 18 .. 18;
      OPAMPIE   at 0 range 19 .. 19;
      LPTIM1IE  at 0 range 20 .. 20;
      LPUART1IE at 0 range 21 .. 21;
      I2C4IE    at 0 range 22 .. 22;
      LPTIM2IE  at 0 range 23 .. 23;
      LPTIM3IE  at 0 range 24 .. 24;
      FDCAN1IE  at 0 range 25 .. 25;
      USBFSIE   at 0 range 26 .. 26;
      UCPD1IE   at 0 range 27 .. 27;
      VREFBUFIE at 0 range 28 .. 28;
      COMPIE    at 0 range 29 .. 29;
      TIM1IE    at 0 range 30 .. 30;
      SPI1IE    at 0 range 31 .. 31;
   end record;

   --  TZIC interrupt enable register 2
   type IER2_Register is record
      --  TIM8IE
      TIM8IE         : Boolean := False;
      --  USART1IE
      USART1IE       : Boolean := False;
      --  TIM15IE
      TIM15IE        : Boolean := False;
      --  TIM16IE
      TIM16IE        : Boolean := False;
      --  TIM17IE
      TIM17IE        : Boolean := False;
      --  SAI1IE
      SAI1IE         : Boolean := False;
      --  SAI2IE
      SAI2IE         : Boolean := False;
      --  DFSDM1IE
      DFSDM1IE       : Boolean := False;
      --  CRCIE
      CRCIE          : Boolean := False;
      --  TSCIE
      TSCIE          : Boolean := False;
      --  ICACHEIE
      ICACHEIE       : Boolean := False;
      --  ADCIE
      ADCIE          : Boolean := False;
      --  AESIE
      AESIE          : Boolean := False;
      --  HASHIE
      HASHIE         : Boolean := False;
      --  RNGIE
      RNGIE          : Boolean := False;
      --  PKAIE
      PKAIE          : Boolean := False;
      --  SDMMC1IE
      SDMMC1IE       : Boolean := False;
      --  FMC_REGIE
      FMC_REGIE      : Boolean := False;
      --  OCTOSPI1_REGIE
      OCTOSPI1_REGIE : Boolean := False;
      --  RTCIE
      RTCIE          : Boolean := False;
      --  PWRIE
      PWRIE          : Boolean := False;
      --  SYSCFGIE
      SYSCFGIE       : Boolean := False;
      --  DMA1IE
      DMA1IE         : Boolean := False;
      --  DMA2IE
      DMA2IE         : Boolean := False;
      --  DMAMUX1IE
      DMAMUX1IE      : Boolean := False;
      --  RCCIE
      RCCIE          : Boolean := False;
      --  FLASHIE
      FLASHIE        : Boolean := False;
      --  FLASH_REGIE
      FLASH_REGIE    : Boolean := False;
      --  EXTIIE
      EXTIIE         : Boolean := False;
      --  OTFDEC1IE
      OTFDEC1IE      : Boolean := False;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IER2_Register use record
      TIM8IE         at 0 range 0 .. 0;
      USART1IE       at 0 range 1 .. 1;
      TIM15IE        at 0 range 2 .. 2;
      TIM16IE        at 0 range 3 .. 3;
      TIM17IE        at 0 range 4 .. 4;
      SAI1IE         at 0 range 5 .. 5;
      SAI2IE         at 0 range 6 .. 6;
      DFSDM1IE       at 0 range 7 .. 7;
      CRCIE          at 0 range 8 .. 8;
      TSCIE          at 0 range 9 .. 9;
      ICACHEIE       at 0 range 10 .. 10;
      ADCIE          at 0 range 11 .. 11;
      AESIE          at 0 range 12 .. 12;
      HASHIE         at 0 range 13 .. 13;
      RNGIE          at 0 range 14 .. 14;
      PKAIE          at 0 range 15 .. 15;
      SDMMC1IE       at 0 range 16 .. 16;
      FMC_REGIE      at 0 range 17 .. 17;
      OCTOSPI1_REGIE at 0 range 18 .. 18;
      RTCIE          at 0 range 19 .. 19;
      PWRIE          at 0 range 20 .. 20;
      SYSCFGIE       at 0 range 21 .. 21;
      DMA1IE         at 0 range 22 .. 22;
      DMA2IE         at 0 range 23 .. 23;
      DMAMUX1IE      at 0 range 24 .. 24;
      RCCIE          at 0 range 25 .. 25;
      FLASHIE        at 0 range 26 .. 26;
      FLASH_REGIE    at 0 range 27 .. 27;
      EXTIIE         at 0 range 28 .. 28;
      OTFDEC1IE      at 0 range 29 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   --  TZIC interrupt enable register 3
   type IER3_Register is record
      --  TZSCIE
      TZSCIE        : Boolean := False;
      --  TZICIE
      TZICIE        : Boolean := False;
      --  MPCWM1IE
      MPCWM1IE      : Boolean := False;
      --  MPCWM2IE
      MPCWM2IE      : Boolean := False;
      --  MPCBB1IE
      MPCBB1IE      : Boolean := False;
      --  MPCBB1_REGIE
      MPCBB1_REGIE  : Boolean := False;
      --  MPCBB2IE
      MPCBB2IE      : Boolean := False;
      --  MPCBB2_REGIE
      MPCBB2_REGIE  : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IER3_Register use record
      TZSCIE        at 0 range 0 .. 0;
      TZICIE        at 0 range 1 .. 1;
      MPCWM1IE      at 0 range 2 .. 2;
      MPCWM2IE      at 0 range 3 .. 3;
      MPCBB1IE      at 0 range 4 .. 4;
      MPCBB1_REGIE  at 0 range 5 .. 5;
      MPCBB2IE      at 0 range 6 .. 6;
      MPCBB2_REGIE  at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  TZIC interrupt status register 1
   type MISR1_Register is record
      --  Read-only. TIM2F
      TIM2F    : Boolean;
      --  Read-only. TIM3F
      TIM3F    : Boolean;
      --  Read-only. TIM4F
      TIM4F    : Boolean;
      --  Read-only. TIM5F
      TIM5F    : Boolean;
      --  Read-only. TIM6F
      TIM6F    : Boolean;
      --  Read-only. TIM7F
      TIM7F    : Boolean;
      --  Read-only. WWDGF
      WWDGF    : Boolean;
      --  Read-only. IWDGF
      IWDGF    : Boolean;
      --  Read-only. SPI2F
      SPI2F    : Boolean;
      --  Read-only. SPI3F
      SPI3F    : Boolean;
      --  Read-only. USART2F
      USART2F  : Boolean;
      --  Read-only. USART3F
      USART3F  : Boolean;
      --  Read-only. UART4F
      UART4F   : Boolean;
      --  Read-only. UART5F
      UART5F   : Boolean;
      --  Read-only. I2C1F
      I2C1F    : Boolean;
      --  Read-only. I2C2F
      I2C2F    : Boolean;
      --  Read-only. I2C3F
      I2C3F    : Boolean;
      --  Read-only. CRSF
      CRSF     : Boolean;
      --  Read-only. DACF
      DACF     : Boolean;
      --  Read-only. OPAMPF
      OPAMPF   : Boolean;
      --  Read-only. LPTIM1F
      LPTIM1F  : Boolean;
      --  Read-only. LPUART1F
      LPUART1F : Boolean;
      --  Read-only. I2C4F
      I2C4F    : Boolean;
      --  Read-only. LPTIM2F
      LPTIM2F  : Boolean;
      --  Read-only. LPTIM3F
      LPTIM3F  : Boolean;
      --  Read-only. FDCAN1F
      FDCAN1F  : Boolean;
      --  Read-only. USBFSF
      USBFSF   : Boolean;
      --  Read-only. UCPD1F
      UCPD1F   : Boolean;
      --  Read-only. VREFBUFF
      VREFBUFF : Boolean;
      --  Read-only. COMPF
      COMPF    : Boolean;
      --  Read-only. TIM1F
      TIM1F    : Boolean;
      --  Read-only. SPI1F
      SPI1F    : Boolean;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MISR1_Register use record
      TIM2F    at 0 range 0 .. 0;
      TIM3F    at 0 range 1 .. 1;
      TIM4F    at 0 range 2 .. 2;
      TIM5F    at 0 range 3 .. 3;
      TIM6F    at 0 range 4 .. 4;
      TIM7F    at 0 range 5 .. 5;
      WWDGF    at 0 range 6 .. 6;
      IWDGF    at 0 range 7 .. 7;
      SPI2F    at 0 range 8 .. 8;
      SPI3F    at 0 range 9 .. 9;
      USART2F  at 0 range 10 .. 10;
      USART3F  at 0 range 11 .. 11;
      UART4F   at 0 range 12 .. 12;
      UART5F   at 0 range 13 .. 13;
      I2C1F    at 0 range 14 .. 14;
      I2C2F    at 0 range 15 .. 15;
      I2C3F    at 0 range 16 .. 16;
      CRSF     at 0 range 17 .. 17;
      DACF     at 0 range 18 .. 18;
      OPAMPF   at 0 range 19 .. 19;
      LPTIM1F  at 0 range 20 .. 20;
      LPUART1F at 0 range 21 .. 21;
      I2C4F    at 0 range 22 .. 22;
      LPTIM2F  at 0 range 23 .. 23;
      LPTIM3F  at 0 range 24 .. 24;
      FDCAN1F  at 0 range 25 .. 25;
      USBFSF   at 0 range 26 .. 26;
      UCPD1F   at 0 range 27 .. 27;
      VREFBUFF at 0 range 28 .. 28;
      COMPF    at 0 range 29 .. 29;
      TIM1F    at 0 range 30 .. 30;
      SPI1F    at 0 range 31 .. 31;
   end record;

   --  TZIC interrupt status register 2
   type MISR2_Register is record
      --  TIM8F
      TIM8F          : Boolean := False;
      --  USART1F
      USART1F        : Boolean := False;
      --  TIM15F
      TIM15F         : Boolean := False;
      --  TIM16F
      TIM16F         : Boolean := False;
      --  TIM17F
      TIM17F         : Boolean := False;
      --  SAI1F
      SAI1F          : Boolean := False;
      --  SAI2F
      SAI2F          : Boolean := False;
      --  DFSDM1F
      DFSDM1F        : Boolean := False;
      --  CRCF
      CRCF           : Boolean := False;
      --  TSCF
      TSCF           : Boolean := False;
      --  ICACHEF
      ICACHEF        : Boolean := False;
      --  ADCF
      ADCF           : Boolean := False;
      --  AESF
      AESF           : Boolean := False;
      --  HASHF
      HASHF          : Boolean := False;
      --  RNGF
      RNGF           : Boolean := False;
      --  PKAF
      PKAF           : Boolean := False;
      --  SDMMC1F
      SDMMC1F        : Boolean := False;
      --  FMC_REGF
      FMC_REGF       : Boolean := False;
      --  OCTOSPI1_REGF
      OCTOSPI1_REGF  : Boolean := False;
      --  RTCF
      RTCF           : Boolean := False;
      --  PWRF
      PWRF           : Boolean := False;
      --  SYSCFGF
      SYSCFGF        : Boolean := False;
      --  DMA1F
      DMA1F          : Boolean := False;
      --  DMA2F
      DMA2F          : Boolean := False;
      --  DMAMUX1F
      DMAMUX1F       : Boolean := False;
      --  RCCF
      RCCF           : Boolean := False;
      --  FLASHF
      FLASHF         : Boolean := False;
      --  FLASH_REGF
      FLASH_REGF     : Boolean := False;
      --  EXTIF
      EXTIF          : Boolean := False;
      --  OTFDEC1F
      OTFDEC1F       : Boolean := False;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MISR2_Register use record
      TIM8F          at 0 range 0 .. 0;
      USART1F        at 0 range 1 .. 1;
      TIM15F         at 0 range 2 .. 2;
      TIM16F         at 0 range 3 .. 3;
      TIM17F         at 0 range 4 .. 4;
      SAI1F          at 0 range 5 .. 5;
      SAI2F          at 0 range 6 .. 6;
      DFSDM1F        at 0 range 7 .. 7;
      CRCF           at 0 range 8 .. 8;
      TSCF           at 0 range 9 .. 9;
      ICACHEF        at 0 range 10 .. 10;
      ADCF           at 0 range 11 .. 11;
      AESF           at 0 range 12 .. 12;
      HASHF          at 0 range 13 .. 13;
      RNGF           at 0 range 14 .. 14;
      PKAF           at 0 range 15 .. 15;
      SDMMC1F        at 0 range 16 .. 16;
      FMC_REGF       at 0 range 17 .. 17;
      OCTOSPI1_REGF  at 0 range 18 .. 18;
      RTCF           at 0 range 19 .. 19;
      PWRF           at 0 range 20 .. 20;
      SYSCFGF        at 0 range 21 .. 21;
      DMA1F          at 0 range 22 .. 22;
      DMA2F          at 0 range 23 .. 23;
      DMAMUX1F       at 0 range 24 .. 24;
      RCCF           at 0 range 25 .. 25;
      FLASHF         at 0 range 26 .. 26;
      FLASH_REGF     at 0 range 27 .. 27;
      EXTIF          at 0 range 28 .. 28;
      OTFDEC1F       at 0 range 29 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   --  TZIC interrupt status register 3
   type MISR3_Register is record
      --  TZSCF
      TZSCF         : Boolean := False;
      --  TZICF
      TZICF         : Boolean := False;
      --  MPCWM1F
      MPCWM1F       : Boolean := False;
      --  MPCWM2F
      MPCWM2F       : Boolean := False;
      --  MPCBB1F
      MPCBB1F       : Boolean := False;
      --  MPCBB1_REGF
      MPCBB1_REGF   : Boolean := False;
      --  MPCBB2F
      MPCBB2F       : Boolean := False;
      --  MPCBB2_REGF
      MPCBB2_REGF   : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MISR3_Register use record
      TZSCF         at 0 range 0 .. 0;
      TZICF         at 0 range 1 .. 1;
      MPCWM1F       at 0 range 2 .. 2;
      MPCWM2F       at 0 range 3 .. 3;
      MPCBB1F       at 0 range 4 .. 4;
      MPCBB1_REGF   at 0 range 5 .. 5;
      MPCBB2F       at 0 range 6 .. 6;
      MPCBB2_REGF   at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  TZIC interrupt clear register 1
   type ICR1_Register is record
      --  Write-only. TIM2FC
      TIM2FC    : Boolean := False;
      --  Write-only. TIM3FC
      TIM3FC    : Boolean := False;
      --  Write-only. TIM4FC
      TIM4FC    : Boolean := False;
      --  Write-only. TIM5FC
      TIM5FC    : Boolean := False;
      --  Write-only. TIM6FC
      TIM6FC    : Boolean := False;
      --  Write-only. TIM7FC
      TIM7FC    : Boolean := False;
      --  Write-only. WWDGFC
      WWDGFC    : Boolean := False;
      --  Write-only. IWDGFC
      IWDGFC    : Boolean := False;
      --  Write-only. SPI2FC
      SPI2FC    : Boolean := False;
      --  Write-only. SPI3FC
      SPI3FC    : Boolean := False;
      --  Write-only. USART2FC
      USART2FC  : Boolean := False;
      --  Write-only. USART3FC
      USART3FC  : Boolean := False;
      --  Write-only. UART4FC
      UART4FC   : Boolean := False;
      --  Write-only. UART5FC
      UART5FC   : Boolean := False;
      --  Write-only. I2C1FC
      I2C1FC    : Boolean := False;
      --  Write-only. I2C2FC
      I2C2FC    : Boolean := False;
      --  Write-only. I2C3FC
      I2C3FC    : Boolean := False;
      --  Write-only. CRSFC
      CRSFC     : Boolean := False;
      --  Write-only. DACFC
      DACFC     : Boolean := False;
      --  Write-only. OPAMPFC
      OPAMPFC   : Boolean := False;
      --  Write-only. LPTIM1FC
      LPTIM1FC  : Boolean := False;
      --  Write-only. LPUART1FC
      LPUART1FC : Boolean := False;
      --  Write-only. I2C4FC
      I2C4FC    : Boolean := False;
      --  Write-only. LPTIM2FC
      LPTIM2FC  : Boolean := False;
      --  Write-only. LPTIM3FC
      LPTIM3FC  : Boolean := False;
      --  Write-only. FDCAN1FC
      FDCAN1FC  : Boolean := False;
      --  Write-only. USBFSFC
      USBFSFC   : Boolean := False;
      --  Write-only. UCPD1FC
      UCPD1FC   : Boolean := False;
      --  Write-only. VREFBUFFC
      VREFBUFFC : Boolean := False;
      --  Write-only. COMPFC
      COMPFC    : Boolean := False;
      --  Write-only. TIM1FC
      TIM1FC    : Boolean := False;
      --  Write-only. SPI1FC
      SPI1FC    : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ICR1_Register use record
      TIM2FC    at 0 range 0 .. 0;
      TIM3FC    at 0 range 1 .. 1;
      TIM4FC    at 0 range 2 .. 2;
      TIM5FC    at 0 range 3 .. 3;
      TIM6FC    at 0 range 4 .. 4;
      TIM7FC    at 0 range 5 .. 5;
      WWDGFC    at 0 range 6 .. 6;
      IWDGFC    at 0 range 7 .. 7;
      SPI2FC    at 0 range 8 .. 8;
      SPI3FC    at 0 range 9 .. 9;
      USART2FC  at 0 range 10 .. 10;
      USART3FC  at 0 range 11 .. 11;
      UART4FC   at 0 range 12 .. 12;
      UART5FC   at 0 range 13 .. 13;
      I2C1FC    at 0 range 14 .. 14;
      I2C2FC    at 0 range 15 .. 15;
      I2C3FC    at 0 range 16 .. 16;
      CRSFC     at 0 range 17 .. 17;
      DACFC     at 0 range 18 .. 18;
      OPAMPFC   at 0 range 19 .. 19;
      LPTIM1FC  at 0 range 20 .. 20;
      LPUART1FC at 0 range 21 .. 21;
      I2C4FC    at 0 range 22 .. 22;
      LPTIM2FC  at 0 range 23 .. 23;
      LPTIM3FC  at 0 range 24 .. 24;
      FDCAN1FC  at 0 range 25 .. 25;
      USBFSFC   at 0 range 26 .. 26;
      UCPD1FC   at 0 range 27 .. 27;
      VREFBUFFC at 0 range 28 .. 28;
      COMPFC    at 0 range 29 .. 29;
      TIM1FC    at 0 range 30 .. 30;
      SPI1FC    at 0 range 31 .. 31;
   end record;

   --  TZIC interrupt clear register 2
   type ICR2_Register is record
      --  TIM8FC
      TIM8FC         : Boolean := False;
      --  USART1FC
      USART1FC       : Boolean := False;
      --  TIM15FC
      TIM15FC        : Boolean := False;
      --  TIM16FC
      TIM16FC        : Boolean := False;
      --  TIM17FC
      TIM17FC        : Boolean := False;
      --  SAI1FC
      SAI1FC         : Boolean := False;
      --  SAI2FC
      SAI2FC         : Boolean := False;
      --  DFSDM1FC
      DFSDM1FC       : Boolean := False;
      --  CRCFC
      CRCFC          : Boolean := False;
      --  TSCFC
      TSCFC          : Boolean := False;
      --  ICACHEFC
      ICACHEFC       : Boolean := False;
      --  ADCFC
      ADCFC          : Boolean := False;
      --  AESFC
      AESFC          : Boolean := False;
      --  HASHFC
      HASHFC         : Boolean := False;
      --  RNGFC
      RNGFC          : Boolean := False;
      --  PKAFC
      PKAFC          : Boolean := False;
      --  SDMMC1FC
      SDMMC1FC       : Boolean := False;
      --  FMC_REGFC
      FMC_REGFC      : Boolean := False;
      --  OCTOSPI1_REGFC
      OCTOSPI1_REGFC : Boolean := False;
      --  RTCFC
      RTCFC          : Boolean := False;
      --  PWRFC
      PWRFC          : Boolean := False;
      --  SYSCFGFC
      SYSCFGFC       : Boolean := False;
      --  DMA1FC
      DMA1FC         : Boolean := False;
      --  DMA2FC
      DMA2FC         : Boolean := False;
      --  DMAMUX1FC
      DMAMUX1FC      : Boolean := False;
      --  RCCFC
      RCCFC          : Boolean := False;
      --  FLASHFC
      FLASHFC        : Boolean := False;
      --  FLASH_REGFC
      FLASH_REGFC    : Boolean := False;
      --  EXTIFC
      EXTIFC         : Boolean := False;
      --  OTFDEC1FC
      OTFDEC1FC      : Boolean := False;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ICR2_Register use record
      TIM8FC         at 0 range 0 .. 0;
      USART1FC       at 0 range 1 .. 1;
      TIM15FC        at 0 range 2 .. 2;
      TIM16FC        at 0 range 3 .. 3;
      TIM17FC        at 0 range 4 .. 4;
      SAI1FC         at 0 range 5 .. 5;
      SAI2FC         at 0 range 6 .. 6;
      DFSDM1FC       at 0 range 7 .. 7;
      CRCFC          at 0 range 8 .. 8;
      TSCFC          at 0 range 9 .. 9;
      ICACHEFC       at 0 range 10 .. 10;
      ADCFC          at 0 range 11 .. 11;
      AESFC          at 0 range 12 .. 12;
      HASHFC         at 0 range 13 .. 13;
      RNGFC          at 0 range 14 .. 14;
      PKAFC          at 0 range 15 .. 15;
      SDMMC1FC       at 0 range 16 .. 16;
      FMC_REGFC      at 0 range 17 .. 17;
      OCTOSPI1_REGFC at 0 range 18 .. 18;
      RTCFC          at 0 range 19 .. 19;
      PWRFC          at 0 range 20 .. 20;
      SYSCFGFC       at 0 range 21 .. 21;
      DMA1FC         at 0 range 22 .. 22;
      DMA2FC         at 0 range 23 .. 23;
      DMAMUX1FC      at 0 range 24 .. 24;
      RCCFC          at 0 range 25 .. 25;
      FLASHFC        at 0 range 26 .. 26;
      FLASH_REGFC    at 0 range 27 .. 27;
      EXTIFC         at 0 range 28 .. 28;
      OTFDEC1FC      at 0 range 29 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   --  TZIC interrupt clear register 3
   type ICR3_Register is record
      --  TZSCFC
      TZSCFC        : Boolean := False;
      --  TZICFC
      TZICFC        : Boolean := False;
      --  MPCWM1FC
      MPCWM1FC      : Boolean := False;
      --  MPCWM2FC
      MPCWM2FC      : Boolean := False;
      --  MPCBB1FC
      MPCBB1FC      : Boolean := False;
      --  MPCBB1_REGFC
      MPCBB1_REGFC  : Boolean := False;
      --  MPCBB2FC
      MPCBB2FC      : Boolean := False;
      --  MPCBB2_REGFC
      MPCBB2_REGFC  : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ICR3_Register use record
      TZSCFC        at 0 range 0 .. 0;
      TZICFC        at 0 range 1 .. 1;
      MPCWM1FC      at 0 range 2 .. 2;
      MPCWM2FC      at 0 range 3 .. 3;
      MPCBB1FC      at 0 range 4 .. 4;
      MPCBB1_REGFC  at 0 range 5 .. 5;
      MPCBB2FC      at 0 range 6 .. 6;
      MPCBB2_REGFC  at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  TZSC control register
   type TZSC_CR_Register is record
      --  LCK
      LCK           : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_CR_Register use record
      LCK           at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  TZSC secure configuration register 1
   type TZSC_SECCFGR1_Register is record
      --  TIM2SEC
      TIM2SEC    : Boolean := False;
      --  TIM3SEC
      TIM3SEC    : Boolean := False;
      --  TIM4SEC
      TIM4SEC    : Boolean := False;
      --  TIM5SEC
      TIM5SEC    : Boolean := False;
      --  TIM6SEC
      TIM6SEC    : Boolean := False;
      --  TIM7SEC
      TIM7SEC    : Boolean := False;
      --  WWDGSEC
      WWDGSEC    : Boolean := False;
      --  IWDGSEC
      IWDGSEC    : Boolean := False;
      --  SPI2SEC
      SPI2SEC    : Boolean := False;
      --  SPI3SEC
      SPI3SEC    : Boolean := False;
      --  USART2SEC
      USART2SEC  : Boolean := False;
      --  USART3SEC
      USART3SEC  : Boolean := False;
      --  UART4SEC
      UART4SEC   : Boolean := False;
      --  UART5SEC
      UART5SEC   : Boolean := False;
      --  I2C1SEC
      I2C1SEC    : Boolean := False;
      --  I2C2SEC
      I2C2SEC    : Boolean := False;
      --  I2C3SEC
      I2C3SEC    : Boolean := False;
      --  CRSSEC
      CRSSEC     : Boolean := False;
      --  DACSEC
      DACSEC     : Boolean := False;
      --  OPAMPSEC
      OPAMPSEC   : Boolean := False;
      --  LPTIM1SEC
      LPTIM1SEC  : Boolean := False;
      --  LPUART1SEC
      LPUART1SEC : Boolean := False;
      --  I2C4SEC
      I2C4SEC    : Boolean := False;
      --  LPTIM2SEC
      LPTIM2SEC  : Boolean := False;
      --  LPTIM3SEC
      LPTIM3SEC  : Boolean := False;
      --  FDCAN1SEC
      FDCAN1SEC  : Boolean := False;
      --  USBFSSEC
      USBFSSEC   : Boolean := False;
      --  UCPD1SEC
      UCPD1SEC   : Boolean := False;
      --  VREFBUFSEC
      VREFBUFSEC : Boolean := False;
      --  COMPSEC
      COMPSEC    : Boolean := False;
      --  TIM1SEC
      TIM1SEC    : Boolean := False;
      --  SPI1SEC
      SPI1SEC    : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_SECCFGR1_Register use record
      TIM2SEC    at 0 range 0 .. 0;
      TIM3SEC    at 0 range 1 .. 1;
      TIM4SEC    at 0 range 2 .. 2;
      TIM5SEC    at 0 range 3 .. 3;
      TIM6SEC    at 0 range 4 .. 4;
      TIM7SEC    at 0 range 5 .. 5;
      WWDGSEC    at 0 range 6 .. 6;
      IWDGSEC    at 0 range 7 .. 7;
      SPI2SEC    at 0 range 8 .. 8;
      SPI3SEC    at 0 range 9 .. 9;
      USART2SEC  at 0 range 10 .. 10;
      USART3SEC  at 0 range 11 .. 11;
      UART4SEC   at 0 range 12 .. 12;
      UART5SEC   at 0 range 13 .. 13;
      I2C1SEC    at 0 range 14 .. 14;
      I2C2SEC    at 0 range 15 .. 15;
      I2C3SEC    at 0 range 16 .. 16;
      CRSSEC     at 0 range 17 .. 17;
      DACSEC     at 0 range 18 .. 18;
      OPAMPSEC   at 0 range 19 .. 19;
      LPTIM1SEC  at 0 range 20 .. 20;
      LPUART1SEC at 0 range 21 .. 21;
      I2C4SEC    at 0 range 22 .. 22;
      LPTIM2SEC  at 0 range 23 .. 23;
      LPTIM3SEC  at 0 range 24 .. 24;
      FDCAN1SEC  at 0 range 25 .. 25;
      USBFSSEC   at 0 range 26 .. 26;
      UCPD1SEC   at 0 range 27 .. 27;
      VREFBUFSEC at 0 range 28 .. 28;
      COMPSEC    at 0 range 29 .. 29;
      TIM1SEC    at 0 range 30 .. 30;
      SPI1SEC    at 0 range 31 .. 31;
   end record;

   --  TZSC secure configuration register 2
   type TZSC_SECCFGR2_Register is record
      --  TIM8SEC
      TIM8SEC         : Boolean := False;
      --  USART1SEC
      USART1SEC       : Boolean := False;
      --  TIM15SEC
      TIM15SEC        : Boolean := False;
      --  TIM16SEC
      TIM16SEC        : Boolean := False;
      --  TIM17SEC
      TIM17SEC        : Boolean := False;
      --  SAI1SEC
      SAI1SEC         : Boolean := False;
      --  SAI2SEC
      SAI2SEC         : Boolean := False;
      --  DFSDM1SEC
      DFSDM1SEC       : Boolean := False;
      --  CRCSEC
      CRCSEC          : Boolean := False;
      --  TSCSEC
      TSCSEC          : Boolean := False;
      --  ICACHESEC
      ICACHESEC       : Boolean := False;
      --  ADCSEC
      ADCSEC          : Boolean := False;
      --  AESSEC
      AESSEC          : Boolean := False;
      --  HASHSEC
      HASHSEC         : Boolean := False;
      --  RNGSEC
      RNGSEC          : Boolean := False;
      --  PKASEC
      PKASEC          : Boolean := False;
      --  SDMMC1SEC
      SDMMC1SEC       : Boolean := False;
      --  FSMC_REGSEC
      FSMC_REGSEC     : Boolean := False;
      --  OCTOSPI1_REGSEC
      OCTOSPI1_REGSEC : Boolean := False;
      --  unspecified
      Reserved_19_31  : HAL.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_SECCFGR2_Register use record
      TIM8SEC         at 0 range 0 .. 0;
      USART1SEC       at 0 range 1 .. 1;
      TIM15SEC        at 0 range 2 .. 2;
      TIM16SEC        at 0 range 3 .. 3;
      TIM17SEC        at 0 range 4 .. 4;
      SAI1SEC         at 0 range 5 .. 5;
      SAI2SEC         at 0 range 6 .. 6;
      DFSDM1SEC       at 0 range 7 .. 7;
      CRCSEC          at 0 range 8 .. 8;
      TSCSEC          at 0 range 9 .. 9;
      ICACHESEC       at 0 range 10 .. 10;
      ADCSEC          at 0 range 11 .. 11;
      AESSEC          at 0 range 12 .. 12;
      HASHSEC         at 0 range 13 .. 13;
      RNGSEC          at 0 range 14 .. 14;
      PKASEC          at 0 range 15 .. 15;
      SDMMC1SEC       at 0 range 16 .. 16;
      FSMC_REGSEC     at 0 range 17 .. 17;
      OCTOSPI1_REGSEC at 0 range 18 .. 18;
      Reserved_19_31  at 0 range 19 .. 31;
   end record;

   --  TZSC privilege configuration register 1
   type TZSC_PRIVCFGR1_Register is record
      --  TIM2PRIV
      TIM2PRIV    : Boolean := False;
      --  TIM3PRIV
      TIM3PRIV    : Boolean := False;
      --  TIM4PRIV
      TIM4PRIV    : Boolean := False;
      --  TIM5PRIV
      TIM5PRIV    : Boolean := False;
      --  TIM6PRIV
      TIM6PRIV    : Boolean := False;
      --  TIM7PRIV
      TIM7PRIV    : Boolean := False;
      --  WWDGPRIV
      WWDGPRIV    : Boolean := False;
      --  IWDGPRIV
      IWDGPRIV    : Boolean := False;
      --  SPI2PRIV
      SPI2PRIV    : Boolean := False;
      --  SPI3PRIV
      SPI3PRIV    : Boolean := False;
      --  USART2PRIV
      USART2PRIV  : Boolean := False;
      --  USART3PRIV
      USART3PRIV  : Boolean := False;
      --  UART4PRIV
      UART4PRIV   : Boolean := False;
      --  UART5PRIV
      UART5PRIV   : Boolean := False;
      --  I2C1PRIV
      I2C1PRIV    : Boolean := False;
      --  I2C2PRIV
      I2C2PRIV    : Boolean := False;
      --  I2C3PRIV
      I2C3PRIV    : Boolean := False;
      --  CRSPRIV
      CRSPRIV     : Boolean := False;
      --  DACPRIV
      DACPRIV     : Boolean := False;
      --  OPAMPPRIV
      OPAMPPRIV   : Boolean := False;
      --  LPTIM1PRIV
      LPTIM1PRIV  : Boolean := False;
      --  LPUART1PRIV
      LPUART1PRIV : Boolean := False;
      --  I2C4PRIV
      I2C4PRIV    : Boolean := False;
      --  LPTIM2PRIV
      LPTIM2PRIV  : Boolean := False;
      --  LPTIM3PRIV
      LPTIM3PRIV  : Boolean := False;
      --  FDCAN1PRIV
      FDCAN1PRIV  : Boolean := False;
      --  USBFSPRIV
      USBFSPRIV   : Boolean := False;
      --  UCPD1PRIV
      UCPD1PRIV   : Boolean := False;
      --  VREFBUFPRIV
      VREFBUFPRIV : Boolean := False;
      --  COMPPRIV
      COMPPRIV    : Boolean := False;
      --  TIM1PRIV
      TIM1PRIV    : Boolean := False;
      --  SPI1PRIV
      SPI1PRIV    : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_PRIVCFGR1_Register use record
      TIM2PRIV    at 0 range 0 .. 0;
      TIM3PRIV    at 0 range 1 .. 1;
      TIM4PRIV    at 0 range 2 .. 2;
      TIM5PRIV    at 0 range 3 .. 3;
      TIM6PRIV    at 0 range 4 .. 4;
      TIM7PRIV    at 0 range 5 .. 5;
      WWDGPRIV    at 0 range 6 .. 6;
      IWDGPRIV    at 0 range 7 .. 7;
      SPI2PRIV    at 0 range 8 .. 8;
      SPI3PRIV    at 0 range 9 .. 9;
      USART2PRIV  at 0 range 10 .. 10;
      USART3PRIV  at 0 range 11 .. 11;
      UART4PRIV   at 0 range 12 .. 12;
      UART5PRIV   at 0 range 13 .. 13;
      I2C1PRIV    at 0 range 14 .. 14;
      I2C2PRIV    at 0 range 15 .. 15;
      I2C3PRIV    at 0 range 16 .. 16;
      CRSPRIV     at 0 range 17 .. 17;
      DACPRIV     at 0 range 18 .. 18;
      OPAMPPRIV   at 0 range 19 .. 19;
      LPTIM1PRIV  at 0 range 20 .. 20;
      LPUART1PRIV at 0 range 21 .. 21;
      I2C4PRIV    at 0 range 22 .. 22;
      LPTIM2PRIV  at 0 range 23 .. 23;
      LPTIM3PRIV  at 0 range 24 .. 24;
      FDCAN1PRIV  at 0 range 25 .. 25;
      USBFSPRIV   at 0 range 26 .. 26;
      UCPD1PRIV   at 0 range 27 .. 27;
      VREFBUFPRIV at 0 range 28 .. 28;
      COMPPRIV    at 0 range 29 .. 29;
      TIM1PRIV    at 0 range 30 .. 30;
      SPI1PRIV    at 0 range 31 .. 31;
   end record;

   --  TZSC privilege configuration register 2
   type TZSC_PRIVCFGR2_Register is record
      --  TIM8PRIV
      TIM8PRIV         : Boolean := False;
      --  USART1PRIV
      USART1PRIV       : Boolean := False;
      --  TIM15PRIV
      TIM15PRIV        : Boolean := False;
      --  TIM16PRIV
      TIM16PRIV        : Boolean := False;
      --  TIM17PRIV
      TIM17PRIV        : Boolean := False;
      --  SAI1PRIV
      SAI1PRIV         : Boolean := False;
      --  SAI2PRIV
      SAI2PRIV         : Boolean := False;
      --  DFSDM1PRIV
      DFSDM1PRIV       : Boolean := False;
      --  CRCPRIV
      CRCPRIV          : Boolean := False;
      --  TSCPRIV
      TSCPRIV          : Boolean := False;
      --  ICACHEPRIV
      ICACHEPRIV       : Boolean := False;
      --  ADCPRIV
      ADCPRIV          : Boolean := False;
      --  AESPRIV
      AESPRIV          : Boolean := False;
      --  HASHPRIV
      HASHPRIV         : Boolean := False;
      --  RNGPRIV
      RNGPRIV          : Boolean := False;
      --  PKAPRIV
      PKAPRIV          : Boolean := False;
      --  SDMMC1PRIV
      SDMMC1PRIV       : Boolean := False;
      --  FSMC_REGPRIV
      FSMC_REGPRIV     : Boolean := False;
      --  OCTOSPI1_REGRIV
      OCTOSPI1_REGPRIV : Boolean := False;
      --  unspecified
      Reserved_19_31   : HAL.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_PRIVCFGR2_Register use record
      TIM8PRIV         at 0 range 0 .. 0;
      USART1PRIV       at 0 range 1 .. 1;
      TIM15PRIV        at 0 range 2 .. 2;
      TIM16PRIV        at 0 range 3 .. 3;
      TIM17PRIV        at 0 range 4 .. 4;
      SAI1PRIV         at 0 range 5 .. 5;
      SAI2PRIV         at 0 range 6 .. 6;
      DFSDM1PRIV       at 0 range 7 .. 7;
      CRCPRIV          at 0 range 8 .. 8;
      TSCPRIV          at 0 range 9 .. 9;
      ICACHEPRIV       at 0 range 10 .. 10;
      ADCPRIV          at 0 range 11 .. 11;
      AESPRIV          at 0 range 12 .. 12;
      HASHPRIV         at 0 range 13 .. 13;
      RNGPRIV          at 0 range 14 .. 14;
      PKAPRIV          at 0 range 15 .. 15;
      SDMMC1PRIV       at 0 range 16 .. 16;
      FSMC_REGPRIV     at 0 range 17 .. 17;
      OCTOSPI1_REGPRIV at 0 range 18 .. 18;
      Reserved_19_31   at 0 range 19 .. 31;
   end record;

   subtype TZSC_MPCWM1_NSWMR1_NSWM1STRT_Field is HAL.UInt11;
   subtype TZSC_MPCWM1_NSWMR1_NSWM1LGTH_Field is HAL.UInt12;

   --  TZSC external memory non-secure watermark register 1
   type TZSC_MPCWM1_NSWMR1_Register is record
      --  NSWM1STRT
      NSWM1STRT      : TZSC_MPCWM1_NSWMR1_NSWM1STRT_Field := 16#0#;
      --  unspecified
      Reserved_11_15 : HAL.UInt5 := 16#0#;
      --  NSWM1LGTH
      NSWM1LGTH      : TZSC_MPCWM1_NSWMR1_NSWM1LGTH_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_MPCWM1_NSWMR1_Register use record
      NSWM1STRT      at 0 range 0 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
      NSWM1LGTH      at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype TZSC_MPCWM1_NSWMR2_NSWM2STRT_Field is HAL.UInt11;
   subtype TZSC_MPCWM1_NSWMR2_NSWM2LGTH_Field is HAL.UInt12;

   --  TZSC external memory non-secure watermark register 1
   type TZSC_MPCWM1_NSWMR2_Register is record
      --  NSWM2STRT
      NSWM2STRT      : TZSC_MPCWM1_NSWMR2_NSWM2STRT_Field := 16#0#;
      --  unspecified
      Reserved_11_15 : HAL.UInt5 := 16#0#;
      --  NSWM2LGTH
      NSWM2LGTH      : TZSC_MPCWM1_NSWMR2_NSWM2LGTH_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_MPCWM1_NSWMR2_Register use record
      NSWM2STRT      at 0 range 0 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
      NSWM2LGTH      at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype TZSC_MPCWM2_NSWMR1_NSWM1STRT_Field is HAL.UInt11;
   subtype TZSC_MPCWM2_NSWMR1_NSWM1LGTH_Field is HAL.UInt12;

   --  TZSC external memory non-secure watermark register 1
   type TZSC_MPCWM2_NSWMR1_Register is record
      --  NSWM1STRT
      NSWM1STRT      : TZSC_MPCWM2_NSWMR1_NSWM1STRT_Field := 16#0#;
      --  unspecified
      Reserved_11_15 : HAL.UInt5 := 16#0#;
      --  NSWM1LGTH
      NSWM1LGTH      : TZSC_MPCWM2_NSWMR1_NSWM1LGTH_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_MPCWM2_NSWMR1_Register use record
      NSWM1STRT      at 0 range 0 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
      NSWM1LGTH      at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype TZSC_MPCWM2_NSWMR2_NSWM2STRT_Field is HAL.UInt11;
   subtype TZSC_MPCWM2_NSWMR2_NSWM2LGTH_Field is HAL.UInt12;

   --  TZSC external memory non-secure watermark register 2
   type TZSC_MPCWM2_NSWMR2_Register is record
      --  NSWM2STRT
      NSWM2STRT      : TZSC_MPCWM2_NSWMR2_NSWM2STRT_Field := 16#0#;
      --  unspecified
      Reserved_11_15 : HAL.UInt5 := 16#0#;
      --  NSWM2LGTH
      NSWM2LGTH      : TZSC_MPCWM2_NSWMR2_NSWM2LGTH_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_MPCWM2_NSWMR2_Register use record
      NSWM2STRT      at 0 range 0 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
      NSWM2LGTH      at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype TZSC_MPCWM3_NSWMR1_NSWM2STRT_Field is HAL.UInt11;
   subtype TZSC_MPCWM3_NSWMR1_NSWM2LGTH_Field is HAL.UInt12;

   --  TZSC external memory non-secure watermark register 2
   type TZSC_MPCWM3_NSWMR1_Register is record
      --  NSWM2STRT
      NSWM2STRT      : TZSC_MPCWM3_NSWMR1_NSWM2STRT_Field := 16#0#;
      --  unspecified
      Reserved_11_15 : HAL.UInt5 := 16#0#;
      --  NSWM2LGTH
      NSWM2LGTH      : TZSC_MPCWM3_NSWMR1_NSWM2LGTH_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TZSC_MPCWM3_NSWMR1_Register use record
      NSWM2STRT      at 0 range 0 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
      NSWM2LGTH      at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  MPCBB1
   type MPCBB1_Peripheral is record
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

   for MPCBB1_Peripheral use record
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

   --  MPCBB1
   MPCBB1_Periph : aliased MPCBB1_Peripheral
     with Import, Address => System'To_Address (16#40032C00#);

   --  MPCBB1
   SEC_MPCBB1_Periph : aliased MPCBB1_Peripheral
     with Import, Address => System'To_Address (16#50032C00#);

   --  MPCBB2
   type MPCBB2_Peripheral is record
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

   for MPCBB2_Peripheral use record
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

   --  MPCBB2
   MPCBB2_Periph : aliased MPCBB2_Peripheral
     with Import, Address => System'To_Address (16#40033000#);

   --  MPCBB2
   SEC_MPCBB2_Periph : aliased MPCBB2_Peripheral
     with Import, Address => System'To_Address (16#50033000#);

   --  TZIC
   type SEC_TZIC_Peripheral is record
      --  TZIC interrupt enable register 1
      IER1  : aliased IER1_Register;
      --  TZIC interrupt enable register 2
      IER2  : aliased IER2_Register;
      --  TZIC interrupt enable register 3
      IER3  : aliased IER3_Register;
      --  TZIC interrupt status register 1
      MISR1 : aliased MISR1_Register;
      --  TZIC interrupt status register 2
      MISR2 : aliased MISR2_Register;
      --  TZIC interrupt status register 3
      MISR3 : aliased MISR3_Register;
      --  TZIC interrupt clear register 1
      ICR1  : aliased ICR1_Register;
      --  TZIC interrupt clear register 2
      ICR2  : aliased ICR2_Register;
      --  TZIC interrupt clear register 3
      ICR3  : aliased ICR3_Register;
   end record
     with Volatile;

   for SEC_TZIC_Peripheral use record
      IER1  at 16#0# range 0 .. 31;
      IER2  at 16#4# range 0 .. 31;
      IER3  at 16#8# range 0 .. 31;
      MISR1 at 16#10# range 0 .. 31;
      MISR2 at 16#14# range 0 .. 31;
      MISR3 at 16#18# range 0 .. 31;
      ICR1  at 16#20# range 0 .. 31;
      ICR2  at 16#24# range 0 .. 31;
      ICR3  at 16#28# range 0 .. 31;
   end record;

   --  TZIC
   SEC_TZIC_Periph : aliased SEC_TZIC_Peripheral
     with Import, Address => System'To_Address (16#50032800#);

   --  TZIC
   TZIC_Periph : aliased SEC_TZIC_Peripheral
     with Import, Address => System'To_Address (16#40032800#);

   --  TZSC
   type SEC_TZSC_Peripheral is record
      --  TZSC control register
      TZSC_CR            : aliased TZSC_CR_Register;
      --  TZSC secure configuration register 1
      TZSC_SECCFGR1      : aliased TZSC_SECCFGR1_Register;
      --  TZSC secure configuration register 2
      TZSC_SECCFGR2      : aliased TZSC_SECCFGR2_Register;
      --  TZSC privilege configuration register 1
      TZSC_PRIVCFGR1     : aliased TZSC_PRIVCFGR1_Register;
      --  TZSC privilege configuration register 2
      TZSC_PRIVCFGR2     : aliased TZSC_PRIVCFGR2_Register;
      --  TZSC external memory non-secure watermark register 1
      TZSC_MPCWM1_NSWMR1 : aliased TZSC_MPCWM1_NSWMR1_Register;
      --  TZSC external memory non-secure watermark register 1
      TZSC_MPCWM1_NSWMR2 : aliased TZSC_MPCWM1_NSWMR2_Register;
      --  TZSC external memory non-secure watermark register 1
      TZSC_MPCWM2_NSWMR1 : aliased TZSC_MPCWM2_NSWMR1_Register;
      --  TZSC external memory non-secure watermark register 2
      TZSC_MPCWM2_NSWMR2 : aliased TZSC_MPCWM2_NSWMR2_Register;
      --  TZSC external memory non-secure watermark register 2
      TZSC_MPCWM3_NSWMR1 : aliased TZSC_MPCWM3_NSWMR1_Register;
   end record
     with Volatile;

   for SEC_TZSC_Peripheral use record
      TZSC_CR            at 16#0# range 0 .. 31;
      TZSC_SECCFGR1      at 16#10# range 0 .. 31;
      TZSC_SECCFGR2      at 16#14# range 0 .. 31;
      TZSC_PRIVCFGR1     at 16#20# range 0 .. 31;
      TZSC_PRIVCFGR2     at 16#24# range 0 .. 31;
      TZSC_MPCWM1_NSWMR1 at 16#30# range 0 .. 31;
      TZSC_MPCWM1_NSWMR2 at 16#34# range 0 .. 31;
      TZSC_MPCWM2_NSWMR1 at 16#38# range 0 .. 31;
      TZSC_MPCWM2_NSWMR2 at 16#3C# range 0 .. 31;
      TZSC_MPCWM3_NSWMR1 at 16#40# range 0 .. 31;
   end record;

   --  TZSC
   SEC_TZSC_Periph : aliased SEC_TZSC_Peripheral
     with Import, Address => System'To_Address (16#50032400#);

   --  TZSC
   TZSC_Periph : aliased SEC_TZSC_Peripheral
     with Import, Address => System'To_Address (16#40032400#);

end STM32_SVD.GTZC;
