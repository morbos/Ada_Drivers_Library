with System;

with STM32_SVD.SAU; use STM32_SVD.SAU;

package STM32.SAU is

   type SAU_Regions is range 0 .. 7;

   procedure Add_Region (Region_Num : SAU_Regions;
                         Addr       : UInt32;
                         Size       : UInt32;
                         NSC        : Boolean);

   procedure Enable_SAU;

   procedure All_NS;

end STM32.SAU;
