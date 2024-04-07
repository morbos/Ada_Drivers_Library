--  This spec has been automatically generated from STM32L562.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.HASH is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype CR_NBW_Field is HAL.UInt4;

   type CR_Register is record
      --  unspecified
      Reserved_0_2   : HAL.UInt3 := 16#0#;
      INIT           : Boolean := False;
      DMAE           : Boolean := False;
      DATATYPE       : Boolean := False;
      MODE           : Boolean := False;
      ALGO           : Boolean := False;
      NBW            : CR_NBW_Field := 16#0#;
      DINNE          : Boolean := False;
      MDMAT          : Boolean := False;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      LKEY           : Boolean := False;
      --  unspecified
      Reserved_17_17 : HAL.Bit := 16#0#;
      ALGO_1         : Boolean := False;
      --  unspecified
      Reserved_19_31 : HAL.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      Reserved_0_2   at 0 range 0 .. 2;
      INIT           at 0 range 3 .. 3;
      DMAE           at 0 range 4 .. 4;
      DATATYPE       at 0 range 5 .. 5;
      MODE           at 0 range 6 .. 6;
      ALGO           at 0 range 7 .. 7;
      NBW            at 0 range 8 .. 11;
      DINNE          at 0 range 12 .. 12;
      MDMAT          at 0 range 13 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      LKEY           at 0 range 16 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      ALGO_1         at 0 range 18 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   subtype STR_NBLW_Field is HAL.UInt5;

   type STR_Register is record
      NBLW          : STR_NBLW_Field := 16#0#;
      --  unspecified
      Reserved_5_7  : HAL.UInt3 := 16#0#;
      DCAL          : Boolean := False;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for STR_Register use record
      NBLW          at 0 range 0 .. 4;
      Reserved_5_7  at 0 range 5 .. 7;
      DCAL          at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   type IMR_Register is record
      DINIE         : Boolean := False;
      DCIE          : Boolean := False;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IMR_Register use record
      DINIE         at 0 range 0 .. 0;
      DCIE          at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   type SR_Register is record
      DINIS         : Boolean := False;
      DCIS          : Boolean := False;
      DMAS          : Boolean := False;
      BUSY          : Boolean := False;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register use record
      DINIS         at 0 range 0 .. 0;
      DCIS          at 0 range 1 .. 1;
      DMAS          at 0 range 2 .. 2;
      BUSY          at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   type HASH_Peripheral is record
      CR    : aliased CR_Register;
      DIN   : aliased HAL.UInt32;
      STR   : aliased STR_Register;
      HRA0  : aliased HAL.UInt32;
      HRA1  : aliased HAL.UInt32;
      HRA2  : aliased HAL.UInt32;
      HRA3  : aliased HAL.UInt32;
      HRA4  : aliased HAL.UInt32;
      IMR   : aliased IMR_Register;
      SR    : aliased SR_Register;
      CSR0  : aliased HAL.UInt32;
      CSR1  : aliased HAL.UInt32;
      CSR2  : aliased HAL.UInt32;
      CSR3  : aliased HAL.UInt32;
      CSR4  : aliased HAL.UInt32;
      CSR5  : aliased HAL.UInt32;
      CSR6  : aliased HAL.UInt32;
      CSR7  : aliased HAL.UInt32;
      CSR8  : aliased HAL.UInt32;
      CSR9  : aliased HAL.UInt32;
      CSR10 : aliased HAL.UInt32;
      CSR11 : aliased HAL.UInt32;
      CSR12 : aliased HAL.UInt32;
      CSR13 : aliased HAL.UInt32;
      CSR14 : aliased HAL.UInt32;
      CSR15 : aliased HAL.UInt32;
      CSR16 : aliased HAL.UInt32;
      CSR17 : aliased HAL.UInt32;
      CSR18 : aliased HAL.UInt32;
      CSR19 : aliased HAL.UInt32;
      CSR20 : aliased HAL.UInt32;
      CSR21 : aliased HAL.UInt32;
      CSR22 : aliased HAL.UInt32;
      CSR23 : aliased HAL.UInt32;
      CSR24 : aliased HAL.UInt32;
      CSR25 : aliased HAL.UInt32;
      CSR26 : aliased HAL.UInt32;
      CSR27 : aliased HAL.UInt32;
      CSR28 : aliased HAL.UInt32;
      CSR29 : aliased HAL.UInt32;
      CSR30 : aliased HAL.UInt32;
      CSR31 : aliased HAL.UInt32;
      CSR32 : aliased HAL.UInt32;
      CSR33 : aliased HAL.UInt32;
      CSR34 : aliased HAL.UInt32;
      CSR35 : aliased HAL.UInt32;
      CSR36 : aliased HAL.UInt32;
      CSR37 : aliased HAL.UInt32;
      CSR38 : aliased HAL.UInt32;
      CSR39 : aliased HAL.UInt32;
      CSR40 : aliased HAL.UInt32;
      CSR41 : aliased HAL.UInt32;
      CSR42 : aliased HAL.UInt32;
      CSR43 : aliased HAL.UInt32;
      CSR44 : aliased HAL.UInt32;
      CSR45 : aliased HAL.UInt32;
      CSR46 : aliased HAL.UInt32;
      CSR47 : aliased HAL.UInt32;
      CSR48 : aliased HAL.UInt32;
      CSR49 : aliased HAL.UInt32;
      CSR50 : aliased HAL.UInt32;
      CSR51 : aliased HAL.UInt32;
      CSR52 : aliased HAL.UInt32;
      CSR53 : aliased HAL.UInt32;
      HR0   : aliased HAL.UInt32;
      HR1   : aliased HAL.UInt32;
      HR2   : aliased HAL.UInt32;
      HR3   : aliased HAL.UInt32;
      HR4   : aliased HAL.UInt32;
      HR5   : aliased HAL.UInt32;
      HR6   : aliased HAL.UInt32;
      HR7   : aliased HAL.UInt32;
   end record
     with Volatile;

   for HASH_Peripheral use record
      CR    at 16#0# range 0 .. 31;
      DIN   at 16#4# range 0 .. 31;
      STR   at 16#8# range 0 .. 31;
      HRA0  at 16#C# range 0 .. 31;
      HRA1  at 16#10# range 0 .. 31;
      HRA2  at 16#14# range 0 .. 31;
      HRA3  at 16#18# range 0 .. 31;
      HRA4  at 16#1C# range 0 .. 31;
      IMR   at 16#20# range 0 .. 31;
      SR    at 16#24# range 0 .. 31;
      CSR0  at 16#F8# range 0 .. 31;
      CSR1  at 16#FC# range 0 .. 31;
      CSR2  at 16#100# range 0 .. 31;
      CSR3  at 16#104# range 0 .. 31;
      CSR4  at 16#108# range 0 .. 31;
      CSR5  at 16#10C# range 0 .. 31;
      CSR6  at 16#110# range 0 .. 31;
      CSR7  at 16#114# range 0 .. 31;
      CSR8  at 16#118# range 0 .. 31;
      CSR9  at 16#11C# range 0 .. 31;
      CSR10 at 16#120# range 0 .. 31;
      CSR11 at 16#124# range 0 .. 31;
      CSR12 at 16#128# range 0 .. 31;
      CSR13 at 16#12C# range 0 .. 31;
      CSR14 at 16#130# range 0 .. 31;
      CSR15 at 16#134# range 0 .. 31;
      CSR16 at 16#138# range 0 .. 31;
      CSR17 at 16#13C# range 0 .. 31;
      CSR18 at 16#140# range 0 .. 31;
      CSR19 at 16#144# range 0 .. 31;
      CSR20 at 16#148# range 0 .. 31;
      CSR21 at 16#14C# range 0 .. 31;
      CSR22 at 16#150# range 0 .. 31;
      CSR23 at 16#154# range 0 .. 31;
      CSR24 at 16#158# range 0 .. 31;
      CSR25 at 16#15C# range 0 .. 31;
      CSR26 at 16#160# range 0 .. 31;
      CSR27 at 16#164# range 0 .. 31;
      CSR28 at 16#168# range 0 .. 31;
      CSR29 at 16#16C# range 0 .. 31;
      CSR30 at 16#170# range 0 .. 31;
      CSR31 at 16#174# range 0 .. 31;
      CSR32 at 16#178# range 0 .. 31;
      CSR33 at 16#17C# range 0 .. 31;
      CSR34 at 16#180# range 0 .. 31;
      CSR35 at 16#184# range 0 .. 31;
      CSR36 at 16#188# range 0 .. 31;
      CSR37 at 16#18C# range 0 .. 31;
      CSR38 at 16#190# range 0 .. 31;
      CSR39 at 16#194# range 0 .. 31;
      CSR40 at 16#198# range 0 .. 31;
      CSR41 at 16#19C# range 0 .. 31;
      CSR42 at 16#1A0# range 0 .. 31;
      CSR43 at 16#1A4# range 0 .. 31;
      CSR44 at 16#1A8# range 0 .. 31;
      CSR45 at 16#1AC# range 0 .. 31;
      CSR46 at 16#1B0# range 0 .. 31;
      CSR47 at 16#1B4# range 0 .. 31;
      CSR48 at 16#1B8# range 0 .. 31;
      CSR49 at 16#1BC# range 0 .. 31;
      CSR50 at 16#1C0# range 0 .. 31;
      CSR51 at 16#1C4# range 0 .. 31;
      CSR52 at 16#1C8# range 0 .. 31;
      CSR53 at 16#1CC# range 0 .. 31;
      HR0   at 16#310# range 0 .. 31;
      HR1   at 16#314# range 0 .. 31;
      HR2   at 16#318# range 0 .. 31;
      HR3   at 16#31C# range 0 .. 31;
      HR4   at 16#320# range 0 .. 31;
      HR5   at 16#324# range 0 .. 31;
      HR6   at 16#328# range 0 .. 31;
      HR7   at 16#32C# range 0 .. 31;
   end record;

   HASH_Periph : aliased HASH_Peripheral
     with Import, Address => System'To_Address (16#420C0400#);

   SEC_HASH_Periph : aliased HASH_Peripheral
     with Import, Address => System'To_Address (16#520C0400#);

end STM32_SVD.HASH;
