--  This spec has been automatically generated from STM32L151.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.FSMC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype BCR_MTYP_Field is HAL.UInt2;
   subtype BCR_MWID_Field is HAL.UInt2;

   --  BCR1
   type BCR_Register is record
      --  MBKEN
      MBKEN          : Boolean := False;
      --  MUXEN
      MUXEN          : Boolean := False;
      --  MTYP
      MTYP           : BCR_MTYP_Field := 16#0#;
      --  MWID
      MWID           : BCR_MWID_Field := 16#0#;
      --  FACCEN
      FACCEN         : Boolean := False;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  BURSTEN
      BURSTEN        : Boolean := False;
      --  WAITPOL
      WAITPOL        : Boolean := False;
      --  WRAPMOD
      WRAPMOD        : Boolean := False;
      --  WAITCFG
      WAITCFG        : Boolean := False;
      --  WREN
      WREN           : Boolean := False;
      --  WAITEN
      WAITEN         : Boolean := False;
      --  EXTMOD
      EXTMOD         : Boolean := False;
      --  ASYNCWAIT
      ASYNCWAIT      : Boolean := False;
      --  unspecified
      Reserved_16_18 : HAL.UInt3 := 16#0#;
      --  CBURSTRW
      CBURSTRW       : Boolean := False;
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for BCR_Register use record
      MBKEN          at 0 range 0 .. 0;
      MUXEN          at 0 range 1 .. 1;
      MTYP           at 0 range 2 .. 3;
      MWID           at 0 range 4 .. 5;
      FACCEN         at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      BURSTEN        at 0 range 8 .. 8;
      WAITPOL        at 0 range 9 .. 9;
      WRAPMOD        at 0 range 10 .. 10;
      WAITCFG        at 0 range 11 .. 11;
      WREN           at 0 range 12 .. 12;
      WAITEN         at 0 range 13 .. 13;
      EXTMOD         at 0 range 14 .. 14;
      ASYNCWAIT      at 0 range 15 .. 15;
      Reserved_16_18 at 0 range 16 .. 18;
      CBURSTRW       at 0 range 19 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   subtype BTR_ADDSET_Field is HAL.UInt4;
   subtype BTR_ADDHLD_Field is HAL.UInt4;
   subtype BTR_DATAST_Field is HAL.UInt8;
   subtype BTR_BUSTURN_Field is HAL.UInt4;
   subtype BTR_CLKDIV_Field is HAL.UInt4;
   subtype BTR_DATLAT_Field is HAL.UInt4;
   subtype BTR_ACCMOD_Field is HAL.UInt2;

   --  BTR1
   type BTR_Register is record
      --  ADDSET
      ADDSET         : BTR_ADDSET_Field := 16#0#;
      --  ADDHLD
      ADDHLD         : BTR_ADDHLD_Field := 16#0#;
      --  DATAST
      DATAST         : BTR_DATAST_Field := 16#0#;
      --  BUSTURN
      BUSTURN        : BTR_BUSTURN_Field := 16#0#;
      --  CLKDIV
      CLKDIV         : BTR_CLKDIV_Field := 16#0#;
      --  DATLAT
      DATLAT         : BTR_DATLAT_Field := 16#0#;
      --  ACCMOD
      ACCMOD         : BTR_ACCMOD_Field := 16#0#;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for BTR_Register use record
      ADDSET         at 0 range 0 .. 3;
      ADDHLD         at 0 range 4 .. 7;
      DATAST         at 0 range 8 .. 15;
      BUSTURN        at 0 range 16 .. 19;
      CLKDIV         at 0 range 20 .. 23;
      DATLAT         at 0 range 24 .. 27;
      ACCMOD         at 0 range 28 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype BWTR_ADDSET_Field is HAL.UInt4;
   subtype BWTR_ADDHLD_Field is HAL.UInt4;
   subtype BWTR_DATAST_Field is HAL.UInt8;
   subtype BWTR_CLKDIV_Field is HAL.UInt4;
   subtype BWTR_DATLAT_Field is HAL.UInt4;
   subtype BWTR_ACCMOD_Field is HAL.UInt2;

   --  BWTR1
   type BWTR_Register is record
      --  ADDSET
      ADDSET         : BWTR_ADDSET_Field := 16#0#;
      --  ADDHLD
      ADDHLD         : BWTR_ADDHLD_Field := 16#0#;
      --  DATAST
      DATAST         : BWTR_DATAST_Field := 16#0#;
      --  unspecified
      Reserved_16_19 : HAL.UInt4 := 16#0#;
      --  CLKDIV
      CLKDIV         : BWTR_CLKDIV_Field := 16#0#;
      --  DATLAT
      DATLAT         : BWTR_DATLAT_Field := 16#0#;
      --  ACCMOD
      ACCMOD         : BWTR_ACCMOD_Field := 16#0#;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for BWTR_Register use record
      ADDSET         at 0 range 0 .. 3;
      ADDHLD         at 0 range 4 .. 7;
      DATAST         at 0 range 8 .. 15;
      Reserved_16_19 at 0 range 16 .. 19;
      CLKDIV         at 0 range 20 .. 23;
      DATLAT         at 0 range 24 .. 27;
      ACCMOD         at 0 range 28 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Flexible static memory controller
   type FSMC_Peripheral is record
      --  BCR1
      BCR1  : aliased BCR_Register;
      --  BTR1
      BTR1  : aliased BTR_Register;
      --  BCR2
      BCR2  : aliased BCR_Register;
      --  BTR2
      BTR2  : aliased BTR_Register;
      --  BCR3
      BCR3  : aliased BCR_Register;
      --  BTR3
      BTR3  : aliased BTR_Register;
      --  BCR4
      BCR4  : aliased BCR_Register;
      --  BTR4
      BTR4  : aliased BTR_Register;
      --  BWTR1
      BWTR1 : aliased BWTR_Register;
      --  BWTR2
      BWTR2 : aliased BWTR_Register;
      --  BWTR3
      BWTR3 : aliased BWTR_Register;
      --  BWTR4
      BWTR4 : aliased BWTR_Register;
   end record
     with Volatile;

   for FSMC_Peripheral use record
      BCR1  at 16#0# range 0 .. 31;
      BTR1  at 16#4# range 0 .. 31;
      BCR2  at 16#8# range 0 .. 31;
      BTR2  at 16#C# range 0 .. 31;
      BCR3  at 16#10# range 0 .. 31;
      BTR3  at 16#14# range 0 .. 31;
      BCR4  at 16#18# range 0 .. 31;
      BTR4  at 16#1C# range 0 .. 31;
      BWTR1 at 16#104# range 0 .. 31;
      BWTR2 at 16#10C# range 0 .. 31;
      BWTR3 at 16#114# range 0 .. 31;
      BWTR4 at 16#11C# range 0 .. 31;
   end record;

   --  Flexible static memory controller
   FSMC_Periph : aliased FSMC_Peripheral
     with Import, Address => System'To_Address (16#A0000000#);

end STM32_SVD.FSMC;
