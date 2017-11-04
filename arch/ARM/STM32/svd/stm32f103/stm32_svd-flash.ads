--  This spec has been automatically generated from STM32F103.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.FLASH is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype ACR_LATENCY_Field is HAL.UInt3;

   --  Flash access control register
   type ACR_Register is record
      --  Latency
      LATENCY       : ACR_LATENCY_Field := 16#0#;
      --  Flash half cycle access enable
      HLFCYA        : Boolean := False;
      --  Prefetch buffer enable
      PRFTBE        : Boolean := True;
      --  Read-only. Prefetch buffer status
      PRFTBS        : Boolean := True;
      --  unspecified
      Reserved_6_31 : HAL.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ACR_Register use record
      LATENCY       at 0 range 0 .. 2;
      HLFCYA        at 0 range 3 .. 3;
      PRFTBE        at 0 range 4 .. 4;
      PRFTBS        at 0 range 5 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   --  Status register
   type SR_Register is record
      --  Read-only. Busy
      BSY           : Boolean := False;
      --  unspecified
      Reserved_1_1  : HAL.Bit := 16#0#;
      --  Programming error
      PGERR         : Boolean := False;
      --  unspecified
      Reserved_3_3  : HAL.Bit := 16#0#;
      --  Write protection error
      WRPRTERR      : Boolean := False;
      --  End of operation
      EOP           : Boolean := False;
      --  unspecified
      Reserved_6_31 : HAL.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register use record
      BSY           at 0 range 0 .. 0;
      Reserved_1_1  at 0 range 1 .. 1;
      PGERR         at 0 range 2 .. 2;
      Reserved_3_3  at 0 range 3 .. 3;
      WRPRTERR      at 0 range 4 .. 4;
      EOP           at 0 range 5 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   --  Control register
   type CR_Register is record
      --  Programming
      PG             : Boolean := False;
      --  Page Erase
      PER            : Boolean := False;
      --  Mass Erase
      MER            : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Option byte programming
      OPTPG          : Boolean := False;
      --  Option byte erase
      OPTER          : Boolean := False;
      --  Start
      STRT           : Boolean := False;
      --  Lock
      LOCK           : Boolean := True;
      --  unspecified
      Reserved_8_8   : HAL.Bit := 16#0#;
      --  Option bytes write enable
      OPTWRE         : Boolean := False;
      --  Error interrupt enable
      ERRIE          : Boolean := False;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  End of operation interrupt enable
      EOPIE          : Boolean := False;
      --  unspecified
      Reserved_13_31 : HAL.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      PG             at 0 range 0 .. 0;
      PER            at 0 range 1 .. 1;
      MER            at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      OPTPG          at 0 range 4 .. 4;
      OPTER          at 0 range 5 .. 5;
      STRT           at 0 range 6 .. 6;
      LOCK           at 0 range 7 .. 7;
      Reserved_8_8   at 0 range 8 .. 8;
      OPTWRE         at 0 range 9 .. 9;
      ERRIE          at 0 range 10 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      EOPIE          at 0 range 12 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   --  OBR_Data array element
   subtype OBR_Data_Element is HAL.UInt8;

   --  OBR_Data array
   type OBR_Data_Field_Array is array (0 .. 1) of OBR_Data_Element
     with Component_Size => 8, Size => 16;

   --  Type definition for OBR_Data
   type OBR_Data_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  Data as a value
            Val : HAL.UInt16;
         when True =>
            --  Data as an array
            Arr : OBR_Data_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for OBR_Data_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  Option byte register
   type OBR_Register is record
      --  Read-only. Option byte error
      OPTERR         : Boolean;
      --  Read-only. Read protection
      RDPRT          : Boolean;
      --  Read-only. WDG_SW
      WDG_SW         : Boolean;
      --  Read-only. nRST_STOP
      nRST_STOP      : Boolean;
      --  Read-only. nRST_STDBY
      nRST_STDBY     : Boolean;
      --  unspecified
      Reserved_5_9   : HAL.UInt5;
      --  Read-only. Data0
      Data           : OBR_Data_Field;
      --  unspecified
      Reserved_26_31 : HAL.UInt6;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OBR_Register use record
      OPTERR         at 0 range 0 .. 0;
      RDPRT          at 0 range 1 .. 1;
      WDG_SW         at 0 range 2 .. 2;
      nRST_STOP      at 0 range 3 .. 3;
      nRST_STDBY     at 0 range 4 .. 4;
      Reserved_5_9   at 0 range 5 .. 9;
      Data           at 0 range 10 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  FLASH
   type FLASH_Peripheral is record
      --  Flash access control register
      ACR     : aliased ACR_Register;
      --  Flash key register
      KEYR    : aliased HAL.UInt32;
      --  Flash option key register
      OPTKEYR : aliased HAL.UInt32;
      --  Status register
      SR      : aliased SR_Register;
      --  Control register
      CR      : aliased CR_Register;
      --  Flash address register
      AR      : aliased HAL.UInt32;
      --  Option byte register
      OBR     : aliased OBR_Register;
      --  Write protection register
      WRPR    : aliased HAL.UInt32;
   end record
     with Volatile;

   for FLASH_Peripheral use record
      ACR     at 16#0# range 0 .. 31;
      KEYR    at 16#4# range 0 .. 31;
      OPTKEYR at 16#8# range 0 .. 31;
      SR      at 16#C# range 0 .. 31;
      CR      at 16#10# range 0 .. 31;
      AR      at 16#14# range 0 .. 31;
      OBR     at 16#1C# range 0 .. 31;
      WRPR    at 16#20# range 0 .. 31;
   end record;

   --  FLASH
   FLASH_Periph : aliased FLASH_Peripheral
     with Import, Address => System'To_Address (16#40022000#);

end STM32_SVD.FLASH;
