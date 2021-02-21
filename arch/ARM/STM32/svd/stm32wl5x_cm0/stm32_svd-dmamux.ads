--  This spec has been automatically generated from STM32WL5x_CM0P.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.DMAMUX is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype C0CR_DMAREQ_ID_Field is HAL.UInt8;
   subtype C0CR_SPOL_Field is HAL.UInt2;
   subtype C0CR_NBREQ_Field is HAL.UInt5;
   subtype C0CR_SYNC_ID_Field is HAL.UInt5;

   --  request line multiplexer channel x configuration register
   type C0CR_Register is record
      --  DMA request identification
      DMAREQ_ID      : C0CR_DMAREQ_ID_Field := 16#0#;
      --  Synchronization overrun interrupt enable
      SOIE           : Boolean := False;
      --  Event generation enable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : Boolean := False;
      --  Synchronization polarity
      SPOL           : C0CR_SPOL_Field := 16#0#;
      --  Number of DMA requests minus 1 to forward
      NBREQ          : C0CR_NBREQ_Field := 16#0#;
      --  Synchronization identification
      SYNC_ID        : C0CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C0CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C1CR_DMAREQ_ID_Field is HAL.UInt8;
   subtype C1CR_SPOL_Field is HAL.UInt2;
   subtype C1CR_NBREQ_Field is HAL.UInt5;
   subtype C1CR_SYNC_ID_Field is HAL.UInt5;

   --  request line multiplexer channel x configuration register
   type C1CR_Register is record
      --  DMA request identification
      DMAREQ_ID      : C1CR_DMAREQ_ID_Field := 16#0#;
      --  Synchronization overrun interrupt enable
      SOIE           : Boolean := False;
      --  Event generation enable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : Boolean := False;
      --  Synchronization polarity
      SPOL           : C1CR_SPOL_Field := 16#0#;
      --  Number of DMA requests minus 1 to forward
      NBREQ          : C1CR_NBREQ_Field := 16#0#;
      --  Synchronization identification
      SYNC_ID        : C1CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C1CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C2CR_DMAREQ_ID_Field is HAL.UInt8;
   subtype C2CR_SPOL_Field is HAL.UInt2;
   subtype C2CR_NBREQ_Field is HAL.UInt5;
   subtype C2CR_SYNC_ID_Field is HAL.UInt5;

   --  request line multiplexer channel x configuration register
   type C2CR_Register is record
      --  DMA request identification
      DMAREQ_ID      : C2CR_DMAREQ_ID_Field := 16#0#;
      --  Synchronization overrun interrupt enable
      SOIE           : Boolean := False;
      --  Event generation enable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : Boolean := False;
      --  Synchronization polarity
      SPOL           : C2CR_SPOL_Field := 16#0#;
      --  Number of DMA requests minus 1 to forward
      NBREQ          : C2CR_NBREQ_Field := 16#0#;
      --  Synchronization identification
      SYNC_ID        : C2CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C2CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C3CR_DMAREQ_ID_Field is HAL.UInt8;
   subtype C3CR_SPOL_Field is HAL.UInt2;
   subtype C3CR_NBREQ_Field is HAL.UInt5;
   subtype C3CR_SYNC_ID_Field is HAL.UInt5;

   --  request line multiplexer channel x configuration register
   type C3CR_Register is record
      --  DMA request identification
      DMAREQ_ID      : C3CR_DMAREQ_ID_Field := 16#0#;
      --  Synchronization overrun interrupt enable
      SOIE           : Boolean := False;
      --  Event generation enable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : Boolean := False;
      --  Synchronization polarity
      SPOL           : C3CR_SPOL_Field := 16#0#;
      --  Number of DMA requests minus 1 to forward
      NBREQ          : C3CR_NBREQ_Field := 16#0#;
      --  Synchronization identification
      SYNC_ID        : C3CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C3CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C4CR_DMAREQ_ID_Field is HAL.UInt8;
   subtype C4CR_SPOL_Field is HAL.UInt2;
   subtype C4CR_NBREQ_Field is HAL.UInt5;
   subtype C4CR_SYNC_ID_Field is HAL.UInt5;

   --  request line multiplexer channel x configuration register
   type C4CR_Register is record
      --  DMA request identification
      DMAREQ_ID      : C4CR_DMAREQ_ID_Field := 16#0#;
      --  Synchronization overrun interrupt enable
      SOIE           : Boolean := False;
      --  Event generation enable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : Boolean := False;
      --  Synchronization polarity
      SPOL           : C4CR_SPOL_Field := 16#0#;
      --  Number of DMA requests minus 1 to forward
      NBREQ          : C4CR_NBREQ_Field := 16#0#;
      --  Synchronization identification
      SYNC_ID        : C4CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C4CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C5CR_DMAREQ_ID_Field is HAL.UInt8;
   subtype C5CR_SPOL_Field is HAL.UInt2;
   subtype C5CR_NBREQ_Field is HAL.UInt5;
   subtype C5CR_SYNC_ID_Field is HAL.UInt5;

   --  request line multiplexer channel x configuration register
   type C5CR_Register is record
      --  DMA request identification
      DMAREQ_ID      : C5CR_DMAREQ_ID_Field := 16#0#;
      --  Synchronization overrun interrupt enable
      SOIE           : Boolean := False;
      --  Event generation enable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : Boolean := False;
      --  Synchronization polarity
      SPOL           : C5CR_SPOL_Field := 16#0#;
      --  Number of DMA requests minus 1 to forward
      NBREQ          : C5CR_NBREQ_Field := 16#0#;
      --  Synchronization identification
      SYNC_ID        : C5CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C5CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C6CR_DMAREQ_ID_Field is HAL.UInt8;
   subtype C6CR_SPOL_Field is HAL.UInt2;
   subtype C6CR_NBREQ_Field is HAL.UInt5;
   subtype C6CR_SYNC_ID_Field is HAL.UInt5;

   --  request line multiplexer channel x configuration register
   type C6CR_Register is record
      --  DMA request identification
      DMAREQ_ID      : C6CR_DMAREQ_ID_Field := 16#0#;
      --  Synchronization overrun interrupt enable
      SOIE           : Boolean := False;
      --  Event generation enable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : Boolean := False;
      --  Synchronization polarity
      SPOL           : C6CR_SPOL_Field := 16#0#;
      --  Number of DMA requests minus 1 to forward
      NBREQ          : C6CR_NBREQ_Field := 16#0#;
      --  Synchronization identification
      SYNC_ID        : C6CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C6CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C7CR_DMAREQ_ID_Field is HAL.UInt8;
   subtype C7CR_SPOL_Field is HAL.UInt2;
   subtype C7CR_NBREQ_Field is HAL.UInt5;
   subtype C7CR_SYNC_ID_Field is HAL.UInt5;

   --  request line multiplexer channel x configuration register
   type C7CR_Register is record
      --  DMA request identification
      DMAREQ_ID      : C7CR_DMAREQ_ID_Field := 16#0#;
      --  Synchronization overrun interrupt enable
      SOIE           : Boolean := False;
      --  Event generation enable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : Boolean := False;
      --  Synchronization polarity
      SPOL           : C7CR_SPOL_Field := 16#0#;
      --  Number of DMA requests minus 1 to forward
      NBREQ          : C7CR_NBREQ_Field := 16#0#;
      --  Synchronization identification
      SYNC_ID        : C7CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C7CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C8CR_DMAREQ_ID_Field is HAL.UInt8;
   subtype C8CR_SPOL_Field is HAL.UInt2;
   subtype C8CR_NBREQ_Field is HAL.UInt5;
   subtype C8CR_SYNC_ID_Field is HAL.UInt5;

   --  request line multiplexer channel x configuration register
   type C8CR_Register is record
      --  DMA request identification
      DMAREQ_ID      : C8CR_DMAREQ_ID_Field := 16#0#;
      --  Synchronization overrun interrupt enable
      SOIE           : Boolean := False;
      --  Event generation enable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : Boolean := False;
      --  Synchronization polarity
      SPOL           : C8CR_SPOL_Field := 16#0#;
      --  Number of DMA requests minus 1 to forward
      NBREQ          : C8CR_NBREQ_Field := 16#0#;
      --  Synchronization identification
      SYNC_ID        : C8CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C8CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C9CR_DMAREQ_ID_Field is HAL.UInt8;
   subtype C9CR_SPOL_Field is HAL.UInt2;
   subtype C9CR_NBREQ_Field is HAL.UInt5;
   subtype C9CR_SYNC_ID_Field is HAL.UInt5;

   --  request line multiplexer channel x configuration register
   type C9CR_Register is record
      --  DMA request identification
      DMAREQ_ID      : C9CR_DMAREQ_ID_Field := 16#0#;
      --  Synchronization overrun interrupt enable
      SOIE           : Boolean := False;
      --  Event generation enable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : Boolean := False;
      --  Synchronization polarity
      SPOL           : C9CR_SPOL_Field := 16#0#;
      --  Number of DMA requests minus 1 to forward
      NBREQ          : C9CR_NBREQ_Field := 16#0#;
      --  Synchronization identification
      SYNC_ID        : C9CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C9CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C10CR_DMAREQ_ID_Field is HAL.UInt8;
   subtype C10CR_SPOL_Field is HAL.UInt2;
   subtype C10CR_NBREQ_Field is HAL.UInt5;
   subtype C10CR_SYNC_ID_Field is HAL.UInt5;

   --  C10CR
   type C10CR_Register is record
      --  DMAREQ_ID
      DMAREQ_ID      : C10CR_DMAREQ_ID_Field := 16#0#;
      --  SOIE
      SOIE           : Boolean := False;
      --  EGE
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  SE
      SE             : Boolean := False;
      --  SPOL
      SPOL           : C10CR_SPOL_Field := 16#0#;
      --  NBREQ
      NBREQ          : C10CR_NBREQ_Field := 16#0#;
      --  SYNC_ID
      SYNC_ID        : C10CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C10CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C11CR_DMAREQ_ID_Field is HAL.UInt8;
   subtype C11CR_SPOL_Field is HAL.UInt2;
   subtype C11CR_NBREQ_Field is HAL.UInt5;
   subtype C11CR_SYNC_ID_Field is HAL.UInt5;

   --  C11CR
   type C11CR_Register is record
      --  DMAREQ_ID
      DMAREQ_ID      : C11CR_DMAREQ_ID_Field := 16#0#;
      --  SOIE
      SOIE           : Boolean := False;
      --  EGE
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  SE
      SE             : Boolean := False;
      --  SPOL
      SPOL           : C11CR_SPOL_Field := 16#0#;
      --  NBREQ
      NBREQ          : C11CR_NBREQ_Field := 16#0#;
      --  SYNC_ID
      SYNC_ID        : C11CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C11CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C12CR_DMAREQ_ID_Field is HAL.UInt8;
   subtype C12CR_SPOL_Field is HAL.UInt2;
   subtype C12CR_NBREQ_Field is HAL.UInt5;
   subtype C12CR_SYNC_ID_Field is HAL.UInt5;

   --  C12CR
   type C12CR_Register is record
      --  DMAREQ_ID
      DMAREQ_ID      : C12CR_DMAREQ_ID_Field := 16#0#;
      --  SOIE
      SOIE           : Boolean := False;
      --  EGE
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  SE
      SE             : Boolean := False;
      --  SPOL
      SPOL           : C12CR_SPOL_Field := 16#0#;
      --  NBREQ
      NBREQ          : C12CR_NBREQ_Field := 16#0#;
      --  SYNC_ID
      SYNC_ID        : C12CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C12CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C13CR_DMAREQ_ID_Field is HAL.UInt8;
   subtype C13CR_SPOL_Field is HAL.UInt2;
   subtype C13CR_NBREQ_Field is HAL.UInt5;
   subtype C13CR_SYNC_ID_Field is HAL.UInt5;

   --  C13CR
   type C13CR_Register is record
      --  DMAREQ_ID
      DMAREQ_ID      : C13CR_DMAREQ_ID_Field := 16#0#;
      --  SOIE
      SOIE           : Boolean := False;
      --  EGE
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  SE
      SE             : Boolean := False;
      --  SPOL
      SPOL           : C13CR_SPOL_Field := 16#0#;
      --  NBREQ
      NBREQ          : C13CR_NBREQ_Field := 16#0#;
      --  SYNC_ID
      SYNC_ID        : C13CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C13CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   --  CSR_SOF array
   type CSR_SOF_Field_Array is array (0 .. 13) of Boolean
     with Component_Size => 1, Size => 14;

   --  Type definition for CSR_SOF
   type CSR_SOF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SOF as a value
            Val : HAL.UInt14;
         when True =>
            --  SOF as an array
            Arr : CSR_SOF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 14;

   for CSR_SOF_Field use record
      Val at 0 range 0 .. 13;
      Arr at 0 range 0 .. 13;
   end record;

   --  request line multiplexer interrupt channel status register
   type CSR_Register is record
      --  Read-only. SOF0
      SOF            : CSR_SOF_Field;
      --  unspecified
      Reserved_14_31 : HAL.UInt18;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CSR_Register use record
      SOF            at 0 range 0 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   --  CCFR_CSOF array
   type CCFR_CSOF_Field_Array is array (0 .. 13) of Boolean
     with Component_Size => 1, Size => 14;

   --  Type definition for CCFR_CSOF
   type CCFR_CSOF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CSOF as a value
            Val : HAL.UInt14;
         when True =>
            --  CSOF as an array
            Arr : CCFR_CSOF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 14;

   for CCFR_CSOF_Field use record
      Val at 0 range 0 .. 13;
      Arr at 0 range 0 .. 13;
   end record;

   --  request line multiplexer interrupt channel clear flag register
   type CCFR_Register is record
      --  Write-only. CSOF0
      CSOF           : CCFR_CSOF_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCFR_Register use record
      CSOF           at 0 range 0 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   subtype RG0CR_SIG_ID_Field is HAL.UInt5;
   subtype RG0CR_GPOL_Field is HAL.UInt2;
   subtype RG0CR_GNBREQ_Field is HAL.UInt5;

   --  request generator channel x configuration register
   type RG0CR_Register is record
      --  Signal identification
      SIG_ID         : RG0CR_SIG_ID_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  Trigger overrun interrupt enable
      OIE            : Boolean := False;
      --  unspecified
      Reserved_9_15  : HAL.UInt7 := 16#0#;
      --  DMA request generator channel x enable
      GE             : Boolean := False;
      --  DMA request generator trigger polarity
      GPOL           : RG0CR_GPOL_Field := 16#0#;
      --  Number of DMA requests to be generated (minus 1)
      GNBREQ         : RG0CR_GNBREQ_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RG0CR_Register use record
      SIG_ID         at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      OIE            at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      GE             at 0 range 16 .. 16;
      GPOL           at 0 range 17 .. 18;
      GNBREQ         at 0 range 19 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype RG1CR_SIG_ID_Field is HAL.UInt5;
   subtype RG1CR_GPOL_Field is HAL.UInt2;
   subtype RG1CR_GNBREQ_Field is HAL.UInt5;

   --  request generator channel x configuration register
   type RG1CR_Register is record
      --  Signal identification
      SIG_ID         : RG1CR_SIG_ID_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  Trigger overrun interrupt enable
      OIE            : Boolean := False;
      --  unspecified
      Reserved_9_15  : HAL.UInt7 := 16#0#;
      --  DMA request generator channel x enable
      GE             : Boolean := False;
      --  DMA request generator trigger polarity
      GPOL           : RG1CR_GPOL_Field := 16#0#;
      --  Number of DMA requests to be generated (minus 1)
      GNBREQ         : RG1CR_GNBREQ_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RG1CR_Register use record
      SIG_ID         at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      OIE            at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      GE             at 0 range 16 .. 16;
      GPOL           at 0 range 17 .. 18;
      GNBREQ         at 0 range 19 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype RG2CR_SIG_ID_Field is HAL.UInt5;
   subtype RG2CR_GPOL_Field is HAL.UInt2;
   subtype RG2CR_GNBREQ_Field is HAL.UInt5;

   --  request generator channel x configuration register
   type RG2CR_Register is record
      --  Signal identification
      SIG_ID         : RG2CR_SIG_ID_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  Trigger overrun interrupt enable
      OIE            : Boolean := False;
      --  unspecified
      Reserved_9_15  : HAL.UInt7 := 16#0#;
      --  DMA request generator channel x enable
      GE             : Boolean := False;
      --  DMA request generator trigger polarity
      GPOL           : RG2CR_GPOL_Field := 16#0#;
      --  Number of DMA requests to be generated (minus 1)
      GNBREQ         : RG2CR_GNBREQ_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RG2CR_Register use record
      SIG_ID         at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      OIE            at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      GE             at 0 range 16 .. 16;
      GPOL           at 0 range 17 .. 18;
      GNBREQ         at 0 range 19 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype RG3CR_SIG_ID_Field is HAL.UInt5;
   subtype RG3CR_GPOL_Field is HAL.UInt2;
   subtype RG3CR_GNBREQ_Field is HAL.UInt5;

   --  request generator channel x configuration register
   type RG3CR_Register is record
      --  Signal identification
      SIG_ID         : RG3CR_SIG_ID_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  Trigger overrun interrupt enable
      OIE            : Boolean := False;
      --  unspecified
      Reserved_9_15  : HAL.UInt7 := 16#0#;
      --  DMA request generator channel x enable
      GE             : Boolean := False;
      --  DMA request generator trigger polarity
      GPOL           : RG3CR_GPOL_Field := 16#0#;
      --  Number of DMA requests to be generated (minus 1)
      GNBREQ         : RG3CR_GNBREQ_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RG3CR_Register use record
      SIG_ID         at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      OIE            at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      GE             at 0 range 16 .. 16;
      GPOL           at 0 range 17 .. 18;
      GNBREQ         at 0 range 19 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  RGSR_OF array
   type RGSR_OF_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for RGSR_OF
   type RGSR_OF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  OF as a value
            Val : HAL.UInt4;
         when True =>
            --  OF as an array
            Arr : RGSR_OF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for RGSR_OF_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  request generator interrupt status register
   type RGSR_Register is record
      --  Read-only. OF0
      OF_k          : RGSR_OF_Field;
      --  unspecified
      Reserved_4_31 : HAL.UInt28;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RGSR_Register use record
      OF_k          at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  RGCFR_COF array
   type RGCFR_COF_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for RGCFR_COF
   type RGCFR_COF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  COF as a value
            Val : HAL.UInt4;
         when True =>
            --  COF as an array
            Arr : RGCFR_COF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for RGCFR_COF_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  request generator interrupt clear flag register
   type RGCFR_Register is record
      --  Write-only. COF0
      COF           : RGCFR_COF_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RGCFR_Register use record
      COF           at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  DMA request multiplexer
   type DMAMUX_Peripheral is record
      --  request line multiplexer channel x configuration register
      C0CR  : aliased C0CR_Register;
      --  request line multiplexer channel x configuration register
      C1CR  : aliased C1CR_Register;
      --  request line multiplexer channel x configuration register
      C2CR  : aliased C2CR_Register;
      --  request line multiplexer channel x configuration register
      C3CR  : aliased C3CR_Register;
      --  request line multiplexer channel x configuration register
      C4CR  : aliased C4CR_Register;
      --  request line multiplexer channel x configuration register
      C5CR  : aliased C5CR_Register;
      --  request line multiplexer channel x configuration register
      C6CR  : aliased C6CR_Register;
      --  request line multiplexer channel x configuration register
      C7CR  : aliased C7CR_Register;
      --  request line multiplexer channel x configuration register
      C8CR  : aliased C8CR_Register;
      --  request line multiplexer channel x configuration register
      C9CR  : aliased C9CR_Register;
      --  C10CR
      C10CR : aliased C10CR_Register;
      --  C11CR
      C11CR : aliased C11CR_Register;
      --  C12CR
      C12CR : aliased C12CR_Register;
      --  C13CR
      C13CR : aliased C13CR_Register;
      --  request line multiplexer interrupt channel status register
      CSR   : aliased CSR_Register;
      --  request line multiplexer interrupt channel clear flag register
      CCFR  : aliased CCFR_Register;
      --  request generator channel x configuration register
      RG0CR : aliased RG0CR_Register;
      --  request generator channel x configuration register
      RG1CR : aliased RG1CR_Register;
      --  request generator channel x configuration register
      RG2CR : aliased RG2CR_Register;
      --  request generator channel x configuration register
      RG3CR : aliased RG3CR_Register;
      --  request generator interrupt status register
      RGSR  : aliased RGSR_Register;
      --  request generator interrupt clear flag register
      RGCFR : aliased RGCFR_Register;
   end record
     with Volatile;

   for DMAMUX_Peripheral use record
      C0CR  at 16#0# range 0 .. 31;
      C1CR  at 16#4# range 0 .. 31;
      C2CR  at 16#8# range 0 .. 31;
      C3CR  at 16#C# range 0 .. 31;
      C4CR  at 16#10# range 0 .. 31;
      C5CR  at 16#14# range 0 .. 31;
      C6CR  at 16#18# range 0 .. 31;
      C7CR  at 16#1C# range 0 .. 31;
      C8CR  at 16#20# range 0 .. 31;
      C9CR  at 16#24# range 0 .. 31;
      C10CR at 16#28# range 0 .. 31;
      C11CR at 16#2C# range 0 .. 31;
      C12CR at 16#30# range 0 .. 31;
      C13CR at 16#34# range 0 .. 31;
      CSR   at 16#80# range 0 .. 31;
      CCFR  at 16#84# range 0 .. 31;
      RG0CR at 16#100# range 0 .. 31;
      RG1CR at 16#104# range 0 .. 31;
      RG2CR at 16#108# range 0 .. 31;
      RG3CR at 16#10C# range 0 .. 31;
      RGSR  at 16#140# range 0 .. 31;
      RGCFR at 16#144# range 0 .. 31;
   end record;

   --  DMA request multiplexer
   DMAMUX_Periph : aliased DMAMUX_Peripheral
     with Import, Address => DMAMUX_Base;

end STM32_SVD.DMAMUX;
