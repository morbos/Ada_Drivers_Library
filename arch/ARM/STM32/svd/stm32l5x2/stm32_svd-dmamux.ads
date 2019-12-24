--  This spec has been automatically generated from STM32L5x2.svd

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

   subtype C0CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype C0CR_SPOL_Field is HAL.UInt2;
   subtype C0CR_NBREQ_Field is HAL.UInt5;
   subtype C0CR_SYNC_ID_Field is HAL.UInt5;

   --  DMA Multiplexer Channel 0 Control register
   type C0CR_Register is record
      --  DMA Request ID
      DMAREQ_ID      : C0CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Synchronization Overrun Interrupt Enable
      SOIE           : Boolean := False;
      --  Event Generation Enable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : Boolean := False;
      --  Sync polarity
      SPOL           : C0CR_SPOL_Field := 16#0#;
      --  Nb request
      NBREQ          : C0CR_NBREQ_Field := 16#0#;
      --  SYNC_ID
      SYNC_ID        : C0CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for C0CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C1CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype C1CR_SPOL_Field is HAL.UInt2;
   subtype C1CR_NBREQ_Field is HAL.UInt5;
   subtype C1CR_SYNC_ID_Field is HAL.UInt5;

   --  DMA Multiplexer Channel 1 Control register
   type C1CR_Register is record
      --  DMA Request ID
      DMAREQ_ID      : C1CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Synchronization Overrun Interrupt Enable
      SOIE           : Boolean := False;
      --  Event Generation Enable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : Boolean := False;
      --  Sync polarity
      SPOL           : C1CR_SPOL_Field := 16#0#;
      --  Nb request
      NBREQ          : C1CR_NBREQ_Field := 16#0#;
      --  SYNC_ID
      SYNC_ID        : C1CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for C1CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C2CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype C2CR_SPOL_Field is HAL.UInt2;
   subtype C2CR_NBREQ_Field is HAL.UInt5;
   subtype C2CR_SYNC_ID_Field is HAL.UInt5;

   --  DMA Multiplexer Channel 2 Control register
   type C2CR_Register is record
      --  DMA Request ID
      DMAREQ_ID      : C2CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Synchronization Overrun Interrupt Enable
      SOIE           : Boolean := False;
      --  Event Generation Enable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : Boolean := False;
      --  Sync polarity
      SPOL           : C2CR_SPOL_Field := 16#0#;
      --  Nb request
      NBREQ          : C2CR_NBREQ_Field := 16#0#;
      --  SYNC_ID
      SYNC_ID        : C2CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for C2CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C3CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype C3CR_SPOL_Field is HAL.UInt2;
   subtype C3CR_NBREQ_Field is HAL.UInt5;
   subtype C3CR_SYNC_ID_Field is HAL.UInt5;

   --  DMA Multiplexer Channel 3 Control register
   type C3CR_Register is record
      --  DMA Request ID
      DMAREQ_ID      : C3CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Synchronization Overrun Interrupt Enable
      SOIE           : Boolean := False;
      --  Event Generation Enable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : Boolean := False;
      --  Sync polarity
      SPOL           : C3CR_SPOL_Field := 16#0#;
      --  Nb request
      NBREQ          : C3CR_NBREQ_Field := 16#0#;
      --  SYNC_ID
      SYNC_ID        : C3CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for C3CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C4CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype C4CR_SPOL_Field is HAL.UInt2;
   subtype C4CR_NBREQ_Field is HAL.UInt5;
   subtype C4CR_SYNC_ID_Field is HAL.UInt5;

   --  DMA Multiplexer Channel 4 Control register
   type C4CR_Register is record
      --  DMA Request ID
      DMAREQ_ID      : C4CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Synchronization Overrun Interrupt Enable
      SOIE           : Boolean := False;
      --  Event Generation Enable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : Boolean := False;
      --  Sync polarity
      SPOL           : C4CR_SPOL_Field := 16#0#;
      --  Nb request
      NBREQ          : C4CR_NBREQ_Field := 16#0#;
      --  SYNC_ID
      SYNC_ID        : C4CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for C4CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C5CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype C5CR_SPOL_Field is HAL.UInt2;
   subtype C5CR_NBREQ_Field is HAL.UInt5;
   subtype C5CR_SYNC_ID_Field is HAL.UInt5;

   --  DMA Multiplexer Channel 5 Control register
   type C5CR_Register is record
      --  DMA Request ID
      DMAREQ_ID      : C5CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Synchronization Overrun Interrupt Enable
      OIE            : Boolean := False;
      --  Event Generation Enable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : Boolean := False;
      --  Sync polarity
      SPOL           : C5CR_SPOL_Field := 16#0#;
      --  Nb request
      NBREQ          : C5CR_NBREQ_Field := 16#0#;
      --  SYNC_ID
      SYNC_ID        : C5CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for C5CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      OIE            at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C6CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype C6CR_SPOL_Field is HAL.UInt2;
   subtype C6CR_NBREQ_Field is HAL.UInt5;
   subtype C6CR_SYNC_ID_Field is HAL.UInt5;

   --  DMA Multiplexer Channel 6 Control register
   type C6CR_Register is record
      --  DMA Request ID
      DMAREQ_ID      : C6CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Synchronization Overrun Interrupt Enable
      SOIE           : Boolean := False;
      --  Event Generation Enable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : Boolean := False;
      --  Sync polarity
      SPOL           : C6CR_SPOL_Field := 16#0#;
      --  Nb request
      NBREQ          : C6CR_NBREQ_Field := 16#0#;
      --  SYNC_ID
      SYNC_ID        : C6CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for C6CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C7CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype C7CR_SPOL_Field is HAL.UInt2;
   subtype C7CR_NBREQ_Field is HAL.UInt5;
   subtype C7CR_SYNC_ID_Field is HAL.UInt5;

   --  DMA Multiplexer Channel 7 Control register
   type C7CR_Register is record
      --  DMA Request ID
      DMAREQ_ID      : C7CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Synchronization Overrun Interrupt Enable
      SOIE           : Boolean := False;
      --  Event Generation Enable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : Boolean := False;
      --  Sync polarity
      SPOL           : C7CR_SPOL_Field := 16#0#;
      --  Nb request
      NBREQ          : C7CR_NBREQ_Field := 16#0#;
      --  SYNC_ID
      SYNC_ID        : C7CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for C7CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C8CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype C8CR_SPOL_Field is HAL.UInt2;
   subtype C8CR_NBREQ_Field is HAL.UInt5;
   subtype C8CR_SYNC_ID_Field is HAL.UInt5;

   --  DMA Multiplexer Channel 8 Control register
   type C8CR_Register is record
      --  DMA Request ID
      DMAREQ_ID      : C8CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Synchronization Overrun Interrupt Enable
      SOIE           : Boolean := False;
      --  Event Generation Enable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : Boolean := False;
      --  Sync polarity
      SPOL           : C8CR_SPOL_Field := 16#0#;
      --  Nb request
      NBREQ          : C8CR_NBREQ_Field := 16#0#;
      --  SYNC_ID
      SYNC_ID        : C8CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for C8CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C9CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype C9CR_SPOL_Field is HAL.UInt2;
   subtype C9CR_NBREQ_Field is HAL.UInt5;
   subtype C9CR_SYNC_ID_Field is HAL.UInt5;

   --  DMA Multiplexer Channel 9 Control register
   type C9CR_Register is record
      --  DMA Request ID
      DMAREQ_ID      : C9CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Synchronization Overrun Interrupt Enable
      SOIE           : Boolean := False;
      --  Event Generation Enable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : Boolean := False;
      --  Sync polarity
      SPOL           : C9CR_SPOL_Field := 16#0#;
      --  Nb request
      NBREQ          : C9CR_NBREQ_Field := 16#0#;
      --  SYNC_ID
      SYNC_ID        : C9CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for C9CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C10CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype C10CR_SPOL_Field is HAL.UInt2;
   subtype C10CR_NBREQ_Field is HAL.UInt5;
   subtype C10CR_SYNC_ID_Field is HAL.UInt5;

   --  DMA Multiplexer Channel 10 Control register
   type C10CR_Register is record
      --  DMA Request ID
      DMAREQ_ID      : C10CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Synchronization Overrun Interrupt Enable
      SOIE           : Boolean := False;
      --  Event Generation Enable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : Boolean := False;
      --  Sync polarity
      SPOL           : C10CR_SPOL_Field := 16#0#;
      --  Nb request
      NBREQ          : C10CR_NBREQ_Field := 16#0#;
      --  SYNC_ID
      SYNC_ID        : C10CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for C10CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C11CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype C11CR_SPOL_Field is HAL.UInt2;
   subtype C11CR_NBREQ_Field is HAL.UInt5;
   subtype C11CR_SYNC_ID_Field is HAL.UInt5;

   --  DMA Multiplexer Channel 11 Control register
   type C11CR_Register is record
      --  DMA Request ID
      DMAREQ_ID      : C11CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Synchronization Overrun Interrupt Enable
      SOIE           : Boolean := False;
      --  Event Generation Enable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : Boolean := False;
      --  Sync polarity
      SPOL           : C11CR_SPOL_Field := 16#0#;
      --  Nb request
      NBREQ          : C11CR_NBREQ_Field := 16#0#;
      --  SYNC_ID
      SYNC_ID        : C11CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for C11CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C12CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype C12CR_SPOL_Field is HAL.UInt2;
   subtype C12CR_NBREQ_Field is HAL.UInt5;
   subtype C12CR_SYNC_ID_Field is HAL.UInt5;

   --  DMA Multiplexer Channel 12 Control register
   type C12CR_Register is record
      --  DMA Request ID
      DMAREQ_ID      : C12CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Synchronization Overrun Interrupt Enable
      SOIE           : Boolean := False;
      --  Event Generation Enable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : Boolean := False;
      --  Sync polarity
      SPOL           : C12CR_SPOL_Field := 16#0#;
      --  Nb request
      NBREQ          : C12CR_NBREQ_Field := 16#0#;
      --  SYNC_ID
      SYNC_ID        : C12CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for C12CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C13CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype C13CR_SPOL_Field is HAL.UInt2;
   subtype C13CR_NBREQ_Field is HAL.UInt5;
   subtype C13CR_SYNC_ID_Field is HAL.UInt5;

   --  DMA Multiplexer Channel 13 Control register
   type C13CR_Register is record
      --  DMA Request ID
      DMAREQ_ID      : C13CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Synchronization Overrun Interrupt Enable
      SOIE           : Boolean := False;
      --  Event Generation Enable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : Boolean := False;
      --  Sync polarity
      SPOL           : C13CR_SPOL_Field := 16#0#;
      --  Nb request
      NBREQ          : C13CR_NBREQ_Field := 16#0#;
      --  SYNC_ID
      SYNC_ID        : C13CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for C13CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
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
   type CSR_SOF_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for CSR_SOF
   type CSR_SOF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SOF as a value
            Val : HAL.UInt16;
         when True =>
            --  SOF as an array
            Arr : CSR_SOF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for CSR_SOF_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  DMA Multiplexer Channel Status register
   type CSR_Register is record
      --  Synchronization Overrun Flag 0
      SOF            : CSR_SOF_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CSR_Register use record
      SOF            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  CCFR_CSOF array
   type CCFR_CSOF_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for CCFR_CSOF
   type CCFR_CSOF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CSOF as a value
            Val : HAL.UInt16;
         when True =>
            --  CSOF as an array
            Arr : CCFR_CSOF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for CCFR_CSOF_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  DMA Channel Clear Flag Register
   type CCFR_Register is record
      --  Synchronization Clear Overrun Flag 0
      CSOF           : CCFR_CSOF_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCFR_Register use record
      CSOF           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype RG0CR_SIG_ID_Field is HAL.UInt5;
   subtype RG0CR_GPOL_Field is HAL.UInt2;
   subtype RG0CR_GNBREQ_Field is HAL.UInt5;

   --  DMA Request Generator 0 Control Register
   type RG0CR_Register is record
      --  Signal ID
      SIG_ID         : RG0CR_SIG_ID_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  Overrun Interrupt Enable
      OIE            : Boolean := False;
      --  unspecified
      Reserved_9_15  : HAL.UInt7 := 16#0#;
      --  Generation Enable
      GE             : Boolean := False;
      --  Generation Polarity
      GPOL           : RG0CR_GPOL_Field := 16#0#;
      --  Number of Request
      GNBREQ         : RG0CR_GNBREQ_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
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

   --  DMA Request Generator 1 Control Register
   type RG1CR_Register is record
      --  Signal ID
      SIG_ID         : RG1CR_SIG_ID_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  Overrun Interrupt Enable
      OIE            : Boolean := False;
      --  unspecified
      Reserved_9_15  : HAL.UInt7 := 16#0#;
      --  Generation Enable
      GE             : Boolean := False;
      --  Generation Polarity
      GPOL           : RG1CR_GPOL_Field := 16#0#;
      --  Number of Request
      GNBREQ         : RG1CR_GNBREQ_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
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

   --  DMA Request Generator 2 Control Register
   type RG2CR_Register is record
      --  Signal ID
      SIG_ID         : RG2CR_SIG_ID_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  Overrun Interrupt Enable
      OIE            : Boolean := False;
      --  unspecified
      Reserved_9_15  : HAL.UInt7 := 16#0#;
      --  Generation Enable
      GE             : Boolean := False;
      --  Generation Polarity
      GPOL           : RG2CR_GPOL_Field := 16#0#;
      --  Number of Request
      GNBREQ         : RG2CR_GNBREQ_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
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

   --  DMA Request Generator 3 Control Register
   type RG3CR_Register is record
      --  Signal ID
      SIG_ID         : RG3CR_SIG_ID_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  Overrun Interrupt Enable
      OIE            : Boolean := False;
      --  unspecified
      Reserved_9_15  : HAL.UInt7 := 16#0#;
      --  Generation Enable
      GE             : Boolean := False;
      --  Generation Polarity
      GPOL           : RG3CR_GPOL_Field := 16#0#;
      --  Number of Request
      GNBREQ         : RG3CR_GNBREQ_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
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

   subtype C14CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype C14CR_SPOL_Field is HAL.UInt2;
   subtype C14CR_NBREQ_Field is HAL.UInt5;
   subtype C14CR_SYNC_ID_Field is HAL.UInt5;

   --  DMA Multiplexer Channel 10 Control register
   type C14CR_Register is record
      --  DMA request identification
      DMAREQ_ID      : C14CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Synchronization overrun interrupt enable
      SOIE           : Boolean := False;
      --  Event generation enable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : Boolean := False;
      --  Synchronization polarity
      SPOL           : C14CR_SPOL_Field := 16#0#;
      --  Number of DMA requests minus 1 to forward
      NBREQ          : C14CR_NBREQ_Field := 16#0#;
      --  Synchronization identification
      SYNC_ID        : C14CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for C14CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C15CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype C15CR_SPOL_Field is HAL.UInt2;
   subtype C15CR_NBREQ_Field is HAL.UInt5;
   subtype C15CR_SYNC_ID_Field is HAL.UInt5;

   --  DMA Multiplexer Channel 10 Control register
   type C15CR_Register is record
      --  DMA request identification
      DMAREQ_ID      : C15CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Synchronization overrun interrupt enable
      SOIE           : Boolean := False;
      --  Event generation enable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : Boolean := False;
      --  Synchronization polarity
      SPOL           : C15CR_SPOL_Field := 16#0#;
      --  Number of DMA requests minus 1 to forward
      NBREQ          : C15CR_NBREQ_Field := 16#0#;
      --  Synchronization identification
      SYNC_ID        : C15CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for C15CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
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

   --  DMA Request Generator Status Register
   type RGSR_Register is record
      --  Read-only. Generator Overrun Flag 0
      OF_k          : RGSR_OF_Field;
      --  unspecified
      Reserved_4_31 : HAL.UInt28;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RGSR_Register use record
      OF_k          at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  RGCFR_CSOF array
   type RGCFR_CSOF_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for RGCFR_CSOF
   type RGCFR_CSOF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CSOF as a value
            Val : HAL.UInt4;
         when True =>
            --  CSOF as an array
            Arr : RGCFR_CSOF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for RGCFR_CSOF_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  DMA Request Generator Clear Flag Register
   type RGCFR_Register is record
      --  Generator Clear Overrun Flag 0
      CSOF          : RGCFR_CSOF_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RGCFR_Register use record
      CSOF          at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Direct memory access Multiplexer
   type DMAMUX_Peripheral is record
      --  DMA Multiplexer Channel 0 Control register
      C0CR  : aliased C0CR_Register;
      --  DMA Multiplexer Channel 1 Control register
      C1CR  : aliased C1CR_Register;
      --  DMA Multiplexer Channel 2 Control register
      C2CR  : aliased C2CR_Register;
      --  DMA Multiplexer Channel 3 Control register
      C3CR  : aliased C3CR_Register;
      --  DMA Multiplexer Channel 4 Control register
      C4CR  : aliased C4CR_Register;
      --  DMA Multiplexer Channel 5 Control register
      C5CR  : aliased C5CR_Register;
      --  DMA Multiplexer Channel 6 Control register
      C6CR  : aliased C6CR_Register;
      --  DMA Multiplexer Channel 7 Control register
      C7CR  : aliased C7CR_Register;
      --  DMA Multiplexer Channel 8 Control register
      C8CR  : aliased C8CR_Register;
      --  DMA Multiplexer Channel 9 Control register
      C9CR  : aliased C9CR_Register;
      --  DMA Multiplexer Channel 10 Control register
      C10CR : aliased C10CR_Register;
      --  DMA Multiplexer Channel 11 Control register
      C11CR : aliased C11CR_Register;
      --  DMA Multiplexer Channel 12 Control register
      C12CR : aliased C12CR_Register;
      --  DMA Multiplexer Channel 13 Control register
      C13CR : aliased C13CR_Register;
      --  DMA Multiplexer Channel Status register
      CSR   : aliased CSR_Register;
      --  DMA Channel Clear Flag Register
      CCFR  : aliased CCFR_Register;
      --  DMA Request Generator 0 Control Register
      RG0CR : aliased RG0CR_Register;
      --  DMA Request Generator 1 Control Register
      RG1CR : aliased RG1CR_Register;
      --  DMA Request Generator 2 Control Register
      RG2CR : aliased RG2CR_Register;
      --  DMA Request Generator 3 Control Register
      RG3CR : aliased RG3CR_Register;
      --  DMA Multiplexer Channel 10 Control register
      C14CR : aliased C14CR_Register;
      --  DMA Multiplexer Channel 10 Control register
      C15CR : aliased C15CR_Register;
      --  DMA Request Generator Status Register
      RGSR  : aliased RGSR_Register;
      --  DMA Request Generator Clear Flag Register
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
      C14CR at 16#138# range 0 .. 31;
      C15CR at 16#13C# range 0 .. 31;
      RGSR  at 16#140# range 0 .. 31;
      RGCFR at 16#144# range 0 .. 31;
   end record;

   --  Direct memory access Multiplexer
   DMAMUX1_Periph : aliased DMAMUX_Peripheral
     with Import, Address => System'To_Address (16#40020800#);

   --  Direct memory access Multiplexer
   SEC_DMAMUX1_Periph : aliased DMAMUX_Peripheral
     with Import, Address => System'To_Address (16#50020800#);

end STM32_SVD.DMAMUX;
