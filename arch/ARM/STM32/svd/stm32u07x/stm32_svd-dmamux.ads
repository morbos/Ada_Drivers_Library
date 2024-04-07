--  This spec has been automatically generated from STM32U073.svd

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

   subtype DMAMUX_C0CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype DMAMUX_C0CR_SPOL_Field is HAL.UInt2;
   subtype DMAMUX_C0CR_NBREQ_Field is HAL.UInt5;
   subtype DMAMUX_C0CR_SYNC_ID_Field is HAL.UInt5;

   --  DMAMUX request line multiplexer channel 0 configuration register
   type DMAMUX_C0CR_Register is record
      --  DMA request identification Selects the input DMA request. See the
      --  DMAMUX table about assignments of multiplexer inputs to resources.
      DMAREQ_ID      : DMAMUX_C0CR_DMAREQ_ID_Field := 16#0#;
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
      --  Synchronization polarity Defines the edge polarity of the selected
      --  synchronization input:
      SPOL           : DMAMUX_C0CR_SPOL_Field := 16#0#;
      --  Number of DMA requests minus 1 to forward Defines the number of DMA
      --  requests to forward to the DMA controller after a synchronization
      --  event, and/or the number of DMA requests before an output event is
      --  generated. This field must only be written when both SE and EGE bits
      --  are low.
      NBREQ          : DMAMUX_C0CR_NBREQ_Field := 16#0#;
      --  Synchronization identification Selects the synchronization input (see
      --  Table137: DMAMUX: assignment of synchronization inputs to resources).
      SYNC_ID        : DMAMUX_C0CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_C0CR_Register use record
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

   subtype DMAMUX_C1CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype DMAMUX_C1CR_SPOL_Field is HAL.UInt2;
   subtype DMAMUX_C1CR_NBREQ_Field is HAL.UInt5;
   subtype DMAMUX_C1CR_SYNC_ID_Field is HAL.UInt5;

   --  DMAMUX request line multiplexer channel 1 configuration register
   type DMAMUX_C1CR_Register is record
      --  DMA request identification Selects the input DMA request. See the
      --  DMAMUX table about assignments of multiplexer inputs to resources.
      DMAREQ_ID      : DMAMUX_C1CR_DMAREQ_ID_Field := 16#0#;
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
      --  Synchronization polarity Defines the edge polarity of the selected
      --  synchronization input:
      SPOL           : DMAMUX_C1CR_SPOL_Field := 16#0#;
      --  Number of DMA requests minus 1 to forward Defines the number of DMA
      --  requests to forward to the DMA controller after a synchronization
      --  event, and/or the number of DMA requests before an output event is
      --  generated. This field must only be written when both SE and EGE bits
      --  are low.
      NBREQ          : DMAMUX_C1CR_NBREQ_Field := 16#0#;
      --  Synchronization identification Selects the synchronization input (see
      --  Table137: DMAMUX: assignment of synchronization inputs to resources).
      SYNC_ID        : DMAMUX_C1CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_C1CR_Register use record
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

   subtype DMAMUX_C2CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype DMAMUX_C2CR_SPOL_Field is HAL.UInt2;
   subtype DMAMUX_C2CR_NBREQ_Field is HAL.UInt5;
   subtype DMAMUX_C2CR_SYNC_ID_Field is HAL.UInt5;

   --  DMAMUX request line multiplexer channel 2 configuration register
   type DMAMUX_C2CR_Register is record
      --  DMA request identification Selects the input DMA request. See the
      --  DMAMUX table about assignments of multiplexer inputs to resources.
      DMAREQ_ID      : DMAMUX_C2CR_DMAREQ_ID_Field := 16#0#;
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
      --  Synchronization polarity Defines the edge polarity of the selected
      --  synchronization input:
      SPOL           : DMAMUX_C2CR_SPOL_Field := 16#0#;
      --  Number of DMA requests minus 1 to forward Defines the number of DMA
      --  requests to forward to the DMA controller after a synchronization
      --  event, and/or the number of DMA requests before an output event is
      --  generated. This field must only be written when both SE and EGE bits
      --  are low.
      NBREQ          : DMAMUX_C2CR_NBREQ_Field := 16#0#;
      --  Synchronization identification Selects the synchronization input (see
      --  Table137: DMAMUX: assignment of synchronization inputs to resources).
      SYNC_ID        : DMAMUX_C2CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_C2CR_Register use record
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

   subtype DMAMUX_C3CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype DMAMUX_C3CR_SPOL_Field is HAL.UInt2;
   subtype DMAMUX_C3CR_NBREQ_Field is HAL.UInt5;
   subtype DMAMUX_C3CR_SYNC_ID_Field is HAL.UInt5;

   --  DMAMUX request line multiplexer channel 3 configuration register
   type DMAMUX_C3CR_Register is record
      --  DMA request identification Selects the input DMA request. See the
      --  DMAMUX table about assignments of multiplexer inputs to resources.
      DMAREQ_ID      : DMAMUX_C3CR_DMAREQ_ID_Field := 16#0#;
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
      --  Synchronization polarity Defines the edge polarity of the selected
      --  synchronization input:
      SPOL           : DMAMUX_C3CR_SPOL_Field := 16#0#;
      --  Number of DMA requests minus 1 to forward Defines the number of DMA
      --  requests to forward to the DMA controller after a synchronization
      --  event, and/or the number of DMA requests before an output event is
      --  generated. This field must only be written when both SE and EGE bits
      --  are low.
      NBREQ          : DMAMUX_C3CR_NBREQ_Field := 16#0#;
      --  Synchronization identification Selects the synchronization input (see
      --  Table137: DMAMUX: assignment of synchronization inputs to resources).
      SYNC_ID        : DMAMUX_C3CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_C3CR_Register use record
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

   subtype DMAMUX_C4CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype DMAMUX_C4CR_SPOL_Field is HAL.UInt2;
   subtype DMAMUX_C4CR_NBREQ_Field is HAL.UInt5;
   subtype DMAMUX_C4CR_SYNC_ID_Field is HAL.UInt5;

   --  DMAMUX request line multiplexer channel 4 configuration register
   type DMAMUX_C4CR_Register is record
      --  DMA request identification Selects the input DMA request. See the
      --  DMAMUX table about assignments of multiplexer inputs to resources.
      DMAREQ_ID      : DMAMUX_C4CR_DMAREQ_ID_Field := 16#0#;
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
      --  Synchronization polarity Defines the edge polarity of the selected
      --  synchronization input:
      SPOL           : DMAMUX_C4CR_SPOL_Field := 16#0#;
      --  Number of DMA requests minus 1 to forward Defines the number of DMA
      --  requests to forward to the DMA controller after a synchronization
      --  event, and/or the number of DMA requests before an output event is
      --  generated. This field must only be written when both SE and EGE bits
      --  are low.
      NBREQ          : DMAMUX_C4CR_NBREQ_Field := 16#0#;
      --  Synchronization identification Selects the synchronization input (see
      --  Table137: DMAMUX: assignment of synchronization inputs to resources).
      SYNC_ID        : DMAMUX_C4CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_C4CR_Register use record
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

   subtype DMAMUX_C5CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype DMAMUX_C5CR_SPOL_Field is HAL.UInt2;
   subtype DMAMUX_C5CR_NBREQ_Field is HAL.UInt5;
   subtype DMAMUX_C5CR_SYNC_ID_Field is HAL.UInt5;

   --  DMAMUX request line multiplexer channel 5 configuration register
   type DMAMUX_C5CR_Register is record
      --  DMA request identification Selects the input DMA request. See the
      --  DMAMUX table about assignments of multiplexer inputs to resources.
      DMAREQ_ID      : DMAMUX_C5CR_DMAREQ_ID_Field := 16#0#;
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
      --  Synchronization polarity Defines the edge polarity of the selected
      --  synchronization input:
      SPOL           : DMAMUX_C5CR_SPOL_Field := 16#0#;
      --  Number of DMA requests minus 1 to forward Defines the number of DMA
      --  requests to forward to the DMA controller after a synchronization
      --  event, and/or the number of DMA requests before an output event is
      --  generated. This field must only be written when both SE and EGE bits
      --  are low.
      NBREQ          : DMAMUX_C5CR_NBREQ_Field := 16#0#;
      --  Synchronization identification Selects the synchronization input (see
      --  Table137: DMAMUX: assignment of synchronization inputs to resources).
      SYNC_ID        : DMAMUX_C5CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_C5CR_Register use record
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

   subtype DMAMUX_C6CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype DMAMUX_C6CR_SPOL_Field is HAL.UInt2;
   subtype DMAMUX_C6CR_NBREQ_Field is HAL.UInt5;
   subtype DMAMUX_C6CR_SYNC_ID_Field is HAL.UInt5;

   --  DMAMUX request line multiplexer channel 6 configuration register
   type DMAMUX_C6CR_Register is record
      --  DMA request identification Selects the input DMA request. See the
      --  DMAMUX table about assignments of multiplexer inputs to resources.
      DMAREQ_ID      : DMAMUX_C6CR_DMAREQ_ID_Field := 16#0#;
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
      --  Synchronization polarity Defines the edge polarity of the selected
      --  synchronization input:
      SPOL           : DMAMUX_C6CR_SPOL_Field := 16#0#;
      --  Number of DMA requests minus 1 to forward Defines the number of DMA
      --  requests to forward to the DMA controller after a synchronization
      --  event, and/or the number of DMA requests before an output event is
      --  generated. This field must only be written when both SE and EGE bits
      --  are low.
      NBREQ          : DMAMUX_C6CR_NBREQ_Field := 16#0#;
      --  Synchronization identification Selects the synchronization input (see
      --  Table137: DMAMUX: assignment of synchronization inputs to resources).
      SYNC_ID        : DMAMUX_C6CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_C6CR_Register use record
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

   subtype DMAMUX_C7CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype DMAMUX_C7CR_SPOL_Field is HAL.UInt2;
   subtype DMAMUX_C7CR_NBREQ_Field is HAL.UInt5;
   subtype DMAMUX_C7CR_SYNC_ID_Field is HAL.UInt5;

   --  DMAMUX request line multiplexer channel 7 configuration register
   type DMAMUX_C7CR_Register is record
      --  DMA request identification Selects the input DMA request. See the
      --  DMAMUX table about assignments of multiplexer inputs to resources.
      DMAREQ_ID      : DMAMUX_C7CR_DMAREQ_ID_Field := 16#0#;
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
      --  Synchronization polarity Defines the edge polarity of the selected
      --  synchronization input:
      SPOL           : DMAMUX_C7CR_SPOL_Field := 16#0#;
      --  Number of DMA requests minus 1 to forward Defines the number of DMA
      --  requests to forward to the DMA controller after a synchronization
      --  event, and/or the number of DMA requests before an output event is
      --  generated. This field must only be written when both SE and EGE bits
      --  are low.
      NBREQ          : DMAMUX_C7CR_NBREQ_Field := 16#0#;
      --  Synchronization identification Selects the synchronization input (see
      --  Table137: DMAMUX: assignment of synchronization inputs to resources).
      SYNC_ID        : DMAMUX_C7CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_C7CR_Register use record
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

   subtype DMAMUX_C8CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype DMAMUX_C8CR_SPOL_Field is HAL.UInt2;
   subtype DMAMUX_C8CR_NBREQ_Field is HAL.UInt5;
   subtype DMAMUX_C8CR_SYNC_ID_Field is HAL.UInt5;

   --  DMAMUX request line multiplexer channel 8 configuration register
   type DMAMUX_C8CR_Register is record
      --  DMA request identification Selects the input DMA request. See the
      --  DMAMUX table about assignments of multiplexer inputs to resources.
      DMAREQ_ID      : DMAMUX_C8CR_DMAREQ_ID_Field := 16#0#;
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
      --  Synchronization polarity Defines the edge polarity of the selected
      --  synchronization input:
      SPOL           : DMAMUX_C8CR_SPOL_Field := 16#0#;
      --  Number of DMA requests minus 1 to forward Defines the number of DMA
      --  requests to forward to the DMA controller after a synchronization
      --  event, and/or the number of DMA requests before an output event is
      --  generated. This field must only be written when both SE and EGE bits
      --  are low.
      NBREQ          : DMAMUX_C8CR_NBREQ_Field := 16#0#;
      --  Synchronization identification Selects the synchronization input (see
      --  Table137: DMAMUX: assignment of synchronization inputs to resources).
      SYNC_ID        : DMAMUX_C8CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_C8CR_Register use record
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

   subtype DMAMUX_C9CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype DMAMUX_C9CR_SPOL_Field is HAL.UInt2;
   subtype DMAMUX_C9CR_NBREQ_Field is HAL.UInt5;
   subtype DMAMUX_C9CR_SYNC_ID_Field is HAL.UInt5;

   --  DMAMUX request line multiplexer channel 9 configuration register
   type DMAMUX_C9CR_Register is record
      --  DMA request identification Selects the input DMA request. See the
      --  DMAMUX table about assignments of multiplexer inputs to resources.
      DMAREQ_ID      : DMAMUX_C9CR_DMAREQ_ID_Field := 16#0#;
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
      --  Synchronization polarity Defines the edge polarity of the selected
      --  synchronization input:
      SPOL           : DMAMUX_C9CR_SPOL_Field := 16#0#;
      --  Number of DMA requests minus 1 to forward Defines the number of DMA
      --  requests to forward to the DMA controller after a synchronization
      --  event, and/or the number of DMA requests before an output event is
      --  generated. This field must only be written when both SE and EGE bits
      --  are low.
      NBREQ          : DMAMUX_C9CR_NBREQ_Field := 16#0#;
      --  Synchronization identification Selects the synchronization input (see
      --  Table137: DMAMUX: assignment of synchronization inputs to resources).
      SYNC_ID        : DMAMUX_C9CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_C9CR_Register use record
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

   subtype DMAMUX_C10CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype DMAMUX_C10CR_SPOL_Field is HAL.UInt2;
   subtype DMAMUX_C10CR_NBREQ_Field is HAL.UInt5;
   subtype DMAMUX_C10CR_SYNC_ID_Field is HAL.UInt5;

   --  DMAMUX request line multiplexer channel 10 configuration register
   type DMAMUX_C10CR_Register is record
      --  DMA request identification Selects the input DMA request. See the
      --  DMAMUX table about assignments of multiplexer inputs to resources.
      DMAREQ_ID      : DMAMUX_C10CR_DMAREQ_ID_Field := 16#0#;
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
      --  Synchronization polarity Defines the edge polarity of the selected
      --  synchronization input:
      SPOL           : DMAMUX_C10CR_SPOL_Field := 16#0#;
      --  Number of DMA requests minus 1 to forward Defines the number of DMA
      --  requests to forward to the DMA controller after a synchronization
      --  event, and/or the number of DMA requests before an output event is
      --  generated. This field must only be written when both SE and EGE bits
      --  are low.
      NBREQ          : DMAMUX_C10CR_NBREQ_Field := 16#0#;
      --  Synchronization identification Selects the synchronization input (see
      --  Table137: DMAMUX: assignment of synchronization inputs to resources).
      SYNC_ID        : DMAMUX_C10CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_C10CR_Register use record
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

   subtype DMAMUX_C11CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype DMAMUX_C11CR_SPOL_Field is HAL.UInt2;
   subtype DMAMUX_C11CR_NBREQ_Field is HAL.UInt5;
   subtype DMAMUX_C11CR_SYNC_ID_Field is HAL.UInt5;

   --  DMAMUX request line multiplexer channel 11 configuration register
   type DMAMUX_C11CR_Register is record
      --  DMA request identification Selects the input DMA request. See the
      --  DMAMUX table about assignments of multiplexer inputs to resources.
      DMAREQ_ID      : DMAMUX_C11CR_DMAREQ_ID_Field := 16#0#;
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
      --  Synchronization polarity Defines the edge polarity of the selected
      --  synchronization input:
      SPOL           : DMAMUX_C11CR_SPOL_Field := 16#0#;
      --  Number of DMA requests minus 1 to forward Defines the number of DMA
      --  requests to forward to the DMA controller after a synchronization
      --  event, and/or the number of DMA requests before an output event is
      --  generated. This field must only be written when both SE and EGE bits
      --  are low.
      NBREQ          : DMAMUX_C11CR_NBREQ_Field := 16#0#;
      --  Synchronization identification Selects the synchronization input (see
      --  Table137: DMAMUX: assignment of synchronization inputs to resources).
      SYNC_ID        : DMAMUX_C11CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_C11CR_Register use record
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

   --  DMAMUX_CSR_SOF array
   type DMAMUX_CSR_SOF_Field_Array is array (0 .. 11) of Boolean
     with Component_Size => 1, Size => 12;

   --  Type definition for DMAMUX_CSR_SOF
   type DMAMUX_CSR_SOF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SOF as a value
            Val : HAL.UInt12;
         when True =>
            --  SOF as an array
            Arr : DMAMUX_CSR_SOF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 12;

   for DMAMUX_CSR_SOF_Field use record
      Val at 0 range 0 .. 11;
      Arr at 0 range 0 .. 11;
   end record;

   --  DMAMUX request line multiplexer interrupt channel status register
   type DMAMUX_CSR_Register is record
      --  Read-only. Synchronization overrun event flag The flag is set when a
      --  synchronization event occurs on a DMA request line multiplexer
      --  channel x, while the DMA request counter value is lower than NBREQ.
      --  The flag is cleared by writing 1 to the corresponding CSOFx bit in
      --  DMAMUX_CFR register.
      SOF            : DMAMUX_CSR_SOF_Field;
      --  unspecified
      Reserved_12_31 : HAL.UInt20;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_CSR_Register use record
      SOF            at 0 range 0 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   --  DMAMUX_CFR_CSOF array
   type DMAMUX_CFR_CSOF_Field_Array is array (0 .. 11) of Boolean
     with Component_Size => 1, Size => 12;

   --  Type definition for DMAMUX_CFR_CSOF
   type DMAMUX_CFR_CSOF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CSOF as a value
            Val : HAL.UInt12;
         when True =>
            --  CSOF as an array
            Arr : DMAMUX_CFR_CSOF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 12;

   for DMAMUX_CFR_CSOF_Field use record
      Val at 0 range 0 .. 11;
      Arr at 0 range 0 .. 11;
   end record;

   --  DMAMUX request line multiplexer interrupt clear flag register
   type DMAMUX_CFR_Register is record
      --  Write-only. Clear synchronization overrun event flag Writing 1 in
      --  each bit clears the corresponding overrun flag SOFx in the DMAMUX_CSR
      --  register.
      CSOF           : DMAMUX_CFR_CSOF_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_CFR_Register use record
      CSOF           at 0 range 0 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype DMAMUX_RG0CR_SIG_ID_Field is HAL.UInt5;
   subtype DMAMUX_RG0CR_GPOL_Field is HAL.UInt2;
   subtype DMAMUX_RG0CR_GNBREQ_Field is HAL.UInt5;

   --  DMAMUX request generator channel 0 configuration register
   type DMAMUX_RG0CR_Register is record
      --  Signal identification Selects the DMA request trigger input used for
      --  the channel x of the DMA request generator
      SIG_ID         : DMAMUX_RG0CR_SIG_ID_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  Trigger overrun interrupt enable
      OIE            : Boolean := False;
      --  unspecified
      Reserved_9_15  : HAL.UInt7 := 16#0#;
      --  DMA request generator channel x enable
      GE             : Boolean := False;
      --  DMA request generator trigger polarity Defines the edge polarity of
      --  the selected trigger input
      GPOL           : DMAMUX_RG0CR_GPOL_Field := 16#0#;
      --  Number of DMA requests to be generated (minus 1) Defines the number
      --  of DMA requests to be generated after a trigger event. The actual
      --  number of generated DMA requests is GNBREQ +1. Note: This field must
      --  be written only when GE bit is disabled.
      GNBREQ         : DMAMUX_RG0CR_GNBREQ_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_RG0CR_Register use record
      SIG_ID         at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      OIE            at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      GE             at 0 range 16 .. 16;
      GPOL           at 0 range 17 .. 18;
      GNBREQ         at 0 range 19 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype DMAMUX_RG1CR_SIG_ID_Field is HAL.UInt5;
   subtype DMAMUX_RG1CR_GPOL_Field is HAL.UInt2;
   subtype DMAMUX_RG1CR_GNBREQ_Field is HAL.UInt5;

   --  DMAMUX request generator channel 1 configuration register
   type DMAMUX_RG1CR_Register is record
      --  Signal identification Selects the DMA request trigger input used for
      --  the channel x of the DMA request generator
      SIG_ID         : DMAMUX_RG1CR_SIG_ID_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  Trigger overrun interrupt enable
      OIE            : Boolean := False;
      --  unspecified
      Reserved_9_15  : HAL.UInt7 := 16#0#;
      --  DMA request generator channel x enable
      GE             : Boolean := False;
      --  DMA request generator trigger polarity Defines the edge polarity of
      --  the selected trigger input
      GPOL           : DMAMUX_RG1CR_GPOL_Field := 16#0#;
      --  Number of DMA requests to be generated (minus 1) Defines the number
      --  of DMA requests to be generated after a trigger event. The actual
      --  number of generated DMA requests is GNBREQ +1. Note: This field must
      --  be written only when GE bit is disabled.
      GNBREQ         : DMAMUX_RG1CR_GNBREQ_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_RG1CR_Register use record
      SIG_ID         at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      OIE            at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      GE             at 0 range 16 .. 16;
      GPOL           at 0 range 17 .. 18;
      GNBREQ         at 0 range 19 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype DMAMUX_RG2CR_SIG_ID_Field is HAL.UInt5;
   subtype DMAMUX_RG2CR_GPOL_Field is HAL.UInt2;
   subtype DMAMUX_RG2CR_GNBREQ_Field is HAL.UInt5;

   --  DMAMUX request generator channel 2 configuration register
   type DMAMUX_RG2CR_Register is record
      --  Signal identification Selects the DMA request trigger input used for
      --  the channel x of the DMA request generator
      SIG_ID         : DMAMUX_RG2CR_SIG_ID_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  Trigger overrun interrupt enable
      OIE            : Boolean := False;
      --  unspecified
      Reserved_9_15  : HAL.UInt7 := 16#0#;
      --  DMA request generator channel x enable
      GE             : Boolean := False;
      --  DMA request generator trigger polarity Defines the edge polarity of
      --  the selected trigger input
      GPOL           : DMAMUX_RG2CR_GPOL_Field := 16#0#;
      --  Number of DMA requests to be generated (minus 1) Defines the number
      --  of DMA requests to be generated after a trigger event. The actual
      --  number of generated DMA requests is GNBREQ +1. Note: This field must
      --  be written only when GE bit is disabled.
      GNBREQ         : DMAMUX_RG2CR_GNBREQ_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_RG2CR_Register use record
      SIG_ID         at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      OIE            at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      GE             at 0 range 16 .. 16;
      GPOL           at 0 range 17 .. 18;
      GNBREQ         at 0 range 19 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype DMAMUX_RG3CR_SIG_ID_Field is HAL.UInt5;
   subtype DMAMUX_RG3CR_GPOL_Field is HAL.UInt2;
   subtype DMAMUX_RG3CR_GNBREQ_Field is HAL.UInt5;

   --  DMAMUX request generator channel 3 configuration register
   type DMAMUX_RG3CR_Register is record
      --  Signal identification Selects the DMA request trigger input used for
      --  the channel x of the DMA request generator
      SIG_ID         : DMAMUX_RG3CR_SIG_ID_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  Trigger overrun interrupt enable
      OIE            : Boolean := False;
      --  unspecified
      Reserved_9_15  : HAL.UInt7 := 16#0#;
      --  DMA request generator channel x enable
      GE             : Boolean := False;
      --  DMA request generator trigger polarity Defines the edge polarity of
      --  the selected trigger input
      GPOL           : DMAMUX_RG3CR_GPOL_Field := 16#0#;
      --  Number of DMA requests to be generated (minus 1) Defines the number
      --  of DMA requests to be generated after a trigger event. The actual
      --  number of generated DMA requests is GNBREQ +1. Note: This field must
      --  be written only when GE bit is disabled.
      GNBREQ         : DMAMUX_RG3CR_GNBREQ_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_RG3CR_Register use record
      SIG_ID         at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      OIE            at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      GE             at 0 range 16 .. 16;
      GPOL           at 0 range 17 .. 18;
      GNBREQ         at 0 range 19 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  DMAMUX_RGSR_OF array
   type DMAMUX_RGSR_OF_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for DMAMUX_RGSR_OF
   type DMAMUX_RGSR_OF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  OF as a value
            Val : HAL.UInt4;
         when True =>
            --  OF as an array
            Arr : DMAMUX_RGSR_OF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for DMAMUX_RGSR_OF_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  DMAMUX request generator interrupt status register
   type DMAMUX_RGSR_Register is record
      --  Read-only. Trigger overrun event flag The flag is set when a new
      --  trigger event occurs on DMA request generator channel x, before the
      --  request counter underrun (the internal request counter programmed via
      --  the GNBREQ field of the DMAMUX_RGxCR register). The flag is cleared
      --  by writing 1 to the corresponding COFx bit in the DMAMUX_RGCFR
      --  register.
      OF_k          : DMAMUX_RGSR_OF_Field;
      --  unspecified
      Reserved_4_31 : HAL.UInt28;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_RGSR_Register use record
      OF_k          at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  DMAMUX_RGCFR_COF array
   type DMAMUX_RGCFR_COF_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for DMAMUX_RGCFR_COF
   type DMAMUX_RGCFR_COF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  COF as a value
            Val : HAL.UInt4;
         when True =>
            --  COF as an array
            Arr : DMAMUX_RGCFR_COF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for DMAMUX_RGCFR_COF_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  DMAMUX request generator interrupt clear flag register
   type DMAMUX_RGCFR_Register is record
      --  Write-only. Clear trigger overrun event flag Writing 1 in each bit
      --  clears the corresponding overrun flag OFx in the DMAMUX_RGSR
      --  register.
      COF           : DMAMUX_RGCFR_COF_Field :=
                       (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_RGCFR_Register use record
      COF           at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  DMAMUX address block description
   type DMAMUX_Peripheral is record
      --  DMAMUX request line multiplexer channel 0 configuration register
      DMAMUX_C0CR  : aliased DMAMUX_C0CR_Register;
      --  DMAMUX request line multiplexer channel 1 configuration register
      DMAMUX_C1CR  : aliased DMAMUX_C1CR_Register;
      --  DMAMUX request line multiplexer channel 2 configuration register
      DMAMUX_C2CR  : aliased DMAMUX_C2CR_Register;
      --  DMAMUX request line multiplexer channel 3 configuration register
      DMAMUX_C3CR  : aliased DMAMUX_C3CR_Register;
      --  DMAMUX request line multiplexer channel 4 configuration register
      DMAMUX_C4CR  : aliased DMAMUX_C4CR_Register;
      --  DMAMUX request line multiplexer channel 5 configuration register
      DMAMUX_C5CR  : aliased DMAMUX_C5CR_Register;
      --  DMAMUX request line multiplexer channel 6 configuration register
      DMAMUX_C6CR  : aliased DMAMUX_C6CR_Register;
      --  DMAMUX request line multiplexer channel 7 configuration register
      DMAMUX_C7CR  : aliased DMAMUX_C7CR_Register;
      --  DMAMUX request line multiplexer channel 8 configuration register
      DMAMUX_C8CR  : aliased DMAMUX_C8CR_Register;
      --  DMAMUX request line multiplexer channel 9 configuration register
      DMAMUX_C9CR  : aliased DMAMUX_C9CR_Register;
      --  DMAMUX request line multiplexer channel 10 configuration register
      DMAMUX_C10CR : aliased DMAMUX_C10CR_Register;
      --  DMAMUX request line multiplexer channel 11 configuration register
      DMAMUX_C11CR : aliased DMAMUX_C11CR_Register;
      --  DMAMUX request line multiplexer interrupt channel status register
      DMAMUX_CSR   : aliased DMAMUX_CSR_Register;
      --  DMAMUX request line multiplexer interrupt clear flag register
      DMAMUX_CFR   : aliased DMAMUX_CFR_Register;
      --  DMAMUX request generator channel 0 configuration register
      DMAMUX_RG0CR : aliased DMAMUX_RG0CR_Register;
      --  DMAMUX request generator channel 1 configuration register
      DMAMUX_RG1CR : aliased DMAMUX_RG1CR_Register;
      --  DMAMUX request generator channel 2 configuration register
      DMAMUX_RG2CR : aliased DMAMUX_RG2CR_Register;
      --  DMAMUX request generator channel 3 configuration register
      DMAMUX_RG3CR : aliased DMAMUX_RG3CR_Register;
      --  DMAMUX request generator interrupt status register
      DMAMUX_RGSR  : aliased DMAMUX_RGSR_Register;
      --  DMAMUX request generator interrupt clear flag register
      DMAMUX_RGCFR : aliased DMAMUX_RGCFR_Register;
   end record
     with Volatile;

   for DMAMUX_Peripheral use record
      DMAMUX_C0CR  at 16#0# range 0 .. 31;
      DMAMUX_C1CR  at 16#4# range 0 .. 31;
      DMAMUX_C2CR  at 16#8# range 0 .. 31;
      DMAMUX_C3CR  at 16#C# range 0 .. 31;
      DMAMUX_C4CR  at 16#10# range 0 .. 31;
      DMAMUX_C5CR  at 16#14# range 0 .. 31;
      DMAMUX_C6CR  at 16#18# range 0 .. 31;
      DMAMUX_C7CR  at 16#1C# range 0 .. 31;
      DMAMUX_C8CR  at 16#20# range 0 .. 31;
      DMAMUX_C9CR  at 16#24# range 0 .. 31;
      DMAMUX_C10CR at 16#28# range 0 .. 31;
      DMAMUX_C11CR at 16#2C# range 0 .. 31;
      DMAMUX_CSR   at 16#80# range 0 .. 31;
      DMAMUX_CFR   at 16#84# range 0 .. 31;
      DMAMUX_RG0CR at 16#100# range 0 .. 31;
      DMAMUX_RG1CR at 16#104# range 0 .. 31;
      DMAMUX_RG2CR at 16#108# range 0 .. 31;
      DMAMUX_RG3CR at 16#10C# range 0 .. 31;
      DMAMUX_RGSR  at 16#140# range 0 .. 31;
      DMAMUX_RGCFR at 16#144# range 0 .. 31;
   end record;

   --  DMAMUX address block description
   DMAMUX_Periph : aliased DMAMUX_Peripheral
     with Import, Address => DMAMUX_Base;

end STM32_SVD.DMAMUX;
