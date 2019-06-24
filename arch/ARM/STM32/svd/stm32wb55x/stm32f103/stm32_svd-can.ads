--  This spec has been automatically generated from STM32F103.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.CAN is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  CAN_MCR
   type CAN_MCR_Register is record
      --  INRQ
      INRQ           : Boolean := False;
      --  SLEEP
      SLEEP          : Boolean := False;
      --  TXFP
      TXFP           : Boolean := False;
      --  RFLM
      RFLM           : Boolean := False;
      --  NART
      NART           : Boolean := False;
      --  AWUM
      AWUM           : Boolean := False;
      --  ABOM
      ABOM           : Boolean := False;
      --  TTCM
      TTCM           : Boolean := False;
      --  unspecified
      Reserved_8_14  : HAL.UInt7 := 16#0#;
      --  RESET
      RESET          : Boolean := False;
      --  DBF
      DBF            : Boolean := False;
      --  unspecified
      Reserved_17_31 : HAL.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CAN_MCR_Register use record
      INRQ           at 0 range 0 .. 0;
      SLEEP          at 0 range 1 .. 1;
      TXFP           at 0 range 2 .. 2;
      RFLM           at 0 range 3 .. 3;
      NART           at 0 range 4 .. 4;
      AWUM           at 0 range 5 .. 5;
      ABOM           at 0 range 6 .. 6;
      TTCM           at 0 range 7 .. 7;
      Reserved_8_14  at 0 range 8 .. 14;
      RESET          at 0 range 15 .. 15;
      DBF            at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   --  CAN_MSR
   type CAN_MSR_Register is record
      --  Read-only. INAK
      INAK           : Boolean := False;
      --  Read-only. SLAK
      SLAK           : Boolean := False;
      --  ERRI
      ERRI           : Boolean := False;
      --  WKUI
      WKUI           : Boolean := False;
      --  SLAKI
      SLAKI          : Boolean := False;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  Read-only. TXM
      TXM            : Boolean := False;
      --  Read-only. RXM
      RXM            : Boolean := False;
      --  Read-only. SAMP
      SAMP           : Boolean := False;
      --  Read-only. RX
      RX             : Boolean := False;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CAN_MSR_Register use record
      INAK           at 0 range 0 .. 0;
      SLAK           at 0 range 1 .. 1;
      ERRI           at 0 range 2 .. 2;
      WKUI           at 0 range 3 .. 3;
      SLAKI          at 0 range 4 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      TXM            at 0 range 8 .. 8;
      RXM            at 0 range 9 .. 9;
      SAMP           at 0 range 10 .. 10;
      RX             at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype CAN_TSR_CODE_Field is HAL.UInt2;

   --  CAN_TSR_TME array
   type CAN_TSR_TME_Field_Array is array (0 .. 2) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for CAN_TSR_TME
   type CAN_TSR_TME_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TME as a value
            Val : HAL.UInt3;
         when True =>
            --  TME as an array
            Arr : CAN_TSR_TME_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for CAN_TSR_TME_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  CAN_TSR_LOW array
   type CAN_TSR_LOW_Field_Array is array (0 .. 2) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for CAN_TSR_LOW
   type CAN_TSR_LOW_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  LOW as a value
            Val : HAL.UInt3;
         when True =>
            --  LOW as an array
            Arr : CAN_TSR_LOW_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for CAN_TSR_LOW_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  CAN_TSR
   type CAN_TSR_Register is record
      --  RQCP0
      RQCP0          : Boolean := False;
      --  TXOK0
      TXOK0          : Boolean := False;
      --  ALST0
      ALST0          : Boolean := False;
      --  TERR0
      TERR0          : Boolean := False;
      --  unspecified
      Reserved_4_6   : HAL.UInt3 := 16#0#;
      --  ABRQ0
      ABRQ0          : Boolean := False;
      --  RQCP1
      RQCP1          : Boolean := False;
      --  TXOK1
      TXOK1          : Boolean := False;
      --  ALST1
      ALST1          : Boolean := False;
      --  TERR1
      TERR1          : Boolean := False;
      --  unspecified
      Reserved_12_14 : HAL.UInt3 := 16#0#;
      --  ABRQ1
      ABRQ1          : Boolean := False;
      --  RQCP2
      RQCP2          : Boolean := False;
      --  TXOK2
      TXOK2          : Boolean := False;
      --  ALST2
      ALST2          : Boolean := False;
      --  TERR2
      TERR2          : Boolean := False;
      --  unspecified
      Reserved_20_22 : HAL.UInt3 := 16#0#;
      --  ABRQ2
      ABRQ2          : Boolean := False;
      --  Read-only. CODE
      CODE           : CAN_TSR_CODE_Field := 16#0#;
      --  Read-only. Lowest priority flag for mailbox 0
      TME            : CAN_TSR_TME_Field := (As_Array => False, Val => 16#0#);
      --  Read-only. Lowest priority flag for mailbox 0
      LOW            : CAN_TSR_LOW_Field := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CAN_TSR_Register use record
      RQCP0          at 0 range 0 .. 0;
      TXOK0          at 0 range 1 .. 1;
      ALST0          at 0 range 2 .. 2;
      TERR0          at 0 range 3 .. 3;
      Reserved_4_6   at 0 range 4 .. 6;
      ABRQ0          at 0 range 7 .. 7;
      RQCP1          at 0 range 8 .. 8;
      TXOK1          at 0 range 9 .. 9;
      ALST1          at 0 range 10 .. 10;
      TERR1          at 0 range 11 .. 11;
      Reserved_12_14 at 0 range 12 .. 14;
      ABRQ1          at 0 range 15 .. 15;
      RQCP2          at 0 range 16 .. 16;
      TXOK2          at 0 range 17 .. 17;
      ALST2          at 0 range 18 .. 18;
      TERR2          at 0 range 19 .. 19;
      Reserved_20_22 at 0 range 20 .. 22;
      ABRQ2          at 0 range 23 .. 23;
      CODE           at 0 range 24 .. 25;
      TME            at 0 range 26 .. 28;
      LOW            at 0 range 29 .. 31;
   end record;

   subtype CAN_RF0R_FMP0_Field is HAL.UInt2;

   --  CAN_RF0R
   type CAN_RF0R_Register is record
      --  Read-only. FMP0
      FMP0          : CAN_RF0R_FMP0_Field := 16#0#;
      --  unspecified
      Reserved_2_2  : HAL.Bit := 16#0#;
      --  FULL0
      FULL0         : Boolean := False;
      --  FOVR0
      FOVR0         : Boolean := False;
      --  RFOM0
      RFOM0         : Boolean := False;
      --  unspecified
      Reserved_6_31 : HAL.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CAN_RF0R_Register use record
      FMP0          at 0 range 0 .. 1;
      Reserved_2_2  at 0 range 2 .. 2;
      FULL0         at 0 range 3 .. 3;
      FOVR0         at 0 range 4 .. 4;
      RFOM0         at 0 range 5 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   subtype CAN_RF1R_FMP1_Field is HAL.UInt2;

   --  CAN_RF1R
   type CAN_RF1R_Register is record
      --  Read-only. FMP1
      FMP1          : CAN_RF1R_FMP1_Field := 16#0#;
      --  unspecified
      Reserved_2_2  : HAL.Bit := 16#0#;
      --  FULL1
      FULL1         : Boolean := False;
      --  FOVR1
      FOVR1         : Boolean := False;
      --  RFOM1
      RFOM1         : Boolean := False;
      --  unspecified
      Reserved_6_31 : HAL.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CAN_RF1R_Register use record
      FMP1          at 0 range 0 .. 1;
      Reserved_2_2  at 0 range 2 .. 2;
      FULL1         at 0 range 3 .. 3;
      FOVR1         at 0 range 4 .. 4;
      RFOM1         at 0 range 5 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   --  CAN_IER
   type CAN_IER_Register is record
      --  TMEIE
      TMEIE          : Boolean := False;
      --  FMPIE0
      FMPIE0         : Boolean := False;
      --  FFIE0
      FFIE0          : Boolean := False;
      --  FOVIE0
      FOVIE0         : Boolean := False;
      --  FMPIE1
      FMPIE1         : Boolean := False;
      --  FFIE1
      FFIE1          : Boolean := False;
      --  FOVIE1
      FOVIE1         : Boolean := False;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  EWGIE
      EWGIE          : Boolean := False;
      --  EPVIE
      EPVIE          : Boolean := False;
      --  BOFIE
      BOFIE          : Boolean := False;
      --  LECIE
      LECIE          : Boolean := False;
      --  unspecified
      Reserved_12_14 : HAL.UInt3 := 16#0#;
      --  ERRIE
      ERRIE          : Boolean := False;
      --  WKUIE
      WKUIE          : Boolean := False;
      --  SLKIE
      SLKIE          : Boolean := False;
      --  unspecified
      Reserved_18_31 : HAL.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CAN_IER_Register use record
      TMEIE          at 0 range 0 .. 0;
      FMPIE0         at 0 range 1 .. 1;
      FFIE0          at 0 range 2 .. 2;
      FOVIE0         at 0 range 3 .. 3;
      FMPIE1         at 0 range 4 .. 4;
      FFIE1          at 0 range 5 .. 5;
      FOVIE1         at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      EWGIE          at 0 range 8 .. 8;
      EPVIE          at 0 range 9 .. 9;
      BOFIE          at 0 range 10 .. 10;
      LECIE          at 0 range 11 .. 11;
      Reserved_12_14 at 0 range 12 .. 14;
      ERRIE          at 0 range 15 .. 15;
      WKUIE          at 0 range 16 .. 16;
      SLKIE          at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   subtype CAN_ESR_LEC_Field is HAL.UInt3;
   subtype CAN_ESR_TEC_Field is HAL.UInt8;
   subtype CAN_ESR_REC_Field is HAL.UInt8;

   --  CAN_ESR
   type CAN_ESR_Register is record
      --  Read-only. EWGF
      EWGF          : Boolean := False;
      --  Read-only. EPVF
      EPVF          : Boolean := False;
      --  Read-only. BOFF
      BOFF          : Boolean := False;
      --  unspecified
      Reserved_3_3  : HAL.Bit := 16#0#;
      --  LEC
      LEC           : CAN_ESR_LEC_Field := 16#0#;
      --  unspecified
      Reserved_7_15 : HAL.UInt9 := 16#0#;
      --  Read-only. TEC
      TEC           : CAN_ESR_TEC_Field := 16#0#;
      --  Read-only. REC
      REC           : CAN_ESR_REC_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CAN_ESR_Register use record
      EWGF          at 0 range 0 .. 0;
      EPVF          at 0 range 1 .. 1;
      BOFF          at 0 range 2 .. 2;
      Reserved_3_3  at 0 range 3 .. 3;
      LEC           at 0 range 4 .. 6;
      Reserved_7_15 at 0 range 7 .. 15;
      TEC           at 0 range 16 .. 23;
      REC           at 0 range 24 .. 31;
   end record;

   subtype CAN_BTR_BRP_Field is HAL.UInt10;
   subtype CAN_BTR_TS1_Field is HAL.UInt4;
   subtype CAN_BTR_TS2_Field is HAL.UInt3;
   subtype CAN_BTR_SJW_Field is HAL.UInt2;

   --  CAN_BTR
   type CAN_BTR_Register is record
      --  BRP
      BRP            : CAN_BTR_BRP_Field := 16#0#;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  TS1
      TS1            : CAN_BTR_TS1_Field := 16#0#;
      --  TS2
      TS2            : CAN_BTR_TS2_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : HAL.Bit := 16#0#;
      --  SJW
      SJW            : CAN_BTR_SJW_Field := 16#0#;
      --  unspecified
      Reserved_26_29 : HAL.UInt4 := 16#0#;
      --  LBKM
      LBKM           : Boolean := False;
      --  SILM
      SILM           : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CAN_BTR_Register use record
      BRP            at 0 range 0 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      TS1            at 0 range 16 .. 19;
      TS2            at 0 range 20 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      SJW            at 0 range 24 .. 25;
      Reserved_26_29 at 0 range 26 .. 29;
      LBKM           at 0 range 30 .. 30;
      SILM           at 0 range 31 .. 31;
   end record;

   subtype CAN_TI0R_EXID_Field is HAL.UInt18;
   subtype CAN_TI0R_STID_Field is HAL.UInt11;

   --  CAN_TI0R
   type CAN_TI0R_Register is record
      --  TXRQ
      TXRQ : Boolean := False;
      --  RTR
      RTR  : Boolean := False;
      --  IDE
      IDE  : Boolean := False;
      --  EXID
      EXID : CAN_TI0R_EXID_Field := 16#0#;
      --  STID
      STID : CAN_TI0R_STID_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CAN_TI0R_Register use record
      TXRQ at 0 range 0 .. 0;
      RTR  at 0 range 1 .. 1;
      IDE  at 0 range 2 .. 2;
      EXID at 0 range 3 .. 20;
      STID at 0 range 21 .. 31;
   end record;

   subtype CAN_TDT0R_DLC_Field is HAL.UInt4;
   subtype CAN_TDT0R_TIME_Field is HAL.UInt16;

   --  CAN_TDT0R
   type CAN_TDT0R_Register is record
      --  DLC
      DLC           : CAN_TDT0R_DLC_Field := 16#0#;
      --  unspecified
      Reserved_4_7  : HAL.UInt4 := 16#0#;
      --  TGT
      TGT           : Boolean := False;
      --  unspecified
      Reserved_9_15 : HAL.UInt7 := 16#0#;
      --  TIME
      TIME          : CAN_TDT0R_TIME_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CAN_TDT0R_Register use record
      DLC           at 0 range 0 .. 3;
      Reserved_4_7  at 0 range 4 .. 7;
      TGT           at 0 range 8 .. 8;
      Reserved_9_15 at 0 range 9 .. 15;
      TIME          at 0 range 16 .. 31;
   end record;

   --  CAN_TDL0R_DATA array element
   subtype CAN_TDL0R_DATA_Element is HAL.UInt8;

   --  CAN_TDL0R_DATA array
   type CAN_TDL0R_DATA_Field_Array is array (0 .. 3)
     of CAN_TDL0R_DATA_Element
     with Component_Size => 8, Size => 32;

   --  CAN_TDL0R
   type CAN_TDL0R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DATA as a value
            Val : HAL.UInt32;
         when True =>
            --  DATA as an array
            Arr : CAN_TDL0R_DATA_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for CAN_TDL0R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  CAN_TDH0R_DATA array element
   subtype CAN_TDH0R_DATA_Element is HAL.UInt8;

   --  CAN_TDH0R_DATA array
   type CAN_TDH0R_DATA_Field_Array is array (4 .. 7)
     of CAN_TDH0R_DATA_Element
     with Component_Size => 8, Size => 32;

   --  CAN_TDH0R
   type CAN_TDH0R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DATA as a value
            Val : HAL.UInt32;
         when True =>
            --  DATA as an array
            Arr : CAN_TDH0R_DATA_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for CAN_TDH0R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   subtype CAN_TI1R_EXID_Field is HAL.UInt18;
   subtype CAN_TI1R_STID_Field is HAL.UInt11;

   --  CAN_TI1R
   type CAN_TI1R_Register is record
      --  TXRQ
      TXRQ : Boolean := False;
      --  RTR
      RTR  : Boolean := False;
      --  IDE
      IDE  : Boolean := False;
      --  EXID
      EXID : CAN_TI1R_EXID_Field := 16#0#;
      --  STID
      STID : CAN_TI1R_STID_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CAN_TI1R_Register use record
      TXRQ at 0 range 0 .. 0;
      RTR  at 0 range 1 .. 1;
      IDE  at 0 range 2 .. 2;
      EXID at 0 range 3 .. 20;
      STID at 0 range 21 .. 31;
   end record;

   subtype CAN_TDT1R_DLC_Field is HAL.UInt4;
   subtype CAN_TDT1R_TIME_Field is HAL.UInt16;

   --  CAN_TDT1R
   type CAN_TDT1R_Register is record
      --  DLC
      DLC           : CAN_TDT1R_DLC_Field := 16#0#;
      --  unspecified
      Reserved_4_7  : HAL.UInt4 := 16#0#;
      --  TGT
      TGT           : Boolean := False;
      --  unspecified
      Reserved_9_15 : HAL.UInt7 := 16#0#;
      --  TIME
      TIME          : CAN_TDT1R_TIME_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CAN_TDT1R_Register use record
      DLC           at 0 range 0 .. 3;
      Reserved_4_7  at 0 range 4 .. 7;
      TGT           at 0 range 8 .. 8;
      Reserved_9_15 at 0 range 9 .. 15;
      TIME          at 0 range 16 .. 31;
   end record;

   --  CAN_TDL1R_DATA array element
   subtype CAN_TDL1R_DATA_Element is HAL.UInt8;

   --  CAN_TDL1R_DATA array
   type CAN_TDL1R_DATA_Field_Array is array (0 .. 3)
     of CAN_TDL1R_DATA_Element
     with Component_Size => 8, Size => 32;

   --  CAN_TDL1R
   type CAN_TDL1R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DATA as a value
            Val : HAL.UInt32;
         when True =>
            --  DATA as an array
            Arr : CAN_TDL1R_DATA_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for CAN_TDL1R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  CAN_TDH1R_DATA array element
   subtype CAN_TDH1R_DATA_Element is HAL.UInt8;

   --  CAN_TDH1R_DATA array
   type CAN_TDH1R_DATA_Field_Array is array (4 .. 7)
     of CAN_TDH1R_DATA_Element
     with Component_Size => 8, Size => 32;

   --  CAN_TDH1R
   type CAN_TDH1R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DATA as a value
            Val : HAL.UInt32;
         when True =>
            --  DATA as an array
            Arr : CAN_TDH1R_DATA_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for CAN_TDH1R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   subtype CAN_TI2R_EXID_Field is HAL.UInt18;
   subtype CAN_TI2R_STID_Field is HAL.UInt11;

   --  CAN_TI2R
   type CAN_TI2R_Register is record
      --  TXRQ
      TXRQ : Boolean := False;
      --  RTR
      RTR  : Boolean := False;
      --  IDE
      IDE  : Boolean := False;
      --  EXID
      EXID : CAN_TI2R_EXID_Field := 16#0#;
      --  STID
      STID : CAN_TI2R_STID_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CAN_TI2R_Register use record
      TXRQ at 0 range 0 .. 0;
      RTR  at 0 range 1 .. 1;
      IDE  at 0 range 2 .. 2;
      EXID at 0 range 3 .. 20;
      STID at 0 range 21 .. 31;
   end record;

   subtype CAN_TDT2R_DLC_Field is HAL.UInt4;
   subtype CAN_TDT2R_TIME_Field is HAL.UInt16;

   --  CAN_TDT2R
   type CAN_TDT2R_Register is record
      --  DLC
      DLC           : CAN_TDT2R_DLC_Field := 16#0#;
      --  unspecified
      Reserved_4_7  : HAL.UInt4 := 16#0#;
      --  TGT
      TGT           : Boolean := False;
      --  unspecified
      Reserved_9_15 : HAL.UInt7 := 16#0#;
      --  TIME
      TIME          : CAN_TDT2R_TIME_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CAN_TDT2R_Register use record
      DLC           at 0 range 0 .. 3;
      Reserved_4_7  at 0 range 4 .. 7;
      TGT           at 0 range 8 .. 8;
      Reserved_9_15 at 0 range 9 .. 15;
      TIME          at 0 range 16 .. 31;
   end record;

   --  CAN_TDL2R_DATA array element
   subtype CAN_TDL2R_DATA_Element is HAL.UInt8;

   --  CAN_TDL2R_DATA array
   type CAN_TDL2R_DATA_Field_Array is array (0 .. 3)
     of CAN_TDL2R_DATA_Element
     with Component_Size => 8, Size => 32;

   --  CAN_TDL2R
   type CAN_TDL2R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DATA as a value
            Val : HAL.UInt32;
         when True =>
            --  DATA as an array
            Arr : CAN_TDL2R_DATA_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for CAN_TDL2R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  CAN_TDH2R_DATA array element
   subtype CAN_TDH2R_DATA_Element is HAL.UInt8;

   --  CAN_TDH2R_DATA array
   type CAN_TDH2R_DATA_Field_Array is array (4 .. 7)
     of CAN_TDH2R_DATA_Element
     with Component_Size => 8, Size => 32;

   --  CAN_TDH2R
   type CAN_TDH2R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DATA as a value
            Val : HAL.UInt32;
         when True =>
            --  DATA as an array
            Arr : CAN_TDH2R_DATA_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for CAN_TDH2R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   subtype CAN_RI0R_EXID_Field is HAL.UInt18;
   subtype CAN_RI0R_STID_Field is HAL.UInt11;

   --  CAN_RI0R
   type CAN_RI0R_Register is record
      --  unspecified
      Reserved_0_0 : HAL.Bit;
      --  Read-only. RTR
      RTR          : Boolean;
      --  Read-only. IDE
      IDE          : Boolean;
      --  Read-only. EXID
      EXID         : CAN_RI0R_EXID_Field;
      --  Read-only. STID
      STID         : CAN_RI0R_STID_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CAN_RI0R_Register use record
      Reserved_0_0 at 0 range 0 .. 0;
      RTR          at 0 range 1 .. 1;
      IDE          at 0 range 2 .. 2;
      EXID         at 0 range 3 .. 20;
      STID         at 0 range 21 .. 31;
   end record;

   subtype CAN_RDT0R_DLC_Field is HAL.UInt4;
   subtype CAN_RDT0R_FMI_Field is HAL.UInt8;
   subtype CAN_RDT0R_TIME_Field is HAL.UInt16;

   --  CAN_RDT0R
   type CAN_RDT0R_Register is record
      --  Read-only. DLC
      DLC          : CAN_RDT0R_DLC_Field;
      --  unspecified
      Reserved_4_7 : HAL.UInt4;
      --  Read-only. FMI
      FMI          : CAN_RDT0R_FMI_Field;
      --  Read-only. TIME
      TIME         : CAN_RDT0R_TIME_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CAN_RDT0R_Register use record
      DLC          at 0 range 0 .. 3;
      Reserved_4_7 at 0 range 4 .. 7;
      FMI          at 0 range 8 .. 15;
      TIME         at 0 range 16 .. 31;
   end record;

   --  CAN_RDL0R_DATA array element
   subtype CAN_RDL0R_DATA_Element is HAL.UInt8;

   --  CAN_RDL0R_DATA array
   type CAN_RDL0R_DATA_Field_Array is array (0 .. 3)
     of CAN_RDL0R_DATA_Element
     with Component_Size => 8, Size => 32;

   --  CAN_RDL0R
   type CAN_RDL0R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DATA as a value
            Val : HAL.UInt32;
         when True =>
            --  DATA as an array
            Arr : CAN_RDL0R_DATA_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for CAN_RDL0R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  CAN_RDH0R_DATA array element
   subtype CAN_RDH0R_DATA_Element is HAL.UInt8;

   --  CAN_RDH0R_DATA array
   type CAN_RDH0R_DATA_Field_Array is array (4 .. 7)
     of CAN_RDH0R_DATA_Element
     with Component_Size => 8, Size => 32;

   --  CAN_RDH0R
   type CAN_RDH0R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DATA as a value
            Val : HAL.UInt32;
         when True =>
            --  DATA as an array
            Arr : CAN_RDH0R_DATA_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for CAN_RDH0R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   subtype CAN_RI1R_EXID_Field is HAL.UInt18;
   subtype CAN_RI1R_STID_Field is HAL.UInt11;

   --  CAN_RI1R
   type CAN_RI1R_Register is record
      --  unspecified
      Reserved_0_0 : HAL.Bit;
      --  Read-only. RTR
      RTR          : Boolean;
      --  Read-only. IDE
      IDE          : Boolean;
      --  Read-only. EXID
      EXID         : CAN_RI1R_EXID_Field;
      --  Read-only. STID
      STID         : CAN_RI1R_STID_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CAN_RI1R_Register use record
      Reserved_0_0 at 0 range 0 .. 0;
      RTR          at 0 range 1 .. 1;
      IDE          at 0 range 2 .. 2;
      EXID         at 0 range 3 .. 20;
      STID         at 0 range 21 .. 31;
   end record;

   subtype CAN_RDT1R_DLC_Field is HAL.UInt4;
   subtype CAN_RDT1R_FMI_Field is HAL.UInt8;
   subtype CAN_RDT1R_TIME_Field is HAL.UInt16;

   --  CAN_RDT1R
   type CAN_RDT1R_Register is record
      --  Read-only. DLC
      DLC          : CAN_RDT1R_DLC_Field;
      --  unspecified
      Reserved_4_7 : HAL.UInt4;
      --  Read-only. FMI
      FMI          : CAN_RDT1R_FMI_Field;
      --  Read-only. TIME
      TIME         : CAN_RDT1R_TIME_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CAN_RDT1R_Register use record
      DLC          at 0 range 0 .. 3;
      Reserved_4_7 at 0 range 4 .. 7;
      FMI          at 0 range 8 .. 15;
      TIME         at 0 range 16 .. 31;
   end record;

   --  CAN_RDL1R_DATA array element
   subtype CAN_RDL1R_DATA_Element is HAL.UInt8;

   --  CAN_RDL1R_DATA array
   type CAN_RDL1R_DATA_Field_Array is array (0 .. 3)
     of CAN_RDL1R_DATA_Element
     with Component_Size => 8, Size => 32;

   --  CAN_RDL1R
   type CAN_RDL1R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DATA as a value
            Val : HAL.UInt32;
         when True =>
            --  DATA as an array
            Arr : CAN_RDL1R_DATA_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for CAN_RDL1R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  CAN_RDH1R_DATA array element
   subtype CAN_RDH1R_DATA_Element is HAL.UInt8;

   --  CAN_RDH1R_DATA array
   type CAN_RDH1R_DATA_Field_Array is array (4 .. 7)
     of CAN_RDH1R_DATA_Element
     with Component_Size => 8, Size => 32;

   --  CAN_RDH1R
   type CAN_RDH1R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DATA as a value
            Val : HAL.UInt32;
         when True =>
            --  DATA as an array
            Arr : CAN_RDH1R_DATA_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for CAN_RDH1R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  CAN_FMR
   type CAN_FMR_Register is record
      --  FINIT
      FINIT         : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CAN_FMR_Register use record
      FINIT         at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  CAN_FM1R_FBM array
   type CAN_FM1R_FBM_Field_Array is array (0 .. 13) of Boolean
     with Component_Size => 1, Size => 14;

   --  Type definition for CAN_FM1R_FBM
   type CAN_FM1R_FBM_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FBM as a value
            Val : HAL.UInt14;
         when True =>
            --  FBM as an array
            Arr : CAN_FM1R_FBM_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 14;

   for CAN_FM1R_FBM_Field use record
      Val at 0 range 0 .. 13;
      Arr at 0 range 0 .. 13;
   end record;

   --  CAN_FM1R
   type CAN_FM1R_Register is record
      --  Filter mode
      FBM            : CAN_FM1R_FBM_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CAN_FM1R_Register use record
      FBM            at 0 range 0 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   --  CAN_FS1R_FSC array
   type CAN_FS1R_FSC_Field_Array is array (0 .. 13) of Boolean
     with Component_Size => 1, Size => 14;

   --  Type definition for CAN_FS1R_FSC
   type CAN_FS1R_FSC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FSC as a value
            Val : HAL.UInt14;
         when True =>
            --  FSC as an array
            Arr : CAN_FS1R_FSC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 14;

   for CAN_FS1R_FSC_Field use record
      Val at 0 range 0 .. 13;
      Arr at 0 range 0 .. 13;
   end record;

   --  CAN_FS1R
   type CAN_FS1R_Register is record
      --  Filter scale configuration
      FSC            : CAN_FS1R_FSC_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CAN_FS1R_Register use record
      FSC            at 0 range 0 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   --  CAN_FFA1R_FFA array
   type CAN_FFA1R_FFA_Field_Array is array (0 .. 13) of Boolean
     with Component_Size => 1, Size => 14;

   --  Type definition for CAN_FFA1R_FFA
   type CAN_FFA1R_FFA_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FFA as a value
            Val : HAL.UInt14;
         when True =>
            --  FFA as an array
            Arr : CAN_FFA1R_FFA_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 14;

   for CAN_FFA1R_FFA_Field use record
      Val at 0 range 0 .. 13;
      Arr at 0 range 0 .. 13;
   end record;

   --  CAN_FFA1R
   type CAN_FFA1R_Register is record
      --  Filter FIFO assignment for filter 0
      FFA            : CAN_FFA1R_FFA_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CAN_FFA1R_Register use record
      FFA            at 0 range 0 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   --  CAN_FA1R_FACT array
   type CAN_FA1R_FACT_Field_Array is array (0 .. 13) of Boolean
     with Component_Size => 1, Size => 14;

   --  Type definition for CAN_FA1R_FACT
   type CAN_FA1R_FACT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FACT as a value
            Val : HAL.UInt14;
         when True =>
            --  FACT as an array
            Arr : CAN_FA1R_FACT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 14;

   for CAN_FA1R_FACT_Field use record
      Val at 0 range 0 .. 13;
      Arr at 0 range 0 .. 13;
   end record;

   --  CAN_FA1R
   type CAN_FA1R_Register is record
      --  Filter active
      FACT           : CAN_FA1R_FACT_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CAN_FA1R_Register use record
      FACT           at 0 range 0 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   --  F0R_FB array
   type F0R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 0 register 1
   type F0R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F0R_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F0R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  F1R_FB array
   type F1R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 1 register 1
   type F1R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F1R_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F1R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  F2R_FB array
   type F2R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 2 register 1
   type F2R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F2R_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F2R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  F3R_FB array
   type F3R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 3 register 1
   type F3R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F3R_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F3R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  F4R_FB array
   type F4R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 4 register 1
   type F4R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F4R_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F4R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  F5R_FB array
   type F5R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 5 register 1
   type F5R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F5R_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F5R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  F6R_FB array
   type F6R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 6 register 1
   type F6R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F6R_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F6R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  F7R_FB array
   type F7R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 7 register 1
   type F7R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F7R_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F7R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  F8R_FB array
   type F8R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 8 register 1
   type F8R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F8R_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F8R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  F9R_FB array
   type F9R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 9 register 1
   type F9R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F9R_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F9R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  F10R_FB array
   type F10R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 10 register 1
   type F10R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F10R_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F10R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  F11R_FB array
   type F11R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 11 register 1
   type F11R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F11R_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F11R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  F12R_FB array
   type F12R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 4 register 1
   type F12R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F12R_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F12R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  F13R_FB array
   type F13R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 13 register 1
   type F13R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F13R_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F13R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Controller area network
   type CAN_Peripheral is record
      --  CAN_MCR
      CAN_MCR   : aliased CAN_MCR_Register;
      --  CAN_MSR
      CAN_MSR   : aliased CAN_MSR_Register;
      --  CAN_TSR
      CAN_TSR   : aliased CAN_TSR_Register;
      --  CAN_RF0R
      CAN_RF0R  : aliased CAN_RF0R_Register;
      --  CAN_RF1R
      CAN_RF1R  : aliased CAN_RF1R_Register;
      --  CAN_IER
      CAN_IER   : aliased CAN_IER_Register;
      --  CAN_ESR
      CAN_ESR   : aliased CAN_ESR_Register;
      --  CAN_BTR
      CAN_BTR   : aliased CAN_BTR_Register;
      --  CAN_TI0R
      CAN_TI0R  : aliased CAN_TI0R_Register;
      --  CAN_TDT0R
      CAN_TDT0R : aliased CAN_TDT0R_Register;
      --  CAN_TDL0R
      CAN_TDL0R : aliased CAN_TDL0R_Register;
      --  CAN_TDH0R
      CAN_TDH0R : aliased CAN_TDH0R_Register;
      --  CAN_TI1R
      CAN_TI1R  : aliased CAN_TI1R_Register;
      --  CAN_TDT1R
      CAN_TDT1R : aliased CAN_TDT1R_Register;
      --  CAN_TDL1R
      CAN_TDL1R : aliased CAN_TDL1R_Register;
      --  CAN_TDH1R
      CAN_TDH1R : aliased CAN_TDH1R_Register;
      --  CAN_TI2R
      CAN_TI2R  : aliased CAN_TI2R_Register;
      --  CAN_TDT2R
      CAN_TDT2R : aliased CAN_TDT2R_Register;
      --  CAN_TDL2R
      CAN_TDL2R : aliased CAN_TDL2R_Register;
      --  CAN_TDH2R
      CAN_TDH2R : aliased CAN_TDH2R_Register;
      --  CAN_RI0R
      CAN_RI0R  : aliased CAN_RI0R_Register;
      --  CAN_RDT0R
      CAN_RDT0R : aliased CAN_RDT0R_Register;
      --  CAN_RDL0R
      CAN_RDL0R : aliased CAN_RDL0R_Register;
      --  CAN_RDH0R
      CAN_RDH0R : aliased CAN_RDH0R_Register;
      --  CAN_RI1R
      CAN_RI1R  : aliased CAN_RI1R_Register;
      --  CAN_RDT1R
      CAN_RDT1R : aliased CAN_RDT1R_Register;
      --  CAN_RDL1R
      CAN_RDL1R : aliased CAN_RDL1R_Register;
      --  CAN_RDH1R
      CAN_RDH1R : aliased CAN_RDH1R_Register;
      --  CAN_FMR
      CAN_FMR   : aliased CAN_FMR_Register;
      --  CAN_FM1R
      CAN_FM1R  : aliased CAN_FM1R_Register;
      --  CAN_FS1R
      CAN_FS1R  : aliased CAN_FS1R_Register;
      --  CAN_FFA1R
      CAN_FFA1R : aliased CAN_FFA1R_Register;
      --  CAN_FA1R
      CAN_FA1R  : aliased CAN_FA1R_Register;
      --  Filter bank 0 register 1
      F0R1      : aliased F0R_Register;
      --  Filter bank 0 register 2
      F0R2      : aliased F0R_Register;
      --  Filter bank 1 register 1
      F1R1      : aliased F1R_Register;
      --  Filter bank 1 register 2
      F1R2      : aliased F1R_Register;
      --  Filter bank 2 register 1
      F2R1      : aliased F2R_Register;
      --  Filter bank 2 register 2
      F2R2      : aliased F2R_Register;
      --  Filter bank 3 register 1
      F3R1      : aliased F3R_Register;
      --  Filter bank 3 register 2
      F3R2      : aliased F3R_Register;
      --  Filter bank 4 register 1
      F4R1      : aliased F4R_Register;
      --  Filter bank 4 register 2
      F4R2      : aliased F4R_Register;
      --  Filter bank 5 register 1
      F5R1      : aliased F5R_Register;
      --  Filter bank 5 register 2
      F5R2      : aliased F5R_Register;
      --  Filter bank 6 register 1
      F6R1      : aliased F6R_Register;
      --  Filter bank 6 register 2
      F6R2      : aliased F6R_Register;
      --  Filter bank 7 register 1
      F7R1      : aliased F7R_Register;
      --  Filter bank 7 register 2
      F7R2      : aliased F7R_Register;
      --  Filter bank 8 register 1
      F8R1      : aliased F8R_Register;
      --  Filter bank 8 register 2
      F8R2      : aliased F8R_Register;
      --  Filter bank 9 register 1
      F9R1      : aliased F9R_Register;
      --  Filter bank 9 register 2
      F9R2      : aliased F9R_Register;
      --  Filter bank 10 register 1
      F10R1     : aliased F10R_Register;
      --  Filter bank 10 register 2
      F10R2     : aliased F10R_Register;
      --  Filter bank 11 register 1
      F11R1     : aliased F11R_Register;
      --  Filter bank 11 register 2
      F11R2     : aliased F11R_Register;
      --  Filter bank 4 register 1
      F12R1     : aliased F12R_Register;
      --  Filter bank 12 register 2
      F12R2     : aliased F12R_Register;
      --  Filter bank 13 register 1
      F13R1     : aliased F13R_Register;
      --  Filter bank 13 register 2
      F13R2     : aliased F13R_Register;
   end record
     with Volatile;

   for CAN_Peripheral use record
      CAN_MCR   at 16#0# range 0 .. 31;
      CAN_MSR   at 16#4# range 0 .. 31;
      CAN_TSR   at 16#8# range 0 .. 31;
      CAN_RF0R  at 16#C# range 0 .. 31;
      CAN_RF1R  at 16#10# range 0 .. 31;
      CAN_IER   at 16#14# range 0 .. 31;
      CAN_ESR   at 16#18# range 0 .. 31;
      CAN_BTR   at 16#1C# range 0 .. 31;
      CAN_TI0R  at 16#180# range 0 .. 31;
      CAN_TDT0R at 16#184# range 0 .. 31;
      CAN_TDL0R at 16#188# range 0 .. 31;
      CAN_TDH0R at 16#18C# range 0 .. 31;
      CAN_TI1R  at 16#190# range 0 .. 31;
      CAN_TDT1R at 16#194# range 0 .. 31;
      CAN_TDL1R at 16#198# range 0 .. 31;
      CAN_TDH1R at 16#19C# range 0 .. 31;
      CAN_TI2R  at 16#1A0# range 0 .. 31;
      CAN_TDT2R at 16#1A4# range 0 .. 31;
      CAN_TDL2R at 16#1A8# range 0 .. 31;
      CAN_TDH2R at 16#1AC# range 0 .. 31;
      CAN_RI0R  at 16#1B0# range 0 .. 31;
      CAN_RDT0R at 16#1B4# range 0 .. 31;
      CAN_RDL0R at 16#1B8# range 0 .. 31;
      CAN_RDH0R at 16#1BC# range 0 .. 31;
      CAN_RI1R  at 16#1C0# range 0 .. 31;
      CAN_RDT1R at 16#1C4# range 0 .. 31;
      CAN_RDL1R at 16#1C8# range 0 .. 31;
      CAN_RDH1R at 16#1CC# range 0 .. 31;
      CAN_FMR   at 16#200# range 0 .. 31;
      CAN_FM1R  at 16#204# range 0 .. 31;
      CAN_FS1R  at 16#20C# range 0 .. 31;
      CAN_FFA1R at 16#214# range 0 .. 31;
      CAN_FA1R  at 16#21C# range 0 .. 31;
      F0R1      at 16#240# range 0 .. 31;
      F0R2      at 16#244# range 0 .. 31;
      F1R1      at 16#248# range 0 .. 31;
      F1R2      at 16#24C# range 0 .. 31;
      F2R1      at 16#250# range 0 .. 31;
      F2R2      at 16#254# range 0 .. 31;
      F3R1      at 16#258# range 0 .. 31;
      F3R2      at 16#25C# range 0 .. 31;
      F4R1      at 16#260# range 0 .. 31;
      F4R2      at 16#264# range 0 .. 31;
      F5R1      at 16#268# range 0 .. 31;
      F5R2      at 16#26C# range 0 .. 31;
      F6R1      at 16#270# range 0 .. 31;
      F6R2      at 16#274# range 0 .. 31;
      F7R1      at 16#278# range 0 .. 31;
      F7R2      at 16#27C# range 0 .. 31;
      F8R1      at 16#280# range 0 .. 31;
      F8R2      at 16#284# range 0 .. 31;
      F9R1      at 16#288# range 0 .. 31;
      F9R2      at 16#28C# range 0 .. 31;
      F10R1     at 16#290# range 0 .. 31;
      F10R2     at 16#294# range 0 .. 31;
      F11R1     at 16#298# range 0 .. 31;
      F11R2     at 16#29C# range 0 .. 31;
      F12R1     at 16#2A0# range 0 .. 31;
      F12R2     at 16#2A4# range 0 .. 31;
      F13R1     at 16#2A8# range 0 .. 31;
      F13R2     at 16#2AC# range 0 .. 31;
   end record;

   --  Controller area network
   CAN1_Periph : aliased CAN_Peripheral
     with Import, Address => System'To_Address (16#40006400#);

   --  Controller area network
   CAN2_Periph : aliased CAN_Peripheral
     with Import, Address => System'To_Address (16#40006800#);

end STM32_SVD.CAN;
