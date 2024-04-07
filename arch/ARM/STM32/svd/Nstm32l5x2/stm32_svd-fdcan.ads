--  This spec has been automatically generated from STM32L562.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.FDCAN is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype FDCAN_CREL_DAY_Field is HAL.UInt8;
   subtype FDCAN_CREL_MON_Field is HAL.UInt8;
   subtype FDCAN_CREL_YEAR_Field is HAL.UInt4;
   subtype FDCAN_CREL_SUBSTEP_Field is HAL.UInt4;
   subtype FDCAN_CREL_STEP_Field is HAL.UInt4;
   subtype FDCAN_CREL_REL_Field is HAL.UInt4;

   --  FDCAN Core Release Register
   type FDCAN_CREL_Register is record
      --  Read-only. Timestamp Day
      DAY     : FDCAN_CREL_DAY_Field;
      --  Read-only. Timestamp Month
      MON     : FDCAN_CREL_MON_Field;
      --  Read-only. Timestamp Year
      YEAR    : FDCAN_CREL_YEAR_Field;
      --  Read-only. Sub-step of Core release
      SUBSTEP : FDCAN_CREL_SUBSTEP_Field;
      --  Read-only. Step of Core release
      STEP    : FDCAN_CREL_STEP_Field;
      --  Read-only. Core release
      REL     : FDCAN_CREL_REL_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_CREL_Register use record
      DAY     at 0 range 0 .. 7;
      MON     at 0 range 8 .. 15;
      YEAR    at 0 range 16 .. 19;
      SUBSTEP at 0 range 20 .. 23;
      STEP    at 0 range 24 .. 27;
      REL     at 0 range 28 .. 31;
   end record;

   subtype FDCAN_DBTP_DSJW_Field is HAL.UInt4;
   subtype FDCAN_DBTP_DTSEG2_Field is HAL.UInt4;
   subtype FDCAN_DBTP_DTSEG1_Field is HAL.UInt5;
   subtype FDCAN_DBTP_DBRP_Field is HAL.UInt5;

   --  FDCAN Data Bit Timing and Prescaler Register
   type FDCAN_DBTP_Register is record
      --  Synchronization Jump Width
      DSJW           : FDCAN_DBTP_DSJW_Field := 16#3#;
      --  Data time segment after sample point
      DTSEG2         : FDCAN_DBTP_DTSEG2_Field := 16#3#;
      --  Data time segment after sample point
      DTSEG1         : FDCAN_DBTP_DTSEG1_Field := 16#A#;
      --  unspecified
      Reserved_13_15 : HAL.UInt3 := 16#0#;
      --  Data BIt Rate Prescaler
      DBRP           : FDCAN_DBTP_DBRP_Field := 16#0#;
      --  unspecified
      Reserved_21_22 : HAL.UInt2 := 16#0#;
      --  Transceiver Delay Compensation
      TDC            : Boolean := False;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_DBTP_Register use record
      DSJW           at 0 range 0 .. 3;
      DTSEG2         at 0 range 4 .. 7;
      DTSEG1         at 0 range 8 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      DBRP           at 0 range 16 .. 20;
      Reserved_21_22 at 0 range 21 .. 22;
      TDC            at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype FDCAN_TEST_TX_Field is HAL.UInt2;

   --  FDCAN Test Register
   type FDCAN_TEST_Register is record
      --  unspecified
      Reserved_0_3  : HAL.UInt4 := 16#0#;
      --  Loop Back mode
      LBCK          : Boolean := True;
      --  Loop Back mode
      TX            : FDCAN_TEST_TX_Field := 16#0#;
      --  Read-only. Control of Transmit Pin
      RX            : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TEST_Register use record
      Reserved_0_3  at 0 range 0 .. 3;
      LBCK          at 0 range 4 .. 4;
      TX            at 0 range 5 .. 6;
      RX            at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype FDCAN_RWD_WDC_Field is HAL.UInt8;
   subtype FDCAN_RWD_WDV_Field is HAL.UInt8;

   --  FDCAN RAM Watchdog Register
   type FDCAN_RWD_Register is record
      --  Watchdog configuration
      WDC            : FDCAN_RWD_WDC_Field := 16#0#;
      --  Read-only. Watchdog value
      WDV            : FDCAN_RWD_WDV_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_RWD_Register use record
      WDC            at 0 range 0 .. 7;
      WDV            at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  FDCAN CC Control Register
   type FDCAN_CCCR_Register is record
      --  Initialization
      INIT           : Boolean := True;
      --  Configuration Change Enable
      CCE            : Boolean := False;
      --  ASM Restricted Operation Mode
      ASM            : Boolean := False;
      --  Clock Stop Acknowledge
      CSA            : Boolean := False;
      --  Clock Stop Request
      CSR            : Boolean := False;
      --  Bus Monitoring Mode
      MON            : Boolean := False;
      --  Disable Automatic Retransmission
      DAR            : Boolean := False;
      --  Test Mode Enable
      TEST           : Boolean := False;
      --  FD Operation Enable
      FDOE           : Boolean := False;
      --  FDCAN Bit Rate Switching
      BSE            : Boolean := False;
      --  unspecified
      Reserved_10_11 : HAL.UInt2 := 16#0#;
      --  Protocol Exception Handling Disable
      PXHD           : Boolean := False;
      --  Edge Filtering during Bus Integration
      EFBI           : Boolean := False;
      --  TXP
      TXP            : Boolean := False;
      --  Non ISO Operation
      NISO           : Boolean := False;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_CCCR_Register use record
      INIT           at 0 range 0 .. 0;
      CCE            at 0 range 1 .. 1;
      ASM            at 0 range 2 .. 2;
      CSA            at 0 range 3 .. 3;
      CSR            at 0 range 4 .. 4;
      MON            at 0 range 5 .. 5;
      DAR            at 0 range 6 .. 6;
      TEST           at 0 range 7 .. 7;
      FDOE           at 0 range 8 .. 8;
      BSE            at 0 range 9 .. 9;
      Reserved_10_11 at 0 range 10 .. 11;
      PXHD           at 0 range 12 .. 12;
      EFBI           at 0 range 13 .. 13;
      TXP            at 0 range 14 .. 14;
      NISO           at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype FDCAN_NBTP_TSEG2_Field is HAL.UInt7;
   subtype FDCAN_NBTP_NTSEG1_Field is HAL.UInt8;
   subtype FDCAN_NBTP_NBRP_Field is HAL.UInt9;
   subtype FDCAN_NBTP_NSJW_Field is HAL.UInt7;

   --  FDCAN Nominal Bit Timing and Prescaler Register
   type FDCAN_NBTP_Register is record
      --  Nominal Time segment after sample point
      TSEG2        : FDCAN_NBTP_TSEG2_Field := 16#3#;
      --  unspecified
      Reserved_7_7 : HAL.Bit := 16#0#;
      --  Nominal Time segment before sample point
      NTSEG1       : FDCAN_NBTP_NTSEG1_Field := 16#A#;
      --  Bit Rate Prescaler
      NBRP         : FDCAN_NBTP_NBRP_Field := 16#0#;
      --  NSJW: Nominal (Re)Synchronization Jump Width
      NSJW         : FDCAN_NBTP_NSJW_Field := 16#3#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_NBTP_Register use record
      TSEG2        at 0 range 0 .. 6;
      Reserved_7_7 at 0 range 7 .. 7;
      NTSEG1       at 0 range 8 .. 15;
      NBRP         at 0 range 16 .. 24;
      NSJW         at 0 range 25 .. 31;
   end record;

   subtype FDCAN_TSCC_TSS_Field is HAL.UInt2;
   subtype FDCAN_TSCC_TCP_Field is HAL.UInt4;

   --  FDCAN Timestamp Counter Configuration Register
   type FDCAN_TSCC_Register is record
      --  Timestamp Select
      TSS            : FDCAN_TSCC_TSS_Field := 16#0#;
      --  unspecified
      Reserved_2_15  : HAL.UInt14 := 16#0#;
      --  Timestamp Counter Prescaler
      TCP            : FDCAN_TSCC_TCP_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TSCC_Register use record
      TSS            at 0 range 0 .. 1;
      Reserved_2_15  at 0 range 2 .. 15;
      TCP            at 0 range 16 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   subtype FDCAN_TSCV_TSC_Field is HAL.UInt16;

   --  FDCAN Timestamp Counter Value Register
   type FDCAN_TSCV_Register is record
      --  Timestamp Counter
      TSC            : FDCAN_TSCV_TSC_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TSCV_Register use record
      TSC            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype FDCAN_TOCC_TOS_Field is HAL.UInt2;
   subtype FDCAN_TOCC_TOP_Field is HAL.UInt16;

   --  FDCAN Timeout Counter Configuration Register
   type FDCAN_TOCC_Register is record
      --  Enable Timeout Counter
      ETOC          : Boolean := False;
      --  Timeout Select
      TOS           : FDCAN_TOCC_TOS_Field := 16#0#;
      --  unspecified
      Reserved_3_15 : HAL.UInt13 := 16#0#;
      --  Timeout Period
      TOP           : FDCAN_TOCC_TOP_Field := 16#FFFF#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TOCC_Register use record
      ETOC          at 0 range 0 .. 0;
      TOS           at 0 range 1 .. 2;
      Reserved_3_15 at 0 range 3 .. 15;
      TOP           at 0 range 16 .. 31;
   end record;

   subtype FDCAN_TOCV_TOC_Field is HAL.UInt16;

   --  FDCAN Timeout Counter Value Register
   type FDCAN_TOCV_Register is record
      --  Timeout Counter
      TOC            : FDCAN_TOCV_TOC_Field := 16#FFFF#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TOCV_Register use record
      TOC            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype FDCAN_ECR_TEC_Field is HAL.UInt8;
   subtype FDCAN_ECR_REC_Field is HAL.UInt7;
   subtype FDCAN_ECR_CEL_Field is HAL.UInt8;

   --  FDCAN Error Counter Register
   type FDCAN_ECR_Register is record
      --  Read-only. Transmit Error Counter
      TEC            : FDCAN_ECR_TEC_Field := 16#0#;
      --  Read-only. Receive Error Counter
      REC            : FDCAN_ECR_REC_Field := 16#0#;
      --  Receive Error Passive
      RP             : Boolean := False;
      --  AN Error Logging
      CEL            : FDCAN_ECR_CEL_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_ECR_Register use record
      TEC            at 0 range 0 .. 7;
      REC            at 0 range 8 .. 14;
      RP             at 0 range 15 .. 15;
      CEL            at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype FDCAN_PSR_LEC_Field is HAL.UInt3;
   subtype FDCAN_PSR_ACT_Field is HAL.UInt2;
   subtype FDCAN_PSR_DLEC_Field is HAL.UInt3;
   subtype FDCAN_PSR_TDCV_Field is HAL.UInt7;

   --  FDCAN Protocol Status Register
   type FDCAN_PSR_Register is record
      --  Last Error Code
      LEC            : FDCAN_PSR_LEC_Field := 16#7#;
      --  Read-only. Activity
      ACT            : FDCAN_PSR_ACT_Field := 16#0#;
      --  Read-only. Error Passive
      EP             : Boolean := False;
      --  Read-only. Warning Status
      EW             : Boolean := False;
      --  Read-only. Bus_Off Status
      BO             : Boolean := False;
      --  Data Last Error Code
      DLEC           : FDCAN_PSR_DLEC_Field := 16#7#;
      --  ESI flag of last received FDCAN Message
      RESI           : Boolean := False;
      --  BRS flag of last received FDCAN Message
      RBRS           : Boolean := False;
      --  Received FDCAN Message
      REDL           : Boolean := False;
      --  Protocol Exception Event
      PXE            : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Read-only. Transmitter Delay Compensation Value
      TDCV           : FDCAN_PSR_TDCV_Field := 16#0#;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_PSR_Register use record
      LEC            at 0 range 0 .. 2;
      ACT            at 0 range 3 .. 4;
      EP             at 0 range 5 .. 5;
      EW             at 0 range 6 .. 6;
      BO             at 0 range 7 .. 7;
      DLEC           at 0 range 8 .. 10;
      RESI           at 0 range 11 .. 11;
      RBRS           at 0 range 12 .. 12;
      REDL           at 0 range 13 .. 13;
      PXE            at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      TDCV           at 0 range 16 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   subtype FDCAN_TDCR_TDCF_Field is HAL.UInt7;
   subtype FDCAN_TDCR_TDCO_Field is HAL.UInt7;

   --  FDCAN Transmitter Delay Compensation Register
   type FDCAN_TDCR_Register is record
      --  Transmitter Delay Compensation Filter Window Length
      TDCF           : FDCAN_TDCR_TDCF_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Transmitter Delay Compensation Offset
      TDCO           : FDCAN_TDCR_TDCO_Field := 16#0#;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TDCR_Register use record
      TDCF           at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      TDCO           at 0 range 8 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   --  FDCAN Interrupt Register
   type FDCAN_IR_Register is record
      --  RF0N
      RF0N           : Boolean := False;
      --  RF0F
      RF0F           : Boolean := False;
      --  RF0L
      RF0L           : Boolean := False;
      --  RF1N
      RF1N           : Boolean := False;
      --  RF1F
      RF1F           : Boolean := False;
      --  RF1L
      RF1L           : Boolean := False;
      --  HPM
      HPM            : Boolean := False;
      --  TC
      TC             : Boolean := False;
      --  TCF
      TCF            : Boolean := False;
      --  TFE
      TFE            : Boolean := False;
      --  TEFN
      TEFN           : Boolean := False;
      --  TEFF
      TEFF           : Boolean := False;
      --  TEFL
      TEFL           : Boolean := False;
      --  TSW
      TSW            : Boolean := False;
      --  MRAF
      MRAF           : Boolean := False;
      --  TOO
      TOO            : Boolean := False;
      --  ELO
      ELO            : Boolean := False;
      --  EP
      EP             : Boolean := False;
      --  EW
      EW             : Boolean := False;
      --  BO
      BO             : Boolean := False;
      --  WDI
      WDI            : Boolean := False;
      --  PEA
      PEA            : Boolean := False;
      --  PED
      PED            : Boolean := False;
      --  ARA
      ARA            : Boolean := False;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_IR_Register use record
      RF0N           at 0 range 0 .. 0;
      RF0F           at 0 range 1 .. 1;
      RF0L           at 0 range 2 .. 2;
      RF1N           at 0 range 3 .. 3;
      RF1F           at 0 range 4 .. 4;
      RF1L           at 0 range 5 .. 5;
      HPM            at 0 range 6 .. 6;
      TC             at 0 range 7 .. 7;
      TCF            at 0 range 8 .. 8;
      TFE            at 0 range 9 .. 9;
      TEFN           at 0 range 10 .. 10;
      TEFF           at 0 range 11 .. 11;
      TEFL           at 0 range 12 .. 12;
      TSW            at 0 range 13 .. 13;
      MRAF           at 0 range 14 .. 14;
      TOO            at 0 range 15 .. 15;
      ELO            at 0 range 16 .. 16;
      EP             at 0 range 17 .. 17;
      EW             at 0 range 18 .. 18;
      BO             at 0 range 19 .. 19;
      WDI            at 0 range 20 .. 20;
      PEA            at 0 range 21 .. 21;
      PED            at 0 range 22 .. 22;
      ARA            at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  FDCAN Interrupt Enable Register
   type FDCAN_IE_Register is record
      --  Rx FIFO 0 New Message Enable
      RF0NE          : Boolean := False;
      --  Rx FIFO 0 Full Enable
      RF0FE          : Boolean := False;
      --  Rx FIFO 0 Message Lost Enable
      RF0LE          : Boolean := False;
      --  Rx FIFO 1 New Message Enable
      RF1NE          : Boolean := False;
      --  Rx FIFO 1 Watermark Reached Enable
      RF1FE          : Boolean := False;
      --  Rx FIFO 1 Message Lost Enable
      RF1LE          : Boolean := False;
      --  High Priority Message Enable
      HPME           : Boolean := False;
      --  Transmission Completed Enable
      TCE            : Boolean := False;
      --  Transmission Cancellation Finished Enable
      TCFE           : Boolean := False;
      --  Tx FIFO Empty Enable
      TEFE           : Boolean := False;
      --  Tx Event FIFO New Entry Enable
      TEFNE          : Boolean := False;
      --  Tx Event FIFO Full Enable
      TEFFE          : Boolean := False;
      --  Tx Event FIFO Element Lost Enable
      TEFLE          : Boolean := False;
      --  Message RAM Access Failure Enable
      MRAFE          : Boolean := False;
      --  Timeout Occurred Enable
      TOOE           : Boolean := False;
      --  Error Logging Overflow Enable
      ELOE           : Boolean := False;
      --  Error Passive Enable
      EPE            : Boolean := False;
      --  Warning Status Enable
      EWE            : Boolean := False;
      --  Bus_Off Status Enable
      BOE            : Boolean := False;
      --  Watchdog Interrupt Enable
      WDIE           : Boolean := False;
      --  Protocol Error in Arbitration Phase Enable
      PEAE           : Boolean := False;
      --  Protocol Error in Data Phase Enable
      PEDE           : Boolean := False;
      --  Access to Reserved Address Enable
      ARAE           : Boolean := False;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_IE_Register use record
      RF0NE          at 0 range 0 .. 0;
      RF0FE          at 0 range 1 .. 1;
      RF0LE          at 0 range 2 .. 2;
      RF1NE          at 0 range 3 .. 3;
      RF1FE          at 0 range 4 .. 4;
      RF1LE          at 0 range 5 .. 5;
      HPME           at 0 range 6 .. 6;
      TCE            at 0 range 7 .. 7;
      TCFE           at 0 range 8 .. 8;
      TEFE           at 0 range 9 .. 9;
      TEFNE          at 0 range 10 .. 10;
      TEFFE          at 0 range 11 .. 11;
      TEFLE          at 0 range 12 .. 12;
      MRAFE          at 0 range 13 .. 13;
      TOOE           at 0 range 14 .. 14;
      ELOE           at 0 range 15 .. 15;
      EPE            at 0 range 16 .. 16;
      EWE            at 0 range 17 .. 17;
      BOE            at 0 range 18 .. 18;
      WDIE           at 0 range 19 .. 19;
      PEAE           at 0 range 20 .. 20;
      PEDE           at 0 range 21 .. 21;
      ARAE           at 0 range 22 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  FDCAN_ILS_RxFIFO array
   type FDCAN_ILS_RxFIFO_Field_Array is array (0 .. 1) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for FDCAN_ILS_RxFIFO
   type FDCAN_ILS_RxFIFO_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  RxFIFO as a value
            Val : HAL.UInt2;
         when True =>
            --  RxFIFO as an array
            Arr : FDCAN_ILS_RxFIFO_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for FDCAN_ILS_RxFIFO_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  FDCAN Interrupt Line Select Register
   type FDCAN_ILS_Register is record
      --  RxFIFO0
      RxFIFO        : FDCAN_ILS_RxFIFO_Field :=
                       (As_Array => False, Val => 16#0#);
      --  SMSG
      SMSG          : Boolean := False;
      --  TFERR
      TFERR         : Boolean := False;
      --  MISC
      MISC          : Boolean := False;
      --  BERR
      BERR          : Boolean := False;
      --  PERR
      PERR          : Boolean := False;
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_ILS_Register use record
      RxFIFO        at 0 range 0 .. 1;
      SMSG          at 0 range 2 .. 2;
      TFERR         at 0 range 3 .. 3;
      MISC          at 0 range 4 .. 4;
      BERR          at 0 range 5 .. 5;
      PERR          at 0 range 6 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   --  FDCAN_ILE_EINT array
   type FDCAN_ILE_EINT_Field_Array is array (0 .. 1) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for FDCAN_ILE_EINT
   type FDCAN_ILE_EINT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EINT as a value
            Val : HAL.UInt2;
         when True =>
            --  EINT as an array
            Arr : FDCAN_ILE_EINT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for FDCAN_ILE_EINT_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  FDCAN Interrupt Line Enable Register
   type FDCAN_ILE_Register is record
      --  Enable Interrupt Line 0
      EINT          : FDCAN_ILE_EINT_Field :=
                       (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_ILE_Register use record
      EINT          at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   subtype FDCAN_RXGFC_ANFE_Field is HAL.UInt2;
   subtype FDCAN_RXGFC_ANFS_Field is HAL.UInt2;
   subtype FDCAN_RXGFC_LSS_Field is HAL.UInt5;
   subtype FDCAN_RXGFC_LSE_Field is HAL.UInt4;

   --  FDCAN Global Filter Configuration Register
   type FDCAN_RXGFC_Register is record
      --  Reject Remote Frames Extended
      RRFE           : Boolean := False;
      --  Reject Remote Frames Standard
      RRFS           : Boolean := False;
      --  Accept Non-matching Frames Extended
      ANFE           : FDCAN_RXGFC_ANFE_Field := 16#0#;
      --  Accept Non-matching Frames Standard
      ANFS           : FDCAN_RXGFC_ANFS_Field := 16#0#;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  F1OM
      F1OM           : Boolean := False;
      --  F0OM
      F0OM           : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  LSS
      LSS            : FDCAN_RXGFC_LSS_Field := 16#0#;
      --  unspecified
      Reserved_21_23 : HAL.UInt3 := 16#0#;
      --  LSE
      LSE            : FDCAN_RXGFC_LSE_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_RXGFC_Register use record
      RRFE           at 0 range 0 .. 0;
      RRFS           at 0 range 1 .. 1;
      ANFE           at 0 range 2 .. 3;
      ANFS           at 0 range 4 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      F1OM           at 0 range 8 .. 8;
      F0OM           at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      LSS            at 0 range 16 .. 20;
      Reserved_21_23 at 0 range 21 .. 23;
      LSE            at 0 range 24 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype FDCAN_XIDAM_EIDM_Field is HAL.UInt29;

   --  FDCAN Extended ID and Mask Register
   type FDCAN_XIDAM_Register is record
      --  Extended ID Mask
      EIDM           : FDCAN_XIDAM_EIDM_Field := 16#1FFFFFFF#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_XIDAM_Register use record
      EIDM           at 0 range 0 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype FDCAN_HPMS_BIDX_Field is HAL.UInt3;
   subtype FDCAN_HPMS_MSI_Field is HAL.UInt2;
   subtype FDCAN_HPMS_FIDX_Field is HAL.UInt5;

   --  FDCAN High Priority Message Status Register
   type FDCAN_HPMS_Register is record
      --  Read-only. Buffer Index
      BIDX           : FDCAN_HPMS_BIDX_Field;
      --  unspecified
      Reserved_3_5   : HAL.UInt3;
      --  Read-only. Message Storage Indicator
      MSI            : FDCAN_HPMS_MSI_Field;
      --  Read-only. Filter Index
      FIDX           : FDCAN_HPMS_FIDX_Field;
      --  unspecified
      Reserved_13_14 : HAL.UInt2;
      --  Read-only. Filter List
      FLST           : Boolean;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_HPMS_Register use record
      BIDX           at 0 range 0 .. 2;
      Reserved_3_5   at 0 range 3 .. 5;
      MSI            at 0 range 6 .. 7;
      FIDX           at 0 range 8 .. 12;
      Reserved_13_14 at 0 range 13 .. 14;
      FLST           at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype FDCAN_RXF0S_F0FL_Field is HAL.UInt4;
   subtype FDCAN_RXF0S_F0GI_Field is HAL.UInt2;
   subtype FDCAN_RXF0S_F0PI_Field is HAL.UInt2;

   --  FDCAN Rx FIFO 0 Status Register
   type FDCAN_RXF0S_Register is record
      --  Rx FIFO 0 Fill Level
      F0FL           : FDCAN_RXF0S_F0FL_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Rx FIFO 0 Get Index
      F0GI           : FDCAN_RXF0S_F0GI_Field := 16#0#;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Rx FIFO 0 Put Index
      F0PI           : FDCAN_RXF0S_F0PI_Field := 16#0#;
      --  unspecified
      Reserved_18_23 : HAL.UInt6 := 16#0#;
      --  Rx FIFO 0 Full
      F0F            : Boolean := False;
      --  Rx FIFO 0 Message Lost
      RF0L           : Boolean := False;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_RXF0S_Register use record
      F0FL           at 0 range 0 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      F0GI           at 0 range 8 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      F0PI           at 0 range 16 .. 17;
      Reserved_18_23 at 0 range 18 .. 23;
      F0F            at 0 range 24 .. 24;
      RF0L           at 0 range 25 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype FDCAN_RXF0A_F0AI_Field is HAL.UInt3;

   --  CAN Rx FIFO 0 Acknowledge Register
   type FDCAN_RXF0A_Register is record
      --  Rx FIFO 0 Acknowledge Index
      F0AI          : FDCAN_RXF0A_F0AI_Field := 16#0#;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_RXF0A_Register use record
      F0AI          at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype FDCAN_RXF1S_F1FL_Field is HAL.UInt4;
   subtype FDCAN_RXF1S_F1GI_Field is HAL.UInt2;
   subtype FDCAN_RXF1S_F1PI_Field is HAL.UInt2;

   --  FDCAN Rx FIFO 1 Status Register
   type FDCAN_RXF1S_Register is record
      --  Rx FIFO 1 Fill Level
      F1FL           : FDCAN_RXF1S_F1FL_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Read-only. Rx FIFO 1 Get Index
      F1GI           : FDCAN_RXF1S_F1GI_Field := 16#0#;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Read-only. Rx FIFO 1 Put Index
      F1PI           : FDCAN_RXF1S_F1PI_Field := 16#0#;
      --  unspecified
      Reserved_18_23 : HAL.UInt6 := 16#0#;
      --  Read-only. Rx FIFO 1 Full
      F1F            : Boolean := False;
      --  Read-only. Rx FIFO 1 Message Lost
      RF1L           : Boolean := False;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_RXF1S_Register use record
      F1FL           at 0 range 0 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      F1GI           at 0 range 8 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      F1PI           at 0 range 16 .. 17;
      Reserved_18_23 at 0 range 18 .. 23;
      F1F            at 0 range 24 .. 24;
      RF1L           at 0 range 25 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype FDCAN_RXF1A_F1AI_Field is HAL.UInt3;

   --  FDCAN Rx FIFO 1 Acknowledge Register
   type FDCAN_RXF1A_Register is record
      --  Rx FIFO 1 Acknowledge Index
      F1AI          : FDCAN_RXF1A_F1AI_Field := 16#0#;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_RXF1A_Register use record
      F1AI          at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  FDCAN Tx buffer configuration register
   type FDCAN_TXBC_Register is record
      --  unspecified
      Reserved_0_23  : HAL.UInt24 := 16#0#;
      --  Tx FIFO/Queue Mode
      TFQM           : Boolean := False;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TXBC_Register use record
      Reserved_0_23  at 0 range 0 .. 23;
      TFQM           at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype FDCAN_TXFQS_TFFL_Field is HAL.UInt3;
   subtype FDCAN_TXFQS_TFGI_Field is HAL.UInt2;
   subtype FDCAN_TXFQS_TFQPI_Field is HAL.UInt2;

   --  FDCAN Tx FIFO/Queue Status Register
   type FDCAN_TXFQS_Register is record
      --  Read-only. Tx FIFO Free Level
      TFFL           : FDCAN_TXFQS_TFFL_Field;
      --  unspecified
      Reserved_3_7   : HAL.UInt5;
      --  Read-only. TFGI
      TFGI           : FDCAN_TXFQS_TFGI_Field;
      --  unspecified
      Reserved_10_15 : HAL.UInt6;
      --  Read-only. Tx FIFO/Queue Put Index
      TFQPI          : FDCAN_TXFQS_TFQPI_Field;
      --  unspecified
      Reserved_18_20 : HAL.UInt3;
      --  Read-only. Tx FIFO/Queue Full
      TFQF           : Boolean;
      --  unspecified
      Reserved_22_31 : HAL.UInt10;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TXFQS_Register use record
      TFFL           at 0 range 0 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      TFGI           at 0 range 8 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      TFQPI          at 0 range 16 .. 17;
      Reserved_18_20 at 0 range 18 .. 20;
      TFQF           at 0 range 21 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   subtype FDCAN_TXBRP_TRP_Field is HAL.UInt3;

   --  FDCAN Tx Buffer Request Pending Register
   type FDCAN_TXBRP_Register is record
      --  Read-only. Transmission Request Pending
      TRP           : FDCAN_TXBRP_TRP_Field;
      --  unspecified
      Reserved_3_31 : HAL.UInt29;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TXBRP_Register use record
      TRP           at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype FDCAN_TXBAR_AR_Field is HAL.UInt3;

   --  FDCAN Tx Buffer Add Request Register
   type FDCAN_TXBAR_Register is record
      --  Add Request
      AR            : FDCAN_TXBAR_AR_Field := 16#0#;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TXBAR_Register use record
      AR            at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype FDCAN_TXBCR_CR_Field is HAL.UInt3;

   --  FDCAN Tx Buffer Cancellation Request Register
   type FDCAN_TXBCR_Register is record
      --  Cancellation Request
      CR            : FDCAN_TXBCR_CR_Field := 16#0#;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TXBCR_Register use record
      CR            at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype FDCAN_TXBTO_TO_Field is HAL.UInt3;

   --  FDCAN Tx Buffer Transmission Occurred Register
   type FDCAN_TXBTO_Register is record
      --  Read-only. Transmission Occurred.
      TO            : FDCAN_TXBTO_TO_Field;
      --  unspecified
      Reserved_3_31 : HAL.UInt29;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TXBTO_Register use record
      TO            at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype FDCAN_TXBCF_CF_Field is HAL.UInt3;

   --  FDCAN Tx Buffer Cancellation Finished Register
   type FDCAN_TXBCF_Register is record
      --  Read-only. Cancellation Finished
      CF            : FDCAN_TXBCF_CF_Field;
      --  unspecified
      Reserved_3_31 : HAL.UInt29;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TXBCF_Register use record
      CF            at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype FDCAN_TXBTIE_TIE_Field is HAL.UInt3;

   --  FDCAN Tx Buffer Transmission Interrupt Enable Register
   type FDCAN_TXBTIE_Register is record
      --  Transmission Interrupt Enable
      TIE           : FDCAN_TXBTIE_TIE_Field := 16#0#;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TXBTIE_Register use record
      TIE           at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype FDCAN_TXBCIE_CF_Field is HAL.UInt3;

   --  FDCAN Tx Buffer Cancellation Finished Interrupt Enable Register
   type FDCAN_TXBCIE_Register is record
      --  Cancellation Finished Interrupt Enable
      CF            : FDCAN_TXBCIE_CF_Field := 16#0#;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TXBCIE_Register use record
      CF            at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype FDCAN_TXEFS_EFFL_Field is HAL.UInt3;
   subtype FDCAN_TXEFS_EFGI_Field is HAL.UInt2;
   subtype FDCAN_TXEFS_EFPI_Field is HAL.UInt2;

   --  FDCAN Tx Event FIFO Status Register
   type FDCAN_TXEFS_Register is record
      --  Read-only. Event FIFO Fill Level
      EFFL           : FDCAN_TXEFS_EFFL_Field;
      --  unspecified
      Reserved_3_7   : HAL.UInt5;
      --  Read-only. Event FIFO Get Index.
      EFGI           : FDCAN_TXEFS_EFGI_Field;
      --  unspecified
      Reserved_10_15 : HAL.UInt6;
      --  Read-only. Event FIFO Put Index
      EFPI           : FDCAN_TXEFS_EFPI_Field;
      --  unspecified
      Reserved_18_23 : HAL.UInt6;
      --  Read-only. Event FIFO Full.
      EFF            : Boolean;
      --  Read-only. Tx Event FIFO Element Lost.
      TEFL           : Boolean;
      --  unspecified
      Reserved_26_31 : HAL.UInt6;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TXEFS_Register use record
      EFFL           at 0 range 0 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      EFGI           at 0 range 8 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      EFPI           at 0 range 16 .. 17;
      Reserved_18_23 at 0 range 18 .. 23;
      EFF            at 0 range 24 .. 24;
      TEFL           at 0 range 25 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype FDCAN_TXEFA_EFAI_Field is HAL.UInt2;

   --  FDCAN Tx Event FIFO Acknowledge Register
   type FDCAN_TXEFA_Register is record
      --  Event FIFO Acknowledge Index
      EFAI          : FDCAN_TXEFA_EFAI_Field := 16#0#;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TXEFA_Register use record
      EFAI          at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   subtype FDCAN_CKDIV_PDIV_Field is HAL.UInt4;

   --  FDCAN TT Trigger Memory Configuration Register
   type FDCAN_CKDIV_Register is record
      --  PDIV
      PDIV          : FDCAN_CKDIV_PDIV_Field := 16#0#;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_CKDIV_Register use record
      PDIV          at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  FDCAN1
   type FDCAN_Peripheral is record
      --  FDCAN Core Release Register
      FDCAN_CREL   : aliased FDCAN_CREL_Register;
      --  FDCAN Core Release Register
      FDCAN_ENDN   : aliased HAL.UInt32;
      --  FDCAN Data Bit Timing and Prescaler Register
      FDCAN_DBTP   : aliased FDCAN_DBTP_Register;
      --  FDCAN Test Register
      FDCAN_TEST   : aliased FDCAN_TEST_Register;
      --  FDCAN RAM Watchdog Register
      FDCAN_RWD    : aliased FDCAN_RWD_Register;
      --  FDCAN CC Control Register
      FDCAN_CCCR   : aliased FDCAN_CCCR_Register;
      --  FDCAN Nominal Bit Timing and Prescaler Register
      FDCAN_NBTP   : aliased FDCAN_NBTP_Register;
      --  FDCAN Timestamp Counter Configuration Register
      FDCAN_TSCC   : aliased FDCAN_TSCC_Register;
      --  FDCAN Timestamp Counter Value Register
      FDCAN_TSCV   : aliased FDCAN_TSCV_Register;
      --  FDCAN Timeout Counter Configuration Register
      FDCAN_TOCC   : aliased FDCAN_TOCC_Register;
      --  FDCAN Timeout Counter Value Register
      FDCAN_TOCV   : aliased FDCAN_TOCV_Register;
      --  FDCAN Error Counter Register
      FDCAN_ECR    : aliased FDCAN_ECR_Register;
      --  FDCAN Protocol Status Register
      FDCAN_PSR    : aliased FDCAN_PSR_Register;
      --  FDCAN Transmitter Delay Compensation Register
      FDCAN_TDCR   : aliased FDCAN_TDCR_Register;
      --  FDCAN Interrupt Register
      FDCAN_IR     : aliased FDCAN_IR_Register;
      --  FDCAN Interrupt Enable Register
      FDCAN_IE     : aliased FDCAN_IE_Register;
      --  FDCAN Interrupt Line Select Register
      FDCAN_ILS    : aliased FDCAN_ILS_Register;
      --  FDCAN Interrupt Line Enable Register
      FDCAN_ILE    : aliased FDCAN_ILE_Register;
      --  FDCAN Global Filter Configuration Register
      FDCAN_RXGFC  : aliased FDCAN_RXGFC_Register;
      --  FDCAN Extended ID and Mask Register
      FDCAN_XIDAM  : aliased FDCAN_XIDAM_Register;
      --  FDCAN High Priority Message Status Register
      FDCAN_HPMS   : aliased FDCAN_HPMS_Register;
      --  FDCAN Rx FIFO 0 Status Register
      FDCAN_RXF0S  : aliased FDCAN_RXF0S_Register;
      --  CAN Rx FIFO 0 Acknowledge Register
      FDCAN_RXF0A  : aliased FDCAN_RXF0A_Register;
      --  FDCAN Rx FIFO 1 Status Register
      FDCAN_RXF1S  : aliased FDCAN_RXF1S_Register;
      --  FDCAN Rx FIFO 1 Acknowledge Register
      FDCAN_RXF1A  : aliased FDCAN_RXF1A_Register;
      --  FDCAN Tx buffer configuration register
      FDCAN_TXBC   : aliased FDCAN_TXBC_Register;
      --  FDCAN Tx FIFO/Queue Status Register
      FDCAN_TXFQS  : aliased FDCAN_TXFQS_Register;
      --  FDCAN Tx Buffer Request Pending Register
      FDCAN_TXBRP  : aliased FDCAN_TXBRP_Register;
      --  FDCAN Tx Buffer Add Request Register
      FDCAN_TXBAR  : aliased FDCAN_TXBAR_Register;
      --  FDCAN Tx Buffer Cancellation Request Register
      FDCAN_TXBCR  : aliased FDCAN_TXBCR_Register;
      --  FDCAN Tx Buffer Transmission Occurred Register
      FDCAN_TXBTO  : aliased FDCAN_TXBTO_Register;
      --  FDCAN Tx Buffer Cancellation Finished Register
      FDCAN_TXBCF  : aliased FDCAN_TXBCF_Register;
      --  FDCAN Tx Buffer Transmission Interrupt Enable Register
      FDCAN_TXBTIE : aliased FDCAN_TXBTIE_Register;
      --  FDCAN Tx Buffer Cancellation Finished Interrupt Enable Register
      FDCAN_TXBCIE : aliased FDCAN_TXBCIE_Register;
      --  FDCAN Tx Event FIFO Status Register
      FDCAN_TXEFS  : aliased FDCAN_TXEFS_Register;
      --  FDCAN Tx Event FIFO Acknowledge Register
      FDCAN_TXEFA  : aliased FDCAN_TXEFA_Register;
      --  FDCAN TT Trigger Memory Configuration Register
      FDCAN_CKDIV  : aliased FDCAN_CKDIV_Register;
   end record
     with Volatile;

   for FDCAN_Peripheral use record
      FDCAN_CREL   at 16#0# range 0 .. 31;
      FDCAN_ENDN   at 16#4# range 0 .. 31;
      FDCAN_DBTP   at 16#C# range 0 .. 31;
      FDCAN_TEST   at 16#10# range 0 .. 31;
      FDCAN_RWD    at 16#14# range 0 .. 31;
      FDCAN_CCCR   at 16#18# range 0 .. 31;
      FDCAN_NBTP   at 16#1C# range 0 .. 31;
      FDCAN_TSCC   at 16#20# range 0 .. 31;
      FDCAN_TSCV   at 16#24# range 0 .. 31;
      FDCAN_TOCC   at 16#28# range 0 .. 31;
      FDCAN_TOCV   at 16#2C# range 0 .. 31;
      FDCAN_ECR    at 16#40# range 0 .. 31;
      FDCAN_PSR    at 16#44# range 0 .. 31;
      FDCAN_TDCR   at 16#48# range 0 .. 31;
      FDCAN_IR     at 16#50# range 0 .. 31;
      FDCAN_IE     at 16#54# range 0 .. 31;
      FDCAN_ILS    at 16#58# range 0 .. 31;
      FDCAN_ILE    at 16#5C# range 0 .. 31;
      FDCAN_RXGFC  at 16#80# range 0 .. 31;
      FDCAN_XIDAM  at 16#84# range 0 .. 31;
      FDCAN_HPMS   at 16#88# range 0 .. 31;
      FDCAN_RXF0S  at 16#90# range 0 .. 31;
      FDCAN_RXF0A  at 16#94# range 0 .. 31;
      FDCAN_RXF1S  at 16#98# range 0 .. 31;
      FDCAN_RXF1A  at 16#9C# range 0 .. 31;
      FDCAN_TXBC   at 16#C0# range 0 .. 31;
      FDCAN_TXFQS  at 16#C4# range 0 .. 31;
      FDCAN_TXBRP  at 16#C8# range 0 .. 31;
      FDCAN_TXBAR  at 16#CC# range 0 .. 31;
      FDCAN_TXBCR  at 16#D0# range 0 .. 31;
      FDCAN_TXBTO  at 16#D4# range 0 .. 31;
      FDCAN_TXBCF  at 16#D8# range 0 .. 31;
      FDCAN_TXBTIE at 16#DC# range 0 .. 31;
      FDCAN_TXBCIE at 16#E0# range 0 .. 31;
      FDCAN_TXEFS  at 16#E4# range 0 .. 31;
      FDCAN_TXEFA  at 16#E8# range 0 .. 31;
      FDCAN_CKDIV  at 16#100# range 0 .. 31;
   end record;

   --  FDCAN1
   FDCAN1_Periph : aliased FDCAN_Peripheral
     with Import, Address => System'To_Address (16#4000A400#);

   --  FDCAN1
   SEC_FDCAN1_Periph : aliased FDCAN_Peripheral
     with Import, Address => System'To_Address (16#5000A400#);

end STM32_SVD.FDCAN;
