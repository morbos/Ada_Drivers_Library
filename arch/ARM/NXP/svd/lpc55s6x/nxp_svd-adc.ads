--  Copyright 2016-2019 NXP
--  All rights reserved.SPDX-License-Identifier: BSD-3-Clause


--  This spec has been automatically generated from LPC55S6x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NXP_SVD.ADC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Resolution
   type VERID_RES_Field is
     (
      --  Up to 13-bit differential/12-bit single ended resolution supported.
      Res_0,
      --  Up to 16-bit differential/16-bit single ended resolution supported.
      Res_1)
     with Size => 1;
   for VERID_RES_Field use
     (Res_0 => 0,
      Res_1 => 1);

   --  Differential Supported
   type VERID_DIFFEN_Field is
     (
      --  Differential operation not supported.
      Diffen_0,
      --  Differential operation supported. CMDLa[CTYPE] controls fields
      --  implemented.
      Diffen_1)
     with Size => 1;
   for VERID_DIFFEN_Field use
     (Diffen_0 => 0,
      Diffen_1 => 1);

   --  Multi Vref Implemented
   type VERID_MVI_Field is
     (
      --  Single voltage reference high (VREFH) input supported.
      Mvi_0,
      --  Multiple voltage reference high (VREFH) inputs supported.
      Mvi_1)
     with Size => 1;
   for VERID_MVI_Field use
     (Mvi_0 => 0,
      Mvi_1 => 1);

   --  Channel Scale Width
   type VERID_CSW_Field is
     (
      --  Channel scaling not supported.
      Csw_0,
      --  Channel scaling supported. 1-bit CSCALE control field.
      Csw_1,
      --  Channel scaling supported. 6-bit CSCALE control field.
      Csw_6)
     with Size => 3;
   for VERID_CSW_Field use
     (Csw_0 => 0,
      Csw_1 => 1,
      Csw_6 => 6);

   --  Voltage Reference 1 Range Control Bit Implemented
   type VERID_VR1RNGI_Field is
     (
      --  Range control not required. CFG[VREF1RNG] is not implemented.
      Vr1Rngi_0,
      --  Range control required. CFG[VREF1RNG] is implemented.
      Vr1Rngi_1)
     with Size => 1;
   for VERID_VR1RNGI_Field use
     (Vr1Rngi_0 => 0,
      Vr1Rngi_1 => 1);

   --  Internal ADC Clock implemented
   type VERID_IADCKI_Field is
     (
      --  Internal clock source not implemented.
      Iadcki_0,
      --  Internal clock source (and CFG[ADCKEN]) implemented.
      Iadcki_1)
     with Size => 1;
   for VERID_IADCKI_Field use
     (Iadcki_0 => 0,
      Iadcki_1 => 1);

   --  Calibration Function Implemented
   type VERID_CALOFSI_Field is
     (
      --  Calibration Not Implemented.
      Calofsi_0,
      --  Calibration Implemented.
      Calofsi_1)
     with Size => 1;
   for VERID_CALOFSI_Field use
     (Calofsi_0 => 0,
      Calofsi_1 => 1);

   --  Number of Single Ended Outputs Supported
   type VERID_NUM_SEC_Field is
     (
      --  This design supports one single ended conversion at a time.
      Num_Sec_0,
      --  This design supports two simultanious single ended conversions.
      Num_Sec_1)
     with Size => 1;
   for VERID_NUM_SEC_Field use
     (Num_Sec_0 => 0,
      Num_Sec_1 => 1);

   --  Number of FIFOs
   type VERID_NUM_FIFO_Field is
     (
      --  N/A
      Num_Fifo_0,
      --  This design supports one result FIFO.
      Num_Fifo_1,
      --  This design supports two result FIFOs.
      Num_Fifo_2,
      --  This design supports three result FIFOs.
      Num_Fifo_3,
      --  This design supports four result FIFOs.
      Num_Fifo_4)
     with Size => 3;
   for VERID_NUM_FIFO_Field use
     (Num_Fifo_0 => 0,
      Num_Fifo_1 => 1,
      Num_Fifo_2 => 2,
      Num_Fifo_3 => 3,
      Num_Fifo_4 => 4);

   subtype VERID_MINOR_Field is HAL.UInt8;
   subtype VERID_MAJOR_Field is HAL.UInt8;

   --  Version ID Register
   type VERID_Register is record
      --  Read-only. Resolution
      RES            : VERID_RES_Field;
      --  Read-only. Differential Supported
      DIFFEN         : VERID_DIFFEN_Field;
      --  unspecified
      Reserved_2_2   : HAL.Bit;
      --  Read-only. Multi Vref Implemented
      MVI            : VERID_MVI_Field;
      --  Read-only. Channel Scale Width
      CSW            : VERID_CSW_Field;
      --  unspecified
      Reserved_7_7   : HAL.Bit;
      --  Read-only. Voltage Reference 1 Range Control Bit Implemented
      VR1RNGI        : VERID_VR1RNGI_Field;
      --  Read-only. Internal ADC Clock implemented
      IADCKI         : VERID_IADCKI_Field;
      --  Read-only. Calibration Function Implemented
      CALOFSI        : VERID_CALOFSI_Field;
      --  Read-only. Number of Single Ended Outputs Supported
      NUM_SEC        : VERID_NUM_SEC_Field;
      --  Read-only. Number of FIFOs
      NUM_FIFO       : VERID_NUM_FIFO_Field;
      --  unspecified
      Reserved_15_15 : HAL.Bit;
      --  Read-only. Minor Version Number
      MINOR          : VERID_MINOR_Field;
      --  Read-only. Major Version Number
      MAJOR          : VERID_MAJOR_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for VERID_Register use record
      RES            at 0 range 0 .. 0;
      DIFFEN         at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      MVI            at 0 range 3 .. 3;
      CSW            at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      VR1RNGI        at 0 range 8 .. 8;
      IADCKI         at 0 range 9 .. 9;
      CALOFSI        at 0 range 10 .. 10;
      NUM_SEC        at 0 range 11 .. 11;
      NUM_FIFO       at 0 range 12 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      MINOR          at 0 range 16 .. 23;
      MAJOR          at 0 range 24 .. 31;
   end record;

   subtype PARAM_TRIG_NUM_Field is HAL.UInt8;

   --  Result FIFO Depth
   type PARAM_FIFOSIZE_Field is
     (
      --  Result FIFO depth = 1 dataword.
      Fifosize_1,
      --  Result FIFO depth = 4 datawords.
      Fifosize_4,
      --  Result FIFO depth = 8 datawords.
      Fifosize_8,
      --  Result FIFO depth = 16 datawords.
      Fifosize_16,
      --  Result FIFO depth = 32 datawords.
      Fifosize_32,
      --  Result FIFO depth = 64 datawords.
      Fifosize_64)
     with Size => 8;
   for PARAM_FIFOSIZE_Field use
     (Fifosize_1 => 1,
      Fifosize_4 => 4,
      Fifosize_8 => 8,
      Fifosize_16 => 16,
      Fifosize_32 => 32,
      Fifosize_64 => 64);

   subtype PARAM_CV_NUM_Field is HAL.UInt8;
   subtype PARAM_CMD_NUM_Field is HAL.UInt8;

   --  Parameter Register
   type PARAM_Register is record
      --  Read-only. Trigger Number
      TRIG_NUM : PARAM_TRIG_NUM_Field;
      --  Read-only. Result FIFO Depth
      FIFOSIZE : PARAM_FIFOSIZE_Field;
      --  Read-only. Compare Value Number
      CV_NUM   : PARAM_CV_NUM_Field;
      --  Read-only. Command Buffer Number
      CMD_NUM  : PARAM_CMD_NUM_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PARAM_Register use record
      TRIG_NUM at 0 range 0 .. 7;
      FIFOSIZE at 0 range 8 .. 15;
      CV_NUM   at 0 range 16 .. 23;
      CMD_NUM  at 0 range 24 .. 31;
   end record;

   --  ADC Enable
   type CTRL_ADCEN_Field is
     (
      --  ADC is disabled.
      Adcen_0,
      --  ADC is enabled.
      Adcen_1)
     with Size => 1;
   for CTRL_ADCEN_Field use
     (Adcen_0 => 0,
      Adcen_1 => 1);

   --  Software Reset
   type CTRL_RST_Field is
     (
      --  ADC logic is not reset.
      Rst_0,
      --  ADC logic is reset.
      Rst_1)
     with Size => 1;
   for CTRL_RST_Field use
     (Rst_0 => 0,
      Rst_1 => 1);

   --  Doze Enable
   type CTRL_DOZEN_Field is
     (
      --  ADC is enabled in Doze mode.
      Dozen_0,
      --  ADC is disabled in Doze mode.
      Dozen_1)
     with Size => 1;
   for CTRL_DOZEN_Field use
     (Dozen_0 => 0,
      Dozen_1 => 1);

   --  Auto-Calibration Request
   type CTRL_CAL_REQ_Field is
     (
      --  No request for auto-calibration has been made.
      Cal_Req_0,
      --  A request for auto-calibration has been made
      Cal_Req_1)
     with Size => 1;
   for CTRL_CAL_REQ_Field use
     (Cal_Req_0 => 0,
      Cal_Req_1 => 1);

   --  Configure for offset calibration function
   type CTRL_CALOFS_Field is
     (
      --  Calibration function disabled
      Calofs_0,
      --  Request for offset calibration function
      Calofs_1)
     with Size => 1;
   for CTRL_CALOFS_Field use
     (Calofs_0 => 0,
      Calofs_1 => 1);

   --  Reset FIFO 0
   type CTRL_RSTFIFO0_Field is
     (
      --  No effect.
      Rstfifo0_0,
      --  FIFO 0 is reset.
      Rstfifo0_1)
     with Size => 1;
   for CTRL_RSTFIFO0_Field use
     (Rstfifo0_0 => 0,
      Rstfifo0_1 => 1);

   --  Reset FIFO 1
   type CTRL_RSTFIFO1_Field is
     (
      --  No effect.
      Rstfifo1_0,
      --  FIFO 1 is reset.
      Rstfifo1_1)
     with Size => 1;
   for CTRL_RSTFIFO1_Field use
     (Rstfifo1_0 => 0,
      Rstfifo1_1 => 1);

   --  Auto-Calibration Averages
   type CTRL_CAL_AVGS_Field is
     (
      --  Single conversion.
      Cal_Avgs_0,
      --  2 conversions averaged.
      Cal_Avgs_1,
      --  4 conversions averaged.
      Cal_Avgs_2,
      --  8 conversions averaged.
      Cal_Avgs_3,
      --  16 conversions averaged.
      Cal_Avgs_4,
      --  32 conversions averaged.
      Cal_Avgs_5,
      --  64 conversions averaged.
      Cal_Avgs_6,
      --  128 conversions averaged.
      Cal_Avgs_7)
     with Size => 3;
   for CTRL_CAL_AVGS_Field use
     (Cal_Avgs_0 => 0,
      Cal_Avgs_1 => 1,
      Cal_Avgs_2 => 2,
      Cal_Avgs_3 => 3,
      Cal_Avgs_4 => 4,
      Cal_Avgs_5 => 5,
      Cal_Avgs_6 => 6,
      Cal_Avgs_7 => 7);

   --  ADC Control Register
   type CTRL_Register is record
      --  ADC Enable
      ADCEN          : CTRL_ADCEN_Field := NXP_SVD.ADC.Adcen_0;
      --  Software Reset
      RST            : CTRL_RST_Field := NXP_SVD.ADC.Rst_0;
      --  Doze Enable
      DOZEN          : CTRL_DOZEN_Field := NXP_SVD.ADC.Dozen_0;
      --  Auto-Calibration Request
      CAL_REQ        : CTRL_CAL_REQ_Field := NXP_SVD.ADC.Cal_Req_0;
      --  Configure for offset calibration function
      CALOFS         : CTRL_CALOFS_Field := NXP_SVD.ADC.Calofs_0;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  Reset FIFO 0
      RSTFIFO0       : CTRL_RSTFIFO0_Field := NXP_SVD.ADC.Rstfifo0_0;
      --  Reset FIFO 1
      RSTFIFO1       : CTRL_RSTFIFO1_Field := NXP_SVD.ADC.Rstfifo1_0;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Auto-Calibration Averages
      CAL_AVGS       : CTRL_CAL_AVGS_Field := NXP_SVD.ADC.Cal_Avgs_0;
      --  unspecified
      Reserved_19_31 : HAL.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CTRL_Register use record
      ADCEN          at 0 range 0 .. 0;
      RST            at 0 range 1 .. 1;
      DOZEN          at 0 range 2 .. 2;
      CAL_REQ        at 0 range 3 .. 3;
      CALOFS         at 0 range 4 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      RSTFIFO0       at 0 range 8 .. 8;
      RSTFIFO1       at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      CAL_AVGS       at 0 range 16 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   --  Result FIFO 0 Ready Flag
   type STAT_RDY0_Field is
     (
      --  Result FIFO 0 data level not above watermark level.
      Rdy0_0,
      --  Result FIFO 0 holding data above watermark level.
      Rdy0_1)
     with Size => 1;
   for STAT_RDY0_Field use
     (Rdy0_0 => 0,
      Rdy0_1 => 1);

   --  Result FIFO 0 Overflow Flag
   type STAT_FOF0_Field is
     (
      --  No result FIFO 0 overflow has occurred since the last time the flag
      --  was cleared.
      Fof0_0,
      --  At least one result FIFO 0 overflow has occurred since the last time
      --  the flag was cleared.
      Fof0_1)
     with Size => 1;
   for STAT_FOF0_Field use
     (Fof0_0 => 0,
      Fof0_1 => 1);

   --  Result FIFO1 Ready Flag
   type STAT_RDY1_Field is
     (
      --  Result FIFO1 data level not above watermark level.
      Rdy1_0,
      --  Result FIFO1 holding data above watermark level.
      Rdy1_1)
     with Size => 1;
   for STAT_RDY1_Field use
     (Rdy1_0 => 0,
      Rdy1_1 => 1);

   --  Result FIFO1 Overflow Flag
   type STAT_FOF1_Field is
     (
      --  No result FIFO1 overflow has occurred since the last time the flag
      --  was cleared.
      Fof1_0,
      --  At least one result FIFO1 overflow has occurred since the last time
      --  the flag was cleared.
      Fof1_1)
     with Size => 1;
   for STAT_FOF1_Field use
     (Fof1_0 => 0,
      Fof1_1 => 1);

   --  Interrupt Flag For High Priority Trigger Exception
   type STAT_TEXC_INT_Field is
     (
      --  No trigger exceptions have occurred.
      Texc_Int_0,
      --  A trigger exception has occurred and is pending acknowledgement.
      Texc_Int_1)
     with Size => 1;
   for STAT_TEXC_INT_Field use
     (Texc_Int_0 => 0,
      Texc_Int_1 => 1);

   --  Interrupt Flag For Trigger Completion
   type STAT_TCOMP_INT_Field is
     (
      --  Either IE[TCOMP_IE] is set to 0, or no trigger sequences have run to
      --  completion.
      Tcomp_Int_0,
      --  Trigger sequence has been completed and all data is stored in the
      --  associated FIFO.
      Tcomp_Int_1)
     with Size => 1;
   for STAT_TCOMP_INT_Field use
     (Tcomp_Int_0 => 0,
      Tcomp_Int_1 => 1);

   --  Calibration Ready
   type STAT_CAL_RDY_Field is
     (
      --  Calibration is incomplete or hasn't been ran.
      Cal_Rdy_0,
      --  The ADC is calibrated.
      Cal_Rdy_1)
     with Size => 1;
   for STAT_CAL_RDY_Field use
     (Cal_Rdy_0 => 0,
      Cal_Rdy_1 => 1);

   --  ADC Active
   type STAT_ADC_ACTIVE_Field is
     (
      --  The ADC is IDLE. There are no pending triggers to service and no
      --  active commands are being processed.
      Adc_Active_0,
      --  The ADC is processing a conversion, running through the power up
      --  delay, or servicing a trigger.
      Adc_Active_1)
     with Size => 1;
   for STAT_ADC_ACTIVE_Field use
     (Adc_Active_0 => 0,
      Adc_Active_1 => 1);

   --  Trigger Active
   type STAT_TRGACT_Field is
     (
      --  Command (sequence) associated with Trigger 0 currently being
      --  executed.
      Trgact_0,
      --  Command (sequence) associated with Trigger 1 currently being
      --  executed.
      Trgact_1,
      --  Command (sequence) associated with Trigger 2 currently being
      --  executed.
      Trgact_2,
      --  Command (sequence) from the associated Trigger number is currently
      --  being executed.
      Trgact_3,
      --  Command (sequence) from the associated Trigger number is currently
      --  being executed.
      Trgact_4,
      --  Command (sequence) from the associated Trigger number is currently
      --  being executed.
      Trgact_5,
      --  Command (sequence) from the associated Trigger number is currently
      --  being executed.
      Trgact_6,
      --  Command (sequence) from the associated Trigger number is currently
      --  being executed.
      Trgact_7,
      --  Command (sequence) from the associated Trigger number is currently
      --  being executed.
      Trgact_8,
      --  Command (sequence) from the associated Trigger number is currently
      --  being executed.
      Trgact_9)
     with Size => 4;
   for STAT_TRGACT_Field use
     (Trgact_0 => 0,
      Trgact_1 => 1,
      Trgact_2 => 2,
      Trgact_3 => 3,
      Trgact_4 => 4,
      Trgact_5 => 5,
      Trgact_6 => 6,
      Trgact_7 => 7,
      Trgact_8 => 8,
      Trgact_9 => 9);

   --  Command Active
   type STAT_CMDACT_Field is
     (
      --  No command is currently in progress.
      Cmdact_0,
      --  Command 1 currently being executed.
      Cmdact_1,
      --  Command 2 currently being executed.
      Cmdact_2,
      --  Associated command number is currently being executed.
      Cmdact_3,
      --  Associated command number is currently being executed.
      Cmdact_4,
      --  Associated command number is currently being executed.
      Cmdact_5,
      --  Associated command number is currently being executed.
      Cmdact_6,
      --  Associated command number is currently being executed.
      Cmdact_7,
      --  Associated command number is currently being executed.
      Cmdact_8,
      --  Associated command number is currently being executed.
      Cmdact_9)
     with Size => 4;
   for STAT_CMDACT_Field use
     (Cmdact_0 => 0,
      Cmdact_1 => 1,
      Cmdact_2 => 2,
      Cmdact_3 => 3,
      Cmdact_4 => 4,
      Cmdact_5 => 5,
      Cmdact_6 => 6,
      Cmdact_7 => 7,
      Cmdact_8 => 8,
      Cmdact_9 => 9);

   --  ADC Status Register
   type STAT_Register is record
      --  Read-only. Result FIFO 0 Ready Flag
      RDY0           : STAT_RDY0_Field := NXP_SVD.ADC.Rdy0_0;
      --  Write data bit of one shall clear (set to zero) the corresponding bit
      --  in the field. Result FIFO 0 Overflow Flag
      FOF0           : STAT_FOF0_Field := NXP_SVD.ADC.Fof0_0;
      --  Read-only. Result FIFO1 Ready Flag
      RDY1           : STAT_RDY1_Field := NXP_SVD.ADC.Rdy1_0;
      --  Write data bit of one shall clear (set to zero) the corresponding bit
      --  in the field. Result FIFO1 Overflow Flag
      FOF1           : STAT_FOF1_Field := NXP_SVD.ADC.Fof1_0;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Write data bit of one shall clear (set to zero) the corresponding bit
      --  in the field. Interrupt Flag For High Priority Trigger Exception
      TEXC_INT       : STAT_TEXC_INT_Field := NXP_SVD.ADC.Texc_Int_0;
      --  Write data bit of one shall clear (set to zero) the corresponding bit
      --  in the field. Interrupt Flag For Trigger Completion
      TCOMP_INT      : STAT_TCOMP_INT_Field := NXP_SVD.ADC.Tcomp_Int_0;
      --  Read-only. Calibration Ready
      CAL_RDY        : STAT_CAL_RDY_Field := NXP_SVD.ADC.Cal_Rdy_0;
      --  Read-only. ADC Active
      ADC_ACTIVE     : STAT_ADC_ACTIVE_Field := NXP_SVD.ADC.Adc_Active_0;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Read-only. Trigger Active
      TRGACT         : STAT_TRGACT_Field := NXP_SVD.ADC.Trgact_0;
      --  unspecified
      Reserved_20_23 : HAL.UInt4 := 16#0#;
      --  Read-only. Command Active
      CMDACT         : STAT_CMDACT_Field := NXP_SVD.ADC.Cmdact_0;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for STAT_Register use record
      RDY0           at 0 range 0 .. 0;
      FOF0           at 0 range 1 .. 1;
      RDY1           at 0 range 2 .. 2;
      FOF1           at 0 range 3 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      TEXC_INT       at 0 range 8 .. 8;
      TCOMP_INT      at 0 range 9 .. 9;
      CAL_RDY        at 0 range 10 .. 10;
      ADC_ACTIVE     at 0 range 11 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      TRGACT         at 0 range 16 .. 19;
      Reserved_20_23 at 0 range 20 .. 23;
      CMDACT         at 0 range 24 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  FIFO 0 Watermark Interrupt Enable
   type IE_FWMIE0_Field is
     (
      --  FIFO 0 watermark interrupts are not enabled.
      Fwmie0_0,
      --  FIFO 0 watermark interrupts are enabled.
      Fwmie0_1)
     with Size => 1;
   for IE_FWMIE0_Field use
     (Fwmie0_0 => 0,
      Fwmie0_1 => 1);

   --  Result FIFO 0 Overflow Interrupt Enable
   type IE_FOFIE0_Field is
     (
      --  FIFO 0 overflow interrupts are not enabled.
      Fofie0_0,
      --  FIFO 0 overflow interrupts are enabled.
      Fofie0_1)
     with Size => 1;
   for IE_FOFIE0_Field use
     (Fofie0_0 => 0,
      Fofie0_1 => 1);

   --  FIFO1 Watermark Interrupt Enable
   type IE_FWMIE1_Field is
     (
      --  FIFO1 watermark interrupts are not enabled.
      Fwmie1_0,
      --  FIFO1 watermark interrupts are enabled.
      Fwmie1_1)
     with Size => 1;
   for IE_FWMIE1_Field use
     (Fwmie1_0 => 0,
      Fwmie1_1 => 1);

   --  Result FIFO1 Overflow Interrupt Enable
   type IE_FOFIE1_Field is
     (
      --  No result FIFO1 overflow has occurred since the last time the flag
      --  was cleared.
      Fofie1_0,
      --  At least one result FIFO1 overflow has occurred since the last time
      --  the flag was cleared.
      Fofie1_1)
     with Size => 1;
   for IE_FOFIE1_Field use
     (Fofie1_0 => 0,
      Fofie1_1 => 1);

   --  Trigger Exception Interrupt Enable
   type IE_TEXC_IE_Field is
     (
      --  Trigger exception interrupts are disabled.
      Texc_Ie_0,
      --  Trigger exception interrupts are enabled.
      Texc_Ie_1)
     with Size => 1;
   for IE_TEXC_IE_Field use
     (Texc_Ie_0 => 0,
      Texc_Ie_1 => 1);

   --  Trigger Completion Interrupt Enable
   type IE_TCOMP_IE_Field is
     (
      --  Trigger completion interrupts are disabled.
      Tcomp_Ie_0,
      --  Trigger completion interrupts are enabled for trigger source 0 only.
      Tcomp_Ie_1,
      --  Trigger completion interrupts are enabled for trigger source 1 only.
      Tcomp_Ie_2,
      --  Associated trigger completion interrupts are enabled.
      Tcomp_Ie_3,
      --  Associated trigger completion interrupts are enabled.
      Tcomp_Ie_4,
      --  Associated trigger completion interrupts are enabled.
      Tcomp_Ie_5,
      --  Associated trigger completion interrupts are enabled.
      Tcomp_Ie_6,
      --  Associated trigger completion interrupts are enabled.
      Tcomp_Ie_7,
      --  Associated trigger completion interrupts are enabled.
      Tcomp_Ie_8,
      --  Associated trigger completion interrupts are enabled.
      Tcomp_Ie_9,
      --  Trigger completion interrupts are enabled for every trigger source.
      Tcomp_Ie_65535)
     with Size => 16;
   for IE_TCOMP_IE_Field use
     (Tcomp_Ie_0 => 0,
      Tcomp_Ie_1 => 1,
      Tcomp_Ie_2 => 2,
      Tcomp_Ie_3 => 3,
      Tcomp_Ie_4 => 4,
      Tcomp_Ie_5 => 5,
      Tcomp_Ie_6 => 6,
      Tcomp_Ie_7 => 7,
      Tcomp_Ie_8 => 8,
      Tcomp_Ie_9 => 9,
      Tcomp_Ie_65535 => 65535);

   --  Interrupt Enable Register
   type IE_Register is record
      --  FIFO 0 Watermark Interrupt Enable
      FWMIE0        : IE_FWMIE0_Field := NXP_SVD.ADC.Fwmie0_0;
      --  Result FIFO 0 Overflow Interrupt Enable
      FOFIE0        : IE_FOFIE0_Field := NXP_SVD.ADC.Fofie0_0;
      --  FIFO1 Watermark Interrupt Enable
      FWMIE1        : IE_FWMIE1_Field := NXP_SVD.ADC.Fwmie1_0;
      --  Result FIFO1 Overflow Interrupt Enable
      FOFIE1        : IE_FOFIE1_Field := NXP_SVD.ADC.Fofie1_0;
      --  unspecified
      Reserved_4_7  : HAL.UInt4 := 16#0#;
      --  Trigger Exception Interrupt Enable
      TEXC_IE       : IE_TEXC_IE_Field := NXP_SVD.ADC.Texc_Ie_0;
      --  unspecified
      Reserved_9_15 : HAL.UInt7 := 16#0#;
      --  Trigger Completion Interrupt Enable
      TCOMP_IE      : IE_TCOMP_IE_Field := NXP_SVD.ADC.Tcomp_Ie_0;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IE_Register use record
      FWMIE0        at 0 range 0 .. 0;
      FOFIE0        at 0 range 1 .. 1;
      FWMIE1        at 0 range 2 .. 2;
      FOFIE1        at 0 range 3 .. 3;
      Reserved_4_7  at 0 range 4 .. 7;
      TEXC_IE       at 0 range 8 .. 8;
      Reserved_9_15 at 0 range 9 .. 15;
      TCOMP_IE      at 0 range 16 .. 31;
   end record;

   --  FIFO 0 Watermark DMA Enable
   type DE_FWMDE0_Field is
     (
      --  DMA request disabled.
      Fwmde0_0,
      --  DMA request enabled.
      Fwmde0_1)
     with Size => 1;
   for DE_FWMDE0_Field use
     (Fwmde0_0 => 0,
      Fwmde0_1 => 1);

   --  FIFO1 Watermark DMA Enable
   type DE_FWMDE1_Field is
     (
      --  DMA request disabled.
      Fwmde1_0,
      --  DMA request enabled.
      Fwmde1_1)
     with Size => 1;
   for DE_FWMDE1_Field use
     (Fwmde1_0 => 0,
      Fwmde1_1 => 1);

   --  DMA Enable Register
   type DE_Register is record
      --  FIFO 0 Watermark DMA Enable
      FWMDE0        : DE_FWMDE0_Field := NXP_SVD.ADC.Fwmde0_0;
      --  FIFO1 Watermark DMA Enable
      FWMDE1        : DE_FWMDE1_Field := NXP_SVD.ADC.Fwmde1_0;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DE_Register use record
      FWMDE0        at 0 range 0 .. 0;
      FWMDE1        at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  ADC trigger priority control
   type CFG_TPRICTRL_Field is
     (
      --  If a higher priority trigger is detected during command processing,
      --  the current conversion is aborted and the new command specified by
      --  the trigger is started.
      Tprictrl_0,
      --  If a higher priority trigger is received during command processing,
      --  the current command is stopped after after completing the current
      --  conversion. If averaging is enabled, the averaging loop will be
      --  completed. However, CMDHa[LOOP] will be ignored and the higher
      --  priority trigger will be serviced.
      Tprictrl_1,
      --  If a higher priority trigger is received during command processing,
      --  the current command will be completed (averaging, looping, compare)
      --  before servicing the higher priority trigger.
      Tprictrl_2)
     with Size => 2;
   for CFG_TPRICTRL_Field use
     (Tprictrl_0 => 0,
      Tprictrl_1 => 1,
      Tprictrl_2 => 2);

   --  Power Configuration Select
   type CFG_PWRSEL_Field is
     (
      --  Lowest power setting.
      Pwrsel_0,
      --  Higher power setting than 0b0.
      Pwrsel_1,
      --  Higher power setting than 0b1.
      Pwrsel_2,
      --  Highest power setting.
      Pwrsel_3)
     with Size => 2;
   for CFG_PWRSEL_Field use
     (Pwrsel_0 => 0,
      Pwrsel_1 => 1,
      Pwrsel_2 => 2,
      Pwrsel_3 => 3);

   --  Voltage Reference Selection
   type CFG_REFSEL_Field is
     (
      --  (Default) Option 1 setting.
      Refsel_0,
      --  Option 2 setting.
      Refsel_1,
      --  Option 3 setting.
      Refsel_2)
     with Size => 2;
   for CFG_REFSEL_Field use
     (Refsel_0 => 0,
      Refsel_1 => 1,
      Refsel_2 => 2);

   --  Trigger Resume Enable
   type CFG_TRES_Field is
     (
      --  Trigger sequences interrupted by a high priority trigger exception
      --  will not be automatically resumed or restarted.
      Tres_0,
      --  Trigger sequences interrupted by a high priority trigger exception
      --  will be automatically resumed or restarted.
      Tres_1)
     with Size => 1;
   for CFG_TRES_Field use
     (Tres_0 => 0,
      Tres_1 => 1);

   --  Trigger Command Resume
   type CFG_TCMDRES_Field is
     (
      --  Trigger sequences interrupted by a high priority trigger exception
      --  will be automatically restarted.
      Tcmdres_0,
      --  Trigger sequences interrupted by a high priority trigger exception
      --  will be resumed from the command executing before the exception.
      Tcmdres_1)
     with Size => 1;
   for CFG_TCMDRES_Field use
     (Tcmdres_0 => 0,
      Tcmdres_1 => 1);

   --  High Priority Trigger Exception Disable
   type CFG_HPT_EXDI_Field is
     (
      --  High priority trigger exceptions are enabled.
      Hpt_Exdi_0,
      --  High priority trigger exceptions are disabled.
      Hpt_Exdi_1)
     with Size => 1;
   for CFG_HPT_EXDI_Field use
     (Hpt_Exdi_0 => 0,
      Hpt_Exdi_1 => 1);

   subtype CFG_PUDLY_Field is HAL.UInt8;

   --  ADC Analog Pre-Enable
   type CFG_PWREN_Field is
     (
      --  ADC analog circuits are only enabled while conversions are active.
      --  Performance is affected due to analog startup delays.
      Pwren_0,
      --  ADC analog circuits are pre-enabled and ready to execute conversions
      --  without startup delays (at the cost of higher DC current
      --  consumption). A single power up delay (CFG[PUDLY]) is executed
      --  immediately once PWREN is set, and any detected trigger does not
      --  begin ADC operation until the power up delay time has passed. After
      --  this initial delay expires the analog will remain pre-enabled, and no
      --  additional delays will be executed.
      Pwren_1)
     with Size => 1;
   for CFG_PWREN_Field use
     (Pwren_0 => 0,
      Pwren_1 => 1);

   --  ADC Configuration Register
   type CFG_Register is record
      --  ADC trigger priority control
      TPRICTRL       : CFG_TPRICTRL_Field := NXP_SVD.ADC.Tprictrl_0;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  Power Configuration Select
      PWRSEL         : CFG_PWRSEL_Field := NXP_SVD.ADC.Pwrsel_0;
      --  Voltage Reference Selection
      REFSEL         : CFG_REFSEL_Field := NXP_SVD.ADC.Refsel_0;
      --  Trigger Resume Enable
      TRES           : CFG_TRES_Field := NXP_SVD.ADC.Tres_0;
      --  Trigger Command Resume
      TCMDRES        : CFG_TCMDRES_Field := NXP_SVD.ADC.Tcmdres_0;
      --  High Priority Trigger Exception Disable
      HPT_EXDI       : CFG_HPT_EXDI_Field := NXP_SVD.ADC.Hpt_Exdi_0;
      --  unspecified
      Reserved_11_15 : HAL.UInt5 := 16#0#;
      --  Power Up Delay
      PUDLY          : CFG_PUDLY_Field := 16#80#;
      --  unspecified
      Reserved_24_27 : HAL.UInt4 := 16#0#;
      --  ADC Analog Pre-Enable
      PWREN          : CFG_PWREN_Field := NXP_SVD.ADC.Pwren_0;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CFG_Register use record
      TPRICTRL       at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      PWRSEL         at 0 range 4 .. 5;
      REFSEL         at 0 range 6 .. 7;
      TRES           at 0 range 8 .. 8;
      TCMDRES        at 0 range 9 .. 9;
      HPT_EXDI       at 0 range 10 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
      PUDLY          at 0 range 16 .. 23;
      Reserved_24_27 at 0 range 24 .. 27;
      PWREN          at 0 range 28 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype PAUSE_PAUSEDLY_Field is HAL.UInt9;

   --  PAUSE Option Enable
   type PAUSE_PAUSEEN_Field is
     (
      --  Pause operation disabled
      Pauseen_0,
      --  Pause operation enabled
      Pauseen_1)
     with Size => 1;
   for PAUSE_PAUSEEN_Field use
     (Pauseen_0 => 0,
      Pauseen_1 => 1);

   --  ADC Pause Register
   type PAUSE_Register is record
      --  Pause Delay
      PAUSEDLY      : PAUSE_PAUSEDLY_Field := 16#0#;
      --  unspecified
      Reserved_9_30 : HAL.UInt22 := 16#0#;
      --  PAUSE Option Enable
      PAUSEEN       : PAUSE_PAUSEEN_Field := NXP_SVD.ADC.Pauseen_0;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PAUSE_Register use record
      PAUSEDLY      at 0 range 0 .. 8;
      Reserved_9_30 at 0 range 9 .. 30;
      PAUSEEN       at 0 range 31 .. 31;
   end record;

   --  Software trigger 0 event
   type SWTRIG_SWT0_Field is
     (
      --  No trigger 0 event generated.
      Swt0_0,
      --  Trigger 0 event generated.
      Swt0_1)
     with Size => 1;
   for SWTRIG_SWT0_Field use
     (Swt0_0 => 0,
      Swt0_1 => 1);

   --  Software trigger 1 event
   type SWTRIG_SWT1_Field is
     (
      --  No trigger 1 event generated.
      Swt1_0,
      --  Trigger 1 event generated.
      Swt1_1)
     with Size => 1;
   for SWTRIG_SWT1_Field use
     (Swt1_0 => 0,
      Swt1_1 => 1);

   --  Software trigger 2 event
   type SWTRIG_SWT2_Field is
     (
      --  No trigger 2 event generated.
      Swt2_0,
      --  Trigger 2 event generated.
      Swt2_1)
     with Size => 1;
   for SWTRIG_SWT2_Field use
     (Swt2_0 => 0,
      Swt2_1 => 1);

   --  Software trigger 3 event
   type SWTRIG_SWT3_Field is
     (
      --  No trigger 3 event generated.
      Swt3_0,
      --  Trigger 3 event generated.
      Swt3_1)
     with Size => 1;
   for SWTRIG_SWT3_Field use
     (Swt3_0 => 0,
      Swt3_1 => 1);

   --  Software trigger 4 event
   type SWTRIG_SWT4_Field is
     (
      --  No trigger 4 event generated.
      Swt4_0,
      --  Trigger 4 event generated.
      Swt4_1)
     with Size => 1;
   for SWTRIG_SWT4_Field use
     (Swt4_0 => 0,
      Swt4_1 => 1);

   --  Software trigger 5 event
   type SWTRIG_SWT5_Field is
     (
      --  No trigger 5 event generated.
      Swt5_0,
      --  Trigger 5 event generated.
      Swt5_1)
     with Size => 1;
   for SWTRIG_SWT5_Field use
     (Swt5_0 => 0,
      Swt5_1 => 1);

   --  Software trigger 6 event
   type SWTRIG_SWT6_Field is
     (
      --  No trigger 6 event generated.
      Swt6_0,
      --  Trigger 6 event generated.
      Swt6_1)
     with Size => 1;
   for SWTRIG_SWT6_Field use
     (Swt6_0 => 0,
      Swt6_1 => 1);

   --  Software trigger 7 event
   type SWTRIG_SWT7_Field is
     (
      --  No trigger 7 event generated.
      Swt7_0,
      --  Trigger 7 event generated.
      Swt7_1)
     with Size => 1;
   for SWTRIG_SWT7_Field use
     (Swt7_0 => 0,
      Swt7_1 => 1);

   --  Software trigger 8 event
   type SWTRIG_SWT8_Field is
     (
      --  No trigger 8 event generated.
      Swt8_0,
      --  Trigger 8 event generated.
      Swt8_1)
     with Size => 1;
   for SWTRIG_SWT8_Field use
     (Swt8_0 => 0,
      Swt8_1 => 1);

   --  Software trigger 9 event
   type SWTRIG_SWT9_Field is
     (
      --  No trigger 9 event generated.
      Swt9_0,
      --  Trigger 9 event generated.
      Swt9_1)
     with Size => 1;
   for SWTRIG_SWT9_Field use
     (Swt9_0 => 0,
      Swt9_1 => 1);

   --  Software trigger 10 event
   type SWTRIG_SWT10_Field is
     (
      --  No trigger 10 event generated.
      Swt10_0,
      --  Trigger 10 event generated.
      Swt10_1)
     with Size => 1;
   for SWTRIG_SWT10_Field use
     (Swt10_0 => 0,
      Swt10_1 => 1);

   --  Software trigger 11 event
   type SWTRIG_SWT11_Field is
     (
      --  No trigger 11 event generated.
      Swt11_0,
      --  Trigger 11 event generated.
      Swt11_1)
     with Size => 1;
   for SWTRIG_SWT11_Field use
     (Swt11_0 => 0,
      Swt11_1 => 1);

   --  Software trigger 12 event
   type SWTRIG_SWT12_Field is
     (
      --  No trigger 12 event generated.
      Swt12_0,
      --  Trigger 12 event generated.
      Swt12_1)
     with Size => 1;
   for SWTRIG_SWT12_Field use
     (Swt12_0 => 0,
      Swt12_1 => 1);

   --  Software trigger 13 event
   type SWTRIG_SWT13_Field is
     (
      --  No trigger 13 event generated.
      Swt13_0,
      --  Trigger 13 event generated.
      Swt13_1)
     with Size => 1;
   for SWTRIG_SWT13_Field use
     (Swt13_0 => 0,
      Swt13_1 => 1);

   --  Software trigger 14 event
   type SWTRIG_SWT14_Field is
     (
      --  No trigger 14 event generated.
      Swt14_0,
      --  Trigger 14 event generated.
      Swt14_1)
     with Size => 1;
   for SWTRIG_SWT14_Field use
     (Swt14_0 => 0,
      Swt14_1 => 1);

   --  Software trigger 15 event
   type SWTRIG_SWT15_Field is
     (
      --  No trigger 15 event generated.
      Swt15_0,
      --  Trigger 15 event generated.
      Swt15_1)
     with Size => 1;
   for SWTRIG_SWT15_Field use
     (Swt15_0 => 0,
      Swt15_1 => 1);

   --  Software Trigger Register
   type SWTRIG_Register is record
      --  Software trigger 0 event
      SWT0           : SWTRIG_SWT0_Field := NXP_SVD.ADC.Swt0_0;
      --  Software trigger 1 event
      SWT1           : SWTRIG_SWT1_Field := NXP_SVD.ADC.Swt1_0;
      --  Software trigger 2 event
      SWT2           : SWTRIG_SWT2_Field := NXP_SVD.ADC.Swt2_0;
      --  Software trigger 3 event
      SWT3           : SWTRIG_SWT3_Field := NXP_SVD.ADC.Swt3_0;
      --  Software trigger 4 event
      SWT4           : SWTRIG_SWT4_Field := NXP_SVD.ADC.Swt4_0;
      --  Software trigger 5 event
      SWT5           : SWTRIG_SWT5_Field := NXP_SVD.ADC.Swt5_0;
      --  Software trigger 6 event
      SWT6           : SWTRIG_SWT6_Field := NXP_SVD.ADC.Swt6_0;
      --  Software trigger 7 event
      SWT7           : SWTRIG_SWT7_Field := NXP_SVD.ADC.Swt7_0;
      --  Software trigger 8 event
      SWT8           : SWTRIG_SWT8_Field := NXP_SVD.ADC.Swt8_0;
      --  Software trigger 9 event
      SWT9           : SWTRIG_SWT9_Field := NXP_SVD.ADC.Swt9_0;
      --  Software trigger 10 event
      SWT10          : SWTRIG_SWT10_Field := NXP_SVD.ADC.Swt10_0;
      --  Software trigger 11 event
      SWT11          : SWTRIG_SWT11_Field := NXP_SVD.ADC.Swt11_0;
      --  Software trigger 12 event
      SWT12          : SWTRIG_SWT12_Field := NXP_SVD.ADC.Swt12_0;
      --  Software trigger 13 event
      SWT13          : SWTRIG_SWT13_Field := NXP_SVD.ADC.Swt13_0;
      --  Software trigger 14 event
      SWT14          : SWTRIG_SWT14_Field := NXP_SVD.ADC.Swt14_0;
      --  Software trigger 15 event
      SWT15          : SWTRIG_SWT15_Field := NXP_SVD.ADC.Swt15_0;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SWTRIG_Register use record
      SWT0           at 0 range 0 .. 0;
      SWT1           at 0 range 1 .. 1;
      SWT2           at 0 range 2 .. 2;
      SWT3           at 0 range 3 .. 3;
      SWT4           at 0 range 4 .. 4;
      SWT5           at 0 range 5 .. 5;
      SWT6           at 0 range 6 .. 6;
      SWT7           at 0 range 7 .. 7;
      SWT8           at 0 range 8 .. 8;
      SWT9           at 0 range 9 .. 9;
      SWT10          at 0 range 10 .. 10;
      SWT11          at 0 range 11 .. 11;
      SWT12          at 0 range 12 .. 12;
      SWT13          at 0 range 13 .. 13;
      SWT14          at 0 range 14 .. 14;
      SWT15          at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  Trigger Exception Number
   type TSTAT_TEXC_NUM_Field is
     (
      --  No triggers have been interrupted by a high priority exception. Or
      --  CFG[TRES] = 1.
      Texc_Num_0,
      --  Trigger 0 has been interrupted by a high priority exception.
      Texc_Num_1,
      --  Trigger 1 has been interrupted by a high priority exception.
      Texc_Num_2,
      --  Associated trigger sequence has interrupted by a high priority
      --  exception.
      Texc_Num_3,
      --  Associated trigger sequence has interrupted by a high priority
      --  exception.
      Texc_Num_4,
      --  Associated trigger sequence has interrupted by a high priority
      --  exception.
      Texc_Num_5,
      --  Associated trigger sequence has interrupted by a high priority
      --  exception.
      Texc_Num_6,
      --  Associated trigger sequence has interrupted by a high priority
      --  exception.
      Texc_Num_7,
      --  Associated trigger sequence has interrupted by a high priority
      --  exception.
      Texc_Num_8,
      --  Associated trigger sequence has interrupted by a high priority
      --  exception.
      Texc_Num_9,
      --  Every trigger sequence has been interrupted by a high priority
      --  exception.
      Texc_Num_65535)
     with Size => 16;
   for TSTAT_TEXC_NUM_Field use
     (Texc_Num_0 => 0,
      Texc_Num_1 => 1,
      Texc_Num_2 => 2,
      Texc_Num_3 => 3,
      Texc_Num_4 => 4,
      Texc_Num_5 => 5,
      Texc_Num_6 => 6,
      Texc_Num_7 => 7,
      Texc_Num_8 => 8,
      Texc_Num_9 => 9,
      Texc_Num_65535 => 65535);

   --  Trigger Completion Flag
   type TSTAT_TCOMP_FLAG_Field is
     (
      --  No triggers have been completed. Trigger completion interrupts are
      --  disabled.
      Tcomp_Flag_0,
      --  Trigger 0 has been completed and triger 0 has enabled completion
      --  interrupts.
      Tcomp_Flag_1,
      --  Trigger 1 has been completed and triger 1 has enabled completion
      --  interrupts.
      Tcomp_Flag_2,
      --  Associated trigger sequence has completed and has enabled completion
      --  interrupts.
      Tcomp_Flag_3,
      --  Associated trigger sequence has completed and has enabled completion
      --  interrupts.
      Tcomp_Flag_4,
      --  Associated trigger sequence has completed and has enabled completion
      --  interrupts.
      Tcomp_Flag_5,
      --  Associated trigger sequence has completed and has enabled completion
      --  interrupts.
      Tcomp_Flag_6,
      --  Associated trigger sequence has completed and has enabled completion
      --  interrupts.
      Tcomp_Flag_7,
      --  Associated trigger sequence has completed and has enabled completion
      --  interrupts.
      Tcomp_Flag_8,
      --  Associated trigger sequence has completed and has enabled completion
      --  interrupts.
      Tcomp_Flag_9,
      --  Every trigger sequence has been completed and every trigger has
      --  enabled completion interrupts.
      Tcomp_Flag_65535)
     with Size => 16;
   for TSTAT_TCOMP_FLAG_Field use
     (Tcomp_Flag_0 => 0,
      Tcomp_Flag_1 => 1,
      Tcomp_Flag_2 => 2,
      Tcomp_Flag_3 => 3,
      Tcomp_Flag_4 => 4,
      Tcomp_Flag_5 => 5,
      Tcomp_Flag_6 => 6,
      Tcomp_Flag_7 => 7,
      Tcomp_Flag_8 => 8,
      Tcomp_Flag_9 => 9,
      Tcomp_Flag_65535 => 65535);

   --  Trigger Status Register
   type TSTAT_Register is record
      --  Write data bit of one shall clear (set to zero) the corresponding bit
      --  in the field. Trigger Exception Number
      TEXC_NUM   : TSTAT_TEXC_NUM_Field := NXP_SVD.ADC.Texc_Num_0;
      --  Write data bit of one shall clear (set to zero) the corresponding bit
      --  in the field. Trigger Completion Flag
      TCOMP_FLAG : TSTAT_TCOMP_FLAG_Field := NXP_SVD.ADC.Tcomp_Flag_0;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TSTAT_Register use record
      TEXC_NUM   at 0 range 0 .. 15;
      TCOMP_FLAG at 0 range 16 .. 31;
   end record;

   subtype OFSTRIM_OFSTRIM_A_Field is HAL.UInt5;
   subtype OFSTRIM_OFSTRIM_B_Field is HAL.UInt5;

   --  ADC Offset Trim Register
   type OFSTRIM_Register is record
      --  Trim for offset
      OFSTRIM_A      : OFSTRIM_OFSTRIM_A_Field := 16#0#;
      --  unspecified
      Reserved_5_15  : HAL.UInt11 := 16#0#;
      --  Trim for offset
      OFSTRIM_B      : OFSTRIM_OFSTRIM_B_Field := 16#0#;
      --  unspecified
      Reserved_21_31 : HAL.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OFSTRIM_Register use record
      OFSTRIM_A      at 0 range 0 .. 4;
      Reserved_5_15  at 0 range 5 .. 15;
      OFSTRIM_B      at 0 range 16 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   --  Trigger enable
   type TCTRL_HTEN_Field is
     (
      --  Hardware trigger source disabled
      Hten_0,
      --  Hardware trigger source enabled
      Hten_1)
     with Size => 1;
   for TCTRL_HTEN_Field use
     (Hten_0 => 0,
      Hten_1 => 1);

   --  SAR Result Destination For Channel A
   type TCTRL_FIFO_SEL_A_Field is
     (
      --  Result written to FIFO 0
      Fifo_Sel_A_0,
      --  Result written to FIFO 1
      Fifo_Sel_A_1)
     with Size => 1;
   for TCTRL_FIFO_SEL_A_Field use
     (Fifo_Sel_A_0 => 0,
      Fifo_Sel_A_1 => 1);

   --  SAR Result Destination For Channel B
   type TCTRL_FIFO_SEL_B_Field is
     (
      --  Result written to FIFO 0
      Fifo_Sel_B_0,
      --  Result written to FIFO 1
      Fifo_Sel_B_1)
     with Size => 1;
   for TCTRL_FIFO_SEL_B_Field use
     (Fifo_Sel_B_0 => 0,
      Fifo_Sel_B_1 => 1);

   --  Trigger priority setting
   type TCTRL_TPRI_Field is
     (
      --  Set to highest priority, Level 1
      Tpri_0,
      --  Set to corresponding priority level
      Tpri_1,
      --  Set to corresponding priority level
      Tpri_2,
      --  Set to corresponding priority level
      Tpri_3,
      --  Set to corresponding priority level
      Tpri_4,
      --  Set to corresponding priority level
      Tpri_5,
      --  Set to corresponding priority level
      Tpri_6,
      --  Set to corresponding priority level
      Tpri_7,
      --  Set to corresponding priority level
      Tpri_8,
      --  Set to corresponding priority level
      Tpri_9,
      --  Set to lowest priority, Level 16
      Tpri_15)
     with Size => 4;
   for TCTRL_TPRI_Field use
     (Tpri_0 => 0,
      Tpri_1 => 1,
      Tpri_2 => 2,
      Tpri_3 => 3,
      Tpri_4 => 4,
      Tpri_5 => 5,
      Tpri_6 => 6,
      Tpri_7 => 7,
      Tpri_8 => 8,
      Tpri_9 => 9,
      Tpri_15 => 15);

   subtype TCTRL_TDLY_Field is HAL.UInt4;

   --  Trigger command select
   type TCTRL_TCMD_Field is
     (
      --  Not a valid selection from the command buffer. Trigger event is
      --  ignored.
      Tcmd_0,
      --  CMD1 is executed
      Tcmd_1,
      --  Corresponding CMD is executed
      Tcmd_2,
      --  Corresponding CMD is executed
      Tcmd_3,
      --  Corresponding CMD is executed
      Tcmd_4,
      --  Corresponding CMD is executed
      Tcmd_5,
      --  Corresponding CMD is executed
      Tcmd_6,
      --  Corresponding CMD is executed
      Tcmd_7,
      --  Corresponding CMD is executed
      Tcmd_8,
      --  Corresponding CMD is executed
      Tcmd_9,
      --  CMD15 is executed
      Tcmd_15)
     with Size => 4;
   for TCTRL_TCMD_Field use
     (Tcmd_0 => 0,
      Tcmd_1 => 1,
      Tcmd_2 => 2,
      Tcmd_3 => 3,
      Tcmd_4 => 4,
      Tcmd_5 => 5,
      Tcmd_6 => 6,
      Tcmd_7 => 7,
      Tcmd_8 => 8,
      Tcmd_9 => 9,
      Tcmd_15 => 15);

   --  Trigger Control Register
   type TCTRL_Register is record
      --  Trigger enable
      HTEN           : TCTRL_HTEN_Field := NXP_SVD.ADC.Hten_0;
      --  SAR Result Destination For Channel A
      FIFO_SEL_A     : TCTRL_FIFO_SEL_A_Field := NXP_SVD.ADC.Fifo_Sel_A_0;
      --  SAR Result Destination For Channel B
      FIFO_SEL_B     : TCTRL_FIFO_SEL_B_Field := NXP_SVD.ADC.Fifo_Sel_B_0;
      --  unspecified
      Reserved_3_7   : HAL.UInt5 := 16#0#;
      --  Trigger priority setting
      TPRI           : TCTRL_TPRI_Field := NXP_SVD.ADC.Tpri_0;
      --  unspecified
      Reserved_12_14 : HAL.UInt3 := 16#0#;
      --  Trigger Resync
      RSYNC          : Boolean := False;
      --  Trigger delay select
      TDLY           : TCTRL_TDLY_Field := 16#0#;
      --  unspecified
      Reserved_20_23 : HAL.UInt4 := 16#0#;
      --  Trigger command select
      TCMD           : TCTRL_TCMD_Field := NXP_SVD.ADC.Tcmd_0;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TCTRL_Register use record
      HTEN           at 0 range 0 .. 0;
      FIFO_SEL_A     at 0 range 1 .. 1;
      FIFO_SEL_B     at 0 range 2 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      TPRI           at 0 range 8 .. 11;
      Reserved_12_14 at 0 range 12 .. 14;
      RSYNC          at 0 range 15 .. 15;
      TDLY           at 0 range 16 .. 19;
      Reserved_20_23 at 0 range 20 .. 23;
      TCMD           at 0 range 24 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  Trigger Control Register
   type TCTRL_Registers is array (0 .. 15) of TCTRL_Register
     with Volatile;

   subtype FCTRL_FCOUNT_Field is HAL.UInt5;
   subtype FCTRL_FWMARK_Field is HAL.UInt4;

   --  FIFO Control Register
   type FCTRL_Register is record
      --  Read-only. Result FIFO counter
      FCOUNT         : FCTRL_FCOUNT_Field := 16#0#;
      --  unspecified
      Reserved_5_15  : HAL.UInt11 := 16#0#;
      --  Watermark level selection
      FWMARK         : FCTRL_FWMARK_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FCTRL_Register use record
      FCOUNT         at 0 range 0 .. 4;
      Reserved_5_15  at 0 range 5 .. 15;
      FWMARK         at 0 range 16 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  FIFO Control Register
   type FCTRL_Registers is array (0 .. 1) of FCTRL_Register
     with Volatile;

   subtype GCC_GAIN_CAL_Field is HAL.UInt16;

   --  Gain Calibration Value Valid
   type GCC_RDY_Field is
     (
      --  The gain calibration value is invalid. Run the auto-calibration
      --  routine for this value to be written.
      Rdy_0,
      --  The gain calibration value is valid. It should be used to update the
      --  GCRa[GCALR] register field.
      Rdy_1)
     with Size => 1;
   for GCC_RDY_Field use
     (Rdy_0 => 0,
      Rdy_1 => 1);

   --  Gain Calibration Control
   type GCC_Register is record
      --  Read-only. Gain Calibration Value
      GAIN_CAL       : GCC_GAIN_CAL_Field;
      --  unspecified
      Reserved_16_23 : HAL.UInt8;
      --  Read-only. Gain Calibration Value Valid
      RDY            : GCC_RDY_Field;
      --  unspecified
      Reserved_25_31 : HAL.UInt7;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for GCC_Register use record
      GAIN_CAL       at 0 range 0 .. 15;
      Reserved_16_23 at 0 range 16 .. 23;
      RDY            at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  Gain Calibration Control
   type GCC_Registers is array (0 .. 1) of GCC_Register
     with Volatile;

   subtype GCR_GCALR_Field is HAL.UInt16;

   --  Gain Calculation Ready
   type GCR_RDY_Field is
     (
      --  The gain offset calculation value is invalid.
      Rdy_0,
      --  The gain calibration value is valid.
      Rdy_1)
     with Size => 1;
   for GCR_RDY_Field use
     (Rdy_0 => 0,
      Rdy_1 => 1);

   --  Gain Calculation Result
   type GCR_Register is record
      --  Gain Calculation Result
      GCALR          : GCR_GCALR_Field := 16#0#;
      --  unspecified
      Reserved_16_23 : HAL.UInt8 := 16#0#;
      --  Gain Calculation Ready
      RDY            : GCR_RDY_Field := NXP_SVD.ADC.Rdy_0;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for GCR_Register use record
      GCALR          at 0 range 0 .. 15;
      Reserved_16_23 at 0 range 16 .. 23;
      RDY            at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  Gain Calculation Result
   type GCR_Registers is array (0 .. 1) of GCR_Register
     with Volatile;

   --  Input channel select
   type CMDL1_ADCH_Field is
     (
      --  Select CH0A or CH0B or CH0A/CH0B pair.
      Adch_0,
      --  Select CH1A or CH1B or CH1A/CH1B pair.
      Adch_1,
      --  Select CH2A or CH2B or CH2A/CH2B pair.
      Adch_2,
      --  Select CH3A or CH3B or CH3A/CH3B pair.
      Adch_3,
      --  Select corresponding channel CHnA or CHnB or CHnA/CHnB pair.
      Adch_4,
      --  Select corresponding channel CHnA or CHnB or CHnA/CHnB pair.
      Adch_5,
      --  Select corresponding channel CHnA or CHnB or CHnA/CHnB pair.
      Adch_6,
      --  Select corresponding channel CHnA or CHnB or CHnA/CHnB pair.
      Adch_7,
      --  Select corresponding channel CHnA or CHnB or CHnA/CHnB pair.
      Adch_8,
      --  Select corresponding channel CHnA or CHnB or CHnA/CHnB pair.
      Adch_9,
      --  Select CH30A or CH30B or CH30A/CH30B pair.
      Adch_30,
      --  Select CH31A or CH31B or CH31A/CH31B pair.
      Adch_31)
     with Size => 5;
   for CMDL1_ADCH_Field use
     (Adch_0 => 0,
      Adch_1 => 1,
      Adch_2 => 2,
      Adch_3 => 3,
      Adch_4 => 4,
      Adch_5 => 5,
      Adch_6 => 6,
      Adch_7 => 7,
      Adch_8 => 8,
      Adch_9 => 9,
      Adch_30 => 30,
      Adch_31 => 31);

   --  Conversion Type
   type CMDL1_CTYPE_Field is
     (
      --  Single-Ended Mode. Only A side channel is converted.
      Ctype_0,
      --  Single-Ended Mode. Only B side channel is converted.
      Ctype_1,
      --  Differential Mode. A-B.
      Ctype_2,
      --  Dual-Single-Ended Mode. Both A side and B side channels are converted
      --  independently.
      Ctype_3)
     with Size => 2;
   for CMDL1_CTYPE_Field use
     (Ctype_0 => 0,
      Ctype_1 => 1,
      Ctype_2 => 2,
      Ctype_3 => 3);

   --  Select resolution of conversions
   type CMDL1_MODE_Field is
     (
      --  Standard resolution. Single-ended 12-bit conversion; Differential
      --  13-bit conversion with 2's complement output.
      Mode_0,
      --  High resolution. Single-ended 16-bit conversion; Differential 16-bit
      --  conversion with 2's complement output.
      Mode_1)
     with Size => 1;
   for CMDL1_MODE_Field use
     (Mode_0 => 0,
      Mode_1 => 1);

   --  ADC Command Low Buffer Register
   type CMDL_Register is record
      --  Input channel select
      ADCH          : CMDL1_ADCH_Field := NXP_SVD.ADC.Adch_0;
      --  Conversion Type
      CTYPE         : CMDL1_CTYPE_Field := NXP_SVD.ADC.Ctype_0;
      --  Select resolution of conversions
      MODE          : CMDL1_MODE_Field := NXP_SVD.ADC.Mode_0;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CMDL_Register use record
      ADCH          at 0 range 0 .. 4;
      CTYPE         at 0 range 5 .. 6;
      MODE          at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  Compare Function Enable
   type CMDH1_CMPEN_Field is
     (
      --  Compare disabled.
      Cmpen_0,
      --  Compare enabled. Store on true.
      Cmpen_2,
      --  Compare enabled. Repeat channel acquisition (sample/convert/compare)
      --  until true.
      Cmpen_3)
     with Size => 2;
   for CMDH1_CMPEN_Field use
     (Cmpen_0 => 0,
      Cmpen_2 => 2,
      Cmpen_3 => 3);

   --  Wait for trigger assertion before execution.
   type CMDH1_WAIT_TRIG_Field is
     (
      --  This command will be automatically executed.
      Wait_Trig_0,
      --  The active trigger must be asserted again before executing this
      --  command.
      Wait_Trig_1)
     with Size => 1;
   for CMDH1_WAIT_TRIG_Field use
     (Wait_Trig_0 => 0,
      Wait_Trig_1 => 1);

   --  Loop with Increment
   type CMDH1_LWI_Field is
     (
      --  Auto channel increment disabled
      Lwi_0,
      --  Auto channel increment enabled
      Lwi_1)
     with Size => 1;
   for CMDH1_LWI_Field use
     (Lwi_0 => 0,
      Lwi_1 => 1);

   --  Sample Time Select
   type CMDH1_STS_Field is
     (
      --  Minimum sample time of 3 ADCK cycles.
      Sts_0,
      --  3 + 21 ADCK cycles; 5 ADCK cycles total sample time.
      Sts_1,
      --  3 + 22 ADCK cycles; 7 ADCK cycles total sample time.
      Sts_2,
      --  3 + 23 ADCK cycles; 11 ADCK cycles total sample time.
      Sts_3,
      --  3 + 24 ADCK cycles; 19 ADCK cycles total sample time.
      Sts_4,
      --  3 + 25 ADCK cycles; 35 ADCK cycles total sample time.
      Sts_5,
      --  3 + 26 ADCK cycles; 67 ADCK cycles total sample time.
      Sts_6,
      --  3 + 27 ADCK cycles; 131 ADCK cycles total sample time.
      Sts_7)
     with Size => 3;
   for CMDH1_STS_Field use
     (Sts_0 => 0,
      Sts_1 => 1,
      Sts_2 => 2,
      Sts_3 => 3,
      Sts_4 => 4,
      Sts_5 => 5,
      Sts_6 => 6,
      Sts_7 => 7);

   --  Hardware Average Select
   type CMDH1_AVGS_Field is
     (
      --  Single conversion.
      Avgs_0,
      --  2 conversions averaged.
      Avgs_1,
      --  4 conversions averaged.
      Avgs_2,
      --  8 conversions averaged.
      Avgs_3,
      --  16 conversions averaged.
      Avgs_4,
      --  32 conversions averaged.
      Avgs_5,
      --  64 conversions averaged.
      Avgs_6,
      --  128 conversions averaged.
      Avgs_7)
     with Size => 3;
   for CMDH1_AVGS_Field use
     (Avgs_0 => 0,
      Avgs_1 => 1,
      Avgs_2 => 2,
      Avgs_3 => 3,
      Avgs_4 => 4,
      Avgs_5 => 5,
      Avgs_6 => 6,
      Avgs_7 => 7);

   --  Loop Count Select
   type CMDH1_LOOP_Field is
     (
      --  Looping not enabled. Command executes 1 time.
      Loop_0,
      --  Loop 1 time. Command executes 2 times.
      Loop_1,
      --  Loop 2 times. Command executes 3 times.
      Loop_2,
      --  Loop corresponding number of times. Command executes LOOP+1 times.
      Loop_3,
      --  Loop corresponding number of times. Command executes LOOP+1 times.
      Loop_4,
      --  Loop corresponding number of times. Command executes LOOP+1 times.
      Loop_5,
      --  Loop corresponding number of times. Command executes LOOP+1 times.
      Loop_6,
      --  Loop corresponding number of times. Command executes LOOP+1 times.
      Loop_7,
      --  Loop corresponding number of times. Command executes LOOP+1 times.
      Loop_8,
      --  Loop corresponding number of times. Command executes LOOP+1 times.
      Loop_9,
      --  Loop 15 times. Command executes 16 times.
      Loop_15)
     with Size => 4;
   for CMDH1_LOOP_Field use
     (Loop_0 => 0,
      Loop_1 => 1,
      Loop_2 => 2,
      Loop_3 => 3,
      Loop_4 => 4,
      Loop_5 => 5,
      Loop_6 => 6,
      Loop_7 => 7,
      Loop_8 => 8,
      Loop_9 => 9,
      Loop_15 => 15);

   --  Next Command Select
   type CMDH1_NEXT_Field is
     (
      --  No next command defined. Terminate conversions at completion of
      --  current command. If lower priority trigger pending, begin command
      --  associated with lower priority trigger.
      Next_0,
      --  Select CMD1 command buffer register as next command.
      Next_1,
      --  Select corresponding CMD command buffer register as next command
      Next_2,
      --  Select corresponding CMD command buffer register as next command
      Next_3,
      --  Select corresponding CMD command buffer register as next command
      Next_4,
      --  Select corresponding CMD command buffer register as next command
      Next_5,
      --  Select corresponding CMD command buffer register as next command
      Next_6,
      --  Select corresponding CMD command buffer register as next command
      Next_7,
      --  Select corresponding CMD command buffer register as next command
      Next_8,
      --  Select corresponding CMD command buffer register as next command
      Next_9,
      --  Select CMD15 command buffer register as next command.
      Next_15)
     with Size => 4;
   for CMDH1_NEXT_Field use
     (Next_0 => 0,
      Next_1 => 1,
      Next_2 => 2,
      Next_3 => 3,
      Next_4 => 4,
      Next_5 => 5,
      Next_6 => 6,
      Next_7 => 7,
      Next_8 => 8,
      Next_9 => 9,
      Next_15 => 15);

   --  ADC Command High Buffer Register
   type CMDH_Register is record
      --  Compare Function Enable
      CMPEN          : CMDH1_CMPEN_Field := NXP_SVD.ADC.Cmpen_0;
      --  Wait for trigger assertion before execution.
      WAIT_TRIG      : CMDH1_WAIT_TRIG_Field := NXP_SVD.ADC.Wait_Trig_0;
      --  unspecified
      Reserved_3_6   : HAL.UInt4 := 16#0#;
      --  Loop with Increment
      LWI            : CMDH1_LWI_Field := NXP_SVD.ADC.Lwi_0;
      --  Sample Time Select
      STS            : CMDH1_STS_Field := NXP_SVD.ADC.Sts_0;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  Hardware Average Select
      AVGS           : CMDH1_AVGS_Field := NXP_SVD.ADC.Avgs_0;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Loop Count Select
      LOOP_k         : CMDH1_LOOP_Field := NXP_SVD.ADC.Loop_0;
      --  unspecified
      Reserved_20_23 : HAL.UInt4 := 16#0#;
      --  Next Command Select
      NEXT           : CMDH1_NEXT_Field := NXP_SVD.ADC.Next_0;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CMDH_Register use record
      CMPEN          at 0 range 0 .. 1;
      WAIT_TRIG      at 0 range 2 .. 2;
      Reserved_3_6   at 0 range 3 .. 6;
      LWI            at 0 range 7 .. 7;
      STS            at 0 range 8 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      AVGS           at 0 range 12 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      LOOP_k         at 0 range 16 .. 19;
      Reserved_20_23 at 0 range 20 .. 23;
      NEXT           at 0 range 24 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  Wait for trigger assertion before execution.
   type CMDH5_WAIT_TRIG_Field is
     (
      --  This command will be automatically executed.
      Wait_Trig_0,
      --  The active trigger must be asserted again before executing this
      --  command.
      Wait_Trig_1)
     with Size => 1;
   for CMDH5_WAIT_TRIG_Field use
     (Wait_Trig_0 => 0,
      Wait_Trig_1 => 1);

   --  Loop with Increment
   type CMDH5_LWI_Field is
     (
      --  Auto channel increment disabled
      Lwi_0,
      --  Auto channel increment enabled
      Lwi_1)
     with Size => 1;
   for CMDH5_LWI_Field use
     (Lwi_0 => 0,
      Lwi_1 => 1);

   --  Sample Time Select
   type CMDH5_STS_Field is
     (
      --  Minimum sample time of 3 ADCK cycles.
      Sts_0,
      --  3 + 21 ADCK cycles; 5 ADCK cycles total sample time.
      Sts_1,
      --  3 + 22 ADCK cycles; 7 ADCK cycles total sample time.
      Sts_2,
      --  3 + 23 ADCK cycles; 11 ADCK cycles total sample time.
      Sts_3,
      --  3 + 24 ADCK cycles; 19 ADCK cycles total sample time.
      Sts_4,
      --  3 + 25 ADCK cycles; 35 ADCK cycles total sample time.
      Sts_5,
      --  3 + 26 ADCK cycles; 67 ADCK cycles total sample time.
      Sts_6,
      --  3 + 27 ADCK cycles; 131 ADCK cycles total sample time.
      Sts_7)
     with Size => 3;
   for CMDH5_STS_Field use
     (Sts_0 => 0,
      Sts_1 => 1,
      Sts_2 => 2,
      Sts_3 => 3,
      Sts_4 => 4,
      Sts_5 => 5,
      Sts_6 => 6,
      Sts_7 => 7);

   --  Hardware Average Select
   type CMDH5_AVGS_Field is
     (
      --  Single conversion.
      Avgs_0,
      --  2 conversions averaged.
      Avgs_1,
      --  4 conversions averaged.
      Avgs_2,
      --  8 conversions averaged.
      Avgs_3,
      --  16 conversions averaged.
      Avgs_4,
      --  32 conversions averaged.
      Avgs_5,
      --  64 conversions averaged.
      Avgs_6,
      --  128 conversions averaged.
      Avgs_7)
     with Size => 3;
   for CMDH5_AVGS_Field use
     (Avgs_0 => 0,
      Avgs_1 => 1,
      Avgs_2 => 2,
      Avgs_3 => 3,
      Avgs_4 => 4,
      Avgs_5 => 5,
      Avgs_6 => 6,
      Avgs_7 => 7);

   --  Loop Count Select
   type CMDH5_LOOP_Field is
     (
      --  Looping not enabled. Command executes 1 time.
      Loop_0,
      --  Loop 1 time. Command executes 2 times.
      Loop_1,
      --  Loop 2 times. Command executes 3 times.
      Loop_2,
      --  Loop corresponding number of times. Command executes LOOP+1 times.
      Loop_3,
      --  Loop corresponding number of times. Command executes LOOP+1 times.
      Loop_4,
      --  Loop corresponding number of times. Command executes LOOP+1 times.
      Loop_5,
      --  Loop corresponding number of times. Command executes LOOP+1 times.
      Loop_6,
      --  Loop corresponding number of times. Command executes LOOP+1 times.
      Loop_7,
      --  Loop corresponding number of times. Command executes LOOP+1 times.
      Loop_8,
      --  Loop corresponding number of times. Command executes LOOP+1 times.
      Loop_9,
      --  Loop 15 times. Command executes 16 times.
      Loop_15)
     with Size => 4;
   for CMDH5_LOOP_Field use
     (Loop_0 => 0,
      Loop_1 => 1,
      Loop_2 => 2,
      Loop_3 => 3,
      Loop_4 => 4,
      Loop_5 => 5,
      Loop_6 => 6,
      Loop_7 => 7,
      Loop_8 => 8,
      Loop_9 => 9,
      Loop_15 => 15);

   --  Next Command Select
   type CMDH5_NEXT_Field is
     (
      --  No next command defined. Terminate conversions at completion of
      --  current command. If lower priority trigger pending, begin command
      --  associated with lower priority trigger.
      Next_0,
      --  Select CMD1 command buffer register as next command.
      Next_1,
      --  Select corresponding CMD command buffer register as next command
      Next_2,
      --  Select corresponding CMD command buffer register as next command
      Next_3,
      --  Select corresponding CMD command buffer register as next command
      Next_4,
      --  Select corresponding CMD command buffer register as next command
      Next_5,
      --  Select corresponding CMD command buffer register as next command
      Next_6,
      --  Select corresponding CMD command buffer register as next command
      Next_7,
      --  Select corresponding CMD command buffer register as next command
      Next_8,
      --  Select corresponding CMD command buffer register as next command
      Next_9,
      --  Select CMD15 command buffer register as next command.
      Next_15)
     with Size => 4;
   for CMDH5_NEXT_Field use
     (Next_0 => 0,
      Next_1 => 1,
      Next_2 => 2,
      Next_3 => 3,
      Next_4 => 4,
      Next_5 => 5,
      Next_6 => 6,
      Next_7 => 7,
      Next_8 => 8,
      Next_9 => 9,
      Next_15 => 15);

   --  ADC Command High Buffer Register
   type CMDH_Register_1 is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  Wait for trigger assertion before execution.
      WAIT_TRIG      : CMDH5_WAIT_TRIG_Field := NXP_SVD.ADC.Wait_Trig_0;
      --  unspecified
      Reserved_3_6   : HAL.UInt4 := 16#0#;
      --  Loop with Increment
      LWI            : CMDH5_LWI_Field := NXP_SVD.ADC.Lwi_0;
      --  Sample Time Select
      STS            : CMDH5_STS_Field := NXP_SVD.ADC.Sts_0;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  Hardware Average Select
      AVGS           : CMDH5_AVGS_Field := NXP_SVD.ADC.Avgs_0;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Loop Count Select
      LOOP_k         : CMDH5_LOOP_Field := NXP_SVD.ADC.Loop_0;
      --  unspecified
      Reserved_20_23 : HAL.UInt4 := 16#0#;
      --  Next Command Select
      NEXT           : CMDH5_NEXT_Field := NXP_SVD.ADC.Next_0;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CMDH_Register_1 use record
      Reserved_0_1   at 0 range 0 .. 1;
      WAIT_TRIG      at 0 range 2 .. 2;
      Reserved_3_6   at 0 range 3 .. 6;
      LWI            at 0 range 7 .. 7;
      STS            at 0 range 8 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      AVGS           at 0 range 12 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      LOOP_k         at 0 range 16 .. 19;
      Reserved_20_23 at 0 range 20 .. 23;
      NEXT           at 0 range 24 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype CV_CVL_Field is HAL.UInt16;
   subtype CV_CVH_Field is HAL.UInt16;

   --  Compare Value Register
   type CV_Register is record
      --  Compare Value Low.
      CVL : CV_CVL_Field := 16#0#;
      --  Compare Value High.
      CVH : CV_CVH_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CV_Register use record
      CVL at 0 range 0 .. 15;
      CVH at 0 range 16 .. 31;
   end record;

   --  Compare Value Register
   type CV_Registers is array (0 .. 3) of CV_Register
     with Volatile;

   subtype RESFIFO_D_Field is HAL.UInt16;

   --  Trigger Source
   type RESFIFO_TSRC_Field is
     (
      --  Trigger source 0 initiated this conversion.
      Tsrc_0,
      --  Trigger source 1 initiated this conversion.
      Tsrc_1,
      --  Corresponding trigger source initiated this conversion.
      Tsrc_2,
      --  Corresponding trigger source initiated this conversion.
      Tsrc_3,
      --  Corresponding trigger source initiated this conversion.
      Tsrc_4,
      --  Corresponding trigger source initiated this conversion.
      Tsrc_5,
      --  Corresponding trigger source initiated this conversion.
      Tsrc_6,
      --  Corresponding trigger source initiated this conversion.
      Tsrc_7,
      --  Corresponding trigger source initiated this conversion.
      Tsrc_8,
      --  Corresponding trigger source initiated this conversion.
      Tsrc_9,
      --  Trigger source 15 initiated this conversion.
      Tsrc_15)
     with Size => 4;
   for RESFIFO_TSRC_Field use
     (Tsrc_0 => 0,
      Tsrc_1 => 1,
      Tsrc_2 => 2,
      Tsrc_3 => 3,
      Tsrc_4 => 4,
      Tsrc_5 => 5,
      Tsrc_6 => 6,
      Tsrc_7 => 7,
      Tsrc_8 => 8,
      Tsrc_9 => 9,
      Tsrc_15 => 15);

   --  Loop count value
   type RESFIFO_LOOPCNT_Field is
     (
      --  Result is from initial conversion in command.
      Loopcnt_0,
      --  Result is from second conversion in command.
      Loopcnt_1,
      --  Result is from LOOPCNT+1 conversion in command.
      Loopcnt_2,
      --  Result is from LOOPCNT+1 conversion in command.
      Loopcnt_3,
      --  Result is from LOOPCNT+1 conversion in command.
      Loopcnt_4,
      --  Result is from LOOPCNT+1 conversion in command.
      Loopcnt_5,
      --  Result is from LOOPCNT+1 conversion in command.
      Loopcnt_6,
      --  Result is from LOOPCNT+1 conversion in command.
      Loopcnt_7,
      --  Result is from LOOPCNT+1 conversion in command.
      Loopcnt_8,
      --  Result is from LOOPCNT+1 conversion in command.
      Loopcnt_9,
      --  Result is from 16th conversion in command.
      Loopcnt_15)
     with Size => 4;
   for RESFIFO_LOOPCNT_Field use
     (Loopcnt_0 => 0,
      Loopcnt_1 => 1,
      Loopcnt_2 => 2,
      Loopcnt_3 => 3,
      Loopcnt_4 => 4,
      Loopcnt_5 => 5,
      Loopcnt_6 => 6,
      Loopcnt_7 => 7,
      Loopcnt_8 => 8,
      Loopcnt_9 => 9,
      Loopcnt_15 => 15);

   --  Command Buffer Source
   type RESFIFO_CMDSRC_Field is
     (
      --  Not a valid value CMDSRC value for a dataword in RESFIFO. 0x0 is only
      --  found in initial FIFO state prior to an ADC conversion result
      --  dataword being stored to a RESFIFO buffer.
      Cmdsrc_0,
      --  CMD1 buffer used as control settings for this conversion.
      Cmdsrc_1,
      --  Corresponding command buffer used as control settings for this
      --  conversion.
      Cmdsrc_2,
      --  Corresponding command buffer used as control settings for this
      --  conversion.
      Cmdsrc_3,
      --  Corresponding command buffer used as control settings for this
      --  conversion.
      Cmdsrc_4,
      --  Corresponding command buffer used as control settings for this
      --  conversion.
      Cmdsrc_5,
      --  Corresponding command buffer used as control settings for this
      --  conversion.
      Cmdsrc_6,
      --  Corresponding command buffer used as control settings for this
      --  conversion.
      Cmdsrc_7,
      --  Corresponding command buffer used as control settings for this
      --  conversion.
      Cmdsrc_8,
      --  Corresponding command buffer used as control settings for this
      --  conversion.
      Cmdsrc_9,
      --  CMD15 buffer used as control settings for this conversion.
      Cmdsrc_15)
     with Size => 4;
   for RESFIFO_CMDSRC_Field use
     (Cmdsrc_0 => 0,
      Cmdsrc_1 => 1,
      Cmdsrc_2 => 2,
      Cmdsrc_3 => 3,
      Cmdsrc_4 => 4,
      Cmdsrc_5 => 5,
      Cmdsrc_6 => 6,
      Cmdsrc_7 => 7,
      Cmdsrc_8 => 8,
      Cmdsrc_9 => 9,
      Cmdsrc_15 => 15);

   --  FIFO entry is valid
   type RESFIFO_VALID_Field is
     (
      --  FIFO is empty. Discard any read from RESFIFO.
      Valid_0,
      --  FIFO record read from RESFIFO is valid.
      Valid_1)
     with Size => 1;
   for RESFIFO_VALID_Field use
     (Valid_0 => 0,
      Valid_1 => 1);

   --  ADC Data Result FIFO Register
   type RESFIFO_Register is record
      --  Read-only. Data result
      D              : RESFIFO_D_Field;
      --  Read-only. Trigger Source
      TSRC           : RESFIFO_TSRC_Field;
      --  Read-only. Loop count value
      LOOPCNT        : RESFIFO_LOOPCNT_Field;
      --  Read-only. Command Buffer Source
      CMDSRC         : RESFIFO_CMDSRC_Field;
      --  unspecified
      Reserved_28_30 : HAL.UInt3;
      --  Read-only. FIFO entry is valid
      VALID          : RESFIFO_VALID_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RESFIFO_Register use record
      D              at 0 range 0 .. 15;
      TSRC           at 0 range 16 .. 19;
      LOOPCNT        at 0 range 20 .. 23;
      CMDSRC         at 0 range 24 .. 27;
      Reserved_28_30 at 0 range 28 .. 30;
      VALID          at 0 range 31 .. 31;
   end record;

   --  ADC Data Result FIFO Register
   type RESFIFO_Registers is array (0 .. 1) of RESFIFO_Register
     with Volatile;

   subtype CAL_GAR_CAL_GAR_VAL_Field is HAL.UInt16;

   --  Calibration General A-Side Registers
   type CAL_GAR_Register is record
      --  Calibration General A Side Register Element
      CAL_GAR_VAL    : CAL_GAR_CAL_GAR_VAL_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CAL_GAR_Register use record
      CAL_GAR_VAL    at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  Calibration General A-Side Registers
   type CAL_GAR_Registers is array (0 .. 32) of CAL_GAR_Register
     with Volatile;

   subtype CAL_GBR_CAL_GBR_VAL_Field is HAL.UInt16;

   --  Calibration General B-Side Registers
   type CAL_GBR_Register is record
      --  Calibration General B Side Register Element
      CAL_GBR_VAL    : CAL_GBR_CAL_GBR_VAL_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CAL_GBR_Register use record
      CAL_GBR_VAL    at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  Calibration General B-Side Registers
   type CAL_GBR_Registers is array (0 .. 32) of CAL_GBR_Register
     with Volatile;

   --  Calibration Sample Time Long
   type TST_CST_LONG_Field is
     (
      --  Normal sample time. Minimum sample time of 3 ADCK cycles.
      Cst_Long_0,
      --  Increased sample time. 67 ADCK cycles total sample time.
      Cst_Long_1)
     with Size => 1;
   for TST_CST_LONG_Field use
     (Cst_Long_0 => 0,
      Cst_Long_1 => 1);

   --  Force M-side positive offset
   type TST_FOFFM_Field is
     (
      --  Normal operation. No forced offset.
      Foffm_0,
      --  Test configuration. Forced positive offset on MDAC.
      Foffm_1)
     with Size => 1;
   for TST_FOFFM_Field use
     (Foffm_0 => 0,
      Foffm_1 => 1);

   --  Force P-side positive offset
   type TST_FOFFP_Field is
     (
      --  Normal operation. No forced offset.
      Foffp_0,
      --  Test configuration. Forced positive offset on PDAC.
      Foffp_1)
     with Size => 1;
   for TST_FOFFP_Field use
     (Foffp_0 => 0,
      Foffp_1 => 1);

   --  Force M-side negative offset
   type TST_FOFFM2_Field is
     (
      --  Normal operation. No forced offset.
      Foffm2_0,
      --  Test configuration. Forced negative offset on MDAC.
      Foffm2_1)
     with Size => 1;
   for TST_FOFFM2_Field use
     (Foffm2_0 => 0,
      Foffm2_1 => 1);

   --  Force P-side negative offset
   type TST_FOFFP2_Field is
     (
      --  Normal operation. No forced offset.
      Foffp2_0,
      --  Test configuration. Forced negative offset on PDAC.
      Foffp2_1)
     with Size => 1;
   for TST_FOFFP2_Field use
     (Foffp2_0 => 0,
      Foffp2_1 => 1);

   --  Enable test configuration
   type TST_TESTEN_Field is
     (
      --  Normal operation. Test configuration not enabled.
      Testen_0,
      --  Hardware BIST Test in progress.
      Testen_1)
     with Size => 1;
   for TST_TESTEN_Field use
     (Testen_0 => 0,
      Testen_1 => 1);

   --  ADC Test Register
   type TST_Register is record
      --  Calibration Sample Time Long
      CST_LONG       : TST_CST_LONG_Field := NXP_SVD.ADC.Cst_Long_0;
      --  unspecified
      Reserved_1_7   : HAL.UInt7 := 16#0#;
      --  Force M-side positive offset
      FOFFM          : TST_FOFFM_Field := NXP_SVD.ADC.Foffm_0;
      --  Force P-side positive offset
      FOFFP          : TST_FOFFP_Field := NXP_SVD.ADC.Foffp_0;
      --  Force M-side negative offset
      FOFFM2         : TST_FOFFM2_Field := NXP_SVD.ADC.Foffm2_0;
      --  Force P-side negative offset
      FOFFP2         : TST_FOFFP2_Field := NXP_SVD.ADC.Foffp2_0;
      --  unspecified
      Reserved_12_22 : HAL.UInt11 := 16#0#;
      --  Enable test configuration
      TESTEN         : TST_TESTEN_Field := NXP_SVD.ADC.Testen_0;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TST_Register use record
      CST_LONG       at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      FOFFM          at 0 range 8 .. 8;
      FOFFP          at 0 range 9 .. 9;
      FOFFM2         at 0 range 10 .. 10;
      FOFFP2         at 0 range 11 .. 11;
      Reserved_12_22 at 0 range 12 .. 22;
      TESTEN         at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  ADC
   type ADC0_Peripheral is record
      --  Version ID Register
      VERID   : aliased VERID_Register;
      --  Parameter Register
      PARAM   : aliased PARAM_Register;
      --  ADC Control Register
      CTRL    : aliased CTRL_Register;
      --  ADC Status Register
      STAT    : aliased STAT_Register;
      --  Interrupt Enable Register
      IE      : aliased IE_Register;
      --  DMA Enable Register
      DE      : aliased DE_Register;
      --  ADC Configuration Register
      CFG     : aliased CFG_Register;
      --  ADC Pause Register
      PAUSE   : aliased PAUSE_Register;
      --  Software Trigger Register
      SWTRIG  : aliased SWTRIG_Register;
      --  Trigger Status Register
      TSTAT   : aliased TSTAT_Register;
      --  ADC Offset Trim Register
      OFSTRIM : aliased OFSTRIM_Register;
      --  Trigger Control Register
      TCTRL   : aliased TCTRL_Registers;
      --  FIFO Control Register
      FCTRL   : aliased FCTRL_Registers;
      --  Gain Calibration Control
      GCC     : aliased GCC_Registers;
      --  Gain Calculation Result
      GCR     : aliased GCR_Registers;
      --  ADC Command Low Buffer Register
      CMDL1   : aliased CMDL_Register;
      --  ADC Command High Buffer Register
      CMDH1   : aliased CMDH_Register;
      --  ADC Command Low Buffer Register
      CMDL2   : aliased CMDL_Register;
      --  ADC Command High Buffer Register
      CMDH2   : aliased CMDH_Register;
      --  ADC Command Low Buffer Register
      CMDL3   : aliased CMDL_Register;
      --  ADC Command High Buffer Register
      CMDH3   : aliased CMDH_Register;
      --  ADC Command Low Buffer Register
      CMDL4   : aliased CMDL_Register;
      --  ADC Command High Buffer Register
      CMDH4   : aliased CMDH_Register;
      --  ADC Command Low Buffer Register
      CMDL5   : aliased CMDL_Register;
      --  ADC Command High Buffer Register
      CMDH5   : aliased CMDH_Register_1;
      --  ADC Command Low Buffer Register
      CMDL6   : aliased CMDL_Register;
      --  ADC Command High Buffer Register
      CMDH6   : aliased CMDH_Register_1;
      --  ADC Command Low Buffer Register
      CMDL7   : aliased CMDL_Register;
      --  ADC Command High Buffer Register
      CMDH7   : aliased CMDH_Register_1;
      --  ADC Command Low Buffer Register
      CMDL8   : aliased CMDL_Register;
      --  ADC Command High Buffer Register
      CMDH8   : aliased CMDH_Register_1;
      --  ADC Command Low Buffer Register
      CMDL9   : aliased CMDL_Register;
      --  ADC Command High Buffer Register
      CMDH9   : aliased CMDH_Register_1;
      --  ADC Command Low Buffer Register
      CMDL10  : aliased CMDL_Register;
      --  ADC Command High Buffer Register
      CMDH10  : aliased CMDH_Register_1;
      --  ADC Command Low Buffer Register
      CMDL11  : aliased CMDL_Register;
      --  ADC Command High Buffer Register
      CMDH11  : aliased CMDH_Register_1;
      --  ADC Command Low Buffer Register
      CMDL12  : aliased CMDL_Register;
      --  ADC Command High Buffer Register
      CMDH12  : aliased CMDH_Register_1;
      --  ADC Command Low Buffer Register
      CMDL13  : aliased CMDL_Register;
      --  ADC Command High Buffer Register
      CMDH13  : aliased CMDH_Register_1;
      --  ADC Command Low Buffer Register
      CMDL14  : aliased CMDL_Register;
      --  ADC Command High Buffer Register
      CMDH14  : aliased CMDH_Register_1;
      --  ADC Command Low Buffer Register
      CMDL15  : aliased CMDL_Register;
      --  ADC Command High Buffer Register
      CMDH15  : aliased CMDH_Register_1;
      --  Compare Value Register
      CV      : aliased CV_Registers;
      --  ADC Data Result FIFO Register
      RESFIFO : aliased RESFIFO_Registers;
      --  Calibration General A-Side Registers
      CAL_GAR : aliased CAL_GAR_Registers;
      --  Calibration General B-Side Registers
      CAL_GBR : aliased CAL_GBR_Registers;
      --  ADC Test Register
      TST     : aliased TST_Register;
   end record
     with Volatile;

   for ADC0_Peripheral use record
      VERID   at 16#0# range 0 .. 31;
      PARAM   at 16#4# range 0 .. 31;
      CTRL    at 16#10# range 0 .. 31;
      STAT    at 16#14# range 0 .. 31;
      IE      at 16#18# range 0 .. 31;
      DE      at 16#1C# range 0 .. 31;
      CFG     at 16#20# range 0 .. 31;
      PAUSE   at 16#24# range 0 .. 31;
      SWTRIG  at 16#34# range 0 .. 31;
      TSTAT   at 16#38# range 0 .. 31;
      OFSTRIM at 16#40# range 0 .. 31;
      TCTRL   at 16#A0# range 0 .. 511;
      FCTRL   at 16#E0# range 0 .. 63;
      GCC     at 16#F0# range 0 .. 63;
      GCR     at 16#F8# range 0 .. 63;
      CMDL1   at 16#100# range 0 .. 31;
      CMDH1   at 16#104# range 0 .. 31;
      CMDL2   at 16#108# range 0 .. 31;
      CMDH2   at 16#10C# range 0 .. 31;
      CMDL3   at 16#110# range 0 .. 31;
      CMDH3   at 16#114# range 0 .. 31;
      CMDL4   at 16#118# range 0 .. 31;
      CMDH4   at 16#11C# range 0 .. 31;
      CMDL5   at 16#120# range 0 .. 31;
      CMDH5   at 16#124# range 0 .. 31;
      CMDL6   at 16#128# range 0 .. 31;
      CMDH6   at 16#12C# range 0 .. 31;
      CMDL7   at 16#130# range 0 .. 31;
      CMDH7   at 16#134# range 0 .. 31;
      CMDL8   at 16#138# range 0 .. 31;
      CMDH8   at 16#13C# range 0 .. 31;
      CMDL9   at 16#140# range 0 .. 31;
      CMDH9   at 16#144# range 0 .. 31;
      CMDL10  at 16#148# range 0 .. 31;
      CMDH10  at 16#14C# range 0 .. 31;
      CMDL11  at 16#150# range 0 .. 31;
      CMDH11  at 16#154# range 0 .. 31;
      CMDL12  at 16#158# range 0 .. 31;
      CMDH12  at 16#15C# range 0 .. 31;
      CMDL13  at 16#160# range 0 .. 31;
      CMDH13  at 16#164# range 0 .. 31;
      CMDL14  at 16#168# range 0 .. 31;
      CMDH14  at 16#16C# range 0 .. 31;
      CMDL15  at 16#170# range 0 .. 31;
      CMDH15  at 16#174# range 0 .. 31;
      CV      at 16#200# range 0 .. 127;
      RESFIFO at 16#300# range 0 .. 63;
      CAL_GAR at 16#400# range 0 .. 1055;
      CAL_GBR at 16#500# range 0 .. 1055;
      TST     at 16#FFC# range 0 .. 31;
   end record;

   --  ADC
   ADC0_Periph : aliased ADC0_Peripheral
     with Import, Address => System'To_Address (16#400A0000#);

end NXP_SVD.ADC;
