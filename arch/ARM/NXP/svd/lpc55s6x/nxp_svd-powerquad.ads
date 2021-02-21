--  Copyright 2016-2019 NXP
--  All rights reserved.SPDX-License-Identifier: BSD-3-Clause
--  This spec has been automatically generated from LPC55S6x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NXP_SVD.POWERQUAD is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype OUTFORMAT_out_formatint_Field is HAL.UInt2;
   subtype OUTFORMAT_out_formatext_Field is HAL.UInt2;
   subtype OUTFORMAT_out_scaler_Field is HAL.UInt8;

   --  Output format
   type OUTFORMAT_Register is record
      --  Output Internal format (00: q15; 01:q31; 10:float)
      out_formatint  : OUTFORMAT_out_formatint_Field := 16#0#;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  Output External format (00: q15; 01:q31; 10:float)
      out_formatext  : OUTFORMAT_out_formatext_Field := 16#0#;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Output Scaler value (for scaled 'q31' formats)
      out_scaler     : OUTFORMAT_out_scaler_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OUTFORMAT_Register use record
      out_formatint  at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      out_formatext  at 0 range 4 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      out_scaler     at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype TMPFORMAT_tmp_formatint_Field is HAL.UInt2;
   subtype TMPFORMAT_tmp_formatext_Field is HAL.UInt2;
   subtype TMPFORMAT_tmp_scaler_Field is HAL.UInt8;

   --  Temp format
   type TMPFORMAT_Register is record
      --  Temp Internal format (00: q15; 01:q31; 10:float)
      tmp_formatint  : TMPFORMAT_tmp_formatint_Field := 16#0#;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  Temp External format (00: q15; 01:q31; 10:float)
      tmp_formatext  : TMPFORMAT_tmp_formatext_Field := 16#0#;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Temp Scaler value (for scaled 'q31' formats)
      tmp_scaler     : TMPFORMAT_tmp_scaler_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TMPFORMAT_Register use record
      tmp_formatint  at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      tmp_formatext  at 0 range 4 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      tmp_scaler     at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype INAFORMAT_ina_formatint_Field is HAL.UInt2;
   subtype INAFORMAT_ina_formatext_Field is HAL.UInt2;
   subtype INAFORMAT_ina_scaler_Field is HAL.UInt8;

   --  Input A format
   type INAFORMAT_Register is record
      --  Input A Internal format (00: q15; 01:q31; 10:float)
      ina_formatint  : INAFORMAT_ina_formatint_Field := 16#0#;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  Input A External format (00: q15; 01:q31; 10:float)
      ina_formatext  : INAFORMAT_ina_formatext_Field := 16#0#;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Input A Scaler value (for scaled 'q31' formats)
      ina_scaler     : INAFORMAT_ina_scaler_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INAFORMAT_Register use record
      ina_formatint  at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      ina_formatext  at 0 range 4 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      ina_scaler     at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype INBFORMAT_inb_formatint_Field is HAL.UInt2;
   subtype INBFORMAT_inb_formatext_Field is HAL.UInt2;
   subtype INBFORMAT_inb_scaler_Field is HAL.UInt8;

   --  Input B format
   type INBFORMAT_Register is record
      --  Input B Internal format (00: q15; 01:q31; 10:float)
      inb_formatint  : INBFORMAT_inb_formatint_Field := 16#0#;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  Input B External format (00: q15; 01:q31; 10:float)
      inb_formatext  : INBFORMAT_inb_formatext_Field := 16#0#;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Input B Scaler value (for scaled 'q31' formats)
      inb_scaler     : INBFORMAT_inb_scaler_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INBFORMAT_Register use record
      inb_formatint  at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      inb_formatext  at 0 range 4 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      inb_scaler     at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CONTROL_decode_opcode_Field is HAL.UInt4;
   subtype CONTROL_decode_machine_Field is HAL.UInt4;

   --  PowerQuad Control register
   type CONTROL_Register is record
      --  opcode specific to decode_machine
      decode_opcode  : CONTROL_decode_opcode_Field := 16#0#;
      --  0 : Coprocessor , 1 : matrix , 2 : fft , 3 : fir , 4 : stat , 5 :
      --  cordic , 6 -15 : NA
      decode_machine : CONTROL_decode_machine_Field := 16#0#;
      --  unspecified
      Reserved_8_30  : HAL.UInt23 := 16#0#;
      --  Read-only. Instruction busy signal when high indicates processing is
      --  on
      inst_busy      : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CONTROL_Register use record
      decode_opcode  at 0 range 0 .. 3;
      decode_machine at 0 range 4 .. 7;
      Reserved_8_30  at 0 range 8 .. 30;
      inst_busy      at 0 range 31 .. 31;
   end record;

   subtype CPPRE_cppre_in_Field is HAL.UInt8;
   subtype CPPRE_cppre_out_Field is HAL.UInt8;

   --  CPPRE_cppre_sat array
   type CPPRE_cppre_sat_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPPRE_cppre_sat
   type CPPRE_cppre_sat_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  cppre_sat as a value
            Val : HAL.UInt2;
         when True =>
            --  cppre_sat as an array
            Arr : CPPRE_cppre_sat_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPPRE_cppre_sat_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Pre-scale register
   type CPPRE_Register is record
      --  co-processor scaling of input
      cppre_in       : CPPRE_cppre_in_Field := 16#0#;
      --  co-processor fixed point output
      cppre_out      : CPPRE_cppre_out_Field := 16#0#;
      --  1 : forces sub-32 bit saturation
      cppre_sat      : CPPRE_cppre_sat_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_18_31 : HAL.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPPRE_Register use record
      cppre_in       at 0 range 0 .. 7;
      cppre_out      at 0 range 8 .. 15;
      cppre_sat      at 0 range 16 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   --  Cursory register
   type CURSORY_Register is record
      --  1 : Enable cursory mode
      cursory       : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CURSORY_Register use record
      cursory       at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Read/Write register where error statuses are captured (sticky)
   type ERRSTAT_Register is record
      --  overflow
      OVERFLOW      : Boolean := False;
      --  nan
      NAN           : Boolean := False;
      --  fixed_pt_overflow
      FIXEDOVERFLOW : Boolean := False;
      --  underflow
      UNDERFLOW     : Boolean := False;
      --  bus_error
      BUSERROR      : Boolean := False;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ERRSTAT_Register use record
      OVERFLOW      at 0 range 0 .. 0;
      NAN           at 0 range 1 .. 1;
      FIXEDOVERFLOW at 0 range 2 .. 2;
      UNDERFLOW     at 0 range 3 .. 3;
      BUSERROR      at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   --  INTERRUPT enable register
   type INTREN_Register is record
      --  1 : Enable interrupt on Floating point overflow
      intr_oflow    : Boolean := False;
      --  1 : Enable interrupt on Floating point NaN
      intr_nan      : Boolean := False;
      --  1: Enable interrupt on Fixed point Overflow
      intr_fixed    : Boolean := False;
      --  1 : Enable interrupt on Subnormal truncation
      intr_uflow    : Boolean := False;
      --  1: Enable interrupt on AHBM Buss Error
      intr_berr     : Boolean := False;
      --  unspecified
      Reserved_5_6  : HAL.UInt2 := 16#0#;
      --  1: Enable interrupt on instruction completion
      intr_comp     : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTREN_Register use record
      intr_oflow    at 0 range 0 .. 0;
      intr_nan      at 0 range 1 .. 1;
      intr_fixed    at 0 range 2 .. 2;
      intr_uflow    at 0 range 3 .. 3;
      intr_berr     at 0 range 4 .. 4;
      Reserved_5_6  at 0 range 5 .. 6;
      intr_comp     at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  Event Enable register
   type EVENTEN_Register is record
      --  1 : Enable event trigger on Floating point overflow
      event_oflow   : Boolean := False;
      --  1 : Enable event trigger on Floating point NaN
      event_nan     : Boolean := False;
      --  1: Enable event trigger on Fixed point Overflow
      event_fixed   : Boolean := False;
      --  1 : Enable event trigger on Subnormal truncation
      event_uflow   : Boolean := False;
      --  1: Enable event trigger on AHBM Buss Error
      event_berr    : Boolean := False;
      --  unspecified
      Reserved_5_6  : HAL.UInt2 := 16#0#;
      --  1: Enable event trigger on instruction completion
      event_comp    : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EVENTEN_Register use record
      event_oflow   at 0 range 0 .. 0;
      event_nan     at 0 range 1 .. 1;
      event_fixed   at 0 range 2 .. 2;
      event_uflow   at 0 range 3 .. 3;
      event_berr    at 0 range 4 .. 4;
      Reserved_5_6  at 0 range 5 .. 6;
      event_comp    at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  INTERRUPT STATUS register
   type INTRSTAT_Register is record
      --  Intr status ( 1 bit to indicate interrupt captured, 0 means no new
      --  interrupt), write any value will clear this bit
      intr_stat     : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTRSTAT_Register use record
      intr_stat     at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  General purpose register bank N.

   --  General purpose register bank N.
   type gpreg_Registers is array (0 .. 15) of HAL.UInt32
     with Volatile;

   --  Compute register bank

   --  Compute register bank
   type compreg_Registers is array (0 .. 7) of HAL.UInt32
     with Volatile;

   -----------------
   -- Peripherals --
   -----------------

   --  Digital Signal Co-Processing companion to a Cortex-M v8M CPU core
   type POWERQUAD_Peripheral is record
      --  Base address register for output region
      OUTBASE   : aliased HAL.UInt32;
      --  Output format
      OUTFORMAT : aliased OUTFORMAT_Register;
      --  Base address register for temp region
      TMPBASE   : aliased HAL.UInt32;
      --  Temp format
      TMPFORMAT : aliased TMPFORMAT_Register;
      --  Base address register for input A region
      INABASE   : aliased HAL.UInt32;
      --  Input A format
      INAFORMAT : aliased INAFORMAT_Register;
      --  Base address register for input B region
      INBBASE   : aliased HAL.UInt32;
      --  Input B format
      INBFORMAT : aliased INBFORMAT_Register;
      --  PowerQuad Control register
      CONTROL   : aliased CONTROL_Register;
      --  Length register
      LENGTH    : aliased HAL.UInt32;
      --  Pre-scale register
      CPPRE     : aliased CPPRE_Register;
      --  Misc register
      MISC      : aliased HAL.UInt32;
      --  Cursory register
      CURSORY   : aliased CURSORY_Register;
      --  Cordic input X register
      CORDIC_X  : aliased HAL.UInt32;
      --  Cordic input Y register
      CORDIC_Y  : aliased HAL.UInt32;
      --  Cordic input Z register
      CORDIC_Z  : aliased HAL.UInt32;
      --  Read/Write register where error statuses are captured (sticky)
      ERRSTAT   : aliased ERRSTAT_Register;
      --  INTERRUPT enable register
      INTREN    : aliased INTREN_Register;
      --  Event Enable register
      EVENTEN   : aliased EVENTEN_Register;
      --  INTERRUPT STATUS register
      INTRSTAT  : aliased INTRSTAT_Register;
      --  General purpose register bank N.
      gpreg     : aliased gpreg_Registers;
      --  Compute register bank
      compreg   : aliased compreg_Registers;
   end record
     with Volatile;

   for POWERQUAD_Peripheral use record
      OUTBASE   at 16#0# range 0 .. 31;
      OUTFORMAT at 16#4# range 0 .. 31;
      TMPBASE   at 16#8# range 0 .. 31;
      TMPFORMAT at 16#C# range 0 .. 31;
      INABASE   at 16#10# range 0 .. 31;
      INAFORMAT at 16#14# range 0 .. 31;
      INBBASE   at 16#18# range 0 .. 31;
      INBFORMAT at 16#1C# range 0 .. 31;
      CONTROL   at 16#100# range 0 .. 31;
      LENGTH    at 16#104# range 0 .. 31;
      CPPRE     at 16#108# range 0 .. 31;
      MISC      at 16#10C# range 0 .. 31;
      CURSORY   at 16#110# range 0 .. 31;
      CORDIC_X  at 16#180# range 0 .. 31;
      CORDIC_Y  at 16#184# range 0 .. 31;
      CORDIC_Z  at 16#188# range 0 .. 31;
      ERRSTAT   at 16#18C# range 0 .. 31;
      INTREN    at 16#190# range 0 .. 31;
      EVENTEN   at 16#194# range 0 .. 31;
      INTRSTAT  at 16#198# range 0 .. 31;
      gpreg     at 16#200# range 0 .. 511;
      compreg   at 16#240# range 0 .. 255;
   end record;

   --  Digital Signal Co-Processing companion to a Cortex-M v8M CPU core
   POWERQUAD_Periph : aliased POWERQUAD_Peripheral
     with Import, Address => System'To_Address (16#400A6000#);

end NXP_SVD.POWERQUAD;
