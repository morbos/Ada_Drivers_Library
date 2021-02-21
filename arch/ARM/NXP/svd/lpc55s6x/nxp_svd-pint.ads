--  Copyright 2016-2019 NXP
--  All rights reserved.SPDX-License-Identifier: BSD-3-Clause

--  This spec has been automatically generated from LPC55S6x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NXP_SVD.PINT is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype ISEL_PMODE_Field is HAL.UInt8;

   --  Pin Interrupt Mode register
   type ISEL_Register is record
      --  Selects the interrupt mode for each pin interrupt. Bit n configures
      --  the pin interrupt selected in PINTSELn. 0 = Edge sensitive 1 = Level
      --  sensitive
      PMODE         : ISEL_PMODE_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISEL_Register use record
      PMODE         at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype IENR_ENRL_Field is HAL.UInt8;

   --  Pin interrupt level or rising edge interrupt enable register
   type IENR_Register is record
      --  Enables the rising edge or level interrupt for each pin interrupt.
      --  Bit n configures the pin interrupt selected in PINTSELn. 0 = Disable
      --  rising edge or level interrupt. 1 = Enable rising edge or level
      --  interrupt.
      ENRL          : IENR_ENRL_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IENR_Register use record
      ENRL          at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype SIENR_SETENRL_Field is HAL.UInt8;

   --  Pin interrupt level or rising edge interrupt set register
   type SIENR_Register is record
      --  Write-only. Ones written to this address set bits in the IENR, thus
      --  enabling interrupts. Bit n sets bit n in the IENR register. 0 = No
      --  operation. 1 = Enable rising edge or level interrupt.
      SETENRL       : SIENR_SETENRL_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SIENR_Register use record
      SETENRL       at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype CIENR_CENRL_Field is HAL.UInt8;

   --  Pin interrupt level (rising edge interrupt) clear register
   type CIENR_Register is record
      --  Write-only. Ones written to this address clear bits in the IENR, thus
      --  disabling the interrupts. Bit n clears bit n in the IENR register. 0
      --  = No operation. 1 = Disable rising edge or level interrupt.
      CENRL         : CIENR_CENRL_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CIENR_Register use record
      CENRL         at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype IENF_ENAF_Field is HAL.UInt8;

   --  Pin interrupt active level or falling edge interrupt enable register
   type IENF_Register is record
      --  Enables the falling edge or configures the active level interrupt for
      --  each pin interrupt. Bit n configures the pin interrupt selected in
      --  PINTSELn. 0 = Disable falling edge interrupt or set active interrupt
      --  level LOW. 1 = Enable falling edge interrupt enabled or set active
      --  interrupt level HIGH.
      ENAF          : IENF_ENAF_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IENF_Register use record
      ENAF          at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype SIENF_SETENAF_Field is HAL.UInt8;

   --  Pin interrupt active level or falling edge interrupt set register
   type SIENF_Register is record
      --  Write-only. Ones written to this address set bits in the IENF, thus
      --  enabling interrupts. Bit n sets bit n in the IENF register. 0 = No
      --  operation. 1 = Select HIGH-active interrupt or enable falling edge
      --  interrupt.
      SETENAF       : SIENF_SETENAF_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SIENF_Register use record
      SETENAF       at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype CIENF_CENAF_Field is HAL.UInt8;

   --  Pin interrupt active level or falling edge interrupt clear register
   type CIENF_Register is record
      --  Write-only. Ones written to this address clears bits in the IENF,
      --  thus disabling interrupts. Bit n clears bit n in the IENF register. 0
      --  = No operation. 1 = LOW-active interrupt selected or falling edge
      --  interrupt disabled.
      CENAF         : CIENF_CENAF_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CIENF_Register use record
      CENAF         at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype RISE_RDET_Field is HAL.UInt8;

   --  Pin interrupt rising edge register
   type RISE_Register is record
      --  Rising edge detect. Bit n detects the rising edge of the pin selected
      --  in PINTSELn. Read 0: No rising edge has been detected on this pin
      --  since Reset or the last time a one was written to this bit. Write 0:
      --  no operation. Read 1: a rising edge has been detected since Reset or
      --  the last time a one was written to this bit. Write 1: clear rising
      --  edge detection for this pin.
      RDET          : RISE_RDET_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RISE_Register use record
      RDET          at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype FALL_FDET_Field is HAL.UInt8;

   --  Pin interrupt falling edge register
   type FALL_Register is record
      --  Falling edge detect. Bit n detects the falling edge of the pin
      --  selected in PINTSELn. Read 0: No falling edge has been detected on
      --  this pin since Reset or the last time a one was written to this bit.
      --  Write 0: no operation. Read 1: a falling edge has been detected since
      --  Reset or the last time a one was written to this bit. Write 1: clear
      --  falling edge detection for this pin.
      FDET          : FALL_FDET_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FALL_Register use record
      FDET          at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype IST_PSTAT_Field is HAL.UInt8;

   --  Pin interrupt status register
   type IST_Register is record
      --  Pin interrupt status. Bit n returns the status, clears the edge
      --  interrupt, or inverts the active level of the pin selected in
      --  PINTSELn. Read 0: interrupt is not being requested for this interrupt
      --  pin. Write 0: no operation. Read 1: interrupt is being requested for
      --  this interrupt pin. Write 1 (edge-sensitive): clear rising- and
      --  falling-edge detection for this pin. Write 1 (level-sensitive):
      --  switch the active level for this pin (in the IENF register).
      PSTAT         : IST_PSTAT_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IST_Register use record
      PSTAT         at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  Specifies whether the 8 pin interrupts are controlled by the pin
   --  interrupt function or by the pattern match function.
   type PMCTRL_SEL_PMATCH_Field is
     (
      --  Pin interrupt. Interrupts are driven in response to the standard pin
      --  interrupt function.
      Pin_Interrupt,
      --  Pattern match. Interrupts are driven in response to pattern matches.
      Pattern_Match)
     with Size => 1;
   for PMCTRL_SEL_PMATCH_Field use
     (Pin_Interrupt => 0,
      Pattern_Match => 1);

   --  Enables the RXEV output to the CPU and/or to a GPIO output when the
   --  specified boolean expression evaluates to true.
   type PMCTRL_ENA_RXEV_Field is
     (
      --  Disabled. RXEV output to the CPU is disabled.
      Disabled,
      --  Enabled. RXEV output to the CPU is enabled.
      Enabled)
     with Size => 1;
   for PMCTRL_ENA_RXEV_Field use
     (Disabled => 0,
      Enabled => 1);

   subtype PMCTRL_PMAT_Field is HAL.UInt8;

   --  Pattern match interrupt control register
   type PMCTRL_Register is record
      --  Specifies whether the 8 pin interrupts are controlled by the pin
      --  interrupt function or by the pattern match function.
      SEL_PMATCH    : PMCTRL_SEL_PMATCH_Field := NXP_SVD.PINT.Pin_Interrupt;
      --  Enables the RXEV output to the CPU and/or to a GPIO output when the
      --  specified boolean expression evaluates to true.
      ENA_RXEV      : PMCTRL_ENA_RXEV_Field := NXP_SVD.PINT.Disabled;
      --  unspecified
      Reserved_2_23 : HAL.UInt22 := 16#0#;
      --  This field displays the current state of pattern matches. A 1 in any
      --  bit of this field indicates that the corresponding product term is
      --  matched by the current state of the appropriate inputs.
      PMAT          : PMCTRL_PMAT_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMCTRL_Register use record
      SEL_PMATCH    at 0 range 0 .. 0;
      ENA_RXEV      at 0 range 1 .. 1;
      Reserved_2_23 at 0 range 2 .. 23;
      PMAT          at 0 range 24 .. 31;
   end record;

   --  Selects the input source for bit slice 0
   type PMSRC_SRC0_Field is
     (
      --  Input 0. Selects the pin selected in the PINTSEL0 register as the
      --  source to bit slice 0.
      Input0,
      --  Input 1. Selects the pin selected in the PINTSEL1 register as the
      --  source to bit slice 0.
      Input1,
      --  Input 2. Selects the pin selected in the PINTSEL2 register as the
      --  source to bit slice 0.
      Input2,
      --  Input 3. Selects the pin selected in the PINTSEL3 register as the
      --  source to bit slice 0.
      Input3,
      --  Input 4. Selects the pin selected in the PINTSEL4 register as the
      --  source to bit slice 0.
      Input4,
      --  Input 5. Selects the pin selected in the PINTSEL5 register as the
      --  source to bit slice 0.
      Input5,
      --  Input 6. Selects the pin selected in the PINTSEL6 register as the
      --  source to bit slice 0.
      Input6,
      --  Input 7. Selects the pin selected in the PINTSEL7 register as the
      --  source to bit slice 0.
      Input7)
     with Size => 3;
   for PMSRC_SRC0_Field use
     (Input0 => 0,
      Input1 => 1,
      Input2 => 2,
      Input3 => 3,
      Input4 => 4,
      Input5 => 5,
      Input6 => 6,
      Input7 => 7);

   --  Selects the input source for bit slice 1
   type PMSRC_SRC1_Field is
     (
      --  Input 0. Selects the pin selected in the PINTSEL0 register as the
      --  source to bit slice 1.
      Input0,
      --  Input 1. Selects the pin selected in the PINTSEL1 register as the
      --  source to bit slice 1.
      Input1,
      --  Input 2. Selects the pin selected in the PINTSEL2 register as the
      --  source to bit slice 1.
      Input2,
      --  Input 3. Selects the pin selected in the PINTSEL3 register as the
      --  source to bit slice 1.
      Input3,
      --  Input 4. Selects the pin selected in the PINTSEL4 register as the
      --  source to bit slice 1.
      Input4,
      --  Input 5. Selects the pin selected in the PINTSEL5 register as the
      --  source to bit slice 1.
      Input5,
      --  Input 6. Selects the pin selected in the PINTSEL6 register as the
      --  source to bit slice 1.
      Input6,
      --  Input 7. Selects the pin selected in the PINTSEL7 register as the
      --  source to bit slice 1.
      Input7)
     with Size => 3;
   for PMSRC_SRC1_Field use
     (Input0 => 0,
      Input1 => 1,
      Input2 => 2,
      Input3 => 3,
      Input4 => 4,
      Input5 => 5,
      Input6 => 6,
      Input7 => 7);

   --  Selects the input source for bit slice 2
   type PMSRC_SRC2_Field is
     (
      --  Input 0. Selects the pin selected in the PINTSEL0 register as the
      --  source to bit slice 2.
      Input0,
      --  Input 1. Selects the pin selected in the PINTSEL1 register as the
      --  source to bit slice 2.
      Input1,
      --  Input 2. Selects the pin selected in the PINTSEL2 register as the
      --  source to bit slice 2.
      Input2,
      --  Input 3. Selects the pin selected in the PINTSEL3 register as the
      --  source to bit slice 2.
      Input3,
      --  Input 4. Selects the pin selected in the PINTSEL4 register as the
      --  source to bit slice 2.
      Input4,
      --  Input 5. Selects the pin selected in the PINTSEL5 register as the
      --  source to bit slice 2.
      Input5,
      --  Input 6. Selects the pin selected in the PINTSEL6 register as the
      --  source to bit slice 2.
      Input6,
      --  Input 7. Selects the pin selected in the PINTSEL7 register as the
      --  source to bit slice 2.
      Input7)
     with Size => 3;
   for PMSRC_SRC2_Field use
     (Input0 => 0,
      Input1 => 1,
      Input2 => 2,
      Input3 => 3,
      Input4 => 4,
      Input5 => 5,
      Input6 => 6,
      Input7 => 7);

   --  Selects the input source for bit slice 3
   type PMSRC_SRC3_Field is
     (
      --  Input 0. Selects the pin selected in the PINTSEL0 register as the
      --  source to bit slice 3.
      Input0,
      --  Input 1. Selects the pin selected in the PINTSEL1 register as the
      --  source to bit slice 3.
      Input1,
      --  Input 2. Selects the pin selected in the PINTSEL2 register as the
      --  source to bit slice 3.
      Input2,
      --  Input 3. Selects the pin selected in the PINTSEL3 register as the
      --  source to bit slice 3.
      Input3,
      --  Input 4. Selects the pin selected in the PINTSEL4 register as the
      --  source to bit slice 3.
      Input4,
      --  Input 5. Selects the pin selected in the PINTSEL5 register as the
      --  source to bit slice 3.
      Input5,
      --  Input 6. Selects the pin selected in the PINTSEL6 register as the
      --  source to bit slice 3.
      Input6,
      --  Input 7. Selects the pin selected in the PINTSEL7 register as the
      --  source to bit slice 3.
      Input7)
     with Size => 3;
   for PMSRC_SRC3_Field use
     (Input0 => 0,
      Input1 => 1,
      Input2 => 2,
      Input3 => 3,
      Input4 => 4,
      Input5 => 5,
      Input6 => 6,
      Input7 => 7);

   --  Selects the input source for bit slice 4
   type PMSRC_SRC4_Field is
     (
      --  Input 0. Selects the pin selected in the PINTSEL0 register as the
      --  source to bit slice 4.
      Input0,
      --  Input 1. Selects the pin selected in the PINTSEL1 register as the
      --  source to bit slice 4.
      Input1,
      --  Input 2. Selects the pin selected in the PINTSEL2 register as the
      --  source to bit slice 4.
      Input2,
      --  Input 3. Selects the pin selected in the PINTSEL3 register as the
      --  source to bit slice 4.
      Input3,
      --  Input 4. Selects the pin selected in the PINTSEL4 register as the
      --  source to bit slice 4.
      Input4,
      --  Input 5. Selects the pin selected in the PINTSEL5 register as the
      --  source to bit slice 4.
      Input5,
      --  Input 6. Selects the pin selected in the PINTSEL6 register as the
      --  source to bit slice 4.
      Input6,
      --  Input 7. Selects the pin selected in the PINTSEL7 register as the
      --  source to bit slice 4.
      Input7)
     with Size => 3;
   for PMSRC_SRC4_Field use
     (Input0 => 0,
      Input1 => 1,
      Input2 => 2,
      Input3 => 3,
      Input4 => 4,
      Input5 => 5,
      Input6 => 6,
      Input7 => 7);

   --  Selects the input source for bit slice 5
   type PMSRC_SRC5_Field is
     (
      --  Input 0. Selects the pin selected in the PINTSEL0 register as the
      --  source to bit slice 5.
      Input0,
      --  Input 1. Selects the pin selected in the PINTSEL1 register as the
      --  source to bit slice 5.
      Input1,
      --  Input 2. Selects the pin selected in the PINTSEL2 register as the
      --  source to bit slice 5.
      Input2,
      --  Input 3. Selects the pin selected in the PINTSEL3 register as the
      --  source to bit slice 5.
      Input3,
      --  Input 4. Selects the pin selected in the PINTSEL4 register as the
      --  source to bit slice 5.
      Input4,
      --  Input 5. Selects the pin selected in the PINTSEL5 register as the
      --  source to bit slice 5.
      Input5,
      --  Input 6. Selects the pin selected in the PINTSEL6 register as the
      --  source to bit slice 5.
      Input6,
      --  Input 7. Selects the pin selected in the PINTSEL7 register as the
      --  source to bit slice 5.
      Input7)
     with Size => 3;
   for PMSRC_SRC5_Field use
     (Input0 => 0,
      Input1 => 1,
      Input2 => 2,
      Input3 => 3,
      Input4 => 4,
      Input5 => 5,
      Input6 => 6,
      Input7 => 7);

   --  Selects the input source for bit slice 6
   type PMSRC_SRC6_Field is
     (
      --  Input 0. Selects the pin selected in the PINTSEL0 register as the
      --  source to bit slice 6.
      Input0,
      --  Input 1. Selects the pin selected in the PINTSEL1 register as the
      --  source to bit slice 6.
      Input1,
      --  Input 2. Selects the pin selected in the PINTSEL2 register as the
      --  source to bit slice 6.
      Input2,
      --  Input 3. Selects the pin selected in the PINTSEL3 register as the
      --  source to bit slice 6.
      Input3,
      --  Input 4. Selects the pin selected in the PINTSEL4 register as the
      --  source to bit slice 6.
      Input4,
      --  Input 5. Selects the pin selected in the PINTSEL5 register as the
      --  source to bit slice 6.
      Input5,
      --  Input 6. Selects the pin selected in the PINTSEL6 register as the
      --  source to bit slice 6.
      Input6,
      --  Input 7. Selects the pin selected in the PINTSEL7 register as the
      --  source to bit slice 6.
      Input7)
     with Size => 3;
   for PMSRC_SRC6_Field use
     (Input0 => 0,
      Input1 => 1,
      Input2 => 2,
      Input3 => 3,
      Input4 => 4,
      Input5 => 5,
      Input6 => 6,
      Input7 => 7);

   --  Selects the input source for bit slice 7
   type PMSRC_SRC7_Field is
     (
      --  Input 0. Selects the pin selected in the PINTSEL0 register as the
      --  source to bit slice 7.
      Input0,
      --  Input 1. Selects the pin selected in the PINTSEL1 register as the
      --  source to bit slice 7.
      Input1,
      --  Input 2. Selects the pin selected in the PINTSEL2 register as the
      --  source to bit slice 7.
      Input2,
      --  Input 3. Selects the pin selected in the PINTSEL3 register as the
      --  source to bit slice 7.
      Input3,
      --  Input 4. Selects the pin selected in the PINTSEL4 register as the
      --  source to bit slice 7.
      Input4,
      --  Input 5. Selects the pin selected in the PINTSEL5 register as the
      --  source to bit slice 7.
      Input5,
      --  Input 6. Selects the pin selected in the PINTSEL6 register as the
      --  source to bit slice 7.
      Input6,
      --  Input 7. Selects the pin selected in the PINTSEL7 register as the
      --  source to bit slice 7.
      Input7)
     with Size => 3;
   for PMSRC_SRC7_Field use
     (Input0 => 0,
      Input1 => 1,
      Input2 => 2,
      Input3 => 3,
      Input4 => 4,
      Input5 => 5,
      Input6 => 6,
      Input7 => 7);

   --  Pattern match interrupt bit-slice source register
   type PMSRC_Register is record
      --  unspecified
      Reserved_0_7 : HAL.UInt8 := 16#0#;
      --  Selects the input source for bit slice 0
      SRC0         : PMSRC_SRC0_Field := NXP_SVD.PINT.Input0;
      --  Selects the input source for bit slice 1
      SRC1         : PMSRC_SRC1_Field := NXP_SVD.PINT.Input0;
      --  Selects the input source for bit slice 2
      SRC2         : PMSRC_SRC2_Field := NXP_SVD.PINT.Input0;
      --  Selects the input source for bit slice 3
      SRC3         : PMSRC_SRC3_Field := NXP_SVD.PINT.Input0;
      --  Selects the input source for bit slice 4
      SRC4         : PMSRC_SRC4_Field := NXP_SVD.PINT.Input0;
      --  Selects the input source for bit slice 5
      SRC5         : PMSRC_SRC5_Field := NXP_SVD.PINT.Input0;
      --  Selects the input source for bit slice 6
      SRC6         : PMSRC_SRC6_Field := NXP_SVD.PINT.Input0;
      --  Selects the input source for bit slice 7
      SRC7         : PMSRC_SRC7_Field := NXP_SVD.PINT.Input0;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMSRC_Register use record
      Reserved_0_7 at 0 range 0 .. 7;
      SRC0         at 0 range 8 .. 10;
      SRC1         at 0 range 11 .. 13;
      SRC2         at 0 range 14 .. 16;
      SRC3         at 0 range 17 .. 19;
      SRC4         at 0 range 20 .. 22;
      SRC5         at 0 range 23 .. 25;
      SRC6         at 0 range 26 .. 28;
      SRC7         at 0 range 29 .. 31;
   end record;

   --  Determines whether slice 0 is an endpoint.
   type PMCFG_PROD_ENDPTS0_Field is
     (
      --  No effect. Slice 0 is not an endpoint.
      No_Effect,
      --  endpoint. Slice 0 is the endpoint of a product term (minterm). Pin
      --  interrupt 0 in the NVIC is raised if the minterm evaluates as true.
      Endpoint)
     with Size => 1;
   for PMCFG_PROD_ENDPTS0_Field use
     (No_Effect => 0,
      Endpoint => 1);

   --  Determines whether slice 1 is an endpoint.
   type PMCFG_PROD_ENDPTS1_Field is
     (
      --  No effect. Slice 1 is not an endpoint.
      No_Effect,
      --  endpoint. Slice 1 is the endpoint of a product term (minterm). Pin
      --  interrupt 1 in the NVIC is raised if the minterm evaluates as true.
      Endpoint)
     with Size => 1;
   for PMCFG_PROD_ENDPTS1_Field use
     (No_Effect => 0,
      Endpoint => 1);

   --  Determines whether slice 2 is an endpoint.
   type PMCFG_PROD_ENDPTS2_Field is
     (
      --  No effect. Slice 2 is not an endpoint.
      No_Effect,
      --  endpoint. Slice 2 is the endpoint of a product term (minterm). Pin
      --  interrupt 2 in the NVIC is raised if the minterm evaluates as true.
      Endpoint)
     with Size => 1;
   for PMCFG_PROD_ENDPTS2_Field use
     (No_Effect => 0,
      Endpoint => 1);

   --  Determines whether slice 3 is an endpoint.
   type PMCFG_PROD_ENDPTS3_Field is
     (
      --  No effect. Slice 3 is not an endpoint.
      No_Effect,
      --  endpoint. Slice 3 is the endpoint of a product term (minterm). Pin
      --  interrupt 3 in the NVIC is raised if the minterm evaluates as true.
      Endpoint)
     with Size => 1;
   for PMCFG_PROD_ENDPTS3_Field use
     (No_Effect => 0,
      Endpoint => 1);

   --  Determines whether slice 4 is an endpoint.
   type PMCFG_PROD_ENDPTS4_Field is
     (
      --  No effect. Slice 4 is not an endpoint.
      No_Effect,
      --  endpoint. Slice 4 is the endpoint of a product term (minterm). Pin
      --  interrupt 4 in the NVIC is raised if the minterm evaluates as true.
      Endpoint)
     with Size => 1;
   for PMCFG_PROD_ENDPTS4_Field use
     (No_Effect => 0,
      Endpoint => 1);

   --  Determines whether slice 5 is an endpoint.
   type PMCFG_PROD_ENDPTS5_Field is
     (
      --  No effect. Slice 5 is not an endpoint.
      No_Effect,
      --  endpoint. Slice 5 is the endpoint of a product term (minterm). Pin
      --  interrupt 5 in the NVIC is raised if the minterm evaluates as true.
      Endpoint)
     with Size => 1;
   for PMCFG_PROD_ENDPTS5_Field use
     (No_Effect => 0,
      Endpoint => 1);

   --  Determines whether slice 6 is an endpoint.
   type PMCFG_PROD_ENDPTS6_Field is
     (
      --  No effect. Slice 6 is not an endpoint.
      No_Effect,
      --  endpoint. Slice 6 is the endpoint of a product term (minterm). Pin
      --  interrupt 6 in the NVIC is raised if the minterm evaluates as true.
      Endpoint)
     with Size => 1;
   for PMCFG_PROD_ENDPTS6_Field use
     (No_Effect => 0,
      Endpoint => 1);

   --  Specifies the match contribution condition for bit slice 0.
   type PMCFG_CFG0_Field is
     (
      --  Constant HIGH. This bit slice always contributes to a product term
      --  match.
      Constant_High,
      --  Sticky rising edge. Match occurs if a rising edge on the specified
      --  input has occurred since the last time the edge detection for this
      --  bit slice was cleared. This bit is only cleared when the PMCFG or the
      --  PMSRC registers are written to.
      Sticky_Rising_Edge,
      --  Sticky falling edge. Match occurs if a falling edge on the specified
      --  input has occurred since the last time the edge detection for this
      --  bit slice was cleared. This bit is only cleared when the PMCFG or the
      --  PMSRC registers are written to.
      Sticky_Falling_Edge,
      --  Sticky rising or falling edge. Match occurs if either a rising or
      --  falling edge on the specified input has occurred since the last time
      --  the edge detection for this bit slice was cleared. This bit is only
      --  cleared when the PMCFG or the PMSRC registers are written to.
      Sticky_Rising_Falling_Edge,
      --  High level. Match (for this bit slice) occurs when there is a high
      --  level on the input specified for this bit slice in the PMSRC
      --  register.
      High_Level,
      --  Low level. Match occurs when there is a low level on the specified
      --  input.
      Low_Level,
      --  Constant 0. This bit slice never contributes to a match (should be
      --  used to disable any unused bit slices).
      Constant_Zero,
      --  Event. Non-sticky rising or falling edge. Match occurs on an event -
      --  i.e. when either a rising or falling edge is first detected on the
      --  specified input (this is a non-sticky version of value 0x3) . This
      --  bit is cleared after one clock cycle.
      Event)
     with Size => 3;
   for PMCFG_CFG0_Field use
     (Constant_High => 0,
      Sticky_Rising_Edge => 1,
      Sticky_Falling_Edge => 2,
      Sticky_Rising_Falling_Edge => 3,
      High_Level => 4,
      Low_Level => 5,
      Constant_Zero => 6,
      Event => 7);

   --  PMCFG_CFG array
   type PMCFG_CFG_Field_Array is array (0 .. 7) of PMCFG_CFG0_Field
     with Component_Size => 3, Size => 24;

   --  Type definition for PMCFG_CFG
   type PMCFG_CFG_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CFG as a value
            Val : HAL.UInt24;
         when True =>
            --  CFG as an array
            Arr : PMCFG_CFG_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 24;

   for PMCFG_CFG_Field use record
      Val at 0 range 0 .. 23;
      Arr at 0 range 0 .. 23;
   end record;

   --  Pattern match interrupt bit slice configuration register
   type PMCFG_Register is record
      --  Determines whether slice 0 is an endpoint.
      PROD_ENDPTS0 : PMCFG_PROD_ENDPTS0_Field := NXP_SVD.PINT.No_Effect;
      --  Determines whether slice 1 is an endpoint.
      PROD_ENDPTS1 : PMCFG_PROD_ENDPTS1_Field := NXP_SVD.PINT.No_Effect;
      --  Determines whether slice 2 is an endpoint.
      PROD_ENDPTS2 : PMCFG_PROD_ENDPTS2_Field := NXP_SVD.PINT.No_Effect;
      --  Determines whether slice 3 is an endpoint.
      PROD_ENDPTS3 : PMCFG_PROD_ENDPTS3_Field := NXP_SVD.PINT.No_Effect;
      --  Determines whether slice 4 is an endpoint.
      PROD_ENDPTS4 : PMCFG_PROD_ENDPTS4_Field := NXP_SVD.PINT.No_Effect;
      --  Determines whether slice 5 is an endpoint.
      PROD_ENDPTS5 : PMCFG_PROD_ENDPTS5_Field := NXP_SVD.PINT.No_Effect;
      --  Determines whether slice 6 is an endpoint.
      PROD_ENDPTS6 : PMCFG_PROD_ENDPTS6_Field := NXP_SVD.PINT.No_Effect;
      --  unspecified
      Reserved_7_7 : HAL.Bit := 16#0#;
      --  Specifies the match contribution condition for bit slice 0.
      CFG          : PMCFG_CFG_Field := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMCFG_Register use record
      PROD_ENDPTS0 at 0 range 0 .. 0;
      PROD_ENDPTS1 at 0 range 1 .. 1;
      PROD_ENDPTS2 at 0 range 2 .. 2;
      PROD_ENDPTS3 at 0 range 3 .. 3;
      PROD_ENDPTS4 at 0 range 4 .. 4;
      PROD_ENDPTS5 at 0 range 5 .. 5;
      PROD_ENDPTS6 at 0 range 6 .. 6;
      Reserved_7_7 at 0 range 7 .. 7;
      CFG          at 0 range 8 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Pin interrupt and pattern match (PINT)
   type PINT_Peripheral is record
      --  Pin Interrupt Mode register
      ISEL   : aliased ISEL_Register;
      --  Pin interrupt level or rising edge interrupt enable register
      IENR   : aliased IENR_Register;
      --  Pin interrupt level or rising edge interrupt set register
      SIENR  : aliased SIENR_Register;
      --  Pin interrupt level (rising edge interrupt) clear register
      CIENR  : aliased CIENR_Register;
      --  Pin interrupt active level or falling edge interrupt enable register
      IENF   : aliased IENF_Register;
      --  Pin interrupt active level or falling edge interrupt set register
      SIENF  : aliased SIENF_Register;
      --  Pin interrupt active level or falling edge interrupt clear register
      CIENF  : aliased CIENF_Register;
      --  Pin interrupt rising edge register
      RISE   : aliased RISE_Register;
      --  Pin interrupt falling edge register
      FALL   : aliased FALL_Register;
      --  Pin interrupt status register
      IST    : aliased IST_Register;
      --  Pattern match interrupt control register
      PMCTRL : aliased PMCTRL_Register;
      --  Pattern match interrupt bit-slice source register
      PMSRC  : aliased PMSRC_Register;
      --  Pattern match interrupt bit slice configuration register
      PMCFG  : aliased PMCFG_Register;
   end record
     with Volatile;

   for PINT_Peripheral use record
      ISEL   at 16#0# range 0 .. 31;
      IENR   at 16#4# range 0 .. 31;
      SIENR  at 16#8# range 0 .. 31;
      CIENR  at 16#C# range 0 .. 31;
      IENF   at 16#10# range 0 .. 31;
      SIENF  at 16#14# range 0 .. 31;
      CIENF  at 16#18# range 0 .. 31;
      RISE   at 16#1C# range 0 .. 31;
      FALL   at 16#20# range 0 .. 31;
      IST    at 16#24# range 0 .. 31;
      PMCTRL at 16#28# range 0 .. 31;
      PMSRC  at 16#2C# range 0 .. 31;
      PMCFG  at 16#30# range 0 .. 31;
   end record;

   --  Pin interrupt and pattern match (PINT)
   PINT_Periph : aliased PINT_Peripheral
     with Import, Address => System'To_Address (16#40004000#);

   --  Pin interrupt and pattern match (PINT)
   SECPINT_Periph : aliased PINT_Peripheral
     with Import, Address => System'To_Address (16#40005000#);

end NXP_SVD.PINT;
