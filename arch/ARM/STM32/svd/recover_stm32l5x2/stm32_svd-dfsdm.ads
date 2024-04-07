--  This spec has been automatically generated from xyzzy

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.DFSDM is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   ----------------------------
   -- CH cluster's Registers --
   ----------------------------

   subtype CFGR1_CH_SITP_Field is HAL.UInt2;
   subtype CFGR1_CH_SPICKSEL_Field is HAL.UInt2;
   subtype CFGR1_CH_DATMPX_Field is HAL.UInt2;
   subtype CFGR1_CH_DATPACK_Field is HAL.UInt2;
   subtype CFGR1_CH_CKOUTDIV_Field is HAL.UInt8;

   --  channel configuration y register
   type CFGR1_CH_Register is record
      --  SITP
      SITP           : CFGR1_CH_SITP_Field := 16#0#;
      --  SPICKSEL
      SPICKSEL       : CFGR1_CH_SPICKSEL_Field := 16#0#;
      --  unspecified
      Reserved_4_4   : HAL.Bit := 16#0#;
      --  SCDEN
      SCDEN          : Boolean := False;
      --  CKABEN
      CKABEN         : Boolean := False;
      --  CHEN
      CHEN           : Boolean := False;
      --  CHINSEL
      CHINSEL        : Boolean := False;
      --  unspecified
      Reserved_9_11  : HAL.UInt3 := 16#0#;
      --  DATMPX
      DATMPX         : CFGR1_CH_DATMPX_Field := 16#0#;
      --  DATPACK
      DATPACK        : CFGR1_CH_DATPACK_Field := 16#0#;
      --  CKOUTDIV
      CKOUTDIV       : CFGR1_CH_CKOUTDIV_Field := 16#0#;
      --  unspecified
      Reserved_24_29 : HAL.UInt6 := 16#0#;
      --  CKOUTSRC
      CKOUTSRC       : Boolean := False;
      --  DFSDMEN
      DFSDMEN        : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CFGR1_CH_Register use record
      SITP           at 0 range 0 .. 1;
      SPICKSEL       at 0 range 2 .. 3;
      Reserved_4_4   at 0 range 4 .. 4;
      SCDEN          at 0 range 5 .. 5;
      CKABEN         at 0 range 6 .. 6;
      CHEN           at 0 range 7 .. 7;
      CHINSEL        at 0 range 8 .. 8;
      Reserved_9_11  at 0 range 9 .. 11;
      DATMPX         at 0 range 12 .. 13;
      DATPACK        at 0 range 14 .. 15;
      CKOUTDIV       at 0 range 16 .. 23;
      Reserved_24_29 at 0 range 24 .. 29;
      CKOUTSRC       at 0 range 30 .. 30;
      DFSDMEN        at 0 range 31 .. 31;
   end record;

   subtype CFGR2_CH_DTRBS_Field is HAL.UInt5;
   subtype CFGR2_CH_OFFSET_Field is HAL.UInt24;

   --  channel configuration y register
   type CFGR2_CH_Register is record
      --  unspecified
      Reserved_0_2 : HAL.UInt3 := 16#0#;
      --  DTRBS
      DTRBS        : CFGR2_CH_DTRBS_Field := 16#0#;
      --  OFFSET
      OFFSET       : CFGR2_CH_OFFSET_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CFGR2_CH_Register use record
      Reserved_0_2 at 0 range 0 .. 2;
      DTRBS        at 0 range 3 .. 7;
      OFFSET       at 0 range 8 .. 31;
   end record;

   subtype AWSCDR_CH_SCDT_Field is HAL.UInt8;
   subtype AWSCDR_CH_BKSCD_Field is HAL.UInt4;
   subtype AWSCDR_CH_AWFOSR_Field is HAL.UInt5;
   subtype AWSCDR_CH_AWFORD_Field is HAL.UInt2;

   --  analog watchdog and short-circuit detector register
   type AWSCDR_CH_Register is record
      --  SCDT
      SCDT           : AWSCDR_CH_SCDT_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : HAL.UInt4 := 16#0#;
      --  BKSCD
      BKSCD          : AWSCDR_CH_BKSCD_Field := 16#0#;
      --  AWFOSR
      AWFOSR         : AWSCDR_CH_AWFOSR_Field := 16#0#;
      --  unspecified
      Reserved_21_21 : HAL.Bit := 16#0#;
      --  AWFORD
      AWFORD         : AWSCDR_CH_AWFORD_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AWSCDR_CH_Register use record
      SCDT           at 0 range 0 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      BKSCD          at 0 range 12 .. 15;
      AWFOSR         at 0 range 16 .. 20;
      Reserved_21_21 at 0 range 21 .. 21;
      AWFORD         at 0 range 22 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype WDATR_CH_WDATA_Field is HAL.UInt16;

   --  channel watchdog filter data register
   type WDATR_CH_Register is record
      --  WDATA
      WDATA          : WDATR_CH_WDATA_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for WDATR_CH_Register use record
      WDATA          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  DATINR_CH_INDAT array element
   subtype DATINR_CH_INDAT_Element is HAL.UInt16;

   --  DATINR_CH_INDAT array
   type DATINR_CH_INDAT_Field_Array is array (0 .. 1)
     of DATINR_CH_INDAT_Element
     with Component_Size => 16, Size => 32;

   --  channel data input register
   type DATINR_CH_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  INDAT as a value
            Val : HAL.UInt32;
         when True =>
            --  INDAT as an array
            Arr : DATINR_CH_INDAT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DATINR_CH_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   subtype DLYR_CH_PLSSKP_Field is HAL.UInt6;

   --  DFSDM channel y delay register
   type DLYR_CH_Register is record
      --  Pulses to skip for input data skipping function
      PLSSKP        : DLYR_CH_PLSSKP_Field := 16#0#;
      --  unspecified
      Reserved_6_31 : HAL.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DLYR_CH_Register use record
      PLSSKP        at 0 range 0 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   type CH_Cluster is record
      --  channel configuration y register
      CFGR1  : aliased CFGR1_CH_Register;
      --  channel configuration y register
      CFGR2  : aliased CFGR2_CH_Register;
      --  analog watchdog and short-circuit detector register
      AWSCDR : aliased AWSCDR_CH_Register;
      --  channel watchdog filter data register
      WDATR  : aliased WDATR_CH_Register;
      --  channel data input register
      DATINR : aliased DATINR_CH_Register;
      --  DFSDM channel y delay register
      DLYR   : aliased DLYR_CH_Register;
   end record
     with Size => 256;

   for CH_Cluster use record
      CFGR1  at 16#0# range 0 .. 31;
      CFGR2  at 16#4# range 0 .. 31;
      AWSCDR at 16#8# range 0 .. 31;
      WDATR  at 16#C# range 0 .. 31;
      DATINR at 16#10# range 0 .. 31;
      DLYR   at 16#14# range 0 .. 31;
   end record;

   type CH_Clusters is array (0 .. 7) of CH_Cluster;

   -----------------------------
   -- FLT cluster's Registers --
   -----------------------------

   subtype CR1_FLT_JEXTSEL_Field is HAL.UInt3;
   subtype CR1_FLT_JEXTEN_Field is HAL.UInt2;
   subtype CR1_FLT_RCH_Field is HAL.UInt3;

   --  control register 1
   type CR1_FLT_Register is record
      --  DFSDM enable
      DFEN           : Boolean := False;
      --  Start a conversion of the injected group of channels
      JSWSTART       : Boolean := False;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  Launch an injected conversion synchronously with the DFSDM0 JSWSTART
      --  trigger
      JSYNC          : Boolean := False;
      --  Scanning conversion mode for injected conversions
      JSCAN          : Boolean := False;
      --  DMA channel enabled to read data for the injected channel group
      JDMAEN         : Boolean := False;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Trigger signal selection for launching injected conversions
      JEXTSEL        : CR1_FLT_JEXTSEL_Field := 16#0#;
      --  unspecified
      Reserved_11_12 : HAL.UInt2 := 16#0#;
      --  Trigger enable and trigger edge selection for injected conversions
      JEXTEN         : CR1_FLT_JEXTEN_Field := 16#0#;
      --  unspecified
      Reserved_15_16 : HAL.UInt2 := 16#0#;
      --  Software start of a conversion on the regular channel
      RSWSTART       : Boolean := False;
      --  Continuous mode selection for regular conversions
      RCONT          : Boolean := False;
      --  Launch regular conversion synchronously with DFSDM0
      RSYNC          : Boolean := False;
      --  unspecified
      Reserved_20_20 : HAL.Bit := 16#0#;
      --  DMA channel enabled to read data for the regular conversion
      RDMAEN         : Boolean := False;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  Regular channel selection
      RCH            : CR1_FLT_RCH_Field := 16#0#;
      --  unspecified
      Reserved_27_28 : HAL.UInt2 := 16#0#;
      --  Fast conversion mode selection for regular conversions
      FAST           : Boolean := False;
      --  Analog watchdog fast mode select
      AWFSEL         : Boolean := False;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR1_FLT_Register use record
      DFEN           at 0 range 0 .. 0;
      JSWSTART       at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      JSYNC          at 0 range 3 .. 3;
      JSCAN          at 0 range 4 .. 4;
      JDMAEN         at 0 range 5 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      JEXTSEL        at 0 range 8 .. 10;
      Reserved_11_12 at 0 range 11 .. 12;
      JEXTEN         at 0 range 13 .. 14;
      Reserved_15_16 at 0 range 15 .. 16;
      RSWSTART       at 0 range 17 .. 17;
      RCONT          at 0 range 18 .. 18;
      RSYNC          at 0 range 19 .. 19;
      Reserved_20_20 at 0 range 20 .. 20;
      RDMAEN         at 0 range 21 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      RCH            at 0 range 24 .. 26;
      Reserved_27_28 at 0 range 27 .. 28;
      FAST           at 0 range 29 .. 29;
      AWFSEL         at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype CR2_FLT_EXCH_Field is HAL.UInt8;
   subtype CR2_FLT_AWDCH_Field is HAL.UInt8;

   --  control register 2
   type CR2_FLT_Register is record
      --  Injected end of conversion interrupt enable
      JEOCIE         : Boolean := False;
      --  Regular end of conversion interrupt enable
      REOCIE         : Boolean := False;
      --  Injected data overrun interrupt enable
      JOVRIE         : Boolean := False;
      --  Regular data overrun interrupt enable
      ROVRIE         : Boolean := False;
      --  Analog watchdog interrupt enable
      AWDIE          : Boolean := False;
      --  Short-circuit detector interrupt enable
      SCDIE          : Boolean := False;
      --  Clock absence interrupt enable
      CKABIE         : Boolean := False;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Extremes detector channel selection
      EXCH           : CR2_FLT_EXCH_Field := 16#0#;
      --  Analog watchdog channel selection
      AWDCH          : CR2_FLT_AWDCH_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR2_FLT_Register use record
      JEOCIE         at 0 range 0 .. 0;
      REOCIE         at 0 range 1 .. 1;
      JOVRIE         at 0 range 2 .. 2;
      ROVRIE         at 0 range 3 .. 3;
      AWDIE          at 0 range 4 .. 4;
      SCDIE          at 0 range 5 .. 5;
      CKABIE         at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      EXCH           at 0 range 8 .. 15;
      AWDCH          at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype ISR_FLT_CKABF_Field is HAL.UInt8;
   subtype ISR_FLT_SCDF_Field is HAL.UInt8;

   --  interrupt and status register
   type ISR_FLT_Register is record
      --  End of injected conversion flag
      JEOCF          : Boolean := False;
      --  End of regular conversion flag
      REOCF          : Boolean := False;
      --  Injected conversion overrun flag
      JOVRF          : Boolean := False;
      --  Regular conversion overrun flag
      ROVRF          : Boolean := False;
      --  Analog watchdog
      AWDF           : Boolean := False;
      --  unspecified
      Reserved_5_12  : HAL.UInt8 := 16#0#;
      --  Injected conversion in progress status
      JCIP           : Boolean := False;
      --  Regular conversion in progress status
      RCIP           : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Clock absence flag
      CKABF          : ISR_FLT_CKABF_Field := 16#0#;
      --  short-circuit detector flag
      SCDF           : ISR_FLT_SCDF_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISR_FLT_Register use record
      JEOCF          at 0 range 0 .. 0;
      REOCF          at 0 range 1 .. 1;
      JOVRF          at 0 range 2 .. 2;
      ROVRF          at 0 range 3 .. 3;
      AWDF           at 0 range 4 .. 4;
      Reserved_5_12  at 0 range 5 .. 12;
      JCIP           at 0 range 13 .. 13;
      RCIP           at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      CKABF          at 0 range 16 .. 23;
      SCDF           at 0 range 24 .. 31;
   end record;

   subtype ICR_FLT_CLRCKABF_Field is HAL.UInt8;
   subtype ICR_FLT_CLRSCDF_Field is HAL.UInt8;

   --  interrupt flag clear register
   type ICR_FLT_Register is record
      --  unspecified
      Reserved_0_1  : HAL.UInt2 := 16#0#;
      --  Clear the injected conversion overrun flag
      CLRJOVRF      : Boolean := False;
      --  Clear the regular conversion overrun flag
      CLRROVRF      : Boolean := False;
      --  unspecified
      Reserved_4_15 : HAL.UInt12 := 16#0#;
      --  Clear the clock absence flag
      CLRCKABF      : ICR_FLT_CLRCKABF_Field := 16#0#;
      --  Clear the short-circuit detector flag
      CLRSCDF       : ICR_FLT_CLRSCDF_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ICR_FLT_Register use record
      Reserved_0_1  at 0 range 0 .. 1;
      CLRJOVRF      at 0 range 2 .. 2;
      CLRROVRF      at 0 range 3 .. 3;
      Reserved_4_15 at 0 range 4 .. 15;
      CLRCKABF      at 0 range 16 .. 23;
      CLRSCDF       at 0 range 24 .. 31;
   end record;

   subtype JCHGR_FLT_JCHG_Field is HAL.UInt8;

   --  injected channel group selection register
   type JCHGR_FLT_Register is record
      --  Injected channel group selection
      JCHG          : JCHGR_FLT_JCHG_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for JCHGR_FLT_Register use record
      JCHG          at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype FCR_FLT_IOSR_Field is HAL.UInt8;
   subtype FCR_FLT_FOSR_Field is HAL.UInt10;
   subtype FCR_FLT_FORD_Field is HAL.UInt3;

   --  filter control register
   type FCR_FLT_Register is record
      --  Integrator oversampling ratio (averaging length)
      IOSR           : FCR_FLT_IOSR_Field := 16#0#;
      --  unspecified
      Reserved_8_15  : HAL.UInt8 := 16#0#;
      --  Sinc filter oversampling ratio (decimation rate)
      FOSR           : FCR_FLT_FOSR_Field := 16#0#;
      --  unspecified
      Reserved_26_28 : HAL.UInt3 := 16#0#;
      --  Sinc filter order
      FORD           : FCR_FLT_FORD_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FCR_FLT_Register use record
      IOSR           at 0 range 0 .. 7;
      Reserved_8_15  at 0 range 8 .. 15;
      FOSR           at 0 range 16 .. 25;
      Reserved_26_28 at 0 range 26 .. 28;
      FORD           at 0 range 29 .. 31;
   end record;

   subtype JDATAR_FLT_JDATACH_Field is HAL.UInt3;
   subtype JDATAR_FLT_JDATA_Field is HAL.UInt24;

   --  data register for injected group
   type JDATAR_FLT_Register is record
      --  Injected channel most recently converted
      JDATACH      : JDATAR_FLT_JDATACH_Field := 16#0#;
      --  unspecified
      Reserved_3_7 : HAL.UInt5 := 16#0#;
      --  Injected group conversion data
      JDATA        : JDATAR_FLT_JDATA_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for JDATAR_FLT_Register use record
      JDATACH      at 0 range 0 .. 2;
      Reserved_3_7 at 0 range 3 .. 7;
      JDATA        at 0 range 8 .. 31;
   end record;

   subtype RDATAR_FLT_RDATACH_Field is HAL.UInt3;
   subtype RDATAR_FLT_RDATA_Field is HAL.UInt24;

   --  data register for the regular channel
   type RDATAR_FLT_Register is record
      --  Regular channel most recently converted
      RDATACH      : RDATAR_FLT_RDATACH_Field := 16#0#;
      --  unspecified
      Reserved_3_3 : HAL.Bit := 16#0#;
      --  Regular channel pending data
      RPEND        : Boolean := False;
      --  unspecified
      Reserved_5_7 : HAL.UInt3 := 16#0#;
      --  Regular channel conversion data
      RDATA        : RDATAR_FLT_RDATA_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RDATAR_FLT_Register use record
      RDATACH      at 0 range 0 .. 2;
      Reserved_3_3 at 0 range 3 .. 3;
      RPEND        at 0 range 4 .. 4;
      Reserved_5_7 at 0 range 5 .. 7;
      RDATA        at 0 range 8 .. 31;
   end record;

   subtype AWHTR_FLT_BKAWH_Field is HAL.UInt4;
   subtype AWHTR_FLT_AWHT_Field is HAL.UInt24;

   --  analog watchdog high threshold register
   type AWHTR_FLT_Register is record
      --  Break signal assignment to analog watchdog high threshold event
      BKAWH        : AWHTR_FLT_BKAWH_Field := 16#0#;
      --  unspecified
      Reserved_4_7 : HAL.UInt4 := 16#0#;
      --  Analog watchdog high threshold
      AWHT         : AWHTR_FLT_AWHT_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AWHTR_FLT_Register use record
      BKAWH        at 0 range 0 .. 3;
      Reserved_4_7 at 0 range 4 .. 7;
      AWHT         at 0 range 8 .. 31;
   end record;

   subtype AWLTR_FLT_BKAWL_Field is HAL.UInt4;
   subtype AWLTR_FLT_AWLT_Field is HAL.UInt24;

   --  analog watchdog low threshold register
   type AWLTR_FLT_Register is record
      --  Break signal assignment to analog watchdog low threshold event
      BKAWL        : AWLTR_FLT_BKAWL_Field := 16#0#;
      --  unspecified
      Reserved_4_7 : HAL.UInt4 := 16#0#;
      --  Analog watchdog low threshold
      AWLT         : AWLTR_FLT_AWLT_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AWLTR_FLT_Register use record
      BKAWL        at 0 range 0 .. 3;
      Reserved_4_7 at 0 range 4 .. 7;
      AWLT         at 0 range 8 .. 31;
   end record;

   subtype AWSR_FLT_AWLTF_Field is HAL.UInt8;
   subtype AWSR_FLT_AWHTF_Field is HAL.UInt8;

   --  analog watchdog status register
   type AWSR_FLT_Register is record
      --  Analog watchdog low threshold flag
      AWLTF          : AWSR_FLT_AWLTF_Field := 16#0#;
      --  Analog watchdog high threshold flag
      AWHTF          : AWSR_FLT_AWHTF_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AWSR_FLT_Register use record
      AWLTF          at 0 range 0 .. 7;
      AWHTF          at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype AWCFR_FLT_CLRAWLTF_Field is HAL.UInt8;
   subtype AWCFR_FLT_CLRAWHTF_Field is HAL.UInt8;

   --  analog watchdog clear flag register
   type AWCFR_FLT_Register is record
      --  Clear the analog watchdog low threshold flag
      CLRAWLTF       : AWCFR_FLT_CLRAWLTF_Field := 16#0#;
      --  Clear the analog watchdog high threshold flag
      CLRAWHTF       : AWCFR_FLT_CLRAWHTF_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AWCFR_FLT_Register use record
      CLRAWLTF       at 0 range 0 .. 7;
      CLRAWHTF       at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype EXMAX_FLT_EXMAXCH_Field is HAL.UInt3;
   subtype EXMAX_FLT_EXMAX_Field is HAL.UInt24;

   --  Extremes detector maximum register
   type EXMAX_FLT_Register is record
      --  Extremes detector maximum data channel
      EXMAXCH      : EXMAX_FLT_EXMAXCH_Field := 16#0#;
      --  unspecified
      Reserved_3_7 : HAL.UInt5 := 16#0#;
      --  Extremes detector maximum value
      EXMAX        : EXMAX_FLT_EXMAX_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EXMAX_FLT_Register use record
      EXMAXCH      at 0 range 0 .. 2;
      Reserved_3_7 at 0 range 3 .. 7;
      EXMAX        at 0 range 8 .. 31;
   end record;

   subtype EXMIN_FLT_EXMINCH_Field is HAL.UInt3;
   subtype EXMIN_FLT_EXMIN_Field is HAL.UInt24;

   --  Extremes detector minimum register
   type EXMIN_FLT_Register is record
      --  Extremes detector minimum data channel
      EXMINCH      : EXMIN_FLT_EXMINCH_Field := 16#0#;
      --  unspecified
      Reserved_3_7 : HAL.UInt5 := 16#0#;
      --  EXMIN
      EXMIN        : EXMIN_FLT_EXMIN_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EXMIN_FLT_Register use record
      EXMINCH      at 0 range 0 .. 2;
      Reserved_3_7 at 0 range 3 .. 7;
      EXMIN        at 0 range 8 .. 31;
   end record;

   subtype CNVTIMR_FLT_CNVCNT_Field is HAL.UInt28;

   --  conversion timer register
   type CNVTIMR_FLT_Register is record
      --  unspecified
      Reserved_0_3 : HAL.UInt4 := 16#0#;
      --  28-bit timer counting conversion time t = CNVCNT[27:0] / fDFSDM_CKIN
      CNVCNT       : CNVTIMR_FLT_CNVCNT_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CNVTIMR_FLT_Register use record
      Reserved_0_3 at 0 range 0 .. 3;
      CNVCNT       at 0 range 4 .. 31;
   end record;

   type FLT_Cluster is record
      --  control register 1
      CR1     : aliased CR1_FLT_Register;
      --  control register 2
      CR2     : aliased CR2_FLT_Register;
      --  interrupt and status register
      ISR     : aliased ISR_FLT_Register;
      --  interrupt flag clear register
      ICR     : aliased ICR_FLT_Register;
      --  injected channel group selection register
      JCHGR   : aliased JCHGR_FLT_Register;
      --  filter control register
      FCR     : aliased FCR_FLT_Register;
      --  data register for injected group
      JDATAR  : aliased JDATAR_FLT_Register;
      --  data register for the regular channel
      RDATAR  : aliased RDATAR_FLT_Register;
      --  analog watchdog high threshold register
      AWHTR   : aliased AWHTR_FLT_Register;
      --  analog watchdog low threshold register
      AWLTR   : aliased AWLTR_FLT_Register;
      --  analog watchdog status register
      AWSR    : aliased AWSR_FLT_Register;
      --  analog watchdog clear flag register
      AWCFR   : aliased AWCFR_FLT_Register;
      --  Extremes detector maximum register
      EXMAX   : aliased EXMAX_FLT_Register;
      --  Extremes detector minimum register
      EXMIN   : aliased EXMIN_FLT_Register;
      --  conversion timer register
      CNVTIMR : aliased CNVTIMR_FLT_Register;
   end record
     with Size => 1024;

   for FLT_Cluster use record
      CR1     at 16#0# range 0 .. 31;
      CR2     at 16#4# range 0 .. 31;
      ISR     at 16#8# range 0 .. 31;
      ICR     at 16#C# range 0 .. 31;
      JCHGR   at 16#10# range 0 .. 31;
      FCR     at 16#14# range 0 .. 31;
      JDATAR  at 16#18# range 0 .. 31;
      RDATAR  at 16#1C# range 0 .. 31;
      AWHTR   at 16#20# range 0 .. 31;
      AWLTR   at 16#24# range 0 .. 31;
      AWSR    at 16#28# range 0 .. 31;
      AWCFR   at 16#2C# range 0 .. 31;
      EXMAX   at 16#30# range 0 .. 31;
      EXMIN   at 16#34# range 0 .. 31;
      CNVTIMR at 16#38# range 0 .. 31;
   end record;

   type FLT_Clusters is array (0 .. 3) of FLT_Cluster;

   ----------------------------
   -- CH cluster's Registers --
   ----------------------------

   -----------------------------
   -- FLT cluster's Registers --
   -----------------------------

   -----------------
   -- Peripherals --
   -----------------

   --  Digital filter for sigma delta modulators
   type DFSDM_Peripheral is record
      CH  : aliased CH_Clusters;
      FLT : aliased FLT_Clusters;
   end record
     with Volatile;

   for DFSDM_Peripheral use record
      CH  at 16#0# range 0 .. 2047;
      FLT at 16#100# range 0 .. 4095;
   end record;

   --  Digital filter for sigma delta modulators
   DFSDM1_Periph : aliased DFSDM_Peripheral
     with Import, Address => DFSDM1_Base;

   --  Digital filter for sigma delta modulators
   SEC_DFSDM1_Periph : aliased DFSDM_Peripheral
     with Import, Address => SEC_DFSDM1_Base;

end STM32_SVD.DFSDM;
