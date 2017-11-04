--  This spec has been automatically generated from STM32F103.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.AFIO is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype EVCR_PIN_Field is HAL.UInt4;
   subtype EVCR_PORT_Field is HAL.UInt3;

   --  Event Control Register (AFIO_EVCR)
   type EVCR_Register is record
      --  Pin selection
      PIN           : EVCR_PIN_Field := 16#0#;
      --  Port selection
      PORT          : EVCR_PORT_Field := 16#0#;
      --  Event Output Enable
      EVOE          : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EVCR_Register use record
      PIN           at 0 range 0 .. 3;
      PORT          at 0 range 4 .. 6;
      EVOE          at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype MAPR_USART3_REMAP_Field is HAL.UInt2;
   subtype MAPR_TIM1_REMAP_Field is HAL.UInt2;
   subtype MAPR_TIM2_REMAP_Field is HAL.UInt2;
   subtype MAPR_TIM3_REMAP_Field is HAL.UInt2;
   subtype MAPR_CAN_REMAP_Field is HAL.UInt2;
   subtype MAPR_SWJ_CFG_Field is HAL.UInt3;

   --  AF remap and debug I/O configuration register (AFIO_MAPR)
   type MAPR_Register is record
      --  SPI1 remapping
      SPI1_REMAP         : Boolean := False;
      --  I2C1 remapping
      I2C1_REMAP         : Boolean := False;
      --  USART1 remapping
      USART1_REMAP       : Boolean := False;
      --  USART2 remapping
      USART2_REMAP       : Boolean := False;
      --  USART3 remapping
      USART3_REMAP       : MAPR_USART3_REMAP_Field := 16#0#;
      --  TIM1 remapping
      TIM1_REMAP         : MAPR_TIM1_REMAP_Field := 16#0#;
      --  TIM2 remapping
      TIM2_REMAP         : MAPR_TIM2_REMAP_Field := 16#0#;
      --  TIM3 remapping
      TIM3_REMAP         : MAPR_TIM3_REMAP_Field := 16#0#;
      --  TIM4 remapping
      TIM4_REMAP         : Boolean := False;
      --  CAN1 remapping
      CAN_REMAP          : MAPR_CAN_REMAP_Field := 16#0#;
      --  Port D0/Port D1 mapping on OSCIN/OSCOUT
      PD01_REMAP         : Boolean := False;
      --  Set and cleared by software
      TIM5CH4_IREMAP     : Boolean := False;
      --  ADC 1 External trigger injected conversion remapping
      ADC1_ETRGINJ_REMAP : Boolean := False;
      --  ADC 1 external trigger regular conversion remapping
      ADC1_ETRGREG_REMAP : Boolean := False;
      --  ADC 2 external trigger injected conversion remapping
      ADC2_ETRGINJ_REMAP : Boolean := False;
      --  ADC 2 external trigger regular conversion remapping
      ADC2_ETRGREG_REMAP : Boolean := False;
      --  unspecified
      Reserved_21_23     : HAL.UInt3 := 16#0#;
      --  Write-only. Serial wire JTAG configuration
      SWJ_CFG            : MAPR_SWJ_CFG_Field := 16#0#;
      --  unspecified
      Reserved_27_31     : HAL.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MAPR_Register use record
      SPI1_REMAP         at 0 range 0 .. 0;
      I2C1_REMAP         at 0 range 1 .. 1;
      USART1_REMAP       at 0 range 2 .. 2;
      USART2_REMAP       at 0 range 3 .. 3;
      USART3_REMAP       at 0 range 4 .. 5;
      TIM1_REMAP         at 0 range 6 .. 7;
      TIM2_REMAP         at 0 range 8 .. 9;
      TIM3_REMAP         at 0 range 10 .. 11;
      TIM4_REMAP         at 0 range 12 .. 12;
      CAN_REMAP          at 0 range 13 .. 14;
      PD01_REMAP         at 0 range 15 .. 15;
      TIM5CH4_IREMAP     at 0 range 16 .. 16;
      ADC1_ETRGINJ_REMAP at 0 range 17 .. 17;
      ADC1_ETRGREG_REMAP at 0 range 18 .. 18;
      ADC2_ETRGINJ_REMAP at 0 range 19 .. 19;
      ADC2_ETRGREG_REMAP at 0 range 20 .. 20;
      Reserved_21_23     at 0 range 21 .. 23;
      SWJ_CFG            at 0 range 24 .. 26;
      Reserved_27_31     at 0 range 27 .. 31;
   end record;

   --  EXTICR1_EXTI array element
   subtype EXTICR1_EXTI_Element is HAL.UInt4;

   --  EXTICR1_EXTI array
   type EXTICR1_EXTI_Field_Array is array (0 .. 3) of EXTICR1_EXTI_Element
     with Component_Size => 4, Size => 16;

   --  Type definition for EXTICR1_EXTI
   type EXTICR1_EXTI_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTI as a value
            Val : HAL.UInt16;
         when True =>
            --  EXTI as an array
            Arr : EXTICR1_EXTI_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for EXTICR1_EXTI_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  External interrupt configuration register 1 (AFIO_EXTICR1)
   type EXTICR1_Register is record
      --  EXTI0 configuration
      EXTI           : EXTICR1_EXTI_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EXTICR1_Register use record
      EXTI           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  EXTICR2_EXTI array element
   subtype EXTICR2_EXTI_Element is HAL.UInt4;

   --  EXTICR2_EXTI array
   type EXTICR2_EXTI_Field_Array is array (4 .. 7) of EXTICR2_EXTI_Element
     with Component_Size => 4, Size => 16;

   --  Type definition for EXTICR2_EXTI
   type EXTICR2_EXTI_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTI as a value
            Val : HAL.UInt16;
         when True =>
            --  EXTI as an array
            Arr : EXTICR2_EXTI_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for EXTICR2_EXTI_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  External interrupt configuration register 2 (AFIO_EXTICR2)
   type EXTICR2_Register is record
      --  EXTI4 configuration
      EXTI           : EXTICR2_EXTI_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EXTICR2_Register use record
      EXTI           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  EXTICR3_EXTI array element
   subtype EXTICR3_EXTI_Element is HAL.UInt4;

   --  EXTICR3_EXTI array
   type EXTICR3_EXTI_Field_Array is array (8 .. 11) of EXTICR3_EXTI_Element
     with Component_Size => 4, Size => 16;

   --  Type definition for EXTICR3_EXTI
   type EXTICR3_EXTI_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTI as a value
            Val : HAL.UInt16;
         when True =>
            --  EXTI as an array
            Arr : EXTICR3_EXTI_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for EXTICR3_EXTI_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  External interrupt configuration register 3 (AFIO_EXTICR3)
   type EXTICR3_Register is record
      --  EXTI8 configuration
      EXTI           : EXTICR3_EXTI_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EXTICR3_Register use record
      EXTI           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  EXTICR4_EXTI array element
   subtype EXTICR4_EXTI_Element is HAL.UInt4;

   --  EXTICR4_EXTI array
   type EXTICR4_EXTI_Field_Array is array (12 .. 15) of EXTICR4_EXTI_Element
     with Component_Size => 4, Size => 16;

   --  Type definition for EXTICR4_EXTI
   type EXTICR4_EXTI_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTI as a value
            Val : HAL.UInt16;
         when True =>
            --  EXTI as an array
            Arr : EXTICR4_EXTI_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for EXTICR4_EXTI_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  External interrupt configuration register 4 (AFIO_EXTICR4)
   type EXTICR4_Register is record
      --  EXTI12 configuration
      EXTI           : EXTICR4_EXTI_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EXTICR4_Register use record
      EXTI           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  AF remap and debug I/O configuration register
   type MAPR2_Register is record
      --  unspecified
      Reserved_0_4   : HAL.UInt5 := 16#0#;
      --  TIM9 remapping
      TIM9_REMAP     : Boolean := False;
      --  TIM10 remapping
      TIM10_REMAP    : Boolean := False;
      --  TIM11 remapping
      TIM11_REMAP    : Boolean := False;
      --  TIM13 remapping
      TIM13_REMAP    : Boolean := False;
      --  TIM14 remapping
      TIM14_REMAP    : Boolean := False;
      --  NADV connect/disconnect
      FSMC_NADV      : Boolean := False;
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MAPR2_Register use record
      Reserved_0_4   at 0 range 0 .. 4;
      TIM9_REMAP     at 0 range 5 .. 5;
      TIM10_REMAP    at 0 range 6 .. 6;
      TIM11_REMAP    at 0 range 7 .. 7;
      TIM13_REMAP    at 0 range 8 .. 8;
      TIM14_REMAP    at 0 range 9 .. 9;
      FSMC_NADV      at 0 range 10 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Alternate function I/O
   type AFIO_Peripheral is record
      --  Event Control Register (AFIO_EVCR)
      EVCR    : aliased EVCR_Register;
      --  AF remap and debug I/O configuration register (AFIO_MAPR)
      MAPR    : aliased MAPR_Register;
      --  External interrupt configuration register 1 (AFIO_EXTICR1)
      EXTICR1 : aliased EXTICR1_Register;
      --  External interrupt configuration register 2 (AFIO_EXTICR2)
      EXTICR2 : aliased EXTICR2_Register;
      --  External interrupt configuration register 3 (AFIO_EXTICR3)
      EXTICR3 : aliased EXTICR3_Register;
      --  External interrupt configuration register 4 (AFIO_EXTICR4)
      EXTICR4 : aliased EXTICR4_Register;
      --  AF remap and debug I/O configuration register
      MAPR2   : aliased MAPR2_Register;
   end record
     with Volatile;

   for AFIO_Peripheral use record
      EVCR    at 16#0# range 0 .. 31;
      MAPR    at 16#4# range 0 .. 31;
      EXTICR1 at 16#8# range 0 .. 31;
      EXTICR2 at 16#C# range 0 .. 31;
      EXTICR3 at 16#10# range 0 .. 31;
      EXTICR4 at 16#14# range 0 .. 31;
      MAPR2   at 16#1C# range 0 .. 31;
   end record;

   --  Alternate function I/O
   AFIO_Periph : aliased AFIO_Peripheral
     with Import, Address => System'To_Address (16#40010000#);

end STM32_SVD.AFIO;
