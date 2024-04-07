--  This spec has been automatically generated from STM32U073.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.SYSCFG is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype SYSCFG_CFGR1_MEM_MODE_Field is HAL.UInt2;
   subtype SYSCFG_CFGR1_IR_MOD_Field is HAL.UInt2;

   --  SYSCFG configuration register 1
   type SYSCFG_CFGR1_Register is record
      --  Memory mapping selection bits These bits are set and cleared by
      --  software. They control the memory internal mapping at address
      --  0x000010000. After reset these bits take on the value selected by the
      --  actual boot mode configuration. Refer to Section12.5: Boot
      --  configuration for more details. X0: Main flash memory mapped at
      --  0x000010000
      MEM_MODE       : SYSCFG_CFGR1_MEM_MODE_Field := 16#0#;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  PA11 pin remapping This bit is set and cleared by software. When set,
      --  it remaps the PA11 pin to operate as PA9 GPIO port, instead as PA11
      --  GPIO port.
      PA11_RMP       : Boolean := False;
      --  PA12 pin remapping This bit is set and cleared by software. When set,
      --  it remaps the PA12 pin to operate as PA10 GPIO port, instead as PA12
      --  GPIO port.
      PA12_RMP       : Boolean := False;
      --  IR output polarity selection
      IR_POL         : Boolean := False;
      --  IR Modulation Envelope signal selection This bitfield selects the
      --  signal for IR modulation envelope:
      IR_MOD         : SYSCFG_CFGR1_IR_MOD_Field := 16#0#;
      --  I/O analog switch voltage booster enable This bit selects the way of
      --  supplying I/O analog switches: When using the analog inputs , setting
      --  to 0 is recommended for high V<sub>DD</sub>, setting to 1 for low
      --  V<sub>DD</sub> (less than 2.4 V).
      BOOSTEN        : Boolean := False;
      --  unspecified
      Reserved_9_15  : HAL.UInt7 := 16#0#;
      --  Fast Mode Plus (FM+) enable for PB6 This bit is set and cleared by
      --  software. It enables I<sup>2</sup>C FM+ driving capability on PB6 I/O
      --  port. With this bit in disable state, the I<sup>2</sup>C FM+ driving
      --  capability on this I/O port can be enabled through one of I2Cx_FMP
      --  bits. When I<sup>2</sup>C FM+ is enabled, the speed control is
      --  ignored. Note: This control bit is kept for legacy reasons. It is
      --  recommended to use the FMP bit of the I2Cx_CR1 register instead.
      I2C_PB6_FMP    : Boolean := False;
      --  Fast Mode Plus (FM+) enable for PB7 This bit is set and cleared by
      --  software. It enables I<sup>2</sup>C FM+ driving capability on PB7 I/O
      --  port. With this bit in disable state, the I<sup>2</sup>C FM+ driving
      --  capability on this I/O port can be enabled through one of I2Cx_FMP
      --  bits. When I<sup>2</sup>C FM+ is enabled, the speed control is
      --  ignored. Note: This control bit is kept for legacy reasons. It is
      --  recommended to use the FMP bit of the I2Cx_CR1 register instead.
      I2C_PB7_FMP    : Boolean := False;
      --  Fast Mode Plus (FM+) enable for PB8 This bit is set and cleared by
      --  software. It enables I<sup>2</sup>C FM+ driving capability on PB8 I/O
      --  port. With this bit in disable state, the I<sup>2</sup>C FM+ driving
      --  capability on this I/O port can be enabled through one of I2Cx_FMP
      --  bits. When I<sup>2</sup>C FM+ is enabled, the speed control is
      --  ignored. Note: This control bit is kept for legacy reasons. It is
      --  recommended to use the FMP bit of the I2Cx_CR1 register instead.
      I2C_PB8_FMP    : Boolean := False;
      --  Fast Mode Plus (FM+) enable for PB9 This bit is set and cleared by
      --  software. It enables I<sup>2</sup>C FM+ driving capability on PB9 I/O
      --  port. With this bit in disable state, the I<sup>2</sup>C FM+ driving
      --  capability on this I/O port can be enabled through one of I2Cx_FMP
      --  bits. When I<sup>2</sup>C FM+ is enabled, the speed control is
      --  ignored. Note: This control bit is kept for legacy reasons. It is
      --  recommended to use the FMP bit of the I2Cx_CR1 register instead.
      I2C_PB9_FMP    : Boolean := False;
      --  unspecified
      Reserved_20_21 : HAL.UInt2 := 16#0#;
      --  Fast Mode Plus (FM+) enable for PA9 This bit is set and cleared by
      --  software. It enables I<sup>2</sup>C FM+ driving capability on PA9 I/O
      --  port. With this bit in disable state, the I<sup>2</sup>C FM+ driving
      --  capability on this I/O port can be enabled through one of I2Cx_FMP
      --  bits. When I<sup>2</sup>C FM+ is enabled, the speed control is
      --  ignored. Note: This control bit is kept for legacy reasons. It is
      --  recommended to use the FMP bit of the I2Cx_CR1 register instead.
      I2C_PA9_FMP    : Boolean := False;
      --  Fast Mode Plus (FM+) enable for PA10 This bit is set and cleared by
      --  software. It enables I<sup>2</sup>C FM+ driving capability on PA10
      --  I/O port. With this bit in disable state, the I<sup>2</sup>C FM+
      --  driving capability on this I/O port can be enabled through one of
      --  I2Cx_FMP bits. When I<sup>2</sup>C FM+ is enabled, the speed control
      --  is ignored. Note: This control bit is kept for legacy reasons. It is
      --  recommended to use the FMP bit of the I2Cx_CR1 register instead.
      I2C_PA10_FMP   : Boolean := False;
      --  Fast Mode Plus (FM+) enable for I2C3 This bit is set and cleared by
      --  software. It enables I<sup>2</sup>C FM+ driving capability on I/O
      --  ports configured as I2C3 through GPIOx_AFR registers. With this bit
      --  in disable state, the I<sup>2</sup>C FM+ driving capability on I/O
      --  ports configured as I2C3 can be enabled through their corresponding
      --  I2Cx_FMP bit. When I<sup>2</sup>C FM+ is enabled, the speed control
      --  is ignored. Note: This control bit is kept for legacy reasons. It is
      --  recommended to use the FMP bit of the I2Cx_CR1 register instead.
      I2C3_FMP       : Boolean := False;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYSCFG_CFGR1_Register use record
      MEM_MODE       at 0 range 0 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      PA11_RMP       at 0 range 3 .. 3;
      PA12_RMP       at 0 range 4 .. 4;
      IR_POL         at 0 range 5 .. 5;
      IR_MOD         at 0 range 6 .. 7;
      BOOSTEN        at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      I2C_PB6_FMP    at 0 range 16 .. 16;
      I2C_PB7_FMP    at 0 range 17 .. 17;
      I2C_PB8_FMP    at 0 range 18 .. 18;
      I2C_PB9_FMP    at 0 range 19 .. 19;
      Reserved_20_21 at 0 range 20 .. 21;
      I2C_PA9_FMP    at 0 range 22 .. 22;
      I2C_PA10_FMP   at 0 range 23 .. 23;
      I2C3_FMP       at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  SYSCFG configuration register 2
   type SYSCFG_CFGR2_Register is record
      --  Cortex<Superscript>1<Default 1 Font>-M0+ LOCKUP bit enable bit This
      --  bit is set by software and cleared by a system reset. It can be use
      --  to enable and lock the connection of Cortex<Superscript>1<Default 1
      --  Font>-M0+ LOCKUP (Hardfault) output to TIM1/15/16 Break input.
      CCL           : Boolean := False;
      --  SRAM1 parity lock bit This bit is set by software and cleared by a
      --  system reset. It can be used to enable and lock the SRAM1 parity
      --  error signal connection to TIM1/15/16 Break input.
      SPL           : Boolean := False;
      --  PVD lock enable bit This bit is set by software and cleared by a
      --  system reset. It can be used to enable and lock the PVD connection to
      --  TIM1/15/16 Break input, as well as the PVDE and PLS[2:0] in the
      --  PWR_CR register.
      PVDL          : Boolean := False;
      --  ECC error lock bit This bit is set by software and cleared by a
      --  system reset. It can be used to enable and lock the flash ECC 2-bit
      --  error detection signal connection to TIM1/15/16 Break input.
      ECCL          : Boolean := False;
      --  Backup SRAM2 parity lock This bit is set by software and cleared by a
      --  system reset. It can be used to enable and lock the SRAM2 parity
      --  error signal connection to TIM1/15/16 Break input.
      BKPL          : Boolean := False;
      --  unspecified
      Reserved_5_6  : HAL.UInt2 := 16#0#;
      --  Backup SRAM2 parity error flag This bit is set by hardware when an
      --  SRAM2 parity error is detected. It is cleared by software by writing
      --  1.
      BKPF          : Boolean := False;
      --  SRAM1 parity error flag This bit is set by hardware when an SRAM1
      --  parity error is detected. It is cleared by software by writing 1.
      SPF           : Boolean := False;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYSCFG_CFGR2_Register use record
      CCL           at 0 range 0 .. 0;
      SPL           at 0 range 1 .. 1;
      PVDL          at 0 range 2 .. 2;
      ECCL          at 0 range 3 .. 3;
      BKPL          at 0 range 4 .. 4;
      Reserved_5_6  at 0 range 5 .. 6;
      BKPF          at 0 range 7 .. 7;
      SPF           at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   --  SYSCFG SRAM2 control and status register
   type SYSCFG_SCSR_Register is record
      --  SRAM2 erase Setting this bit starts a hardware SRAM2 erase operation.
      --  This bit is automatically cleared at the end of the SRAM2 erase
      --  operation. Note: This bit is write-protected: setting this bit is
      --  possible only after the correct key sequence is written in the
      --  SYSCFG_SKR register.
      SRAM2ER       : Boolean := False;
      --  Read-only. SRAM2 busy by erase operation
      SRAM2BSY      : Boolean := False;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYSCFG_SCSR_Register use record
      SRAM2ER       at 0 range 0 .. 0;
      SRAM2BSY      at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   subtype SYSCFG_SKR_KEY_Field is HAL.UInt8;

   --  SYSCFG SRAM2 key register
   type SYSCFG_SKR_Register is record
      --  Write-only. SRAM2 write protection key for software erase The
      --  following steps are required to unlock the write protection of the
      --  SRAM2ER bit in the SYSCFG_CFGR2 register: Write 0xCA into KEY[7:0]
      --  Write 0x53 into KEY[7:0] Writing a wrong key reactivates the write
      --  protection.
      KEY           : SYSCFG_SKR_KEY_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYSCFG_SKR_Register use record
      KEY           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  SYSCFG_TSCCR_G2_IO array
   type SYSCFG_TSCCR_G2_IO_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for SYSCFG_TSCCR_G2_IO
   type SYSCFG_TSCCR_G2_IO_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  G2_IO as a value
            Val : HAL.UInt2;
         when True =>
            --  G2_IO as an array
            Arr : SYSCFG_TSCCR_G2_IO_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for SYSCFG_TSCCR_G2_IO_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  SYSCFG TSC comparator register
   type SYSCFG_TSCCR_Register is record
      --  Comparator mode for group 2 on I/O 1
      G2_IO         : SYSCFG_TSCCR_G2_IO_Field :=
                       (As_Array => False, Val => 16#0#);
      --  Comparator mode for group 4 on I/O 3
      G4_IO3        : Boolean := False;
      --  Comparator mode for group 6 on I/O 1
      G6_IO1        : Boolean := False;
      --  Comparator mode for group 7 on I/O 1
      G7_IO1        : Boolean := False;
      --  I/O control in comparator mode The I/O control in comparator mode can
      --  be overwritten by hardware.
      TSC_IOCTRL    : Boolean := False;
      --  unspecified
      Reserved_6_31 : HAL.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYSCFG_TSCCR_Register use record
      G2_IO         at 0 range 0 .. 1;
      G4_IO3        at 0 range 2 .. 2;
      G6_IO1        at 0 range 3 .. 3;
      G7_IO1        at 0 range 4 .. 4;
      TSC_IOCTRL    at 0 range 5 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   --  SYSCFG interrupt line 0 status register
   type SYSCFG_ITLINE0_Register is record
      --  Read-only. Window watchdog interrupt pending flag
      WWDG          : Boolean;
      --  unspecified
      Reserved_1_31 : HAL.UInt31;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYSCFG_ITLINE0_Register use record
      WWDG          at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  SYSCFG_ITLINE1_PVMOUT array
   type SYSCFG_ITLINE1_PVMOUT_Field_Array is array (1 .. 3) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for SYSCFG_ITLINE1_PVMOUT
   type SYSCFG_ITLINE1_PVMOUT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PVMOUT as a value
            Val : HAL.UInt3;
         when True =>
            --  PVMOUT as an array
            Arr : SYSCFG_ITLINE1_PVMOUT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for SYSCFG_ITLINE1_PVMOUT_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  SYSCFG interrupt line 1 status register
   type SYSCFG_ITLINE1_Register is record
      --  Read-only. PVD supply monitoring interrupt request pending (EXTI line
      --  16).
      PVDOUT        : Boolean;
      --  Read-only. V<sub>DDUSB</sub> supply monitoring interrupt request
      --  pending (EXTI line 19)
      PVMOUT        : SYSCFG_ITLINE1_PVMOUT_Field;
      --  unspecified
      Reserved_4_31 : HAL.UInt28;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYSCFG_ITLINE1_Register use record
      PVDOUT        at 0 range 0 .. 0;
      PVMOUT        at 0 range 1 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  SYSCFG interrupt line 2 status register
   type SYSCFG_ITLINE2_Register is record
      --  Read-only. Tamper interrupt request pending (EXTI line 21)
      TAMP          : Boolean;
      --  Read-only. RTC interrupt request pending (EXTI line 19)
      RTC           : Boolean;
      --  unspecified
      Reserved_2_31 : HAL.UInt30;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYSCFG_ITLINE2_Register use record
      TAMP          at 0 range 0 .. 0;
      RTC           at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  SYSCFG interrupt line 3 status register
   type SYSCFG_ITLINE3_Register is record
      --  Read-only. Flash interface interrupt request pending
      FLASH_ITF     : Boolean;
      --  Read-only. Flash interface ECC interrupt request pending
      FLASH_ECC     : Boolean;
      --  unspecified
      Reserved_2_31 : HAL.UInt30;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYSCFG_ITLINE3_Register use record
      FLASH_ITF     at 0 range 0 .. 0;
      FLASH_ECC     at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  SYSCFG interrupt line 4 status register
   type SYSCFG_ITLINE4_Register is record
      --  Read-only. Reset and clock control interrupt request pending
      RCC           : Boolean;
      --  Read-only. CRS interrupt request pending
      CRS           : Boolean;
      --  unspecified
      Reserved_2_31 : HAL.UInt30;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYSCFG_ITLINE4_Register use record
      RCC           at 0 range 0 .. 0;
      CRS           at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  SYSCFG_ITLINE5_EXTI array
   type SYSCFG_ITLINE5_EXTI_Field_Array is array (0 .. 1) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for SYSCFG_ITLINE5_EXTI
   type SYSCFG_ITLINE5_EXTI_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTI as a value
            Val : HAL.UInt2;
         when True =>
            --  EXTI as an array
            Arr : SYSCFG_ITLINE5_EXTI_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for SYSCFG_ITLINE5_EXTI_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  SYSCFG interrupt line 5 status register
   type SYSCFG_ITLINE5_Register is record
      --  Read-only. EXTI line 0 interrupt request pending
      EXTI          : SYSCFG_ITLINE5_EXTI_Field;
      --  unspecified
      Reserved_2_31 : HAL.UInt30;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYSCFG_ITLINE5_Register use record
      EXTI          at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  SYSCFG_ITLINE6_EXTI array
   type SYSCFG_ITLINE6_EXTI_Field_Array is array (2 .. 3) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for SYSCFG_ITLINE6_EXTI
   type SYSCFG_ITLINE6_EXTI_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTI as a value
            Val : HAL.UInt2;
         when True =>
            --  EXTI as an array
            Arr : SYSCFG_ITLINE6_EXTI_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for SYSCFG_ITLINE6_EXTI_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  SYSCFG interrupt line 6 status register
   type SYSCFG_ITLINE6_Register is record
      --  Read-only. EXTI line 2 interrupt request pending
      EXTI          : SYSCFG_ITLINE6_EXTI_Field;
      --  unspecified
      Reserved_2_31 : HAL.UInt30;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYSCFG_ITLINE6_Register use record
      EXTI          at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  SYSCFG_ITLINE7_EXTI array
   type SYSCFG_ITLINE7_EXTI_Field_Array is array (4 .. 15) of Boolean
     with Component_Size => 1, Size => 12;

   --  Type definition for SYSCFG_ITLINE7_EXTI
   type SYSCFG_ITLINE7_EXTI_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTI as a value
            Val : HAL.UInt12;
         when True =>
            --  EXTI as an array
            Arr : SYSCFG_ITLINE7_EXTI_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 12;

   for SYSCFG_ITLINE7_EXTI_Field use record
      Val at 0 range 0 .. 11;
      Arr at 0 range 0 .. 11;
   end record;

   --  SYSCFG interrupt line 7 status register
   type SYSCFG_ITLINE7_Register is record
      --  Read-only. EXTI line 4 interrupt request pending
      EXTI           : SYSCFG_ITLINE7_EXTI_Field;
      --  unspecified
      Reserved_12_31 : HAL.UInt20;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYSCFG_ITLINE7_Register use record
      EXTI           at 0 range 0 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   --  SYSCFG interrupt line 8 status register
   type SYSCFG_ITLINE8_Register is record
      --  Read-only. USB interrupt request pending
      USB           : Boolean;
      --  unspecified
      Reserved_1_31 : HAL.UInt31;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYSCFG_ITLINE8_Register use record
      USB           at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  SYSCFG interrupt line 9 status register
   type SYSCFG_ITLINE9_Register is record
      --  Read-only. DMA1 channel 1 interrupt request pending
      DMA1_CH1      : Boolean;
      --  unspecified
      Reserved_1_31 : HAL.UInt31;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYSCFG_ITLINE9_Register use record
      DMA1_CH1      at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  SYSCFG_ITLINE10_DMA1_CH array
   type SYSCFG_ITLINE10_DMA1_CH_Field_Array is array (2 .. 3) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for SYSCFG_ITLINE10_DMA1_CH
   type SYSCFG_ITLINE10_DMA1_CH_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DMA1_CH as a value
            Val : HAL.UInt2;
         when True =>
            --  DMA1_CH as an array
            Arr : SYSCFG_ITLINE10_DMA1_CH_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for SYSCFG_ITLINE10_DMA1_CH_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  SYSCFG interrupt line 10 status register
   type SYSCFG_ITLINE10_Register is record
      --  Read-only. DMA1 channel 2 interrupt request pending
      DMA1_CH       : SYSCFG_ITLINE10_DMA1_CH_Field;
      --  unspecified
      Reserved_2_31 : HAL.UInt30;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYSCFG_ITLINE10_Register use record
      DMA1_CH       at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  SYSCFG_ITLINE11_DMA1_CH array
   type SYSCFG_ITLINE11_DMA1_CH_Field_Array is array (4 .. 7) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for SYSCFG_ITLINE11_DMA1_CH
   type SYSCFG_ITLINE11_DMA1_CH_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DMA1_CH as a value
            Val : HAL.UInt4;
         when True =>
            --  DMA1_CH as an array
            Arr : SYSCFG_ITLINE11_DMA1_CH_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for SYSCFG_ITLINE11_DMA1_CH_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  SYSCFG_ITLINE11_DMA2_CH array
   type SYSCFG_ITLINE11_DMA2_CH_Field_Array is array (1 .. 5) of Boolean
     with Component_Size => 1, Size => 5;

   --  Type definition for SYSCFG_ITLINE11_DMA2_CH
   type SYSCFG_ITLINE11_DMA2_CH_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DMA2_CH as a value
            Val : HAL.UInt5;
         when True =>
            --  DMA2_CH as an array
            Arr : SYSCFG_ITLINE11_DMA2_CH_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 5;

   for SYSCFG_ITLINE11_DMA2_CH_Field use record
      Val at 0 range 0 .. 4;
      Arr at 0 range 0 .. 4;
   end record;

   --  SYSCFG interrupt line 11 status register
   type SYSCFG_ITLINE11_Register is record
      --  Read-only. DMAMUX interrupt request pending
      DMAMUX         : Boolean;
      --  Read-only. DMA1 channel 4 interrupt request pending
      DMA1_CH        : SYSCFG_ITLINE11_DMA1_CH_Field;
      --  Read-only. DMA2 channel 1 interrupt request pending
      DMA2_CH        : SYSCFG_ITLINE11_DMA2_CH_Field;
      --  unspecified
      Reserved_10_31 : HAL.UInt22;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYSCFG_ITLINE11_Register use record
      DMAMUX         at 0 range 0 .. 0;
      DMA1_CH        at 0 range 1 .. 4;
      DMA2_CH        at 0 range 5 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   --  SYSCFG_ITLINE12_COMP array
   type SYSCFG_ITLINE12_COMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for SYSCFG_ITLINE12_COMP
   type SYSCFG_ITLINE12_COMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  COMP as a value
            Val : HAL.UInt2;
         when True =>
            --  COMP as an array
            Arr : SYSCFG_ITLINE12_COMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for SYSCFG_ITLINE12_COMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  SYSCFG interrupt line 12 status register
   type SYSCFG_ITLINE12_Register is record
      --  Read-only. ADC interrupt request pending
      ADC           : Boolean;
      --  Read-only. Comparator 1 interrupt request pending (EXTI line 17)
      COMP          : SYSCFG_ITLINE12_COMP_Field;
      --  unspecified
      Reserved_3_31 : HAL.UInt29;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYSCFG_ITLINE12_Register use record
      ADC           at 0 range 0 .. 0;
      COMP          at 0 range 1 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  SYSCFG interrupt line 13 status register
   type SYSCFG_ITLINE13_Register is record
      --  Read-only. Timer 1 commutation interrupt request pending
      TIM1_CCU      : Boolean;
      --  Read-only. Timer 1 trigger interrupt request pending
      TIM1_TRG      : Boolean;
      --  Read-only. Timer 1 update interrupt request pending
      TIM1_UPD      : Boolean;
      --  Read-only. Timer 1 break interrupt request pending
      TIM1_BRK      : Boolean;
      --  unspecified
      Reserved_4_31 : HAL.UInt28;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYSCFG_ITLINE13_Register use record
      TIM1_CCU      at 0 range 0 .. 0;
      TIM1_TRG      at 0 range 1 .. 1;
      TIM1_UPD      at 0 range 2 .. 2;
      TIM1_BRK      at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  SYSCFG_ITLINE14_TIM1_CC array
   type SYSCFG_ITLINE14_TIM1_CC_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for SYSCFG_ITLINE14_TIM1_CC
   type SYSCFG_ITLINE14_TIM1_CC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIM1_CC as a value
            Val : HAL.UInt4;
         when True =>
            --  TIM1_CC as an array
            Arr : SYSCFG_ITLINE14_TIM1_CC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for SYSCFG_ITLINE14_TIM1_CC_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  SYSCFG interrupt line 14 status register
   type SYSCFG_ITLINE14_Register is record
      --  Read-only. Timer 1 capture compare 1 interrupt request pending
      TIM1_CC       : SYSCFG_ITLINE14_TIM1_CC_Field;
      --  unspecified
      Reserved_4_31 : HAL.UInt28;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYSCFG_ITLINE14_Register use record
      TIM1_CC       at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  SYSCFG interrupt line 15 status register
   type SYSCFG_ITLINE15_Register is record
      --  Read-only. Timer 2 interrupt request pending
      TIM2          : Boolean;
      --  unspecified
      Reserved_1_31 : HAL.UInt31;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYSCFG_ITLINE15_Register use record
      TIM2          at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  SYSCFG interrupt line 16 status register
   type SYSCFG_ITLINE16_Register is record
      --  Read-only. Timer 3 interrupt request pending
      TIM3          : Boolean;
      --  unspecified
      Reserved_1_31 : HAL.UInt31;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYSCFG_ITLINE16_Register use record
      TIM3          at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  SYSCFG interrupt line 17 status register
   type SYSCFG_ITLINE17_Register is record
      --  Read-only. Timer 6 interrupt request pending
      TIM6          : Boolean;
      --  Read-only. DAC underrun interrupt request pending
      DAC           : Boolean;
      --  Read-only. Low-power timer 1 interrupt request pending (EXTI line 29)
      LPTIM1        : Boolean;
      --  unspecified
      Reserved_3_31 : HAL.UInt29;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYSCFG_ITLINE17_Register use record
      TIM6          at 0 range 0 .. 0;
      DAC           at 0 range 1 .. 1;
      LPTIM1        at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  SYSCFG interrupt line 18 status register
   type SYSCFG_ITLINE18_Register is record
      --  Read-only. Timer 7 interrupt request pending
      TIM7          : Boolean;
      --  Read-only. Low-power timer 2 interrupt request pending (EXTI line 30)
      LPTIM2        : Boolean;
      --  unspecified
      Reserved_2_31 : HAL.UInt30;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYSCFG_ITLINE18_Register use record
      TIM7          at 0 range 0 .. 0;
      LPTIM2        at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  SYSCFG interrupt line 19 status register
   type SYSCFG_ITLINE19_Register is record
      --  Read-only. Timer 15 interrupt request pending
      TIM15         : Boolean;
      --  Read-only. Low-power timer 3 interrupt request pending
      LPTIM3        : Boolean;
      --  unspecified
      Reserved_2_31 : HAL.UInt30;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYSCFG_ITLINE19_Register use record
      TIM15         at 0 range 0 .. 0;
      LPTIM3        at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  SYSCFG interrupt line 20 status register
   type SYSCFG_ITLINE20_Register is record
      --  Read-only. Timer 16 interrupt request pending
      TIM16         : Boolean;
      --  unspecified
      Reserved_1_31 : HAL.UInt31;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYSCFG_ITLINE20_Register use record
      TIM16         at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  SYSCFG interrupt line 21 status register
   type SYSCFG_ITLINE21_Register is record
      --  Read-only. TSC max count error interrupt request pending
      TSC_MCE       : Boolean;
      --  Read-only. TSC end of acquisition interrupt request pending
      TSC_EOA       : Boolean;
      --  unspecified
      Reserved_2_31 : HAL.UInt30;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYSCFG_ITLINE21_Register use record
      TSC_MCE       at 0 range 0 .. 0;
      TSC_EOA       at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  SYSCFG interrupt line 22 status register
   type SYSCFG_ITLINE22_Register is record
      --  Read-only. LCD interrupt request pending
      LCD           : Boolean;
      --  unspecified
      Reserved_1_31 : HAL.UInt31;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYSCFG_ITLINE22_Register use record
      LCD           at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  SYSCFG interrupt line 23 status register
   type SYSCFG_ITLINE23_Register is record
      --  Read-only. I2C1 interrupt request pending (EXTI line 33)
      I2C1          : Boolean;
      --  unspecified
      Reserved_1_31 : HAL.UInt31;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYSCFG_ITLINE23_Register use record
      I2C1          at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  SYSCFG_ITLINE24_I2C array
   type SYSCFG_ITLINE24_I2C_Field_Array is array (2 .. 4) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for SYSCFG_ITLINE24_I2C
   type SYSCFG_ITLINE24_I2C_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  I2C as a value
            Val : HAL.UInt3;
         when True =>
            --  I2C as an array
            Arr : SYSCFG_ITLINE24_I2C_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for SYSCFG_ITLINE24_I2C_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  SYSCFG interrupt line 24 status register
   type SYSCFG_ITLINE24_Register is record
      --  Read-only. I2C2 interrupt request pending
      I2C           : SYSCFG_ITLINE24_I2C_Field;
      --  unspecified
      Reserved_3_31 : HAL.UInt29;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYSCFG_ITLINE24_Register use record
      I2C           at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  SYSCFG interrupt line 25 status register
   type SYSCFG_ITLINE25_Register is record
      --  Read-only. SPI1 interrupt request pending
      SPI1          : Boolean;
      --  unspecified
      Reserved_1_31 : HAL.UInt31;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYSCFG_ITLINE25_Register use record
      SPI1          at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  SYSCFG_ITLINE26_SPI array
   type SYSCFG_ITLINE26_SPI_Field_Array is array (2 .. 3) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for SYSCFG_ITLINE26_SPI
   type SYSCFG_ITLINE26_SPI_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SPI as a value
            Val : HAL.UInt2;
         when True =>
            --  SPI as an array
            Arr : SYSCFG_ITLINE26_SPI_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for SYSCFG_ITLINE26_SPI_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  SYSCFG interrupt line 26 status register
   type SYSCFG_ITLINE26_Register is record
      --  Read-only. SPI2 interrupt request pending
      SPI           : SYSCFG_ITLINE26_SPI_Field;
      --  unspecified
      Reserved_2_31 : HAL.UInt30;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYSCFG_ITLINE26_Register use record
      SPI           at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  SYSCFG interrupt line 27 status register
   type SYSCFG_ITLINE27_Register is record
      --  Read-only. USART1 interrupt request pending, combined with EXTI line
      --  25
      USART1        : Boolean;
      --  unspecified
      Reserved_1_31 : HAL.UInt31;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYSCFG_ITLINE27_Register use record
      USART1        at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  SYSCFG interrupt line 28 status register
   type SYSCFG_ITLINE28_Register is record
      --  Read-only. USART2 interrupt request pending (EXTI line 35)
      USART2        : Boolean;
      --  Read-only. LPUART2 interrupt request pending (EXTI line 31)
      LPUART2       : Boolean;
      --  unspecified
      Reserved_2_31 : HAL.UInt30;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYSCFG_ITLINE28_Register use record
      USART2        at 0 range 0 .. 0;
      LPUART2       at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  SYSCFG interrupt line 29 status register
   type SYSCFG_ITLINE29_Register is record
      --  Read-only. USART3 interrupt request pending
      USART3        : Boolean;
      --  Read-only. LPUART1 interrupt request pending (EXTI line 30)
      LPUART1       : Boolean;
      --  unspecified
      Reserved_2_31 : HAL.UInt30;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYSCFG_ITLINE29_Register use record
      USART3        at 0 range 0 .. 0;
      LPUART1       at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  SYSCFG interrupt line 30 status register
   type SYSCFG_ITLINE30_Register is record
      --  Read-only. USART4 interrupt request pending
      USART4        : Boolean;
      --  Read-only. LPUART3 interrupt request pending (EXTI line 32)
      LPUART3       : Boolean;
      --  unspecified
      Reserved_2_31 : HAL.UInt30;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYSCFG_ITLINE30_Register use record
      USART4        at 0 range 0 .. 0;
      LPUART3       at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  SYSCFG interrupt line 31 status register
   type SYSCFG_ITLINE31_Register is record
      --  Read-only. RNG interrupt request pending
      RNG           : Boolean;
      --  unspecified
      Reserved_1_31 : HAL.UInt31;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYSCFG_ITLINE31_Register use record
      RNG           at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  SYSCFG register block
   type SYSCFG_Peripheral is record
      --  SYSCFG configuration register 1
      SYSCFG_CFGR1    : aliased SYSCFG_CFGR1_Register;
      --  SYSCFG configuration register 2
      SYSCFG_CFGR2    : aliased SYSCFG_CFGR2_Register;
      --  SYSCFG SRAM2 control and status register
      SYSCFG_SCSR     : aliased SYSCFG_SCSR_Register;
      --  SYSCFG SRAM2 key register
      SYSCFG_SKR      : aliased SYSCFG_SKR_Register;
      --  SYSCFG TSC comparator register
      SYSCFG_TSCCR    : aliased SYSCFG_TSCCR_Register;
      --  SYSCFG interrupt line 0 status register
      SYSCFG_ITLINE0  : aliased SYSCFG_ITLINE0_Register;
      --  SYSCFG interrupt line 1 status register
      SYSCFG_ITLINE1  : aliased SYSCFG_ITLINE1_Register;
      --  SYSCFG interrupt line 2 status register
      SYSCFG_ITLINE2  : aliased SYSCFG_ITLINE2_Register;
      --  SYSCFG interrupt line 3 status register
      SYSCFG_ITLINE3  : aliased SYSCFG_ITLINE3_Register;
      --  SYSCFG interrupt line 4 status register
      SYSCFG_ITLINE4  : aliased SYSCFG_ITLINE4_Register;
      --  SYSCFG interrupt line 5 status register
      SYSCFG_ITLINE5  : aliased SYSCFG_ITLINE5_Register;
      --  SYSCFG interrupt line 6 status register
      SYSCFG_ITLINE6  : aliased SYSCFG_ITLINE6_Register;
      --  SYSCFG interrupt line 7 status register
      SYSCFG_ITLINE7  : aliased SYSCFG_ITLINE7_Register;
      --  SYSCFG interrupt line 8 status register
      SYSCFG_ITLINE8  : aliased SYSCFG_ITLINE8_Register;
      --  SYSCFG interrupt line 9 status register
      SYSCFG_ITLINE9  : aliased SYSCFG_ITLINE9_Register;
      --  SYSCFG interrupt line 10 status register
      SYSCFG_ITLINE10 : aliased SYSCFG_ITLINE10_Register;
      --  SYSCFG interrupt line 11 status register
      SYSCFG_ITLINE11 : aliased SYSCFG_ITLINE11_Register;
      --  SYSCFG interrupt line 12 status register
      SYSCFG_ITLINE12 : aliased SYSCFG_ITLINE12_Register;
      --  SYSCFG interrupt line 13 status register
      SYSCFG_ITLINE13 : aliased SYSCFG_ITLINE13_Register;
      --  SYSCFG interrupt line 14 status register
      SYSCFG_ITLINE14 : aliased SYSCFG_ITLINE14_Register;
      --  SYSCFG interrupt line 15 status register
      SYSCFG_ITLINE15 : aliased SYSCFG_ITLINE15_Register;
      --  SYSCFG interrupt line 16 status register
      SYSCFG_ITLINE16 : aliased SYSCFG_ITLINE16_Register;
      --  SYSCFG interrupt line 17 status register
      SYSCFG_ITLINE17 : aliased SYSCFG_ITLINE17_Register;
      --  SYSCFG interrupt line 18 status register
      SYSCFG_ITLINE18 : aliased SYSCFG_ITLINE18_Register;
      --  SYSCFG interrupt line 19 status register
      SYSCFG_ITLINE19 : aliased SYSCFG_ITLINE19_Register;
      --  SYSCFG interrupt line 20 status register
      SYSCFG_ITLINE20 : aliased SYSCFG_ITLINE20_Register;
      --  SYSCFG interrupt line 21 status register
      SYSCFG_ITLINE21 : aliased SYSCFG_ITLINE21_Register;
      --  SYSCFG interrupt line 22 status register
      SYSCFG_ITLINE22 : aliased SYSCFG_ITLINE22_Register;
      --  SYSCFG interrupt line 23 status register
      SYSCFG_ITLINE23 : aliased SYSCFG_ITLINE23_Register;
      --  SYSCFG interrupt line 24 status register
      SYSCFG_ITLINE24 : aliased SYSCFG_ITLINE24_Register;
      --  SYSCFG interrupt line 25 status register
      SYSCFG_ITLINE25 : aliased SYSCFG_ITLINE25_Register;
      --  SYSCFG interrupt line 26 status register
      SYSCFG_ITLINE26 : aliased SYSCFG_ITLINE26_Register;
      --  SYSCFG interrupt line 27 status register
      SYSCFG_ITLINE27 : aliased SYSCFG_ITLINE27_Register;
      --  SYSCFG interrupt line 28 status register
      SYSCFG_ITLINE28 : aliased SYSCFG_ITLINE28_Register;
      --  SYSCFG interrupt line 29 status register
      SYSCFG_ITLINE29 : aliased SYSCFG_ITLINE29_Register;
      --  SYSCFG interrupt line 30 status register
      SYSCFG_ITLINE30 : aliased SYSCFG_ITLINE30_Register;
      --  SYSCFG interrupt line 31 status register
      SYSCFG_ITLINE31 : aliased SYSCFG_ITLINE31_Register;
   end record
     with Volatile;

   for SYSCFG_Peripheral use record
      SYSCFG_CFGR1    at 16#0# range 0 .. 31;
      SYSCFG_CFGR2    at 16#18# range 0 .. 31;
      SYSCFG_SCSR     at 16#1C# range 0 .. 31;
      SYSCFG_SKR      at 16#20# range 0 .. 31;
      SYSCFG_TSCCR    at 16#24# range 0 .. 31;
      SYSCFG_ITLINE0  at 16#80# range 0 .. 31;
      SYSCFG_ITLINE1  at 16#84# range 0 .. 31;
      SYSCFG_ITLINE2  at 16#88# range 0 .. 31;
      SYSCFG_ITLINE3  at 16#8C# range 0 .. 31;
      SYSCFG_ITLINE4  at 16#90# range 0 .. 31;
      SYSCFG_ITLINE5  at 16#94# range 0 .. 31;
      SYSCFG_ITLINE6  at 16#98# range 0 .. 31;
      SYSCFG_ITLINE7  at 16#9C# range 0 .. 31;
      SYSCFG_ITLINE8  at 16#A0# range 0 .. 31;
      SYSCFG_ITLINE9  at 16#A4# range 0 .. 31;
      SYSCFG_ITLINE10 at 16#A8# range 0 .. 31;
      SYSCFG_ITLINE11 at 16#AC# range 0 .. 31;
      SYSCFG_ITLINE12 at 16#B0# range 0 .. 31;
      SYSCFG_ITLINE13 at 16#B4# range 0 .. 31;
      SYSCFG_ITLINE14 at 16#B8# range 0 .. 31;
      SYSCFG_ITLINE15 at 16#BC# range 0 .. 31;
      SYSCFG_ITLINE16 at 16#C0# range 0 .. 31;
      SYSCFG_ITLINE17 at 16#C4# range 0 .. 31;
      SYSCFG_ITLINE18 at 16#C8# range 0 .. 31;
      SYSCFG_ITLINE19 at 16#CC# range 0 .. 31;
      SYSCFG_ITLINE20 at 16#D0# range 0 .. 31;
      SYSCFG_ITLINE21 at 16#D4# range 0 .. 31;
      SYSCFG_ITLINE22 at 16#D8# range 0 .. 31;
      SYSCFG_ITLINE23 at 16#DC# range 0 .. 31;
      SYSCFG_ITLINE24 at 16#E0# range 0 .. 31;
      SYSCFG_ITLINE25 at 16#E4# range 0 .. 31;
      SYSCFG_ITLINE26 at 16#E8# range 0 .. 31;
      SYSCFG_ITLINE27 at 16#EC# range 0 .. 31;
      SYSCFG_ITLINE28 at 16#F0# range 0 .. 31;
      SYSCFG_ITLINE29 at 16#F4# range 0 .. 31;
      SYSCFG_ITLINE30 at 16#F8# range 0 .. 31;
      SYSCFG_ITLINE31 at 16#FC# range 0 .. 31;
   end record;

   --  SYSCFG register block
   SYSCFG_Periph : aliased SYSCFG_Peripheral
     with Import, Address => SYSCFG_Base;

end STM32_SVD.SYSCFG;
