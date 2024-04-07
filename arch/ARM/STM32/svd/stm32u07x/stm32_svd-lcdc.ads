--  This spec has been automatically generated from STM32U073.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.LCDC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype LCD_CR_DUTY_Field is HAL.UInt3;
   subtype LCD_CR_BIAS_Field is HAL.UInt2;

   --  LCD control register
   type LCD_CR_Register is record
      --  LCD controller enable This bit is set by software to enable the LCD
      --  controller/driver. It is cleared by software to turn off the LCD at
      --  the beginning of the next frame. When the LCD is disabled, all COM
      --  and SEG pins are driven to V<sub>SS</sub>.
      LCDEN         : Boolean := False;
      --  Voltage source selection This bit determines the voltage source for
      --  the LCD.
      VSEL          : Boolean := False;
      --  Duty selection These bits determine the duty cycle. Values 101, 110
      --  and 111 are forbidden. Others: Reserved
      DUTY          : LCD_CR_DUTY_Field := 16#0#;
      --  Bias selector These bits determine the bias used. Value 11 is
      --  forbidden.
      BIAS          : LCD_CR_BIAS_Field := 16#0#;
      --  Mux segment enable This bit is used to enable SEG pin remapping. Four
      --  SEG pins can be multiplexed with1SEG[31:28] or SEG[15:12]. See
      --  Section118.3.7.
      MUX_SEG       : Boolean := False;
      --  Voltage output buffer enable This bit is used to enable/disable the
      --  voltage output buffer for higher driving capability.
      BUFEN         : Boolean := False;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LCD_CR_Register use record
      LCDEN         at 0 range 0 .. 0;
      VSEL          at 0 range 1 .. 1;
      DUTY          at 0 range 2 .. 4;
      BIAS          at 0 range 5 .. 6;
      MUX_SEG       at 0 range 7 .. 7;
      BUFEN         at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   subtype LCD_FCR_PON_Field is HAL.UInt3;
   subtype LCD_FCR_DEAD_Field is HAL.UInt3;
   subtype LCD_FCR_CC_Field is HAL.UInt3;
   subtype LCD_FCR_BLINKF_Field is HAL.UInt3;
   subtype LCD_FCR_BLINK_Field is HAL.UInt2;
   subtype LCD_FCR_DIV_Field is HAL.UInt4;
   subtype LCD_FCR_PS_Field is HAL.UInt4;

   --  LCD frame control register
   type LCD_FCR_Register is record
      --  High drive enable This bit is written by software to enable a low
      --  resistance divider. Displays with high internal resistance may need a
      --  longer drive time to achieve satisfactory contrast. This bit is
      --  useful in this case if some additional power consumption can be
      --  tolerated.
      HD             : Boolean := False;
      --  Start of frame interrupt enable This bit is set and cleared by
      --  software.
      SOFIE          : Boolean := False;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  Update display done interrupt enable This bit is set and cleared by
      --  software.
      UDDIE          : Boolean := False;
      --  Pulse ON duration These bits are written by software to define the
      --  pulse duration in terms of ck_ps pulses. A1short pulse leads to lower
      --  power consumption, but displays with high internal resistance may
      --  need a longer pulse to achieve satisfactory contrast. Note that the
      --  pulse is never longer than one half prescaled LCD clock period. PON
      --  duration example with LCDCLK = 32.7681kHz and PS=0x03:
      PON            : LCD_FCR_PON_Field := 16#0#;
      --  Dead time duration These bits are written by software to configure
      --  the length of the dead time between frames. During the dead time the
      --  COM and SEG voltage levels are held at 0 V to reduce the contrast
      --  without modifying the frame rate. ......
      DEAD           : LCD_FCR_DEAD_Field := 16#0#;
      --  Contrast control These bits specify one of the V<sub>LCD
      --  </sub>maximum voltages (independent of V<sub>DD</sub>). It ranges
      --  from12.60 V to 3.51V. Note: Refer to the datasheet for the
      --  V<sub>LCDx</sub> values.
      CC             : LCD_FCR_CC_Field := 16#0#;
      --  Blink frequency selection
      BLINKF         : LCD_FCR_BLINKF_Field := 16#0#;
      --  Blink mode selection
      BLINK          : LCD_FCR_BLINK_Field := 16#0#;
      --  DIV clock divider These bits are written by software to define the
      --  division factor of the DIV divider (see1Section118.3.2.) ...
      DIV            : LCD_FCR_DIV_Field := 16#0#;
      --  PS 16-bit prescaler These bits are written by software to define the
      --  division factor of the PS 16-bit prescaler. ck_ps =
      --  LCDCLK/(2<sup>PS[3:0]</sup>). See<sub>
      PS             : LCD_FCR_PS_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LCD_FCR_Register use record
      HD             at 0 range 0 .. 0;
      SOFIE          at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      UDDIE          at 0 range 3 .. 3;
      PON            at 0 range 4 .. 6;
      DEAD           at 0 range 7 .. 9;
      CC             at 0 range 10 .. 12;
      BLINKF         at 0 range 13 .. 15;
      BLINK          at 0 range 16 .. 17;
      DIV            at 0 range 18 .. 21;
      PS             at 0 range 22 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   --  LCD status register
   type LCD_SR_Register is record
      --  Read-only. LCD enabled status This bit is set and cleared by
      --  hardware. It indicates the LCD controller status. Note: This bit is
      --  set immediately when LCDEN in LCD_CR goes from 0 to 1. On
      --  deactivation, it reflects the real LCD status. It becomes 0 at the
      --  end of the last displayed frame.
      ENS           : Boolean := False;
      --  Read-only. Start-of-frame flag This bit is set by hardware at the
      --  beginning of a new frame, at the same time as the display data is
      --  updated. It is cleared by writing a 1 to SOFC in LCD_CLR. The bit
      --  clear has priority over the set.
      SOF           : Boolean := False;
      --  Update display request Each time software modifies the LCD_RAM, it
      --  must set this bit to transfer the updated data to the second level
      --  buffer. This bit stays set until the end of the update. During this
      --  time, the LCD_RAM is write protected. When the display is disabled,
      --  the update is performed for all LCD_DISPLAY locations. When the
      --  display is enabled, the update is performed only for locations for
      --  which commons are active (depending on DUTY). For example if DUTY =
      --  1/2, Note: only the LCD_DISPLAY of COM0 and COM1 are updated. Note:
      --  Writing 0 on this bit or writing 1 when it is already 1 has no
      --  effect. This bit can be cleared by hardware only. It can be cleared
      --  only when LCDEN = 1
      UDR           : Boolean := False;
      --  Read-only. Update display done This bit is set by hardware. It is
      --  cleared by writing 1 to UDDC in LCD_CLR. The bit set has priority
      --  over the clear. Note: If the device is in Stop mode (PCLK not
      --  provided), UDD does not generate an interrupt even if UDDIE = 1. If
      --  the display is not enabled, the UDD interrupt never occurs.
      UDD           : Boolean := False;
      --  Read-only. Ready flag This bit is set and cleared by hardware. It
      --  indicates the status of the stepup converter.
      RDY           : Boolean := False;
      --  Read-only. LCD frame control register synchronization flag This bit
      --  is set by hardware each time the LCD_FCR register is updated in the
      --  LCDCLK domain. It is cleared by hardware when writing to the LCD_FCR
      --  register.
      FCRSF         : Boolean := True;
      --  unspecified
      Reserved_6_31 : HAL.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LCD_SR_Register use record
      ENS           at 0 range 0 .. 0;
      SOF           at 0 range 1 .. 1;
      UDR           at 0 range 2 .. 2;
      UDD           at 0 range 3 .. 3;
      RDY           at 0 range 4 .. 4;
      FCRSF         at 0 range 5 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   --  LCD clear register
   type LCD_CLR_Register is record
      --  unspecified
      Reserved_0_0  : HAL.Bit := 16#0#;
      --  Write-only. Start-of-frame flag clear This bit is written by software
      --  to clear SOF in LCD_SR.
      SOFC          : Boolean := False;
      --  unspecified
      Reserved_2_2  : HAL.Bit := 16#0#;
      --  Write-only. Update display done clear This bit is written by software
      --  to clear UDD in LCD_SR.
      UDDC          : Boolean := False;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LCD_CLR_Register use record
      Reserved_0_0  at 0 range 0 .. 0;
      SOFC          at 0 range 1 .. 1;
      Reserved_2_2  at 0 range 2 .. 2;
      UDDC          at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   subtype LCD_RAM_SEGMENT_DATA_Field is HAL.UInt20;

   --  LCD display memory
   type LCD_RAM_Register is record
      --  Each bit corresponds to one pixel of the LCD display.
      SEGMENT_DATA   : LCD_RAM_SEGMENT_DATA_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LCD_RAM_Register use record
      SEGMENT_DATA   at 0 range 0 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   subtype LCD_RAM_SEGMENT_DATA_Field_1 is HAL.UInt16;

   --  LCD display memory
   type LCD_RAM_Register_1 is record
      --  Each bit corresponds to one pixel of the LCD display.
      SEGMENT_DATA   : LCD_RAM_SEGMENT_DATA_Field_1 := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LCD_RAM_Register_1 use record
      SEGMENT_DATA   at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  LCD address block description
   type LCD_Peripheral is record
      --  LCD control register
      LCD_CR    : aliased LCD_CR_Register;
      --  LCD frame control register
      LCD_FCR   : aliased LCD_FCR_Register;
      --  LCD status register
      LCD_SR    : aliased LCD_SR_Register;
      --  LCD clear register
      LCD_CLR   : aliased LCD_CLR_Register;
      --  LCD display memory
      LCD_RAM0  : aliased HAL.UInt32;
      --  LCD display memory
      LCD_RAM1  : aliased LCD_RAM_Register;
      --  LCD display memory
      LCD_RAM2  : aliased HAL.UInt32;
      --  LCD display memory
      LCD_RAM3  : aliased LCD_RAM_Register;
      --  LCD display memory
      LCD_RAM4  : aliased HAL.UInt32;
      --  LCD display memory
      LCD_RAM5  : aliased LCD_RAM_Register;
      --  LCD display memory
      LCD_RAM6  : aliased HAL.UInt32;
      --  LCD display memory
      LCD_RAM7  : aliased LCD_RAM_Register;
      --  LCD display memory
      LCD_RAM8  : aliased HAL.UInt32;
      --  LCD display memory
      LCD_RAM9  : aliased LCD_RAM_Register_1;
      --  LCD display memory
      LCD_RAM10 : aliased HAL.UInt32;
      --  LCD display memory
      LCD_RAM11 : aliased LCD_RAM_Register_1;
      --  LCD display memory
      LCD_RAM12 : aliased HAL.UInt32;
      --  LCD display memory
      LCD_RAM13 : aliased LCD_RAM_Register_1;
      --  LCD display memory
      LCD_RAM14 : aliased HAL.UInt32;
      --  LCD display memory
      LCD_RAM15 : aliased LCD_RAM_Register_1;
   end record
     with Volatile;

   for LCD_Peripheral use record
      LCD_CR    at 16#0# range 0 .. 31;
      LCD_FCR   at 16#4# range 0 .. 31;
      LCD_SR    at 16#8# range 0 .. 31;
      LCD_CLR   at 16#C# range 0 .. 31;
      LCD_RAM0  at 16#14# range 0 .. 31;
      LCD_RAM1  at 16#18# range 0 .. 31;
      LCD_RAM2  at 16#1C# range 0 .. 31;
      LCD_RAM3  at 16#20# range 0 .. 31;
      LCD_RAM4  at 16#24# range 0 .. 31;
      LCD_RAM5  at 16#28# range 0 .. 31;
      LCD_RAM6  at 16#2C# range 0 .. 31;
      LCD_RAM7  at 16#30# range 0 .. 31;
      LCD_RAM8  at 16#34# range 0 .. 31;
      LCD_RAM9  at 16#38# range 0 .. 31;
      LCD_RAM10 at 16#3C# range 0 .. 31;
      LCD_RAM11 at 16#40# range 0 .. 31;
      LCD_RAM12 at 16#44# range 0 .. 31;
      LCD_RAM13 at 16#48# range 0 .. 31;
      LCD_RAM14 at 16#4C# range 0 .. 31;
      LCD_RAM15 at 16#50# range 0 .. 31;
   end record;

   --  LCD address block description
   LCD_Periph : aliased LCD_Peripheral
     with Import, Address => LCD_Base;

end STM32_SVD.LCDC;
