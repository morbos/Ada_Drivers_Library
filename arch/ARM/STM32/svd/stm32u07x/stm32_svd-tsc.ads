--  This spec has been automatically generated from STM32U073.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.TSC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype TSC_CR_MCV_Field is HAL.UInt3;
   subtype TSC_CR_PGPSC_Field is HAL.UInt3;
   subtype TSC_CR_SSD_Field is HAL.UInt7;
   subtype TSC_CR_CTPL_Field is HAL.UInt4;
   subtype TSC_CR_CTPH_Field is HAL.UInt4;

   --  TSC control register
   type TSC_CR_Register is record
      --  Touch sensing controller enable This bit is set and cleared by
      --  software to enable/disable the touch sensing controller. Note: When
      --  the touch sensing controller is disabled, TSC registers settings have
      --  no effect.
      TSCE          : Boolean := False;
      --  Start a new acquisition This bit is set by software to start a new
      --  acquisition. It is cleared by hardware as soon as the acquisition is
      --  complete or by software to cancel the ongoing acquisition.
      START         : Boolean := False;
      --  Acquisition mode This bit is set and cleared by software to select
      --  the acquisition mode. Note: This bit must not be modified when an
      --  acquisition is ongoing.
      AM            : Boolean := False;
      --  Synchronization pin polarity This bit is set and cleared by software
      --  to select the polarity of the synchronization input pin.
      SYNCPOL       : Boolean := False;
      --  I/O Default mode This bit is set and cleared by software. It defines
      --  the configuration of all the TSC I/Os when there is no ongoing
      --  acquisition. When there is an ongoing acquisition, it defines the
      --  configuration of all unused I/Os (not defined as sampling capacitor
      --  I/O or as channel I/O). Note: This bit must not be modified when an
      --  acquisition is ongoing.
      IODEF         : Boolean := False;
      --  Max count value These bits are set and cleared by software. They
      --  define the maximum number of charge transfer pulses that can be
      --  generated before a max count error is generated. Note: These bits
      --  must not be modified when an acquisition is ongoing.
      MCV           : TSC_CR_MCV_Field := 16#0#;
      --  unspecified
      Reserved_8_11 : HAL.UInt4 := 16#0#;
      --  Pulse generator prescaler These bits are set and cleared by
      --  software.They select the AHB clock divider used to generate the pulse
      --  generator clock (PGCLK). Note: These bits must not be modified when
      --  an acquisition is ongoing. Note: Some configurations are forbidden.
      --  Refer to the Section119.4.4: Charge transfer acquisition sequence for
      --  details.
      PGPSC         : TSC_CR_PGPSC_Field := 16#0#;
      --  Spread spectrum prescaler This bit is set and cleared by software. It
      --  selects the AHB clock divider used to generate the spread spectrum
      --  clock (SSCLK). Note: This bit must not be modified when an
      --  acquisition is ongoing.
      SSPSC         : Boolean := False;
      --  Spread spectrum enable This bit is set and cleared by software to
      --  enable/disable the spread spectrum feature. Note: This bit must not
      --  be modified when an acquisition is ongoing.
      SSE           : Boolean := False;
      --  Spread spectrum deviation These bits are set and cleared by software.
      --  They define the spread spectrum deviation which consists in adding a
      --  variable number of periods of the SSCLK clock to the charge transfer
      --  pulse high state. ... Note: These bits must not be modified when an
      --  acquisition is ongoing.
      SSD           : TSC_CR_SSD_Field := 16#0#;
      --  Charge transfer pulse low These bits are set and cleared by software.
      --  They define the duration of the low state of the charge transfer
      --  pulse (transfer of charge from C<sub>X</sub> to C<sub>S</sub>). ...
      --  Note: These bits must not be modified when an acquisition is ongoing.
      --  Note: Some configurations are forbidden. Refer to the Section119.4.4:
      --  Charge transfer acquisition sequence for details.
      CTPL          : TSC_CR_CTPL_Field := 16#0#;
      --  Charge transfer pulse high These bits are set and cleared by
      --  software. They define the duration of the high state of the charge
      --  transfer pulse (charge of C<sub>X</sub>). ... Note: These bits must
      --  not be modified when an acquisition is ongoing.
      CTPH          : TSC_CR_CTPH_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TSC_CR_Register use record
      TSCE          at 0 range 0 .. 0;
      START         at 0 range 1 .. 1;
      AM            at 0 range 2 .. 2;
      SYNCPOL       at 0 range 3 .. 3;
      IODEF         at 0 range 4 .. 4;
      MCV           at 0 range 5 .. 7;
      Reserved_8_11 at 0 range 8 .. 11;
      PGPSC         at 0 range 12 .. 14;
      SSPSC         at 0 range 15 .. 15;
      SSE           at 0 range 16 .. 16;
      SSD           at 0 range 17 .. 23;
      CTPL          at 0 range 24 .. 27;
      CTPH          at 0 range 28 .. 31;
   end record;

   --  TSC interrupt enable register
   type TSC_IER_Register is record
      --  End of acquisition interrupt enable This bit is set and cleared by
      --  software to enable/disable the end of acquisition interrupt.
      EOAIE         : Boolean := False;
      --  Max count error interrupt enable This bit is set and cleared by
      --  software to enable/disable the max count error interrupt.
      MCEIE         : Boolean := False;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TSC_IER_Register use record
      EOAIE         at 0 range 0 .. 0;
      MCEIE         at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  TSC interrupt clear register
   type TSC_ICR_Register is record
      --  End of acquisition interrupt clear This bit is set by software to
      --  clear the end of acquisition flag and it is cleared by hardware when
      --  the flag is reset. Writing a 0 has no effect.
      EOAIC         : Boolean := False;
      --  Max count error interrupt clear This bit is set by software to clear
      --  the max count error flag and it is cleared by hardware when the flag
      --  is reset. Writing a 0 has no effect.
      MCEIC         : Boolean := False;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TSC_ICR_Register use record
      EOAIC         at 0 range 0 .. 0;
      MCEIC         at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  TSC interrupt status register
   type TSC_ISR_Register is record
      --  Read-only. End of acquisition flag This bit is set by hardware when
      --  the acquisition of all enabled group is complete (all GxS bits of all
      --  enabled analog I/O groups are set or when a max count error is
      --  detected). It is cleared by software writing 1 to the bit EOAIC of
      --  the TSC_ICR register.
      EOAF          : Boolean;
      --  Read-only. Max count error flag This bit is set by hardware as soon
      --  as an analog I/O group counter reaches the max count value specified.
      --  It is cleared by software writing 1 to the bit MCEIC of the TSC_ICR
      --  register.
      MCEF          : Boolean;
      --  unspecified
      Reserved_2_31 : HAL.UInt30;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TSC_ISR_Register use record
      EOAF          at 0 range 0 .. 0;
      MCEF          at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  TSC_IOHCR_G1_IO array
   type TSC_IOHCR_G1_IO_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for TSC_IOHCR_G1_IO
   type TSC_IOHCR_G1_IO_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  G1_IO as a value
            Val : HAL.UInt4;
         when True =>
            --  G1_IO as an array
            Arr : TSC_IOHCR_G1_IO_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for TSC_IOHCR_G1_IO_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  TSC_IOHCR_G2_IO array
   type TSC_IOHCR_G2_IO_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for TSC_IOHCR_G2_IO
   type TSC_IOHCR_G2_IO_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  G2_IO as a value
            Val : HAL.UInt4;
         when True =>
            --  G2_IO as an array
            Arr : TSC_IOHCR_G2_IO_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for TSC_IOHCR_G2_IO_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  TSC_IOHCR_G3_IO array
   type TSC_IOHCR_G3_IO_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for TSC_IOHCR_G3_IO
   type TSC_IOHCR_G3_IO_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  G3_IO as a value
            Val : HAL.UInt4;
         when True =>
            --  G3_IO as an array
            Arr : TSC_IOHCR_G3_IO_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for TSC_IOHCR_G3_IO_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  TSC_IOHCR_G4_IO array
   type TSC_IOHCR_G4_IO_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for TSC_IOHCR_G4_IO
   type TSC_IOHCR_G4_IO_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  G4_IO as a value
            Val : HAL.UInt4;
         when True =>
            --  G4_IO as an array
            Arr : TSC_IOHCR_G4_IO_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for TSC_IOHCR_G4_IO_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  TSC_IOHCR_G5_IO array
   type TSC_IOHCR_G5_IO_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for TSC_IOHCR_G5_IO
   type TSC_IOHCR_G5_IO_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  G5_IO as a value
            Val : HAL.UInt4;
         when True =>
            --  G5_IO as an array
            Arr : TSC_IOHCR_G5_IO_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for TSC_IOHCR_G5_IO_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  TSC_IOHCR_G6_IO array
   type TSC_IOHCR_G6_IO_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for TSC_IOHCR_G6_IO
   type TSC_IOHCR_G6_IO_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  G6_IO as a value
            Val : HAL.UInt4;
         when True =>
            --  G6_IO as an array
            Arr : TSC_IOHCR_G6_IO_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for TSC_IOHCR_G6_IO_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  TSC_IOHCR_G7_IO array
   type TSC_IOHCR_G7_IO_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for TSC_IOHCR_G7_IO
   type TSC_IOHCR_G7_IO_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  G7_IO as a value
            Val : HAL.UInt4;
         when True =>
            --  G7_IO as an array
            Arr : TSC_IOHCR_G7_IO_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for TSC_IOHCR_G7_IO_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  TSC I/O hysteresis control register
   type TSC_IOHCR_Register is record
      --  Gx_IOy Schmitt trigger hysteresis mode These bits are set and cleared
      --  by software to enable/disable the Gx_IOy Schmitt trigger hysteresis.
      --  Note: These bits control the I/O Schmitt trigger hysteresis whatever
      --  the I/O control mode is (even if controlled by standard GPIO
      --  registers).
      G1_IO          : TSC_IOHCR_G1_IO_Field :=
                        (As_Array => False, Val => 16#1#);
      --  Gx_IOy Schmitt trigger hysteresis mode These bits are set and cleared
      --  by software to enable/disable the Gx_IOy Schmitt trigger hysteresis.
      --  Note: These bits control the I/O Schmitt trigger hysteresis whatever
      --  the I/O control mode is (even if controlled by standard GPIO
      --  registers).
      G2_IO          : TSC_IOHCR_G2_IO_Field :=
                        (As_Array => False, Val => 16#1#);
      --  Gx_IOy Schmitt trigger hysteresis mode These bits are set and cleared
      --  by software to enable/disable the Gx_IOy Schmitt trigger hysteresis.
      --  Note: These bits control the I/O Schmitt trigger hysteresis whatever
      --  the I/O control mode is (even if controlled by standard GPIO
      --  registers).
      G3_IO          : TSC_IOHCR_G3_IO_Field :=
                        (As_Array => False, Val => 16#1#);
      --  Gx_IOy Schmitt trigger hysteresis mode These bits are set and cleared
      --  by software to enable/disable the Gx_IOy Schmitt trigger hysteresis.
      --  Note: These bits control the I/O Schmitt trigger hysteresis whatever
      --  the I/O control mode is (even if controlled by standard GPIO
      --  registers).
      G4_IO          : TSC_IOHCR_G4_IO_Field :=
                        (As_Array => False, Val => 16#1#);
      --  Gx_IOy Schmitt trigger hysteresis mode These bits are set and cleared
      --  by software to enable/disable the Gx_IOy Schmitt trigger hysteresis.
      --  Note: These bits control the I/O Schmitt trigger hysteresis whatever
      --  the I/O control mode is (even if controlled by standard GPIO
      --  registers).
      G5_IO          : TSC_IOHCR_G5_IO_Field :=
                        (As_Array => False, Val => 16#1#);
      --  Gx_IOy Schmitt trigger hysteresis mode These bits are set and cleared
      --  by software to enable/disable the Gx_IOy Schmitt trigger hysteresis.
      --  Note: These bits control the I/O Schmitt trigger hysteresis whatever
      --  the I/O control mode is (even if controlled by standard GPIO
      --  registers).
      G6_IO          : TSC_IOHCR_G6_IO_Field :=
                        (As_Array => False, Val => 16#1#);
      --  Gx_IOy Schmitt trigger hysteresis mode These bits are set and cleared
      --  by software to enable/disable the Gx_IOy Schmitt trigger hysteresis.
      --  Note: These bits control the I/O Schmitt trigger hysteresis whatever
      --  the I/O control mode is (even if controlled by standard GPIO
      --  registers).
      G7_IO          : TSC_IOHCR_G7_IO_Field :=
                        (As_Array => False, Val => 16#1#);
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#F#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TSC_IOHCR_Register use record
      G1_IO          at 0 range 0 .. 3;
      G2_IO          at 0 range 4 .. 7;
      G3_IO          at 0 range 8 .. 11;
      G4_IO          at 0 range 12 .. 15;
      G5_IO          at 0 range 16 .. 19;
      G6_IO          at 0 range 20 .. 23;
      G7_IO          at 0 range 24 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  TSC_IOASCR_G1_IO array
   type TSC_IOASCR_G1_IO_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for TSC_IOASCR_G1_IO
   type TSC_IOASCR_G1_IO_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  G1_IO as a value
            Val : HAL.UInt4;
         when True =>
            --  G1_IO as an array
            Arr : TSC_IOASCR_G1_IO_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for TSC_IOASCR_G1_IO_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  TSC_IOASCR_G2_IO array
   type TSC_IOASCR_G2_IO_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for TSC_IOASCR_G2_IO
   type TSC_IOASCR_G2_IO_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  G2_IO as a value
            Val : HAL.UInt4;
         when True =>
            --  G2_IO as an array
            Arr : TSC_IOASCR_G2_IO_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for TSC_IOASCR_G2_IO_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  TSC_IOASCR_G3_IO array
   type TSC_IOASCR_G3_IO_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for TSC_IOASCR_G3_IO
   type TSC_IOASCR_G3_IO_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  G3_IO as a value
            Val : HAL.UInt4;
         when True =>
            --  G3_IO as an array
            Arr : TSC_IOASCR_G3_IO_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for TSC_IOASCR_G3_IO_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  TSC_IOASCR_G4_IO array
   type TSC_IOASCR_G4_IO_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for TSC_IOASCR_G4_IO
   type TSC_IOASCR_G4_IO_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  G4_IO as a value
            Val : HAL.UInt4;
         when True =>
            --  G4_IO as an array
            Arr : TSC_IOASCR_G4_IO_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for TSC_IOASCR_G4_IO_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  TSC_IOASCR_G5_IO array
   type TSC_IOASCR_G5_IO_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for TSC_IOASCR_G5_IO
   type TSC_IOASCR_G5_IO_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  G5_IO as a value
            Val : HAL.UInt4;
         when True =>
            --  G5_IO as an array
            Arr : TSC_IOASCR_G5_IO_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for TSC_IOASCR_G5_IO_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  TSC_IOASCR_G6_IO array
   type TSC_IOASCR_G6_IO_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for TSC_IOASCR_G6_IO
   type TSC_IOASCR_G6_IO_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  G6_IO as a value
            Val : HAL.UInt4;
         when True =>
            --  G6_IO as an array
            Arr : TSC_IOASCR_G6_IO_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for TSC_IOASCR_G6_IO_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  TSC_IOASCR_G7_IO array
   type TSC_IOASCR_G7_IO_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for TSC_IOASCR_G7_IO
   type TSC_IOASCR_G7_IO_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  G7_IO as a value
            Val : HAL.UInt4;
         when True =>
            --  G7_IO as an array
            Arr : TSC_IOASCR_G7_IO_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for TSC_IOASCR_G7_IO_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  TSC I/O analog switch control register
   type TSC_IOASCR_Register is record
      --  Gx_IOy analog switch enable These bits are set and cleared by
      --  software to enable/disable the Gx_IOy analog switch. Note: These bits
      --  control the I/O analog switch whatever the I/O control mode is (even
      --  if controlled by standard GPIO registers).
      G1_IO          : TSC_IOASCR_G1_IO_Field :=
                        (As_Array => False, Val => 16#0#);
      --  Gx_IOy analog switch enable These bits are set and cleared by
      --  software to enable/disable the Gx_IOy analog switch. Note: These bits
      --  control the I/O analog switch whatever the I/O control mode is (even
      --  if controlled by standard GPIO registers).
      G2_IO          : TSC_IOASCR_G2_IO_Field :=
                        (As_Array => False, Val => 16#0#);
      --  Gx_IOy analog switch enable These bits are set and cleared by
      --  software to enable/disable the Gx_IOy analog switch. Note: These bits
      --  control the I/O analog switch whatever the I/O control mode is (even
      --  if controlled by standard GPIO registers).
      G3_IO          : TSC_IOASCR_G3_IO_Field :=
                        (As_Array => False, Val => 16#0#);
      --  Gx_IOy analog switch enable These bits are set and cleared by
      --  software to enable/disable the Gx_IOy analog switch. Note: These bits
      --  control the I/O analog switch whatever the I/O control mode is (even
      --  if controlled by standard GPIO registers).
      G4_IO          : TSC_IOASCR_G4_IO_Field :=
                        (As_Array => False, Val => 16#0#);
      --  Gx_IOy analog switch enable These bits are set and cleared by
      --  software to enable/disable the Gx_IOy analog switch. Note: These bits
      --  control the I/O analog switch whatever the I/O control mode is (even
      --  if controlled by standard GPIO registers).
      G5_IO          : TSC_IOASCR_G5_IO_Field :=
                        (As_Array => False, Val => 16#0#);
      --  Gx_IOy analog switch enable These bits are set and cleared by
      --  software to enable/disable the Gx_IOy analog switch. Note: These bits
      --  control the I/O analog switch whatever the I/O control mode is (even
      --  if controlled by standard GPIO registers).
      G6_IO          : TSC_IOASCR_G6_IO_Field :=
                        (As_Array => False, Val => 16#0#);
      --  Gx_IOy analog switch enable These bits are set and cleared by
      --  software to enable/disable the Gx_IOy analog switch. Note: These bits
      --  control the I/O analog switch whatever the I/O control mode is (even
      --  if controlled by standard GPIO registers).
      G7_IO          : TSC_IOASCR_G7_IO_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TSC_IOASCR_Register use record
      G1_IO          at 0 range 0 .. 3;
      G2_IO          at 0 range 4 .. 7;
      G3_IO          at 0 range 8 .. 11;
      G4_IO          at 0 range 12 .. 15;
      G5_IO          at 0 range 16 .. 19;
      G6_IO          at 0 range 20 .. 23;
      G7_IO          at 0 range 24 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  TSC_IOSCR_G1_IO array
   type TSC_IOSCR_G1_IO_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for TSC_IOSCR_G1_IO
   type TSC_IOSCR_G1_IO_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  G1_IO as a value
            Val : HAL.UInt4;
         when True =>
            --  G1_IO as an array
            Arr : TSC_IOSCR_G1_IO_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for TSC_IOSCR_G1_IO_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  TSC_IOSCR_G2_IO array
   type TSC_IOSCR_G2_IO_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for TSC_IOSCR_G2_IO
   type TSC_IOSCR_G2_IO_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  G2_IO as a value
            Val : HAL.UInt4;
         when True =>
            --  G2_IO as an array
            Arr : TSC_IOSCR_G2_IO_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for TSC_IOSCR_G2_IO_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  TSC_IOSCR_G3_IO array
   type TSC_IOSCR_G3_IO_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for TSC_IOSCR_G3_IO
   type TSC_IOSCR_G3_IO_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  G3_IO as a value
            Val : HAL.UInt4;
         when True =>
            --  G3_IO as an array
            Arr : TSC_IOSCR_G3_IO_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for TSC_IOSCR_G3_IO_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  TSC_IOSCR_G4_IO array
   type TSC_IOSCR_G4_IO_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for TSC_IOSCR_G4_IO
   type TSC_IOSCR_G4_IO_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  G4_IO as a value
            Val : HAL.UInt4;
         when True =>
            --  G4_IO as an array
            Arr : TSC_IOSCR_G4_IO_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for TSC_IOSCR_G4_IO_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  TSC_IOSCR_G5_IO array
   type TSC_IOSCR_G5_IO_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for TSC_IOSCR_G5_IO
   type TSC_IOSCR_G5_IO_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  G5_IO as a value
            Val : HAL.UInt4;
         when True =>
            --  G5_IO as an array
            Arr : TSC_IOSCR_G5_IO_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for TSC_IOSCR_G5_IO_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  TSC_IOSCR_G6_IO array
   type TSC_IOSCR_G6_IO_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for TSC_IOSCR_G6_IO
   type TSC_IOSCR_G6_IO_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  G6_IO as a value
            Val : HAL.UInt4;
         when True =>
            --  G6_IO as an array
            Arr : TSC_IOSCR_G6_IO_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for TSC_IOSCR_G6_IO_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  TSC_IOSCR_G7_IO array
   type TSC_IOSCR_G7_IO_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for TSC_IOSCR_G7_IO
   type TSC_IOSCR_G7_IO_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  G7_IO as a value
            Val : HAL.UInt4;
         when True =>
            --  G7_IO as an array
            Arr : TSC_IOSCR_G7_IO_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for TSC_IOSCR_G7_IO_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  TSC I/O sampling control register
   type TSC_IOSCR_Register is record
      --  Gx_IOy sampling mode These bits are set and cleared by software to
      --  configure the Gx_IOy as a sampling capacitor I/O. Only one I/O per
      --  analog I/O group must be defined as sampling capacitor. Note: These
      --  bits must not be modified when an acquisition is ongoing. Note:
      --  During the acquisition phase and even if the TSC peripheral alternate
      --  function is not enabled, as soon as the TSC_IOSCR bit is set, the
      --  corresponding GPIO analog switch is automatically controlled by the
      --  touch sensing controller.
      G1_IO          : TSC_IOSCR_G1_IO_Field :=
                        (As_Array => False, Val => 16#0#);
      --  Gx_IOy sampling mode These bits are set and cleared by software to
      --  configure the Gx_IOy as a sampling capacitor I/O. Only one I/O per
      --  analog I/O group must be defined as sampling capacitor. Note: These
      --  bits must not be modified when an acquisition is ongoing. Note:
      --  During the acquisition phase and even if the TSC peripheral alternate
      --  function is not enabled, as soon as the TSC_IOSCR bit is set, the
      --  corresponding GPIO analog switch is automatically controlled by the
      --  touch sensing controller.
      G2_IO          : TSC_IOSCR_G2_IO_Field :=
                        (As_Array => False, Val => 16#0#);
      --  Gx_IOy sampling mode These bits are set and cleared by software to
      --  configure the Gx_IOy as a sampling capacitor I/O. Only one I/O per
      --  analog I/O group must be defined as sampling capacitor. Note: These
      --  bits must not be modified when an acquisition is ongoing. Note:
      --  During the acquisition phase and even if the TSC peripheral alternate
      --  function is not enabled, as soon as the TSC_IOSCR bit is set, the
      --  corresponding GPIO analog switch is automatically controlled by the
      --  touch sensing controller.
      G3_IO          : TSC_IOSCR_G3_IO_Field :=
                        (As_Array => False, Val => 16#0#);
      --  Gx_IOy sampling mode These bits are set and cleared by software to
      --  configure the Gx_IOy as a sampling capacitor I/O. Only one I/O per
      --  analog I/O group must be defined as sampling capacitor. Note: These
      --  bits must not be modified when an acquisition is ongoing. Note:
      --  During the acquisition phase and even if the TSC peripheral alternate
      --  function is not enabled, as soon as the TSC_IOSCR bit is set, the
      --  corresponding GPIO analog switch is automatically controlled by the
      --  touch sensing controller.
      G4_IO          : TSC_IOSCR_G4_IO_Field :=
                        (As_Array => False, Val => 16#0#);
      --  Gx_IOy sampling mode These bits are set and cleared by software to
      --  configure the Gx_IOy as a sampling capacitor I/O. Only one I/O per
      --  analog I/O group must be defined as sampling capacitor. Note: These
      --  bits must not be modified when an acquisition is ongoing. Note:
      --  During the acquisition phase and even if the TSC peripheral alternate
      --  function is not enabled, as soon as the TSC_IOSCR bit is set, the
      --  corresponding GPIO analog switch is automatically controlled by the
      --  touch sensing controller.
      G5_IO          : TSC_IOSCR_G5_IO_Field :=
                        (As_Array => False, Val => 16#0#);
      --  Gx_IOy sampling mode These bits are set and cleared by software to
      --  configure the Gx_IOy as a sampling capacitor I/O. Only one I/O per
      --  analog I/O group must be defined as sampling capacitor. Note: These
      --  bits must not be modified when an acquisition is ongoing. Note:
      --  During the acquisition phase and even if the TSC peripheral alternate
      --  function is not enabled, as soon as the TSC_IOSCR bit is set, the
      --  corresponding GPIO analog switch is automatically controlled by the
      --  touch sensing controller.
      G6_IO          : TSC_IOSCR_G6_IO_Field :=
                        (As_Array => False, Val => 16#0#);
      --  Gx_IOy sampling mode These bits are set and cleared by software to
      --  configure the Gx_IOy as a sampling capacitor I/O. Only one I/O per
      --  analog I/O group must be defined as sampling capacitor. Note: These
      --  bits must not be modified when an acquisition is ongoing. Note:
      --  During the acquisition phase and even if the TSC peripheral alternate
      --  function is not enabled, as soon as the TSC_IOSCR bit is set, the
      --  corresponding GPIO analog switch is automatically controlled by the
      --  touch sensing controller.
      G7_IO          : TSC_IOSCR_G7_IO_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TSC_IOSCR_Register use record
      G1_IO          at 0 range 0 .. 3;
      G2_IO          at 0 range 4 .. 7;
      G3_IO          at 0 range 8 .. 11;
      G4_IO          at 0 range 12 .. 15;
      G5_IO          at 0 range 16 .. 19;
      G6_IO          at 0 range 20 .. 23;
      G7_IO          at 0 range 24 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  TSC_IOCCR_G1_IO array
   type TSC_IOCCR_G1_IO_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for TSC_IOCCR_G1_IO
   type TSC_IOCCR_G1_IO_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  G1_IO as a value
            Val : HAL.UInt4;
         when True =>
            --  G1_IO as an array
            Arr : TSC_IOCCR_G1_IO_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for TSC_IOCCR_G1_IO_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  TSC_IOCCR_G2_IO array
   type TSC_IOCCR_G2_IO_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for TSC_IOCCR_G2_IO
   type TSC_IOCCR_G2_IO_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  G2_IO as a value
            Val : HAL.UInt4;
         when True =>
            --  G2_IO as an array
            Arr : TSC_IOCCR_G2_IO_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for TSC_IOCCR_G2_IO_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  TSC_IOCCR_G3_IO array
   type TSC_IOCCR_G3_IO_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for TSC_IOCCR_G3_IO
   type TSC_IOCCR_G3_IO_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  G3_IO as a value
            Val : HAL.UInt4;
         when True =>
            --  G3_IO as an array
            Arr : TSC_IOCCR_G3_IO_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for TSC_IOCCR_G3_IO_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  TSC_IOCCR_G4_IO array
   type TSC_IOCCR_G4_IO_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for TSC_IOCCR_G4_IO
   type TSC_IOCCR_G4_IO_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  G4_IO as a value
            Val : HAL.UInt4;
         when True =>
            --  G4_IO as an array
            Arr : TSC_IOCCR_G4_IO_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for TSC_IOCCR_G4_IO_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  TSC_IOCCR_G5_IO array
   type TSC_IOCCR_G5_IO_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for TSC_IOCCR_G5_IO
   type TSC_IOCCR_G5_IO_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  G5_IO as a value
            Val : HAL.UInt4;
         when True =>
            --  G5_IO as an array
            Arr : TSC_IOCCR_G5_IO_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for TSC_IOCCR_G5_IO_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  TSC_IOCCR_G6_IO array
   type TSC_IOCCR_G6_IO_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for TSC_IOCCR_G6_IO
   type TSC_IOCCR_G6_IO_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  G6_IO as a value
            Val : HAL.UInt4;
         when True =>
            --  G6_IO as an array
            Arr : TSC_IOCCR_G6_IO_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for TSC_IOCCR_G6_IO_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  TSC_IOCCR_G7_IO array
   type TSC_IOCCR_G7_IO_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for TSC_IOCCR_G7_IO
   type TSC_IOCCR_G7_IO_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  G7_IO as a value
            Val : HAL.UInt4;
         when True =>
            --  G7_IO as an array
            Arr : TSC_IOCCR_G7_IO_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for TSC_IOCCR_G7_IO_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  TSC I/O channel control register
   type TSC_IOCCR_Register is record
      --  Gx_IOy channel mode These bits are set and cleared by software to
      --  configure the Gx_IOy as a channel I/O. Note: These bits must not be
      --  modified when an acquisition is ongoing. Note: During the acquisition
      --  phase and even if the TSC peripheral alternate function is not
      --  enabled, as soon as the TSC_IOCCR bit is set, the corresponding GPIO
      --  analog switch is automatically controlled by the touch sensing
      --  controller.
      G1_IO          : TSC_IOCCR_G1_IO_Field :=
                        (As_Array => False, Val => 16#0#);
      --  Gx_IOy channel mode These bits are set and cleared by software to
      --  configure the Gx_IOy as a channel I/O. Note: These bits must not be
      --  modified when an acquisition is ongoing. Note: During the acquisition
      --  phase and even if the TSC peripheral alternate function is not
      --  enabled, as soon as the TSC_IOCCR bit is set, the corresponding GPIO
      --  analog switch is automatically controlled by the touch sensing
      --  controller.
      G2_IO          : TSC_IOCCR_G2_IO_Field :=
                        (As_Array => False, Val => 16#0#);
      --  Gx_IOy channel mode These bits are set and cleared by software to
      --  configure the Gx_IOy as a channel I/O. Note: These bits must not be
      --  modified when an acquisition is ongoing. Note: During the acquisition
      --  phase and even if the TSC peripheral alternate function is not
      --  enabled, as soon as the TSC_IOCCR bit is set, the corresponding GPIO
      --  analog switch is automatically controlled by the touch sensing
      --  controller.
      G3_IO          : TSC_IOCCR_G3_IO_Field :=
                        (As_Array => False, Val => 16#0#);
      --  Gx_IOy channel mode These bits are set and cleared by software to
      --  configure the Gx_IOy as a channel I/O. Note: These bits must not be
      --  modified when an acquisition is ongoing. Note: During the acquisition
      --  phase and even if the TSC peripheral alternate function is not
      --  enabled, as soon as the TSC_IOCCR bit is set, the corresponding GPIO
      --  analog switch is automatically controlled by the touch sensing
      --  controller.
      G4_IO          : TSC_IOCCR_G4_IO_Field :=
                        (As_Array => False, Val => 16#0#);
      --  Gx_IOy channel mode These bits are set and cleared by software to
      --  configure the Gx_IOy as a channel I/O. Note: These bits must not be
      --  modified when an acquisition is ongoing. Note: During the acquisition
      --  phase and even if the TSC peripheral alternate function is not
      --  enabled, as soon as the TSC_IOCCR bit is set, the corresponding GPIO
      --  analog switch is automatically controlled by the touch sensing
      --  controller.
      G5_IO          : TSC_IOCCR_G5_IO_Field :=
                        (As_Array => False, Val => 16#0#);
      --  Gx_IOy channel mode These bits are set and cleared by software to
      --  configure the Gx_IOy as a channel I/O. Note: These bits must not be
      --  modified when an acquisition is ongoing. Note: During the acquisition
      --  phase and even if the TSC peripheral alternate function is not
      --  enabled, as soon as the TSC_IOCCR bit is set, the corresponding GPIO
      --  analog switch is automatically controlled by the touch sensing
      --  controller.
      G6_IO          : TSC_IOCCR_G6_IO_Field :=
                        (As_Array => False, Val => 16#0#);
      --  Gx_IOy channel mode These bits are set and cleared by software to
      --  configure the Gx_IOy as a channel I/O. Note: These bits must not be
      --  modified when an acquisition is ongoing. Note: During the acquisition
      --  phase and even if the TSC peripheral alternate function is not
      --  enabled, as soon as the TSC_IOCCR bit is set, the corresponding GPIO
      --  analog switch is automatically controlled by the touch sensing
      --  controller.
      G7_IO          : TSC_IOCCR_G7_IO_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TSC_IOCCR_Register use record
      G1_IO          at 0 range 0 .. 3;
      G2_IO          at 0 range 4 .. 7;
      G3_IO          at 0 range 8 .. 11;
      G4_IO          at 0 range 12 .. 15;
      G5_IO          at 0 range 16 .. 19;
      G6_IO          at 0 range 20 .. 23;
      G7_IO          at 0 range 24 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  TSC I/O group control status register
   type TSC_IOGCSR_Register is record
      --  Analog I/O group x enable These bits are set and cleared by software
      --  to enable/disable the acquisition (counter is counting) on the
      --  corresponding analog I/O group x.
      G1E            : Boolean := False;
      --  Analog I/O group x enable These bits are set and cleared by software
      --  to enable/disable the acquisition (counter is counting) on the
      --  corresponding analog I/O group x.
      G2E            : Boolean := False;
      --  Analog I/O group x enable These bits are set and cleared by software
      --  to enable/disable the acquisition (counter is counting) on the
      --  corresponding analog I/O group x.
      G3E            : Boolean := False;
      --  Analog I/O group x enable These bits are set and cleared by software
      --  to enable/disable the acquisition (counter is counting) on the
      --  corresponding analog I/O group x.
      G4E            : Boolean := False;
      --  Analog I/O group x enable These bits are set and cleared by software
      --  to enable/disable the acquisition (counter is counting) on the
      --  corresponding analog I/O group x.
      G5E            : Boolean := False;
      --  Analog I/O group x enable These bits are set and cleared by software
      --  to enable/disable the acquisition (counter is counting) on the
      --  corresponding analog I/O group x.
      G6E            : Boolean := False;
      --  Analog I/O group x enable These bits are set and cleared by software
      --  to enable/disable the acquisition (counter is counting) on the
      --  corresponding analog I/O group x.
      G7E            : Boolean := False;
      --  unspecified
      Reserved_7_15  : HAL.UInt9 := 16#0#;
      --  Read-only. Analog I/O group x status These bits are set by hardware
      --  when the acquisition on the corresponding enabled analog I/O group x
      --  is complete. They are cleared by hardware when a new acquisition is
      --  started. Note: When a max count error is detected the remaining GxS
      --  bits of the enabled analog I/O groups are not set.
      G1S            : Boolean := False;
      --  Read-only. Analog I/O group x status These bits are set by hardware
      --  when the acquisition on the corresponding enabled analog I/O group x
      --  is complete. They are cleared by hardware when a new acquisition is
      --  started. Note: When a max count error is detected the remaining GxS
      --  bits of the enabled analog I/O groups are not set.
      G2S            : Boolean := False;
      --  Read-only. Analog I/O group x status These bits are set by hardware
      --  when the acquisition on the corresponding enabled analog I/O group x
      --  is complete. They are cleared by hardware when a new acquisition is
      --  started. Note: When a max count error is detected the remaining GxS
      --  bits of the enabled analog I/O groups are not set.
      G3S            : Boolean := False;
      --  Read-only. Analog I/O group x status These bits are set by hardware
      --  when the acquisition on the corresponding enabled analog I/O group x
      --  is complete. They are cleared by hardware when a new acquisition is
      --  started. Note: When a max count error is detected the remaining GxS
      --  bits of the enabled analog I/O groups are not set.
      G4S            : Boolean := False;
      --  Read-only. Analog I/O group x status These bits are set by hardware
      --  when the acquisition on the corresponding enabled analog I/O group x
      --  is complete. They are cleared by hardware when a new acquisition is
      --  started. Note: When a max count error is detected the remaining GxS
      --  bits of the enabled analog I/O groups are not set.
      G5S            : Boolean := False;
      --  Read-only. Analog I/O group x status These bits are set by hardware
      --  when the acquisition on the corresponding enabled analog I/O group x
      --  is complete. They are cleared by hardware when a new acquisition is
      --  started. Note: When a max count error is detected the remaining GxS
      --  bits of the enabled analog I/O groups are not set.
      G6S            : Boolean := False;
      --  Read-only. Analog I/O group x status These bits are set by hardware
      --  when the acquisition on the corresponding enabled analog I/O group x
      --  is complete. They are cleared by hardware when a new acquisition is
      --  started. Note: When a max count error is detected the remaining GxS
      --  bits of the enabled analog I/O groups are not set.
      G7S            : Boolean := False;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TSC_IOGCSR_Register use record
      G1E            at 0 range 0 .. 0;
      G2E            at 0 range 1 .. 1;
      G3E            at 0 range 2 .. 2;
      G4E            at 0 range 3 .. 3;
      G5E            at 0 range 4 .. 4;
      G6E            at 0 range 5 .. 5;
      G7E            at 0 range 6 .. 6;
      Reserved_7_15  at 0 range 7 .. 15;
      G1S            at 0 range 16 .. 16;
      G2S            at 0 range 17 .. 17;
      G3S            at 0 range 18 .. 18;
      G4S            at 0 range 19 .. 19;
      G5S            at 0 range 20 .. 20;
      G6S            at 0 range 21 .. 21;
      G7S            at 0 range 22 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   subtype TSC_IOG1CR_CNT_Field is HAL.UInt14;

   --  TSC I/O group 1 counter register
   type TSC_IOG1CR_Register is record
      --  Read-only. Counter value These bits represent the number of charge
      --  transfer cycles generated on the analog I/O group x to complete its
      --  acquisition (voltage across C<sub>S</sub> has reached the threshold).
      CNT            : TSC_IOG1CR_CNT_Field;
      --  unspecified
      Reserved_14_31 : HAL.UInt18;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TSC_IOG1CR_Register use record
      CNT            at 0 range 0 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   subtype TSC_IOG2CR_CNT_Field is HAL.UInt14;

   --  TSC I/O group 2 counter register
   type TSC_IOG2CR_Register is record
      --  Read-only. Counter value These bits represent the number of charge
      --  transfer cycles generated on the analog I/O group x to complete its
      --  acquisition (voltage across C<sub>S</sub> has reached the threshold).
      CNT            : TSC_IOG2CR_CNT_Field;
      --  unspecified
      Reserved_14_31 : HAL.UInt18;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TSC_IOG2CR_Register use record
      CNT            at 0 range 0 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   subtype TSC_IOG3CR_CNT_Field is HAL.UInt14;

   --  TSC I/O group 3 counter register
   type TSC_IOG3CR_Register is record
      --  Read-only. Counter value These bits represent the number of charge
      --  transfer cycles generated on the analog I/O group x to complete its
      --  acquisition (voltage across C<sub>S</sub> has reached the threshold).
      CNT            : TSC_IOG3CR_CNT_Field;
      --  unspecified
      Reserved_14_31 : HAL.UInt18;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TSC_IOG3CR_Register use record
      CNT            at 0 range 0 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   subtype TSC_IOG4CR_CNT_Field is HAL.UInt14;

   --  TSC I/O group 4 counter register
   type TSC_IOG4CR_Register is record
      --  Read-only. Counter value These bits represent the number of charge
      --  transfer cycles generated on the analog I/O group x to complete its
      --  acquisition (voltage across C<sub>S</sub> has reached the threshold).
      CNT            : TSC_IOG4CR_CNT_Field;
      --  unspecified
      Reserved_14_31 : HAL.UInt18;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TSC_IOG4CR_Register use record
      CNT            at 0 range 0 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   subtype TSC_IOG5CR_CNT_Field is HAL.UInt14;

   --  TSC I/O group 5 counter register
   type TSC_IOG5CR_Register is record
      --  Read-only. Counter value These bits represent the number of charge
      --  transfer cycles generated on the analog I/O group x to complete its
      --  acquisition (voltage across C<sub>S</sub> has reached the threshold).
      CNT            : TSC_IOG5CR_CNT_Field;
      --  unspecified
      Reserved_14_31 : HAL.UInt18;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TSC_IOG5CR_Register use record
      CNT            at 0 range 0 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   subtype TSC_IOG6CR_CNT_Field is HAL.UInt14;

   --  TSC I/O group 6 counter register
   type TSC_IOG6CR_Register is record
      --  Read-only. Counter value These bits represent the number of charge
      --  transfer cycles generated on the analog I/O group x to complete its
      --  acquisition (voltage across C<sub>S</sub> has reached the threshold).
      CNT            : TSC_IOG6CR_CNT_Field;
      --  unspecified
      Reserved_14_31 : HAL.UInt18;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TSC_IOG6CR_Register use record
      CNT            at 0 range 0 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   subtype TSC_IOG7CR_CNT_Field is HAL.UInt14;

   --  TSC I/O group 7 counter register
   type TSC_IOG7CR_Register is record
      --  Read-only. Counter value These bits represent the number of charge
      --  transfer cycles generated on the analog I/O group x to complete its
      --  acquisition (voltage across C<sub>S</sub> has reached the threshold).
      CNT            : TSC_IOG7CR_CNT_Field;
      --  unspecified
      Reserved_14_31 : HAL.UInt18;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TSC_IOG7CR_Register use record
      CNT            at 0 range 0 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  TSC address block description
   type TSC_Peripheral is record
      --  TSC control register
      TSC_CR     : aliased TSC_CR_Register;
      --  TSC interrupt enable register
      TSC_IER    : aliased TSC_IER_Register;
      --  TSC interrupt clear register
      TSC_ICR    : aliased TSC_ICR_Register;
      --  TSC interrupt status register
      TSC_ISR    : aliased TSC_ISR_Register;
      --  TSC I/O hysteresis control register
      TSC_IOHCR  : aliased TSC_IOHCR_Register;
      --  TSC I/O analog switch control register
      TSC_IOASCR : aliased TSC_IOASCR_Register;
      --  TSC I/O sampling control register
      TSC_IOSCR  : aliased TSC_IOSCR_Register;
      --  TSC I/O channel control register
      TSC_IOCCR  : aliased TSC_IOCCR_Register;
      --  TSC I/O group control status register
      TSC_IOGCSR : aliased TSC_IOGCSR_Register;
      --  TSC I/O group 1 counter register
      TSC_IOG1CR : aliased TSC_IOG1CR_Register;
      --  TSC I/O group 2 counter register
      TSC_IOG2CR : aliased TSC_IOG2CR_Register;
      --  TSC I/O group 3 counter register
      TSC_IOG3CR : aliased TSC_IOG3CR_Register;
      --  TSC I/O group 4 counter register
      TSC_IOG4CR : aliased TSC_IOG4CR_Register;
      --  TSC I/O group 5 counter register
      TSC_IOG5CR : aliased TSC_IOG5CR_Register;
      --  TSC I/O group 6 counter register
      TSC_IOG6CR : aliased TSC_IOG6CR_Register;
      --  TSC I/O group 7 counter register
      TSC_IOG7CR : aliased TSC_IOG7CR_Register;
   end record
     with Volatile;

   for TSC_Peripheral use record
      TSC_CR     at 16#0# range 0 .. 31;
      TSC_IER    at 16#4# range 0 .. 31;
      TSC_ICR    at 16#8# range 0 .. 31;
      TSC_ISR    at 16#C# range 0 .. 31;
      TSC_IOHCR  at 16#10# range 0 .. 31;
      TSC_IOASCR at 16#18# range 0 .. 31;
      TSC_IOSCR  at 16#20# range 0 .. 31;
      TSC_IOCCR  at 16#28# range 0 .. 31;
      TSC_IOGCSR at 16#30# range 0 .. 31;
      TSC_IOG1CR at 16#34# range 0 .. 31;
      TSC_IOG2CR at 16#38# range 0 .. 31;
      TSC_IOG3CR at 16#3C# range 0 .. 31;
      TSC_IOG4CR at 16#40# range 0 .. 31;
      TSC_IOG5CR at 16#44# range 0 .. 31;
      TSC_IOG6CR at 16#48# range 0 .. 31;
      TSC_IOG7CR at 16#4C# range 0 .. 31;
   end record;

   --  TSC address block description
   TSC_Periph : aliased TSC_Peripheral
     with Import, Address => TSC_Base;

end STM32_SVD.TSC;
