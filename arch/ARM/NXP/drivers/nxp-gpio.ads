private with NXP_SVD.GPIO;
with HAL.GPIO;
with NXP.Pint;       use NXP.Pint;
--  limited with NXP.IOCON;

package NXP.GPIO is

   type GPIO_Port is limited private;

   type GPIO_Pin is
     (Pin_0, Pin_1, Pin_2,  Pin_3,  Pin_4,  Pin_5,  Pin_6,  Pin_7,
      Pin_8, Pin_9, Pin_10, Pin_11, Pin_12, Pin_13, Pin_14, Pin_15,
      Pin_16, Pin_17, Pin_18, Pin_19, Pin_20, Pin_21, Pin_22, Pin_23,
      Pin_24, Pin_25, Pin_26, Pin_27, Pin_28, Pin_29, Pin_30, Pin_31
     );

   for GPIO_Pin use
     (Pin_0  => 16#0000_0001#,
      Pin_1  => 16#0000_0002#,
      Pin_2  => 16#0000_0004#,
      Pin_3  => 16#0000_0008#,
      Pin_4  => 16#0000_0010#,
      Pin_5  => 16#0000_0020#,
      Pin_6  => 16#0000_0040#,
      Pin_7  => 16#0000_0080#,
      Pin_8  => 16#0000_0100#,
      Pin_9  => 16#0000_0200#,
      Pin_10 => 16#0000_0400#,
      Pin_11 => 16#0000_0800#,
      Pin_12 => 16#0000_1000#,
      Pin_13 => 16#0000_2000#,
      Pin_14 => 16#0000_4000#,
      Pin_15 => 16#0000_8000#,
      Pin_16 => 16#0001_0000#,
      Pin_17 => 16#0002_0000#,
      Pin_18 => 16#0004_0000#,
      Pin_19 => 16#0008_0000#,
      Pin_20 => 16#0010_0000#,
      Pin_21 => 16#0020_0000#,
      Pin_22 => 16#0040_0000#,
      Pin_23 => 16#0080_0000#,
      Pin_24 => 16#0100_0000#,
      Pin_25 => 16#0200_0000#,
      Pin_26 => 16#0400_0000#,
      Pin_27 => 16#0800_0000#,
      Pin_28 => 16#1000_0000#,
      Pin_29 => 16#2000_0000#,
      Pin_30 => 16#4000_0000#,
      Pin_31 => 16#8000_0000#);

   for GPIO_Pin'Size use 32;
   --  for compatibility with hardware registers

   subtype GPIO_Pin_Index is Natural range 0 .. GPIO_Pin'Pos (GPIO_Pin'Last);

   type GPIO_Pins is array (Positive range <>) of GPIO_Pin;
   --  Note that, in addition to aggregates, the language-defined catenation
   --  operator "&" is available for types GPIO_Pin and GPIO_Pins, allowing one
   --  to construct GPIO_Pins values conveniently

   All_Pins : constant GPIO_Pins :=
                (Pin_0, Pin_1, Pin_2, Pin_3, Pin_4, Pin_5, Pin_6, Pin_7,
                 Pin_8, Pin_9, Pin_10, Pin_11, Pin_12, Pin_13, Pin_14, Pin_15,
                 Pin_16, Pin_17, Pin_18, Pin_19, Pin_20, Pin_21, Pin_22, Pin_23,
                 Pin_24, Pin_25, Pin_26, Pin_27, Pin_28, Pin_29, Pin_30, Pin_31
                );

   type Pin_IO_Modes is (Mode_In, Mode_Out, Mode_AF, Mode_Analog)
     with Size => 2;

   for Pin_IO_Modes use
     (Mode_In     => 0,
      Mode_Out    => 1,
      Mode_AF     => 2,
      Mode_Analog => 3);

   type Pin_Output_Types is (Push_Pull, Open_Drain)
     with Size => 1;

   for Pin_Output_Types use (Push_Pull => 0, Open_Drain => 1);

   type Pin_Output_Speeds is (Speed_Low, Speed_High)
     with Size => 1;

   for Pin_Output_Speeds use
     (Speed_Low   => 0,
      Speed_High  => 1);

   type Internal_Pin_Resistors is (Floating, Pull_Down, Pull_Up, Repeater)
     with Size => 2;

   type GPIO_Port_Configuration is record
      Mode        : Pin_IO_Modes;
      Output_Type : Pin_Output_Types;
      Speed       : Pin_Output_Speeds;
      Resistors   : Internal_Pin_Resistors;
      Invert      : Boolean;
   end record;

   type Ports is (Port_0, Port_1);

   type GPIO_Point is new HAL.GPIO.GPIO_Point with record
      Periph : access GPIO_Port;
      --  Port should be a not null access, but this raises an exception
      --  during elaboration.
      Port   : Ports;
      Pin    : GPIO_Pin;
   end record;

   overriding
   function Mode (This : GPIO_Point) return HAL.GPIO.GPIO_Mode;

   overriding
   function Set_Mode (This : in out GPIO_Point;
                      Mode : HAL.GPIO.GPIO_Config_Mode) return Boolean;

   overriding
   function Pull_Resistor (This : GPIO_Point)
                           return HAL.GPIO.GPIO_Pull_Resistor;

   overriding
   function Set_Pull_Resistor (This : in out GPIO_Point;
                               Pull : HAL.GPIO.GPIO_Pull_Resistor)
                               return Boolean;

   overriding
   function Set (This : GPIO_Point) return Boolean with
     Inline;
   --  Returns True if the bit specified by This.Pin is set (not zero) in the
   --  input data register of This.Port.all; returns False otherwise.

   overriding
   procedure Set (This : in out GPIO_Point) with
     Inline;
   --  For This.Port.all, sets the output data register bit specified by
   --  This.Pin to one. Other pins are unaffected.

   overriding
   procedure Clear (This : in out GPIO_Point) with
     Inline;
   --  For This.Port.all, sets the output data register bit specified by
   --  This.Pin to zero. Other pins are unaffected.

   overriding
   procedure Toggle (This : in out GPIO_Point) with
     Inline;
   --  For This.Port.all, negates the output data register bit specified by
   --  This.Pin (one becomes zero and vice versa). Other pins are unaffected.

   procedure Configure_IO
     (This   : GPIO_Point;
      Config : GPIO_Port_Configuration);
   --  For Point.Pin on the Point.Port.all, configures the
   --  characteristics specified by Config

   type GPIO_Points is array (Positive range <>) of GPIO_Point;

   function Any_Set (Pins : GPIO_Points) return Boolean with
     Inline;
   --  Returns True if any one of the bits specified by Pins is set (not zero)
   --  in the Port input data register; returns False otherwise.

   function Set (Pins : GPIO_Points) return Boolean
     renames Any_Set;
   --  Defined for readability when only one pin is specified in GPIO_Pins

   function All_Set (Pins : GPIO_Points) return Boolean with
     Inline;
   --  Returns True iff all of the bits specified by Pins are set (not zero) in
   --  the Port input data register; returns False otherwise.

   procedure Set (Pins : in out GPIO_Points) with
     Inline;
   --  For the given GPIO port, sets all of the output data register bits
   --  specified by Pins to one. Other pins are unaffected.

   procedure Clear (Pins : in out GPIO_Points) with
     Inline;
   --  For the given GPIO port, sets of all of the output data register bits
   --  specified by Pins to zero. Other pins are unaffected.

   procedure Toggle (Points : in out GPIO_Points) with Inline;
   --  For the given GPIO ports, negates all of the output data register bis
   --  specified by Pins (ones become zeros and vice versa). Other pins are
   --  unaffected.

   procedure Configure_IO
     (Points : GPIO_Points;
      Config : GPIO_Port_Configuration);
   --  For Point.Pin on the Point.Port.all, configures the
   --  characteristics specified by Config

   procedure Configure_Alternate_Function
     (This : GPIO_Point;
      AF   : GPIO_Alternate_Function);

   procedure Configure_Alternate_Function
     (Points : GPIO_Points;
      AF     : GPIO_Alternate_Function);

   procedure Enable_GPIO_Interrupt (Pin : GPIO_Point; Config : Pint_Configuration);

private

   type GPIO_Port is new NXP_SVD.GPIO.GPIO_Peripheral;

end NXP.GPIO;
