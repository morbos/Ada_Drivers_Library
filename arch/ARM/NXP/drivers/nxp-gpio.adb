with NXP.Device;         use NXP.Device;
with System;             use System;
with NXP_SVD;            use NXP_SVD;
with NXP_SVD.GPIO;       use NXP_SVD.GPIO;
with NXP_SVD.IOCON;      use NXP_SVD.IOCON;
with NXP_SVD.INPUTMUX;   use NXP_SVD.INPUTMUX;
with NXP_SVD.PINT;       use NXP_SVD.PINT;

package body NXP.GPIO is

   IOCON : aliased IOCON_Peripheral with Import, Address => S_NS_Periph (IOCON_Base);

   -------------
   -- Any_Set --
   -------------

   function Any_Set (Pins : GPIO_Points) return Boolean is
   begin
      for Pin of Pins loop
         if Pin.Set then
            return True;
         end if;
      end loop;
      return False;
   end Any_Set;

   ----------
   -- Mode --
   ----------

   overriding
   function Mode (This : GPIO_Point) return HAL.GPIO.GPIO_Mode is
      Index : constant GPIO_Pin_Index := GPIO_Pin'Pos (This.Pin);
   begin
      return HAL.GPIO.Output;
   end Mode;

   --------------
   -- Set_Mode --
   --------------

   overriding
   function Set_Mode
     (This : in out GPIO_Point;
      Mode : HAL.GPIO.GPIO_Config_Mode)
      return Boolean
   is
      Index : constant GPIO_Pin_Index := GPIO_Pin'Pos (This.Pin);
   begin
      return True;
   end Set_Mode;

   -------------------
   -- Pull_Resistor --
   -------------------

   overriding
   function Pull_Resistor
     (This : GPIO_Point)
      return HAL.GPIO.GPIO_Pull_Resistor
   is
      Index : constant GPIO_Pin_Index := GPIO_Pin'Pos (This.Pin);
   begin
      return HAL.GPIO.Floating;
   end Pull_Resistor;

   -----------------------
   -- Set_Pull_Resistor --
   -----------------------

   overriding
   function Set_Pull_Resistor
     (This : in out GPIO_Point;
      Pull : HAL.GPIO.GPIO_Pull_Resistor)
      return Boolean
   is
      Index : constant GPIO_Pin_Index := GPIO_Pin'Pos (This.Pin);
   begin
      return True;
   end Set_Pull_Resistor;

   ---------
   -- Set --
   ---------

   overriding
   function Set (This : GPIO_Point) return Boolean
   is
      Port_Idx : Integer := This.Port'Enum_Rep;
      Index : constant GPIO_Pin_Index := GPIO_Pin'Pos (This.Pin);
   begin
      return This.Periph.B (Port_Idx).B (Index).PBYTE;
   end Set;

   -------------
   -- All_Set --
   -------------

   function All_Set (Pins : GPIO_Points) return Boolean is
   begin
      for Pin of Pins loop
         if not Pin.Set then
            return False;
         end if;
      end loop;

      return True;
   end All_Set;

   ---------
   -- Set --
   ---------

   overriding
   procedure Set (This : in out GPIO_Point)
   is
      Port_Idx : Integer := This.Port'Enum_Rep;
      Index : constant GPIO_Pin_Index := GPIO_Pin'Pos (This.Pin);
   begin
      This.Periph.B (Port_Idx).B (Index).PBYTE := True;
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set (Pins : in out GPIO_Points) is
   begin
      for Pin of Pins loop
         Pin.Set;
      end loop;
   end Set;

   -----------
   -- Clear --
   -----------

   overriding
   procedure Clear (This : in out GPIO_Point)
   is
      Port_Idx : Integer := This.Port'Enum_Rep;
      Index : constant GPIO_Pin_Index := GPIO_Pin'Pos (This.Pin);
   begin
      This.Periph.B (Port_Idx).B (Index).PBYTE := False;
   end Clear;

   -----------
   -- Clear --
   -----------

   procedure Clear (Pins : in out GPIO_Points) is
   begin
      for Pin of Pins loop
         Pin.Clear;
      end loop;
   end Clear;

   ------------
   -- Toggle --
   ------------

   overriding
   procedure Toggle (This : in out GPIO_Point)
   is
      Port_Idx : Integer := This.Port'Enum_Rep;
      Index : constant GPIO_Pin_Index := GPIO_Pin'Pos (This.Pin);
      Mask : UInt32 := 2 ** Index;
   begin
      This.Periph.NOT_k (Port_Idx) := Mask;
   end Toggle;

   ------------
   -- Toggle --
   ------------

   procedure Toggle (Points : in out GPIO_Points) is
   begin
      for Point of Points loop
         Point.Toggle;
      end loop;
   end Toggle;

   ------------------
   -- Configure_IO --
   ------------------

   procedure Configure_IO
     (This   : GPIO_Point;
      Config : GPIO_Port_Configuration)
   is
      Port_Idx : Integer := This.Port'Enum_Rep;
      Index : constant GPIO_Pin_Index := GPIO_Pin'Pos (This.Pin);
      Mask : UInt32 := 2 ** Index;
   begin
      case Config.Mode is
         when Mode_Out =>
            This.Periph.DIR (Port_Idx) := This.Periph.DIR (Port_Idx) or Mask;
            IOCON.P (Port_Idx).PIO (Index).CTL.SLEW :=
              CTL_SLEW_Field'Enum_Val (Config.Speed'Enum_Rep);
            IOCON.P (Port_Idx).PIO (Index).CTL.OD :=
              CTL_OD_Field'Enum_Val (Config.Output_Type'Enum_Rep);
         when Mode_In =>
            This.Periph.DIR (Port_Idx) := This.Periph.DIR (Port_Idx) and not Mask;
            IOCON.P (Port_Idx).PIO (Index).CTL.MODE :=
              CTL_MODE_Field'Enum_Val (Config.Resistors'Enum_Rep);
            IOCON.P (Port_Idx).PIO (Index).CTL.DIGIMODE := Digital;
            IOCON.P (Port_Idx).PIO (Index).CTL.INVERT :=
              (if Config.Invert then Enabled else Disabled);
         when Mode_AF =>
            IOCON.P (Port_Idx).PIO (Index).CTL.MODE :=
              CTL_MODE_Field'Enum_Val (Config.Resistors'Enum_Rep);
            IOCON.P (Port_Idx).PIO (Index).CTL.DIGIMODE := Digital;
            IOCON.P (Port_Idx).PIO (Index).CTL.INVERT :=
              (if Config.Invert then Enabled else Disabled);
         when others =>
            null;
      end case;
   end Configure_IO;

   ------------------
   -- Configure_IO --
   ------------------

   procedure Configure_IO
     (Points : GPIO_Points;
      Config : GPIO_Port_Configuration)
   is
   begin
      for Point of Points loop
         Point.Configure_IO (Config);
      end loop;
   end Configure_IO;

   ----------------------------------
   -- Configure_Alternate_Function --
   ----------------------------------

   procedure Configure_Alternate_Function
     (This : GPIO_Point;
      AF   : GPIO_Alternate_Function)
   is
      Port_Idx : Integer := This.Port'Enum_Rep;
      Index : constant GPIO_Pin_Index := GPIO_Pin'Pos (This.Pin);
   begin
      IOCON.P (Port_Idx).PIO (Index).CTL.FUNC := UInt4 (AF);
   end Configure_Alternate_Function;

   ----------------------------------
   -- Configure_Alternate_Function --
   ----------------------------------

   procedure Configure_Alternate_Function
     (Points : GPIO_Points;
      AF     : GPIO_Alternate_Function)
   is
   begin
      for Point of Points loop
         Point.Configure_Alternate_Function (AF);
      end loop;
   end Configure_Alternate_Function;

   procedure Enable_GPIO_Interrupt (Pin : GPIO_Point; Config : Pint_Configuration)
   is
      Port_Idx : Integer := Pin.Port'Enum_Rep;
      Index : constant GPIO_Pin_Index := GPIO_Pin'Pos (Pin.Pin);
      INPUTMUX : aliased INPUTMUX_Peripheral
        with Import, Address => S_NS_Periph (INPUTMUX_Base);
      PINT : aliased PINT_Peripheral
        with Import, Address => S_NS_Periph (PINT_Base);
      Slot : Integer := Config.Slot'Enum_Rep;
      Mask : UInt8 := (2 ** Slot);
   begin
      INPUTMUX.PINTSEL (Slot'Enum_Rep).INTPIN := UInt7 ((Port_Idx * 32) + Index);
      if Config.Mode = Pint_Edge then
         PINT.ISEL.PMODE := PINT.ISEL.PMODE and not Mask;
         if Config.Edge = Pint_Rising then
            PINT.SIENR.SETENRL := Mask;
         else
            PINT.SIENF.SETENAF := Mask;
         end if;
      else --  handle level
         PINT.ISEL.PMODE := PINT.ISEL.PMODE or Mask;
         if Config.Level = Pint_High then
            PINT.SIENR.SETENRL := Mask;
         else
            PINT.SIENF.SETENAF := Mask;
         end if;
      end if;
   end Enable_GPIO_Interrupt;

end NXP.GPIO;
