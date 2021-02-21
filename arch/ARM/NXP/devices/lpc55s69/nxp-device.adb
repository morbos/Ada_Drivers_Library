with HAL;            use HAL;
with System;         use System;
pragma Warnings (Off, "* is an internal GNAT unit");
with System.BB.Parameters;
pragma Warnings (On, "* is an internal GNAT unit");

package body NXP.Device is

--   SYSCON : aliased SYSCON_Peripheral
--     with Import, Address => S_NS_Periph (SYSCON_Base);

   Secure_Code : UInt32;
   pragma Import (C, Secure_Code, "secure_code");

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : aliased in out GPIO_Port; Port : Ports)
   is
      Idx : Integer := Port'Enum_Rep;
   begin
      SYSCON.AHBCLKCTRL0.GPIO.Arr (Idx) := Enable;
   end Enable_Clock;

   procedure Disable_Clock (This : aliased in out GPIO_Port; Port : Ports)
   is
      Idx : Integer := Port'Enum_Rep;
   begin
      SYSCON.AHBCLKCTRL0.GPIO.Arr (Idx) := Disable;
   end Disable_Clock;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (Point : GPIO_Point)
   is
   begin
      Enable_Clock (Point.Periph.all, Point.Port);
   end Enable_Clock;

   procedure Disable_Clock (Point : GPIO_Point)
   is
   begin
      Disable_Clock (Point.Periph.all, Point.Port);
   end Disable_Clock;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (Points : GPIO_Points)
   is
   begin
      for Point of Points loop
         Enable_Clock (Point.Periph.all, Point.Port);
      end loop;
   end Enable_Clock;


   procedure Disable_Clock (Points : GPIO_Points)
   is
   begin
      for Point of Points loop
         Disable_Clock (Point.Periph.all, Point.Port);
      end loop;
   end Disable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : aliased in out GPIO_Port) is
   begin
      null;
   end Reset;

   -----------
   -- Reset --
   -----------

   procedure Reset (Point : GPIO_Point) is
   begin
      Reset (Point.Periph.all);
   end Reset;

   -----------
   -- Reset --
   -----------

   procedure Reset (Points : GPIO_Points)
   is
      Do_Reset : Boolean;
   begin
      for J in Points'Range loop
         Do_Reset := True;
         for K in Points'First .. J - 1 loop
            if Points (K).Periph = Points (J).Periph then
               Do_Reset := False;

               exit;
            end if;
         end loop;

         if Do_Reset then
            Reset (Points (J).Periph.all);
         end if;
      end loop;
   end Reset;

   function S_NS_Periph (Addr : System.Address) return System.Address
   is
      X : UInt32;
      LAddr : System.Address;
      for X'Address use LAddr'Address;
   begin
      LAddr := Addr;
      if Secure_Code > 0 then
         X := X + 16#1000_0000#;
      end if;
      return LAddr;
   end S_NS_Periph;

end NXP.Device;
