with NXP.GPIO;       use NXP.GPIO;
with NXP_SVD;        use NXP_SVD;
with NXP_SVD.SYSCON; use NXP_SVD.SYSCON;
with NXP_SVD.IOCON;  use NXP_SVD.IOCON;
with System;

package NXP.Device is
   pragma Elaborate_Body;

   Unknown_Port : exception;

   procedure Enable_Clock (This : aliased in out GPIO_Port; Port : Ports);
   procedure Enable_Clock (Point : GPIO_Point);
   procedure Enable_Clock (Points : GPIO_Points);

   procedure Disable_Clock (This : aliased in out GPIO_Port; Port : Ports);
   procedure Disable_Clock (Point : GPIO_Point);
   procedure Disable_Clock (Points : GPIO_Points);

   procedure Reset (This : aliased in out GPIO_Port)
     with Inline;
   procedure Reset (Point : GPIO_Point)
     with Inline;
   procedure Reset (Points : GPIO_Points)
     with Inline;

   function S_NS_Periph (Addr : System.Address) return System.Address
     with Inline;

   GPIO : aliased GPIO_Port with Import, Volatile, Address => S_NS_Periph (GPIO_Base);
   SECGPIO : aliased GPIO_Port with Import, Volatile, Address => S_NS_Periph (SECGPIO_Base);
   SYSCON : aliased SYSCON_Peripheral with Import, Address => S_NS_Periph (SYSCON_Base);

   P0_0  : aliased GPIO_Point := (GPIO'Access, Port_0, Pin_0);
   P0_1  : aliased GPIO_Point := (GPIO'Access, Port_0, Pin_1);
   P0_2  : aliased GPIO_Point := (GPIO'Access, Port_0, Pin_2);
   P0_3  : aliased GPIO_Point := (GPIO'Access, Port_0, Pin_3);
   P0_4  : aliased GPIO_Point := (GPIO'Access, Port_0, Pin_4);
   P0_5  : aliased GPIO_Point := (GPIO'Access, Port_0, Pin_5);
   P0_6  : aliased GPIO_Point := (GPIO'Access, Port_0, Pin_6);
   P0_7  : aliased GPIO_Point := (GPIO'Access, Port_0, Pin_7);
   P0_8  : aliased GPIO_Point := (GPIO'Access, Port_0, Pin_8);
   P0_9  : aliased GPIO_Point := (GPIO'Access, Port_0, Pin_9);
   P0_10  : aliased GPIO_Point := (GPIO'Access, Port_0, Pin_10);
   P0_11  : aliased GPIO_Point := (GPIO'Access, Port_0, Pin_11);
   P0_12  : aliased GPIO_Point := (GPIO'Access, Port_0, Pin_12);
   P0_13  : aliased GPIO_Point := (GPIO'Access, Port_0, Pin_13);
   P0_14  : aliased GPIO_Point := (GPIO'Access, Port_0, Pin_14);
   P0_15  : aliased GPIO_Point := (GPIO'Access, Port_0, Pin_15);
   P0_16  : aliased GPIO_Point := (GPIO'Access, Port_0, Pin_16);
   P0_17  : aliased GPIO_Point := (GPIO'Access, Port_0, Pin_17);
   P0_18  : aliased GPIO_Point := (GPIO'Access, Port_0, Pin_18);
   P0_19  : aliased GPIO_Point := (GPIO'Access, Port_0, Pin_19);
   P0_20  : aliased GPIO_Point := (GPIO'Access, Port_0, Pin_20);
   P0_21  : aliased GPIO_Point := (GPIO'Access, Port_0, Pin_21);
   P0_22  : aliased GPIO_Point := (GPIO'Access, Port_0, Pin_22);
   P0_23  : aliased GPIO_Point := (GPIO'Access, Port_0, Pin_23);
   P0_24  : aliased GPIO_Point := (GPIO'Access, Port_0, Pin_24);
   P0_25  : aliased GPIO_Point := (GPIO'Access, Port_0, Pin_25);
   P0_26  : aliased GPIO_Point := (GPIO'Access, Port_0, Pin_26);
   P0_27  : aliased GPIO_Point := (GPIO'Access, Port_0, Pin_27);
   P0_28  : aliased GPIO_Point := (GPIO'Access, Port_0, Pin_28);
   P0_29  : aliased GPIO_Point := (GPIO'Access, Port_0, Pin_29);
   P0_30  : aliased GPIO_Point := (GPIO'Access, Port_0, Pin_30);
   P0_31  : aliased GPIO_Point := (GPIO'Access, Port_0, Pin_31);

   P1_0  : aliased GPIO_Point := (GPIO'Access, Port_1, Pin_0);
   P1_1  : aliased GPIO_Point := (GPIO'Access, Port_1, Pin_1);
   P1_2  : aliased GPIO_Point := (GPIO'Access, Port_1, Pin_2);
   P1_3  : aliased GPIO_Point := (GPIO'Access, Port_1, Pin_3);
   P1_4  : aliased GPIO_Point := (GPIO'Access, Port_1, Pin_4);
   P1_5  : aliased GPIO_Point := (GPIO'Access, Port_1, Pin_5);
   P1_6  : aliased GPIO_Point := (GPIO'Access, Port_1, Pin_6);
   P1_7  : aliased GPIO_Point := (GPIO'Access, Port_1, Pin_7);
   P1_8  : aliased GPIO_Point := (GPIO'Access, Port_1, Pin_8);
   P1_9  : aliased GPIO_Point := (GPIO'Access, Port_1, Pin_9);
   P1_10  : aliased GPIO_Point := (GPIO'Access, Port_1, Pin_10);
   P1_11  : aliased GPIO_Point := (GPIO'Access, Port_1, Pin_11);
   P1_12  : aliased GPIO_Point := (GPIO'Access, Port_1, Pin_12);
   P1_13  : aliased GPIO_Point := (GPIO'Access, Port_1, Pin_13);
   P1_14  : aliased GPIO_Point := (GPIO'Access, Port_1, Pin_14);
   P1_15  : aliased GPIO_Point := (GPIO'Access, Port_1, Pin_15);
   P1_16  : aliased GPIO_Point := (GPIO'Access, Port_1, Pin_16);
   P1_17  : aliased GPIO_Point := (GPIO'Access, Port_1, Pin_17);
   P1_18  : aliased GPIO_Point := (GPIO'Access, Port_1, Pin_18);
   P1_19  : aliased GPIO_Point := (GPIO'Access, Port_1, Pin_19);
   P1_20  : aliased GPIO_Point := (GPIO'Access, Port_1, Pin_20);
   P1_21  : aliased GPIO_Point := (GPIO'Access, Port_1, Pin_21);
   P1_22  : aliased GPIO_Point := (GPIO'Access, Port_1, Pin_22);
   P1_23  : aliased GPIO_Point := (GPIO'Access, Port_1, Pin_23);
   P1_24  : aliased GPIO_Point := (GPIO'Access, Port_1, Pin_24);
   P1_25  : aliased GPIO_Point := (GPIO'Access, Port_1, Pin_25);
   P1_26  : aliased GPIO_Point := (GPIO'Access, Port_1, Pin_26);
   P1_27  : aliased GPIO_Point := (GPIO'Access, Port_1, Pin_27);
   P1_28  : aliased GPIO_Point := (GPIO'Access, Port_1, Pin_28);
   P1_29  : aliased GPIO_Point := (GPIO'Access, Port_1, Pin_29);
   P1_30  : aliased GPIO_Point := (GPIO'Access, Port_1, Pin_30);
   P1_31  : aliased GPIO_Point := (GPIO'Access, Port_1, Pin_31);

   GPIO_AF_FC0_USART   : constant GPIO_Alternate_Function;

private

   GPIO_AF_FC0_USART   : constant GPIO_Alternate_Function := 1;

end NXP.Device;
