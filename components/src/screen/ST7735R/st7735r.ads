------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2017, AdaCore                     --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

--  This a buffer-less driver for the ST7735R LCD. No pixels are stored in RAM
--  which means low memory consumption but also slow operations.
--
--  Please use ST7735R.RAM_Framebuffer for a faster implementation.

with HAL;                  use HAL;
with HAL.SPI;              use HAL.SPI;
with HAL.GPIO;             use HAL.GPIO;
with HAL.Framebuffer;      use HAL.Framebuffer;
with HAL.Bitmap;           use HAL.Bitmap;
with HAL.Time;
with Soft_Drawing_Bitmap;  use Soft_Drawing_Bitmap;
with System;

package ST7735R is

   type ST7735R_Screen
     (Port : not null Any_SPI_Port;
      CS   : not null Any_GPIO_Point;
      RS   : not null Any_GPIO_Point;
      RST  : not null Any_GPIO_Point;
      Time : not null HAL.Time.Any_Delays)
   is limited new HAL.Framebuffer.Frame_Buffer_Display with private;

   type Any_ST7735R_Device is access all ST7735R_Screen'Class;

   procedure Initialize (LCD : in out ST7735R_Screen);

   overriding
   function Initialized (LCD : ST7735R_Screen) return Boolean;

   procedure Turn_On (LCD : ST7735R_Screen);
   procedure Turn_Off (LCD : ST7735R_Screen);
   procedure Display_Inversion_On (LCD : ST7735R_Screen);
   procedure Display_Inversion_Off (LCD : ST7735R_Screen);
   procedure Gamma_Set (LCD : ST7735R_Screen; Gamma_Curve : UInt4);

   type Pixel_Format is (Pixel_12bits, Pixel_16bits, Pixel_18bits);

   procedure Set_Pixel_Format (LCD : ST7735R_Screen; Pix_Fmt : Pixel_Format);

   type RGB_BGR_Order is (RGB_Order, BGR_Order)
     with Size => 1;
   type Vertical_Refresh_Order is (Vertical_Refresh_Top_Bottom,
                                   Vertical_Refresh_Botton_Top)
     with Size => 1;
   type Horizontal_Refresh_Order is (Horizontal_Refresh_Left_Right,
                                     Horizontal_Refresh_Right_Left)
     with Size => 1;
   type Row_Address_Order is (Row_Address_Top_Bottom,
                              Row_Address_Bottom_Top)
     with Size => 1;
   type Column_Address_Order is (Column_Address_Left_Right,
                                 Column_Address_Right_Left)
     with Size => 1;

   procedure Set_Memory_Data_Access
     (LCD                 : ST7735R_Screen;
      Color_Order         : RGB_BGR_Order;
      Vertical            : Vertical_Refresh_Order;
      Horizontal          : Horizontal_Refresh_Order;
      Row_Addr_Order      : Row_Address_Order;
      Column_Addr_Order   : Column_Address_Order;
      Row_Column_Exchange : Boolean);

   procedure Set_Frame_Rate_Normal
     (LCD         : ST7735R_Screen;
      RTN         : UInt4;
      Front_Porch : UInt6;
      Back_Porch  : UInt6);

   procedure Set_Frame_Rate_Idle
     (LCD         : ST7735R_Screen;
      RTN         : UInt4;
      Front_Porch : UInt6;
      Back_Porch  : UInt6);

   procedure Set_Frame_Rate_Partial_Full
     (LCD              : ST7735R_Screen;
      RTN_Part         : UInt4;
      Front_Porch_Part : UInt6;
      Back_Porch_Part  : UInt6;
      RTN_Full         : UInt4;
      Front_Porch_Full : UInt6;
      Back_Porch_Full  : UInt6);

   type Inversion_Control is (Dot_Inversion, Line_Inversion);

   procedure Set_Inversion_Control
     (LCD : ST7735R_Screen;
      Normal, Idle, Full_Partial : Inversion_Control);

   procedure Set_Power_Control_1
     (LCD  : ST7735R_Screen;
      AVDD : UInt3;
      VRHP : UInt5;
      VRHN : UInt5;
      MODE : UInt2);

   procedure Set_Power_Control_2
     (LCD   : ST7735R_Screen;
      VGH25 : UInt2;
      VGSEL : UInt2;
      VGHBT : UInt2);

   procedure Set_Power_Control_3
     (LCD : ST7735R_Screen;
      P1, P2 : UInt8);

   procedure Set_Power_Control_4
     (LCD : ST7735R_Screen;
      P1, P2 : UInt8);

   procedure Set_Power_Control_5
     (LCD : ST7735R_Screen;
      P1, P2 : UInt8);

   procedure Set_Vcom (LCD : ST7735R_Screen; VCOMS : UInt6);

   procedure Set_Column_Address (LCD : ST7735R_Screen; X_Start, X_End : UInt16);
   procedure Set_Row_Address (LCD : ST7735R_Screen; Y_Start, Y_End : UInt16);
   procedure Set_Address (LCD : ST7735R_Screen;
                          X_Start, X_End, Y_Start, Y_End : UInt16);

   procedure Set_Pixel (LCD   : ST7735R_Screen;
                        X, Y  : UInt16;
                        Color : UInt16);

   function Pixel (LCD   : ST7735R_Screen;
                   X, Y  : UInt16)
                   return UInt16;

   procedure Write_Raw_Pixels (LCD  : ST7735R_Screen;
                               Data : HAL.UInt8_Array);
   procedure Write_Raw_Pixels (LCD  : ST7735R_Screen;
                               Data : HAL.UInt16_Array);

   procedure Write_Value (LCD  : ST7735R_Screen;
                          Data : UInt16;
                          Total : UInt16);

   overriding
   function Max_Layers
     (Display : ST7735R_Screen) return Positive;

   overriding
   function Supported
     (Display : ST7735R_Screen;
      Mode    : FB_Color_Mode) return Boolean;

   overriding
   procedure Set_Orientation
     (Display     : in out ST7735R_Screen;
      Orientation : Display_Orientation);

   overriding
   procedure Set_Mode
     (Display : in out ST7735R_Screen;
      Mode    : Wait_Mode);

   overriding
   function Width
     (Display : ST7735R_Screen) return Positive;

   overriding
   function Height
     (Display : ST7735R_Screen) return Positive;

   overriding
   function Swapped
     (Display : ST7735R_Screen) return Boolean;
   --  Whether X/Y coordinates are considered Swapped by the drawing primitives
   --  This simulates Landscape/Portrait orientation on displays not supporting
   --  hardware orientation change

   overriding
   procedure Set_Background
     (Display : ST7735R_Screen; R, G, B : UInt8);

   overriding
   procedure Initialize_Layer
     (Display : in out ST7735R_Screen;
      Layer   : Positive;
      Mode    : FB_Color_Mode;
      X       : Natural := 0;
      Y       : Natural := 0;
      Width   : Positive := Positive'Last;
      Height  : Positive := Positive'Last);
   --  All layers are double buffered, so an explicit call to Update_Layer
   --  needs to be performed to actually display the current buffer attached
   --  to the layer.
   --  Alloc is called to create the actual buffer.

   overriding
   function Initialized
     (Display : ST7735R_Screen;
      Layer   : Positive) return Boolean;

   overriding
   procedure Update_Layer
     (Display   : in out ST7735R_Screen;
      Layer     : Positive;
      Copy_Back : Boolean := False);
   --  Updates the layer so that the hidden buffer is displayed.

   overriding
   procedure Update_Layers
     (Display : in out ST7735R_Screen);
   --  Updates all initialized layers at once with their respective hidden
   --  buffer

   overriding
   function Color_Mode
     (Display : ST7735R_Screen;
      Layer   : Positive) return FB_Color_Mode;
   --  Retrieves the current color mode for the layer.

   overriding
   function Hidden_Buffer
     (Display : in out ST7735R_Screen;
      Layer   : Positive) return not null HAL.Bitmap.Any_Bitmap_Buffer;
   --  Retrieves the current hidden buffer for the layer.

   overriding
   function Pixel_Size
     (Display : ST7735R_Screen;
      Layer   : Positive) return Positive;
   --  Retrieves the current hidden buffer for the layer.

   --  some colour choices
   ST7735_BLACK          : constant UInt16 := 16#0000#;  --    0,   0,   0
   ST7735_NAVY           : constant UInt16 := 16#000F#;  --    0,   0, 128
   ST7735_DARKGREEN      : constant UInt16 := 16#03E0#;  --    0, 128,   0
   ST7735_DARKCYAN       : constant UInt16 := 16#03EF#;  --    0, 128, 128
   ST7735_MAROON         : constant UInt16 := 16#7800#;  --  128,   0,   0
   ST7735_PURPLE         : constant UInt16 := 16#780F#;  --  128,   0, 128
   ST7735_OLIVE          : constant UInt16 := 16#7BE0#;  --  128, 128,   0
   ST7735_LIGHTGREY      : constant UInt16 := 16#C618#;  --  192, 192, 192
   ST7735_DARKGREY       : constant UInt16 := 16#7BEF#;  --  128, 128, 128
   ST7735_BLUE           : constant UInt16 := 16#001F#;  --    0,   0, 255
   ST7735_GREEN          : constant UInt16 := 16#07E0#;  --    0, 255,   0
   ST7735_CYAN           : constant UInt16 := 16#07FF#;  --    0, 255, 255
   ST7735_RED            : constant UInt16 := 16#F800#;  --  255,   0,   0
   ST7735_MAGENTA        : constant UInt16 := 16#F81F#;  --  255,   0, 255
   ST7735_YELLOW         : constant UInt16 := 16#FFE0#;  --  255, 255,   0
   ST7735_WHITE          : constant UInt16 := 16#FFFF#;  --  255, 255, 255
   ST7735_ORANGE         : constant UInt16 := 16#FD20#;  --  255, 165,   0
   ST7735_GREENYELLOW    : constant UInt16 := 16#AFE5#;  --  173, 255,  47
   ST7735_PINK           : constant UInt16 := 16#F81F#;

private

   Screen_Width  : constant := 128;
   Screen_Height : constant := 160;

   for RGB_BGR_Order use
     (RGB_Order => 0,
      BGR_Order => 1);

   for Vertical_Refresh_Order use
     (Vertical_Refresh_Top_Bottom => 0,
      Vertical_Refresh_Botton_Top => 1);

   for Horizontal_Refresh_Order use
     (Horizontal_Refresh_Left_Right => 0,
      Horizontal_Refresh_Right_Left => 1);

   for Row_Address_Order use
     (Row_Address_Top_Bottom => 0,
      Row_Address_Bottom_Top => 1);

   for Column_Address_Order use
     (Column_Address_Left_Right => 0,
      Column_Address_Right_Left => 1);

   type ST7735R_Bitmap_Buffer is new Soft_Drawing_Bitmap_Buffer with record
      LCD           : Any_ST7735R_Device := null;
      Native_Source : UInt32;
   end record;

   overriding
   function Width (Buffer : ST7735R_Bitmap_Buffer) return Natural is
     (Screen_Width);

   overriding
   function Height (Buffer : ST7735R_Bitmap_Buffer) return Natural is
     (Screen_Height);

   overriding
   function Swapped (Buffer : ST7735R_Bitmap_Buffer) return Boolean is
     (False);

   overriding
   function Color_Mode (Buffer : ST7735R_Bitmap_Buffer) return Bitmap_Color_Mode is
     (RGB_565);

   overriding
   function Mapped_In_RAM (Buffer : ST7735R_Bitmap_Buffer) return Boolean is
      (False);

   overriding
   function Memory_Address (Buffer : ST7735R_Bitmap_Buffer) return System.Address is
     (System.Null_Address);

   overriding
   procedure Set_Source (Buffer : in out ST7735R_Bitmap_Buffer;
                         Native : UInt32);

   overriding
   function Source
     (Buffer : ST7735R_Bitmap_Buffer)
      return UInt32;

   overriding
   procedure Set_Pixel
     (Buffer  : in out ST7735R_Bitmap_Buffer;
      Pt      : Point)
     with Pre => Buffer.LCD /= null;

   overriding
   procedure Set_Pixel_Blend
     (Buffer : in out ST7735R_Bitmap_Buffer;
      Pt     : Point)
     with Pre => Buffer.LCD /= null;

   overriding
   function Pixel
     (Buffer : ST7735R_Bitmap_Buffer;
      Pt     : Point)
      return UInt32
     with Pre => Buffer.LCD /= null;

   overriding
   function Buffer_Size (Buffer : ST7735R_Bitmap_Buffer) return Natural is
      (0);

   type ST7735R_Screen
     (Port : not null Any_SPI_Port;
      CS   : not null Any_GPIO_Point;
      RS   : not null Any_GPIO_Point;
      RST  : not null Any_GPIO_Point;
      Time : not null HAL.Time.Any_Delays)
   is limited new HAL.Framebuffer.Frame_Buffer_Display with record
      Initialized : Boolean := True;
      Layer       : aliased ST7735R_Bitmap_Buffer;
   end record;

   ST7735_NOP                     : constant := 16#00#;
   ST7735_SWRESET                 : constant := 16#01#;
   ST7735_RDDID                   : constant := 16#04#;
   ST7735_RDDST                   : constant := 16#09#;
   ST7735_SLPIN                   : constant := 16#10#;
   ST7735_SLPOUT                  : constant := 16#11#;
   ST7735_PTLON                   : constant := 16#12#;
   ST7735_NORON                   : constant := 16#13#;

   ST7735_INVOFF                  : constant := 16#20#;
   ST7735_INVON                   : constant := 16#21#;
   ST7735_GAMSET                  : constant := 16#26#;
   ST7735_DISPOFF                 : constant := 16#28#;
   ST7735_DISPON                  : constant := 16#29#;
   ST7735_CASET                   : constant := 16#2A#;
   ST7735_RASET                   : constant := 16#2B#;
   ST7735_RAMWR                   : constant := 16#2C#;
   ST7735_RAMRD                   : constant := 16#2E#;

   ST7735_PTLAR                   : constant := 16#30#;
   ST7735_COLMOD                  : constant := 16#3A#;
   ST7735_MADCTL                  : constant := 16#36#;

   ST7735_FRMCTR1                 : constant := 16#B1#;
   ST7735_FRMCTR2                 : constant := 16#B2#;
   ST7735_FRMCTR3                 : constant := 16#B3#;
   ST7735_INVCTR                  : constant := 16#B4#;
   ST7735_DISSET5                 : constant := 16#B6#;

   ST7735_PWCTR1                  : constant := 16#C0#;
   ST7735_PWCTR2                  : constant := 16#C1#;
   ST7735_PWCTR3                  : constant := 16#C2#;
   ST7735_PWCTR4                  : constant := 16#C3#;
   ST7735_PWCTR5                  : constant := 16#C4#;
   ST7735_VMCTR1                  : constant := 16#C5#;

   ST7735_RDID1                   : constant := 16#DA#;
   ST7735_RDID2                   : constant := 16#DB#;
   ST7735_RDID3                   : constant := 16#DC#;
   ST7735_RDID4                   : constant := 16#DD#;

   ST7735_PWCTR6                  : constant := 16#FC#;

   ST7735_GMCTRP1                 : constant := 16#E0#;
   ST7735_GMCTRN1                 : constant := 16#E1#;

end ST7735R;
