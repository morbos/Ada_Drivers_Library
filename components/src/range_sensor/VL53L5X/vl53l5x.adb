------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2017, AdaCore                           --
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
--     3. Neither the name of STMicroelectronics nor the names of its       --
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
--                                                                          --
--  This file is based on X-CUBE-53L0A1 STM32Cube expansion                 --
--                                                                          --
--   COPYRIGHT(c) 2016 STMicroelectronics                                   --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;

with HAL;        use HAL;

package body VL53L5X is

   ---------------
   -- I2C_Write --
   ---------------

   procedure I2C_Write
     (This   : VL53L5X_Ranging_Sensor;
      Data   : HAL.UInt8_Array;
      Status : out Boolean)
   is
      use type HAL.I2C.I2C_Status;
      Ret : HAL.I2C.I2C_Status;
   begin
      HAL.I2C.Master_Transmit
        (This    => This.Port.all,
         Addr    => This.I2C_Address,
         Data    => Data,
         Status  => Ret);
      Status := Ret = HAL.I2C.Ok;
   end I2C_Write;

   --------------
   -- I2C_Read --
   --------------

   procedure I2C_Read
     (This   : VL53L5X_Ranging_Sensor;
      Data   : out HAL.UInt8_Array;
      Status : out Boolean)
   is
      use type HAL.I2C.I2C_Status;
      Ret : HAL.I2C.I2C_Status;
   begin
      HAL.I2C.Master_Receive
        (This    => This.Port.all,
         Addr    => This.I2C_Address,
         Data    => Data,
         Status  => Ret);
      Status := Ret = HAL.I2C.Ok;
   end I2C_Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (This   : VL53L5X_Ranging_Sensor;
      Index  : HAL.UInt16;
      Data   : HAL.UInt8_Array;
      Status : out Boolean)
   is
      IndexHi : constant UInt8 := UInt8 (Shift_Right (Index, 8) and 16#ff#);
      IndexLo : constant UInt8 := UInt8 (Index and 16#ff#);
      Buf : constant HAL.UInt8_Array := (1 => IndexHi, 2 => IndexLo) & Data;
   begin
      I2C_Write (This, Buf, Status);
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write
     (This   : VL53L5X_Ranging_Sensor;
      Index  : HAL.UInt16;
      Data   : HAL.UInt8;
      Status : out Boolean)
   is
      IndexHi : constant UInt8 := UInt8 (Shift_Right (Index, 8) and 16#ff#);
      IndexLo : constant UInt8 := UInt8 (Index and 16#ff#);
   begin
      I2C_Write (This, (IndexHi, IndexLo, Data), Status);
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write
     (This   : VL53L5X_Ranging_Sensor;
      Index  : HAL.UInt16;
      Data   : HAL.UInt16;
      Status : out Boolean)
   is
      IndexHi : constant UInt8 := UInt8 (Shift_Right (Index, 8) and 16#ff#);
      IndexLo : constant UInt8 := UInt8 (Index and 16#ff#);
   begin
      I2C_Write
        (This,
         (IndexHi, IndexLo,
          HAL.UInt8 (Shift_Right (Data, 8)),
          HAL.UInt8 (Data and 16#FF#)),
         Status);
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write
     (This   : VL53L5X_Ranging_Sensor;
      Index  : HAL.UInt16;
      Data   : HAL.UInt32;
      Status : out Boolean)
   is
      IndexHi : constant UInt8 := UInt8 (Shift_Right (Index, 8) and 16#ff#);
      IndexLo : constant UInt8 := UInt8 (Index and 16#ff#);
   begin
      I2C_Write
        (This,
         (IndexHi, IndexLo,
          HAL.UInt8 (Shift_Right (Data, 24)),
          HAL.UInt8 (Shift_Right (Data, 16) and 16#FF#),
          HAL.UInt8 (Shift_Right (Data, 8) and 16#FF#),
          HAL.UInt8 (Data and 16#FF#)),
         Status);
   end Write;

   ----------
   -- Read --
   ----------

   procedure Read
     (This   : VL53L5X_Ranging_Sensor;
      Index  : HAL.UInt16;
      Data   : out HAL.UInt8_Array;
      Status : out Boolean)
   is
      IndexHi : constant UInt8 := UInt8 (Shift_Right (Index, 8) and 16#ff#);
      IndexLo : constant UInt8 := UInt8 (Index and 16#ff#);
   begin
      I2C_Write (This, (1 => IndexHi, 2 => IndexLo), Status);
      if Status then
         I2C_Read (This, Data, Status);
      end if;
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read
     (This   : VL53L5X_Ranging_Sensor;
      Index  : HAL.UInt16;
      Data   : out HAL.UInt8;
      Status : out Boolean)
   is
      IndexHi : constant UInt8 := UInt8 (Shift_Right (Index, 8) and 16#ff#);
      IndexLo : constant UInt8 := UInt8 (Index and 16#ff#);
      Buf : UInt8_Array (1 .. 1);
   begin
      I2C_Write (This, (1 => IndexHi, 2 => IndexLo), Status);
      if Status then
         I2C_Read (This, Buf, Status);
         Data := Buf (1);
      end if;
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read
     (This   : VL53L5X_Ranging_Sensor;
      Index  : HAL.UInt16;
      Data   : out HAL.UInt16;
      Status : out Boolean)
   is
      IndexHi : constant UInt8 := UInt8 (Shift_Right (Index, 8) and 16#ff#);
      IndexLo : constant UInt8 := UInt8 (Index and 16#ff#);
      Buf : UInt8_Array (1 .. 2);
   begin
      I2C_Write (This, (1 => IndexHi, 2 => IndexLo), Status);
      if Status then
         I2C_Read (This, Buf, Status);
         Data := Shift_Left (UInt16 (Buf (1)), 8) or UInt16 (Buf (2));
      end if;
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read
     (This   : VL53L5X_Ranging_Sensor;
      Index  : HAL.UInt16;
      Data   : out HAL.UInt32;
      Status : out Boolean)
   is
      IndexHi : constant UInt8 := UInt8 (Shift_Right (Index, 8) and 16#ff#);
      IndexLo : constant UInt8 := UInt8 (Index and 16#ff#);
      Buf : UInt8_Array (1 .. 4);
   begin
      I2C_Write (This, (1 => IndexHi, 2 => IndexLo), Status);
      if Status then
         I2C_Read (This, Buf, Status);
         Data := Shift_Left (UInt32 (Buf (1)), 24) or
           Shift_Left (UInt32 (Buf (2)), 16) or
           Shift_Left (UInt32 (Buf (3)), 8) or
           UInt32 (Buf (4));
      end if;
   end Read;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This : in out VL53L5X_Ranging_Sensor)
   is
   begin
      This.I2C_Address := 16#52#;
   end Initialize;

   -------------
   -- Read_Id --
   -------------

   function Read_Id (This : VL53L5X_Ranging_Sensor) return HAL.UInt16
   is
--      Ret    : UInt16;
--      Status : Boolean;
   begin
--      Read (This, REG_IDENTIFICATION_MODEL_ID, Ret, Status);

--      if not Status then
         return 0;
--      else
--         return Ret;
--      end if;
   end Read_Id;

end VL53L5X;
