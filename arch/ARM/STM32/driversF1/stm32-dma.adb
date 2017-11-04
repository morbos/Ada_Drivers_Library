-----------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2015, AdaCore                           --
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
--  This file is based on:                                                  --
--                                                                          --
--   @file    stm32f4xx_hal_dma.c                                           --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   DMA HAL module driver.                                        --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
--                                                                          --
--   Modified for the STM32F10x series. That series has only channel, no    --
--   streams                                                                --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with System.Storage_Elements;

with STM32_SVD.DMA; use STM32_SVD.DMA;

package body STM32.DMA is

   function To_Bit is new Ada.Unchecked_Conversion (Boolean, Bit);

   type CCR_Ptr is access all CCR_Register;

   function Get_Channel
     (Port : DMA_Controller;
      Num  : DMA_Channel_Selector) return CCR_Ptr;

--   procedure Set_Interrupt_Enabler
--     (This_Channel : DMA_Channel;
--      Source       : DMA_Interrupt;
--      Value        : Boolean);
   --  An internal routine, used to enable and disable the specified interrupt

   ----------------
   -- Get_Channel --
   ----------------

   function Get_Channel
     (Port : DMA_Controller;
      Num  : DMA_Channel_Selector) return CCR_Ptr
   is
      Addr : System.Address;
      function To_CCR is new Ada.Unchecked_Conversion
        (System.Address, CCR_Ptr);
   begin
      case Num is
         when Channel_1 =>
            Addr := Port.CCR1'Address;
         when Channel_2 =>
            Addr := Port.CCR2'Address;
         when Channel_3 =>
            Addr := Port.CCR3'Address;
         when Channel_4 =>
            Addr := Port.CCR4'Address;
         when Channel_5 =>
            Addr := Port.CCR5'Address;
         when Channel_6 =>
            Addr := Port.CCR6'Address;
         when Channel_7 =>
            Addr := Port.CCR7'Address;
      end case;

      return To_CCR (Addr);
   end Get_Channel;


   ------------
   -- Enable --
   ------------

   procedure Enable
     (This    : in out DMA_Controller;
      Channel : DMA_Channel_Selector)
   is
   begin
      Get_Channel (This, Channel).EN := True;
   end Enable;

   -------------
   -- Enabled --
   -------------

   function Enabled
     (This    : DMA_Controller;
      Channel : DMA_Channel_Selector)
      return Boolean
   is
   begin
      return Get_Channel (This, Channel).EN;
   end Enabled;

   -------------
   -- Disable --
   -------------

   procedure Disable
     (This    : in out DMA_Controller;
      Channel : DMA_Channel_Selector)
   is
   begin
      Get_Channel (This, Channel).EN := False;
      loop
         exit when not Enabled (This, Channel);
      end loop;
   end Disable;

   ----------------------
   -- Enable_Interrupt --
   ----------------------

   procedure Enable_Interrupt
     (This    : DMA_Controller;
      Channel : DMA_Channel_Selector;
      Source  : DMA_Interrupt)
   is
      Ch :  CCR_Ptr := Get_Channel (This, Channel);
   begin
      case Source is
         when Transfer_Error_Interrupt =>
            Ch.TEIE := True;
         when Half_Transfer_Complete_Interrupt =>
            Ch.HTIE := True;
         when Transfer_Complete_Interrupt =>
            Ch.TCIE := True;
      end case;
   end Enable_Interrupt;

   -----------------------
   -- Disable_Interrupt --
   -----------------------

   procedure Disable_Interrupt
     (This    : DMA_Controller;
      Channel : DMA_Channel_Selector;
      Source  : DMA_Interrupt)
   is
      Ch :  CCR_Ptr := Get_Channel (This, Channel);
   begin
      case Source is
         when Transfer_Error_Interrupt =>
            Ch.TEIE := False;
         when Half_Transfer_Complete_Interrupt =>
            Ch.HTIE := False;
         when Transfer_Complete_Interrupt =>
            Ch.TCIE := False;
      end case;
   end Disable_Interrupt;

   -----------------------
   -- Interrupt_Enabled --
   -----------------------

   function Interrupt_Enabled
     (This        : DMA_Controller;
      Channel      : DMA_Channel_Selector;
      Source      : DMA_Interrupt)
      return Boolean
   is
      Ch :  CCR_Ptr := Get_Channel (This, Channel);
      Result : Boolean := False;
   begin
      case Source is
         when Transfer_Error_Interrupt =>
            Result := Ch.TEIE;
         when Half_Transfer_Complete_Interrupt =>
            Result := Ch.HTIE;
         when Transfer_Complete_Interrupt =>
            Result := Ch.TCIE;
      end case;
      return Result;
   end Interrupt_Enabled;

   --------------------
   -- Start_Transfer --
   --------------------

   procedure Start_Transfer
     (This        : in out DMA_Controller;
      Channel     : DMA_Channel_Selector;
      Source      : Address;
      Destination : Address;
      Data_Count  : UInt16)
   is
   begin
      Disable (This, Channel);

      Configure_Data_Flow
        (This,
         Channel,
         Source      => Source,
         Destination => Destination,
         Data_Count => Data_Count);

      Enable (This, Channel);
   end Start_Transfer;

   ------------------------------------
   -- Start_Transfer_with_Interrupts --
   ------------------------------------

   procedure Start_Transfer_with_Interrupts
     (This               : in out DMA_Controller;
      Channel            : DMA_Channel_Selector;
      Source             : Address;
      Destination        : Address;
      Data_Count         : UInt16;
      Enabled_Interrupts : Interrupt_Selections := (others => True))
   is
   begin
      Disable (This, Channel);

      Configure_Data_Flow
        (This,
         Channel,
         Source      => Source,
         Destination => Destination,
         Data_Count => Data_Count);

      for Selected_Interrupt in Enabled_Interrupts'Range loop
         if Enabled_Interrupts (Selected_Interrupt) then
            Enable_Interrupt (This, Channel, Selected_Interrupt);
         end if;
      end loop;

      Enable (This, Channel);
   end Start_Transfer_with_Interrupts;

   -------------------------
   -- Configure_Data_Flow --
   -------------------------

   procedure Configure_Data_Flow
     (This        : in out DMA_Controller;
      Channel     : DMA_Channel_Selector;
      Source      : Address;
      Destination : Address;
      Data_Count  : UInt16)
   is
      function W is new Ada.Unchecked_Conversion
        (Address, UInt32);
      Addr1       : Address;
      Addr2       : Address;
   begin
      if Get_Channel (This, Channel).DIR then
         Addr1 := Source;
         Addr2 := Destination;
      else
         Addr1 := Destination;
         Addr2 := Source;
      end if;
      case Channel is
         when Channel_1 =>
            This.CNDTR1.NDT := Data_Count;
            This.CMAR1 := W (Addr1);
            This.CPAR1 := W (Addr2);
         when Channel_2 =>
            This.CNDTR2.NDT := Data_Count;
            This.CMAR2 := W (Addr1);
            This.CPAR2 := W (Addr2);
         when Channel_3 =>
            This.CNDTR3.NDT := Data_Count;
            This.CMAR3 := W (Addr1);
            This.CPAR3 := W (Addr2);
         when Channel_4 =>
            This.CNDTR4.NDT := Data_Count;
            This.CMAR4 := W (Addr1);
            This.CPAR4 := W (Addr2);
         when Channel_5 =>
            This.CNDTR5.NDT := Data_Count;
            This.CMAR5 := W (Addr1);
            This.CPAR5 := W (Addr2);
         when Channel_6 =>
            This.CNDTR6.NDT := Data_Count;
            This.CMAR6 := W (Addr1);
            This.CPAR6 := W (Addr2);
         when Channel_7 =>
            This.CNDTR7.NDT := Data_Count;
            This.CMAR7 := W (Addr1);
            This.CPAR7 := W (Addr2);
      end case;
   end Configure_Data_Flow;

   --------------------
   -- Abort_Transfer --
   --------------------

   procedure Abort_Transfer
     (This   : DMA_Controller;
      Channel : DMA_Channel_Selector;
      Result : out DMA_Error_Code)
   is
   begin
      Result := DMA_No_Error;
   end Abort_Transfer;

   -------------------------
   -- Poll_For_Completion --
   -------------------------

   procedure Poll_For_Completion
     (This           : in out DMA_Controller;
      Channel        : DMA_Channel_Selector;
      Expected_Level : DMA_Transfer_Level;
      Timeout        : Time_Span;
      Result         : out DMA_Error_Code)
   is
   begin
      Result := DMA_No_Error;  -- initially anyway
   end Poll_For_Completion;

   ------------------
   -- Clear_Status --
   ------------------

   procedure Clear_Status
     (This    : in out DMA_Controller;
      Channel : DMA_Channel_Selector;
      Flag    : DMA_Status_Flag)
   is
   begin
      case Flag is
         when Transfer_Complete_Indicated =>
            case Channel is
               when Channel_1 =>
                  This.IFCR.CTCIF1 := True;
               when Channel_2 =>
                  This.IFCR.CTCIF2 := True;
               when Channel_3 =>
                  This.IFCR.CTCIF3 := True;
               when Channel_4 =>
                  This.IFCR.CTCIF4 := True;
               when Channel_5 =>
                  This.IFCR.CTCIF5 := True;
               when Channel_6 =>
                  This.IFCR.CTCIF6 := True;
               when Channel_7 =>
                  This.IFCR.CTCIF7 := True;
            end case;
         when Transfer_Error_Indicated  =>
            case Channel is
               when Channel_1 =>
                  This.IFCR.CTEIF1 := True;
               when Channel_2 =>
                  This.IFCR.CTEIF2 := True;
               when Channel_3 =>
                  This.IFCR.CTEIF3 := True;
               when Channel_4 =>
                  This.IFCR.CTEIF4 := True;
               when Channel_5 =>
                  This.IFCR.CTEIF5 := True;
               when Channel_6 =>
                  This.IFCR.CTEIF6 := True;
               when Channel_7 =>
                  This.IFCR.CTEIF7 := True;
            end case;
         when Half_Transfer_Complete_Indicated =>
            case Channel is
               when Channel_1 =>
                  This.IFCR.CHTIF1 := True;
               when Channel_2 =>
                  This.IFCR.CHTIF2 := True;
               when Channel_3 =>
                  This.IFCR.CHTIF3 := True;
               when Channel_4 =>
                  This.IFCR.CHTIF4 := True;
               when Channel_5 =>
                  This.IFCR.CHTIF5 := True;
               when Channel_6 =>
                  This.IFCR.CHTIF6 := True;
               when Channel_7 =>
                  This.IFCR.CHTIF7 := True;
            end case;
      end case;
   end Clear_Status;

   ----------------------
   -- Clear_All_Status --
   ----------------------

   procedure Clear_All_Status
     (This   : in out DMA_Controller;
      Channel : DMA_Channel_Selector)
   is
   begin
      for Flag in DMA_Status_Flag'Range loop
         Clear_Status (This, Channel, Flag);
      end loop;
   end Clear_All_Status;

   ------------
   -- Status --
   ------------

   function Status
     (This    : DMA_Controller;
      Channel  : DMA_Channel_Selector;
      Flag    : DMA_Status_Flag)
      return Boolean
   is
      Result : Boolean := False;
   begin
      case Flag is
         when Transfer_Complete_Indicated =>
            case Channel is
               when Channel_1 =>
                  Result := This.ISR.TCIF1;
               when Channel_2 =>
                  Result := This.ISR.TCIF2;
               when Channel_3 =>
                  Result := This.ISR.TCIF3;
               when Channel_4 =>
                  Result := This.ISR.TCIF4;
               when Channel_5 =>
                  Result := This.ISR.TCIF5;
               when Channel_6 =>
                  Result := This.ISR.TCIF6;
               when Channel_7 =>
                  Result := This.ISR.TCIF7;
            end case;
         when Transfer_Error_Indicated  =>
            case Channel is
               when Channel_1 =>
                  Result := This.ISR.TEIF1;
               when Channel_2 =>
                  Result := This.ISR.TEIF2;
               when Channel_3 =>
                  Result := This.ISR.TEIF3;
               when Channel_4 =>
                  Result := This.ISR.TEIF4;
               when Channel_5 =>
                  Result := This.ISR.TEIF5;
               when Channel_6 =>
                  Result := This.ISR.TEIF6;
               when Channel_7 =>
                  Result := This.ISR.TEIF7;
            end case;
         when Half_Transfer_Complete_Indicated =>
            case Channel is
               when Channel_1 =>
                  Result := This.ISR.HTIF1;
               when Channel_2 =>
                  Result := This.ISR.HTIF2;
               when Channel_3 =>
                  Result := This.ISR.HTIF3;
               when Channel_4 =>
                  Result := This.ISR.HTIF4;
               when Channel_5 =>
                  Result := This.ISR.HTIF5;
               when Channel_6 =>
                  Result := This.ISR.HTIF6;
               when Channel_7 =>
                  Result := This.ISR.HTIF7;
            end case;
      end case;
      return Result;
   end Status;

   -----------------
   -- Set_Counter --
   -----------------

   procedure Set_NDT
     (This       : DMA_Controller;
      Channel    : DMA_Channel_Selector;
      Data_Count : UInt16)
   is
   begin
      null;
   end Set_NDT;

   function Items_Transferred
     (This   : DMA_Controller;
      Channel : DMA_Channel_Selector)
      return UInt16
   is
   begin
      return 0;
   end Items_Transferred;

   function Current_NDT
     (This   : DMA_Controller;
      Channel : DMA_Channel_Selector)
      return UInt16
   is
   begin
      return 0;
   end Current_NDT;

   -------------------
   -- Circular_Mode --
   -------------------

   function Circular_Mode
     (This    : DMA_Controller;
      Channel : DMA_Channel_Selector)
      return Boolean
   is
   begin
      return False;
   end Circular_Mode;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (This    : in out DMA_Controller;
      Channel : DMA_Channel_Selector;
      Config  : DMA_Channel_Configuration)
   is
      Ch :  CCR_Ptr := Get_Channel (This, Channel);
   begin
      Disable (This, Channel);

      case Config.Direction is
         when Peripheral_To_Memory =>
            Ch.DIR := False;
            Ch.MEM2MEM := False;
         when Memory_To_Peripheral =>
            Ch.DIR := True;
            Ch.MEM2MEM := False;
         when Memory_To_Memory =>
            Ch.MEM2MEM := True;
      end case;

      Ch.PINC := Config.Increment_Peripheral_Address;
      Ch.MINC := Config.Increment_Memory_Address;
      Ch.PSIZE :=
        DMA_Data_Transfer_Widths'Enum_Rep (Config.Peripheral_Data_Format);
      Ch.MSIZE :=
        DMA_Data_Transfer_Widths'Enum_Rep (Config.Memory_Data_Format);
      Ch.PL :=
        DMA_Priority_Level'Enum_Rep (Config.Priority);

      if Config.Operation_Mode = Circular_Mode then
         Ch.CIRC   := True;  --  Enable circular mode
      end if;
   end Configure;

   -----------
   -- Reset --
   -----------

   procedure Reset
     (This   : in out DMA_Controller;
      Channel : DMA_Channel_Selector)
   is
   begin
      Disable (This, Channel);

--      This_Stream.CR := (others => <>);
--      This_Stream.NDTR.NDT := 0;
--      This_Stream.PAR := 0;
--      This_Stream.M0AR := 0;
--      This_Stream.M1AR := 0;
--      This_Stream.FCR := (others => <>);

      Clear_All_Status (This, Channel);
   end Reset;

   ---------------------------
   -- Peripheral_Data_Width --
   ---------------------------

   function Peripheral_Data_Width
     (This    : DMA_Controller;
      Channel : DMA_Channel_Selector)
      return DMA_Data_Transfer_Widths
   is
      Size : UInt2;
   begin
      case Channel is
         when Channel_1 =>
            Size := This.CCR1.PSIZE;
         when Channel_2 =>
            Size := This.CCR2.PSIZE;
         when Channel_3 =>
            Size := This.CCR3.PSIZE;
         when Channel_4 =>
            Size := This.CCR4.PSIZE;
         when Channel_5 =>
            Size := This.CCR5.PSIZE;
         when Channel_6 =>
            Size := This.CCR6.PSIZE;
         when Channel_7 =>
            Size := This.CCR7.PSIZE;
      end case;
      return DMA_Data_Transfer_Widths'Val (Size);
   end Peripheral_Data_Width;

   -----------------------
   -- Memory_Data_Width --
   -----------------------

   function Memory_Data_Width
     (This : DMA_Controller;
      Channel : DMA_Channel_Selector)
      return DMA_Data_Transfer_Widths
   is
   begin
      return DMA_Data_Transfer_Widths'Val
        (Get_Channel (This, Channel).MSIZE);
   end Memory_Data_Width;

   ------------------------
   -- Transfer_Direction --
   ------------------------

   function Transfer_Direction
     (This : DMA_Controller;
      Channel : DMA_Channel_Selector)
      return DMA_Data_Transfer_Direction
   is
      Dir : Boolean := Get_Channel (This, Channel).DIR;
   begin
      return DMA_Data_Transfer_Direction'Val (To_Bit (Dir));
   end Transfer_Direction;

   --------------------
   -- Operating_Mode --
   --------------------

   function Operating_Mode
     (This    : DMA_Controller;
      Channel : DMA_Channel_Selector)
      return DMA_Mode
   is
   begin
      if Get_Channel (This, Channel).CIRC then
         return Circular_Mode;
      end if;
      return Normal_Mode;
   end Operating_Mode;

   --------------
   -- Priority --
   --------------

   function Priority
     (This : DMA_Controller;
      Channel : DMA_Channel_Selector)
      return DMA_Priority_Level
   is
   begin
      return DMA_Priority_Level'Val (Get_Channel (This, Channel).PL);
   end Priority;

   -------------
   -- Aligned --
   -------------

   function Aligned (This  : Address;
                     Width : DMA_Data_Transfer_Widths)
      return Boolean
   is
      use System.Storage_Elements;
   begin
      return True;
   end Aligned;

end STM32.DMA;
