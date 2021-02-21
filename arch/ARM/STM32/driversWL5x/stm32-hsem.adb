with STM32_SVD.HSEM;     use STM32_SVD.HSEM;
with System;

package body STM32.HSEM is

   function OneStepLock (Semaphore : UInt5) return Boolean
   is
      Idx : Sema_Range := Sema_Range (Semaphore);
   begin
      return not
        (STM32_SVD.HSEM.HSEM_Periph.RLR (Idx).LOCK and
           (STM32_SVD.HSEM.HSEM_Periph.RLR (Idx).COREID = 4));
   end OneStepLock;

   function Locked (Semaphore : UInt5; Process : UInt8) return Boolean
   is
      Idx : Sema_Range := Sema_Range (Semaphore);
   begin
      return
        STM32_SVD.HSEM.HSEM_Periph.R (Idx).LOCK and
        STM32_SVD.HSEM.HSEM_Periph.R (Idx).COREID = 4 and
        STM32_SVD.HSEM.HSEM_Periph.R (Idx).PROCID = Process;
   end Locked;

   function AnyLock (Semaphore : UInt5) return Boolean
   is
      Idx : Sema_Range := Sema_Range (Semaphore);
   begin
      return STM32_SVD.HSEM.HSEM_Periph.R (Idx).LOCK;
   end AnyLock;

   procedure ReleaseLock (Semaphore : UInt5; Process : UInt8)
   is
      Idx : Sema_Range := Sema_Range (Semaphore);
      As_Array : UInt32_Array (0 .. 31)
        with Address => System'To_Address (16#5800_1400#);
      X : UInt32;
      R : R_Register;
      for R'Address use X'Address;
   begin
--      STM32_SVD.HSEM.HSEM_Periph.R (Idx).LOCK := False;
--      STM32_SVD.HSEM.HSEM_Periph.R (Idx).COREID := 4;
--      STM32_SVD.HSEM.HSEM_Periph.R (Idx).PROCID := Process;
      R.LOCK   := False;
      R.COREID := 4;
      R.PROCID := Process;
      As_Array (Integer (Idx)) := X;
   end ReleaseLock;

end STM32.HSEM;
