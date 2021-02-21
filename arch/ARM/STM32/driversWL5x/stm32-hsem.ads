with HAL;        use HAL;

package STM32.HSEM is

   function Locked (Semaphore : UInt5; Process : UInt8) return Boolean;

   function AnyLock (Semaphore : UInt5) return Boolean;

   function OneStepLock (Semaphore : UInt5) return Boolean;

   procedure ReleaseLock (Semaphore : UInt5; Process : UInt8)
     with Post => (not Locked (Semaphore, Process));

end STM32.HSEM;
