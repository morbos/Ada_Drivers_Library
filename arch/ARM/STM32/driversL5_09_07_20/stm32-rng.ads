with System;

with STM32.Device;   use STM32.Device;
with STM32_SVD;      use STM32_SVD;
with STM32_SVD.RNG;  use STM32_SVD.RNG;

package STM32.RNG is

   RNG : aliased RNG_Peripheral with Import, Volatile, Address => S_NS_Periph (RNG_Base);

   procedure Enable_Rng;

   function Random return UInt32;

end STM32.RNG;
