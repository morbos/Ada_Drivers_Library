with System;

with STM32_SVD.RCC;  use STM32_SVD.RCC;

package body STM32.RNG is

--  Enable the TRNG
--  This requires some care:
--  1) HSI48 must be on see: setup_pll.adb
--  2) The CONDRST must be set with the CONFIGx params
--  3) Then the CONDRST is SW set to low but will only become 0
--  after HW clears it.
--  4) Finally, we enable the RNGEN in the block.
--  Failure to follow the procedure will yield no RND numbers
   procedure Enable_Rng
   is
      --  Here we do some work to overlay the CR reg record
      --  on a UInt32. Reason is, the datasheet makes it clear
      --  that if you are to set the CONDRST then
      --  simultaneously you need to write the CONFIGx vals also.
      --  Thus we map it and make one write to Y which has the address
      --  of the CR reg.
      X : UInt32;
      Cr : STM32_SVD.RNG.CR_Register;
      for X'Address use Cr'Address;
      CrAddr : System.Address := RNG_Periph.CR'Address;
      Y : UInt32;
      for Y'Address use CrAddr;
      --  vvv some magic here.
      --  We want a UInt32 to live on top of the DR
      --  This was more an experiment than a requirement.
      Z : System.Address := RNG_Periph.DR'Address;
      Rval : UInt32;
      for Rval'Address use Z;
      RCC : aliased RCC_Peripheral with Import, Address => RCC_Base;
   begin
      RCC.AHB3ENR.RNGEN := True;
      --  Per selection A
      --  NISTC bit = 0
      --  CONFIG1 = 8
      --  CLKDIV = 0
      --  CONFIG2 = 0
      --  CONFIG3 = D
      --  CED = 0
      Cr.RNG_CONFIG1 := 8;
      Cr.RNG_CONFIG3 := 16#D#;
      Cr.CONDRST := True;
      Y := X;
      RNG.CR.CONDRST := False;
      loop
         exit when not RNG_Periph.CR.CONDRST;
      end loop;

      RNG.CR.RNGEN := True;

   end Enable_Rng;

   function Random return UInt32
   is
   begin
      loop
         exit when RNG.SR.DRDY;
      end loop;
      return RNG.DR;
   end Random;

end STM32.RNG;
