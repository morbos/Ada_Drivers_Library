--  This spec has been automatically generated from nrf53.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with System;

--  nRF5340
package NRF53_SVD is
   pragma Preelaborate;

   --------------------
   -- Base addresses --
   --------------------

   TAD_Base : constant System.Address :=
     System'To_Address (16#E0080000#);
   GPIO_P0_Base : constant System.Address :=
     System'To_Address (16#50842500#);
   GPIO_P1_Base : constant System.Address :=
     System'To_Address (16#50842800#);

end NRF53_SVD;
