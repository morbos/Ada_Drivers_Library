with System;                use System;
private with STM32_SVD.DFSDM;

package STM32.DFSDM is

   type DFSDM_Block is limited private;

   type DFSDM_Channel is (Channel_0,
                          Channel_1,
                          Channel_2,
                          Channel_3,
                          Channel_4,
                          Channel_5,
                          Channel_6,
                          Channel_7
                         );
   procedure Enable
     (This    : in out DFSDM_Block;
      Channel : DFSDM_Channel)
     with
       Inline,
       Post => Enabled (This, Channel);

   procedure Disable
     (This    : in out DFSDM_Block;
      Channel : DFSDM_Channel)
     with
       Inline,
       Post => not Enabled (This, Channel);

   function Enabled
     (This    : DFSDM_Block;
      Channel : DFSDM_Channel)
      return Boolean;

private

   type DFSDM_Block is new STM32_SVD.DFSDM.DFSDM1_Peripheral;

end STM32.DFSDM;
