with STM32_SVD.RCC; use STM32_SVD.RCC;
with STM32_SVD.PWR; use STM32_SVD.PWR;

package body STM32.SubGhzPhy is

   procedure SubGhzPhy_Init
   is
      Config : SPI_Configuration;
   begin
      Enable_Clock (SubGhzPhyPort.all);
      RCC_Periph.CSR.RFRST := False;
      loop
         exit when not RCC_Periph.CSR.RFRSTF;
      end loop;
      Enable (SubGhzPhyPort.all);
      STM32.Device.Reset (SubGhzPhyPort.all);

--      RCC_Periph.APB3RSTR.SUBGHZSPIRST := True;

      Config.Mode := Master;
      Config.Baud_Rate_Prescaler := BRP_4;
      Config.Clock_Polarity := Low;
      Config.Clock_Phase := P1Edge;
      Config.First_Bit := MSB;
      Config.CRC_Poly := 7;
      Config.Slave_Management := Software_Managed;  --  essential!!
      Config.Slave_Internal := Internal_NSS;
      Config.Direction := D2Lines_FullDuplex;
      Config.Data_Size := HAL.SPI.Data_Size_8b;
      Config.Fifo_Level := True;
      Config.Transmit_DMA := False;
      Config.Receive_DMA := False;

      Disable (SubGhzPhyPort.all);
      Configure (SubGhzPhyPort.all, Config);

      Enable (SubGhzPhyPort.all);

      PWR_Periph.CR3.EWRFBUSY := True;

      PWR_Periph.SCR.CWRFBUSYF   := True;

   end SubGhzPhy_Init;

end STM32.SubGhzPhy;
