with HAL;             use HAL;
with HAL.SPI;
with STM32.Device;    use STM32.Device;
with STM32.SPI;       use STM32.SPI;

package STM32.SubGhzPhy is

   SubGhzPhyPort : constant access SPI_Port := SPI_3'Access;

   procedure SubGhzPhy_Init;
end STM32.SubGhzPhy;
