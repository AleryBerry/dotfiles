import { z } from 'zod';
import { exit } from 'process';
import ini from 'ini';
import os from 'os';

const GeneralSchema = z.object({
  base_currency: z.string(),
  display: z.enum(['price', 'percentage', 'both']),
});

const CurrenciesSchema = z.record(z.string(), z.object({ icon: z.string().optional() }));

const ConfigSchema = z.intersection(
  z.object({ general: GeneralSchema }),
  CurrenciesSchema
);

const CURRENCIES_FILE_PATH = '/.config/polybar/crypto-config';

const configFile = Bun.file(os.homedir + CURRENCIES_FILE_PATH);

const configText = await configFile.text();

const config = ConfigSchema.parse(ini.parse(configText));

const currencies = Object.keys(config).filter(
  (key) => key !== 'general'
) as (keyof typeof config)[];

const { base_currency, display } = config.general;

const output: string[] = [];

try {
  const ids = currencies.join(',');
  const response = await fetch(
    `https://api.coingecko.com/api/v3/coins/markets?vs_currency=${base_currency}&ids=${ids}`
  );
  const data = await response.json();

  if (!Array.isArray(data)) {
    const { status } = data as { status: { error_message: string } };
    if (status && status.error_message) {
      throw new Error(status.error_message);
    }
    throw new Error('Unexpected API response format');
  }

  const priceData: { [key: string]: { price: number; change: number } } = {};
  for (const item of data) {
    priceData[item.id] = {
      price: item.current_price,
      change: item.price_change_percentage_24h,
    };
  }

  for (const currency of currencies) {
    const icon = config[currency].icon;
    const currencyData = priceData[currency];

    if (currencyData) {
      const localPrice = new Intl.NumberFormat('en-US', {
        style: 'currency',
        currency: base_currency,
      }).format(currencyData.price);

      const change24h = currencyData.change;

      if (display === 'both') {
        output.push(`${icon} ${localPrice}/${change24h.toFixed(2)}%`);
      } else if (display === 'percentage') {
        output.push(`${icon} ${change24h.toFixed(2)}%`);
      } else {
        output.push(`${icon} ${localPrice}`);
      }
    }
  }
} catch (error) {
  console.error(error);
  exit(1);
}

console.log(output.join('  '));
