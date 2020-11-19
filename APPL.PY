#%%
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
# %%
APPL = pd.read_csv('data\HistoricalQuotes.csv')

# %%
APPL.isna().any()
# %%
APPL.info()
# %%
APPL = APPL.rename(columns={'Close/Last':'Close', 'Volume':'Vol','Open':'Open','High':'High','Low':'Low'})
APPL['Close'] = APPL['Close'].str.replace('$','').astype('float')
APPL['Open'] = APPL['Open'].str.replace('$','').astype('float')
APPL['High'] = APPL['High'].str.replace('$','').astype('float')
APPL['Low'] = APPL['Low'].str.replace('$','').astype('float')

# %%

APPL.describe()
# %%
#average price over years
APPL['Open'].plot(figsize=(16, 6))
APPL.rolling(100).mean()['Open'].plot()
# %%
