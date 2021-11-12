

import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns

# Need to compute the number of bins

d = pd.read_csv("data.csv")
#print(d)
sns.histplot(data=d, x="x", weights="#", kde=True, bins=100)
plt.show()
