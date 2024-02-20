import numpy as np
import pandas as pd

iterations = np.arange(0, 40)

df_merged = pd.DataFrame([])
path_results = "/path-to-results/"

for itr in iterations:
    print(itr)
    df = pd.read_csv(path_results + "roi_4test_corrs_allChannels_itr{}.csv".format(itr+1))
    df["correlation"] = abs(df["correlation"])
    df_tmp = df.groupby(["iteration", "stimulus", "condition", "subject", "trial_1", "trial_2"])["correlation"].mean().reset_index()
    df_merged = df_merged.append(df_tmp, ignore_index=True)

df_merged.to_csv(path_results + "roi_4test_corrs.csv", index=False)
print(df_merged.info())
print(df_merged)
print("..: Done")
    