#@title Mount drive files
import timeit

import pandas as pd
import pyarrow as pa
import pyarrow.parquet as pq
from google.colab import drive
from IPython.display import clear_output
from scipy.io import loadmat
from tensorpac import Pac

#@title Define Libraries

path_data = "/path-to-file/"

drive.mount('/content/drive/')
data = loadmat(path_data + "PT_TimeLocked_1000Back.MAT")

#@title Parameter Initializing
all_subjects = {"healthy": [1, 2, 3, 4, 5, 6, 7, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22],
                "parkinson_disease": [1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 13, 14, 15, 16, 17, 18, 19, 20]
                }

trials   = list(range(1, 11))
channels = list(range(1, 28))

peak_time     = 12
reaction_time = 13
sampling_rate = 1000

# Define the parameters to extract data
keys    = ['dataHCTask2', 'dataPD1Task2', 'dataPD2Task2']
tasks   = ['task_gvssham.mat', 'task_gvsstim7.mat', 'task_gvsstim8.mat']
stimuli = ['sham', 'stim7', 'stim8']

healthy_cases   = ["hcoffmed", "hcoffmed", "hcoffmed_gvs8"]
pd_off_cases    = ["pdoffmed", "pdoffmed", "pdoffmed_gvs8"]
pd_on_cases     = ["pdonmed", "pdonmed", "pdonmed_gvs8"]

#@title Obtain PAC function

def obtain_pac(chosed_signal):
    # Phase Range: 0 - 30
    # Amplitude Range: 0 - 60
    pac  = Pac(idpac=(5, 2, 0), f_pha=(0, 30.1, 1, 1), f_amp=(0, 60.1, 1, 1), dcomplex='wavelet')
    xpac = pac.filterfit(sampling_rate, chosed_signal)
    return xpac.mean(-1)

#@title Compute PAC values
data_stats = pd.DataFrame([])
task_run = 1
stim = 1
run_id = 0

for key in keys:
    print("Data Type for getting data: ", key)
    data_task = data[key]
    
    if key == 'dataHCTask2':
        subjects = all_subjects['healthy']
        print("Healthy cases are selected!")
    else:
        subjects = all_subjects['parkinson_disease']
        print("PD cases are selected!")
    
    print("The run is for stimulus: ", tasks[stim])
    task = loadmat(path_data + f"{tasks[stim]}")
    
    # Uncomment for generating each case
    medication = healthy_cases[stim]
    # medication = pd_off_cases[stim]
    # medication = pd_on_cases[stim]

    gvs_data = task[medication]
    
    for subject in subjects:
        print("Subject ID: ", subject)
        eeg_data = data_task[stim, subject-1]
        print("Shape of selected EEG DATA: ", eeg_data.shape)
        
        for channel in channels:
            print("Channel ID: ", channel)
            for trial in trials:
                behavior_signal = gvs_data.item(subject-1)
                p_time = behavior_signal[trial - 1, peak_time -1 ]
                r_time = behavior_signal[trial -1 , reaction_time -1]
                print("Trial: ", trial)
                signal = eeg_data[channel-1, :, trial-1]
                start = timeit.default_timer()
                pac_values = obtain_pac(signal).reshape(1, 60*30)
                stop = timeit.default_timer()
                run_id = run_id + 1
                print('Run id: ', run_id)
                print('Time of computing pac: ', stop - start)
                stats = {
                        "task": key,
                        "stimulus": stimuli[stim],
                        "medication": medication,
                        "channel": channel,
                        "subject": subject,
                        "trial": trial,
                        "peak_time": p_time,
                        "reaction_time": r_time,
                        "pac_values": pac_values[0]
                        }
                data_stats = data_stats.append(stats, ignore_index=True)
            clear_output(wait=True)
            print()
        print("For all selected channels PACs are computed!")
    print("For selected subject, PACs are computed!")

print("..: Start saving pandas as a pyarrow!")
table = pa.Table.from_pandas(data_stats)
pq.write_table(table, path_data + f"data_{task_run}.parquet", compression='BROTLI')
print("Save data as a parquet file has been done!")

print("..: Done ...")
