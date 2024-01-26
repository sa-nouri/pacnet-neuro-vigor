import numpy as np
import pandas as pd
from google.colab import drive
from keras import backend as K
from keras import losses, metrics
from keras.applications.vgg16 import VGG16, preprocess_input
from keras.layers import BatchNormalization, Dense, Dropout, Flatten
from keras.models import Sequential
from keras.preprocessing.image import array_to_img, img_to_array
from scipy.stats import spearmanr
from tensorflow.keras import callbacks
from tensorflow.keras.optimizers import schedules
from tensorflow_addons import optimizers

print("..: Libraries have been loaded!")

drive.mount('/content/drive')
path_dataset = "/path-to-dataset-dir/"
print("..: Load the PAC dataset with MV!")
df = pd.read_parquet(path_dataset + "data_merged.parquet")

# Apply preprocessing to pacs-motor-vigour-data
df = df.dropna(how='all').reset_index(inplace=True, drop=True)

# Get the indices (e.g., information) for test dataset (40 iterations)
print("..: Load the selected Trials for Test Model!")
data_tt = pd.read_excel(path_dataset + "Trials4Test_All_PD.xlsx")

print("..: Get the Test data for training model!")
TEST_DATA_ID = 0
itr = np.arange(0, len(data_tt))
row = data_tt.iloc[int(itr[TEST_DATA_ID])]
row = pd.DataFrame(row).iloc[1:,:].reset_index()
row.rename(columns={"index": "subject", 0: "trial"}, inplace=True)
row["subject"] = row["subject"].apply(lambda rec: float(rec[2]))

df['label'] = np.arange(0, len(df))
df_test = pd.DataFrame([])
for j, item in row.iterrows():
    temp = df[(df["subject"] == item["subject"]) & (df["trial"] == item["trial"])]
    df_test = pd.concat([df_test, temp])
df_test = df_test.drop_duplicates(subset='label', keep="last")

print("..: Get the training data for training model!")
df_train = df[~df["label"].isin(df_test.label)]

print("..: Process the training data for VGG16 model!")
split_df = pd.DataFrame(df_train.pac_values.tolist())
split_df = (split_df - split_df.min())/(split_df.max() - split_df.min())

df = pd.concat([df_train, split_df], axis=1)
df_train.fillna(method='ffill', inplace=True)
df_train.fillna(df.median, inplace=True)

pac_vals = np.array(df_train.iloc[:, 9:])
pt_vals = np.array(df_train["peak_time"])
rt_vals = np.array(df_train["reaction_time"])
pacs_3d = np.dstack([pac_vals] * 3)
pacs_3d = pacs_3d.reshape(-1, 60, 30, 3)
pacs_3d_reshaped = np.asarray([img_to_array(array_to_img(im, scale=False).resize((64, 32))) for im in pacs_3d])

# Define the parameters for instanitaing the model
IMG_WIDTH, IMG_HEIGHT, IMG_DEPTH = pacs_3d_reshaped[0].shape
print(f"..: The image shape is :({IMG_WIDTH}, {IMG_HEIGHT}, {IMG_DEPTH})")
data = preprocess_input(pacs_3d_reshaped)

print("..: Process the test data for evaluating VGG16 model!")
split_df = pd.DataFrame(df_test.pac_values.tolist())
split_df = (split_df-split_df.min())/(split_df.max() - split_df.min())

df = pd.concat([df_test, split_df], axis=1)
df_test.fillna(method='ffill', inplace=True)
df_test.fillna(df.median, inplace=True)

pac_vals = np.array(df_test.iloc[:, 9:])
pt_vals = np.array(df_test["peak_time"])
rt_vals = np.array(df_test["reaction_time"])
pacs_3d = np.dstack([pac_vals] * 3)
pacs_3d = pacs_3d.reshape(-1, 60, 30, 3)
pacs_3d_reshaped = np.asarray([img_to_array(array_to_img(im, scale=False).resize((64, 32))) for im in pacs_3d])

# Define the parameters for instanitaing the model
data_test = preprocess_input(pacs_3d_reshaped)

print("..: Defining the pearson correlation as a metric!")
# x: y_true, y:y_pred
def pearson_r(x, y):
    mx = K.mean(x, axis=0)
    my = K.mean(y, axis=0)
    xm, ym = x - mx, y - my
    r_num = K.sum(xm * ym)
    x_square_sum = K.sum(xm * xm)
    y_square_sum = K.sum(ym * ym)
    r_den = K.sqrt(x_square_sum * y_square_sum)
    r = r_num / r_den
    return K.mean(r)

print("..: Defining spearman correlation as a metric!")
def compute_spearmanr(y, y_pred):
    spearsum = 0
    cnt = 0 
    for col in range(y_pred.shape[1]):
        v = spearmanr(y_pred[:,col], y[:,col]).correlation
        if np.isnan(v):
            continue
        spearsum += v
        cnt += 1
    res = spearsum / cnt
    return res

print("..: Defining the loss function of the model")
loss = losses.MeanAbsoluteError(reduction="auto",
                                name="mean_absolute_error")

print("..: Defining the performance metrics")
metrics_ = [
            metrics.MeanSquaredError(name="mse", dtype=None),
            metrics.RootMeanSquaredError(name="rmse", dtype=None),
            metrics.MeanAbsoluteError(name="mabe", dtype=None),
            metrics.MeanAbsolutePercentageError(name="mabpe", dtype=None),
            metrics.MeanSquaredLogarithmicError(name="msle", dtype=None),
            compute_spearmanr,
            pearson_r
            ]

print("..: Defining the optimizer for training model with schedule!")
lr_schedule = schedules.ExponentialDecay(
                                        initial_learning_rate=1e-3,
                                        decay_steps=10,
                                        decay_rate=0.9
                                        )

radam = optimizers.RectifiedAdam(learning_rate=lr_schedule)
optimizer_ = optimizers.Lookahead(radam, sync_period=6, slow_step_size=0.5)
callback_ = callbacks.EarlyStopping(monitor='mabe', patience=10)

## Loading VGG16 model
print("..: Load VGG16 model with imagenet weights")
base_model = VGG16(weights="imagenet", 
                    include_top=False,
                    input_shape=(IMG_WIDTH, IMG_HEIGHT, IMG_DEPTH))
base_model.trainable = True ## Trainable weights

print("..: Defining model's architecture ...")
model = Sequential()
model.add(base_model)
model.add(Flatten())
model.add(BatchNormalization())
model.add(Dense(4096, activation='relu'))
model.add(Dropout(0.3))
model.add(Dense(2048, activation='relu'))
model.add(Dropout(0.3))
model.add(Dense(1024, activation='relu'))
model.add(Dropout(0.3))
model.add(Dense(512, activation='relu'))
model.add(Dropout(0.3))
model.add(Dense(256, activation='relu'))
model.add(Dropout(0.4))
model.add(Dense(128, activation='relu'))
model.add(Dropout(0.3))
model.add(Dense(64, activation='relu'))
model.add(Dropout(0.3))
model.add(Dense(32, activation='relu'))
model.add(Dropout(0.3))
model.add(Dense(1, activation='linear'))

print("..: Defining model compile parameters ...")
model.compile(loss='mse', optimizer='adam', metrics=metrics_)

print("..: Train the model and gets its history")
history = model.fit(data,
                    pt_vals,
                    validation_split=0.2,
                    epochs=40,
                    callbacks=[callback_],
                )

print("..: Save the model history")
np.save(f"model_history_id_{TEST_DATA_ID}.npy", history.history)

print("..: Save the model")
path_models = "/path-to-models-dir/"
model.save(path_models + f"model_id_{TEST_DATA_ID}.h5")

print("..: Evaluation of the trained model on test dataset!")
model.evaluate(data_test)
print("..: Done ...")
