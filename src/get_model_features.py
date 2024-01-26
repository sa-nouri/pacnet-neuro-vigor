import numpy as np
import pandas as pd
import pyarrow as pa
import pyarrow.parquet as pq
import tensorflow as tf
from google.colab import drive
from tensorflow.keras.applications.vgg16 import VGG16, preprocess_input
from tensorflow.keras.models import load_model

print("..: Libraries have been loaded!")

drive.mount('/content/drive')

path_dataset = "/path-to-dataset-dir/"
print("..: Load the PAC dataset with MV!")
df = pd.read_parquet(path_dataset + "data_merged.parquet")

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
df_test['ID'] = range(1, len(df_test) + 1)

print("Get the saved model")
path_models = "/path-to-models-dir/"
model = load_model(path_models + f"model_id_{TEST_DATA_ID}.h5")
model = VGG16(weights='imagenet')

# Function to apply Grad-CAM to an input array
def apply_grad_cam(input_array):
    img_array = np.expand_dims(input_array, axis=0)
    img_array = preprocess_input(img_array)

    last_conv_layer = model.get_layer('block5_conv3')
    grad_model = tf.keras.models.Model([model.inputs], [last_conv_layer.output, model.output])

    with tf.GradientTape() as tape:
        last_conv_layer_output, predictions = grad_model(img_array)
        predicted_class = tf.argmax(predictions[0])
        grads = tape.gradient(predictions, last_conv_layer_output)

    pooled_grads = tf.reduce_mean(grads, axis=(0, 1, 2))

    last_conv_layer_output = last_conv_layer_output[0]
    heatmap = last_conv_layer_output @ pooled_grads[..., tf.newaxis]
    heatmap = tf.squeeze(heatmap)

    heatmap = tf.maximum(heatmap, 0)
    heatmap /= tf.reduce_max(heatmap)

    return heatmap.numpy()

# Create an empty DataFrame to store the results
result_df = pd.DataFrame(columns=['ID', 'pac_values', 'sm_pac_values'])

# Iterate through rows of test DataFrame
for index, row in df_test.iterrows():
    print("The index of test dataset: ", index)
    input_array = row['pac_values']
    ID = row['ID']
    # Apply Grad-CAM
    output_array = apply_grad_cam(input_array)
    # Append the results to the new DataFrame
    result_df = result_df.append({'ID': ID, 'pac_values': input_array, 'sm_pac_values': output_array}, ignore_index=True)
    print()

print()
print(result_df)
print(result_df.info)

print("..: Start saving results as a pyarrow!")
path_saliency_maps = "/path-to-sm-results/"
table = pa.Table.from_pandas(result_df)
pq.write_table(table,
                path_saliency_maps + f"results_saliency_maps_gradcam_id_{TEST_DATA_ID + 1}.parquet",
                compression='BROTLI')
print("Save results as a parquet file has been done!")

print("..: Done ..")
