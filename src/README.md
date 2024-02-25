# src Directory Overview

This directory contains the source code for various components of the "pacnet-neuro-vigor" project, each implemented in different programming languages (Python, MATLAB, R) and serving distinct purposes within the research framework.

## Components

### PACNET

- **Language**: Python
- **Description**: Implements the generation of raw PAC data and images, utilizes deep transfer learning based on VGG16 for estimating motor vigor from raw PAC data, includes Grad-CAM implementation for feature visualization, and performs correlation analysis.

### Headplots

- **Language**: MATLAB
- **Description**: Provides implementations for visualizing headplots

### Lasso

- **Language**: MATLAB
- **Description**: Utilizes Lasso regression to estimate motor vigor from selected features and PAC data

### Statistical Analysis

- **Language**: R
- **Description**: Contains scripts for conducting comprehensive statistical analysis on PAC data and saliency maps, exploring the relationships and effects under various conditions.
