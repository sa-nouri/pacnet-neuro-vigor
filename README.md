# EEG Phase Amplitude Coupling in Parkinson’s Disease 

This repository contains code and resources for [Predicting Motor Vigor from EEG Phase Amplitude Coupling (PAC) in Parkinson’s Disease: Effects of Dopaminergic Medication and Non-invasive Modulation.](https://www.medrxiv.org/content/10.1101/2024.02.20.24303077v1) 
The project explores the effects of dopaminergic medication and non-invasive modulation on motor vigor estimation using deep neural networks and statistical methods.

## Table of Contents

- [EEG Phase Amplitude Coupling in Parkinson’s Disease](#eeg-phase-amplitude-coupling-in-parkinsons-disease)
  - [Table of Contents](#table-of-contents)
  - [Abstract](#abstract)
  - [Project Structure](#project-structure)
  - [Notebooks](#notebooks)
  - [Contributing](#contributing)
  - [License](#license)

## Abstract

Impaired motor vigor is a defining characteristic of Parkinson's disease (PD), yet the underlying brain mechanisms governing motor vigor (MV) remain unclear. Recent studies have suggested beta-gamma Phase-Amplitude Coupling (PAC) derived from the resting-state electroencephalogram (EEG) is a potential biomarker for PD that is modulated by Deep Brain Stimulation (DBS) and L-dopa treatment. Specifically, PAC has been suggested to be a marker of transitions between motor movements, as opposed to encoding the vigor of the current movement. Here, we comprehensively investigate the potential of various PAC interactions, across different frequency pairs, beyond the linear approaches typically employed to predict MV during motor tasks in PD and study the effects of dopaminergic medication and non-invasive Galvanic Vestibular Stimulation (GVS). We recorded EEG data from 20 PD patients and 22 healthy controls executing a simple, overlearned handgrip task. Subjects were tested on and off L-dopa medication and with and without GVS (multi-sine either 50-100Hz, 100-150Hz). In a preliminary linear (LASSO-based) analysis comparing various PACs and a broad range of commonly used EEG features, PAC features were found to be crucial for predicting MV approximately equally in PD and HC. Initial findings from the linear analysis showed PAC as a significant indicator for MV in both groups, although with variability in cross-validation that implied a complex, non-linear relationship between PAC and MV. To extensively investigate the PAC-MV relation, we used a deep convolutional neural network (PACNET), developed based on pre-trained VGG-16 architecture, to estimate MV from PAC values. In both PD and HCs, Delta-Beta, Theta-, Alpha-, and Beta-Gamma PACs were important for MV prediction. In PD subjects, GVS affected Delta-Beta, Theta- Gamma-, and Beta-Gamma PACs role in MV prediction, which was sensitive to different GVS stimulation parameters. These PACs were also relevant for PD patients' MV prediction after L-dopa medication. This study supports the hypothesis that EEG PAC across multiple frequency pairs, not just beta-gamma, predicts MV and not just motor transitions and can be a biomarker for assessing the impact of electrical stimulation and dopaminergic medication in PD. Our results suggest that PAC is involved in MV, in addition to a range of previously reported cognitive processes, including working and long-term memory, attention, language, and fluid intelligence. Non-linear approaches appear important for examining EEG PAC and behavior relations.

Keywords: Parkinson’s disease, Phase-amplitude coupling, Deep neural network, Transfer learning, Motor vigor, EEG regression, Neurophysiological analysis, Biomarker, Brain stimulation.

## Project Structure

- **notebooks/**: Jupyter notebooks for exploration and analysis.
- **src/**: Source code for data preprocessing, model training, and analysis.

## Notebooks

- `analyzing_pacs`: Explore and visualize the generated PAC data.
- `analyzing_saliency_maps`: Explore and visualize the generated saliency maps data.
- `grad_camp`: Apply an example of Grad-CAM as an explainable AI method.

## Contributing

If you want to contribute to this project, please check our [contributing guidelines](./CONTRIBUTING.md).

## License

This project is licensed under the [MIT License](./LICENSE.md).
