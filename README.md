# Inclusive Mobility in Chiang Mai — Code & Reproducible Analysis

[![Mendeley Data](https://img.shields.io/badge/Mendeley%20Data-10.17632%2Fyfkgwtrmh4.1-blue)](https://doi.org/10.17632/yfkgwtrmh4.1)

This repository hosts analysis and model code for the study of inclusive pedestrian mobility in Chiang Mai (walkers vs. manual wheelchair users). It includes the Bayesian state-space model, R analysis scripts, and an optional Python helper for object detection on street-level video.

- **Stan model:** `moving_speed_spatial_ar1_interaction.stan`  
- **R analysis:** `moving_speed_temples_cnx.r` (fit) and `moving_speed_temples_cnx_figure.r` (figures/tables)  
- **Python (optional):** `yolov8_object_detection.py` for detector preprocessing

> **Note on scaling:** predictors such as `N_car` are **z-scored** (x − mean) / sd. Effects are reported **per 1-SD** unless otherwise stated.

---

## Data availability

De-identified data supporting this study are openly available:

> Utsunomiya, Y., 2025. *Inclusive Mobility in Chiang Mai*. Mendeley Data, V1. https://doi.org/10.17632/yfkgwtrmh4.1

Raw frames containing identifiable people are not redistributed. The repository and dataset provide anonymized trajectories and derived covariates (e.g., `N_car`, crowding, visibility). See the Mendeley record for details.

---

## Quick start

### R (analysis & figures)

**Requirements:** R ≥ 4.2, `cmdstanr`, `posterior`, `tidyverse`, `ggplot2`.

```r
# install core packages
install.packages(c("tidyverse", "posterior", "ggplot2", "readr"))

# cmdstanr (if not installed)
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
cmdstanr::install_cmdstan()   # one-time toolchain install

# 1) Fit model & generate outputs
source("moving_speed_temples_cnx.r")

# 2) Create figures and tables
source("moving_speed_temples_cnx_figure.r")
```

Tips:
- If the outcome is on a transformed scale (e.g., log speed), back-transform before reporting.
- Because `N_car` is standardized, label effects **“per 1-SD in N_car”**. To convert to “per car,” divide by `sd(N_car_raw)`.

### Python (optional: object detection)

**Requirements:** Python ≥ 3.10.

```bash
python -m venv .venv
# Windows: .venv\Scripts\activate
# macOS/Linux:
source .venv/bin/activate
pip install ultralytics opencv-python numpy torch torchvision torchaudio
```

Example run (edit paths and options as needed):

```bash
python yolov8_object_detection.py   --input data/raw/videos/*.mp4   --output data/processed/detections   --model yolov8n.pt   --conf 0.25
```

---

## Repository layout (typical)

```
.
├─ models/
│  └─ moving_speed_spatial_ar1_interaction.stan
├─ R/
│  ├─ moving_speed_temples_cnx.r
│  └─ moving_speed_temples_cnx_figure.r
├─ python/
│  └─ yolov8_object_detection.py
├─ data/
│  ├─ raw/         # (not tracked) original videos / GPS logs
│  ├─ interim/     # detector outputs aligned with GPS
│  └─ processed/   # analysis-ready CSVs (from Mendeley Data)
├─ outputs/
│  ├─ figures/
│  └─ tables/
└─ README.md
```

Folder names can differ; the structure above is a clear convention for reproduction.

---

## Reproducibility

- **Seed/state:** set a fixed seed where applicable (e.g., `set.seed(2025)` in R).  
- **CmdStan version:** record with `cmdstanr::cmdstan_version()`.  
- **Environments:** one of the following is recommended:
  - R: `renv::snapshot()` to create `renv.lock`, or list package versions in the README.  
  - Python: add `requirements.txt` or `environment.yml` (examples below).

**Example `requirements.txt`**
```
ultralytics
opencv-python
numpy
torch
torchvision
torchaudio
```

**Example `environment.yml`**
```yaml
name: mobility-cnx
channels: [conda-forge, pytorch]
dependencies:
  - r-base>=4.2
  - r-tidyverse
  - r-posterior
  - r-cmdstanr
  - python>=3.10
  - pip
  - pip:
      - ultralytics
      - opencv-python
      - numpy
      - torch
      - torchvision
      - torchaudio
```

---

## How to cite

- **Data:** Utsunomiya, Y., 2025. *Inclusive Mobility in Chiang Mai*. Mendeley Data, V1. https://doi.org/10.17632/yfkgwtrmh4.1  
- **Article:** (add full citation and DOI when available).

Consider adding a `CITATION.cff` file so GitHub renders a “Cite this repository” panel.

---

## License

- **Code:** choose a permissive license such as **MIT** or **Apache-2.0** and add a `LICENSE` file.  
- **Data:** governed by the license specified on the Mendeley record (e.g., **CC BY 4.0** if selected). See the dataset DOI above.

---

## Acknowledgements

Support from Chiang Mai partners and participants is gratefully acknowledged. If applicable, include grant/funding details (e.g., “This work was supported by [Funder, grant no. XXX].”).

---

## Contributing / Issues

Bug reports and small pull requests are welcome. Please avoid uploading raw videos or any files containing personally identifiable information.

---

