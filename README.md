# Catalog Processing – README
**Author:** MASSON Camille / Adaptation of Rémy PERRON script  
**Last update:** 23/09/2025

## Introduction

This Git project aims to share the script written and used by PERRON Rémy for the preparation of the article:  
*"Fine-scale tracking of sheep grazing in mountain pastures: frugal solution and relevant indicators for improved ecosystem and practice management."*  

This script, developed during his PhD, allows for the extraction and analysis of data from Catlog GPS collars to characterize animal movements according to three behaviors:  

- Rest
- Movement  
- Grazing  

Additionally, it enables the calculation of stocking rates in the studied alpine pastures.  

The implementation is based on R and RStudio, with collaborative management via GitHub, facilitating transparency and reproducibility of the methodology.  







---

## Installation and Repository Setup

### Prerequisites

Before starting, ensure you have installed Git:

- [Git](https://git-scm.com/) | Also, make sure it is properly configured with your IDE (RStudio). You can refer to the following tutorials if needed:  
  - [Tutorial 1](https://youtu.be/QLFc9gw_Hfs?si=W2lMnlM_NHd5S7ba)  
  - [Tutorial 2](https://youtu.be/bUoN85QvC10?si=O33JTkJ3NFHiF1zp)

### Cloning the Repository

The project is structured as a GitHub repository. To clone it locally:

```
# Installation steps
1. Create a GitHub account (if not already done)
2. Open RStudio
3. Go to File → New Project → Version Control → Git
4. Enter the GitHub repository URL and select a local directory to save it:

git clone https://github.com/Camille-masson/Catlog_processing.git
```

---

## File Organization

The project follows a well-structured organization:

```
📂 Catlog_processing/ # Main project directory
│-- 📂 Functions/     # Folder containing shared functions via Git
│-- 📂 raster/        # NDVI files required for calculations, in .tif format
│-- 📂 data/          # Input data, with a sample training dataset
│   │-- 📂 Collars_AAAA_raw/ (AAAA = Study year, 9999 for demonstration dataset)
│   │   │-- 9999_pasture_info.csv  (Information on studied pastures)
│   │   │-- 9999_herd_sizes.csv (Herd size evolution during the season)
│   │   │-- 9999_collars_deployed.csv (Details of collars and parameters)
│   │   │-- 📂 Alpage_demo/  (GPS data of studied individuals)
│   │   │   │-- C00_000000.csv  (Example of a GPS collar file)
│   │   │   │-- C01_000000.csv  (Example of a GPS collar file)
│   │   │   │-- C02_000000.csv  (Example of a GPS collar file)
│-- 📂 outputs/       # Folder where output files are stored
│-- config.R          # Script defining paths and package loading
│-- script_analysis.R # Main script executing the processing steps
```

All files will already be created when sharing the Git project! However, especially for the `data` folder, files must be properly organized following the model described above and based on the structure of the provided training dataset. See Section 4 for more details on data format and structure.

---

## Input Data Description

### Demonstration Dataset
This dataset is already structured and correctly shared via Git. It is a dataset with a fictitious sampling year (9999), and the fictitious pasture is named Alpage_demo. This training dataset includes a sample of GPS points from three collars over a ten day sampling period. It helps illustrate and clarified the different input tables and facilitates the interpretation of the main script.

### GPS Data

Each GPS collar extracted via the Catlog software generates a `.csv` file encoded in UTF-8, containing the following columns:

| **Column**           | **Description**                                        |
|----------------------|--------------------------------------------------------|
| Date                | Recording date *(format: `dd/mm/yyyy`)*                  |
| Time                | Recording time *(format: `hh:mm:ss`)*                    |
| Latitude            | GPS latitude                                           |
| Longitude           | GPS longitude                                          |
| Altitude            | Altitude in meters                                     |
| Satellites          | Number of satellites used                              |
| HDOP                | Horizontal GPS precision                              |
| PDOP                | Three-dimensional GPS precision                       |
| Temperature [C]     | Temperature measured in degrees Celsius               |
| Speed [km/h]       | Speed in km/h                                         |
| TTFF                | Time to first fix                                      |
| SNR AVG             | Average signal-to-noise ratio of used satellites      |
| SNR MAX             | Maximum signal-to-noise ratio among satellites        |

**Note**: For users without Catlog collars, only the columns Date, Time, Latitude, and Longitude are essential.



### Metadata Tables

#### Information on Studied Pastures: `AAAA_pasture_info.csv`

This table compiles the main information related to the pasture. In the demonstration dataset, only one pasture is detailed, but if other pastures are included in your dataset, simply add new rows specifying a reference name for each pasture.

| **Variable**              | **Description**                                                    |
|---------------------------|------------------------------------------------------------------|
| pasture                    | Name of the studied pasture                                      |
| determining_pasture_name    | Long format name of the pasture (for visualizations)            |
| deployment_date                | Collar deployment date *(format: `dd/mm/yyyy hh:mm:ss`)*       |
| removal_date             | Collar removal date *(format: `dd/mm/yyyy hh:mm:ss`)*          |
| proportion_active_day    | Fraction of the day when the collars are active *(1 = 24h/24)*  |
| herd_size          | Herd size associated with the pasture                           |
| nom1_UP                   | Name of the pastoral unit in the Alpine summer pasture map layer|
| medcrit                   | Median threshold value (in meters); *Bjørneraas parameters*     |
| meancrit                  | Mean threshold value (in meters); *Bjørneraas parameters*       |
| spikesp                   | Spike speed threshold; *Bjørneraas parameters*                  |
| spikecos                  | Spike angle threshold; *Bjørneraas parameters*                  |


#### Evolution of Herd Size: `AAAA_herd_sizes.csv`

This table presents the evolution of herd size throughout the alpine grazing season.

The herd size can be:

- Fixed (the most common case, where it does not change).
- Variable throughout the season, particularly due to technical constraints or management measures (e.g., Alpage du Viso).

In this case, it is necessary to record the evolution of herd size for each variation date, specifying the concerned pasture and the new herd size on the given date.

| **Variable**              | **Description**                                                    |
|---------------------------|------------------------------------------------------------------|
| pasture                   | Name of the studied pasture                                    |
| strat_period_date         | Start date of the period *(format: `dd/mm/yyyy`)*               |
| herd_total_size           | Total herd size at this date                                    |


#### Individual Information on Collars and Animals: `AAAA_collars_deployed.csv`

This table contains information related to the individuals on which the collars were placed, such as species, sampling period, proportion of time the collar was active, as well as deployment and removal dates for each individual.

| **Variable**              | **Description**                                                   |
|---------------------------|-----------------------------------------------------------------|
| Collar                   | Collar identifier                                               |
| Programmation             | Type of collar programming                                     |
| pasture                    | Name of the pasture where the individual is located           |
| Species                   | Species
| Period_sampling   | Sampling period, duration in seconds *(time between acquisitions)* |
| Fraction_day_active    | Fraction of the day during which the collar is active *(1 = 24h/24)* |
| deployment_date                 | Collar deployment date *(format: `dd/mm/yyyy hh:mm:ss`)*      |
| removal_date              | Collar removal date *(format: `dd/mm/yyyy hh:mm:ss`)*         |


## Code Description

### config.R

The `config.R` script automatically configures the project's working environment. It defines:

- The main folder paths
- Library loading
- Automatic function loading
- Global parameters

*Note*: It is essential to execute (`source`) this script to ensure the project runs correctly. This script is automatically called in `script_analysis.R` (Section `0. LIBRARIES AND CONSTANTS`).

---

### script_analysis.R

The `script_analysis.R` script is the main script that generates all output data (behavior characterization, stocking rate calculation, etc.) from GPS data.

This script consists of five parts described below.

---

**Part 1: DATA SIMPLIFICATION IN GPKG**

**Objective:** 

This first step simplifies raw GPS data and converts it into a GPKG file. The goal is to visualize in QGIS the exact date of collar deployment and removal.

**Input Data:** 

- Raw GPS collar data: `data/Collars_9999_raw/`

**Output Data:**

- GPKG file generated in `outputs/GPS_simple_GPKG/`
- File name: `Raw_data_9999_Alpage_demo_simplified.gpkg`
- This file will contain the simplified data by pasture

**QGIS Processing Steps to Identify Collar Deployment and Removal Dates:**

1. Run the code section in `script_analysis.R`.  
   This will generate the GPKG file in `outputs/GPS_simple_GPKG/`.

2. Import the GPKG layer into QGIS.  
   - Open QGIS.  
   - Go to "Add a layer" → "Add vector layer".  
   - Select the file `Raw_data_9999_Alpage_demo_simplified.gpkg`.

3. Create a "Datetime" column for temporal analysis.  
   - Open "Field Calculator".  
   - Add a new field:  
     - Name: `Datetime`  
     - Type: `Date and time`  
     - Expression:  
       `to_datetime(date)` 
     - Validate and apply.

4. Apply a color style by collar ID.  
   - Go to Layer Properties → Symbology.  
   - Choose "Categorized".  
   - Select the field `ID`.  
   - Click "Classify" to assign a unique color to each collar.

5. Enable dynamic temporal control.  
   - Go to Layer Properties → "Temporal" tab.  
   - Check the "Enable dynamic temporal control" box.  
   - In "Configuration," choose:  
     - "Single field with date and time".  
     - Select the `Datetime` field.  
   - Validate.

6. Use temporal animation to visualize GPS trajectories.  
   - In the main window, click the clock icon to open the Temporal Control Panel.  
   - Set the "Animation range" with:  
     - Start and end dates corresponding to the study season.  
     - Time step: 24 hours.

7. Explore GPS trajectories to identify collar deployment and removal dates.  
   - Play the animation to see when each collar stopped transmitting.  
   - Check/uncheck collars in the legend to isolate their movement.

---

**Part 2: BJØRNERAAS FILTER CALIBRATION**

**Objective:**

This section analyzes and filters GPS data using the Bjørneraas method to eliminate location errors and improve trajectory quality. It provides a visualization of raw and filtered data and allows testing multiple parameter sets before adopting an optimal filter.

**Bjørneraas Filter Principle:**

Bjørneraas et al. (2010) developed a GPS error filtering method in movement ecology based on:

- A median filter (`medcrit`): Detects outliers based on median distances between points.
- A mean filter (`meancrit`): Removes points that are very far from the average traveled distances.
- A speed threshold (`spikesp`): Eliminates points where speed exceeds a defined threshold.
- A sharp turn criterion (`spikecos`): Removes abrupt turns.

These filters help eliminate errors caused by GPS jumps or signal reflections.

**Input Data:** 

- Raw GPS collar data: `data/Collars_9999_raw/`
- Pasture information file: `data/Collars_9999_raw/9999_pasture_info.csv`

**Output Data:** 

- Output PDF file: `outputs/Bjorneraas_Filter/Filtering_calibration_YEAR_Alpage_demo.pdf`
  - This file contains visualizations of raw GPS trajectories, results of applied filters, and detected errors (R1 and R2).
  - Each graph displays trajectories with color codes for valid points and detected errors.

---

**Part 3: FILTERING CATLOG DATA**

**Objective:**
This section filters raw GPS data using the Bjørneraas filter.

**Input Data:** 

- Raw GPS collar data: `data/Collars_9999_raw/`
- Individual collar information file: `data/Collars_9999_raw/9999_collars_deployed.csv`

**Output Data:** 

- Filtered GPS data: `outputs/Bjorneraas_Filter/Catlog_9999_filtered_Alpage_demo.rds`
- CSV file: `outputs/Bjorneraas_Filter/9999_filtering_Alpage_demo.csv`
  - This file contains the number of points filtered per collar due to detected errors (R1 and R2).
  - Also includes the total number of filtered points across all collars.

---

**Part 4: HMM FITTING**

**Objective:**
This section analyzes GPS trajectories of sheep using the Hidden Markov Model (*HMM*), which classifies three behavioral states: movement, grazing, and resting.  
It relies on the filtered trajectories from the Bjørneraas filter applied in Part 3.

**Input Data:** 

- Filtered GPS data: `outputs/Bjorneraas_Filter/Catlog_9999_filtered_Alpage_demo.rds`
- Individual collar information file: `data/Collars_9999_raw/9999_collars_deployed.csv`

**Output Data:** 

- Categorized GPS trajectories: `outputs/HMM_behavior/Catlog_9999_Alpage_demo_viterbi.rds`
- PDF report: `outputs/HMM_behavior/individual_trajectories/C00.pdf`
  - A PDF is generated per collar.
  - It contains individual results of the fitted Hidden Markov Model.

---

**Part 5: FLOCK STOCKING RATE**

**Objective:**  
This script calculates the flock stocking rate based on the day and behavior (resting, movement, grazing).  
It uses GPS trajectories classified with the HMM model and herd sizes to estimate grazing pressure on the studied pastures.

**Input Data:**  

- Categorized GPS trajectories:  
  `outputs/HMM_behavior/Catlog_9999_Alpage_demo_viterbi.rds`
- Individual collar information file:  
  `data/Collars_9999_raw/9999_collars_deployed.csv`
- Herd size evolution file:  
  `data/Collars_9999_raw/9999_herd_sizes.csv`

**Output Data:**  

Results are stored in `outputs/Calculated_StockingRates/` and saved as `.rds` files per pasture.

- Stocking rate per day and behavior:  
  `outputs/Calculated_StockingRates/Alpage_demo_9999/by_day_and_state_9999_Alpage_demo.rds`
  - Daily stocking pressure per behavioral state (`resting`, `movement`, `grazing`).

- Stocking rate per state (season total):  
  `outputs/Calculated_StockingRates/Alpage_demo_9999/by_state_9999_Alpage_demo.rds`
  - Total pressure per behavioral state across all days.

- Stocking rate per day:  
  `outputs/Calculated_StockingRates/Alpage_demo_9999/by_day_9999_Alpage_demo.rds`
  - Total stocking pressure per day without behavior distinction.

- Total stocking rate (full season):  
  `outputs/Calculated_StockingRates/Alpage_demo_9999/total_9999_Alpage_demo.rds`
  - Sum of stocking pressure over the entire season for the studied pasture.

---

## Possible Issues and Errors  

This section identifies key issues that users may encounter when using this project:  

- **Input file format:**  
Ensure that files are in UTF-8 CSV format with `,` as the separator.  

- **Date format:**  
It is important to follow the format **`dd/mm/yyyy hh:mm:ss`** for collar deployment and removal dates.  
Seconds are crucial for ensuring proper script functionality and correct data interpretation by functions.  

- **Stable Internet connection:**  
A reliable connection is required, especially for Part 4 (*HMM FITTING*), which involves downloading open data.  

- **Managing CPU cores during parallelization:**  
  - By default, the number of cores used is set to `number of cores / 3` in the `config.R` file.  
  - It can be adjusted based on the computer's capacity.  
  - Some steps require long computation times:  
    - Increasing the number of cores improves execution speed.  
    - However, this increases RAM usage, which may cause the script to stop if memory is overloaded.  

- **Dataset size and number of collars processed simultaneously:**  
  - The more collars analyzed, the longer the computations.  
  - Example: Calculating the stocking rate takes about 12 hours for 40 collars.  
  - It's important to plan accordingly. :)  

