# R Economics Masterclass

**This is the introduction to R I wish I had.** 

A masterclass on professional, reproducible workflows for economists: cleaning, debugging, data filtration, modelling, simulations, and automated exporting.

Suitable for all levels, this course is designed to make you a **better economist**. The goal is simple: improve your efficiency, save you hours of headaches, and eliminate errors through automation. Business students use Excel - economists use software (R/Stata/Python).

These scripts provide the hacks and workflows necessary to move from "manual" analysis to **replicable, professional code**.

---

## 📺 The Video Series (Watch & Code Along)

| Video Title | Skill Learned | Script | 
| :--- | :--- | :--- | 
| **1. Automated Import, Convert, Combine** ((Video Coming Soon)) | Reproducible data loading (`tidyverse`) | `01_Clean_Data_Automated.R` | 
| **2. Debug Like a Pro** ((Video Coming Soon)) | Problem solving common coding errors | `02_Debug_Like_A_Pro.R` | 
| **3. Essential Data Skills** (Video Coming Soon) | Professional analysis & visualization (`ggplot2`) | `03_Essential_Skills.R` | 
| **4. The Spurious Trap** (Video Coming Soon) | Monte Carlo Simulations | `04_Spurious_Regression_Trap.R` | 
| **5. The Copy-Paste Intervention** (Video Coming Soon) | Exporting Results to Word/Excel/LaTeX (`modelsummary`/`stargazer`) | `05_Copy_Paste_Intervention.R` |

---

## 🚀 Quick Start

1. **Download** this repository (green "Code" button > Download ZIP).
2. **Unzip** the folder. Keep all files in the same directory.
3. **Open** the `.Rproj` file (R Project) to launch RStudio. This automatically sets your working directory to the project folder (never set absolute paths like `C:/Users/...`).

---

## 🇨🇦 Data Source

The best economists **know the data**.

The datasets used in this course (`CDataQ.csv`, `CDataM.csv`) are real Canadian macroeconomic data.

**Want to learn how to fetch this data yourself?**
I have a separate guide on how to build this exact dataset from scratch using official Statistics Canada sources (using the `cansim` package).

* **[Watch the Video: StatsCan Economic Data Guide](https://youtu.be/YtObmeC5rYw)**
* **[Get the Data Guide & Raw Files](https://github.com/SSEconomics/statscan-econ-data-guide)**

---

## 🐛 The Golden Rules of Debugging

When your code crashes (and it will), do not panic. Follow this 3-step workflow from **Video 2** before trying to fix it:

1. **Read the Error:** The Console is your friend. Read the red text to understand *why* it failed. Do not ignore warnings.
2. **Find out where the error occurred:** Don't guess. Look at the line number or use `traceback()`. Run the code line by line (Ctrl+Enter) to find the specific function that caused the stop.
3. **Check the environment after each command:** Code can run without crashing and still be wrong—always double-check output and verify calculations in the data viewer 

---

## 🏆 The "Better Economist" Standard

You should care deeply about the quality of your figures and tables. In this course, we adhere to the **Stand-Alone Principle**: A stranger should be able to pick up your graph or table and understand it perfectly without reading your text.

### 1. Code Hygiene

* **Clean Scripts:** Files include only the commands used and descriptions.
* **No Raw Output:** Never show raw R console output in a report. Everything must be summarized in words or formatted into a suitable table.

### 2. General Presentation Rules

* **Notes are Mandatory:** Must describe the data source, date range, transformations, and seasonal adjustments.
* **Titles:** It is best to handle titles and notes in LaTeX/Markdown rather than the figure itself.
* **Real Names:** Always use the actual name of the series (e.g., "Real GDP Growth"), never the R variable name (e.g., `gdp_real_sa`).

### 3. Guidelines for Figures

* **Axis Labels:** Use units only (e.g., "Percent", "Billions of Dollars").
* **Legends:** Label the series clearly without syntax.
* **No Borders:** Remove borders around legends and figures.
* **Stationarity:** Graph what you are modelling. Non-stationary data only if showing trends.
* **Visuals:** No default grey backgrounds (standard `ggplot2`). Format figures to look like they belong in a journal (e.g., `theme_classic()`).

### 4. Guidelines for Tables

* **Relevance:** Only describe stationary data. Do not show variables that were not asked for.
* **Precision:** No more than **three significant digits** (e.g., `0.752`, not `0.75165`).

---

## 👤 Author

**Stephen Snudden, PhD**

* [YouTube Channel](https://youtube.com/@ssnudden)
* [Personal Website](https://stephensnudden.com/)
