# 4-Parameter Logistic Regression ELISA Analysis Tool

This R Shiny app provides an interactive interface for performing ELISA (Enzyme-Linked Immunosorbent Assay) data analysis using a 4-parameter logistic (4PL) regression model. Users can upload their ELISA data, view analysis results, and download both the generated plots and 4PL results.

## Features

- Perform 4PL ELISA analysis right from your web browser with minimal setup
- Works for all ranges and units of standards, and all types of ELISA kits (R&D Systems Quantikine, DuoSet; Abcam, Fisher, etc)
- Immediate visualization of the sample data superimposed on the standard curve as well as the 4PL equation used to estimate concentrations (bottom right of plot)
- Immediate preview of the standards input and the 4PL results in table format
- Downloadable plots (as a `.png` file) and data (as `.xlsx` files) for further analysis

## Getting Started

To use the ELISA Analysis Tool, simply access the app through its URL (listed below), upload your ELISA data file, and interact with the analysis results directly in your web browser. This App assumes that you have already performed the necessary background corrections and adjustments, e.g. subtracted the absorbance of the blank well from all wells and corrected for background absorbance, etc.

## Input File Requirements

The input file should be an Excel file (.xlsx or .xls) containing two sheets in the order specifed below and specific columns named and ordered EXACTLY as follows.

### Sheet 1: Standards

- **Column A**: The unit of measurement, i.e. pg/mL, ng/mL, umol/L, etc. 
- **Column B**: Absorbance

Example for Sheet 1:
| pg/mL | Absorbance |
|-------|------------|
| 2500  | 1.4817     |
| 1250  | 0.9027     |
| ...   | ...        |

This sheet should contain the calibration standards used in the ELISA assay. Each row represents a different standard sample with specified concentration and measured absorbance.

### Sheet 2: Results

- **Column A**: SampleID
- **Column B**: Absorbance
- **Column C**: Dilution

Example for Sheet 2:
| SampleID              | Absorbance | Dilution |
|-----------------------|------------|----------|
| Test_Sample_1         | 0.4773     | 40       |
| Test_Sample_2         | 0.5685     | 40       |
| ...                   | ...        | ...      |

This sheet should contain the experimental samples. Each row represents a different sample with a unique ID, its measured absorbance, and the dilution factor used. You must write the dilution factor as 40 if the dilution was 1:40, 10 if it was 1:10, etc. If you leave the Dilution column blank, the App will assume a dilution factor of 1. 

### Additional Requirements

- The file should not contain any merged cells.
- Ensure the first row in each sheet contains the column headers as described above.
- The sheets must be named according to their content (e.g., "Standards" and "Results").

## Using the App

1. **Navigate to the App URL**: https://nicholas-camarda.shinyapps.io/elisa_4pl/
2. **Upload Data**: Click the "Choose ELISA File" button and select your input Excel file.
3. **Enter the name of your Project**: This ensures that your files are named according to your project. All files will also receive a date stamp.
4. **Run Analysis**: After uploading the file and specifying a project name, click the "Run Analysis" button to start the processing.
5. **View Results**: View the visualization and preview of the analysis results.
6. **Download Outputs**: Use the provided buttons to download the plot and data files.

## Example Data

An example Excel file following the required structure is available in this repository: `example.xlsx`. You can use this file to test the functionality of the ELISA Analysis Tool.

## Support

If you encounter any issues or have suggestions for improvements, please open an issue in this GitHub repository.

## License

This project is licensed under the MIT License - see the LICENSE file for details.
