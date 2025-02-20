# Norwich Science Festival 2025: Speedy Skyscrapers - Data Collection and Visualization App
## University of East Anglia School of Biological and Environmental Sciences Personal and Professional Development Club

### SciComms group Members
- Katie Farrow (BIO PGT Student) - Shiny app developer
- Imie James (BIO UG Student)
- Rosie Gibbs (BIO UG Student)
- Elisabeth Odgers Orford (BIO UG Student)
- Sathvik Surapaneni (BIO UG Student) 
- Freddie Marlowe (BIO UG Student) - Shiny app developer
- Emily Chiu (BIO UG Student) - Shiny app developer
#  
This repository contains the code for an interactive R Shiny application used to collect and visualize data during the "Speedy Skyscrapers" activity at the Norwich Science Festival 2025. The activity involved participants building towers from blocks, and the app was used to record and analyze data related to their tower-building attempts.

### Project Description
The "Speedy Skyscrapers" activity aimed to explore whether age influences tower-building techniques. Participants were challenged to build the tallest stable tower possible using 10 blocks. The Shiny app was designed to:
* Collect data on the number of blocks used (tower height), participant age group, and favourite colour.
* Provide real-time visualization of the collected data through an interactive scatter plot.
* Store the data in a CSV file for further analysis.

### Features

* **Interactive Data Input:** User-friendly interface with sliders and radio buttons for data entry.
* **Real-time Visualization:** Dynamic scatter plot displaying tower height against age group, colored by favourite colour.
* **Data Persistence:** Data is stored in a CSV file, ensuring no data loss.
* **Custom Styling:** The app features a custom theme using Norwich City Football Club colours and a typewriter font.
* **Interactive Plotly Plot:** The scatter plot is interactive using the plotly library.
* **Data tooltips:** The plotly plot displays data point information when hovered over.
* **Responsive Design:** The plotly plot is set to autosize.
#
### Getting Started
**Prerequisites**

* R (version 4.0 or later)
* RStudio (recommended)
* The following R packages:
    * `shiny`
    * `tidyverse`
    * `plotly`

**Installation**

1.  Clone the repository to your local machine:

    ```bash
    git clone [repository URL]
    ```

2.  Open the `ui_nsf25_3.R` file in RStudio.

3.  Install the required R packages if you haven't already:

    ```R
    install.packages(c("shiny", "tidyverse", "plotly"))
    ```

**Running the App**
1.  In RStudio, click the "Run App" button at the top of the `ui_nsf25_3.R` script.

2.  The Shiny app will open in your web browser.

**Usage**

1.  Use the slider to select the number of blocks used in the tower.

2.  Select the participant's age group using the radio buttons.

3.  Select the participant's favorite color using the radio buttons.

4.  Click the "Plot Data" button to add the data point to the scatter plot and save it to the CSV file.

5.  Observe the real-time updates to the scatter plot.

**Data**

The collected data is stored in the `data/nsf2025_data_collection_1.csv` file.
