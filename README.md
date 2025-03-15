# Weather Analysis and Prediction - Shiny App

## 📌 Overview
This interactive **Shiny app** provides comprehensive **historical weather analysis** and **weather forecasting** based on user inputs. The app allows users to analyze past weather trends, visualize key weather parameters, and generate AI-driven insights. Additionally, users can forecast weather conditions for up to three days ahead, helping them make informed decisions.

## 🌟 Features

### 1️⃣ Historical Weather Analysis
- Users can input a **start date, end date, and city name** to analyze past weather data.
- Generates interactive graphs displaying:
  - **Max Temperature**
  - **Min Temperature**
  - **Rainfall**
  - **Max Wind Speed**
  - **Average Temperature**
- Provides summarized statistics:
  - **Average Temperature**
  - **Maximum Temperature**
  - **Minimum Temperature**
  - **Total Rainfall**
- AI-powered **Weather Analysis Report** including:
  - **Temperature patterns and variations**
  - **Wind impact analysis**
  - **Precipitation insights**
  - **Potential risks (heatwave, drought, fog, fire hazards)**
  - **Natural calamity risk assessment**
  - **Actionable recommendations**

### 2️⃣ Weather Forecasting
- Users can enter a **city name** and select forecast duration:
  - **Next Day**
  - **Next 2 Days**
  - **Next 3 Days**
- Generates graphs displaying historical trends (last 3 days) and predicted weather conditions.
- Includes AI-generated **forecast insights** similar to historical analysis.

### 3️⃣ Interactive Graphs with AI Insights
- Graphs are fully **interactive**, allowing users to explore trends dynamically.
- **Lasso Selection** feature:
  - Users can select any random region of the graph.
  - AI generates insights specifically for the selected range.

## 🛠️ Technologies Used
- **R** and **Shiny** for building the interactive web application.
- **ggplot2, plotly** for visualization.
- **tidyverse** for data processing.
- **API Integration** for fetching real-time weather data.
- **AI Models** for generating weather insights and recommendations.

## 🚀 Installation
1. **Clone the repository:**
   ```sh
   git clone https://github.com/parth1006/Weather-Analysis.git
    cd Weather-Analysis

   ```
2. **Run the Shiny app:**
   ```r
   library(shiny)
   runApp("finalAPP.R")
   ```
## ▶️ Running the Application
To run the app, execute the following command in your R console:
```r
shiny::runApp()
```
Alternatively, if the app is structured in a separate `app.R` file, use:
```r
Rscript finalAPP.R
```
## 📷 Screenshots
- **Home Screen**
![image](https://github.com/user-attachments/assets/3a309e43-4a00-41f0-ba92-628d2362f3e1)
- **Historical analysis screen with output**
![image](https://github.com/user-attachments/assets/c26d4eae-8c46-43c6-97fb-463241d3b4a4)
- **lasso selection and results**
  ![image](https://github.com/user-attachments/assets/26daf0eb-f1d1-41c7-a324-45b1ef17e1c1)
  ![image](https://github.com/user-attachments/assets/feb42be0-405d-4dbe-a887-70ea30918253)
- **Weather forecasting with output**
  ![image](https://github.com/user-attachments/assets/4697b1f3-551f-4507-87e0-39f202859a4f)



## 🤝 Contributing
Contributions are welcome! If you'd like to improve the app, feel free to submit a pull request or report issues.


