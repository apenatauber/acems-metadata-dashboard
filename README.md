### ACEMS Metadata Dashboard

This repository is a copy of a final project from STAT-231, in which we created a dashboard for visualizing geographic and calendar-time data for calls received by ACEMS, the EMS service running on Amherst College campus. It was created by Andrés Peña-Tauber '23, Reihaneh Iranmanesh '25, and Alex Hartwich '23 using [https://www.rstudio.com/products/shiny/](Shiny), a powerful app for making visual web apps in R.

The dashboard uses a "freeze" of data from when call metadata was available starting 2016 to when it was downloaded as an Excel file and manually imported. Thus, it does not automatically update with new calls. Further, data needed to be manually cleaned and categorized. While helpful for past data analytics and perhaps categorizing data in various different ways, it may not be the best data pipeline to have a reliable tool to analyze data from newer time periods. It would need to be automated to serve this purpose.

The two tabs of the dashboard display (1) geographic ("Map") and (2) temporal ("Bar Chart") data. The geographic tab could be useful for understanding where calls are most likely to come in from, as well as what chief complaints and categories thereof are most common in each area of campus. The temporal data could serve to illustrate how call volumes and chief complaints differ in each month or semester for a given academic year. It does not currently normalize by actual academic days in a given time period, which could be a confounding factor for call volume, especially when comparing between months.

The web app is currently accessible through [https://apenatauber.shinyapps.io/acems-metadata-hq/](my personal Shinyapps.io account).

### File directory structure

`app` contains the Shiny app that is loaded onto the web. `app.R` is the code for running the Shiny app, `data` contains the raw call data files as well as wrangling steps, and `www` contains images to load onto the app.

`data-wrangling.Rmd` and `data-wrangling.pdf` explain the process for taking the input (raw) call metadata table downloaded from the ACEMS Board Drive and processing it for input into the app.
