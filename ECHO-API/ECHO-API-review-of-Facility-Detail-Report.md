# ECHO API 'Facility Detail Report' - Data Summary
## Goals
- Understand what data are returned by the ['Facility Detail Report' API](https://echo.epa.gov/tools/web-services/detailed-facility-report).

- Explore example API data from carceral facilities (see section below).

## Background
We started exploring ECHO data via a flat ['ECHO Exporter'](https://echo.epa.gov/tools/data-downloads#exporter) file with summary data per facility. This data includes summaries without detail, e.g. the amount of total fines accrued under a given statute, without data on the individual fines.

What additional data are available in the ECHO 'Facility Detail Report', which returns all ECHO data for a facility, and are these data useful over and above the summary data?

## Results
Some API data of potential interest and/or data not included in the ECHO Exporter summary file are:

- data on specific instances of inspections/violations/legal cases/fines and settlements/measurement exceedences, etc. Most all of these data lack contextual narrative detail, and will need to be interpreted. 

- more data per statute, e.g. in addition to an overall summary of Clean Water Act compliance, the API returns granular categories like D80/D90 violations (failture to receive discharge monitoring reports).
- generally more specific and specialized data, e.g. links to detailed ['Compliance Assurance and Enforcement Division Documents'](https://ofmpub.epa.gov/apex/tocar/tocar/resp/attach/4674), data on 'sanitary surveys' ("including technical assistance, sample collections, and site inspections"), reports on compliance and emmissions related to boilers.
- information on water bodies that receive a facility's released pollutants.
- [EJSCREEN](https://github.com/Carceral-Ecologies/Carceral-EJSCREEN-data) national percentiles and demographics.
- time series data, covering a 13 quarter or 3 year period, for example compliance status by quarter (data is easier to interpret than in summary file, and more granular)

## Explore API Carceral Data
Via [this link](https://jsonformatter.org/json-pretty-print?url=https://raw.githubusercontent.com/Carceral-Ecologies/Carceral-ECHO-data/master/ECHO-API/ECHO-API-JSON-composite-results-annotated-w-rough-data-dictionary.txt) you can explore an example API data response (try the 'tree' view in the right-hand panel), collected from carceral facilities. Data definitions are included throughout entered under 'UCD/UCLA', some are speculative, drawn from the [ECHO web report data dictionary](https://echo.epa.gov/help/reports/dfr-data-dictionary) and a [2014 API dictionary](https://echo.epa.gov/system/files/ECHO-DFR_Rest_Services.pdf).

The example API response is a *composite* file with API data combined from several carceral facilities coded in the [FRS](https://www.epa.gov/frs) by NAICS/SIC code as correctional institutions. The API returns 46 top-level fields, and in general these fields may or may not contain data depending on the queried facility. To explore the fields here, several facilities' data are included, so that each top-level field has an example. Data within a top-level field (e.g. 'Permits') belong to a single facility, but use caution if making comparisons across top-level fields as data will likely come from different facilities. The example data for many top-level fields were chosen arbitrarily (vs searching for exemplary cases).

## Next Steps
Next we'll need to dig deeper on these data and work on what questions we might formulate for analysis.