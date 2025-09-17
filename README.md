# Soil-DRaH

The Soil Data Rescue and Harmonization (Soil-DRaH) project is a collaborative effort to rescue, harmonize, and curate soil data from multiple sources.

``` mermaid
flowchart LR
    remote['archived data'] --download--> lvl0['Level 0 data']
    remote --'create'--> meta['data annotations']
    lvl0 --pivot--> lvl1['Level 1 data']
    meta --join--> lvl1
    lvl1 --curate--> lvl2['Level 2 data']
    lvl1a[['Other level 1 data']] --curate--> lvl2
```

## Table of Contents

-   [Data Sources](#data-sources)
-   [Installation](#installation)
-   [Usage](#usage)
-   [License](#license)


## Data Sources

Soil-DRaH is currently working with the following data sources:

- [CPEAT](https://www.pangaea.de/?q=project%3Alabel%3APAGES_C-PEAT)
- [FIA](https://apps.fs.usda.gov/fia/datamart/datamart.html)
- [ISCN3](http://iscn.fluxdata.org/data/access-data/database-reports/)
- [NRCS-NCSS (Web of Soil version)](https://websoilsurvey.nrcs.usda.gov/app/WebSoilSurvey.aspx)

Additional data sources may be suggested in the Issues.
Please see [these instructions](https://github.com/ktoddbrown/SoilDRaH/wiki/Open-Ticket) for how to open a new issue to suggest additional data sources.

## Installation

This is currently not a functional R package and the scripts must be run directly in R.
Please fork this repository to work with the functions created here.

## Usage

Each data source has an associated `read` function in the R folder.
In addition there is an article under `vignettes/articles` that gives an example of the use of the `read` function in the context of a data workflow.

The data workflow is further described in the [wiki](https://github.com/ktoddbrown/SoilDRaH/wiki)

## License

[MIT License](LICENSE)
