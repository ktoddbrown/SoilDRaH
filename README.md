# Soil-DRaH

## Overview {#overview}

The Soil Data Rescue and Harmonization (Soil-DRaH) project is a collaborative effort to rescue, harmonize, and curate soil data from multiple sources.

``` mermaid
flowchart LR
    A[data] & B[annotation]
    -->id1([read])
    -->id2([pivot])
    -->C[data+metadata]
    -->id3([pivot])
    -->id4{{curation}}
```

## Table of Contents

-   [Overview](#overview)
-   [Data Sources](#data-sources)
-   [Installation](#installation)
-   [Usage](#usage)
-   [License](#license)


## Data Sources {#data-sources}
Soil-DRaH is currently working with the following data sources:

- [CPEAT](https://www.pangaea.de/?q=project%3Alabel%3APAGES_C-PEAT)
- [FIA](https://apps.fs.usda.gov/fia/datamart/datamart.html)
- [ISCN3](http://iscn.fluxdata.org/data/access-data/database-reports/)


## Installation {#installation}


## Usage {#usage}

**Helper scripts**

1)  [Create annotation skeleton from tables]("R/data-templateAnnotations.R")
2)  [Check annotation for compliance against yaml]("R/checkAnnotations.R")


## License {#license}

[MIT License](LICENSE.md)
