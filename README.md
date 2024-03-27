# Soil-DRaH

Soil Data Rescue and Harmonization:

``` mermaid
sequenceDiagram;
  participant annotation;
  participant read;
  participant shoestring;
  participant curate;
  annotation->>read;
  read->>shoestring;
  shoestring->>curate;
```

Helper scripts

  1) create annotation skeleton from tables 
  2) check annotation for compliance against yaml
