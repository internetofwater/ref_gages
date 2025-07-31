# Reference stream gages for geoconnex.us

This repository houses workflow software for compilation of community reference stream gages. The output of this workflow will generate Persistent Identifiers for the [geoconnex.us system](https://github.com/internetofwater/geoconnex.us), reference landing page content for the [reference.geoconnex.us system](https://reference.geoconnex.us/), and a set of best available network locations for the [Network Linked Data Index](https://labs.waterdata.usgs.gov/about-nldi/index.html).

These reference locations are intended to be a shared community resource that anyone can contribute to. If a reference location does not exist and data are collected there, the location can and should be contributed such that others can reference the location.

Workflow:

The diagram below illustrates the overall workflow implemented in this repository.

The workflow exists in four phases. 

1. load data and update registry
1. establish unique and reference spatial locations
1. establish hydrologic locations
1. write reference output

Unique logic is introduced for each provider in some cases.

```mermaid
graph LR
  style Graph fill:#FFFFFF,stroke:#000000;
  subgraph Graph
    direction LR
    x68fd7d2729e8459c(["cdec_gage"]):::uptodate --> x14690c3296417993(["gage_locations"]):::uptodate
    xc02b3fa6e288c95f(["co_gage"]):::uptodate --> x14690c3296417993(["gage_locations"]):::uptodate
    x26dbd46269195f71(["nwis_gage"]):::uptodate --> x14690c3296417993(["gage_locations"]):::uptodate
    x8221330b9bd19285(["pnw_gage"]):::uptodate --> x14690c3296417993(["gage_locations"]):::uptodate
    xa5c0bb03406aa7a0(["streamstats_sites"]):::uptodate --> x14690c3296417993(["gage_locations"]):::uptodate
    x68fd7d2729e8459c(["cdec_gage"]):::uptodate --> x75417e6b8a919cb9(["cdec_gage_address"]):::uptodate
    x75417e6b8a919cb9(["cdec_gage_address"]):::uptodate --> x9f75c3a258e544f6(["gage_hydrologic_locations"]):::uptodate
    x2c545b6cd69230ad(["co_gage_address"]):::uptodate --> x9f75c3a258e544f6(["gage_hydrologic_locations"]):::uptodate
    x14690c3296417993(["gage_locations"]):::uptodate --> x9f75c3a258e544f6(["gage_hydrologic_locations"]):::uptodate
    xcd07b5771d79298c(["nhdpv2_fline_proc"]):::uptodate --> x9f75c3a258e544f6(["gage_hydrologic_locations"]):::uptodate
    xcf9c4895c540caf6(["qa_gages"]):::uptodate --> x9f75c3a258e544f6(["gage_hydrologic_locations"]):::uptodate
    x851c6bba0e062390(["ref_locations"]):::uptodate --> x9f75c3a258e544f6(["gage_hydrologic_locations"]):::uptodate
    x2aa750ef5905719e(["duplicate_locations"]):::uptodate --> x96d1384a6f25eeca(["reference_out"]):::uptodate
    xfe50b36338a451f2(["gage_hydrologic_locations_with_mainstems"]):::uptodate --> x96d1384a6f25eeca(["reference_out"]):::uptodate
    xf515f77fccd1b2cc(["providers"]):::uptodate --> x96d1384a6f25eeca(["reference_out"]):::uptodate
    xa5d81f243d49b74b(["registry"]):::uptodate --> x96d1384a6f25eeca(["reference_out"]):::uptodate
    xfe50b36338a451f2(["gage_hydrologic_locations_with_mainstems"]):::uptodate --> x2aa750ef5905719e(["duplicate_locations"]):::uptodate
    x14690c3296417993(["gage_locations"]):::uptodate --> xa5d81f243d49b74b(["registry"]):::uptodate
    xf515f77fccd1b2cc(["providers"]):::uptodate --> xa5d81f243d49b74b(["registry"]):::uptodate
    xa5d81f243d49b74b(["registry"]):::uptodate --> x3c26915feb6d5453(["registry_out"]):::uptodate
    xfe50b36338a451f2(["gage_hydrologic_locations_with_mainstems"]):::uptodate --> x9423f5225a1fe63d(["usgs_reference_out"]):::uptodate
    xf515f77fccd1b2cc(["providers"]):::uptodate --> x9423f5225a1fe63d(["usgs_reference_out"]):::uptodate
    xa5d81f243d49b74b(["registry"]):::uptodate --> x9423f5225a1fe63d(["usgs_reference_out"]):::uptodate
    x14690c3296417993(["gage_locations"]):::uptodate --> x851c6bba0e062390(["ref_locations"]):::uptodate
    xf515f77fccd1b2cc(["providers"]):::uptodate --> x851c6bba0e062390(["ref_locations"]):::uptodate
    x9f75c3a258e544f6(["gage_hydrologic_locations"]):::uptodate --> xfe50b36338a451f2(["gage_hydrologic_locations_with_mainstems"]):::uptodate
    x62f64b5e1492705f(["mainstems"]):::uptodate --> xfe50b36338a451f2(["gage_hydrologic_locations_with_mainstems"]):::uptodate
    xcaf37b018c5051fa(["nws_gages"]):::uptodate --> xfe50b36338a451f2(["gage_hydrologic_locations_with_mainstems"]):::uptodate
    x3e10572866348fbd(["vaa"]):::uptodate --> xfe50b36338a451f2(["gage_hydrologic_locations_with_mainstems"]):::uptodate
    xc02b3fa6e288c95f(["co_gage"]):::uptodate --> x2c545b6cd69230ad(["co_gage_address"]):::uptodate
    xaa36b9c75449da58(["nhdpv2_fline"]):::uptodate --> xcd07b5771d79298c(["nhdpv2_fline_proc"]):::uptodate
    x96d1384a6f25eeca(["reference_out"]):::uptodate --> xd1529679f089b8ed(["validation"]):::uptodate
    x3c26915feb6d5453(["registry_out"]):::uptodate --> xd1529679f089b8ed(["validation"]):::uptodate
    x17318bfe07205f04(["providers_csv"]):::uptodate --> xf515f77fccd1b2cc(["providers"]):::uptodate
    xcc0828d76b923a09(["nat_db"]):::uptodate --> xaa36b9c75449da58(["nhdpv2_fline"]):::uptodate
  end
  classDef uptodate stroke:#000000,color:#000000,fill:#85F1FF;
```

# Architecture

This project exists in a linked data architecture that relies on Web uniform resource identifiers (URIs) for both digital and real world entities. There are three types of resources in the architecture:
1. Real-world monitoring locations identified by a so-called "non-information URI".
1. Information about the real world monitoring locations identified by a URL that is the target of a redirect from a non-information URI.
1. A particular organization's information available for the real world monitoring location.

In practice, these urls will look like: 
1. `https://geoconnex.us/ref/gages/1071321` (which will redirect to 2)
1. `https://reference.geoconnex.us/collections/gages/items/1071321` (which will provide information about the reference location, including a link to 3)
1. `https://waterdata.usgs.gov/monitoring-location/02469600` (which is the USGS's site information for this particular reference location.)

It is important to maintain this separation because no one organization, other than a community organization set up to fulfill this role, can be expected to be both the community reference catalog and a provider of their own information.  

# Project structure

- `/R` functions defined for this project.
- `/data` data downloaded for this project.
- `/temp` temporary output that may be of interest for debugging.
- `/out` output to be contributed elsewhere. 
- `/reg` registry of gages tracked in source control.
- `/docs` contains artifacts to be served via github.io

# Contributing

First, thank you for considering a contribution! For this to work, everyone with unique monitoring locations need to be willing to contribute those locations here. 

This is a new project and, as such, exactly how contributions are made will be flexible and a work in progress. If you have locations to add, just reach out [in the issues](https://github.com/internetofwater/ref_gages/issues) and/or submit a pull request. The maintainer(s) are more than happy to coordinate and do whatever legwork is needed to get new reference locations into the registry.

As time goes on and the nature of contributions becomes more clear, this guidance will become more specific, but until then, just get in touch and we'll work together.

## Disclaimer

This information is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The information has not received final approval by the U.S. Geological Survey (USGS) and is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the information.

This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey  (USGS), an agency of the United States Department of Interior. For more information, see the official USGS copyright policy at [https://www.usgs.gov/visual-id/credit_usgs.html#copyright](https://www.usgs.gov/visual-id/credit_usgs.html#copyright)

Although this software program has been used by the USGS, no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."

 [
    ![CC0](https://i.creativecommons.org/p/zero/1.0/88x31.png)
  ](https://creativecommons.org/publicdomain/zero/1.0/)
