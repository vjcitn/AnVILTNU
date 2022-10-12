# BiocTNU

The [DataBiosphere project](https://www.databiosphere.org) includes a vision

![schema](https://raw.githubusercontent.com/vjcitn/BiocTNU/main/databiosfig.jpg)

of which AnVIL/Terra forms a part.

The [terra-notebook-utils python modules](https://github.com/DataBiosphere/terra-notebook-utils)
is described as a "Python API and CLI providing utilities for working with DRS objects, VCF files, and the Terra notebook environment."

This R package aims to provide a regulated interface between R and terra-notebook-utils
for use in AnVIL.  By "regulated" we mean that the entire python ecosystem used
to work with terra-notebook-utils is defined in a virtual environment.


Note: in AnVIL, a .Renviron must have PIP_USER=false for proper installation with basilisk
