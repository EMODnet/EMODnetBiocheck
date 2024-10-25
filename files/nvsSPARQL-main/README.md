# nvsSPARQL :: Query NVS using sparql 

## intro
This repo contains some examples of how to use SPARQL to query the [NVS vocab server](https://vocab.nerc.ac.uk/)

## python users

### basic
If you choose to work  with pyhton please install the [sparqlwrapper](https://rdflib.github.io/sparqlwrapper/) with this command:

``` bash
pip install SPARQLWrapper
```

Check the examples in the folder `./pythonSPARQL`

### convenience
Alternatively you can work with the [pykg2tbl](https://pypi.org/project/pykg2tbl/) library. This includes the previous one, but adds some convenience:

* removes the embedded sparql from your py code into separate files that support templated - generative patterns that avoid repeating yourself
* exports results to pandas dataframes for further analysis
* allows to test queries versus RDF dump files as well (not only remote SPARQL endpoints)

So, if this sounds like you, then go for this install:

``` bash
pip install pykg2tbl
```

And check out these ipynb examples in the `./templated-queries` folder:
| notebook                         | showcasing                                                                                     | used templates                                                  |
|----------------------------------|------------------------------------------------------------------------------------------------|-----------------------------------------------------------------|
| 01-listing-collections-and-terms | some standard lookups of collections and terms within them                                     | nsv-list-collections, nsv-listing, nsv-find  |
| 02-eov-matching                  | how to use both the puv and iop crosswalks between EOV (A05) and applicable measurements (P01) | nsv-eov-to-usage_via-iop/puv|
