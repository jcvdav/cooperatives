# cooperatives

Research on cooperative vulnerability to Climate Change

# Structure of the repository

```
cooperatives:
|
|-- cooperatives.Rproj"            
|-- data"                          
|   |__clean_cooperatives_data.csv"
|   |__tsdiff.rds"                 
|-- docs"                          
|   |__analysis.html"              
|   |__analysis.Rmd"               
|-- raw_data"                      
|   |__cooperative_coordinates.csv"
|   |__CoopResilience.csv"         
|   |__fishtype.csv"               
|   |__Jones_Cheung_SDATA.csv"     
|   |__spp_by_cooperative.csv"     
|   |__spp_sci_name.csv"           
|-- README.md"                     
|-- scripts"                       
|   |__change_in_temperature.R" 
```

## Data sources:

- Cooperative database from [Ovando et al. 2012](https://www.sciencedirect.com/science/article/pii/S0308597X12000565?via%3Dihub)
- Sea Surface Temperature images come from [NASA](https://cds.nccs.nasa.gov/nex-gddp/)
  - Full ftp BCSD/rcp45/day/atmos/tasmax/r1i1p1 source [here](ftp://ftp.nccs.nasa.gov/BCSD/rcp45/day/atmos/tasmax/r1i1p1/v1.0/)