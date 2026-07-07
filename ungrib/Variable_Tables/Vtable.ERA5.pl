GRIB1| Level| From |  To  | metgrid  | metgrid  | metgrid                                  |GRIB2|GRIB2|GRIB2|GRIB2|
Param| Type |Level1|Level2| Name     | Units    | Description                              |Discp|Catgy|Param|Level|
-----+------+------+------+----------+----------+------------------------------------------+-----+-----+-----+-----+
 129 | 100  |   *  |      | GEOPT    | m2 s-2   |                                          |   0 |   3 |   4 | 100 |
 156 | 100  |   *  |      | HGT      | m        | Geopotential Height                      |   0 |   3 |   5 | 100 |
 130 | 100  |   *  |      | TT       | K        | Temperature                              |   0 |   0 |   0 | 100 |
 131 | 100  |   *  |      | UU       | m s-1    | U                                        |   0 |   2 |   2 | 100 |
 132 | 100  |   *  |      | VV       | m s-1    | V                                        |   0 |   2 |   3 | 100 |
 157 | 100  |   *  |      | RH       | %        | Relative Humidity                        |   0 |   1 |   1 | 100 |
 165 |  1   |   0  |      | UU       | m s-1    | U                    At 10 m             |   0 |   2 |   2 | 103 |
 166 |  1   |   0  |      | VV       | m s-1    | V                    At 10 m             |   0 |   2 |   3 | 103 |
 167 |  1   |   0  |      | TT       | K        | Temperature          At  2 m             |   0 |   0 |   0 | 103 |
 168 |  1   |   0  |      | DEWPT    | K        |                                          |   0 |   0 |   6 | 103 |
     |  1   |   0  |      | RH       | %        | Relative Humidity    At  2 m             |   0 |   1 |   1 | 103 |
 172 |  1   |   0  |      | LANDSEA  | 0/1 Flag | Land/Sea Flag                            |   2 |   0 |   0 |   1 |
 129 |  1   |   0  |      | SOILGEO  | m2 s-2   |                                          |   0 |   3 |   4 | 100 |
 156 |  1   |   0  |      | SOILHGT  | m        | Geopotential Height - Surface            |   0 |   3 |   5 | 100 |
 134 |  1   |   0  |      | PSFC     | Pa       | Surface Pressure                         |   0 |   3 |   0 |   1 |
 151 |  1   |   0  |      | PMSL     | Pa       | Sea-Level Pressure                       |   0 |   3 |   0 | 101 |
 235 |  1   |   0  |      | SKINTEMP | K        | Earth-Surface Temperature                |   0 |   0 |  17 |   1 |
  31 |  1   |   0  |      | SEAICE   | 0/1 Flag | Sea-Ice-Flag                             |  10 |   2 |   0 |   1 |
  34 |  1   |   0  |      | SST      | K        | Sea-Surface Temperature                  |  10 |   3 |   0 |   1 |
 141 |  1   |   0  |      | SNOW_EC  | m        |                                          |   0 |   1 | 254 | 101 |
     |  1   |   0  |      | SNOW     | kg m-2   | Water Equivalent of Snow Depth           |     |     |     |     |
 139 | 112  |   0  |   7  | ST000007 | K        | T of 0-7 cm ground layer                 | 192 | 128 | 139 | 106 |
 170 | 112  |   7  |  28  | ST007028 | K        | T of 7-28 cm ground layer                | 192 | 128 | 170 | 106 |
 183 | 112  |  28  | 100  | ST028100 | K        | T of 28-100 cm ground layer              | 192 | 128 | 183 | 106 |
 236 | 112  | 100  | 255  | ST100289 | K        | T of 100-289 cm ground layer             | 192 | 128 | 236 | 106 |
  39 | 112  |   0  |   7  | SM000007 | fraction | Soil moisture of 0-7 cm ground layer     | 192 | 128 |  39 | 106 |
  40 | 112  |   7  |  28  | SM007028 | fraction | Soil moisture of 7-28 cm ground layer    | 192 | 128 |  40 | 106 |
  41 | 112  |  28  | 100  | SM028100 | fraction | Soil moisture of 28-100 cm ground layer  | 192 | 128 |  41 | 106 |
  42 | 112  | 100  | 255  | SM100289 | fraction | Soil moisture of 100-289 cm ground layer | 192 | 128 |  42 | 106 |
-----+------+------+------+----------+----------+------------------------------------------+-----+-----+-----+-----+
#
# This Vtable is designed to be used with the ERA5 data provided on single levels and pressure levels by the
# Climate Data Store (CDS) at the following 2 links:
# Single Level Data: https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels?tab=overview
# Pressure Level Data: https://cds.climate.copernicus.eu/datasets/reanalysis-era5-pressure-levels?tab=overview
#
# Grib codes are from ECMWF Parameter Database: https://codes.ecmwf.int/grib/param-db/
# Note that some of the above parameters do not have a description written in the "metgrid Description" column
# because we do not want to write these fields to the intermediate files.
#
# In rrpr.F:
# "HGT" is calculated from "GEOPT"
# "SOILHGT" is calculated from "SOILGEO"
# "RH" (at 2 m) is calculated from "DEWPT"
# "SNOW" is calculated from "SNOW_EC"
#
# ERA5 is based on 4D-Var data assimilation using Cycle 41r2 of the Integrated Forecasting System (IFS),
# which was operational at ECMWF in 2016.
#
# The names of all the parameters listed in the table above (that need to be downloaded from the CDS) have the
# following human readable names associated with their ID (first column) in the CDS:
#
# Pressure Levels:
# 129 = Geopotential
# 130 = Temperature
# 131 = U-component of wind
# 132 = V-component of wind
# 157 = Relative humidity
#
# Single Levels:
# 165 = 10m u-component of wind
# 166 = 10m v-component of wind
# 167 = 2m temperature
# 168 = 2m dewpoint temperature
# 172 = Land-sea mask
# 129 = Geopotential
# 134 = Surface pressure
# 151 = Mean sea level pressure
# 235 = Skin temperature
#  31 = Sea-ice cover
#  34 = Sea surface temperature
# 141 = Snow depth
# 139 = Soil temperature level 1
# 170 = Soil temperature level 2
# 183 = Soil temperature level 3
# 236 = Soil temperature level 4
#  39 = Volumetric soil water layer 1
#  40 = Volumetric soil water layer 2
#  41 = Volumetric soil water layer 3
#  42 = Volumetric soil water layer 4
