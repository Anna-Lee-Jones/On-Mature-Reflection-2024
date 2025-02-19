# On mature reflection: ozone damage can be detected in oak trees by hyperspectral reflectance
![Heat Map of Normalised Indices correlation to ozone concentration](heat_map.png)
Code accompanying the paper  "On mature reflection: ozone damage can be detected in oak trees by hyperspectral reflectance" by Lee Jones et al, 2025, Ecological Indicators.
Datasets available at https://doi.org/10.5281/zenodo.13928876

## Script order: 

## 1). Process raw spectral data in Python
[Wytham Spectral Preprocessing Notebook](WYTHAM.ipynb)
[BIFoR Spectral Preprocessing Notebook](BIFOR.ipynb)

## 2). Merge spectral libraries
[Combine spectral libraries](Bifor_plus_Wytham.R) 

## 3). Analyse ozone monitoring data
[Ozone monitoring](Ozone_monitoring.R)

## 4). Principal component analysis of spectral data
[PCA](PCA_Wytham_Bifor.R)

## 5). Partial least squares regression analysis 
[PLSR](PLSR_Wytham_Bifor.R)

## 6). Vegetation indices analysis
[Vegetation Indices](VIs_Wytham_Bifor.R)

## 7). Normalised difference pearson analysis 
[NDSI Repo](https://github.com/AdamOrmondroyd/ndsi.git)


