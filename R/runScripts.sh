# Process raw data
Rscript Data/getData/getCoW_Ally.R
Rscript Data/getData/getCoW_IGO.R
Rscript Data/getData/getVoeten_UN.R

# Buld data for amen model, run with IGO separately because of temporal discrepancy
Rscript Data/buildAmenData.R
Rscript Data/buildAmenData_wIGO.R

# Run Amen model and latent space anlysis
Rscript Analysis/runYrlyAmen.R
Rscript Analysis/getLatDist.R
Rscript Analysis/runYrlyAmen_wIGO.R
Rscript Analysis/getLatDist_wIGO.R