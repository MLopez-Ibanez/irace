# create a directory for saving output
mkdir -p results

# example of using irace2pyimp with filter conditions and normalisation:
# - filter tuning data such that only configurations with n_templates_middle<=40 are used, then convert irace.Rdata to PIMP's input format, 
# - the cost values of all configurations are normalised on an instance-basis
irace2pyimp --out-dir results --normalise instance --filter-conditions "n_templates_middle<=40" --irace-data-file irace.Rdata

# call PyImp (executable name is "pimp") with two modules: forward-selection & ablation analysis
cd results
pimp -S scenario.txt -H runhistory.json -M forward-selection 
pimp -S scenario.txt -H runhistory.json -M ablation
cd ..
