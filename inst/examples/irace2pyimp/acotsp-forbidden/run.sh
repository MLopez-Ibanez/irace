# create a directory for saving output
mkdir -p results

# example 1: convert irace.Rdata to PyImp's input format, without normalisation
#irace2pyimp --out-dir results --instance-feature-file features.csv --irace-data-file irace.Rdata --ignore-unsupported 0

# example 2: convert irace.Rdata to PyImp's input format, without normalisation, ignore forbidden constraints & repairConfiguration (irace2pyimp doesn't support those features), then call PyImp (executable name is "pimp") with all modules (fanova, abalation analysis, and forward selection)
irace2pyimp --out-dir results --instance-feature-file features.csv --irace-data-file irace.Rdata --ignore-unsupported 1
cd results
pimp -S scenario.txt -H runhistory.json -T traj_aclib2.json -M all
cd ..
