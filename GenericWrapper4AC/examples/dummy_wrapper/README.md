# Example call I)

`python examples/dummy_wrapper/dummy_wrapper.py --cutoff 12 --instance cost+time --seed 44 --config -cost 2 -runtime 5`

Expected output:

`Result of this algorithm run: {"status": "SUCCESS", "cost": 2.0, "runtime": 5.0, "misc": ""}`

# Example call II)

`python examples/dummy_wrapper/dummy_wrapper.py --cutoff 12 --instance time --seed 45 --config -cost 2 -runtime 50`

Expected output:

`Result of this algorithm run: {"status": "TIMEOUT", "cost": 12.0, "runtime": 12.0, "misc": ""}`

# Example call III)

`python examples/dummy_wrapper/dummy_wrapper.py --cutoff 12 --instance time --seed 44 --config -cost 20 -runtime 50`

Expected output:

`Result of this algorithm run: {"status": "SUCCESS", "cost": 12.0, "runtime": 12.0, "misc": ""}`

# Example call IV)

`python examples/dummy_wrapper/dummy_wrapper.py --cutoff 12 --instance time --seed 46 --config -cost 20 -runtime 50`

Expected output:

`Result of this algorithm run: {"status": "CRASHED", "cost": 12.0, "runtime": 12.0, "misc": "; Problem with run. Exit code was N/A.; Preserving runsolver output at XXX - preserving target algorithm output at XXX"}`

# Example call V)

`python examples/dummy_wrapper/dummy_wrapper.py --cutoff 12 --instance time --seed 47 --config -cost 20 -runtime 50`

Expected output:

`Result of this algorithm run: {"status": "ABORT", "cost": 12.0, "runtime": 12.0, "misc": "; Problem with run. Exit code was N/A.; Preserving runsolver output at XXX - preserving target algorithm output at XXX"}`

# Example call VI)

`python examples/dummy_wrapper/dummy_wrapper.py --cutoff 12 --instance cost --seed 48 --config -cost 20 -runtime 50`

Expected output:

`Result of this algorithm run: {"status": "SUCCESS", "cost": 20.0, "runtime": 0.000515, "misc": ""}`
