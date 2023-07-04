#!/usr/bin/env python3
###############################################################################
# This script is the command that is executed every run.
# Check the examples in examples/
#
# This script is run in the execution directory (execDir, --exec-dir).
#
# PARAMETERS:
# argv[1] is the candidate configuration number
# argv[2] is the instance ID
# argv[3] is the seed
# argv[4] is the instance name
# The rest (argv[5:]) are parameters to the run
#
# RETURN VALUE:
# This script should print one numerical value: the cost that must be minimized.
# Exit with 0 if no error, with 1 in case of error
###############################################################################

import datetime
import os.path
import re
import subprocess
import sys

## This a dummy example that shows how to parse the parameters defined in
## parameters.txt and does not need to call any other software.

if __name__=='__main__':
    if len(sys.argv) < 5:
        print("\nUsage: ./target-runner.py <configuration_id> <instance_id> <seed> <instance_path_name> <list of parameters>\n")
        sys.exit(1)

    # Get the parameters as command line arguments.
    configuration_id = sys.argv[1]
    instance_id = sys.argv[2]
    seed = sys.argv[3]
    instance = sys.argv[4]
    cand_params = sys.argv[5:]

    # Default values (if any)
    a = None
    b = None
    # Parse parameters
    
    while cand_params:
        # Get and remove first and second elements.
        param = cand_params.pop(0)
        value = cand_params.pop(0)
        if param == "--param1":
            a = float(value)
        elif param == "--param2":
            b = int(value)
        elif param == "--ptype":
            ptype = value
        else:
            target_runner_error("unknown parameter %s" % (param))
    
    # Sanity checks
    if a == None and b == None:
        target_runner_error("either a or b must be set, something is wrong!")
    
    if ptype == "a" and a == None:
        target_runner_error("ptype is 'a' but no value assigned to a, something is wrong!")
    
    if ptype == "b" and b == None:
        target_runner_error("ptype is 'b' but no value assigned to b, something is wrong!")

    a = a or 1
    b = b or 1
    
    print(str(a * b) + '\n')
    sys.exit(0)

# Useful function to print errors.
def target_runner_error(msg):
    now = datetime.datetime.now()
    print(str(now) + " error: " + msg)
    sys.exit(1)

