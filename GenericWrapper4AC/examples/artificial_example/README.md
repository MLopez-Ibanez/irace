# Test Wrapper 

This is a basic example for a simple artificial function to show the features of the generic wrapper.
Furthermore, this example should be useful for testing the generic wrapper and compatible algorithm configurators.

The target algorithm is a simple python script `target_algorithm.py` 
which parses three parameters. In the pcs format, this would correspond to:

```
str_param categorical {str1,str2}[str2]
int_param integer [1,100][1]
float_param float [0,1][0.1]
```

The script supports pseudo instances in the format `inst:<hardness>`, where `hardness` is parsed as an integer and multiplied to the final result.

The random seed is used for letting the target algorithm sleep for `int(seed)/100` seconds. 
Since it is only a sleep and the generic wrapper measures CPU time by default,
the random see will change the wallclock time, but not the returned CPU time.

The `wrapper.py` implements the required two functions:

  1. `get_command_line_args()` that generates the command line call by starting with the call of the target algorithm script, adds the random seed as the second parameter and adds all parameters to the command line call
  1. `process_results()` reads only the printed value from the `target_algorithm.py` script and returns it as the quality of the function.
  
An example call of the wrapper would be:

`python examples/artificial_example/wrapper.py --seed 10 --instance inst:10 --config -float_param 0.1 -int_param 2 -str_param str2`

which is translated to 

`python examples/artificial_example/target_algorithm.py inst:10 10 float_param 0.1 int_param 2 str_param str2`

The expected output should include a line such as:

`Result of this algorithm run: {"status": "SUCCESS", "cost": 41.0, "runtime": 0.038031, "misc": ""}`

where the measured CPU time can vary slightly.