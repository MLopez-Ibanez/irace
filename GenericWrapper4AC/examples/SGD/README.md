#Wrapper for SGD

This is a more advanced example for optimizing a black box function (here SGD on a fixed data set) which has an additional requirement: scikit-learn.

The target algorithm is a simple python script `sgd_ta.py` which loads the iris data set from scikit-learn, reads the parameter configuration, fits a SGD classifier and prints the accuracy on a holdout set.

The script supports three kind of "instances":

  1. if the instance is "train", we split the data into train, validation and test set, and train SGD on train and evaluate it on valid
  2. if the instance is "test", we split the data into train (train+validation) and test set, and train SGD on train and evaluate it on test
  3. if the instance is "cvX", we split the data into train and test, and further use a k-fold CV on train; the X-th split is used for training and evaluation   

The `SGDWrapper.py` implements the required two functions:

  1. `get_command_line_args()` that generates the command line call by starting with the call of the sgd script, adds the random seed as a parameter called `random_state` (as done in sklearn) and adds all parameters to the command line call
  1. `process_results()` reads only the printed accuracy from the `sgd_ta.py` script and returns it as the quality of the function.
  
An example call of the wrapper would be:

`python examples/SGD/SGDWrapper.py train 0 5 0 9 -learning_rate constant -eta0 1 -loss hinge -penalty l2 -alpha 0.0001 -learning_rate optimal -eta0 0.0 -n_iter 2`

which is translated to 

`python examples/SGD/sgd_ta.py train random_state 9 learning_rate optimal eta0 0.0 loss hinge penalty l2 alpha 0.0001 n_iter 2`


  