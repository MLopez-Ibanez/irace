import os
from genericWrapper4AC.domain_specific.satwrapper import SatWrapper 


class MiniSATWrapper(SatWrapper):
    

    def get_command_line_args(self, runargs, config):
        '''
        @contact:    lindauer@informatik.uni-freiburg.de, fh@informatik.uni-freiburg.de
        Returns the command line call string to execute the target algorithm (here: Spear).
        Args:
            runargs: a map of several optional arguments for the execution of the target algorithm.
                    {
                      "instance": <instance>,
                      "specifics" : <extra data associated with the instance>,
                      "cutoff" : <runtime cutoff>,
                      "runlength" : <runlength cutoff>,
                      "seed" : <seed>
                    }
            config: a mapping from parameter name to parameter value
        Returns:
            A command call list to execute the target algorithm.
        '''
        solver_binary = os.path.join(os.path.dirname(__file__), "minisat")
        cmd = "%s -rnd-seed=%d" %(solver_binary, runargs["seed"])       
        for name, value in config.items():
            cmd += " %s=%s" %(name,  value)
        cmd += " %s" %(runargs["instance"])        
    
        return cmd
    
if __name__ == "__main__":
    wrapper = MiniSATWrapper()
    wrapper.main()  
