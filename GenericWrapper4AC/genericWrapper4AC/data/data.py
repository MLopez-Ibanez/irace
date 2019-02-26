'''
@author:     Marius Lindauer  
@copyright:  2018 ML4AAD. All rights reserved.
@license:    BSD
'''

class Data(object):

    def __init__(self):

        self.instance = None
        self.specifics = None
        self.cutoff = None
        self.runlength = None
        self.seed = None
        self.config = []

        # return values
        self.status = "CRASHED"
        self.cost = 2**32 - 1
        self.time = 0
        self.additional = ""
        self.exit_code = 0

        # call arguments
        self.runsolver = None
        self.tmp_dir = None
        self.mem_limit = None
        self.max_quality = 2**32 - 1

        self.new_format = False
