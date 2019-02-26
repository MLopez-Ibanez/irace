import unittest
import sys
import os

from examples.MiniSAT.MiniSATWrapper import MiniSATWrapper
from examples.SGD.SGDWrapper import SGDWrapper


class TestCalls(unittest.TestCase):

    def setUp(self):
        self.runsolver = os.path.join(
            os.path.dirname(os.path.dirname(__file__)),
            "test_binaries", "runsolver")

    def test_minisat_old(self):

        wrapper = MiniSATWrapper()

        sys.argv = "examples/MiniSAT/SATCSSCWrapper.py examples/MiniSAT/gzip_vc1071.cnf SAT 10 0 42 -rnd-freq 0 -var-decay 0.001 -cla-decay 0.001 -gc-frac 0.000001 -rfirst 1000"
        sys.argv += " --runsolver-path " + self.runsolver
        sys.argv = sys.argv.split(" ")

        wrapper.main(exit=False)

        self.assertEqual(wrapper.data.status, "SUCCESS")
        self.assertGreater(2, wrapper.data.time)
        self.assertEqual(wrapper.data.seed, 42)
        self.assertEqual(wrapper.data.runlength, 0)
        self.assertFalse(wrapper.data.new_format)
        
    def test_minisat_new(self):

        wrapper = MiniSATWrapper()

        sys.argv = "python examples/MiniSAT/SATCSSCWrapper.py --instance examples/MiniSAT/gzip_vc1071.cnf --cutoff 10 --seed 42 --config -rnd-freq 0 -var-decay 0.001 -cla-decay 0.001 -gc-frac 0.000001 -rfirst 1000"
        sys.argv += " --runsolver-path " + self.runsolver
        sys.argv = sys.argv.split(" ")

        wrapper.main(exit=False)

        self.assertEqual(wrapper.data.status, "SUCCESS")
        self.assertGreater(2, wrapper.data.time)
        self.assertEqual(wrapper.data.seed, 42)
        # important hack for irace that cost and time is equal
        # if cost was not set
        self.assertEqual(wrapper.data.cost, wrapper.data.time)
        self.assertIsNone(wrapper.data.runlength)
        self.assertTrue(wrapper.data.new_format)
        
    def test_sgd_old(self):

        wrapper = SGDWrapper()

        sys.argv = "examples/SGD/SGDWrapper.py train 0 5 0 9 -learning_rate constant -eta0 1 -loss hinge -penalty l2 -alpha 0.0001 -learning_rate optimal -eta0 0.0 -n_iter 2"
        sys.argv += " --runsolver-path " + self.runsolver
        sys.argv = sys.argv.split(" ")

        wrapper.main(exit=False)

        self.assertEqual(wrapper.data.status, "SUCCESS")
        self.assertGreater(2, wrapper.data.time)
        self.assertGreater(1, wrapper.data.cost)
        # the irace hack should not change the results 
        # for set cost values
        self.assertNotEqual(wrapper.data.cost, wrapper.data.time)
        self.assertEqual(wrapper.data.seed, 9)
        self.assertEqual(wrapper.data.runlength, 0)
        self.assertFalse(wrapper.data.new_format)

    def test_sgd_new(self):

        wrapper = SGDWrapper()

        sys.argv = "examples/SGD/SGDWrapper.py --instance train --seed 9 --config -learning_rate constant -eta0 1 -loss hinge -penalty l2 -alpha 0.0001 -learning_rate optimal -eta0 0.0 -n_iter 2"
        sys.argv += " --runsolver-path " + self.runsolver
        sys.argv = sys.argv.split(" ")

        wrapper.main(exit=False)

        self.assertEqual(wrapper.data.status, "SUCCESS")
        self.assertGreater(2, wrapper.data.time)
        self.assertGreater(1, wrapper.data.cost)
        # the irace hack should not change the results 
        # for set cost values
        self.assertNotEqual(wrapper.data.cost, wrapper.data.time)
        self.assertEqual(wrapper.data.seed, 9)
        self.assertTrue(wrapper.data.new_format)