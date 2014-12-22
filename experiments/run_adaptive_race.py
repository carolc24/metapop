#! /usr/bin/python

# Copyright 2014 Adam Waite
#
# This file is part of metapop.
#
# metapop is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.  
#
# metapop is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with metapop.  If not, see <http://www.gnu.org/licenses/>.

import os
import sys
import subprocess
import socket
from itertools import product, chain
from time import time, sleep
from numbers import Number

class SeedGenerator(object):
    base = None 
    def __init__(self):
        if SeedGenerator.base is None:
            SeedGenerator.base = int(time())
        else:
            SeedGenerator.base += 1

    def generate(self,n):
        SeedGenerator.base += 10
        return [str(SeedGenerator.base+i) for i in range(n)]

class AdaptiveRaceParams(object):
    def __init__(self):

        self.class_base = 'build/classes/framework'
        self.code_base = os.path.dirname(os.path.abspath('.'))
        self.package = 'org.fhcrc.honeycomb.metapop.experiment'
        self.time = '8-0'

        self.program = "NoDilutionMutationAR"

        self.n_seeds = 5
        self.seeds = []
        self.sg = SeedGenerator()

        self.migration_ranges = ['global', 'local']
        self.migration_type = ['indv']
        self.coop_freq = [0.5]
        self.initial_pop_size = [1e5]
        self.mutant_freqs = [0, 2e-5, 2e-4, 2e-3]
        self.frac_occupied = [0.25, 0.5, 0.75, 1]
        self.coop_to_cheat = [1e-8] 
        self.cheat_to_coop = [0]
        self.mut_rate = [1e-10]
        self.migration_rates = [0] + [10**-y for y in reversed(range(4,13))]

        self.base_km = [10]
        self.coop_release = [2.4]
        self.amount_needed = [5.5]
        self.cheat_adv = [1.2]
        self.evo_km_adv = [10]
        self.evo_death_adv = [2]
        self.evo_trade = [0.7]

        self.initial_resource = [0.0]
        self.size = [12]
        self.hours = [20000]
        self.save_every = [10]
        self.randomize = False
        self.output = ''

        self.commands = []

    def set_env(self, hostname):
        self.sbatch = ''
        self.end = ''
        self.save_base = 'dat'
        if hostname == 'Nietzsche':
            self.java = "time java -Xmx1000m -server"
        elif hostname == 'eureka':
            self.java = "time java -Xmx4000m -server"
        else:
            self.mem = '4000'
            self.mem2 = '5000'
            self.java = ("echo node id: $SLURM_NODEID; "
                         "echo nodelist: $SLURM_NODELIST; "
                         "time java -d64 -server -Xmx{0}m "
                         "-XX:+HeapDumpOnOutOfMemoryError "
                         "-Djava.io.tmpdir=$TMPDIR".format(self.mem))
            self.sbatch = ("sbatch --mem={} -n1 -t{} "
                           "--wrap='".format(self.mem2, self.time))
            self.end = "'"
            self.save_base = ('metapop_results_mutation')


    def make_params(self):
        self.set_env(socket.gethostname())
        self.cp = ('-cp .:$CLASSPATH:{0}/lib/commons-math.jar:'
                   '{0}/{1}').format(self.code_base, self.class_base)

        self.params = dict((k, string(v)) for (k,v) in
                           self.__dict__.iteritems() if k != "params")

    def prep(self, reps):
        subprocess.call(['ant', '-f', '../build.xml'])
        for i in range(reps):
            self.build_commands()

    def test(self, reps):
        self.prep(reps)
        print "\n\n".join(self.commands)
        print len(self.commands), "runs."

    def run(self, reps):
        self.prep(reps)
        if self.sbatch:
            sent = 0
            n_runs = len(self.commands)
            for cmd in self.commands:
                while True:
                    queue = check_queue()
                    if queue > 100:
                        print ("queue is {}. {} of {} jobs sent."
                               " Sleeping...".format(queue, sent, n_runs))
                        sys.stdout.flush()
                        sleep(60)
                    else:
                        break

                os.system(cmd)
                sent += 1
                sleep(10)
            print "Done!"
        else:
            map(os.system, self.commands)

    def build_commands(self):
        self.make_params()
        params = self.params

        self.full_program_name = self.package + '.' + self.program
        command =  " ".join([self.sbatch, self.java, self.cp,
                             self.full_program_name])

        args = list(list(i) for i in product(
                                             params['migration_ranges'], #0
                                             params['migration_type'],   #1
                                             params['initial_pop_size'], #2
                                             params['mutant_freqs'],     #3
                                             params['coop_release'],     #4
                                             params['amount_needed'],    #5
                                             params['coop_freq'],        #6
                                             params['base_km'],          #7
                                             params['cheat_adv'],        #8
                                             params['evo_km_adv'],       #9
                                             params['evo_death_adv'],    #10
                                             params['evo_trade'],        #11
                                             params['initial_resource'], #12
                                             params['size'],             #13
                                             params['frac_occupied'],    #14
                                             params['migration_rates'],  #15
                                             params['coop_to_cheat'],    #16
                                             params['cheat_to_coop'],    #17
                                             params['mut_rate']))        #18

        output = self.output
        if output == '': output = self.__class__.__name__
        outputs = []
        cmds = []
        template = '{0}_{1}_n={2}_mutant-freq={3}_mig={15}_' \
                   'mut-rate={18}_coop-to-cheat={16}_coop-release={4}_' \
                   'cheat-adv={8}_km-adv={9}_death-adv={10}_' \
                   'coop-freq={6}_size={13}_occ={14}'

        for arg in args:
            seeds = []
            if len(self.seeds)==0:
                seeds = self.sg.generate(self.n_seeds)
            elif len(self.seeds) < self.n_seeds:
                raise Exception("need " + self.n_seeds + " seeds.")
            else:
                seeds = self.seeds

            cmds.append(arg + [str(self.randomize)] + seeds +
                        params['hours'] + params['save_every'])
            out_string = (template.format(*arg) + 
                          '_hrs={}'.format(params['hours'][0]))
            outputs.append(os.path.join(self.save_base, output,
                           out_string))

        for cmd, out in zip(cmds, outputs):
            self.commands.append(
                    " ".join([command, " ".join(cmd), out, self.end]))


def check_queue():
    squeue = 'squeue -u ccannist | wc -l'

    # one line is the header.
    return int(subprocess.check_output(squeue, shell=True).rstrip('\n')) - 1

def string(s):
    if type(s) == list and len(s) > 0:
        if isinstance(s[0], Number) and type(s[0]) != bool:
            return ["{:.2e}".format(i) for i in s]
        else:
            return s
    elif type(s) != str:
        return str(s)
    else:
        return s

class AdaptiveRace(AdaptiveRaceParams):
    def __init__(self):
        super(AdaptiveRace, self).__init__()
        self.migration_ranges = ['global']
        self.output='no_dilution'

class NoMut(AdaptiveRace):
    def __init__(self):
        super(NoMut, self).__init__()
        self.mutant_freqs = [0];
        self.coop_to_cheat = [0];
        self.mut_rate = [0];
        self.frac_occupied = [1];
        self.migration_rates = [0];
        

class NodeTest(AdaptiveRace):
    def __init__(self):
        super(NodeTest, self).__init__()
        self.migration_rates = [0];
        self.mutant_freqs = [0];
        self.coop_to_cheat = [0];
        self.mut_rate = [0];
        self.frac_occupied = [0.1];
        self.hours = [100];
        self.save_every = [100];

class TwoMut(AdaptiveRace):
    def __init__(self):
        super(TwoMut, self).__init__()
        self.migration_rates = [1e-8];
        self.mutant_freqs = [0.00004];
        self.coop_to_cheat = [1e-10];

class MutAssay(AdaptiveRace):
    def __init__(self):
        super(MutAssay, self).__init__()
        self.mut_rate = [0];
        self.coop_to_cheat = [1e-7];
        self.mutant_freqs = [0];
        self.frac_occupied = [0.5, 0.9, 1];

class LowOcc(AdaptiveRace):
    def __init__(self):
        super(LowOcc, self).__init__()
        self.frac_occupied = [0.01, 0.03, 0.1, 0.2]
        self.output='no_dilution/keep'
        #self.frac_occupied = [y/100.0 for y in range(26, 30)]


class OtherConds(AdaptiveRace):
    def __init__(self):
        super(OtherConds, self).__init__()
        self.migration_ranges = ['global']
        self.frac_occupied = [0.5]
        self.mutant_freqs = [0, 2e-5, 2e-4, 2e-3]

class HighOcc(OtherConds):
    def __init__(self):
        super(HighOcc, self).__init__()
        self.frac_occupied = [0.99, 0.97, 0.90]

class CheaterTitration(OtherConds):
    def __init__(self):
        super(CheaterTitration, self).__init__()
        self.mutant_freqs = [1e-4]
        self.coop_freq = [0.1]
        self.migration_rates = [0]
        self.output = 'no_dilution/cheater_titration'

class LowRelease(OtherConds):
    def __init__(self):
        super(LowRelease, self).__init__()
        self.coop_release = [0.457]
        self.mutant_freqs = [2e-3]
        self.output = 'no_dilution/evo_limiting_release/beta=0.457'

class VeryLowRelease(OtherConds):
    def __init__(self):
        super(VeryLowRelease, self).__init__()
        self.coop_release = [0.00172]
        self.output = 'no_dilution/evo_limiting_release_retry/beta=0.0017'

class ReleaseTest(AdaptiveRaceParams):
    def __init__(self):
        super(ReleaseTest, self).__init__()
        self.size = [32]
        self.initial_pop_size = [1]
        self.frac_occupied = [1]
        self.migration_ranges = ['global']
        self.migration_rates = [0]
        self.hours = [5000]
        self.save_every = [1]
        self.coop_freq = [1]
        self.coop_release = ([x + y/20.0 for x in range (0,3) 
                                for y in range(0,20)] + range(3,100))

class AncReleaseTest(ReleaseTest):
    def __init__(self):
        super(AncReleaseTest, self).__init__()
        self.mutant_freqs = [0]
        self.output = 'release_test/anc'

class EvoReleaseTest(ReleaseTest):
    def __init__(self):
        super(EvoReleaseTest, self).__init__()
        self.mutant_freqs = [1]
        self.output = 'release_test/evo'

class Test(AdaptiveRaceParams):
    def __init__(self):
        super(Test, self).__init__()
        self.migration_ranges = ['global']
        self.mutant_freqs = [2e-5]
        self.migration_rates = [1e-7]
        self.frac_occupied = [0.25]
        self.initial_resource = [0.0]
        self.coop_to_cheat = [0.0]
        self.seeds = [str(i) for i in '1'*self.n_seeds]
        self.save_every = [10]

class Benchmark(AdaptiveRaceParams):
    def __init__(self):
        super(Benchmark, self).__init__()
        self.migration_ranges = ['global']
        self.size = [2]
        self.coop_freq = [0.99]
        self.mutant_freqs = [2e-3]
        self.migration_rates = [0]
        self.frac_occupied = [0.5]
        self.initial_resource = [0.0]
        self.coop_to_cheat = [0.0]
        self.hours = [1000]
        self.save_every = [1]
        self.seeds = [str(i) for i in '1'*self.n_seeds]

if __name__ == "__main__":
    #ps = Benchmark()
    #ps = Test()
    #ps = AncReleaseTest()
    #ps = AdaptiveRace()
    #ps = CoopToCheat()
    #ps = LongRunning2()
    #ps = PeriodicDilution99_1e3()
    #ps = NoMut()
    #ps = LowRelease()
    #ps = VeryLowRelease()
    #ps = CheaterTitration()
    #ps = LowOcc()
    nm = NoMut()
    #ps.test(1)
    nm.run(1)
