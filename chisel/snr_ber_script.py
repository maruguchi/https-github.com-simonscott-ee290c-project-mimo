#!/bin/python

import os

fix_pt = [(24, 12), (20, 10), (16, 10), (14, 8)]
max_ant = [4]
snr = range(0, 35, 5)

testDir = ["snr_adaptive_" + str(s) + "db" for s in snr]

for f in fix_pt:
    for ntx in max_ant:
        for tDir in testDir:

            os.system('make test fix_pt_wd=' + str(f[0]) + ' fix_pt_exp=' + str(f[1]) + ' max_ntx=' + str(ntx) + ' max_nrx=' + str(ntx) + ' T=LMSDecoder testDir=' + tDir)



