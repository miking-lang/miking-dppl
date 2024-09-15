# for each of the cases run with and without transformation with increasing number of particles
# plot a graph where x-axis is the number of particles and y-axis is the sample mean
# save them in a format

#python scripts/run-plot-transform-results-cppl.py coreppl/models/experiments/lda/ --modelN lda --datasetName lw --filename lda-lw --lstPT 300 3000 30000 --lstP 300 3000 30000 --numruns 30
#nohup python scripts/run-plot-transform-results-cppl.py coreppl/models/static-delay-paper-experiment/lda/ --modelN lda --datasetName nips40 --filename lda-nips40 --lstPT 100 1000 10000 --lstP 100 1000 10000 --numruns 30 &
#nohup python scripts/run-plot-transform-results-cppl.py coreppl/models/static-delay-paper-experiment/lda/ --modelN lda --datasetName lwv100 --filename lda-lw-v100 --lstPT 300 3000 30000 --lstP 300 3000 30000 --numruns 30 &
#nohup python scripts/run-plot-transform-results-cppl.py coreppl/models/static-delay-paper-experiment/lda/ --modelN lda --datasetName lwd50 --filename lda-lw-d50--lstPT 300 3000 30000 --lstP 300 3000 30000 --numruns 30 &
#nohup python scripts/run-plot-transform-results-cppl.py coreppl/models/static-delay-paper-experiment/lda/ --modelN lda --datasetName lww20 --filename lda-lw-w20 --lstPT 300 3000 30000 --lstP 300 3000 30000 --numruns 30 &
# python scripts/run-plot-transform-results-cppl.py coreppl/models/experiments/blr/ --modelN blr --datasetName housing --filename blr --lstPT 300 3000 30000 --lstP 300 3000 30000 --numruns 30

import os
import argparse
import seaborn as sns
import pandas as pd
from scipy.special import rel_entr
import matplotlib.pyplot as plt
import subprocess
import math
import time
import numpy as np
parser = argparse.ArgumentParser(description=
        "Compile and run the model with specified number of particles and plot the samples with transformed and nontransformed based on the chosen metric")
parser.add_argument('filepath', nargs='*', type=str,help= "File path of the model")
parser.add_argument('--numruns',  type=int, default=100)
parser.add_argument('--modelN',  type=str, default="jc")
parser.add_argument('--filename',  type=str, default="tree-inference")
parser.add_argument('--datasetName',  type=str, default="toy")
parser.add_argument('--datasetIdent',  type=str, default="")
parser.add_argument('--lstPT', nargs='+', type=int, default=[100,1000,10000,100000,1000000],help='A list of particles')
parser.add_argument('--lstP', nargs='+', type=int, default=[100,1000,10000,100000,1000000],help='A list of particles')

args = parser.parse_args()
resdir = "data_results/" + args.datasetName + args.datasetIdent
datasetName = args.datasetName
isExist = os.path.exists(resdir)
if not isExist:
    os.makedirs(resdir)
outname = 'out.mc'
numparticlesTransformed= args.lstPT
numparticlesNontransformed=args.lstP
modelN = args.modelN
filenameS = args.filename


def parse_results(result, num_runs, num_particles):
    result = result.split("\n")
    norm_constants = np.zeros(num_runs)
    count = 0
    for r in range(num_runs):
#        norm_constant = math.exp(float(result[count]))
        norm_constants[r] = result[count]
        count += 1
    return norm_constants

def plot_box(data1,label1,data2,label2,data3,label3,xrange):
    sns.set(style="darkgrid")
    dfs =[]

    for ind,p in enumerate(xrange):
        df = pd.DataFrame(np.column_stack((data1[ind],data2[ind],data3[ind])),columns=[label1,label2,label3]).assign(N=p)
        dfs.append(df)
    cdf = pd.concat(dfs)
    mdf = pd.melt(cdf, id_vars=['N'], var_name=['Method']) 
    mdf.rename(columns={"value": "Log(z)"}, inplace=True) 
    ax = sns.boxplot(x="N", y="Log(z)", hue="Method", data=mdf)  # RUN PLOT   
    plt.savefig(resdir +'/box_plot' + modelN + '.pdf')

filepath = args.filepath[0]
filepathAuto = filepath + "dynamic-delayed/" + filenameS
numruns = args.numruns
norm_auto_dynamic_delayed = []
subprocess.run(['cppl',  filepathAuto + ".mc", '--no-print-samples','--dynamic-delay', "--extract-simplification", "inline","--cps","none"])
filenameT =  resdir + "/time" + modelN + "-" + datasetName +  "-" +"dynamic" + ".dat"
fT = open(filenameT, "w")
fT.write("particles\truntime\tstd\n")
for i,p in enumerate(numparticlesTransformed):
    fT.write(f"{p}\t")
    elapsed_time = np.zeros(numruns)
    filename = resdir + "/logz" + modelN + "-" + datasetName + "-" +"dynamic" + "-" + str(i+1) + ".dat"
    f = open(filename, "w")
    f.write("data\n")
    norm_constants = np.zeros(numruns)
    for r in range(numruns):
         # compile from coreppl to rootppl with transformation
        # Record the starting time
        start_time = time.time()
        result = subprocess.run(['./out', str(p)], stdout=subprocess.PIPE)
        # Record the ending time
        end_time = time.time()
        # Calculate the elapsed time
        elapsed_time[r] = (end_time - start_time)
        result_transformation = result.stdout.decode('utf-8')
        norm_constants[r] = float(result.stdout.decode('utf-8'))
        f.write(str(norm_constants[r]) +"\n")
    
    norm_auto_dynamic_delayed.append(norm_constants) 
    elapsed_time_mean = np.mean(elapsed_time)
    elapsed_time_std = np.std(elapsed_time)
    fT.write(f"{elapsed_time_mean}\t")
    fT.write(f"{elapsed_time_std}\n")
    f.close()
subprocess.run(['rm', './out'])
fT.close()


filepathNodelay = filepath + "no-delayed/" + filenameS
subprocess.run(['cppl',  filepathNodelay + ".mc", '--no-print-samples', "--extract-simplification", "inline","--cps","none"])

norm_nodelayed = []
filenameT =  resdir + "/time" + modelN + "-" + datasetName + "-" + "no-delayed" + ".dat"
fT = open(filenameT, "w")
fT.write("particles\truntime\tstd\n")
for i,p in enumerate(numparticlesTransformed):
    fT.write(f"{p}\t")
    elapsed_time = np.zeros(numruns)
    filename =  resdir + "/logz" + modelN + "-" + datasetName + "-" + "no-delayed" + "-" + str(i+1) + ".dat"
    f = open(filename, "w")
    f.write("data\n")
    norm_constants = np.zeros(numruns)
    for r in range(numruns):
         # compile from coreppl to rootppl with transformation
        # Record the starting time
        start_time = time.time()
        result = subprocess.run(['./out', str(p)], stdout=subprocess.PIPE)
        # Record the ending time
        end_time = time.time()
        # Calculate the elapsed time
        elapsed_time[r] = (end_time - start_time)
        result_transformation = result.stdout.decode('utf-8')
        norm_constants[r] = float(result.stdout.decode('utf-8'))
        f.write(str(norm_constants[r]) +"\n")
    
    norm_nodelayed.append(norm_constants) 
    elapsed_time_mean = np.mean(elapsed_time)
    elapsed_time_std = np.std(elapsed_time)
    fT.write(f"{elapsed_time_mean}\t")
    fT.write(f"{elapsed_time_std}\n")
    f.close()
subprocess.run(['rm', './out'])
fT.close()


filepathHardcoded = filepath + "static-delayed/" + filenameS
subprocess.run(['cppl',  filepathHardcoded +".mc", '--no-print-samples', '--static-delay-ref', "--extract-simplification", "inline","--cps","none"])
norm_static = []
filenameT =  resdir + "/time" + modelN + "-"  + datasetName + "-" + "static" + ".dat"
fT = open(filenameT, "w")
fT.write("particles\truntime\tstd\n")
for i,p in enumerate(numparticlesTransformed):
    fT.write(f"{p}\t")
    elapsed_time = np.zeros(numruns)
    filename =  resdir + "/logz" + modelN + "-" + datasetName + "-" + "static" + "-" + str(i+1) + ".dat"
    f = open(filename, "w")
    f.write("data\n")
    norm_constants = np.zeros(numruns)
    for r in range(numruns):
         # compile from coreppl to rootppl with transformation
        # Record the starting time
        start_time = time.time()
        result = subprocess.run(['./out', str(p)], stdout=subprocess.PIPE)
        # Record the ending time
        end_time = time.time()
        # Calculate the elapsed time
        elapsed_time[r] = (end_time - start_time)
        result_transformation = result.stdout.decode('utf-8')
        norm_constants[r] = float(result.stdout.decode('utf-8'))
        f.write(str(norm_constants[r]) +"\n")
    
    norm_static.append(norm_constants) 
    elapsed_time_mean = np.mean(elapsed_time)
    elapsed_time_std = np.std(elapsed_time)
    fT.write(f"{elapsed_time_mean}\t")
    fT.write(f"{elapsed_time_std}\n")
    f.close()
subprocess.run(['rm', './out'])
fT.close()

plot_box(norm_auto_dynamic_delayed, "Dynamic pruning",norm_static,"Static pruning",norm_nodelayed, "Nodelay",numparticlesNontransformed)


