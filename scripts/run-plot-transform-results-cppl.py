#python scripts/run-plot-transform-results-cppl.py coreppl/models/diversification-models/crbd-synthetic.mc --datasetName syntheticSmall --lstPT 1000 10000 100000 --lstP 1000 10000 100000
#python scripts/run-plot-transform-results-cppl.py coreppl/models/diversification-models/crbd-synthetic.mc --datasetName synthetic --lstPT 1000 10000 100000 --lstP 1000 10000 100000
#python scripts/run-plot-transform-results-cppl.py coreppl/models/ --datasetName tree-inference --lstPT 10 100 1000 10000 --lstP 10 100 1000 10000

#python scripts/run-plot-transform-results-cppl.py coreppl/models/paperExperiment/lda-lw.mc --datasetName lw --lstPT 100 1000 10000 --lstP 100 1000 10000 100000 1000000
#python scripts/run-plot-transform-results-cppl.py coreppl/models/paperExperiment/lda-c3.mc --datasetName c3 --lstPT 100 1000 10000 --lstP 100 1000 10000 100000 1000000

# for each of the cases run with and without transformation with increasing number of particles
# plot a graph where x-axis is the number of particles and y-axis is the sample mean
# save them in a format
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

parser.add_argument('filepath', nargs='*', type=str,help= "File path of the model to be sampled")
parser.add_argument('--mean', action='store_true')
# parser.add_argument('--maxnumparticles',  type=int, default=1000)
parser.add_argument('--numruns',  type=int, default=100)
parser.add_argument('--datasetName',  type=str, default="default")
# Add a list argument using 'nargs' parameter
parser.add_argument('--lstPT', nargs='+', type=int, default=[100,1000,10000,100000,1000000],help='A list of particles')
# Add a list argument using 'nargs' parameter
parser.add_argument('--lstP', nargs='+', type=int, default=[100,1000,10000,100000,1000000],help='A list of particles')

# parser.add_argument('--step',  type=int, default=10)
args = parser.parse_args()
resdir = "data_results/" + args.datasetName
isExist = os.path.exists(resdir)
if not isExist:
    os.makedirs(resdir)
outname = 'out.mc'
numparticlesTransformed= args.lstPT
numparticlesNontransformed=args.lstP
def parse_results(result, num_runs, num_particles):
    result = result.split("\n")
    norm_constants = np.zeros(num_runs)
    count = 0
    for r in range(num_runs):
#        norm_constant = math.exp(float(result[count]))
        norm_constants[r] = result[count]
        count += 1
    return norm_constants

def mean(samples):
    sums = 0
    for s in samples:
        sums += s
    return sums/len(samples)

def plot(data1, label1, data2, label2, x_range):
    x_axis = x_range
    ax = plt.subplot(1,1,1)
    ax.plot(x_axis, data1, 'ro', label = label1)
    ax.plot(x_axis, data2, 'bx', label = label2)
    ax.legend(fancybox=True, framealpha=1, shadow=True, borderpad=1)
    #plot1 = plt.figure(v)
    plt.show()

def plot_box(data1,label1,data2,label2,xrange):
    sns.set(style="darkgrid")
    # print(data1)
    # print(data2)
    dfs =[]

    for ind,p in enumerate(xrange):
        df = pd.DataFrame(np.column_stack((data1[ind],data2[ind])),columns=[label1,label2]).assign(N=p)
        dfs.append(df)
        # df2 = pd.DataFrame(data2[p],columns=).assign(N=p)
    cdf = pd.concat(dfs)
    mdf = pd.melt(cdf, id_vars=['N'], var_name=['Method']) 
    mdf.rename(columns={"value": "Log(z)"}, inplace=True) 
    ax = sns.boxplot(x="N", y="Log(z)", hue="Method", data=mdf)  # RUN PLOT   
    plt.show()

modeln = "crbd"
norm_transformed = []
for i,p in enumerate(numparticlesTransformed):
     # compile from coreppl to rootppl with transformation
    p = numparticlesTransformed[i]
    subprocess.run(['cppl',  "coreppl/models/tree-inference-user.mc", '--no-print-samples','--prune', '-m','smc-apf'])
    # Record the starting time
    start_time = time.time()
    result = subprocess.run(['./out', str(p), str(args.numruns)], stdout=subprocess.PIPE)
    # Record the ending time
    end_time = time.time()
    # Calculate the elapsed time
    elapsed_time_transformed = end_time - start_time
    result_transformation = result.stdout.decode('utf-8')
    transformed_norm_constants = parse_results(result_transformation, args.numruns, p)
    norm_transformed.append(transformed_norm_constants)
    filename =  resdir + "/logz" + modeln + "-" + "pruned" + "-" + str(i+1) + ".dat"
    f = open(filename, "w")
    f.write("data\n")
    for e in transformed_norm_constants:
        f.write(str(e) +"\n")
    f.close()
    filename =  resdir + "/time" + modeln + "-" + "pruned" + "-" + str(i+1) + ".dat"
    f = open(filename, "w")
    f.write("data\n")
    f.write(f"{elapsed_time_transformed}\n")
    f.close()
    #subprocess.run(['mi', 'compile', 'out.mc', '--output', 'transformed'])
    subprocess.run(['rm', './out'])
    
norm_nontransformed = []
for i,p in enumerate(numparticlesNontransformed):
    p = numparticlesNontransformed[i]
    # compile from coreppl to rootppl without transformation
    subprocess.run(['cppl',  "coreppl/models/tree-inference.mc", '--no-print-samples','-m','smc-apf'])    # run the rootppl
    start_time = time.time()
    result = subprocess.run(['./out', str(p), str(args.numruns)], stdout=subprocess.PIPE)
    # Record the ending time
    end_time = time.time()
    # Calculate the elapsed time
    elapsed_time_nontransformed = end_time - start_time
    result_no_transformation = result.stdout.decode('utf-8')
    nontransformed_norm_constants = parse_results(result_no_transformation, args.numruns, p)
    filename =  resdir + "/logz" + modeln + "-" + "noprune" + "-" + str(i+1) + ".dat"
    f = open(filename, "w")
    f.write("data\n")
    for e in nontransformed_norm_constants:
        f.write(str(e) +"\n")
    f.close()

    filename =  resdir + "/time" + modeln + "-" + "noprune" + "-" + str(i+1) + ".dat"
    f = open(filename, "w")
    f.write("data\n")
    f.write(f"{elapsed_time_nontransformed}\n")
    f.close()
    
    norm_nontransformed.append(nontransformed_norm_constants)
    subprocess.run(['rm', './out'])
plot_box(norm_transformed,"Pruned",norm_nontransformed, "No pruning",numparticlesNontransformed)


