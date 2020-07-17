# SRC_FILE=models/phylogenetics/crbd/crbd_webppl.cu
# MODEL=models/airplane/airplane.cu
define \n


endef


# ifndef MODEL
# $(error MODEL argument needs to be set with e.g.: $(\n)make MODEL=path/to/model.cu$(\n))
# endif

ifdef GPU
# GPU
CC=/usr/local/cuda-11.0/bin/nvcc
FLAGS=-I. -std=c++14 -arch=sm_75 -rdc=true -lcudadevrt -O3 -DRELEASE
FLAGS_LINK=-std=c++14 -arch=sm_75 -rdc=true -lcudadevrt -O3 -DRELEASE
else
# CPU
CC=g++
FLAGS=-I. -xc++ -std=c++14 -O3
FLAGS_LINK=-std=c++14 -O3
endif

EXEC_NAME=prog

SMC_SRC=inference/smc/smc_impl.cu
DISTS_SRC=dists/dists.cu
MISC_SRC=utils/misc.cu
MATH_SRC=utils/math.cu

OBJDIR=out

# $@ is target name
# $< is dependency list

# Link object files to executable
$(EXEC_NAME): $(OBJDIR)/model.o $(OBJDIR)/smc_impl.o $(OBJDIR)/dists.o $(OBJDIR)/misc.o $(OBJDIR)/math.o
	$(CC) $(FLAGS_LINK) out/*.o -o $@
	rm $(OBJDIR)/model.o

# Compile model, which is always recompiled as it is deleted after the executable is created.
$(OBJDIR)/model.o: $(MODEL)
	$(CC) -c $(FLAGS) $(MODEL) -o $@
# $(CC) -c $(FLAGS) $(MODEL) -o model.o

# Compile only framework
.PHONY: framework
framework: $(OBJDIR)/smc_impl.o $(OBJDIR)/dists.o $(OBJDIR)/misc.o $(OBJDIR)/math.o


# Compile smc, depend on all files in smc dir and cuh files in resample dirs
$(OBJDIR)/smc_impl.o: $(wildcard inference/smc/*) $(wildcard inference/smc/resample/*/*.cuh) $(wildcard macros/*.cuh) dists/dists.cuh utils/cuda_error_utils.cuh utils/misc.cuh utils/math.cuh
	$(CC) -c $(FLAGS) $(SMC_SRC) -o $@

$(OBJDIR)/dists.o: $(wildcard dists/*) utils/misc.cuh utils/math.cuh $(wildcard macros/*.cuh)
	$(CC) -c $(FLAGS) $(DISTS_SRC) -o $@

$(OBJDIR)/misc.o: $(MISC_SRC) utils/misc.cuh utils/math.cuh $(wildcard macros/*.cuh)
	$(CC) -c $(FLAGS) $(MISC_SRC) -o $@

$(OBJDIR)/math.o: $(MATH_SRC) utils/math.cuh $(wildcard macros/*.cuh)
	$(CC) -c $(FLAGS) $(MATH_SRC) -o $@


.PHONY: clean
clean:
	rm out/*.o $(EXEC_NAME)

# run: ./$(EXEC_NAME)
.PHONY: run
run:
	./$(EXEC_NAME) $(N)
