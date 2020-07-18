# SRC_FILE=models/phylogenetics/crbd/crbd_webppl.cu
# MODEL=models/airplane/airplane.cu
# define \n


# endef


# ifndef MODEL
# $(error MODEL argument needs to be set with e.g.: $(\n)make MODEL=path/to/model.cu$(\n))
# endif

ifdef arch
# GPU
CC=/usr/local/cuda-11.0/bin/nvcc
FLAGS=-I. -std=c++14 -arch=sm_$(arch) -rdc=true -lcudadevrt -O3 -DRELEASE
FLAGS_LINK=-std=c++14 -arch=sm_$(arch) -rdc=true -lcudadevrt -O3 -DRELEASE
else
# CPU
CC=g++
FLAGS=-I. -xc++ -std=c++14 -O3
FLAGS_LINK=-std=c++14 -O3
endif

EXEC_NAME=program

SMC_SRC=inference/smc/smc.cu
SMC_NESTED_SRC=inference/smc/smc_nested.cu
DISTS_SRC=dists/dists.cu
SCORES_SRC=dists/scores.cu
PARTICLES_MEMORY_HANDLER_SRC=inference/smc/particles_memory_handler.cu
MISC_SRC=utils/misc.cu
MATH_SRC=utils/math.cu
SMC_KERNELS_SRC=inference/smc/smc_kernels.cu
SYSTEMATIC_COMMON_SRC=inference/smc/resample/systematic/common.cu
SYSTEMATIC_SEQ_SRC=inference/smc/resample/systematic/sequential.cu
SYSTEMATIC_PARALLEL_SRC=inference/smc/resample/systematic/parallel.cu
SYSTEMATIC_KERNELS_SRC=inference/smc/resample/systematic/kernels.cu

OBJDIR=out
_OBJ_FILES_FRAMEWORK=smc.o smc_nested.o dists.o scores.o particles_memory_handler.o misc.o math.o \
	smc_kernels.o systematic_common.o systematic_seq.o systematic_parallel.o systematic_kernels.o
OBJ_FILES_FRAMEWORK=$(patsubst %.o, $(OBJDIR)/%.o, $(_OBJ_FILES_FRAMEWORK))

# SMC_DEPS = $(wildcard inference/smc/*.cuh) $(wildcard inference/smc/resample/*/*.cuh) $(wildcard macros/*.cuh) dists/dists.cuh utils/cuda_error_utils.cuh utils/misc.cuh utils/math.cuh
FRAMEWORK_FILES = $(wildcard inference/*) $(wildcard inference/*/*) $(wildcard inference/*/*/*) $(wildcard inference/*/*/*/*) \
	$(wildcard dists/*) $(wildcard macros/*) $(wildcard utils/*)

# $@ is target name
# $< is dependency list

# Link object files to executable
$(EXEC_NAME): $(OBJDIR)/model.o $(OBJ_FILES_FRAMEWORK)
	$(CC) $(FLAGS_LINK) out/*.o -o $@
	rm $(OBJDIR)/model.o

# Compile model, which is always recompiled as it is deleted after the executable is created.
$(OBJDIR)/model.o: $(MODEL)
	$(CC) -c $(FLAGS) $(MODEL) -o $@
# $(CC) -c $(FLAGS) $(MODEL) -o model.o

# Compile only framework
.PHONY: framework
framework: $(OBJ_FILES_FRAMEWORK)

# $(OBJ_FILES_FRAMEWORK): $(FRAMEWORK_FILES)
	# $(CC) -c $(FLAGS) $< -o $@

# Compile smc, depend on all files in smc dir and cuh files in resample dirs
# $(OBJDIR)/smc_impl.o: $(wildcard inference/*.cuh) $(wildcard macros/*.cuh) dists/dists.cuh $(wildcard utils/*.cuh)

# $(OBJDIR)/smc_impl.o: $(SMC_SRC) $(SMC_DEPS)
$(OBJDIR)/smc.o: $(FRAMEWORK_FILES)
	$(CC) -c $(FLAGS) $(SMC_SRC) -o $@

# $(OBJDIR)/smc_impl_nested.o: $(SMC_NESTED_SRC) $(SMC_DEPS)
$(OBJDIR)/smc_nested.o: $(FRAMEWORK_FILES)
	$(CC) -c $(FLAGS) $(SMC_NESTED_SRC) -o $@

# $(OBJDIR)/dists.o: $(wildcard dists/*) utils/misc.cuh utils/math.cuh $(wildcard macros/*.cuh)
$(OBJDIR)/dists.o: $(FRAMEWORK_FILES)
	$(CC) -c $(FLAGS) $(DISTS_SRC) -o $@

$(OBJDIR)/scores.o: $(FRAMEWORK_FILES)
	$(CC) -c $(FLAGS) $(SCORES_SRC) -o $@

# $(OBJDIR)/particles_memory_handler.o: $(PARTICLES_MEMORY_HANDLER_SRC) inference/smc/smc.cuh utils/misc.cuh
$(OBJDIR)/particles_memory_handler.o: $(FRAMEWORK_FILES)
	$(CC) -c $(FLAGS) $(PARTICLES_MEMORY_HANDLER_SRC) -o $@

# $(OBJDIR)/misc.o: $(MISC_SRC) utils/misc.cuh utils/math.cuh $(wildcard macros/*.cuh)
$(OBJDIR)/misc.o: $(FRAMEWORK_FILES)
	$(CC) -c $(FLAGS) $(MISC_SRC) -o $@

# $(OBJDIR)/math.o: $(MATH_SRC) utils/math.cuh $(wildcard macros/*.cuh)
$(OBJDIR)/math.o: $(FRAMEWORK_FILES)
	$(CC) -c $(FLAGS) $(MATH_SRC) -o $@

# $(OBJDIR)/smc_kernels.o: $(SMC_KERNELS_SRC) inference/smc/smc.cuh
$(OBJDIR)/smc_kernels.o: $(FRAMEWORK_FILES)
	$(CC) -c $(FLAGS) $(SMC_KERNELS_SRC) -o $@

$(OBJDIR)/systematic_common.o: $(FRAMEWORK_FILES)
	$(CC) -c $(FLAGS) $(SYSTEMATIC_COMMON_SRC) -o $@

# $(OBJDIR)/systematic_seq.o: $(SYSTEMATIC_SEQ_SRC) inference/smc/resample/systematic/common.cuh inference/smc/resample/systematic/kernels.cuh inference/smc/smc.cuh utils/misc.cuh 
$(OBJDIR)/systematic_seq.o: $(FRAMEWORK_FILES)
	$(CC) -c $(FLAGS) $(SYSTEMATIC_SEQ_SRC) -o $@

# $(OBJDIR)/systematic_parallel.o: $(SYSTEMATIC_PARALLEL_SRC) inference/smc/resample/systematic/common.cuh inference/smc/resample/systematic/kernels.cuh inference/smc/smc.cuh utils/misc.cuh 
$(OBJDIR)/systematic_parallel.o: $(FRAMEWORK_FILES)
	$(CC) -c $(FLAGS) $(SYSTEMATIC_PARALLEL_SRC) -o $@

# $(OBJDIR)/systematic_kernels.o: $(SYSTEMATIC_KERNELS_SRC) inference/smc/smc.cuh
$(OBJDIR)/systematic_kernels.o: $(FRAMEWORK_FILES)
	$(CC) -c $(FLAGS) $(SYSTEMATIC_KERNELS_SRC) -o $@

.PHONY: clean
clean:
	rm out/*.o $(EXEC_NAME)

# run: ./$(EXEC_NAME)
.PHONY: run
run:
	./$(EXEC_NAME) $(N)
